// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/AssemblyBuilderA64.h"
#include "Luau/AssemblyBuilderX64.h"
#include "Luau/CodeGen.h"
#include "Luau/IrBuilder.h"
#include "Luau/IrDump.h"
#include "Luau/IrUtils.h"
#include "Luau/OptimizeConstProp.h"
#include "Luau/OptimizeFinalX64.h"

#include "EmitCommon.h"
#include "IrLoweringA64.h"
#include "IrLoweringX64.h"

#include "lobject.h"
#include "lstate.h"

#include <algorithm>
#include <vector>

LUAU_FASTFLAG(DebugCodegenNoOpt)
LUAU_FASTFLAG(DebugCodegenOptSize)
LUAU_FASTFLAG(DebugCodegenSkipNumbering)

namespace Luau
{
namespace CodeGen
{

inline void gatherFunctions(std::vector<Proto*>& results, Proto* proto)
{
    if (results.size() <= size_t(proto->bytecodeid))
        results.resize(proto->bytecodeid + 1);

    // Skip protos that we've already compiled in this run: this happens because at -O2, inlined functions get their protos reused
    if (results[proto->bytecodeid])
        return;

    results[proto->bytecodeid] = proto;

    for (int i = 0; i < proto->sizep; i++)
        gatherFunctions(results, proto->p[i]);
}

inline IrBlock& getNextBlock(IrFunction& function, std::vector<uint32_t>& sortedBlocks, IrBlock& dummy, size_t i)
{
    for (size_t j = i + 1; j < sortedBlocks.size(); ++j)
    {
        IrBlock& block = function.blocks[sortedBlocks[j]];
        if (block.kind != IrBlockKind::Dead)
            return block;
    }

    return dummy;
}

template<typename AssemblyBuilder, typename IrLowering>
inline bool lowerImpl(AssemblyBuilder& build, IrLowering& lowering, IrFunction& function, int bytecodeid, AssemblyOptions options)
{
    // While we will need a better block ordering in the future, right now we want to mostly preserve build order with fallbacks outlined
    std::vector<uint32_t> sortedBlocks;
    sortedBlocks.reserve(function.blocks.size());
    for (uint32_t i = 0; i < function.blocks.size(); i++)
        sortedBlocks.push_back(i);

    std::sort(sortedBlocks.begin(), sortedBlocks.end(), [&](uint32_t idxA, uint32_t idxB) {
        const IrBlock& a = function.blocks[idxA];
        const IrBlock& b = function.blocks[idxB];

        // Place fallback blocks at the end
        if ((a.kind == IrBlockKind::Fallback) != (b.kind == IrBlockKind::Fallback))
            return (a.kind == IrBlockKind::Fallback) < (b.kind == IrBlockKind::Fallback);

        // Try to order by instruction order
        return a.sortkey < b.sortkey;
    });

    // For each IR instruction that begins a bytecode instruction, which bytecode instruction is it?
    std::vector<uint32_t> bcLocations(function.instructions.size() + 1, ~0u);

    for (size_t i = 0; i < function.bcMapping.size(); ++i)
    {
        uint32_t irLocation = function.bcMapping[i].irLocation;

        if (irLocation != ~0u)
            bcLocations[irLocation] = uint32_t(i);
    }

    bool outputEnabled = options.includeAssembly || options.includeIr;

    IrToStringContext ctx{build.text, function.blocks, function.constants, function.cfg};

    // We use this to skip outlined fallback blocks from IR/asm text output
    size_t textSize = build.text.length();
    uint32_t codeSize = build.getCodeSize();
    bool seenFallback = false;

    IrBlock dummy;
    dummy.start = ~0u;

    for (size_t i = 0; i < sortedBlocks.size(); ++i)
    {
        uint32_t blockIndex = sortedBlocks[i];
        IrBlock& block = function.blocks[blockIndex];

        if (block.kind == IrBlockKind::Dead)
            continue;

        LUAU_ASSERT(block.start != ~0u);
        LUAU_ASSERT(block.finish != ~0u);

        // If we want to skip fallback code IR/asm, we'll record when those blocks start once we see them
        if (block.kind == IrBlockKind::Fallback && !seenFallback)
        {
            textSize = build.text.length();
            codeSize = build.getCodeSize();
            seenFallback = true;
        }

        if (options.includeIr)
        {
            build.logAppend("# ");
            toStringDetailed(ctx, block, blockIndex, /* includeUseInfo */ true);
        }

        // Values can only reference restore operands in the current block
        function.validRestoreOpBlockIdx = blockIndex;

        build.setLabel(block.label);

        if (blockIndex == function.entryBlock)
        {
            function.entryLocation = build.getLabelOffset(block.label);
        }

        IrBlock& nextBlock = getNextBlock(function, sortedBlocks, dummy, i);

        for (uint32_t index = block.start; index <= block.finish; index++)
        {
            LUAU_ASSERT(index < function.instructions.size());

            uint32_t bcLocation = bcLocations[index];

            // If IR instruction is the first one for the original bytecode, we can annotate it with source code text
            if (outputEnabled && options.annotator && bcLocation != ~0u)
            {
                options.annotator(options.annotatorContext, build.text, bytecodeid, bcLocation);
            }

            // If bytecode needs the location of this instruction for jumps, record it
            if (bcLocation != ~0u)
            {
                Label label = (index == block.start) ? block.label : build.setLabel();
                function.bcMapping[bcLocation].asmLocation = build.getLabelOffset(label);
            }

            IrInst& inst = function.instructions[index];

            // Skip pseudo instructions, but make sure they are not used at this stage
            // This also prevents them from getting into text output when that's enabled
            if (isPseudo(inst.cmd))
            {
                LUAU_ASSERT(inst.useCount == 0);
                continue;
            }

            // Either instruction result value is not referenced or the use count is not zero
            LUAU_ASSERT(inst.lastUse == 0 || inst.useCount != 0);

            if (options.includeIr)
            {
                build.logAppend("# ");
                toStringDetailed(ctx, block, blockIndex, inst, index, /* includeUseInfo */ true);
            }

            lowering.lowerInst(inst, index, nextBlock);

            if (lowering.hasError())
            {
                // Place labels for all blocks that we're skipping
                // This is needed to avoid AssemblyBuilder assertions about jumps in earlier blocks with unplaced labels
                for (size_t j = i + 1; j < sortedBlocks.size(); ++j)
                {
                    IrBlock& abandoned = function.blocks[sortedBlocks[j]];

                    build.setLabel(abandoned.label);
                }

                lowering.finishFunction();

                return false;
            }
        }

        lowering.finishBlock(block, nextBlock);

        if (options.includeIr)
            build.logAppend("#\n");
    }

    if (!seenFallback)
    {
        textSize = build.text.length();
        codeSize = build.getCodeSize();
    }

    lowering.finishFunction();

    if (outputEnabled && !options.includeOutlinedCode && textSize < build.text.size())
    {
        build.text.resize(textSize);

        if (options.includeAssembly)
            build.logAppend("; skipping %u bytes of outlined code\n", unsigned((build.getCodeSize() - codeSize) * sizeof(build.code[0])));
    }

    return true;
}

inline bool lowerIr(X64::AssemblyBuilderX64& build, IrBuilder& ir, ModuleHelpers& helpers, Proto* proto, AssemblyOptions options)
{
    optimizeMemoryOperandsX64(ir.function);

    X64::IrLoweringX64 lowering(build, helpers, ir.function);

    return lowerImpl(build, lowering, ir.function, proto->bytecodeid, options);
}

inline bool lowerIr(A64::AssemblyBuilderA64& build, IrBuilder& ir, ModuleHelpers& helpers, Proto* proto, AssemblyOptions options)
{
    A64::IrLoweringA64 lowering(build, helpers, ir.function);

    return lowerImpl(build, lowering, ir.function, proto->bytecodeid, options);
}

template<typename AssemblyBuilder>
inline bool lowerFunction(IrBuilder& ir, AssemblyBuilder& build, ModuleHelpers& helpers, Proto* proto, AssemblyOptions options)
{
    killUnusedBlocks(ir.function);

    computeCfgInfo(ir.function);

    if (!FFlag::DebugCodegenNoOpt)
    {
        bool useValueNumbering = !FFlag::DebugCodegenSkipNumbering;

        constPropInBlockChains(ir, useValueNumbering);

        if (!FFlag::DebugCodegenOptSize)
            createLinearBlocks(ir, useValueNumbering);
    }

    return lowerIr(build, ir, helpers, proto, options);
}

} // namespace CodeGen
} // namespace Luau
