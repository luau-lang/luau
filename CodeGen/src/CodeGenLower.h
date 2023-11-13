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
LUAU_FASTINT(CodegenHeuristicsInstructionLimit)
LUAU_FASTINT(CodegenHeuristicsBlockLimit)
LUAU_FASTINT(CodegenHeuristicsBlockInstructionLimit)

namespace Luau
{
namespace CodeGen
{

inline void gatherFunctions(std::vector<Proto*>& results, Proto* proto, unsigned int flags)
{
    if (results.size() <= size_t(proto->bytecodeid))
        results.resize(proto->bytecodeid + 1);

    // Skip protos that we've already compiled in this run: this happens because at -O2, inlined functions get their protos reused
    if (results[proto->bytecodeid])
        return;

    // Only compile cold functions if requested
    if ((proto->flags & LPF_NATIVE_COLD) == 0 || (flags & CodeGen_ColdFunctions) != 0)
        results[proto->bytecodeid] = proto;

    // Recursively traverse child protos even if we aren't compiling this one
    for (int i = 0; i < proto->sizep; i++)
        gatherFunctions(results, proto->p[i], flags);
}

inline unsigned getInstructionCount(const std::vector<IrInst>& instructions, IrCmd cmd)
{
    return unsigned(std::count_if(instructions.begin(), instructions.end(), [&cmd](const IrInst& inst) {
        return inst.cmd == cmd;
    }));
}

template<typename AssemblyBuilder, typename IrLowering>
inline bool lowerImpl(AssemblyBuilder& build, IrLowering& lowering, IrFunction& function, const std::vector<uint32_t>& sortedBlocks, int bytecodeid,
    AssemblyOptions options)
{
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

    // Make sure entry block is first
    LUAU_ASSERT(sortedBlocks[0] == 0);

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

        // Optimizations often propagate information between blocks
        // To make sure the register and spill state is correct when blocks are lowered, we check that sorted block order matches the expected one
        if (block.expectedNextBlock != ~0u)
            LUAU_ASSERT(function.getBlockIndex(nextBlock) == block.expectedNextBlock);

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

inline bool lowerIr(X64::AssemblyBuilderX64& build, IrBuilder& ir, const std::vector<uint32_t>& sortedBlocks, ModuleHelpers& helpers, Proto* proto,
    AssemblyOptions options, LoweringStats* stats)
{
    optimizeMemoryOperandsX64(ir.function);

    X64::IrLoweringX64 lowering(build, helpers, ir.function, stats);

    return lowerImpl(build, lowering, ir.function, sortedBlocks, proto->bytecodeid, options);
}

inline bool lowerIr(A64::AssemblyBuilderA64& build, IrBuilder& ir, const std::vector<uint32_t>& sortedBlocks, ModuleHelpers& helpers, Proto* proto,
    AssemblyOptions options, LoweringStats* stats)
{
    A64::IrLoweringA64 lowering(build, helpers, ir.function, stats);

    return lowerImpl(build, lowering, ir.function, sortedBlocks, proto->bytecodeid, options);
}

template<typename AssemblyBuilder>
inline bool lowerFunction(IrBuilder& ir, AssemblyBuilder& build, ModuleHelpers& helpers, Proto* proto, AssemblyOptions options, LoweringStats* stats)
{
    helpers.bytecodeInstructionCount += unsigned(ir.function.instructions.size());

    if (helpers.bytecodeInstructionCount >= unsigned(FInt::CodegenHeuristicsInstructionLimit.value))
        return false;

    killUnusedBlocks(ir.function);

    unsigned preOptBlockCount = 0;
    unsigned maxBlockInstructions = 0;

    for (const IrBlock& block : ir.function.blocks)
    {
        preOptBlockCount += (block.kind != IrBlockKind::Dead);
        unsigned blockInstructions = block.finish - block.start;
        maxBlockInstructions = std::max(maxBlockInstructions, blockInstructions);
    };

    helpers.preOptBlockCount += preOptBlockCount;

    // we update stats before checking the heuristic so that even if we bail out
    // our stats include information about the limit that was exceeded.
    if (stats)
    {
        stats->blocksPreOpt += preOptBlockCount;
        stats->maxBlockInstructions = maxBlockInstructions;
    }

    // we use helpers.blocksPreOpt instead of stats.blocksPreOpt since
    // stats can be null across some code paths.
    if (helpers.preOptBlockCount >= unsigned(FInt::CodegenHeuristicsBlockLimit.value))
        return false;

    if (maxBlockInstructions >= unsigned(FInt::CodegenHeuristicsBlockInstructionLimit.value))
        return false;

    computeCfgInfo(ir.function);

    if (!FFlag::DebugCodegenNoOpt)
    {
        bool useValueNumbering = !FFlag::DebugCodegenSkipNumbering;

        constPropInBlockChains(ir, useValueNumbering);

        if (!FFlag::DebugCodegenOptSize)
        {
            double startTime = 0.0;
            unsigned constPropInstructionCount = 0;

            if (stats)
            {
                constPropInstructionCount = getInstructionCount(ir.function.instructions, IrCmd::SUBSTITUTE);
                startTime = lua_clock();
            }

            createLinearBlocks(ir, useValueNumbering);

            if (stats)
            {
                stats->blockLinearizationStats.timeSeconds += lua_clock() - startTime;
                constPropInstructionCount = getInstructionCount(ir.function.instructions, IrCmd::SUBSTITUTE) - constPropInstructionCount;
                stats->blockLinearizationStats.constPropInstructionCount += constPropInstructionCount;
            }
        }
    }

    std::vector<uint32_t> sortedBlocks = getSortedBlockOrder(ir.function);

    // In order to allocate registers during lowering, we need to know where instruction results are last used
    updateLastUseLocations(ir.function, sortedBlocks);

    if (stats)
    {
        for (const IrBlock& block : ir.function.blocks)
        {
            if (block.kind != IrBlockKind::Dead)
                ++stats->blocksPostOpt;
        }
    }

    return lowerIr(build, ir, sortedBlocks, helpers, proto, options, stats);
}

} // namespace CodeGen
} // namespace Luau
