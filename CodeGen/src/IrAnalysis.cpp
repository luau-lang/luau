// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrAnalysis.h"

#include "Luau/DenseHash.h"
#include "Luau/IrData.h"
#include "Luau/IrUtils.h"

#include "lobject.h"

#include <bitset>

#include <stddef.h>

namespace Luau
{
namespace CodeGen
{

void updateUseCounts(IrFunction& function)
{
    std::vector<IrBlock>& blocks = function.blocks;
    std::vector<IrInst>& instructions = function.instructions;

    for (IrBlock& block : blocks)
        block.useCount = 0;

    for (IrInst& inst : instructions)
        inst.useCount = 0;

    auto checkOp = [&](IrOp op) {
        if (op.kind == IrOpKind::Inst)
        {
            IrInst& target = instructions[op.index];
            LUAU_ASSERT(target.useCount < 0xffff);
            target.useCount++;
        }
        else if (op.kind == IrOpKind::Block)
        {
            IrBlock& target = blocks[op.index];
            LUAU_ASSERT(target.useCount < 0xffff);
            target.useCount++;
        }
    };

    for (IrInst& inst : instructions)
    {
        checkOp(inst.a);
        checkOp(inst.b);
        checkOp(inst.c);
        checkOp(inst.d);
        checkOp(inst.e);
        checkOp(inst.f);
    }
}

void updateLastUseLocations(IrFunction& function)
{
    std::vector<IrInst>& instructions = function.instructions;

    for (IrInst& inst : instructions)
        inst.lastUse = 0;

    for (size_t instIdx = 0; instIdx < instructions.size(); ++instIdx)
    {
        IrInst& inst = instructions[instIdx];

        auto checkOp = [&](IrOp op) {
            if (op.kind == IrOpKind::Inst)
                instructions[op.index].lastUse = uint32_t(instIdx);
        };

        if (isPseudo(inst.cmd))
            continue;

        checkOp(inst.a);
        checkOp(inst.b);
        checkOp(inst.c);
        checkOp(inst.d);
        checkOp(inst.e);
        checkOp(inst.f);
    }
}

uint32_t getNextInstUse(IrFunction& function, uint32_t targetInstIdx, uint32_t startInstIdx)
{
    LUAU_ASSERT(startInstIdx < function.instructions.size());
    IrInst& targetInst = function.instructions[targetInstIdx];

    for (uint32_t i = startInstIdx; i <= targetInst.lastUse; i++)
    {
        IrInst& inst = function.instructions[i];

        if (isPseudo(inst.cmd))
            continue;

        if (inst.a.kind == IrOpKind::Inst && inst.a.index == targetInstIdx)
            return i;

        if (inst.b.kind == IrOpKind::Inst && inst.b.index == targetInstIdx)
            return i;

        if (inst.c.kind == IrOpKind::Inst && inst.c.index == targetInstIdx)
            return i;

        if (inst.d.kind == IrOpKind::Inst && inst.d.index == targetInstIdx)
            return i;

        if (inst.e.kind == IrOpKind::Inst && inst.e.index == targetInstIdx)
            return i;

        if (inst.f.kind == IrOpKind::Inst && inst.f.index == targetInstIdx)
            return i;
    }

    // There must be a next use since there is the last use location
    LUAU_ASSERT(!"failed to find next use");
    return targetInst.lastUse;
}

std::pair<uint32_t, uint32_t> getLiveInOutValueCount(IrFunction& function, IrBlock& block)
{
    uint32_t liveIns = 0;
    uint32_t liveOuts = 0;

    auto checkOp = [&](IrOp op) {
        if (op.kind == IrOpKind::Inst)
        {
            if (op.index >= block.start && op.index <= block.finish)
                liveOuts--;
            else
                liveIns++;
        }
    };

    for (uint32_t instIdx = block.start; instIdx <= block.finish; instIdx++)
    {
        IrInst& inst = function.instructions[instIdx];

        if (isPseudo(inst.cmd))
            continue;

        liveOuts += inst.useCount;

        checkOp(inst.a);
        checkOp(inst.b);
        checkOp(inst.c);
        checkOp(inst.d);
        checkOp(inst.e);
        checkOp(inst.f);
    }

    return std::make_pair(liveIns, liveOuts);
}

uint32_t getLiveInValueCount(IrFunction& function, IrBlock& block)
{
    return getLiveInOutValueCount(function, block).first;
}

uint32_t getLiveOutValueCount(IrFunction& function, IrBlock& block)
{
    return getLiveInOutValueCount(function, block).second;
}

static void requireVariadicSequence(RegisterSet& sourceRs, const RegisterSet& defRs, uint8_t varargStart)
{
    if (!defRs.varargSeq)
    {
        // Peel away registers from variadic sequence that we define
        while (defRs.regs.test(varargStart))
            varargStart++;

        LUAU_ASSERT(!sourceRs.varargSeq || sourceRs.varargStart == varargStart);

        sourceRs.varargSeq = true;
        sourceRs.varargStart = varargStart;
    }
    else
    {
        // Variadic use sequence might include registers before def sequence
        for (int i = varargStart; i < defRs.varargStart; i++)
        {
            if (!defRs.regs.test(i))
                sourceRs.regs.set(i);
        }
    }
}

static RegisterSet computeBlockLiveInRegSet(IrFunction& function, const IrBlock& block, RegisterSet& defRs, std::bitset<256>& capturedRegs)
{
    RegisterSet inRs;

    auto def = [&](IrOp op, int offset = 0) {
        defRs.regs.set(vmRegOp(op) + offset, true);
    };

    auto use = [&](IrOp op, int offset = 0) {
        if (!defRs.regs.test(vmRegOp(op) + offset))
            inRs.regs.set(vmRegOp(op) + offset, true);
    };

    auto maybeDef = [&](IrOp op) {
        if (op.kind == IrOpKind::VmReg)
            defRs.regs.set(vmRegOp(op), true);
    };

    auto maybeUse = [&](IrOp op) {
        if (op.kind == IrOpKind::VmReg)
        {
            if (!defRs.regs.test(vmRegOp(op)))
                inRs.regs.set(vmRegOp(op), true);
        }
    };

    auto defVarargs = [&](uint8_t varargStart) {
        defRs.varargSeq = true;
        defRs.varargStart = varargStart;
    };

    auto useVarargs = [&](uint8_t varargStart) {
        requireVariadicSequence(inRs, defRs, varargStart);

        // Variadic sequence has been consumed
        defRs.varargSeq = false;
        defRs.varargStart = 0;
    };

    auto defRange = [&](int start, int count) {
        if (count == -1)
        {
            defVarargs(start);
        }
        else
        {
            for (int i = start; i < start + count; i++)
                defRs.regs.set(i, true);
        }
    };

    auto useRange = [&](int start, int count) {
        if (count == -1)
        {
            useVarargs(start);
        }
        else
        {
            for (int i = start; i < start + count; i++)
            {
                if (!defRs.regs.test(i))
                    inRs.regs.set(i, true);
            }
        }
    };

    for (uint32_t instIdx = block.start; instIdx <= block.finish; instIdx++)
    {
        const IrInst& inst = function.instructions[instIdx];

        // For correct analysis, all instruction uses must be handled before handling the definitions
        switch (inst.cmd)
        {
        case IrCmd::LOAD_TAG:
        case IrCmd::LOAD_POINTER:
        case IrCmd::LOAD_DOUBLE:
        case IrCmd::LOAD_INT:
        case IrCmd::LOAD_TVALUE:
            maybeUse(inst.a); // Argument can also be a VmConst
            break;
        case IrCmd::STORE_TAG:
        case IrCmd::STORE_POINTER:
        case IrCmd::STORE_DOUBLE:
        case IrCmd::STORE_INT:
        case IrCmd::STORE_VECTOR:
        case IrCmd::STORE_TVALUE:
            maybeDef(inst.a); // Argument can also be a pointer value
            break;
        case IrCmd::JUMP_IF_TRUTHY:
        case IrCmd::JUMP_IF_FALSY:
            use(inst.a);
            break;
        case IrCmd::JUMP_CMP_ANY:
            use(inst.a);
            use(inst.b);
            break;
            // A <- B, C
        case IrCmd::DO_ARITH:
        case IrCmd::GET_TABLE:
            use(inst.b);
            maybeUse(inst.c); // Argument can also be a VmConst

            def(inst.a);
            break;
        case IrCmd::SET_TABLE:
            use(inst.a);
            use(inst.b);
            maybeUse(inst.c); // Argument can also be a VmConst
            break;
            // A <- B
        case IrCmd::DO_LEN:
            use(inst.b);

            def(inst.a);
            break;
        case IrCmd::GET_IMPORT:
            def(inst.a);
            break;
        case IrCmd::CONCAT:
            useRange(vmRegOp(inst.a), function.uintOp(inst.b));

            defRange(vmRegOp(inst.a), function.uintOp(inst.b));
            break;
        case IrCmd::GET_UPVALUE:
            def(inst.a);
            break;
        case IrCmd::SET_UPVALUE:
            use(inst.b);
            break;
        case IrCmd::PREPARE_FORN:
            use(inst.a);
            use(inst.b);
            use(inst.c);

            def(inst.a);
            def(inst.b);
            def(inst.c);
            break;
        case IrCmd::INTERRUPT:
            break;
        case IrCmd::BARRIER_OBJ:
        case IrCmd::BARRIER_TABLE_FORWARD:
            use(inst.b);
            break;
        case IrCmd::CLOSE_UPVALS:
            // Closing an upvalue should be counted as a register use (it copies the fresh register value)
            // But we lack the required information about the specific set of registers that are affected
            // Because we don't plan to optimize captured registers atm, we skip full dataflow analysis for them right now
            break;
        case IrCmd::CAPTURE:
            maybeUse(inst.a);

            if (function.boolOp(inst.b))
                capturedRegs.set(vmRegOp(inst.a), true);
            break;
        case IrCmd::SETLIST:
            use(inst.b);
            useRange(vmRegOp(inst.c), function.intOp(inst.d));
            break;
        case IrCmd::CALL:
            use(inst.a);
            useRange(vmRegOp(inst.a) + 1, function.intOp(inst.b));

            defRange(vmRegOp(inst.a), function.intOp(inst.c));
            break;
        case IrCmd::RETURN:
            useRange(vmRegOp(inst.a), function.intOp(inst.b));
            break;
        case IrCmd::FASTCALL:
        case IrCmd::INVOKE_FASTCALL:
            if (int count = function.intOp(inst.e); count != -1)
            {
                if (count >= 3)
                {
                    LUAU_ASSERT(inst.d.kind == IrOpKind::VmReg && vmRegOp(inst.d) == vmRegOp(inst.c) + 1);

                    useRange(vmRegOp(inst.c), count);
                }
                else
                {
                    if (count >= 1)
                        use(inst.c);

                    if (count >= 2)
                        maybeUse(inst.d); // Argument can also be a VmConst
                }
            }
            else
            {
                useVarargs(vmRegOp(inst.c));
            }

            // Multiple return sequences (count == -1) are defined by ADJUST_STACK_TO_REG
            if (int count = function.intOp(inst.f); count != -1)
                defRange(vmRegOp(inst.b), count);
            break;
        case IrCmd::FORGLOOP:
            // First register is not used by instruction, we check that it's still 'nil' with CHECK_TAG
            use(inst.a, 1);
            use(inst.a, 2);

            def(inst.a, 2);
            defRange(vmRegOp(inst.a) + 3, function.intOp(inst.b));
            break;
        case IrCmd::FORGLOOP_FALLBACK:
            useRange(vmRegOp(inst.a), 3);

            def(inst.a, 2);
            defRange(vmRegOp(inst.a) + 3, uint8_t(function.intOp(inst.b))); // ignore most significant bit
            break;
        case IrCmd::FORGPREP_XNEXT_FALLBACK:
            use(inst.b);
            break;
        case IrCmd::FALLBACK_GETGLOBAL:
            def(inst.b);
            break;
        case IrCmd::FALLBACK_SETGLOBAL:
            use(inst.b);
            break;
        case IrCmd::FALLBACK_GETTABLEKS:
            use(inst.c);

            def(inst.b);
            break;
        case IrCmd::FALLBACK_SETTABLEKS:
            use(inst.b);
            use(inst.c);
            break;
        case IrCmd::FALLBACK_NAMECALL:
            use(inst.c);

            defRange(vmRegOp(inst.b), 2);
            break;
        case IrCmd::FALLBACK_PREPVARARGS:
            // No effect on explicitly referenced registers
            break;
        case IrCmd::FALLBACK_GETVARARGS:
            defRange(vmRegOp(inst.b), function.intOp(inst.c));
            break;
        case IrCmd::FALLBACK_NEWCLOSURE:
            def(inst.b);
            break;
        case IrCmd::FALLBACK_DUPCLOSURE:
            def(inst.b);
            break;
        case IrCmd::FALLBACK_FORGPREP:
            use(inst.b);

            defRange(vmRegOp(inst.b), 3);
            break;
        case IrCmd::ADJUST_STACK_TO_REG:
            defRange(vmRegOp(inst.a), -1);
            break;
        case IrCmd::ADJUST_STACK_TO_TOP:
            // While this can be considered to be a vararg consumer, it is already handled in fastcall instructions
            break;

        default:
            // All instructions which reference registers have to be handled explicitly
            LUAU_ASSERT(inst.a.kind != IrOpKind::VmReg);
            LUAU_ASSERT(inst.b.kind != IrOpKind::VmReg);
            LUAU_ASSERT(inst.c.kind != IrOpKind::VmReg);
            LUAU_ASSERT(inst.d.kind != IrOpKind::VmReg);
            LUAU_ASSERT(inst.e.kind != IrOpKind::VmReg);
            LUAU_ASSERT(inst.f.kind != IrOpKind::VmReg);
            break;
        }
    }

    return inRs;
}

// The algorithm used here is commonly known as backwards data-flow analysis.
// For each block, we track 'upward-exposed' (live-in) uses of registers - a use of a register that hasn't been defined in the block yet.
// We also track the set of registers that were defined in the block.
// When initial live-in sets of registers are computed, propagation of those uses upwards through predecessors is performed.
// If predecessor doesn't define the register, we have to add it to the live-in set.
// Extending the set of live-in registers of a block requires re-checking of that block.
// Propagation runs iteratively, using a worklist of blocks to visit until a fixed point is reached.
// This algorithm can be easily extended to cover phi instructions, but we don't use those yet.
static void computeCfgLiveInOutRegSets(IrFunction& function)
{
    CfgInfo& info = function.cfg;

    // Clear existing data
    // 'in' and 'captured' data is not cleared because it will be overwritten below
    info.def.clear();
    info.out.clear();

    // Try to compute Luau VM register use-def info
    info.in.resize(function.blocks.size());
    info.def.resize(function.blocks.size());
    info.out.resize(function.blocks.size());

    // Captured registers are tracked for the whole function
    // It should be possible to have a more precise analysis for them in the future
    std::bitset<256> capturedRegs;

    // First we compute live-in set of each block
    for (size_t blockIdx = 0; blockIdx < function.blocks.size(); blockIdx++)
    {
        const IrBlock& block = function.blocks[blockIdx];

        if (block.kind == IrBlockKind::Dead)
            continue;

        info.in[blockIdx] = computeBlockLiveInRegSet(function, block, info.def[blockIdx], capturedRegs);
    }

    info.captured.regs = capturedRegs;

    // With live-in sets ready, we can arrive at a fixed point for both in/out registers by requesting required registers from predecessors
    std::vector<uint32_t> worklist;

    std::vector<uint8_t> inWorklist;
    inWorklist.resize(function.blocks.size(), false);

    // We will have to visit each block at least once, so we add all of them to the worklist immediately
    for (size_t blockIdx = 0; blockIdx < function.blocks.size(); blockIdx++)
    {
        const IrBlock& block = function.blocks[blockIdx];

        if (block.kind == IrBlockKind::Dead)
            continue;

        worklist.push_back(uint32_t(blockIdx));
        inWorklist[blockIdx] = true;
    }

    while (!worklist.empty())
    {
        uint32_t blockIdx = worklist.back();
        worklist.pop_back();
        inWorklist[blockIdx] = false;

        IrBlock& curr = function.blocks[blockIdx];
        RegisterSet& inRs = info.in[blockIdx];
        RegisterSet& defRs = info.def[blockIdx];
        RegisterSet& outRs = info.out[blockIdx];

        // Current block has to provide all registers in successor blocks
        for (uint32_t succIdx : successors(info, blockIdx))
        {
            IrBlock& succ = function.blocks[succIdx];

            // This is a step away from the usual definition of live range flow through CFG
            // Exit from a regular block to a fallback block is not considered a block terminator
            // This is because fallback blocks define an alternative implementation of the same operations
            // This can cause the current block to define more registers that actually were available at fallback entry
            if (curr.kind != IrBlockKind::Fallback && succ.kind == IrBlockKind::Fallback)
                continue;

            const RegisterSet& succRs = info.in[succIdx];

            outRs.regs |= succRs.regs;

            if (succRs.varargSeq)
            {
                LUAU_ASSERT(!outRs.varargSeq || outRs.varargStart == succRs.varargStart);

                outRs.varargSeq = true;
                outRs.varargStart = succRs.varargStart;
            }
        }

        RegisterSet oldInRs = inRs;

        // If current block didn't define a live-out, it has to be live-in
        inRs.regs |= outRs.regs & ~defRs.regs;

        if (outRs.varargSeq)
            requireVariadicSequence(inRs, defRs, outRs.varargStart);

        // If we have new live-ins, we have to notify all predecessors
        // We don't allow changes to the start of the variadic sequence, so we skip checking that member
        if (inRs.regs != oldInRs.regs || inRs.varargSeq != oldInRs.varargSeq)
        {
            for (uint32_t predIdx : predecessors(info, blockIdx))
            {
                if (!inWorklist[predIdx])
                {
                    worklist.push_back(predIdx);
                    inWorklist[predIdx] = true;
                }
            }
        }
    }

    // If Proto data is available, validate that entry block arguments match required registers
    if (function.proto)
    {
        RegisterSet& entryIn = info.in[0];

        LUAU_ASSERT(!entryIn.varargSeq);

        for (size_t i = 0; i < entryIn.regs.size(); i++)
            LUAU_ASSERT(!entryIn.regs.test(i) || i < function.proto->numparams);
    }
}

static void computeCfgBlockEdges(IrFunction& function)
{
    CfgInfo& info = function.cfg;

    // Clear existing data
    info.predecessorsOffsets.clear();
    info.successorsOffsets.clear();

    // Compute predecessors block edges
    info.predecessorsOffsets.reserve(function.blocks.size());
    info.successorsOffsets.reserve(function.blocks.size());

    int edgeCount = 0;

    for (const IrBlock& block : function.blocks)
    {
        info.predecessorsOffsets.push_back(edgeCount);
        edgeCount += block.useCount;
    }

    info.predecessors.resize(edgeCount);
    info.successors.resize(edgeCount);

    edgeCount = 0;

    for (size_t blockIdx = 0; blockIdx < function.blocks.size(); blockIdx++)
    {
        const IrBlock& block = function.blocks[blockIdx];

        info.successorsOffsets.push_back(edgeCount);

        if (block.kind == IrBlockKind::Dead)
            continue;

        for (uint32_t instIdx = block.start; instIdx <= block.finish; instIdx++)
        {
            const IrInst& inst = function.instructions[instIdx];

            auto checkOp = [&](IrOp op) {
                if (op.kind == IrOpKind::Block)
                {
                    // We use a trick here, where we use the starting offset of the predecessor list as the position where to write next predecessor
                    // The values will be adjusted back in a separate loop later
                    info.predecessors[info.predecessorsOffsets[op.index]++] = uint32_t(blockIdx);

                    info.successors[edgeCount++] = op.index;
                }
            };

            checkOp(inst.a);
            checkOp(inst.b);
            checkOp(inst.c);
            checkOp(inst.d);
            checkOp(inst.e);
            checkOp(inst.f);
        }
    }

    // Offsets into the predecessor list were used as iterators in the previous loop
    // To adjust them back, block use count is subtracted (predecessor count is equal to how many uses block has)
    for (size_t blockIdx = 0; blockIdx < function.blocks.size(); blockIdx++)
    {
        const IrBlock& block = function.blocks[blockIdx];

        info.predecessorsOffsets[blockIdx] -= block.useCount;
    }
}

void computeCfgInfo(IrFunction& function)
{
    computeCfgBlockEdges(function);
    computeCfgLiveInOutRegSets(function);
}

BlockIteratorWrapper predecessors(const CfgInfo& cfg, uint32_t blockIdx)
{
    LUAU_ASSERT(blockIdx < cfg.predecessorsOffsets.size());

    uint32_t start = cfg.predecessorsOffsets[blockIdx];
    uint32_t end = blockIdx + 1 < cfg.predecessorsOffsets.size() ? cfg.predecessorsOffsets[blockIdx + 1] : uint32_t(cfg.predecessors.size());

    return BlockIteratorWrapper{cfg.predecessors.data() + start, cfg.predecessors.data() + end};
}

BlockIteratorWrapper successors(const CfgInfo& cfg, uint32_t blockIdx)
{
    LUAU_ASSERT(blockIdx < cfg.successorsOffsets.size());

    uint32_t start = cfg.successorsOffsets[blockIdx];
    uint32_t end = blockIdx + 1 < cfg.successorsOffsets.size() ? cfg.successorsOffsets[blockIdx + 1] : uint32_t(cfg.successors.size());

    return BlockIteratorWrapper{cfg.successors.data() + start, cfg.successors.data() + end};
}

} // namespace CodeGen
} // namespace Luau
