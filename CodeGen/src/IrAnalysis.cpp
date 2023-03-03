// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrAnalysis.h"

#include "Luau/DenseHash.h"
#include "Luau/IrData.h"
#include "Luau/IrUtils.h"

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

        checkOp(inst.a);
        checkOp(inst.b);
        checkOp(inst.c);
        checkOp(inst.d);
        checkOp(inst.e);
        checkOp(inst.f);
    }
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

} // namespace CodeGen
} // namespace Luau
