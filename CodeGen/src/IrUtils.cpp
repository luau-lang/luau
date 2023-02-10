// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrUtils.h"

namespace Luau
{
namespace CodeGen
{

static uint32_t getBlockEnd(IrFunction& function, uint32_t start)
{
    uint32_t end = start;

    // Find previous block terminator
    while (!isBlockTerminator(function.instructions[end].cmd))
        end++;

    return end;
}

static void addUse(IrFunction& function, IrOp op)
{
    if (op.kind == IrOpKind::Inst)
        function.instructions[op.index].useCount++;
    else if (op.kind == IrOpKind::Block)
        function.blocks[op.index].useCount++;
}

static void removeUse(IrFunction& function, IrOp op)
{
    if (op.kind == IrOpKind::Inst)
        removeUse(function, function.instructions[op.index]);
    else if (op.kind == IrOpKind::Block)
        removeUse(function, function.blocks[op.index]);
}

void kill(IrFunction& function, IrInst& inst)
{
    LUAU_ASSERT(inst.useCount == 0);

    inst.cmd = IrCmd::NOP;

    removeUse(function, inst.a);
    removeUse(function, inst.b);
    removeUse(function, inst.c);
    removeUse(function, inst.d);
    removeUse(function, inst.e);
}

void kill(IrFunction& function, uint32_t start, uint32_t end)
{
    // Kill instructions in reverse order to avoid killing instructions that are still marked as used
    for (int i = int(end); i >= int(start); i--)
    {
        IrInst& curr = function.instructions[i];

        if (curr.cmd == IrCmd::NOP)
            continue;

        kill(function, curr);
    }
}

void kill(IrFunction& function, IrBlock& block)
{
    LUAU_ASSERT(block.useCount == 0);

    block.kind = IrBlockKind::Dead;

    uint32_t start = block.start;
    uint32_t end = getBlockEnd(function, start);

    kill(function, start, end);
}

void removeUse(IrFunction& function, IrInst& inst)
{
    LUAU_ASSERT(inst.useCount);
    inst.useCount--;

    if (inst.useCount == 0)
        kill(function, inst);
}

void removeUse(IrFunction& function, IrBlock& block)
{
    LUAU_ASSERT(block.useCount);
    block.useCount--;

    if (block.useCount == 0)
        kill(function, block);
}

void replace(IrFunction& function, IrOp& original, IrOp replacement)
{
    // Add use before removing new one if that's the last one keeping target operand alive
    addUse(function, replacement);
    removeUse(function, original);

    original = replacement;
}

void replace(IrFunction& function, uint32_t instIdx, IrInst replacement)
{
    IrInst& inst = function.instructions[instIdx];
    IrCmd prevCmd = inst.cmd;

    // Add uses before removing new ones if those are the last ones keeping target operand alive
    addUse(function, replacement.a);
    addUse(function, replacement.b);
    addUse(function, replacement.c);
    addUse(function, replacement.d);
    addUse(function, replacement.e);

    removeUse(function, inst.a);
    removeUse(function, inst.b);
    removeUse(function, inst.c);
    removeUse(function, inst.d);
    removeUse(function, inst.e);

    inst = replacement;

    // If we introduced an earlier terminating instruction, all following instructions become dead
    if (!isBlockTerminator(prevCmd) && isBlockTerminator(inst.cmd))
    {
        uint32_t start = instIdx + 1;
        uint32_t end = getBlockEnd(function, start);

        kill(function, start, end);
    }
}

} // namespace CodeGen
} // namespace Luau
