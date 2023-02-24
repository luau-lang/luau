// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrUtils.h"

#include "Luau/IrBuilder.h"

#include "lua.h"
#include "lnumutils.h"

#include <limits.h>
#include <math.h>

namespace Luau
{
namespace CodeGen
{

void addUse(IrFunction& function, IrOp op)
{
    if (op.kind == IrOpKind::Inst)
        function.instructions[op.index].useCount++;
    else if (op.kind == IrOpKind::Block)
        function.blocks[op.index].useCount++;
}

void removeUse(IrFunction& function, IrOp op)
{
    if (op.kind == IrOpKind::Inst)
        removeUse(function, function.instructions[op.index]);
    else if (op.kind == IrOpKind::Block)
        removeUse(function, function.blocks[op.index]);
}

bool isGCO(uint8_t tag)
{
    // mirrors iscollectable(o) from VM/lobject.h
    return tag >= LUA_TSTRING;
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
    removeUse(function, inst.f);

    inst.a = {};
    inst.b = {};
    inst.c = {};
    inst.d = {};
    inst.e = {};
    inst.f = {};
}

void kill(IrFunction& function, uint32_t start, uint32_t end)
{
    // Kill instructions in reverse order to avoid killing instructions that are still marked as used
    for (int i = int(end); i >= int(start); i--)
    {
        LUAU_ASSERT(unsigned(i) < function.instructions.size());
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

    kill(function, block.start, block.finish);
    block.start = ~0u;
    block.finish = ~0u;
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

void replace(IrFunction& function, IrBlock& block, uint32_t instIdx, IrInst replacement)
{
    IrInst& inst = function.instructions[instIdx];

    // Add uses before removing new ones if those are the last ones keeping target operand alive
    addUse(function, replacement.a);
    addUse(function, replacement.b);
    addUse(function, replacement.c);
    addUse(function, replacement.d);
    addUse(function, replacement.e);
    addUse(function, replacement.f);

    // If we introduced an earlier terminating instruction, all following instructions become dead
    if (!isBlockTerminator(inst.cmd) && isBlockTerminator(replacement.cmd))
    {
        // Block has has to be fully constructed before replacement is performed
        LUAU_ASSERT(block.finish != ~0u);
        LUAU_ASSERT(instIdx + 1 <= block.finish);

        kill(function, instIdx + 1, block.finish);

        block.finish = instIdx;
    }

    removeUse(function, inst.a);
    removeUse(function, inst.b);
    removeUse(function, inst.c);
    removeUse(function, inst.d);
    removeUse(function, inst.e);
    removeUse(function, inst.f);

    inst = replacement;
}

void substitute(IrFunction& function, IrInst& inst, IrOp replacement)
{
    LUAU_ASSERT(!isBlockTerminator(inst.cmd));

    inst.cmd = IrCmd::SUBSTITUTE;

    removeUse(function, inst.a);
    removeUse(function, inst.b);
    removeUse(function, inst.c);
    removeUse(function, inst.d);
    removeUse(function, inst.e);
    removeUse(function, inst.f);

    inst.a = replacement;
    inst.b = {};
    inst.c = {};
    inst.d = {};
    inst.e = {};
    inst.f = {};
}

void applySubstitutions(IrFunction& function, IrOp& op)
{
    if (op.kind == IrOpKind::Inst)
    {
        IrInst& src = function.instructions[op.index];

        if (src.cmd == IrCmd::SUBSTITUTE)
        {
            op.kind = src.a.kind;
            op.index = src.a.index;

            // If we substitute with the result of a different instruction, update the use count
            if (op.kind == IrOpKind::Inst)
            {
                IrInst& dst = function.instructions[op.index];
                LUAU_ASSERT(dst.cmd != IrCmd::SUBSTITUTE && "chained substitutions are not allowed");

                dst.useCount++;
            }

            LUAU_ASSERT(src.useCount > 0);
            src.useCount--;
        }
    }
}

void applySubstitutions(IrFunction& function, IrInst& inst)
{
    applySubstitutions(function, inst.a);
    applySubstitutions(function, inst.b);
    applySubstitutions(function, inst.c);
    applySubstitutions(function, inst.d);
    applySubstitutions(function, inst.e);
    applySubstitutions(function, inst.f);
}

bool compare(double a, double b, IrCondition cond)
{
    switch (cond)
    {
    case IrCondition::Equal:
        return a == b;
    case IrCondition::NotEqual:
        return a != b;
    case IrCondition::Less:
        return a < b;
    case IrCondition::NotLess:
        return !(a < b);
    case IrCondition::LessEqual:
        return a <= b;
    case IrCondition::NotLessEqual:
        return !(a <= b);
    case IrCondition::Greater:
        return a > b;
    case IrCondition::NotGreater:
        return !(a > b);
    case IrCondition::GreaterEqual:
        return a >= b;
    case IrCondition::NotGreaterEqual:
        return !(a >= b);
    default:
        LUAU_ASSERT(!"unsupported conidtion");
    }

    return false;
}

void foldConstants(IrBuilder& build, IrFunction& function, IrBlock& block, uint32_t index)
{
    IrInst& inst = function.instructions[index];

    switch (inst.cmd)
    {
    case IrCmd::ADD_INT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            // We need to avoid signed integer overflow, but we also have to produce a result
            // So we add numbers as unsigned and use fixed-width integer types to force a two's complement evaluation
            int32_t lhs = function.intOp(inst.a);
            int32_t rhs = function.intOp(inst.b);
            int sum = int32_t(uint32_t(lhs) + uint32_t(rhs));

            substitute(function, inst, build.constInt(sum));
        }
        break;
    case IrCmd::SUB_INT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            // We need to avoid signed integer overflow, but we also have to produce a result
            // So we subtract numbers as unsigned and use fixed-width integer types to force a two's complement evaluation
            int32_t lhs = function.intOp(inst.a);
            int32_t rhs = function.intOp(inst.b);
            int sum = int32_t(uint32_t(lhs) - uint32_t(rhs));

            substitute(function, inst, build.constInt(sum));
        }
        break;
    case IrCmd::ADD_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(function.doubleOp(inst.a) + function.doubleOp(inst.b)));
        break;
    case IrCmd::SUB_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(function.doubleOp(inst.a) - function.doubleOp(inst.b)));
        break;
    case IrCmd::MUL_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(function.doubleOp(inst.a) * function.doubleOp(inst.b)));
        break;
    case IrCmd::DIV_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(function.doubleOp(inst.a) / function.doubleOp(inst.b)));
        break;
    case IrCmd::MOD_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(luai_nummod(function.doubleOp(inst.a), function.doubleOp(inst.b))));
        break;
    case IrCmd::POW_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(pow(function.doubleOp(inst.a), function.doubleOp(inst.b))));
        break;
    case IrCmd::UNM_NUM:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(-function.doubleOp(inst.a)));
        break;
    case IrCmd::NOT_ANY:
        if (inst.a.kind == IrOpKind::Constant)
        {
            uint8_t a = function.tagOp(inst.a);

            if (a == LUA_TNIL)
                substitute(function, inst, build.constInt(1));
            else if (a != LUA_TBOOLEAN)
                substitute(function, inst, build.constInt(0));
            else if (inst.b.kind == IrOpKind::Constant)
                substitute(function, inst, build.constInt(function.intOp(inst.b) == 1 ? 0 : 1));
        }
        break;
    case IrCmd::JUMP_EQ_TAG:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            if (function.tagOp(inst.a) == function.tagOp(inst.b))
                replace(function, block, index, {IrCmd::JUMP, inst.c});
            else
                replace(function, block, index, {IrCmd::JUMP, inst.d});
        }
        break;
    case IrCmd::JUMP_EQ_INT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            if (function.intOp(inst.a) == function.intOp(inst.b))
                replace(function, block, index, {IrCmd::JUMP, inst.c});
            else
                replace(function, block, index, {IrCmd::JUMP, inst.d});
        }
        break;
    case IrCmd::JUMP_CMP_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            if (compare(function.doubleOp(inst.a), function.doubleOp(inst.b), function.conditionOp(inst.c)))
                replace(function, block, index, {IrCmd::JUMP, inst.d});
            else
                replace(function, block, index, {IrCmd::JUMP, inst.e});
        }
        break;
    case IrCmd::NUM_TO_INDEX:
        if (inst.a.kind == IrOpKind::Constant)
        {
            double value = function.doubleOp(inst.a);

            // To avoid undefined behavior of casting a value not representable in the target type, we check the range
            if (value >= INT_MIN && value <= INT_MAX)
            {
                int arrIndex = int(value);

                if (double(arrIndex) == value)
                    substitute(function, inst, build.constInt(arrIndex));
                else
                    replace(function, block, index, {IrCmd::JUMP, inst.b});
            }
            else
            {
                replace(function, block, index, {IrCmd::JUMP, inst.b});
            }
        }
        break;
    case IrCmd::INT_TO_NUM:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(double(function.intOp(inst.a))));
        break;
    case IrCmd::CHECK_TAG:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            if (function.tagOp(inst.a) == function.tagOp(inst.b))
                kill(function, inst);
            else
                replace(function, block, index, {IrCmd::JUMP, inst.c}); // Shows a conflict in assumptions on this path
        }
        break;
    default:
        break;
    }
}

} // namespace CodeGen
} // namespace Luau
