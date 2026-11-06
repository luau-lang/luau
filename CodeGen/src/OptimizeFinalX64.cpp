// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/OptimizeFinalX64.h"

#include "Luau/IrUtils.h"
#include "Luau/IrVisitUseDef.h"

#include <utility>

LUAU_FASTFLAGVARIABLE(LuauCodegenX64ConflictCheck)
LUAU_FLAGVERSION(LuauCodegenX64ConflictCheck, 2)

// x64 assembly allows memory operands, but IR separates loads from uses
// To improve final x64 lowering, we try to 'inline' single-use register/constant loads into some of our instructions
// This pass might not be useful on different architectures

namespace Luau
{
namespace CodeGen
{

static constexpr uint32_t kMaxInlineDistance = 16;

struct DefinitionVisitor
{
    explicit DefinitionVisitor(int targetReg)
        : targetReg(targetReg)
    {
    }

    void def(IrOp op, int offset = 0)
    {
        matched |= (vmRegOp(op) + offset) == targetReg;
    }

    void maybeDef(IrOp op)
    {
        if (op.kind == IrOpKind::VmReg)
            def(op, 0);
    }

    void defRange(int start, int count)
    {
        matched |= targetReg >= start && (count == -1 || targetReg < start + count);
    }

    // Required for a full visitor interface
    void use(IrOp op, int offset = 0) {}
    void maybeUse(IrOp op) {}
    void useVarargs(uint8_t varargStart) {}
    void useRange(int start, int count) {}
    void capture(int reg) {}

    int targetReg = 0;
    bool matched = false;
};

static bool hasDefinitionInRange(IrFunction& function, int targetReg, uint32_t start, uint32_t end)
{
    DefinitionVisitor visitor{targetReg};

    for (uint32_t instIdx = start; instIdx < end && !visitor.matched; instIdx++)
        visitVmRegDefsUses(visitor, function, function.instructions[instIdx]);

    return visitor.matched;
}

// Check that the operand value will be the same as it was at the load location
static bool canInlineOperand(IrFunction& function, uint32_t sourceInstIdx, IrBlock& useBlock, uint32_t useInstIdx, IrOp op)
{
    if (op.kind != IrOpKind::VmReg)
        return true;

    // Simple check if the definition is inside our block
    if (sourceInstIdx >= useBlock.start && sourceInstIdx < useInstIdx)
    {
        if (useInstIdx - sourceInstIdx > kMaxInlineDistance)
            return false;

        return !hasDefinitionInRange(function, vmRegOp(op), sourceInstIdx + 1, useInstIdx);
    }

    // Check source values coming from a previous block if there's a 1:1 relationship
    uint32_t useBlockIdx = function.getBlockIndex(useBlock);
    if (useBlockIdx >= function.cfg.predecessorsOffsets.size())
        return false;

    BlockIteratorWrapper preds = predecessors(function.cfg, useBlockIdx);
    if (preds.size() != 1)
        return false;

    IrBlock& predBlock = function.blocks[*preds.begin()];
    if (sourceInstIdx < predBlock.start || sourceInstIdx > predBlock.finish)
        return false;

    if ((predBlock.finish - sourceInstIdx) + (useInstIdx - useBlock.start) > kMaxInlineDistance)
        return false;

    return !hasDefinitionInRange(function, vmRegOp(op), sourceInstIdx + 1, predBlock.finish + 1) &&
           !hasDefinitionInRange(function, vmRegOp(op), useBlock.start, useInstIdx);
}

static void replaceIfNoConflicts(IrFunction& function, IrInst& source, IrBlock& useBlock, uint32_t useInstIdx, IrOp& original, IrOp replacement)
{
    if (canInlineOperand(function, function.getInstIndex(source), useBlock, useInstIdx, replacement))
        replace(function, original, replacement);
}

static void optimizeMemoryOperandsX64(IrFunction& function, IrBlock& block)
{
    CODEGEN_ASSERT(block.kind != IrBlockKind::Dead);

    for (uint32_t index = block.start; index <= block.finish; index++)
    {
        CODEGEN_ASSERT(index < function.instructions.size());
        IrInst& inst = function.instructions[index];

        switch (inst.cmd)
        {
        case IrCmd::CHECK_TAG:
        {
            if (OP_A(inst).kind == IrOpKind::Inst)
            {
                IrInst& tag = function.instOp(OP_A(inst));

                if (tag.useCount == 1 && tag.cmd == IrCmd::LOAD_TAG && (OP_A(tag).kind == IrOpKind::VmReg || OP_A(tag).kind == IrOpKind::VmConst))
                {
                    if (FFlag::LuauCodegenX64ConflictCheck)
                        replaceIfNoConflicts(function, tag, block, index, OP_A(inst), OP_A(tag));
                    else
                        replace(function, OP_A(inst), OP_A(tag));
                }
            }
            break;
        }
        case IrCmd::CHECK_TRUTHY:
        {
            if (OP_A(inst).kind == IrOpKind::Inst)
            {
                IrInst& tag = function.instOp(OP_A(inst));

                if (tag.useCount == 1 && tag.cmd == IrCmd::LOAD_TAG && (OP_A(tag).kind == IrOpKind::VmReg || OP_A(tag).kind == IrOpKind::VmConst))
                {
                    if (FFlag::LuauCodegenX64ConflictCheck)
                        replaceIfNoConflicts(function, tag, block, index, OP_A(inst), OP_A(tag));
                    else
                        replace(function, OP_A(inst), OP_A(tag));
                }
            }

            if (OP_B(inst).kind == IrOpKind::Inst)
            {
                IrInst& value = function.instOp(OP_B(inst));

                if (value.useCount == 1 && value.cmd == IrCmd::LOAD_INT)
                {
                    if (FFlag::LuauCodegenX64ConflictCheck)
                        replaceIfNoConflicts(function, value, block, index, OP_B(inst), OP_A(value));
                    else
                        replace(function, OP_B(inst), OP_A(value));
                }
            }
            break;
        }
        case IrCmd::ADD_NUM:
        case IrCmd::SUB_NUM:
        case IrCmd::MUL_NUM:
        case IrCmd::DIV_NUM:
        case IrCmd::IDIV_NUM:
        case IrCmd::MOD_NUM:
        case IrCmd::MIN_NUM:
        case IrCmd::MAX_NUM:
        {
            if (OP_B(inst).kind == IrOpKind::Inst)
            {
                IrInst& rhs = function.instOp(OP_B(inst));

                if (rhs.useCount == 1 && rhs.cmd == IrCmd::LOAD_DOUBLE && (OP_A(rhs).kind == IrOpKind::VmReg || OP_A(rhs).kind == IrOpKind::VmConst))
                {
                    if (FFlag::LuauCodegenX64ConflictCheck)
                        replaceIfNoConflicts(function, rhs, block, index, OP_B(inst), OP_A(rhs));
                    else
                        replace(function, OP_B(inst), OP_A(rhs));
                }
            }
            break;
        }
        case IrCmd::JUMP_EQ_TAG:
        {
            if (OP_A(inst).kind == IrOpKind::Inst)
            {
                IrInst& tagA = function.instOp(OP_A(inst));

                if (tagA.useCount == 1 && tagA.cmd == IrCmd::LOAD_TAG && (OP_A(tagA).kind == IrOpKind::VmReg || OP_A(tagA).kind == IrOpKind::VmConst))
                {
                    if (FFlag::LuauCodegenX64ConflictCheck)
                        replaceIfNoConflicts(function, tagA, block, index, OP_A(inst), OP_A(tagA));
                    else
                        replace(function, OP_A(inst), OP_A(tagA));
                    break;
                }
            }

            if (OP_B(inst).kind == IrOpKind::Inst)
            {
                IrInst& tagB = function.instOp(OP_B(inst));

                if (tagB.useCount == 1 && tagB.cmd == IrCmd::LOAD_TAG && (OP_A(tagB).kind == IrOpKind::VmReg || OP_A(tagB).kind == IrOpKind::VmConst))
                {
                    std::swap(OP_A(inst), OP_B(inst));

                    if (FFlag::LuauCodegenX64ConflictCheck)
                        replaceIfNoConflicts(function, tagB, block, index, OP_A(inst), OP_A(tagB));
                    else
                        replace(function, OP_A(inst), OP_A(tagB));
                }
            }
            break;
        }
        case IrCmd::JUMP_CMP_NUM:
        {
            if (OP_A(inst).kind == IrOpKind::Inst)
            {
                IrInst& num = function.instOp(OP_A(inst));

                if (num.useCount == 1 && num.cmd == IrCmd::LOAD_DOUBLE)
                {
                    if (FFlag::LuauCodegenX64ConflictCheck)
                        replaceIfNoConflicts(function, num, block, index, OP_A(inst), OP_A(num));
                    else
                        replace(function, OP_A(inst), OP_A(num));
                }
            }
            break;
        }
        case IrCmd::FLOOR_NUM:
        case IrCmd::CEIL_NUM:
        case IrCmd::ROUND_NUM:
        case IrCmd::SQRT_NUM:
        case IrCmd::ABS_NUM:
        {
            if (OP_A(inst).kind == IrOpKind::Inst)
            {
                IrInst& arg = function.instOp(OP_A(inst));

                if (arg.useCount == 1 && arg.cmd == IrCmd::LOAD_DOUBLE && (OP_A(arg).kind == IrOpKind::VmReg || OP_A(arg).kind == IrOpKind::VmConst))
                {
                    if (FFlag::LuauCodegenX64ConflictCheck)
                        replaceIfNoConflicts(function, arg, block, index, OP_A(inst), OP_A(arg));
                    else
                        replace(function, OP_A(inst), OP_A(arg));
                }
            }
            break;
        }
        default:
            break;
        }
    }
}

void optimizeMemoryOperandsX64(IrFunction& function)
{
    for (IrBlock& block : function.blocks)
    {
        if (block.kind == IrBlockKind::Dead)
            continue;

        // Inlining a load into its consumer inside the ExitSync block would will kill the operands listed in VM exit sync info argOps
        if (block.kind == IrBlockKind::ExitSync)
            continue;

        optimizeMemoryOperandsX64(function, block);
    }
}

} // namespace CodeGen
} // namespace Luau
