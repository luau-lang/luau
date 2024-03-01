// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/OptimizeFinalX64.h"

#include "Luau/IrUtils.h"

#include <utility>

namespace Luau
{
namespace CodeGen
{

// x64 assembly allows memory operands, but IR separates loads from uses
// To improve final x64 lowering, we try to 'inline' single-use register/constant loads into some of our instructions
// This pass might not be useful on different architectures
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
            if (inst.a.kind == IrOpKind::Inst)
            {
                IrInst& tag = function.instOp(inst.a);

                if (tag.useCount == 1 && tag.cmd == IrCmd::LOAD_TAG && (tag.a.kind == IrOpKind::VmReg || tag.a.kind == IrOpKind::VmConst))
                    replace(function, inst.a, tag.a);
            }
            break;
        }
        case IrCmd::CHECK_TRUTHY:
        {
            if (inst.a.kind == IrOpKind::Inst)
            {
                IrInst& tag = function.instOp(inst.a);

                if (tag.useCount == 1 && tag.cmd == IrCmd::LOAD_TAG && (tag.a.kind == IrOpKind::VmReg || tag.a.kind == IrOpKind::VmConst))
                    replace(function, inst.a, tag.a);
            }

            if (inst.b.kind == IrOpKind::Inst)
            {
                IrInst& value = function.instOp(inst.b);

                if (value.useCount == 1 && value.cmd == IrCmd::LOAD_INT)
                    replace(function, inst.b, value.a);
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
            if (inst.b.kind == IrOpKind::Inst)
            {
                IrInst& rhs = function.instOp(inst.b);

                if (rhs.useCount == 1 && rhs.cmd == IrCmd::LOAD_DOUBLE && (rhs.a.kind == IrOpKind::VmReg || rhs.a.kind == IrOpKind::VmConst))
                    replace(function, inst.b, rhs.a);
            }
            break;
        }
        case IrCmd::JUMP_EQ_TAG:
        {
            if (inst.a.kind == IrOpKind::Inst)
            {
                IrInst& tagA = function.instOp(inst.a);

                if (tagA.useCount == 1 && tagA.cmd == IrCmd::LOAD_TAG && (tagA.a.kind == IrOpKind::VmReg || tagA.a.kind == IrOpKind::VmConst))
                {
                    replace(function, inst.a, tagA.a);
                    break;
                }
            }

            if (inst.b.kind == IrOpKind::Inst)
            {
                IrInst& tagB = function.instOp(inst.b);

                if (tagB.useCount == 1 && tagB.cmd == IrCmd::LOAD_TAG && (tagB.a.kind == IrOpKind::VmReg || tagB.a.kind == IrOpKind::VmConst))
                {
                    std::swap(inst.a, inst.b);
                    replace(function, inst.a, tagB.a);
                }
            }
            break;
        }
        case IrCmd::JUMP_CMP_NUM:
        {
            if (inst.a.kind == IrOpKind::Inst)
            {
                IrInst& num = function.instOp(inst.a);

                if (num.useCount == 1 && num.cmd == IrCmd::LOAD_DOUBLE)
                    replace(function, inst.a, num.a);
            }
            break;
        }
        case IrCmd::FLOOR_NUM:
        case IrCmd::CEIL_NUM:
        case IrCmd::ROUND_NUM:
        case IrCmd::SQRT_NUM:
        case IrCmd::ABS_NUM:
        {
            if (inst.a.kind == IrOpKind::Inst)
            {
                IrInst& arg = function.instOp(inst.a);

                if (arg.useCount == 1 && arg.cmd == IrCmd::LOAD_DOUBLE && (arg.a.kind == IrOpKind::VmReg || arg.a.kind == IrOpKind::VmConst))
                    replace(function, inst.a, arg.a);
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

        optimizeMemoryOperandsX64(function, block);
    }
}

} // namespace CodeGen
} // namespace Luau
