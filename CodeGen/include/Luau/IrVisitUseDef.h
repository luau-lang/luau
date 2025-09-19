// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/IrData.h"

LUAU_FASTFLAG(LuauCodegenDirectCompare)

namespace Luau
{
namespace CodeGen
{

template<typename T>
static void visitVmRegDefsUses(T& visitor, IrFunction& function, const IrInst& inst)
{
    // For correct analysis, all instruction uses must be handled before handling the definitions
    switch (inst.cmd)
    {
    case IrCmd::LOAD_TAG:
    case IrCmd::LOAD_POINTER:
    case IrCmd::LOAD_DOUBLE:
    case IrCmd::LOAD_INT:
    case IrCmd::LOAD_FLOAT:
    case IrCmd::LOAD_TVALUE:
        visitor.maybeUse(inst.a); // Argument can also be a VmConst
        break;
    case IrCmd::STORE_TAG:
    case IrCmd::STORE_EXTRA:
    case IrCmd::STORE_POINTER:
    case IrCmd::STORE_DOUBLE:
    case IrCmd::STORE_INT:
    case IrCmd::STORE_VECTOR:
    case IrCmd::STORE_TVALUE:
    case IrCmd::STORE_SPLIT_TVALUE:
        visitor.maybeDef(inst.a); // Argument can also be a pointer value
        break;
    case IrCmd::CMP_ANY:
        visitor.use(inst.a);
        visitor.use(inst.b);
        break;
    case IrCmd::CMP_TAG:
        if (FFlag::LuauCodegenDirectCompare)
            visitor.maybeUse(inst.a);
        break;
    case IrCmd::JUMP_IF_TRUTHY:
    case IrCmd::JUMP_IF_FALSY:
        visitor.use(inst.a);
        break;
    case IrCmd::JUMP_EQ_TAG:
        if (FFlag::LuauCodegenDirectCompare)
            visitor.maybeUse(inst.a);
        break;
        // A <- B, C
    case IrCmd::DO_ARITH:
        visitor.maybeUse(inst.b); // Argument can also be a VmConst
        visitor.maybeUse(inst.c); // Argument can also be a VmConst

        visitor.def(inst.a);
        break;
    case IrCmd::GET_TABLE:
        visitor.use(inst.b);
        visitor.maybeUse(inst.c); // Argument can also be a VmConst

        visitor.def(inst.a);
        break;
    case IrCmd::SET_TABLE:
        visitor.use(inst.a);
        visitor.use(inst.b);
        visitor.maybeUse(inst.c); // Argument can also be a VmConst
        break;
        // A <- B
    case IrCmd::DO_LEN:
        visitor.use(inst.b);

        visitor.def(inst.a);
        break;
    case IrCmd::GET_CACHED_IMPORT:
        visitor.def(inst.a);
        break;
    case IrCmd::CONCAT:
        visitor.useRange(vmRegOp(inst.a), function.uintOp(inst.b));

        visitor.defRange(vmRegOp(inst.a), function.uintOp(inst.b));
        break;
    case IrCmd::GET_UPVALUE:
        visitor.def(inst.a);
        break;
    case IrCmd::SET_UPVALUE:
        visitor.use(inst.b);
        break;
    case IrCmd::INTERRUPT:
        break;
    case IrCmd::BARRIER_OBJ:
    case IrCmd::BARRIER_TABLE_FORWARD:
        visitor.maybeUse(inst.b);
        break;
    case IrCmd::CLOSE_UPVALS:
        // Closing an upvalue should be counted as a register use (it copies the fresh register value)
        // But we lack the required information about the specific set of registers that are affected
        // Because we don't plan to optimize captured registers atm, we skip full dataflow analysis for them right now
        break;
    case IrCmd::CAPTURE:
        visitor.maybeUse(inst.a);

        if (function.uintOp(inst.b) == 1)
            visitor.capture(vmRegOp(inst.a));
        break;
    case IrCmd::SETLIST:
        visitor.use(inst.b);
        visitor.useRange(vmRegOp(inst.c), function.intOp(inst.d));
        break;
    case IrCmd::CALL:
        visitor.use(inst.a);
        visitor.useRange(vmRegOp(inst.a) + 1, function.intOp(inst.b));

        visitor.defRange(vmRegOp(inst.a), function.intOp(inst.c));
        break;
    case IrCmd::RETURN:
        visitor.useRange(vmRegOp(inst.a), function.intOp(inst.b));
        break;

    case IrCmd::FASTCALL:
        visitor.use(inst.c);

        if (int nresults = function.intOp(inst.d); nresults != -1)
            visitor.defRange(vmRegOp(inst.b), nresults);
        break;
    case IrCmd::INVOKE_FASTCALL:
        if (int count = function.intOp(inst.f); count != -1)
        {
            // Only LOP_FASTCALL3 lowering is allowed to have third optional argument
            if (count >= 3 && inst.e.kind == IrOpKind::Undef)
            {
                CODEGEN_ASSERT(inst.d.kind == IrOpKind::VmReg && vmRegOp(inst.d) == vmRegOp(inst.c) + 1);

                visitor.useRange(vmRegOp(inst.c), count);
            }
            else
            {
                if (count >= 1)
                    visitor.use(inst.c);

                if (count >= 2)
                    visitor.maybeUse(inst.d); // Argument can also be a VmConst

                if (count >= 3)
                    visitor.maybeUse(inst.e); // Argument can also be a VmConst
            }
        }
        else
        {
            visitor.useVarargs(vmRegOp(inst.c));
        }

        // Multiple return sequences (count == -1) are defined by ADJUST_STACK_TO_REG
        if (int count = function.intOp(inst.g); count != -1)
            visitor.defRange(vmRegOp(inst.b), count);
        break;
    case IrCmd::FORGLOOP:
        // First register is not used by instruction, we check that it's still 'nil' with CHECK_TAG
        visitor.use(inst.a, 1);
        visitor.use(inst.a, 2);

        visitor.def(inst.a, 2);
        visitor.defRange(vmRegOp(inst.a) + 3, function.intOp(inst.b));
        break;
    case IrCmd::FORGLOOP_FALLBACK:
        visitor.useRange(vmRegOp(inst.a), 3);

        visitor.def(inst.a, 2);
        visitor.defRange(vmRegOp(inst.a) + 3, uint8_t(function.intOp(inst.b))); // ignore most significant bit
        break;
    case IrCmd::FORGPREP_XNEXT_FALLBACK:
        visitor.use(inst.b);
        break;
    case IrCmd::FALLBACK_GETGLOBAL:
        visitor.def(inst.b);
        break;
    case IrCmd::FALLBACK_SETGLOBAL:
        visitor.use(inst.b);
        break;
    case IrCmd::FALLBACK_GETTABLEKS:
        visitor.use(inst.c);

        visitor.def(inst.b);
        break;
    case IrCmd::FALLBACK_SETTABLEKS:
        visitor.use(inst.b);
        visitor.use(inst.c);
        break;
    case IrCmd::FALLBACK_NAMECALL:
        visitor.use(inst.c);

        visitor.defRange(vmRegOp(inst.b), 2);
        break;
    case IrCmd::FALLBACK_PREPVARARGS:
        // No effect on explicitly referenced registers
        break;
    case IrCmd::FALLBACK_GETVARARGS:
        visitor.defRange(vmRegOp(inst.b), function.intOp(inst.c));
        break;
    case IrCmd::FALLBACK_DUPCLOSURE:
        visitor.def(inst.b);
        break;
    case IrCmd::FALLBACK_FORGPREP:
        // This instruction doesn't always redefine Rn, Rn+1, Rn+2, so we have to mark it as implicit use
        visitor.useRange(vmRegOp(inst.b), 3);

        visitor.defRange(vmRegOp(inst.b), 3);
        break;
    case IrCmd::ADJUST_STACK_TO_REG:
        visitor.defRange(vmRegOp(inst.a), -1);
        break;
    case IrCmd::ADJUST_STACK_TO_TOP:
        // While this can be considered to be a vararg consumer, it is already handled in fastcall instructions
        break;
    case IrCmd::GET_TYPEOF:
        visitor.use(inst.a);
        break;

    case IrCmd::FINDUPVAL:
        visitor.use(inst.a);
        break;

    default:
        // All instructions which reference registers have to be handled explicitly
        CODEGEN_ASSERT(inst.a.kind != IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.b.kind != IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind != IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.d.kind != IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.e.kind != IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.f.kind != IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.g.kind != IrOpKind::VmReg);
        break;
    }
}

template<typename T>
static void visitVmRegDefsUses(T& visitor, IrFunction& function, const IrBlock& block)
{
    for (uint32_t instIdx = block.start; instIdx <= block.finish; instIdx++)
    {
        IrInst& inst = function.instructions[instIdx];

        visitVmRegDefsUses(visitor, function, inst);
    }
}

} // namespace CodeGen
} // namespace Luau
