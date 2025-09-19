// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrValueLocationTracking.h"

#include "Luau/IrUtils.h"

LUAU_FASTFLAGVARIABLE(LuauCodeGenRestoreFromSplitStore)

namespace Luau
{
namespace CodeGen
{

IrValueLocationTracking::IrValueLocationTracking(IrFunction& function)
    : function(function)
{
    vmRegValue.fill(kInvalidInstIdx);
}

void IrValueLocationTracking::setRestoreCallback(void* context, void (*callback)(void* context, IrInst& inst))
{
    restoreCallbackCtx = context;
    restoreCallback = callback;
}

void IrValueLocationTracking::beforeInstLowering(IrInst& inst)
{
    switch (inst.cmd)
    {
    case IrCmd::STORE_TAG:
        // Tag update is a bit tricky, restore operations of values are not affected
        invalidateRestoreOp(inst.a, /*skipValueInvalidation*/ true);
        break;
    case IrCmd::STORE_EXTRA:
        // While extra field update doesn't invalidate some of the values, it can invalidate a vector type field
        invalidateRestoreOp(inst.a, /*skipValueInvalidation*/ false);
        break;
    case IrCmd::STORE_POINTER:
    case IrCmd::STORE_DOUBLE:
    case IrCmd::STORE_INT:
    case IrCmd::STORE_VECTOR:
    case IrCmd::STORE_TVALUE:
    case IrCmd::STORE_SPLIT_TVALUE:
        invalidateRestoreOp(inst.a, /*skipValueInvalidation*/ false);
        break;
    case IrCmd::ADJUST_STACK_TO_REG:
        invalidateRestoreVmRegs(vmRegOp(inst.a), -1);
        break;
    case IrCmd::FASTCALL:
        invalidateRestoreVmRegs(vmRegOp(inst.b), function.intOp(inst.d));
        break;
    case IrCmd::INVOKE_FASTCALL:
        // Multiple return sequences (count == -1) are defined by ADJUST_STACK_TO_REG
        if (int count = function.intOp(inst.g); count != -1)
            invalidateRestoreVmRegs(vmRegOp(inst.b), count);
        break;
    case IrCmd::DO_ARITH:
    case IrCmd::DO_LEN:
    case IrCmd::GET_TABLE:
    case IrCmd::GET_CACHED_IMPORT:
        invalidateRestoreOp(inst.a, /*skipValueInvalidation*/ false);
        break;
    case IrCmd::CONCAT:
        invalidateRestoreVmRegs(vmRegOp(inst.a), function.uintOp(inst.b));
        break;
    case IrCmd::GET_UPVALUE:
        invalidateRestoreOp(inst.a, /*skipValueInvalidation*/ false);
        break;
    case IrCmd::CALL:
        // Even if result count is limited, all registers starting from function (ra) might be modified
        invalidateRestoreVmRegs(vmRegOp(inst.a), -1);
        break;
    case IrCmd::FORGLOOP:
    case IrCmd::FORGLOOP_FALLBACK:
        // Even if result count is limited, all registers starting from iteration index (ra+2) might be modified
        invalidateRestoreVmRegs(vmRegOp(inst.a) + 2, -1);
        break;
    case IrCmd::FALLBACK_GETGLOBAL:
    case IrCmd::FALLBACK_GETTABLEKS:
        invalidateRestoreOp(inst.b, /*skipValueInvalidation*/ false);
        break;
    case IrCmd::FALLBACK_NAMECALL:
        invalidateRestoreVmRegs(vmRegOp(inst.b), 2);
        break;
    case IrCmd::FALLBACK_GETVARARGS:
        invalidateRestoreVmRegs(vmRegOp(inst.b), function.intOp(inst.c));
        break;
    case IrCmd::FALLBACK_DUPCLOSURE:
        invalidateRestoreOp(inst.b, /*skipValueInvalidation*/ false);
        break;
    case IrCmd::FALLBACK_FORGPREP:
        invalidateRestoreVmRegs(vmRegOp(inst.b), 3);
        break;

        // Make sure all VmReg referencing instructions are handled explicitly (only register reads here)
    case IrCmd::LOAD_TAG:
    case IrCmd::LOAD_POINTER:
    case IrCmd::LOAD_DOUBLE:
    case IrCmd::LOAD_INT:
    case IrCmd::LOAD_FLOAT:
    case IrCmd::LOAD_TVALUE:
    case IrCmd::CMP_ANY:
    case IrCmd::CMP_TAG:
    case IrCmd::JUMP_IF_TRUTHY:
    case IrCmd::JUMP_IF_FALSY:
    case IrCmd::JUMP_EQ_TAG:
    case IrCmd::SET_TABLE:
    case IrCmd::SET_UPVALUE:
    case IrCmd::INTERRUPT:
    case IrCmd::BARRIER_OBJ:
    case IrCmd::BARRIER_TABLE_FORWARD:
    case IrCmd::CLOSE_UPVALS:
    case IrCmd::CAPTURE:
    case IrCmd::SETLIST:
    case IrCmd::RETURN:
    case IrCmd::FORGPREP_XNEXT_FALLBACK:
    case IrCmd::FALLBACK_SETGLOBAL:
    case IrCmd::FALLBACK_SETTABLEKS:
    case IrCmd::FALLBACK_PREPVARARGS:
    case IrCmd::ADJUST_STACK_TO_TOP:
    case IrCmd::GET_TYPEOF:
    case IrCmd::NEWCLOSURE:
    case IrCmd::FINDUPVAL:
        break;

        // These instructions read VmReg only after optimizeMemoryOperandsX64
    case IrCmd::CHECK_TAG:
    case IrCmd::CHECK_TRUTHY:
    case IrCmd::ADD_NUM:
    case IrCmd::SUB_NUM:
    case IrCmd::MUL_NUM:
    case IrCmd::DIV_NUM:
    case IrCmd::IDIV_NUM:
    case IrCmd::MOD_NUM:
    case IrCmd::MIN_NUM:
    case IrCmd::MAX_NUM:
    case IrCmd::JUMP_CMP_NUM:
    case IrCmd::FLOOR_NUM:
    case IrCmd::CEIL_NUM:
    case IrCmd::ROUND_NUM:
    case IrCmd::SQRT_NUM:
    case IrCmd::ABS_NUM:
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

void IrValueLocationTracking::afterInstLowering(IrInst& inst, uint32_t instIdx)
{
    switch (inst.cmd)
    {
    case IrCmd::LOAD_TAG:
    case IrCmd::LOAD_POINTER:
    case IrCmd::LOAD_DOUBLE:
    case IrCmd::LOAD_INT:
    case IrCmd::LOAD_TVALUE:
        if (inst.a.kind == IrOpKind::VmReg)
            invalidateRestoreOp(inst.a, /*skipValueInvalidation*/ false);

        recordRestoreOp(instIdx, inst.a);
        break;
    case IrCmd::STORE_POINTER:
    case IrCmd::STORE_DOUBLE:
    case IrCmd::STORE_INT:
    case IrCmd::STORE_TVALUE:
        // If this is not the last use of the stored value, we can restore it from this new location
        if (inst.b.kind == IrOpKind::Inst && function.instOp(inst.b).lastUse != instIdx)
            recordRestoreOp(inst.b.index, inst.a);
        break;
    case IrCmd::STORE_SPLIT_TVALUE:
        if (FFlag::LuauCodeGenRestoreFromSplitStore)
        {
            // If this is not the last use of the stored value, we can restore it from this new location
            if (inst.c.kind == IrOpKind::Inst && function.instOp(inst.c).lastUse != instIdx)
                recordRestoreOp(inst.c.index, inst.a);
        }
        break;
    default:
        break;
    }
}

void IrValueLocationTracking::recordRestoreOp(uint32_t instIdx, IrOp location)
{
    if (location.kind == IrOpKind::VmReg)
    {
        int reg = vmRegOp(location);

        if (reg > maxReg)
            maxReg = reg;

        // Record location in register memory only if register is not captured
        if (!function.cfg.captured.regs.test(reg))
            function.recordRestoreOp(instIdx, location);

        vmRegValue[reg] = instIdx;
    }
    else if (location.kind == IrOpKind::VmConst)
    {
        function.recordRestoreOp(instIdx, location);
    }
}

void IrValueLocationTracking::invalidateRestoreOp(IrOp location, bool skipValueInvalidation)
{
    if (location.kind == IrOpKind::VmReg)
    {
        uint32_t& instIdx = vmRegValue[vmRegOp(location)];

        if (instIdx != kInvalidInstIdx)
        {
            IrInst& inst = function.instructions[instIdx];

            // If we are only modifying the tag, we can avoid invalidating tracked location of values
            if (skipValueInvalidation)
            {
                switch (getCmdValueKind(inst.cmd))
                {
                case IrValueKind::Double:
                case IrValueKind::Pointer:
                case IrValueKind::Int:
                    return;
                default:
                    break;
                }
            }

            // If instruction value is spilled and memory location is about to be lost, it has to be restored immediately
            if (inst.needsReload)
                restoreCallback(restoreCallbackCtx, inst);

            // Instruction loses its memory storage location
            function.recordRestoreOp(instIdx, IrOp());

            // Register loses link with instruction
            instIdx = kInvalidInstIdx;
        }
    }
    else if (location.kind == IrOpKind::VmConst)
    {
        CODEGEN_ASSERT(!"VM constants are immutable");
    }
}

void IrValueLocationTracking::invalidateRestoreVmRegs(int start, int count)
{
    int end = count == -1 ? 255 : start + count;

    if (end > maxReg)
        end = maxReg;

    for (int reg = start; reg <= end; reg++)
        invalidateRestoreOp(IrOp{IrOpKind::VmReg, uint8_t(reg)}, /*skipValueInvalidation*/ false);
}

} // namespace CodeGen
} // namespace Luau
