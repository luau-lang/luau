// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "EmitCommonX64.h"

#include "Luau/AssemblyBuilderX64.h"
#include "Luau/IrCallWrapperX64.h"
#include "Luau/IrData.h"
#include "Luau/IrRegAllocX64.h"

#include "NativeState.h"

#include "lgc.h"
#include "lstate.h"

namespace Luau
{
namespace CodeGen
{
namespace X64
{

void jumpOnNumberCmp(AssemblyBuilderX64& build, RegisterX64 tmp, OperandX64 lhs, OperandX64 rhs, IrCondition cond, Label& label)
{
    // Refresher on comi/ucomi EFLAGS:
    // CF only: less
    // ZF only: equal
    // PF+CF+ZF: unordered (NaN)

    if (rhs.cat == CategoryX64::reg)
    {
        build.vucomisd(rhs, lhs);
    }
    else
    {
        build.vmovsd(tmp, rhs);
        build.vucomisd(tmp, lhs);
    }

    // Keep in mind that 'Not' conditions want 'true' for comparisons with NaN
    // And because of NaN, integer check interchangeability like 'not less or equal' <-> 'greater' does not hold
    switch (cond)
    {
    case IrCondition::NotLessEqual:
        // (b < a) is the same as !(a <= b). jnae checks CF=1 which means < or NaN
        build.jcc(ConditionX64::NotAboveEqual, label);
        break;
    case IrCondition::LessEqual:
        // (b >= a) is the same as (a <= b). jae checks CF=0 which means >= and not NaN
        build.jcc(ConditionX64::AboveEqual, label);
        break;
    case IrCondition::NotLess:
        // (b <= a) is the same as !(a < b). jna checks CF=1 or ZF=1 which means <= or NaN
        build.jcc(ConditionX64::NotAbove, label);
        break;
    case IrCondition::Less:
        // (b > a) is the same as (a < b). ja checks CF=0 and ZF=0 which means > and not NaN
        build.jcc(ConditionX64::Above, label);
        break;
    case IrCondition::NotEqual:
        // ZF=0 or PF=1 means != or NaN
        build.jcc(ConditionX64::NotZero, label);
        build.jcc(ConditionX64::Parity, label);
        break;
    default:
        LUAU_ASSERT(!"Unsupported condition");
    }
}

void jumpOnAnyCmpFallback(IrRegAllocX64& regs, AssemblyBuilderX64& build, int ra, int rb, IrCondition cond, Label& label)
{
    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::qword, rState);
    callWrap.addArgument(SizeX64::qword, luauRegAddress(ra));
    callWrap.addArgument(SizeX64::qword, luauRegAddress(rb));

    if (cond == IrCondition::NotLessEqual || cond == IrCondition::LessEqual)
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_lessequal)]);
    else if (cond == IrCondition::NotLess || cond == IrCondition::Less)
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_lessthan)]);
    else if (cond == IrCondition::NotEqual || cond == IrCondition::Equal)
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_equalval)]);
    else
        LUAU_ASSERT(!"Unsupported condition");

    emitUpdateBase(build);
    build.test(eax, eax);
    build.jcc(cond == IrCondition::NotLessEqual || cond == IrCondition::NotLess || cond == IrCondition::NotEqual ? ConditionX64::Zero
                                                                                                                 : ConditionX64::NotZero,
        label);
}

void getTableNodeAtCachedSlot(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 node, RegisterX64 table, int pcpos)
{
    LUAU_ASSERT(tmp != node);
    LUAU_ASSERT(table != node);

    build.mov(node, qword[table + offsetof(Table, node)]);

    // compute cached slot
    build.mov(tmp, sCode);
    build.movzx(dwordReg(tmp), byte[tmp + pcpos * sizeof(Instruction) + kOffsetOfInstructionC]);
    build.and_(byteReg(tmp), byte[table + offsetof(Table, nodemask8)]);

    // LuaNode* n = &h->node[slot];
    build.shl(dwordReg(tmp), kLuaNodeSizeLog2);
    build.add(node, tmp);
}

void convertNumberToIndexOrJump(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 numd, RegisterX64 numi, Label& label)
{
    LUAU_ASSERT(numi.size == SizeX64::dword);

    // Convert to integer, NaN is converted into 0x80000000
    build.vcvttsd2si(numi, numd);

    // Convert that integer back to double
    build.vcvtsi2sd(tmp, numd, numi);

    build.vucomisd(tmp, numd); // Sets ZF=1 if equal or NaN
    // We don't need non-integer values
    // But to skip the PF=1 check, we proceed with NaN because 0x80000000 index is out of bounds
    build.jcc(ConditionX64::NotZero, label);
}

void callArithHelper(IrRegAllocX64& regs, AssemblyBuilderX64& build, int ra, int rb, OperandX64 c, TMS tm)
{
    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::qword, rState);
    callWrap.addArgument(SizeX64::qword, luauRegAddress(ra));
    callWrap.addArgument(SizeX64::qword, luauRegAddress(rb));
    callWrap.addArgument(SizeX64::qword, c);
    callWrap.addArgument(SizeX64::dword, tm);
    callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_doarith)]);

    emitUpdateBase(build);
}

void callLengthHelper(IrRegAllocX64& regs, AssemblyBuilderX64& build, int ra, int rb)
{
    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::qword, rState);
    callWrap.addArgument(SizeX64::qword, luauRegAddress(ra));
    callWrap.addArgument(SizeX64::qword, luauRegAddress(rb));
    callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_dolen)]);

    emitUpdateBase(build);
}

void callPrepareForN(IrRegAllocX64& regs, AssemblyBuilderX64& build, int limit, int step, int init)
{
    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::qword, rState);
    callWrap.addArgument(SizeX64::qword, luauRegAddress(limit));
    callWrap.addArgument(SizeX64::qword, luauRegAddress(step));
    callWrap.addArgument(SizeX64::qword, luauRegAddress(init));
    callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_prepareFORN)]);
}

void callGetTable(IrRegAllocX64& regs, AssemblyBuilderX64& build, int rb, OperandX64 c, int ra)
{
    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::qword, rState);
    callWrap.addArgument(SizeX64::qword, luauRegAddress(rb));
    callWrap.addArgument(SizeX64::qword, c);
    callWrap.addArgument(SizeX64::qword, luauRegAddress(ra));
    callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_gettable)]);

    emitUpdateBase(build);
}

void callSetTable(IrRegAllocX64& regs, AssemblyBuilderX64& build, int rb, OperandX64 c, int ra)
{
    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::qword, rState);
    callWrap.addArgument(SizeX64::qword, luauRegAddress(rb));
    callWrap.addArgument(SizeX64::qword, c);
    callWrap.addArgument(SizeX64::qword, luauRegAddress(ra));
    callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_settable)]);

    emitUpdateBase(build);
}

void checkObjectBarrierConditions(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 object, int ra, Label& skip)
{
    // iscollectable(ra)
    build.cmp(luauRegTag(ra), LUA_TSTRING);
    build.jcc(ConditionX64::Less, skip);

    // isblack(obj2gco(o))
    build.test(byte[object + offsetof(GCheader, marked)], bitmask(BLACKBIT));
    build.jcc(ConditionX64::Zero, skip);

    // iswhite(gcvalue(ra))
    build.mov(tmp, luauRegValue(ra));
    build.test(byte[tmp + offsetof(GCheader, marked)], bit2mask(WHITE0BIT, WHITE1BIT));
    build.jcc(ConditionX64::Zero, skip);
}

void callBarrierObject(IrRegAllocX64& regs, AssemblyBuilderX64& build, RegisterX64 object, IrOp objectOp, int ra)
{
    Label skip;

    ScopedRegX64 tmp{regs, SizeX64::qword};
    checkObjectBarrierConditions(build, tmp.reg, object, ra, skip);

    {
        ScopedSpills spillGuard(regs);

        IrCallWrapperX64 callWrap(regs, build);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::qword, object, objectOp);
        callWrap.addArgument(SizeX64::qword, tmp);
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaC_barrierf)]);
    }

    build.setLabel(skip);
}

void callBarrierTableFast(IrRegAllocX64& regs, AssemblyBuilderX64& build, RegisterX64 table, IrOp tableOp)
{
    Label skip;

    // isblack(obj2gco(t))
    build.test(byte[table + offsetof(GCheader, marked)], bitmask(BLACKBIT));
    build.jcc(ConditionX64::Zero, skip);

    {
        ScopedSpills spillGuard(regs);

        IrCallWrapperX64 callWrap(regs, build);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::qword, table, tableOp);
        callWrap.addArgument(SizeX64::qword, addr[table + offsetof(Table, gclist)]);
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaC_barrierback)]);
    }

    build.setLabel(skip);
}

void callStepGc(IrRegAllocX64& regs, AssemblyBuilderX64& build)
{
    Label skip;

    {
        ScopedRegX64 tmp1{regs, SizeX64::qword};
        ScopedRegX64 tmp2{regs, SizeX64::qword};

        build.mov(tmp1.reg, qword[rState + offsetof(lua_State, global)]);
        build.mov(tmp2.reg, qword[tmp1.reg + offsetof(global_State, totalbytes)]);
        build.cmp(tmp2.reg, qword[tmp1.reg + offsetof(global_State, GCthreshold)]);
        build.jcc(ConditionX64::Below, skip);
    }

    {
        ScopedSpills spillGuard(regs);

        IrCallWrapperX64 callWrap(regs, build);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::dword, 1);
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaC_step)]);
        emitUpdateBase(build);
    }

    build.setLabel(skip);
}

void emitExit(AssemblyBuilderX64& build, bool continueInVm)
{
    if (continueInVm)
        build.mov(eax, 1);
    else
        build.xor_(eax, eax);

    build.jmp(qword[rNativeContext + offsetof(NativeContext, gateExit)]);
}

void emitUpdateBase(AssemblyBuilderX64& build)
{
    build.mov(rBase, qword[rState + offsetof(lua_State, base)]);
}

static void emitSetSavedPc(IrRegAllocX64& regs, AssemblyBuilderX64& build, int pcpos)
{
    ScopedRegX64 tmp1{regs, SizeX64::qword};
    ScopedRegX64 tmp2{regs, SizeX64::qword};

    build.mov(tmp1.reg, sCode);
    build.add(tmp1.reg, pcpos * sizeof(Instruction));
    build.mov(tmp2.reg, qword[rState + offsetof(lua_State, ci)]);
    build.mov(qword[tmp2.reg + offsetof(CallInfo, savedpc)], tmp1.reg);
}

void emitInterrupt(IrRegAllocX64& regs, AssemblyBuilderX64& build, int pcpos)
{
    Label skip;

    ScopedRegX64 tmp{regs, SizeX64::qword};

    // Skip if there is no interrupt set
    build.mov(tmp.reg, qword[rState + offsetof(lua_State, global)]);
    build.mov(tmp.reg, qword[tmp.reg + offsetof(global_State, cb.interrupt)]);
    build.test(tmp.reg, tmp.reg);
    build.jcc(ConditionX64::Zero, skip);

    emitSetSavedPc(regs, build, pcpos + 1);

    // Call interrupt
    // TODO: This code should move to the end of the function, or even be outlined so that it can be shared by multiple interruptible instructions
    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::qword, rState);
    callWrap.addArgument(SizeX64::dword, -1);
    callWrap.call(tmp.release());

    emitUpdateBase(build); // interrupt may have reallocated stack

    // Check if we need to exit
    build.mov(al, byte[rState + offsetof(lua_State, status)]);
    build.test(al, al);
    build.jcc(ConditionX64::Zero, skip);

    build.mov(rax, qword[rState + offsetof(lua_State, ci)]);
    build.sub(qword[rax + offsetof(CallInfo, savedpc)], sizeof(Instruction));
    emitExit(build, /* continueInVm */ false);

    build.setLabel(skip);
}

void emitFallback(IrRegAllocX64& regs, AssemblyBuilderX64& build, int offset, int pcpos)
{
    // fallback(L, instruction, base, k)
    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::qword, rState);

    RegisterX64 reg = callWrap.suggestNextArgumentRegister(SizeX64::qword);
    build.mov(reg, sCode);
    callWrap.addArgument(SizeX64::qword, addr[reg + pcpos * sizeof(Instruction)]);

    callWrap.addArgument(SizeX64::qword, rBase);
    callWrap.addArgument(SizeX64::qword, rConstants);
    callWrap.call(qword[rNativeContext + offset]);

    emitUpdateBase(build);
}

void emitContinueCallInVm(AssemblyBuilderX64& build)
{
    RegisterX64 proto = rcx; // Sync with emitInstCall

    build.mov(rdx, qword[proto + offsetof(Proto, code)]);
    build.mov(rax, qword[rState + offsetof(lua_State, ci)]);
    build.mov(qword[rax + offsetof(CallInfo, savedpc)], rdx);

    emitExit(build, /* continueInVm */ true);
}

} // namespace X64
} // namespace CodeGen
} // namespace Luau
