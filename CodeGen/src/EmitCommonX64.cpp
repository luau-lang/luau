// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "EmitCommonX64.h"

#include "Luau/AssemblyBuilderX64.h"

#include "CustomExecUtils.h"
#include "NativeState.h"

#include "lgc.h"
#include "lstate.h"

namespace Luau
{
namespace CodeGen
{

void jumpOnNumberCmp(AssemblyBuilderX64& build, RegisterX64 tmp, OperandX64 lhs, OperandX64 rhs, Condition cond, Label& label)
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
    case Condition::NotLessEqual:
        // (b < a) is the same as !(a <= b). jnae checks CF=1 which means < or NaN
        build.jcc(Condition::NotAboveEqual, label);
        break;
    case Condition::LessEqual:
        // (b >= a) is the same as (a <= b). jae checks CF=0 which means >= and not NaN
        build.jcc(Condition::AboveEqual, label);
        break;
    case Condition::NotLess:
        // (b <= a) is the same as !(a < b). jna checks CF=1 or ZF=1 which means <= or NaN
        build.jcc(Condition::NotAbove, label);
        break;
    case Condition::Less:
        // (b > a) is the same as (a < b). ja checks CF=0 and ZF=0 which means > and not NaN
        build.jcc(Condition::Above, label);
        break;
    case Condition::NotEqual:
        // ZF=0 or PF=1 means != or NaN
        build.jcc(Condition::NotZero, label);
        build.jcc(Condition::Parity, label);
        break;
    default:
        LUAU_ASSERT(!"Unsupported condition");
    }
}

void jumpOnAnyCmpFallback(AssemblyBuilderX64& build, int ra, int rb, Condition cond, Label& label, int pcpos)
{
    emitSetSavedPc(build, pcpos + 1);
    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegValue(ra));
    build.lea(rArg3, luauRegValue(rb));

    if (cond == Condition::NotLessEqual || cond == Condition::LessEqual)
        build.call(qword[rNativeContext + offsetof(NativeContext, luaV_lessequal)]);
    else if (cond == Condition::NotLess || cond == Condition::Less)
        build.call(qword[rNativeContext + offsetof(NativeContext, luaV_lessthan)]);
    else if (cond == Condition::NotEqual || cond == Condition::Equal)
        build.call(qword[rNativeContext + offsetof(NativeContext, luaV_equalval)]);
    else
        LUAU_ASSERT(!"Unsupported condition");

    emitUpdateBase(build);
    build.test(eax, eax);
    build.jcc(
        cond == Condition::NotLessEqual || cond == Condition::NotLess || cond == Condition::NotEqual ? Condition::Zero : Condition::NotZero, label);
}

void convertNumberToIndexOrJump(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 numd, RegisterX64 numi, int ri, Label& label)
{
    LUAU_ASSERT(numi.size == SizeX64::dword);

    build.vmovsd(numd, luauRegValue(ri));

    // Convert to integer, NaN is converted into 0x80000000
    build.vcvttsd2si(numi, numd);

    // Convert that integer back to double
    build.vcvtsi2sd(tmp, numd, numi);

    build.vucomisd(tmp, numd); // Sets ZF=1 if equal or NaN
    // We don't need non-integer values
    // But to skip the PF=1 check, we proceed with NaN because 0x80000000 index is out of bounds
    build.jcc(Condition::NotZero, label);
}

void callArithHelper(AssemblyBuilderX64& build, int ra, int rb, OperandX64 c, int pcpos, TMS tm)
{
    emitSetSavedPc(build, pcpos + 1);

    if (getCurrentX64ABI() == X64ABI::Windows)
        build.mov(sArg5, tm);
    else
        build.mov(rArg5, tm);

    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegValue(ra));
    build.lea(rArg3, luauRegValue(rb));
    build.lea(rArg4, c);
    build.call(qword[rNativeContext + offsetof(NativeContext, luaV_doarith)]);

    emitUpdateBase(build);
}

void callLengthHelper(AssemblyBuilderX64& build, int ra, int rb, int pcpos)
{
    emitSetSavedPc(build, pcpos + 1);

    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegValue(ra));
    build.lea(rArg3, luauRegValue(rb));
    build.call(qword[rNativeContext + offsetof(NativeContext, luaV_dolen)]);

    emitUpdateBase(build);
}

void callPrepareForN(AssemblyBuilderX64& build, int limit, int step, int init, int pcpos)
{
    emitSetSavedPc(build, pcpos + 1);

    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegValue(limit));
    build.lea(rArg3, luauRegValue(step));
    build.lea(rArg4, luauRegValue(init));
    build.call(qword[rNativeContext + offsetof(NativeContext, luaV_prepareFORN)]);
}

void callGetTable(AssemblyBuilderX64& build, int rb, OperandX64 c, int ra, int pcpos)
{
    emitSetSavedPc(build, pcpos + 1);

    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegValue(rb));
    build.lea(rArg3, c);
    build.lea(rArg4, luauRegValue(ra));
    build.call(qword[rNativeContext + offsetof(NativeContext, luaV_gettable)]);

    emitUpdateBase(build);
}

void callSetTable(AssemblyBuilderX64& build, int rb, OperandX64 c, int ra, int pcpos)
{
    emitSetSavedPc(build, pcpos + 1);

    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegValue(rb));
    build.lea(rArg3, c);
    build.lea(rArg4, luauRegValue(ra));
    build.call(qword[rNativeContext + offsetof(NativeContext, luaV_settable)]);

    emitUpdateBase(build);
}

void callBarrierTable(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 table, int ra, Label& skip)
{
    LUAU_ASSERT(tmp != table);

    // iscollectable(ra)
    build.cmp(luauRegTag(ra), LUA_TSTRING);
    build.jcc(Condition::Less, skip);

    // isblack(obj2gco(h))
    build.test(byte[table + offsetof(GCheader, marked)], bitmask(BLACKBIT));
    build.jcc(Condition::Zero, skip);

    // iswhite(gcvalue(ra))
    build.mov(tmp, luauRegValue(ra));
    build.test(byte[tmp + offsetof(GCheader, marked)], bit2mask(WHITE0BIT, WHITE1BIT));
    build.jcc(Condition::Zero, skip);

    LUAU_ASSERT(table != rArg3);
    build.mov(rArg3, tmp);
    build.mov(rArg2, table);
    build.mov(rArg1, rState);
    build.call(qword[rNativeContext + offsetof(NativeContext, luaC_barriertable)]);
}

void emitExit(AssemblyBuilderX64& build, bool continueInVm)
{
    if (continueInVm)
        build.mov(al, 1);
    else
        build.xor_(eax, eax);

    build.jmp(qword[rNativeContext + offsetof(NativeContext, gateExit)]);
}

void emitUpdateBase(AssemblyBuilderX64& build)
{
    build.mov(rBase, qword[rState + offsetof(lua_State, base)]);
}

// Note: only uses rax/rdx, the caller may use other registers
void emitSetSavedPc(AssemblyBuilderX64& build, int pcpos)
{
    build.mov(rdx, sCode);
    build.add(rdx, pcpos * sizeof(Instruction));
    build.mov(rax, qword[rState + offsetof(lua_State, ci)]);
    build.mov(qword[rax + offsetof(CallInfo, savedpc)], rdx);
}

void emitInterrupt(AssemblyBuilderX64& build, int pcpos)
{
    Label skip;

    // Skip if there is no interrupt set
    build.mov(r8, qword[rState + offsetof(lua_State, global)]);
    build.mov(r8, qword[r8 + offsetof(global_State, cb.interrupt)]);
    build.test(r8, r8);
    build.jcc(Condition::Zero, skip);

    emitSetSavedPc(build, pcpos + 1); // uses rax/rdx

    // Call interrupt
    // TODO: This code should move to the end of the function, or even be outlined so that it can be shared by multiple interruptible instructions
    build.mov(rArg1, rState);
    build.mov(rArg2d, -1);
    build.call(r8);

    // Check if we need to exit
    build.mov(al, byte[rState + offsetof(lua_State, status)]);
    build.test(al, al);
    build.jcc(Condition::Zero, skip);

    build.mov(rax, qword[rState + offsetof(lua_State, ci)]);
    build.sub(qword[rax + offsetof(CallInfo, savedpc)], sizeof(Instruction));
    emitExit(build, /* continueInVm */ false);

    build.setLabel(skip);
}

void emitFallback(AssemblyBuilderX64& build, NativeState& data, int op, int pcpos)
{
    if (op == LOP_CAPTURE)
        return;

    NativeFallback& opinfo = data.context.fallback[op];
    LUAU_ASSERT(opinfo.fallback);

    if (build.logText)
        build.logAppend("; fallback\n");

    // fallback(L, instruction, base, k)
    build.mov(rArg1, rState);
    build.mov(rArg2, sCode);
    build.add(rArg2, pcpos * sizeof(Instruction));
    build.mov(rArg3, rBase);
    build.mov(rArg4, rConstants);
    build.call(qword[rNativeContext + offsetof(NativeContext, fallback) + op * sizeof(NativeFallback) + offsetof(NativeFallback, fallback)]);

    // Some instructions may interrupt the execution
    if (opinfo.flags & kFallbackCheckInterrupt)
    {
        Label skip;

        build.test(rax, rax);
        build.jcc(Condition::NotZero, skip);
        emitExit(build, /* continueInVm */ false);
        build.setLabel(skip);
    }

    emitUpdateBase(build);

    // Some instructions may jump to a different instruction or a completely different function
    if (opinfo.flags & kFallbackUpdatePc)
    {
        build.mov(rcx, sClosure);
        build.mov(rcx, qword[rcx + offsetof(Closure, l.p)]);

        // Get instruction index from returned instruction pointer
        // To get instruction index from instruction pointer, we need to divide byte offset by 4
        // But we will actually need to scale instruction index by 8 back to byte offset later so it cancels out
        build.sub(rax, sCode);

        build.mov(rdx, qword[rcx + offsetofProtoExecData]);

        // Get new instruction location and jump to it
        build.mov(rcx, qword[rdx + offsetof(NativeProto, instTargets)]);
        build.jmp(qword[rax * 2 + rcx]);
    }
    else if (opinfo.flags & kFallbackUpdateCi)
    {
        // Need to update state of the current function before we jump away
        build.mov(rcx, qword[rState + offsetof(lua_State, ci)]); // L->ci
        build.mov(rcx, qword[rcx + offsetof(CallInfo, func)]);   // L->ci->func
        build.mov(rcx, qword[rcx + offsetof(TValue, value.gc)]); // L->ci->func->value.gc aka cl
        build.mov(sClosure, rcx);
        build.mov(rsi, qword[rcx + offsetof(Closure, l.p)]);    // cl->l.p aka proto
        build.mov(rConstants, qword[rsi + offsetof(Proto, k)]); // proto->k
        build.mov(rcx, qword[rsi + offsetof(Proto, code)]);     // proto->code
        build.mov(sCode, rcx);

        // We'll need original instruction pointer later to handle return to interpreter
        if (op == LOP_CALL)
            build.mov(r9, rax);

        // Get instruction index from instruction pointer
        // To get instruction index from instruction pointer, we need to divide byte offset by 4
        // But we will actually need to scale instruction index by 8 back to byte offset later so it cancels out
        build.sub(rax, sCode);

        // We need to check if the new function can be executed natively
        Label returnToInterpreter;

        build.mov(rdx, qword[rsi + offsetofProtoExecData]);
        build.test(rdx, rdx);
        build.jcc(Condition::Zero, returnToInterpreter);

        // Get new instruction location and jump to it
        build.mov(rcx, qword[rdx + offsetof(NativeProto, instTargets)]);
        build.jmp(qword[rax * 2 + rcx]);

        build.setLabel(returnToInterpreter);

        // If we are returning to the interpreter to make a call, we need to update the current instruction
        if (op == LOP_CALL)
        {
            build.mov(rax, qword[rState + offsetof(lua_State, ci)]);
            build.mov(qword[rax + offsetof(CallInfo, savedpc)], r9);
        }

        // Continue in the interpreter
        emitExit(build, /* continueInVm */ true);
    }
}

} // namespace CodeGen
} // namespace Luau
