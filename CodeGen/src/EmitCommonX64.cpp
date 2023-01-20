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

void jumpOnNumberCmp(AssemblyBuilderX64& build, RegisterX64 tmp, OperandX64 lhs, OperandX64 rhs, ConditionX64 cond, Label& label)
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
    case ConditionX64::NotLessEqual:
        // (b < a) is the same as !(a <= b). jnae checks CF=1 which means < or NaN
        build.jcc(ConditionX64::NotAboveEqual, label);
        break;
    case ConditionX64::LessEqual:
        // (b >= a) is the same as (a <= b). jae checks CF=0 which means >= and not NaN
        build.jcc(ConditionX64::AboveEqual, label);
        break;
    case ConditionX64::NotLess:
        // (b <= a) is the same as !(a < b). jna checks CF=1 or ZF=1 which means <= or NaN
        build.jcc(ConditionX64::NotAbove, label);
        break;
    case ConditionX64::Less:
        // (b > a) is the same as (a < b). ja checks CF=0 and ZF=0 which means > and not NaN
        build.jcc(ConditionX64::Above, label);
        break;
    case ConditionX64::NotEqual:
        // ZF=0 or PF=1 means != or NaN
        build.jcc(ConditionX64::NotZero, label);
        build.jcc(ConditionX64::Parity, label);
        break;
    default:
        LUAU_ASSERT(!"Unsupported condition");
    }
}

void jumpOnAnyCmpFallback(AssemblyBuilderX64& build, int ra, int rb, ConditionX64 cond, Label& label)
{
    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegAddress(ra));
    build.lea(rArg3, luauRegAddress(rb));

    if (cond == ConditionX64::NotLessEqual || cond == ConditionX64::LessEqual)
        build.call(qword[rNativeContext + offsetof(NativeContext, luaV_lessequal)]);
    else if (cond == ConditionX64::NotLess || cond == ConditionX64::Less)
        build.call(qword[rNativeContext + offsetof(NativeContext, luaV_lessthan)]);
    else if (cond == ConditionX64::NotEqual || cond == ConditionX64::Equal)
        build.call(qword[rNativeContext + offsetof(NativeContext, luaV_equalval)]);
    else
        LUAU_ASSERT(!"Unsupported condition");

    emitUpdateBase(build);
    build.test(eax, eax);
    build.jcc(cond == ConditionX64::NotLessEqual || cond == ConditionX64::NotLess || cond == ConditionX64::NotEqual ? ConditionX64::Zero
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

void callArithHelper(AssemblyBuilderX64& build, int ra, int rb, OperandX64 c, TMS tm)
{
    if (build.abi == ABIX64::Windows)
        build.mov(sArg5, tm);
    else
        build.mov(rArg5, tm);

    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegAddress(ra));
    build.lea(rArg3, luauRegAddress(rb));
    build.lea(rArg4, c);
    build.call(qword[rNativeContext + offsetof(NativeContext, luaV_doarith)]);

    emitUpdateBase(build);
}

void callLengthHelper(AssemblyBuilderX64& build, int ra, int rb)
{
    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegAddress(ra));
    build.lea(rArg3, luauRegAddress(rb));
    build.call(qword[rNativeContext + offsetof(NativeContext, luaV_dolen)]);

    emitUpdateBase(build);
}

void callPrepareForN(AssemblyBuilderX64& build, int limit, int step, int init)
{
    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegAddress(limit));
    build.lea(rArg3, luauRegAddress(step));
    build.lea(rArg4, luauRegAddress(init));
    build.call(qword[rNativeContext + offsetof(NativeContext, luaV_prepareFORN)]);
}

void callGetTable(AssemblyBuilderX64& build, int rb, OperandX64 c, int ra)
{
    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegAddress(rb));
    build.lea(rArg3, c);
    build.lea(rArg4, luauRegAddress(ra));
    build.call(qword[rNativeContext + offsetof(NativeContext, luaV_gettable)]);

    emitUpdateBase(build);
}

void callSetTable(AssemblyBuilderX64& build, int rb, OperandX64 c, int ra)
{
    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegAddress(rb));
    build.lea(rArg3, c);
    build.lea(rArg4, luauRegAddress(ra));
    build.call(qword[rNativeContext + offsetof(NativeContext, luaV_settable)]);

    emitUpdateBase(build);
}

// works for luaC_barriertable, luaC_barrierf
static void callBarrierImpl(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 object, int ra, Label& skip, int contextOffset)
{
    LUAU_ASSERT(tmp != object);

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

    LUAU_ASSERT(object != rArg3);
    build.mov(rArg3, tmp);
    build.mov(rArg2, object);
    build.mov(rArg1, rState);
    build.call(qword[rNativeContext + contextOffset]);
}

void callBarrierTable(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 table, int ra, Label& skip)
{
    callBarrierImpl(build, tmp, table, ra, skip, offsetof(NativeContext, luaC_barriertable));
}

void callBarrierObject(AssemblyBuilderX64& build, RegisterX64 tmp, RegisterX64 object, int ra, Label& skip)
{
    callBarrierImpl(build, tmp, object, ra, skip, offsetof(NativeContext, luaC_barrierf));
}

void callBarrierTableFast(AssemblyBuilderX64& build, RegisterX64 table, Label& skip)
{
    // isblack(obj2gco(t))
    build.test(byte[table + offsetof(GCheader, marked)], bitmask(BLACKBIT));
    build.jcc(ConditionX64::Zero, skip);

    // Argument setup re-ordered to avoid conflicts with table register
    if (table != rArg2)
        build.mov(rArg2, table);
    build.lea(rArg3, addr[rArg2 + offsetof(Table, gclist)]);
    build.mov(rArg1, rState);
    build.call(qword[rNativeContext + offsetof(NativeContext, luaC_barrierback)]);
}

void callCheckGc(AssemblyBuilderX64& build, int pcpos, bool savepc, Label& skip)
{
    build.mov(rax, qword[rState + offsetof(lua_State, global)]);
    build.mov(rdx, qword[rax + offsetof(global_State, totalbytes)]);
    build.cmp(rdx, qword[rax + offsetof(global_State, GCthreshold)]);
    build.jcc(ConditionX64::Below, skip);

    if (savepc)
        emitSetSavedPc(build, pcpos + 1);

    build.mov(rArg1, rState);
    build.mov(dwordReg(rArg2), 1);
    build.call(qword[rNativeContext + offsetof(NativeContext, luaC_step)]);

    emitUpdateBase(build);
}

void callGetFastTmOrFallback(AssemblyBuilderX64& build, RegisterX64 table, TMS tm, Label& fallback)
{
    build.mov(rArg1, qword[table + offsetof(Table, metatable)]);
    build.test(rArg1, rArg1);
    build.jcc(ConditionX64::Zero, fallback); // no metatable

    build.test(byte[rArg1 + offsetof(Table, tmcache)], 1 << tm);
    build.jcc(ConditionX64::NotZero, fallback); // no tag method

    // rArg1 is already prepared
    build.mov(rArg2, tm);
    build.mov(rax, qword[rState + offsetof(lua_State, global)]);
    build.mov(rArg3, qword[rax + offsetof(global_State, tmname) + tm * sizeof(TString*)]);
    build.call(qword[rNativeContext + offsetof(NativeContext, luaT_gettm)]);
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
    build.jcc(ConditionX64::Zero, skip);

    emitSetSavedPc(build, pcpos + 1); // uses rax/rdx

    // Call interrupt
    // TODO: This code should move to the end of the function, or even be outlined so that it can be shared by multiple interruptible instructions
    build.mov(rArg1, rState);
    build.mov(dwordReg(rArg2), -1); // function accepts 'int' here and using qword reg would've forced 8 byte constant here
    build.call(r8);

    // Check if we need to exit
    build.mov(al, byte[rState + offsetof(lua_State, status)]);
    build.test(al, al);
    build.jcc(ConditionX64::Zero, skip);

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
}

void emitContinueCallInVm(AssemblyBuilderX64& build)
{
    RegisterX64 proto = rcx; // Sync with emitInstCall

    build.mov(rdx, qword[proto + offsetof(Proto, code)]);
    build.mov(rax, qword[rState + offsetof(lua_State, ci)]);
    build.mov(qword[rax + offsetof(CallInfo, savedpc)], rdx);

    emitExit(build, /* continueInVm */ true);
}

} // namespace CodeGen
} // namespace Luau
