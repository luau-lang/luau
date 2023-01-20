// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "EmitInstructionX64.h"

#include "Luau/AssemblyBuilderX64.h"

#include "CustomExecUtils.h"
#include "EmitBuiltinsX64.h"
#include "EmitCommonX64.h"
#include "NativeState.h"

#include "lobject.h"
#include "ltm.h"

namespace Luau
{
namespace CodeGen
{

void emitInstLoadNil(AssemblyBuilderX64& build, const Instruction* pc)
{
    int ra = LUAU_INSN_A(*pc);

    build.mov(luauRegTag(ra), LUA_TNIL);
}

void emitInstLoadB(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr)
{
    int ra = LUAU_INSN_A(*pc);

    build.mov(luauRegValue(ra), LUAU_INSN_B(*pc));
    build.mov(luauRegTag(ra), LUA_TBOOLEAN);

    if (int target = LUAU_INSN_C(*pc))
        build.jmp(labelarr[pcpos + 1 + target]);
}

void emitInstLoadN(AssemblyBuilderX64& build, const Instruction* pc)
{
    int ra = LUAU_INSN_A(*pc);

    build.vmovsd(xmm0, build.f64(double(LUAU_INSN_D(*pc))));
    build.vmovsd(luauRegValue(ra), xmm0);
    build.mov(luauRegTag(ra), LUA_TNUMBER);
}

void emitInstLoadK(AssemblyBuilderX64& build, const Instruction* pc)
{
    int ra = LUAU_INSN_A(*pc);

    build.vmovups(xmm0, luauConstant(LUAU_INSN_D(*pc)));
    build.vmovups(luauReg(ra), xmm0);
}

void emitInstLoadKX(AssemblyBuilderX64& build, const Instruction* pc)
{
    int ra = LUAU_INSN_A(*pc);
    uint32_t aux = pc[1];

    build.vmovups(xmm0, luauConstant(aux));
    build.vmovups(luauReg(ra), xmm0);
}

void emitInstMove(AssemblyBuilderX64& build, const Instruction* pc)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);

    build.vmovups(xmm0, luauReg(rb));
    build.vmovups(luauReg(ra), xmm0);
}

void emitInstNameCall(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, const TValue* k, Label& next, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    uint32_t aux = pc[1];

    Label secondfpath;

    jumpIfTagIsNot(build, rb, LUA_TTABLE, fallback);

    RegisterX64 table = r8;
    build.mov(table, luauRegValue(rb));

    // &h->node[tsvalue(kv)->hash & (sizenode(h) - 1)];
    RegisterX64 node = rdx;
    build.mov(node, qword[table + offsetof(Table, node)]);
    build.mov(eax, 1);
    build.mov(cl, byte[table + offsetof(Table, lsizenode)]);
    build.shl(eax, cl);
    build.dec(eax);
    build.and_(eax, tsvalue(&k[aux])->hash);
    build.shl(rax, kLuaNodeSizeLog2);
    build.add(node, rax);

    jumpIfNodeKeyNotInExpectedSlot(build, rax, node, luauConstantValue(aux), secondfpath);

    setLuauReg(build, xmm0, ra + 1, luauReg(rb));
    setLuauReg(build, xmm0, ra, luauNodeValue(node));
    build.jmp(next);

    build.setLabel(secondfpath);

    jumpIfNodeHasNext(build, node, fallback);
    callGetFastTmOrFallback(build, table, TM_INDEX, fallback);
    jumpIfTagIsNot(build, rax, LUA_TTABLE, fallback);

    build.mov(table, qword[rax + offsetof(TValue, value)]);

    getTableNodeAtCachedSlot(build, rax, node, table, pcpos);
    jumpIfNodeKeyNotInExpectedSlot(build, rax, node, luauConstantValue(aux), fallback);

    setLuauReg(build, xmm0, ra + 1, luauReg(rb));
    setLuauReg(build, xmm0, ra, luauNodeValue(node));
}

void emitInstCall(AssemblyBuilderX64& build, ModuleHelpers& helpers, const Instruction* pc, int pcpos)
{
    int ra = LUAU_INSN_A(*pc);
    int nparams = LUAU_INSN_B(*pc) - 1;
    int nresults = LUAU_INSN_C(*pc) - 1;

    emitInterrupt(build, pcpos);

    emitSetSavedPc(build, pcpos + 1);

    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegAddress(ra));

    if (nparams == LUA_MULTRET)
        build.mov(rArg3, qword[rState + offsetof(lua_State, top)]);
    else
        build.lea(rArg3, luauRegAddress(ra + 1 + nparams));

    build.mov(dwordReg(rArg4), nresults);
    build.call(qword[rNativeContext + offsetof(NativeContext, callProlog)]);
    RegisterX64 ccl = rax; // Returned from callProlog

    emitUpdateBase(build);

    Label cFuncCall;

    build.test(byte[ccl + offsetof(Closure, isC)], 1);
    build.jcc(ConditionX64::NotZero, cFuncCall);

    {
        RegisterX64 proto = rcx; // Sync with emitContinueCallInVm
        RegisterX64 ci = rdx;
        RegisterX64 argi = rsi;
        RegisterX64 argend = rdi;

        build.mov(proto, qword[ccl + offsetof(Closure, l.p)]);

        // Switch current Closure
        build.mov(sClosure, ccl); // Last use of 'ccl'

        build.mov(ci, qword[rState + offsetof(lua_State, ci)]);

        Label fillnil, exitfillnil;

        // argi = L->top
        build.mov(argi, qword[rState + offsetof(lua_State, top)]);

        // argend = L->base + p->numparams
        build.movzx(eax, byte[proto + offsetof(Proto, numparams)]);
        build.shl(eax, kTValueSizeLog2);
        build.lea(argend, addr[rBase + rax]);

        // while (argi < argend) setnilvalue(argi++);
        build.setLabel(fillnil);
        build.cmp(argi, argend);
        build.jcc(ConditionX64::NotBelow, exitfillnil);

        build.mov(dword[argi + offsetof(TValue, tt)], LUA_TNIL);
        build.add(argi, sizeof(TValue));
        build.jmp(fillnil); // This loop rarely runs so it's not worth repeating cmp/jcc

        build.setLabel(exitfillnil);

        // Set L->top to ci->top as most function expect (no vararg)
        build.mov(rax, qword[ci + offsetof(CallInfo, top)]);
        build.mov(qword[rState + offsetof(lua_State, top)], rax);

        build.mov(rax, qword[proto + offsetofProtoExecData]); // We'll need this value later

        // But if it is vararg, update it to 'argi'
        Label skipVararg;

        build.test(byte[proto + offsetof(Proto, is_vararg)], 1);
        build.jcc(ConditionX64::Zero, skipVararg);

        build.mov(qword[rState + offsetof(lua_State, top)], argi);
        build.setLabel(skipVararg);

        // Check native function data
        build.test(rax, rax);
        build.jcc(ConditionX64::Zero, helpers.continueCallInVm);

        // Switch current constants
        build.mov(rConstants, qword[proto + offsetof(Proto, k)]);

        // Switch current code
        build.mov(rdx, qword[proto + offsetof(Proto, code)]);
        build.mov(sCode, rdx);

        build.jmp(qword[rax + offsetof(NativeProto, entryTarget)]);
    }

    build.setLabel(cFuncCall);

    {
        // results = ccl->c.f(L);
        build.mov(rArg1, rState);
        build.call(qword[ccl + offsetof(Closure, c.f)]); // Last use of 'ccl'
        RegisterX64 results = eax;

        build.test(results, results);                            // test here will set SF=1 for a negative number and it always sets OF to 0
        build.jcc(ConditionX64::Less, helpers.exitNoContinueVm); // jl jumps if SF != OF

        // We have special handling for small number of expected results below
        if (nresults != 0 && nresults != 1)
        {
            build.mov(rArg1, rState);
            build.mov(dwordReg(rArg2), nresults);
            build.mov(dwordReg(rArg3), results);
            build.call(qword[rNativeContext + offsetof(NativeContext, callEpilogC)]);

            emitUpdateBase(build);
            return;
        }

        RegisterX64 ci = rdx;
        RegisterX64 cip = rcx;
        RegisterX64 vali = rsi;

        build.mov(ci, qword[rState + offsetof(lua_State, ci)]);
        build.lea(cip, addr[ci - sizeof(CallInfo)]);

        // L->base = cip->base
        build.mov(rBase, qword[cip + offsetof(CallInfo, base)]);
        build.mov(qword[rState + offsetof(lua_State, base)], rBase);

        if (nresults == 1)
        {
            // Opportunistically copy the result we expected from (L->top - results)
            build.mov(vali, qword[rState + offsetof(lua_State, top)]);
            build.shl(results, kTValueSizeLog2);
            build.sub(vali, qwordReg(results));
            build.vmovups(xmm0, xmmword[vali]);
            build.vmovups(luauReg(ra), xmm0);

            Label skipnil;

            // If there was no result, override the value with 'nil'
            build.test(results, results);
            build.jcc(ConditionX64::NotZero, skipnil);
            build.mov(luauRegTag(ra), LUA_TNIL);
            build.setLabel(skipnil);
        }

        // L->ci = cip
        build.mov(qword[rState + offsetof(lua_State, ci)], cip);

        // L->top = cip->top
        build.mov(rax, qword[cip + offsetof(CallInfo, top)]);
        build.mov(qword[rState + offsetof(lua_State, top)], rax);
    }
}

void emitInstReturn(AssemblyBuilderX64& build, ModuleHelpers& helpers, const Instruction* pc, int pcpos)
{
    emitInterrupt(build, pcpos);

    int ra = LUAU_INSN_A(*pc);
    int b = LUAU_INSN_B(*pc) - 1;

    RegisterX64 ci = r8;
    RegisterX64 cip = r9;
    RegisterX64 res = rdi;
    RegisterX64 nresults = esi;

    build.mov(ci, qword[rState + offsetof(lua_State, ci)]);
    build.lea(cip, addr[ci - sizeof(CallInfo)]);

    // res = ci->func; note: we assume CALL always puts func+args and expects results to start at func
    build.mov(res, qword[ci + offsetof(CallInfo, func)]);
    // nresults = ci->nresults
    build.mov(nresults, dword[ci + offsetof(CallInfo, nresults)]);

    {
        Label skipResultCopy;

        RegisterX64 counter = ecx;

        if (b == 0)
        {
            // Our instruction doesn't have any results, so just fill results expected in parent with 'nil'
            build.test(nresults, nresults);                     // test here will set SF=1 for a negative number, ZF=1 for zero and OF=0
            build.jcc(ConditionX64::LessEqual, skipResultCopy); // jle jumps if SF != OF or ZF == 1

            build.mov(counter, nresults);

            Label repeatNilLoop = build.setLabel();
            build.mov(dword[res + offsetof(TValue, tt)], LUA_TNIL);
            build.add(res, sizeof(TValue));
            build.dec(counter);
            build.jcc(ConditionX64::NotZero, repeatNilLoop);
        }
        else if (b == 1)
        {
            // Try setting our 1 result
            build.test(nresults, nresults);
            build.jcc(ConditionX64::Zero, skipResultCopy);

            build.lea(counter, addr[nresults - 1]);

            build.vmovups(xmm0, luauReg(ra));
            build.vmovups(xmmword[res], xmm0);
            build.add(res, sizeof(TValue));

            // Fill the rest of the expected results with 'nil'
            build.test(counter, counter);                       // test here will set SF=1 for a negative number, ZF=1 for zero and OF=0
            build.jcc(ConditionX64::LessEqual, skipResultCopy); // jle jumps if SF != OF or ZF == 1

            Label repeatNilLoop = build.setLabel();
            build.mov(dword[res + offsetof(TValue, tt)], LUA_TNIL);
            build.add(res, sizeof(TValue));
            build.dec(counter);
            build.jcc(ConditionX64::NotZero, repeatNilLoop);
        }
        else
        {
            RegisterX64 vali = rax;
            RegisterX64 valend = rdx;

            // Copy return values into parent stack (but only up to nresults!)
            build.test(nresults, nresults);
            build.jcc(ConditionX64::Zero, skipResultCopy);

            // vali = ra
            build.lea(vali, luauRegAddress(ra));

            // Copy as much as possible for MULTRET calls, and only as much as needed otherwise
            if (b == LUA_MULTRET)
                build.mov(valend, qword[rState + offsetof(lua_State, top)]); // valend = L->top
            else
                build.lea(valend, luauRegAddress(ra + b)); // valend = ra + b

            build.mov(counter, nresults);

            Label repeatValueLoop, exitValueLoop;

            build.setLabel(repeatValueLoop);
            build.cmp(vali, valend);
            build.jcc(ConditionX64::NotBelow, exitValueLoop);

            build.vmovups(xmm0, xmmword[vali]);
            build.vmovups(xmmword[res], xmm0);
            build.add(vali, sizeof(TValue));
            build.add(res, sizeof(TValue));
            build.dec(counter);
            build.jcc(ConditionX64::NotZero, repeatValueLoop);

            build.setLabel(exitValueLoop);

            // Fill the rest of the expected results with 'nil'
            build.test(counter, counter);                       // test here will set SF=1 for a negative number, ZF=1 for zero and OF=0
            build.jcc(ConditionX64::LessEqual, skipResultCopy); // jle jumps if SF != OF or ZF == 1

            Label repeatNilLoop = build.setLabel();
            build.mov(dword[res + offsetof(TValue, tt)], LUA_TNIL);
            build.add(res, sizeof(TValue));
            build.dec(counter);
            build.jcc(ConditionX64::NotZero, repeatNilLoop);
        }

        build.setLabel(skipResultCopy);
    }

    build.mov(qword[rState + offsetof(lua_State, ci)], cip);     // L->ci = cip
    build.mov(rBase, qword[cip + offsetof(CallInfo, base)]);     // sync base = L->base while we have a chance
    build.mov(qword[rState + offsetof(lua_State, base)], rBase); // L->base = cip->base

    // Start with result for LUA_MULTRET/exit value
    build.mov(qword[rState + offsetof(lua_State, top)], res); // L->top = res

    // Unlikely, but this might be the last return from VM
    build.test(byte[ci + offsetof(CallInfo, flags)], LUA_CALLINFO_RETURN);
    build.jcc(ConditionX64::NotZero, helpers.exitNoContinueVm);

    Label skipFixedRetTop;
    build.test(nresults, nresults);                 // test here will set SF=1 for a negative number and it always sets OF to 0
    build.jcc(ConditionX64::Less, skipFixedRetTop); // jl jumps if SF != OF
    build.mov(rax, qword[cip + offsetof(CallInfo, top)]);
    build.mov(qword[rState + offsetof(lua_State, top)], rax); // L->top = cip->top
    build.setLabel(skipFixedRetTop);

    // Returning back to the previous function is a bit tricky
    // Registers alive: r9 (cip)
    RegisterX64 proto = rcx;
    RegisterX64 execdata = rbx;

    // Change closure
    build.mov(rax, qword[cip + offsetof(CallInfo, func)]);
    build.mov(rax, qword[rax + offsetof(TValue, value.gc)]);
    build.mov(sClosure, rax);

    build.mov(proto, qword[rax + offsetof(Closure, l.p)]);

    build.mov(execdata, qword[proto + offsetofProtoExecData]);
    build.test(execdata, execdata);
    build.jcc(ConditionX64::Zero, helpers.exitContinueVm); // Continue in interpreter if function has no native data

    // Change constants
    build.mov(rConstants, qword[proto + offsetof(Proto, k)]);

    // Change code
    build.mov(rdx, qword[proto + offsetof(Proto, code)]);
    build.mov(sCode, rdx);

    build.mov(rax, qword[cip + offsetof(CallInfo, savedpc)]);

    // To get instruction index from instruction pointer, we need to divide byte offset by 4
    // But we will actually need to scale instruction index by 8 back to byte offset later so it cancels out
    build.sub(rax, rdx);

    // Get new instruction location and jump to it
    build.mov(rdx, qword[execdata + offsetof(NativeProto, instTargets)]);
    build.jmp(qword[rdx + rax * 2]);
}

void emitInstJump(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr)
{
    build.jmp(labelarr[pcpos + 1 + LUAU_INSN_D(*pc)]);
}

void emitInstJumpBack(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr)
{
    emitInterrupt(build, pcpos);

    build.jmp(labelarr[pcpos + 1 + LUAU_INSN_D(*pc)]);
}

void emitInstJumpIf(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, bool not_)
{
    int ra = LUAU_INSN_A(*pc);

    Label& target = labelarr[pcpos + 1 + LUAU_INSN_D(*pc)];
    Label& exit = labelarr[pcpos + 1];

    if (not_)
        jumpIfFalsy(build, ra, target, exit);
    else
        jumpIfTruthy(build, ra, target, exit);
}

void emitInstJumpIfEq(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, bool not_, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = pc[1];

    Label& target = labelarr[pcpos + 1 + LUAU_INSN_D(*pc)];
    Label& exit = labelarr[pcpos + 2];

    build.mov(eax, luauRegTag(ra));
    build.cmp(eax, luauRegTag(rb));
    build.jcc(ConditionX64::NotEqual, not_ ? target : exit);

    // fast-path: number
    build.cmp(eax, LUA_TNUMBER);
    build.jcc(ConditionX64::NotEqual, fallback);

    jumpOnNumberCmp(build, xmm0, luauRegValue(ra), luauRegValue(rb), ConditionX64::NotEqual, not_ ? target : exit);

    if (!not_)
        build.jmp(target);
}

void emitInstJumpIfEqFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, bool not_)
{
    Label& target = labelarr[pcpos + 1 + LUAU_INSN_D(*pc)];

    emitSetSavedPc(build, pcpos + 1);
    jumpOnAnyCmpFallback(build, LUAU_INSN_A(*pc), pc[1], not_ ? ConditionX64::NotEqual : ConditionX64::Equal, target);
}

void emitInstJumpIfCond(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, ConditionX64 cond, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = pc[1];

    Label& target = labelarr[pcpos + 1 + LUAU_INSN_D(*pc)];

    // fast-path: number
    jumpIfTagIsNot(build, ra, LUA_TNUMBER, fallback);
    jumpIfTagIsNot(build, rb, LUA_TNUMBER, fallback);

    jumpOnNumberCmp(build, xmm0, luauRegValue(ra), luauRegValue(rb), cond, target);
}

void emitInstJumpIfCondFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, ConditionX64 cond)
{
    Label& target = labelarr[pcpos + 1 + LUAU_INSN_D(*pc)];

    emitSetSavedPc(build, pcpos + 1);
    jumpOnAnyCmpFallback(build, LUAU_INSN_A(*pc), pc[1], cond, target);
}

void emitInstJumpX(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr)
{
    emitInterrupt(build, pcpos);

    build.jmp(labelarr[pcpos + 1 + LUAU_INSN_E(*pc)]);
}

void emitInstJumpxEqNil(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr)
{
    int ra = LUAU_INSN_A(*pc);
    bool not_ = (pc[1] & 0x80000000) != 0;

    Label& target = labelarr[pcpos + 1 + LUAU_INSN_D(*pc)];

    build.cmp(luauRegTag(ra), LUA_TNIL);
    build.jcc(not_ ? ConditionX64::NotEqual : ConditionX64::Equal, target);
}

void emitInstJumpxEqB(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr)
{
    int ra = LUAU_INSN_A(*pc);
    uint32_t aux = pc[1];
    bool not_ = (aux & 0x80000000) != 0;

    Label& target = labelarr[pcpos + 1 + LUAU_INSN_D(*pc)];
    Label& exit = labelarr[pcpos + 2];

    jumpIfTagIsNot(build, ra, LUA_TBOOLEAN, not_ ? target : exit);

    build.test(luauRegValueInt(ra), 1);
    build.jcc((aux & 0x1) ^ not_ ? ConditionX64::NotZero : ConditionX64::Zero, target);
}

void emitInstJumpxEqN(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos, Label* labelarr)
{
    int ra = LUAU_INSN_A(*pc);
    uint32_t aux = pc[1];
    bool not_ = (aux & 0x80000000) != 0;
    TValue kv = k[aux & 0xffffff];

    Label& target = labelarr[pcpos + 1 + LUAU_INSN_D(*pc)];
    Label& exit = labelarr[pcpos + 2];

    jumpIfTagIsNot(build, ra, LUA_TNUMBER, not_ ? target : exit);

    if (not_)
    {
        jumpOnNumberCmp(build, xmm0, luauRegValue(ra), build.f64(kv.value.n), ConditionX64::NotEqual, target);
    }
    else
    {
        // Compact equality check requires two labels, so it's not supported in generic 'jumpOnNumberCmp'
        build.vmovsd(xmm0, luauRegValue(ra));
        build.vucomisd(xmm0, build.f64(kv.value.n));
        build.jcc(ConditionX64::Parity, exit); // We first have to check PF=1 for NaN operands, because it also sets ZF=1
        build.jcc(ConditionX64::Zero, target); // Now that NaN is out of the way, we can check ZF=1 for equality
    }
}

void emitInstJumpxEqS(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr)
{
    int ra = LUAU_INSN_A(*pc);
    uint32_t aux = pc[1];
    bool not_ = (aux & 0x80000000) != 0;

    Label& target = labelarr[pcpos + 1 + LUAU_INSN_D(*pc)];
    Label& exit = labelarr[pcpos + 2];

    jumpIfTagIsNot(build, ra, LUA_TSTRING, not_ ? target : exit);

    build.mov(rax, luauRegValue(ra));
    build.cmp(rax, luauConstantValue(aux & 0xffffff));
    build.jcc(not_ ? ConditionX64::NotEqual : ConditionX64::Equal, target);
}

static void emitInstBinaryNumeric(AssemblyBuilderX64& build, int ra, int rb, int rc, OperandX64 opc, TMS tm, Label& fallback)
{
    jumpIfTagIsNot(build, rb, LUA_TNUMBER, fallback);

    if (rc != -1 && rc != rb)
        jumpIfTagIsNot(build, rc, LUA_TNUMBER, fallback);

    // fast-path: number
    build.vmovsd(xmm0, luauRegValue(rb));

    switch (tm)
    {
    case TM_ADD:
        build.vaddsd(xmm0, xmm0, opc);
        break;
    case TM_SUB:
        build.vsubsd(xmm0, xmm0, opc);
        break;
    case TM_MUL:
        build.vmulsd(xmm0, xmm0, opc);
        break;
    case TM_DIV:
        build.vdivsd(xmm0, xmm0, opc);
        break;
    case TM_MOD:
        // This follows the implementation of 'luai_nummod' which is less precise than 'fmod' for better performance
        build.vmovsd(xmm1, opc);
        build.vdivsd(xmm2, xmm0, xmm1);
        build.vroundsd(xmm2, xmm2, xmm2, RoundingModeX64::RoundToNegativeInfinity);
        build.vmulsd(xmm1, xmm2, xmm1);
        build.vsubsd(xmm0, xmm0, xmm1);
        break;
    case TM_POW:
        build.vmovsd(xmm1, luauRegValue(rc));
        build.call(qword[rNativeContext + offsetof(NativeContext, libm_pow)]);
        break;
    default:
        LUAU_ASSERT(!"unsupported binary op");
    }

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != rb && ra != rc)
        build.mov(luauRegTag(ra), LUA_TNUMBER);
}

void emitInstBinary(AssemblyBuilderX64& build, const Instruction* pc, TMS tm, Label& fallback)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), LUAU_INSN_C(*pc), luauRegValue(LUAU_INSN_C(*pc)), tm, fallback);
}

void emitInstBinaryFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, TMS tm)
{
    emitSetSavedPc(build, pcpos + 1);
    callArithHelper(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), luauRegAddress(LUAU_INSN_C(*pc)), tm);
}

void emitInstBinaryK(AssemblyBuilderX64& build, const Instruction* pc, TMS tm, Label& fallback)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), -1, luauConstantValue(LUAU_INSN_C(*pc)), tm, fallback);
}

void emitInstBinaryKFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, TMS tm)
{
    emitSetSavedPc(build, pcpos + 1);
    callArithHelper(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), luauConstantAddress(LUAU_INSN_C(*pc)), tm);
}

void emitInstPowK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    double kv = nvalue(&k[LUAU_INSN_C(*pc)]);

    jumpIfTagIsNot(build, rb, LUA_TNUMBER, fallback);

    // fast-path: number
    build.vmovsd(xmm0, luauRegValue(rb));

    // Specialize for a few constants, similar to how it's done in the VM
    if (kv == 2.0)
    {
        build.vmulsd(xmm0, xmm0, xmm0);
    }
    else if (kv == 0.5)
    {
        build.vsqrtsd(xmm0, xmm0, xmm0);
    }
    else if (kv == 3.0)
    {
        build.vmulsd(xmm1, xmm0, xmm0);
        build.vmulsd(xmm0, xmm0, xmm1);
    }
    else
    {
        build.vmovsd(xmm1, build.f64(kv));
        build.call(qword[rNativeContext + offsetof(NativeContext, libm_pow)]);
    }

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != rb)
        build.mov(luauRegTag(ra), LUA_TNUMBER);
}

void emitInstNot(AssemblyBuilderX64& build, const Instruction* pc)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);

    Label saveone, savezero, exit;

    jumpIfFalsy(build, rb, saveone, savezero);

    build.setLabel(savezero);
    build.mov(luauRegValueInt(ra), 0);
    build.jmp(exit);

    build.setLabel(saveone);
    build.mov(luauRegValueInt(ra), 1);

    build.setLabel(exit);
    build.mov(luauRegTag(ra), LUA_TBOOLEAN);
}

void emitInstMinus(AssemblyBuilderX64& build, const Instruction* pc, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);

    jumpIfTagIsNot(build, rb, LUA_TNUMBER, fallback);

    // fast-path: number
    build.vxorpd(xmm0, xmm0, xmm0);
    build.vsubsd(xmm0, xmm0, luauRegValue(rb));
    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != rb)
        build.mov(luauRegTag(ra), LUA_TNUMBER);
}

void emitInstMinusFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    emitSetSavedPc(build, pcpos + 1);
    callArithHelper(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), luauRegAddress(LUAU_INSN_B(*pc)), TM_UNM);
}

void emitInstLength(AssemblyBuilderX64& build, const Instruction* pc, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);

    jumpIfTagIsNot(build, rb, LUA_TTABLE, fallback);

    // fast-path: table without __len
    build.mov(rArg1, luauRegValue(rb));
    jumpIfMetatablePresent(build, rArg1, fallback);

    // First argument (Table*) is already in rArg1
    build.call(qword[rNativeContext + offsetof(NativeContext, luaH_getn)]);

    build.vcvtsi2sd(xmm0, xmm0, eax);
    build.vmovsd(luauRegValue(ra), xmm0);
    build.mov(luauRegTag(ra), LUA_TNUMBER);
}

void emitInstLengthFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    emitSetSavedPc(build, pcpos + 1);
    callLengthHelper(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc));
}

void emitInstNewTable(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& next)
{
    int ra = LUAU_INSN_A(*pc);
    int b = LUAU_INSN_B(*pc);
    uint32_t aux = pc[1];

    emitSetSavedPc(build, pcpos + 1);

    build.mov(rArg1, rState);
    build.mov(dwordReg(rArg2), aux);
    build.mov(dwordReg(rArg3), b == 0 ? 0 : 1 << (b - 1));
    build.call(qword[rNativeContext + offsetof(NativeContext, luaH_new)]);
    build.mov(luauRegValue(ra), rax);
    build.mov(luauRegTag(ra), LUA_TTABLE);

    callCheckGc(build, pcpos, /* savepc = */ false, next);
}

void emitInstDupTable(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& next)
{
    int ra = LUAU_INSN_A(*pc);

    emitSetSavedPc(build, pcpos + 1);

    build.mov(rArg1, rState);
    build.mov(rArg2, luauConstantValue(LUAU_INSN_D(*pc)));
    build.call(qword[rNativeContext + offsetof(NativeContext, luaH_clone)]);
    build.mov(luauRegValue(ra), rax);
    build.mov(luauRegTag(ra), LUA_TTABLE);

    callCheckGc(build, pcpos, /* savepc= */ false, next);
}

void emitInstSetList(AssemblyBuilderX64& build, const Instruction* pc, Label& next)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    int c = LUAU_INSN_C(*pc) - 1;
    uint32_t index = pc[1];

    OperandX64 last = index + c - 1;

    // Using non-volatile 'rbx' for dynamic 'c' value (for LUA_MULTRET) to skip later recomputation
    // We also keep 'c' scaled by sizeof(TValue) here as it helps in the loop below
    RegisterX64 cscaled = rbx;

    if (c == LUA_MULTRET)
    {
        RegisterX64 tmp = rax;

        // c = L->top - rb
        build.mov(cscaled, qword[rState + offsetof(lua_State, top)]);
        build.lea(tmp, luauRegAddress(rb));
        build.sub(cscaled, tmp); // Using byte difference

        // L->top = L->ci->top
        build.mov(tmp, qword[rState + offsetof(lua_State, ci)]);
        build.mov(tmp, qword[tmp + offsetof(CallInfo, top)]);
        build.mov(qword[rState + offsetof(lua_State, top)], tmp);

        // last = index + c - 1;
        last = edx;
        build.mov(last, dwordReg(cscaled));
        build.shr(last, kTValueSizeLog2);
        build.add(last, index - 1);
    }

    Label skipResize;

    RegisterX64 table = rax;

    build.mov(table, luauRegValue(ra));

    // Resize if h->sizearray < last
    build.cmp(dword[table + offsetof(Table, sizearray)], last);
    build.jcc(ConditionX64::NotBelow, skipResize);

    // Argument setup reordered to avoid conflicts
    LUAU_ASSERT(rArg3 != table);
    build.mov(dwordReg(rArg3), last);
    build.mov(rArg2, table);
    build.mov(rArg1, rState);
    build.call(qword[rNativeContext + offsetof(NativeContext, luaH_resizearray)]);
    build.mov(table, luauRegValue(ra)); // Reload cloberred register value

    build.setLabel(skipResize);

    RegisterX64 arrayDst = rdx;
    RegisterX64 offset = rcx;

    build.mov(arrayDst, qword[table + offsetof(Table, array)]);

    const int kUnrollSetListLimit = 4;

    if (c != LUA_MULTRET && c <= kUnrollSetListLimit)
    {
        for (int i = 0; i < c; ++i)
        {
            // setobj2t(L, &array[index + i - 1], rb + i);
            build.vmovups(xmm0, luauRegValue(rb + i));
            build.vmovups(xmmword[arrayDst + (index + i - 1) * sizeof(TValue)], xmm0);
        }
    }
    else
    {
        LUAU_ASSERT(c != 0);

        build.xor_(offset, offset);
        if (index != 1)
            build.add(arrayDst, (index - 1) * sizeof(TValue));

        Label repeatLoop, endLoop;
        OperandX64 limit = c == LUA_MULTRET ? cscaled : OperandX64(c * sizeof(TValue));

        // If c is static, we will always do at least one iteration
        if (c == LUA_MULTRET)
        {
            build.cmp(offset, limit);
            build.jcc(ConditionX64::NotBelow, endLoop);
        }

        build.setLabel(repeatLoop);

        // setobj2t(L, &array[index + i - 1], rb + i);
        build.vmovups(xmm0, xmmword[offset + rBase + rb * sizeof(TValue)]); // luauReg(rb) unwrapped to add offset
        build.vmovups(xmmword[offset + arrayDst], xmm0);

        build.add(offset, sizeof(TValue));
        build.cmp(offset, limit);
        build.jcc(ConditionX64::Below, repeatLoop);

        build.setLabel(endLoop);
    }

    callBarrierTableFast(build, table, next);
}

void emitInstGetUpval(AssemblyBuilderX64& build, const Instruction* pc)
{
    int ra = LUAU_INSN_A(*pc);
    int up = LUAU_INSN_B(*pc);

    build.mov(rax, sClosure);
    build.add(rax, offsetof(Closure, l.uprefs) + sizeof(TValue) * up);

    // uprefs[] is either an actual value, or it points to UpVal object which has a pointer to value
    Label skip;
    // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
    build.cmp(dword[rax + offsetof(TValue, tt)], LUA_TUPVAL);
    build.jcc(ConditionX64::NotEqual, skip);

    // UpVal.v points to the value (either on stack, or on heap inside each UpVal, but we can deref it unconditionally)
    build.mov(rax, qword[rax + offsetof(TValue, value.gc)]);
    build.mov(rax, qword[rax + offsetof(UpVal, v)]);

    build.setLabel(skip);

    build.vmovups(xmm0, xmmword[rax]);
    build.vmovups(luauReg(ra), xmm0);
}

void emitInstSetUpval(AssemblyBuilderX64& build, const Instruction* pc, Label& next)
{
    int ra = LUAU_INSN_A(*pc);
    int up = LUAU_INSN_B(*pc);

    RegisterX64 upval = rax;
    RegisterX64 tmp = rcx;

    build.mov(tmp, sClosure);
    build.mov(upval, qword[tmp + offsetof(Closure, l.uprefs) + sizeof(TValue) * up + offsetof(TValue, value.gc)]);

    build.mov(tmp, qword[upval + offsetof(UpVal, v)]);
    build.vmovups(xmm0, luauReg(ra));
    build.vmovups(xmmword[tmp], xmm0);

    callBarrierObject(build, tmp, upval, ra, next);
}

void emitInstCloseUpvals(AssemblyBuilderX64& build, const Instruction* pc, Label& next)
{
    int ra = LUAU_INSN_A(*pc);

    // L->openupval != 0
    build.mov(rax, qword[rState + offsetof(lua_State, openupval)]);
    build.test(rax, rax);
    build.jcc(ConditionX64::Zero, next);

    // ra <= L->openuval->v
    build.lea(rcx, addr[rBase + ra * sizeof(TValue)]);
    build.cmp(rcx, qword[rax + offsetof(UpVal, v)]);
    build.jcc(ConditionX64::Above, next);

    build.mov(rArg2, rcx);
    build.mov(rArg1, rState);
    build.call(qword[rNativeContext + offsetof(NativeContext, luaF_close)]);
}

static int emitInstFastCallN(
    AssemblyBuilderX64& build, const Instruction* pc, bool customParams, int customParamCount, OperandX64 customArgs, int pcpos, Label& fallback)
{
    int bfid = LUAU_INSN_A(*pc);
    int skip = LUAU_INSN_C(*pc);

    Instruction call = pc[skip + 1];
    LUAU_ASSERT(LUAU_INSN_OP(call) == LOP_CALL);
    int ra = LUAU_INSN_A(call);

    int nparams = customParams ? customParamCount : LUAU_INSN_B(call) - 1;
    int nresults = LUAU_INSN_C(call) - 1;
    int arg = customParams ? LUAU_INSN_B(*pc) : ra + 1;
    OperandX64 args = customParams ? customArgs : luauRegAddress(ra + 2);

    jumpIfUnsafeEnv(build, rax, fallback);

    BuiltinImplResult br = emitBuiltin(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, fallback);

    if (br.type == BuiltinImplType::UsesFallback)
    {
        if (nresults == LUA_MULTRET)
        {
            // L->top = ra + n;
            build.lea(rax, addr[rBase + (ra + br.actualResultCount) * sizeof(TValue)]);
            build.mov(qword[rState + offsetof(lua_State, top)], rax);
        }
        else if (nparams == LUA_MULTRET)
        {
            // L->top = L->ci->top;
            build.mov(rax, qword[rState + offsetof(lua_State, ci)]);
            build.mov(rax, qword[rax + offsetof(CallInfo, top)]);
            build.mov(qword[rState + offsetof(lua_State, top)], rax);
        }

        return skip; // Return fallback instruction sequence length
    }

    // TODO: we can skip saving pc for some well-behaved builtins which we didn't inline
    emitSetSavedPc(build, pcpos + 1); // uses rax/rdx

    build.mov(rax, qword[rNativeContext + offsetof(NativeContext, luauF_table) + bfid * sizeof(luau_FastFunction)]);

    // 5th parameter (args) is left unset for LOP_FASTCALL1
    if (args.cat == CategoryX64::mem)
    {
        if (build.abi == ABIX64::Windows)
        {
            build.lea(rcx, args);
            build.mov(sArg5, rcx);
        }
        else
        {
            build.lea(rArg5, args);
        }
    }

    if (nparams == LUA_MULTRET)
    {
        // L->top - (ra + 1)
        RegisterX64 reg = (build.abi == ABIX64::Windows) ? rcx : rArg6;
        build.mov(reg, qword[rState + offsetof(lua_State, top)]);
        build.lea(rdx, addr[rBase + (ra + 1) * sizeof(TValue)]);
        build.sub(reg, rdx);
        build.shr(reg, kTValueSizeLog2);

        if (build.abi == ABIX64::Windows)
            build.mov(sArg6, reg);
    }
    else
    {
        if (build.abi == ABIX64::Windows)
            build.mov(sArg6, nparams);
        else
            build.mov(rArg6, nparams);
    }

    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegAddress(ra));
    build.lea(rArg3, luauRegAddress(arg));
    build.mov(dwordReg(rArg4), nresults);

    build.call(rax);

    build.test(eax, eax);                    // test here will set SF=1 for a negative number and it always sets OF to 0
    build.jcc(ConditionX64::Less, fallback); // jl jumps if SF != OF

    if (nresults == LUA_MULTRET)
    {
        // L->top = ra + n;
        build.shl(rax, kTValueSizeLog2);
        build.lea(rax, addr[rBase + rax + ra * sizeof(TValue)]);
        build.mov(qword[rState + offsetof(lua_State, top)], rax);
    }
    else if (nparams == LUA_MULTRET)
    {
        // L->top = L->ci->top;
        build.mov(rax, qword[rState + offsetof(lua_State, ci)]);
        build.mov(rax, qword[rax + offsetof(CallInfo, top)]);
        build.mov(qword[rState + offsetof(lua_State, top)], rax);
    }

    return skip; // Return fallback instruction sequence length
}

int emitInstFastCall1(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback)
{
    return emitInstFastCallN(build, pc, /* customParams */ true, /* customParamCount */ 1, /* customArgs */ 0, pcpos, fallback);
}

int emitInstFastCall2(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback)
{
    return emitInstFastCallN(build, pc, /* customParams */ true, /* customParamCount */ 2, /* customArgs */ luauRegAddress(pc[1]), pcpos, fallback);
}

int emitInstFastCall2K(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback)
{
    return emitInstFastCallN(
        build, pc, /* customParams */ true, /* customParamCount */ 2, /* customArgs */ luauConstantAddress(pc[1]), pcpos, fallback);
}

int emitInstFastCall(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback)
{
    return emitInstFastCallN(build, pc, /* customParams */ false, /* customParamCount */ 0, /* customArgs */ 0, pcpos, fallback);
}

void emitInstForNPrep(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& loopExit)
{
    int ra = LUAU_INSN_A(*pc);

    Label tryConvert, exit;

    jumpIfTagIsNot(build, ra + 0, LUA_TNUMBER, tryConvert);
    jumpIfTagIsNot(build, ra + 1, LUA_TNUMBER, tryConvert);
    jumpIfTagIsNot(build, ra + 2, LUA_TNUMBER, tryConvert);

    // After successful conversion of arguments to number, we return here
    Label retry = build.setLabel();

    RegisterX64 limit = xmm0;
    RegisterX64 step = xmm1;
    RegisterX64 idx = xmm2;
    RegisterX64 zero = xmm3;

    build.vxorpd(zero, xmm0, xmm0);
    build.vmovsd(limit, luauRegValue(ra + 0));
    build.vmovsd(step, luauRegValue(ra + 1));
    build.vmovsd(idx, luauRegValue(ra + 2));

    Label reverse;

    // step <= 0
    jumpOnNumberCmp(build, noreg, step, zero, ConditionX64::LessEqual, reverse);

    // TODO: target branches can probably be arranged better, but we need tests for NaN behavior preservation
    // false: idx <= limit
    jumpOnNumberCmp(build, noreg, idx, limit, ConditionX64::LessEqual, exit);
    build.jmp(loopExit);

    // true: limit <= idx
    build.setLabel(reverse);
    jumpOnNumberCmp(build, noreg, limit, idx, ConditionX64::LessEqual, exit);
    build.jmp(loopExit);

    // TOOD: place at the end of the function
    build.setLabel(tryConvert);
    emitSetSavedPc(build, pcpos + 1);
    callPrepareForN(build, ra + 0, ra + 1, ra + 2);
    build.jmp(retry);

    build.setLabel(exit);
}

void emitInstForNLoop(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& loopRepeat, Label& loopExit)
{
    emitInterrupt(build, pcpos);

    int ra = LUAU_INSN_A(*pc);

    RegisterX64 limit = xmm0;
    RegisterX64 step = xmm1;
    RegisterX64 idx = xmm2;
    RegisterX64 zero = xmm3;

    build.vxorpd(zero, xmm0, xmm0);
    build.vmovsd(limit, luauRegValue(ra + 0));
    build.vmovsd(step, luauRegValue(ra + 1));
    build.vmovsd(idx, luauRegValue(ra + 2));
    build.vaddsd(idx, idx, step);
    build.vmovsd(luauRegValue(ra + 2), idx);

    Label reverse;

    // step <= 0
    jumpOnNumberCmp(build, noreg, step, zero, ConditionX64::LessEqual, reverse);

    // false: idx <= limit
    jumpOnNumberCmp(build, noreg, idx, limit, ConditionX64::LessEqual, loopRepeat);
    build.jmp(loopExit);

    // true: limit <= idx
    build.setLabel(reverse);
    jumpOnNumberCmp(build, noreg, limit, idx, ConditionX64::LessEqual, loopRepeat);
}

void emitinstForGLoop(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& loopRepeat, Label& loopExit, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int aux = pc[1];

    emitInterrupt(build, pcpos);

    // fast-path: builtin table iteration
    jumpIfTagIsNot(build, ra, LUA_TNIL, fallback);

    // Registers are chosen in this way to simplify fallback code for the node part
    RegisterX64 table = rArg2;
    RegisterX64 index = rArg3;
    RegisterX64 elemPtr = rax;

    build.mov(table, luauRegValue(ra + 1));
    build.mov(index, luauRegValue(ra + 2));

    // &array[index]
    build.mov(dwordReg(elemPtr), dwordReg(index));
    build.shl(dwordReg(elemPtr), kTValueSizeLog2);
    build.add(elemPtr, qword[table + offsetof(Table, array)]);

    // Clear extra variables since we might have more than two
    for (int i = 2; i < aux; ++i)
        build.mov(luauRegTag(ra + 3 + i), LUA_TNIL);

    // ipairs-style traversal is terminated early when array part ends of nil array element is encountered
    bool isIpairsIter = aux < 0;

    Label skipArray, skipArrayNil;

    // First we advance index through the array portion
    // while (unsigned(index) < unsigned(sizearray))
    Label arrayLoop = build.setLabel();
    build.cmp(dwordReg(index), dword[table + offsetof(Table, sizearray)]);
    build.jcc(ConditionX64::NotBelow, isIpairsIter ? loopExit : skipArray);

    // If element is nil, we increment the index; if it's not, we still need 'index + 1' inside
    build.inc(index);

    build.cmp(dword[elemPtr + offsetof(TValue, tt)], LUA_TNIL);
    build.jcc(ConditionX64::Equal, isIpairsIter ? loopExit : skipArrayNil);

    // setpvalue(ra + 2, reinterpret_cast<void*>(uintptr_t(index + 1)));
    build.mov(luauRegValue(ra + 2), index);
    // Tag should already be set to lightuserdata

    // setnvalue(ra + 3, double(index + 1));
    build.vcvtsi2sd(xmm0, xmm0, dwordReg(index));
    build.vmovsd(luauRegValue(ra + 3), xmm0);
    build.mov(luauRegTag(ra + 3), LUA_TNUMBER);

    // setobj2s(L, ra + 4, e);
    setLuauReg(build, xmm2, ra + 4, xmmword[elemPtr]);

    build.jmp(loopRepeat);

    if (!isIpairsIter)
    {
        build.setLabel(skipArrayNil);

        // Index already incremented, advance to next array element
        build.add(elemPtr, sizeof(TValue));
        build.jmp(arrayLoop);

        build.setLabel(skipArray);

        // Call helper to assign next node value or to signal loop exit
        build.mov(rArg1, rState);
        // rArg2 and rArg3 are already set
        build.lea(rArg4, luauRegAddress(ra));
        build.call(qword[rNativeContext + offsetof(NativeContext, forgLoopNodeIter)]);
        build.test(al, al);
        build.jcc(ConditionX64::NotZero, loopRepeat);
    }
}

void emitinstForGLoopFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& loopRepeat)
{
    int ra = LUAU_INSN_A(*pc);
    int aux = pc[1];

    emitSetSavedPc(build, pcpos + 1);

    build.mov(rArg1, rState);
    build.mov(dwordReg(rArg2), ra);
    build.mov(dwordReg(rArg3), aux);
    build.call(qword[rNativeContext + offsetof(NativeContext, forgLoopNonTableFallback)]);
    emitUpdateBase(build);
    build.test(al, al);
    build.jcc(ConditionX64::NotZero, loopRepeat);
}

void emitInstForGPrepNext(AssemblyBuilderX64& build, const Instruction* pc, Label& target, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);

    // fast-path: pairs/next
    jumpIfUnsafeEnv(build, rax, fallback);
    jumpIfTagIsNot(build, ra + 1, LUA_TTABLE, fallback);
    jumpIfTagIsNot(build, ra + 2, LUA_TNIL, fallback);

    build.mov(luauRegTag(ra), LUA_TNIL);

    // setpvalue(ra + 2, reinterpret_cast<void*>(uintptr_t(0)));
    build.mov(luauRegValue(ra + 2), 0);
    build.mov(luauRegTag(ra + 2), LUA_TLIGHTUSERDATA);

    build.jmp(target);
}

void emitInstForGPrepInext(AssemblyBuilderX64& build, const Instruction* pc, Label& target, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);

    // fast-path: ipairs/inext
    jumpIfUnsafeEnv(build, rax, fallback);
    jumpIfTagIsNot(build, ra + 1, LUA_TTABLE, fallback);
    jumpIfTagIsNot(build, ra + 2, LUA_TNUMBER, fallback);

    build.vxorpd(xmm0, xmm0, xmm0);
    build.vmovsd(xmm1, luauRegValue(ra + 2));
    jumpOnNumberCmp(build, noreg, xmm0, xmm1, ConditionX64::NotEqual, fallback);

    build.mov(luauRegTag(ra), LUA_TNIL);

    // setpvalue(ra + 2, reinterpret_cast<void*>(uintptr_t(0)));
    build.mov(luauRegValue(ra + 2), 0);
    build.mov(luauRegTag(ra + 2), LUA_TLIGHTUSERDATA);

    build.jmp(target);
}

void emitInstForGPrepXnextFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& target)
{
    int ra = LUAU_INSN_A(*pc);

    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegAddress(ra));
    build.mov(dwordReg(rArg3), pcpos + 1);
    build.call(qword[rNativeContext + offsetof(NativeContext, forgPrepXnextFallback)]);
    build.jmp(target);
}

static void emitInstAndX(AssemblyBuilderX64& build, int ra, int rb, OperandX64 c)
{
    Label target, fallthrough;
    jumpIfFalsy(build, rb, target, fallthrough);

    build.setLabel(fallthrough);

    build.vmovups(xmm0, c);
    build.vmovups(luauReg(ra), xmm0);

    if (ra == rb)
    {
        build.setLabel(target);
    }
    else
    {
        Label exit;
        build.jmp(exit);

        build.setLabel(target);

        build.vmovups(xmm0, luauReg(rb));
        build.vmovups(luauReg(ra), xmm0);

        build.setLabel(exit);
    }
}

void emitInstAnd(AssemblyBuilderX64& build, const Instruction* pc)
{
    emitInstAndX(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), luauReg(LUAU_INSN_C(*pc)));
}

void emitInstAndK(AssemblyBuilderX64& build, const Instruction* pc)
{
    emitInstAndX(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), luauConstant(LUAU_INSN_C(*pc)));
}

static void emitInstOrX(AssemblyBuilderX64& build, int ra, int rb, OperandX64 c)
{
    Label target, fallthrough;
    jumpIfTruthy(build, rb, target, fallthrough);

    build.setLabel(fallthrough);

    build.vmovups(xmm0, c);
    build.vmovups(luauReg(ra), xmm0);

    if (ra == rb)
    {
        build.setLabel(target);
    }
    else
    {
        Label exit;
        build.jmp(exit);

        build.setLabel(target);

        build.vmovups(xmm0, luauReg(rb));
        build.vmovups(luauReg(ra), xmm0);

        build.setLabel(exit);
    }
}

void emitInstOr(AssemblyBuilderX64& build, const Instruction* pc)
{
    emitInstOrX(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), luauReg(LUAU_INSN_C(*pc)));
}

void emitInstOrK(AssemblyBuilderX64& build, const Instruction* pc)
{
    emitInstOrX(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), luauConstant(LUAU_INSN_C(*pc)));
}

void emitInstGetTableN(AssemblyBuilderX64& build, const Instruction* pc, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    int c = LUAU_INSN_C(*pc);

    jumpIfTagIsNot(build, rb, LUA_TTABLE, fallback);

    RegisterX64 table = rcx;
    build.mov(table, luauRegValue(rb));

    // unsigned(c) < unsigned(h->sizearray)
    build.cmp(dword[table + offsetof(Table, sizearray)], c);
    build.jcc(ConditionX64::BelowEqual, fallback);

    jumpIfMetatablePresent(build, table, fallback);

    build.mov(rax, qword[table + offsetof(Table, array)]);
    setLuauReg(build, xmm0, ra, xmmword[rax + c * sizeof(TValue)]);
}

void emitInstGetTableNFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    emitSetSavedPc(build, pcpos + 1);

    TValue n;
    setnvalue(&n, LUAU_INSN_C(*pc) + 1);
    callGetTable(build, LUAU_INSN_B(*pc), build.bytes(&n, sizeof(n)), LUAU_INSN_A(*pc));
}

void emitInstSetTableN(AssemblyBuilderX64& build, const Instruction* pc, Label& next, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    int c = LUAU_INSN_C(*pc);

    jumpIfTagIsNot(build, rb, LUA_TTABLE, fallback);

    RegisterX64 table = rcx;
    build.mov(table, luauRegValue(rb));

    // unsigned(c) < unsigned(h->sizearray)
    build.cmp(dword[table + offsetof(Table, sizearray)], c);
    build.jcc(ConditionX64::BelowEqual, fallback);

    jumpIfMetatablePresent(build, table, fallback);
    jumpIfTableIsReadOnly(build, table, fallback);

    // setobj2t(L, &h->array[c], ra);
    build.mov(rax, qword[table + offsetof(Table, array)]);
    build.vmovups(xmm0, luauReg(ra));
    build.vmovups(xmmword[rax + c * sizeof(TValue)], xmm0);

    callBarrierTable(build, rax, table, ra, next);
}

void emitInstSetTableNFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    emitSetSavedPc(build, pcpos + 1);

    TValue n;
    setnvalue(&n, LUAU_INSN_C(*pc) + 1);
    callSetTable(build, LUAU_INSN_B(*pc), build.bytes(&n, sizeof(n)), LUAU_INSN_A(*pc));
}

void emitInstGetTable(AssemblyBuilderX64& build, const Instruction* pc, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    int rc = LUAU_INSN_C(*pc);

    jumpIfTagIsNot(build, rb, LUA_TTABLE, fallback);
    jumpIfTagIsNot(build, rc, LUA_TNUMBER, fallback);

    // fast-path: table with a number index
    RegisterX64 table = rcx;
    build.mov(table, luauRegValue(rb));

    RegisterX64 intIndex = eax;
    RegisterX64 fpIndex = xmm0;
    build.vmovsd(fpIndex, luauRegValue(rc));
    convertNumberToIndexOrJump(build, xmm1, fpIndex, intIndex, fallback);

    // index - 1
    build.dec(intIndex);

    // unsigned(index - 1) < unsigned(h->sizearray)
    build.cmp(dword[table + offsetof(Table, sizearray)], intIndex);
    build.jcc(ConditionX64::BelowEqual, fallback);

    jumpIfMetatablePresent(build, table, fallback);

    // setobj2s(L, ra, &h->array[unsigned(index - 1)]);
    build.mov(rdx, qword[table + offsetof(Table, array)]);
    build.shl(intIndex, kTValueSizeLog2);
    setLuauReg(build, xmm0, ra, xmmword[rdx + rax]);
}

void emitInstGetTableFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    emitSetSavedPc(build, pcpos + 1);
    callGetTable(build, LUAU_INSN_B(*pc), luauRegAddress(LUAU_INSN_C(*pc)), LUAU_INSN_A(*pc));
}

void emitInstSetTable(AssemblyBuilderX64& build, const Instruction* pc, Label& next, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    int rc = LUAU_INSN_C(*pc);

    jumpIfTagIsNot(build, rb, LUA_TTABLE, fallback);
    jumpIfTagIsNot(build, rc, LUA_TNUMBER, fallback);

    // fast-path: table with a number index
    RegisterX64 table = rcx;
    build.mov(table, luauRegValue(rb));

    RegisterX64 intIndex = eax;
    RegisterX64 fpIndex = xmm0;
    build.vmovsd(fpIndex, luauRegValue(rc));
    convertNumberToIndexOrJump(build, xmm1, fpIndex, intIndex, fallback);

    // index - 1
    build.dec(intIndex);

    // unsigned(index - 1) < unsigned(h->sizearray)
    build.cmp(dword[table + offsetof(Table, sizearray)], intIndex);
    build.jcc(ConditionX64::BelowEqual, fallback);

    jumpIfMetatablePresent(build, table, fallback);
    jumpIfTableIsReadOnly(build, table, fallback);

    // setobj2t(L, &h->array[unsigned(index - 1)], ra);
    build.mov(rdx, qword[table + offsetof(Table, array)]);
    build.shl(intIndex, kTValueSizeLog2);
    build.vmovups(xmm0, luauReg(ra));
    build.vmovups(xmmword[rdx + rax], xmm0);

    callBarrierTable(build, rdx, table, ra, next);
}

void emitInstSetTableFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    emitSetSavedPc(build, pcpos + 1);
    callSetTable(build, LUAU_INSN_B(*pc), luauRegAddress(LUAU_INSN_C(*pc)), LUAU_INSN_A(*pc));
}

void emitInstGetImport(AssemblyBuilderX64& build, const Instruction* pc, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int k = LUAU_INSN_D(*pc);

    jumpIfUnsafeEnv(build, rax, fallback);

    // note: if import failed, k[] is nil; we could check this during codegen, but we instead use runtime fallback
    // this allows us to handle ahead-of-time codegen smoothly when an import fails to resolve at runtime
    build.cmp(luauConstantTag(k), LUA_TNIL);
    build.jcc(ConditionX64::Equal, fallback);

    build.vmovups(xmm0, luauConstant(k));
    build.vmovups(luauReg(ra), xmm0);
}

void emitInstGetImportFallback(AssemblyBuilderX64& build, int ra, uint32_t aux)
{
    build.mov(rax, sClosure);

    // luaV_getimport(L, cl->env, k, aux, /* propagatenil= */ false)
    build.mov(rArg1, rState);
    build.mov(rArg2, qword[rax + offsetof(Closure, env)]);
    build.mov(rArg3, rConstants);
    build.mov(dwordReg(rArg4), aux);

    if (build.abi == ABIX64::Windows)
        build.mov(sArg5, 0);
    else
        build.xor_(rArg5, rArg5);

    build.call(qword[rNativeContext + offsetof(NativeContext, luaV_getimport)]);

    emitUpdateBase(build);

    // setobj2s(L, ra, L->top - 1)
    build.mov(rax, qword[rState + offsetof(lua_State, top)]);
    build.sub(rax, sizeof(TValue));
    build.vmovups(xmm0, xmmword[rax]);
    build.vmovups(luauReg(ra), xmm0);

    // L->top--
    build.mov(qword[rState + offsetof(lua_State, top)], rax);
}

void emitInstGetTableKS(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    uint32_t aux = pc[1];

    jumpIfTagIsNot(build, rb, LUA_TTABLE, fallback);

    RegisterX64 table = rcx;
    build.mov(table, luauRegValue(rb));
    RegisterX64 node = rdx;
    getTableNodeAtCachedSlot(build, rax, node, table, pcpos);

    jumpIfNodeKeyNotInExpectedSlot(build, rax, node, luauConstantValue(aux), fallback);

    setLuauReg(build, xmm0, ra, luauNodeValue(node));
}

void emitInstSetTableKS(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& next, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    uint32_t aux = pc[1];

    jumpIfTagIsNot(build, rb, LUA_TTABLE, fallback);

    RegisterX64 table = rcx;
    build.mov(table, luauRegValue(rb));

    // fast-path: set value at the expected slot
    RegisterX64 node = rdx;
    getTableNodeAtCachedSlot(build, rax, node, table, pcpos);

    jumpIfNodeKeyNotInExpectedSlot(build, rax, node, luauConstantValue(aux), fallback);
    jumpIfTableIsReadOnly(build, table, fallback);

    setNodeValue(build, xmm0, luauNodeValue(node), ra);

    callBarrierTable(build, rax, table, ra, next);
}

void emitInstGetGlobal(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    uint32_t aux = pc[1];

    RegisterX64 table = rcx;
    build.mov(rax, sClosure);
    build.mov(table, qword[rax + offsetof(Closure, env)]);
    RegisterX64 node = rdx;
    getTableNodeAtCachedSlot(build, rax, node, table, pcpos);

    jumpIfNodeKeyNotInExpectedSlot(build, rax, node, luauConstantValue(aux), fallback);

    setLuauReg(build, xmm0, ra, luauNodeValue(node));
}

void emitInstSetGlobal(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& next, Label& fallback)
{
    int ra = LUAU_INSN_A(*pc);
    uint32_t aux = pc[1];

    RegisterX64 table = rcx;
    build.mov(rax, sClosure);
    build.mov(table, qword[rax + offsetof(Closure, env)]);
    RegisterX64 node = rdx;
    getTableNodeAtCachedSlot(build, rax, node, table, pcpos);

    jumpIfNodeKeyNotInExpectedSlot(build, rax, node, luauConstantValue(aux), fallback);
    jumpIfTableIsReadOnly(build, table, fallback);

    setNodeValue(build, xmm0, luauNodeValue(node), ra);

    callBarrierTable(build, rax, table, ra, next);
}

void emitInstConcat(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& next)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    int rc = LUAU_INSN_C(*pc);

    emitSetSavedPc(build, pcpos + 1);

    // luaV_concat(L, c - b + 1, c)
    build.mov(rArg1, rState);
    build.mov(dwordReg(rArg2), rc - rb + 1);
    build.mov(dwordReg(rArg3), rc);
    build.call(qword[rNativeContext + offsetof(NativeContext, luaV_concat)]);

    emitUpdateBase(build);

    // setobj2s(L, ra, base + b)
    build.vmovups(xmm0, luauReg(rb));
    build.vmovups(luauReg(ra), xmm0);

    callCheckGc(build, pcpos, /* savepc= */ false, next);
}

void emitInstCoverage(AssemblyBuilderX64& build, int pcpos)
{
    build.mov(rcx, sCode);
    build.add(rcx, pcpos * sizeof(Instruction));

    // hits = LUAU_INSN_E(*pc)
    build.mov(edx, dword[rcx]);
    build.sar(edx, 8);

    // hits = (hits < (1 << 23) - 1) ? hits + 1 : hits;
    build.xor_(eax, eax);
    build.cmp(edx, (1 << 23) - 1);
    build.setcc(ConditionX64::NotEqual, al);
    build.add(edx, eax);


    // VM_PATCH_E(pc, hits);
    build.sal(edx, 8);
    build.movzx(eax, byte[rcx]);
    build.or_(eax, edx);
    build.mov(dword[rcx], eax);
}

} // namespace CodeGen
} // namespace Luau
