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

void emitInstLoadNil(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc)
{
    int ra = LUAU_INSN_A(*pc);

    build.mov(luauRegTag(ra), LUA_TNIL);
}

void emitInstLoadB(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr)
{
    int ra = LUAU_INSN_A(*pc);

    build.mov(luauRegValue(ra), LUAU_INSN_B(*pc));
    build.mov(luauRegTag(ra), LUA_TBOOLEAN);

    if (int target = LUAU_INSN_C(*pc))
        build.jmp(labelarr[pcpos + target + 1]);
}

void emitInstLoadN(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc)
{
    int ra = LUAU_INSN_A(*pc);

    build.vmovsd(xmm0, build.f64(double(LUAU_INSN_D(*pc))));
    build.vmovsd(luauRegValue(ra), xmm0);
    build.mov(luauRegTag(ra), LUA_TNUMBER);
}

void emitInstLoadK(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, const TValue* k)
{
    int ra = LUAU_INSN_A(*pc);

    build.vmovups(xmm0, luauConstant(LUAU_INSN_D(*pc)));
    build.vmovups(luauReg(ra), xmm0);
}

void emitInstMove(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);

    build.vmovups(xmm0, luauReg(rb));
    build.vmovups(luauReg(ra), xmm0);
}

void emitInstJump(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr)
{
    build.jmp(labelarr[pcpos + LUAU_INSN_D(*pc) + 1]);
}

void emitInstJumpBack(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr)
{
    emitInterrupt(build, pcpos);

    build.jmp(labelarr[pcpos + LUAU_INSN_D(*pc) + 1]);
}

void emitInstJumpIf(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr, bool not_)
{
    int ra = LUAU_INSN_A(*pc);

    Label& target = labelarr[pcpos + LUAU_INSN_D(*pc) + 1];
    Label& exit = labelarr[pcpos + 1];

    if (not_)
        jumpIfFalsy(build, ra, target, exit);
    else
        jumpIfTruthy(build, ra, target, exit);
}

void emitInstJumpIfEq(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr, bool not_)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = pc[1];

    Label& target = labelarr[pcpos + LUAU_INSN_D(*pc) + 1];
    Label& exit = labelarr[pcpos + 2];
    Label any;

    build.mov(eax, luauRegTag(ra));
    build.cmp(eax, luauRegTag(rb));
    build.jcc(Condition::NotEqual, not_ ? target : exit);

    // fast-path: number
    build.cmp(eax, LUA_TNUMBER);
    build.jcc(Condition::NotEqual, any);

    jumpOnNumberCmp(build, xmm0, luauRegValue(ra), luauRegValue(rb), Condition::NotEqual, not_ ? target : exit);
    build.jmp(not_ ? exit : target);

    // slow-path
    // TODO: move to the end of the function
    build.setLabel(any);
    jumpOnAnyCmpFallback(build, ra, rb, not_ ? Condition::NotEqual : Condition::Equal, target, pcpos);
}

void emitInstJumpIfCond(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr, Condition cond)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = pc[1];

    Label& target = labelarr[pcpos + LUAU_INSN_D(*pc) + 1];
    Label& exit = labelarr[pcpos + 2];
    Label any;

    // fast-path: number
    jumpIfTagIsNot(build, ra, LUA_TNUMBER, any);
    jumpIfTagIsNot(build, rb, LUA_TNUMBER, any);

    jumpOnNumberCmp(build, xmm0, luauRegValue(ra), luauRegValue(rb), cond, target);
    build.jmp(exit);

    // slow-path
    // TODO: move to the end of the function
    build.setLabel(any);
    jumpOnAnyCmpFallback(build, ra, rb, cond, target, pcpos);
}

void emitInstJumpX(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr)
{
    emitInterrupt(build, pcpos);

    build.jmp(labelarr[pcpos + LUAU_INSN_E(*pc) + 1]);
}

void emitInstJumpxEqNil(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, const TValue* k, int pcpos, Label* labelarr)
{
    int ra = LUAU_INSN_A(*pc);
    bool not_ = (pc[1] & 0x80000000) != 0;

    Label& target = labelarr[pcpos + LUAU_INSN_D(*pc) + 1];

    build.cmp(luauRegTag(ra), LUA_TNIL);
    build.jcc(not_ ? Condition::NotEqual : Condition::Equal, target);
}

void emitInstJumpxEqB(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, const TValue* k, int pcpos, Label* labelarr)
{
    int ra = LUAU_INSN_A(*pc);
    uint32_t aux = pc[1];
    bool not_ = (aux & 0x80000000) != 0;

    Label& target = labelarr[pcpos + LUAU_INSN_D(*pc) + 1];
    Label& exit = labelarr[pcpos + 2];

    jumpIfTagIsNot(build, ra, LUA_TBOOLEAN, not_ ? target : exit);

    build.test(luauRegValueBoolean(ra), 1);
    build.jcc((aux & 0x1) ^ not_ ? Condition::NotZero : Condition::Zero, target);
}

void emitInstJumpxEqN(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, const TValue* k, int pcpos, Label* labelarr)
{
    int ra = LUAU_INSN_A(*pc);
    uint32_t aux = pc[1];
    bool not_ = (aux & 0x80000000) != 0;
    TValue kv = k[aux & 0xffffff];

    Label& target = labelarr[pcpos + LUAU_INSN_D(*pc) + 1];
    Label& exit = labelarr[pcpos + 2];

    jumpIfTagIsNot(build, ra, LUA_TNUMBER, not_ ? target : exit);

    if (not_)
    {
        jumpOnNumberCmp(build, xmm0, luauRegValue(ra), build.f64(kv.value.n), Condition::NotEqual, target);
    }
    else
    {
        // Compact equality check requires two labels, so it's not supported in generic 'jumpOnNumberCmp'
        build.vmovsd(xmm0, luauRegValue(ra));
        build.vucomisd(xmm0, build.f64(kv.value.n));
        build.jcc(Condition::Parity, exit); // We first have to check PF=1 for NaN operands, because it also sets ZF=1
        build.jcc(Condition::Zero, target); // Now that NaN is out of the way, we can check ZF=1 for equality
    }
}

void emitInstJumpxEqS(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, const TValue* k, int pcpos, Label* labelarr)
{
    int ra = LUAU_INSN_A(*pc);
    uint32_t aux = pc[1];
    bool not_ = (aux & 0x80000000) != 0;

    Label& target = labelarr[pcpos + LUAU_INSN_D(*pc) + 1];
    Label& exit = labelarr[pcpos + 2];

    jumpIfTagIsNot(build, ra, LUA_TSTRING, not_ ? target : exit);

    build.mov(rax, luauRegValue(ra));
    build.cmp(rax, luauConstantValue(aux & 0xffffff));
    build.jcc(not_ ? Condition::NotEqual : Condition::Equal, target);
}

static void emitInstBinaryNumeric(AssemblyBuilderX64& build, int ra, int rb, int rc, OperandX64 opc, int pcpos, TMS tm)
{
    Label common, exit;

    jumpIfTagIsNot(build, rb, LUA_TNUMBER, common);

    if (rc != -1 && rc != rb)
        jumpIfTagIsNot(build, rc, LUA_TNUMBER, common);

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

    build.jmp(exit);

    // slow-path
    // TODO: move to the end of the function
    build.setLabel(common);
    callArithHelper(build, ra, rb, opc, pcpos, tm);

    build.setLabel(exit);
}

void emitInstAdd(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), LUAU_INSN_C(*pc), luauRegValue(LUAU_INSN_C(*pc)), pcpos, TM_ADD);
}

void emitInstSub(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), LUAU_INSN_C(*pc), luauRegValue(LUAU_INSN_C(*pc)), pcpos, TM_SUB);
}

void emitInstMul(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), LUAU_INSN_C(*pc), luauRegValue(LUAU_INSN_C(*pc)), pcpos, TM_MUL);
}

void emitInstDiv(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), LUAU_INSN_C(*pc), luauRegValue(LUAU_INSN_C(*pc)), pcpos, TM_DIV);
}

void emitInstMod(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), LUAU_INSN_C(*pc), luauRegValue(LUAU_INSN_C(*pc)), pcpos, TM_MOD);
}

void emitInstPow(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), LUAU_INSN_C(*pc), luauRegValue(LUAU_INSN_C(*pc)), pcpos, TM_POW);
}

void emitInstAddK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), -1, luauConstantValue(LUAU_INSN_C(*pc)), pcpos, TM_ADD);
}

void emitInstSubK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), -1, luauConstantValue(LUAU_INSN_C(*pc)), pcpos, TM_SUB);
}

void emitInstMulK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), -1, luauConstantValue(LUAU_INSN_C(*pc)), pcpos, TM_MUL);
}

void emitInstDivK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), -1, luauConstantValue(LUAU_INSN_C(*pc)), pcpos, TM_DIV);
}

void emitInstModK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos)
{
    emitInstBinaryNumeric(build, LUAU_INSN_A(*pc), LUAU_INSN_B(*pc), -1, luauConstantValue(LUAU_INSN_C(*pc)), pcpos, TM_MOD);
}

void emitInstPowK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    double kv = nvalue(&k[LUAU_INSN_C(*pc)]);

    Label common, exit;

    jumpIfTagIsNot(build, rb, LUA_TNUMBER, common);

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

    build.jmp(exit);

    // slow-path
    // TODO: move to the end of the function
    build.setLabel(common);
    callArithHelper(build, ra, rb, luauConstantValue(LUAU_INSN_C(*pc)), pcpos, TM_POW);

    build.setLabel(exit);
}

void emitInstNot(AssemblyBuilderX64& build, const Instruction* pc)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);

    Label saveone, savezero, exit;

    jumpIfFalsy(build, rb, saveone, savezero);

    build.setLabel(savezero);
    build.mov(luauRegValueBoolean(ra), 0);
    build.jmp(exit);

    build.setLabel(saveone);
    build.mov(luauRegValueBoolean(ra), 1);

    build.setLabel(exit);
    build.mov(luauRegTag(ra), LUA_TBOOLEAN);
}

void emitInstMinus(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);

    Label any, exit;

    jumpIfTagIsNot(build, rb, LUA_TNUMBER, any);

    // fast-path: number
    build.vxorpd(xmm0, xmm0, xmm0);
    build.vsubsd(xmm0, xmm0, luauRegValue(rb));
    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != rb)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    build.jmp(exit);

    // slow-path
    // TODO: move to the end of the function
    build.setLabel(any);
    callArithHelper(build, ra, rb, luauRegValue(rb), pcpos, TM_UNM);

    build.setLabel(exit);
}

void emitInstLength(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);

    Label any, exit;

    jumpIfTagIsNot(build, rb, LUA_TTABLE, any);

    // fast-path: table without __len
    build.mov(rArg1, luauRegValue(rb));
    jumpIfMetatablePresent(build, rArg1, any);

    // First argument (Table*) is already in rArg1
    build.call(qword[rNativeContext + offsetof(NativeContext, luaH_getn)]);

    build.vcvtsi2sd(xmm0, xmm0, eax);
    build.vmovsd(luauRegValue(ra), xmm0);
    build.mov(luauRegTag(ra), LUA_TNUMBER);
    build.jmp(exit);

    // slow-path
    // TODO: move to the end of the function
    build.setLabel(any);
    callLengthHelper(build, ra, rb, pcpos);

    build.setLabel(exit);
}

void emitInstGetUpval(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    int ra = LUAU_INSN_A(*pc);
    int up = LUAU_INSN_B(*pc);

    build.mov(rax, sClosure);
    build.add(rax, offsetof(Closure, l.uprefs) + sizeof(TValue) * up);

    // uprefs[] is either an actual value, or it points to UpVal object which has a pointer to value
    Label skip;
    // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
    build.cmp(dword[rax + offsetof(TValue, tt)], LUA_TUPVAL);
    build.jcc(Condition::NotEqual, skip);

    // UpVal.v points to the value (either on stack, or on heap inside each UpVal, but we can deref it unconditionally)
    build.mov(rax, qword[rax + offsetof(TValue, value.gc)]);
    build.mov(rax, qword[rax + offsetof(UpVal, v)]);

    build.setLabel(skip);

    build.vmovups(xmm0, xmmword[rax]);
    build.vmovups(luauReg(ra), xmm0);
}

static void emitInstFastCallN(
    AssemblyBuilderX64& build, const Instruction* pc, bool customParams, int customParamCount, OperandX64 customArgs, int pcpos, Label* labelarr)
{
    int bfid = LUAU_INSN_A(*pc);
    int skip = LUAU_INSN_C(*pc);

    Instruction call = pc[skip + 1];
    LUAU_ASSERT(LUAU_INSN_OP(call) == LOP_CALL);
    int ra = LUAU_INSN_A(call);

    int nparams = customParams ? customParamCount : LUAU_INSN_B(call) - 1;
    int nresults = LUAU_INSN_C(call) - 1;
    int arg = customParams ? LUAU_INSN_B(*pc) : ra + 1;
    OperandX64 args = customParams ? customArgs : luauRegValue(ra + 2);

    Label exit;

    jumpIfUnsafeEnv(build, rax, exit);

    BuiltinImplResult br = emitBuiltin(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, exit);

    if (br.type == BuiltinImplType::UsesFallback)
    {
        if (nresults == LUA_MULTRET)
        {
            // L->top = ra + n;
            build.lea(rax, qword[rBase + (ra + br.actualResultCount) * sizeof(TValue)]);
            build.mov(qword[rState + offsetof(lua_State, top)], rax);
        }
        else if (nparams == LUA_MULTRET)
        {
            // L->top = L->ci->top;
            build.mov(rax, qword[rState + offsetof(lua_State, ci)]);
            build.mov(rax, qword[rax + offsetof(CallInfo, top)]);
            build.mov(qword[rState + offsetof(lua_State, top)], rax);
        }

        // TODO: once we start outlining the fallback, we will be able to fallthrough to the next instruction
        build.jmp(labelarr[pcpos + skip + 2]);
        build.setLabel(exit);
        return;
    }

    // TODO: we can skip saving pc for some well-behaved builtins which we didn't inline
    emitSetSavedPc(build, pcpos); // uses rax/rdx

    build.mov(rax, qword[rNativeContext + offsetof(NativeContext, luauF_table) + bfid * sizeof(luau_FastFunction)]);

    // 5th parameter (args) is left unset for LOP_FASTCALL1
    if (args.cat == CategoryX64::mem)
    {
        if (getCurrentX64ABI() == X64ABI::Windows)
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
        // TODO: for SystemV ABI we can compute the result directly into rArg6
        // L->top - (ra + 1)
        build.mov(rcx, qword[rState + offsetof(lua_State, top)]);
        build.lea(rdx, qword[rBase + (ra + 1) * sizeof(TValue)]);
        build.sub(rcx, rdx);
        build.shr(rcx, kTValueSizeLog2);

        if (getCurrentX64ABI() == X64ABI::Windows)
            build.mov(sArg6, rcx);
        else
            build.mov(rArg6, rcx);
    }
    else
    {
        if (getCurrentX64ABI() == X64ABI::Windows)
            build.mov(sArg6, nparams);
        else
            build.mov(rArg6, nparams);
    }

    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegValue(ra));
    build.lea(rArg3, luauRegValue(arg));
    build.mov(rArg4, nresults);

    build.call(rax);

    build.test(eax, eax);             // test here will set SF=1 for a negative number and it always sets OF to 0
    build.jcc(Condition::Less, exit); // jl jumps if SF != OF

    if (nresults == LUA_MULTRET)
    {
        // L->top = ra + n;
        build.shl(rax, kTValueSizeLog2);
        build.lea(rax, qword[rBase + rax + ra * sizeof(TValue)]);
        build.mov(qword[rState + offsetof(lua_State, top)], rax);
    }
    else if (nparams == LUA_MULTRET)
    {
        // L->top = L->ci->top;
        build.mov(rax, qword[rState + offsetof(lua_State, ci)]);
        build.mov(rax, qword[rax + offsetof(CallInfo, top)]);
        build.mov(qword[rState + offsetof(lua_State, top)], rax);
    }

    build.jmp(labelarr[pcpos + skip + 2]);

    build.setLabel(exit);

    // TODO: fallback to LOP_CALL after a fast call should be outlined
}

void emitInstFastCall1(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr)
{
    emitInstFastCallN(build, pc, /* customParams */ true, /* customParamCount */ 1, /* customArgs */ 0, pcpos, labelarr);
}

void emitInstFastCall2(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr)
{
    emitInstFastCallN(build, pc, /* customParams */ true, /* customParamCount */ 2, /* customArgs */ luauRegValue(pc[1]), pcpos, labelarr);
}

void emitInstFastCall2K(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos, Label* labelarr)
{
    emitInstFastCallN(build, pc, /* customParams */ true, /* customParamCount */ 2, /* customArgs */ luauConstantValue(pc[1]), pcpos, labelarr);
}

void emitInstFastCall(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr)
{
    emitInstFastCallN(build, pc, /* customParams */ false, /* customParamCount */ 0, /* customArgs */ 0, pcpos, labelarr);
}

void emitInstForNPrep(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr)
{
    int ra = LUAU_INSN_A(*pc);
    Label& loopExit = labelarr[pcpos + LUAU_INSN_D(*pc) + 1];

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
    jumpOnNumberCmp(build, noreg, step, zero, Condition::LessEqual, reverse);

    // TODO: target branches can probably be arranged better, but we need tests for NaN behavior preservation
    // false: idx <= limit
    jumpOnNumberCmp(build, noreg, idx, limit, Condition::LessEqual, exit);
    build.jmp(loopExit);

    // true: limit <= idx
    build.setLabel(reverse);
    jumpOnNumberCmp(build, noreg, limit, idx, Condition::LessEqual, exit);
    build.jmp(loopExit);

    // TOOD: place at the end of the function
    build.setLabel(tryConvert);
    callPrepareForN(build, ra + 0, ra + 1, ra + 2, pcpos);
    build.jmp(retry);

    build.setLabel(exit);
}

void emitInstForNLoop(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr)
{
    emitInterrupt(build, pcpos);

    int ra = LUAU_INSN_A(*pc);
    Label& loopRepeat = labelarr[pcpos + LUAU_INSN_D(*pc) + 1];

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

    Label reverse, exit;

    // step <= 0
    jumpOnNumberCmp(build, noreg, step, zero, Condition::LessEqual, reverse);

    // false: idx <= limit
    jumpOnNumberCmp(build, noreg, idx, limit, Condition::LessEqual, loopRepeat);
    build.jmp(exit);

    // true: limit <= idx
    build.setLabel(reverse);
    jumpOnNumberCmp(build, noreg, limit, idx, Condition::LessEqual, loopRepeat);

    build.setLabel(exit);
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

void emitInstGetTableN(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    int c = LUAU_INSN_C(*pc);

    Label fallback, exit;

    jumpIfTagIsNot(build, rb, LUA_TTABLE, fallback);

    RegisterX64 table = rcx;
    build.mov(table, luauRegValue(rb));

    // unsigned(c) < unsigned(h->sizearray)
    build.cmp(dword[table + offsetof(Table, sizearray)], c);
    build.jcc(Condition::BelowEqual, fallback);

    jumpIfMetatablePresent(build, table, fallback);

    build.mov(rax, qword[table + offsetof(Table, array)]);
    setLuauReg(build, xmm0, ra, xmmword[rax + c * sizeof(TValue)]);

    build.jmp(exit);

    // slow-path
    // TODO: move to the end of the function
    build.setLabel(fallback);
    TValue n;
    setnvalue(&n, c + 1);
    callGetTable(build, rb, build.bytes(&n, sizeof(n)), ra, pcpos);

    build.setLabel(exit);
}

void emitInstSetTableN(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    int c = LUAU_INSN_C(*pc);

    Label fallback, exit;

    jumpIfTagIsNot(build, rb, LUA_TTABLE, fallback);

    RegisterX64 table = rcx;
    build.mov(table, luauRegValue(rb));

    // unsigned(c) < unsigned(h->sizearray)
    build.cmp(dword[table + offsetof(Table, sizearray)], c);
    build.jcc(Condition::BelowEqual, fallback);

    jumpIfMetatablePresent(build, table, fallback);
    jumpIfTableIsReadOnly(build, table, fallback);

    // setobj2t(L, &h->array[c], ra);
    build.mov(rax, qword[table + offsetof(Table, array)]);
    build.vmovups(xmm0, luauReg(ra));
    build.vmovups(xmmword[rax + c * sizeof(TValue)], xmm0);

    callBarrierTable(build, rax, table, ra, exit);
    build.jmp(exit);

    // slow-path
    // TODO: move to the end of the function
    build.setLabel(fallback);
    TValue n;
    setnvalue(&n, c + 1);
    callSetTable(build, rb, build.bytes(&n, sizeof(n)), ra, pcpos);

    build.setLabel(exit);
}

void emitInstGetTable(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    int rc = LUAU_INSN_C(*pc);

    Label fallback, exit;

    jumpIfTagIsNot(build, rb, LUA_TTABLE, fallback);
    jumpIfTagIsNot(build, rc, LUA_TNUMBER, fallback);

    // fast-path: table with a number index
    RegisterX64 table = rcx;
    build.mov(table, luauRegValue(rb));

    convertNumberToIndexOrJump(build, xmm1, xmm0, eax, rc, fallback);

    // index - 1
    build.dec(eax);

    // unsigned(index - 1) < unsigned(h->sizearray)
    build.cmp(dword[table + offsetof(Table, sizearray)], eax);
    build.jcc(Condition::BelowEqual, fallback);

    jumpIfMetatablePresent(build, table, fallback);

    // setobj2s(L, ra, &h->array[unsigned(index - 1)]);
    build.mov(rdx, qword[table + offsetof(Table, array)]);
    build.shl(eax, kTValueSizeLog2);
    setLuauReg(build, xmm0, ra, xmmword[rdx + rax]);

    build.jmp(exit);

    build.setLabel(fallback);

    // slow-path
    // TODO: move to the end of the function
    callGetTable(build, rb, luauRegValue(rc), ra, pcpos);

    build.setLabel(exit);
}

void emitInstSetTable(AssemblyBuilderX64& build, const Instruction* pc, int pcpos)
{
    int ra = LUAU_INSN_A(*pc);
    int rb = LUAU_INSN_B(*pc);
    int rc = LUAU_INSN_C(*pc);

    Label fallback, exit;

    jumpIfTagIsNot(build, rb, LUA_TTABLE, fallback);
    jumpIfTagIsNot(build, rc, LUA_TNUMBER, fallback);

    // fast-path: table with a number index
    RegisterX64 table = rcx;
    build.mov(table, luauRegValue(rb));

    convertNumberToIndexOrJump(build, xmm1, xmm0, eax, rc, fallback);

    // index - 1
    build.dec(eax);

    // unsigned(index - 1) < unsigned(h->sizearray)
    build.cmp(dword[table + offsetof(Table, sizearray)], eax);
    build.jcc(Condition::BelowEqual, fallback);

    jumpIfMetatablePresent(build, table, fallback);
    jumpIfTableIsReadOnly(build, table, fallback);

    // setobj2t(L, &h->array[unsigned(index - 1)], ra);
    build.mov(rdx, qword[table + offsetof(Table, array)]);
    build.shl(eax, kTValueSizeLog2);
    build.vmovups(xmm0, luauReg(ra));
    build.vmovups(xmmword[rdx + rax], xmm0);

    callBarrierTable(build, rdx, table, ra, exit);
    build.jmp(exit);

    build.setLabel(fallback);

    // slow-path
    // TODO: move to the end of the function
    callSetTable(build, rb, luauRegValue(rc), ra, pcpos);

    build.setLabel(exit);
}

} // namespace CodeGen
} // namespace Luau
