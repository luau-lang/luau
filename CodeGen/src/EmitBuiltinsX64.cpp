// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "EmitBuiltinsX64.h"

#include "Luau/AssemblyBuilderX64.h"
#include "Luau/Bytecode.h"

#include "EmitCommonX64.h"
#include "IrTranslateBuiltins.h" // Used temporarily for shared definition of BuiltinImplResult
#include "NativeState.h"

#include "lstate.h"

// TODO: LBF_MATH_FREXP and LBF_MATH_MODF can work for 1 result case if second store is removed

namespace Luau
{
namespace CodeGen
{

BuiltinImplResult emitBuiltinMathFloor(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_FLOOR\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    build.vroundsd(xmm0, xmm0, luauRegValue(arg), RoundingModeX64::RoundToNegativeInfinity);
    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathCeil(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_CEIL\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    build.vroundsd(xmm0, xmm0, luauRegValue(arg), RoundingModeX64::RoundToPositiveInfinity);
    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathSqrt(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_SQRT\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    build.vsqrtsd(xmm0, xmm0, luauRegValue(arg));
    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathAbs(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_ABS\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    build.vmovsd(xmm0, luauRegValue(arg));
    build.vandpd(xmm0, xmm0, build.i64(~(1LL << 63)));
    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

static BuiltinImplResult emitBuiltinMathSingleArgFunc(
    AssemblyBuilderX64& build, int nparams, int ra, int arg, int nresults, Label& fallback, const char* name, int32_t offset)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined %s\n", name);

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    build.vmovsd(xmm0, luauRegValue(arg));
    build.call(qword[rNativeContext + offset]);

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathExp(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    return emitBuiltinMathSingleArgFunc(build, nparams, ra, arg, nresults, fallback, "LBF_MATH_EXP", offsetof(NativeContext, libm_exp));
}

BuiltinImplResult emitBuiltinMathDeg(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_DEG\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    const double rpd = (3.14159265358979323846 / 180.0);

    build.vmovsd(xmm0, luauRegValue(arg));
    build.vdivsd(xmm0, xmm0, build.f64(rpd));
    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathRad(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_RAD\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    const double rpd = (3.14159265358979323846 / 180.0);

    build.vmovsd(xmm0, luauRegValue(arg));
    build.vmulsd(xmm0, xmm0, build.f64(rpd));
    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathFmod(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_FMOD\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
    build.cmp(dword[args + offsetof(TValue, tt)], LUA_TNUMBER);
    build.jcc(ConditionX64::NotEqual, fallback);

    build.vmovsd(xmm0, luauRegValue(arg));
    build.vmovsd(xmm1, qword[args + offsetof(TValue, value)]);
    build.call(qword[rNativeContext + offsetof(NativeContext, libm_fmod)]);

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathPow(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_POW\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
    build.cmp(dword[args + offsetof(TValue, tt)], LUA_TNUMBER);
    build.jcc(ConditionX64::NotEqual, fallback);

    build.vmovsd(xmm0, luauRegValue(arg));
    build.vmovsd(xmm1, qword[args + offsetof(TValue, value)]);
    build.call(qword[rNativeContext + offsetof(NativeContext, libm_pow)]);

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathMin(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams != 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_MIN\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
    build.cmp(dword[args + offsetof(TValue, tt)], LUA_TNUMBER);
    build.jcc(ConditionX64::NotEqual, fallback);

    build.vmovsd(xmm0, qword[args + offsetof(TValue, value)]);
    build.vminsd(xmm0, xmm0, luauRegValue(arg));

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathMax(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams != 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_MAX\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
    build.cmp(dword[args + offsetof(TValue, tt)], LUA_TNUMBER);
    build.jcc(ConditionX64::NotEqual, fallback);

    build.vmovsd(xmm0, qword[args + offsetof(TValue, value)]);
    build.vmaxsd(xmm0, xmm0, luauRegValue(arg));

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathAsin(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    return emitBuiltinMathSingleArgFunc(build, nparams, ra, arg, nresults, fallback, "LBF_MATH_ASIN", offsetof(NativeContext, libm_asin));
}

BuiltinImplResult emitBuiltinMathSin(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    return emitBuiltinMathSingleArgFunc(build, nparams, ra, arg, nresults, fallback, "LBF_MATH_SIN", offsetof(NativeContext, libm_sin));
}

BuiltinImplResult emitBuiltinMathSinh(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    return emitBuiltinMathSingleArgFunc(build, nparams, ra, arg, nresults, fallback, "LBF_MATH_SINH", offsetof(NativeContext, libm_sinh));
}

BuiltinImplResult emitBuiltinMathAcos(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    return emitBuiltinMathSingleArgFunc(build, nparams, ra, arg, nresults, fallback, "LBF_MATH_ACOS", offsetof(NativeContext, libm_acos));
}

BuiltinImplResult emitBuiltinMathCos(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    return emitBuiltinMathSingleArgFunc(build, nparams, ra, arg, nresults, fallback, "LBF_MATH_COS", offsetof(NativeContext, libm_cos));
}

BuiltinImplResult emitBuiltinMathCosh(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    return emitBuiltinMathSingleArgFunc(build, nparams, ra, arg, nresults, fallback, "LBF_MATH_COSH", offsetof(NativeContext, libm_cosh));
}

BuiltinImplResult emitBuiltinMathAtan(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    return emitBuiltinMathSingleArgFunc(build, nparams, ra, arg, nresults, fallback, "LBF_MATH_ATAN", offsetof(NativeContext, libm_atan));
}

BuiltinImplResult emitBuiltinMathTan(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    return emitBuiltinMathSingleArgFunc(build, nparams, ra, arg, nresults, fallback, "LBF_MATH_TAN", offsetof(NativeContext, libm_tan));
}

BuiltinImplResult emitBuiltinMathTanh(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    return emitBuiltinMathSingleArgFunc(build, nparams, ra, arg, nresults, fallback, "LBF_MATH_TANH", offsetof(NativeContext, libm_tanh));
}

BuiltinImplResult emitBuiltinMathAtan2(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_ATAN2\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
    build.cmp(dword[args + offsetof(TValue, tt)], LUA_TNUMBER);
    build.jcc(ConditionX64::NotEqual, fallback);

    build.vmovsd(xmm0, luauRegValue(arg));
    build.vmovsd(xmm1, qword[args + offsetof(TValue, value)]);
    build.call(qword[rNativeContext + offsetof(NativeContext, libm_atan2)]);

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathLog10(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    return emitBuiltinMathSingleArgFunc(build, nparams, ra, arg, nresults, fallback, "LBF_MATH_LOG10", offsetof(NativeContext, libm_log10));
}

BuiltinImplResult emitBuiltinMathLog(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_LOG\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    build.vmovsd(xmm0, luauRegValue(arg));

    if (nparams == 1)
    {
        build.call(qword[rNativeContext + offsetof(NativeContext, libm_log)]);
    }
    else
    {
        Label log10check, logdivlog, exit;

        // Using 'rbx' for non-volatile temporary storage of log(arg1) result
        RegisterX64 tmp = rbx;
        OperandX64 arg2value = qword[args + offsetof(TValue, value)];

        // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
        build.cmp(dword[args + offsetof(TValue, tt)], LUA_TNUMBER);
        build.jcc(ConditionX64::NotEqual, fallback);

        build.vmovsd(xmm1, arg2value);

        jumpOnNumberCmp(build, noreg, build.f64(2.0), xmm1, ConditionX64::NotEqual, log10check);

        build.call(qword[rNativeContext + offsetof(NativeContext, libm_log2)]);
        build.jmp(exit);

        build.setLabel(log10check);
        jumpOnNumberCmp(build, noreg, build.f64(10.0), xmm1, ConditionX64::NotEqual, logdivlog);

        build.call(qword[rNativeContext + offsetof(NativeContext, libm_log10)]);
        build.jmp(exit);

        build.setLabel(logdivlog);

        // log(arg1)
        build.call(qword[rNativeContext + offsetof(NativeContext, libm_log)]);
        build.vmovq(tmp, xmm0);

        // log(arg2)
        build.vmovsd(xmm0, arg2value);
        build.call(qword[rNativeContext + offsetof(NativeContext, libm_log)]);

        // log(arg1) / log(arg2)
        build.vmovq(xmm1, tmp);
        build.vdivsd(xmm0, xmm1, xmm0);

        build.setLabel(exit);
    }

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}


BuiltinImplResult emitBuiltinMathLdexp(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_LDEXP\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
    build.cmp(dword[args + offsetof(TValue, tt)], LUA_TNUMBER);
    build.jcc(ConditionX64::NotEqual, fallback);

    build.vmovsd(xmm0, luauRegValue(arg));

    if (build.abi == ABIX64::Windows)
        build.vcvttsd2si(rArg2, qword[args + offsetof(TValue, value)]);
    else
        build.vcvttsd2si(rArg1, qword[args + offsetof(TValue, value)]);

    build.call(qword[rNativeContext + offsetof(NativeContext, libm_ldexp)]);

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathRound(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_ROUND\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    build.vmovsd(xmm0, luauRegValue(arg));
    build.vandpd(xmm1, xmm0, build.f64x2(-0.0, -0.0));
    build.vmovsd(xmm2, build.i64(0x3fdfffffffffffff)); // 0.49999999999999994
    build.vorpd(xmm1, xmm1, xmm2);
    build.vaddsd(xmm0, xmm0, xmm1);
    build.vroundsd(xmm0, xmm0, xmm0, RoundingModeX64::RoundToZero);

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathFrexp(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 1 || nresults > 2)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_FREXP\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    build.vmovsd(xmm0, luauRegValue(arg));

    if (build.abi == ABIX64::Windows)
        build.lea(rArg2, sTemporarySlot);
    else
        build.lea(rArg1, sTemporarySlot);

    build.call(qword[rNativeContext + offsetof(NativeContext, libm_frexp)]);

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    build.vcvtsi2sd(xmm0, xmm0, dword[sTemporarySlot + 0]);
    build.vmovsd(luauRegValue(ra + 1), xmm0);
    build.mov(luauRegTag(ra + 1), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 2};
}

BuiltinImplResult emitBuiltinMathModf(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 1 || nresults > 2)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_MODF\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    build.vmovsd(xmm0, luauRegValue(arg));

    if (build.abi == ABIX64::Windows)
        build.lea(rArg2, sTemporarySlot);
    else
        build.lea(rArg1, sTemporarySlot);

    build.call(qword[rNativeContext + offsetof(NativeContext, libm_modf)]);

    build.vmovsd(xmm1, qword[sTemporarySlot + 0]);
    build.vmovsd(luauRegValue(ra), xmm1);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    build.vmovsd(luauRegValue(ra + 1), xmm0);
    build.mov(luauRegTag(ra + 1), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 2};
}

BuiltinImplResult emitBuiltinMathSign(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_SIGN\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    build.vmovsd(xmm0, luauRegValue(arg));
    build.vxorpd(xmm1, xmm1, xmm1);

    // Set xmm2 to -1 if arg < 0, else 0
    build.vcmpltsd(xmm2, xmm0, xmm1);
    build.vmovsd(xmm3, build.f64(-1));
    build.vandpd(xmm2, xmm2, xmm3);

    // Set mask bit to 1 if 0 < arg, else 0
    build.vcmpltsd(xmm0, xmm1, xmm0);

    // Result = (mask-bit == 1) ? 1.0 : xmm2
    // If arg < 0 then xmm2 is -1 and mask-bit is 0, result is -1
    // If arg == 0 then xmm2 is 0 and mask-bit is 0, result is 0
    // If arg > 0 then xmm2 is 0 and mask-bit is 1, result is 1
    build.vblendvpd(xmm0, xmm2, build.f64x2(1, 1), xmm0);

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult emitBuiltinMathClamp(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 3 || nresults > 1)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_MATH_CLAMP\n");

    jumpIfTagIsNot(build, arg, LUA_TNUMBER, fallback);

    // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
    build.cmp(dword[args + offsetof(TValue, tt)], LUA_TNUMBER);
    build.jcc(ConditionX64::NotEqual, fallback);

    // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
    build.cmp(dword[args + sizeof(TValue) + offsetof(TValue, tt)], LUA_TNUMBER);
    build.jcc(ConditionX64::NotEqual, fallback);

    RegisterX64 min = xmm1;
    RegisterX64 max = xmm2;
    build.vmovsd(min, qword[args + offsetof(TValue, value)]);
    build.vmovsd(max, qword[args + sizeof(TValue) + offsetof(TValue, value)]);

    jumpOnNumberCmp(build, noreg, min, max, ConditionX64::NotLessEqual, fallback);

    build.vmaxsd(xmm0, min, luauRegValue(arg));
    build.vminsd(xmm0, max, xmm0);

    build.vmovsd(luauRegValue(ra), xmm0);

    if (ra != arg)
        build.mov(luauRegTag(ra), LUA_TNUMBER);

    return {BuiltinImplType::UsesFallback, 1};
}


BuiltinImplResult emitBuiltin(AssemblyBuilderX64& build, int bfid, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    switch (bfid)
    {
    case LBF_ASSERT:
        // This builtin fast-path was already translated to IR
        return {BuiltinImplType::None, -1};
    case LBF_MATH_FLOOR:
        return emitBuiltinMathFloor(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_CEIL:
        return emitBuiltinMathCeil(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_SQRT:
        return emitBuiltinMathSqrt(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_ABS:
        return emitBuiltinMathAbs(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_EXP:
        return emitBuiltinMathExp(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_DEG:
        return emitBuiltinMathDeg(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_RAD:
        return emitBuiltinMathRad(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_FMOD:
        return emitBuiltinMathFmod(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_POW:
        return emitBuiltinMathPow(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_MIN:
        return emitBuiltinMathMin(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_MAX:
        return emitBuiltinMathMax(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_ASIN:
        return emitBuiltinMathAsin(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_SIN:
        return emitBuiltinMathSin(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_SINH:
        return emitBuiltinMathSinh(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_ACOS:
        return emitBuiltinMathAcos(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_COS:
        return emitBuiltinMathCos(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_COSH:
        return emitBuiltinMathCosh(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_ATAN:
        return emitBuiltinMathAtan(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_TAN:
        return emitBuiltinMathTan(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_TANH:
        return emitBuiltinMathTanh(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_ATAN2:
        return emitBuiltinMathAtan2(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_LOG10:
        return emitBuiltinMathLog10(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_LOG:
        return emitBuiltinMathLog(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_LDEXP:
        return emitBuiltinMathLdexp(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_ROUND:
        return emitBuiltinMathRound(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_FREXP:
        return emitBuiltinMathFrexp(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_MODF:
        return emitBuiltinMathModf(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_SIGN:
        return emitBuiltinMathSign(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_CLAMP:
        return emitBuiltinMathClamp(build, nparams, ra, arg, args, nresults, fallback);
    default:
        return {BuiltinImplType::None, -1};
    }
}

} // namespace CodeGen
} // namespace Luau
