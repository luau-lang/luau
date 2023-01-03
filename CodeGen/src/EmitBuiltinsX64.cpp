// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "EmitBuiltinsX64.h"

#include "Luau/AssemblyBuilderX64.h"
#include "Luau/Bytecode.h"

#include "EmitCommonX64.h"
#include "NativeState.h"

#include "lstate.h"

namespace Luau
{
namespace CodeGen
{

BuiltinImplResult emitBuiltinAssert(AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    if (nparams < 1 || nresults != 0)
        return {BuiltinImplType::None, -1};

    if (build.logText)
        build.logAppend("; inlined LBF_ASSERT\n");

    Label skip;

    jumpIfFalsy(build, arg, fallback, skip);

    // TODO: use of 'skip' causes a jump to a jump instruction that skips the fallback - can be optimized
    build.setLabel(skip);

    return {BuiltinImplType::UsesFallback, 0};
}

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

BuiltinImplResult emitBuiltin(AssemblyBuilderX64& build, int bfid, int nparams, int ra, int arg, OperandX64 args, int nresults, Label& fallback)
{
    switch (bfid)
    {
    case LBF_ASSERT:
        return emitBuiltinAssert(build, nparams, ra, arg, args, nresults, fallback);
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
    default:
        return {BuiltinImplType::None, -1};
    }
}

} // namespace CodeGen
} // namespace Luau
