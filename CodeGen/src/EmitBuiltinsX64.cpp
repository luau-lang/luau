// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "EmitBuiltinsX64.h"

#include "Luau/AssemblyBuilderX64.h"
#include "Luau/Bytecode.h"

#include "EmitCommonX64.h"

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
    default:
        return {BuiltinImplType::None, -1};
    }
}

} // namespace CodeGen
} // namespace Luau
