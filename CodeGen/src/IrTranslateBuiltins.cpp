// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrTranslateBuiltins.h"

#include "Luau/Bytecode.h"
#include "Luau/IrBuilder.h"

#include "lstate.h"

// TODO: when nresults is less than our actual result count, we can skip computing/writing unused results

static const int kMinMaxUnrolledParams = 5;

namespace Luau
{
namespace CodeGen
{

// Wrapper code for all builtins with a fixed signature and manual assembly lowering of the body

// (number, ...) -> number
BuiltinImplResult translateBuiltinNumberToNumber(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    build.loadAndCheckTag(build.vmReg(arg), LUA_TNUMBER, fallback);
    build.inst(IrCmd::FASTCALL, build.constUint(bfid), build.vmReg(ra), build.vmReg(arg), args, build.constInt(1), build.constInt(1));

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

// (number, number, ...) -> number
BuiltinImplResult translateBuiltin2NumberToNumber(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    build.loadAndCheckTag(build.vmReg(arg), LUA_TNUMBER, fallback);
    build.loadAndCheckTag(args, LUA_TNUMBER, fallback);
    build.inst(IrCmd::FASTCALL, build.constUint(bfid), build.vmReg(ra), build.vmReg(arg), args, build.constInt(2), build.constInt(1));

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

// (number, ...) -> (number, number)
BuiltinImplResult translateBuiltinNumberTo2Number(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 1 || nresults > 2)
        return {BuiltinImplType::None, -1};

    build.loadAndCheckTag(build.vmReg(arg), LUA_TNUMBER, fallback);
    build.inst(
        IrCmd::FASTCALL, build.constUint(bfid), build.vmReg(ra), build.vmReg(arg), args, build.constInt(1), build.constInt(nresults == 1 ? 1 : 2));

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    if (nresults != 1)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra + 1), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 2};
}

BuiltinImplResult translateBuiltinAssert(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 1 || nresults != 0)
        return {BuiltinImplType::None, -1};

    IrOp cont = build.block(IrBlockKind::Internal);

    // TODO: maybe adding a guard like CHECK_TRUTHY can be useful
    build.inst(IrCmd::JUMP_IF_FALSY, build.vmReg(arg), fallback, cont);
    build.beginBlock(cont);

    return {BuiltinImplType::UsesFallback, 0};
}

BuiltinImplResult translateBuiltinMathDeg(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    build.loadAndCheckTag(build.vmReg(arg), LUA_TNUMBER, fallback);

    const double rpd = (3.14159265358979323846 / 180.0);

    IrOp varg = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(arg));
    IrOp value = build.inst(IrCmd::DIV_NUM, varg, build.constDouble(rpd));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), value);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult translateBuiltinMathRad(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    build.loadAndCheckTag(build.vmReg(arg), LUA_TNUMBER, fallback);

    const double rpd = (3.14159265358979323846 / 180.0);

    IrOp varg = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(arg));
    IrOp value = build.inst(IrCmd::MUL_NUM, varg, build.constDouble(rpd));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), value);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult translateBuiltinMathLog(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    LuauBuiltinFunction fcId = bfid;
    int fcParams = 1;

    if (nparams != 1)
    {
        if (args.kind != IrOpKind::VmConst)
            return {BuiltinImplType::None, -1};

        LUAU_ASSERT(build.function.proto);
        TValue protok = build.function.proto->k[vmConstOp(args)];

        if (protok.tt != LUA_TNUMBER)
            return {BuiltinImplType::None, -1};

        // TODO: IR builtin lowering assumes that the only valid 2-argument call is log2; ideally, we use a less hacky way to indicate that
        if (protok.value.n == 2.0)
            fcParams = 2;
        else if (protok.value.n == 10.0)
            fcId = LBF_MATH_LOG10;
        else
            // TODO: We can precompute log(args) and divide by it, but that requires extra LOAD/STORE so for now just fall back as this is rare
            return {BuiltinImplType::None, -1};
    }

    build.loadAndCheckTag(build.vmReg(arg), LUA_TNUMBER, fallback);

    build.inst(IrCmd::FASTCALL, build.constUint(fcId), build.vmReg(ra), build.vmReg(arg), args, build.constInt(fcParams), build.constInt(1));

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult translateBuiltinMathMin(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 2 || nparams > kMinMaxUnrolledParams || nresults > 1)
        return {BuiltinImplType::None, -1};

    build.loadAndCheckTag(build.vmReg(arg), LUA_TNUMBER, fallback);
    build.loadAndCheckTag(args, LUA_TNUMBER, fallback);

    for (int i = 3; i <= nparams; ++i)
        build.loadAndCheckTag(build.vmReg(vmRegOp(args) + (i - 2)), LUA_TNUMBER, fallback);

    IrOp varg1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(arg));
    IrOp varg2 = build.inst(IrCmd::LOAD_DOUBLE, args);

    IrOp res = build.inst(IrCmd::MIN_NUM, varg2, varg1); // Swapped arguments are required for consistency with VM builtins

    for (int i = 3; i <= nparams; ++i)
    {
        IrOp arg = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(vmRegOp(args) + (i - 2)));
        res = build.inst(IrCmd::MIN_NUM, arg, res);
    }

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), res);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult translateBuiltinMathMax(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 2 || nparams > kMinMaxUnrolledParams || nresults > 1)
        return {BuiltinImplType::None, -1};

    build.loadAndCheckTag(build.vmReg(arg), LUA_TNUMBER, fallback);
    build.loadAndCheckTag(args, LUA_TNUMBER, fallback);

    for (int i = 3; i <= nparams; ++i)
        build.loadAndCheckTag(build.vmReg(vmRegOp(args) + (i - 2)), LUA_TNUMBER, fallback);

    IrOp varg1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(arg));
    IrOp varg2 = build.inst(IrCmd::LOAD_DOUBLE, args);

    IrOp res = build.inst(IrCmd::MAX_NUM, varg2, varg1); // Swapped arguments are required for consistency with VM builtins

    for (int i = 3; i <= nparams; ++i)
    {
        IrOp arg = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(vmRegOp(args) + (i - 2)));
        res = build.inst(IrCmd::MAX_NUM, arg, res);
    }

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), res);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult translateBuiltinMathClamp(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 3 || nresults > 1)
        return {BuiltinImplType::None, -1};

    IrOp block = build.block(IrBlockKind::Internal);

    LUAU_ASSERT(args.kind == IrOpKind::VmReg);

    build.loadAndCheckTag(build.vmReg(arg), LUA_TNUMBER, fallback);
    build.loadAndCheckTag(args, LUA_TNUMBER, fallback);
    build.loadAndCheckTag(build.vmReg(vmRegOp(args) + 1), LUA_TNUMBER, fallback);

    IrOp min = build.inst(IrCmd::LOAD_DOUBLE, args);
    IrOp max = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(vmRegOp(args) + 1));

    build.inst(IrCmd::JUMP_CMP_NUM, min, max, build.cond(IrCondition::NotLessEqual), fallback, block);
    build.beginBlock(block);

    IrOp v = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(arg));
    IrOp r = build.inst(IrCmd::MAX_NUM, min, v);
    IrOp clamped = build.inst(IrCmd::MIN_NUM, max, r);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), clamped);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult translateBuiltinMathUnary(IrBuilder& build, IrCmd cmd, int nparams, int ra, int arg, int nresults, IrOp fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    build.loadAndCheckTag(build.vmReg(arg), LUA_TNUMBER, fallback);

    IrOp varg = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(arg));
    IrOp result = build.inst(cmd, varg);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), result);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult translateBuiltinMathBinary(IrBuilder& build, IrCmd cmd, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    build.loadAndCheckTag(build.vmReg(arg), LUA_TNUMBER, fallback);
    build.loadAndCheckTag(args, LUA_TNUMBER, fallback);

    IrOp lhs = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(arg));
    IrOp rhs = build.inst(IrCmd::LOAD_DOUBLE, args);
    IrOp result = build.inst(cmd, lhs, rhs);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), result);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult translateBuiltinType(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    build.inst(IrCmd::FASTCALL, build.constUint(LBF_TYPE), build.vmReg(ra), build.vmReg(arg), args, build.constInt(1), build.constInt(1));

    build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TSTRING));

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult translateBuiltinTypeof(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    build.inst(IrCmd::FASTCALL, build.constUint(LBF_TYPEOF), build.vmReg(ra), build.vmReg(arg), args, build.constInt(1), build.constInt(1));

    build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TSTRING));

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult translateBuiltinVector(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback)
{
    if (nparams < 3 || nresults > 1)
        return {BuiltinImplType::None, -1};

    LUAU_ASSERT(LUA_VECTOR_SIZE == 3);

    build.loadAndCheckTag(build.vmReg(arg), LUA_TNUMBER, fallback);
    build.loadAndCheckTag(args, LUA_TNUMBER, fallback);
    build.loadAndCheckTag(build.vmReg(vmRegOp(args) + 1), LUA_TNUMBER, fallback);

    IrOp x = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(arg));
    IrOp y = build.inst(IrCmd::LOAD_DOUBLE, args);
    IrOp z = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(vmRegOp(args) + 1));

    build.inst(IrCmd::STORE_VECTOR, build.vmReg(ra), x, y, z);
    build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TVECTOR));

    return {BuiltinImplType::UsesFallback, 1};
}

BuiltinImplResult translateBuiltin(IrBuilder& build, int bfid, int ra, int arg, IrOp args, int nparams, int nresults, IrOp fallback)
{
    // Builtins are not allowed to handle variadic arguments
    if (nparams == LUA_MULTRET)
        return {BuiltinImplType::None, -1};

    switch (bfid)
    {
    case LBF_ASSERT:
        return translateBuiltinAssert(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_DEG:
        return translateBuiltinMathDeg(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_RAD:
        return translateBuiltinMathRad(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_LOG:
        return translateBuiltinMathLog(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_MIN:
        return translateBuiltinMathMin(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_MAX:
        return translateBuiltinMathMax(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_CLAMP:
        return translateBuiltinMathClamp(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_FLOOR:
        return translateBuiltinMathUnary(build, IrCmd::FLOOR_NUM, nparams, ra, arg, nresults, fallback);
    case LBF_MATH_CEIL:
        return translateBuiltinMathUnary(build, IrCmd::CEIL_NUM, nparams, ra, arg, nresults, fallback);
    case LBF_MATH_SQRT:
        return translateBuiltinMathUnary(build, IrCmd::SQRT_NUM, nparams, ra, arg, nresults, fallback);
    case LBF_MATH_ABS:
        return translateBuiltinMathUnary(build, IrCmd::ABS_NUM, nparams, ra, arg, nresults, fallback);
    case LBF_MATH_ROUND:
        return translateBuiltinMathUnary(build, IrCmd::ROUND_NUM, nparams, ra, arg, nresults, fallback);
    case LBF_MATH_POW:
        return translateBuiltinMathBinary(build, IrCmd::POW_NUM, nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_EXP:
    case LBF_MATH_ASIN:
    case LBF_MATH_SIN:
    case LBF_MATH_SINH:
    case LBF_MATH_ACOS:
    case LBF_MATH_COS:
    case LBF_MATH_COSH:
    case LBF_MATH_ATAN:
    case LBF_MATH_TAN:
    case LBF_MATH_TANH:
    case LBF_MATH_LOG10:
    case LBF_MATH_SIGN:
        return translateBuiltinNumberToNumber(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_FMOD:
    case LBF_MATH_ATAN2:
    case LBF_MATH_LDEXP:
        return translateBuiltin2NumberToNumber(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, fallback);
    case LBF_MATH_FREXP:
    case LBF_MATH_MODF:
        return translateBuiltinNumberTo2Number(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, fallback);
    case LBF_TYPE:
        return translateBuiltinType(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_TYPEOF:
        return translateBuiltinTypeof(build, nparams, ra, arg, args, nresults, fallback);
    case LBF_VECTOR:
        return translateBuiltinVector(build, nparams, ra, arg, args, nresults, fallback);
    default:
        return {BuiltinImplType::None, -1};
    }
}

} // namespace CodeGen
} // namespace Luau
