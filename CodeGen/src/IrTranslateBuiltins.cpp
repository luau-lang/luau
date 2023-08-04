// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrTranslateBuiltins.h"

#include "Luau/Bytecode.h"
#include "Luau/IrBuilder.h"

#include "lstate.h"

#include <math.h>

// TODO: when nresults is less than our actual result count, we can skip computing/writing unused results

static const int kMinMaxUnrolledParams = 5;
static const int kBit32BinaryOpUnrolledParams = 5;

namespace Luau
{
namespace CodeGen
{

static void builtinCheckDouble(IrBuilder& build, IrOp arg, int pcpos)
{
    if (arg.kind == IrOpKind::Constant)
        LUAU_ASSERT(build.function.constOp(arg).kind == IrConstKind::Double);
    else
        build.loadAndCheckTag(arg, LUA_TNUMBER, build.vmExit(pcpos));
}

static IrOp builtinLoadDouble(IrBuilder& build, IrOp arg)
{
    if (arg.kind == IrOpKind::Constant)
        return arg;

    return build.inst(IrCmd::LOAD_DOUBLE, arg);
}

// Wrapper code for all builtins with a fixed signature and manual assembly lowering of the body

// (number, ...) -> number
static BuiltinImplResult translateBuiltinNumberToNumber(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    build.inst(IrCmd::FASTCALL, build.constUint(bfid), build.vmReg(ra), build.vmReg(arg), args, build.constInt(1), build.constInt(1));

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinNumberToNumberLibm(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    IrOp va = builtinLoadDouble(build, build.vmReg(arg));

    IrOp res = build.inst(IrCmd::INVOKE_LIBM, build.constUint(bfid), va);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), res);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltin2NumberToNumberLibm(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    builtinCheckDouble(build, args, pcpos);

    IrOp va = builtinLoadDouble(build, build.vmReg(arg));
    IrOp vb = builtinLoadDouble(build, args);

    IrOp res = build.inst(IrCmd::INVOKE_LIBM, build.constUint(bfid), va, vb);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), res);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinMathLdexp(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    builtinCheckDouble(build, args, pcpos);

    IrOp va = builtinLoadDouble(build, build.vmReg(arg));
    IrOp vb = builtinLoadDouble(build, args);

    IrOp vbi = build.inst(IrCmd::NUM_TO_INT, vb);

    IrOp res = build.inst(IrCmd::INVOKE_LIBM, build.constUint(bfid), va, vbi);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), res);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

// (number, ...) -> (number, number)
static BuiltinImplResult translateBuiltinNumberTo2Number(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 1 || nresults > 2)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    build.inst(
        IrCmd::FASTCALL, build.constUint(bfid), build.vmReg(ra), build.vmReg(arg), args, build.constInt(1), build.constInt(nresults == 1 ? 1 : 2));

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    if (nresults != 1)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra + 1), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 2};
}

static BuiltinImplResult translateBuiltinAssert(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 1 || nresults != 0)
        return {BuiltinImplType::None, -1};

    IrOp tag = build.inst(IrCmd::LOAD_TAG, build.vmReg(arg));

    // We don't know if it's really a boolean at this point, but we will only check this value if it is
    IrOp value = build.inst(IrCmd::LOAD_INT, build.vmReg(arg));

    build.inst(IrCmd::CHECK_TRUTHY, tag, value, build.vmExit(pcpos));

    return {BuiltinImplType::UsesFallback, 0};
}

static BuiltinImplResult translateBuiltinMathDeg(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);

    const double rpd = (3.14159265358979323846 / 180.0);

    IrOp varg = builtinLoadDouble(build, build.vmReg(arg));
    IrOp value = build.inst(IrCmd::DIV_NUM, varg, build.constDouble(rpd));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), value);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinMathRad(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);

    const double rpd = (3.14159265358979323846 / 180.0);

    IrOp varg = builtinLoadDouble(build, build.vmReg(arg));
    IrOp value = build.inst(IrCmd::MUL_NUM, varg, build.constDouble(rpd));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), value);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinMathLog(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    int libmId = bfid;
    std::optional<double> denom;

    if (nparams != 1)
    {
        std::optional<double> y = build.function.asDoubleOp(args);

        if (!y)
            return {BuiltinImplType::None, -1};

        if (*y == 2.0)
            libmId = LBF_IR_MATH_LOG2;
        else if (*y == 10.0)
            libmId = LBF_MATH_LOG10;
        else
            denom = log(*y);
    }

    builtinCheckDouble(build, build.vmReg(arg), pcpos);

    IrOp va = builtinLoadDouble(build, build.vmReg(arg));

    IrOp res = build.inst(IrCmd::INVOKE_LIBM, build.constUint(libmId), va);

    if (denom)
        res = build.inst(IrCmd::DIV_NUM, res, build.constDouble(*denom));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), res);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinMathMin(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 2 || nparams > kMinMaxUnrolledParams || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    builtinCheckDouble(build, args, pcpos);

    for (int i = 3; i <= nparams; ++i)
        builtinCheckDouble(build, build.vmReg(vmRegOp(args) + (i - 2)), pcpos);

    IrOp varg1 = builtinLoadDouble(build, build.vmReg(arg));
    IrOp varg2 = builtinLoadDouble(build, args);

    IrOp res = build.inst(IrCmd::MIN_NUM, varg2, varg1); // Swapped arguments are required for consistency with VM builtins

    for (int i = 3; i <= nparams; ++i)
    {
        IrOp arg = builtinLoadDouble(build, build.vmReg(vmRegOp(args) + (i - 2)));
        res = build.inst(IrCmd::MIN_NUM, arg, res);
    }

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), res);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinMathMax(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 2 || nparams > kMinMaxUnrolledParams || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    builtinCheckDouble(build, args, pcpos);

    for (int i = 3; i <= nparams; ++i)
        builtinCheckDouble(build, build.vmReg(vmRegOp(args) + (i - 2)), pcpos);

    IrOp varg1 = builtinLoadDouble(build, build.vmReg(arg));
    IrOp varg2 = builtinLoadDouble(build, args);

    IrOp res = build.inst(IrCmd::MAX_NUM, varg2, varg1); // Swapped arguments are required for consistency with VM builtins

    for (int i = 3; i <= nparams; ++i)
    {
        IrOp arg = builtinLoadDouble(build, build.vmReg(vmRegOp(args) + (i - 2)));
        res = build.inst(IrCmd::MAX_NUM, arg, res);
    }

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), res);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinMathClamp(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback, int pcpos)
{
    if (nparams < 3 || nresults > 1)
        return {BuiltinImplType::None, -1};

    IrOp block = build.block(IrBlockKind::Internal);

    LUAU_ASSERT(args.kind == IrOpKind::VmReg);

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    builtinCheckDouble(build, args, pcpos);
    builtinCheckDouble(build, build.vmReg(vmRegOp(args) + 1), pcpos);

    IrOp min = builtinLoadDouble(build, args);
    IrOp max = builtinLoadDouble(build, build.vmReg(vmRegOp(args) + 1));

    build.inst(IrCmd::JUMP_CMP_NUM, min, max, build.cond(IrCondition::NotLessEqual), fallback, block);
    build.beginBlock(block);

    IrOp v = builtinLoadDouble(build, build.vmReg(arg));
    IrOp r = build.inst(IrCmd::MAX_NUM, min, v);
    IrOp clamped = build.inst(IrCmd::MIN_NUM, max, r);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), clamped);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

static BuiltinImplResult translateBuiltinMathUnary(IrBuilder& build, IrCmd cmd, int nparams, int ra, int arg, int nresults, int pcpos)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);

    IrOp varg = builtinLoadDouble(build, build.vmReg(arg));
    IrOp result = build.inst(cmd, varg);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), result);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinType(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    IrOp tag = build.inst(IrCmd::LOAD_TAG, build.vmReg(arg));
    IrOp name = build.inst(IrCmd::GET_TYPE, tag);

    build.inst(IrCmd::STORE_POINTER, build.vmReg(ra), name);
    build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TSTRING));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinTypeof(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    IrOp name = build.inst(IrCmd::GET_TYPEOF, build.vmReg(arg));

    build.inst(IrCmd::STORE_POINTER, build.vmReg(ra), name);
    build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TSTRING));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinBit32BinaryOp(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 2 || nparams > kBit32BinaryOpUnrolledParams || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    builtinCheckDouble(build, args, pcpos);

    for (int i = 3; i <= nparams; ++i)
        builtinCheckDouble(build, build.vmReg(vmRegOp(args) + (i - 2)), pcpos);

    IrOp va = builtinLoadDouble(build, build.vmReg(arg));
    IrOp vb = builtinLoadDouble(build, args);

    IrOp vaui = build.inst(IrCmd::NUM_TO_UINT, va);
    IrOp vbui = build.inst(IrCmd::NUM_TO_UINT, vb);


    IrCmd cmd = IrCmd::NOP;
    if (bfid == LBF_BIT32_BAND || bfid == LBF_BIT32_BTEST)
        cmd = IrCmd::BITAND_UINT;
    else if (bfid == LBF_BIT32_BXOR)
        cmd = IrCmd::BITXOR_UINT;
    else if (bfid == LBF_BIT32_BOR)
        cmd = IrCmd::BITOR_UINT;

    LUAU_ASSERT(cmd != IrCmd::NOP);

    IrOp res = build.inst(cmd, vaui, vbui);

    for (int i = 3; i <= nparams; ++i)
    {
        IrOp vc = builtinLoadDouble(build, build.vmReg(vmRegOp(args) + (i - 2)));
        IrOp arg = build.inst(IrCmd::NUM_TO_UINT, vc);

        res = build.inst(cmd, res, arg);
    }

    if (bfid == LBF_BIT32_BTEST)
    {
        IrOp falsey = build.block(IrBlockKind::Internal);
        IrOp truthy = build.block(IrBlockKind::Internal);
        IrOp exit = build.block(IrBlockKind::Internal);
        build.inst(IrCmd::JUMP_EQ_INT, res, build.constInt(0), falsey, truthy);

        build.beginBlock(falsey);
        build.inst(IrCmd::STORE_INT, build.vmReg(ra), build.constInt(0));
        build.inst(IrCmd::JUMP, exit);

        build.beginBlock(truthy);
        build.inst(IrCmd::STORE_INT, build.vmReg(ra), build.constInt(1));
        build.inst(IrCmd::JUMP, exit);


        build.beginBlock(exit);
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TBOOLEAN));
    }
    else
    {
        IrOp value = build.inst(IrCmd::UINT_TO_NUM, res);
        build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), value);

        if (ra != arg)
            build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));
    }

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinBit32Bnot(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    IrOp va = builtinLoadDouble(build, build.vmReg(arg));

    IrOp vaui = build.inst(IrCmd::NUM_TO_UINT, va);
    IrOp not_ = build.inst(IrCmd::BITNOT_UINT, vaui);
    IrOp value = build.inst(IrCmd::UINT_TO_NUM, not_);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), value);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinBit32Shift(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback, int pcpos)
{
    if (nparams < 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    builtinCheckDouble(build, args, pcpos);

    IrOp va = builtinLoadDouble(build, build.vmReg(arg));
    IrOp vb = builtinLoadDouble(build, args);

    IrOp vaui = build.inst(IrCmd::NUM_TO_UINT, va);

    IrOp vbi;

    if (std::optional<double> vbd = build.function.asDoubleOp(vb); vbd && *vbd >= INT_MIN && *vbd <= INT_MAX)
        vbi = build.constInt(int(*vbd));
    else
        vbi = build.inst(IrCmd::NUM_TO_INT, vb);

    bool knownGoodShift = unsigned(build.function.asIntOp(vbi).value_or(-1)) < 32u;

    if (!knownGoodShift)
    {
        IrOp block = build.block(IrBlockKind::Internal);
        build.inst(IrCmd::JUMP_GE_UINT, vbi, build.constInt(32), fallback, block);
        build.beginBlock(block);
    }

    IrCmd cmd = IrCmd::NOP;
    if (bfid == LBF_BIT32_LSHIFT)
        cmd = IrCmd::BITLSHIFT_UINT;
    else if (bfid == LBF_BIT32_RSHIFT)
        cmd = IrCmd::BITRSHIFT_UINT;
    else if (bfid == LBF_BIT32_ARSHIFT)
        cmd = IrCmd::BITARSHIFT_UINT;

    LUAU_ASSERT(cmd != IrCmd::NOP);

    IrOp shift = build.inst(cmd, vaui, vbi);

    IrOp value = build.inst(IrCmd::UINT_TO_NUM, shift);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), value);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

static BuiltinImplResult translateBuiltinBit32Rotate(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    builtinCheckDouble(build, args, pcpos);

    IrOp va = builtinLoadDouble(build, build.vmReg(arg));
    IrOp vb = builtinLoadDouble(build, args);

    IrOp vaui = build.inst(IrCmd::NUM_TO_UINT, va);
    IrOp vbi = build.inst(IrCmd::NUM_TO_INT, vb);

    IrCmd cmd = (bfid == LBF_BIT32_LROTATE) ? IrCmd::BITLROTATE_UINT : IrCmd::BITRROTATE_UINT;
    IrOp shift = build.inst(cmd, vaui, vbi);

    IrOp value = build.inst(IrCmd::UINT_TO_NUM, shift);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), value);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinBit32Extract(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback, int pcpos)
{
    if (nparams < 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    builtinCheckDouble(build, args, pcpos);

    IrOp va = builtinLoadDouble(build, build.vmReg(arg));
    IrOp vb = builtinLoadDouble(build, args);

    IrOp n = build.inst(IrCmd::NUM_TO_UINT, va);
    IrOp f = build.inst(IrCmd::NUM_TO_INT, vb);

    IrOp value;
    if (nparams == 2)
    {
        IrOp block = build.block(IrBlockKind::Internal);
        build.inst(IrCmd::JUMP_GE_UINT, f, build.constInt(32), fallback, block);
        build.beginBlock(block);

        // TODO: this can be optimized using a bit-select instruction (bt on x86)
        IrOp shift = build.inst(IrCmd::BITRSHIFT_UINT, n, f);
        value = build.inst(IrCmd::BITAND_UINT, shift, build.constInt(1));
    }
    else
    {
        builtinCheckDouble(build, build.vmReg(args.index + 1), pcpos);
        IrOp vc = builtinLoadDouble(build, build.vmReg(args.index + 1));
        IrOp w = build.inst(IrCmd::NUM_TO_INT, vc);

        IrOp block1 = build.block(IrBlockKind::Internal);
        build.inst(IrCmd::JUMP_LT_INT, f, build.constInt(0), fallback, block1);
        build.beginBlock(block1);

        IrOp block2 = build.block(IrBlockKind::Internal);
        build.inst(IrCmd::JUMP_LT_INT, w, build.constInt(1), fallback, block2);
        build.beginBlock(block2);

        IrOp block3 = build.block(IrBlockKind::Internal);
        IrOp fw = build.inst(IrCmd::ADD_INT, f, w);
        build.inst(IrCmd::JUMP_LT_INT, fw, build.constInt(33), block3, fallback);
        build.beginBlock(block3);

        IrOp shift = build.inst(IrCmd::BITLSHIFT_UINT, build.constInt(0xfffffffe), build.inst(IrCmd::SUB_INT, w, build.constInt(1)));
        IrOp m = build.inst(IrCmd::BITNOT_UINT, shift);

        IrOp nf = build.inst(IrCmd::BITRSHIFT_UINT, n, f);
        value = build.inst(IrCmd::BITAND_UINT, nf, m);
    }

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), build.inst(IrCmd::UINT_TO_NUM, value));

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

static BuiltinImplResult translateBuiltinBit32ExtractK(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 2 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);

    IrOp va = builtinLoadDouble(build, build.vmReg(arg));
    IrOp n = build.inst(IrCmd::NUM_TO_UINT, va);

    double a2 = build.function.doubleOp(args);
    int fw = int(a2);

    int f = fw & 31;
    int w1 = fw >> 5;

    uint32_t m = ~(0xfffffffeu << w1);

    IrOp nf = build.inst(IrCmd::BITRSHIFT_UINT, n, build.constInt(f));
    IrOp and_ = build.inst(IrCmd::BITAND_UINT, nf, build.constInt(m));

    IrOp value = build.inst(IrCmd::UINT_TO_NUM, and_);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), value);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinBit32Countz(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    IrOp va = builtinLoadDouble(build, build.vmReg(arg));

    IrOp vaui = build.inst(IrCmd::NUM_TO_UINT, va);

    IrCmd cmd = (bfid == LBF_BIT32_COUNTLZ) ? IrCmd::BITCOUNTLZ_UINT : IrCmd::BITCOUNTRZ_UINT;
    IrOp bin = build.inst(cmd, vaui);

    IrOp value = build.inst(IrCmd::UINT_TO_NUM, bin);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), value);

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinBit32Replace(
    IrBuilder& build, LuauBuiltinFunction bfid, int nparams, int ra, int arg, IrOp args, int nresults, IrOp fallback, int pcpos)
{
    if (nparams < 3 || nresults > 1)
        return {BuiltinImplType::None, -1};

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    builtinCheckDouble(build, args, pcpos);
    builtinCheckDouble(build, build.vmReg(args.index + 1), pcpos);

    IrOp va = builtinLoadDouble(build, build.vmReg(arg));
    IrOp vb = builtinLoadDouble(build, args);
    IrOp vc = builtinLoadDouble(build, build.vmReg(args.index + 1));

    IrOp n = build.inst(IrCmd::NUM_TO_UINT, va);
    IrOp v = build.inst(IrCmd::NUM_TO_UINT, vb);
    IrOp f = build.inst(IrCmd::NUM_TO_INT, vc);

    IrOp value;
    if (nparams == 3)
    {
        IrOp block = build.block(IrBlockKind::Internal);
        build.inst(IrCmd::JUMP_GE_UINT, f, build.constInt(32), fallback, block);
        build.beginBlock(block);

        // TODO: this can be optimized using a bit-select instruction (btr on x86)
        IrOp m = build.constInt(1);
        IrOp shift = build.inst(IrCmd::BITLSHIFT_UINT, m, f);
        IrOp not_ = build.inst(IrCmd::BITNOT_UINT, shift);
        IrOp lhs = build.inst(IrCmd::BITAND_UINT, n, not_);

        IrOp vm = build.inst(IrCmd::BITAND_UINT, v, m);
        IrOp rhs = build.inst(IrCmd::BITLSHIFT_UINT, vm, f);

        value = build.inst(IrCmd::BITOR_UINT, lhs, rhs);
    }
    else
    {
        builtinCheckDouble(build, build.vmReg(args.index + 2), pcpos);
        IrOp vd = builtinLoadDouble(build, build.vmReg(args.index + 2));
        IrOp w = build.inst(IrCmd::NUM_TO_INT, vd);

        IrOp block1 = build.block(IrBlockKind::Internal);
        build.inst(IrCmd::JUMP_LT_INT, f, build.constInt(0), fallback, block1);
        build.beginBlock(block1);

        IrOp block2 = build.block(IrBlockKind::Internal);
        build.inst(IrCmd::JUMP_LT_INT, w, build.constInt(1), fallback, block2);
        build.beginBlock(block2);

        IrOp block3 = build.block(IrBlockKind::Internal);
        IrOp fw = build.inst(IrCmd::ADD_INT, f, w);
        build.inst(IrCmd::JUMP_LT_INT, fw, build.constInt(33), block3, fallback);
        build.beginBlock(block3);

        IrOp shift1 = build.inst(IrCmd::BITLSHIFT_UINT, build.constInt(0xfffffffe), build.inst(IrCmd::SUB_INT, w, build.constInt(1)));
        IrOp m = build.inst(IrCmd::BITNOT_UINT, shift1);

        IrOp shift2 = build.inst(IrCmd::BITLSHIFT_UINT, m, f);
        IrOp not_ = build.inst(IrCmd::BITNOT_UINT, shift2);
        IrOp lhs = build.inst(IrCmd::BITAND_UINT, n, not_);

        IrOp vm = build.inst(IrCmd::BITAND_UINT, v, m);
        IrOp rhs = build.inst(IrCmd::BITLSHIFT_UINT, vm, f);

        value = build.inst(IrCmd::BITOR_UINT, lhs, rhs);
    }

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), build.inst(IrCmd::UINT_TO_NUM, value));

    if (ra != arg)
        build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::UsesFallback, 1};
}

static BuiltinImplResult translateBuiltinVector(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 3 || nresults > 1)
        return {BuiltinImplType::None, -1};

    LUAU_ASSERT(LUA_VECTOR_SIZE == 3);

    builtinCheckDouble(build, build.vmReg(arg), pcpos);
    builtinCheckDouble(build, args, pcpos);
    builtinCheckDouble(build, build.vmReg(vmRegOp(args) + 1), pcpos);

    IrOp x = builtinLoadDouble(build, build.vmReg(arg));
    IrOp y = builtinLoadDouble(build, args);
    IrOp z = builtinLoadDouble(build, build.vmReg(vmRegOp(args) + 1));

    build.inst(IrCmd::STORE_VECTOR, build.vmReg(ra), x, y, z);
    build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TVECTOR));

    return {BuiltinImplType::Full, 1};
}

static BuiltinImplResult translateBuiltinStringLen(IrBuilder& build, int nparams, int ra, int arg, IrOp args, int nresults, int pcpos)
{
    if (nparams < 1 || nresults > 1)
        return {BuiltinImplType::None, -1};

    build.loadAndCheckTag(build.vmReg(arg), LUA_TSTRING, build.vmExit(pcpos));

    IrOp ts = build.inst(IrCmd::LOAD_POINTER, build.vmReg(arg));

    IrOp len = build.inst(IrCmd::STRING_LEN, ts);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(ra), build.inst(IrCmd::INT_TO_NUM, len));
    build.inst(IrCmd::STORE_TAG, build.vmReg(ra), build.constTag(LUA_TNUMBER));

    return {BuiltinImplType::Full, 1};
}

BuiltinImplResult translateBuiltin(IrBuilder& build, int bfid, int ra, int arg, IrOp args, int nparams, int nresults, IrOp fallback, int pcpos)
{
    // Builtins are not allowed to handle variadic arguments
    if (nparams == LUA_MULTRET)
        return {BuiltinImplType::None, -1};

    switch (bfid)
    {
    case LBF_ASSERT:
        return translateBuiltinAssert(build, nparams, ra, arg, args, nresults, pcpos);
    case LBF_MATH_DEG:
        return translateBuiltinMathDeg(build, nparams, ra, arg, args, nresults, pcpos);
    case LBF_MATH_RAD:
        return translateBuiltinMathRad(build, nparams, ra, arg, args, nresults, pcpos);
    case LBF_MATH_LOG:
        return translateBuiltinMathLog(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, pcpos);
    case LBF_MATH_MIN:
        return translateBuiltinMathMin(build, nparams, ra, arg, args, nresults, pcpos);
    case LBF_MATH_MAX:
        return translateBuiltinMathMax(build, nparams, ra, arg, args, nresults, pcpos);
    case LBF_MATH_CLAMP:
        return translateBuiltinMathClamp(build, nparams, ra, arg, args, nresults, fallback, pcpos);
    case LBF_MATH_FLOOR:
        return translateBuiltinMathUnary(build, IrCmd::FLOOR_NUM, nparams, ra, arg, nresults, pcpos);
    case LBF_MATH_CEIL:
        return translateBuiltinMathUnary(build, IrCmd::CEIL_NUM, nparams, ra, arg, nresults, pcpos);
    case LBF_MATH_SQRT:
        return translateBuiltinMathUnary(build, IrCmd::SQRT_NUM, nparams, ra, arg, nresults, pcpos);
    case LBF_MATH_ABS:
        return translateBuiltinMathUnary(build, IrCmd::ABS_NUM, nparams, ra, arg, nresults, pcpos);
    case LBF_MATH_ROUND:
        return translateBuiltinMathUnary(build, IrCmd::ROUND_NUM, nparams, ra, arg, nresults, pcpos);
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
        return translateBuiltinNumberToNumberLibm(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, pcpos);
    case LBF_MATH_SIGN:
        return translateBuiltinNumberToNumber(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, pcpos);
    case LBF_MATH_POW:
    case LBF_MATH_FMOD:
    case LBF_MATH_ATAN2:
        return translateBuiltin2NumberToNumberLibm(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, pcpos);
    case LBF_MATH_LDEXP:
        return translateBuiltinMathLdexp(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, pcpos);
    case LBF_MATH_FREXP:
    case LBF_MATH_MODF:
        return translateBuiltinNumberTo2Number(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, pcpos);
    case LBF_BIT32_BAND:
    case LBF_BIT32_BOR:
    case LBF_BIT32_BXOR:
    case LBF_BIT32_BTEST:
        return translateBuiltinBit32BinaryOp(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, pcpos);
    case LBF_BIT32_BNOT:
        return translateBuiltinBit32Bnot(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, pcpos);
    case LBF_BIT32_LSHIFT:
    case LBF_BIT32_RSHIFT:
    case LBF_BIT32_ARSHIFT:
        return translateBuiltinBit32Shift(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, fallback, pcpos);
    case LBF_BIT32_LROTATE:
    case LBF_BIT32_RROTATE:
        return translateBuiltinBit32Rotate(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, pcpos);
    case LBF_BIT32_EXTRACT:
        return translateBuiltinBit32Extract(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, fallback, pcpos);
    case LBF_BIT32_EXTRACTK:
        return translateBuiltinBit32ExtractK(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, pcpos);
    case LBF_BIT32_COUNTLZ:
    case LBF_BIT32_COUNTRZ:
        return translateBuiltinBit32Countz(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, pcpos);
    case LBF_BIT32_REPLACE:
        return translateBuiltinBit32Replace(build, LuauBuiltinFunction(bfid), nparams, ra, arg, args, nresults, fallback, pcpos);
    case LBF_TYPE:
        return translateBuiltinType(build, nparams, ra, arg, args, nresults);
    case LBF_TYPEOF:
        return translateBuiltinTypeof(build, nparams, ra, arg, args, nresults);
    case LBF_VECTOR:
        return translateBuiltinVector(build, nparams, ra, arg, args, nresults, pcpos);
    case LBF_STRING_LEN:
        return translateBuiltinStringLen(build, nparams, ra, arg, args, nresults, pcpos);
    default:
        return {BuiltinImplType::None, -1};
    }
}

} // namespace CodeGen
} // namespace Luau
