// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "EmitBuiltinsX64.h"

#include "Luau/AssemblyBuilderX64.h"
#include "Luau/Bytecode.h"

#include "EmitCommonX64.h"
#include "IrRegAllocX64.h"
#include "NativeState.h"

#include "lstate.h"

// TODO: LBF_MATH_FREXP and LBF_MATH_MODF can work for 1 result case if second store is removed

namespace Luau
{
namespace CodeGen
{
namespace X64
{

void emitBuiltinMathFloor(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    ScopedRegX64 tmp{regs, SizeX64::xmmword};
    build.vroundsd(tmp.reg, tmp.reg, luauRegValue(arg), RoundingModeX64::RoundToNegativeInfinity);
    build.vmovsd(luauRegValue(ra), tmp.reg);
}

void emitBuiltinMathCeil(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    ScopedRegX64 tmp{regs, SizeX64::xmmword};
    build.vroundsd(tmp.reg, tmp.reg, luauRegValue(arg), RoundingModeX64::RoundToPositiveInfinity);
    build.vmovsd(luauRegValue(ra), tmp.reg);
}

void emitBuiltinMathSqrt(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    ScopedRegX64 tmp{regs, SizeX64::xmmword};
    build.vsqrtsd(tmp.reg, tmp.reg, luauRegValue(arg));
    build.vmovsd(luauRegValue(ra), tmp.reg);
}

void emitBuiltinMathAbs(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    ScopedRegX64 tmp{regs, SizeX64::xmmword};
    build.vmovsd(tmp.reg, luauRegValue(arg));
    build.vandpd(tmp.reg, tmp.reg, build.i64(~(1LL << 63)));
    build.vmovsd(luauRegValue(ra), tmp.reg);
}

static void emitBuiltinMathSingleArgFunc(IrRegAllocX64& regs, AssemblyBuilderX64& build, int ra, int arg, int32_t offset)
{
    regs.assertAllFree();
    build.vmovsd(xmm0, luauRegValue(arg));
    build.call(qword[rNativeContext + offset]);

    build.vmovsd(luauRegValue(ra), xmm0);
}

void emitBuiltinMathExp(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    emitBuiltinMathSingleArgFunc(regs, build, ra, arg, offsetof(NativeContext, libm_exp));
}

void emitBuiltinMathFmod(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    regs.assertAllFree();
    build.vmovsd(xmm0, luauRegValue(arg));
    build.vmovsd(xmm1, qword[args + offsetof(TValue, value)]);
    build.call(qword[rNativeContext + offsetof(NativeContext, libm_fmod)]);

    build.vmovsd(luauRegValue(ra), xmm0);
}

void emitBuiltinMathPow(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    regs.assertAllFree();
    build.vmovsd(xmm0, luauRegValue(arg));
    build.vmovsd(xmm1, qword[args + offsetof(TValue, value)]);
    build.call(qword[rNativeContext + offsetof(NativeContext, libm_pow)]);

    build.vmovsd(luauRegValue(ra), xmm0);
}

void emitBuiltinMathAsin(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    emitBuiltinMathSingleArgFunc(regs, build, ra, arg, offsetof(NativeContext, libm_asin));
}

void emitBuiltinMathSin(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    emitBuiltinMathSingleArgFunc(regs, build, ra, arg, offsetof(NativeContext, libm_sin));
}

void emitBuiltinMathSinh(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    emitBuiltinMathSingleArgFunc(regs, build, ra, arg, offsetof(NativeContext, libm_sinh));
}

void emitBuiltinMathAcos(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    emitBuiltinMathSingleArgFunc(regs, build, ra, arg, offsetof(NativeContext, libm_acos));
}

void emitBuiltinMathCos(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    emitBuiltinMathSingleArgFunc(regs, build, ra, arg, offsetof(NativeContext, libm_cos));
}

void emitBuiltinMathCosh(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    emitBuiltinMathSingleArgFunc(regs, build, ra, arg, offsetof(NativeContext, libm_cosh));
}

void emitBuiltinMathAtan(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    emitBuiltinMathSingleArgFunc(regs, build, ra, arg, offsetof(NativeContext, libm_atan));
}

void emitBuiltinMathTan(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    emitBuiltinMathSingleArgFunc(regs, build, ra, arg, offsetof(NativeContext, libm_tan));
}

void emitBuiltinMathTanh(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    emitBuiltinMathSingleArgFunc(regs, build, ra, arg, offsetof(NativeContext, libm_tanh));
}

void emitBuiltinMathAtan2(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    regs.assertAllFree();
    build.vmovsd(xmm0, luauRegValue(arg));
    build.vmovsd(xmm1, qword[args + offsetof(TValue, value)]);
    build.call(qword[rNativeContext + offsetof(NativeContext, libm_atan2)]);

    build.vmovsd(luauRegValue(ra), xmm0);
}

void emitBuiltinMathLog10(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    emitBuiltinMathSingleArgFunc(regs, build, ra, arg, offsetof(NativeContext, libm_log10));
}

void emitBuiltinMathLog(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    regs.assertAllFree();
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

        build.vmovsd(xmm1, arg2value);

        jumpOnNumberCmp(build, noreg, build.f64(2.0), xmm1, IrCondition::NotEqual, log10check);

        build.call(qword[rNativeContext + offsetof(NativeContext, libm_log2)]);
        build.jmp(exit);

        build.setLabel(log10check);
        jumpOnNumberCmp(build, noreg, build.f64(10.0), xmm1, IrCondition::NotEqual, logdivlog);

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
}

void emitBuiltinMathLdexp(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    regs.assertAllFree();
    build.vmovsd(xmm0, luauRegValue(arg));

    if (build.abi == ABIX64::Windows)
        build.vcvttsd2si(rArg2, qword[args + offsetof(TValue, value)]);
    else
        build.vcvttsd2si(rArg1, qword[args + offsetof(TValue, value)]);

    build.call(qword[rNativeContext + offsetof(NativeContext, libm_ldexp)]);

    build.vmovsd(luauRegValue(ra), xmm0);
}

void emitBuiltinMathRound(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    ScopedRegX64 tmp0{regs, SizeX64::xmmword};
    ScopedRegX64 tmp1{regs, SizeX64::xmmword};
    ScopedRegX64 tmp2{regs, SizeX64::xmmword};

    build.vmovsd(tmp0.reg, luauRegValue(arg));
    build.vandpd(tmp1.reg, tmp0.reg, build.f64x2(-0.0, -0.0));
    build.vmovsd(tmp2.reg, build.i64(0x3fdfffffffffffff)); // 0.49999999999999994
    build.vorpd(tmp1.reg, tmp1.reg, tmp2.reg);
    build.vaddsd(tmp0.reg, tmp0.reg, tmp1.reg);
    build.vroundsd(tmp0.reg, tmp0.reg, tmp0.reg, RoundingModeX64::RoundToZero);

    build.vmovsd(luauRegValue(ra), tmp0.reg);
}

void emitBuiltinMathFrexp(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    regs.assertAllFree();
    build.vmovsd(xmm0, luauRegValue(arg));

    if (build.abi == ABIX64::Windows)
        build.lea(rArg2, sTemporarySlot);
    else
        build.lea(rArg1, sTemporarySlot);

    build.call(qword[rNativeContext + offsetof(NativeContext, libm_frexp)]);

    build.vmovsd(luauRegValue(ra), xmm0);

    build.vcvtsi2sd(xmm0, xmm0, dword[sTemporarySlot + 0]);
    build.vmovsd(luauRegValue(ra + 1), xmm0);
}

void emitBuiltinMathModf(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    regs.assertAllFree();
    build.vmovsd(xmm0, luauRegValue(arg));

    if (build.abi == ABIX64::Windows)
        build.lea(rArg2, sTemporarySlot);
    else
        build.lea(rArg1, sTemporarySlot);

    build.call(qword[rNativeContext + offsetof(NativeContext, libm_modf)]);

    build.vmovsd(xmm1, qword[sTemporarySlot + 0]);
    build.vmovsd(luauRegValue(ra), xmm1);

    build.vmovsd(luauRegValue(ra + 1), xmm0);
}

void emitBuiltinMathSign(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    ScopedRegX64 tmp0{regs, SizeX64::xmmword};
    ScopedRegX64 tmp1{regs, SizeX64::xmmword};
    ScopedRegX64 tmp2{regs, SizeX64::xmmword};
    ScopedRegX64 tmp3{regs, SizeX64::xmmword};

    build.vmovsd(tmp0.reg, luauRegValue(arg));
    build.vxorpd(tmp1.reg, tmp1.reg, tmp1.reg);

    // Set tmp2 to -1 if arg < 0, else 0
    build.vcmpltsd(tmp2.reg, tmp0.reg, tmp1.reg);
    build.vmovsd(tmp3.reg, build.f64(-1));
    build.vandpd(tmp2.reg, tmp2.reg, tmp3.reg);

    // Set mask bit to 1 if 0 < arg, else 0
    build.vcmpltsd(tmp0.reg, tmp1.reg, tmp0.reg);

    // Result = (mask-bit == 1) ? 1.0 : tmp2
    // If arg < 0 then tmp2 is -1 and mask-bit is 0, result is -1
    // If arg == 0 then tmp2 is 0 and mask-bit is 0, result is 0
    // If arg > 0 then tmp2 is 0 and mask-bit is 1, result is 1
    build.vblendvpd(tmp0.reg, tmp2.reg, build.f64x2(1, 1), tmp0.reg);

    build.vmovsd(luauRegValue(ra), tmp0.reg);
}

void emitBuiltinType(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    ScopedRegX64 tmp0{regs, SizeX64::qword};
    ScopedRegX64 tag{regs, SizeX64::dword};

    build.mov(tag.reg, luauRegTag(arg));

    build.mov(tmp0.reg, qword[rState + offsetof(lua_State, global)]);
    build.mov(tmp0.reg, qword[tmp0.reg + qwordReg(tag.reg) * sizeof(TString*) + offsetof(global_State, ttname)]);

    build.mov(luauRegValue(ra), tmp0.reg);
}

void emitBuiltinTypeof(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    regs.assertAllFree();

    build.mov(rArg1, rState);
    build.lea(rArg2, luauRegAddress(arg));

    build.call(qword[rNativeContext + offsetof(NativeContext, luaT_objtypenamestr)]);

    build.mov(luauRegValue(ra), rax);
}

void emitBuiltin(IrRegAllocX64& regs, AssemblyBuilderX64& build, int bfid, int ra, int arg, IrOp args, int nparams, int nresults)
{
    OperandX64 argsOp = 0;

    if (args.kind == IrOpKind::VmReg)
        argsOp = luauRegAddress(args.index);
    else if (args.kind == IrOpKind::VmConst)
        argsOp = luauConstantAddress(args.index);

    switch (bfid)
    {
    case LBF_ASSERT:
    case LBF_MATH_DEG:
    case LBF_MATH_RAD:
    case LBF_MATH_MIN:
    case LBF_MATH_MAX:
    case LBF_MATH_CLAMP:
        // These instructions are fully translated to IR
        break;
    case LBF_MATH_FLOOR:
        return emitBuiltinMathFloor(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_CEIL:
        return emitBuiltinMathCeil(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_SQRT:
        return emitBuiltinMathSqrt(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_ABS:
        return emitBuiltinMathAbs(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_EXP:
        return emitBuiltinMathExp(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_FMOD:
        return emitBuiltinMathFmod(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_POW:
        return emitBuiltinMathPow(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_ASIN:
        return emitBuiltinMathAsin(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_SIN:
        return emitBuiltinMathSin(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_SINH:
        return emitBuiltinMathSinh(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_ACOS:
        return emitBuiltinMathAcos(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_COS:
        return emitBuiltinMathCos(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_COSH:
        return emitBuiltinMathCosh(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_ATAN:
        return emitBuiltinMathAtan(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_TAN:
        return emitBuiltinMathTan(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_TANH:
        return emitBuiltinMathTanh(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_ATAN2:
        return emitBuiltinMathAtan2(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_LOG10:
        return emitBuiltinMathLog10(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_LOG:
        return emitBuiltinMathLog(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_LDEXP:
        return emitBuiltinMathLdexp(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_ROUND:
        return emitBuiltinMathRound(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_FREXP:
        return emitBuiltinMathFrexp(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_MODF:
        return emitBuiltinMathModf(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_SIGN:
        return emitBuiltinMathSign(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_TYPE:
        return emitBuiltinType(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_TYPEOF:
        return emitBuiltinTypeof(regs, build, nparams, ra, arg, argsOp, nresults);
    default:
        LUAU_ASSERT(!"missing x64 lowering");
        break;
    }
}

} // namespace X64
} // namespace CodeGen
} // namespace Luau
