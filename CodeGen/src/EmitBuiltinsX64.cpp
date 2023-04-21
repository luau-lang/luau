// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "EmitBuiltinsX64.h"

#include "Luau/AssemblyBuilderX64.h"
#include "Luau/Bytecode.h"
#include "Luau/IrCallWrapperX64.h"
#include "Luau/IrRegAllocX64.h"

#include "EmitCommonX64.h"
#include "NativeState.h"

#include "lstate.h"

namespace Luau
{
namespace CodeGen
{
namespace X64
{

void emitBuiltinMathLog(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    regs.assertAllFree();
    build.vmovsd(xmm0, luauRegValue(arg));

    // TODO: IR builtin lowering assumes that the only valid 2-argument call is log2; ideally, we use a less hacky way to indicate that
    if (nparams == 2)
        build.call(qword[rNativeContext + offsetof(NativeContext, libm_log2)]);
    else
        build.call(qword[rNativeContext + offsetof(NativeContext, libm_log)]);

    build.vmovsd(luauRegValue(ra), xmm0);
}

void emitBuiltinMathLdexp(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    ScopedRegX64 tmp{regs, SizeX64::qword};
    build.vcvttsd2si(tmp.reg, qword[args + offsetof(TValue, value)]);

    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::xmmword, luauRegValue(arg));
    callWrap.addArgument(SizeX64::qword, tmp);
    callWrap.call(qword[rNativeContext + offsetof(NativeContext, libm_ldexp)]);

    build.vmovsd(luauRegValue(ra), xmm0);
}

void emitBuiltinMathFrexp(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::xmmword, luauRegValue(arg));
    callWrap.addArgument(SizeX64::qword, sTemporarySlot);
    callWrap.call(qword[rNativeContext + offsetof(NativeContext, libm_frexp)]);

    build.vmovsd(luauRegValue(ra), xmm0);

    if (nresults > 1)
    {
        build.vcvtsi2sd(xmm0, xmm0, dword[sTemporarySlot + 0]);
        build.vmovsd(luauRegValue(ra + 1), xmm0);
    }
}

void emitBuiltinMathModf(IrRegAllocX64& regs, AssemblyBuilderX64& build, int nparams, int ra, int arg, OperandX64 args, int nresults)
{
    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::xmmword, luauRegValue(arg));
    callWrap.addArgument(SizeX64::qword, sTemporarySlot);
    callWrap.call(qword[rNativeContext + offsetof(NativeContext, libm_modf)]);

    build.vmovsd(xmm1, qword[sTemporarySlot + 0]);
    build.vmovsd(luauRegValue(ra), xmm1);

    if (nresults > 1)
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
    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::qword, rState);
    callWrap.addArgument(SizeX64::qword, luauRegAddress(arg));
    callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaT_objtypenamestr)]);

    build.mov(luauRegValue(ra), rax);
}

void emitBuiltin(IrRegAllocX64& regs, AssemblyBuilderX64& build, int bfid, int ra, int arg, IrOp args, int nparams, int nresults)
{
    OperandX64 argsOp = 0;

    if (args.kind == IrOpKind::VmReg)
        argsOp = luauRegAddress(vmRegOp(args));
    else if (args.kind == IrOpKind::VmConst)
        argsOp = luauConstantAddress(vmConstOp(args));

    switch (bfid)
    {
    case LBF_MATH_LOG:
        LUAU_ASSERT((nparams == 1 || nparams == 2) && nresults == 1);
        return emitBuiltinMathLog(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_LDEXP:
        LUAU_ASSERT(nparams == 2 && nresults == 1);
        return emitBuiltinMathLdexp(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_FREXP:
        LUAU_ASSERT(nparams == 1 && (nresults == 1 || nresults == 2));
        return emitBuiltinMathFrexp(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_MODF:
        LUAU_ASSERT(nparams == 1 && (nresults == 1 || nresults == 2));
        return emitBuiltinMathModf(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_MATH_SIGN:
        LUAU_ASSERT(nparams == 1 && nresults == 1);
        return emitBuiltinMathSign(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_TYPE:
        LUAU_ASSERT(nparams == 1 && nresults == 1);
        return emitBuiltinType(regs, build, nparams, ra, arg, argsOp, nresults);
    case LBF_TYPEOF:
        LUAU_ASSERT(nparams == 1 && nresults == 1);
        return emitBuiltinTypeof(regs, build, nparams, ra, arg, argsOp, nresults);
    default:
        LUAU_ASSERT(!"Missing x64 lowering");
        break;
    }
}

} // namespace X64
} // namespace CodeGen
} // namespace Luau
