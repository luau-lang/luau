// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "EmitBuiltinsX64.h"

#include "Luau/Bytecode.h"

#include "Luau/AssemblyBuilderX64.h"
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

static void emitBuiltinMathFrexp(IrRegAllocX64& regs, AssemblyBuilderX64& build, int ra, int arg, int nresults)
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

static void emitBuiltinMathModf(IrRegAllocX64& regs, AssemblyBuilderX64& build, int ra, int arg, int nresults)
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

static void emitBuiltinMathSign(IrRegAllocX64& regs, AssemblyBuilderX64& build, int ra, int arg)
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

void emitBuiltin(IrRegAllocX64& regs, AssemblyBuilderX64& build, int bfid, int ra, int arg, OperandX64 arg2, int nparams, int nresults)
{
    switch (bfid)
    {
    case LBF_MATH_FREXP:
        LUAU_ASSERT(nparams == 1 && (nresults == 1 || nresults == 2));
        return emitBuiltinMathFrexp(regs, build, ra, arg, nresults);
    case LBF_MATH_MODF:
        LUAU_ASSERT(nparams == 1 && (nresults == 1 || nresults == 2));
        return emitBuiltinMathModf(regs, build, ra, arg, nresults);
    case LBF_MATH_SIGN:
        LUAU_ASSERT(nparams == 1 && nresults == 1);
        return emitBuiltinMathSign(regs, build, ra, arg);
    default:
        LUAU_ASSERT(!"Missing x64 lowering");
    }
}

} // namespace X64
} // namespace CodeGen
} // namespace Luau
