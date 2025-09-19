// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrLoweringX64.h"

#include "Luau/DenseHash.h"
#include "Luau/IrData.h"
#include "Luau/IrUtils.h"
#include "Luau/LoweringStats.h"

#include "Luau/IrCallWrapperX64.h"

#include "EmitBuiltinsX64.h"
#include "EmitCommonX64.h"
#include "EmitInstructionX64.h"
#include "NativeState.h"

#include "lstate.h"
#include "lgc.h"

LUAU_FASTFLAG(LuauCodeGenUnassignedBcTargetAbort)
LUAU_FASTFLAG(LuauCodeGenDirectBtest)
LUAU_FASTFLAGVARIABLE(LuauCodeGenVBlendpdReorder)
LUAU_FASTFLAG(LuauCodeGenRegAutoSpillA64)
LUAU_FASTFLAG(LuauCodegenDirectCompare)

namespace Luau
{
namespace CodeGen
{
namespace X64
{

IrLoweringX64::IrLoweringX64(AssemblyBuilderX64& build, ModuleHelpers& helpers, IrFunction& function, LoweringStats* stats)
    : build(build)
    , helpers(helpers)
    , function(function)
    , stats(stats)
    , regs(build, function, stats)
    , valueTracker(function)
    , exitHandlerMap(~0u)
{
    valueTracker.setRestoreCallback(
        &regs,
        [](void* context, IrInst& inst)
        {
            ((IrRegAllocX64*)context)->restore(inst, false);
        }
    );

    build.align(kFunctionAlignment, X64::AlignmentDataX64::Ud2);
}

void IrLoweringX64::storeDoubleAsFloat(OperandX64 dst, IrOp src)
{
    ScopedRegX64 tmp{regs, SizeX64::xmmword};

    if (src.kind == IrOpKind::Constant)
    {
        build.vmovss(tmp.reg, build.f32(float(doubleOp(src))));
    }
    else if (src.kind == IrOpKind::Inst)
    {
        build.vcvtsd2ss(tmp.reg, regOp(src), regOp(src));
    }
    else
    {
        CODEGEN_ASSERT(!"Unsupported instruction form");
    }
    build.vmovss(dst, tmp.reg);
}

void IrLoweringX64::lowerInst(IrInst& inst, uint32_t index, const IrBlock& next)
{
    regs.currInstIdx = index;

    valueTracker.beforeInstLowering(inst);

    switch (inst.cmd)
    {
    case IrCmd::LOAD_TAG:
        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        if (inst.a.kind == IrOpKind::VmReg)
            build.mov(inst.regX64, luauRegTag(vmRegOp(inst.a)));
        else if (inst.a.kind == IrOpKind::VmConst)
            build.mov(inst.regX64, luauConstantTag(vmConstOp(inst.a)));
        // If we have a register, we assume it's a pointer to TValue
        // We might introduce explicit operand types in the future to make this more robust
        else if (inst.a.kind == IrOpKind::Inst)
            build.mov(inst.regX64, dword[regOp(inst.a) + offsetof(TValue, tt)]);
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::LOAD_POINTER:
        inst.regX64 = regs.allocReg(SizeX64::qword, index);

        if (inst.a.kind == IrOpKind::VmReg)
            build.mov(inst.regX64, luauRegValue(vmRegOp(inst.a)));
        else if (inst.a.kind == IrOpKind::VmConst)
            build.mov(inst.regX64, luauConstantValue(vmConstOp(inst.a)));
        // If we have a register, we assume it's a pointer to TValue
        // We might introduce explicit operand types in the future to make this more robust
        else if (inst.a.kind == IrOpKind::Inst)
            build.mov(inst.regX64, qword[regOp(inst.a) + offsetof(TValue, value)]);
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::LOAD_DOUBLE:
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        if (inst.a.kind == IrOpKind::VmReg)
            build.vmovsd(inst.regX64, luauRegValue(vmRegOp(inst.a)));
        else if (inst.a.kind == IrOpKind::VmConst)
            build.vmovsd(inst.regX64, luauConstantValue(vmConstOp(inst.a)));
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::LOAD_INT:
        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        build.mov(inst.regX64, luauRegValueInt(vmRegOp(inst.a)));
        break;
    case IrCmd::LOAD_FLOAT:
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        if (inst.a.kind == IrOpKind::VmReg)
            build.vcvtss2sd(inst.regX64, inst.regX64, dword[rBase + vmRegOp(inst.a) * sizeof(TValue) + offsetof(TValue, value) + intOp(inst.b)]);
        else if (inst.a.kind == IrOpKind::VmConst)
            build.vcvtss2sd(
                inst.regX64, inst.regX64, dword[rConstants + vmConstOp(inst.a) * sizeof(TValue) + offsetof(TValue, value) + intOp(inst.b)]
            );
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::LOAD_TVALUE:
    {
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        int addrOffset = inst.b.kind != IrOpKind::None ? intOp(inst.b) : 0;

        if (inst.a.kind == IrOpKind::VmReg)
            build.vmovups(inst.regX64, luauReg(vmRegOp(inst.a)));
        else if (inst.a.kind == IrOpKind::VmConst)
            build.vmovups(inst.regX64, luauConstant(vmConstOp(inst.a)));
        else if (inst.a.kind == IrOpKind::Inst)
            build.vmovups(inst.regX64, xmmword[regOp(inst.a) + addrOffset]);
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");
        break;
    }
    case IrCmd::LOAD_ENV:
        inst.regX64 = regs.allocReg(SizeX64::qword, index);

        build.mov(inst.regX64, sClosure);
        build.mov(inst.regX64, qword[inst.regX64 + offsetof(Closure, env)]);
        break;
    case IrCmd::GET_ARR_ADDR:
        if (inst.b.kind == IrOpKind::Inst)
        {
            inst.regX64 = regs.allocRegOrReuse(SizeX64::qword, index, {inst.b});

            if (dwordReg(inst.regX64) != regOp(inst.b))
                build.mov(dwordReg(inst.regX64), regOp(inst.b));

            build.shl(dwordReg(inst.regX64), kTValueSizeLog2);
            build.add(inst.regX64, qword[regOp(inst.a) + offsetof(LuaTable, array)]);
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            inst.regX64 = regs.allocRegOrReuse(SizeX64::qword, index, {inst.a});

            build.mov(inst.regX64, qword[regOp(inst.a) + offsetof(LuaTable, array)]);

            if (intOp(inst.b) != 0)
                build.lea(inst.regX64, addr[inst.regX64 + intOp(inst.b) * sizeof(TValue)]);
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::GET_SLOT_NODE_ADDR:
    {
        inst.regX64 = regs.allocReg(SizeX64::qword, index);

        ScopedRegX64 tmp{regs, SizeX64::qword};

        getTableNodeAtCachedSlot(build, tmp.reg, inst.regX64, regOp(inst.a), uintOp(inst.b));
        break;
    }
    case IrCmd::GET_HASH_NODE_ADDR:
    {
        // Custom bit shift value can only be placed in cl
        ScopedRegX64 shiftTmp{regs, regs.takeReg(rcx, kInvalidInstIdx)};

        inst.regX64 = regs.allocReg(SizeX64::qword, index);

        ScopedRegX64 tmp{regs, SizeX64::qword};

        build.mov(inst.regX64, qword[regOp(inst.a) + offsetof(LuaTable, node)]);
        build.mov(dwordReg(tmp.reg), 1);
        build.mov(byteReg(shiftTmp.reg), byte[regOp(inst.a) + offsetof(LuaTable, lsizenode)]);
        build.shl(dwordReg(tmp.reg), byteReg(shiftTmp.reg));
        build.dec(dwordReg(tmp.reg));
        build.and_(dwordReg(tmp.reg), uintOp(inst.b));
        build.shl(tmp.reg, kLuaNodeSizeLog2);
        build.add(inst.regX64, tmp.reg);
        break;
    };
    case IrCmd::GET_CLOSURE_UPVAL_ADDR:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::qword, index, {inst.a});

        if (inst.a.kind == IrOpKind::Undef)
        {
            build.mov(inst.regX64, sClosure);
        }
        else
        {
            RegisterX64 cl = regOp(inst.a);
            if (inst.regX64 != cl)
                build.mov(inst.regX64, cl);
        }

        build.add(inst.regX64, offsetof(Closure, l.uprefs) + sizeof(TValue) * vmUpvalueOp(inst.b));
        break;
    }
    case IrCmd::STORE_TAG:
        if (inst.b.kind == IrOpKind::Constant)
        {
            if (inst.a.kind == IrOpKind::Inst)
                build.mov(dword[regOp(inst.a) + offsetof(TValue, tt)], tagOp(inst.b));
            else
                build.mov(luauRegTag(vmRegOp(inst.a)), tagOp(inst.b));
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::STORE_POINTER:
    {
        OperandX64 valueLhs = inst.a.kind == IrOpKind::Inst ? qword[regOp(inst.a) + offsetof(TValue, value)] : luauRegValue(vmRegOp(inst.a));

        if (inst.b.kind == IrOpKind::Constant)
        {
            CODEGEN_ASSERT(intOp(inst.b) == 0);
            build.mov(valueLhs, 0);
        }
        else if (inst.b.kind == IrOpKind::Inst)
        {
            build.mov(valueLhs, regOp(inst.b));
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    }
    case IrCmd::STORE_EXTRA:
        if (inst.b.kind == IrOpKind::Constant)
        {
            if (inst.a.kind == IrOpKind::Inst)
                build.mov(dword[regOp(inst.a) + offsetof(TValue, extra)], intOp(inst.b));
            else
                build.mov(luauRegExtra(vmRegOp(inst.a)), intOp(inst.b));
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::STORE_DOUBLE:
    {
        OperandX64 valueLhs = inst.a.kind == IrOpKind::Inst ? qword[regOp(inst.a) + offsetof(TValue, value)] : luauRegValue(vmRegOp(inst.a));

        if (inst.b.kind == IrOpKind::Constant)
        {
            ScopedRegX64 tmp{regs, SizeX64::xmmword};

            build.vmovsd(tmp.reg, build.f64(doubleOp(inst.b)));
            build.vmovsd(valueLhs, tmp.reg);
        }
        else if (inst.b.kind == IrOpKind::Inst)
        {
            build.vmovsd(valueLhs, regOp(inst.b));
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    }
    case IrCmd::STORE_INT:
        if (inst.b.kind == IrOpKind::Constant)
            build.mov(luauRegValueInt(vmRegOp(inst.a)), intOp(inst.b));
        else if (inst.b.kind == IrOpKind::Inst)
            build.mov(luauRegValueInt(vmRegOp(inst.a)), regOp(inst.b));
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::STORE_VECTOR:
        storeDoubleAsFloat(luauRegValueVector(vmRegOp(inst.a), 0), inst.b);
        storeDoubleAsFloat(luauRegValueVector(vmRegOp(inst.a), 1), inst.c);
        storeDoubleAsFloat(luauRegValueVector(vmRegOp(inst.a), 2), inst.d);

        if (inst.e.kind != IrOpKind::None)
            build.mov(luauRegTag(vmRegOp(inst.a)), tagOp(inst.e));
        break;
    case IrCmd::STORE_TVALUE:
    {
        int addrOffset = inst.c.kind != IrOpKind::None ? intOp(inst.c) : 0;

        if (inst.a.kind == IrOpKind::VmReg)
            build.vmovups(luauReg(vmRegOp(inst.a)), regOp(inst.b));
        else if (inst.a.kind == IrOpKind::Inst)
            build.vmovups(xmmword[regOp(inst.a) + addrOffset], regOp(inst.b));
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");
        break;
    }
    case IrCmd::STORE_SPLIT_TVALUE:
    {
        int addrOffset = inst.d.kind != IrOpKind::None ? intOp(inst.d) : 0;

        OperandX64 tagLhs = inst.a.kind == IrOpKind::Inst ? dword[regOp(inst.a) + offsetof(TValue, tt) + addrOffset] : luauRegTag(vmRegOp(inst.a));
        build.mov(tagLhs, tagOp(inst.b));

        if (tagOp(inst.b) == LUA_TBOOLEAN)
        {
            OperandX64 valueLhs =
                inst.a.kind == IrOpKind::Inst ? dword[regOp(inst.a) + offsetof(TValue, value) + addrOffset] : luauRegValueInt(vmRegOp(inst.a));
            build.mov(valueLhs, inst.c.kind == IrOpKind::Constant ? OperandX64(intOp(inst.c)) : regOp(inst.c));
        }
        else if (tagOp(inst.b) == LUA_TNUMBER)
        {
            OperandX64 valueLhs =
                inst.a.kind == IrOpKind::Inst ? qword[regOp(inst.a) + offsetof(TValue, value) + addrOffset] : luauRegValue(vmRegOp(inst.a));

            if (inst.c.kind == IrOpKind::Constant)
            {
                ScopedRegX64 tmp{regs, SizeX64::xmmword};

                build.vmovsd(tmp.reg, build.f64(doubleOp(inst.c)));
                build.vmovsd(valueLhs, tmp.reg);
            }
            else
            {
                build.vmovsd(valueLhs, regOp(inst.c));
            }
        }
        else if (isGCO(tagOp(inst.b)))
        {
            OperandX64 valueLhs =
                inst.a.kind == IrOpKind::Inst ? qword[regOp(inst.a) + offsetof(TValue, value) + addrOffset] : luauRegValue(vmRegOp(inst.a));
            build.mov(valueLhs, regOp(inst.c));
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    }
    case IrCmd::ADD_INT:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.a.kind == IrOpKind::Constant)
        {
            build.lea(inst.regX64, addr[regOp(inst.b) + intOp(inst.a)]);
        }
        else if (inst.a.kind == IrOpKind::Inst)
        {
            if (inst.regX64 == regOp(inst.a))
            {
                if (inst.b.kind == IrOpKind::Inst)
                    build.add(inst.regX64, regOp(inst.b));
                else if (intOp(inst.b) == 1)
                    build.inc(inst.regX64);
                else
                    build.add(inst.regX64, intOp(inst.b));
            }
            else
            {
                if (inst.b.kind == IrOpKind::Inst)
                    build.lea(inst.regX64, addr[regOp(inst.a) + regOp(inst.b)]);
                else
                    build.lea(inst.regX64, addr[regOp(inst.a) + intOp(inst.b)]);
            }
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    }
    case IrCmd::SUB_INT:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.regX64 == regOp(inst.a) && intOp(inst.b) == 1)
            build.dec(inst.regX64);
        else if (inst.regX64 == regOp(inst.a))
            build.sub(inst.regX64, intOp(inst.b));
        else
            build.lea(inst.regX64, addr[regOp(inst.a) - intOp(inst.b)]);
        break;
    case IrCmd::ADD_NUM:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        if (inst.a.kind == IrOpKind::Constant)
        {
            ScopedRegX64 tmp{regs, SizeX64::xmmword};

            build.vmovsd(tmp.reg, memRegDoubleOp(inst.a));
            build.vaddsd(inst.regX64, tmp.reg, memRegDoubleOp(inst.b));
        }
        else
        {
            build.vaddsd(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b));
        }
        break;
    case IrCmd::SUB_NUM:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        if (inst.a.kind == IrOpKind::Constant)
        {
            ScopedRegX64 tmp{regs, SizeX64::xmmword};

            build.vmovsd(tmp.reg, memRegDoubleOp(inst.a));
            build.vsubsd(inst.regX64, tmp.reg, memRegDoubleOp(inst.b));
        }
        else
        {
            build.vsubsd(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b));
        }
        break;
    case IrCmd::MUL_NUM:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        if (inst.a.kind == IrOpKind::Constant)
        {
            ScopedRegX64 tmp{regs, SizeX64::xmmword};

            build.vmovsd(tmp.reg, memRegDoubleOp(inst.a));
            build.vmulsd(inst.regX64, tmp.reg, memRegDoubleOp(inst.b));
        }
        else
        {
            build.vmulsd(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b));
        }
        break;
    case IrCmd::DIV_NUM:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        if (inst.a.kind == IrOpKind::Constant)
        {
            ScopedRegX64 tmp{regs, SizeX64::xmmword};

            build.vmovsd(tmp.reg, memRegDoubleOp(inst.a));
            build.vdivsd(inst.regX64, tmp.reg, memRegDoubleOp(inst.b));
        }
        else
        {
            build.vdivsd(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b));
        }
        break;
    case IrCmd::IDIV_NUM:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        if (inst.a.kind == IrOpKind::Constant)
        {
            ScopedRegX64 tmp{regs, SizeX64::xmmword};

            build.vmovsd(tmp.reg, memRegDoubleOp(inst.a));
            build.vdivsd(inst.regX64, tmp.reg, memRegDoubleOp(inst.b));
        }
        else
        {
            build.vdivsd(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b));
        }
        build.vroundsd(inst.regX64, inst.regX64, inst.regX64, RoundingModeX64::RoundToNegativeInfinity);
        break;
    case IrCmd::MULADD_NUM:
    {
        if ((build.features & Feature_FMA3) != 0)
        {
            if (inst.a.kind != IrOpKind::Inst)
            {
                inst.regX64 = regs.allocReg(SizeX64::xmmword, index);
                build.vmovsd(inst.regX64, memRegDoubleOp(inst.a));
            }
            else
            {
                inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a});
                RegisterX64 aReg = regOp(inst.a);
                if (inst.regX64 != aReg)
                    build.vmovupd(inst.regX64, aReg);
            }

            ScopedRegX64 optBTmp{regs};
            RegisterX64 bReg{};

            if (inst.b.kind == IrOpKind::Constant)
            {
                optBTmp.alloc(SizeX64::xmmword);

                build.vmovsd(optBTmp.reg, memRegDoubleOp(inst.b));
                bReg = optBTmp.reg;
            }
            else
            {
                bReg = regOp(inst.b);
            }

            build.vfmadd213pd(inst.regX64, bReg, memRegDoubleOp(inst.c));
        }
        else
        {
            inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a});

            if (inst.a.kind != IrOpKind::Inst && inst.b.kind != IrOpKind::Inst)
            {
                build.vmovsd(inst.regX64, memRegDoubleOp(inst.a));
                build.vmulsd(inst.regX64, inst.regX64, memRegDoubleOp(inst.b));
            }
            else if (inst.a.kind == IrOpKind::Inst)
            {
                build.vmulsd(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b));
            }
            else
            {
                CODEGEN_ASSERT(inst.b.kind == IrOpKind::Inst);
                build.vmulsd(inst.regX64, regOp(inst.b), memRegDoubleOp(inst.a));
            }

            build.vaddsd(inst.regX64, inst.regX64, memRegDoubleOp(inst.c));
        }
        break;
    }
    case IrCmd::MOD_NUM:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        ScopedRegX64 optLhsTmp{regs};
        RegisterX64 lhs;

        if (inst.a.kind == IrOpKind::Constant)
        {
            optLhsTmp.alloc(SizeX64::xmmword);

            build.vmovsd(optLhsTmp.reg, memRegDoubleOp(inst.a));
            lhs = optLhsTmp.reg;
        }
        else
        {
            lhs = regOp(inst.a);
        }

        if (inst.b.kind == IrOpKind::Inst)
        {
            ScopedRegX64 tmp{regs, SizeX64::xmmword};

            build.vdivsd(tmp.reg, lhs, memRegDoubleOp(inst.b));
            build.vroundsd(tmp.reg, tmp.reg, tmp.reg, RoundingModeX64::RoundToNegativeInfinity);
            build.vmulsd(tmp.reg, tmp.reg, memRegDoubleOp(inst.b));
            build.vsubsd(inst.regX64, lhs, tmp.reg);
        }
        else
        {
            ScopedRegX64 tmp1{regs, SizeX64::xmmword};
            ScopedRegX64 tmp2{regs, SizeX64::xmmword};

            build.vmovsd(tmp1.reg, memRegDoubleOp(inst.b));
            build.vdivsd(tmp2.reg, lhs, tmp1.reg);
            build.vroundsd(tmp2.reg, tmp2.reg, tmp2.reg, RoundingModeX64::RoundToNegativeInfinity);
            build.vmulsd(tmp1.reg, tmp2.reg, tmp1.reg);
            build.vsubsd(inst.regX64, lhs, tmp1.reg);
        }
        break;
    }
    case IrCmd::MIN_NUM:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        if (inst.a.kind == IrOpKind::Constant)
        {
            ScopedRegX64 tmp{regs, SizeX64::xmmword};

            build.vmovsd(tmp.reg, memRegDoubleOp(inst.a));
            build.vminsd(inst.regX64, tmp.reg, memRegDoubleOp(inst.b));
        }
        else
        {
            build.vminsd(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b));
        }
        break;
    case IrCmd::MAX_NUM:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        if (inst.a.kind == IrOpKind::Constant)
        {
            ScopedRegX64 tmp{regs, SizeX64::xmmword};

            build.vmovsd(tmp.reg, memRegDoubleOp(inst.a));
            build.vmaxsd(inst.regX64, tmp.reg, memRegDoubleOp(inst.b));
        }
        else
        {
            build.vmaxsd(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b));
        }
        break;
    case IrCmd::UNM_NUM:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a});

        build.vxorpd(inst.regX64, regOp(inst.a), build.f64(-0.0));
        break;
    }
    case IrCmd::FLOOR_NUM:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a});

        build.vroundsd(inst.regX64, inst.regX64, memRegDoubleOp(inst.a), RoundingModeX64::RoundToNegativeInfinity);
        break;
    case IrCmd::CEIL_NUM:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a});

        build.vroundsd(inst.regX64, inst.regX64, memRegDoubleOp(inst.a), RoundingModeX64::RoundToPositiveInfinity);
        break;
    case IrCmd::ROUND_NUM:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a});

        ScopedRegX64 tmp1{regs, SizeX64::xmmword};
        ScopedRegX64 tmp2{regs, SizeX64::xmmword};

        if (inst.a.kind != IrOpKind::Inst)
            build.vmovsd(inst.regX64, memRegDoubleOp(inst.a));
        else if (regOp(inst.a) != inst.regX64)
            build.vmovsd(inst.regX64, inst.regX64, regOp(inst.a));

        build.vandpd(tmp1.reg, inst.regX64, build.f64x2(-0.0, -0.0));
        build.vmovsd(tmp2.reg, build.i64(0x3fdfffffffffffff)); // 0.49999999999999994
        build.vorpd(tmp1.reg, tmp1.reg, tmp2.reg);
        build.vaddsd(inst.regX64, inst.regX64, tmp1.reg);
        build.vroundsd(inst.regX64, inst.regX64, inst.regX64, RoundingModeX64::RoundToZero);
        break;
    }
    case IrCmd::SQRT_NUM:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a});

        build.vsqrtsd(inst.regX64, inst.regX64, memRegDoubleOp(inst.a));
        break;
    case IrCmd::ABS_NUM:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a});

        if (inst.a.kind != IrOpKind::Inst)
            build.vmovsd(inst.regX64, memRegDoubleOp(inst.a));
        else if (regOp(inst.a) != inst.regX64)
            build.vmovsd(inst.regX64, inst.regX64, regOp(inst.a));

        build.vandpd(inst.regX64, inst.regX64, build.i64(~(1LL << 63)));
        break;
    case IrCmd::SIGN_NUM:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a});

        ScopedRegX64 tmp0{regs, SizeX64::xmmword};
        ScopedRegX64 tmp1{regs, SizeX64::xmmword};
        ScopedRegX64 tmp2{regs, SizeX64::xmmword};

        build.vxorpd(tmp0.reg, tmp0.reg, tmp0.reg);

        // Set tmp1 to -1 if arg < 0, else 0
        build.vcmpltsd(tmp1.reg, regOp(inst.a), tmp0.reg);
        build.vmovsd(tmp2.reg, build.f64(-1));
        build.vandpd(tmp1.reg, tmp1.reg, tmp2.reg);

        // Set mask bit to 1 if 0 < arg, else 0
        build.vcmpltsd(inst.regX64, tmp0.reg, regOp(inst.a));

        // Result = (mask-bit == 1) ? 1.0 : tmp1
        // If arg < 0 then tmp1 is -1 and mask-bit is 0, result is -1
        // If arg == 0 then tmp1 is 0 and mask-bit is 0, result is 0
        // If arg > 0 then tmp1 is 0 and mask-bit is 1, result is 1
        if (FFlag::LuauCodeGenVBlendpdReorder)
            build.vblendvpd(inst.regX64, tmp1.reg, inst.regX64, build.f64x2(1, 1));
        else
            build.vblendvpd_DEPRECATED(inst.regX64, tmp1.reg, build.f64x2(1, 1), inst.regX64);
        break;
    }
    case IrCmd::SELECT_NUM:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.c, inst.d}); // can't reuse b if a is a memory operand

        ScopedRegX64 tmp{regs, SizeX64::xmmword};

        if (inst.c.kind == IrOpKind::Inst)
            build.vcmpeqsd(tmp.reg, regOp(inst.c), memRegDoubleOp(inst.d));
        else
        {
            build.vmovsd(tmp.reg, memRegDoubleOp(inst.c));
            build.vcmpeqsd(tmp.reg, tmp.reg, memRegDoubleOp(inst.d));
        }

        if (inst.a.kind == IrOpKind::Inst)
            if (FFlag::LuauCodeGenVBlendpdReorder)
                build.vblendvpd(inst.regX64, regOp(inst.a), tmp.reg, memRegDoubleOp(inst.b));
            else
                build.vblendvpd_DEPRECATED(inst.regX64, regOp(inst.a), memRegDoubleOp(inst.b), tmp.reg);
        else
        {
            build.vmovsd(inst.regX64, memRegDoubleOp(inst.a));
            if (FFlag::LuauCodeGenVBlendpdReorder)
                build.vblendvpd(inst.regX64, inst.regX64, tmp.reg, memRegDoubleOp(inst.b));
            else
                build.vblendvpd_DEPRECATED(inst.regX64, inst.regX64, memRegDoubleOp(inst.b), tmp.reg);
        }
        break;
    }
    case IrCmd::SELECT_VEC:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.c, inst.d});

        ScopedRegX64 tmp1{regs};
        ScopedRegX64 tmp2{regs};
        RegisterX64 tmpc = vecOp(inst.c, tmp1);
        RegisterX64 tmpd = vecOp(inst.d, tmp2);

        build.vcmpeqps(inst.regX64, tmpc, tmpd);
        build.vblendvps(inst.regX64, vecOp(inst.a, tmp1), vecOp(inst.b, tmp2), inst.regX64);

        break;
    }
    case IrCmd::ADD_VEC:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        ScopedRegX64 tmp1{regs};
        ScopedRegX64 tmp2{regs};

        RegisterX64 tmpa = vecOp(inst.a, tmp1);
        RegisterX64 tmpb = (inst.a == inst.b) ? tmpa : vecOp(inst.b, tmp2);

        build.vaddps(inst.regX64, tmpa, tmpb);
        break;
    }
    case IrCmd::SUB_VEC:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        ScopedRegX64 tmp1{regs};
        ScopedRegX64 tmp2{regs};

        RegisterX64 tmpa = vecOp(inst.a, tmp1);
        RegisterX64 tmpb = (inst.a == inst.b) ? tmpa : vecOp(inst.b, tmp2);

        build.vsubps(inst.regX64, tmpa, tmpb);
        break;
    }
    case IrCmd::MUL_VEC:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        ScopedRegX64 tmp1{regs};
        ScopedRegX64 tmp2{regs};

        RegisterX64 tmpa = vecOp(inst.a, tmp1);
        RegisterX64 tmpb = (inst.a == inst.b) ? tmpa : vecOp(inst.b, tmp2);

        build.vmulps(inst.regX64, tmpa, tmpb);
        break;
    }
    case IrCmd::DIV_VEC:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        ScopedRegX64 tmp1{regs};
        ScopedRegX64 tmp2{regs};

        RegisterX64 tmpa = vecOp(inst.a, tmp1);
        RegisterX64 tmpb = (inst.a == inst.b) ? tmpa : vecOp(inst.b, tmp2);

        build.vdivps(inst.regX64, tmpa, tmpb);
        break;
    }
    case IrCmd::MULADD_VEC:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a});
        ScopedRegX64 tmp1{regs};
        ScopedRegX64 tmp2{regs};
        ScopedRegX64 tmp3{regs};

        RegisterX64 tmpa = vecOp(inst.a, tmp1);
        RegisterX64 tmpb = vecOp(inst.b, tmp2);
        RegisterX64 tmpc = vecOp(inst.c, tmp3);

        if ((build.features & Feature_FMA3) != 0)
        {
            if (inst.regX64 != tmpa)
                build.vmovups(inst.regX64, tmpa);

            build.vfmadd213ps(inst.regX64, tmpb, tmpc);
        }
        else
        {
            build.vmulps(inst.regX64, tmpa, tmpb);
            build.vaddps(inst.regX64, inst.regX64, tmpc);
        }

        break;
    }
    case IrCmd::UNM_VEC:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a});

        build.vxorpd(inst.regX64, regOp(inst.a), build.f32x4(-0.0, -0.0, -0.0, -0.0));
        break;
    }
    case IrCmd::DOT_VEC:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a, inst.b});

        ScopedRegX64 tmp1{regs};
        ScopedRegX64 tmp2{regs};

        RegisterX64 tmpa = vecOp(inst.a, tmp1);
        RegisterX64 tmpb = (inst.a == inst.b) ? tmpa : vecOp(inst.b, tmp2);

        build.vdpps(inst.regX64, tmpa, tmpb, 0x71); // 7 = 0b0111, sum first 3 products into first float
        build.vcvtss2sd(inst.regX64, inst.regX64, inst.regX64);
        break;
    }
    case IrCmd::NOT_ANY:
    {
        // TODO: if we have a single user which is a STORE_INT, we are missing the opportunity to write directly to target
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a, inst.b});

        Label saveOne, saveZero, exit;

        if (inst.a.kind == IrOpKind::Constant)
        {
            // Other cases should've been constant folded
            CODEGEN_ASSERT(tagOp(inst.a) == LUA_TBOOLEAN);
        }
        else
        {
            build.cmp(regOp(inst.a), LUA_TNIL);
            build.jcc(ConditionX64::Equal, saveOne);

            build.cmp(regOp(inst.a), LUA_TBOOLEAN);
            build.jcc(ConditionX64::NotEqual, saveZero);
        }

        if (inst.b.kind == IrOpKind::Constant)
        {
            // If value is 1, we fallthrough to storing 0
            if (intOp(inst.b) == 0)
                build.jmp(saveOne);
        }
        else
        {
            build.cmp(regOp(inst.b), 0);
            build.jcc(ConditionX64::Equal, saveOne);
        }

        build.setLabel(saveZero);
        build.mov(inst.regX64, 0);
        build.jmp(exit);

        build.setLabel(saveOne);
        build.mov(inst.regX64, 1);

        build.setLabel(exit);
        break;
    }
    case IrCmd::CMP_INT:
    {
        CODEGEN_ASSERT(FFlag::LuauCodeGenDirectBtest);

        // Cannot reuse operand registers as a target because we have to modify it before the comparison
        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        // We are going to operate on byte register, those do not clear high bits on write
        build.xor_(inst.regX64, inst.regX64);

        IrCondition cond = conditionOp(inst.c);

        if (inst.a.kind == IrOpKind::Constant)
        {
            build.cmp(regOp(inst.b), intOp(inst.a));
            build.setcc(getInverseCondition(getConditionInt(cond)), byteReg(inst.regX64));
        }
        else if (inst.a.kind == IrOpKind::Inst)
        {
            build.cmp(regOp(inst.a), intOp(inst.b));
            build.setcc(getConditionInt(cond), byteReg(inst.regX64));
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    }
    case IrCmd::CMP_ANY:
    {
        IrCondition cond = conditionOp(inst.c);

        IrCallWrapperX64 callWrap(regs, build);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::qword, luauRegAddress(vmRegOp(inst.a)));
        callWrap.addArgument(SizeX64::qword, luauRegAddress(vmRegOp(inst.b)));

        if (cond == IrCondition::LessEqual)
            callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_lessequal)]);
        else if (cond == IrCondition::Less)
            callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_lessthan)]);
        else if (cond == IrCondition::Equal)
            callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_equalval)]);
        else
            CODEGEN_ASSERT(!"Unsupported condition");

        emitUpdateBase(build);

        inst.regX64 = regs.takeReg(eax, index);
        break;
    }
    case IrCmd::CMP_TAG:
    {
        CODEGEN_ASSERT(FFlag::LuauCodegenDirectCompare);
        // Cannot reuse operand registers as a target because we have to modify it before the comparison
        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        // We are going to operate on byte register, those do not clear high bits on write
        build.xor_(inst.regX64, inst.regX64);

        IrCondition cond = conditionOp(inst.c);
        CODEGEN_ASSERT(cond == IrCondition::Equal || cond == IrCondition::NotEqual);
        ConditionX64 condX64 = getConditionInt(cond);

        if (tagOp(inst.b) == LUA_TNIL && inst.a.kind == IrOpKind::Inst)
            build.test(regOp(inst.a), regOp(inst.a));
        else
            build.cmp(memRegTagOp(inst.a), tagOp(inst.b));

        build.setcc(condX64, byteReg(inst.regX64));

        break;
    }
    case IrCmd::CMP_SPLIT_TVALUE:
    {
        CODEGEN_ASSERT(FFlag::LuauCodegenDirectCompare);
        // Cannot reuse operand registers as a target because we have to modify it before the comparison
        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        // Second operand of this instruction must be a constant
        // Without a constant type, we wouldn't know the correct way to compare the values at lowering time
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::Constant);

        // We are going to operate on byte registers, those do not clear high bits on write
        build.xor_(inst.regX64, inst.regX64);

        IrCondition cond = conditionOp(inst.e);
        CODEGEN_ASSERT(cond == IrCondition::Equal || cond == IrCondition::NotEqual);

        // Check tag equality first
        ScopedRegX64 tmp1{regs, SizeX64::byte};

        if (inst.a.kind != IrOpKind::Constant)
        {
            build.cmp(regOp(inst.a), tagOp(inst.b));
            build.setcc(getConditionInt(cond), byteReg(tmp1.reg));
        }
        else
        {
            // Constant folding had to handle different constant tags
            CODEGEN_ASSERT(tagOp(inst.a) == tagOp(inst.b));
        }

        if (tagOp(inst.b) == LUA_TBOOLEAN)
        {
            if (inst.c.kind == IrOpKind::Constant)
                build.cmp(regOp(inst.d), intOp(inst.c)); // swapped arguments
            else if (inst.d.kind == IrOpKind::Constant)
                build.cmp(regOp(inst.c), intOp(inst.d));
            else
                build.cmp(regOp(inst.c), regOp(inst.d));

            build.setcc(getConditionInt(cond), byteReg(inst.regX64));
        }
        else if (tagOp(inst.b) == LUA_TSTRING)
        {
            build.cmp(regOp(inst.c), regOp(inst.d));
            build.setcc(getConditionInt(cond), byteReg(inst.regX64));
        }
        else if (tagOp(inst.b) == LUA_TNUMBER)
        {
            if (inst.c.kind == IrOpKind::Constant)
                build.vucomisd(regOp(inst.d), memRegDoubleOp(inst.c)); // swapped arguments
            else if (inst.d.kind == IrOpKind::Constant)
                build.vucomisd(regOp(inst.c), memRegDoubleOp(inst.d));
            else
                build.vucomisd(regOp(inst.c), regOp(inst.d));

            ScopedRegX64 tmp2{regs, SizeX64::dword};

            if (cond == IrCondition::Equal)
            {
                build.mov(tmp2.reg, 0);
                build.setcc(ConditionX64::NotParity, byteReg(inst.regX64));
                build.cmov(ConditionX64::NotEqual, inst.regX64, tmp2.reg);
            }
            else
            {
                build.mov(tmp2.reg, 1);
                build.setcc(ConditionX64::Parity, byteReg(inst.regX64));
                build.cmov(ConditionX64::NotEqual, inst.regX64, tmp2.reg);
            }
        }
        else
        {
            CODEGEN_ASSERT(!"unsupported type tag in CMP_SPLIT_TVALUE");
        }

        if (inst.a.kind != IrOpKind::Constant)
        {
            if (cond == IrCondition::Equal)
                build.and_(byteReg(inst.regX64), byteReg(tmp1.reg));
            else
                build.or_(byteReg(inst.regX64), byteReg(tmp1.reg));
        }
        break;
    }
    case IrCmd::JUMP:
        jumpOrAbortOnUndef(inst.a, next);
        break;
    case IrCmd::JUMP_IF_TRUTHY:
        jumpIfTruthy(build, vmRegOp(inst.a), labelOp(inst.b), labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.c), next);
        break;
    case IrCmd::JUMP_IF_FALSY:
        jumpIfFalsy(build, vmRegOp(inst.a), labelOp(inst.b), labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.c), next);
        break;
    case IrCmd::JUMP_EQ_TAG:
    {
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::Inst || inst.b.kind == IrOpKind::Constant);
        OperandX64 opb = inst.b.kind == IrOpKind::Inst ? regOp(inst.b) : OperandX64(tagOp(inst.b));

        if (inst.a.kind == IrOpKind::Constant)
            build.cmp(opb, tagOp(inst.a));
        else
            build.cmp(memRegTagOp(inst.a), opb);

        if (isFallthroughBlock(blockOp(inst.d), next))
        {
            build.jcc(ConditionX64::Equal, labelOp(inst.c));
            jumpOrFallthrough(blockOp(inst.d), next);
        }
        else
        {
            build.jcc(ConditionX64::NotEqual, labelOp(inst.d));
            jumpOrFallthrough(blockOp(inst.c), next);
        }
        break;
    }
    case IrCmd::JUMP_CMP_INT:
    {
        IrCondition cond = conditionOp(inst.c);

        if ((cond == IrCondition::Equal || cond == IrCondition::NotEqual) && intOp(inst.b) == 0)
        {
            bool invert = cond == IrCondition::NotEqual;

            build.test(regOp(inst.a), regOp(inst.a));

            if (isFallthroughBlock(blockOp(inst.d), next))
            {
                build.jcc(invert ? ConditionX64::Zero : ConditionX64::NotZero, labelOp(inst.e));
                jumpOrFallthrough(blockOp(inst.d), next);
            }
            else
            {
                build.jcc(invert ? ConditionX64::NotZero : ConditionX64::Zero, labelOp(inst.d));
                jumpOrFallthrough(blockOp(inst.e), next);
            }
        }
        else
        {
            build.cmp(regOp(inst.a), intOp(inst.b));

            build.jcc(getConditionInt(cond), labelOp(inst.d));
            jumpOrFallthrough(blockOp(inst.e), next);
        }
        break;
    }
    case IrCmd::JUMP_EQ_POINTER:
        build.cmp(regOp(inst.a), regOp(inst.b));

        build.jcc(ConditionX64::Equal, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::JUMP_CMP_NUM:
    {
        IrCondition cond = conditionOp(inst.c);

        ScopedRegX64 tmp{regs, SizeX64::xmmword};

        jumpOnNumberCmp(build, tmp.reg, memRegDoubleOp(inst.a), memRegDoubleOp(inst.b), cond, labelOp(inst.d));
        jumpOrFallthrough(blockOp(inst.e), next);
        break;
    }
    case IrCmd::JUMP_FORN_LOOP_COND:
    {
        ScopedRegX64 tmp1{regs, SizeX64::xmmword};
        ScopedRegX64 tmp2{regs, SizeX64::xmmword};
        ScopedRegX64 tmp3{regs, SizeX64::xmmword};

        RegisterX64 index = inst.a.kind == IrOpKind::Inst ? regOp(inst.a) : tmp1.reg;
        RegisterX64 limit = inst.b.kind == IrOpKind::Inst ? regOp(inst.b) : tmp2.reg;

        if (inst.a.kind != IrOpKind::Inst)
            build.vmovsd(tmp1.reg, memRegDoubleOp(inst.a));

        if (inst.b.kind != IrOpKind::Inst)
            build.vmovsd(tmp2.reg, memRegDoubleOp(inst.b));

        Label direct;

        // step > 0
        jumpOnNumberCmp(build, tmp3.reg, memRegDoubleOp(inst.c), build.f64(0.0), IrCondition::Greater, direct);

        // !(limit <= index)
        jumpOnNumberCmp(build, noreg, limit, index, IrCondition::NotLessEqual, labelOp(inst.e));
        build.jmp(labelOp(inst.d));

        // !(index <= limit)
        build.setLabel(direct);
        jumpOnNumberCmp(build, noreg, index, limit, IrCondition::NotLessEqual, labelOp(inst.e));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    }
    case IrCmd::TABLE_LEN:
    {
        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, regOp(inst.a), inst.a);
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaH_getn)]);
        inst.regX64 = regs.takeReg(eax, index);
        break;
    }
    case IrCmd::TABLE_SETNUM:
    {
        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::qword, regOp(inst.a), inst.a);
        callWrap.addArgument(SizeX64::dword, regOp(inst.b), inst.b);
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaH_setnum)]);
        inst.regX64 = regs.takeReg(rax, index);
        break;
    }
    case IrCmd::STRING_LEN:
    {
        RegisterX64 ptr = regOp(inst.a);
        inst.regX64 = regs.allocReg(SizeX64::dword, index);
        build.mov(inst.regX64, dword[ptr + offsetof(TString, len)]);
        break;
    }
    case IrCmd::NEW_TABLE:
    {
        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::dword, int32_t(uintOp(inst.a)));
        callWrap.addArgument(SizeX64::dword, int32_t(uintOp(inst.b)));
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaH_new)]);
        inst.regX64 = regs.takeReg(rax, index);
        break;
    }
    case IrCmd::DUP_TABLE:
    {
        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::qword, regOp(inst.a), inst.a);
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaH_clone)]);
        inst.regX64 = regs.takeReg(rax, index);
        break;
    }
    case IrCmd::TRY_NUM_TO_INDEX:
    {
        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        ScopedRegX64 tmp{regs, SizeX64::xmmword};

        convertNumberToIndexOrJump(build, tmp.reg, regOp(inst.a), inst.regX64, labelOp(inst.b));
        break;
    }
    case IrCmd::TRY_CALL_FASTGETTM:
    {
        ScopedRegX64 tmp{regs, SizeX64::qword};

        build.mov(tmp.reg, qword[regOp(inst.a) + offsetof(LuaTable, metatable)]);
        regs.freeLastUseReg(function.instOp(inst.a), index); // Release before the call if it's the last use

        build.test(tmp.reg, tmp.reg);
        build.jcc(ConditionX64::Zero, labelOp(inst.c)); // No metatable

        build.test(byte[tmp.reg + offsetof(LuaTable, tmcache)], 1 << intOp(inst.b));
        build.jcc(ConditionX64::NotZero, labelOp(inst.c)); // No tag method

        ScopedRegX64 tmp2{regs, SizeX64::qword};
        build.mov(tmp2.reg, qword[rState + offsetof(lua_State, global)]);

        {
            ScopedSpills spillGuard(regs);

            IrCallWrapperX64 callWrap(regs, build, index);
            callWrap.addArgument(SizeX64::qword, tmp);
            callWrap.addArgument(SizeX64::qword, intOp(inst.b));
            callWrap.addArgument(SizeX64::qword, qword[tmp2.release() + offsetof(global_State, tmname) + intOp(inst.b) * sizeof(TString*)]);
            callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaT_gettm)]);
        }

        build.test(rax, rax);
        build.jcc(ConditionX64::Zero, labelOp(inst.c)); // No tag method

        inst.regX64 = regs.takeReg(rax, index);
        break;
    }
    case IrCmd::NEW_USERDATA:
    {
        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::qword, intOp(inst.a));
        callWrap.addArgument(SizeX64::dword, intOp(inst.b));
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, newUserdata)]);
        inst.regX64 = regs.takeReg(rax, index);
        break;
    }
    case IrCmd::INT_TO_NUM:
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        build.vcvtsi2sd(inst.regX64, inst.regX64, regOp(inst.a));
        break;
    case IrCmd::UINT_TO_NUM:
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        // AVX has no uint->double conversion; the source must come from UINT op and they all should clear top 32 bits so we can usually
        // use 64-bit reg; the one exception is NUM_TO_UINT which doesn't clear top bits
        if (IrCmd source = function.instOp(inst.a).cmd; source == IrCmd::NUM_TO_UINT)
        {
            ScopedRegX64 tmp{regs, SizeX64::dword};
            build.mov(tmp.reg, regOp(inst.a));
            build.vcvtsi2sd(inst.regX64, inst.regX64, qwordReg(tmp.reg));
        }
        else
        {
            CODEGEN_ASSERT(source != IrCmd::SUBSTITUTE); // we don't process substitutions
            build.vcvtsi2sd(inst.regX64, inst.regX64, qwordReg(regOp(inst.a)));
        }
        break;
    case IrCmd::NUM_TO_INT:
        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        build.vcvttsd2si(inst.regX64, memRegDoubleOp(inst.a));
        break;
    case IrCmd::NUM_TO_UINT:
        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        build.vcvttsd2si(qwordReg(inst.regX64), memRegDoubleOp(inst.a));
        break;
    case IrCmd::NUM_TO_VEC:
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        if (inst.a.kind == IrOpKind::Constant)
        {
            float value = float(doubleOp(inst.a));
            uint32_t asU32;
            static_assert(sizeof(asU32) == sizeof(value), "Expecting float to be 32-bit");
            memcpy(&asU32, &value, sizeof(value));

            build.vmovaps(inst.regX64, build.u32x4(asU32, asU32, asU32, 0));
        }
        else
        {
            build.vcvtsd2ss(inst.regX64, inst.regX64, memRegDoubleOp(inst.a));
            build.vpshufps(inst.regX64, inst.regX64, inst.regX64, 0b00'00'00'00);
        }
        break;
    case IrCmd::TAG_VECTOR:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::xmmword, index, {inst.a});

        build.vpinsrd(inst.regX64, regOp(inst.a), build.i32(LUA_TVECTOR), 3);
        break;
    case IrCmd::ADJUST_STACK_TO_REG:
    {
        ScopedRegX64 tmp{regs, SizeX64::qword};

        if (inst.b.kind == IrOpKind::Constant)
        {
            build.lea(tmp.reg, addr[rBase + (vmRegOp(inst.a) + intOp(inst.b)) * sizeof(TValue)]);
            build.mov(qword[rState + offsetof(lua_State, top)], tmp.reg);
        }
        else if (inst.b.kind == IrOpKind::Inst)
        {
            build.mov(dwordReg(tmp.reg), regOp(inst.b));
            build.shl(tmp.reg, kTValueSizeLog2);
            build.lea(tmp.reg, addr[rBase + tmp.reg + vmRegOp(inst.a) * sizeof(TValue)]);
            build.mov(qword[rState + offsetof(lua_State, top)], tmp.reg);
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    }
    case IrCmd::ADJUST_STACK_TO_TOP:
    {
        ScopedRegX64 tmp{regs, SizeX64::qword};
        build.mov(tmp.reg, qword[rState + offsetof(lua_State, ci)]);
        build.mov(tmp.reg, qword[tmp.reg + offsetof(CallInfo, top)]);
        build.mov(qword[rState + offsetof(lua_State, top)], tmp.reg);
        break;
    }

    case IrCmd::FASTCALL:
    {
        emitBuiltin(regs, build, uintOp(inst.a), vmRegOp(inst.b), vmRegOp(inst.c), intOp(inst.d));
        break;
    }
    case IrCmd::INVOKE_FASTCALL:
    {
        unsigned bfid = uintOp(inst.a);

        OperandX64 args = 0;
        ScopedRegX64 argsAlt{regs};

        // 'E' argument can only be produced by LOP_FASTCALL3
        if (inst.e.kind != IrOpKind::Undef)
        {
            CODEGEN_ASSERT(intOp(inst.f) == 3);

            ScopedRegX64 tmp{regs, SizeX64::xmmword};
            argsAlt.alloc(SizeX64::qword);

            build.mov(argsAlt.reg, qword[rState + offsetof(lua_State, top)]);

            build.vmovups(tmp.reg, luauReg(vmRegOp(inst.d)));
            build.vmovups(xmmword[argsAlt.reg], tmp.reg);

            build.vmovups(tmp.reg, luauReg(vmRegOp(inst.e)));
            build.vmovups(xmmword[argsAlt.reg + sizeof(TValue)], tmp.reg);
        }
        else
        {
            if (inst.d.kind == IrOpKind::VmReg)
                args = luauRegAddress(vmRegOp(inst.d));
            else if (inst.d.kind == IrOpKind::VmConst)
                args = luauConstantAddress(vmConstOp(inst.d));
            else
                CODEGEN_ASSERT(inst.d.kind == IrOpKind::Undef);
        }

        int ra = vmRegOp(inst.b);
        int arg = vmRegOp(inst.c);
        int nparams = intOp(inst.f);
        int nresults = intOp(inst.g);

        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::qword, luauRegAddress(ra));
        callWrap.addArgument(SizeX64::qword, luauRegAddress(arg));
        callWrap.addArgument(SizeX64::dword, nresults);

        if (inst.e.kind != IrOpKind::Undef)
            callWrap.addArgument(SizeX64::qword, argsAlt);
        else
            callWrap.addArgument(SizeX64::qword, args);

        if (nparams == LUA_MULTRET)
        {
            RegisterX64 reg = callWrap.suggestNextArgumentRegister(SizeX64::qword);
            ScopedRegX64 tmp{regs, SizeX64::qword};

            // L->top - (ra + 1)
            build.mov(reg, qword[rState + offsetof(lua_State, top)]);
            build.lea(tmp.reg, addr[rBase + (ra + 1) * sizeof(TValue)]);
            build.sub(reg, tmp.reg);
            build.shr(reg, kTValueSizeLog2);

            callWrap.addArgument(SizeX64::dword, dwordReg(reg));
        }
        else
        {
            callWrap.addArgument(SizeX64::dword, nparams);
        }

        ScopedRegX64 func{regs, SizeX64::qword};
        build.mov(func.reg, qword[rNativeContext + offsetof(NativeContext, luauF_table) + bfid * sizeof(luau_FastFunction)]);

        callWrap.call(func.release());
        inst.regX64 = regs.takeReg(eax, index); // Result of a builtin call is returned in eax
        break;
    }
    case IrCmd::CHECK_FASTCALL_RES:
    {
        RegisterX64 res = regOp(inst.a);

        build.test(res, res);                           // test here will set SF=1 for a negative number and it always sets OF to 0
        build.jcc(ConditionX64::Less, labelOp(inst.b)); // jl jumps if SF != OF
        break;
    }
    case IrCmd::DO_ARITH:
    {
        OperandX64 opb = inst.b.kind == IrOpKind::VmReg ? luauRegAddress(vmRegOp(inst.b)) : luauConstantAddress(vmConstOp(inst.b));
        OperandX64 opc = inst.c.kind == IrOpKind::VmReg ? luauRegAddress(vmRegOp(inst.c)) : luauConstantAddress(vmConstOp(inst.c));
        callArithHelper(regs, build, vmRegOp(inst.a), opb, opc, TMS(intOp(inst.d)));
        break;
    }
    case IrCmd::DO_LEN:
        callLengthHelper(regs, build, vmRegOp(inst.a), vmRegOp(inst.b));
        break;
    case IrCmd::GET_TABLE:
        if (inst.c.kind == IrOpKind::VmReg)
        {
            callGetTable(regs, build, vmRegOp(inst.b), luauRegAddress(vmRegOp(inst.c)), vmRegOp(inst.a));
        }
        else if (inst.c.kind == IrOpKind::Constant)
        {
            TValue n = {};
            setnvalue(&n, uintOp(inst.c));
            callGetTable(regs, build, vmRegOp(inst.b), build.bytes(&n, sizeof(n)), vmRegOp(inst.a));
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::SET_TABLE:
        if (inst.c.kind == IrOpKind::VmReg)
        {
            callSetTable(regs, build, vmRegOp(inst.b), luauRegAddress(vmRegOp(inst.c)), vmRegOp(inst.a));
        }
        else if (inst.c.kind == IrOpKind::Constant)
        {
            TValue n = {};
            setnvalue(&n, uintOp(inst.c));
            callSetTable(regs, build, vmRegOp(inst.b), build.bytes(&n, sizeof(n)), vmRegOp(inst.a));
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::GET_CACHED_IMPORT:
    {
        regs.assertAllFree();
        regs.assertNoSpills();

        Label skip, exit;

        // If the constant for the import is set, we will use it directly, otherwise we have to call an import path lookup function
        build.cmp(luauConstantTag(vmConstOp(inst.b)), LUA_TNIL);
        build.jcc(ConditionX64::NotEqual, skip);

        {
            ScopedSpills spillGuard(regs);

            IrCallWrapperX64 callWrap(regs, build, index);
            callWrap.addArgument(SizeX64::qword, rState);
            callWrap.addArgument(SizeX64::qword, luauRegAddress(vmRegOp(inst.a)));
            callWrap.addArgument(SizeX64::dword, importOp(inst.c));
            callWrap.addArgument(SizeX64::dword, uintOp(inst.d));
            callWrap.call(qword[rNativeContext + offsetof(NativeContext, getImport)]);
        }

        emitUpdateBase(build);
        build.jmp(exit);

        build.setLabel(skip);

        ScopedRegX64 tmp1{regs, SizeX64::xmmword};

        build.vmovups(tmp1.reg, luauConstant(vmConstOp(inst.b)));
        build.vmovups(luauReg(vmRegOp(inst.a)), tmp1.reg);
        build.setLabel(exit);
        break;
    }
    case IrCmd::CONCAT:
    {
        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::dword, int32_t(uintOp(inst.b)));
        callWrap.addArgument(SizeX64::dword, int32_t(vmRegOp(inst.a) + uintOp(inst.b) - 1));
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_concat)]);

        emitUpdateBase(build);
        break;
    }
    case IrCmd::GET_UPVALUE:
    {
        ScopedRegX64 tmp1{regs, SizeX64::qword};
        ScopedRegX64 tmp2{regs, SizeX64::xmmword};

        build.mov(tmp1.reg, sClosure);
        build.add(tmp1.reg, offsetof(Closure, l.uprefs) + sizeof(TValue) * vmUpvalueOp(inst.b));

        // uprefs[] is either an actual value, or it points to UpVal object which has a pointer to value
        Label skip;
        build.cmp(dword[tmp1.reg + offsetof(TValue, tt)], LUA_TUPVAL);
        build.jcc(ConditionX64::NotEqual, skip);

        // UpVal.v points to the value (either on stack, or on heap inside each UpVal, but we can deref it unconditionally)
        build.mov(tmp1.reg, qword[tmp1.reg + offsetof(TValue, value.gc)]);
        build.mov(tmp1.reg, qword[tmp1.reg + offsetof(UpVal, v)]);

        build.setLabel(skip);

        build.vmovups(tmp2.reg, xmmword[tmp1.reg]);
        build.vmovups(luauReg(vmRegOp(inst.a)), tmp2.reg);
        break;
    }
    case IrCmd::SET_UPVALUE:
    {
        ScopedRegX64 tmp1{regs, SizeX64::qword};
        ScopedRegX64 tmp2{regs, SizeX64::qword};

        build.mov(tmp1.reg, sClosure);
        build.mov(tmp2.reg, qword[tmp1.reg + offsetof(Closure, l.uprefs) + sizeof(TValue) * vmUpvalueOp(inst.a) + offsetof(TValue, value.gc)]);

        build.mov(tmp1.reg, qword[tmp2.reg + offsetof(UpVal, v)]);

        {
            ScopedRegX64 tmp3{regs, SizeX64::xmmword};
            build.vmovups(tmp3.reg, luauReg(vmRegOp(inst.b)));
            build.vmovups(xmmword[tmp1.reg], tmp3.reg);
        }

        tmp1.free();

        if (inst.c.kind == IrOpKind::Undef || isGCO(tagOp(inst.c)))
            callBarrierObject(regs, build, tmp2.release(), {}, inst.b, inst.c.kind == IrOpKind::Undef ? -1 : tagOp(inst.c));
        break;
    }
    case IrCmd::CHECK_TAG:
        build.cmp(memRegTagOp(inst.a), tagOp(inst.b));
        jumpOrAbortOnUndef(ConditionX64::NotEqual, inst.c, next);
        break;
    case IrCmd::CHECK_TRUTHY:
    {
        // Constant tags which don't require boolean value check should've been removed in constant folding
        CODEGEN_ASSERT(inst.a.kind != IrOpKind::Constant || tagOp(inst.a) == LUA_TBOOLEAN);

        Label skip;

        if (inst.a.kind != IrOpKind::Constant)
        {
            // Fail to fallback on 'nil' (falsy)
            build.cmp(memRegTagOp(inst.a), LUA_TNIL);
            jumpOrAbortOnUndef(ConditionX64::Equal, inst.c, next);

            // Skip value test if it's not a boolean (truthy)
            build.cmp(memRegTagOp(inst.a), LUA_TBOOLEAN);
            build.jcc(ConditionX64::NotEqual, skip);
        }

        // fail to fallback on 'false' boolean value (falsy)
        if (inst.b.kind != IrOpKind::Constant)
        {
            build.cmp(memRegUintOp(inst.b), 0);
            jumpOrAbortOnUndef(ConditionX64::Equal, inst.c, next);
        }
        else
        {
            if (intOp(inst.b) == 0)
                jumpOrAbortOnUndef(inst.c, next);
        }

        if (inst.a.kind != IrOpKind::Constant)
            build.setLabel(skip);
        break;
    }
    case IrCmd::CHECK_READONLY:
        build.cmp(byte[regOp(inst.a) + offsetof(LuaTable, readonly)], 0);
        jumpOrAbortOnUndef(ConditionX64::NotEqual, inst.b, next);
        break;
    case IrCmd::CHECK_NO_METATABLE:
        build.cmp(qword[regOp(inst.a) + offsetof(LuaTable, metatable)], 0);
        jumpOrAbortOnUndef(ConditionX64::NotEqual, inst.b, next);
        break;
    case IrCmd::CHECK_SAFE_ENV:
    {
        ScopedRegX64 tmp{regs, SizeX64::qword};

        build.mov(tmp.reg, sClosure);
        build.mov(tmp.reg, qword[tmp.reg + offsetof(Closure, env)]);
        build.cmp(byte[tmp.reg + offsetof(LuaTable, safeenv)], 0);

        jumpOrAbortOnUndef(ConditionX64::Equal, inst.a, next);
        break;
    }
    case IrCmd::CHECK_ARRAY_SIZE:
        if (inst.b.kind == IrOpKind::Inst)
            build.cmp(dword[regOp(inst.a) + offsetof(LuaTable, sizearray)], regOp(inst.b));
        else if (inst.b.kind == IrOpKind::Constant)
            build.cmp(dword[regOp(inst.a) + offsetof(LuaTable, sizearray)], intOp(inst.b));
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");

        jumpOrAbortOnUndef(ConditionX64::BelowEqual, inst.c, next);
        break;
    case IrCmd::JUMP_SLOT_MATCH:
    case IrCmd::CHECK_SLOT_MATCH:
    {
        Label abort; // Used when guard aborts execution
        const IrOp& mismatchOp = inst.cmd == IrCmd::JUMP_SLOT_MATCH ? inst.d : inst.c;
        Label& mismatch = mismatchOp.kind == IrOpKind::Undef ? abort : labelOp(mismatchOp);

        ScopedRegX64 tmp{regs, SizeX64::qword};

        // Check if node key tag is a string
        build.mov(dwordReg(tmp.reg), luauNodeKeyTag(regOp(inst.a)));
        build.and_(dwordReg(tmp.reg), kTKeyTagMask);
        build.cmp(dwordReg(tmp.reg), LUA_TSTRING);
        build.jcc(ConditionX64::NotEqual, mismatch);

        // Check that node key value matches the expected one
        build.mov(tmp.reg, luauConstantValue(vmConstOp(inst.b)));
        build.cmp(tmp.reg, luauNodeKeyValue(regOp(inst.a)));
        build.jcc(ConditionX64::NotEqual, mismatch);

        // Check that node value is not nil
        build.cmp(dword[regOp(inst.a) + offsetof(LuaNode, val) + offsetof(TValue, tt)], LUA_TNIL);
        build.jcc(ConditionX64::Equal, mismatch);

        if (inst.cmd == IrCmd::JUMP_SLOT_MATCH)
        {
            jumpOrFallthrough(blockOp(inst.c), next);
        }
        else if (mismatchOp.kind == IrOpKind::Undef)
        {
            Label skip;
            build.jmp(skip);
            build.setLabel(abort);
            build.ud2();
            build.setLabel(skip);
        }
        break;
    }
    case IrCmd::CHECK_NODE_NO_NEXT:
    {
        ScopedRegX64 tmp{regs, SizeX64::dword};

        build.mov(tmp.reg, dword[regOp(inst.a) + offsetof(LuaNode, key) + kOffsetOfTKeyTagNext]);
        build.shr(tmp.reg, kTKeyTagBits);
        jumpOrAbortOnUndef(ConditionX64::NotZero, inst.b, next);
        break;
    }
    case IrCmd::CHECK_NODE_VALUE:
    {
        build.cmp(dword[regOp(inst.a) + offsetof(LuaNode, val) + offsetof(TValue, tt)], LUA_TNIL);
        jumpOrAbortOnUndef(ConditionX64::Equal, inst.b, next);
        break;
    }
    case IrCmd::CHECK_BUFFER_LEN:
    {
        int accessSize = intOp(inst.c);
        CODEGEN_ASSERT(accessSize > 0);

        if (inst.b.kind == IrOpKind::Inst)
        {
            if (accessSize == 1)
            {
                // Simpler check for a single byte access
                build.cmp(dword[regOp(inst.a) + offsetof(Buffer, len)], regOp(inst.b));
                jumpOrAbortOnUndef(ConditionX64::BelowEqual, inst.d, next);
            }
            else
            {
                ScopedRegX64 tmp1{regs, SizeX64::qword};
                ScopedRegX64 tmp2{regs, SizeX64::dword};

                // To perform the bounds check using a single branch, we take index that is limited to 32 bit int
                // Access size is then added using a 64 bit addition
                // This will make sure that addition will not wrap around for values like 0xffffffff

                if (IrCmd source = function.instOp(inst.b).cmd; source == IrCmd::NUM_TO_INT)
                {
                    // When previous operation is a conversion to an integer (common case), it is guaranteed to have high register bits cleared
                    build.lea(tmp1.reg, addr[qwordReg(regOp(inst.b)) + accessSize]);
                }
                else
                {
                    // When the source of the index is unknown, it could contain garbage in the high bits, so we zero-extend it explicitly
                    build.mov(dwordReg(tmp1.reg), regOp(inst.b));
                    build.add(tmp1.reg, accessSize);
                }

                build.mov(tmp2.reg, dword[regOp(inst.a) + offsetof(Buffer, len)]);
                build.cmp(qwordReg(tmp2.reg), tmp1.reg);

                jumpOrAbortOnUndef(ConditionX64::Below, inst.d, next);
            }
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            int offset = intOp(inst.b);

            // Constant folding can take care of it, but for safety we avoid overflow/underflow cases here
            if (offset < 0 || unsigned(offset) + unsigned(accessSize) >= unsigned(INT_MAX))
                jumpOrAbortOnUndef(inst.d, next);
            else
                build.cmp(dword[regOp(inst.a) + offsetof(Buffer, len)], offset + accessSize);

            jumpOrAbortOnUndef(ConditionX64::Below, inst.d, next);
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    }
    case IrCmd::CHECK_USERDATA_TAG:
    {
        build.cmp(byte[regOp(inst.a) + offsetof(Udata, tag)], intOp(inst.b));
        jumpOrAbortOnUndef(ConditionX64::NotEqual, inst.c, next);
        break;
    }
    case IrCmd::INTERRUPT:
    {
        unsigned pcpos = uintOp(inst.a);

        // We unconditionally spill values here because that allows us to ignore register state when we synthesize interrupt handler
        // This can be changed in the future if we can somehow record interrupt handler code separately
        // Since interrupts are loop edges or call/ret, we don't have a significant opportunity for register reuse here anyway
        regs.preserveAndFreeInstValues();

        ScopedRegX64 tmp{regs, SizeX64::qword};

        Label self;

        build.mov(tmp.reg, qword[rState + offsetof(lua_State, global)]);
        build.cmp(qword[tmp.reg + offsetof(global_State, cb.interrupt)], 0);
        build.jcc(ConditionX64::NotEqual, self);

        Label next = build.setLabel();

        interruptHandlers.push_back({self, pcpos, next});
        break;
    }
    case IrCmd::CHECK_GC:
        callStepGc(regs, build);
        break;
    case IrCmd::BARRIER_OBJ:
        callBarrierObject(regs, build, regOp(inst.a), inst.a, inst.b, inst.c.kind == IrOpKind::Undef ? -1 : tagOp(inst.c));
        break;
    case IrCmd::BARRIER_TABLE_BACK:
        callBarrierTableFast(regs, build, regOp(inst.a), inst.a);
        break;
    case IrCmd::BARRIER_TABLE_FORWARD:
    {
        Label skip;

        ScopedRegX64 tmp{regs, SizeX64::qword};

        checkObjectBarrierConditions(build, tmp.reg, regOp(inst.a), inst.b, inst.c.kind == IrOpKind::Undef ? -1 : tagOp(inst.c), skip);

        {
            ScopedSpills spillGuard(regs);

            IrCallWrapperX64 callWrap(regs, build, index);
            callWrap.addArgument(SizeX64::qword, rState);
            callWrap.addArgument(SizeX64::qword, regOp(inst.a), inst.a);
            callWrap.addArgument(SizeX64::qword, tmp);
            callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaC_barriertable)]);
        }

        build.setLabel(skip);
        break;
    }
    case IrCmd::SET_SAVEDPC:
    {
        ScopedRegX64 tmp1{regs, SizeX64::qword};
        ScopedRegX64 tmp2{regs, SizeX64::qword};

        build.mov(tmp2.reg, sCode);
        build.add(tmp2.reg, uintOp(inst.a) * sizeof(Instruction));
        build.mov(tmp1.reg, qword[rState + offsetof(lua_State, ci)]);
        build.mov(qword[tmp1.reg + offsetof(CallInfo, savedpc)], tmp2.reg);
        break;
    }
    case IrCmd::CLOSE_UPVALS:
    {
        Label next;
        ScopedRegX64 tmp1{regs, SizeX64::qword};
        ScopedRegX64 tmp2{regs, SizeX64::qword};

        // L->openupval != 0
        build.mov(tmp1.reg, qword[rState + offsetof(lua_State, openupval)]);
        build.test(tmp1.reg, tmp1.reg);
        build.jcc(ConditionX64::Zero, next);

        // ra <= L->openupval->v
        build.lea(tmp2.reg, addr[rBase + vmRegOp(inst.a) * sizeof(TValue)]);
        build.cmp(tmp2.reg, qword[tmp1.reg + offsetof(UpVal, v)]);
        build.jcc(ConditionX64::Above, next);

        tmp1.free();

        {
            ScopedSpills spillGuard(regs);

            IrCallWrapperX64 callWrap(regs, build, index);
            callWrap.addArgument(SizeX64::qword, rState);
            callWrap.addArgument(SizeX64::qword, tmp2);
            callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaF_close)]);
        }

        build.setLabel(next);
        break;
    }
    case IrCmd::CAPTURE:
        // No-op right now
        break;

        // Fallbacks to non-IR instruction implementations
    case IrCmd::SETLIST:
        regs.assertAllFree();
        emitInstSetList(
            regs, build, vmRegOp(inst.b), vmRegOp(inst.c), intOp(inst.d), uintOp(inst.e), inst.f.kind == IrOpKind::Undef ? -1 : int(uintOp(inst.f))
        );
        break;
    case IrCmd::CALL:
        regs.assertAllFree();
        regs.assertNoSpills();
        emitInstCall(build, helpers, vmRegOp(inst.a), intOp(inst.b), intOp(inst.c));
        break;
    case IrCmd::RETURN:
        regs.assertAllFree();
        regs.assertNoSpills();
        emitInstReturn(build, helpers, vmRegOp(inst.a), intOp(inst.b), function.variadic);
        break;
    case IrCmd::FORGLOOP:
        regs.assertAllFree();
        emitInstForGLoop(build, vmRegOp(inst.a), intOp(inst.b), labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::FORGLOOP_FALLBACK:
    {
        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::dword, vmRegOp(inst.a));
        callWrap.addArgument(SizeX64::dword, intOp(inst.b));
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, forgLoopNonTableFallback)]);

        emitUpdateBase(build);

        build.test(al, al);
        build.jcc(ConditionX64::NotZero, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    }
    case IrCmd::FORGPREP_XNEXT_FALLBACK:
    {
        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::qword, luauRegAddress(vmRegOp(inst.b)));
        callWrap.addArgument(SizeX64::dword, uintOp(inst.a) + 1);
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, forgPrepXnextFallback)]);
        jumpOrFallthrough(blockOp(inst.c), next);
        break;
    }
    case IrCmd::COVERAGE:
    {
        ScopedRegX64 tmp1{regs, SizeX64::qword};
        ScopedRegX64 tmp2{regs, SizeX64::dword};
        ScopedRegX64 tmp3{regs, SizeX64::dword};

        build.mov(tmp1.reg, sCode);
        build.add(tmp1.reg, uintOp(inst.a) * sizeof(Instruction));

        // hits = LUAU_INSN_E(*pc)
        build.mov(tmp2.reg, dword[tmp1.reg]);
        build.sar(tmp2.reg, 8);

        // hits = (hits < (1 << 23) - 1) ? hits + 1 : hits;
        build.xor_(tmp3.reg, tmp3.reg);
        build.cmp(tmp2.reg, (1 << 23) - 1);
        build.setcc(ConditionX64::NotEqual, byteReg(tmp3.reg));
        build.add(tmp2.reg, tmp3.reg);

        // VM_PATCH_E(pc, hits);
        build.sal(tmp2.reg, 8);
        build.movzx(tmp3.reg, byte[tmp1.reg]);
        build.or_(tmp3.reg, tmp2.reg);
        build.mov(dword[tmp1.reg], tmp3.reg);
        break;
    }

        // Full instruction fallbacks
    case IrCmd::FALLBACK_GETGLOBAL:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::VmConst);

        emitFallback(regs, build, offsetof(NativeContext, executeGETGLOBAL), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETGLOBAL:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::VmConst);

        emitFallback(regs, build, offsetof(NativeContext, executeSETGLOBAL), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETTABLEKS:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitFallback(regs, build, offsetof(NativeContext, executeGETTABLEKS), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETTABLEKS:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitFallback(regs, build, offsetof(NativeContext, executeSETTABLEKS), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_NAMECALL:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitFallback(regs, build, offsetof(NativeContext, executeNAMECALL), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_PREPVARARGS:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::Constant);

        emitFallback(regs, build, offsetof(NativeContext, executePREPVARARGS), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETVARARGS:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::Constant);

        if (intOp(inst.c) == LUA_MULTRET)
        {
            IrCallWrapperX64 callWrap(regs, build);
            callWrap.addArgument(SizeX64::qword, rState);

            RegisterX64 reg = callWrap.suggestNextArgumentRegister(SizeX64::qword);
            build.mov(reg, sCode);
            callWrap.addArgument(SizeX64::qword, addr[reg + uintOp(inst.a) * sizeof(Instruction)]);

            callWrap.addArgument(SizeX64::qword, rBase);
            callWrap.addArgument(SizeX64::dword, vmRegOp(inst.b));
            callWrap.call(qword[rNativeContext + offsetof(NativeContext, executeGETVARARGSMultRet)]);

            emitUpdateBase(build);
        }
        else
        {
            IrCallWrapperX64 callWrap(regs, build);
            callWrap.addArgument(SizeX64::qword, rState);
            callWrap.addArgument(SizeX64::qword, rBase);
            callWrap.addArgument(SizeX64::dword, vmRegOp(inst.b));
            callWrap.addArgument(SizeX64::dword, intOp(inst.c));
            callWrap.call(qword[rNativeContext + offsetof(NativeContext, executeGETVARARGSConst)]);
        }
        break;
    case IrCmd::NEWCLOSURE:
    {
        ScopedRegX64 tmp2{regs, SizeX64::qword};
        build.mov(tmp2.reg, sClosure);
        build.mov(tmp2.reg, qword[tmp2.reg + offsetof(Closure, l.p)]);
        build.mov(tmp2.reg, qword[tmp2.reg + offsetof(Proto, p)]);
        build.mov(tmp2.reg, qword[tmp2.reg + sizeof(Proto*) * uintOp(inst.c)]);

        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::dword, uintOp(inst.a), inst.a);
        callWrap.addArgument(SizeX64::qword, regOp(inst.b), inst.b);
        callWrap.addArgument(SizeX64::qword, tmp2);

        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaF_newLclosure)]);

        inst.regX64 = regs.takeReg(rax, index);
        break;
    }
    case IrCmd::FALLBACK_DUPCLOSURE:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::VmConst);

        emitFallback(regs, build, offsetof(NativeContext, executeDUPCLOSURE), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_FORGPREP:
        emitFallback(regs, build, offsetof(NativeContext, executeFORGPREP), uintOp(inst.a));
        jumpOrFallthrough(blockOp(inst.c), next);
        break;
    case IrCmd::BITAND_UINT:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        build.and_(inst.regX64, memRegUintOp(inst.b));
        break;
    case IrCmd::BITXOR_UINT:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        build.xor_(inst.regX64, memRegUintOp(inst.b));
        break;
    case IrCmd::BITOR_UINT:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        build.or_(inst.regX64, memRegUintOp(inst.b));
        break;
    case IrCmd::BITNOT_UINT:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        build.not_(inst.regX64);
        break;
    case IrCmd::BITLSHIFT_UINT:
    {
        ScopedRegX64 shiftTmp{regs};

        // Custom bit shift value can only be placed in cl
        // but we use it if the shift value is not a constant stored in b
        if (inst.b.kind != IrOpKind::Constant)
            shiftTmp.take(ecx);

        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        if (inst.b.kind == IrOpKind::Constant)
        {
            // if shift value is a constant, we extract the byte-sized shift amount
            int8_t shift = int8_t(unsigned(intOp(inst.b)));
            build.shl(inst.regX64, shift);
        }
        else
        {
            build.mov(shiftTmp.reg, memRegUintOp(inst.b));
            build.shl(inst.regX64, byteReg(shiftTmp.reg));
        }

        break;
    }
    case IrCmd::BITRSHIFT_UINT:
    {
        ScopedRegX64 shiftTmp{regs};

        // Custom bit shift value can only be placed in cl
        // but we use it if the shift value is not a constant stored in b
        if (inst.b.kind != IrOpKind::Constant)
            shiftTmp.take(ecx);

        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        if (inst.b.kind == IrOpKind::Constant)
        {
            // if shift value is a constant, we extract the byte-sized shift amount
            int8_t shift = int8_t(unsigned(intOp(inst.b)));
            build.shr(inst.regX64, shift);
        }
        else
        {
            build.mov(shiftTmp.reg, memRegUintOp(inst.b));
            build.shr(inst.regX64, byteReg(shiftTmp.reg));
        }

        break;
    }
    case IrCmd::BITARSHIFT_UINT:
    {
        ScopedRegX64 shiftTmp{regs};

        // Custom bit shift value can only be placed in cl
        // but we use it if the shift value is not a constant stored in b
        if (inst.b.kind != IrOpKind::Constant)
            shiftTmp.take(ecx);

        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        if (inst.b.kind == IrOpKind::Constant)
        {
            // if shift value is a constant, we extract the byte-sized shift amount
            int8_t shift = int8_t(unsigned(intOp(inst.b)));
            build.sar(inst.regX64, shift);
        }
        else
        {
            build.mov(shiftTmp.reg, memRegUintOp(inst.b));
            build.sar(inst.regX64, byteReg(shiftTmp.reg));
        }

        break;
    }
    case IrCmd::BITLROTATE_UINT:
    {
        ScopedRegX64 shiftTmp{regs};

        // Custom bit shift value can only be placed in cl
        // but we use it if the shift value is not a constant stored in b
        if (inst.b.kind != IrOpKind::Constant)
            shiftTmp.take(ecx);

        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        if (inst.b.kind == IrOpKind::Constant)
        {
            // if shift value is a constant, we extract the byte-sized shift amount
            int8_t shift = int8_t(unsigned(intOp(inst.b)));
            build.rol(inst.regX64, shift);
        }
        else
        {
            build.mov(shiftTmp.reg, memRegUintOp(inst.b));
            build.rol(inst.regX64, byteReg(shiftTmp.reg));
        }

        break;
    }
    case IrCmd::BITRROTATE_UINT:
    {
        ScopedRegX64 shiftTmp{regs};

        // Custom bit shift value can only be placed in cl
        // but we use it if the shift value is not a constant stored in b
        if (inst.b.kind != IrOpKind::Constant)
            shiftTmp.take(ecx);

        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        if (inst.b.kind == IrOpKind::Constant)
        {
            // if shift value is a constant, we extract the byte-sized shift amount
            int8_t shift = int8_t(unsigned(intOp(inst.b)));
            build.ror(inst.regX64, shift);
        }
        else
        {
            build.mov(shiftTmp.reg, memRegUintOp(inst.b));
            build.ror(inst.regX64, byteReg(shiftTmp.reg));
        }

        break;
    }
    case IrCmd::BITCOUNTLZ_UINT:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        Label zero, exit;

        build.test(regOp(inst.a), regOp(inst.a));
        build.jcc(ConditionX64::Equal, zero);

        build.bsr(inst.regX64, regOp(inst.a));
        build.xor_(inst.regX64, 0x1f);
        build.jmp(exit);

        build.setLabel(zero);
        build.mov(inst.regX64, 32);

        build.setLabel(exit);
        break;
    }
    case IrCmd::BITCOUNTRZ_UINT:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        Label zero, exit;

        build.test(regOp(inst.a), regOp(inst.a));
        build.jcc(ConditionX64::Equal, zero);

        build.bsf(inst.regX64, regOp(inst.a));
        build.jmp(exit);

        build.setLabel(zero);
        build.mov(inst.regX64, 32);

        build.setLabel(exit);
        break;
    }
    case IrCmd::BYTESWAP_UINT:
    {
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        build.bswap(inst.regX64);
        break;
    }
    case IrCmd::INVOKE_LIBM:
    {
        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::xmmword, memRegDoubleOp(inst.b), inst.b);

        if (inst.c.kind != IrOpKind::None)
        {
            bool isInt = (inst.c.kind == IrOpKind::Constant) ? constOp(inst.c).kind == IrConstKind::Int
                                                             : getCmdValueKind(function.instOp(inst.c).cmd) == IrValueKind::Int;

            if (isInt)
                callWrap.addArgument(SizeX64::dword, memRegUintOp(inst.c), inst.c);
            else
                callWrap.addArgument(SizeX64::xmmword, memRegDoubleOp(inst.c), inst.c);
        }

        callWrap.call(qword[rNativeContext + getNativeContextOffset(uintOp(inst.a))]);
        inst.regX64 = regs.takeReg(xmm0, index);
        break;
    }
    case IrCmd::GET_TYPE:
    {
        inst.regX64 = regs.allocReg(SizeX64::qword, index);

        build.mov(inst.regX64, qword[rState + offsetof(lua_State, global)]);

        if (inst.a.kind == IrOpKind::Inst)
            build.mov(inst.regX64, qword[inst.regX64 + qwordReg(regOp(inst.a)) * sizeof(TString*) + offsetof(global_State, ttname)]);
        else if (inst.a.kind == IrOpKind::Constant)
            build.mov(inst.regX64, qword[inst.regX64 + tagOp(inst.a) * sizeof(TString*) + offsetof(global_State, ttname)]);
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");
        break;
    }
    case IrCmd::GET_TYPEOF:
    {
        IrCallWrapperX64 callWrap(regs, build);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::qword, luauRegAddress(vmRegOp(inst.a)));
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaT_objtypenamestr)]);

        inst.regX64 = regs.takeReg(rax, index);
        break;
    }

    case IrCmd::FINDUPVAL:
    {
        IrCallWrapperX64 callWrap(regs, build);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::qword, luauRegAddress(vmRegOp(inst.a)));
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaF_findupval)]);

        inst.regX64 = regs.takeReg(rax, index);
        break;
    }

    case IrCmd::BUFFER_READI8:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a, inst.b});

        build.movsx(inst.regX64, byte[bufferAddrOp(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c))]);
        break;

    case IrCmd::BUFFER_READU8:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a, inst.b});

        build.movzx(inst.regX64, byte[bufferAddrOp(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c))]);
        break;

    case IrCmd::BUFFER_WRITEI8:
    {
        OperandX64 value = inst.c.kind == IrOpKind::Inst ? byteReg(regOp(inst.c)) : OperandX64(int8_t(intOp(inst.c)));

        build.mov(byte[bufferAddrOp(inst.a, inst.b, inst.d.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.d))], value);
        break;
    }

    case IrCmd::BUFFER_READI16:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a, inst.b});

        build.movsx(inst.regX64, word[bufferAddrOp(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c))]);
        break;

    case IrCmd::BUFFER_READU16:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a, inst.b});

        build.movzx(inst.regX64, word[bufferAddrOp(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c))]);
        break;

    case IrCmd::BUFFER_WRITEI16:
    {
        OperandX64 value = inst.c.kind == IrOpKind::Inst ? wordReg(regOp(inst.c)) : OperandX64(int16_t(intOp(inst.c)));

        build.mov(word[bufferAddrOp(inst.a, inst.b, inst.d.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.d))], value);
        break;
    }

    case IrCmd::BUFFER_READI32:
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a, inst.b});

        build.mov(inst.regX64, dword[bufferAddrOp(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c))]);
        break;

    case IrCmd::BUFFER_WRITEI32:
    {
        OperandX64 value = inst.c.kind == IrOpKind::Inst ? regOp(inst.c) : OperandX64(intOp(inst.c));

        build.mov(dword[bufferAddrOp(inst.a, inst.b, inst.d.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.d))], value);
        break;
    }

    case IrCmd::BUFFER_READF32:
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        build.vcvtss2sd(inst.regX64, inst.regX64, dword[bufferAddrOp(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c))]);
        break;

    case IrCmd::BUFFER_WRITEF32:
        storeDoubleAsFloat(dword[bufferAddrOp(inst.a, inst.b, inst.d.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.d))], inst.c);
        break;

    case IrCmd::BUFFER_READF64:
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        build.vmovsd(inst.regX64, qword[bufferAddrOp(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c))]);
        break;

    case IrCmd::BUFFER_WRITEF64:
        if (inst.c.kind == IrOpKind::Constant)
        {
            ScopedRegX64 tmp{regs, SizeX64::xmmword};
            build.vmovsd(tmp.reg, build.f64(doubleOp(inst.c)));
            build.vmovsd(qword[bufferAddrOp(inst.a, inst.b, inst.d.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.d))], tmp.reg);
        }
        else if (inst.c.kind == IrOpKind::Inst)
        {
            build.vmovsd(qword[bufferAddrOp(inst.a, inst.b, inst.d.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.d))], regOp(inst.c));
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;

    // Pseudo instructions
    case IrCmd::NOP:
    case IrCmd::SUBSTITUTE:
        CODEGEN_ASSERT(!"Pseudo instructions should not be lowered");
        break;
    }

    valueTracker.afterInstLowering(inst, index);

    if (FFlag::LuauCodeGenRegAutoSpillA64)
        regs.currInstIdx = kInvalidInstIdx;

    regs.freeLastUseRegs(inst, index);
}

void IrLoweringX64::finishBlock(const IrBlock& curr, const IrBlock& next)
{
    if (!regs.spills.empty())
    {
        // If we have spills remaining, we have to immediately lower the successor block
        for (uint32_t predIdx : predecessors(function.cfg, function.getBlockIndex(next)))
            CODEGEN_ASSERT(predIdx == function.getBlockIndex(curr) || function.blocks[predIdx].kind == IrBlockKind::Dead);

        // And the next block cannot be a join block in cfg
        CODEGEN_ASSERT(next.useCount == 1);
    }
}

void IrLoweringX64::finishFunction()
{
    if (build.logText)
        build.logAppend("; interrupt handlers\n");

    for (InterruptHandler& handler : interruptHandlers)
    {
        build.setLabel(handler.self);
        build.mov(eax, handler.pcpos + 1);
        build.lea(rbx, handler.next);
        build.jmp(helpers.interrupt);
    }

    if (build.logText)
        build.logAppend("; exit handlers\n");

    for (ExitHandler& handler : exitHandlers)
    {
        CODEGEN_ASSERT(handler.pcpos != kVmExitEntryGuardPc);

        build.setLabel(handler.self);

        build.mov(edx, handler.pcpos * sizeof(Instruction));
        build.jmp(helpers.updatePcAndContinueInVm);
    }

    if (FFlag::LuauCodeGenUnassignedBcTargetAbort)
    {
        // An undefined instruction is placed after the function to be used as an aborting jump offset
        function.endLocation = build.setLabel().location;
        build.ud2();
    }

    if (stats)
    {
        if (regs.maxUsedSlot > kSpillSlots)
            stats->regAllocErrors++;

        if (regs.maxUsedSlot > stats->maxSpillSlotsUsed)
            stats->maxSpillSlotsUsed = regs.maxUsedSlot;
    }
}

bool IrLoweringX64::hasError() const
{
    // If register allocator had to use more stack slots than we have available, this function can't run natively
    if (regs.maxUsedSlot > kSpillSlots)
        return true;

    return false;
}

bool IrLoweringX64::isFallthroughBlock(const IrBlock& target, const IrBlock& next)
{
    return target.start == next.start;
}

Label& IrLoweringX64::getTargetLabel(IrOp op, Label& fresh)
{
    if (op.kind == IrOpKind::Undef)
        return fresh;

    if (op.kind == IrOpKind::VmExit)
    {
        // Special exit case that doesn't have to update pcpos
        if (vmExitOp(op) == kVmExitEntryGuardPc)
            return helpers.exitContinueVmClearNativeFlag;

        if (uint32_t* index = exitHandlerMap.find(vmExitOp(op)))
            return exitHandlers[*index].self;

        return fresh;
    }

    return labelOp(op);
}

void IrLoweringX64::finalizeTargetLabel(IrOp op, Label& fresh)
{
    if (op.kind == IrOpKind::VmExit && fresh.id != 0 && fresh.id != helpers.exitContinueVmClearNativeFlag.id)
    {
        exitHandlerMap[vmExitOp(op)] = uint32_t(exitHandlers.size());
        exitHandlers.push_back({fresh, vmExitOp(op)});
    }
}

void IrLoweringX64::jumpOrFallthrough(IrBlock& target, const IrBlock& next)
{
    if (!isFallthroughBlock(target, next))
        build.jmp(target.label);
}

void IrLoweringX64::jumpOrAbortOnUndef(ConditionX64 cond, IrOp target, const IrBlock& next)
{
    Label fresh;
    Label& label = getTargetLabel(target, fresh);

    if (target.kind == IrOpKind::Undef)
    {
        if (cond == ConditionX64::Count)
        {
            build.ud2(); // Unconditional jump to abort is just an abort
        }
        else
        {
            build.jcc(getNegatedCondition(cond), label);
            build.ud2();
            build.setLabel(label);
        }
    }
    else if (cond == ConditionX64::Count)
    {
        // Unconditional jump can be skipped if it's a fallthrough
        if (target.kind == IrOpKind::VmExit || !isFallthroughBlock(blockOp(target), next))
            build.jmp(label);
    }
    else
    {
        build.jcc(cond, label);
    }

    finalizeTargetLabel(target, fresh);
}

void IrLoweringX64::jumpOrAbortOnUndef(IrOp target, const IrBlock& next)
{
    jumpOrAbortOnUndef(ConditionX64::Count, target, next);
}

OperandX64 IrLoweringX64::memRegDoubleOp(IrOp op)
{
    switch (op.kind)
    {
    case IrOpKind::Inst:
        return regOp(op);
    case IrOpKind::Constant:
        return build.f64(doubleOp(op));
    case IrOpKind::VmReg:
        return luauRegValue(vmRegOp(op));
    case IrOpKind::VmConst:
        return luauConstantValue(vmConstOp(op));
    default:
        CODEGEN_ASSERT(!"Unsupported operand kind");
    }

    return noreg;
}

OperandX64 IrLoweringX64::memRegUintOp(IrOp op)
{
    switch (op.kind)
    {
    case IrOpKind::Inst:
        return regOp(op);
    case IrOpKind::Constant:
        return OperandX64(unsigned(intOp(op)));
    case IrOpKind::VmReg:
        return luauRegValueInt(vmRegOp(op));
    default:
        CODEGEN_ASSERT(!"Unsupported operand kind");
    }

    return noreg;
}

OperandX64 IrLoweringX64::memRegTagOp(IrOp op)
{
    switch (op.kind)
    {
    case IrOpKind::Inst:
        return regOp(op);
    case IrOpKind::VmReg:
        return luauRegTag(vmRegOp(op));
    case IrOpKind::VmConst:
        return luauConstantTag(vmConstOp(op));
    default:
        CODEGEN_ASSERT(!"Unsupported operand kind");
    }

    return noreg;
}

RegisterX64 IrLoweringX64::regOp(IrOp op)
{
    IrInst& inst = function.instOp(op);

    if (inst.spilled || inst.needsReload)
        regs.restore(inst, false);

    CODEGEN_ASSERT(inst.regX64 != noreg);
    return inst.regX64;
}

OperandX64 IrLoweringX64::bufferAddrOp(IrOp bufferOp, IrOp indexOp, uint8_t tag)
{
    CODEGEN_ASSERT(tag == LUA_TUSERDATA || tag == LUA_TBUFFER);
    int dataOffset = tag == LUA_TBUFFER ? offsetof(Buffer, data) : offsetof(Udata, data);

    if (indexOp.kind == IrOpKind::Inst)
        return regOp(bufferOp) + qwordReg(regOp(indexOp)) + dataOffset;
    else if (indexOp.kind == IrOpKind::Constant)
        return regOp(bufferOp) + intOp(indexOp) + dataOffset;

    CODEGEN_ASSERT(!"Unsupported instruction form");
    return noreg;
}

RegisterX64 IrLoweringX64::vecOp(IrOp op, ScopedRegX64& tmp)
{
    IrInst source = function.instOp(op);
    CODEGEN_ASSERT(source.cmd != IrCmd::SUBSTITUTE); // we don't process substitutions

    // source that comes from memory or from tag instruction has .w = TVECTOR, which is denormal
    // to avoid performance degradation on some CPUs we mask this component to produce zero
    // otherwise we conservatively assume the vector is a result of a well formed math op so .w is a normal number or zero
    if (source.cmd != IrCmd::LOAD_TVALUE && source.cmd != IrCmd::TAG_VECTOR)
        return regOp(op);

    tmp.alloc(SizeX64::xmmword);
    build.vandps(tmp.reg, regOp(op), vectorAndMaskOp());
    return tmp.reg;
}

IrConst IrLoweringX64::constOp(IrOp op) const
{
    return function.constOp(op);
}

uint8_t IrLoweringX64::tagOp(IrOp op) const
{
    return function.tagOp(op);
}

int IrLoweringX64::intOp(IrOp op) const
{
    return function.intOp(op);
}

unsigned IrLoweringX64::uintOp(IrOp op) const
{
    return function.uintOp(op);
}

unsigned IrLoweringX64::importOp(IrOp op) const
{
    return function.importOp(op);
}

double IrLoweringX64::doubleOp(IrOp op) const
{
    return function.doubleOp(op);
}

IrBlock& IrLoweringX64::blockOp(IrOp op) const
{
    return function.blockOp(op);
}

Label& IrLoweringX64::labelOp(IrOp op) const
{
    return blockOp(op).label;
}

OperandX64 IrLoweringX64::vectorAndMaskOp()
{
    if (vectorAndMask.base == noreg)
        vectorAndMask = build.u32x4(~0u, ~0u, ~0u, 0);

    return vectorAndMask;
}

} // namespace X64
} // namespace CodeGen
} // namespace Luau
