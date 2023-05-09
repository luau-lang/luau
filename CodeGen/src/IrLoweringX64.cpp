// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrLoweringX64.h"

#include "Luau/CodeGen.h"
#include "Luau/DenseHash.h"
#include "Luau/IrAnalysis.h"
#include "Luau/IrCallWrapperX64.h"
#include "Luau/IrDump.h"
#include "Luau/IrUtils.h"

#include "EmitBuiltinsX64.h"
#include "EmitCommonX64.h"
#include "EmitInstructionX64.h"
#include "NativeState.h"

#include "lstate.h"

namespace Luau
{
namespace CodeGen
{
namespace X64
{

IrLoweringX64::IrLoweringX64(AssemblyBuilderX64& build, ModuleHelpers& helpers, NativeState& data, IrFunction& function)
    : build(build)
    , helpers(helpers)
    , data(data)
    , function(function)
    , regs(build, function)
    , valueTracker(function)
{
    // In order to allocate registers during lowering, we need to know where instruction results are last used
    updateLastUseLocations(function);

    valueTracker.setRestoreCallack(&regs, [](void* context, IrInst& inst) {
        ((IrRegAllocX64*)context)->restore(inst, false);
    });

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
        LUAU_ASSERT(!"Unsupported instruction form");
    }
    build.vmovss(dst, tmp.reg);
}

void IrLoweringX64::lowerInst(IrInst& inst, uint32_t index, IrBlock& next)
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
            LUAU_ASSERT(!"Unsupported instruction form");
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
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::LOAD_DOUBLE:
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        if (inst.a.kind == IrOpKind::VmReg)
            build.vmovsd(inst.regX64, luauRegValue(vmRegOp(inst.a)));
        else if (inst.a.kind == IrOpKind::VmConst)
            build.vmovsd(inst.regX64, luauConstantValue(vmConstOp(inst.a)));
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::LOAD_INT:
        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        build.mov(inst.regX64, luauRegValueInt(vmRegOp(inst.a)));
        break;
    case IrCmd::LOAD_TVALUE:
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        if (inst.a.kind == IrOpKind::VmReg)
            build.vmovups(inst.regX64, luauReg(vmRegOp(inst.a)));
        else if (inst.a.kind == IrOpKind::VmConst)
            build.vmovups(inst.regX64, luauConstant(vmConstOp(inst.a)));
        else if (inst.a.kind == IrOpKind::Inst)
            build.vmovups(inst.regX64, xmmword[regOp(inst.a)]);
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::LOAD_NODE_VALUE_TV:
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        build.vmovups(inst.regX64, luauNodeValue(regOp(inst.a)));
        break;
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
            build.add(inst.regX64, qword[regOp(inst.a) + offsetof(Table, array)]);
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            inst.regX64 = regs.allocRegOrReuse(SizeX64::qword, index, {inst.a});

            build.mov(inst.regX64, qword[regOp(inst.a) + offsetof(Table, array)]);

            if (intOp(inst.b) != 0)
                build.lea(inst.regX64, addr[inst.regX64 + intOp(inst.b) * sizeof(TValue)]);
        }
        else
        {
            LUAU_ASSERT(!"Unsupported instruction form");
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

        build.mov(inst.regX64, qword[regOp(inst.a) + offsetof(Table, node)]);
        build.mov(dwordReg(tmp.reg), 1);
        build.mov(byteReg(shiftTmp.reg), byte[regOp(inst.a) + offsetof(Table, lsizenode)]);
        build.shl(dwordReg(tmp.reg), byteReg(shiftTmp.reg));
        build.dec(dwordReg(tmp.reg));
        build.and_(dwordReg(tmp.reg), uintOp(inst.b));
        build.shl(tmp.reg, kLuaNodeSizeLog2);
        build.add(inst.regX64, tmp.reg);
        break;
    };
    case IrCmd::STORE_TAG:
        if (inst.b.kind == IrOpKind::Constant)
            build.mov(luauRegTag(vmRegOp(inst.a)), tagOp(inst.b));
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::STORE_POINTER:
        build.mov(luauRegValue(vmRegOp(inst.a)), regOp(inst.b));
        break;
    case IrCmd::STORE_DOUBLE:
        if (inst.b.kind == IrOpKind::Constant)
        {
            ScopedRegX64 tmp{regs, SizeX64::xmmword};

            build.vmovsd(tmp.reg, build.f64(doubleOp(inst.b)));
            build.vmovsd(luauRegValue(vmRegOp(inst.a)), tmp.reg);
        }
        else if (inst.b.kind == IrOpKind::Inst)
        {
            build.vmovsd(luauRegValue(vmRegOp(inst.a)), regOp(inst.b));
        }
        else
        {
            LUAU_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::STORE_INT:
        if (inst.b.kind == IrOpKind::Constant)
            build.mov(luauRegValueInt(vmRegOp(inst.a)), intOp(inst.b));
        else if (inst.b.kind == IrOpKind::Inst)
            build.mov(luauRegValueInt(vmRegOp(inst.a)), regOp(inst.b));
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::STORE_VECTOR:
        storeDoubleAsFloat(luauRegValueVector(vmRegOp(inst.a), 0), inst.b);
        storeDoubleAsFloat(luauRegValueVector(vmRegOp(inst.a), 1), inst.c);
        storeDoubleAsFloat(luauRegValueVector(vmRegOp(inst.a), 2), inst.d);
        break;
    case IrCmd::STORE_TVALUE:
        if (inst.a.kind == IrOpKind::VmReg)
            build.vmovups(luauReg(vmRegOp(inst.a)), regOp(inst.b));
        else if (inst.a.kind == IrOpKind::Inst)
            build.vmovups(xmmword[regOp(inst.a)], regOp(inst.b));
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::STORE_NODE_VALUE_TV:
        build.vmovups(luauNodeValue(regOp(inst.a)), regOp(inst.b));
        break;
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
            LUAU_ASSERT(!"Unsupported instruction form");
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

        RegisterX64 src = regOp(inst.a);

        if (inst.regX64 == src)
        {
            build.vxorpd(inst.regX64, inst.regX64, build.f64(-0.0));
        }
        else
        {
            build.vmovsd(inst.regX64, src, src);
            build.vxorpd(inst.regX64, inst.regX64, build.f64(-0.0));
        }

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
    case IrCmd::NOT_ANY:
    {
        // TODO: if we have a single user which is a STORE_INT, we are missing the opportunity to write directly to target
        inst.regX64 = regs.allocRegOrReuse(SizeX64::dword, index, {inst.a, inst.b});

        Label saveone, savezero, exit;

        if (inst.a.kind == IrOpKind::Constant)
        {
            // Other cases should've been constant folded
            LUAU_ASSERT(tagOp(inst.a) == LUA_TBOOLEAN);
        }
        else
        {
            build.cmp(regOp(inst.a), LUA_TNIL);
            build.jcc(ConditionX64::Equal, saveone);

            build.cmp(regOp(inst.a), LUA_TBOOLEAN);
            build.jcc(ConditionX64::NotEqual, savezero);
        }

        build.cmp(regOp(inst.b), 0);
        build.jcc(ConditionX64::Equal, saveone);

        build.setLabel(savezero);
        build.mov(inst.regX64, 0);
        build.jmp(exit);

        build.setLabel(saveone);
        build.mov(inst.regX64, 1);

        build.setLabel(exit);
        break;
    }
    case IrCmd::JUMP:
        jumpOrFallthrough(blockOp(inst.a), next);
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
        LUAU_ASSERT(inst.b.kind == IrOpKind::Inst || inst.b.kind == IrOpKind::Constant);
        OperandX64 opb = inst.b.kind == IrOpKind::Inst ? regOp(inst.b) : OperandX64(tagOp(inst.b));

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
    case IrCmd::JUMP_EQ_INT:
        build.cmp(regOp(inst.a), intOp(inst.b));

        build.jcc(ConditionX64::Equal, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::JUMP_LT_INT:
        build.cmp(regOp(inst.a), intOp(inst.b));

        build.jcc(ConditionX64::Less, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::JUMP_GE_UINT:
        build.cmp(regOp(inst.a), unsigned(intOp(inst.b)));

        build.jcc(ConditionX64::AboveEqual, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::JUMP_EQ_POINTER:
        build.cmp(regOp(inst.a), regOp(inst.b));

        build.jcc(ConditionX64::Equal, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::JUMP_CMP_NUM:
    {
        IrCondition cond = conditionOp(inst.c);

        ScopedRegX64 tmp{regs, SizeX64::xmmword};

        // TODO: jumpOnNumberCmp should work on IrCondition directly
        jumpOnNumberCmp(build, tmp.reg, memRegDoubleOp(inst.a), memRegDoubleOp(inst.b), cond, labelOp(inst.d));
        jumpOrFallthrough(blockOp(inst.e), next);
        break;
    }
    case IrCmd::JUMP_CMP_ANY:
        jumpOnAnyCmpFallback(regs, build, vmRegOp(inst.a), vmRegOp(inst.b), conditionOp(inst.c), labelOp(inst.d));
        jumpOrFallthrough(blockOp(inst.e), next);
        break;
    case IrCmd::JUMP_SLOT_MATCH:
    {
        ScopedRegX64 tmp{regs, SizeX64::qword};

        jumpIfNodeKeyNotInExpectedSlot(build, tmp.reg, regOp(inst.a), luauConstantValue(vmConstOp(inst.b)), labelOp(inst.d));
        jumpOrFallthrough(blockOp(inst.c), next);
        break;
    }
    case IrCmd::TABLE_LEN:
    {
        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, regOp(inst.a), inst.a);
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaH_getn)]);

        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);
        build.vcvtsi2sd(inst.regX64, inst.regX64, eax);
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

        build.mov(tmp.reg, qword[regOp(inst.a) + offsetof(Table, metatable)]);
        regs.freeLastUseReg(function.instOp(inst.a), index); // Release before the call if it's the last use

        build.test(tmp.reg, tmp.reg);
        build.jcc(ConditionX64::Zero, labelOp(inst.c)); // No metatable

        build.test(byte[tmp.reg + offsetof(Table, tmcache)], 1 << intOp(inst.b));
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

        inst.regX64 = regs.takeReg(rax, index);
        break;
    }
    case IrCmd::INT_TO_NUM:
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        build.vcvtsi2sd(inst.regX64, inst.regX64, regOp(inst.a));
        break;
    case IrCmd::UINT_TO_NUM:
        inst.regX64 = regs.allocReg(SizeX64::xmmword, index);

        build.vcvtsi2sd(inst.regX64, inst.regX64, qwordReg(regOp(inst.a)));
        break;
    case IrCmd::NUM_TO_INT:
        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        build.vcvttsd2si(inst.regX64, memRegDoubleOp(inst.a));
        break;
    case IrCmd::NUM_TO_UINT:
        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        build.vcvttsd2si(qwordReg(inst.regX64), memRegDoubleOp(inst.a));
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
            LUAU_ASSERT(!"Unsupported instruction form");
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
        OperandX64 arg2 = inst.d.kind != IrOpKind::Undef ? memRegDoubleOp(inst.d) : OperandX64{0};

        emitBuiltin(regs, build, uintOp(inst.a), vmRegOp(inst.b), vmRegOp(inst.c), arg2, intOp(inst.e), intOp(inst.f));
        break;
    }
    case IrCmd::INVOKE_FASTCALL:
    {
        unsigned bfid = uintOp(inst.a);

        OperandX64 args = 0;

        if (inst.d.kind == IrOpKind::VmReg)
            args = luauRegAddress(vmRegOp(inst.d));
        else if (inst.d.kind == IrOpKind::VmConst)
            args = luauConstantAddress(vmConstOp(inst.d));
        else
            LUAU_ASSERT(inst.d.kind == IrOpKind::Undef);

        int ra = vmRegOp(inst.b);
        int arg = vmRegOp(inst.c);
        int nparams = intOp(inst.e);
        int nresults = intOp(inst.f);

        ScopedRegX64 func{regs, SizeX64::qword};
        build.mov(func.reg, qword[rNativeContext + offsetof(NativeContext, luauF_table) + bfid * sizeof(luau_FastFunction)]);

        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::qword, luauRegAddress(ra));
        callWrap.addArgument(SizeX64::qword, luauRegAddress(arg));
        callWrap.addArgument(SizeX64::dword, nresults);
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
        if (inst.c.kind == IrOpKind::VmReg)
            callArithHelper(regs, build, vmRegOp(inst.a), vmRegOp(inst.b), luauRegAddress(vmRegOp(inst.c)), TMS(intOp(inst.d)));
        else
            callArithHelper(regs, build, vmRegOp(inst.a), vmRegOp(inst.b), luauConstantAddress(vmConstOp(inst.c)), TMS(intOp(inst.d)));
        break;
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
            TValue n;
            setnvalue(&n, uintOp(inst.c));
            callGetTable(regs, build, vmRegOp(inst.b), build.bytes(&n, sizeof(n)), vmRegOp(inst.a));
        }
        else
        {
            LUAU_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::SET_TABLE:
        if (inst.c.kind == IrOpKind::VmReg)
        {
            callSetTable(regs, build, vmRegOp(inst.b), luauRegAddress(vmRegOp(inst.c)), vmRegOp(inst.a));
        }
        else if (inst.c.kind == IrOpKind::Constant)
        {
            TValue n;
            setnvalue(&n, uintOp(inst.c));
            callSetTable(regs, build, vmRegOp(inst.b), build.bytes(&n, sizeof(n)), vmRegOp(inst.a));
        }
        else
        {
            LUAU_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::GET_IMPORT:
    {
        ScopedRegX64 tmp1{regs, SizeX64::qword};

        build.mov(tmp1.reg, sClosure);

        IrCallWrapperX64 callWrap(regs, build, index);
        callWrap.addArgument(SizeX64::qword, rState);
        callWrap.addArgument(SizeX64::qword, qword[tmp1.release() + offsetof(Closure, env)]);
        callWrap.addArgument(SizeX64::qword, rConstants);
        callWrap.addArgument(SizeX64::dword, uintOp(inst.b));
        callWrap.addArgument(SizeX64::dword, 0);
        callWrap.call(qword[rNativeContext + offsetof(NativeContext, luaV_getimport)]);

        emitUpdateBase(build);

        ScopedRegX64 tmp2{regs, SizeX64::qword};

        // setobj2s(L, ra, L->top - 1)
        build.mov(tmp2.reg, qword[rState + offsetof(lua_State, top)]);
        build.sub(tmp2.reg, sizeof(TValue));

        ScopedRegX64 tmp3{regs, SizeX64::xmmword};
        build.vmovups(tmp3.reg, xmmword[tmp2.reg]);
        build.vmovups(luauReg(vmRegOp(inst.a)), tmp3.reg);

        // L->top--
        build.mov(qword[rState + offsetof(lua_State, top)], tmp2.reg);
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

        callBarrierObject(regs, build, tmp2.release(), {}, vmRegOp(inst.b));
        break;
    }
    case IrCmd::PREPARE_FORN:
        callPrepareForN(regs, build, vmRegOp(inst.a), vmRegOp(inst.b), vmRegOp(inst.c));
        break;
    case IrCmd::CHECK_TAG:
        build.cmp(memRegTagOp(inst.a), tagOp(inst.b));
        build.jcc(ConditionX64::NotEqual, labelOp(inst.c));
        break;
    case IrCmd::CHECK_READONLY:
        build.cmp(byte[regOp(inst.a) + offsetof(Table, readonly)], 0);
        build.jcc(ConditionX64::NotEqual, labelOp(inst.b));
        break;
    case IrCmd::CHECK_NO_METATABLE:
        build.cmp(qword[regOp(inst.a) + offsetof(Table, metatable)], 0);
        build.jcc(ConditionX64::NotEqual, labelOp(inst.b));
        break;
    case IrCmd::CHECK_SAFE_ENV:
    {
        ScopedRegX64 tmp{regs, SizeX64::qword};

        build.mov(tmp.reg, sClosure);
        build.mov(tmp.reg, qword[tmp.reg + offsetof(Closure, env)]);
        build.cmp(byte[tmp.reg + offsetof(Table, safeenv)], 0);
        build.jcc(ConditionX64::Equal, labelOp(inst.a));
        break;
    }
    case IrCmd::CHECK_ARRAY_SIZE:
        if (inst.b.kind == IrOpKind::Inst)
            build.cmp(dword[regOp(inst.a) + offsetof(Table, sizearray)], regOp(inst.b));
        else if (inst.b.kind == IrOpKind::Constant)
            build.cmp(dword[regOp(inst.a) + offsetof(Table, sizearray)], intOp(inst.b));
        else
            LUAU_ASSERT(!"Unsupported instruction form");

        build.jcc(ConditionX64::BelowEqual, labelOp(inst.c));
        break;
    case IrCmd::CHECK_SLOT_MATCH:
    {
        ScopedRegX64 tmp{regs, SizeX64::qword};

        jumpIfNodeKeyNotInExpectedSlot(build, tmp.reg, regOp(inst.a), luauConstantValue(vmConstOp(inst.b)), labelOp(inst.c));
        break;
    }
    case IrCmd::CHECK_NODE_NO_NEXT:
    {
        ScopedRegX64 tmp{regs, SizeX64::dword};

        build.mov(tmp.reg, dword[regOp(inst.a) + offsetof(LuaNode, key) + kOffsetOfTKeyNext]);
        build.shr(tmp.reg, kNextBitOffset);
        build.jcc(ConditionX64::NotZero, labelOp(inst.b));
        break;
    }
    case IrCmd::INTERRUPT:
        emitInterrupt(regs, build, uintOp(inst.a));
        break;
    case IrCmd::CHECK_GC:
        callStepGc(regs, build);
        break;
    case IrCmd::BARRIER_OBJ:
        callBarrierObject(regs, build, regOp(inst.a), inst.a, vmRegOp(inst.b));
        break;
    case IrCmd::BARRIER_TABLE_BACK:
        callBarrierTableFast(regs, build, regOp(inst.a), inst.a);
        break;
    case IrCmd::BARRIER_TABLE_FORWARD:
    {
        Label skip;

        ScopedRegX64 tmp{regs, SizeX64::qword};
        checkObjectBarrierConditions(build, tmp.reg, regOp(inst.a), vmRegOp(inst.b), skip);

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
        // This is like emitSetSavedPc, but using register allocation instead of relying on rax/rdx
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

        // ra <= L->openuval->v
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
        emitInstSetList(regs, build, vmRegOp(inst.b), vmRegOp(inst.c), intOp(inst.d), uintOp(inst.e));
        break;
    case IrCmd::CALL:
        regs.assertAllFree();
        regs.assertNoSpills();
        emitInstCall(build, helpers, vmRegOp(inst.a), intOp(inst.b), intOp(inst.c));
        break;
    case IrCmd::RETURN:
        regs.assertAllFree();
        regs.assertNoSpills();
        emitInstReturn(build, helpers, vmRegOp(inst.a), intOp(inst.b));
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
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        emitFallback(regs, build, data, LOP_GETGLOBAL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETGLOBAL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        emitFallback(regs, build, data, LOP_SETGLOBAL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETTABLEKS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitFallback(regs, build, data, LOP_GETTABLEKS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETTABLEKS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitFallback(regs, build, data, LOP_SETTABLEKS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_NAMECALL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitFallback(regs, build, data, LOP_NAMECALL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_PREPVARARGS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::Constant);

        emitFallback(regs, build, data, LOP_PREPVARARGS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETVARARGS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);

        emitFallback(regs, build, data, LOP_GETVARARGS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_NEWCLOSURE:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);

        emitFallback(regs, build, data, LOP_NEWCLOSURE, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_DUPCLOSURE:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        emitFallback(regs, build, data, LOP_DUPCLOSURE, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_FORGPREP:
        emitFallback(regs, build, data, LOP_FORGPREP, uintOp(inst.a));
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
        // Custom bit shift value can only be placed in cl
        ScopedRegX64 shiftTmp{regs, regs.takeReg(ecx, kInvalidInstIdx)};

        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        build.mov(shiftTmp.reg, memRegUintOp(inst.b));

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        build.shl(inst.regX64, byteReg(shiftTmp.reg));
        break;
    }
    case IrCmd::BITRSHIFT_UINT:
    {
        // Custom bit shift value can only be placed in cl
        ScopedRegX64 shiftTmp{regs, regs.takeReg(ecx, kInvalidInstIdx)};

        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        build.mov(shiftTmp.reg, memRegUintOp(inst.b));

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        build.shr(inst.regX64, byteReg(shiftTmp.reg));
        break;
    }
    case IrCmd::BITARSHIFT_UINT:
    {
        // Custom bit shift value can only be placed in cl
        ScopedRegX64 shiftTmp{regs, regs.takeReg(ecx, kInvalidInstIdx)};

        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        build.mov(shiftTmp.reg, memRegUintOp(inst.b));

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        build.sar(inst.regX64, byteReg(shiftTmp.reg));
        break;
    }
    case IrCmd::BITLROTATE_UINT:
    {
        // Custom bit shift value can only be placed in cl
        ScopedRegX64 shiftTmp{regs, regs.takeReg(ecx, kInvalidInstIdx)};

        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        build.mov(shiftTmp.reg, memRegUintOp(inst.b));

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        build.rol(inst.regX64, byteReg(shiftTmp.reg));
        break;
    }
    case IrCmd::BITRROTATE_UINT:
    {
        // Custom bit shift value can only be placed in cl
        ScopedRegX64 shiftTmp{regs, regs.takeReg(ecx, kInvalidInstIdx)};

        inst.regX64 = regs.allocReg(SizeX64::dword, index);

        build.mov(shiftTmp.reg, memRegUintOp(inst.b));

        if (inst.a.kind != IrOpKind::Inst || inst.regX64 != regOp(inst.a))
            build.mov(inst.regX64, memRegUintOp(inst.a));

        build.ror(inst.regX64, byteReg(shiftTmp.reg));
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

    // Pseudo instructions
    case IrCmd::NOP:
    case IrCmd::SUBSTITUTE:
        LUAU_ASSERT(!"Pseudo instructions should not be lowered");
        break;
    }

    valueTracker.afterInstLowering(inst, index);

    regs.freeLastUseRegs(inst, index);
}

void IrLoweringX64::finishBlock()
{
    regs.assertNoSpills();
}

bool IrLoweringX64::hasError() const
{
    // If register allocator had to use more stack slots than we have available, this function can't run natively
    if (regs.maxUsedSlot > kSpillSlots)
        return true;

    return false;
}

bool IrLoweringX64::isFallthroughBlock(IrBlock target, IrBlock next)
{
    return target.start == next.start;
}

void IrLoweringX64::jumpOrFallthrough(IrBlock& target, IrBlock& next)
{
    if (!isFallthroughBlock(target, next))
        build.jmp(target.label);
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
        LUAU_ASSERT(!"Unsupported operand kind");
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
    default:
        LUAU_ASSERT(!"Unsupported operand kind");
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
        LUAU_ASSERT(!"Unsupported operand kind");
    }

    return noreg;
}

RegisterX64 IrLoweringX64::regOp(IrOp op)
{
    IrInst& inst = function.instOp(op);

    if (inst.spilled || inst.needsReload)
        regs.restore(inst, false);

    LUAU_ASSERT(inst.regX64 != noreg);
    return inst.regX64;
}

IrConst IrLoweringX64::constOp(IrOp op) const
{
    return function.constOp(op);
}

uint8_t IrLoweringX64::tagOp(IrOp op) const
{
    return function.tagOp(op);
}

bool IrLoweringX64::boolOp(IrOp op) const
{
    return function.boolOp(op);
}

int IrLoweringX64::intOp(IrOp op) const
{
    return function.intOp(op);
}

unsigned IrLoweringX64::uintOp(IrOp op) const
{
    return function.uintOp(op);
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

} // namespace X64
} // namespace CodeGen
} // namespace Luau
