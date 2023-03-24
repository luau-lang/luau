// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrLoweringX64.h"

#include "Luau/CodeGen.h"
#include "Luau/DenseHash.h"
#include "Luau/IrAnalysis.h"
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
    , regs(function)
{
    // In order to allocate registers during lowering, we need to know where instruction results are last used
    updateLastUseLocations(function);
}

void IrLoweringX64::lowerInst(IrInst& inst, uint32_t index, IrBlock& next)
{
    switch (inst.cmd)
    {
    case IrCmd::LOAD_TAG:
        inst.regX64 = regs.allocGprReg(SizeX64::dword);

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
        inst.regX64 = regs.allocGprReg(SizeX64::qword);

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
        inst.regX64 = regs.allocXmmReg();

        if (inst.a.kind == IrOpKind::VmReg)
            build.vmovsd(inst.regX64, luauRegValue(vmRegOp(inst.a)));
        else if (inst.a.kind == IrOpKind::VmConst)
            build.vmovsd(inst.regX64, luauConstantValue(vmConstOp(inst.a)));
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    case IrCmd::LOAD_INT:
        inst.regX64 = regs.allocGprReg(SizeX64::dword);

        build.mov(inst.regX64, luauRegValueInt(vmRegOp(inst.a)));
        break;
    case IrCmd::LOAD_TVALUE:
        inst.regX64 = regs.allocXmmReg();

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
        inst.regX64 = regs.allocXmmReg();

        build.vmovups(inst.regX64, luauNodeValue(regOp(inst.a)));
        break;
    case IrCmd::LOAD_ENV:
        inst.regX64 = regs.allocGprReg(SizeX64::qword);

        build.mov(inst.regX64, sClosure);
        build.mov(inst.regX64, qword[inst.regX64 + offsetof(Closure, env)]);
        break;
    case IrCmd::GET_ARR_ADDR:
        if (inst.b.kind == IrOpKind::Inst)
        {
            inst.regX64 = regs.allocGprRegOrReuse(SizeX64::qword, index, {inst.b});

            if (dwordReg(inst.regX64) != regOp(inst.b))
                build.mov(dwordReg(inst.regX64), regOp(inst.b));

            build.shl(dwordReg(inst.regX64), kTValueSizeLog2);
            build.add(inst.regX64, qword[regOp(inst.a) + offsetof(Table, array)]);
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            inst.regX64 = regs.allocGprRegOrReuse(SizeX64::qword, index, {inst.a});

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
        inst.regX64 = regs.allocGprReg(SizeX64::qword);

        ScopedRegX64 tmp{regs, SizeX64::qword};

        getTableNodeAtCachedSlot(build, tmp.reg, inst.regX64, regOp(inst.a), uintOp(inst.b));
        break;
    }
    case IrCmd::GET_HASH_NODE_ADDR:
    {
        inst.regX64 = regs.allocGprReg(SizeX64::qword);

        // Custom bit shift value can only be placed in cl
        ScopedRegX64 shiftTmp{regs, regs.takeGprReg(rcx)};

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
    {
        if (inst.b.kind == IrOpKind::Constant)
            build.mov(luauRegValueInt(vmRegOp(inst.a)), intOp(inst.b));
        else if (inst.b.kind == IrOpKind::Inst)
            build.mov(luauRegValueInt(vmRegOp(inst.a)), regOp(inst.b));
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    }
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
        inst.regX64 = regs.allocGprRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.regX64 == regOp(inst.a) && intOp(inst.b) == 1)
            build.inc(inst.regX64);
        else if (inst.regX64 == regOp(inst.a))
            build.add(inst.regX64, intOp(inst.b));
        else
            build.lea(inst.regX64, addr[regOp(inst.a) + intOp(inst.b)]);
        break;
    case IrCmd::SUB_INT:
        inst.regX64 = regs.allocGprRegOrReuse(SizeX64::dword, index, {inst.a});

        if (inst.regX64 == regOp(inst.a) && intOp(inst.b) == 1)
            build.dec(inst.regX64);
        else if (inst.regX64 == regOp(inst.a))
            build.sub(inst.regX64, intOp(inst.b));
        else
            build.lea(inst.regX64, addr[regOp(inst.a) - intOp(inst.b)]);
        break;
    case IrCmd::ADD_NUM:
        inst.regX64 = regs.allocXmmRegOrReuse(index, {inst.a, inst.b});

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
        inst.regX64 = regs.allocXmmRegOrReuse(index, {inst.a, inst.b});

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
        inst.regX64 = regs.allocXmmRegOrReuse(index, {inst.a, inst.b});

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
        inst.regX64 = regs.allocXmmRegOrReuse(index, {inst.a, inst.b});

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
        inst.regX64 = regs.allocXmmRegOrReuse(index, {inst.a, inst.b});

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
    case IrCmd::POW_NUM:
    {
        inst.regX64 = regs.allocXmmRegOrReuse(index, {inst.a, inst.b});

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
            // TODO: this doesn't happen with current local-only register allocation, but has to be handled in the future
            LUAU_ASSERT(regOp(inst.b) != xmm0);

            if (lhs != xmm0)
                build.vmovsd(xmm0, lhs, lhs);

            if (regOp(inst.b) != xmm1)
                build.vmovsd(xmm1, regOp(inst.b), regOp(inst.b));

            build.call(qword[rNativeContext + offsetof(NativeContext, libm_pow)]);

            if (inst.regX64 != xmm0)
                build.vmovsd(inst.regX64, xmm0, xmm0);
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            double rhs = doubleOp(inst.b);

            if (rhs == 2.0)
            {
                build.vmulsd(inst.regX64, lhs, lhs);
            }
            else if (rhs == 0.5)
            {
                build.vsqrtsd(inst.regX64, lhs, lhs);
            }
            else if (rhs == 3.0)
            {
                ScopedRegX64 tmp{regs, SizeX64::xmmword};

                build.vmulsd(tmp.reg, lhs, lhs);
                build.vmulsd(inst.regX64, lhs, tmp.reg);
            }
            else
            {
                if (lhs != xmm0)
                    build.vmovsd(xmm0, xmm0, lhs);

                build.vmovsd(xmm1, build.f64(rhs));
                build.call(qword[rNativeContext + offsetof(NativeContext, libm_pow)]);

                if (inst.regX64 != xmm0)
                    build.vmovsd(inst.regX64, xmm0, xmm0);
            }
        }
        else
        {
            if (lhs != xmm0)
                build.vmovsd(xmm0, lhs, lhs);

            build.vmovsd(xmm1, memRegDoubleOp(inst.b));
            build.call(qword[rNativeContext + offsetof(NativeContext, libm_pow)]);

            if (inst.regX64 != xmm0)
                build.vmovsd(inst.regX64, xmm0, xmm0);
        }

        break;
    }
    case IrCmd::MIN_NUM:
        inst.regX64 = regs.allocXmmRegOrReuse(index, {inst.a, inst.b});

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
        inst.regX64 = regs.allocXmmRegOrReuse(index, {inst.a, inst.b});

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
        inst.regX64 = regs.allocXmmRegOrReuse(index, {inst.a});

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
    case IrCmd::NOT_ANY:
    {
        // TODO: if we have a single user which is a STORE_INT, we are missing the opportunity to write directly to target
        inst.regX64 = regs.allocGprRegOrReuse(SizeX64::dword, index, {inst.a, inst.b});

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
        jumpOnAnyCmpFallback(build, vmRegOp(inst.a), vmRegOp(inst.b), conditionOp(inst.c), labelOp(inst.d));
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
        inst.regX64 = regs.allocXmmReg();

        build.mov(rArg1, regOp(inst.a));
        build.call(qword[rNativeContext + offsetof(NativeContext, luaH_getn)]);
        build.vcvtsi2sd(inst.regX64, inst.regX64, eax);
        break;
    case IrCmd::NEW_TABLE:
        inst.regX64 = regs.allocGprReg(SizeX64::qword);

        build.mov(rArg1, rState);
        build.mov(dwordReg(rArg2), uintOp(inst.a));
        build.mov(dwordReg(rArg3), uintOp(inst.b));
        build.call(qword[rNativeContext + offsetof(NativeContext, luaH_new)]);

        if (inst.regX64 != rax)
            build.mov(inst.regX64, rax);
        break;
    case IrCmd::DUP_TABLE:
        inst.regX64 = regs.allocGprReg(SizeX64::qword);

        // Re-ordered to avoid register conflict
        build.mov(rArg2, regOp(inst.a));
        build.mov(rArg1, rState);
        build.call(qword[rNativeContext + offsetof(NativeContext, luaH_clone)]);

        if (inst.regX64 != rax)
            build.mov(inst.regX64, rax);
        break;
    case IrCmd::TRY_NUM_TO_INDEX:
    {
        inst.regX64 = regs.allocGprReg(SizeX64::dword);

        ScopedRegX64 tmp{regs, SizeX64::xmmword};

        convertNumberToIndexOrJump(build, tmp.reg, regOp(inst.a), inst.regX64, labelOp(inst.b));
        break;
    }
    case IrCmd::TRY_CALL_FASTGETTM:
    {
        inst.regX64 = regs.allocGprReg(SizeX64::qword);

        callGetFastTmOrFallback(build, regOp(inst.a), TMS(intOp(inst.b)), labelOp(inst.c));

        if (inst.regX64 != rax)
            build.mov(inst.regX64, rax);
        break;
    }
    case IrCmd::INT_TO_NUM:
        inst.regX64 = regs.allocXmmReg();

        build.vcvtsi2sd(inst.regX64, inst.regX64, regOp(inst.a));
        break;
    case IrCmd::ADJUST_STACK_TO_REG:
    {
        if (inst.b.kind == IrOpKind::Constant)
        {
            ScopedRegX64 tmp{regs, SizeX64::qword};

            build.lea(tmp.reg, addr[rBase + (vmRegOp(inst.a) + intOp(inst.b)) * sizeof(TValue)]);
            build.mov(qword[rState + offsetof(lua_State, top)], tmp.reg);
        }
        else if (inst.b.kind == IrOpKind::Inst)
        {
            ScopedRegX64 tmp(regs, regs.allocGprRegOrReuse(SizeX64::dword, index, {inst.b}));

            build.shl(qwordReg(tmp.reg), kTValueSizeLog2);
            build.lea(qwordReg(tmp.reg), addr[rBase + qwordReg(tmp.reg) + vmRegOp(inst.a) * sizeof(TValue)]);
            build.mov(qword[rState + offsetof(lua_State, top)], qwordReg(tmp.reg));
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
        emitBuiltin(regs, build, uintOp(inst.a), vmRegOp(inst.b), vmRegOp(inst.c), inst.d, intOp(inst.e), intOp(inst.f));
        break;
    case IrCmd::INVOKE_FASTCALL:
    {
        unsigned bfid = uintOp(inst.a);

        OperandX64 args = 0;

        if (inst.d.kind == IrOpKind::VmReg)
            args = luauRegAddress(vmRegOp(inst.d));
        else if (inst.d.kind == IrOpKind::VmConst)
            args = luauConstantAddress(vmConstOp(inst.d));
        else
            LUAU_ASSERT(boolOp(inst.d) == false);

        int ra = vmRegOp(inst.b);
        int arg = vmRegOp(inst.c);
        int nparams = intOp(inst.e);
        int nresults = intOp(inst.f);

        regs.assertAllFree();

        build.mov(rax, qword[rNativeContext + offsetof(NativeContext, luauF_table) + bfid * sizeof(luau_FastFunction)]);

        // 5th parameter (args) is left unset for LOP_FASTCALL1
        if (args.cat == CategoryX64::mem)
        {
            if (build.abi == ABIX64::Windows)
            {
                build.lea(rcx, args);
                build.mov(sArg5, rcx);
            }
            else
            {
                build.lea(rArg5, args);
            }
        }

        if (nparams == LUA_MULTRET)
        {
            // L->top - (ra + 1)
            RegisterX64 reg = (build.abi == ABIX64::Windows) ? rcx : rArg6;
            build.mov(reg, qword[rState + offsetof(lua_State, top)]);
            build.lea(rdx, addr[rBase + (ra + 1) * sizeof(TValue)]);
            build.sub(reg, rdx);
            build.shr(reg, kTValueSizeLog2);

            if (build.abi == ABIX64::Windows)
                build.mov(sArg6, reg);
        }
        else
        {
            if (build.abi == ABIX64::Windows)
                build.mov(sArg6, nparams);
            else
                build.mov(rArg6, nparams);
        }

        build.mov(rArg1, rState);
        build.lea(rArg2, luauRegAddress(ra));
        build.lea(rArg3, luauRegAddress(arg));
        build.mov(dwordReg(rArg4), nresults);

        build.call(rax);

        inst.regX64 = regs.takeGprReg(eax); // Result of a builtin call is returned in eax
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
            callArithHelper(build, vmRegOp(inst.a), vmRegOp(inst.b), luauRegAddress(vmRegOp(inst.c)), TMS(intOp(inst.d)));
        else
            callArithHelper(build, vmRegOp(inst.a), vmRegOp(inst.b), luauConstantAddress(vmConstOp(inst.c)), TMS(intOp(inst.d)));
        break;
    case IrCmd::DO_LEN:
        callLengthHelper(build, vmRegOp(inst.a), vmRegOp(inst.b));
        break;
    case IrCmd::GET_TABLE:
        if (inst.c.kind == IrOpKind::VmReg)
        {
            callGetTable(build, vmRegOp(inst.b), luauRegAddress(vmRegOp(inst.c)), vmRegOp(inst.a));
        }
        else if (inst.c.kind == IrOpKind::Constant)
        {
            TValue n;
            setnvalue(&n, uintOp(inst.c));
            callGetTable(build, vmRegOp(inst.b), build.bytes(&n, sizeof(n)), vmRegOp(inst.a));
        }
        else
        {
            LUAU_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::SET_TABLE:
        if (inst.c.kind == IrOpKind::VmReg)
        {
            callSetTable(build, vmRegOp(inst.b), luauRegAddress(vmRegOp(inst.c)), vmRegOp(inst.a));
        }
        else if (inst.c.kind == IrOpKind::Constant)
        {
            TValue n;
            setnvalue(&n, uintOp(inst.c));
            callSetTable(build, vmRegOp(inst.b), build.bytes(&n, sizeof(n)), vmRegOp(inst.a));
        }
        else
        {
            LUAU_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::GET_IMPORT:
        emitInstGetImportFallback(build, vmRegOp(inst.a), uintOp(inst.b));
        break;
    case IrCmd::CONCAT:
        build.mov(rArg1, rState);
        build.mov(dwordReg(rArg2), uintOp(inst.b));
        build.mov(dwordReg(rArg3), vmRegOp(inst.a) + uintOp(inst.b) - 1);
        build.call(qword[rNativeContext + offsetof(NativeContext, luaV_concat)]);

        emitUpdateBase(build);
        break;
    case IrCmd::GET_UPVALUE:
    {
        ScopedRegX64 tmp1{regs, SizeX64::qword};
        ScopedRegX64 tmp2{regs, SizeX64::xmmword};

        build.mov(tmp1.reg, sClosure);
        build.add(tmp1.reg, offsetof(Closure, l.uprefs) + sizeof(TValue) * vmUpvalueOp(inst.b));

        // uprefs[] is either an actual value, or it points to UpVal object which has a pointer to value
        Label skip;
        // TODO: jumpIfTagIsNot can be generalized to take OperandX64 and then we can use it here; let's wait until we see this more though
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
        Label next;
        ScopedRegX64 tmp1{regs, SizeX64::qword};
        ScopedRegX64 tmp2{regs, SizeX64::qword};
        ScopedRegX64 tmp3{regs, SizeX64::xmmword};

        build.mov(tmp1.reg, sClosure);
        build.mov(tmp2.reg, qword[tmp1.reg + offsetof(Closure, l.uprefs) + sizeof(TValue) * vmUpvalueOp(inst.a) + offsetof(TValue, value.gc)]);

        build.mov(tmp1.reg, qword[tmp2.reg + offsetof(UpVal, v)]);
        build.vmovups(tmp3.reg, luauReg(vmRegOp(inst.b)));
        build.vmovups(xmmword[tmp1.reg], tmp3.reg);

        callBarrierObject(build, tmp1.reg, tmp2.reg, vmRegOp(inst.b), next);
        build.setLabel(next);
        break;
    }
    case IrCmd::PREPARE_FORN:
        callPrepareForN(build, vmRegOp(inst.a), vmRegOp(inst.b), vmRegOp(inst.c));
        break;
    case IrCmd::CHECK_TAG:
        if (inst.a.kind == IrOpKind::Inst)
        {
            build.cmp(regOp(inst.a), tagOp(inst.b));
            build.jcc(ConditionX64::NotEqual, labelOp(inst.c));
        }
        else if (inst.a.kind == IrOpKind::VmReg)
        {
            jumpIfTagIsNot(build, vmRegOp(inst.a), lua_Type(tagOp(inst.b)), labelOp(inst.c));
        }
        else if (inst.a.kind == IrOpKind::VmConst)
        {
            build.cmp(luauConstantTag(vmConstOp(inst.a)), tagOp(inst.b));
            build.jcc(ConditionX64::NotEqual, labelOp(inst.c));
        }
        else
        {
            LUAU_ASSERT(!"Unsupported instruction form");
        }
        break;
    case IrCmd::CHECK_READONLY:
        jumpIfTableIsReadOnly(build, regOp(inst.a), labelOp(inst.b));
        break;
    case IrCmd::CHECK_NO_METATABLE:
        jumpIfMetatablePresent(build, regOp(inst.a), labelOp(inst.b));
        break;
    case IrCmd::CHECK_SAFE_ENV:
    {
        ScopedRegX64 tmp{regs, SizeX64::qword};

        jumpIfUnsafeEnv(build, tmp.reg, labelOp(inst.a));
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
        jumpIfNodeHasNext(build, regOp(inst.a), labelOp(inst.b));
        break;
    case IrCmd::INTERRUPT:
        emitInterrupt(build, uintOp(inst.a));
        break;
    case IrCmd::CHECK_GC:
    {
        Label skip;
        callCheckGc(build, -1, false, skip);
        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_OBJ:
    {
        Label skip;
        ScopedRegX64 tmp{regs, SizeX64::qword};

        callBarrierObject(build, tmp.reg, regOp(inst.a), vmRegOp(inst.b), skip);
        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_TABLE_BACK:
    {
        Label skip;

        callBarrierTableFast(build, regOp(inst.a), skip);
        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_TABLE_FORWARD:
    {
        Label skip;
        ScopedRegX64 tmp{regs, SizeX64::qword};

        callBarrierTable(build, tmp.reg, regOp(inst.a), vmRegOp(inst.b), skip);
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

        if (rArg2 != tmp2.reg)
            build.mov(rArg2, tmp2.reg);

        build.mov(rArg1, rState);
        build.call(qword[rNativeContext + offsetof(NativeContext, luaF_close)]);

        build.setLabel(next);
        break;
    }
    case IrCmd::CAPTURE:
        // No-op right now
        break;

        // Fallbacks to non-IR instruction implementations
    case IrCmd::LOP_SETLIST:
    {
        Label next;
        emitInstSetList(build, next, vmRegOp(inst.b), vmRegOp(inst.c), intOp(inst.d), uintOp(inst.e));
        build.setLabel(next);
        break;
    }
    case IrCmd::LOP_CALL:
        emitInstCall(build, helpers, vmRegOp(inst.a), intOp(inst.b), intOp(inst.c));
        break;
    case IrCmd::LOP_RETURN:
        emitInstReturn(build, helpers, vmRegOp(inst.a), intOp(inst.b));
        break;
    case IrCmd::LOP_FORGLOOP:
        emitinstForGLoop(build, vmRegOp(inst.a), intOp(inst.b), labelOp(inst.c), labelOp(inst.d));
        break;
    case IrCmd::LOP_FORGLOOP_FALLBACK:
        emitinstForGLoopFallback(build, uintOp(inst.a), vmRegOp(inst.b), intOp(inst.c), labelOp(inst.d));
        build.jmp(labelOp(inst.e));
        break;
    case IrCmd::LOP_FORGPREP_XNEXT_FALLBACK:
        emitInstForGPrepXnextFallback(build, uintOp(inst.a), vmRegOp(inst.b), labelOp(inst.c));
        break;
    case IrCmd::LOP_AND:
        emitInstAnd(build, vmRegOp(inst.a), vmRegOp(inst.b), vmRegOp(inst.c));
        break;
    case IrCmd::LOP_ANDK:
        emitInstAndK(build, vmRegOp(inst.a), vmRegOp(inst.b), vmConstOp(inst.c));
        break;
    case IrCmd::LOP_OR:
        emitInstOr(build, vmRegOp(inst.a), vmRegOp(inst.b), vmRegOp(inst.c));
        break;
    case IrCmd::LOP_ORK:
        emitInstOrK(build, vmRegOp(inst.a), vmRegOp(inst.b), vmConstOp(inst.c));
        break;
    case IrCmd::LOP_COVERAGE:
        emitInstCoverage(build, uintOp(inst.a));
        break;

        // Full instruction fallbacks
    case IrCmd::FALLBACK_GETGLOBAL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        emitFallback(build, data, LOP_GETGLOBAL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETGLOBAL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        emitFallback(build, data, LOP_SETGLOBAL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETTABLEKS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitFallback(build, data, LOP_GETTABLEKS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETTABLEKS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitFallback(build, data, LOP_SETTABLEKS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_NAMECALL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        emitFallback(build, data, LOP_NAMECALL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_PREPVARARGS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::Constant);

        emitFallback(build, data, LOP_PREPVARARGS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETVARARGS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);

        emitFallback(build, data, LOP_GETVARARGS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_NEWCLOSURE:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);

        emitFallback(build, data, LOP_NEWCLOSURE, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_DUPCLOSURE:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        emitFallback(build, data, LOP_DUPCLOSURE, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_FORGPREP:
        emitFallback(build, data, LOP_FORGPREP, uintOp(inst.a));
        break;
    default:
        LUAU_ASSERT(!"Not supported yet");
        break;
    }

    regs.freeLastUseRegs(inst, index);
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

OperandX64 IrLoweringX64::memRegDoubleOp(IrOp op) const
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

OperandX64 IrLoweringX64::memRegTagOp(IrOp op) const
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

RegisterX64 IrLoweringX64::regOp(IrOp op) const
{
    IrInst& inst = function.instOp(op);
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
