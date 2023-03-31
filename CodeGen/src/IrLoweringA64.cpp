// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrLoweringA64.h"

#include "Luau/CodeGen.h"
#include "Luau/DenseHash.h"
#include "Luau/IrAnalysis.h"
#include "Luau/IrDump.h"
#include "Luau/IrUtils.h"

#include "EmitCommonA64.h"
#include "EmitInstructionA64.h"
#include "NativeState.h"

#include "lstate.h"

// TODO: Eventually this can go away
// #define TRACE

namespace Luau
{
namespace CodeGen
{
namespace A64
{

#ifdef TRACE
struct LoweringStatsA64
{
    size_t can;
    size_t total;

    ~LoweringStatsA64()
    {
        if (total)
            printf("A64 lowering succeded for %.1f%% functions (%d/%d)\n", double(can) / double(total) * 100, int(can), int(total));
    }
} gStatsA64;
#endif

inline ConditionA64 getConditionFP(IrCondition cond)
{
    switch (cond)
    {
    case IrCondition::Equal:
        return ConditionA64::Equal;

    case IrCondition::NotEqual:
        return ConditionA64::NotEqual;

    case IrCondition::Less:
        return ConditionA64::Minus;

    case IrCondition::NotLess:
        return ConditionA64::Plus;

    case IrCondition::LessEqual:
        return ConditionA64::UnsignedLessEqual;

    case IrCondition::NotLessEqual:
        return ConditionA64::UnsignedGreater;

    case IrCondition::Greater:
        return ConditionA64::Greater;

    case IrCondition::NotGreater:
        return ConditionA64::LessEqual;

    case IrCondition::GreaterEqual:
        return ConditionA64::GreaterEqual;

    case IrCondition::NotGreaterEqual:
        return ConditionA64::Less;

    default:
        LUAU_ASSERT(!"Unexpected condition code");
        return ConditionA64::Always;
    }
}

IrLoweringA64::IrLoweringA64(AssemblyBuilderA64& build, ModuleHelpers& helpers, NativeState& data, Proto* proto, IrFunction& function)
    : build(build)
    , helpers(helpers)
    , data(data)
    , proto(proto)
    , function(function)
    , regs(function, {{x0, x15}, {q0, q7}, {q16, q31}})
{
    // In order to allocate registers during lowering, we need to know where instruction results are last used
    updateLastUseLocations(function);
}

// TODO: Eventually this can go away
bool IrLoweringA64::canLower(const IrFunction& function)
{
#ifdef TRACE
    gStatsA64.total++;
#endif

    for (const IrInst& inst : function.instructions)
    {
        switch (inst.cmd)
        {
        case IrCmd::NOP:
        case IrCmd::LOAD_TAG:
        case IrCmd::LOAD_POINTER:
        case IrCmd::LOAD_DOUBLE:
        case IrCmd::LOAD_INT:
        case IrCmd::LOAD_TVALUE:
        case IrCmd::LOAD_NODE_VALUE_TV:
        case IrCmd::LOAD_ENV:
        case IrCmd::STORE_TAG:
        case IrCmd::STORE_POINTER:
        case IrCmd::STORE_DOUBLE:
        case IrCmd::STORE_INT:
        case IrCmd::STORE_TVALUE:
        case IrCmd::STORE_NODE_VALUE_TV:
        case IrCmd::ADD_NUM:
        case IrCmd::SUB_NUM:
        case IrCmd::MUL_NUM:
        case IrCmd::DIV_NUM:
        case IrCmd::MOD_NUM:
        case IrCmd::UNM_NUM:
        case IrCmd::JUMP:
        case IrCmd::JUMP_EQ_TAG:
        case IrCmd::JUMP_CMP_NUM:
        case IrCmd::JUMP_CMP_ANY:
        case IrCmd::DO_ARITH:
        case IrCmd::GET_IMPORT:
        case IrCmd::GET_UPVALUE:
        case IrCmd::CHECK_TAG:
        case IrCmd::CHECK_READONLY:
        case IrCmd::CHECK_NO_METATABLE:
        case IrCmd::CHECK_SAFE_ENV:
        case IrCmd::INTERRUPT:
        case IrCmd::SET_SAVEDPC:
        case IrCmd::CALL:
        case IrCmd::RETURN:
        case IrCmd::SUBSTITUTE:
            continue;

        default:
            return false;
        }
    }

#ifdef TRACE
    gStatsA64.can++;
#endif

    return true;
}

void IrLoweringA64::lowerInst(IrInst& inst, uint32_t index, IrBlock& next)
{
    switch (inst.cmd)
    {
    case IrCmd::LOAD_TAG:
    {
        inst.regA64 = regs.allocReg(KindA64::w);
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, tt));
        build.ldr(inst.regA64, addr);
        break;
    }
    case IrCmd::LOAD_POINTER:
    {
        inst.regA64 = regs.allocReg(KindA64::x);
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value));
        build.ldr(inst.regA64, addr);
        break;
    }
    case IrCmd::LOAD_DOUBLE:
    {
        inst.regA64 = regs.allocReg(KindA64::d);
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value));
        build.ldr(inst.regA64, addr);
        break;
    }
    case IrCmd::LOAD_INT:
    {
        inst.regA64 = regs.allocReg(KindA64::w);
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value));
        build.ldr(inst.regA64, addr);
        break;
    }
    case IrCmd::LOAD_TVALUE:
    {
        inst.regA64 = regs.allocReg(KindA64::q);
        AddressA64 addr = tempAddr(inst.a, 0);
        build.ldr(inst.regA64, addr);
        break;
    }
    case IrCmd::LOAD_NODE_VALUE_TV:
    {
        inst.regA64 = regs.allocReg(KindA64::q);
        build.ldr(inst.regA64, mem(regOp(inst.a), offsetof(LuaNode, val)));
        break;
    }
    case IrCmd::LOAD_ENV:
        inst.regA64 = regs.allocReg(KindA64::x);
        build.ldr(inst.regA64, mem(rClosure, offsetof(Closure, env)));
        break;
    case IrCmd::STORE_TAG:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::w);
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, tt));
        build.mov(temp, tagOp(inst.b));
        build.str(temp, addr);
        break;
    }
    case IrCmd::STORE_POINTER:
    {
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value));
        build.str(regOp(inst.b), addr);
        break;
    }
    case IrCmd::STORE_DOUBLE:
    {
        RegisterA64 temp = tempDouble(inst.b);
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value));
        build.str(temp, addr);
        break;
    }
    case IrCmd::STORE_INT:
    {
        RegisterA64 temp = tempInt(inst.b);
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value));
        build.str(temp, addr);
        break;
    }
    case IrCmd::STORE_TVALUE:
    {
        AddressA64 addr = tempAddr(inst.a, 0);
        build.str(regOp(inst.b), addr);
        break;
    }
    case IrCmd::STORE_NODE_VALUE_TV:
        build.str(regOp(inst.b), mem(regOp(inst.a), offsetof(LuaNode, val)));
        break;
    case IrCmd::ADD_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a, inst.b});
        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);
        build.fadd(inst.regA64, temp1, temp2);
        break;
    }
    case IrCmd::SUB_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a, inst.b});
        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);
        build.fsub(inst.regA64, temp1, temp2);
        break;
    }
    case IrCmd::MUL_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a, inst.b});
        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);
        build.fmul(inst.regA64, temp1, temp2);
        break;
    }
    case IrCmd::DIV_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a, inst.b});
        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);
        build.fdiv(inst.regA64, temp1, temp2);
        break;
    }
    case IrCmd::MOD_NUM:
    {
        inst.regA64 = regs.allocReg(KindA64::d);
        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);
        build.fdiv(inst.regA64, temp1, temp2);
        build.frintm(inst.regA64, inst.regA64);
        build.fmul(inst.regA64, inst.regA64, temp2);
        build.fsub(inst.regA64, temp1, inst.regA64);
        break;
    }
    case IrCmd::UNM_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a});
        RegisterA64 temp = tempDouble(inst.a);
        build.fneg(inst.regA64, temp);
        break;
    }
    case IrCmd::JUMP:
        jumpOrFallthrough(blockOp(inst.a), next);
        break;
    case IrCmd::JUMP_EQ_TAG:
        if (inst.b.kind == IrOpKind::Constant)
            build.cmp(regOp(inst.a), tagOp(inst.b));
        else if (inst.b.kind == IrOpKind::Inst)
            build.cmp(regOp(inst.a), regOp(inst.b));
        else
            LUAU_ASSERT(!"Unsupported instruction form");

        if (isFallthroughBlock(blockOp(inst.d), next))
        {
            build.b(ConditionA64::Equal, labelOp(inst.c));
            jumpOrFallthrough(blockOp(inst.d), next);
        }
        else
        {
            build.b(ConditionA64::NotEqual, labelOp(inst.d));
            jumpOrFallthrough(blockOp(inst.c), next);
        }
        break;
    case IrCmd::JUMP_CMP_NUM:
    {
        IrCondition cond = conditionOp(inst.c);

        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);

        build.fcmp(temp1, temp2);
        build.b(getConditionFP(cond), labelOp(inst.d));
        jumpOrFallthrough(blockOp(inst.e), next);
        break;
    }
    case IrCmd::JUMP_CMP_ANY:
    {
        IrCondition cond = conditionOp(inst.c);

        regs.assertAllFree();
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.add(x2, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));

        if (cond == IrCondition::NotLessEqual || cond == IrCondition::LessEqual)
            build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaV_lessequal)));
        else if (cond == IrCondition::NotLess || cond == IrCondition::Less)
            build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaV_lessthan)));
        else if (cond == IrCondition::NotEqual || cond == IrCondition::Equal)
            build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaV_equalval)));
        else
            LUAU_ASSERT(!"Unsupported condition");

        build.blr(x3);

        emitUpdateBase(build);

        if (cond == IrCondition::NotLessEqual || cond == IrCondition::NotLess || cond == IrCondition::NotEqual)
            build.cbz(x0, labelOp(inst.d));
        else
            build.cbnz(x0, labelOp(inst.d));
        jumpOrFallthrough(blockOp(inst.e), next);
        break;
    }
    case IrCmd::DO_ARITH:
        regs.assertAllFree();
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.add(x2, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));

        if (inst.c.kind == IrOpKind::VmConst)
        {
            // TODO: refactor into a common helper
            if (vmConstOp(inst.c) * sizeof(TValue) <= AssemblyBuilderA64::kMaxImmediate)
            {
                build.add(x3, rConstants, uint16_t(vmConstOp(inst.c) * sizeof(TValue)));
            }
            else
            {
                build.mov(x3, vmConstOp(inst.c) * sizeof(TValue));
                build.add(x3, rConstants, x3);
            }
        }
        else
            build.add(x3, rBase, uint16_t(vmRegOp(inst.c) * sizeof(TValue)));

        build.mov(w4, TMS(intOp(inst.d)));
        build.ldr(x5, mem(rNativeContext, offsetof(NativeContext, luaV_doarith)));
        build.blr(x5);

        emitUpdateBase(build);
        break;
    case IrCmd::GET_IMPORT:
        regs.assertAllFree();
        emitInstGetImport(build, vmRegOp(inst.a), uintOp(inst.b));
        break;
    case IrCmd::GET_UPVALUE:
    {
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::q);
        RegisterA64 temp3 = regs.allocTemp(KindA64::w);

        build.add(temp1, rClosure, uint16_t(offsetof(Closure, l.uprefs) + sizeof(TValue) * vmUpvalueOp(inst.b)));

        // uprefs[] is either an actual value, or it points to UpVal object which has a pointer to value
        Label skip;
        build.ldr(temp3, mem(temp1, offsetof(TValue, tt)));
        build.cmp(temp3, LUA_TUPVAL);
        build.b(ConditionA64::NotEqual, skip);

        // UpVal.v points to the value (either on stack, or on heap inside each UpVal, but we can deref it unconditionally)
        build.ldr(temp1, mem(temp1, offsetof(TValue, value.gc)));
        build.ldr(temp1, mem(temp1, offsetof(UpVal, v)));

        build.setLabel(skip);

        build.ldr(temp2, temp1);
        build.str(temp2, mem(rBase, vmRegOp(inst.a) * sizeof(TValue)));
        break;
    }
    case IrCmd::CHECK_TAG:
        build.cmp(regOp(inst.a), tagOp(inst.b));
        build.b(ConditionA64::NotEqual, labelOp(inst.c));
        break;
    case IrCmd::CHECK_READONLY:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.ldrb(temp, mem(regOp(inst.a), offsetof(Table, readonly)));
        build.cbnz(temp, labelOp(inst.b));
        break;
    }
    case IrCmd::CHECK_NO_METATABLE:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::x);
        build.ldr(temp, mem(regOp(inst.a), offsetof(Table, metatable)));
        build.cbnz(temp, labelOp(inst.b));
        break;
    }
    case IrCmd::CHECK_SAFE_ENV:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::x);
        RegisterA64 tempw{KindA64::w, temp.index};
        build.ldr(temp, mem(rClosure, offsetof(Closure, env)));
        build.ldrb(tempw, mem(temp, offsetof(Table, safeenv)));
        build.cbz(tempw, labelOp(inst.a));
        break;
    }
    case IrCmd::INTERRUPT:
    {
        unsigned int pcpos = uintOp(inst.a);
        regs.assertAllFree();

        Label skip;
        build.ldr(x2, mem(rState, offsetof(lua_State, global)));
        build.ldr(x2, mem(x2, offsetof(global_State, cb.interrupt)));
        build.cbz(x2, skip);

        // Jump to outlined interrupt handler, it will give back control to x1
        build.mov(x0, (pcpos + 1) * sizeof(Instruction));
        build.adr(x1, skip);
        build.b(helpers.interrupt);

        build.setLabel(skip);
        break;
    }
    case IrCmd::SET_SAVEDPC:
    {
        unsigned int pcpos = uintOp(inst.a);
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::x);

        // TODO: refactor into a common helper
        if (pcpos * sizeof(Instruction) <= AssemblyBuilderA64::kMaxImmediate)
        {
            build.add(temp1, rCode, uint16_t(pcpos * sizeof(Instruction)));
        }
        else
        {
            build.mov(temp1, pcpos * sizeof(Instruction));
            build.add(temp1, rCode, temp1);
        }

        build.ldr(temp2, mem(rState, offsetof(lua_State, ci)));
        build.str(temp1, mem(temp2, offsetof(CallInfo, savedpc)));
        break;
    }
    case IrCmd::CALL:
        regs.assertAllFree();
        emitInstCall(build, helpers, vmRegOp(inst.a), intOp(inst.b), intOp(inst.c));
        break;
    case IrCmd::RETURN:
        regs.assertAllFree();
        emitInstReturn(build, helpers, vmRegOp(inst.a), intOp(inst.b));
        break;
    default:
        LUAU_ASSERT(!"Not supported yet");
        break;
    }

    regs.freeLastUseRegs(inst, index);
    regs.freeTempRegs();
}

bool IrLoweringA64::isFallthroughBlock(IrBlock target, IrBlock next)
{
    return target.start == next.start;
}

void IrLoweringA64::jumpOrFallthrough(IrBlock& target, IrBlock& next)
{
    if (!isFallthroughBlock(target, next))
        build.b(target.label);
}

RegisterA64 IrLoweringA64::tempDouble(IrOp op)
{
    if (op.kind == IrOpKind::Inst)
        return regOp(op);
    else if (op.kind == IrOpKind::Constant)
    {
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::d);
        build.adr(temp1, doubleOp(op));
        build.ldr(temp2, temp1);
        return temp2;
    }
    else
    {
        LUAU_ASSERT(!"Unsupported instruction form");
        return noreg;
    }
}

RegisterA64 IrLoweringA64::tempInt(IrOp op)
{
    if (op.kind == IrOpKind::Inst)
        return regOp(op);
    else if (op.kind == IrOpKind::Constant)
    {
        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.mov(temp, intOp(op));
        return temp;
    }
    else
    {
        LUAU_ASSERT(!"Unsupported instruction form");
        return noreg;
    }
}

AddressA64 IrLoweringA64::tempAddr(IrOp op, int offset)
{
    // This is needed to tighten the bounds checks in the VmConst case below
    LUAU_ASSERT(offset % 4 == 0);

    if (op.kind == IrOpKind::VmReg)
        return mem(rBase, vmRegOp(op) * sizeof(TValue) + offset);
    else if (op.kind == IrOpKind::VmConst)
    {
        size_t constantOffset = vmConstOp(op) * sizeof(TValue) + offset;

        // Note: cumulative offset is guaranteed to be divisible by 4; we can use that to expand the useful range that doesn't require temporaries
        if (constantOffset / 4 <= AddressA64::kMaxOffset)
            return mem(rConstants, int(constantOffset));

        RegisterA64 temp = regs.allocTemp(KindA64::x);

        // TODO: refactor into a common helper
        if (constantOffset <= AssemblyBuilderA64::kMaxImmediate)
        {
            build.add(temp, rConstants, uint16_t(constantOffset));
        }
        else
        {
            build.mov(temp, int(constantOffset));
            build.add(temp, rConstants, temp);
        }

        return temp;
    }
    // If we have a register, we assume it's a pointer to TValue
    // We might introduce explicit operand types in the future to make this more robust
    else if (op.kind == IrOpKind::Inst)
        return mem(regOp(op), offset);
    else
    {
        LUAU_ASSERT(!"Unsupported instruction form");
        return noreg;
    }
}

RegisterA64 IrLoweringA64::regOp(IrOp op) const
{
    IrInst& inst = function.instOp(op);
    LUAU_ASSERT(inst.regA64 != noreg);
    return inst.regA64;
}

IrConst IrLoweringA64::constOp(IrOp op) const
{
    return function.constOp(op);
}

uint8_t IrLoweringA64::tagOp(IrOp op) const
{
    return function.tagOp(op);
}

bool IrLoweringA64::boolOp(IrOp op) const
{
    return function.boolOp(op);
}

int IrLoweringA64::intOp(IrOp op) const
{
    return function.intOp(op);
}

unsigned IrLoweringA64::uintOp(IrOp op) const
{
    return function.uintOp(op);
}

double IrLoweringA64::doubleOp(IrOp op) const
{
    return function.doubleOp(op);
}

IrBlock& IrLoweringA64::blockOp(IrOp op) const
{
    return function.blockOp(op);
}

Label& IrLoweringA64::labelOp(IrOp op) const
{
    return blockOp(op).label;
}

} // namespace A64
} // namespace CodeGen
} // namespace Luau
