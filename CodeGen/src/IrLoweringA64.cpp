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
#include "lgc.h"

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
            printf("A64 lowering succeeded for %.1f%% functions (%d/%d)\n", double(can) / double(total) * 100, int(can), int(total));
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

// TODO: instead of temp1/temp2 we can take a register that we will use for ra->value; that way callers to this function will be able to use it when
// calling luaC_barrier*
static void checkObjectBarrierConditions(AssemblyBuilderA64& build, RegisterA64 object, RegisterA64 temp1, RegisterA64 temp2, int ra, Label& skip)
{
    RegisterA64 temp1w = castReg(KindA64::w, temp1);
    RegisterA64 temp2w = castReg(KindA64::w, temp2);

    // iscollectable(ra)
    build.ldr(temp1w, mem(rBase, ra * sizeof(TValue) + offsetof(TValue, tt)));
    build.cmp(temp1w, LUA_TSTRING);
    build.b(ConditionA64::Less, skip);

    // isblack(obj2gco(o))
    // TODO: conditional bit test with BLACKBIT
    build.ldrb(temp1w, mem(object, offsetof(GCheader, marked)));
    build.mov(temp2w, bitmask(BLACKBIT));
    build.and_(temp1w, temp1w, temp2w);
    build.cbz(temp1w, skip);

    // iswhite(gcvalue(ra))
    // TODO: tst with bitmask(WHITE0BIT, WHITE1BIT)
    build.ldr(temp1, mem(rBase, ra * sizeof(TValue) + offsetof(TValue, value)));
    build.ldrb(temp1w, mem(temp1, offsetof(GCheader, marked)));
    build.mov(temp2w, bit2mask(WHITE0BIT, WHITE1BIT));
    build.and_(temp1w, temp1w, temp2w);
    build.cbz(temp1w, skip);
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
        case IrCmd::GET_ARR_ADDR:
        case IrCmd::GET_SLOT_NODE_ADDR:
        case IrCmd::GET_HASH_NODE_ADDR:
        case IrCmd::STORE_TAG:
        case IrCmd::STORE_POINTER:
        case IrCmd::STORE_DOUBLE:
        case IrCmd::STORE_INT:
        case IrCmd::STORE_TVALUE:
        case IrCmd::STORE_NODE_VALUE_TV:
        case IrCmd::ADD_INT:
        case IrCmd::SUB_INT:
        case IrCmd::ADD_NUM:
        case IrCmd::SUB_NUM:
        case IrCmd::MUL_NUM:
        case IrCmd::DIV_NUM:
        case IrCmd::MOD_NUM:
        case IrCmd::POW_NUM:
        case IrCmd::MIN_NUM:
        case IrCmd::MAX_NUM:
        case IrCmd::UNM_NUM:
        case IrCmd::FLOOR_NUM:
        case IrCmd::CEIL_NUM:
        case IrCmd::ROUND_NUM:
        case IrCmd::SQRT_NUM:
        case IrCmd::ABS_NUM:
        case IrCmd::JUMP:
        case IrCmd::JUMP_IF_TRUTHY:
        case IrCmd::JUMP_IF_FALSY:
        case IrCmd::JUMP_EQ_TAG:
        case IrCmd::JUMP_EQ_INT:
        case IrCmd::JUMP_EQ_POINTER:
        case IrCmd::JUMP_CMP_NUM:
        case IrCmd::JUMP_CMP_ANY:
        case IrCmd::TABLE_LEN:
        case IrCmd::NEW_TABLE:
        case IrCmd::DUP_TABLE:
        case IrCmd::TRY_NUM_TO_INDEX:
        case IrCmd::INT_TO_NUM:
        case IrCmd::ADJUST_STACK_TO_REG:
        case IrCmd::ADJUST_STACK_TO_TOP:
        case IrCmd::INVOKE_FASTCALL:
        case IrCmd::CHECK_FASTCALL_RES:
        case IrCmd::DO_ARITH:
        case IrCmd::DO_LEN:
        case IrCmd::GET_TABLE:
        case IrCmd::SET_TABLE:
        case IrCmd::GET_IMPORT:
        case IrCmd::CONCAT:
        case IrCmd::GET_UPVALUE:
        case IrCmd::SET_UPVALUE:
        case IrCmd::PREPARE_FORN:
        case IrCmd::CHECK_TAG:
        case IrCmd::CHECK_READONLY:
        case IrCmd::CHECK_NO_METATABLE:
        case IrCmd::CHECK_SAFE_ENV:
        case IrCmd::CHECK_ARRAY_SIZE:
        case IrCmd::CHECK_SLOT_MATCH:
        case IrCmd::INTERRUPT:
        case IrCmd::CHECK_GC:
        case IrCmd::BARRIER_OBJ:
        case IrCmd::BARRIER_TABLE_BACK:
        case IrCmd::BARRIER_TABLE_FORWARD:
        case IrCmd::SET_SAVEDPC:
        case IrCmd::CLOSE_UPVALS:
        case IrCmd::CAPTURE:
        case IrCmd::CALL:
        case IrCmd::RETURN:
        case IrCmd::FALLBACK_GETGLOBAL:
        case IrCmd::FALLBACK_SETGLOBAL:
        case IrCmd::FALLBACK_GETTABLEKS:
        case IrCmd::FALLBACK_SETTABLEKS:
        case IrCmd::FALLBACK_NAMECALL:
        case IrCmd::FALLBACK_PREPVARARGS:
        case IrCmd::FALLBACK_GETVARARGS:
        case IrCmd::FALLBACK_NEWCLOSURE:
        case IrCmd::FALLBACK_DUPCLOSURE:
        case IrCmd::SUBSTITUTE:
            continue;

        default:
#ifdef TRACE
            printf("A64 lowering missing %s\n", getCmdName(inst.cmd));
#endif
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
    case IrCmd::GET_ARR_ADDR:
    {
        inst.regA64 = regs.allocReg(KindA64::x);
        build.ldr(inst.regA64, mem(regOp(inst.a), offsetof(Table, array)));

        if (inst.b.kind == IrOpKind::Inst)
        {
            // TODO: This is a temporary hack that reads wN register as if it was xN. This should use unsigned extension shift once we support it.
            build.add(inst.regA64, inst.regA64, castReg(KindA64::x, regOp(inst.b)), kTValueSizeLog2);
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            LUAU_ASSERT(size_t(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate >> kTValueSizeLog2); // TODO: handle out of range values
            build.add(inst.regA64, inst.regA64, uint16_t(intOp(inst.b) << kTValueSizeLog2));
        }
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    }
    case IrCmd::GET_SLOT_NODE_ADDR:
    {
        inst.regA64 = regs.allocReuse(KindA64::x, index, {inst.a});
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp1w = castReg(KindA64::w, temp1);
        RegisterA64 temp2 = regs.allocTemp(KindA64::w);

        // TODO: this can use a slightly more efficient sequence with a 4b load + and-with-right-shift for pcpos<1024 but we don't support it yet.
        build.mov(temp1, uintOp(inst.b) * sizeof(Instruction) + kOffsetOfInstructionC);
        build.ldrb(temp1w, mem(rCode, temp1));
        build.ldrb(temp2, mem(regOp(inst.a), offsetof(Table, nodemask8)));
        build.and_(temp2, temp2, temp1w);

        // note: this may clobber inst.a, so it's important that we don't use it after this
        build.ldr(inst.regA64, mem(regOp(inst.a), offsetof(Table, node)));
        // TODO: This is a temporary hack that reads wN register as if it was xN. This should use unsigned extension shift once we support it.
        build.add(inst.regA64, inst.regA64, castReg(KindA64::x, temp2), kLuaNodeSizeLog2);
        break;
    }
    case IrCmd::GET_HASH_NODE_ADDR:
    {
        inst.regA64 = regs.allocReuse(KindA64::x, index, {inst.a});
        RegisterA64 temp1 = regs.allocTemp(KindA64::w);
        RegisterA64 temp2 = regs.allocTemp(KindA64::w);

        // TODO: this can use bic (andnot) to do hash & ~(-1 << lsizenode) instead but we don't support it yet
        build.mov(temp1, 1);
        build.ldrb(temp2, mem(regOp(inst.a), offsetof(Table, lsizenode)));
        build.lsl(temp1, temp1, temp2);
        build.sub(temp1, temp1, 1);
        build.mov(temp2, uintOp(inst.b));
        build.and_(temp2, temp2, temp1);

        // note: this may clobber inst.a, so it's important that we don't use it after this
        build.ldr(inst.regA64, mem(regOp(inst.a), offsetof(Table, node)));
        // TODO: This is a temporary hack that reads wN register as if it was xN. This should use unsigned extension shift once we support it.
        build.add(inst.regA64, inst.regA64, castReg(KindA64::x, temp2), kLuaNodeSizeLog2);
        break;
    }
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
    case IrCmd::ADD_INT:
        LUAU_ASSERT(unsigned(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate); // TODO: handle out of range values
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a});
        build.add(inst.regA64, regOp(inst.a), uint16_t(intOp(inst.b)));
        break;
    case IrCmd::SUB_INT:
        LUAU_ASSERT(unsigned(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate); // TODO: handle out of range values
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a});
        build.sub(inst.regA64, regOp(inst.a), uint16_t(intOp(inst.b)));
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
        inst.regA64 = regs.allocReg(KindA64::d); // can't allocReuse because both A and B are used twice
        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);
        build.fdiv(inst.regA64, temp1, temp2);
        build.frintm(inst.regA64, inst.regA64);
        build.fmul(inst.regA64, inst.regA64, temp2);
        build.fsub(inst.regA64, temp1, inst.regA64);
        break;
    }
    case IrCmd::POW_NUM:
    {
        // TODO: this instruction clobbers all registers because of a call but it's unclear how to assert that cleanly atm
        inst.regA64 = regs.allocReg(KindA64::d);
        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);
        build.fmov(d0, temp1); // TODO: aliasing hazard
        build.fmov(d1, temp2); // TODO: aliasing hazard
        build.ldr(x0, mem(rNativeContext, offsetof(NativeContext, libm_pow)));
        build.blr(x0);
        build.fmov(inst.regA64, d0);
        break;
    }
    case IrCmd::MIN_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a, inst.b});
        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);
        build.fcmp(temp1, temp2);
        build.fcsel(inst.regA64, temp1, temp2, getConditionFP(IrCondition::Less));
        break;
    }
    case IrCmd::MAX_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a, inst.b});
        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);
        build.fcmp(temp1, temp2);
        build.fcsel(inst.regA64, temp1, temp2, getConditionFP(IrCondition::Greater));
        break;
    }
    case IrCmd::UNM_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a});
        RegisterA64 temp = tempDouble(inst.a);
        build.fneg(inst.regA64, temp);
        break;
    }
    case IrCmd::FLOOR_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a});
        RegisterA64 temp = tempDouble(inst.a);
        build.frintm(inst.regA64, temp);
        break;
    }
    case IrCmd::CEIL_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a});
        RegisterA64 temp = tempDouble(inst.a);
        build.frintp(inst.regA64, temp);
        break;
    }
    case IrCmd::ROUND_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a});
        RegisterA64 temp = tempDouble(inst.a);
        build.frinta(inst.regA64, temp);
        break;
    }
    case IrCmd::SQRT_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a});
        RegisterA64 temp = tempDouble(inst.a);
        build.fsqrt(inst.regA64, temp);
        break;
    }
    case IrCmd::ABS_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a});
        RegisterA64 temp = tempDouble(inst.a);
        build.fabs(inst.regA64, temp);
        break;
    }
    case IrCmd::JUMP:
        jumpOrFallthrough(blockOp(inst.a), next);
        break;
    case IrCmd::JUMP_IF_TRUTHY:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.ldr(temp, mem(rBase, vmRegOp(inst.a) * sizeof(TValue) + offsetof(TValue, tt)));
        // nil => falsy
        LUAU_ASSERT(LUA_TNIL == 0);
        build.cbz(temp, labelOp(inst.c));
        // not boolean => truthy
        build.cmp(temp, LUA_TBOOLEAN);
        build.b(ConditionA64::NotEqual, labelOp(inst.b));
        // compare boolean value
        build.ldr(temp, mem(rBase, vmRegOp(inst.a) * sizeof(TValue) + offsetof(TValue, value)));
        build.cbnz(temp, labelOp(inst.b));
        jumpOrFallthrough(blockOp(inst.c), next);
        break;
    }
    case IrCmd::JUMP_IF_FALSY:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.ldr(temp, mem(rBase, vmRegOp(inst.a) * sizeof(TValue) + offsetof(TValue, tt)));
        // nil => falsy
        LUAU_ASSERT(LUA_TNIL == 0);
        build.cbz(temp, labelOp(inst.b));
        // not boolean => truthy
        build.cmp(temp, LUA_TBOOLEAN);
        build.b(ConditionA64::NotEqual, labelOp(inst.c));
        // compare boolean value
        build.ldr(temp, mem(rBase, vmRegOp(inst.a) * sizeof(TValue) + offsetof(TValue, value)));
        build.cbz(temp, labelOp(inst.b));
        jumpOrFallthrough(blockOp(inst.c), next);
        break;
    }
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
    case IrCmd::JUMP_EQ_INT:
        LUAU_ASSERT(unsigned(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate);
        build.cmp(regOp(inst.a), uint16_t(intOp(inst.b)));
        build.b(ConditionA64::Equal, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::JUMP_EQ_POINTER:
        build.cmp(regOp(inst.a), regOp(inst.b));
        build.b(ConditionA64::Equal, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
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
    case IrCmd::TABLE_LEN:
    {
        regs.assertAllFreeExcept(regOp(inst.a));
        build.mov(x0, regOp(inst.a)); // TODO: minor aliasing hazard
        build.ldr(x1, mem(rNativeContext, offsetof(NativeContext, luaH_getn)));
        build.blr(x1);

        inst.regA64 = regs.allocReg(KindA64::d);
        build.scvtf(inst.regA64, x0);
        break;
    }
    case IrCmd::NEW_TABLE:
    {
        regs.assertAllFree();
        build.mov(x0, rState);
        build.mov(x1, uintOp(inst.a));
        build.mov(x2, uintOp(inst.b));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaH_new)));
        build.blr(x3);
        // TODO: we could takeReg x0 but it's unclear if we will be able to keep x0 allocatable due to aliasing concerns
        inst.regA64 = regs.allocReg(KindA64::x);
        build.mov(inst.regA64, x0);
        break;
    }
    case IrCmd::DUP_TABLE:
    {
        regs.assertAllFreeExcept(regOp(inst.a));
        build.mov(x0, rState);
        build.mov(x1, regOp(inst.a)); // TODO: aliasing hazard
        build.ldr(x2, mem(rNativeContext, offsetof(NativeContext, luaH_clone)));
        build.blr(x2);
        // TODO: we could takeReg x0 but it's unclear if we will be able to keep x0 allocatable due to aliasing concerns
        inst.regA64 = regs.allocReg(KindA64::x);
        build.mov(inst.regA64, x0);
        break;
    }
    case IrCmd::TRY_NUM_TO_INDEX:
    {
        inst.regA64 = regs.allocReg(KindA64::w);
        RegisterA64 temp1 = tempDouble(inst.a);

        if (build.features & Feature_JSCVT)
        {
            build.fjcvtzs(inst.regA64, temp1); // fjcvtzs sets PSTATE.Z (equal) iff conversion is exact
            build.b(ConditionA64::NotEqual, labelOp(inst.b));
        }
        else
        {
            RegisterA64 temp2 = regs.allocTemp(KindA64::d);

            build.fcvtzs(inst.regA64, temp1);
            build.scvtf(temp2, inst.regA64);
            build.fcmp(temp1, temp2);
            build.b(ConditionA64::NotEqual, labelOp(inst.b));
        }
        break;
    }
    case IrCmd::INT_TO_NUM:
    {
        inst.regA64 = regs.allocReg(KindA64::d);
        RegisterA64 temp = tempInt(inst.a);
        build.scvtf(inst.regA64, temp);
        break;
    }
    case IrCmd::ADJUST_STACK_TO_REG:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::x);

        if (inst.b.kind == IrOpKind::Constant)
        {
            build.add(temp, rBase, uint16_t((vmRegOp(inst.a) + intOp(inst.b)) * sizeof(TValue)));
            build.str(temp, mem(rState, offsetof(lua_State, top)));
        }
        else if (inst.b.kind == IrOpKind::Inst)
        {
            build.add(temp, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
            // TODO: This is a temporary hack that reads wN register as if it was xN. This should use unsigned extension shift once we support it.
            build.add(temp, temp, castReg(KindA64::x, regOp(inst.b)), kTValueSizeLog2);
            build.str(temp, mem(rState, offsetof(lua_State, top)));
        }
        else
            LUAU_ASSERT(!"Unsupported instruction form");
        break;
    }
    case IrCmd::ADJUST_STACK_TO_TOP:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::x);
        build.ldr(temp, mem(rState, offsetof(lua_State, ci)));
        build.ldr(temp, mem(temp, offsetof(CallInfo, top)));
        build.str(temp, mem(rState, offsetof(lua_State, top)));
        break;
    }
    case IrCmd::INVOKE_FASTCALL:
    {
        regs.assertAllFree();
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));
        build.add(x2, rBase, uint16_t(vmRegOp(inst.c) * sizeof(TValue)));
        build.mov(w3, intOp(inst.f)); // nresults

        if (inst.d.kind == IrOpKind::VmReg)
            build.add(x4, rBase, uint16_t(vmRegOp(inst.d) * sizeof(TValue)));
        else if (inst.d.kind == IrOpKind::VmConst)
        {
            // TODO: refactor into a common helper
            if (vmConstOp(inst.d) * sizeof(TValue) <= AssemblyBuilderA64::kMaxImmediate)
            {
                build.add(x4, rConstants, uint16_t(vmConstOp(inst.d) * sizeof(TValue)));
            }
            else
            {
                build.mov(x4, vmConstOp(inst.d) * sizeof(TValue));
                build.add(x4, rConstants, x4);
            }
        }
        else
            LUAU_ASSERT(boolOp(inst.d) == false);

        // nparams
        if (intOp(inst.e) == LUA_MULTRET)
        {
            // L->top - (ra + 1)
            build.ldr(x5, mem(rState, offsetof(lua_State, top)));
            build.sub(x5, x5, rBase);
            build.sub(x5, x5, uint16_t((vmRegOp(inst.b) + 1) * sizeof(TValue)));
            // TODO: this can use immediate shift right or maybe add/sub with shift right but we don't implement them yet
            build.mov(x6, kTValueSizeLog2);
            build.lsr(x5, x5, x6);
        }
        else
            build.mov(w5, intOp(inst.e));

        build.ldr(x6, mem(rNativeContext, offsetof(NativeContext, luauF_table) + uintOp(inst.a) * sizeof(luau_FastFunction)));
        build.blr(x6);

        // TODO: we could takeReg w0 but it's unclear if we will be able to keep x0 allocatable due to aliasing concerns
        inst.regA64 = regs.allocReg(KindA64::w);
        build.mov(inst.regA64, w0);
        break;
    }
    case IrCmd::CHECK_FASTCALL_RES:
        build.cmp(regOp(inst.a), 0);
        build.b(ConditionA64::Less, labelOp(inst.b));
        break;
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
    case IrCmd::DO_LEN:
        regs.assertAllFree();
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.add(x2, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaV_dolen)));
        build.blr(x3);

        emitUpdateBase(build);
        break;
    case IrCmd::GET_TABLE:
        regs.assertAllFree();
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));

        if (inst.c.kind == IrOpKind::VmReg)
            build.add(x2, rBase, uint16_t(vmRegOp(inst.c) * sizeof(TValue)));
        else if (inst.c.kind == IrOpKind::Constant)
        {
            TValue n;
            setnvalue(&n, uintOp(inst.c));
            build.adr(x2, &n, sizeof(n));
        }
        else
            LUAU_ASSERT(!"Unsupported instruction form");

        build.add(x3, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_gettable)));
        build.blr(x4);

        emitUpdateBase(build);
        break;
    case IrCmd::SET_TABLE:
        regs.assertAllFree();
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));

        if (inst.c.kind == IrOpKind::VmReg)
            build.add(x2, rBase, uint16_t(vmRegOp(inst.c) * sizeof(TValue)));
        else if (inst.c.kind == IrOpKind::Constant)
        {
            TValue n;
            setnvalue(&n, uintOp(inst.c));
            build.adr(x2, &n, sizeof(n));
        }
        else
            LUAU_ASSERT(!"Unsupported instruction form");

        build.add(x3, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_settable)));
        build.blr(x4);

        emitUpdateBase(build);
        break;
    case IrCmd::GET_IMPORT:
        regs.assertAllFree();
        emitInstGetImport(build, vmRegOp(inst.a), uintOp(inst.b));
        break;
    case IrCmd::CONCAT:
        regs.assertAllFree();
        build.mov(x0, rState);
        build.mov(x1, uintOp(inst.b));
        build.mov(x2, vmRegOp(inst.a) + uintOp(inst.b) - 1);
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaV_concat)));
        build.blr(x3);

        emitUpdateBase(build);
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
    case IrCmd::SET_UPVALUE:
    {
        regs.assertAllFree();
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::x);
        RegisterA64 temp3 = regs.allocTemp(KindA64::q);
        RegisterA64 temp4 = regs.allocTemp(KindA64::x);

        // UpVal*
        build.ldr(temp1, mem(rClosure, offsetof(Closure, l.uprefs) + sizeof(TValue) * vmUpvalueOp(inst.a) + offsetof(TValue, value.gc)));

        build.ldr(temp2, mem(temp1, offsetof(UpVal, v)));
        build.ldr(temp3, mem(rBase, vmRegOp(inst.b) * sizeof(TValue)));
        build.str(temp3, temp2);

        Label skip;
        checkObjectBarrierConditions(build, temp1, temp2, temp4, vmRegOp(inst.b), skip);

        build.mov(x0, rState);
        build.mov(x1, temp1); // TODO: aliasing hazard
        build.ldr(x2, mem(rBase, vmRegOp(inst.b) * sizeof(TValue) + offsetof(TValue, value)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaC_barrierf)));
        build.blr(x3);

        // note: no emitUpdateBase necessary because luaC_ barriers do not reallocate stack
        build.setLabel(skip);
        break;
    }
    case IrCmd::PREPARE_FORN:
        regs.assertAllFree();
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.add(x2, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));
        build.add(x3, rBase, uint16_t(vmRegOp(inst.c) * sizeof(TValue)));
        build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_prepareFORN)));
        build.blr(x4);
        // note: no emitUpdateBase necessary because prepareFORN does not reallocate stack
        break;
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
        RegisterA64 tempw = castReg(KindA64::w, temp);
        build.ldr(temp, mem(rClosure, offsetof(Closure, env)));
        build.ldrb(tempw, mem(temp, offsetof(Table, safeenv)));
        build.cbz(tempw, labelOp(inst.a));
        break;
    }
    case IrCmd::CHECK_ARRAY_SIZE:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.ldr(temp, mem(regOp(inst.a), offsetof(Table, sizearray)));

        if (inst.b.kind == IrOpKind::Inst)
            build.cmp(temp, regOp(inst.b));
        else if (inst.b.kind == IrOpKind::Constant)
        {
            LUAU_ASSERT(size_t(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate); // TODO: handle out of range values
            build.cmp(temp, uint16_t(intOp(inst.b)));
        }
        else
            LUAU_ASSERT(!"Unsupported instruction form");

        build.b(ConditionA64::UnsignedLessEqual, labelOp(inst.c));
        break;
    }
    case IrCmd::CHECK_SLOT_MATCH:
    {
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp1w = castReg(KindA64::w, temp1);
        RegisterA64 temp2 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2w = castReg(KindA64::w, temp2);

        build.ldr(temp1w, mem(regOp(inst.a), kOffsetOfLuaNodeTag));
        // TODO: this needs bitfield extraction, or and-immediate
        build.mov(temp2w, kLuaNodeTagMask);
        build.and_(temp1w, temp1w, temp2w);
        build.cmp(temp1w, LUA_TSTRING);
        build.b(ConditionA64::NotEqual, labelOp(inst.c));

        AddressA64 addr = tempAddr(inst.b, offsetof(TValue, value));
        build.ldr(temp1, mem(regOp(inst.a), offsetof(LuaNode, key.value)));
        build.ldr(temp2, addr);
        build.cmp(temp1, temp2);
        build.b(ConditionA64::NotEqual, labelOp(inst.c));

        build.ldr(temp1w, mem(regOp(inst.a), offsetof(LuaNode, val.tt)));
        LUAU_ASSERT(LUA_TNIL == 0);
        build.cbz(temp1w, labelOp(inst.c));
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
    case IrCmd::CHECK_GC:
    {
        regs.assertAllFree();
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::x);

        Label skip;
        build.ldr(temp1, mem(rState, offsetof(lua_State, global)));
        build.ldr(temp2, mem(temp1, offsetof(global_State, totalbytes)));
        build.ldr(temp1, mem(temp1, offsetof(global_State, GCthreshold)));
        build.cmp(temp1, temp2);
        build.b(ConditionA64::UnsignedGreater, skip);

        build.mov(x0, rState);
        build.mov(w1, 1);
        build.ldr(x1, mem(rNativeContext, offsetof(NativeContext, luaC_step)));
        build.blr(x1);

        emitUpdateBase(build);
        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_OBJ:
    {
        regs.assertAllFreeExcept(regOp(inst.a));

        Label skip;
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::x);

        checkObjectBarrierConditions(build, regOp(inst.a), temp1, temp2, vmRegOp(inst.b), skip);

        build.mov(x0, rState);
        build.mov(x1, regOp(inst.a)); // TODO: aliasing hazard
        build.ldr(x2, mem(rBase, vmRegOp(inst.b) * sizeof(TValue) + offsetof(TValue, value)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaC_barrierf)));
        build.blr(x3);

        // note: no emitUpdateBase necessary because luaC_ barriers do not reallocate stack
        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_TABLE_BACK:
    {
        regs.assertAllFreeExcept(regOp(inst.a));

        Label skip;
        RegisterA64 temp1 = regs.allocTemp(KindA64::w);
        RegisterA64 temp2 = regs.allocTemp(KindA64::w);

        // isblack(obj2gco(t))
        build.ldrb(temp1, mem(regOp(inst.a), offsetof(GCheader, marked)));
        // TODO: conditional bit test with BLACKBIT
        build.mov(temp2, bitmask(BLACKBIT));
        build.and_(temp1, temp1, temp2);
        build.cbz(temp1, skip);

        build.mov(x0, rState);
        build.mov(x1, regOp(inst.a)); // TODO: aliasing hazard here and below
        build.add(x2, regOp(inst.a), uint16_t(offsetof(Table, gclist)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaC_barrierback)));
        build.blr(x3);

        // note: no emitUpdateBase necessary because luaC_ barriers do not reallocate stack
        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_TABLE_FORWARD:
    {
        regs.assertAllFreeExcept(regOp(inst.a));

        Label skip;
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::x);

        checkObjectBarrierConditions(build, regOp(inst.a), temp1, temp2, vmRegOp(inst.b), skip);

        build.mov(x0, rState);
        build.mov(x1, regOp(inst.a)); // TODO: aliasing hazard
        build.ldr(x2, mem(rBase, vmRegOp(inst.b) * sizeof(TValue) + offsetof(TValue, value)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaC_barriertable)));
        build.blr(x3);

        // note: no emitUpdateBase necessary because luaC_ barriers do not reallocate stack
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
    case IrCmd::CLOSE_UPVALS:
    {
        regs.assertAllFree();
        Label skip;
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::x);

        // L->openupval != 0
        build.ldr(temp1, mem(rState, offsetof(lua_State, openupval)));
        build.cbz(temp1, skip);

        // ra <= L->openuval->v
        build.ldr(temp1, mem(temp1, offsetof(UpVal, v)));
        build.add(temp2, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.cmp(temp2, temp1);
        build.b(ConditionA64::UnsignedGreater, skip);

        build.mov(x0, rState);
        build.mov(x1, temp2); // TODO: aliasing hazard
        build.ldr(x2, mem(rNativeContext, offsetof(NativeContext, luaF_close)));
        build.blr(x2);

        build.setLabel(skip);
        break;
    }
    case IrCmd::CAPTURE:
        // no-op
        break;
    case IrCmd::CALL:
        regs.assertAllFree();
        emitInstCall(build, helpers, vmRegOp(inst.a), intOp(inst.b), intOp(inst.c));
        break;
    case IrCmd::RETURN:
        regs.assertAllFree();
        emitInstReturn(build, helpers, vmRegOp(inst.a), intOp(inst.b));
        break;

        // Full instruction fallbacks
    case IrCmd::FALLBACK_GETGLOBAL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        regs.assertAllFree();
        emitFallback(build, LOP_GETGLOBAL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETGLOBAL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        regs.assertAllFree();
        emitFallback(build, LOP_SETGLOBAL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETTABLEKS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        regs.assertAllFree();
        emitFallback(build, LOP_GETTABLEKS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETTABLEKS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        regs.assertAllFree();
        emitFallback(build, LOP_SETTABLEKS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_NAMECALL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        regs.assertAllFree();
        emitFallback(build, LOP_NAMECALL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_PREPVARARGS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::Constant);

        regs.assertAllFree();
        emitFallback(build, LOP_PREPVARARGS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETVARARGS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);

        regs.assertAllFree();
        emitFallback(build, LOP_GETVARARGS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_NEWCLOSURE:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);

        regs.assertAllFree();
        emitFallback(build, LOP_NEWCLOSURE, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_DUPCLOSURE:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        regs.assertAllFree();
        emitFallback(build, LOP_DUPCLOSURE, uintOp(inst.a));
        break;

    default:
        LUAU_ASSERT(!"Not supported yet");
        break;
    }

    regs.freeLastUseRegs(inst, index);
    regs.freeTempRegs();
}

bool IrLoweringA64::hasError() const
{
    return false;
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
