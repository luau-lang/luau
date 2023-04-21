// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrLoweringA64.h"

#include "Luau/CodeGen.h"
#include "Luau/DenseHash.h"
#include "Luau/IrAnalysis.h"
#include "Luau/IrDump.h"
#include "Luau/IrUtils.h"

#include "EmitCommonA64.h"
#include "NativeState.h"

#include "lstate.h"
#include "lgc.h"

namespace Luau
{
namespace CodeGen
{
namespace A64
{

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

static void checkObjectBarrierConditions(AssemblyBuilderA64& build, RegisterA64 object, RegisterA64 temp, int ra, Label& skip)
{
    RegisterA64 tempw = castReg(KindA64::w, temp);

    // iscollectable(ra)
    build.ldr(tempw, mem(rBase, ra * sizeof(TValue) + offsetof(TValue, tt)));
    build.cmp(tempw, LUA_TSTRING);
    build.b(ConditionA64::Less, skip);

    // isblack(obj2gco(o))
    build.ldrb(tempw, mem(object, offsetof(GCheader, marked)));
    build.tbz(tempw, BLACKBIT, skip);

    // iswhite(gcvalue(ra))
    build.ldr(temp, mem(rBase, ra * sizeof(TValue) + offsetof(TValue, value)));
    build.ldrb(tempw, mem(temp, offsetof(GCheader, marked)));
    build.tst(tempw, bit2mask(WHITE0BIT, WHITE1BIT));
    build.b(ConditionA64::Equal, skip); // Equal = Zero after tst
}

static void emitAddOffset(AssemblyBuilderA64& build, RegisterA64 dst, RegisterA64 src, size_t offset)
{
    LUAU_ASSERT(dst != src);
    LUAU_ASSERT(offset <= INT_MAX);

    if (offset <= AssemblyBuilderA64::kMaxImmediate)
    {
        build.add(dst, src, uint16_t(offset));
    }
    else
    {
        build.mov(dst, int(offset));
        build.add(dst, dst, src);
    }
}

static void emitFallback(AssemblyBuilderA64& build, int op, int pcpos)
{
    // fallback(L, instruction, base, k)
    build.mov(x0, rState);
    emitAddOffset(build, x1, rCode, pcpos * sizeof(Instruction));
    build.mov(x2, rBase);
    build.mov(x3, rConstants);
    build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, fallback) + op * sizeof(FallbackFn)));
    build.blr(x4);

    emitUpdateBase(build);
}

static void emitInvokeLibm1(AssemblyBuilderA64& build, size_t func, int res, int arg)
{
    build.ldr(d0, mem(rBase, arg * sizeof(TValue) + offsetof(TValue, value.n)));
    build.ldr(x0, mem(rNativeContext, uint32_t(func)));
    build.blr(x0);
    build.str(d0, mem(rBase, res * sizeof(TValue) + offsetof(TValue, value.n)));
}

static void emitInvokeLibm2(AssemblyBuilderA64& build, size_t func, int res, int arg, IrOp args, bool argsInt = false)
{
    if (args.kind == IrOpKind::VmReg)
        build.ldr(d1, mem(rBase, args.index * sizeof(TValue) + offsetof(TValue, value.n)));
    else if (args.kind == IrOpKind::VmConst)
    {
        size_t constantOffset = args.index * sizeof(TValue) + offsetof(TValue, value.n);

        // Note: cumulative offset is guaranteed to be divisible by 8 (since we're loading a double); we can use that to expand the useful range that
        // doesn't require temporaries
        if (constantOffset / 8 <= AddressA64::kMaxOffset)
        {
            build.ldr(d1, mem(rConstants, int(constantOffset)));
        }
        else
        {
            emitAddOffset(build, x0, rConstants, constantOffset);
            build.ldr(d1, x0);
        }
    }
    else
        LUAU_ASSERT(!"Unsupported instruction form");

    if (argsInt)
        build.fcvtzs(w0, d1);

    build.ldr(d0, mem(rBase, arg * sizeof(TValue) + offsetof(TValue, value.n)));
    build.ldr(x1, mem(rNativeContext, uint32_t(func)));
    build.blr(x1);
    build.str(d0, mem(rBase, res * sizeof(TValue) + offsetof(TValue, value.n)));
}

static void emitInvokeLibm1P(AssemblyBuilderA64& build, size_t func, int arg)
{
    build.ldr(d0, mem(rBase, arg * sizeof(TValue) + offsetof(TValue, value.n)));
    build.add(x0, sp, sTemporary.data); // sp-relative offset
    build.ldr(x1, mem(rNativeContext, uint32_t(func)));
    build.blr(x1);
}

static bool emitBuiltin(AssemblyBuilderA64& build, IrRegAllocA64& regs, int bfid, int res, int arg, IrOp args, int nparams, int nresults)
{
    switch (bfid)
    {
    case LBF_MATH_LOG:
        LUAU_ASSERT((nparams == 1 || nparams == 2) && nresults == 1);
        // TODO: IR builtin lowering assumes that the only valid 2-argument call is log2; ideally, we use a less hacky way to indicate that
        if (nparams == 2)
            emitInvokeLibm1(build, offsetof(NativeContext, libm_log2), res, arg);
        else
            emitInvokeLibm1(build, offsetof(NativeContext, libm_log), res, arg);
        return true;
    case LBF_MATH_LDEXP:
        LUAU_ASSERT(nparams == 2 && nresults == 1);
        emitInvokeLibm2(build, offsetof(NativeContext, libm_ldexp), res, arg, args, /* argsInt= */ true);
        return true;
    case LBF_MATH_FREXP:
        LUAU_ASSERT(nparams == 1 && (nresults == 1 || nresults == 2));
        emitInvokeLibm1P(build, offsetof(NativeContext, libm_frexp), arg);
        build.str(d0, mem(rBase, res * sizeof(TValue) + offsetof(TValue, value.n)));
        if (nresults == 2)
        {
            build.ldr(w0, sTemporary);
            build.scvtf(d1, w0);
            build.str(d1, mem(rBase, (res + 1) * sizeof(TValue) + offsetof(TValue, value.n)));
        }
        return true;
    case LBF_MATH_MODF:
        LUAU_ASSERT(nparams == 1 && (nresults == 1 || nresults == 2));
        emitInvokeLibm1P(build, offsetof(NativeContext, libm_modf), arg);
        build.ldr(d1, sTemporary);
        build.str(d1, mem(rBase, res * sizeof(TValue) + offsetof(TValue, value.n)));
        if (nresults == 2)
            build.str(d0, mem(rBase, (res + 1) * sizeof(TValue) + offsetof(TValue, value.n)));
        return true;
    case LBF_MATH_SIGN:
        LUAU_ASSERT(nparams == 1 && nresults == 1);
        build.ldr(d0, mem(rBase, arg * sizeof(TValue) + offsetof(TValue, value.n)));
        build.fcmpz(d0);
        build.fmov(d0, 0.0);
        build.fmov(d1, 1.0);
        build.fcsel(d0, d1, d0, getConditionFP(IrCondition::Greater));
        build.fmov(d1, -1.0);
        build.fcsel(d0, d1, d0, getConditionFP(IrCondition::Less));
        build.str(d0, mem(rBase, res * sizeof(TValue) + offsetof(TValue, value.n)));
        return true;

    case LBF_TYPE:
        build.ldr(w0, mem(rBase, arg * sizeof(TValue) + offsetof(TValue, tt)));
        build.ldr(x1, mem(rState, offsetof(lua_State, global)));
        LUAU_ASSERT(sizeof(TString*) == 8);
        build.add(x1, x1, zextReg(w0), 3);
        build.ldr(x0, mem(x1, offsetof(global_State, ttname)));
        build.str(x0, mem(rBase, res * sizeof(TValue) + offsetof(TValue, value.gc)));
        return true;

    case LBF_TYPEOF:
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(arg * sizeof(TValue)));
        build.ldr(x2, mem(rNativeContext, offsetof(NativeContext, luaT_objtypenamestr)));
        build.blr(x2);
        build.str(x0, mem(rBase, res * sizeof(TValue) + offsetof(TValue, value.gc)));
        return true;

    default:
        LUAU_ASSERT(!"Missing A64 lowering");
        return false;
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

void IrLoweringA64::lowerInst(IrInst& inst, uint32_t index, IrBlock& next)
{
    switch (inst.cmd)
    {
    case IrCmd::LOAD_TAG:
    {
        inst.regA64 = regs.allocReg(KindA64::w, index);
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, tt));
        build.ldr(inst.regA64, addr);
        break;
    }
    case IrCmd::LOAD_POINTER:
    {
        inst.regA64 = regs.allocReg(KindA64::x, index);
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value.gc));
        build.ldr(inst.regA64, addr);
        break;
    }
    case IrCmd::LOAD_DOUBLE:
    {
        inst.regA64 = regs.allocReg(KindA64::d, index);
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value.n));
        build.ldr(inst.regA64, addr);
        break;
    }
    case IrCmd::LOAD_INT:
    {
        inst.regA64 = regs.allocReg(KindA64::w, index);
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value));
        build.ldr(inst.regA64, addr);
        break;
    }
    case IrCmd::LOAD_TVALUE:
    {
        inst.regA64 = regs.allocReg(KindA64::q, index);
        AddressA64 addr = tempAddr(inst.a, 0);
        build.ldr(inst.regA64, addr);
        break;
    }
    case IrCmd::LOAD_NODE_VALUE_TV:
    {
        inst.regA64 = regs.allocReg(KindA64::q, index);
        build.ldr(inst.regA64, mem(regOp(inst.a), offsetof(LuaNode, val)));
        break;
    }
    case IrCmd::LOAD_ENV:
        inst.regA64 = regs.allocReg(KindA64::x, index);
        build.ldr(inst.regA64, mem(rClosure, offsetof(Closure, env)));
        break;
    case IrCmd::GET_ARR_ADDR:
    {
        inst.regA64 = regs.allocReuse(KindA64::x, index, {inst.a});
        build.ldr(inst.regA64, mem(regOp(inst.a), offsetof(Table, array)));

        if (inst.b.kind == IrOpKind::Inst)
        {
            build.add(inst.regA64, inst.regA64, zextReg(regOp(inst.b)), kTValueSizeLog2);
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            // TODO: refactor into a common helper? can't use emitAddOffset because we need a temp register
            if (intOp(inst.b) * sizeof(TValue) <= AssemblyBuilderA64::kMaxImmediate)
            {
                build.add(inst.regA64, inst.regA64, uint16_t(intOp(inst.b) * sizeof(TValue)));
            }
            else
            {
                RegisterA64 temp = regs.allocTemp(KindA64::x);
                build.mov(temp, intOp(inst.b) * sizeof(TValue));
                build.add(inst.regA64, inst.regA64, temp);
            }
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

        // note: since the stride of the load is the same as the destination register size, we can range check the array index, not the byte offset
        if (uintOp(inst.b) <= AddressA64::kMaxOffset)
            build.ldr(temp1w, mem(rCode, uintOp(inst.b) * sizeof(Instruction)));
        else
        {
            build.mov(temp1, uintOp(inst.b) * sizeof(Instruction));
            build.ldr(temp1w, mem(rCode, temp1));
        }

        // C field can be shifted as long as it's at the most significant byte of the instruction word
        LUAU_ASSERT(kOffsetOfInstructionC == 3);
        build.ldrb(temp2, mem(regOp(inst.a), offsetof(Table, nodemask8)));
        build.and_(temp2, temp2, temp1w, -24);

        // note: this may clobber inst.a, so it's important that we don't use it after this
        build.ldr(inst.regA64, mem(regOp(inst.a), offsetof(Table, node)));
        build.add(inst.regA64, inst.regA64, zextReg(temp2), kLuaNodeSizeLog2);
        break;
    }
    case IrCmd::GET_HASH_NODE_ADDR:
    {
        inst.regA64 = regs.allocReuse(KindA64::x, index, {inst.a});
        RegisterA64 temp1 = regs.allocTemp(KindA64::w);
        RegisterA64 temp2 = regs.allocTemp(KindA64::w);

        // hash & ((1 << lsizenode) - 1) == hash & ~(-1 << lsizenode)
        build.mov(temp1, -1);
        build.ldrb(temp2, mem(regOp(inst.a), offsetof(Table, lsizenode)));
        build.lsl(temp1, temp1, temp2);
        build.mov(temp2, uintOp(inst.b));
        build.bic(temp2, temp2, temp1);

        // note: this may clobber inst.a, so it's important that we don't use it after this
        build.ldr(inst.regA64, mem(regOp(inst.a), offsetof(Table, node)));
        build.add(inst.regA64, inst.regA64, zextReg(temp2), kLuaNodeSizeLog2);
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
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.b.kind == IrOpKind::Constant && unsigned(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate)
            build.add(inst.regA64, regOp(inst.a), uint16_t(intOp(inst.b)));
        else
        {
            RegisterA64 temp = tempInt(inst.b);
            build.add(inst.regA64, regOp(inst.a), temp);
        }
        break;
    case IrCmd::SUB_INT:
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.b.kind == IrOpKind::Constant && unsigned(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate)
            build.sub(inst.regA64, regOp(inst.a), uint16_t(intOp(inst.b)));
        else
        {
            RegisterA64 temp = tempInt(inst.b);
            build.sub(inst.regA64, regOp(inst.a), temp);
        }
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
        inst.regA64 = regs.allocReg(KindA64::d, index); // can't allocReuse because both A and B are used twice
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
        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);
        build.fmov(d0, temp1); // TODO: aliasing hazard
        build.fmov(d1, temp2); // TODO: aliasing hazard
        regs.spill(build, index, {d0, d1});
        build.ldr(x0, mem(rNativeContext, offsetof(NativeContext, libm_pow)));
        build.blr(x0);

        // TODO: we could takeReg d0 but it's unclear if we will be able to keep d0 allocatable due to aliasing concerns
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a, inst.b});
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
    case IrCmd::NOT_ANY:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});

        if (inst.a.kind == IrOpKind::Constant)
        {
            // other cases should've been constant folded
            LUAU_ASSERT(tagOp(inst.a) == LUA_TBOOLEAN);
            build.eor(inst.regA64, regOp(inst.b), 1);
        }
        else
        {
            Label notbool, exit;

            // use the fact that NIL is the only value less than BOOLEAN to do two tag comparisons at once
            LUAU_ASSERT(LUA_TNIL == 0 && LUA_TBOOLEAN == 1);
            build.cmp(regOp(inst.a), LUA_TBOOLEAN);
            build.b(ConditionA64::NotEqual, notbool);

            // boolean => invert value
            build.eor(inst.regA64, regOp(inst.b), 1);
            build.b(exit);

            // not boolean => result is true iff tag was nil
            build.setLabel(notbool);
            build.cset(inst.regA64, ConditionA64::Less);

            build.setLabel(exit);
        }
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
        if (inst.a.kind == IrOpKind::Inst && inst.b.kind == IrOpKind::Constant)
            build.cmp(regOp(inst.a), tagOp(inst.b));
        else if (inst.a.kind == IrOpKind::Inst && inst.b.kind == IrOpKind::Inst)
            build.cmp(regOp(inst.a), regOp(inst.b));
        else if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Inst)
            build.cmp(regOp(inst.b), tagOp(inst.a));
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
    case IrCmd::JUMP_LT_INT:
        LUAU_ASSERT(unsigned(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate);
        build.cmp(regOp(inst.a), uint16_t(intOp(inst.b)));
        build.b(ConditionA64::Less, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::JUMP_GE_UINT:
    {
        LUAU_ASSERT(uintOp(inst.b) <= AssemblyBuilderA64::kMaxImmediate);
        build.cmp(regOp(inst.a), uint16_t(uintOp(inst.b)));
        build.b(ConditionA64::CarrySet, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    }
    case IrCmd::JUMP_EQ_POINTER:
        build.cmp(regOp(inst.a), regOp(inst.b));
        build.b(ConditionA64::Equal, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::JUMP_CMP_NUM:
    {
        IrCondition cond = conditionOp(inst.c);

        if (inst.b.kind == IrOpKind::Constant && doubleOp(inst.b) == 0.0)
        {
            RegisterA64 temp = tempDouble(inst.a);

            build.fcmpz(temp);
        }
        else
        {
            RegisterA64 temp1 = tempDouble(inst.a);
            RegisterA64 temp2 = tempDouble(inst.b);

            build.fcmp(temp1, temp2);
        }

        build.b(getConditionFP(cond), labelOp(inst.d));
        jumpOrFallthrough(blockOp(inst.e), next);
        break;
    }
    case IrCmd::JUMP_CMP_ANY:
    {
        IrCondition cond = conditionOp(inst.c);

        regs.spill(build, index);
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
    case IrCmd::JUMP_SLOT_MATCH:
    {
        // TODO: share code with CHECK_SLOT_MATCH
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp1w = castReg(KindA64::w, temp1);
        RegisterA64 temp2 = regs.allocTemp(KindA64::x);

        build.ldr(temp1w, mem(regOp(inst.a), offsetof(LuaNode, key) + kOffsetOfTKeyTag));
        build.and_(temp1w, temp1w, kLuaNodeTagMask);
        build.cmp(temp1w, LUA_TSTRING);
        build.b(ConditionA64::NotEqual, labelOp(inst.d));

        AddressA64 addr = tempAddr(inst.b, offsetof(TValue, value));
        build.ldr(temp1, mem(regOp(inst.a), offsetof(LuaNode, key.value)));
        build.ldr(temp2, addr);
        build.cmp(temp1, temp2);
        build.b(ConditionA64::NotEqual, labelOp(inst.d));

        build.ldr(temp1w, mem(regOp(inst.a), offsetof(LuaNode, val.tt)));
        LUAU_ASSERT(LUA_TNIL == 0);
        build.cbz(temp1w, labelOp(inst.d));
        jumpOrFallthrough(blockOp(inst.c), next);
        break;
    }
    case IrCmd::TABLE_LEN:
    {
        build.mov(x0, regOp(inst.a)); // TODO: aliasing hazard
        regs.spill(build, index, {x0});
        build.ldr(x1, mem(rNativeContext, offsetof(NativeContext, luaH_getn)));
        build.blr(x1);
        inst.regA64 = regs.allocReg(KindA64::d, index);
        build.scvtf(inst.regA64, x0);
        break;
    }
    case IrCmd::NEW_TABLE:
    {
        regs.spill(build, index);
        build.mov(x0, rState);
        build.mov(x1, uintOp(inst.a));
        build.mov(x2, uintOp(inst.b));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaH_new)));
        build.blr(x3);
        // TODO: we could takeReg x0 but it's unclear if we will be able to keep x0 allocatable due to aliasing concerns
        inst.regA64 = regs.allocReg(KindA64::x, index);
        build.mov(inst.regA64, x0);
        break;
    }
    case IrCmd::DUP_TABLE:
    {
        build.mov(x1, regOp(inst.a)); // TODO: aliasing hazard
        regs.spill(build, index, {x1});
        build.mov(x0, rState);
        build.ldr(x2, mem(rNativeContext, offsetof(NativeContext, luaH_clone)));
        build.blr(x2);
        // TODO: we could takeReg x0 but it's unclear if we will be able to keep x0 allocatable due to aliasing concerns
        inst.regA64 = regs.allocReuse(KindA64::x, index, {inst.a});
        build.mov(inst.regA64, x0);
        break;
    }
    case IrCmd::TRY_NUM_TO_INDEX:
    {
        inst.regA64 = regs.allocReg(KindA64::w, index);
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
    case IrCmd::TRY_CALL_FASTGETTM:
    {
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::w);

        build.ldr(temp1, mem(regOp(inst.a), offsetof(Table, metatable)));
        build.cbz(temp1, labelOp(inst.c)); // no metatable

        build.ldrb(temp2, mem(temp1, offsetof(Table, tmcache)));
        build.tst(temp2, 1 << intOp(inst.b));             // can't use tbz/tbnz because their jump offsets are too short
        build.b(ConditionA64::NotEqual, labelOp(inst.c)); // Equal = Zero after tst; tmcache caches *absence* of metamethods

        build.mov(x0, temp1);
        regs.spill(build, index, {x0});
        build.mov(w1, intOp(inst.b));
        build.ldr(x2, mem(rState, offsetof(lua_State, global)));
        build.ldr(x2, mem(x2, offsetof(global_State, tmname) + intOp(inst.b) * sizeof(TString*)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaT_gettm)));
        build.blr(x3);

        // TODO: we could takeReg x0 but it's unclear if we will be able to keep x0 allocatable due to aliasing concerns
        inst.regA64 = regs.allocReuse(KindA64::x, index, {inst.a});
        build.mov(inst.regA64, x0);
        break;
    }
    case IrCmd::INT_TO_NUM:
    {
        inst.regA64 = regs.allocReg(KindA64::d, index);
        RegisterA64 temp = tempInt(inst.a);
        build.scvtf(inst.regA64, temp);
        break;
    }
    case IrCmd::UINT_TO_NUM:
    {
        inst.regA64 = regs.allocReg(KindA64::d, index);
        RegisterA64 temp = tempInt(inst.a);
        build.ucvtf(inst.regA64, temp);
        break;
    }
    case IrCmd::NUM_TO_INT:
    {
        inst.regA64 = regs.allocReg(KindA64::w, index);
        RegisterA64 temp = tempDouble(inst.a);
        build.fcvtzs(inst.regA64, temp);
        break;
    }
    case IrCmd::NUM_TO_UINT:
    {
        inst.regA64 = regs.allocReg(KindA64::w, index);
        RegisterA64 temp = tempDouble(inst.a);
        build.fcvtzs(castReg(KindA64::x, inst.regA64), temp);
        // truncation needs to clear high bits to preserve zextReg safety contract
        build.mov(inst.regA64, inst.regA64);
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
            build.add(temp, temp, zextReg(regOp(inst.b)), kTValueSizeLog2);
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
    case IrCmd::FASTCALL:
        regs.spill(build, index);
        // TODO: emitBuiltin should be exhaustive
        if (!emitBuiltin(build, regs, uintOp(inst.a), vmRegOp(inst.b), vmRegOp(inst.c), inst.d, intOp(inst.e), intOp(inst.f)))
            error = true;
        break;
    case IrCmd::INVOKE_FASTCALL:
    {
        regs.spill(build, index);
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));
        build.add(x2, rBase, uint16_t(vmRegOp(inst.c) * sizeof(TValue)));
        build.mov(w3, intOp(inst.f)); // nresults

        if (inst.d.kind == IrOpKind::VmReg)
            build.add(x4, rBase, uint16_t(vmRegOp(inst.d) * sizeof(TValue)));
        else if (inst.d.kind == IrOpKind::VmConst)
            emitAddOffset(build, x4, rConstants, vmConstOp(inst.d) * sizeof(TValue));
        else
            LUAU_ASSERT(boolOp(inst.d) == false);

        // nparams
        if (intOp(inst.e) == LUA_MULTRET)
        {
            // L->top - (ra + 1)
            build.ldr(x5, mem(rState, offsetof(lua_State, top)));
            build.sub(x5, x5, rBase);
            build.sub(x5, x5, uint16_t((vmRegOp(inst.b) + 1) * sizeof(TValue)));
            build.lsr(x5, x5, kTValueSizeLog2);
        }
        else
            build.mov(w5, intOp(inst.e));

        build.ldr(x6, mem(rNativeContext, offsetof(NativeContext, luauF_table) + uintOp(inst.a) * sizeof(luau_FastFunction)));
        build.blr(x6);

        // since w0 came from a call, we need to move it so that we don't violate zextReg safety contract
        inst.regA64 = regs.allocReg(KindA64::w, index);
        build.mov(inst.regA64, w0);
        break;
    }
    case IrCmd::CHECK_FASTCALL_RES:
        build.cmp(regOp(inst.a), 0);
        build.b(ConditionA64::Less, labelOp(inst.b));
        break;
    case IrCmd::DO_ARITH:
        regs.spill(build, index);
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.add(x2, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));

        if (inst.c.kind == IrOpKind::VmConst)
            emitAddOffset(build, x3, rConstants, vmConstOp(inst.c) * sizeof(TValue));
        else
            build.add(x3, rBase, uint16_t(vmRegOp(inst.c) * sizeof(TValue)));

        build.mov(w4, TMS(intOp(inst.d)));
        build.ldr(x5, mem(rNativeContext, offsetof(NativeContext, luaV_doarith)));
        build.blr(x5);

        emitUpdateBase(build);
        break;
    case IrCmd::DO_LEN:
        regs.spill(build, index);
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.add(x2, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaV_dolen)));
        build.blr(x3);

        emitUpdateBase(build);
        break;
    case IrCmd::GET_TABLE:
        regs.spill(build, index);
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
        regs.spill(build, index);
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
        regs.spill(build, index);
        // luaV_getimport(L, cl->env, k, aux, /* propagatenil= */ false)
        build.mov(x0, rState);
        build.ldr(x1, mem(rClosure, offsetof(Closure, env)));
        build.mov(x2, rConstants);
        build.mov(w3, uintOp(inst.b));
        build.mov(w4, 0);
        build.ldr(x5, mem(rNativeContext, offsetof(NativeContext, luaV_getimport)));
        build.blr(x5);

        emitUpdateBase(build);

        // setobj2s(L, ra, L->top - 1)
        build.ldr(x0, mem(rState, offsetof(lua_State, top)));
        build.sub(x0, x0, sizeof(TValue));
        build.ldr(q0, x0);
        build.str(q0, mem(rBase, vmRegOp(inst.a) * sizeof(TValue)));

        // L->top--
        build.str(x0, mem(rState, offsetof(lua_State, top)));
        break;
    case IrCmd::CONCAT:
        regs.spill(build, index);
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
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::x);
        RegisterA64 temp3 = regs.allocTemp(KindA64::q);

        // UpVal*
        build.ldr(temp1, mem(rClosure, offsetof(Closure, l.uprefs) + sizeof(TValue) * vmUpvalueOp(inst.a) + offsetof(TValue, value.gc)));

        build.ldr(temp2, mem(temp1, offsetof(UpVal, v)));
        build.ldr(temp3, mem(rBase, vmRegOp(inst.b) * sizeof(TValue)));
        build.str(temp3, temp2);

        Label skip;
        checkObjectBarrierConditions(build, temp1, temp2, vmRegOp(inst.b), skip);

        build.mov(x1, temp1); // TODO: aliasing hazard

        size_t spills = regs.spill(build, index, {x1});

        build.mov(x0, rState);
        build.ldr(x2, mem(rBase, vmRegOp(inst.b) * sizeof(TValue) + offsetof(TValue, value)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaC_barrierf)));
        build.blr(x3);

        regs.restore(build, spills); // need to restore before skip so that registers are in a consistent state

        // note: no emitUpdateBase necessary because luaC_ barriers do not reallocate stack
        build.setLabel(skip);
        break;
    }
    case IrCmd::PREPARE_FORN:
        regs.spill(build, index);
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
            // TODO: refactor into a common helper?
            if (size_t(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate)
            {
                build.cmp(temp, uint16_t(intOp(inst.b)));
            }
            else
            {
                RegisterA64 temp2 = regs.allocTemp(KindA64::w);
                build.mov(temp2, intOp(inst.b));
                build.cmp(temp, temp2);
            }
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

        build.ldr(temp1w, mem(regOp(inst.a), offsetof(LuaNode, key) + kOffsetOfTKeyTag));
        build.and_(temp1w, temp1w, kLuaNodeTagMask);
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
    case IrCmd::CHECK_NODE_NO_NEXT:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::w);

        build.ldr(temp, mem(regOp(inst.a), offsetof(LuaNode, key) + kOffsetOfTKeyNext));
        build.lsr(temp, temp, kNextBitOffset);
        build.cbnz(temp, labelOp(inst.b));
        break;
    }
    case IrCmd::INTERRUPT:
    {
        unsigned int pcpos = uintOp(inst.a);

        Label skip, next;
        build.ldr(x2, mem(rState, offsetof(lua_State, global)));
        build.ldr(x2, mem(x2, offsetof(global_State, cb.interrupt)));
        build.cbz(x2, skip);

        size_t spills = regs.spill(build, index, {x2});

        // Jump to outlined interrupt handler, it will give back control to x1
        build.mov(x0, (pcpos + 1) * sizeof(Instruction));
        build.adr(x1, next);
        build.b(helpers.interrupt);

        build.setLabel(next);

        regs.restore(build, spills); // need to restore before skip so that registers are in a consistent state

        build.setLabel(skip);
        break;
    }
    case IrCmd::CHECK_GC:
    {
        regs.spill(build, index);
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
        RegisterA64 temp = regs.allocTemp(KindA64::x);

        Label skip;
        checkObjectBarrierConditions(build, regOp(inst.a), temp, vmRegOp(inst.b), skip);

        build.mov(x1, regOp(inst.a)); // TODO: aliasing hazard
        size_t spills = regs.spill(build, index, {x1});
        build.mov(x0, rState);
        build.ldr(x2, mem(rBase, vmRegOp(inst.b) * sizeof(TValue) + offsetof(TValue, value)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaC_barrierf)));
        build.blr(x3);

        regs.restore(build, spills); // need to restore before skip so that registers are in a consistent state

        // note: no emitUpdateBase necessary because luaC_ barriers do not reallocate stack
        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_TABLE_BACK:
    {
        Label skip;
        RegisterA64 temp = regs.allocTemp(KindA64::w);

        // isblack(obj2gco(t))
        build.ldrb(temp, mem(regOp(inst.a), offsetof(GCheader, marked)));
        build.tbz(temp, BLACKBIT, skip);

        build.mov(x1, regOp(inst.a)); // TODO: aliasing hazard
        size_t spills = regs.spill(build, index, {x1});
        build.mov(x0, rState);
        build.add(x2, x1, uint16_t(offsetof(Table, gclist)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaC_barrierback)));
        build.blr(x3);

        regs.restore(build, spills); // need to restore before skip so that registers are in a consistent state

        // note: no emitUpdateBase necessary because luaC_ barriers do not reallocate stack
        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_TABLE_FORWARD:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::x);

        Label skip;
        checkObjectBarrierConditions(build, regOp(inst.a), temp, vmRegOp(inst.b), skip);

        build.mov(x1, regOp(inst.a)); // TODO: aliasing hazard
        size_t spills = regs.spill(build, index, {x1});
        build.mov(x0, rState);
        build.ldr(x2, mem(rBase, vmRegOp(inst.b) * sizeof(TValue) + offsetof(TValue, value)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaC_barriertable)));
        build.blr(x3);

        regs.restore(build, spills); // need to restore before skip so that registers are in a consistent state

        // note: no emitUpdateBase necessary because luaC_ barriers do not reallocate stack
        build.setLabel(skip);
        break;
    }
    case IrCmd::SET_SAVEDPC:
    {
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::x);

        emitAddOffset(build, temp1, rCode, uintOp(inst.a) * sizeof(Instruction));
        build.ldr(temp2, mem(rState, offsetof(lua_State, ci)));
        build.str(temp1, mem(temp2, offsetof(CallInfo, savedpc)));
        break;
    }
    case IrCmd::CLOSE_UPVALS:
    {
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

        build.mov(x1, temp2); // TODO: aliasing hazard
        size_t spills = regs.spill(build, index, {x1});
        build.mov(x0, rState);
        build.ldr(x2, mem(rNativeContext, offsetof(NativeContext, luaF_close)));
        build.blr(x2);

        regs.restore(build, spills); // need to restore before skip so that registers are in a consistent state

        build.setLabel(skip);
        break;
    }
    case IrCmd::CAPTURE:
        // no-op
        break;
    case IrCmd::SETLIST:
        regs.spill(build, index);
        emitFallback(build, LOP_SETLIST, uintOp(inst.a));
        break;
    case IrCmd::CALL:
        regs.spill(build, index);
        // argtop = (nparams == LUA_MULTRET) ? L->top : ra + 1 + nparams;
        if (intOp(inst.b) == LUA_MULTRET)
            build.ldr(x2, mem(rState, offsetof(lua_State, top)));
        else
            build.add(x2, rBase, uint16_t((vmRegOp(inst.a) + 1 + intOp(inst.b)) * sizeof(TValue)));

        // callFallback(L, ra, argtop, nresults)
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.mov(w3, intOp(inst.c));
        build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, callFallback)));
        build.blr(x4);

        // reentry with x0=closure (NULL will trigger exit)
        build.b(helpers.reentry);
        break;
    case IrCmd::RETURN:
        regs.spill(build, index);
        // valend = (n == LUA_MULTRET) ? L->top : ra + n
        if (intOp(inst.b) == LUA_MULTRET)
            build.ldr(x2, mem(rState, offsetof(lua_State, top)));
        else
            build.add(x2, rBase, uint16_t((vmRegOp(inst.a) + intOp(inst.b)) * sizeof(TValue)));
        // returnFallback(L, ra, valend)
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, returnFallback)));
        build.blr(x3);

        // reentry with x0=closure (NULL will trigger exit)
        build.b(helpers.reentry);
        break;
    case IrCmd::FORGLOOP:
        // register layout: ra + 1 = table, ra + 2 = internal index, ra + 3 .. ra + aux = iteration variables
        regs.spill(build, index);
        // clear extra variables since we might have more than two
        if (intOp(inst.b) > 2)
        {
            build.mov(w0, LUA_TNIL);
            for (int i = 2; i < intOp(inst.b); ++i)
                build.str(w0, mem(rBase, (vmRegOp(inst.a) + 3 + i) * sizeof(TValue) + offsetof(TValue, tt)));
        }
        // we use full iter fallback for now; in the future it could be worthwhile to accelerate array iteration here
        build.mov(x0, rState);
        build.ldr(x1, mem(rBase, (vmRegOp(inst.a) + 1) * sizeof(TValue) + offsetof(TValue, value.gc)));
        build.ldr(w2, mem(rBase, (vmRegOp(inst.a) + 2) * sizeof(TValue) + offsetof(TValue, value.p)));
        build.add(x3, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, forgLoopTableIter)));
        build.blr(x4);
        // note: no emitUpdateBase necessary because forgLoopTableIter does not reallocate stack
        build.cbnz(w0, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::FORGLOOP_FALLBACK:
        regs.spill(build, index);
        build.mov(x0, rState);
        build.mov(w1, vmRegOp(inst.a));
        build.mov(w2, intOp(inst.b));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, forgLoopNonTableFallback)));
        build.blr(x3);
        emitUpdateBase(build);
        build.cbnz(w0, labelOp(inst.c));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    case IrCmd::FORGPREP_XNEXT_FALLBACK:
        regs.spill(build, index);
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));
        build.mov(w2, uintOp(inst.a) + 1);
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, forgPrepXnextFallback)));
        build.blr(x3);
        // note: no emitUpdateBase necessary because forgLoopNonTableFallback does not reallocate stack
        jumpOrFallthrough(blockOp(inst.c), next);
        break;
    case IrCmd::COVERAGE:
    {
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::w);
        RegisterA64 temp3 = regs.allocTemp(KindA64::w);

        build.mov(temp1, uintOp(inst.a) * sizeof(Instruction));
        build.ldr(temp2, mem(rCode, temp1));

        // increments E (high 24 bits); if the result overflows a 23-bit counter, high bit becomes 1
        // note: cmp can be eliminated with adds but we aren't concerned with code size for coverage
        build.add(temp3, temp2, 256);
        build.cmp(temp3, 0);
        build.csel(temp2, temp2, temp3, ConditionA64::Less);

        build.str(temp2, mem(rCode, temp1));
        break;
    }

        // Full instruction fallbacks
    case IrCmd::FALLBACK_GETGLOBAL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        regs.spill(build, index);
        emitFallback(build, LOP_GETGLOBAL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETGLOBAL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        regs.spill(build, index);
        emitFallback(build, LOP_SETGLOBAL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETTABLEKS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        regs.spill(build, index);
        emitFallback(build, LOP_GETTABLEKS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETTABLEKS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        regs.spill(build, index);
        emitFallback(build, LOP_SETTABLEKS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_NAMECALL:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.d.kind == IrOpKind::VmConst);

        regs.spill(build, index);
        emitFallback(build, LOP_NAMECALL, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_PREPVARARGS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::Constant);

        regs.spill(build, index);
        emitFallback(build, LOP_PREPVARARGS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETVARARGS:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);

        regs.spill(build, index);
        emitFallback(build, LOP_GETVARARGS, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_NEWCLOSURE:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::Constant);

        regs.spill(build, index);
        emitFallback(build, LOP_NEWCLOSURE, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_DUPCLOSURE:
        LUAU_ASSERT(inst.b.kind == IrOpKind::VmReg);
        LUAU_ASSERT(inst.c.kind == IrOpKind::VmConst);

        regs.spill(build, index);
        emitFallback(build, LOP_DUPCLOSURE, uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_FORGPREP:
        regs.spill(build, index);
        emitFallback(build, LOP_FORGPREP, uintOp(inst.a));
        jumpOrFallthrough(blockOp(inst.c), next);
        break;

    // Pseudo instructions
    case IrCmd::NOP:
    case IrCmd::SUBSTITUTE:
        LUAU_ASSERT(!"Pseudo instructions should not be lowered");
        break;

    case IrCmd::BITAND_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.b.kind == IrOpKind::Constant && AssemblyBuilderA64::isMaskSupported(uintOp(inst.b)))
            build.and_(inst.regA64, regOp(inst.a), uintOp(inst.b));
        else
        {
            RegisterA64 temp1 = tempUint(inst.a);
            RegisterA64 temp2 = tempUint(inst.b);
            build.and_(inst.regA64, temp1, temp2);
        }
        break;
    }
    case IrCmd::BITXOR_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.b.kind == IrOpKind::Constant && AssemblyBuilderA64::isMaskSupported(uintOp(inst.b)))
            build.eor(inst.regA64, regOp(inst.a), uintOp(inst.b));
        else
        {
            RegisterA64 temp1 = tempUint(inst.a);
            RegisterA64 temp2 = tempUint(inst.b);
            build.eor(inst.regA64, temp1, temp2);
        }
        break;
    }
    case IrCmd::BITOR_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.b.kind == IrOpKind::Constant && AssemblyBuilderA64::isMaskSupported(uintOp(inst.b)))
            build.orr(inst.regA64, regOp(inst.a), uintOp(inst.b));
        else
        {
            RegisterA64 temp1 = tempUint(inst.a);
            RegisterA64 temp2 = tempUint(inst.b);
            build.orr(inst.regA64, temp1, temp2);
        }
        break;
    }
    case IrCmd::BITNOT_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a});
        RegisterA64 temp = tempUint(inst.a);
        build.mvn(inst.regA64, temp);
        break;
    }
    case IrCmd::BITLSHIFT_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.b.kind == IrOpKind::Constant)
            build.lsl(inst.regA64, regOp(inst.a), uint8_t(uintOp(inst.b) & 31));
        else
        {
            RegisterA64 temp1 = tempUint(inst.a);
            RegisterA64 temp2 = tempUint(inst.b);
            build.lsl(inst.regA64, temp1, temp2);
        }
        break;
    }
    case IrCmd::BITRSHIFT_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.b.kind == IrOpKind::Constant)
            build.lsr(inst.regA64, regOp(inst.a), uint8_t(uintOp(inst.b) & 31));
        else
        {
            RegisterA64 temp1 = tempUint(inst.a);
            RegisterA64 temp2 = tempUint(inst.b);
            build.lsr(inst.regA64, temp1, temp2);
        }
        break;
    }
    case IrCmd::BITARSHIFT_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.b.kind == IrOpKind::Constant)
            build.asr(inst.regA64, regOp(inst.a), uint8_t(uintOp(inst.b) & 31));
        else
        {
            RegisterA64 temp1 = tempUint(inst.a);
            RegisterA64 temp2 = tempUint(inst.b);
            build.asr(inst.regA64, temp1, temp2);
        }
        break;
    }
    case IrCmd::BITLROTATE_UINT:
    {
        if (inst.b.kind == IrOpKind::Constant)
        {
            inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a});
            build.ror(inst.regA64, regOp(inst.a), uint8_t((32 - uintOp(inst.b)) & 31));
        }
        else
        {
            inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.b}); // can't reuse a because it would be clobbered by neg
            RegisterA64 temp1 = tempUint(inst.a);
            RegisterA64 temp2 = tempUint(inst.b);
            build.neg(inst.regA64, temp2);
            build.ror(inst.regA64, temp1, inst.regA64);
        }
        break;
    }
    case IrCmd::BITRROTATE_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.b.kind == IrOpKind::Constant)
            build.ror(inst.regA64, regOp(inst.a), uint8_t(uintOp(inst.b) & 31));
        else
        {
            RegisterA64 temp1 = tempUint(inst.a);
            RegisterA64 temp2 = tempUint(inst.b);
            build.ror(inst.regA64, temp1, temp2);
        }
        break;
    }
    case IrCmd::BITCOUNTLZ_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a});
        RegisterA64 temp = tempUint(inst.a);
        build.clz(inst.regA64, temp);
        break;
    }
    case IrCmd::BITCOUNTRZ_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a});
        RegisterA64 temp = tempUint(inst.a);
        build.rbit(inst.regA64, temp);
        build.clz(inst.regA64, inst.regA64);
        break;
    }
    case IrCmd::INVOKE_LIBM:
    {
        RegisterA64 temp1 = tempDouble(inst.b);

        build.fmov(d0, temp1); // TODO: aliasing hazard

        if (inst.c.kind != IrOpKind::None)
        {
            RegisterA64 temp2 = tempDouble(inst.c);
            build.fmov(d1, temp2); // TODO: aliasing hazard
            regs.spill(build, index, {d0, d1});
        }
        else
            regs.spill(build, index, {d0});

        build.ldr(x0, mem(rNativeContext, getNativeContextOffset(LuauBuiltinFunction(uintOp(inst.a)))));
        build.blr(x0);
        // TODO: we could takeReg d0 but it's unclear if we will be able to keep d0 allocatable due to aliasing concerns
        inst.regA64 = regs.allocReg(KindA64::d, index);
        build.fmov(inst.regA64, d0);
        break;
    }

    // Unsupported instructions
    // Note: when adding implementations for these, please move the case: label so that implemented instructions match the order in IrData.h
    case IrCmd::STORE_VECTOR:
        error = true;
        break;
    }

    regs.freeLastUseRegs(inst, index);
    regs.freeTempRegs();
}

void IrLoweringA64::finishBlock() {}

bool IrLoweringA64::hasError() const
{
    return error;
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
        double val = doubleOp(op);

        if (AssemblyBuilderA64::isFmovSupported(val))
        {
            RegisterA64 temp = regs.allocTemp(KindA64::d);
            build.fmov(temp, val);
            return temp;
        }
        else
        {
            RegisterA64 temp1 = regs.allocTemp(KindA64::x);
            RegisterA64 temp2 = regs.allocTemp(KindA64::d);
            build.adr(temp1, val);
            build.ldr(temp2, temp1);
            return temp2;
        }
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

RegisterA64 IrLoweringA64::tempUint(IrOp op)
{
    if (op.kind == IrOpKind::Inst)
        return regOp(op);
    else if (op.kind == IrOpKind::Constant)
    {
        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.mov(temp, uintOp(op));
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

        emitAddOffset(build, temp, rConstants, constantOffset);
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

RegisterA64 IrLoweringA64::regOp(IrOp op)
{
    IrInst& inst = function.instOp(op);

    if (inst.spilled)
        regs.restoreReg(build, inst);

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
