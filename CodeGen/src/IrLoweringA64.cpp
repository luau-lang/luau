// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrLoweringA64.h"

#include "Luau/DenseHash.h"
#include "Luau/IrData.h"
#include "Luau/IrUtils.h"
#include "Luau/LoweringStats.h"

#include "EmitCommonA64.h"
#include "NativeState.h"

#include "lstate.h"
#include "lgc.h"

LUAU_FASTFLAG(LuauCodeGenUnassignedBcTargetAbort)
LUAU_FASTFLAG(LuauCodeGenDirectBtest)
LUAU_FASTFLAG(LuauCodeGenRegAutoSpillA64)
LUAU_FASTFLAG(LuauCodegenDirectCompare)

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
        CODEGEN_ASSERT(!"Unexpected condition code");
        return ConditionA64::Always;
    }
}

inline ConditionA64 getConditionInt(IrCondition cond)
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
        return ConditionA64::LessEqual;

    case IrCondition::NotLessEqual:
        return ConditionA64::Greater;

    case IrCondition::Greater:
        return ConditionA64::Greater;

    case IrCondition::NotGreater:
        return ConditionA64::LessEqual;

    case IrCondition::GreaterEqual:
        return ConditionA64::GreaterEqual;

    case IrCondition::NotGreaterEqual:
        return ConditionA64::Less;

    case IrCondition::UnsignedLess:
        return ConditionA64::CarryClear;

    case IrCondition::UnsignedLessEqual:
        return ConditionA64::UnsignedLessEqual;

    case IrCondition::UnsignedGreater:
        return ConditionA64::UnsignedGreater;

    case IrCondition::UnsignedGreaterEqual:
        return ConditionA64::CarrySet;

    default:
        CODEGEN_ASSERT(!"Unexpected condition code");
        return ConditionA64::Always;
    }
}

static void emitAddOffset(AssemblyBuilderA64& build, RegisterA64 dst, RegisterA64 src, size_t offset)
{
    CODEGEN_ASSERT(dst != src);
    CODEGEN_ASSERT(offset <= INT_MAX);

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

static void checkObjectBarrierConditions(AssemblyBuilderA64& build, RegisterA64 object, RegisterA64 temp, IrOp ra, int ratag, Label& skip)
{
    RegisterA64 tempw = castReg(KindA64::w, temp);
    AddressA64 addr = temp;

    // iscollectable(ra)
    if (ratag == -1 || !isGCO(ratag))
    {
        if (ra.kind == IrOpKind::VmReg)
            addr = mem(rBase, vmRegOp(ra) * sizeof(TValue) + offsetof(TValue, tt));
        else if (ra.kind == IrOpKind::VmConst)
            emitAddOffset(build, temp, rConstants, vmConstOp(ra) * sizeof(TValue) + offsetof(TValue, tt));

        build.ldr(tempw, addr);
        build.cmp(tempw, LUA_TSTRING);
        build.b(ConditionA64::Less, skip);
    }

    // isblack(obj2gco(o))
    build.ldrb(tempw, mem(object, offsetof(GCheader, marked)));
    build.tbz(tempw, BLACKBIT, skip);

    // iswhite(gcvalue(ra))
    if (ra.kind == IrOpKind::VmReg)
        addr = mem(rBase, vmRegOp(ra) * sizeof(TValue) + offsetof(TValue, value));
    else if (ra.kind == IrOpKind::VmConst)
        emitAddOffset(build, temp, rConstants, vmConstOp(ra) * sizeof(TValue) + offsetof(TValue, value));

    build.ldr(temp, addr);
    build.ldrb(tempw, mem(temp, offsetof(GCheader, marked)));
    build.tst(tempw, bit2mask(WHITE0BIT, WHITE1BIT));
    build.b(ConditionA64::Equal, skip); // Equal = Zero after tst
}

static void emitAbort(AssemblyBuilderA64& build, Label& abort)
{
    Label skip;
    build.b(skip);
    build.setLabel(abort);
    build.udf();
    build.setLabel(skip);
}

static void emitFallback(AssemblyBuilderA64& build, int offset, int pcpos)
{
    // fallback(L, instruction, base, k)
    build.mov(x0, rState);
    emitAddOffset(build, x1, rCode, pcpos * sizeof(Instruction));
    build.mov(x2, rBase);
    build.mov(x3, rConstants);
    build.ldr(x4, mem(rNativeContext, offset));
    build.blr(x4);

    emitUpdateBase(build);
}

static void emitInvokeLibm1P(AssemblyBuilderA64& build, size_t func, int arg)
{
    CODEGEN_ASSERT(kTempSlots >= 1);
    build.ldr(d0, mem(rBase, arg * sizeof(TValue) + offsetof(TValue, value.n)));
    build.add(x0, sp, sTemporary.data); // sp-relative offset
    build.ldr(x1, mem(rNativeContext, uint32_t(func)));
    build.blr(x1);
}

static bool emitBuiltin(AssemblyBuilderA64& build, IrFunction& function, IrRegAllocA64& regs, int bfid, int res, int arg, int nresults)
{
    switch (bfid)
    {
    case LBF_MATH_FREXP:
    {
        CODEGEN_ASSERT(nresults == 1 || nresults == 2);
        emitInvokeLibm1P(build, offsetof(NativeContext, libm_frexp), arg);
        build.str(d0, mem(rBase, res * sizeof(TValue) + offsetof(TValue, value.n)));

        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.mov(temp, LUA_TNUMBER);
        build.str(temp, mem(rBase, res * sizeof(TValue) + offsetof(TValue, tt)));

        if (nresults == 2)
        {
            build.ldr(w0, sTemporary);
            build.scvtf(d1, w0);
            build.str(d1, mem(rBase, (res + 1) * sizeof(TValue) + offsetof(TValue, value.n)));
            build.str(temp, mem(rBase, (res + 1) * sizeof(TValue) + offsetof(TValue, tt)));
        }
        return true;
    }
    case LBF_MATH_MODF:
    {
        CODEGEN_ASSERT(nresults == 1 || nresults == 2);
        emitInvokeLibm1P(build, offsetof(NativeContext, libm_modf), arg);
        build.ldr(d1, sTemporary);
        build.str(d1, mem(rBase, res * sizeof(TValue) + offsetof(TValue, value.n)));

        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.mov(temp, LUA_TNUMBER);
        build.str(temp, mem(rBase, res * sizeof(TValue) + offsetof(TValue, tt)));

        if (nresults == 2)
        {
            build.str(d0, mem(rBase, (res + 1) * sizeof(TValue) + offsetof(TValue, value.n)));
            build.str(temp, mem(rBase, (res + 1) * sizeof(TValue) + offsetof(TValue, tt)));
        }
        return true;
    }

    default:
        CODEGEN_ASSERT(!"Missing A64 lowering");
        return false;
    }
}

static uint64_t getDoubleBits(double value)
{
    uint64_t result;
    static_assert(sizeof(result) == sizeof(value), "Expecting double to be 64-bit");
    memcpy(&result, &value, sizeof(value));
    return result;
}

IrLoweringA64::IrLoweringA64(AssemblyBuilderA64& build, ModuleHelpers& helpers, IrFunction& function, LoweringStats* stats)
    : build(build)
    , helpers(helpers)
    , function(function)
    , stats(stats)
    , regs(build, function, stats, {{x0, x15}, {x16, x17}, {q0, q7}, {q16, q31}})
    , valueTracker(function)
    , exitHandlerMap(~0u)
{
    valueTracker.setRestoreCallback(
        this,
        [](void* context, IrInst& inst)
        {
            IrLoweringA64* self = static_cast<IrLoweringA64*>(context);
            self->regs.restoreReg(inst);
        }
    );
}

void IrLoweringA64::lowerInst(IrInst& inst, uint32_t index, const IrBlock& next)
{
    if (FFlag::LuauCodeGenRegAutoSpillA64)
        regs.currInstIdx = index;

    valueTracker.beforeInstLowering(inst);

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
    case IrCmd::LOAD_FLOAT:
    {
        inst.regA64 = regs.allocReg(KindA64::d, index);
        RegisterA64 temp = castReg(KindA64::s, inst.regA64); // safe to alias a fresh register
        AddressA64 addr = tempAddr(inst.a, intOp(inst.b));

        build.ldr(temp, addr);
        build.fcvt(inst.regA64, temp);
        break;
    }
    case IrCmd::LOAD_TVALUE:
    {
        inst.regA64 = regs.allocReg(KindA64::q, index);

        int addrOffset = inst.b.kind != IrOpKind::None ? intOp(inst.b) : 0;
        AddressA64 addr = tempAddr(inst.a, addrOffset);
        build.ldr(inst.regA64, addr);
        break;
    }
    case IrCmd::LOAD_ENV:
        inst.regA64 = regs.allocReg(KindA64::x, index);
        build.ldr(inst.regA64, mem(rClosure, offsetof(Closure, env)));
        break;
    case IrCmd::GET_ARR_ADDR:
    {
        inst.regA64 = regs.allocReuse(KindA64::x, index, {inst.a});
        build.ldr(inst.regA64, mem(regOp(inst.a), offsetof(LuaTable, array)));

        if (inst.b.kind == IrOpKind::Inst)
        {
            build.add(inst.regA64, inst.regA64, regOp(inst.b), kTValueSizeLog2); // implicit uxtw
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            if (intOp(inst.b) == 0)
            {
                // no offset required
            }
            else if (intOp(inst.b) * sizeof(TValue) <= AssemblyBuilderA64::kMaxImmediate)
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
            CODEGEN_ASSERT(!"Unsupported instruction form");
        break;
    }
    case IrCmd::GET_SLOT_NODE_ADDR:
    {
        inst.regA64 = regs.allocReuse(KindA64::x, index, {inst.a});
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp1w = castReg(KindA64::w, temp1);
        RegisterA64 temp2 = regs.allocTemp(KindA64::w);
        RegisterA64 temp2x = castReg(KindA64::x, temp2);

        // note: since the stride of the load is the same as the destination register size, we can range check the array index, not the byte offset
        if (uintOp(inst.b) <= AddressA64::kMaxOffset)
            build.ldr(temp1w, mem(rCode, uintOp(inst.b) * sizeof(Instruction)));
        else
        {
            build.mov(temp1, uintOp(inst.b) * sizeof(Instruction));
            build.ldr(temp1w, mem(rCode, temp1));
        }

        // C field can be shifted as long as it's at the most significant byte of the instruction word
        CODEGEN_ASSERT(kOffsetOfInstructionC == 3);
        build.ldrb(temp2, mem(regOp(inst.a), offsetof(LuaTable, nodemask8)));
        build.and_(temp2, temp2, temp1w, -24);

        // note: this may clobber inst.a, so it's important that we don't use it after this
        build.ldr(inst.regA64, mem(regOp(inst.a), offsetof(LuaTable, node)));
        build.add(inst.regA64, inst.regA64, temp2x, kLuaNodeSizeLog2); // "zero extend" temp2 to get a larger shift (top 32 bits are zero)
        break;
    }
    case IrCmd::GET_HASH_NODE_ADDR:
    {
        inst.regA64 = regs.allocReuse(KindA64::x, index, {inst.a});
        RegisterA64 temp1 = regs.allocTemp(KindA64::w);
        RegisterA64 temp2 = regs.allocTemp(KindA64::w);
        RegisterA64 temp2x = castReg(KindA64::x, temp2);

        // hash & ((1 << lsizenode) - 1) == hash & ~(-1 << lsizenode)
        build.mov(temp1, -1);
        build.ldrb(temp2, mem(regOp(inst.a), offsetof(LuaTable, lsizenode)));
        build.lsl(temp1, temp1, temp2);
        build.mov(temp2, uintOp(inst.b));
        build.bic(temp2, temp2, temp1);

        // note: this may clobber inst.a, so it's important that we don't use it after this
        build.ldr(inst.regA64, mem(regOp(inst.a), offsetof(LuaTable, node)));
        build.add(inst.regA64, inst.regA64, temp2x, kLuaNodeSizeLog2); // "zero extend" temp2 to get a larger shift (top 32 bits are zero)
        break;
    }
    case IrCmd::GET_CLOSURE_UPVAL_ADDR:
    {
        inst.regA64 = regs.allocReuse(KindA64::x, index, {inst.a});
        RegisterA64 cl = inst.a.kind == IrOpKind::Undef ? rClosure : regOp(inst.a);

        build.add(inst.regA64, cl, uint16_t(offsetof(Closure, l.uprefs) + sizeof(TValue) * vmUpvalueOp(inst.b)));
        break;
    }
    case IrCmd::STORE_TAG:
    {
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, tt));
        if (tagOp(inst.b) == 0)
        {
            build.str(wzr, addr);
        }
        else
        {
            RegisterA64 temp = regs.allocTemp(KindA64::w);
            build.mov(temp, tagOp(inst.b));
            build.str(temp, addr);
        }
        break;
    }
    case IrCmd::STORE_POINTER:
    {
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value));
        if (inst.b.kind == IrOpKind::Constant)
        {
            CODEGEN_ASSERT(intOp(inst.b) == 0);
            build.str(xzr, addr);
        }
        else
        {
            build.str(regOp(inst.b), addr);
        }
        break;
    }
    case IrCmd::STORE_EXTRA:
    {
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, extra));
        if (intOp(inst.b) == 0)
        {
            build.str(wzr, addr);
        }
        else
        {
            RegisterA64 temp = regs.allocTemp(KindA64::w);
            build.mov(temp, intOp(inst.b));
            build.str(temp, addr);
        }
        break;
    }
    case IrCmd::STORE_DOUBLE:
    {
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value));
        if (inst.b.kind == IrOpKind::Constant && getDoubleBits(doubleOp(inst.b)) == 0)
        {
            build.str(xzr, addr);
        }
        else
        {
            RegisterA64 temp = tempDouble(inst.b);
            build.str(temp, addr);
        }
        break;
    }
    case IrCmd::STORE_INT:
    {
        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value));
        if (inst.b.kind == IrOpKind::Constant && intOp(inst.b) == 0)
        {
            build.str(wzr, addr);
        }
        else
        {
            RegisterA64 temp = tempInt(inst.b);
            build.str(temp, addr);
        }
        break;
    }
    case IrCmd::STORE_VECTOR:
    {
        RegisterA64 temp1 = tempDouble(inst.b);
        RegisterA64 temp2 = tempDouble(inst.c);
        RegisterA64 temp3 = tempDouble(inst.d);
        RegisterA64 temp4 = regs.allocTemp(KindA64::s);

        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value));
        CODEGEN_ASSERT(addr.kind == AddressKindA64::imm && addr.data % 4 == 0 && unsigned(addr.data + 8) / 4 <= AddressA64::kMaxOffset);

        build.fcvt(temp4, temp1);
        build.str(temp4, AddressA64(addr.base, addr.data + 0));
        build.fcvt(temp4, temp2);
        build.str(temp4, AddressA64(addr.base, addr.data + 4));
        build.fcvt(temp4, temp3);
        build.str(temp4, AddressA64(addr.base, addr.data + 8));

        if (inst.e.kind != IrOpKind::None)
        {
            RegisterA64 temp = regs.allocTemp(KindA64::w);
            build.mov(temp, tagOp(inst.e));
            build.str(temp, tempAddr(inst.a, offsetof(TValue, tt)));
        }
        break;
    }
    case IrCmd::STORE_TVALUE:
    {
        int addrOffset = inst.c.kind != IrOpKind::None ? intOp(inst.c) : 0;
        AddressA64 addr = tempAddr(inst.a, addrOffset);
        build.str(regOp(inst.b), addr);
        break;
    }
    case IrCmd::STORE_SPLIT_TVALUE:
    {
        int addrOffset = inst.d.kind != IrOpKind::None ? intOp(inst.d) : 0;

        RegisterA64 tempt = regs.allocTemp(KindA64::w);
        AddressA64 addrt = tempAddr(inst.a, offsetof(TValue, tt) + addrOffset);
        build.mov(tempt, tagOp(inst.b));
        build.str(tempt, addrt);

        AddressA64 addr = tempAddr(inst.a, offsetof(TValue, value) + addrOffset);

        if (tagOp(inst.b) == LUA_TBOOLEAN)
        {
            if (inst.c.kind == IrOpKind::Constant)
            {
                // note: we reuse tag temp register as value for true booleans, and use built-in zero register for false values
                CODEGEN_ASSERT(LUA_TBOOLEAN == 1);
                build.str(intOp(inst.c) ? tempt : wzr, addr);
            }
            else
                build.str(regOp(inst.c), addr);
        }
        else if (tagOp(inst.b) == LUA_TNUMBER)
        {
            RegisterA64 temp = tempDouble(inst.c);
            build.str(temp, addr);
        }
        else if (isGCO(tagOp(inst.b)))
        {
            build.str(regOp(inst.c), addr);
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        break;
    }
    case IrCmd::ADD_INT:
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.b.kind == IrOpKind::Constant && unsigned(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate)
            build.add(inst.regA64, regOp(inst.a), uint16_t(intOp(inst.b)));
        else if (inst.a.kind == IrOpKind::Constant && unsigned(intOp(inst.a)) <= AssemblyBuilderA64::kMaxImmediate)
            build.add(inst.regA64, regOp(inst.b), uint16_t(intOp(inst.a)));
        else
        {
            RegisterA64 temp1 = tempInt(inst.a);
            RegisterA64 temp2 = tempInt(inst.b);
            build.add(inst.regA64, temp1, temp2);
        }
        break;
    case IrCmd::SUB_INT:
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.b.kind == IrOpKind::Constant && unsigned(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate)
            build.sub(inst.regA64, regOp(inst.a), uint16_t(intOp(inst.b)));
        else
        {
            RegisterA64 temp1 = tempInt(inst.a);
            RegisterA64 temp2 = tempInt(inst.b);
            build.sub(inst.regA64, temp1, temp2);
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
    case IrCmd::IDIV_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a, inst.b});
        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);
        build.fdiv(inst.regA64, temp1, temp2);
        build.frintm(inst.regA64, inst.regA64);
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
    case IrCmd::MULADD_NUM:
    {
        RegisterA64 tempA = tempDouble(inst.a);
        RegisterA64 tempB = tempDouble(inst.b);
        RegisterA64 tempC = tempDouble(inst.c);

        if ((build.features & Feature_AdvSIMD) != 0)
        {
            inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.c});
            if (inst.regA64 != tempC)
                build.fmov(inst.regA64, tempC);
            build.fmla(inst.regA64, tempB, tempA);
        }
        else
        {
            inst.regA64 = regs.allocReg(KindA64::d, index);
            build.fmul(inst.regA64, tempB, tempA);
            build.fadd(inst.regA64, inst.regA64, tempC);
        }
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
    case IrCmd::SIGN_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a});

        RegisterA64 temp = tempDouble(inst.a);
        RegisterA64 temp0 = regs.allocTemp(KindA64::d);
        RegisterA64 temp1 = regs.allocTemp(KindA64::d);

        build.fcmpz(temp);
        build.fmov(temp0, 0.0);
        build.fmov(temp1, 1.0);
        build.fcsel(inst.regA64, temp1, temp0, getConditionFP(IrCondition::Greater));
        build.fmov(temp1, -1.0);
        build.fcsel(inst.regA64, temp1, inst.regA64, getConditionFP(IrCondition::Less));
        break;
    }
    case IrCmd::SELECT_NUM:
    {
        inst.regA64 = regs.allocReuse(KindA64::d, index, {inst.a, inst.b, inst.c, inst.d});

        RegisterA64 temp1 = tempDouble(inst.a);
        RegisterA64 temp2 = tempDouble(inst.b);
        RegisterA64 temp3 = tempDouble(inst.c);
        RegisterA64 temp4 = tempDouble(inst.d);

        build.fcmp(temp3, temp4);
        build.fcsel(inst.regA64, temp2, temp1, getConditionFP(IrCondition::Equal));
        break;
    }
    case IrCmd::SELECT_VEC:
    {
        // `inst.b` cannot be reused for return value, because it can be overwritten with A before the first usage
        inst.regA64 = regs.allocReuse(KindA64::q, index, {inst.a, inst.c, inst.d});

        RegisterA64 temp1 = regOp(inst.a);
        RegisterA64 temp2 = regOp(inst.b);
        RegisterA64 temp3 = regOp(inst.c);
        RegisterA64 temp4 = regOp(inst.d);

        RegisterA64 mask = regs.allocTemp(KindA64::q);

        // Evaluate predicate and calculate mask.
        build.fcmeq_4s(mask, temp3, temp4);
        // mov A to res register
        build.mov(inst.regA64, temp1);
        // If numbers are equal override A with B in res register.
        build.bit(inst.regA64, temp2, mask);
        break;
    }
    case IrCmd::MULADD_VEC:
    {
        RegisterA64 tempA = regOp(inst.a);
        RegisterA64 tempB = regOp(inst.b);
        RegisterA64 tempC = regOp(inst.c);

        if ((build.features & Feature_AdvSIMD) != 0)
        {
            inst.regA64 = regs.allocReuse(KindA64::q, index, {inst.c});
            if (inst.regA64 != tempC)
                build.mov(inst.regA64, tempC);
            build.fmla(inst.regA64, tempB, tempA);
        }
        else
        {
            inst.regA64 = regs.allocReg(KindA64::q, index);
            build.fmul(inst.regA64, tempB, tempA);
            build.fadd(inst.regA64, inst.regA64, tempC);
        }
        break;
    }
    case IrCmd::ADD_VEC:
    {
        inst.regA64 = regs.allocReuse(KindA64::q, index, {inst.a, inst.b});

        build.fadd(inst.regA64, regOp(inst.a), regOp(inst.b));
        break;
    }
    case IrCmd::SUB_VEC:
    {
        inst.regA64 = regs.allocReuse(KindA64::q, index, {inst.a, inst.b});

        build.fsub(inst.regA64, regOp(inst.a), regOp(inst.b));
        break;
    }
    case IrCmd::MUL_VEC:
    {
        inst.regA64 = regs.allocReuse(KindA64::q, index, {inst.a, inst.b});

        build.fmul(inst.regA64, regOp(inst.a), regOp(inst.b));
        break;
    }
    case IrCmd::DIV_VEC:
    {
        inst.regA64 = regs.allocReuse(KindA64::q, index, {inst.a, inst.b});

        build.fdiv(inst.regA64, regOp(inst.a), regOp(inst.b));
        break;
    }
    case IrCmd::UNM_VEC:
    {
        inst.regA64 = regs.allocReuse(KindA64::q, index, {inst.a});

        build.fneg(inst.regA64, regOp(inst.a));
        break;
    }
    case IrCmd::DOT_VEC:
    {
        inst.regA64 = regs.allocReg(KindA64::d, index);

        RegisterA64 temp = regs.allocTemp(KindA64::q);
        RegisterA64 temps = castReg(KindA64::s, temp);
        RegisterA64 regs = castReg(KindA64::s, inst.regA64);

        build.fmul(temp, regOp(inst.a), regOp(inst.b));
        build.faddp(regs, temps); // x+y
        build.dup_4s(temp, temp, 2);
        build.fadd(regs, regs, temps); // +z
        build.fcvt(inst.regA64, regs);
        break;
    }
    case IrCmd::NOT_ANY:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});

        if (inst.a.kind == IrOpKind::Constant)
        {
            // other cases should've been constant folded
            CODEGEN_ASSERT(tagOp(inst.a) == LUA_TBOOLEAN);
            build.eor(inst.regA64, regOp(inst.b), 1);
        }
        else
        {
            Label notBool, exit;

            // use the fact that NIL is the only value less than BOOLEAN to do two tag comparisons at once
            CODEGEN_ASSERT(LUA_TNIL == 0 && LUA_TBOOLEAN == 1);
            build.cmp(regOp(inst.a), LUA_TBOOLEAN);
            build.b(ConditionA64::NotEqual, notBool);

            if (inst.b.kind == IrOpKind::Constant)
                build.mov(inst.regA64, intOp(inst.b) == 0 ? 1 : 0);
            else
                build.eor(inst.regA64, regOp(inst.b), 1); // boolean => invert value

            build.b(exit);

            // not boolean => result is true iff tag was nil
            build.setLabel(notBool);
            build.cset(inst.regA64, ConditionA64::Less);

            build.setLabel(exit);
        }
        break;
    }
    case IrCmd::CMP_INT:
    {
        CODEGEN_ASSERT(FFlag::LuauCodeGenDirectBtest);

        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});

        IrCondition cond = conditionOp(inst.c);

        if (inst.a.kind == IrOpKind::Constant)
        {
            build.cmp(regOp(inst.b), intOp(inst.a));
            build.cset(inst.regA64, getInverseCondition(getConditionInt(cond)));
        }
        else if (inst.a.kind == IrOpKind::Inst)
        {
            build.cmp(regOp(inst.a), intOp(inst.b));
            build.cset(inst.regA64, getConditionInt(cond));
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

        regs.spill(index);
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.add(x2, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));

        if (cond == IrCondition::LessEqual)
            build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaV_lessequal)));
        else if (cond == IrCondition::Less)
            build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaV_lessthan)));
        else if (cond == IrCondition::Equal)
            build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaV_equalval)));
        else
            CODEGEN_ASSERT(!"Unsupported condition");

        build.blr(x3);

        emitUpdateBase(build);

        inst.regA64 = regs.takeReg(w0, index);
        break;
    }
    case IrCmd::CMP_TAG:
    {
        CODEGEN_ASSERT(FFlag::LuauCodegenDirectCompare);
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});

        IrCondition cond = conditionOp(inst.c);
        CODEGEN_ASSERT(cond == IrCondition::Equal || cond == IrCondition::NotEqual);
        RegisterA64 aReg = noreg;
        RegisterA64 bReg = noreg;

        if (inst.a.kind == IrOpKind::Inst)
        {
            aReg = regOp(inst.a);
        }
        else if (inst.a.kind == IrOpKind::VmReg)
        {
            aReg = regs.allocTemp(KindA64::w);
            AddressA64 addr = tempAddr(inst.a, offsetof(TValue, tt));
            build.ldr(aReg, addr);
        }
        else
        {
            CODEGEN_ASSERT(inst.a.kind == IrOpKind::Constant);
        }

        if (inst.b.kind == IrOpKind::Inst)
        {
            bReg = regOp(inst.b);
        }
        else if (inst.b.kind == IrOpKind::VmReg)
        {
            bReg = regs.allocTemp(KindA64::w);
            AddressA64 addr = tempAddr(inst.b, offsetof(TValue, tt));
            build.ldr(bReg, addr);
        }
        else
        {
            CODEGEN_ASSERT(inst.b.kind == IrOpKind::Constant);
        }

        if (inst.a.kind == IrOpKind::Constant)
        {
            build.cmp(bReg, tagOp(inst.a));
            build.cset(inst.regA64, getInverseCondition(getConditionInt(cond)));
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            build.cmp(aReg, tagOp(inst.b));
            build.cset(inst.regA64, getConditionInt(cond));
        }
        else
        {
            build.cmp(aReg, bReg);
            build.cset(inst.regA64, getConditionInt(cond));
        }
        break;
    }
    case IrCmd::CMP_SPLIT_TVALUE:
    {
        CODEGEN_ASSERT(FFlag::LuauCodegenDirectCompare);
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});

        // Second operand of this instruction must be a constant
        // Without a constant type, we wouldn't know the correct way to compare the values at lowering time
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::Constant);

        IrCondition cond = conditionOp(inst.e);
        CODEGEN_ASSERT(cond == IrCondition::Equal || cond == IrCondition::NotEqual);

        // Check tag equality first
        RegisterA64 temp = regs.allocTemp(KindA64::w);

        if (inst.a.kind != IrOpKind::Constant)
        {
            build.cmp(regOp(inst.a), tagOp(inst.b));
            build.cset(temp, getConditionInt(cond));
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

            build.cset(inst.regA64, getConditionInt(cond));
        }
        else if (tagOp(inst.b) == LUA_TSTRING)
        {
            build.cmp(regOp(inst.c), regOp(inst.d));
            build.cset(inst.regA64, getConditionInt(cond));
        }
        else if (tagOp(inst.b) == LUA_TNUMBER)
        {
            RegisterA64 temp1 = tempDouble(inst.c);
            RegisterA64 temp2 = tempDouble(inst.d);

            build.fcmp(temp1, temp2);
            build.cset(inst.regA64, getConditionFP(cond));
        }
        else
        {
            CODEGEN_ASSERT(!"unsupported type tag in CMP_SPLIT_TVALUE");
        }

        if (inst.a.kind != IrOpKind::Constant)
        {
            if (cond == IrCondition::Equal)
                build.and_(inst.regA64, inst.regA64, temp);
            else
                build.orr(inst.regA64, inst.regA64, temp);
        }
        break;
    }
    case IrCmd::JUMP:
        if (inst.a.kind == IrOpKind::Undef || inst.a.kind == IrOpKind::VmExit)
        {
            Label fresh;
            build.b(getTargetLabel(inst.a, fresh));
            finalizeTargetLabel(inst.a, fresh);
        }
        else
        {
            jumpOrFallthrough(blockOp(inst.a), next);
        }
        break;
    case IrCmd::JUMP_IF_TRUTHY:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.ldr(temp, mem(rBase, vmRegOp(inst.a) * sizeof(TValue) + offsetof(TValue, tt)));
        // nil => falsy
        CODEGEN_ASSERT(LUA_TNIL == 0);
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
        CODEGEN_ASSERT(LUA_TNIL == 0);
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
    {
        RegisterA64 zr = noreg;

        if (FFlag::LuauCodegenDirectCompare)
        {
            RegisterA64 aReg = noreg;
            RegisterA64 bReg = noreg;

            if (inst.a.kind == IrOpKind::Inst)
            {
                aReg = regOp(inst.a);
            }
            else if (inst.a.kind == IrOpKind::VmReg)
            {
                aReg = regs.allocTemp(KindA64::w);
                AddressA64 addr = tempAddr(inst.a, offsetof(TValue, tt));
                build.ldr(aReg, addr);
            }
            else
            {
                CODEGEN_ASSERT(inst.a.kind == IrOpKind::Constant);
            }

            if (inst.b.kind == IrOpKind::Inst)
            {
                bReg = regOp(inst.b);
            }
            else if (inst.b.kind == IrOpKind::VmReg)
            {
                bReg = regs.allocTemp(KindA64::w);
                AddressA64 addr = tempAddr(inst.b, offsetof(TValue, tt));
                build.ldr(bReg, addr);
            }
            else
            {
                CODEGEN_ASSERT(inst.b.kind == IrOpKind::Constant);
            }

            if (inst.a.kind == IrOpKind::Constant && tagOp(inst.a) == 0)
                zr = bReg;
            else if (inst.b.kind == IrOpKind::Constant && tagOp(inst.b) == 0)
                zr = aReg;
            else if (inst.b.kind == IrOpKind::Constant)
                build.cmp(aReg, tagOp(inst.b));
            else if (inst.a.kind == IrOpKind::Constant)
                build.cmp(bReg, tagOp(inst.a));
            else
                build.cmp(aReg, bReg);
        }
        else
        {
            if (inst.a.kind == IrOpKind::Constant && tagOp(inst.a) == 0)
                zr = regOp(inst.b);
            else if (inst.b.kind == IrOpKind::Constant && tagOp(inst.b) == 0)
                zr = regOp(inst.a);
            else if (inst.a.kind == IrOpKind::Inst && inst.b.kind == IrOpKind::Constant)
                build.cmp(regOp(inst.a), tagOp(inst.b));
            else if (inst.a.kind == IrOpKind::Inst && inst.b.kind == IrOpKind::Inst)
                build.cmp(regOp(inst.a), regOp(inst.b));
            else if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Inst)
                build.cmp(regOp(inst.b), tagOp(inst.a));
            else
                CODEGEN_ASSERT(!"Unsupported instruction form");
        }

        if (isFallthroughBlock(blockOp(inst.d), next))
        {
            if (zr != noreg)
                build.cbz(zr, labelOp(inst.c));
            else
                build.b(ConditionA64::Equal, labelOp(inst.c));
            jumpOrFallthrough(blockOp(inst.d), next);
        }
        else
        {
            if (zr != noreg)
                build.cbnz(zr, labelOp(inst.d));
            else
                build.b(ConditionA64::NotEqual, labelOp(inst.d));
            jumpOrFallthrough(blockOp(inst.c), next);
        }
        break;
    }
    case IrCmd::JUMP_CMP_INT:
    {
        IrCondition cond = conditionOp(inst.c);

        if (cond == IrCondition::Equal && intOp(inst.b) == 0)
        {
            build.cbz(regOp(inst.a), labelOp(inst.d));
        }
        else if (cond == IrCondition::NotEqual && intOp(inst.b) == 0)
        {
            build.cbnz(regOp(inst.a), labelOp(inst.d));
        }
        else
        {
            CODEGEN_ASSERT(unsigned(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate);
            build.cmp(regOp(inst.a), uint16_t(intOp(inst.b)));
            build.b(getConditionInt(cond), labelOp(inst.d));
        }
        jumpOrFallthrough(blockOp(inst.e), next);
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
    case IrCmd::JUMP_FORN_LOOP_COND:
    {
        RegisterA64 index = tempDouble(inst.a);
        RegisterA64 limit = tempDouble(inst.b);
        RegisterA64 step = tempDouble(inst.c);

        Label direct;

        // step > 0
        build.fcmpz(step);
        build.b(getConditionFP(IrCondition::Greater), direct);

        // !(limit <= index)
        build.fcmp(limit, index);
        build.b(getConditionFP(IrCondition::NotLessEqual), labelOp(inst.e));
        build.b(labelOp(inst.d));

        // !(index <= limit)
        build.setLabel(direct);

        build.fcmp(index, limit);
        build.b(getConditionFP(IrCondition::NotLessEqual), labelOp(inst.e));
        jumpOrFallthrough(blockOp(inst.d), next);
        break;
    }
    // IrCmd::JUMP_SLOT_MATCH implemented below
    case IrCmd::TABLE_LEN:
    {
        RegisterA64 reg = regOp(inst.a); // note: we need to call regOp before spill so that we don't do redundant reloads
        regs.spill(index, {reg});
        build.mov(x0, reg);
        build.ldr(x1, mem(rNativeContext, offsetof(NativeContext, luaH_getn)));
        build.blr(x1);

        inst.regA64 = regs.takeReg(w0, index);
        break;
    }
    case IrCmd::STRING_LEN:
    {
        inst.regA64 = regs.allocReg(KindA64::w, index);

        build.ldr(inst.regA64, mem(regOp(inst.a), offsetof(TString, len)));
        break;
    }
    case IrCmd::TABLE_SETNUM:
    {
        // note: we need to call regOp before spill so that we don't do redundant reloads
        RegisterA64 table = regOp(inst.a);
        RegisterA64 key = regOp(inst.b);
        RegisterA64 temp = regs.allocTemp(KindA64::w);

        regs.spill(index, {table, key});

        if (w1 != key)
        {
            build.mov(x1, table);
            build.mov(w2, key);
        }
        else
        {
            build.mov(temp, w1);
            build.mov(x1, table);
            build.mov(w2, temp);
        }

        build.mov(x0, rState);
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaH_setnum)));
        build.blr(x3);
        inst.regA64 = regs.takeReg(x0, index);
        break;
    }
    case IrCmd::NEW_TABLE:
    {
        regs.spill(index);
        build.mov(x0, rState);
        build.mov(x1, uintOp(inst.a));
        build.mov(x2, uintOp(inst.b));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaH_new)));
        build.blr(x3);
        inst.regA64 = regs.takeReg(x0, index);
        break;
    }
    case IrCmd::DUP_TABLE:
    {
        RegisterA64 reg = regOp(inst.a); // note: we need to call regOp before spill so that we don't do redundant reloads
        regs.spill(index, {reg});
        build.mov(x1, reg);
        build.mov(x0, rState);
        build.ldr(x2, mem(rNativeContext, offsetof(NativeContext, luaH_clone)));
        build.blr(x2);
        inst.regA64 = regs.takeReg(x0, index);
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

        build.ldr(temp1, mem(regOp(inst.a), offsetof(LuaTable, metatable)));
        build.cbz(temp1, labelOp(inst.c)); // no metatable

        build.ldrb(temp2, mem(temp1, offsetof(LuaTable, tmcache)));
        build.tst(temp2, 1 << intOp(inst.b));             // can't use tbz/tbnz because their jump offsets are too short
        build.b(ConditionA64::NotEqual, labelOp(inst.c)); // Equal = Zero after tst; tmcache caches *absence* of metamethods

        regs.spill(index, {temp1});
        build.mov(x0, temp1);
        build.mov(w1, intOp(inst.b));
        build.ldr(x2, mem(rGlobalState, offsetof(global_State, tmname) + intOp(inst.b) * sizeof(TString*)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaT_gettm)));
        build.blr(x3);

        build.cbz(x0, labelOp(inst.c)); // no tag method

        inst.regA64 = regs.takeReg(x0, index);
        break;
    }
    case IrCmd::NEW_USERDATA:
    {
        regs.spill(index);
        build.mov(x0, rState);
        build.mov(x1, intOp(inst.a));
        build.mov(x2, intOp(inst.b));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, newUserdata)));
        build.blr(x3);
        inst.regA64 = regs.takeReg(x0, index);
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
        // note: we don't use fcvtzu for consistency with C++ code
        build.fcvtzs(castReg(KindA64::x, inst.regA64), temp);
        break;
    }
    case IrCmd::NUM_TO_VEC:
    {
        inst.regA64 = regs.allocReg(KindA64::q, index);

        if (inst.a.kind == IrOpKind::Constant)
        {
            float value = float(doubleOp(inst.a));
            uint32_t asU32;
            static_assert(sizeof(asU32) == sizeof(value), "Expecting float to be 32-bit");
            memcpy(&asU32, &value, sizeof(value));

            if (AssemblyBuilderA64::isFmovSupported(value))
            {
                build.fmov(inst.regA64, value);
            }
            else
            {
                RegisterA64 temp = regs.allocTemp(KindA64::x);

                uint32_t vec[4] = {asU32, asU32, asU32, 0};
                build.adr(temp, vec, sizeof(vec));
                build.ldr(inst.regA64, temp);
            }
        }
        else
        {
            RegisterA64 tempd = tempDouble(inst.a);
            RegisterA64 temps = regs.allocTemp(KindA64::s);

            build.fcvt(temps, tempd);
            build.dup_4s(inst.regA64, castReg(KindA64::q, temps), 0);
        }
        break;
    }
    case IrCmd::TAG_VECTOR:
    {
        inst.regA64 = regs.allocReuse(KindA64::q, index, {inst.a});

        RegisterA64 reg = regOp(inst.a);
        RegisterA64 tempw = regs.allocTemp(KindA64::w);

        if (inst.regA64 != reg)
            build.mov(inst.regA64, reg);

        build.mov(tempw, LUA_TVECTOR);
        build.ins_4s(inst.regA64, tempw, 3);
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
            build.add(temp, temp, regOp(inst.b), kTValueSizeLog2); // implicit uxtw
            build.str(temp, mem(rState, offsetof(lua_State, top)));
        }
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");
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
        regs.spill(index);

        error |= !emitBuiltin(build, function, regs, uintOp(inst.a), vmRegOp(inst.b), vmRegOp(inst.c), intOp(inst.d));
        break;
    case IrCmd::INVOKE_FASTCALL:
    {
        // We might need a temporary and we have to preserve it over the spill
        RegisterA64 temp = regs.allocTemp(KindA64::q);
        regs.spill(index, {temp});

        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));
        build.add(x2, rBase, uint16_t(vmRegOp(inst.c) * sizeof(TValue)));
        build.mov(w3, intOp(inst.g)); // nresults

        // 'E' argument can only be produced by LOP_FASTCALL3 lowering
        if (inst.e.kind != IrOpKind::Undef)
        {
            CODEGEN_ASSERT(intOp(inst.f) == 3);

            build.ldr(x4, mem(rState, offsetof(lua_State, top)));

            build.ldr(temp, mem(rBase, vmRegOp(inst.d) * sizeof(TValue)));
            build.str(temp, mem(x4, 0));

            build.ldr(temp, mem(rBase, vmRegOp(inst.e) * sizeof(TValue)));
            build.str(temp, mem(x4, sizeof(TValue)));
        }
        else
        {
            if (inst.d.kind == IrOpKind::VmReg)
                build.add(x4, rBase, uint16_t(vmRegOp(inst.d) * sizeof(TValue)));
            else if (inst.d.kind == IrOpKind::VmConst)
                emitAddOffset(build, x4, rConstants, vmConstOp(inst.d) * sizeof(TValue));
            else
                CODEGEN_ASSERT(inst.d.kind == IrOpKind::Undef);
        }

        // nparams
        if (intOp(inst.f) == LUA_MULTRET)
        {
            // L->top - (ra + 1)
            build.ldr(x5, mem(rState, offsetof(lua_State, top)));
            build.sub(x5, x5, rBase);
            build.sub(x5, x5, uint16_t((vmRegOp(inst.b) + 1) * sizeof(TValue)));
            build.lsr(x5, x5, kTValueSizeLog2);
        }
        else
            build.mov(w5, intOp(inst.f));

        build.ldr(x6, mem(rNativeContext, offsetof(NativeContext, luauF_table) + uintOp(inst.a) * sizeof(luau_FastFunction)));
        build.blr(x6);

        inst.regA64 = regs.takeReg(w0, index);
        break;
    }
    case IrCmd::CHECK_FASTCALL_RES:
        build.cmp(regOp(inst.a), 0);
        build.b(ConditionA64::Less, labelOp(inst.b));
        break;
    case IrCmd::DO_ARITH:
        regs.spill(index);
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));

        if (inst.b.kind == IrOpKind::VmConst)
            emitAddOffset(build, x2, rConstants, vmConstOp(inst.b) * sizeof(TValue));
        else
            build.add(x2, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));

        if (inst.c.kind == IrOpKind::VmConst)
            emitAddOffset(build, x3, rConstants, vmConstOp(inst.c) * sizeof(TValue));
        else
            build.add(x3, rBase, uint16_t(vmRegOp(inst.c) * sizeof(TValue)));

        switch (TMS(intOp(inst.d)))
        {
        case TM_ADD:
            build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_doarithadd)));
            break;
        case TM_SUB:
            build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_doarithsub)));
            break;
        case TM_MUL:
            build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_doarithmul)));
            break;
        case TM_DIV:
            build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_doarithdiv)));
            break;
        case TM_IDIV:
            build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_doarithidiv)));
            break;
        case TM_MOD:
            build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_doarithmod)));
            break;
        case TM_POW:
            build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_doarithpow)));
            break;
        case TM_UNM:
            build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_doarithunm)));
            break;
        default:
            CODEGEN_ASSERT(!"Invalid doarith helper operation tag");
            break;
        }

        build.blr(x4);

        emitUpdateBase(build);
        break;
    case IrCmd::DO_LEN:
        regs.spill(index);
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.add(x2, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaV_dolen)));
        build.blr(x3);

        emitUpdateBase(build);
        break;
    case IrCmd::GET_TABLE:
        regs.spill(index);
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));

        if (inst.c.kind == IrOpKind::VmReg)
            build.add(x2, rBase, uint16_t(vmRegOp(inst.c) * sizeof(TValue)));
        else if (inst.c.kind == IrOpKind::Constant)
        {
            TValue n = {};
            setnvalue(&n, uintOp(inst.c));
            build.adr(x2, &n, sizeof(n));
        }
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");

        build.add(x3, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_gettable)));
        build.blr(x4);

        emitUpdateBase(build);
        break;
    case IrCmd::SET_TABLE:
        regs.spill(index);
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.b) * sizeof(TValue)));

        if (inst.c.kind == IrOpKind::VmReg)
            build.add(x2, rBase, uint16_t(vmRegOp(inst.c) * sizeof(TValue)));
        else if (inst.c.kind == IrOpKind::Constant)
        {
            TValue n = {};
            setnvalue(&n, uintOp(inst.c));
            build.adr(x2, &n, sizeof(n));
        }
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");

        build.add(x3, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaV_settable)));
        build.blr(x4);

        emitUpdateBase(build);
        break;
    case IrCmd::GET_CACHED_IMPORT:
    {
        regs.spill(index);

        Label skip, exit;

        RegisterA64 tempTag = regs.allocTemp(KindA64::w);

        AddressA64 addrConstTag = tempAddr(inst.b, offsetof(TValue, tt));
        build.ldr(tempTag, addrConstTag);

        // If the constant for the import is set, we will use it directly, otherwise we have to call an import path lookup function
        CODEGEN_ASSERT(LUA_TNIL == 0);
        build.cbnz(tempTag, skip);

        {
            build.mov(x0, rState);
            build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
            build.mov(w2, importOp(inst.c));
            build.mov(w3, uintOp(inst.d));
            build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, getImport)));
            build.blr(x4);

            emitUpdateBase(build);
            build.b(exit);
        }

        build.setLabel(skip);

        RegisterA64 tempTv = regs.allocTemp(KindA64::q);

        AddressA64 addrConst = tempAddr(inst.b, 0);
        build.ldr(tempTv, addrConst);

        AddressA64 addrReg = tempAddr(inst.a, 0);
        build.str(tempTv, addrReg);

        build.setLabel(exit);
        break;
    }
    case IrCmd::CONCAT:
        regs.spill(index);
        build.mov(x0, rState);
        build.mov(w1, uintOp(inst.b));
        build.mov(w2, vmRegOp(inst.a) + uintOp(inst.b) - 1);
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

        if (inst.c.kind == IrOpKind::Undef || isGCO(tagOp(inst.c)))
        {
            Label skip;
            checkObjectBarrierConditions(build, temp1, temp2, inst.b, inst.c.kind == IrOpKind::Undef ? -1 : tagOp(inst.c), skip);

            size_t spills = regs.spill(index, {temp1});

            build.mov(x1, temp1);
            build.mov(x0, rState);
            build.ldr(x2, mem(rBase, vmRegOp(inst.b) * sizeof(TValue) + offsetof(TValue, value)));
            build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaC_barrierf)));
            build.blr(x3);

            regs.restore(spills); // need to restore before skip so that registers are in a consistent state

            // note: no emitUpdateBase necessary because luaC_ barriers do not reallocate stack
            build.setLabel(skip);
        }
        break;
    }
    case IrCmd::CHECK_TAG:
    {
        Label fresh; // used when guard aborts execution or jumps to a VM exit
        Label& fail = getTargetLabel(inst.c, fresh);

        if (tagOp(inst.b) == 0)
        {
            build.cbnz(regOp(inst.a), fail);
        }
        else
        {
            build.cmp(regOp(inst.a), tagOp(inst.b));
            build.b(ConditionA64::NotEqual, fail);
        }

        finalizeTargetLabel(inst.c, fresh);
        break;
    }
    case IrCmd::CHECK_TRUTHY:
    {
        // Constant tags which don't require boolean value check should've been removed in constant folding
        CODEGEN_ASSERT(inst.a.kind != IrOpKind::Constant || tagOp(inst.a) == LUA_TBOOLEAN);

        Label fresh; // used when guard aborts execution or jumps to a VM exit
        Label& target = getTargetLabel(inst.c, fresh);

        Label skip;

        if (inst.a.kind != IrOpKind::Constant)
        {
            // fail to fallback on 'nil' (falsy)
            CODEGEN_ASSERT(LUA_TNIL == 0);
            build.cbz(regOp(inst.a), target);

            // skip value test if it's not a boolean (truthy)
            build.cmp(regOp(inst.a), LUA_TBOOLEAN);
            build.b(ConditionA64::NotEqual, skip);
        }

        // fail to fallback on 'false' boolean value (falsy)
        if (inst.b.kind != IrOpKind::Constant)
        {
            build.cbz(regOp(inst.b), target);
        }
        else
        {
            if (intOp(inst.b) == 0)
                build.b(target);
        }

        if (inst.a.kind != IrOpKind::Constant)
            build.setLabel(skip);

        finalizeTargetLabel(inst.c, fresh);
        break;
    }
    case IrCmd::CHECK_READONLY:
    {
        Label fresh; // used when guard aborts execution or jumps to a VM exit
        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.ldrb(temp, mem(regOp(inst.a), offsetof(LuaTable, readonly)));
        build.cbnz(temp, getTargetLabel(inst.b, fresh));
        finalizeTargetLabel(inst.b, fresh);
        break;
    }
    case IrCmd::CHECK_NO_METATABLE:
    {
        Label fresh; // used when guard aborts execution or jumps to a VM exit
        RegisterA64 temp = regs.allocTemp(KindA64::x);
        build.ldr(temp, mem(regOp(inst.a), offsetof(LuaTable, metatable)));
        build.cbnz(temp, getTargetLabel(inst.b, fresh));
        finalizeTargetLabel(inst.b, fresh);
        break;
    }
    case IrCmd::CHECK_SAFE_ENV:
    {
        Label fresh; // used when guard aborts execution or jumps to a VM exit
        RegisterA64 temp = regs.allocTemp(KindA64::x);
        RegisterA64 tempw = castReg(KindA64::w, temp);
        build.ldr(temp, mem(rClosure, offsetof(Closure, env)));
        build.ldrb(tempw, mem(temp, offsetof(LuaTable, safeenv)));
        build.cbz(tempw, getTargetLabel(inst.a, fresh));
        finalizeTargetLabel(inst.a, fresh);
        break;
    }
    case IrCmd::CHECK_ARRAY_SIZE:
    {
        Label fresh; // used when guard aborts execution or jumps to a VM exit
        Label& fail = getTargetLabel(inst.c, fresh);

        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.ldr(temp, mem(regOp(inst.a), offsetof(LuaTable, sizearray)));

        if (inst.b.kind == IrOpKind::Inst)
        {
            build.cmp(temp, regOp(inst.b));
            build.b(ConditionA64::UnsignedLessEqual, fail);
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            if (intOp(inst.b) == 0)
            {
                build.cbz(temp, fail);
            }
            else if (size_t(intOp(inst.b)) <= AssemblyBuilderA64::kMaxImmediate)
            {
                build.cmp(temp, uint16_t(intOp(inst.b)));
                build.b(ConditionA64::UnsignedLessEqual, fail);
            }
            else
            {
                RegisterA64 temp2 = regs.allocTemp(KindA64::w);
                build.mov(temp2, intOp(inst.b));
                build.cmp(temp, temp2);
                build.b(ConditionA64::UnsignedLessEqual, fail);
            }
        }
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");

        finalizeTargetLabel(inst.c, fresh);
        break;
    }
    case IrCmd::JUMP_SLOT_MATCH:
    case IrCmd::CHECK_SLOT_MATCH:
    {
        Label abort; // used when guard aborts execution
        const IrOp& mismatchOp = inst.cmd == IrCmd::JUMP_SLOT_MATCH ? inst.d : inst.c;
        Label& mismatch = mismatchOp.kind == IrOpKind::Undef ? abort : labelOp(mismatchOp);

        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp1w = castReg(KindA64::w, temp1);
        RegisterA64 temp2 = regs.allocTemp(KindA64::x);

        static_assert(offsetof(LuaNode, key.value) == offsetof(LuaNode, key) && kOffsetOfTKeyTagNext >= 8 && kOffsetOfTKeyTagNext < 16);
        build.ldp(temp1, temp2, mem(regOp(inst.a), offsetof(LuaNode, key))); // load key.value into temp1 and key.tt (alongside other bits) into temp2
        build.ubfx(temp2, temp2, (kOffsetOfTKeyTagNext - 8) * 8, kTKeyTagBits); // .tt is right before .next, and 8 bytes are skipped by ldp
        build.cmp(temp2, LUA_TSTRING);
        build.b(ConditionA64::NotEqual, mismatch);

        AddressA64 addr = tempAddr(inst.b, offsetof(TValue, value));
        build.ldr(temp2, addr);
        build.cmp(temp1, temp2);
        build.b(ConditionA64::NotEqual, mismatch);

        build.ldr(temp1w, mem(regOp(inst.a), offsetof(LuaNode, val.tt)));
        CODEGEN_ASSERT(LUA_TNIL == 0);
        build.cbz(temp1w, mismatch);

        if (inst.cmd == IrCmd::JUMP_SLOT_MATCH)
            jumpOrFallthrough(blockOp(inst.c), next);
        else if (abort.id)
            emitAbort(build, abort);
        break;
    }
    case IrCmd::CHECK_NODE_NO_NEXT:
    {
        Label fresh; // used when guard aborts execution or jumps to a VM exit
        RegisterA64 temp = regs.allocTemp(KindA64::w);

        build.ldr(temp, mem(regOp(inst.a), offsetof(LuaNode, key) + kOffsetOfTKeyTagNext));
        build.lsr(temp, temp, kTKeyTagBits);
        build.cbnz(temp, getTargetLabel(inst.b, fresh));
        finalizeTargetLabel(inst.b, fresh);
        break;
    }
    case IrCmd::CHECK_NODE_VALUE:
    {
        Label fresh; // used when guard aborts execution or jumps to a VM exit
        RegisterA64 temp = regs.allocTemp(KindA64::w);

        build.ldr(temp, mem(regOp(inst.a), offsetof(LuaNode, val.tt)));
        CODEGEN_ASSERT(LUA_TNIL == 0);
        build.cbz(temp, getTargetLabel(inst.b, fresh));
        finalizeTargetLabel(inst.b, fresh);
        break;
    }
    case IrCmd::CHECK_BUFFER_LEN:
    {
        int accessSize = intOp(inst.c);
        CODEGEN_ASSERT(accessSize > 0 && accessSize <= int(AssemblyBuilderA64::kMaxImmediate));

        Label fresh; // used when guard aborts execution or jumps to a VM exit
        Label& target = getTargetLabel(inst.d, fresh);

        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.ldr(temp, mem(regOp(inst.a), offsetof(Buffer, len)));

        if (inst.b.kind == IrOpKind::Inst)
        {
            if (accessSize == 1)
            {
                // fails if offset >= len
                build.cmp(temp, regOp(inst.b));
                build.b(ConditionA64::UnsignedLessEqual, target);
            }
            else
            {
                // fails if offset + size > len; we compute it as len - offset < size
                RegisterA64 tempx = castReg(KindA64::x, temp);
                build.sub(tempx, tempx, regOp(inst.b)); // implicit uxtw
                build.cmp(tempx, uint16_t(accessSize));
                build.b(ConditionA64::Less, target); // note: this is a signed 64-bit comparison so that out of bounds offset fails
            }
        }
        else if (inst.b.kind == IrOpKind::Constant)
        {
            int offset = intOp(inst.b);

            // Constant folding can take care of it, but for safety we avoid overflow/underflow cases here
            if (offset < 0 || unsigned(offset) + unsigned(accessSize) >= unsigned(INT_MAX))
            {
                build.b(target);
            }
            else if (offset + accessSize <= int(AssemblyBuilderA64::kMaxImmediate))
            {
                build.cmp(temp, uint16_t(offset + accessSize));
                build.b(ConditionA64::UnsignedLessEqual, target);
            }
            else
            {
                RegisterA64 temp2 = regs.allocTemp(KindA64::w);
                build.mov(temp2, offset + accessSize);
                build.cmp(temp, temp2);
                build.b(ConditionA64::UnsignedLessEqual, target);
            }
        }
        else
        {
            CODEGEN_ASSERT(!"Unsupported instruction form");
        }
        finalizeTargetLabel(inst.d, fresh);
        break;
    }
    case IrCmd::CHECK_USERDATA_TAG:
    {
        Label fresh; // used when guard aborts execution or jumps to a VM exit
        Label& fail = getTargetLabel(inst.c, fresh);
        RegisterA64 temp = regs.allocTemp(KindA64::w);
        build.ldrb(temp, mem(regOp(inst.a), offsetof(Udata, tag)));
        build.cmp(temp, intOp(inst.b));
        build.b(ConditionA64::NotEqual, fail);
        finalizeTargetLabel(inst.c, fresh);
        break;
    }
    case IrCmd::INTERRUPT:
    {
        regs.spill(index);

        Label self;

        build.ldr(x0, mem(rGlobalState, offsetof(global_State, cb.interrupt)));
        build.cbnz(x0, self);

        Label next = build.setLabel();

        interruptHandlers.push_back({self, uintOp(inst.a), next});
        break;
    }
    case IrCmd::CHECK_GC:
    {
        RegisterA64 temp1 = regs.allocTemp(KindA64::x);
        RegisterA64 temp2 = regs.allocTemp(KindA64::x);

        static_assert(offsetof(global_State, totalbytes) == offsetof(global_State, GCthreshold) + sizeof(global_State::GCthreshold));
        Label skip;
        build.ldp(temp1, temp2, mem(rGlobalState, offsetof(global_State, GCthreshold)));
        build.cmp(temp1, temp2);
        build.b(ConditionA64::UnsignedGreater, skip);

        size_t spills = regs.spill(index);

        build.mov(x0, rState);
        build.mov(w1, 1);
        build.ldr(x2, mem(rNativeContext, offsetof(NativeContext, luaC_step)));
        build.blr(x2);

        emitUpdateBase(build);

        regs.restore(spills); // need to restore before skip so that registers are in a consistent state

        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_OBJ:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::x);

        Label skip;
        checkObjectBarrierConditions(build, regOp(inst.a), temp, inst.b, inst.c.kind == IrOpKind::Undef ? -1 : tagOp(inst.c), skip);

        RegisterA64 reg = regOp(inst.a); // note: we need to call regOp before spill so that we don't do redundant reloads
        size_t spills = regs.spill(index, {reg});
        build.mov(x1, reg);
        build.mov(x0, rState);
        build.ldr(x2, mem(rBase, vmRegOp(inst.b) * sizeof(TValue) + offsetof(TValue, value)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaC_barrierf)));
        build.blr(x3);

        regs.restore(spills); // need to restore before skip so that registers are in a consistent state

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

        RegisterA64 reg = regOp(inst.a); // note: we need to call regOp before spill so that we don't do redundant reloads
        size_t spills = regs.spill(index, {reg});
        build.mov(x1, reg);
        build.mov(x0, rState);
        build.add(x2, x1, uint16_t(offsetof(LuaTable, gclist)));
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaC_barrierback)));
        build.blr(x3);

        regs.restore(spills); // need to restore before skip so that registers are in a consistent state

        // note: no emitUpdateBase necessary because luaC_ barriers do not reallocate stack
        build.setLabel(skip);
        break;
    }
    case IrCmd::BARRIER_TABLE_FORWARD:
    {
        RegisterA64 temp = regs.allocTemp(KindA64::x);

        Label skip;
        checkObjectBarrierConditions(build, regOp(inst.a), temp, inst.b, inst.c.kind == IrOpKind::Undef ? -1 : tagOp(inst.c), skip);

        RegisterA64 reg = regOp(inst.a); // note: we need to call regOp before spill so that we don't do redundant reloads
        AddressA64 addr = tempAddr(inst.b, offsetof(TValue, value));
        size_t spills = regs.spill(index, {reg});
        build.mov(x1, reg);
        build.mov(x0, rState);
        build.ldr(x2, addr);
        build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, luaC_barriertable)));
        build.blr(x3);

        regs.restore(spills); // need to restore before skip so that registers are in a consistent state

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

        // ra <= L->openupval->v
        build.ldr(temp1, mem(temp1, offsetof(UpVal, v)));
        build.add(temp2, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.cmp(temp2, temp1);
        build.b(ConditionA64::UnsignedGreater, skip);

        size_t spills = regs.spill(index, {temp2});
        build.mov(x1, temp2);
        build.mov(x0, rState);
        build.ldr(x2, mem(rNativeContext, offsetof(NativeContext, luaF_close)));
        build.blr(x2);

        regs.restore(spills); // need to restore before skip so that registers are in a consistent state

        build.setLabel(skip);
        break;
    }
    case IrCmd::CAPTURE:
        // no-op
        break;
    case IrCmd::SETLIST:
        regs.spill(index);
        emitFallback(build, offsetof(NativeContext, executeSETLIST), uintOp(inst.a));
        break;
    case IrCmd::CALL:
        regs.spill(index);
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

        emitUpdateBase(build);

        // reentry with x0=closure (NULL implies C function; CALL_FALLBACK_YIELD will trigger exit)
        build.cbnz(x0, helpers.continueCall);
        break;
    case IrCmd::RETURN:
        regs.spill(index);

        if (function.variadic)
        {
            build.ldr(x1, mem(rState, offsetof(lua_State, ci)));
            build.ldr(x1, mem(x1, offsetof(CallInfo, func)));
        }
        else if (intOp(inst.b) != 1)
            build.sub(x1, rBase, sizeof(TValue)); // invariant: ci->func + 1 == ci->base for non-variadic frames

        if (intOp(inst.b) == 0)
        {
            build.mov(w2, 0);
            build.b(helpers.return_);
        }
        else if (intOp(inst.b) == 1 && !function.variadic)
        {
            // fast path: minimizes x1 adjustments
            // note that we skipped x1 computation for this specific case above
            build.ldr(q0, mem(rBase, vmRegOp(inst.a) * sizeof(TValue)));
            build.str(q0, mem(rBase, -int(sizeof(TValue))));
            build.mov(x1, rBase);
            build.mov(w2, 1);
            build.b(helpers.return_);
        }
        else if (intOp(inst.b) >= 1 && intOp(inst.b) <= 3)
        {
            for (int r = 0; r < intOp(inst.b); ++r)
            {
                build.ldr(q0, mem(rBase, (vmRegOp(inst.a) + r) * sizeof(TValue)));
                build.str(q0, mem(x1, sizeof(TValue), AddressKindA64::post));
            }
            build.mov(w2, intOp(inst.b));
            build.b(helpers.return_);
        }
        else
        {
            build.mov(w2, 0);

            // vali = ra
            build.add(x3, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));

            // valend = (n == LUA_MULTRET) ? L->top : ra + n
            if (intOp(inst.b) == LUA_MULTRET)
                build.ldr(x4, mem(rState, offsetof(lua_State, top)));
            else
                build.add(x4, rBase, uint16_t((vmRegOp(inst.a) + intOp(inst.b)) * sizeof(TValue)));

            Label repeatValueLoop, exitValueLoop;

            if (intOp(inst.b) == LUA_MULTRET)
            {
                build.cmp(x3, x4);
                build.b(ConditionA64::CarrySet, exitValueLoop); // CarrySet == UnsignedGreaterEqual
            }

            build.setLabel(repeatValueLoop);
            build.ldr(q0, mem(x3, sizeof(TValue), AddressKindA64::post));
            build.str(q0, mem(x1, sizeof(TValue), AddressKindA64::post));
            build.add(w2, w2, 1);
            build.cmp(x3, x4);
            build.b(ConditionA64::CarryClear, repeatValueLoop); // CarryClear == UnsignedLess

            build.setLabel(exitValueLoop);
            build.b(helpers.return_);
        }
        break;
    case IrCmd::FORGLOOP:
        // register layout: ra + 1 = table, ra + 2 = internal index, ra + 3 .. ra + aux = iteration variables
        regs.spill(index);
        // clear extra variables since we might have more than two
        if (intOp(inst.b) > 2)
        {
            CODEGEN_ASSERT(LUA_TNIL == 0);
            for (int i = 2; i < intOp(inst.b); ++i)
                build.str(wzr, mem(rBase, (vmRegOp(inst.a) + 3 + i) * sizeof(TValue) + offsetof(TValue, tt)));
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
        regs.spill(index);
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
        regs.spill(index);
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
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::VmConst);

        regs.spill(index);
        emitFallback(build, offsetof(NativeContext, executeGETGLOBAL), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETGLOBAL:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::VmConst);

        regs.spill(index);
        emitFallback(build, offsetof(NativeContext, executeSETGLOBAL), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETTABLEKS:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.d.kind == IrOpKind::VmConst);

        regs.spill(index);
        emitFallback(build, offsetof(NativeContext, executeGETTABLEKS), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_SETTABLEKS:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.d.kind == IrOpKind::VmConst);

        regs.spill(index);
        emitFallback(build, offsetof(NativeContext, executeSETTABLEKS), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_NAMECALL:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.d.kind == IrOpKind::VmConst);

        regs.spill(index);
        emitFallback(build, offsetof(NativeContext, executeNAMECALL), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_PREPVARARGS:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::Constant);

        regs.spill(index);
        emitFallback(build, offsetof(NativeContext, executePREPVARARGS), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_GETVARARGS:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::Constant);

        regs.spill(index);
        build.mov(x0, rState);

        if (intOp(inst.c) == LUA_MULTRET)
        {
            emitAddOffset(build, x1, rCode, uintOp(inst.a) * sizeof(Instruction));
            build.mov(x2, rBase);
            build.mov(w3, vmRegOp(inst.b));
            build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, executeGETVARARGSMultRet)));
            build.blr(x4);

            emitUpdateBase(build);
        }
        else
        {
            build.mov(x1, rBase);
            build.mov(w2, vmRegOp(inst.b));
            build.mov(w3, intOp(inst.c));
            build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, executeGETVARARGSConst)));
            build.blr(x4);

            // note: no emitUpdateBase necessary because executeGETVARARGSConst does not reallocate stack
        }
        break;
    case IrCmd::NEWCLOSURE:
    {
        RegisterA64 reg = regOp(inst.b); // note: we need to call regOp before spill so that we don't do redundant reloads

        regs.spill(index, {reg});
        build.mov(x2, reg);

        build.mov(x0, rState);
        build.mov(w1, uintOp(inst.a));

        build.ldr(x3, mem(rClosure, offsetof(Closure, l.p)));
        build.ldr(x3, mem(x3, offsetof(Proto, p)));
        build.ldr(x3, mem(x3, sizeof(Proto*) * uintOp(inst.c)));

        build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, luaF_newLclosure)));
        build.blr(x4);

        inst.regA64 = regs.takeReg(x0, index);
        break;
    }
    case IrCmd::FALLBACK_DUPCLOSURE:
        CODEGEN_ASSERT(inst.b.kind == IrOpKind::VmReg);
        CODEGEN_ASSERT(inst.c.kind == IrOpKind::VmConst);

        regs.spill(index);
        emitFallback(build, offsetof(NativeContext, executeDUPCLOSURE), uintOp(inst.a));
        break;
    case IrCmd::FALLBACK_FORGPREP:
        regs.spill(index);
        emitFallback(build, offsetof(NativeContext, executeFORGPREP), uintOp(inst.a));
        jumpOrFallthrough(blockOp(inst.c), next);
        break;

    // Pseudo instructions
    case IrCmd::NOP:
    case IrCmd::SUBSTITUTE:
        CODEGEN_ASSERT(!"Pseudo instructions should not be lowered");
        break;

    case IrCmd::BITAND_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.a.kind == IrOpKind::Inst && inst.b.kind == IrOpKind::Constant && AssemblyBuilderA64::isMaskSupported(unsigned(intOp(inst.b))))
            build.and_(inst.regA64, regOp(inst.a), unsigned(intOp(inst.b)));
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
        if (inst.a.kind == IrOpKind::Inst && inst.b.kind == IrOpKind::Constant && AssemblyBuilderA64::isMaskSupported(unsigned(intOp(inst.b))))
            build.eor(inst.regA64, regOp(inst.a), unsigned(intOp(inst.b)));
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
        if (inst.a.kind == IrOpKind::Inst && inst.b.kind == IrOpKind::Constant && AssemblyBuilderA64::isMaskSupported(unsigned(intOp(inst.b))))
            build.orr(inst.regA64, regOp(inst.a), unsigned(intOp(inst.b)));
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
        build.mvn_(inst.regA64, temp);
        break;
    }
    case IrCmd::BITLSHIFT_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a, inst.b});
        if (inst.a.kind == IrOpKind::Inst && inst.b.kind == IrOpKind::Constant)
            build.lsl(inst.regA64, regOp(inst.a), uint8_t(unsigned(intOp(inst.b)) & 31));
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
        if (inst.a.kind == IrOpKind::Inst && inst.b.kind == IrOpKind::Constant)
            build.lsr(inst.regA64, regOp(inst.a), uint8_t(unsigned(intOp(inst.b)) & 31));
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
        if (inst.a.kind == IrOpKind::Inst && inst.b.kind == IrOpKind::Constant)
            build.asr(inst.regA64, regOp(inst.a), uint8_t(unsigned(intOp(inst.b)) & 31));
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
        if (inst.a.kind == IrOpKind::Inst && inst.b.kind == IrOpKind::Constant)
        {
            inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a});
            build.ror(inst.regA64, regOp(inst.a), uint8_t((32 - unsigned(intOp(inst.b))) & 31));
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
        if (inst.a.kind == IrOpKind::Inst && inst.b.kind == IrOpKind::Constant)
            build.ror(inst.regA64, regOp(inst.a), uint8_t(unsigned(intOp(inst.b)) & 31));
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
    case IrCmd::BYTESWAP_UINT:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.a});
        RegisterA64 temp = tempUint(inst.a);
        build.rev(inst.regA64, temp);
        break;
    }
    case IrCmd::INVOKE_LIBM:
    {
        if (inst.c.kind != IrOpKind::None)
        {
            bool isInt = (inst.c.kind == IrOpKind::Constant) ? constOp(inst.c).kind == IrConstKind::Int
                                                             : getCmdValueKind(function.instOp(inst.c).cmd) == IrValueKind::Int;

            RegisterA64 temp1 = tempDouble(inst.b);
            RegisterA64 temp2 = isInt ? tempInt(inst.c) : tempDouble(inst.c);
            RegisterA64 temp3 = isInt ? noreg : regs.allocTemp(KindA64::d); // note: spill() frees all registers so we need to avoid alloc after spill
            regs.spill(index, {temp1, temp2});

            if (isInt)
            {
                build.fmov(d0, temp1);
                build.mov(w0, temp2);
            }
            else if (d0 != temp2)
            {
                build.fmov(d0, temp1);
                build.fmov(d1, temp2);
            }
            else
            {
                build.fmov(temp3, d0);
                build.fmov(d0, temp1);
                build.fmov(d1, temp3);
            }
        }
        else
        {
            RegisterA64 temp1 = tempDouble(inst.b);
            regs.spill(index, {temp1});
            build.fmov(d0, temp1);
        }

        build.ldr(x1, mem(rNativeContext, getNativeContextOffset(uintOp(inst.a))));
        build.blr(x1);
        inst.regA64 = regs.takeReg(d0, index);
        break;
    }
    case IrCmd::GET_TYPE:
    {
        inst.regA64 = regs.allocReg(KindA64::x, index);

        CODEGEN_ASSERT(sizeof(TString*) == 8);

        if (inst.a.kind == IrOpKind::Inst)
            build.add(inst.regA64, rGlobalState, regOp(inst.a), 3); // implicit uxtw
        else if (inst.a.kind == IrOpKind::Constant)
            build.add(inst.regA64, rGlobalState, uint16_t(tagOp(inst.a)) * 8);
        else
            CODEGEN_ASSERT(!"Unsupported instruction form");

        build.ldr(inst.regA64, mem(inst.regA64, offsetof(global_State, ttname)));
        break;
    }
    case IrCmd::GET_TYPEOF:
    {
        regs.spill(index);
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.ldr(x2, mem(rNativeContext, offsetof(NativeContext, luaT_objtypenamestr)));
        build.blr(x2);

        inst.regA64 = regs.takeReg(x0, index);
        break;
    }

    case IrCmd::FINDUPVAL:
    {
        regs.spill(index);
        build.mov(x0, rState);
        build.add(x1, rBase, uint16_t(vmRegOp(inst.a) * sizeof(TValue)));
        build.ldr(x2, mem(rNativeContext, offsetof(NativeContext, luaF_findupval)));
        build.blr(x2);

        inst.regA64 = regs.takeReg(x0, index);
        break;
    }

    case IrCmd::BUFFER_READI8:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.b});
        AddressA64 addr = tempAddrBuffer(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c));

        build.ldrsb(inst.regA64, addr);
        break;
    }

    case IrCmd::BUFFER_READU8:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.b});
        AddressA64 addr = tempAddrBuffer(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c));

        build.ldrb(inst.regA64, addr);
        break;
    }

    case IrCmd::BUFFER_WRITEI8:
    {
        RegisterA64 temp = tempInt(inst.c);
        AddressA64 addr = tempAddrBuffer(inst.a, inst.b, inst.d.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.d));

        build.strb(temp, addr);
        break;
    }

    case IrCmd::BUFFER_READI16:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.b});
        AddressA64 addr = tempAddrBuffer(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c));

        build.ldrsh(inst.regA64, addr);
        break;
    }

    case IrCmd::BUFFER_READU16:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.b});
        AddressA64 addr = tempAddrBuffer(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c));

        build.ldrh(inst.regA64, addr);
        break;
    }

    case IrCmd::BUFFER_WRITEI16:
    {
        RegisterA64 temp = tempInt(inst.c);
        AddressA64 addr = tempAddrBuffer(inst.a, inst.b, inst.d.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.d));

        build.strh(temp, addr);
        break;
    }

    case IrCmd::BUFFER_READI32:
    {
        inst.regA64 = regs.allocReuse(KindA64::w, index, {inst.b});
        AddressA64 addr = tempAddrBuffer(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c));

        build.ldr(inst.regA64, addr);
        break;
    }

    case IrCmd::BUFFER_WRITEI32:
    {
        RegisterA64 temp = tempInt(inst.c);
        AddressA64 addr = tempAddrBuffer(inst.a, inst.b, inst.d.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.d));

        build.str(temp, addr);
        break;
    }

    case IrCmd::BUFFER_READF32:
    {
        inst.regA64 = regs.allocReg(KindA64::d, index);
        RegisterA64 temp = castReg(KindA64::s, inst.regA64); // safe to alias a fresh register
        AddressA64 addr = tempAddrBuffer(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c));

        build.ldr(temp, addr);
        build.fcvt(inst.regA64, temp);
        break;
    }

    case IrCmd::BUFFER_WRITEF32:
    {
        RegisterA64 temp1 = tempDouble(inst.c);
        RegisterA64 temp2 = regs.allocTemp(KindA64::s);
        AddressA64 addr = tempAddrBuffer(inst.a, inst.b, inst.d.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.d));

        build.fcvt(temp2, temp1);
        build.str(temp2, addr);
        break;
    }

    case IrCmd::BUFFER_READF64:
    {
        inst.regA64 = regs.allocReg(KindA64::d, index);
        AddressA64 addr = tempAddrBuffer(inst.a, inst.b, inst.c.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.c));

        build.ldr(inst.regA64, addr);
        break;
    }

    case IrCmd::BUFFER_WRITEF64:
    {
        RegisterA64 temp = tempDouble(inst.c);
        AddressA64 addr = tempAddrBuffer(inst.a, inst.b, inst.d.kind == IrOpKind::None ? LUA_TBUFFER : tagOp(inst.d));

        build.str(temp, addr);
        break;
    }

        // To handle unsupported instructions, add "case IrCmd::OP" and make sure to set error = true!
    }

    valueTracker.afterInstLowering(inst, index);

    if (FFlag::LuauCodeGenRegAutoSpillA64)
        regs.currInstIdx = kInvalidInstIdx;

    regs.freeLastUseRegs(inst, index);
    regs.freeTempRegs();
}

void IrLoweringA64::finishBlock(const IrBlock& curr, const IrBlock& next)
{
    if (!regs.spills.empty())
    {
        // If we have spills remaining, we have to immediately lower the successor block
        for (uint32_t predIdx : predecessors(function.cfg, function.getBlockIndex(next)))
            CODEGEN_ASSERT(predIdx == function.getBlockIndex(curr));

        // And the next block cannot be a join block in cfg
        CODEGEN_ASSERT(next.useCount == 1);
    }
}

void IrLoweringA64::finishFunction()
{
    if (build.logText)
        build.logAppend("; interrupt handlers\n");

    for (InterruptHandler& handler : interruptHandlers)
    {
        build.setLabel(handler.self);
        build.mov(x0, (handler.pcpos + 1) * sizeof(Instruction));
        build.adr(x1, handler.next);
        build.b(helpers.interrupt);
    }

    if (build.logText)
        build.logAppend("; exit handlers\n");

    for (ExitHandler& handler : exitHandlers)
    {
        CODEGEN_ASSERT(handler.pcpos != kVmExitEntryGuardPc);

        build.setLabel(handler.self);

        build.mov(x0, handler.pcpos * sizeof(Instruction));
        build.b(helpers.updatePcAndContinueInVm);
    }

    if (FFlag::LuauCodeGenUnassignedBcTargetAbort)
    {
        // An undefined instruction is placed after the function to be used as an aborting jump offset
        function.endLocation = build.setLabel().location;
        build.udf();
    }

    if (stats)
    {
        if (error)
            stats->loweringErrors++;

        if (regs.error)
            stats->regAllocErrors++;
    }
}

bool IrLoweringA64::hasError() const
{
    return error || regs.error;
}

bool IrLoweringA64::isFallthroughBlock(const IrBlock& target, const IrBlock& next)
{
    return target.start == next.start;
}

void IrLoweringA64::jumpOrFallthrough(IrBlock& target, const IrBlock& next)
{
    if (!isFallthroughBlock(target, next))
        build.b(target.label);
}

Label& IrLoweringA64::getTargetLabel(IrOp op, Label& fresh)
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

void IrLoweringA64::finalizeTargetLabel(IrOp op, Label& fresh)
{
    if (op.kind == IrOpKind::Undef)
    {
        emitAbort(build, fresh);
    }
    else if (op.kind == IrOpKind::VmExit && fresh.id != 0 && fresh.id != helpers.exitContinueVmClearNativeFlag.id)
    {
        exitHandlerMap[vmExitOp(op)] = uint32_t(exitHandlers.size());
        exitHandlers.push_back({fresh, vmExitOp(op)});
    }
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

            uint64_t vali = getDoubleBits(val);

            if ((vali << 16) == 0)
            {
                build.movz(temp1, uint16_t(vali >> 48), 48);
                build.fmov(temp2, temp1);
            }
            else if ((vali << 32) == 0)
            {
                build.movz(temp1, uint16_t(vali >> 48), 48);
                build.movk(temp1, uint16_t(vali >> 32), 32);
                build.fmov(temp2, temp1);
            }
            else
            {
                build.adr(temp1, val);
                build.ldr(temp2, temp1);
            }

            return temp2;
        }
    }
    else
    {
        CODEGEN_ASSERT(!"Unsupported instruction form");
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
        CODEGEN_ASSERT(!"Unsupported instruction form");
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
        build.mov(temp, unsigned(intOp(op)));
        return temp;
    }
    else
    {
        CODEGEN_ASSERT(!"Unsupported instruction form");
        return noreg;
    }
}

AddressA64 IrLoweringA64::tempAddr(IrOp op, int offset)
{
    // This is needed to tighten the bounds checks in the VmConst case below
    CODEGEN_ASSERT(offset % 4 == 0);
    // Full encoded range is wider depending on the load size, but this assertion helps establish a smaller guaranteed working range [0..4096)
    CODEGEN_ASSERT(offset >= 0 && unsigned(offset / 4) <= AssemblyBuilderA64::kMaxImmediate);

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
        CODEGEN_ASSERT(!"Unsupported instruction form");
        return noreg;
    }
}

AddressA64 IrLoweringA64::tempAddrBuffer(IrOp bufferOp, IrOp indexOp, uint8_t tag)
{
    CODEGEN_ASSERT(tag == LUA_TUSERDATA || tag == LUA_TBUFFER);
    int dataOffset = tag == LUA_TBUFFER ? offsetof(Buffer, data) : offsetof(Udata, data);

    if (indexOp.kind == IrOpKind::Inst)
    {
        RegisterA64 temp = regs.allocTemp(KindA64::x);
        build.add(temp, regOp(bufferOp), regOp(indexOp)); // implicit uxtw
        return mem(temp, dataOffset);
    }
    else if (indexOp.kind == IrOpKind::Constant)
    {
        // Since the resulting address may be used to load any size, including 1 byte, from an unaligned offset, we are limited by unscaled
        // encoding
        if (unsigned(intOp(indexOp)) + dataOffset <= 255)
            return mem(regOp(bufferOp), int(intOp(indexOp) + dataOffset));

        // indexOp can only be negative in dead code (since offsets are checked); this avoids assertion in emitAddOffset
        if (intOp(indexOp) < 0)
            return mem(regOp(bufferOp), dataOffset);

        RegisterA64 temp = regs.allocTemp(KindA64::x);
        emitAddOffset(build, temp, regOp(bufferOp), size_t(intOp(indexOp)));
        return mem(temp, dataOffset);
    }
    else
    {
        CODEGEN_ASSERT(!"Unsupported instruction form");
        return noreg;
    }
}

RegisterA64 IrLoweringA64::regOp(IrOp op)
{
    IrInst& inst = function.instOp(op);

    if (inst.spilled || inst.needsReload)
        regs.restoreReg(inst);

    CODEGEN_ASSERT(inst.regA64 != noreg);
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

int IrLoweringA64::intOp(IrOp op) const
{
    return function.intOp(op);
}

unsigned IrLoweringA64::uintOp(IrOp op) const
{
    return function.uintOp(op);
}

unsigned IrLoweringA64::importOp(IrOp op) const
{
    return function.importOp(op);
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
