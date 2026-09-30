// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "TValueVmConstImpl.h"

#include "lnumutils.h"
#include "lgc.h"
#include "lobject.h"
#include "lvector.h"
#include "lvm.h"

namespace Luau
{
namespace Bytecode
{

std::optional<BcOp> TValueVmConstImpl::evaluate(const BcOp& lhsOp, const BcOp& rhsOp, LuauOpcode op) const
{
    if (isNumber(lhsOp) && isNumber(rhsOp))
    {
        if (std::optional<double> resultOpt = evaluateNumberBinaryOp(asNumber(lhsOp), asNumber(rhsOp), op))
        {
            TValue* tv = backing.nextTValue();
            setnvalue(tv, *resultOpt);
            return func.addConst(tv);
        }

        return std::nullopt;
    }

    if (lhsOp.kind != BcOpKind::VmConst || rhsOp.kind != BcOpKind::VmConst)
        return std::nullopt;

    TValue* lhs = func.constOp(lhsOp);
    TValue* rhs = func.constOp(rhsOp);

    TValue* tv = backing.nextTValue();

    switch (op)
    {
    case LOP_ADD:
    case LOP_ADDK:
        if (ttisvector(lhs) && ttisvector(rhs))
        {
            const LUA_VECTOR_TYPE* lv = vvalue(lhs);
            const LUA_VECTOR_TYPE* rv = vvalue(rhs);
            setvvalue(backing.L, tv, lv[0] + rv[0], lv[1] + rv[1], lv[2] + rv[2], lv[3] + rv[3]);
        }
        else
        {
            return std::nullopt;
        }
        break;
    case LOP_SUB:
    case LOP_SUBK:
        if (ttisvector(lhs) && ttisvector(rhs))
        {
            const LUA_VECTOR_TYPE* lv = vvalue(lhs);
            const LUA_VECTOR_TYPE* rv = vvalue(rhs);
            setvvalue(backing.L, tv, lv[0] - rv[0], lv[1] - rv[1], lv[2] - rv[2], lv[3] - rv[3]);
        }
        else
        {
            return std::nullopt;
        }
        break;
    case LOP_MUL:
    case LOP_MULK:
        if (ttisvector(lhs) && ttisnumber(rhs))
        {
            const LUA_VECTOR_TYPE* vb = vvalue(lhs);
            LUA_VECTOR_TYPE vc = cast_to(LUA_VECTOR_TYPE, nvalue(rhs));
            setvvalue(backing.L, tv, vb[0] * vc, vb[1] * vc, vb[2] * vc, vb[3] * vc);
        }
        else if (ttisvector(lhs) && ttisvector(rhs))
        {
            const LUA_VECTOR_TYPE* vb = vvalue(lhs);
            const LUA_VECTOR_TYPE* vc = vvalue(rhs);
            setvvalue(backing.L, tv, vb[0] * vc[0], vb[1] * vc[1], vb[2] * vc[2], vb[3] * vc[3]);
        }
        else if (ttisnumber(lhs) && ttisvector(rhs))
        {
            LUA_VECTOR_TYPE vb = cast_to(LUA_VECTOR_TYPE, nvalue(lhs));
            const LUA_VECTOR_TYPE* vc = vvalue(rhs);
            setvvalue(backing.L, tv, vb * vc[0], vb * vc[1], vb * vc[2], vb * vc[3]);
        }
        else
        {
            return std::nullopt;
        }
        break;
    case LOP_DIV:
    case LOP_DIVK:
        if (ttisvector(lhs) && ttisnumber(rhs))
        {
            const LUA_VECTOR_TYPE* vb = vvalue(lhs);
            LUA_VECTOR_TYPE vc = cast_to(LUA_VECTOR_TYPE, nvalue(rhs));
            setvvalue(backing.L, tv, vb[0] / vc, vb[1] / vc, vb[2] / vc, vb[3] / vc);
        }
        else if (ttisvector(lhs) && ttisvector(rhs))
        {
            const LUA_VECTOR_TYPE* vb = vvalue(lhs);
            const LUA_VECTOR_TYPE* vc = vvalue(rhs);
            setvvalue(backing.L, tv, vb[0] / vc[0], vb[1] / vc[1], vb[2] / vc[2], vb[3] / vc[3]);
        }
        else if (ttisnumber(lhs) && ttisvector(rhs))
        {
            LUA_VECTOR_TYPE vb = cast_to(LUA_VECTOR_TYPE, nvalue(lhs));
            const LUA_VECTOR_TYPE* vc = vvalue(rhs);
            setvvalue(backing.L, tv, vb / vc[0], vb / vc[1], vb / vc[2], vb / vc[3]);
        }
        else
        {
            return std::nullopt;
        }
        break;
    case LOP_IDIV:
    case LOP_IDIVK:
        if (ttisvector(lhs) && ttisnumber(rhs))
        {
            const LUA_VECTOR_TYPE* vb = vvalue(lhs);
            LUA_VECTOR_TYPE vc = cast_to(LUA_VECTOR_TYPE, nvalue(rhs));
            setvvalue(
                backing.L,
                tv,
                float(luai_numidiv(vb[0], vc)),
                float(luai_numidiv(vb[1], vc)),
                float(luai_numidiv(vb[2], vc)),
                float(luai_numidiv(vb[3], vc))
            );
        }
        else
        {
            return std::nullopt;
        }
        break;
    default:
        return std::nullopt;
    }

    return func.addConst(tv);
}

bool TValueVmConstImpl::falsey(const BcOp& falseyOp) const
{
    if (isNil(falseyOp))
        return true;

    if (isBoolean(falseyOp))
        return asBoolean(falseyOp) == false;

    return false;
}

bool TValueVmConstImpl::compare(const BcOp& lhsOp, const BcOp& rhsOp, BcCondition condition) const
{
    LUAU_ASSERT(isOrderable(lhsOp));
    LUAU_ASSERT(isOrderable(rhsOp));

    if (isNumber(lhsOp) && isNumber(rhsOp))
        return bcCompare(asNumber(lhsOp), asNumber(rhsOp), condition);

    if (lhsOp.kind == BcOpKind::VmConst && rhsOp.kind == BcOpKind::VmConst)
    {
        TValue*& lhs = func.constOp(lhsOp);
        TValue*& rhs = func.constOp(rhsOp);
        LUAU_ASSERT(ttype(lhs) == ttype(rhs));

        if (ttisstring(lhs))
        {
            TString* ls = tsvalue(lhs);
            TString* rs = tsvalue(rhs);
            return bcCompare(luaV_strcmp(ls, rs), 0, condition);
        }
    }

    LUAU_ASSERT(!"unsupported comparison");
    return false;
}

BcOp TValueVmConstImpl::makeNil() const
{
    TValue* tv = backing.nextTValue();
    setnilvalue(tv);
    return func.addConst(tv);
}

BcOp TValueVmConstImpl::makeImmBool(bool value) const
{
    return func.addImmBool(value);
}

BcRef<BcImm> TValueVmConstImpl::asImm(BcOp op) const
{
    return func.imm(op);
}

bool TValueVmConstImpl::isOrderable(const BcOp& op) const
{
    if (op.kind == BcOpKind::Imm)
    {
        BcImm& imm = func.immOp(op);
        return imm.kind == BcImmKind::Int;
    }

    if (op.kind == BcOpKind::VmConst)
    {
        TValue*& v = func.constOp(op);
        return ttisnumber(v) || ttisstring(v);
    }

    return false;
}

bool TValueVmConstImpl::kindEquals(const BcOp& lhsOp, const BcOp& rhsOp) const
{
    if (isBoolean(lhsOp) && isBoolean(rhsOp))
        return true;

    if (isNumber(lhsOp) && isNumber(rhsOp))
        return true;

    if (lhsOp.kind != BcOpKind::VmConst || rhsOp.kind != BcOpKind::VmConst)
        return false;

    TValue*& lhs = func.constOp(lhsOp);
    TValue*& rhs = func.constOp(rhsOp);

    return ttype(lhs) == ttype(rhs);
}

bool TValueVmConstImpl::fullyequal(const BcOp& lhsOp, const BcOp& rhsOp) const
{
    if (lhsOp.kind != rhsOp.kind)
        return false;

    if (lhsOp.kind == BcOpKind::Imm)
        return func.immOp(lhsOp) == func.immOp(rhsOp);

    if (lhsOp.kind == BcOpKind::VmConst)
        return lhsOp.index == rhsOp.index;

    LUAU_ASSERT(!"unsupported kind");
    return false;
}

std::optional<bool> TValueVmConstImpl::eq(const BcOp& lhsOp, const BcOp& rhsOp) const
{
    if (isNil(lhsOp) && isNil(rhsOp))
        return true;

    if (isBoolean(lhsOp) && isBoolean(rhsOp))
        return asBoolean(lhsOp) == asBoolean(rhsOp);

    if (isNumber(lhsOp) && isNumber(rhsOp))
        return asNumber(lhsOp) == asNumber(rhsOp);

    // Handle other VM constant types
    if (lhsOp.kind == BcOpKind::VmConst && rhsOp.kind == BcOpKind::VmConst)
    {
        TValue*& lhs = func.constOp(lhsOp);
        TValue*& rhs = func.constOp(rhsOp);

        if (ttype(lhs) != ttype(rhs))
            return false;

        if (ttisinteger(lhs))
            return lvalue(lhs) == lvalue(rhs);

        if (ttisstring(lhs))
            return tsvalue(lhs) == tsvalue(rhs); // strings are interned
    }

    return std::nullopt;
}

bool TValueVmConstImpl::isNil(const BcOp& op) const
{
    if (op.kind == BcOpKind::VmConst)
        return ttisnil(func.constOp(op));

    return false;
}

bool TValueVmConstImpl::isBoolean(const BcOp& op) const
{
    if (op.kind == BcOpKind::Imm)
        return func.immOp(op).kind == BcImmKind::Boolean;

    if (op.kind == BcOpKind::VmConst)
        return ttisboolean(func.constOp(op));

    return false;
}

bool TValueVmConstImpl::isNumber(const BcOp& op) const
{
    if (op.kind == BcOpKind::Imm)
        return func.immOp(op).kind == BcImmKind::Int;

    if (op.kind == BcOpKind::VmConst)
        return ttisnumber(func.constOp(op));

    return false;
}

bool TValueVmConstImpl::asBoolean(const BcOp& op) const
{
    if (op.kind == BcOpKind::Imm)
    {
        BcImm& imm = func.immOp(op);
        LUAU_ASSERT(imm.kind == BcImmKind::Boolean && "use isBoolean first");
        return imm.valueBoolean;
    }

    if (op.kind == BcOpKind::VmConst)
    {
        LUAU_ASSERT(ttisboolean(func.constOp(op)));
        return bvalue(func.constOp(op)) == 1;
    }

    LUAU_ASSERT(!"unsupported type, use isBoolean first");
    return false;
}

double TValueVmConstImpl::asNumber(const BcOp& op) const
{
    if (op.kind == BcOpKind::Imm)
    {
        BcImm& imm = func.immOp(op);
        LUAU_ASSERT(imm.kind == BcImmKind::Int && "use isNumber first");
        return double(imm.valueInt);
    }

    if (op.kind == BcOpKind::VmConst)
    {
        LUAU_ASSERT(ttisnumber(func.constOp(op)));
        return nvalue(func.constOp(op));
    }

    LUAU_ASSERT(!"unsupported type, use isNumber first");
    return 0.0;
}

} // namespace Bytecode
} // namespace Luau
