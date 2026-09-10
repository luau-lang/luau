// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "TValueVmConstImpl.h"

#include "lnumutils.h"
#include "lgc.h"
#include "lobject.h"
#include "lvector.h"

#include <algorithm>
#include <cmath>

namespace Luau
{
namespace Bytecode
{

std::optional<BcOp> TValueVmConstImpl::evaluate(const BcOp& lhsOp, const BcOp& rhsOp, LuauOpcode op) const
{
    TValue*& lhs = func.constOp(lhsOp);
    TValue*& rhs = func.constOp(rhsOp);

    TValue* tv = backing.nextTValue();

    switch (op)
    {
    case LOP_ADD:
    case LOP_ADDK:
        if (ttisnumber(lhs) && ttisnumber(rhs))
        {
            setnvalue(tv, nvalue(lhs) + nvalue(rhs));
        }
        else if (ttisvector(lhs) && ttisvector(rhs))
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
        if (ttisnumber(lhs) && ttisnumber(rhs))
        {
            setnvalue(tv, nvalue(lhs) - nvalue(rhs));
        }
        else if (ttisvector(lhs) && ttisvector(rhs))
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
        if (ttisnumber(lhs) && ttisnumber(rhs))
        {
            setnvalue(tv, nvalue(lhs) * nvalue(rhs));
        }
        else if (ttisvector(lhs) && ttisnumber(rhs))
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
        if (ttisnumber(lhs) && ttisnumber(rhs))
        {
            setnvalue(tv, nvalue(lhs) / nvalue(rhs));
        }
        else if (ttisvector(lhs) && ttisnumber(rhs))
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
        if (ttisnumber(lhs) && ttisnumber(rhs))
        {
            setnvalue(tv, luai_numidiv(nvalue(lhs), nvalue(rhs)));
        }
        else if (ttisvector(lhs) && ttisnumber(rhs))
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
    case LOP_MOD:
    case LOP_MODK:
        if (ttisnumber(lhs) && ttisnumber(rhs))
        {
            setnvalue(tv, luai_nummod(nvalue(lhs), nvalue(rhs)));
        }
        else
        {
            return std::nullopt;
        }
        break;
    case LOP_POW:
    case LOP_POWK:
        if (ttisnumber(lhs) && ttisnumber(rhs))
        {
            setnvalue(tv, pow(nvalue(lhs), nvalue(rhs)));
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
    if (falseyOp.kind == BcOpKind::VmConst)
    {
        TValue*& v = func.constOp(falseyOp);
        return l_isfalse(v);
    }
    else if (falseyOp.kind == BcOpKind::Imm)
    {
        BcImm& imm = func.immOp(falseyOp);
        return imm.kind == BcImmKind::Boolean && imm.valueBoolean == false;
    }

    return false;
}

int TValueVmConstImpl::cmp(const BcOp& lhsOp, const BcOp& rhsOp) const
{
    TValue*& lhs = func.constOp(lhsOp);
    TValue*& rhs = func.constOp(rhsOp);

    if (ttisnumber(lhs) && ttisnumber(rhs))
    {
        double l = nvalue(lhs);
        double r = nvalue(rhs);
        if (l < r)
            return -1;
        if (l > r)
            return 1;
        return 0;
    }

    if (ttisboolean(lhs) && ttisboolean(rhs))
        return (bvalue(lhs) == bvalue(rhs)) ? 0 : 1;

    if (ttisstring(lhs) && ttisstring(rhs))
    {
        TString* ls = tsvalue(lhs);
        TString* rs = tsvalue(rhs);
        int c = memcmp(ls->data, rs->data, std::min(ls->len, rs->len));
        if (c == 0)
            c = (ls->len < rs->len) ? -1 : (ls->len > rs->len) ? 1 : 0;
        if (c < 0)
            return -1;
        if (c > 0)
            return 1;
        return 0;
    }

    return 0;
}

int TValueVmConstImpl::cmp(const BcOp& lhsOp, const BcImm& rhs) const
{
    TValue*& lhs = func.constOp(lhsOp);
    if (rhs.kind == BcImmKind::Int)
    {
        if (ttisnumber(lhs))
        {
            double l = nvalue(lhs);
            double r = static_cast<double>(rhs.valueInt);
            if (l < r)
                return -1;
            if (l > r)
                return 1;
            return 0;
        }
    }
    else if (rhs.kind == BcImmKind::Boolean)
    {
        if (ttisboolean(lhs))
            return (bvalue(lhs) == static_cast<int>(rhs.valueBoolean)) ? 0 : 1;
    }

    // BcImms are only either Int, Boolean or Import, and we are not doing import comparisons, so return false
    return 0;
}

BcOp TValueVmConstImpl::makeNil() const
{
    TValue* tv = backing.nextTValue();
    setnilvalue(tv);
    return func.addConst(tv);
}

BcImm TValueVmConstImpl::makeImm(bool value) const
{
    BcImm result{};
    result.kind = BcImmKind::Boolean;
    result.valueBoolean = value;
    return result;
}

BcImm TValueVmConstImpl::makeImm(int32_t value) const
{
    BcImm result{};
    result.kind = BcImmKind::Int;
    result.valueInt = value;
    return result;
}

BcRef<BcImm> TValueVmConstImpl::asImm(BcOp op) const
{
    return func.imm(op);
}

bool TValueVmConstImpl::isOrderable(const BcOp& vmConstOp) const
{
    TValue*& v = func.constOp(vmConstOp);
    return ttisnumber(v) || ttisstring(v);
}

bool TValueVmConstImpl::kindEquals(const BcOp& lhsOp, const BcOp& rhsOp) const
{
    TValue*& lhs = func.constOp(lhsOp);
    TValue*& rhs = func.constOp(rhsOp);
    return ttype(lhs) == ttype(rhs);
}

std::optional<bool> TValueVmConstImpl::eq(const BcOp& lhsOp, const BcOp& rhsOp) const
{
    if (lhsOp.kind == BcOpKind::VmConst && rhsOp.kind == BcOpKind::VmConst)
    {
        TValue*& lhs = func.constOp(lhsOp);
        TValue*& rhs = func.constOp(rhsOp);

        if (ttisnumber(lhs) && ttisnumber(rhs))
            return nvalue(lhs) == nvalue(rhs);
        if (ttisstring(lhs) && ttisstring(rhs))
        {
            TString* ls = tsvalue(lhs);
            TString* rs = tsvalue(rhs);
            return ls == rs || (ls->len == rs->len && memcmp(ls->data, rs->data, ls->len) == 0);
        }
    }
    else if (lhsOp.kind == BcOpKind::VmConst && rhsOp.kind == BcOpKind::Imm)
    {
        TValue*& lhs = func.constOp(lhsOp);
        BcImm& rhs = func.immOp(rhsOp);
        if (ttisboolean(lhs) && rhs.kind == BcImmKind::Boolean)
            return bvalue(lhs) == static_cast<int>(rhs.valueBoolean);
    }
    else if (lhsOp.kind == BcOpKind::Imm && rhsOp.kind == BcOpKind::Imm)
    {
        BcImm& lhs = func.immOp(lhsOp);
        BcImm& rhs = func.immOp(rhsOp);
        if (lhs.kind == BcImmKind::Boolean && rhs.kind == BcImmKind::Boolean)
            return lhs.valueBoolean == rhs.valueBoolean;
        else if (lhs.kind == BcImmKind::Int && rhs.kind == BcImmKind::Int)
            return lhs.valueInt == rhs.valueInt;
    }
    return std::nullopt;
}

std::optional<bool> TValueVmConstImpl::eq(const BcOp& lhsOp, bool rhs) const
{
    TValue*& lhs = func.constOp(lhsOp);

    if (ttisboolean(lhs))
        return bvalue(lhs) == static_cast<int>(rhs);
    return std::nullopt;
}

std::optional<bool> TValueVmConstImpl::eq(const BcOp& lhsOp, int32_t rhs) const
{
    TValue*& lhs = func.constOp(lhsOp);

    if (ttisnumber(lhs))
        return static_cast<double>(rhs) == nvalue(lhs);
    return std::nullopt;
}

bool TValueVmConstImpl::isArithmeticConstant(const BcOp& vmConstOp) const
{
    TValue*& v = func.constOp(vmConstOp);
    return ttisnumber(v);
}

double TValueVmConstImpl::asNumber(const BcOp& vmConstOp) const
{
    TValue*& v = func.constOp(vmConstOp);
    return nvalue(v);
}

} // namespace Bytecode
} // namespace Luau
