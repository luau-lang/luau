// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Sccp.h"
#include "Luau/BytecodeGraph.h"

#include <cmath>

namespace Luau
{
namespace Bytecode
{

// standard three way comparison: -1 if a < b, 0 if a == b, 1 if a > b
template<typename T>
static int threeWay(const T& a, const T& b)
{
    return static_cast<int>(a > b) - static_cast<int>(a < b);
}

static BcOp findOrAddConst(BcFunction<BcVmConst>& func, const BcVmConst& value)
{
    for (size_t i = 0; i < func.constants.size(); i++)
    {
        if (func.constants[i] == value)
            return BcOp{BcOpKind::VmConst, static_cast<uint32_t>(i)};
    }
    return func.addConst(value);
}

std::optional<BcOp> BcVmConstImpl::evaluate(const BcOp& lhsOp, const BcOp& rhsOp, LuauOpcode op) const
{
    BcVmConst& lhs = func.constOp(lhsOp);
    BcVmConst& rhs = func.constOp(rhsOp);

    // arithmetic folding is only defined for two numbers
    if (lhs.kind != rhs.kind || lhs.kind != BcVmConstKind::Number)
        return std::nullopt;

    const double a = lhs.valueNumber;
    const double b = rhs.valueNumber;
    double r;

    switch (op)
    {
    case LuauOpcode::LOP_ADD:
        r = a + b;
        break;
    case LuauOpcode::LOP_SUB:
        r = a - b;
        break;
    case LuauOpcode::LOP_MUL:
        r = a * b;
        break;
    case LuauOpcode::LOP_DIV:
        if (b == 0.0)
            return std::nullopt;
        r = a / b;
        break;
    case LuauOpcode::LOP_MOD:
        if (b == 0.0)
            return std::nullopt;
        r = a - floor(a / b) * b;
        break;
    case LuauOpcode::LOP_POW:
        r = pow(a, b);
        break;
    case LuauOpcode::LOP_IDIV:
        if (b == 0.0)
            return std::nullopt;
        r = floor(a / b);
        break;
    default:
        return std::nullopt;
    }

    BcVmConst result;
    result.kind = BcVmConstKind::Number;
    result.valueNumber = r;
    return findOrAddConst(func, result);
}

bool BcVmConstImpl::falsey(const BcOp& falseyOp) const
{
    if (falseyOp.kind == BcOpKind::VmConst)
    {
        BcVmConst& vmConst = func.constOp(falseyOp);

        return vmConst.kind == BcVmConstKind::Nil || (vmConst.kind == BcVmConstKind::Boolean && vmConst.valueBoolean == false);
    }
    else if (falseyOp.kind == BcOpKind::Imm)
    {
        BcImm& imm = func.immOp(falseyOp);
        return imm.kind == BcImmKind::Boolean && imm.valueBoolean == false;
    }

    return false;
}

int BcVmConstImpl::cmp(const BcOp& lhsOp, const BcOp& rhsOp) const
{
    BcVmConst& lhs = func.constOp(lhsOp);
    BcVmConst& rhs = func.constOp(rhsOp);
    LUAU_ASSERT(lhs.kind == rhs.kind);

    switch (lhs.kind)
    {
    case BcVmConstKind::Number:
        return threeWay(lhs.valueNumber, rhs.valueNumber);
    case BcVmConstKind::Integer:
        return threeWay(lhs.valueInteger, rhs.valueInteger);
    case BcVmConstKind::Boolean:
        return (lhs.valueBoolean == rhs.valueBoolean) ? 0 : 1;
    case BcVmConstKind::String:
        return threeWay(lhs.valueString.compare(rhs.valueString), 0);
    default:
        return 0;
    }
};

int BcVmConstImpl::cmp(const BcOp& lhsOp, const BcImm& rhs) const
{
    BcVmConst& lhs = func.constOp(lhsOp);

    if (rhs.kind == BcImmKind::Int)
    {
        if (lhs.kind == BcVmConstKind::Number)
            return threeWay(lhs.valueNumber, static_cast<double>(rhs.valueInt));
        else if (lhs.kind == BcVmConstKind::Integer)
            return threeWay(lhs.valueInteger, static_cast<int64_t>(rhs.valueInt));
    }
    else if (rhs.kind == BcImmKind::Boolean)
    {
        if (lhs.kind == BcVmConstKind::Boolean)
            return (lhs.valueBoolean == rhs.valueBoolean) ? 0 : 1;
    }

    LUAU_ASSERT(!"incompatible types for immCmpBcVmConst");
    return 0;
}

BcOp BcVmConstImpl::makeNil() const
{
    BcVmConst result{};
    result.kind = BcVmConstKind::Nil;
    return findOrAddConst(func, result);
}

BcImm BcVmConstImpl::makeImm(bool value) const
{
    BcImm result{};
    result.kind = BcImmKind::Boolean;
    result.valueBoolean = value;
    return result;
}

BcImm BcVmConstImpl::makeImm(int32_t value) const
{
    BcImm result{};
    result.kind = BcImmKind::Int;
    result.valueInt = value;
    return result;
}

BcRef<BcImm> BcVmConstImpl::asImm(BcOp op) const
{
    return func.imm(op);
}

bool BcVmConstImpl::isOrderable(const BcOp& vmConstOp) const
{
    BcVmConst& vmConst = func.constOp(vmConstOp);
    return vmConst.kind == BcVmConstKind::Number || vmConst.kind == BcVmConstKind::Integer || vmConst.kind == BcVmConstKind::String;
}
bool BcVmConstImpl::kindEquals(const BcOp& lhsOp, const BcOp& rhsOp) const
{
    BcVmConst& lhs = func.constOp(lhsOp);
    BcVmConst& rhs = func.constOp(rhsOp);

    return lhs.kind == rhs.kind;
}

std::optional<bool> BcVmConstImpl::eq(const BcOp& lhsOp, const BcOp& rhsOp) const
{
    if (lhsOp.kind == BcOpKind::VmConst && rhsOp.kind == BcOpKind::VmConst)
    {

        BcVmConst& lhs = func.constOp(lhsOp);
        BcVmConst& rhs = func.constOp(rhsOp);

        if (lhs.kind == BcVmConstKind::Number && rhs.kind == BcVmConstKind::Number)
            return lhs.valueNumber == rhs.valueNumber;
        if (lhs.kind == BcVmConstKind::Integer && rhs.kind == BcVmConstKind::Integer)
            return lhs.valueInteger == rhs.valueInteger;
        if (lhs.kind == BcVmConstKind::Number && rhs.kind == BcVmConstKind::Integer)
            return lhs.valueNumber == static_cast<double>(rhs.valueInteger);
        if (lhs.kind == BcVmConstKind::Integer && rhs.kind == BcVmConstKind::Number)
            return static_cast<double>(lhs.valueInteger) == rhs.valueNumber;
        if (lhs.kind == BcVmConstKind::String && rhs.kind == BcVmConstKind::String)
            return lhs.valueString == rhs.valueString;
    }
    else if (lhsOp.kind == BcOpKind::VmConst && rhsOp.kind == BcOpKind::Imm)
    {

        BcVmConst& lhs = func.constOp(lhsOp);
        BcImm& rhs = func.immOp(rhsOp);
        if (lhs.kind == BcVmConstKind::Boolean && rhs.kind == BcImmKind::Boolean)
            return lhs.valueBoolean == rhs.valueBoolean;
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

std::optional<bool> BcVmConstImpl::eq(const BcOp& lhsOp, bool rhs) const
{
    BcVmConst& lhs = func.constOp(lhsOp);

    if (lhs.kind == BcVmConstKind::Boolean)
        return lhs.valueBoolean == rhs;
    return std::nullopt;
}

std::optional<bool> BcVmConstImpl::eq(const BcOp& lhsOp, int32_t rhs) const
{
    BcVmConst& lhs = func.constOp(lhsOp);

    if (lhs.kind == BcVmConstKind::Number)
        return static_cast<double>(rhs) == lhs.valueNumber;
    if (lhs.kind == BcVmConstKind::Integer)
        return static_cast<int64_t>(rhs) == lhs.valueInteger;
    return std::nullopt;
}

bool BcVmConstImpl::isArithmeticConstant(const BcOp& vmConstOp) const
{
    BcVmConst& vmConst = func.constOp(vmConstOp);
    return vmConst.kind == BcVmConstKind::Number;
}

} // namespace Bytecode
} // namespace Luau
