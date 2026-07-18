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

double BcVmConstImpl::asNumber(const BcOp& vmConstOp) const
{
    BcVmConst& vmConst = func.constOp(vmConstOp);
    LUAU_ASSERT(vmConst.kind == BcVmConstKind::Number);
    return vmConst.valueNumber;
}

// TODO: support imm + vmconst
ConstnessLattice SccpInterpreter::evaluateArith(LuauOpcode opcode, BcRef<BcInst> instRepr)
{
    auto lhs = instRepr->ops[0];
    auto rhs = instRepr->ops[1];

    ConstnessLattice lhsConstness = this->state->operandLattice(lhs);
    ConstnessLattice rhsConstness = this->state->operandLattice(rhs);

    if (lhsConstness.kind == Constness::ImmConstant && rhsConstness.kind == Constness::ImmConstant)
    {
        const BcImm& lhsImm = lhsConstness.immConst.value();
        const BcImm& rhsImm = rhsConstness.immConst.value();

        if (lhsImm.kind == BcImmKind::Int && rhsImm.kind == BcImmKind::Int)
        {
            int lv = lhsImm.valueInt;
            int rv = rhsImm.valueInt;

            // Division/modulo by zero cannot be folded
            if (rv == 0 && (opcode == LOP_DIV || opcode == LOP_MOD || opcode == LOP_IDIV))
                return ConstnessLattice(Constness::NotAConstant);

            // LOP_DIV and LOP_POW are always floating-point, but BcImm cannot represent floats
            if (opcode == LOP_DIV || opcode == LOP_POW)
                return ConstnessLattice(Constness::NotAConstant);

            int64_t result;
            switch (opcode)
            {
            case LOP_ADD:
                result = int64_t(lv) + rv;
                break;
            case LOP_SUB:
                result = int64_t(lv) - rv;
                break;
            case LOP_MUL:
                result = int64_t(lv) * rv;
                break;
            case LOP_MOD:
            {
                // Lua modulo: result has the sign of the divisor
                int64_t remainder = int64_t(lv) % rv;
                if ((remainder != 0) && ((lv < 0) != (rv < 0)))
                    remainder += rv;
                result = remainder;
                break;
            }
            case LOP_IDIV:
            {
                // Lua floor division: round toward negative infinity
                result = int64_t(lv) / rv;
                if ((result < 0) && ((int64_t(lv) % rv) != 0))
                    result -= 1;
                break;
            }
            default:
                LUAU_ASSERT(!"Unhandled opcode");
                return ConstnessLattice(Constness::NotAConstant);
            }

            // LOADN is max 16-bit signed, and we use LOADN in replaceUses
            // we could investigate adding a new VmConst for > 16 bit representable numbers
            if (result < INT16_MIN || result > INT16_MAX)
                return ConstnessLattice(Constness::NotAConstant);

            return ConstnessLattice(Constness::ImmConstant, impl->makeImm(static_cast<int32_t>(result)));
        }
    }
    else if (lhsConstness.kind == Constness::VmConstant && rhsConstness.kind == Constness::VmConstant)
    {
        std::optional<BcOp> vmConst = impl->evaluate(lhsConstness.vmConst.value(), rhsConstness.vmConst.value(), opcode);
        if (vmConst)
            return ConstnessLattice(Constness::VmConstant, vmConst.value());
        else
            return ConstnessLattice(Constness::NotAConstant);
    }
    else if (lhsConstness.kind == Constness::Undetermined && rhsConstness.kind == Constness::Undetermined)
    {
        return ConstnessLattice(Constness::Undetermined);
    }

    return ConstnessLattice(Constness::NotAConstant);
}

ConditionState SccpInterpreter::evaluateComparisonCondition(LuauOpcode op, const BcOp& lhs, const BcOp& rhs)
{
    ConstnessLattice lhsConst = this->state->operandLattice(lhs);
    ConstnessLattice rhsConst = this->state->operandLattice(rhs);

    bool isOrderingOp = (op == LOP_JUMPIFLT || op == LOP_JUMPIFLE || op == LOP_JUMPIFNOTLT || op == LOP_JUMPIFNOTLE);

    auto isOrderableLattice = [&](const ConstnessLattice& c) -> bool
    {
        if (c.kind == Constness::VmConstant)
            return impl->isOrderable(c.vmConst.value());
        if (c.kind == Constness::ImmConstant)
            return c.immConst.value().kind == BcImmKind::Int;
        return false;
    };

    if (isOrderingOp && (!isOrderableLattice(lhsConst) || !isOrderableLattice(rhsConst)))
        return ConditionState::Unknown;

    // Mismatched VM constant kinds have no defined ordering/equality
    // we may be able to compare imm bools and vm bools, imm numbers and vm numbers, but we are not currently
    if (lhsConst.kind == Constness::VmConstant && rhsConst.kind == Constness::VmConstant &&
        !impl->kindEquals(lhsConst.vmConst.value(), rhsConst.vmConst.value()))
        return ConditionState::Unknown;

    auto applyOp = [](int cmp, LuauOpcode op) -> bool
    {
        switch (op)
        {
        case LOP_JUMPIFEQ:
        case LOP_JUMPIFNOTEQ:
            return cmp == 0;
        case LOP_JUMPIFLT:
        case LOP_JUMPIFNOTLT:
            return cmp < 0;
        case LOP_JUMPIFLE:
        case LOP_JUMPIFNOTLE:
            return cmp <= 0;
        default:
            LUAU_ASSERT(!"Unhandled comparison opcode");
            return false;
        }
    };

    if (lhsConst.kind == Constness::VmConstant && rhsConst.kind == Constness::VmConstant)
    {
        int cmp = impl->cmp(lhsConst.vmConst.value(), rhsConst.vmConst.value());
        bool condTrue = applyOp(cmp, op);
        return condTrue ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
    }
    else if (lhsConst.kind == Constness::ImmConstant && rhsConst.kind == Constness::ImmConstant)
    {
        const BcImm& lhsImm = lhsConst.immConst.value();
        const BcImm& rhsImm = rhsConst.immConst.value();

        if (lhsImm.kind == BcImmKind::Int && rhsImm.kind == BcImmKind::Int)
        {
            int lv = lhsImm.valueInt;
            int rv = rhsImm.valueInt;

            int cmp = static_cast<int>(lv > rv) - static_cast<int>(lv < rv);
            bool condTrue = applyOp(cmp, op);
            return condTrue ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
        }
        else if (lhsImm.kind == BcImmKind::Boolean && rhsImm.kind == BcImmKind::Boolean)
        {
            bool lv = lhsImm.valueBoolean;
            bool rv = rhsImm.valueBoolean;

            int cmp = (lv == rv) ? 0 : 1;
            bool condTrue = applyOp(cmp, op);
            return condTrue ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
        }
    }
    else if (lhsConst.kind == Constness::VmConstant && rhsConst.kind == Constness::ImmConstant)
    {
        int cmp = impl->cmp(lhsConst.vmConst.value(), rhsConst.immConst.value());
        bool condTrue = applyOp(cmp, op);
        return condTrue ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
    }
    else if (lhsConst.kind == Constness::ImmConstant && rhsConst.kind == Constness::VmConstant)
    {
        int cmp = -impl->cmp(rhsConst.vmConst.value(), lhsConst.immConst.value());
        bool condTrue = applyOp(cmp, op);
        return condTrue ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
    }

    return ConditionState::Unknown;
}

ConditionState SccpInterpreter::evaluateXeqkCondition(BcRef<BcInst> inst)
{
    ConstnessLattice valConst = this->state->operandLattice(inst->ops[0]);

    switch (inst->op)
    {
    case LOP_JUMPXEQKNIL:
        if (valConst.kind == Constness::VmConstant && impl->falsey(valConst.vmConst.value()) &&
            impl->kindEquals(valConst.vmConst.value(), impl->makeNil()))
            return ConditionState::AlwaysTrue;
        else if (
            valConst.kind == Constness::ImmConstant ||
            (valConst.kind == Constness::VmConstant && !impl->kindEquals(valConst.vmConst.value(), impl->makeNil()))
        )
            return ConditionState::AlwaysFalse;
        break;
    case LOP_JUMPXEQKB:
    {
        const BcOp& cmpImmOp = inst->ops[3];
        LUAU_ASSERT(cmpImmOp.kind == BcOpKind::Imm);
        if (valConst.kind == Constness::ImmConstant && valConst.immConst.value().kind == BcImmKind::Boolean)
        {
            std::optional<bool> eq = impl->eq(valConst.vmConst.value(), cmpImmOp);
            if (eq)
                return *eq ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
        }
        else if (valConst.kind == Constness::VmConstant)
        {
            std::optional<bool> eq = impl->eq(valConst.vmConst.value(), cmpImmOp);
            if (eq)
                return *eq ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
        }
        break;
    }
    case LOP_JUMPXEQKN:
    {
        const BcOp& cmpConstOp = inst->ops[3];
        LUAU_ASSERT(cmpConstOp.kind == BcOpKind::VmConst);
        if (valConst.kind == Constness::VmConstant)
        {
            std::optional<bool> eq = impl->eq(valConst.vmConst.value(), cmpConstOp);
            if (eq)
                return *eq ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
        }
        else if (valConst.kind == Constness::ImmConstant && valConst.immConst.value().kind == BcImmKind::Int)
        {
            std::optional<bool> eq = impl->eq(cmpConstOp, valConst.immConst.value().valueInt);
            if (eq)
                return *eq ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
        }
        break;
    }
    case LOP_JUMPXEQKS:
    {
        const BcOp& cmpConstOp = inst->ops[3];
        LUAU_ASSERT(cmpConstOp.kind == BcOpKind::VmConst);
        if (valConst.kind == Constness::VmConstant)
        {
            std::optional<bool> eq = impl->eq(valConst.vmConst.value(), cmpConstOp);
            if (eq)
                return *eq ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
        }
        break;
    }
    default:
        break;
    }

    return ConditionState::Unknown;
}

ConditionState SccpInterpreter::evaluateCondition(const BcOp& op)
{
    ConstnessLattice lhs = this->state->operandLattice(op);
    if (lhs.kind == Constness::VmConstant)
        return impl->falsey(lhs.vmConst.value()) ? ConditionState::AlwaysFalse : ConditionState::AlwaysTrue;
    else if (lhs.kind == Constness::ImmConstant)
    {
        const BcImm& imm = lhs.immConst.value();
        if (imm.kind == BcImmKind::Boolean)
            return imm.valueBoolean == false ? ConditionState::AlwaysFalse : ConditionState::AlwaysTrue;
    }
    return ConditionState::Unknown;
}

ConstnessLattice SccpInterpreter::evaluate(LuauOpcode op, BcRef<BcInst> instRepr)
{
    switch (op)
    {
    case LOP_LOADK:
    case LOP_LOADKX:
    {
        const BcOp& op = instRepr->ops[0];
        LUAU_ASSERT(op.kind == BcOpKind::VmConst);
        return ConstnessLattice(Constness::VmConstant, op);
    }
    case LOP_LOADB:
    case LOP_LOADN:
    {
        const BcOp& op = instRepr->ops[0];
        LUAU_ASSERT(op.kind == BcOpKind::Imm);
        return ConstnessLattice(Constness::ImmConstant, *impl->asImm(op));
    }
    case LOP_LOADNIL:
    {
        BcOp nilConst = impl->makeNil();
        return ConstnessLattice(Constness::VmConstant, nilConst);
    }

    case LOP_ADD:
    case LOP_SUB:
    case LOP_MUL:
    case LOP_DIV:
    case LOP_MOD:
    case LOP_POW:
    case LOP_IDIV:
    {
        return evaluateArith(op, instRepr);
    }
    case LOP_MOVE:
    {
        return this->state->operandLattice(instRepr->ops[0]);
    }

    case LOP_JUMPIF:
    case LOP_JUMPIFNOT:
    {
        ConditionState cond = evaluateCondition(instRepr->ops[0]);
        if (cond == ConditionState::Unknown)
            return ConstnessLattice(this->state->unknownConditionConstness({instRepr->ops[0]}));

        bool jumpsOnTrue = (instRepr->op == LOP_JUMPIF);
        bool takesJump = (cond == ConditionState::AlwaysTrue) == jumpsOnTrue;
        return ConstnessLattice(Constness::ImmConstant, impl->makeImm(takesJump));
    }

    case LOP_JUMPIFEQ:
    case LOP_JUMPIFLE:
    case LOP_JUMPIFLT:
    case LOP_JUMPIFNOTEQ:
    case LOP_JUMPIFNOTLE:
    case LOP_JUMPIFNOTLT:
    {
        ConditionState cond = evaluateComparisonCondition(instRepr->op, instRepr->ops[0], instRepr->ops[1]);
        if (cond == ConditionState::Unknown)
            return ConstnessLattice(this->state->unknownConditionConstness({instRepr->ops[0], instRepr->ops[1]}));

        bool negated = (instRepr->op == LOP_JUMPIFNOTEQ || instRepr->op == LOP_JUMPIFNOTLE || instRepr->op == LOP_JUMPIFNOTLT);
        bool takesJump = (cond == ConditionState::AlwaysTrue) != negated;
        return ConstnessLattice(Constness::ImmConstant, impl->makeImm(takesJump));
    }

    case LOP_JUMPXEQKNIL:
    case LOP_JUMPXEQKB:
    case LOP_JUMPXEQKN:
    case LOP_JUMPXEQKS:
    {
        ConditionState cond = evaluateXeqkCondition(instRepr);
        if (cond == ConditionState::Unknown)
            return ConstnessLattice(this->state->unknownConditionConstness({instRepr->ops[0]}));

        const BcOp& negImmOp = instRepr->ops[1];
        bool negated = !impl->falsey(negImmOp);
        bool takesJump = (cond == ConditionState::AlwaysTrue) != negated;

        return ConstnessLattice(Constness::ImmConstant, impl->makeImm(takesJump));
    }

    case LOP_JUMP:
    case LOP_JUMPBACK:
    default:
        return ConstnessLattice(Constness::NotAConstant);
    }
}

} // namespace Bytecode
} // namespace Luau
