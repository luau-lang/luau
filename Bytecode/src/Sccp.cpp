// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Sccp.h"
#include "Luau/BytecodeGraph.h"

#include <cmath>

namespace Luau
{
namespace Bytecode
{

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
    if (isNumber(lhsOp) && isNumber(rhsOp))
    {
        if (std::optional<double> resultOpt = evaluateNumberBinaryOp(asNumber(lhsOp), asNumber(rhsOp), op))
        {
            BcVmConst result;
            result.kind = BcVmConstKind::Number;
            result.valueNumber = *resultOpt;
            return findOrAddConst(func, result);
        }

        return std::nullopt;
    }

    // TODO: vector support
    return std::nullopt;
}

bool BcVmConstImpl::falsey(const BcOp& falseyOp) const
{
    if (isNil(falseyOp))
        return true;

    if (isBoolean(falseyOp))
        return asBoolean(falseyOp) == false;

    return false;
}

bool BcVmConstImpl::compare(const BcOp& lhsOp, const BcOp& rhsOp, BcCondition condition) const
{
    LUAU_ASSERT(isOrderable(lhsOp));
    LUAU_ASSERT(isOrderable(rhsOp));

    if (isNumber(lhsOp) && isNumber(rhsOp))
        return bcCompare(asNumber(lhsOp), asNumber(rhsOp), condition);

    BcVmConst& lhs = func.constOp(lhsOp);
    BcVmConst& rhs = func.constOp(rhsOp);
    LUAU_ASSERT(lhs.kind == rhs.kind);

    switch (lhs.kind)
    {
    case BcVmConstKind::String:
        return bcCompare(lhs.valueString, rhs.valueString, condition);
    default:
        LUAU_ASSERT(!"unsupported comparison");
        return false;
    }
}

BcOp BcVmConstImpl::makeNil() const
{
    BcVmConst result{};
    result.kind = BcVmConstKind::Nil;
    return findOrAddConst(func, result);
}

BcOp BcVmConstImpl::makeImmBool(bool value) const
{
    return func.addImmBool(value);
}

BcRef<BcImm> BcVmConstImpl::asImm(BcOp op) const
{
    return func.imm(op);
}

bool BcVmConstImpl::isOrderable(const BcOp& op) const
{
    if (op.kind == BcOpKind::Imm)
    {
        BcImm& imm = func.immOp(op);
        return imm.kind == BcImmKind::Int;
    }

    if (op.kind == BcOpKind::VmConst)
    {
        BcVmConst& vmConst = func.constOp(op);
        return vmConst.kind == BcVmConstKind::Number || vmConst.kind == BcVmConstKind::String;
    }

    return false;
}

bool BcVmConstImpl::kindEquals(const BcOp& lhsOp, const BcOp& rhsOp) const
{
    if (isBoolean(lhsOp) && isBoolean(rhsOp))
        return true;

    if (isNumber(lhsOp) && isNumber(rhsOp))
        return true;

    if (lhsOp.kind != BcOpKind::VmConst || rhsOp.kind != BcOpKind::VmConst)
        return false;

    BcVmConst& lhs = func.constOp(lhsOp);
    BcVmConst& rhs = func.constOp(rhsOp);

    return lhs.kind == rhs.kind;
}

bool BcVmConstImpl::fullyequal(const BcOp& lhsOp, const BcOp& rhsOp) const
{
    if (lhsOp.kind != rhsOp.kind)
        return false;

    if (lhsOp.kind == BcOpKind::Imm)
        return func.immOp(lhsOp) == func.immOp(rhsOp);

    if (lhsOp.kind == BcOpKind::VmConst)
        return lhsOp.index == rhsOp.index; // This relies on findOrAddConst for de-duplication

    LUAU_ASSERT(!"unsupported kind");
    return false;
}

std::optional<bool> BcVmConstImpl::eq(const BcOp& lhsOp, const BcOp& rhsOp) const
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
        BcVmConst& lhs = func.constOp(lhsOp);
        BcVmConst& rhs = func.constOp(rhsOp);

        if (lhs.kind != rhs.kind)
            return false;

        if (lhs.kind == BcVmConstKind::Integer && rhs.kind == BcVmConstKind::Integer)
            return lhs.valueInteger == rhs.valueInteger;

        if (lhs.kind == BcVmConstKind::String && rhs.kind == BcVmConstKind::String)
            return lhs.valueString == rhs.valueString;
    }

    return std::nullopt;
}

bool BcVmConstImpl::isNil(const BcOp& op) const
{
    if (op.kind == BcOpKind::VmConst)
        return func.constOp(op).kind == BcVmConstKind::Nil;

    return false;
}

bool BcVmConstImpl::isBoolean(const BcOp& op) const
{
    if (op.kind == BcOpKind::Imm)
        return func.immOp(op).kind == BcImmKind::Boolean;

    if (op.kind == BcOpKind::VmConst)
        return func.constOp(op).kind == BcVmConstKind::Boolean;

    return false;
}

bool BcVmConstImpl::isNumber(const BcOp& op) const
{
    if (op.kind == BcOpKind::Imm)
        return func.immOp(op).kind == BcImmKind::Int;

    if (op.kind == BcOpKind::VmConst)
        return func.constOp(op).kind == BcVmConstKind::Number;

    return false;
}

bool BcVmConstImpl::asBoolean(const BcOp& op) const
{
    if (op.kind == BcOpKind::Imm)
    {
        BcImm& imm = func.immOp(op);
        LUAU_ASSERT(imm.kind == BcImmKind::Boolean && "use isBoolean first");
        return imm.valueBoolean;
    }

    if (op.kind == BcOpKind::VmConst)
    {
        BcVmConst& vmConst = func.constOp(op);
        LUAU_ASSERT(vmConst.kind == BcVmConstKind::Boolean && "use isBoolean first");
        return vmConst.valueBoolean;
    }

    LUAU_ASSERT(!"unsupported type, use isBoolean first");
    return false;
}

double BcVmConstImpl::asNumber(const BcOp& op) const
{
    if (op.kind == BcOpKind::Imm)
    {
        BcImm& imm = func.immOp(op);
        LUAU_ASSERT(imm.kind == BcImmKind::Int && "use isNumber first");
        return double(imm.valueInt);
    }

    if (op.kind == BcOpKind::VmConst)
    {
        BcVmConst& vmConst = func.constOp(op);
        LUAU_ASSERT(vmConst.kind == BcVmConstKind::Number && "use isNumber first");
        return vmConst.valueNumber;
    }

    LUAU_ASSERT(!"unsupported type, use isNumber first");
    return 0.0;
}

ConstnessLattice SccpInterpreter::evaluateArith(LuauOpcode opcode, BcRef<BcInst> instRepr)
{
    auto lhs = instRepr->ops[0];
    auto rhs = instRepr->ops[1];

    ConstnessLattice lhsConstness = this->state->operandLattice(lhs);
    ConstnessLattice rhsConstness = this->state->operandLattice(rhs);

    if (lhsConstness.kind == Constness::Constant && rhsConstness.kind == Constness::Constant)
    {
        std::optional<BcOp> vmConst = impl->evaluate(lhsConstness.constant.value(), rhsConstness.constant.value(), opcode);
        if (vmConst)
            return ConstnessLattice(Constness::Constant, vmConst.value());
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
        if (c.kind == Constness::Constant)
            return impl->isOrderable(c.constant.value());
        return false;
    };

    if (isOrderingOp && (!isOrderableLattice(lhsConst) || !isOrderableLattice(rhsConst)))
        return ConditionState::Unknown;

    if (lhsConst.kind == Constness::Constant && rhsConst.kind == Constness::Constant)
    {
        if (isOrderingOp)
        {
            if (!impl->kindEquals(lhsConst.constant.value(), rhsConst.constant.value()))
                return ConditionState::Unknown;

            bool condTrue = impl->compare(lhsConst.constant.value(), rhsConst.constant.value(), opcodeToCondition(op));
            return condTrue ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
        }
        else if (std::optional<bool> condTrueOpt = impl->eq(lhsConst.constant.value(), rhsConst.constant.value()))
        {
            bool condTrue = op == LOP_JUMPIFEQ ? *condTrueOpt : !*condTrueOpt;
            return condTrue ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
        }
    }

    return ConditionState::Unknown;
}

ConditionState SccpInterpreter::evaluateXeqkCondition(BcRef<BcInst> inst)
{
    ConstnessLattice valConst = this->state->operandLattice(inst->ops[0]);

    switch (inst->op)
    {
    case LOP_JUMPXEQKNIL:
        if (valConst.kind == Constness::Constant)
            return impl->isNil(*valConst.constant) ? ConditionState::AlwaysTrue : ConditionState::AlwaysFalse;
        break;
    case LOP_JUMPXEQKB:
    case LOP_JUMPXEQKN:
    case LOP_JUMPXEQKS:
    {
        if (valConst.kind == Constness::Constant)
        {
            if (std::optional<bool> eq = impl->eq(valConst.constant.value(), inst->ops[3]))
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
    if (lhs.kind == Constness::Constant)
        return impl->falsey(lhs.constant.value()) ? ConditionState::AlwaysFalse : ConditionState::AlwaysTrue;
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
        return ConstnessLattice(Constness::Constant, op);
    }
    case LOP_LOADB:
    {
        const BcOp& op = instRepr->ops[0];
        LUAU_ASSERT(op.kind == BcOpKind::Imm);
        LUAU_ASSERT(impl->asImm(op)->kind == BcImmKind::Boolean);
        return ConstnessLattice(Constness::Constant, op);
    }
    case LOP_LOADN:
    {
        const BcOp& op = instRepr->ops[0];
        LUAU_ASSERT(op.kind == BcOpKind::Imm);
        LUAU_ASSERT(impl->asImm(op)->kind == BcImmKind::Int);
        return ConstnessLattice(Constness::Constant, op);
    }
    case LOP_LOADNIL:
    {
        BcOp nilConst = impl->makeNil();
        return ConstnessLattice(Constness::Constant, nilConst);
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
        return ConstnessLattice(Constness::Constant, impl->makeImmBool(takesJump));
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

        return ConstnessLattice(Constness::Constant, impl->makeImmBool(cond == ConditionState::AlwaysTrue));
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

        return ConstnessLattice(Constness::Constant, impl->makeImmBool(takesJump));
    }

    case LOP_JUMP:
    case LOP_JUMPBACK:
    default:
        return ConstnessLattice(Constness::NotAConstant);
    }
}

} // namespace Bytecode
} // namespace Luau
