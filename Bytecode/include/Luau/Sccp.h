// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Bytecode.h"
#include "Luau/BytecodeGraph.h"
#include "Luau/BytecodeUtils.h"
#include "Luau/BytecodeValidation.h"
#include "Luau/VecDeque.h"

#include <cstdint>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <utility>

namespace Luau
{
namespace Bytecode
{

// SCCP is generic over the constant representation
// Each instantiation inherits VmConstOps with the operations the pass needs to evaluate constants
struct VmConstOps
{
    virtual std::optional<BcOp> evaluate(const BcOp& lhsOp, const BcOp& rhsOp, LuauOpcode op) const = 0;
    virtual bool falsey(const BcOp& falseyOp) const = 0;

    // standard three way comparison: -1 if lhsOp < rhsOp, 0 if lhsOp == rhsOp, 1 if lhsOp > rhsOp
    virtual int cmp(const BcOp& lhsOp, const BcOp& rhsOp) const = 0;
    virtual int cmp(const BcOp& lhsOp, const BcImm& rhs) const = 0;

    virtual BcOp makeNil() const = 0;
    virtual BcImm makeImm(bool value) const = 0;
    virtual BcImm makeImm(int32_t value) const = 0;

    // true if the VmConst supports ordering comparisons (number, integer, string)
    virtual bool isOrderable(const BcOp& vmConstOp) const = 0;
    virtual bool kindEquals(const BcOp& lhsOp, const BcOp& rhsOp) const = 0;

    // returns std::nullopt if the comparison is not supported
    // rhsOp may either be a VmConst or an Imm, lhs only VmConst
    virtual std::optional<bool> eq(const BcOp& lhsOp, const BcOp& rhsOp) const = 0;
    virtual std::optional<bool> eq(const BcOp& lhsOp, bool rhs) const = 0;
    virtual std::optional<bool> eq(const BcOp& lhsOp, int32_t rhs) const = 0;

    // only true for LUA_TNUMBER
    virtual bool isArithmeticConstant(const BcOp& vmConstOp) const = 0;

    virtual BcRef<BcImm> asImm(BcOp op) const = 0;

    VmConstOps() = default;
    virtual ~VmConstOps() = default;
    VmConstOps(const VmConstOps&) = default;
    VmConstOps(VmConstOps&&) = delete;
    VmConstOps& operator=(const VmConstOps&) = default;
    VmConstOps& operator=(VmConstOps&&) = delete;
};

struct BcVmConstImpl : public VmConstOps
{
    std::optional<BcOp> evaluate(const BcOp& lhsOp, const BcOp& rhsOp, LuauOpcode op) const override;
    bool falsey(const BcOp& falseyOp) const override;

    int cmp(const BcOp& lhsOp, const BcOp& rhsOp) const override;
    int cmp(const BcOp& lhsOp, const BcImm& rhs) const override;

    BcOp makeNil() const override;
    BcImm makeImm(bool value) const override;
    BcImm makeImm(int32_t value) const override;
    BcRef<BcImm> asImm(BcOp op) const override;

    bool isOrderable(const BcOp& vmConstOp) const override;
    bool kindEquals(const BcOp& lhsOp, const BcOp& rhsOp) const override;

    std::optional<bool> eq(const BcOp& lhsOp, const BcOp& rhsOp) const override;
    std::optional<bool> eq(const BcOp& lhsOp, bool rhs) const override;
    std::optional<bool> eq(const BcOp& lhsOp, int32_t rhs) const override;

    bool isArithmeticConstant(const BcOp& vmConstOp) const override;

    explicit BcVmConstImpl(BcFunction<BcVmConst>& func)
        : VmConstOps()
        , func(func)
    {
    }

    BcFunction<BcVmConst>& func;
};


enum class Constness
{
    Undetermined, //  lattice top
    NotAConstant, //  lattice bottom
    VmConstant,
    ImmConstant,
};

struct ConstnessLattice
{
    Constness kind = Constness::Undetermined;
    std::optional<BcOp> vmConst = std::nullopt;
    std::optional<BcImm> immConst = std::nullopt;

    ConstnessLattice() = default;

    ConstnessLattice(Constness kind, BcOp bcOp)
        : kind(kind)
        , vmConst(bcOp)
    {
        LUAU_ASSERT(kind == Constness::VmConstant);
    }

    ConstnessLattice(Constness kind, BcImm imm)
        : kind(kind)
        , vmConst(std::nullopt)
        , immConst(imm)
    {
        LUAU_ASSERT(kind == Constness::ImmConstant);
    }

    explicit ConstnessLattice(Constness kind)
        : kind(kind)
        , vmConst(std::nullopt)
        , immConst(std::nullopt)
    {
    }

    ConstnessLattice merge(const ConstnessLattice& other) const
    {
        // Undetermined is lattice top: meeting with it yields the other operand
        if (kind == Constness::Undetermined)
            return other;
        if (other.kind == Constness::Undetermined)
            return *this;
        // two equal constants meet to themselves; anything else falls to bottom
        if (*this == other)
            return *this;
        return ConstnessLattice(Constness::NotAConstant);
    }

    bool operator==(const ConstnessLattice& other) const
    {
        if (kind != other.kind)
            return false;
        if (kind == Constness::ImmConstant)
            return immConst == other.immConst;
        if (kind == Constness::VmConstant)
            return vmConst == other.vmConst;
        return true;
    }

    bool operator!=(const ConstnessLattice& other) const
    {
        return !(*this == other);
    }
};

enum class ConditionState
{
    AlwaysFalse,
    AlwaysTrue,
    Unknown,
};

struct JumpTarget
{
    bool dead = false;
    BcOp blockOp;
    ConditionState condition = ConditionState::Unknown;
};

using OpConstness = std::unordered_map<BcOp, ConstnessLattice, BcOpHash>;

struct SccpState
{
    OpConstness opConstness;

    ConstnessLattice operandLattice(const BcOp& op)
    {
        if (op.kind == BcOpKind::Proj || op.kind == BcOpKind::VmReg || op.kind == BcOpKind::VmUpvalue)
            return ConstnessLattice(Constness::NotAConstant);
        return opConstness[op];
    }

    // an unresolved condition is bottom if any operand is bottom, else top
    Constness unknownConditionConstness(std::initializer_list<BcOp> ops)
    {
        for (const BcOp& op : ops)
        {
            ConstnessLattice lat = operandLattice(op);
            if (lat.kind == Constness::NotAConstant)
                return Constness::NotAConstant;
        }
        return Constness::Undetermined;
    }
};

} // namespace Bytecode
} // namespace Luau
