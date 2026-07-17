// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Bytecode.h"
#include "Luau/BytecodeGraph.h"
#include "Luau/BytecodeUtils.h"
#include "Luau/BytecodeValidation.h"
#include "Luau/VecDeque.h"
#include "Luau/DenseHash2.h"

#include <cstdint>
#include <optional>
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

    virtual double asNumber(const BcOp& vmConstOp) const = 0;

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

    double asNumber(const BcOp& vmConstOp) const override;

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

using OpConstness = DenseHashMap2<BcOp, ConstnessLattice, BcOpHash>;

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

class SccpInterpreter
{
public:
    explicit SccpInterpreter(VmConstOps* impl, SccpState* state)
        : impl(impl)
        , state(state)
    {
    }

    ConditionState evaluateCondition(const BcOp& op);
    ConditionState evaluateComparisonCondition(LuauOpcode op, const BcOp& lhs, const BcOp& rhs);
    ConditionState evaluateXeqkCondition(BcRef<BcInst> inst);

    ConstnessLattice evaluateArith(LuauOpcode op, BcRef<BcInst> instRepr);
    ConstnessLattice evaluate(LuauOpcode op, BcRef<BcInst> instRepr);


private:
    VmConstOps* impl;
    SccpState* state;
};

template<typename VmConst>
struct Sccp
{
    BcFunction<VmConst>& func;
    VmConstOps* impl;

    SccpState state;
    SccpInterpreter interpreter;

    // this maps a block (index) to its predecessors that it was reached from
    // if a block is not in this map, it is unreachable
    DenseHashMap2<uint32_t, DenseHashSet2<BcOp, BcOpHash>> blockUses;

    VecDeque<BcOp> flowWorklist;
    DenseHashSet2<BcOp, BcOpHash> flowWorklistSet;

    // when a def's lattice value changes, its uses must be re-evaluated
    VecDeque<BcOp> ssaWorklist;

    explicit Sccp(BcFunction<VmConst>& func, VmConstOps* impl)
        : func(func)
        , impl(impl)
        , interpreter(impl, &state)
    {
    }

    ConstnessLattice makeBoolImm(bool value)
    {
        BcImm imm{};
        imm.kind = BcImmKind::Boolean;
        imm.valueBoolean = value;
        return ConstnessLattice(Constness::ImmConstant, imm);
    }

    std::optional<BcOp> getFallthrough(BcRef<BcBlock> block)
    {
        std::optional<BcOp> fallthrough;
        for (auto& succOp : block->successors)
        {
            if (succOp.kind == BcBlockEdgeKind::Fallthrough)
            {
                if (fallthrough)
                {
                    LUAU_ASSERT(!"Multiple fallthroughs");
                    return std::nullopt;
                }

                fallthrough = succOp.target;
            }
        }

        return fallthrough;
    }

    // builds the target/fallthrough pair for a two-way branch
    // targetTakenOnTrue says which edge the jump takes when the condition holds; the untaken edge is marked dead when the condition resolves
    // a cond of Unknown leaves both edges live, which also covers branches whose condition the pass never folds (loops, CMPPROTO)
    std::vector<JumpTarget> conditionalTargets(BcRef<BcInst> inst, const BcOp& target, ConditionState cond, bool targetTakenOnTrue)
    {
        LUAU_ASSERT(target.kind == BcOpKind::Block);
        std::optional<BcOp> fallthrough = getFallthrough(func.block(inst->block));
        LUAU_ASSERT(fallthrough);

        bool targetDead = false;
        bool fallthroughDead = false;
        if (cond == ConditionState::AlwaysTrue)
        {
            targetDead = !targetTakenOnTrue;
            fallthroughDead = targetTakenOnTrue;
        }
        else if (cond == ConditionState::AlwaysFalse)
        {
            targetDead = targetTakenOnTrue;
            fallthroughDead = !targetTakenOnTrue;
        }

        return {{targetDead, target, cond}, {fallthroughDead, *fallthrough, cond}};
    }

    // when the condition is a known constant, the untaken path is marked dead and each target carries its resolved condition
    std::vector<JumpTarget> jumpTargets(BcRef<BcInst> inst)
    {
        switch (inst->op)
        {
        case LOP_JUMP:
        case LOP_JUMPBACK:
        {
            const BcOp& targetOp = inst->ops[0];
            LUAU_ASSERT(targetOp.kind == BcOpKind::Block);
            return {{false, targetOp, ConditionState::AlwaysTrue}};
        }
        case LOP_JUMPIF:
        case LOP_JUMPIFNOT:
        {
            ConditionState cond = interpreter.evaluateCondition(inst->ops[0]);
            return conditionalTargets(inst, inst->ops[1], cond, inst->op == LOP_JUMPIF);
        }
        case LOP_JUMPIFEQ:
        case LOP_JUMPIFLE:
        case LOP_JUMPIFLT:
        case LOP_JUMPIFNOTEQ:
        case LOP_JUMPIFNOTLE:
        case LOP_JUMPIFNOTLT:
        {
            ConditionState cond = interpreter.evaluateComparisonCondition(inst->op, inst->ops[0], inst->ops[1]);
            bool negated = (inst->op == LOP_JUMPIFNOTEQ || inst->op == LOP_JUMPIFNOTLE || inst->op == LOP_JUMPIFNOTLT);
            return conditionalTargets(inst, inst->ops[2], cond, !negated);
        }
        case LOP_JUMPXEQKNIL:
        case LOP_JUMPXEQKB:
        case LOP_JUMPXEQKN:
        case LOP_JUMPXEQKS:
        {
            ConditionState cond = interpreter.evaluateXeqkCondition(inst);
            bool negated = func.immOp(inst->ops[1]).valueBoolean;
            return conditionalTargets(inst, inst->ops[2], cond, !negated);
        }
        case LOP_FORNPREP:
        case LOP_FORNLOOP:
        case LOP_FORGPREP:
        case LOP_FORGPREP_NEXT:
        case LOP_FORGPREP_INEXT:
            return conditionalTargets(inst, inst->ops[3], ConditionState::Unknown, true);
        case LOP_FORGLOOP:
            // FORGLOOP has two leading imm operands FORGPREP* lack, so its target is ops[5]
            return conditionalTargets(inst, inst->ops[5], ConditionState::Unknown, true);
        case LOP_CMPPROTO:
            return conditionalTargets(inst, inst->ops[2], ConditionState::Unknown, true);
        case LOP_JUMPX:
            LUAU_ASSERT(!"Should have never parsed this");
            [[fallthrough]];
        default:
            return {};
        }
    }

    // for each operand, evaluate and merge the lattice with the phi lattice
    void visitPhi(BcRef<BcPhi> phi)
    {
        LUAU_ASSERT(phi->ops.size() > 0);

        ConstnessLattice fold;

        for (size_t i = 0; i < phi->ops.size(); i++)
        {
            BcOp op = phi->ops[i];

            ConstnessLattice lattice = state.operandLattice(op);
            fold = lattice.merge(fold);
        }

        const ConstnessLattice& prevLattice = state.opConstness[phi.op];
        if (fold != prevLattice)
        {
            for (BcOp use : phi->uses)
                ssaWorklist.push_back(use);
        }
        state.opConstness[phi.op] = fold;
    }

    // evaluate an instruction, comparing against the previous lattice value, and inserting all uses into the SSA worklist if it changed
    void visitInst(BcRef<BcInst> inst)
    {
        // CAPTURE REF can be mutated externally via SETUPVAL
        // the SSA graph doesn't model that alias, so mark the source non-constant to avoid folding a stale value
        if (inst->op == LOP_CAPTURE && inst->ops.size() >= 2)
        {
            const BcOp& captureTypeOp = inst->ops[0];
            LUAU_ASSERT(captureTypeOp.kind == BcOpKind::Imm);
            const BcImm& captureImm = func.immOp(captureTypeOp);
            if (captureImm.kind == BcImmKind::Int && captureImm.valueInt == LCT_REF)
            {
                const BcOp& srcOp = inst->ops[1];
                ConstnessLattice prev = state.opConstness[srcOp];
                if (prev.kind != Constness::NotAConstant && (srcOp.kind == BcOpKind::Inst || srcOp.kind == BcOpKind::Phi))
                {
                    state.opConstness[srcOp] = ConstnessLattice(Constness::NotAConstant);


                    for (BcOp use : usesOf(func, srcOp))
                        ssaWorklist.push_back(use);
                }
            }
        }

        ConstnessLattice lattice = interpreter.evaluate(inst->op, inst);
        const ConstnessLattice& prevLattice = state.opConstness[inst.op];

        ConstnessLattice newVal = lattice.merge(prevLattice);
        if (newVal != prevLattice)
        {
            for (BcOp use : inst->uses)
                ssaWorklist.push_back(use);
        }

        for (const JumpTarget& target : jumpTargets(inst))
        {
            uint32_t blockIdx = func.getBlockIndex(func.blockOp(target.blockOp));
            if (!target.dead)
            {
                blockUses[blockIdx].insert(inst->block);
                if (!flowWorklistSet.find(target.blockOp))
                    flowWorklist.push_back(target.blockOp);
            }
        }

        state.opConstness[inst.op] = newVal;
    }

    void propagate()
    {
        BcRef<BcBlock> entryBlock = func.block(func.entryBlock);
        BcRef<BcBlock> exitBlock = func.block(func.exitBlock);
        // the entry block is always live
        blockUses[func.getBlockIndex(*entryBlock)].insert(entryBlock.op);
        // the exit block is always live per serialization requirements
        blockUses[func.getBlockIndex(*exitBlock)].insert(exitBlock.op);

        flowWorklist.push_back(func.entryBlock);
        while (!flowWorklist.empty() || !ssaWorklist.empty())
        {
            while (!flowWorklist.empty())
            {
                BcOp blockOp = flowWorklist.front();
                BcRef<BcBlock> block = func.block(blockOp);

                flowWorklist.pop_front();
                if (flowWorklistSet.contains(block.op))
                    continue;

                for (BcOp& phiOp : block->phis)
                {
                    LUAU_ASSERT(phiOp.kind == BcOpKind::Phi);
                    visitPhi(func.phi(phiOp));
                }

                for (BcOp& op : block->ops)
                {
                    LUAU_ASSERT(op.kind == BcOpKind::Inst);
                    visitInst(func.inst(op));
                }

                bool blockEndsWithBranch = false;
                if (!block->ops.empty() && block->ops.back().kind == BcOpKind::Inst)
                    blockEndsWithBranch = !jumpTargets(func.inst(block->ops.back())).empty();

                for (BcBlockEdge succEdge : block->successors)
                {
                    if (succEdge.kind == BcBlockEdgeKind::Fallthrough)
                    {
                        uint32_t succIdx = func.getBlockIndex(func.blockOp(succEdge.target));
                        // If this block ends with a branch and the fallthrough wasn't already added by visitInst, skip it
                        if (blockEndsWithBranch && !blockUses[succIdx].contains(block.op))
                            continue;

                        blockUses[succIdx].insert(block.op);
                        if (!flowWorklistSet.contains(succEdge.target))
                            flowWorklist.push_back(succEdge.target);
                    }
                }
                flowWorklistSet.insert(block.op);
            }

            while (!ssaWorklist.empty())
            {

                BcOp op = ssaWorklist.front();
                ssaWorklist.pop_front();

                if (op.kind == BcOpKind::Inst)
                    visitInst(func.inst(op));
                else if (op.kind == BcOpKind::Phi)
                    visitPhi(func.phi(op));
            }
        }
    }

    BcOp makeConstantOp(const ConstnessLattice& lattice)
    {
        if (lattice.kind == Constness::VmConstant)
            return func.addConst(lattice.vmConst.value());
        else if (lattice.kind == Constness::ImmConstant)
            return func.addImm(lattice.immConst.value());

        LUAU_ASSERT(!"makeConstantOp called on non-constant lattice value");
        return BcOp{};
    }

    void replaceOperand(BcRef<BcInst> inst, BcOp oldOp, BcOp newOp)
    {
        for (BcOp& op : inst->ops)
        {
            if (op == oldOp)
                op = newOp;
        }
    }

    void replacePhiOperand(BcRef<BcPhi> phi, BcOp oldOp, BcOp newOp)
    {
        for (BcOp& op : phi->ops)
        {
            if (op == oldOp)
                op = newOp;
        }
    }

    bool isLoadInst(BcOp op)
    {
        if (op.kind != BcOpKind::Inst)
            return false;
        LuauOpcode opcode = func.inst(op)->op;
        return opcode == LOP_LOADK || opcode == LOP_LOADKX || opcode == LOP_LOADN || opcode == LOP_LOADB || opcode == LOP_LOADNIL;
    }

    // rewrite a folded instruction in-place to a load instruction
    // op identity is preserved so all existing uses remain valid
    void rewriteToLoad(BcOp op, const ConstnessLattice& lattice)
    {
        BcRef<BcInst> inst = func.inst(op);
        for (BcOp& usedOp : inst->ops)
        {
            eraseUse(op, usedOp);
        }

        inst->ops.clear();
        if (lattice.kind == Constness::VmConstant)
        {
            inst->op = LOP_LOADK;
            BcOp constOp = lattice.vmConst.value();
            inst->ops.push_back(constOp);
        }
        else
        {
            LUAU_ASSERT(lattice.kind == Constness::ImmConstant);
            const BcImm& imm = lattice.immConst.value();
            inst->op = (imm.kind == BcImmKind::Boolean) ? LOP_LOADB : LOP_LOADN;
            inst->ops.push_back(func.addImm(imm));
        }
    }

    void eraseOp(BcOp op)
    {
        BcRef<BcInst> inst = func.inst(op);
        BcRef<BcBlock> block = func.block(inst->block);
        block->ops.erase(std::remove(block->ops.begin(), block->ops.end(), op), block->ops.end());
    }

    void eraseUse(BcOp userOp, BcOp usedOp)
    {
        if (usedOp.kind == BcOpKind::Inst)
        {
            BcRef<BcInst> usedInst = func.inst(usedOp);
            usedInst->uses.erase(std::remove(usedInst->uses.begin(), usedInst->uses.end(), userOp), usedInst->uses.end());
        }
        else if (usedOp.kind == BcOpKind::Phi)
        {
            BcRef<BcPhi> usedPhi = func.phi(usedOp);
            usedPhi->uses.erase(std::remove(usedPhi->uses.begin(), usedPhi->uses.end(), userOp), usedPhi->uses.end());
        }
    }

    void removeDeadEdges(BcRef<BcInst> inst)
    {
        std::vector<JumpTarget> targets = jumpTargets(inst);

        BcRef<BcBlock> block = func.block(inst->block);

        BcOp liveTarget{};
        bool hasLive = false;

        for (const JumpTarget& target : targets)
        {
            if (target.dead)
            {
                BcEdges& succs = block->successors;
                unsigned writeIdx = 0;
                for (unsigned i = 0; i < succs.size(); i++)
                {
                    if (!(succs[i].target == target.blockOp))
                        succs[writeIdx++] = succs[i];
                }
                succs.resize(writeIdx);
            }
            else
            {
                liveTarget = target.blockOp;
                hasLive = true;
            }
        }

        if (!hasLive)
            return;

        // ensure the live target has a fallthrough edge
        bool hasFallthrough = false;
        for (unsigned i = 0; i < block->successors.size(); i++)
        {
            if (block->successors[i].target == liveTarget)
            {
                block->successors[i].kind = BcBlockEdgeKind::Fallthrough;
                hasFallthrough = true;
                break;
            }
        }

        if (!hasFallthrough)
            block->successors.push_back({BcBlockEdgeKind::Fallthrough, liveTarget});
    }

    void replaceUses()
    {
        for (auto& [op, lattice] : state.opConstness)
        {
            if (lattice.kind != Constness::ImmConstant && lattice.kind != Constness::VmConstant)
                continue;
            if (op.kind != BcOpKind::Inst)
                continue;
            if (isLoadInst(op))
                continue;

            BcRef<BcInst> inst = func.inst(op);
            LUAU_ASSERT(inst->op != LOP_JUMPX);

            // isJumpD is safe to use here because JUMPX is not parsed by the GraphParser
            if (isJumpD(inst->op))
            {
                removeDeadEdges(inst);
                eraseOp(op);
            }
            else
                rewriteToLoad(op, lattice);
        }
    }

    void simplifyPhis()
    {
        for (BcOp blockOp : flowWorklistSet)
        {
            BcRef<BcBlock> block = func.block(blockOp);

            for (auto it = block->phis.begin(); it != block->phis.end();)
            {
                BcOp op = *it;
                LUAU_ASSERT(op.kind == BcOpKind::Phi);

                BcRef<BcPhi> phi = func.phi(op);
                if (phi->ops.empty())
                {
                    ++it;
                    continue;
                }

                BcOp unique = phi->ops[0];
                bool allSame = true;
                for (size_t i = 1; i < phi->ops.size(); i++)
                {
                    if (phi->ops[i] != unique)
                    {
                        allSame = false;
                        break;
                    }
                }

                if (!allSame)
                {
                    ++it;
                    continue;
                }

                for (BcOp use : usesOf(func, op))
                {
                    if (use.kind == BcOpKind::Inst)
                        replaceOperand(func.inst(use), op, unique);
                    else if (use.kind == BcOpKind::Phi)
                        replacePhiOperand(func.phi(use), op, unique);

                    usesOf(func, unique).push_back(use);
                }
                phi->uses.clear();
                it = block->phis.erase(it);
            }
        }
    }

    void updateBlockUses()
    {
        // mark dead blocks by forward reachability from entry, not by blockUses
        // (which can miss blocks depending on worklist ordering)
        DenseHashSet2<uint32_t> reachable;
        std::vector<uint32_t> worklist;

        uint32_t entryIdx = func.getBlockIndex(*func.block(func.entryBlock));
        uint32_t exitIdx = func.getBlockIndex(*func.block(func.exitBlock));
        reachable.insert(entryIdx);
        reachable.insert(exitIdx);
        worklist.push_back(entryIdx);

        while (!worklist.empty())
        {
            uint32_t idx = worklist.back();
            worklist.pop_back();
            BcBlock& blk = func.blocks[idx];
            for (const BcBlockEdge& edge : blk.successors)
            {
                uint32_t succIdx = func.getBlockIndex(func.blockOp(edge.target));
                if (!reachable.contains(succIdx))
                {
                    reachable.insert(succIdx);
                    worklist.push_back(succIdx);
                }
            }
        }

        for (BcBlock& block : func.blocks)
        {
            uint32_t blockidx = func.getBlockIndex(block);
            block.useCount = blockUses[blockidx].size();
            if (!reachable.contains(blockidx))
                block.flags |= BcBlockFlag::Dead;
        }
    }

    static std::optional<LuauOpcode> arithToKOpcode(LuauOpcode op)
    {
        switch (op)
        {
        case LOP_ADD:
            return LOP_ADDK;
        case LOP_SUB:
            return LOP_SUBK;
        case LOP_MUL:
            return LOP_MULK;
        case LOP_DIV:
            return LOP_DIVK;
        case LOP_MOD:
            return LOP_MODK;
        case LOP_POW:
            return LOP_POWK;
        default:
            return std::nullopt;
        }
    }

    // pure value producers can be removed once nothing references their result
    bool isPureProducer(LuauOpcode op) const
    {
        switch (op)
        {
        case LOP_LOADK:
        case LOP_LOADKX:
        case LOP_LOADN:
        case LOP_LOADB:
        case LOP_LOADNIL:
        case LOP_GETUPVAL:
            return true;
        default:
            return false;
        }
    }

    std::optional<Reg> registerOf(BcOp op)
    {
        if (op.kind == BcOpKind::VmReg)
            return static_cast<Reg>(op.index);
        if (auto it = func.regs.find(op); it != func.regs.end())
            return it->second;
        return std::nullopt;
    }

    void eraseDeadProducer(BcOp op)
    {
        if (op.kind != BcOpKind::Inst)
            return;
        BcRef<BcInst> inst = func.inst(op);
        if (!isPureProducer(inst->op))
            return;
        if (!inst->uses.empty())
            return;
        eraseOp(op);
    }

    // values that are known constant used in arithmetic operations can be turned into their KR or RK variants
    // sub and div have both KR and RK
    void arithToK()
    {
        for (BcBlock& block : func.blocks)
        {
            uint32_t blockidx = func.getBlockIndex(block);
            if (blockUses[blockidx].empty())
                continue;

            std::vector<BcOp> toErase;
            toErase.reserve(block.ops.size());

            for (auto bcOpIt = block.ops.begin(); bcOpIt != block.ops.end(); ++bcOpIt)
            {
                BcOp& op = *bcOpIt;

                BcRef<BcInst> inst = func.inst(op);

                std::optional<LuauOpcode> kOpcode = arithToKOpcode(inst->op);
                if (!kOpcode || inst->ops.size() != 2)
                    continue;

                BcOp lhs = inst->ops[0];
                BcOp rhs = inst->ops[1];
                // we can safely assume that, at most, one of these can be a VmConstant
                // if they both were constant, the arith would have been folded
                // TODO: ImmConstant?
                ConstnessLattice lhsLat = state.operandLattice(lhs);
                ConstnessLattice rhsLat = state.operandLattice(rhs);

                BcOp nonConstantOp;
                ConstnessLattice constantK;
                bool rk = false;

                auto isConstNumber = [&](const ConstnessLattice& lat) -> bool
                {
                    return lat.kind == Constness::VmConstant && lat.vmConst && impl->isArithmeticConstant(lat.vmConst.value());
                };

                if (isConstNumber(rhsLat) && lhsLat.kind == Constness::NotAConstant)
                {
                    // can fold this to a <ARITH>K variant
                    nonConstantOp = lhs;
                    constantK = rhsLat;
                }
                else if (isConstNumber(lhsLat) && rhsLat.kind == Constness::NotAConstant)
                {
                    if (inst->op == LOP_ADD || inst->op == LOP_MUL || inst->op == LOP_SUB || inst->op == LOP_DIV)
                    {
                        // LOP_ADD and LOP_MUL are commutative
                        // LOP_SUB and LOP_DIV can emit the RK variant

                        nonConstantOp = rhs;
                        constantK = lhsLat;

                        if (inst->op == LOP_SUB)
                        {
                            kOpcode = LOP_SUBRK;
                            rk = true;
                        }
                        else if (inst->op == LOP_DIV)
                        {
                            kOpcode = LOP_DIVRK;
                            rk = true;
                        }
                    }
                }
                else
                {
                    continue;
                }

                BcOp prevConstOperand = (nonConstantOp == lhs) ? rhs : lhs;
                // we can do some potential folding here now that we know one operand is constant
                // for instance, adds of zero, muls of zero or 1, pows of zero or 1, etc
                double valueNumber = impl->asNumber(constantK.vmConst.value());
                if (valueNumber == 0)
                {
                    if (inst->op == LOP_ADD || inst->op == LOP_SUB)
                    {
                        inst->op = LOP_MOVE;
                        inst->ops.clear();
                        inst->ops.push_back(nonConstantOp);
                    }
                    else if (inst->op == LOP_MUL)
                    {
                        inst->op = LOP_LOADN;
                        inst->ops.clear();
                        BcImm imm{BcImmKind::Int};
                        imm.valueInt = 0;
                        BcOp immOp = func.addImm(imm);
                        inst->ops.push_back(immOp);
                    }
                    else if (inst->op == LOP_POW)
                    {
                        inst->op = LOP_LOADN;
                        inst->ops.clear();
                        BcImm imm{BcImmKind::Int};
                        imm.valueInt = 1;
                        BcOp immOp = func.addImm(imm);
                        inst->ops.push_back(immOp);
                    }
                }
                else if (valueNumber == 1)
                {
                    if (inst->op == LOP_MUL || inst->op == LOP_POW || inst->op == LOP_DIV)
                    {
                        inst->op = LOP_MOVE;
                        inst->ops.clear();
                        inst->ops.push_back(nonConstantOp);
                    }
                }
                else
                {
                    inst->op = *kOpcode;
                    inst->ops.clear();
                    if (!rk)
                    {
                        inst->ops.push_back(nonConstantOp);
                        inst->ops.push_back(constantK.vmConst.value());
                    }
                    else
                    {
                        // SUBRK and DIVRK expect B as the constant table index
                        inst->ops.push_back(constantK.vmConst.value());
                        inst->ops.push_back(nonConstantOp);
                    }
                }


                toErase.push_back(prevConstOperand);
            }

            for (BcOp op : toErase)
            {
                eraseDeadProducer(op);
            }
        }
    }

    void rewrite()
    {
        arithToK();
        replaceUses();
        simplifyPhis();
        updateBlockUses();
    }
};

template<typename VmConst>
void foldConstants(BcFunction<VmConst>& func, VmConstOps& impl)
{
    Sccp<VmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();
}

} // namespace Bytecode
} // namespace Luau
