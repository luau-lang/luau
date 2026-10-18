// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/BytecodeGraph.h"
#include "Luau/BytecodeUtils.h"
#include "Luau/DenseHash.h"

#include <algorithm>
#include <array>
#include <bitset>
#include <cstdint>
#include <optional>
#include <vector>

namespace Luau
{
namespace Bytecode
{

struct SlotAllocationResult
{
    // if applied is false, the register map and frame size are unchanged and the removed move count is zero
    uint32_t movesRemoved = 0;
    uint8_t frameSizeBefore = 0;
    uint8_t frameSizeAfter = 0;
    bool applied = false;

    // diagnostic counters: how much coalescing headroom remains after this pass
    uint32_t movesRemaining = 0;
    uint32_t loopMovesRemaining = 0;

    explicit operator bool() const
    {
        return applied;
    }
};

// the allocator only moves ordinary values whose complete lifetime is contained in one basic block
// values involved in phis, captures, explicit vm-register references, or instructions with implicit register layouts retain their inherited slots
//
// TODO: add whole-cfg liveness with phi edge uses and multi-block intervals
// TODO: replace layout barriers with movable windows for calls, returns, loops, fastcalls, and multret
// TODO: coalesce phis and moves, splitting critical edges and emitting parallel copies on conflicts
// TODO: model ref captures and raw vm register constraints explicitly instead of reserving their slots
// TODO: verify conflicts, windows, captures, phis, and maxstacksize before publishing
inline bool isLayoutBarrier(LuauOpcode op)
{
    // these instructions depend on contiguous or otherwise implicit vm register layouts
    switch (op)
    {
    case LOP_NAMECALL:
    case LOP_NAMECALLUDATA:
    case LOP_CALL:
    case LOP_CALLFB:
    case LOP_RETURN:
    case LOP_CONCAT:
    case LOP_SETLIST:
    case LOP_FORNPREP:
    case LOP_FORNLOOP:
    case LOP_FORGPREP:
    case LOP_FORGPREP_NEXT:
    case LOP_FORGPREP_INEXT:
    case LOP_FORGLOOP:
    case LOP_FASTCALL:
    case LOP_FASTCALL1:
    case LOP_FASTCALL2:
    case LOP_FASTCALL2K:
    case LOP_FASTCALL3:
    case LOP_FASTPCALL:
    case LOP_GETVARARGS:
    case LOP_CAPTURE:
    case LOP_CLOSEUPVALS:
        return true;
    default:
        return false;
    }
}

template<typename VmConst>
struct SlotAllocator
{
    static constexpr uint16_t kUnassignedSlot = 256;

    struct Use
    {
        uint32_t block;
        uint32_t position;
        // whether the use requires inherited layout
        bool barrier;
    };

    struct Interval
    {
        BcOp value;
        uint32_t block = 0U; // block containing the complete lifetime

        uint32_t start = 0U;  // position immediately after the definition
        uint32_t finish = 0U; // position of the final use
        uint16_t slot = 0U;   // inherited slot, then assigned slot
    };

    struct Location
    {
        uint32_t block;
        uint32_t position;
    };

    BcFunction<VmConst>& function;
    // frame size before allocation
    uint8_t inheritedFrame;
    DenseHashSet<BcOp, BcOpHash> reachable;
    DenseHashMap<BcOp, Location, BcOpHash> locations;            // block-local location of each instruction
    std::vector<std::vector<uint32_t>> barriers;                 // layout barrier positions in each block
    DenseHashMap<BcOp, std::vector<Use>, BcOpHash> uses;         // uses of each instruction result
    DenseHashMap<BcOp, std::vector<BcOp>, BcOpHash> projections; // projections produced by each instruction
    DenseHashSet<BcOp, BcOpHash> fixed;                          // instructions retaining their inherited slots
    std::bitset<256> identitySlots;                              // slots referenced directly by vm register operands
    std::bitset<256> reservedSlots;                              // inherited slots excluded from reassignment
    std::vector<Interval> intervals;                             // movable single-block live intervals
    std::vector<uint16_t> assignedSlots;                         // candidate slots indexed by instruction

    uint16_t minimumFrame = 1; // lowest frame size required by reserved slots

    explicit SlotAllocator(BcFunction<VmConst>& function)
        : function(function)
        , inheritedFrame(function.maxstacksize)
        , barriers(function.blocks.size())
        , assignedSlots(function.instructions.size(), kUnassignedSlot)
    {
    }

    std::optional<int> immediate(const BcInst& instruction, size_t operand) const
    {
        if (operand >= instruction.ops.size())
            return std::nullopt;

        BcOp op = instruction.ops[operand];
        if (op.kind != BcOpKind::Imm || function.immediates[op.index].kind != BcImmKind::Int)
            return std::nullopt;

        return function.immediates[op.index].valueInt;
    }

    bool isProducer(const BcInst& instr) const
    {
        return ((instr.op == LOP_CALL || instr.op == LOP_CALLFB) && immediate(instr, 1) == -1) ||
               (instr.op == LOP_GETVARARGS && immediate(instr, 1) == -1);
    }

    bool isConsumer(const BcInst& instr) const
    {
        return ((instr.op == LOP_CALL || instr.op == LOP_CALLFB) && immediate(instr, 0) == -1) ||
               (instr.op == LOP_RETURN && immediate(instr, 0) == -1) || (instr.op == LOP_SETLIST && immediate(instr, 1) == -1);
    }

    // resolves a value through the inherited serializer slot mapping
    std::optional<uint16_t> inheritedSlot(BcOp op) const
    {
        std::vector<BcOp> pending{op};
        DenseHashSet<BcOp, BcOpHash> visited;

        while (!pending.empty())
        {
            BcOp current = pending.back();
            pending.pop_back();

            // phi operands can form cycles
            // visiting each value once bounds both stack use and runtime
            if (visited.contains(current))
                continue;
            visited.insert(current);

            if (current.kind == BcOpKind::VmReg)
                return current.index < inheritedFrame ? std::optional<uint16_t>(uint16_t(current.index)) : std::nullopt;

            if (current.kind == BcOpKind::Proj)
            {
                // projections are encoded as offsets from their producer's base slot
                const BcProj& projection = function.projOp(current);
                if (projection.op.kind != BcOpKind::Inst)
                    return std::nullopt;

                auto it = function.regs.find(projection.op);
                if (it == function.regs.end() || it->second + projection.index >= inheritedFrame)
                    return std::nullopt;

                return uint16_t(it->second + projection.index);
            }

            if (current.kind != BcOpKind::Inst && current.kind != BcOpKind::Phi)
                continue;

            auto it = function.regs.find(current);
            if (it != function.regs.end())
                return it->second < inheritedFrame ? std::optional<uint16_t>(it->second) : std::nullopt;

            if (current.kind != BcOpKind::Phi)
                return std::nullopt;

            // reverse insertion preserves the recursive implementation's first-resolvable-operand order
            const BcOps& operands = function.phiOp(current).ops;
            for (size_t operandIndex = operands.size(); operandIndex > 0; --operandIndex)
                pending.push_back(operands[operandIndex - 1]);
        }

        return std::nullopt;
    }

    void reserve(uint32_t slot)
    {
        if (slot < inheritedFrame)
        {
            reservedSlots[slot] = true;
            minimumFrame = std::max<uint16_t>(minimumFrame, uint16_t(slot + 1));
        }
    }

    void reserve(BcOp value)
    {
        if (std::optional<uint16_t> slot = inheritedSlot(value))
            reserve(*slot);
    }

    void makeFixed(BcOp value)
    {
        if (value.kind == BcOpKind::Inst)
            fixed.insert(value);
        else if (value.kind == BcOpKind::Proj)
        {
            BcOp producer = function.projOp(value).op;
            if (producer.kind == BcOpKind::Inst)
                fixed.insert(producer);
        }
    }

    // preserves every explicit and implicit slot used by a layout-sensitive instruction
    void reserveLayout(BcOp instructionOp, const BcInst& instruction)
    {
        // layout-sensitive instructions keep their results, projections, and operands in place
        makeFixed(instructionOp);
        reserve(instructionOp);
        if (const std::vector<BcOp>* instructionProjections = projections.find(instructionOp))
            for (BcOp projection : *instructionProjections)
                reserve(projection);
        for (BcOp operand : instruction.ops)
        {
            makeFixed(operand);
            reserve(operand);
        }

        if (instruction.op == LOP_FORNPREP)
        {
            // numeric for uses four consecutive slots starting at the instruction result
            if (std::optional<uint16_t> base = inheritedSlot(instructionOp))
                for (uint32_t slot = *base; slot < std::min<uint32_t>(*base + 4, inheritedFrame); ++slot)
                    reserve(slot);
        }

        if (instruction.op == LOP_FORGLOOP && !instruction.ops.empty())
        {
            // the generic for fast paths store a key/value pair to A+3/A+4 even for one variable, beyond the projections the graph has
            if (std::optional<uint16_t> base = inheritedSlot(instruction.ops[0]))
                for (uint32_t slot = *base; slot < std::min<uint32_t>(*base + 5, inheritedFrame); ++slot)
                    reserve(slot);
        }

        bool openProducer = isProducer(instruction);
        bool openConsumer = isConsumer(instruction);
        if (openProducer || openConsumer)
        {
            // open multret sequences can consume every slot from their base to the frame end
            std::optional<uint16_t> base = inheritedSlot(instructionOp);
            if (!base)
            {
                for (BcOp operand : instruction.ops)
                {
                    base = inheritedSlot(operand);
                    if (base)
                        break;
                }
            }

            if (base)
            {
                for (uint32_t slot = *base; slot < inheritedFrame; ++slot)
                {
                    reserve(slot);
                }
            }
        }
    }

    bool indexProjections()
    {
        for (uint32_t projectionIndex = 0; projectionIndex < function.projections.size(); ++projectionIndex)
        {
            // producers with projections remain fixed because projection slots are implicit
            const BcProj& projection = function.projections[projectionIndex];
            if (projection.op.kind != BcOpKind::Inst)
                return false;

            BcOp projectionOp{BcOpKind::Proj, projectionIndex};
            projections[projection.op].push_back(projectionOp);
            fixed.insert(projection.op);
        }

        return true;
    }

    bool locateInstructions()
    {
        for (uint32_t blockIndex = 0; blockIndex < function.blocks.size(); ++blockIndex)
        {
            const BcBlock& block = function.blocks[blockIndex];
            if (block.flags & BcBlockFlag::Dead)
                continue;

            for (BcOp phi : block.phis)
            {
                if (phi.kind != BcOpKind::Phi)
                    return false;

                std::optional<uint16_t> phiSlot = inheritedSlot(phi);
                if (!phiSlot)
                    return false;

                reserve(*phiSlot);
                for (BcOp operand : function.phiOp(phi).ops)
                {
                    makeFixed(operand);
                    std::optional<uint16_t> operandSlot = inheritedSlot(operand);
                    if (!operandSlot)
                        return false;

                    reserve(*operandSlot);
                }
            }

            uint32_t position = 0;
            for (BcOp instructionOp : block.ops)
            {
                if (instructionOp.kind != BcOpKind::Inst)
                    return false;

                reachable.insert(instructionOp);
                locations[instructionOp] = {blockIndex, position++};

                // positions are doubled later so definitions sort after uses at one instruction
                if (isLayoutBarrier(function.instOp(instructionOp).op))
                    barriers[blockIndex].push_back(locations[instructionOp].position * 2);
            }
        }

        return true;
    }

    bool collectUsesAndLayoutConstraints()
    {
        for (uint32_t blockIndex = 0; blockIndex < function.blocks.size(); ++blockIndex)
        {
            const BcBlock& block = function.blocks[blockIndex];
            if (block.flags & BcBlockFlag::Dead)
                continue;

            bool openSequence = false;
            for (BcOp instructionOp : block.ops)
            {
                const BcInst& instruction = function.instOp(instructionOp);
                const Location* location = locations.find(instructionOp);
                LUAU_ASSERT(location);

                bool barrier = isLayoutBarrier(instruction.op);
                if (barrier)
                    reserveLayout(instructionOp, instruction);

                if (openSequence)
                {
                    // values produced inside an open multret sequence retain inherited layout
                    makeFixed(instructionOp);
                    reserve(instructionOp);
                }

                for (BcOp operand : instruction.ops)
                {
                    if (operand.kind == BcOpKind::Inst)
                    {
                        uses[operand].push_back({blockIndex, location->position, barrier || openSequence});
                    }
                    else if (operand.kind == BcOpKind::Proj)
                    {
                        BcOp producer = function.projOp(operand).op;

                        if (producer.kind == BcOpKind::Inst)
                            uses[producer].push_back({blockIndex, location->position, true});
                    }
                    else if (operand.kind == BcOpKind::VmReg)
                    {
                        if (operand.index >= inheritedFrame)
                            return false;

                        identitySlots[operand.index] = true;
                        reserve(operand.index);
                    }
                }

                if (instruction.op == LOP_CAPTURE && immediate(instruction, 0) == LCT_REF && instruction.ops.size() > 1)
                {
                    std::optional<uint16_t> captured = inheritedSlot(instruction.ops[1]);
                    if (!captured)
                        return false;

                    identitySlots[*captured] = true;
                    reserve(*captured);
                }

                bool consumer = isConsumer(instruction);
                bool producer = isProducer(instruction);

                if (consumer)
                    openSequence = false;

                if (producer)
                    openSequence = true;
            }
        }

        return true;
    }

    void buildIntervals()
    {
        for (uint32_t instructionIndex = 0; instructionIndex < function.instructions.size(); ++instructionIndex)
        {
            BcOp instructionOp{BcOpKind::Inst, instructionIndex};
            if (!reachable.contains(instructionOp))
                continue;

            std::optional<uint16_t> slot = inheritedSlot(instructionOp);
            if (!slot)
                continue;

            if (identitySlots[*slot])
                fixed.insert(instructionOp);

            const Location* location = locations.find(instructionOp);
            LUAU_ASSERT(location);
            uint32_t block = location->block;

            // uses occupy even positions and definitions occupy the following odd positions
            uint32_t start = location->position * 2 + 1;
            uint32_t finish = start;
            if (const std::vector<Use>* instructionUses = uses.find(instructionOp))
            {
                for (const Use& use : *instructionUses)
                {
                    uint32_t usePosition = use.position * 2;
                    // only complete, forward, barrier-free single-block lifetimes can move
                    if (use.block != block || usePosition < start || use.barrier)
                        fixed.insert(instructionOp);

                    finish = std::max(finish, usePosition);
                }
            }
            const std::vector<uint32_t>& blockBarriers = barriers[block];
            auto nextBarrier = std::upper_bound(blockBarriers.begin(), blockBarriers.end(), start);
            if (nextBarrier != blockBarriers.end() && *nextBarrier < finish)
                fixed.insert(instructionOp);

            if (fixed.contains(instructionOp))
                reserve(*slot);
            else
                intervals.push_back({instructionOp, block, start, finish, *slot});
        }

        intervals.erase(
            std::remove_if(
                intervals.begin(),
                intervals.end(),
                [this](const Interval& interval)
                {
                    // leaving these values inherited makes every reserved slot globally safe
                    return reservedSlots[interval.slot];
                }
            ),
            intervals.end()
        );
    }

    bool scan()
    {
        // debug local metadata still names inherited slots and is not rewritten by this pass
        if (inheritedFrame == 0 || !function.locals.empty() || !function.localTypes.empty())
            return false;

        for (uint32_t parameter = 0; parameter < function.numparams; ++parameter)
        {
            identitySlots[parameter] = true;
            reserve(parameter);
        }

        if (!indexProjections() || !locateInstructions() || !collectUsesAndLayoutConstraints())
            return false;

        buildIntervals();
        return true;
    }

    std::optional<uint16_t> allocatedSlot(BcOp op) const
    {
        if (op.kind == BcOpKind::Inst && assignedSlots[op.index] != kUnassignedSlot)
            return assignedSlots[op.index];

        if (op.kind == BcOpKind::Proj)
        {
            const BcProj& projection = function.projOp(op);
            if (std::optional<uint16_t> base = allocatedSlot(projection.op); base && *base + projection.index < kInvalidReg)
                return uint16_t(*base + projection.index);
            return std::nullopt;
        }

        return inheritedSlot(op);
    }

    std::vector<BcOp> findNewlyCoalescedMoves() const
    {
        DenseHashSet<BcOp, BcOpHash> fastCallBlocks;
        for (uint32_t instructionIndex = 0; instructionIndex < function.instructions.size(); ++instructionIndex)
        {
            BcOp instructionOp{BcOpKind::Inst, instructionIndex};
            const BcInst& instruction = function.instructions[instructionIndex];
            if (reachable.contains(instructionOp) && isFastCall(instruction.op))
                fastCallBlocks.insert(instruction.block);
        }

        std::vector<BcOp> result;
        for (uint32_t instructionIndex = 0; instructionIndex < function.instructions.size(); ++instructionIndex)
        {
            BcOp instructionOp{BcOpKind::Inst, instructionIndex};
            const BcInst& instruction = function.instructions[instructionIndex];
            // fastcall skips encode bytecode distances, so moves in those blocks remain until offset patching is supported
            if (!reachable.contains(instructionOp) || fastCallBlocks.contains(instruction.block) || instruction.op != LOP_MOVE ||
                instruction.ops.empty())
                continue;

            std::optional<uint16_t> inheritedOutput = inheritedSlot(instructionOp);
            std::optional<uint16_t> inheritedInput = inheritedSlot(instruction.ops[0]);
            std::optional<uint16_t> allocatedOutput = allocatedSlot(instructionOp);
            std::optional<uint16_t> allocatedInput = allocatedSlot(instruction.ops[0]);
            if (inheritedOutput && inheritedInput && *inheritedOutput != *inheritedInput && allocatedOutput && allocatedInput &&
                *allocatedOutput == *allocatedInput)
                result.push_back(instructionOp);
        }
        return result;
    }

    void removeMoves(const std::vector<BcOp>& moves)
    {
        for (BcOp moveOp : moves)
        {
            BcInst& move = function.instOp(moveOp);
            LUAU_ASSERT(move.op == LOP_MOVE && move.ops.size() == 1);
            BcOp source = move.ops[0];

            for (BcOp userOp : move.uses)
            {
                LUAU_ASSERT(userOp.kind == BcOpKind::Inst || userOp.kind == BcOpKind::Phi);
                BcOps& operands = userOp.kind == BcOpKind::Inst ? function.instOp(userOp).ops : function.phiOp(userOp).ops;
                for (BcOp& operand : operands)
                {
                    if (operand == moveOp)
                    {
                        operand = source;
                        function.recordUse(source, userOp);
                    }
                }
            }

            function.eraseUse(moveOp, source);
            move.uses.clear();
            function.eraseOp(moveOp);
            function.regs.erase(moveOp);
        }
    }

    std::optional<uint16_t> adjacentMoveDestination(const Interval& interval) const
    {
        const BcInst& producer = function.instOp(interval.value);
        if (producer.uses.size() != 1 || producer.uses[0].kind != BcOpKind::Inst)
            return std::nullopt;

        BcOp moveOp = producer.uses[0];
        const BcInst& move = function.instOp(moveOp);
        const Location* moveLocation = locations.find(moveOp);
        if (move.op != LOP_MOVE || !moveLocation || moveLocation->block != interval.block || moveLocation->position * 2 != interval.start + 1)
            return std::nullopt;

        return inheritedSlot(moveOp);
    }

    SlotAllocationResult assign()
    {
        // left-edge order lets each interval take the lowest available unreserved slot
        // grouping by block allows a single finish position per slot to replace rescanning every previously assigned interval
        std::stable_sort(
            intervals.begin(),
            intervals.end(),
            [](const Interval& lhs, const Interval& rhs)
            {
                return lhs.block < rhs.block || (lhs.block == rhs.block && lhs.start < rhs.start);
            }
        );

        std::array<uint32_t, 256> occupiedUntil{};
        uint32_t currentBlock = ~0u;
        for (Interval& interval : intervals)
        {
            if (interval.block != currentBlock)
            {
                occupiedUntil.fill(0);
                currentBlock = interval.block;
            }

            auto available = [this, &occupiedUntil, &interval](uint16_t candidate)
            {
                return candidate < kInvalidReg && !reservedSlots[candidate] && occupiedUntil[candidate] < interval.start;
            };

            bool found = false;
            const BcInst& instruction = function.instOp(interval.value);
            std::optional<uint16_t> preferred;
            bool allowReservedPreferred = false;
            if (instruction.op == LOP_MOVE && !instruction.ops.empty())
                preferred = allocatedSlot(instruction.ops[0]);
            else
            {
                preferred = adjacentMoveDestination(interval);
                allowReservedPreferred = preferred.has_value();
            }

            if (preferred && *preferred < kInvalidReg && !identitySlots[*preferred] && occupiedUntil[*preferred] < interval.start &&
                (allowReservedPreferred || !reservedSlots[*preferred]))
            {
                interval.slot = *preferred;
                found = true;
            }

            for (uint16_t candidate = 0; !found && candidate < kInvalidReg; ++candidate)
            {
                if (!available(candidate))
                    continue;

                interval.slot = candidate;
                found = true;
            }

            if (!found)
                return {0, inheritedFrame, inheritedFrame, false};

            occupiedUntil[interval.slot] = interval.finish;
            assignedSlots[interval.value.index] = interval.slot;
        }

        uint16_t peak = minimumFrame;
        for (const Interval& interval : intervals)
            peak = std::max<uint16_t>(peak, uint16_t(interval.slot + 1));

        std::vector<BcOp> movesToRemove = findNewlyCoalescedMoves();
        // move reduction is the primary objective; frame size breaks ties when the move count is unchanged
        if (movesToRemove.empty() && peak >= inheritedFrame)
            return {0, inheritedFrame, inheritedFrame, false};

        for (const Interval& interval : intervals)
            function.regs[interval.value] = Reg(interval.slot);

        function.maxstacksize = uint8_t(peak);
        removeMoves(movesToRemove);
        return {uint32_t(movesToRemove.size()), inheritedFrame, uint8_t(peak), true};
    }

    // counts the moves left after any removal ran; used to gauge how much coalescing opportunity this pass declines
    void countResidualMoves(SlotAllocationResult& result) const
    {
        std::vector<bool> loopBody(function.blocks.size(), false);
        for (uint32_t blockIndex = 0; blockIndex < function.blocks.size(); ++blockIndex)
        {
            const BcBlock& block = function.blocks[blockIndex];
            if (block.flags & BcBlockFlag::Dead)
                continue;

            for (const BcBlockEdge& edge : block.successors)
            {
                if (edge.kind != BcBlockEdgeKind::Loop || edge.target.kind != BcOpKind::Block)
                    continue;

                // natural loop of a back edge: the header plus every block that can reach this latch without passing the header
                uint32_t header = edge.target.index;
                loopBody[header] = true;

                std::vector<uint32_t> pending{blockIndex};
                std::vector<bool> visited(function.blocks.size(), false);
                visited[header] = true;
                while (!pending.empty())
                {
                    uint32_t current = pending.back();
                    pending.pop_back();
                    if (visited[current] || current >= function.blocks.size())
                        continue;
                    visited[current] = true;
                    loopBody[current] = true;
                    for (const BcBlockEdge& pred : function.blocks[current].predecessors)
                        if (pred.target.kind == BcOpKind::Block)
                            pending.push_back(pred.target.index);
                }
            }
        }

        for (uint32_t blockIndex = 0; blockIndex < function.blocks.size(); ++blockIndex)
        {
            const BcBlock& block = function.blocks[blockIndex];
            if (block.flags & BcBlockFlag::Dead)
                continue;

            for (BcOp instructionOp : block.ops)
            {
                if (function.instOp(instructionOp).op != LOP_MOVE)
                    continue;

                result.movesRemaining++;
                if (loopBody[blockIndex])
                    result.loopMovesRemaining++;
            }
        }
    }

    SlotAllocationResult run()
    {
        // slot allocation is a two step process:
        // - gather layout constraints and build conservative single-block intervals
        // - assign movable intervals and publish a layout that shrinks the frame or removes moves
        if (!scan())
            return {0, inheritedFrame, inheritedFrame, false};

        SlotAllocationResult result = assign();
        countResidualMoves(result);
        return result;
    }
};

template<typename VmConst>
SlotAllocationResult allocateBytecodeSlots(BcFunction<VmConst>& function)
{
    // prefer fewer moves, even at the cost of a larger frame; use frame size to break ties
    // if neither metric improves, reuse the inherited layout
    SlotAllocator<VmConst> allocator(function);
    return allocator.run();
}

} // namespace Bytecode
} // namespace Luau
