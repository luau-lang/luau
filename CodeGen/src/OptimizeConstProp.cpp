// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/OptimizeConstProp.h"

#include "Luau/DenseHash.h"
#include "Luau/IrBuilder.h"
#include "Luau/IrUtils.h"

#include "lua.h"

namespace Luau
{
namespace CodeGen
{

// Data we know about the register value
struct RegisterInfo
{
    uint8_t tag = 0xff;
    IrOp value;

    // Used to quickly invalidate links between SSA values and register memory
    // It's a bit imprecise where value and tag both always invalidate together
    uint32_t version = 0;

    bool knownNotReadonly = false;
    bool knownNoMetatable = false;
};

// Load instructions are linked to target register to carry knowledge about the target
// We track a register version at the point of the load so it's easy to break the link when register is updated
struct RegisterLink
{
    uint8_t reg = 0;
    uint32_t version = 0;
};

// Data we know about the current VM state
struct ConstPropState
{
    uint8_t tryGetTag(IrOp op)
    {
        if (RegisterInfo* info = tryGetRegisterInfo(op))
            return info->tag;

        return 0xff;
    }

    void saveTag(IrOp op, uint8_t tag)
    {
        if (RegisterInfo* info = tryGetRegisterInfo(op))
            info->tag = tag;
    }

    IrOp tryGetValue(IrOp op)
    {
        if (RegisterInfo* info = tryGetRegisterInfo(op))
            return info->value;

        return IrOp{IrOpKind::None, 0u};
    }

    void saveValue(IrOp op, IrOp value)
    {
        LUAU_ASSERT(value.kind == IrOpKind::Constant);

        if (RegisterInfo* info = tryGetRegisterInfo(op))
            info->value = value;
    }

    void invalidate(RegisterInfo& reg, bool invalidateTag, bool invalidateValue)
    {
        if (invalidateTag)
        {
            reg.tag = 0xff;
        }

        if (invalidateValue)
        {
            reg.value = {};
            reg.knownNotReadonly = false;
            reg.knownNoMetatable = false;
        }

        reg.version++;
    }

    void invalidateTag(IrOp regOp)
    {
        LUAU_ASSERT(regOp.kind == IrOpKind::VmReg);
        invalidate(regs[regOp.index], /* invalidateTag */ true, /* invalidateValue */ false);
    }

    void invalidateValue(IrOp regOp)
    {
        LUAU_ASSERT(regOp.kind == IrOpKind::VmReg);
        invalidate(regs[regOp.index], /* invalidateTag */ false, /* invalidateValue */ true);
    }

    void invalidate(IrOp regOp)
    {
        LUAU_ASSERT(regOp.kind == IrOpKind::VmReg);
        invalidate(regs[regOp.index], /* invalidateTag */ true, /* invalidateValue */ true);
    }

    void invalidateRegistersFrom(uint32_t firstReg)
    {
        for (int i = int(firstReg); i <= maxReg; ++i)
            invalidate(regs[i], /* invalidateTag */ true, /* invalidateValue */ true);

        maxReg = int(firstReg) - 1;
    }

    void invalidateHeap()
    {
        for (int i = 0; i <= maxReg; ++i)
            invalidateHeap(regs[i]);
    }

    void invalidateHeap(RegisterInfo& reg)
    {
        reg.knownNotReadonly = false;
        reg.knownNoMetatable = false;
    }

    void invalidateAll()
    {
        // Invalidating registers also invalidates what we know about the heap (stored in RegisterInfo)
        invalidateRegistersFrom(0u);
        inSafeEnv = false;
    }

    void createRegLink(uint32_t instIdx, IrOp regOp)
    {
        LUAU_ASSERT(regOp.kind == IrOpKind::VmReg);
        LUAU_ASSERT(!instLink.contains(instIdx));
        instLink[instIdx] = RegisterLink{uint8_t(regOp.index), regs[regOp.index].version};
    }

    RegisterInfo* tryGetRegisterInfo(IrOp op)
    {
        if (op.kind == IrOpKind::VmReg)
        {
            maxReg = int(op.index) > maxReg ? int(op.index) : maxReg;
            return &regs[op.index];
        }

        if (RegisterLink* link = tryGetRegLink(op))
        {
            maxReg = int(link->reg) > maxReg ? int(link->reg) : maxReg;
            return &regs[link->reg];
        }

        return nullptr;
    }

    RegisterLink* tryGetRegLink(IrOp instOp)
    {
        if (instOp.kind != IrOpKind::Inst)
            return nullptr;

        if (RegisterLink* link = instLink.find(instOp.index))
        {
            // Check that the target register hasn't changed the value
            if (link->version > regs[link->reg].version)
                return nullptr;

            return link;
        }

        return nullptr;
    }

    RegisterInfo regs[256];

    // For range/full invalidations, we only want to visit a limited number of data that we have recorded
    int maxReg = 0;

    bool inSafeEnv = false;
    bool checkedGc = false;

    DenseHashMap<uint32_t, RegisterLink> instLink{~0u};
};

static void constPropInInst(ConstPropState& state, IrBuilder& build, IrFunction& function, IrBlock& block, IrInst& inst, uint32_t index)
{
    switch (inst.cmd)
    {
    case IrCmd::LOAD_TAG:
        if (uint8_t tag = state.tryGetTag(inst.a); tag != 0xff)
            substitute(function, inst, build.constTag(tag));
        else if (inst.a.kind == IrOpKind::VmReg)
            state.createRegLink(index, inst.a);
        break;
    case IrCmd::LOAD_POINTER:
        if (inst.a.kind == IrOpKind::VmReg)
            state.createRegLink(index, inst.a);
        break;
    case IrCmd::LOAD_DOUBLE:
        if (IrOp value = state.tryGetValue(inst.a); value.kind == IrOpKind::Constant)
            substitute(function, inst, value);
        else if (inst.a.kind == IrOpKind::VmReg)
            state.createRegLink(index, inst.a);
        break;
    case IrCmd::LOAD_INT:
        if (IrOp value = state.tryGetValue(inst.a); value.kind == IrOpKind::Constant)
            substitute(function, inst, value);
        else if (inst.a.kind == IrOpKind::VmReg)
            state.createRegLink(index, inst.a);
        break;
    case IrCmd::LOAD_TVALUE:
        if (inst.a.kind == IrOpKind::VmReg)
            state.createRegLink(index, inst.a);
        break;
    case IrCmd::STORE_TAG:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            if (inst.b.kind == IrOpKind::Constant)
            {
                uint8_t value = function.tagOp(inst.b);

                if (state.tryGetTag(inst.a) == value)
                    kill(function, inst);
                else
                    state.saveTag(inst.a, value);
            }
            else
            {
                state.invalidateTag(inst.a);
            }
        }
        break;
    case IrCmd::STORE_POINTER:
        if (inst.a.kind == IrOpKind::VmReg)
            state.invalidateValue(inst.a);
        break;
    case IrCmd::STORE_DOUBLE:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            if (inst.b.kind == IrOpKind::Constant)
            {
                std::optional<double> oldValue = function.asDoubleOp(state.tryGetValue(inst.a));
                double newValue = function.doubleOp(inst.b);

                if (oldValue && *oldValue == newValue)
                    kill(function, inst);
                else
                    state.saveValue(inst.a, inst.b);
            }
            else
            {
                state.invalidateValue(inst.a);
            }
        }
        break;
    case IrCmd::STORE_INT:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            if (inst.b.kind == IrOpKind::Constant)
            {
                std::optional<int> oldValue = function.asIntOp(state.tryGetValue(inst.a));
                int newValue = function.intOp(inst.b);

                if (oldValue && *oldValue == newValue)
                    kill(function, inst);
                else
                    state.saveValue(inst.a, inst.b);
            }
            else
            {
                state.invalidateValue(inst.a);
            }
        }
        break;
    case IrCmd::STORE_TVALUE:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            state.invalidate(inst.a);

            if (uint8_t tag = state.tryGetTag(inst.b); tag != 0xff)
                state.saveTag(inst.a, tag);

            if (IrOp value = state.tryGetValue(inst.b); value.kind != IrOpKind::None)
                state.saveValue(inst.a, value);
        }
        break;
    case IrCmd::JUMP_IF_TRUTHY:
        if (uint8_t tag = state.tryGetTag(inst.a); tag != 0xff)
        {
            if (tag == LUA_TNIL)
                replace(function, block, index, {IrCmd::JUMP, inst.c});
            else if (tag != LUA_TBOOLEAN)
                replace(function, block, index, {IrCmd::JUMP, inst.b});
        }
        break;
    case IrCmd::JUMP_IF_FALSY:
        if (uint8_t tag = state.tryGetTag(inst.a); tag != 0xff)
        {
            if (tag == LUA_TNIL)
                replace(function, block, index, {IrCmd::JUMP, inst.b});
            else if (tag != LUA_TBOOLEAN)
                replace(function, block, index, {IrCmd::JUMP, inst.c});
        }
        break;
    case IrCmd::JUMP_EQ_TAG:
    {
        uint8_t tagA = inst.a.kind == IrOpKind::Constant ? function.tagOp(inst.a) : state.tryGetTag(inst.a);
        uint8_t tagB = inst.b.kind == IrOpKind::Constant ? function.tagOp(inst.b) : state.tryGetTag(inst.b);

        if (tagA != 0xff && tagB != 0xff)
        {
            if (tagA == tagB)
                replace(function, block, index, {IrCmd::JUMP, inst.c});
            else
                replace(function, block, index, {IrCmd::JUMP, inst.d});
        }
        break;
    }
    case IrCmd::JUMP_EQ_INT:
    {
        std::optional<int> valueA = function.asIntOp(inst.a.kind == IrOpKind::Constant ? inst.a : state.tryGetValue(inst.a));
        std::optional<int> valueB = function.asIntOp(inst.b.kind == IrOpKind::Constant ? inst.b : state.tryGetValue(inst.b));

        if (valueA && valueB)
        {
            if (*valueA == *valueB)
                replace(function, block, index, {IrCmd::JUMP, inst.c});
            else
                replace(function, block, index, {IrCmd::JUMP, inst.d});
        }
        break;
    }
    case IrCmd::JUMP_CMP_NUM:
    {
        std::optional<double> valueA = function.asDoubleOp(inst.a.kind == IrOpKind::Constant ? inst.a : state.tryGetValue(inst.a));
        std::optional<double> valueB = function.asDoubleOp(inst.b.kind == IrOpKind::Constant ? inst.b : state.tryGetValue(inst.b));

        if (valueA && valueB)
        {
            if (compare(*valueA, *valueB, function.conditionOp(inst.c)))
                replace(function, block, index, {IrCmd::JUMP, inst.d});
            else
                replace(function, block, index, {IrCmd::JUMP, inst.e});
        }
        break;
    }
    case IrCmd::GET_UPVALUE:
        state.invalidate(inst.a);
        break;
    case IrCmd::CHECK_TAG:
    {
        uint8_t b = function.tagOp(inst.b);

        if (uint8_t tag = state.tryGetTag(inst.a); tag != 0xff)
        {
            if (tag == b)
                kill(function, inst);
            else
                replace(function, block, index, {IrCmd::JUMP, inst.c}); // Shows a conflict in assumptions on this path
        }
        else
        {
            state.saveTag(inst.a, b); // We can assume the tag value going forward
        }
        break;
    }
    case IrCmd::CHECK_READONLY:
        if (RegisterInfo* info = state.tryGetRegisterInfo(inst.a))
        {
            if (info->knownNotReadonly)
                kill(function, inst);
            else
                info->knownNotReadonly = true;
        }
        break;
    case IrCmd::CHECK_NO_METATABLE:
        if (RegisterInfo* info = state.tryGetRegisterInfo(inst.a))
        {
            if (info->knownNoMetatable)
                kill(function, inst);
            else
                info->knownNoMetatable = true;
        }
        break;
    case IrCmd::CHECK_SAFE_ENV:
        if (state.inSafeEnv)
            kill(function, inst);
        else
            state.inSafeEnv = true;
        break;
    case IrCmd::CHECK_GC:
        // It is enough to perform a GC check once in a block
        if (state.checkedGc)
            kill(function, inst);
        else
            state.checkedGc = true;
        break;
    case IrCmd::BARRIER_OBJ:
    case IrCmd::BARRIER_TABLE_FORWARD:
        if (inst.b.kind == IrOpKind::VmReg)
        {
            if (uint8_t tag = state.tryGetTag(inst.b); tag != 0xff)
            {
                // If the written object is not collectable, barrier is not required
                if (!isGCO(tag))
                    kill(function, inst);
            }
        }
        break;
    case IrCmd::LOP_FASTCALL:
    case IrCmd::LOP_FASTCALL1:
    case IrCmd::LOP_FASTCALL2:
    case IrCmd::LOP_FASTCALL2K:
        // TODO: classify fast call behaviors to avoid heap invalidation
        state.invalidateHeap(); // Even a builtin method can change table properties
        state.invalidateRegistersFrom(inst.b.index);
        break;
    case IrCmd::LOP_AND:
    case IrCmd::LOP_ANDK:
    case IrCmd::LOP_OR:
    case IrCmd::LOP_ORK:
        state.invalidate(inst.b);
        break;

        // These instructions don't have an effect on register/memory state we are tracking
    case IrCmd::NOP:
    case IrCmd::LOAD_NODE_VALUE_TV:
    case IrCmd::LOAD_ENV:
    case IrCmd::GET_ARR_ADDR:
    case IrCmd::GET_SLOT_NODE_ADDR:
    case IrCmd::STORE_NODE_VALUE_TV:
    case IrCmd::ADD_INT:
    case IrCmd::SUB_INT:
    case IrCmd::ADD_NUM:
    case IrCmd::SUB_NUM:
    case IrCmd::MUL_NUM:
    case IrCmd::DIV_NUM:
    case IrCmd::MOD_NUM:
    case IrCmd::POW_NUM:
    case IrCmd::UNM_NUM:
    case IrCmd::NOT_ANY:
    case IrCmd::JUMP:
    case IrCmd::JUMP_EQ_POINTER:
    case IrCmd::TABLE_LEN:
    case IrCmd::NEW_TABLE:
    case IrCmd::DUP_TABLE:
    case IrCmd::NUM_TO_INDEX:
    case IrCmd::INT_TO_NUM:
    case IrCmd::CHECK_ARRAY_SIZE:
    case IrCmd::CHECK_SLOT_MATCH:
    case IrCmd::BARRIER_TABLE_BACK:
    case IrCmd::LOP_RETURN:
    case IrCmd::LOP_COVERAGE:
    case IrCmd::SET_UPVALUE:
    case IrCmd::LOP_SETLIST:  // We don't track table state that this can invalidate
    case IrCmd::SET_SAVEDPC:  // TODO: we may be able to remove some updates to PC
    case IrCmd::CLOSE_UPVALS: // Doesn't change memory that we track
    case IrCmd::CAPTURE:
    case IrCmd::SUBSTITUTE:
    case IrCmd::ADJUST_STACK_TO_REG: // Changes stack top, but not the values
    case IrCmd::ADJUST_STACK_TO_TOP: // Changes stack top, but not the values
        break;

        // We don't model the following instructions, so we just clear all the knowledge we have built up
        // Many of these call user functions that can change memory and captured registers
        // Some of these might yield with similar effects
    case IrCmd::JUMP_CMP_ANY:
    case IrCmd::DO_ARITH:
    case IrCmd::DO_LEN:
    case IrCmd::GET_TABLE:
    case IrCmd::SET_TABLE:
    case IrCmd::GET_IMPORT:
    case IrCmd::CONCAT:
    case IrCmd::PREPARE_FORN:
    case IrCmd::INTERRUPT: // TODO: it will be important to keep tag/value state, but we have to track register capture
    case IrCmd::LOP_NAMECALL:
    case IrCmd::LOP_CALL:
    case IrCmd::LOP_FORGLOOP:
    case IrCmd::LOP_FORGLOOP_FALLBACK:
    case IrCmd::LOP_FORGPREP_XNEXT_FALLBACK:
    case IrCmd::FALLBACK_GETGLOBAL:
    case IrCmd::FALLBACK_SETGLOBAL:
    case IrCmd::FALLBACK_GETTABLEKS:
    case IrCmd::FALLBACK_SETTABLEKS:
    case IrCmd::FALLBACK_NAMECALL:
    case IrCmd::FALLBACK_PREPVARARGS:
    case IrCmd::FALLBACK_GETVARARGS:
    case IrCmd::FALLBACK_NEWCLOSURE:
    case IrCmd::FALLBACK_DUPCLOSURE:
    case IrCmd::FALLBACK_FORGPREP:
        // TODO: this is very conservative, some of there instructions can be tracked better
        // TODO: non-captured register tags and values should not be cleared here
        state.invalidateAll();
        break;
    }
}

static void constPropInBlock(IrBuilder& build, IrBlock& block, ConstPropState& state)
{
    IrFunction& function = build.function;

    for (uint32_t index = block.start; index <= block.finish; index++)
    {
        LUAU_ASSERT(index < function.instructions.size());
        IrInst& inst = function.instructions[index];

        applySubstitutions(function, inst);

        foldConstants(build, function, block, index);

        constPropInInst(state, build, function, block, inst, index);
    }
}

static void constPropInBlockChain(IrBuilder& build, std::vector<uint8_t>& visited, IrBlock* block)
{
    IrFunction& function = build.function;

    ConstPropState state;

    while (block)
    {
        uint32_t blockIdx = function.getBlockIndex(*block);
        LUAU_ASSERT(!visited[blockIdx]);
        visited[blockIdx] = true;

        constPropInBlock(build, *block, state);

        IrInst& termInst = function.instructions[block->finish];

        IrBlock* nextBlock = nullptr;

        // Unconditional jump into a block with a single user (current block) allows us to continue optimization
        // with the information we have gathered so far (unless we have already visited that block earlier)
        if (termInst.cmd == IrCmd::JUMP)
        {
            IrBlock& target = function.blockOp(termInst.a);

            if (target.useCount == 1 && !visited[function.getBlockIndex(target)] && target.kind != IrBlockKind::Fallback)
                nextBlock = &target;
        }

        block = nextBlock;
    }
}

void constPropInBlockChains(IrBuilder& build)
{
    IrFunction& function = build.function;

    std::vector<uint8_t> visited(function.blocks.size(), false);

    for (IrBlock& block : function.blocks)
    {
        if (block.kind == IrBlockKind::Fallback || block.kind == IrBlockKind::Dead)
            continue;

        if (visited[function.getBlockIndex(block)])
            continue;

        constPropInBlockChain(build, visited, &block);
    }
}

} // namespace CodeGen
} // namespace Luau
