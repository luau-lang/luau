// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/OptimizeConstProp.h"

#include "Luau/DenseHash.h"
#include "Luau/IrAnalysis.h"
#include "Luau/IrBuilder.h"
#include "Luau/IrUtils.h"

#include "lua.h"

#include <array>
#include <vector>

LUAU_FASTINTVARIABLE(LuauCodeGenMinLinearBlockPath, 3)

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
    ConstPropState(IrFunction& function)
        : function(function)
        , valueMap({})
    {
    }

    uint8_t tryGetTag(IrOp op)
    {
        if (RegisterInfo* info = tryGetRegisterInfo(op))
            return info->tag;

        return 0xff;
    }

    void saveTag(IrOp op, uint8_t tag)
    {
        if (RegisterInfo* info = tryGetRegisterInfo(op))
        {
            if (info->tag != tag)
            {
                info->tag = tag;
                info->version++;
            }
        }
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
        {
            if (info->value != value)
            {
                info->value = value;
                info->knownNotReadonly = false;
                info->knownNoMetatable = false;
                info->version++;
            }
        }
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
        // TODO: use maxstacksize from Proto
        maxReg = vmRegOp(regOp) > maxReg ? vmRegOp(regOp) : maxReg;
        invalidate(regs[vmRegOp(regOp)], /* invalidateTag */ true, /* invalidateValue */ false);
    }

    void invalidateValue(IrOp regOp)
    {
        // TODO: use maxstacksize from Proto
        maxReg = vmRegOp(regOp) > maxReg ? vmRegOp(regOp) : maxReg;
        invalidate(regs[vmRegOp(regOp)], /* invalidateTag */ false, /* invalidateValue */ true);
    }

    void invalidate(IrOp regOp)
    {
        // TODO: use maxstacksize from Proto
        maxReg = vmRegOp(regOp) > maxReg ? vmRegOp(regOp) : maxReg;
        invalidate(regs[vmRegOp(regOp)], /* invalidateTag */ true, /* invalidateValue */ true);
    }

    void invalidateRegistersFrom(int firstReg)
    {
        for (int i = firstReg; i <= maxReg; ++i)
            invalidate(regs[i], /* invalidateTag */ true, /* invalidateValue */ true);
    }

    void invalidateRegisterRange(int firstReg, int count)
    {
        for (int i = firstReg; i < firstReg + count && i <= maxReg; ++i)
            invalidate(regs[i], /* invalidateTag */ true, /* invalidateValue */ true);
    }

    void invalidateCapturedRegisters()
    {
        for (int i = 0; i <= maxReg; ++i)
        {
            if (function.cfg.captured.regs.test(i))
                invalidate(regs[i], /* invalidateTag */ true, /* invalidateValue */ true);
        }
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

    void invalidateUserCall()
    {
        invalidateHeap();
        invalidateCapturedRegisters();
        inSafeEnv = false;
    }

    void createRegLink(uint32_t instIdx, IrOp regOp)
    {
        LUAU_ASSERT(!instLink.contains(instIdx));
        instLink[instIdx] = RegisterLink{uint8_t(vmRegOp(regOp)), regs[vmRegOp(regOp)].version};
    }

    RegisterInfo* tryGetRegisterInfo(IrOp op)
    {
        if (op.kind == IrOpKind::VmReg)
        {
            maxReg = vmRegOp(op) > maxReg ? vmRegOp(op) : maxReg;
            return &regs[vmRegOp(op)];
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

    // Attach register version number to the register operand in a load instruction
    // This is used to allow instructions with register references to be compared for equality
    IrInst versionedVmRegLoad(IrCmd loadCmd, IrOp op)
    {
        LUAU_ASSERT(op.kind == IrOpKind::VmReg);
        uint32_t version = regs[vmRegOp(op)].version;
        LUAU_ASSERT(version <= 0xffffff);
        op.index = vmRegOp(op) | (version << 8);
        return IrInst{loadCmd, op};
    }

    // Find existing value of the instruction that is exactly the same, or record current on for future lookups
    void substituteOrRecord(IrInst& inst, uint32_t instIdx)
    {
        if (!useValueNumbering)
            return;

        if (uint32_t* prevIdx = valueMap.find(inst))
            substitute(function, inst, IrOp{IrOpKind::Inst, *prevIdx});
        else
            valueMap[inst] = instIdx;
    }

    // Vm register load can be replaced by a previous load of the same version of the register
    // If there is no previous load, we record the current one for future lookups
    void substituteOrRecordVmRegLoad(IrInst& loadInst)
    {
        LUAU_ASSERT(loadInst.a.kind == IrOpKind::VmReg);

        if (!useValueNumbering)
            return;

        // To avoid captured register invalidation tracking in lowering later, values from loads from captured registers are not propagated
        // This prevents the case where load value location is linked to memory in case of a spill and is then cloberred in a user call
        if (function.cfg.captured.regs.test(vmRegOp(loadInst.a)))
            return;

        IrInst versionedLoad = versionedVmRegLoad(loadInst.cmd, loadInst.a);

        // Check if there is a value that already has this version of the register
        if (uint32_t* prevIdx = valueMap.find(versionedLoad))
        {
            // Previous value might not be linked to a register yet
            // For example, it could be a NEW_TABLE stored into a register and we might need to track guards made with this value
            if (!instLink.contains(*prevIdx))
                createRegLink(*prevIdx, loadInst.a);

            // Substitute load instructon with the previous value
            substitute(function, loadInst, IrOp{IrOpKind::Inst, *prevIdx});
        }
        else
        {
            uint32_t instIdx = function.getInstIndex(loadInst);

            // Record load of this register version for future substitution
            valueMap[versionedLoad] = instIdx;

            createRegLink(instIdx, loadInst.a);
        }
    }

    // VM register loads can use the value that was stored in the same Vm register earlier
    void forwardVmRegStoreToLoad(const IrInst& storeInst, IrCmd loadCmd)
    {
        LUAU_ASSERT(storeInst.a.kind == IrOpKind::VmReg);
        LUAU_ASSERT(storeInst.b.kind == IrOpKind::Inst);

        if (!useValueNumbering)
            return;

        // To avoid captured register invalidation tracking in lowering later, values from stores into captured registers are not propagated
        // This prevents the case where store creates an alternative value location in case of a spill and is then cloberred in a user call
        if (function.cfg.captured.regs.test(vmRegOp(storeInst.a)))
            return;

        // Future loads of this register version can use the value we stored
        valueMap[versionedVmRegLoad(loadCmd, storeInst.a)] = storeInst.b.index;
    }

    void clear()
    {
        for (int i = 0; i <= maxReg; ++i)
            regs[i] = RegisterInfo();

        maxReg = 0;

        inSafeEnv = false;
        checkedGc = false;

        instLink.clear();
        valueMap.clear();
    }

    IrFunction& function;

    bool useValueNumbering = false;

    std::array<RegisterInfo, 256> regs;

    // For range/full invalidations, we only want to visit a limited number of data that we have recorded
    int maxReg = 0;

    bool inSafeEnv = false;
    bool checkedGc = false;

    DenseHashMap<uint32_t, RegisterLink> instLink{~0u};

    DenseHashMap<IrInst, uint32_t, IrInstHash, IrInstEq> valueMap;
};

static void handleBuiltinEffects(ConstPropState& state, LuauBuiltinFunction bfid, uint32_t firstReturnReg, int nresults)
{
    // Switch over all values is used to force new items to be handled
    switch (bfid)
    {
    case LBF_NONE:
    case LBF_ASSERT:
    case LBF_MATH_ABS:
    case LBF_MATH_ACOS:
    case LBF_MATH_ASIN:
    case LBF_MATH_ATAN2:
    case LBF_MATH_ATAN:
    case LBF_MATH_CEIL:
    case LBF_MATH_COSH:
    case LBF_MATH_COS:
    case LBF_MATH_DEG:
    case LBF_MATH_EXP:
    case LBF_MATH_FLOOR:
    case LBF_MATH_FMOD:
    case LBF_MATH_FREXP:
    case LBF_MATH_LDEXP:
    case LBF_MATH_LOG10:
    case LBF_MATH_LOG:
    case LBF_MATH_MAX:
    case LBF_MATH_MIN:
    case LBF_MATH_MODF:
    case LBF_MATH_POW:
    case LBF_MATH_RAD:
    case LBF_MATH_SINH:
    case LBF_MATH_SIN:
    case LBF_MATH_SQRT:
    case LBF_MATH_TANH:
    case LBF_MATH_TAN:
    case LBF_BIT32_ARSHIFT:
    case LBF_BIT32_BAND:
    case LBF_BIT32_BNOT:
    case LBF_BIT32_BOR:
    case LBF_BIT32_BXOR:
    case LBF_BIT32_BTEST:
    case LBF_BIT32_EXTRACT:
    case LBF_BIT32_LROTATE:
    case LBF_BIT32_LSHIFT:
    case LBF_BIT32_REPLACE:
    case LBF_BIT32_RROTATE:
    case LBF_BIT32_RSHIFT:
    case LBF_TYPE:
    case LBF_STRING_BYTE:
    case LBF_STRING_CHAR:
    case LBF_STRING_LEN:
    case LBF_TYPEOF:
    case LBF_STRING_SUB:
    case LBF_MATH_CLAMP:
    case LBF_MATH_SIGN:
    case LBF_MATH_ROUND:
    case LBF_RAWSET:
    case LBF_RAWGET:
    case LBF_RAWEQUAL:
    case LBF_TABLE_INSERT:
    case LBF_TABLE_UNPACK:
    case LBF_VECTOR:
    case LBF_BIT32_COUNTLZ:
    case LBF_BIT32_COUNTRZ:
    case LBF_SELECT_VARARG:
    case LBF_RAWLEN:
    case LBF_BIT32_EXTRACTK:
    case LBF_GETMETATABLE:
        break;
    case LBF_SETMETATABLE:
        state.invalidateHeap(); // TODO: only knownNoMetatable is affected and we might know which one
        break;
    }

    // TODO: classify further using switch above, some fastcalls only modify the value, not the tag
    // TODO: fastcalls are different from calls and it might be possible to not invalidate all register starting from return
    state.invalidateRegistersFrom(firstReturnReg);
}

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
            state.substituteOrRecordVmRegLoad(inst);
        break;
    case IrCmd::LOAD_DOUBLE:
        if (IrOp value = state.tryGetValue(inst.a); value.kind == IrOpKind::Constant)
            substitute(function, inst, value);
        else if (inst.a.kind == IrOpKind::VmReg)
            state.substituteOrRecordVmRegLoad(inst);
        break;
    case IrCmd::LOAD_INT:
        if (IrOp value = state.tryGetValue(inst.a); value.kind == IrOpKind::Constant)
            substitute(function, inst, value);
        else if (inst.a.kind == IrOpKind::VmReg)
            state.substituteOrRecordVmRegLoad(inst);
        break;
    case IrCmd::LOAD_TVALUE:
        if (inst.a.kind == IrOpKind::VmReg)
            state.substituteOrRecordVmRegLoad(inst);
        break;
    case IrCmd::STORE_TAG:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            const IrOp source = inst.a;
            uint32_t activeLoadDoubleValue = kInvalidInstIdx;

            if (inst.b.kind == IrOpKind::Constant)
            {
                uint8_t value = function.tagOp(inst.b);

                // STORE_TAG usually follows a store of the value, but it also bumps the version of the whole register
                // To be able to propagate STORE_DOUBLE into LOAD_DOUBLE, we find active LOAD_DOUBLE value and recreate it with updated version
                // Register in this optimization cannot be captured to avoid complications in lowering (IrValueLocationTracking doesn't model it)
                // If stored tag is not a number, we can skip the lookup as there won't be future loads of this register as a number
                if (value == LUA_TNUMBER && !function.cfg.captured.regs.test(vmRegOp(source)))
                {
                    if (uint32_t* prevIdx = state.valueMap.find(state.versionedVmRegLoad(IrCmd::LOAD_DOUBLE, source)))
                        activeLoadDoubleValue = *prevIdx;
                }

                if (state.tryGetTag(source) == value)
                    kill(function, inst);
                else
                    state.saveTag(source, value);
            }
            else
            {
                state.invalidateTag(source);
            }

            // Future LOAD_DOUBLE instructions can re-use previous register version load
            if (activeLoadDoubleValue != kInvalidInstIdx)
                state.valueMap[state.versionedVmRegLoad(IrCmd::LOAD_DOUBLE, source)] = activeLoadDoubleValue;
        }
        break;
    case IrCmd::STORE_POINTER:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            state.invalidateValue(inst.a);
            state.forwardVmRegStoreToLoad(inst, IrCmd::LOAD_POINTER);
        }
        break;
    case IrCmd::STORE_DOUBLE:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            if (inst.b.kind == IrOpKind::Constant)
            {
                if (state.tryGetValue(inst.a) == inst.b)
                    kill(function, inst);
                else
                    state.saveValue(inst.a, inst.b);
            }
            else
            {
                state.invalidateValue(inst.a);
                state.forwardVmRegStoreToLoad(inst, IrCmd::LOAD_DOUBLE);
            }
        }
        break;
    case IrCmd::STORE_INT:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            if (inst.b.kind == IrOpKind::Constant)
            {
                if (state.tryGetValue(inst.a) == inst.b)
                    kill(function, inst);
                else
                    state.saveValue(inst.a, inst.b);
            }
            else
            {
                state.invalidateValue(inst.a);
                state.forwardVmRegStoreToLoad(inst, IrCmd::LOAD_INT);
            }
        }
        break;
    case IrCmd::STORE_VECTOR:
        state.invalidateValue(inst.a);
        break;
    case IrCmd::STORE_TVALUE:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            state.invalidate(inst.a);

            if (uint8_t tag = state.tryGetTag(inst.b); tag != 0xff)
                state.saveTag(inst.a, tag);

            if (IrOp value = state.tryGetValue(inst.b); value.kind != IrOpKind::None)
                state.saveValue(inst.a, value);

            state.forwardVmRegStoreToLoad(inst, IrCmd::LOAD_TVALUE);
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
    case IrCmd::JUMP_LT_INT:
    {
        std::optional<int> valueA = function.asIntOp(inst.a.kind == IrOpKind::Constant ? inst.a : state.tryGetValue(inst.a));
        std::optional<int> valueB = function.asIntOp(inst.b.kind == IrOpKind::Constant ? inst.b : state.tryGetValue(inst.b));

        if (valueA && valueB)
        {
            if (*valueA < *valueB)
                replace(function, block, index, {IrCmd::JUMP, inst.c});
            else
                replace(function, block, index, {IrCmd::JUMP, inst.d});
        }
        break;
    }
    case IrCmd::JUMP_GE_UINT:
    {
        std::optional<unsigned> valueA = function.asUintOp(inst.a.kind == IrOpKind::Constant ? inst.a : state.tryGetValue(inst.a));
        std::optional<unsigned> valueB = function.asUintOp(inst.b.kind == IrOpKind::Constant ? inst.b : state.tryGetValue(inst.b));

        if (valueA && valueB)
        {
            if (*valueA >= *valueB)
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
            if (compare(*valueA, *valueB, conditionOp(inst.c)))
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

        // TODO: FASTCALL is more restrictive than INVOKE_FASTCALL; we should either determine the exact semantics, or rework it
    case IrCmd::FASTCALL:
    case IrCmd::INVOKE_FASTCALL:
        handleBuiltinEffects(state, LuauBuiltinFunction(function.uintOp(inst.a)), vmRegOp(inst.b), function.intOp(inst.f));
        break;

        // These instructions don't have an effect on register/memory state we are tracking
    case IrCmd::NOP:
    case IrCmd::LOAD_NODE_VALUE_TV:
    case IrCmd::STORE_NODE_VALUE_TV:
    case IrCmd::LOAD_ENV:
    case IrCmd::GET_ARR_ADDR:
    case IrCmd::GET_SLOT_NODE_ADDR:
    case IrCmd::GET_HASH_NODE_ADDR:
        break;
    case IrCmd::ADD_INT:
    case IrCmd::SUB_INT:
    case IrCmd::ADD_NUM:
    case IrCmd::SUB_NUM:
    case IrCmd::MUL_NUM:
    case IrCmd::DIV_NUM:
    case IrCmd::MOD_NUM:
    case IrCmd::MIN_NUM:
    case IrCmd::MAX_NUM:
    case IrCmd::UNM_NUM:
    case IrCmd::FLOOR_NUM:
    case IrCmd::CEIL_NUM:
    case IrCmd::ROUND_NUM:
    case IrCmd::SQRT_NUM:
    case IrCmd::ABS_NUM:
    case IrCmd::NOT_ANY:
        state.substituteOrRecord(inst, index);
        break;
    case IrCmd::JUMP:
    case IrCmd::JUMP_EQ_POINTER:
    case IrCmd::JUMP_SLOT_MATCH:
    case IrCmd::TABLE_LEN:
    case IrCmd::NEW_TABLE:
    case IrCmd::DUP_TABLE:
    case IrCmd::TRY_NUM_TO_INDEX:
    case IrCmd::TRY_CALL_FASTGETTM:
    case IrCmd::INT_TO_NUM:
    case IrCmd::UINT_TO_NUM:
    case IrCmd::NUM_TO_INT:
    case IrCmd::NUM_TO_UINT:
    case IrCmd::CHECK_ARRAY_SIZE:
    case IrCmd::CHECK_SLOT_MATCH:
    case IrCmd::CHECK_NODE_NO_NEXT:
    case IrCmd::BARRIER_TABLE_BACK:
    case IrCmd::RETURN:
    case IrCmd::COVERAGE:
    case IrCmd::SET_UPVALUE:
    case IrCmd::SET_SAVEDPC:  // TODO: we may be able to remove some updates to PC
    case IrCmd::CLOSE_UPVALS: // Doesn't change memory that we track
    case IrCmd::CAPTURE:
    case IrCmd::SUBSTITUTE:
    case IrCmd::ADJUST_STACK_TO_REG: // Changes stack top, but not the values
    case IrCmd::ADJUST_STACK_TO_TOP: // Changes stack top, but not the values
    case IrCmd::CHECK_FASTCALL_RES:  // Changes stack top, but not the values
    case IrCmd::BITAND_UINT:
    case IrCmd::BITXOR_UINT:
    case IrCmd::BITOR_UINT:
    case IrCmd::BITNOT_UINT:
    case IrCmd::BITLSHIFT_UINT:
    case IrCmd::BITRSHIFT_UINT:
    case IrCmd::BITARSHIFT_UINT:
    case IrCmd::BITRROTATE_UINT:
    case IrCmd::BITLROTATE_UINT:
    case IrCmd::BITCOUNTLZ_UINT:
    case IrCmd::BITCOUNTRZ_UINT:
    case IrCmd::INVOKE_LIBM:
        break;

    case IrCmd::JUMP_CMP_ANY:
        state.invalidateUserCall(); // TODO: if arguments are strings, there will be no user calls
        break;
    case IrCmd::DO_ARITH:
        state.invalidate(inst.a);
        state.invalidateUserCall();
        break;
    case IrCmd::DO_LEN:
        state.invalidate(inst.a);
        state.invalidateUserCall(); // TODO: if argument is a string, there will be no user call

        state.saveTag(inst.a, LUA_TNUMBER);
        break;
    case IrCmd::GET_TABLE:
        state.invalidate(inst.a);
        state.invalidateUserCall();
        break;
    case IrCmd::SET_TABLE:
        state.invalidateUserCall();
        break;
    case IrCmd::GET_IMPORT:
        state.invalidate(inst.a);
        state.invalidateUserCall();
        break;
    case IrCmd::CONCAT:
        state.invalidateRegisterRange(vmRegOp(inst.a), function.uintOp(inst.b));
        state.invalidateUserCall(); // TODO: if only strings and numbers are concatenated, there will be no user calls
        break;
    case IrCmd::PREPARE_FORN:
        state.invalidateValue(inst.a);
        state.saveTag(inst.a, LUA_TNUMBER);
        state.invalidateValue(inst.b);
        state.saveTag(inst.b, LUA_TNUMBER);
        state.invalidateValue(inst.c);
        state.saveTag(inst.c, LUA_TNUMBER);
        break;
    case IrCmd::INTERRUPT:
        state.invalidateUserCall();
        break;
    case IrCmd::SETLIST:
        state.valueMap.clear(); // TODO: this can be relaxed when x64 emitInstSetList becomes aware of register allocator
        break;
    case IrCmd::CALL:
        state.invalidateRegistersFrom(vmRegOp(inst.a));
        state.invalidateUserCall();

        // We cannot guarantee right now that all live values can be remeterialized from non-stack memory locations
        // To prevent earlier values from being propagated to after the call, we have to clear the map
        // TODO: remove only the values that don't have a guaranteed restore location
        state.valueMap.clear();
        break;
    case IrCmd::FORGLOOP:
        state.invalidateRegistersFrom(vmRegOp(inst.a) + 2); // Rn and Rn+1 are not modified
        state.valueMap.clear();                             // TODO: this can be relaxed when x64 emitInstForGLoop becomes aware of register allocator
        break;
    case IrCmd::FORGLOOP_FALLBACK:
        state.invalidateRegistersFrom(vmRegOp(inst.a) + 2); // Rn and Rn+1 are not modified
        state.invalidateUserCall();
        break;
    case IrCmd::FORGPREP_XNEXT_FALLBACK:
        // This fallback only conditionally throws an exception
        break;

        // Full fallback instructions
    case IrCmd::FALLBACK_GETGLOBAL:
        state.invalidate(inst.b);
        state.invalidateUserCall();
        break;
    case IrCmd::FALLBACK_SETGLOBAL:
        state.invalidateUserCall();
        break;
    case IrCmd::FALLBACK_GETTABLEKS:
        state.invalidate(inst.b);
        state.invalidateUserCall();
        break;
    case IrCmd::FALLBACK_SETTABLEKS:
        state.invalidateUserCall();
        break;
    case IrCmd::FALLBACK_NAMECALL:
        state.invalidate(IrOp{inst.b.kind, vmRegOp(inst.b) + 0u});
        state.invalidate(IrOp{inst.b.kind, vmRegOp(inst.b) + 1u});
        state.invalidateUserCall();
        break;
    case IrCmd::FALLBACK_PREPVARARGS:
        break;
    case IrCmd::FALLBACK_GETVARARGS:
        state.invalidateRegisterRange(vmRegOp(inst.b), function.intOp(inst.c));
        break;
    case IrCmd::FALLBACK_NEWCLOSURE:
        state.invalidate(inst.b);
        break;
    case IrCmd::FALLBACK_DUPCLOSURE:
        state.invalidate(inst.b);
        break;
    case IrCmd::FALLBACK_FORGPREP:
        state.invalidate(IrOp{inst.b.kind, vmRegOp(inst.b) + 0u});
        state.invalidate(IrOp{inst.b.kind, vmRegOp(inst.b) + 1u});
        state.invalidate(IrOp{inst.b.kind, vmRegOp(inst.b) + 2u});
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

    // Value numbering and load/store propagation is not performed between blocks
    state.valueMap.clear();
}

static void constPropInBlockChain(IrBuilder& build, std::vector<uint8_t>& visited, IrBlock* block, ConstPropState& state)
{
    IrFunction& function = build.function;

    state.clear();

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
            uint32_t targetIdx = function.getBlockIndex(target);

            if (target.useCount == 1 && !visited[targetIdx] && target.kind != IrBlockKind::Fallback)
                nextBlock = &target;
        }

        block = nextBlock;
    }
}

// Note that blocks in the collected path are marked as visited
static std::vector<uint32_t> collectDirectBlockJumpPath(IrFunction& function, std::vector<uint8_t>& visited, IrBlock* block)
{
    // Path shouldn't be built starting with a block that has 'live out' values.
    // One theoretical way to get it is if we had 'block' jumping unconditionally into a successor that uses values from 'block'
    // * if the successor has only one use, the starting conditions of 'tryCreateLinearBlock' would prevent this
    // * if the successor has multiple uses, it can't have such 'live in' values without phi nodes that we don't have yet
    // Another possibility is to have two paths from 'block' into the target through two intermediate blocks
    // Usually that would mean that we would have a conditional jump at the end of 'block'
    // But using check guards and fallback blocks it becomes a possible setup
    // We avoid this by making sure fallbacks rejoin the other immediate successor of 'block'
    LUAU_ASSERT(getLiveOutValueCount(function, *block) == 0);

    std::vector<uint32_t> path;

    while (block)
    {
        IrInst& termInst = function.instructions[block->finish];
        IrBlock* nextBlock = nullptr;

        // A chain is made from internal blocks that were not a part of bytecode CFG
        if (termInst.cmd == IrCmd::JUMP)
        {
            IrBlock& target = function.blockOp(termInst.a);
            uint32_t targetIdx = function.getBlockIndex(target);

            if (!visited[targetIdx] && target.kind == IrBlockKind::Internal)
            {
                // Additional restriction is that to join a block, it cannot produce values that are used in other blocks
                // And it also can't use values produced in other blocks
                auto [liveIns, liveOuts] = getLiveInOutValueCount(function, target);

                if (liveIns == 0 && liveOuts == 0)
                {
                    visited[targetIdx] = true;
                    path.push_back(targetIdx);

                    nextBlock = &target;
                }
            }
        }

        block = nextBlock;
    }

    return path;
}

static void tryCreateLinearBlock(IrBuilder& build, std::vector<uint8_t>& visited, IrBlock& startingBlock, ConstPropState& state)
{
    IrFunction& function = build.function;

    uint32_t blockIdx = function.getBlockIndex(startingBlock);
    LUAU_ASSERT(!visited[blockIdx]);
    visited[blockIdx] = true;

    IrInst& termInst = function.instructions[startingBlock.finish];

    // Block has to end with an unconditional jump
    if (termInst.cmd != IrCmd::JUMP)
        return;

    // And it has to jump to a block with more than one user
    // If there's only one use, it should already be optimized by constPropInBlockChain
    if (function.blockOp(termInst.a).useCount == 1)
        return;

    uint32_t targetBlockIdx = termInst.a.index;

    // Check if this path is worth it (and it will also mark path blocks as visited)
    std::vector<uint32_t> path = collectDirectBlockJumpPath(function, visited, &startingBlock);

    // If path is too small, we are not going to linearize it
    if (int(path.size()) < FInt::LuauCodeGenMinLinearBlockPath)
        return;

    // Initialize state with the knowledge of our current block
    state.clear();

    // TODO: using values from the first block can cause 'live out' of the linear block predecessor to not have all required registers
    constPropInBlock(build, startingBlock, state);

    // Veryfy that target hasn't changed
    LUAU_ASSERT(function.instructions[startingBlock.finish].a.index == targetBlockIdx);

    // Create new linearized block into which we are going to redirect starting block jump
    IrOp newBlock = build.block(IrBlockKind::Linearized);
    visited.push_back(false);

    // TODO: placement of linear blocks in final lowering is sub-optimal, it should follow our predecessor
    build.beginBlock(newBlock);

    replace(function, termInst.a, newBlock);

    // Clone the collected path into our fresh block
    for (uint32_t pathBlockIdx : path)
        build.clone(function.blocks[pathBlockIdx], /* removeCurrentTerminator */ true);

    // If all live in/out data is defined aside from the new block, generate it
    // Note that liveness information is not strictly correct after optimization passes and may need to be recomputed before next passes
    // The information generated here is consistent with current state that could be outdated, but still useful in IR inspection
    if (function.cfg.in.size() == newBlock.index)
    {
        LUAU_ASSERT(function.cfg.in.size() == function.cfg.out.size());
        LUAU_ASSERT(function.cfg.in.size() == function.cfg.def.size());

        // Live in is the same as the input of the original first block
        function.cfg.in.push_back(function.cfg.in[path.front()]);

        // Live out is the same as the result of the original last block
        function.cfg.out.push_back(function.cfg.out[path.back()]);

        // Defs are tricky, registers are joined together, but variadic sequences can be consumed inside the block
        function.cfg.def.push_back({});
        RegisterSet& def = function.cfg.def.back();

        for (uint32_t pathBlockIdx : path)
        {
            const RegisterSet& pathDef = function.cfg.def[pathBlockIdx];

            def.regs |= pathDef.regs;

            // Taking only the last defined variadic sequence if it's not consumed before before the end
            if (pathDef.varargSeq && function.cfg.out.back().varargSeq)
            {
                def.varargSeq = true;
                def.varargStart = pathDef.varargStart;
            }
        }
    }

    // Optimize our linear block
    IrBlock& linearBlock = function.blockOp(newBlock);
    constPropInBlock(build, linearBlock, state);
}

void constPropInBlockChains(IrBuilder& build, bool useValueNumbering)
{
    IrFunction& function = build.function;

    ConstPropState state{function};
    state.useValueNumbering = useValueNumbering;

    std::vector<uint8_t> visited(function.blocks.size(), false);

    for (IrBlock& block : function.blocks)
    {
        if (block.kind == IrBlockKind::Fallback || block.kind == IrBlockKind::Dead)
            continue;

        if (visited[function.getBlockIndex(block)])
            continue;

        constPropInBlockChain(build, visited, &block, state);
    }
}

void createLinearBlocks(IrBuilder& build, bool useValueNumbering)
{
    // Go through internal block chains and outline them into a single new block.
    // Outlining will be able to linearize the execution, even if there was a jump to a block with multiple users,
    // new 'block' will only be reachable from a single one and all gathered information can be preserved.
    IrFunction& function = build.function;

    ConstPropState state{function};
    state.useValueNumbering = useValueNumbering;

    std::vector<uint8_t> visited(function.blocks.size(), false);

    // This loop can create new 'linear' blocks, so index-based loop has to be used (and it intentionally won't reach those new blocks)
    size_t originalBlockCount = function.blocks.size();
    for (size_t i = 0; i < originalBlockCount; i++)
    {
        IrBlock& block = function.blocks[i];

        if (block.kind == IrBlockKind::Fallback || block.kind == IrBlockKind::Dead)
            continue;

        if (visited[function.getBlockIndex(block)])
            continue;

        tryCreateLinearBlock(build, visited, block, state);
    }
}

} // namespace CodeGen
} // namespace Luau
