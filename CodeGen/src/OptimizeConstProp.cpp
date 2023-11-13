// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/OptimizeConstProp.h"

#include "Luau/DenseHash.h"
#include "Luau/IrData.h"
#include "Luau/IrBuilder.h"
#include "Luau/IrUtils.h"

#include "lua.h"

#include <array>
#include <utility>
#include <vector>

LUAU_FASTINTVARIABLE(LuauCodeGenMinLinearBlockPath, 3)
LUAU_FASTINTVARIABLE(LuauCodeGenReuseSlotLimit, 64)
LUAU_FASTFLAGVARIABLE(DebugLuauAbortingChecks, false)
LUAU_FASTFLAGVARIABLE(LuauReuseArrSlots2, false)
LUAU_FASTFLAG(LuauLowerAltLoopForn)

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
    int knownTableArraySize = -1;
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

    void updateTag(IrOp op, uint8_t tag)
    {
        if (RegisterInfo* info = tryGetRegisterInfo(op))
            info->tag = tag;
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
                info->knownTableArraySize = -1;
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
            reg.knownTableArraySize = -1;
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
        if (count == -1)
        {
            invalidateRegistersFrom(firstReg);
        }
        else
        {
            for (int i = firstReg; i < firstReg + count && i <= maxReg; ++i)
                invalidate(regs[i], /* invalidateTag */ true, /* invalidateValue */ true);
        }
    }

    void invalidateCapturedRegisters()
    {
        for (int i = 0; i <= maxReg; ++i)
        {
            if (function.cfg.captured.regs.test(i))
                invalidate(regs[i], /* invalidateTag */ true, /* invalidateValue */ true);
        }
    }

    // Value propagation extends the live range of an SSA register
    // In some cases we can't propagate earlier values because we can't guarantee that we will be able to find a storage/restore location
    // As an example, when Luau call is performed, both volatile registers and stack slots might be overwritten
    void invalidateValuePropagation()
    {
        valueMap.clear();
        tryNumToIndexCache.clear();
    }

    // If table memory has changed, we can't reuse previously computed and validated table slot lookups
    // Same goes for table array elements as well
    void invalidateHeapTableData()
    {
        getSlotNodeCache.clear();
        checkSlotMatchCache.clear();

        getArrAddrCache.clear();
        checkArraySizeCache.clear();
    }

    void invalidateHeap()
    {
        for (int i = 0; i <= maxReg; ++i)
            invalidateHeap(regs[i]);

        invalidateHeapTableData();
    }

    void invalidateHeap(RegisterInfo& reg)
    {
        reg.knownNotReadonly = false;
        reg.knownNoMetatable = false;
        reg.knownTableArraySize = -1;
    }

    void invalidateUserCall()
    {
        invalidateHeap();
        invalidateCapturedRegisters();
        inSafeEnv = false;
    }

    void invalidateTableArraySize()
    {
        for (int i = 0; i <= maxReg; ++i)
            invalidateTableArraySize(regs[i]);

        invalidateHeapTableData();
    }

    void invalidateTableArraySize(RegisterInfo& reg)
    {
        reg.knownTableArraySize = -1;
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
            if (link->version < regs[link->reg].version)
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

    uint32_t* getPreviousInstIndex(const IrInst& inst)
    {
        LUAU_ASSERT(useValueNumbering);

        if (uint32_t* prevIdx = valueMap.find(inst))
        {
            // Previous load might have been removed as unused
            if (function.instructions[*prevIdx].useCount != 0)
                return prevIdx;
        }

        return nullptr;
    }

    uint32_t* getPreviousVersionedLoadIndex(IrCmd cmd, IrOp vmReg)
    {
        LUAU_ASSERT(vmReg.kind == IrOpKind::VmReg);
        return getPreviousInstIndex(versionedVmRegLoad(cmd, vmReg));
    }

    std::pair<IrCmd, uint32_t> getPreviousVersionedLoadForTag(uint8_t tag, IrOp vmReg)
    {
        if (useValueNumbering && !function.cfg.captured.regs.test(vmRegOp(vmReg)))
        {
            if (tag == LUA_TBOOLEAN)
            {
                if (uint32_t* prevIdx = getPreviousVersionedLoadIndex(IrCmd::LOAD_INT, vmReg))
                    return std::make_pair(IrCmd::LOAD_INT, *prevIdx);
            }
            else if (tag == LUA_TNUMBER)
            {
                if (uint32_t* prevIdx = getPreviousVersionedLoadIndex(IrCmd::LOAD_DOUBLE, vmReg))
                    return std::make_pair(IrCmd::LOAD_DOUBLE, *prevIdx);
            }
            else if (isGCO(tag))
            {
                if (uint32_t* prevIdx = getPreviousVersionedLoadIndex(IrCmd::LOAD_POINTER, vmReg))
                    return std::make_pair(IrCmd::LOAD_POINTER, *prevIdx);
            }
        }

        return std::make_pair(IrCmd::NOP, kInvalidInstIdx);
    }

    // Find existing value of the instruction that is exactly the same, or record current on for future lookups
    void substituteOrRecord(IrInst& inst, uint32_t instIdx)
    {
        if (!useValueNumbering)
            return;

        if (uint32_t* prevIdx = getPreviousInstIndex(inst))
        {
            substitute(function, inst, IrOp{IrOpKind::Inst, *prevIdx});
            return;
        }

        valueMap[inst] = instIdx;
    }

    // VM register load can be replaced by a previous load of the same version of the register
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
        if (uint32_t* prevIdx = getPreviousInstIndex(versionedLoad))
        {
            // Previous value might not be linked to a register yet
            // For example, it could be a NEW_TABLE stored into a register and we might need to track guards made with this value
            if (!instLink.contains(*prevIdx))
                createRegLink(*prevIdx, loadInst.a);

            // Substitute load instructon with the previous value
            substitute(function, loadInst, IrOp{IrOpKind::Inst, *prevIdx});
            return;
        }

        uint32_t instIdx = function.getInstIndex(loadInst);

        // Record load of this register version for future substitution
        valueMap[versionedLoad] = instIdx;

        createRegLink(instIdx, loadInst.a);
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

        invalidateValuePropagation();
        invalidateHeapTableData();
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

    // Some instruction re-uses can't be stored in valueMap because of extra requirements
    std::vector<uint32_t> tryNumToIndexCache; // Fallback block argument might be different

    // Heap changes might affect table state
    std::vector<uint32_t> getSlotNodeCache;    // Additionally, pcpos argument might be different
    std::vector<uint32_t> checkSlotMatchCache; // Additionally, fallback block argument might be different

    std::vector<uint32_t> getArrAddrCache;
    std::vector<uint32_t> checkArraySizeCache; // Additionally, fallback block argument might be different
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
    case LBF_RAWGET:
    case LBF_RAWEQUAL:
    case LBF_TABLE_UNPACK:
    case LBF_VECTOR:
    case LBF_BIT32_COUNTLZ:
    case LBF_BIT32_COUNTRZ:
    case LBF_SELECT_VARARG:
    case LBF_RAWLEN:
    case LBF_BIT32_EXTRACTK:
    case LBF_GETMETATABLE:
    case LBF_TONUMBER:
    case LBF_TOSTRING:
    case LBF_BIT32_BYTESWAP:
    case LBF_BUFFER_READI8:
    case LBF_BUFFER_READU8:
    case LBF_BUFFER_WRITEU8:
    case LBF_BUFFER_READI16:
    case LBF_BUFFER_READU16:
    case LBF_BUFFER_WRITEU16:
    case LBF_BUFFER_READI32:
    case LBF_BUFFER_READU32:
    case LBF_BUFFER_WRITEU32:
    case LBF_BUFFER_READF32:
    case LBF_BUFFER_WRITEF32:
    case LBF_BUFFER_READF64:
    case LBF_BUFFER_WRITEF64:
        break;
    case LBF_TABLE_INSERT:
        state.invalidateHeap();
        return; // table.insert does not modify result registers.
    case LBF_RAWSET:
        state.invalidateHeap();
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
        {
            substitute(function, inst, build.constTag(tag));
        }
        else if (inst.a.kind == IrOpKind::VmReg)
        {
            state.substituteOrRecordVmRegLoad(inst);
        }
        break;
    case IrCmd::LOAD_POINTER:
        if (inst.a.kind == IrOpKind::VmReg)
            state.substituteOrRecordVmRegLoad(inst);
        break;
    case IrCmd::LOAD_DOUBLE:
    {
        IrOp value = state.tryGetValue(inst.a);

        if (function.asDoubleOp(value))
            substitute(function, inst, value);
        else if (inst.a.kind == IrOpKind::VmReg)
            state.substituteOrRecordVmRegLoad(inst);
        break;
    }
    case IrCmd::LOAD_INT:
    {
        IrOp value = state.tryGetValue(inst.a);

        if (function.asIntOp(value))
            substitute(function, inst, value);
        else if (inst.a.kind == IrOpKind::VmReg)
            state.substituteOrRecordVmRegLoad(inst);
        break;
    }
    case IrCmd::LOAD_TVALUE:
        if (inst.a.kind == IrOpKind::VmReg)
            state.substituteOrRecordVmRegLoad(inst);
        break;
    case IrCmd::STORE_TAG:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            const IrOp source = inst.a;

            IrCmd activeLoadCmd = IrCmd::NOP;
            uint32_t activeLoadValue = kInvalidInstIdx;

            if (inst.b.kind == IrOpKind::Constant)
            {
                uint8_t value = function.tagOp(inst.b);

                // STORE_TAG usually follows a store of the value, but it also bumps the version of the whole register
                // To be able to propagate STORE_*** into LOAD_***, we find active LOAD_*** value and recreate it with updated version
                // Register in this optimization cannot be captured to avoid complications in lowering (IrValueLocationTracking doesn't model it)
                std::tie(activeLoadCmd, activeLoadValue) = state.getPreviousVersionedLoadForTag(value, source);

                if (state.tryGetTag(source) == value)
                {
                    if (FFlag::DebugLuauAbortingChecks)
                        replace(function, block, index, {IrCmd::CHECK_TAG, inst.a, inst.b, build.undef()});
                    else
                        kill(function, inst);
                }
                else
                {
                    state.saveTag(source, value);
                }
            }
            else
            {
                state.invalidateTag(source);
            }

            // Future LOAD_*** instructions can re-use previous register version load
            if (activeLoadValue != kInvalidInstIdx)
                state.valueMap[state.versionedVmRegLoad(activeLoadCmd, source)] = activeLoadValue;
        }
        break;
    case IrCmd::STORE_POINTER:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            state.invalidateValue(inst.a);
            state.forwardVmRegStoreToLoad(inst, IrCmd::LOAD_POINTER);

            if (IrInst* instOp = function.asInstOp(inst.b); instOp && instOp->cmd == IrCmd::NEW_TABLE)
            {
                if (RegisterInfo* info = state.tryGetRegisterInfo(inst.a))
                {
                    info->knownNotReadonly = true;
                    info->knownNoMetatable = true;
                    info->knownTableArraySize = function.uintOp(instOp->a);
                }
            }
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
        if (inst.a.kind == IrOpKind::VmReg || inst.a.kind == IrOpKind::Inst)
        {
            if (inst.a.kind == IrOpKind::VmReg)
                state.invalidate(inst.a);

            uint8_t tag = state.tryGetTag(inst.b);
            IrOp value = state.tryGetValue(inst.b);

            if (inst.a.kind == IrOpKind::VmReg)
            {
                if (tag != 0xff)
                    state.saveTag(inst.a, tag);

                if (value.kind != IrOpKind::None)
                    state.saveValue(inst.a, value);
            }

            IrCmd activeLoadCmd = IrCmd::NOP;
            uint32_t activeLoadValue = kInvalidInstIdx;

            if (tag != 0xff)
            {
                // If we know the tag, try to extract the value from a register used by LOAD_TVALUE
                if (IrInst* arg = function.asInstOp(inst.b); arg && arg->cmd == IrCmd::LOAD_TVALUE && arg->a.kind == IrOpKind::VmReg)
                {
                    std::tie(activeLoadCmd, activeLoadValue) = state.getPreviousVersionedLoadForTag(tag, arg->a);

                    if (activeLoadValue != kInvalidInstIdx)
                        value = IrOp{IrOpKind::Inst, activeLoadValue};
                }
            }

            // If we have constant tag and value, replace TValue store with tag/value pair store
            if (tag != 0xff && value.kind != IrOpKind::None && (tag == LUA_TBOOLEAN || tag == LUA_TNUMBER || isGCO(tag)))
            {
                replace(function, block, index, {IrCmd::STORE_SPLIT_TVALUE, inst.a, build.constTag(tag), value, inst.c});

                // Value can be propagated to future loads of the same register
                if (inst.a.kind == IrOpKind::VmReg && activeLoadValue != kInvalidInstIdx)
                    state.valueMap[state.versionedVmRegLoad(activeLoadCmd, inst.a)] = activeLoadValue;
            }
            else if (inst.a.kind == IrOpKind::VmReg)
            {
                state.forwardVmRegStoreToLoad(inst, IrCmd::LOAD_TVALUE);
            }
        }
        break;
    case IrCmd::STORE_SPLIT_TVALUE:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            state.invalidate(inst.a);

            state.saveTag(inst.a, function.tagOp(inst.b));

            if (inst.c.kind == IrOpKind::Constant)
                state.saveValue(inst.a, inst.c);
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
        else if (inst.a == inst.b)
        {
            replace(function, block, index, {IrCmd::JUMP, inst.c});
        }
        break;
    }
    case IrCmd::JUMP_CMP_INT:
    {
        std::optional<int> valueA = function.asIntOp(inst.a.kind == IrOpKind::Constant ? inst.a : state.tryGetValue(inst.a));
        std::optional<int> valueB = function.asIntOp(inst.b.kind == IrOpKind::Constant ? inst.b : state.tryGetValue(inst.b));

        if (valueA && valueB)
        {
            if (compare(*valueA, *valueB, conditionOp(inst.c)))
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
    case IrCmd::JUMP_FORN_LOOP_COND:
    {
        std::optional<double> step = function.asDoubleOp(inst.c.kind == IrOpKind::Constant ? inst.c : state.tryGetValue(inst.c));

        if (!step)
            break;

        std::optional<double> idx = function.asDoubleOp(inst.a.kind == IrOpKind::Constant ? inst.a : state.tryGetValue(inst.a));
        std::optional<double> limit = function.asDoubleOp(inst.b.kind == IrOpKind::Constant ? inst.b : state.tryGetValue(inst.b));

        if (*step > 0)
        {
            if (idx && limit)
            {
                if (compare(*idx, *limit, IrCondition::NotLessEqual))
                    replace(function, block, index, {IrCmd::JUMP, inst.e});
                else
                    replace(function, block, index, {IrCmd::JUMP, inst.d});
            }
            else
            {
                replace(function, block, index, IrInst{IrCmd::JUMP_CMP_NUM, inst.a, inst.b, build.cond(IrCondition::NotLessEqual), inst.e, inst.d});
            }
        }
        else
        {
            if (idx && limit)
            {
                if (compare(*limit, *idx, IrCondition::NotLessEqual))
                    replace(function, block, index, {IrCmd::JUMP, inst.e});
                else
                    replace(function, block, index, {IrCmd::JUMP, inst.d});
            }
            else
            {
                replace(function, block, index, IrInst{IrCmd::JUMP_CMP_NUM, inst.b, inst.a, build.cond(IrCondition::NotLessEqual), inst.e, inst.d});
            }
        }
        break;
    }
    case IrCmd::GET_UPVALUE:
        state.invalidate(inst.a);
        break;
    case IrCmd::SET_UPVALUE:
        if (inst.b.kind == IrOpKind::VmReg)
        {
            if (uint8_t tag = state.tryGetTag(inst.b); tag != 0xff)
            {
                replace(function, inst.c, build.constTag(tag));
            }
        }
        break;
    case IrCmd::CHECK_TAG:
    {
        uint8_t b = function.tagOp(inst.b);

        if (uint8_t tag = state.tryGetTag(inst.a); tag != 0xff)
        {
            if (tag == b)
            {
                if (FFlag::DebugLuauAbortingChecks)
                    replace(function, inst.c, build.undef());
                else
                    kill(function, inst);
            }
            else
            {
                replace(function, block, index, {IrCmd::JUMP, inst.c}); // Shows a conflict in assumptions on this path
            }
        }
        else
        {
            state.updateTag(inst.a, b); // We can assume the tag value going forward
        }
        break;
    }
    case IrCmd::CHECK_TRUTHY:
        // It is possible to check if current tag in state is truthy or not, but this case almost never comes up
        break;
    case IrCmd::CHECK_READONLY:
        if (RegisterInfo* info = state.tryGetRegisterInfo(inst.a))
        {
            if (info->knownNotReadonly)
            {
                if (FFlag::DebugLuauAbortingChecks)
                    replace(function, inst.b, build.undef());
                else
                    kill(function, inst);
            }
            else
            {
                info->knownNotReadonly = true;
            }
        }
        break;
    case IrCmd::CHECK_NO_METATABLE:
        if (RegisterInfo* info = state.tryGetRegisterInfo(inst.a))
        {
            if (info->knownNoMetatable)
            {
                if (FFlag::DebugLuauAbortingChecks)
                    replace(function, inst.b, build.undef());
                else
                    kill(function, inst);
            }
            else
            {
                info->knownNoMetatable = true;
            }
        }
        break;
    case IrCmd::CHECK_SAFE_ENV:
        if (state.inSafeEnv)
        {
            if (FFlag::DebugLuauAbortingChecks)
                replace(function, inst.a, build.undef());
            else
                kill(function, inst);
        }
        else
        {
            state.inSafeEnv = true;
        }
        break;
    case IrCmd::CHECK_BUFFER_LEN:
        // TODO: remove duplicate checks and extend earlier check bound when possible
        break;
    case IrCmd::BUFFER_READI8:
    case IrCmd::BUFFER_READU8:
    case IrCmd::BUFFER_WRITEI8:
    case IrCmd::BUFFER_READI16:
    case IrCmd::BUFFER_READU16:
    case IrCmd::BUFFER_WRITEI16:
    case IrCmd::BUFFER_READI32:
    case IrCmd::BUFFER_WRITEI32:
    case IrCmd::BUFFER_READF32:
    case IrCmd::BUFFER_WRITEF32:
    case IrCmd::BUFFER_READF64:
    case IrCmd::BUFFER_WRITEF64:
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
                else
                    replace(function, inst.c, build.constTag(tag));
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
    case IrCmd::LOAD_ENV:
        break;
    case IrCmd::GET_ARR_ADDR:
        if (!FFlag::LuauReuseArrSlots2)
            break;

        for (uint32_t prevIdx : state.getArrAddrCache)
        {
            const IrInst& prev = function.instructions[prevIdx];

            if (prev.a == inst.a && prev.b == inst.b)
            {
                substitute(function, inst, IrOp{IrOpKind::Inst, prevIdx});
                return; // Break out from both the loop and the switch
            }
        }

        if (int(state.getArrAddrCache.size()) < FInt::LuauCodeGenReuseSlotLimit)
            state.getArrAddrCache.push_back(index);
        break;
    case IrCmd::GET_SLOT_NODE_ADDR:
        for (uint32_t prevIdx : state.getSlotNodeCache)
        {
            const IrInst& prev = function.instructions[prevIdx];

            if (prev.a == inst.a && prev.c == inst.c)
            {
                substitute(function, inst, IrOp{IrOpKind::Inst, prevIdx});
                return; // Break out from both the loop and the switch
            }
        }

        if (int(state.getSlotNodeCache.size()) < FInt::LuauCodeGenReuseSlotLimit)
            state.getSlotNodeCache.push_back(index);
        break;
    case IrCmd::GET_HASH_NODE_ADDR:
    case IrCmd::GET_CLOSURE_UPVAL_ADDR:
        break;
    case IrCmd::ADD_INT:
    case IrCmd::SUB_INT:
    case IrCmd::ADD_NUM:
    case IrCmd::SUB_NUM:
    case IrCmd::MUL_NUM:
    case IrCmd::DIV_NUM:
    case IrCmd::IDIV_NUM:
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
    case IrCmd::CMP_ANY:
        state.invalidateUserCall();
        break;
    case IrCmd::JUMP:
    case IrCmd::JUMP_EQ_POINTER:
    case IrCmd::JUMP_SLOT_MATCH:
    case IrCmd::TABLE_LEN:
        break;
    case IrCmd::TABLE_SETNUM:
        state.invalidateTableArraySize();
        break;
    case IrCmd::STRING_LEN:
    case IrCmd::NEW_TABLE:
    case IrCmd::DUP_TABLE:
        break;
    case IrCmd::TRY_NUM_TO_INDEX:
        if (!FFlag::LuauReuseArrSlots2)
            break;

        for (uint32_t prevIdx : state.tryNumToIndexCache)
        {
            const IrInst& prev = function.instructions[prevIdx];

            if (prev.a == inst.a)
            {
                substitute(function, inst, IrOp{IrOpKind::Inst, prevIdx});
                return; // Break out from both the loop and the switch
            }
        }

        if (int(state.tryNumToIndexCache.size()) < FInt::LuauCodeGenReuseSlotLimit)
            state.tryNumToIndexCache.push_back(index);
        break;
    case IrCmd::TRY_CALL_FASTGETTM:
        break;
    case IrCmd::INT_TO_NUM:
    case IrCmd::UINT_TO_NUM:
        state.substituteOrRecord(inst, index);
        break;
    case IrCmd::NUM_TO_INT:
        if (IrInst* src = function.asInstOp(inst.a); src && src->cmd == IrCmd::INT_TO_NUM)
            substitute(function, inst, src->a);
        else
            state.substituteOrRecord(inst, index);
        break;
    case IrCmd::NUM_TO_UINT:
        if (IrInst* src = function.asInstOp(inst.a); src && src->cmd == IrCmd::UINT_TO_NUM)
            substitute(function, inst, src->a);
        else
            state.substituteOrRecord(inst, index);
        break;
    case IrCmd::CHECK_ARRAY_SIZE:
    {
        std::optional<int> arrayIndex = function.asIntOp(inst.b.kind == IrOpKind::Constant ? inst.b : state.tryGetValue(inst.b));

        // Negative offsets will jump to fallback, no need to keep the check
        if (FFlag::LuauReuseArrSlots2 && arrayIndex && *arrayIndex < 0)
        {
            replace(function, block, index, {IrCmd::JUMP, inst.c});
            break;
        }

        if (RegisterInfo* info = state.tryGetRegisterInfo(inst.a); info && arrayIndex)
        {
            if (info->knownTableArraySize >= 0)
            {
                if (unsigned(*arrayIndex) < unsigned(info->knownTableArraySize))
                {
                    if (FFlag::DebugLuauAbortingChecks)
                        replace(function, inst.c, build.undef());
                    else
                        kill(function, inst);
                }
                else
                {
                    replace(function, block, index, {IrCmd::JUMP, inst.c});
                }

                break;
            }
        }

        if (!FFlag::LuauReuseArrSlots2)
            break;

        for (uint32_t prevIdx : state.checkArraySizeCache)
        {
            const IrInst& prev = function.instructions[prevIdx];

            if (prev.a != inst.a)
                continue;

            bool sameBoundary = prev.b == inst.b;

            // If arguments are different, in case they are both constant, we can check if a larger bound was already tested
            if (!sameBoundary && inst.b.kind == IrOpKind::Constant && prev.b.kind == IrOpKind::Constant &&
                unsigned(function.intOp(inst.b)) < unsigned(function.intOp(prev.b)))
                sameBoundary = true;

            if (sameBoundary)
            {
                if (FFlag::DebugLuauAbortingChecks)
                    replace(function, inst.c, build.undef());
                else
                    kill(function, inst);
                return; // Break out from both the loop and the switch
            }

            // TODO: it should be possible to update previous check with a higher bound if current and previous checks are against a constant
        }

        if (int(state.checkArraySizeCache.size()) < FInt::LuauCodeGenReuseSlotLimit)
            state.checkArraySizeCache.push_back(index);
        break;
    }
    case IrCmd::CHECK_SLOT_MATCH:
        for (uint32_t prevIdx : state.checkSlotMatchCache)
        {
            const IrInst& prev = function.instructions[prevIdx];

            if (prev.a == inst.a && prev.b == inst.b)
            {
                // Only a check for 'nil' value is left
                replace(function, block, index, {IrCmd::CHECK_NODE_VALUE, inst.a, inst.c});
                return; // Break out from both the loop and the switch
            }
        }

        if (int(state.checkSlotMatchCache.size()) < FInt::LuauCodeGenReuseSlotLimit)
            state.checkSlotMatchCache.push_back(index);
        break;
    case IrCmd::CHECK_NODE_NO_NEXT:
    case IrCmd::CHECK_NODE_VALUE:
    case IrCmd::BARRIER_TABLE_BACK:
    case IrCmd::RETURN:
    case IrCmd::COVERAGE:
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
        break;
    case IrCmd::BITLSHIFT_UINT:
    case IrCmd::BITRSHIFT_UINT:
    case IrCmd::BITARSHIFT_UINT:
    case IrCmd::BITRROTATE_UINT:
    case IrCmd::BITLROTATE_UINT:
    case IrCmd::BITCOUNTLZ_UINT:
    case IrCmd::BITCOUNTRZ_UINT:
    case IrCmd::BYTESWAP_UINT:
    case IrCmd::INVOKE_LIBM:
    case IrCmd::GET_TYPE:
    case IrCmd::GET_TYPEOF:
    case IrCmd::FINDUPVAL:
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
    case IrCmd::INTERRUPT:
        state.invalidateUserCall();
        break;
    case IrCmd::SETLIST:
        if (RegisterInfo* info = state.tryGetRegisterInfo(inst.b); info && info->knownTableArraySize >= 0)
            replace(function, inst.f, build.constUint(info->knownTableArraySize));

        // TODO: this can be relaxed when x64 emitInstSetList becomes aware of register allocator
        state.invalidateValuePropagation();
        state.invalidateHeapTableData();
        break;
    case IrCmd::CALL:
        state.invalidateRegistersFrom(vmRegOp(inst.a));
        state.invalidateUserCall();

        // We cannot guarantee right now that all live values can be rematerialized from non-stack memory locations
        // To prevent earlier values from being propagated to after the call, we have to clear the map
        // TODO: remove only the values that don't have a guaranteed restore location
        state.invalidateValuePropagation();
        break;
    case IrCmd::FORGLOOP:
        state.invalidateRegistersFrom(vmRegOp(inst.a) + 2); // Rn and Rn+1 are not modified

        // TODO: this can be relaxed when x64 emitInstForGLoop becomes aware of register allocator
        state.invalidateValuePropagation();
        state.invalidateHeapTableData();
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
    case IrCmd::NEWCLOSURE:
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
    state.invalidateValuePropagation();

    // Same for table slot data propagation
    state.invalidateHeapTableData();
}

static void constPropInBlockChain(IrBuilder& build, std::vector<uint8_t>& visited, IrBlock* block, ConstPropState& state)
{
    IrFunction& function = build.function;

    state.clear();

    const uint32_t startSortkey = block->sortkey;
    uint32_t chainPos = 0;

    while (block)
    {
        uint32_t blockIdx = function.getBlockIndex(*block);
        LUAU_ASSERT(!visited[blockIdx]);
        visited[blockIdx] = true;

        constPropInBlock(build, *block, state);

        // Blocks in a chain are guaranteed to follow each other
        // We force that by giving all blocks the same sorting key, but consecutive chain keys
        block->sortkey = startSortkey;
        block->chainkey = chainPos++;

        IrInst& termInst = function.instructions[block->finish];

        IrBlock* nextBlock = nullptr;

        // Unconditional jump into a block with a single user (current block) allows us to continue optimization
        // with the information we have gathered so far (unless we have already visited that block earlier)
        if (termInst.cmd == IrCmd::JUMP && termInst.a.kind == IrOpKind::Block)
        {
            IrBlock& target = function.blockOp(termInst.a);
            uint32_t targetIdx = function.getBlockIndex(target);

            if (target.useCount == 1 && !visited[targetIdx] && target.kind != IrBlockKind::Fallback)
            {
                if (FFlag::LuauLowerAltLoopForn && getLiveOutValueCount(function, target) != 0)
                    break;

                // Make sure block ordering guarantee is checked at lowering time
                block->expectedNextBlock = function.getBlockIndex(target);

                nextBlock = &target;
            }
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
        if (termInst.cmd == IrCmd::JUMP && termInst.a.kind == IrOpKind::Block)
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

    // And it can't be jump to a VM exit or undef
    if (termInst.a.kind != IrOpKind::Block)
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

    constPropInBlock(build, startingBlock, state);

    // Verify that target hasn't changed
    LUAU_ASSERT(function.instructions[startingBlock.finish].a.index == targetBlockIdx);

    // Note: using startingBlock after this line is unsafe as the reference may be reallocated by build.block() below
    const uint32_t startingSortKey = startingBlock.sortkey;
    const uint32_t startingChainKey = startingBlock.chainkey;

    // Create new linearized block into which we are going to redirect starting block jump
    IrOp newBlock = build.block(IrBlockKind::Linearized);
    visited.push_back(false);

    build.beginBlock(newBlock);

    // By default, blocks are ordered according to start instruction; we alter sort order to make sure linearized block is placed right after the
    // starting block
    function.blocks[newBlock.index].sortkey = startingSortKey;
    function.blocks[newBlock.index].chainkey = startingChainKey + 1;

    // Make sure block ordering guarantee is checked at lowering time
    function.blocks[blockIdx].expectedNextBlock = newBlock.index;

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

        // Update predecessors
        function.cfg.predecessorsOffsets.push_back(uint32_t(function.cfg.predecessors.size()));
        function.cfg.predecessors.push_back(blockIdx);

        // Updating successors will require visiting the instructions again and we don't have a current use for linearized block successor list
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
