// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/OptimizeDeadStore.h"

#include "Luau/IrBuilder.h"
#include "Luau/IrVisitUseDef.h"
#include "Luau/IrUtils.h"

#include <algorithm>
#include <array>

#include "lobject.h"

LUAU_FASTFLAGVARIABLE(LuauCodegenDsePtrStoreTagCheck)
LUAU_FASTFLAG(LuauCodegenVmExitSync)
LUAU_FASTFLAGVARIABLE(LuauCodegenVmExitSyncFix)
LUAU_FASTFLAGVARIABLE(LuauCodegenDseRestoreHints)
LUAU_FLAGVERSION(LuauCodegenDseRestoreHints, 2)

// TODO: optimization can be improved by knowing which registers are live in at each VM exit

namespace Luau
{
namespace CodeGen
{

// Result-producing instructions that pass !hasSideEffects but are still unsafe to sink into ExitSync blocks
static bool isUnsafeToSink(IrCmd cmd)
{
    switch (cmd)
    {
    // VM register reads: STORE_TAG/STORE_DOUBLE/STORE_TVALUE/etc. to the same VM register
    case IrCmd::LOAD_TAG:
    case IrCmd::LOAD_POINTER:
    case IrCmd::LOAD_DOUBLE:
    case IrCmd::LOAD_INT:
    case IrCmd::LOAD_INT64:
    case IrCmd::LOAD_FLOAT:
    case IrCmd::LOAD_TVALUE:

    // Buffer reads: BUFFER_WRITE* to the same buffer at the same offset
    case IrCmd::BUFFER_READI8:
    case IrCmd::BUFFER_READU8:
    case IrCmd::BUFFER_READI16:
    case IrCmd::BUFFER_READU16:
    case IrCmd::BUFFER_READI32:
    case IrCmd::BUFFER_READI64:
    case IrCmd::BUFFER_READF32:
    case IrCmd::BUFFER_READF64:

    // Upvalue read: SET_UPVALUE to the same upvalue slot
    case IrCmd::GET_UPVALUE:

    // Reads table array metadata: TABLE_SETNUM can grow the array and change the length
    case IrCmd::TABLE_LEN:

    // Reads VM register: STORE_TAG/STORE_TVALUE/etc. to the same VM register
    case IrCmd::GET_TYPEOF:

    // Mutates table array part, invalidating reads
    case IrCmd::TABLE_SETNUM:

    // Can execute user metamethods via luaV_equalval/luaV_lessthan/luaV_lessequal
    case IrCmd::CMP_ANY:

    // Branch operand targets a fallback block: can't appear in an exit sync sequence
    case IrCmd::TRY_NUM_TO_INDEX:
    case IrCmd::TRY_CALL_FASTGETTM:
        return true;
    default:
        return false;
    }
}

// Luau value structure reminder:
// [              TValue             ]
// [     Value     ][ Extra ][  Tag  ]
// Storing individual components will not kill any previous TValue stores
// Storing TValue will kill any full store or a component store ('extra' excluded because it's rare)

struct StoreRegInfo
{
    // Indices of the last unused store instructions
    uint32_t tagInstIdx = ~0u;
    uint32_t valueInstIdx = ~0u;
    uint32_t tvalueInstIdx = ~0u;

    // This register might contain a GC object
    bool maybeGco = false;

    // This register can be assumed to not be used in a VM exit
    bool ignoreAtExit = false;

    // Knowing the last stored tag can help safely remove additional unused partial stores
    uint8_t knownTag = kUnknownTag;
};

struct RemoveDeadStoreState
{
    RemoveDeadStoreState(IrFunction& function, std::vector<uint32_t>& remainingUses)
        : function(function)
        , remainingUses(remainingUses)
    {
        maxReg = function.proto ? function.proto->maxstacksize : 255;
    }

    void recordHintBeforeKill(uint32_t storeInstIdx)
    {
        IrInst& storeInst = function.instructions[storeInstIdx];

        IrOp dest = OP_A(storeInst);

        if (dest.kind != IrOpKind::VmReg)
            return;

        IrOp value;
        IrValueKind kind = IrValueKind::Unknown;

        switch (storeInst.cmd)
        {
        case IrCmd::STORE_DOUBLE:
            value = OP_B(storeInst);
            kind = IrValueKind::Double;
            break;
        case IrCmd::STORE_INT:
            value = OP_B(storeInst);
            kind = IrValueKind::Int;
            break;
        case IrCmd::STORE_INT64:
            value = OP_B(storeInst);
            kind = IrValueKind::Int64;
            break;
        case IrCmd::STORE_POINTER:
            value = OP_B(storeInst);
            kind = IrValueKind::Pointer;
            break;
        case IrCmd::STORE_TVALUE:
            value = OP_B(storeInst);
            kind = IrValueKind::Tvalue;
            break;
        case IrCmd::STORE_SPLIT_TVALUE:
            value = OP_C(storeInst);
            if (value.kind == IrOpKind::Inst)
                kind = getCmdValueKind(function.instOp(value).cmd);
            if (kind == IrValueKind::Unknown)
                return;
            break;
        case IrCmd::STORE_VECTOR:
            return; // multi-component, not useful as a single-value restore hint
        default:
            return;
        }

        if (value.kind != IrOpKind::Inst)
            return;

        function.recordStoreLocationHint(storeInstIdx, {dest, value.index, kind});
    }
    void killTagStore(StoreRegInfo& regInfo)
    {
        if (regInfo.tagInstIdx != ~0u)
        {
            kill(function, function.instructions[regInfo.tagInstIdx]);

            regInfo.tagInstIdx = ~0u;
            regInfo.maybeGco = false;
        }
    }

    void killValueStore(StoreRegInfo& regInfo)
    {
        if (regInfo.valueInstIdx != ~0u)
        {
            kill(function, function.instructions[regInfo.valueInstIdx]);

            regInfo.valueInstIdx = ~0u;
            regInfo.maybeGco = false;
        }
    }

    bool tagValuePairEstablished(StoreRegInfo& regInfo)
    {
        bool tagEstablished = regInfo.tagInstIdx != ~0u || regInfo.knownTag != kUnknownTag;

        // When tag is 'nil', we don't need to remove the unused value store
        bool valueEstablished = regInfo.valueInstIdx != ~0u || regInfo.knownTag == LUA_TNIL;

        return tagEstablished && valueEstablished;
    }

    void killTagAndValueStorePair(StoreRegInfo& regInfo)
    {
        // Partial stores can only be removed if the whole pair is established
        if (tagValuePairEstablished(regInfo))
        {
            if (regInfo.tagInstIdx != ~0u)
            {
                kill(function, function.instructions[regInfo.tagInstIdx]);
                regInfo.tagInstIdx = ~0u;
            }

            if (regInfo.valueInstIdx != ~0u)
            {
                if (FFlag::LuauCodegenDseRestoreHints)
                    recordHintBeforeKill(regInfo.valueInstIdx);

                kill(function, function.instructions[regInfo.valueInstIdx]);
                regInfo.valueInstIdx = ~0u;
            }

            regInfo.maybeGco = false;
        }
    }

    void killTValueStore(StoreRegInfo& regInfo)
    {
        // TValue can only be killed if it is not overlayed by a partial tag/value write
        if (regInfo.tvalueInstIdx != kInvalidInstIdx && regInfo.tagInstIdx == kInvalidInstIdx && regInfo.valueInstIdx == kInvalidInstIdx)
        {
            if (FFlag::LuauCodegenDseRestoreHints)
                recordHintBeforeKill(regInfo.tvalueInstIdx);

            kill(function, function.instructions[regInfo.tvalueInstIdx]);

            regInfo.tvalueInstIdx = kInvalidInstIdx;
            regInfo.maybeGco = false;
        }
    }

    // When a register value is being defined, it kills previous stores
    void defReg(uint8_t reg)
    {
        StoreRegInfo& regInfo = info[reg];

        // Stores to captured registers are not removed since we don't track their uses outside of function
        if (function.cfg.captured.regs.test(reg))
            return;

        killTagAndValueStorePair(regInfo);
        killTValueStore(regInfo);

        regInfo.tagInstIdx = kInvalidInstIdx;
        regInfo.valueInstIdx = kInvalidInstIdx;
        regInfo.tvalueInstIdx = kInvalidInstIdx;

        // Opaque register definition removes the knowledge of the actual tag value
        regInfo.knownTag = kUnknownTag;

        // New value defined, before MARK_DEAD is used again, it might be used in a VM exit
        regInfo.ignoreAtExit = false;
    }

    // When a register value is being used (read), we forget about the last store location to not kill them
    void useReg(uint8_t reg)
    {
        StoreRegInfo& regInfo = info[reg];

        // Register read doesn't clear the known tag
        regInfo.tagInstIdx = ~0u;
        regInfo.valueInstIdx = ~0u;
        regInfo.tvalueInstIdx = ~0u;
        regInfo.maybeGco = false;
    }

    // Marks pending stores as non-propagating to prevent moving their uses into VM exit blocks
    // Moving a store into a VM exit extends the live range of the store operands
    // We must ensure that this lifetime extension does not cross instructions which invalidate physical locations
    // This is similar to value propagation barriers in OptimizeConstProp.cpp
    void invalidateValuePropagation(StoreRegInfo& regInfo)
    {
        auto hasInstArg = [](IrInst& inst)
        {
            return anyArgumentMatch(
                inst,
                [](IrOp op)
                {
                    return op.kind == IrOpKind::Inst;
                }
            );
        };

        if (regInfo.tagInstIdx != kInvalidInstIdx && hasInstArg(function.instructions[regInfo.tagInstIdx]))
            nonPropagatingStore.insert(regInfo.tagInstIdx);

        if (regInfo.valueInstIdx != kInvalidInstIdx && hasInstArg(function.instructions[regInfo.valueInstIdx]))
            nonPropagatingStore.insert(regInfo.valueInstIdx);

        if (regInfo.tvalueInstIdx != kInvalidInstIdx && hasInstArg(function.instructions[regInfo.tvalueInstIdx]))
            nonPropagatingStore.insert(regInfo.tvalueInstIdx);
    }

    void invalidateValuePropagation()
    {
        for (int i = 0; i <= maxReg; i++)
            invalidateValuePropagation(info[i]);
    }

    // VmExit information contains data that needs a sync if the stores are removed as unused
    // If the store was not removed as dead, we don't need to sync it in the exit
    void pruneVmExitInfo()
    {
        for (uint32_t instIdx : recordedVmExitSyncs)
        {
            VmExitSyncInfo& syncInfo = function.vmExitInfo[instIdx];

            for (size_t i = 0; i < syncInfo.regStores.size();)
            {
                auto& el = syncInfo.regStores[i];

                for (size_t j = 0; j < el.stores.size();)
                {
                    if (function.instructions[el.stores[j].instIdx].cmd != IrCmd::NOP)
                    {
                        visitArguments(
                            function.instructions[el.stores[j].instIdx],
                            [&](IrOp op)
                            {
                                removeUse(function, op);
                            }
                        );

                        el.stores[j] = el.stores.back();
                        el.stores.pop_back();
                    }
                    else
                    {
                        j++;
                    }
                }

                if (el.stores.empty())
                {
                    syncInfo.regStores[i] = syncInfo.regStores.back();
                    syncInfo.regStores.pop_back();
                }
                else
                {
                    i++;
                }
            }
        }
    }

    // When checking control flow, such as exit to fallback blocks:
    // For VM exits, we keep all stores except marked dead because we don't have information on what registers are live at the start of the VM assist
    // For regular blocks, we check which registers are expected to be live at entry (if we have CFG information available)
    void checkLiveIns(IrOp op, uint32_t instIdx, bool recordVmExitSync)
    {
        if (op.kind == IrOpKind::VmExit)
        {
            if (FFlag::LuauCodegenVmExitSync && recordVmExitSync && vmExitOp(op) != kVmExitEntryGuardPc)
            {
                VmExitSyncInfo& syncInfo = function.vmExitInfo[instIdx];
                CODEGEN_ASSERT(syncInfo.regStores.empty());

                syncInfo.vmExit = op;

                recordedVmExitSyncs.push_back(instIdx);

                // Reverse order so that we capture lexically close VM registers first
                // In case the limit is hit, shortest live ranges will be included
                for (int i = maxReg; i >= 0; i--)
                {
                    StoreRegInfo& regInfo = info[i];

                    // If value cannot be propagated into the exit, store must remain as used by the exit
                    if ((regInfo.tagInstIdx != kInvalidInstIdx && nonPropagatingStore.contains(regInfo.tagInstIdx)) ||
                        (regInfo.valueInstIdx != kInvalidInstIdx && nonPropagatingStore.contains(regInfo.valueInstIdx)) ||
                        (regInfo.tvalueInstIdx != kInvalidInstIdx && nonPropagatingStore.contains(regInfo.tvalueInstIdx)))
                    {
                        useReg(i);
                        continue;
                    }

                    if (regInfo.ignoreAtExit && !regInfo.maybeGco)
                        continue;

                    if (syncInfo.regStores.size() >= 16)
                    {
                        useReg(i);
                        continue;
                    }

                    bool hasPartialOverlap = (regInfo.tagInstIdx != kInvalidInstIdx || regInfo.valueInstIdx != kInvalidInstIdx) &&
                                             regInfo.tvalueInstIdx != kInvalidInstIdx;

                    if (hasPartialOverlap)
                    {
                        useReg(i);
                        continue;
                    }

                    VmExitStoreInfo storeInfo;

                    storeInfo.reg = uint8_t(i);

                    auto recordStore = [&](uint32_t instIdx)
                    {
                        IrInst& store = function.instructions[instIdx];
                        storeInfo.stores.push_back({instIdx, store});
                        visitArguments(
                            store,
                            [&](IrOp op)
                            {
                                addUse(function, op);
                            }
                        );
                    };

                    if (regInfo.tagInstIdx != kInvalidInstIdx)
                    {
                        CODEGEN_ASSERT(regInfo.tvalueInstIdx == kInvalidInstIdx);
                        recordStore(regInfo.tagInstIdx);
                    }

                    if (regInfo.valueInstIdx != kInvalidInstIdx)
                    {
                        CODEGEN_ASSERT(regInfo.tvalueInstIdx == kInvalidInstIdx);
                        recordStore(regInfo.valueInstIdx);
                    }

                    if (regInfo.tvalueInstIdx != kInvalidInstIdx)
                    {
                        IrInst& store = function.instructions[regInfo.tvalueInstIdx];
                        CODEGEN_ASSERT(regInfo.tagInstIdx == kInvalidInstIdx && regInfo.valueInstIdx == kInvalidInstIdx);
                        CODEGEN_ASSERT(
                            store.cmd == IrCmd::STORE_SPLIT_TVALUE || store.cmd == IrCmd::STORE_TVALUE || store.cmd == IrCmd::STORE_VECTOR ||
                            (store.cmd == IrCmd::STORE_TAG && function.tagOp(OP_B(store)) == LUA_TNIL)
                        );

                        recordStore(regInfo.tvalueInstIdx);
                    }

                    if (!storeInfo.stores.empty())
                        syncInfo.regStores.push_back(storeInfo);
                }
            }
            else
            {
                for (int i = 0; i <= maxReg; i++)
                {
                    StoreRegInfo& regInfo = info[i];

                    if (regInfo.ignoreAtExit && !regInfo.maybeGco)
                        continue;

                    useReg(i);
                }

                hasGcoToClear = false;
            }
        }
        else if (op.kind == IrOpKind::Block)
        {
            if (op.index < function.cfg.in.size())
            {
                const RegisterSet& in = function.cfg.in[op.index];

                for (int i = 0; i <= maxReg; i++)
                {
                    if (in.regs.test(i) || (in.varargSeq && i >= in.varargStart))
                        useReg(i);
                }
            }
            else
            {
                readAllRegs();
            }
        }
        else if (op.kind == IrOpKind::Undef)
        {
            // Nothing to do for a debug abort
        }
        else
        {
            CODEGEN_ASSERT(!"unexpected jump target type");
        }
    }

    // When checking block terminators, any registers that are not live out can be removed by saying that a new value is being 'defined'
    void checkLiveOuts(const IrBlock& block)
    {
        uint32_t index = function.getBlockIndex(block);

        if (index < function.cfg.out.size())
        {
            const RegisterSet& out = function.cfg.out[index];

            for (int i = 0; i <= maxReg; i++)
            {
                bool isOut = out.regs.test(i) || (out.varargSeq && i >= out.varargStart);

                if (!isOut)
                {
                    StoreRegInfo& regInfo = info[i];

                    // Stores to captured registers are not removed since we don't track their uses outside of function
                    if (!function.cfg.captured.regs.test(i))
                    {
                        killTagAndValueStorePair(regInfo);
                        killTValueStore(regInfo);
                    }
                }
            }
        }
    }

    void markUnusedAtExit(int start, int count)
    {
        CODEGEN_ASSERT(count != 0);

        int e = count == -1 ? maxReg : start + count - 1;

        for (int i = start; i <= e; i++)
        {
            StoreRegInfo& regInfo = info[i];

            // Stores to captured registers are not removed since we don't track their uses outside of function
            if (!function.cfg.captured.regs.test(i))
                regInfo.ignoreAtExit = true;
        }
    }

    // Common instruction visitor handling
    void defVarargs(uint8_t varargStart)
    {
        for (int i = varargStart; i <= maxReg; i++)
            defReg(uint8_t(i));
    }

    void useVarargs(uint8_t varargStart)
    {
        for (int i = varargStart; i <= maxReg; i++)
            useReg(uint8_t(i));
    }

    void def(IrOp op, int offset = 0)
    {
        defReg(vmRegOp(op) + offset);
    }

    void use(IrOp op, int offset = 0)
    {
        useReg(vmRegOp(op) + offset);
    }

    void maybeDef(IrOp op)
    {
        if (op.kind == IrOpKind::VmReg)
            defReg(vmRegOp(op));
    }

    void maybeUse(IrOp op)
    {
        if (op.kind == IrOpKind::VmReg)
            useReg(vmRegOp(op));
    }

    void defRange(int start, int count)
    {
        if (count == -1)
        {
            defVarargs(start);
        }
        else
        {
            for (int i = start; i < start + count; i++)
                defReg(i);
        }
    }

    void useRange(int start, int count)
    {
        if (count == -1)
        {
            useVarargs(start);
        }
        else
        {
            for (int i = start; i < start + count; i++)
                useReg(i);
        }
    }

    // Required for a full visitor interface
    void capture(int reg) {}

    // Full clear of the tracked information
    void readAllRegs()
    {
        for (int i = 0; i <= maxReg; i++)
            useReg(i);

        hasGcoToClear = false;
    }

    bool hasRemainingUses(uint32_t instIdx)
    {
        IrInst& inst = function.instructions[instIdx];

        return anyArgumentMatch(
            inst,
            [&](IrOp op)
            {
                return op.kind == IrOpKind::Inst && remainingUses[op.index] != 0;
            }
        );
    }

    // Partial clear of information about registers that might contain a GC object
    // This is used by instructions that might perform a GC assist and GC needs all pointers to be pinned to stack
    void flushGcoRegs()
    {
        for (int i = 0; i <= maxReg; i++)
        {
            StoreRegInfo& regInfo = info[i];

            if (regInfo.maybeGco)
            {
                // If we happen to know the exact tag, it has to be a GCO, otherwise 'maybeGCO' should be false
                CODEGEN_ASSERT(regInfo.knownTag == kUnknownTag || isGCO(regInfo.knownTag));

                // If the values stored are still used and might be a GCO object, we have to pin in to the stack
                // And we have to pin all components of the register containing GCO
                bool tagUsedAfter = regInfo.tagInstIdx != ~0u && hasRemainingUses(regInfo.tagInstIdx);
                bool valueUsedAfter = regInfo.valueInstIdx != ~0u && hasRemainingUses(regInfo.valueInstIdx);
                bool tvalueUsedAfter = regInfo.tvalueInstIdx != ~0u && hasRemainingUses(regInfo.tvalueInstIdx);

                if (tagUsedAfter || valueUsedAfter || tvalueUsedAfter)
                {
                    regInfo.tagInstIdx = ~0u;
                    regInfo.valueInstIdx = ~0u;
                    regInfo.tvalueInstIdx = ~0u;
                }

                if (FFlag::LuauCodegenVmExitSync)
                {
                    // If the GCO values remain, they can no longer be propagated further as that will create a new use
                    // And we ensured there will be no more uses with 'hasRemainingUses' above
                    invalidateValuePropagation(regInfo);
                }

                // Indirect register read by GC doesn't clear the known tag
                regInfo.maybeGco = false;
            }
        }

        hasGcoToClear = false;
    }

    IrFunction& function;
    std::vector<uint32_t>& remainingUses;

    std::array<StoreRegInfo, 256> info;
    int maxReg = 255;

    // Some of the registers contain values which might be a GC object
    bool hasGcoToClear = false;

    // Have there been any object allocations which might remain unused
    bool hasAllocations = false;

    DenseHashSet<uint32_t> nonPropagatingStore{kInvalidInstIdx};
    std::vector<uint32_t> recordedVmExitSyncs;
};

static bool tryReplaceTagWithFullStore(
    RemoveDeadStoreState& state,
    IrBuilder& build,
    IrFunction& function,
    IrBlock& block,
    uint32_t instIndex,
    IrOp targetOp,
    IrOp tagOp,
    StoreRegInfo& regInfo
)
{
    uint8_t tag = function.tagOp(tagOp);

    // If the tag+value pair is established, we can mark both as dead and use a single split TValue store
    if (regInfo.tagInstIdx != ~0u && (regInfo.valueInstIdx != ~0u || regInfo.knownTag == LUA_TNIL))
    {
        // If the 'nil' is stored, we keep 'STORE_TAG Rn, tnil' as it writes the 'full' TValue
        // If a 'nil' tag is being replaced by something else, we also keep 'STORE_TAG Rn, tag', expecting a value store to follow
        // And value store has to follow, as the pre-DSO code would not allow GC to observe an incomplete stack variable
        if (tag != LUA_TNIL && regInfo.valueInstIdx != ~0u)
        {
            IrInst& prevValueInst = function.instructions[regInfo.valueInstIdx];

            if (prevValueInst.cmd == IrCmd::STORE_VECTOR)
            {
                CODEGEN_ASSERT(!HAS_OP_E(prevValueInst));
                IrOp prevValueX = OP_B(prevValueInst);
                IrOp prevValueY = OP_C(prevValueInst);
                IrOp prevValueZ = OP_D(prevValueInst);
                replace(function, block, instIndex, IrInst{IrCmd::STORE_VECTOR, {targetOp, prevValueX, prevValueY, prevValueZ, tagOp}});
            }
            else
            {
                IrOp prevValueOp = OP_B(prevValueInst);
                replace(function, block, instIndex, IrInst{IrCmd::STORE_SPLIT_TVALUE, {targetOp, tagOp, prevValueOp}});
            }
        }

        if (FFlag::LuauCodegenDseRestoreHints)
        {
            state.killTagAndValueStorePair(regInfo);
        }
        else
        {
            state.killTagStore(regInfo);
            state.killValueStore(regInfo);
        }

        regInfo.tvalueInstIdx = instIndex;
        regInfo.maybeGco = isGCO(tag);
        regInfo.knownTag = tag;
        state.hasGcoToClear |= regInfo.maybeGco;
        return true;
    }

    // We can also replace a dead split TValue store with a new one, while keeping the value the same
    if (regInfo.tvalueInstIdx != ~0u)
    {
        IrInst& prev = function.instructions[regInfo.tvalueInstIdx];

        if (prev.cmd == IrCmd::STORE_SPLIT_TVALUE)
        {
            CODEGEN_ASSERT(!HAS_OP_D(prev));

            // If the 'nil' is stored, we keep 'STORE_TAG Rn, tnil' as it writes the 'full' TValue
            if (tag != LUA_TNIL)
            {
                IrOp prevValueOp = OP_C(prev);
                replace(function, block, instIndex, IrInst{IrCmd::STORE_SPLIT_TVALUE, {targetOp, tagOp, prevValueOp}});
            }

            CODEGEN_ASSERT(regInfo.tagInstIdx == kInvalidInstIdx && regInfo.valueInstIdx == kInvalidInstIdx);
            state.killTValueStore(regInfo);

            regInfo.tvalueInstIdx = instIndex;
            regInfo.maybeGco = isGCO(tag);
            regInfo.knownTag = tag;
            state.hasGcoToClear |= regInfo.maybeGco;
            return true;
        }
        else if (prev.cmd == IrCmd::STORE_VECTOR)
        {
            // If the 'nil' is stored, we keep 'STORE_TAG Rn, tnil' as it writes the 'full' TValue
            if (tag != LUA_TNIL)
            {
                IrOp prevValueX = OP_B(prev);
                IrOp prevValueY = OP_C(prev);
                IrOp prevValueZ = OP_D(prev);
                replace(function, block, instIndex, IrInst{IrCmd::STORE_VECTOR, {targetOp, prevValueX, prevValueY, prevValueZ, tagOp}});
            }

            CODEGEN_ASSERT(regInfo.tagInstIdx == kInvalidInstIdx && regInfo.valueInstIdx == kInvalidInstIdx);
            state.killTValueStore(regInfo);

            regInfo.tvalueInstIdx = instIndex;
            regInfo.maybeGco = isGCO(tag);
            regInfo.knownTag = tag;
            state.hasGcoToClear |= regInfo.maybeGco;
            return true;
        }
    }

    return false;
}

static bool tryReplaceValueWithFullStore(
    RemoveDeadStoreState& state,
    IrBuilder& build,
    IrFunction& function,
    IrBlock& block,
    uint32_t instIndex,
    IrOp targetOp,
    IrOp valueOp,
    StoreRegInfo& regInfo
)
{
    // If the tag+value pair is established, we can mark both as dead and use a single split TValue store
    if (regInfo.tagInstIdx != ~0u && regInfo.valueInstIdx != ~0u)
    {
        IrOp prevTagOp = OP_B(function.instructions[regInfo.tagInstIdx]);
        uint8_t prevTag = function.tagOp(prevTagOp);

        CODEGEN_ASSERT(regInfo.knownTag == prevTag);
        replace(function, block, instIndex, IrInst{IrCmd::STORE_SPLIT_TVALUE, {targetOp, prevTagOp, valueOp}});

        if (FFlag::LuauCodegenDseRestoreHints)
        {
            state.killTagAndValueStorePair(regInfo);
        }
        else
        {
            state.killTagStore(regInfo);
            state.killValueStore(regInfo);
        }

        regInfo.tvalueInstIdx = instIndex;
        return true;
    }

    // We can also replace a dead split TValue store with a new one, while keeping the value the same
    if (regInfo.tvalueInstIdx != ~0u)
    {
        IrInst& prev = function.instructions[regInfo.tvalueInstIdx];

        if (prev.cmd == IrCmd::STORE_SPLIT_TVALUE)
        {
            IrOp prevTagOp = OP_B(prev);
            uint8_t prevTag = function.tagOp(prevTagOp);

            CODEGEN_ASSERT(regInfo.knownTag == prevTag);
            CODEGEN_ASSERT(!HAS_OP_D(prev));
            replace(function, block, instIndex, IrInst{IrCmd::STORE_SPLIT_TVALUE, {targetOp, prevTagOp, valueOp}});

            CODEGEN_ASSERT(regInfo.tagInstIdx == kInvalidInstIdx && regInfo.valueInstIdx == kInvalidInstIdx);
            state.killTValueStore(regInfo);

            regInfo.tvalueInstIdx = instIndex;
            return true;
        }
        else if (prev.cmd == IrCmd::STORE_VECTOR)
        {
            IrOp prevTagOp = OP_E(prev);
            CODEGEN_ASSERT(prevTagOp.kind != IrOpKind::None);
            uint8_t prevTag = function.tagOp(prevTagOp);

            CODEGEN_ASSERT(regInfo.knownTag == prevTag);
            replace(function, block, instIndex, IrInst{IrCmd::STORE_SPLIT_TVALUE, {targetOp, prevTagOp, valueOp}});

            CODEGEN_ASSERT(regInfo.tagInstIdx == kInvalidInstIdx && regInfo.valueInstIdx == kInvalidInstIdx);
            state.killTValueStore(regInfo);

            regInfo.tvalueInstIdx = instIndex;
            return true;
        }
        else if (prev.cmd == IrCmd::STORE_TVALUE && regInfo.knownTag != kUnknownTag && regInfo.tagInstIdx == kInvalidInstIdx)
        {
            IrOp prevTagOp = build.constTag(regInfo.knownTag);
            replace(function, block, instIndex, IrInst{IrCmd::STORE_SPLIT_TVALUE, {targetOp, prevTagOp, valueOp}});

            CODEGEN_ASSERT(regInfo.tagInstIdx == kInvalidInstIdx && regInfo.valueInstIdx == kInvalidInstIdx);
            state.killTValueStore(regInfo);

            regInfo.tvalueInstIdx = instIndex;
            return true;
        }
    }

    return false;
}

static bool tryReplaceVectorValueWithFullStore(
    RemoveDeadStoreState& state,
    IrBuilder& build,
    IrFunction& function,
    IrBlock& block,
    uint32_t instIndex,
    StoreRegInfo& regInfo
)
{
    // If the tag+value pair is established, we can mark both as dead and use a single split TValue store
    if (regInfo.tagInstIdx != ~0u && regInfo.valueInstIdx != ~0u)
    {
        IrOp prevTagOp = OP_B(function.instructions[regInfo.tagInstIdx]);
        uint8_t prevTag = function.tagOp(prevTagOp);

        CODEGEN_ASSERT(regInfo.knownTag == prevTag);

        IrInst& storeInst = function.instructions[instIndex];
        CODEGEN_ASSERT(storeInst.cmd == IrCmd::STORE_VECTOR);

        if (!HAS_OP_E(storeInst))
            storeInst.ops.push_back({});

        replace(function, OP_E(storeInst), prevTagOp);

        if (FFlag::LuauCodegenDseRestoreHints)
        {
            state.killTagAndValueStorePair(regInfo);
        }
        else
        {
            state.killTagStore(regInfo);
            state.killValueStore(regInfo);
        }

        regInfo.tvalueInstIdx = instIndex;
        return true;
    }

    // We can also replace a dead split TValue store with a new one, while keeping the value the same
    if (regInfo.tvalueInstIdx != ~0u)
    {
        IrInst& prev = function.instructions[regInfo.tvalueInstIdx];

        if (prev.cmd == IrCmd::STORE_SPLIT_TVALUE)
        {
            IrOp prevTagOp = OP_B(prev);
            uint8_t prevTag = function.tagOp(prevTagOp);

            CODEGEN_ASSERT(regInfo.knownTag == prevTag);
            CODEGEN_ASSERT(!HAS_OP_D(prev));

            IrInst& storeInst = function.instructions[instIndex];
            CODEGEN_ASSERT(storeInst.cmd == IrCmd::STORE_VECTOR);

            if (!HAS_OP_E(storeInst))
                storeInst.ops.push_back({});

            replace(function, OP_E(storeInst), prevTagOp);

            CODEGEN_ASSERT(regInfo.tagInstIdx == kInvalidInstIdx && regInfo.valueInstIdx == kInvalidInstIdx);
            state.killTValueStore(regInfo);

            regInfo.tvalueInstIdx = instIndex;
            return true;
        }
        else if (prev.cmd == IrCmd::STORE_VECTOR)
        {
            IrOp prevTagOp = OP_E(prev);
            CODEGEN_ASSERT(prevTagOp.kind != IrOpKind::None);
            uint8_t prevTag = function.tagOp(prevTagOp);

            CODEGEN_ASSERT(regInfo.knownTag == prevTag);

            IrInst& storeInst = function.instructions[instIndex];
            CODEGEN_ASSERT(storeInst.cmd == IrCmd::STORE_VECTOR);

            if (!HAS_OP_E(storeInst))
                storeInst.ops.push_back({});

            replace(function, OP_E(storeInst), prevTagOp);

            CODEGEN_ASSERT(regInfo.tagInstIdx == kInvalidInstIdx && regInfo.valueInstIdx == kInvalidInstIdx);
            state.killTValueStore(regInfo);

            regInfo.tvalueInstIdx = instIndex;
            return true;
        }
    }

    return false;
}

static void updateRemainingUses(RemoveDeadStoreState& state, IrInst& inst, uint32_t index)
{
    state.remainingUses[index] = inst.useCount;

    visitArguments(
        inst,
        [&](IrOp op)
        {
            if (op.kind == IrOpKind::Inst)
            {
                CODEGEN_ASSERT(state.remainingUses[op.index] != 0);
                state.remainingUses[op.index]--;
            }
        }
    );
}

static void markDeadStoresInInst(RemoveDeadStoreState& state, IrBuilder& build, IrFunction& function, IrBlock& block, IrInst& inst, uint32_t index)
{
    updateRemainingUses(state, inst, index);

    switch (inst.cmd)
    {
    case IrCmd::STORE_TAG:
        if (OP_A(inst).kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(OP_A(inst));

            if (function.cfg.captured.regs.test(reg))
                return;

            StoreRegInfo& regInfo = state.info[reg];

            regInfo.ignoreAtExit = false;

            if (tryReplaceTagWithFullStore(state, build, function, block, index, OP_A(inst), OP_B(inst), regInfo))
                break;

            uint8_t tag = function.tagOp(OP_B(inst));

            regInfo.tagInstIdx = index;

            if (state.tagValuePairEstablished(regInfo))
            {
                if (tag == LUA_TNIL)
                    regInfo.valueInstIdx = kInvalidInstIdx;

                regInfo.tvalueInstIdx = kInvalidInstIdx;
            }

            regInfo.maybeGco = isGCO(tag);
            regInfo.knownTag = tag;
            state.hasGcoToClear |= regInfo.maybeGco;
        }
        break;
    case IrCmd::STORE_EXTRA:
        // To simplify, extra field store is preserved along with all other stores made so far
        if (OP_A(inst).kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(OP_A(inst));

            state.useReg(reg);

            StoreRegInfo& regInfo = state.info[reg];

            regInfo.ignoreAtExit = false;
        }
        break;
    case IrCmd::STORE_POINTER:
        if (OP_A(inst).kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(OP_A(inst));

            if (function.cfg.captured.regs.test(reg))
                return;

            StoreRegInfo& regInfo = state.info[reg];

            regInfo.ignoreAtExit = false;

            bool maybeGco;

            if (FFlag::LuauCodegenDsePtrStoreTagCheck)
            {
                // If we have a known tag and it is not a pointer, we cannot generate a full store in invalid form
                maybeGco = regInfo.knownTag == kUnknownTag || isGCO(regInfo.knownTag);

                if (maybeGco && tryReplaceValueWithFullStore(state, build, function, block, index, OP_A(inst), OP_B(inst), regInfo))
                {
                    regInfo.maybeGco = true;
                    state.hasGcoToClear = true;
                    break;
                }
            }
            else
            {
                if (tryReplaceValueWithFullStore(state, build, function, block, index, OP_A(inst), OP_B(inst), regInfo))
                {
                    regInfo.maybeGco = true;
                    state.hasGcoToClear |= true;
                    break;
                }
            }

            // Partial value store can be removed by a new one if the tag is known
            if (regInfo.knownTag != kUnknownTag)
                state.killValueStore(regInfo);

            regInfo.valueInstIdx = index;

            if (state.tagValuePairEstablished(regInfo))
                regInfo.tvalueInstIdx = kInvalidInstIdx;

            if (FFlag::LuauCodegenDsePtrStoreTagCheck)
            {
                // While pointer was stored, TValue can still be under a non-GCO tag
                regInfo.maybeGco = maybeGco;
                state.hasGcoToClear |= maybeGco;
            }
            else
            {
                regInfo.maybeGco = true;
                state.hasGcoToClear = true;
            }
        }
        break;
    case IrCmd::STORE_DOUBLE:
    case IrCmd::STORE_INT64:
    case IrCmd::STORE_INT:
        if (OP_A(inst).kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(OP_A(inst));

            if (function.cfg.captured.regs.test(reg))
                return;

            StoreRegInfo& regInfo = state.info[reg];

            regInfo.ignoreAtExit = false;

            if (tryReplaceValueWithFullStore(state, build, function, block, index, OP_A(inst), OP_B(inst), regInfo))
                break;

            // Partial value store can be removed by a new one if the tag is known
            if (regInfo.knownTag != kUnknownTag)
                state.killValueStore(regInfo);

            regInfo.valueInstIdx = index;

            if (state.tagValuePairEstablished(regInfo))
                regInfo.tvalueInstIdx = kInvalidInstIdx;

            regInfo.maybeGco = false;
        }
        break;
    case IrCmd::STORE_VECTOR:
        if (OP_A(inst).kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(OP_A(inst));

            if (function.cfg.captured.regs.test(reg))
                return;

            StoreRegInfo& regInfo = state.info[reg];

            regInfo.ignoreAtExit = false;

            if (tryReplaceVectorValueWithFullStore(state, build, function, block, index, regInfo))
                break;

            // Partial value store can be removed by a new one if the tag is known
            if (regInfo.knownTag != kUnknownTag)
                state.killValueStore(regInfo);

            regInfo.valueInstIdx = index;

            if (state.tagValuePairEstablished(regInfo))
                regInfo.tvalueInstIdx = kInvalidInstIdx;

            regInfo.maybeGco = false;
        }
        break;
    case IrCmd::STORE_TVALUE:
        if (OP_A(inst).kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(OP_A(inst));

            if (function.cfg.captured.regs.test(reg))
                return;

            StoreRegInfo& regInfo = state.info[reg];

            regInfo.ignoreAtExit = false;

            state.killTagAndValueStorePair(regInfo);
            state.killTValueStore(regInfo);

            regInfo.tagInstIdx = kInvalidInstIdx;
            regInfo.valueInstIdx = kInvalidInstIdx;

            regInfo.tvalueInstIdx = index;

            regInfo.knownTag = tryGetOperandTag(function, OP_B(inst)).value_or(kUnknownTag);
            regInfo.maybeGco = regInfo.knownTag == kUnknownTag || isGCO(regInfo.knownTag);

            state.hasGcoToClear |= regInfo.maybeGco;
        }
        break;
    case IrCmd::STORE_SPLIT_TVALUE:
        if (OP_A(inst).kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(OP_A(inst));

            if (function.cfg.captured.regs.test(reg))
                return;

            StoreRegInfo& regInfo = state.info[reg];

            regInfo.ignoreAtExit = false;

            state.killTagAndValueStorePair(regInfo);
            state.killTValueStore(regInfo);

            regInfo.tagInstIdx = kInvalidInstIdx;
            regInfo.valueInstIdx = kInvalidInstIdx;

            regInfo.tvalueInstIdx = index;
            regInfo.maybeGco = isGCO(function.tagOp(OP_B(inst)));
            regInfo.knownTag = function.tagOp(OP_B(inst));
            state.hasGcoToClear |= regInfo.maybeGco;
        }
        break;

        // Guard checks can jump to a block which might be using some or all the values we stored
    case IrCmd::CHECK_TAG:
        state.checkLiveIns(OP_C(inst), index, true);

        // Tag guard establishes the tag value of the register in the current block
        if (IrInst* load = function.asInstOp(OP_A(inst)); load && load->cmd == IrCmd::LOAD_TAG && OP_A(load).kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(OP_A(load));

            StoreRegInfo& regInfo = state.info[reg];

            regInfo.knownTag = function.tagOp(OP_B(inst));
        }
        break;
    case IrCmd::TRY_NUM_TO_INDEX:
        state.checkLiveIns(OP_B(inst), index, true);
        break;
    case IrCmd::TRY_CALL_FASTGETTM:
        state.checkLiveIns(OP_C(inst), index, true);
        break;
    case IrCmd::CHECK_FASTCALL_RES:
        state.checkLiveIns(OP_B(inst), index, true);
        break;
    case IrCmd::CHECK_TRUTHY:
        // This instruction has two jumps to the exit in the lowering and that prevents exit sync record from being generated
        state.checkLiveIns(OP_C(inst), index, false);
        break;
    case IrCmd::CHECK_READONLY:
        state.checkLiveIns(OP_B(inst), index, true);
        break;
    case IrCmd::CHECK_NO_METATABLE:
        state.checkLiveIns(OP_B(inst), index, true);
        break;
    case IrCmd::CHECK_SAFE_ENV:
        state.checkLiveIns(OP_A(inst), index, true);
        break;
    case IrCmd::CHECK_ARRAY_SIZE:
        state.checkLiveIns(OP_C(inst), index, true);
        break;
    case IrCmd::CHECK_DIV_INT64:
        // This instruction has two jumps to the exit in the lowering and that prevents exit sync record from being generated
        state.checkLiveIns(OP_C(inst), index, false);
        break;
    case IrCmd::CHECK_SLOT_MATCH:
        state.checkLiveIns(OP_C(inst), index, true);
        break;
    case IrCmd::CHECK_NODE_NO_NEXT:
        state.checkLiveIns(OP_B(inst), index, true);
        break;
    case IrCmd::CHECK_NODE_VALUE:
        state.checkLiveIns(OP_B(inst), index, true);
        break;
    case IrCmd::CHECK_BUFFER_LEN:
        state.checkLiveIns(OP_F(inst), index, true);
        break;
    case IrCmd::CHECK_USERDATA_TAG:
        state.checkLiveIns(OP_C(inst), index, true);
        break;
    case IrCmd::CHECK_CMP_NUM:
    case IrCmd::CHECK_CMP_INT:
    case IrCmd::CHECK_CMP_INT64:
        state.checkLiveIns(OP_D(inst), index, true);
        break;

    case IrCmd::JUMP_IF_TRUTHY:
    case IrCmd::JUMP_IF_FALSY:
    case IrCmd::JUMP_EQ_TAG:
    case IrCmd::JUMP_CMP_INT:
    case IrCmd::JUMP_EQ_POINTER:
    case IrCmd::JUMP_CMP_NUM:
    case IrCmd::JUMP_CMP_FLOAT:
    case IrCmd::JUMP_FORN_LOOP_COND:
    case IrCmd::JUMP_SLOT_MATCH:
    case IrCmd::JUMP_CMP_PROTOID:
        visitVmRegDefsUses(state, function, inst);

        state.checkLiveOuts(block);
        break;

    case IrCmd::JUMP:
        // Ideally, we would be able to remove stores to registers that are not live out from a block
        // But during chain optimizations, we rely on data stored in the predecessor even when it's not an explicit live out
        break;
    case IrCmd::RETURN:
        visitVmRegDefsUses(state, function, inst);

        // At the end of a function, we can kill stores to registers that are not live out
        state.checkLiveOuts(block);
        break;
    case IrCmd::ADJUST_STACK_TO_REG:
        // visitVmRegDefsUses considers adjustment as the fast call register definition point, but for dead store removal, we count the actual writes
        break;

        // This group of instructions can trigger GC assist internally
        // For GC to work correctly, all values containing a GCO have to be stored on stack - otherwise a live reference might be missed
    case IrCmd::CMP_ANY:
    case IrCmd::DO_ARITH:
    case IrCmd::DO_LEN:
    case IrCmd::GET_TABLE:
    case IrCmd::SET_TABLE:
    case IrCmd::GET_CACHED_IMPORT:
    case IrCmd::CONCAT:
    case IrCmd::INTERRUPT:
    case IrCmd::CHECK_GC:
    case IrCmd::CALL:
    case IrCmd::FORGLOOP_FALLBACK:
    case IrCmd::FALLBACK_GETGLOBAL:
    case IrCmd::FALLBACK_SETGLOBAL:
    case IrCmd::FALLBACK_GETTABLEKS:
    case IrCmd::FALLBACK_SETTABLEKS:
    case IrCmd::FALLBACK_NAMECALL:
    case IrCmd::FALLBACK_DUPCLOSURE:
    case IrCmd::FALLBACK_FORGPREP:
        if (state.hasGcoToClear)
            state.flushGcoRegs();

        visitVmRegDefsUses(state, function, inst);
        break;

    case IrCmd::NEW_USERDATA:
        state.hasAllocations = true;
        break;

    case IrCmd::MARK_DEAD:
        state.markUnusedAtExit(vmRegOp(OP_A(inst)), function.intOp(OP_B(inst)));
        break;

    default:
        // Guards have to be covered explicitly
        CODEGEN_ASSERT(!isNonTerminatingJump(inst.cmd));

        visitVmRegDefsUses(state, function, inst);
        break;
    }

    if (FFlag::LuauCodegenVmExitSync)
    {
        // Pending stores with SSA operands must not be deferred to ExitSync blocks past instructions that can invalidate operand physical location
        switch (inst.cmd)
        {
            // These instructions can perform an indirect Luau function call through metamethods
            // Creating new native execution frames can invalidate shared extended spill area
        case IrCmd::CMP_ANY:
        case IrCmd::DO_ARITH:
        case IrCmd::DO_LEN:
        case IrCmd::GET_TABLE:
        case IrCmd::SET_TABLE:
        case IrCmd::CONCAT:
        case IrCmd::GET_CACHED_IMPORT:
        case IrCmd::FORGLOOP_FALLBACK:
        case IrCmd::FALLBACK_GETGLOBAL:
        case IrCmd::FALLBACK_SETGLOBAL:
        case IrCmd::FALLBACK_GETTABLEKS:
        case IrCmd::FALLBACK_SETTABLEKS:
        case IrCmd::FALLBACK_NAMECALL:
        case IrCmd::FALLBACK_DUPCLOSURE:
        case IrCmd::FALLBACK_FORGPREP:
            // CALL directly executes a Luau function on the same native stack frame
        case IrCmd::CALL:
            // These instructions use lowering that is not aware of register allocator and demand no active values to exist
        case IrCmd::SETLIST:
        case IrCmd::FORGLOOP:
            state.invalidateValuePropagation();
            break;
        default:
            break;
        }
    }
}

static void markDeadStoresInBlock(IrBuilder& build, IrBlock& block, RemoveDeadStoreState& state)
{
    IrFunction& function = build.function;

    // Block might establish a safe environment right at the start and might take a VM exit
    if ((block.flags & kBlockFlagSafeEnvCheck) != 0)
        state.readAllRegs();

    for (uint32_t index = block.start; index <= block.finish; index++)
    {
        CODEGEN_ASSERT(index < function.instructions.size());
        IrInst& inst = function.instructions[index];

        markDeadStoresInInst(state, build, function, block, inst, index);
    }
}

static void setupBlockEntryState(const IrFunction& function, const IrBlock& block, RemoveDeadStoreState& state)
{
    propagateTagsFromPredecessors(
        function,
        block,
        [&](size_t i)
        {
            return state.info[i].knownTag;
        },
        [&](size_t i, uint8_t tag)
        {
            state.info[i].knownTag = tag;
        }
    );
}

static void markDeadStoresInBlockChain(
    IrBuilder& build,
    std::vector<uint8_t>& visited,
    std::vector<uint32_t>& remainingUses,
    std::vector<uint32_t>& blockIdxChain,
    std::vector<uint32_t>& allRecordedVmExitSyncs,
    IrBlock* block
)
{
    IrFunction& function = build.function;

    RemoveDeadStoreState state{function, remainingUses};

    // We will be visiting this chain a few times to clean unreferenced temporaries
    // Clear the storage we reuse
    blockIdxChain.clear();

    setupBlockEntryState(function, *block, state);

    while (block)
    {
        uint32_t blockIdx = function.getBlockIndex(*block);
        CODEGEN_ASSERT(!visited[blockIdx]);
        visited[blockIdx] = true;

        blockIdxChain.push_back(blockIdx);

        markDeadStoresInBlock(build, *block, state);

        IrInst& termInst = function.instructions[block->finish];

        IrBlock* nextBlock = nullptr;

        // Unconditional jump into a block with a single user (current block) allows us to continue optimization
        // with the information we have gathered so far (unless we have already visited that block earlier)
        if (termInst.cmd == IrCmd::JUMP && OP_A(termInst).kind == IrOpKind::Block)
        {
            IrBlock& target = function.blockOp(OP_A(termInst));
            uint32_t targetIdx = function.getBlockIndex(target);

            if (target.useCount == 1 && !visited[targetIdx] && target.kind != IrBlockKind::Fallback)
            {
                // If this block isn't glued to the target in the lowering order, we cannot capture any remaining stores from it in ExitSync blocks
                if (FFlag::LuauCodegenVmExitSyncFix && block->expectedNextBlock != targetIdx)
                    state.invalidateValuePropagation();

                nextBlock = &target;
            }
        }

        block = nextBlock;
    }

    if (FFlag::LuauCodegenVmExitSync)
    {
        state.pruneVmExitInfo();

        allRecordedVmExitSyncs.insert(allRecordedVmExitSyncs.end(), state.recordedVmExitSyncs.begin(), state.recordedVmExitSyncs.end());
    }

    // If there are allocating instructions, check if they have 'read' uses after DSE
    if (state.hasAllocations)
    {
        bool foundUnused = false;

        // Remove uses in instructions writing to the allocations
        for (uint32_t blockIdx : blockIdxChain)
        {
            IrBlock& block = function.blocks[blockIdx];

            for (uint32_t index = block.start; index <= block.finish; index++)
            {
                IrInst& inst = function.instructions[index];

                state.remainingUses[index] = inst.useCount;

                switch (inst.cmd)
                {
                case IrCmd::BUFFER_WRITEI8:
                case IrCmd::BUFFER_WRITEI16:
                case IrCmd::BUFFER_WRITEI32:
                case IrCmd::BUFFER_WRITEI64:
                case IrCmd::BUFFER_WRITEF32:
                case IrCmd::BUFFER_WRITEF64:
                    state.remainingUses[OP_A(inst).index]--;

                    if (state.remainingUses[OP_A(inst).index] == 0)
                        foundUnused = true;
                    break;
                default:
                    break;
                }
            }
        }

        // Remove those write instructions if they were the only users of the allocation
        if (foundUnused)
        {
            for (uint32_t blockIdx : blockIdxChain)
            {
                IrBlock& block = function.blocks[blockIdx];

                for (uint32_t index = block.start; index <= block.finish; index++)
                {
                    IrInst& inst = function.instructions[index];

                    switch (inst.cmd)
                    {
                    case IrCmd::BUFFER_WRITEI8:
                    case IrCmd::BUFFER_WRITEI16:
                    case IrCmd::BUFFER_WRITEI32:
                    case IrCmd::BUFFER_WRITEI64:
                    case IrCmd::BUFFER_WRITEF32:
                    case IrCmd::BUFFER_WRITEF64:
                        if (state.remainingUses[OP_A(inst).index] == 0)
                        {
                            IrInst& pointer = function.instOp(OP_A(inst));

                            if (pointer.cmd == IrCmd::NEW_USERDATA)
                                kill(function, inst);
                        }
                        break;
                    default:
                        break;
                    }
                }
            }
        }
    }
}

static void generateVmExitBlocks(IrBuilder& build, const std::vector<uint32_t>& recordedVmExitSyncs)
{
    IrFunction& function = build.function;

    for (uint32_t vmExitSyncLocation : recordedVmExitSyncs)
    {
        VmExitSyncInfo& syncInfo = function.vmExitInfo[vmExitSyncLocation];

        if (syncInfo.regStores.empty())
            continue;

        // We will be collecting instructions we want to move into the VM exit in reverse order
        SmallVector<IrInst, 8> storeInstructions;
        SmallVector<uint32_t, 8> argInstructions;

        std::vector<std::pair<IrOp, uint32_t>> inputs;

        auto visitor = [&](IrOp op)
        {
            if (op.kind == IrOpKind::Inst)
            {
                if (auto it = std::find_if(
                        inputs.begin(),
                        inputs.end(),
                        [&](auto&& el)
                        {
                            return el.first == op;
                        }
                    );
                    it != inputs.end())
                    it->second++;
                else
                    inputs.emplace_back(op, 1u);
            }
        };

        // Start with the store instruction we got
        for (auto& regStore : syncInfo.regStores)
        {
            for (auto& record : regStore.stores)
            {
                storeInstructions.push_back(record.backup);
                visitArguments(record.backup, visitor);
            }
        }

        // For each input we got, see if we are the only user of it and if we are (and it has no side effects), schedule a move inside
        for (size_t i = 0; i < inputs.size();)
        {
            IrInst& inst = function.instOp(inputs[i].first);

            if (inst.useCount == inputs[i].second && !hasSideEffects(inst.cmd) && !isUnsafeToSink(inst.cmd))
            {
                uint32_t instIdx = function.getInstIndex(inst);
                argInstructions.push_back(instIdx);

                inputs.erase(inputs.begin() + i); // Delete this input

                visitArguments(function.instructions[instIdx], visitor);
            }
            else
            {
                i++;
            }
        }

        for (auto input : inputs)
            syncInfo.argOps.push_back(input.first);

        // We now should have an extracted instruction chain with no side effects in reverse order
        syncInfo.block = build.block(IrBlockKind::ExitSync);
        function.blockToVmExitMap[syncInfo.block.index] = vmExitSyncLocation;
        build.beginBlock(syncInfo.block);

        DenseHashMap<uint32_t, uint32_t> instRedir{~0u};

        auto redirect = [&instRedir, &inputs](IrOp& op)
        {
            if (op.kind == IrOpKind::Inst)
            {
                if (const uint32_t* newIndex = instRedir.find(op.index))
                    op.index = *newIndex;
                else if (std::find_if(
                             inputs.begin(),
                             inputs.end(),
                             [op](auto& el)
                             {
                                 return el.first == op;
                             }
                         ) == inputs.end())
                    CODEGEN_ASSERT(!"Values can only be used if they are defined in the same block or be an input");
            }
        };

        for (int i = int(argInstructions.size()) - 1; i >= 0; i--)
        {
            uint32_t instIdx = argInstructions[i];

            CODEGEN_ASSERT(instIdx < function.instructions.size());
            IrInst clone = function.instructions[instIdx];

            for (auto& op : clone.ops)
                redirect(op);

            for (auto& op : clone.ops)
                addUse(function, op);

            // Instructions that referenced the original will have to be adjusted to use the clone
            instRedir[instIdx] = uint32_t(function.instructions.size());

            // Reconstruct the fresh clone
            build.inst(clone.cmd, clone.ops);
        }

        for (IrInst& storeInstruction : storeInstructions)
        {
            IrInst clone = storeInstruction;

            for (auto& op : clone.ops)
                redirect(op);

            for (auto& op : clone.ops)
                addUse(function, op);

            // Reconstruct the fresh clone
            build.inst(clone.cmd, clone.ops);

            visitArguments(
                storeInstruction,
                [&](IrOp op)
                {
                    removeUse(function, op);
                }
            );
        }

        build.inst(IrCmd::JUMP, syncInfo.vmExit);

        // Replace guard VM exit with an exit sync block
        IrInst& guardInst = function.instructions[vmExitSyncLocation];

        for (auto& op : guardInst.ops)
        {
            if (op.kind == IrOpKind::VmExit && op == syncInfo.vmExit)
            {
                replace(function, op, syncInfo.block);
                break;
            }
        }
    }
}

void markDeadStoresInBlockChains(IrBuilder& build)
{
    IrFunction& function = build.function;

    std::vector<uint8_t> visited(function.blocks.size(), false);
    std::vector<uint32_t> remainingUses(function.instructions.size(), 0u);
    std::vector<uint32_t> blockIdxChain;
    std::vector<uint32_t> recordedVmExitSyncs;

    for (IrBlock& block : function.blocks)
    {
        if (block.kind == IrBlockKind::Fallback || block.kind == IrBlockKind::Dead)
            continue;

        if (visited[function.getBlockIndex(block)])
            continue;

        markDeadStoresInBlockChain(build, visited, remainingUses, blockIdxChain, recordedVmExitSyncs, &block);
    }

    if (FFlag::LuauCodegenVmExitSync)
        generateVmExitBlocks(build, recordedVmExitSyncs);
}

} // namespace CodeGen
} // namespace Luau
