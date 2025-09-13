// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/OptimizeDeadStore.h"

#include "Luau/IrBuilder.h"
#include "Luau/IrVisitUseDef.h"
#include "Luau/IrUtils.h"

#include <array>

#include "lobject.h"

// TODO: optimization can be improved by knowing which registers are live in at each VM exit

namespace Luau
{
namespace CodeGen
{

constexpr uint8_t kUnknownTag = 0xff;

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

    // Knowing the last stored tag can help safely remove additional unused partial stores
    uint8_t knownTag = kUnknownTag;
};

struct RemoveDeadStoreState
{
    RemoveDeadStoreState(IrFunction& function)
        : function(function)
    {
        maxReg = function.proto ? function.proto->maxstacksize : 255;
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

    void killTagAndValueStorePair(StoreRegInfo& regInfo)
    {
        bool tagEstablished = regInfo.tagInstIdx != ~0u || regInfo.knownTag != kUnknownTag;

        // When tag is 'nil', we don't need to remove the unused value store
        bool valueEstablished = regInfo.valueInstIdx != ~0u || regInfo.knownTag == LUA_TNIL;

        // Partial stores can only be removed if the whole pair is established
        if (tagEstablished && valueEstablished)
        {
            if (regInfo.tagInstIdx != ~0u)
            {
                kill(function, function.instructions[regInfo.tagInstIdx]);
                regInfo.tagInstIdx = ~0u;
            }

            if (regInfo.valueInstIdx != ~0u)
            {
                kill(function, function.instructions[regInfo.valueInstIdx]);
                regInfo.valueInstIdx = ~0u;
            }

            regInfo.maybeGco = false;
        }
    }

    void killTValueStore(StoreRegInfo& regInfo)
    {
        if (regInfo.tvalueInstIdx != ~0u)
        {
            kill(function, function.instructions[regInfo.tvalueInstIdx]);

            regInfo.tvalueInstIdx = ~0u;
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

        // Opaque register definition removes the knowledge of the actual tag value
        regInfo.knownTag = kUnknownTag;
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

    // When checking control flow, such as exit to fallback blocks:
    // For VM exits, we keep all stores because we don't have information on what registers are live at the start of the VM assist
    // For regular blocks, we check which registers are expected to be live at entry (if we have CFG information available)
    void checkLiveIns(IrOp op)
    {
        if (op.kind == IrOpKind::VmExit)
        {
            readAllRegs();
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

                // Indirect register read by GC doesn't clear the known tag
                regInfo.tagInstIdx = ~0u;
                regInfo.valueInstIdx = ~0u;
                regInfo.tvalueInstIdx = ~0u;
                regInfo.maybeGco = false;
            }
        }

        hasGcoToClear = false;
    }

    IrFunction& function;

    std::array<StoreRegInfo, 256> info;
    int maxReg = 255;

    // Some of the registers contain values which might be a GC object
    bool hasGcoToClear = false;
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
                CODEGEN_ASSERT(prevValueInst.e.kind == IrOpKind::None);
                IrOp prevValueX = prevValueInst.b;
                IrOp prevValueY = prevValueInst.c;
                IrOp prevValueZ = prevValueInst.d;
                replace(function, block, instIndex, IrInst{IrCmd::STORE_VECTOR, targetOp, prevValueX, prevValueY, prevValueZ, tagOp});
            }
            else
            {
                IrOp prevValueOp = prevValueInst.b;
                replace(function, block, instIndex, IrInst{IrCmd::STORE_SPLIT_TVALUE, targetOp, tagOp, prevValueOp});
            }
        }

        state.killTagStore(regInfo);
        state.killValueStore(regInfo);

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
            CODEGEN_ASSERT(prev.d.kind == IrOpKind::None);

            // If the 'nil' is stored, we keep 'STORE_TAG Rn, tnil' as it writes the 'full' TValue
            if (tag != LUA_TNIL)
            {
                IrOp prevValueOp = prev.c;
                replace(function, block, instIndex, IrInst{IrCmd::STORE_SPLIT_TVALUE, targetOp, tagOp, prevValueOp});
            }

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
                IrOp prevValueX = prev.b;
                IrOp prevValueY = prev.c;
                IrOp prevValueZ = prev.d;
                replace(function, block, instIndex, IrInst{IrCmd::STORE_VECTOR, targetOp, prevValueX, prevValueY, prevValueZ, tagOp});
            }

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
        IrOp prevTagOp = function.instructions[regInfo.tagInstIdx].b;
        uint8_t prevTag = function.tagOp(prevTagOp);

        CODEGEN_ASSERT(regInfo.knownTag == prevTag);
        replace(function, block, instIndex, IrInst{IrCmd::STORE_SPLIT_TVALUE, targetOp, prevTagOp, valueOp});

        state.killTagStore(regInfo);
        state.killValueStore(regInfo);

        regInfo.tvalueInstIdx = instIndex;
        return true;
    }

    // We can also replace a dead split TValue store with a new one, while keeping the value the same
    if (regInfo.tvalueInstIdx != ~0u)
    {
        IrInst& prev = function.instructions[regInfo.tvalueInstIdx];

        if (prev.cmd == IrCmd::STORE_SPLIT_TVALUE)
        {
            IrOp prevTagOp = prev.b;
            uint8_t prevTag = function.tagOp(prevTagOp);

            CODEGEN_ASSERT(regInfo.knownTag == prevTag);
            CODEGEN_ASSERT(prev.d.kind == IrOpKind::None);
            replace(function, block, instIndex, IrInst{IrCmd::STORE_SPLIT_TVALUE, targetOp, prevTagOp, valueOp});

            state.killTValueStore(regInfo);

            regInfo.tvalueInstIdx = instIndex;
            return true;
        }
        else if (prev.cmd == IrCmd::STORE_VECTOR)
        {
            IrOp prevTagOp = prev.e;
            CODEGEN_ASSERT(prevTagOp.kind != IrOpKind::None);
            uint8_t prevTag = function.tagOp(prevTagOp);

            CODEGEN_ASSERT(regInfo.knownTag == prevTag);
            replace(function, block, instIndex, IrInst{IrCmd::STORE_SPLIT_TVALUE, targetOp, prevTagOp, valueOp});

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
        IrOp prevTagOp = function.instructions[regInfo.tagInstIdx].b;
        uint8_t prevTag = function.tagOp(prevTagOp);

        CODEGEN_ASSERT(regInfo.knownTag == prevTag);

        IrInst& storeInst = function.instructions[instIndex];
        CODEGEN_ASSERT(storeInst.cmd == IrCmd::STORE_VECTOR);
        replace(function, storeInst.e, prevTagOp);

        state.killTagStore(regInfo);
        state.killValueStore(regInfo);

        regInfo.tvalueInstIdx = instIndex;
        return true;
    }

    // We can also replace a dead split TValue store with a new one, while keeping the value the same
    if (regInfo.tvalueInstIdx != ~0u)
    {
        IrInst& prev = function.instructions[regInfo.tvalueInstIdx];

        if (prev.cmd == IrCmd::STORE_SPLIT_TVALUE)
        {
            IrOp prevTagOp = prev.b;
            uint8_t prevTag = function.tagOp(prevTagOp);

            CODEGEN_ASSERT(regInfo.knownTag == prevTag);
            CODEGEN_ASSERT(prev.d.kind == IrOpKind::None);

            IrInst& storeInst = function.instructions[instIndex];
            CODEGEN_ASSERT(storeInst.cmd == IrCmd::STORE_VECTOR);
            replace(function, storeInst.e, prevTagOp);

            state.killTValueStore(regInfo);

            regInfo.tvalueInstIdx = instIndex;
            return true;
        }
        else if (prev.cmd == IrCmd::STORE_VECTOR)
        {
            IrOp prevTagOp = prev.e;
            CODEGEN_ASSERT(prevTagOp.kind != IrOpKind::None);
            uint8_t prevTag = function.tagOp(prevTagOp);

            CODEGEN_ASSERT(regInfo.knownTag == prevTag);

            IrInst& storeInst = function.instructions[instIndex];
            CODEGEN_ASSERT(storeInst.cmd == IrCmd::STORE_VECTOR);
            replace(function, storeInst.e, prevTagOp);

            state.killTValueStore(regInfo);

            regInfo.tvalueInstIdx = instIndex;
            return true;
        }
    }

    return false;
}

static void markDeadStoresInInst(RemoveDeadStoreState& state, IrBuilder& build, IrFunction& function, IrBlock& block, IrInst& inst, uint32_t index)
{
    switch (inst.cmd)
    {
    case IrCmd::STORE_TAG:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(inst.a);

            if (function.cfg.captured.regs.test(reg))
                return;

            StoreRegInfo& regInfo = state.info[reg];

            if (tryReplaceTagWithFullStore(state, build, function, block, index, inst.a, inst.b, regInfo))
                break;

            uint8_t tag = function.tagOp(inst.b);

            regInfo.tagInstIdx = index;
            regInfo.maybeGco = isGCO(tag);
            regInfo.knownTag = tag;
            state.hasGcoToClear |= regInfo.maybeGco;
        }
        break;
    case IrCmd::STORE_EXTRA:
        // To simplify, extra field store is preserved along with all other stores made so far
        if (inst.a.kind == IrOpKind::VmReg)
        {
            state.useReg(vmRegOp(inst.a));
        }
        break;
    case IrCmd::STORE_POINTER:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(inst.a);

            if (function.cfg.captured.regs.test(reg))
                return;

            StoreRegInfo& regInfo = state.info[reg];

            if (tryReplaceValueWithFullStore(state, build, function, block, index, inst.a, inst.b, regInfo))
            {
                regInfo.maybeGco = true;
                state.hasGcoToClear |= true;
                break;
            }

            // Partial value store can be removed by a new one if the tag is known
            if (regInfo.knownTag != kUnknownTag)
                state.killValueStore(regInfo);

            regInfo.valueInstIdx = index;
            regInfo.maybeGco = true;
            state.hasGcoToClear = true;
        }
        break;
    case IrCmd::STORE_DOUBLE:
    case IrCmd::STORE_INT:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(inst.a);

            if (function.cfg.captured.regs.test(reg))
                return;

            StoreRegInfo& regInfo = state.info[reg];

            if (tryReplaceValueWithFullStore(state, build, function, block, index, inst.a, inst.b, regInfo))
                break;

            // Partial value store can be removed by a new one if the tag is known
            if (regInfo.knownTag != kUnknownTag)
                state.killValueStore(regInfo);

            regInfo.valueInstIdx = index;
            regInfo.maybeGco = false;
        }
        break;
    case IrCmd::STORE_VECTOR:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(inst.a);

            if (function.cfg.captured.regs.test(reg))
                return;

            StoreRegInfo& regInfo = state.info[reg];

            if (tryReplaceVectorValueWithFullStore(state, build, function, block, index, regInfo))
                break;

            // Partial value store can be removed by a new one if the tag is known
            if (regInfo.knownTag != kUnknownTag)
                state.killValueStore(regInfo);

            regInfo.valueInstIdx = index;
            regInfo.maybeGco = false;
        }
        break;
    case IrCmd::STORE_TVALUE:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(inst.a);

            if (function.cfg.captured.regs.test(reg))
                return;

            StoreRegInfo& regInfo = state.info[reg];

            state.killTagAndValueStorePair(regInfo);
            state.killTValueStore(regInfo);

            regInfo.tvalueInstIdx = index;
            regInfo.maybeGco = true;

            // We do not use tag inference from the source instruction here as it doesn't provide useful opportunities for dead store removal
            regInfo.knownTag = kUnknownTag;

            // If the argument is a vector, it's not a GC object
            // Note that for known boolean/number/GCO, we already optimize into STORE_SPLIT_TVALUE form
            // TODO (CLI-101027): similar code is used in constant propagation optimization and should be shared in utilities
            if (IrInst* arg = function.asInstOp(inst.b))
            {
                if (arg->cmd == IrCmd::TAG_VECTOR)
                    regInfo.maybeGco = false;

                if (arg->cmd == IrCmd::LOAD_TVALUE && arg->c.kind != IrOpKind::None)
                    regInfo.maybeGco = isGCO(function.tagOp(arg->c));
            }

            state.hasGcoToClear |= regInfo.maybeGco;
        }
        break;
    case IrCmd::STORE_SPLIT_TVALUE:
        if (inst.a.kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(inst.a);

            if (function.cfg.captured.regs.test(reg))
                return;

            StoreRegInfo& regInfo = state.info[reg];

            state.killTagAndValueStorePair(regInfo);
            state.killTValueStore(regInfo);

            regInfo.tvalueInstIdx = index;
            regInfo.maybeGco = isGCO(function.tagOp(inst.b));
            regInfo.knownTag = function.tagOp(inst.b);
            state.hasGcoToClear |= regInfo.maybeGco;
        }
        break;

        // Guard checks can jump to a block which might be using some or all the values we stored
    case IrCmd::CHECK_TAG:
        state.checkLiveIns(inst.c);

        // Tag guard establishes the tag value of the register in the current block
        if (IrInst* load = function.asInstOp(inst.a); load && load->cmd == IrCmd::LOAD_TAG && load->a.kind == IrOpKind::VmReg)
        {
            int reg = vmRegOp(load->a);

            StoreRegInfo& regInfo = state.info[reg];

            regInfo.knownTag = function.tagOp(inst.b);
        }
        break;
    case IrCmd::TRY_NUM_TO_INDEX:
        state.checkLiveIns(inst.b);
        break;
    case IrCmd::TRY_CALL_FASTGETTM:
        state.checkLiveIns(inst.c);
        break;
    case IrCmd::CHECK_FASTCALL_RES:
        state.checkLiveIns(inst.b);
        break;
    case IrCmd::CHECK_TRUTHY:
        state.checkLiveIns(inst.c);
        break;
    case IrCmd::CHECK_READONLY:
        state.checkLiveIns(inst.b);
        break;
    case IrCmd::CHECK_NO_METATABLE:
        state.checkLiveIns(inst.b);
        break;
    case IrCmd::CHECK_SAFE_ENV:
        state.checkLiveIns(inst.a);
        break;
    case IrCmd::CHECK_ARRAY_SIZE:
        state.checkLiveIns(inst.c);
        break;
    case IrCmd::CHECK_SLOT_MATCH:
        state.checkLiveIns(inst.c);
        break;
    case IrCmd::CHECK_NODE_NO_NEXT:
        state.checkLiveIns(inst.b);
        break;
    case IrCmd::CHECK_NODE_VALUE:
        state.checkLiveIns(inst.b);
        break;
    case IrCmd::CHECK_BUFFER_LEN:
        state.checkLiveIns(inst.d);
        break;
    case IrCmd::CHECK_USERDATA_TAG:
        state.checkLiveIns(inst.c);
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

    default:
        // Guards have to be covered explicitly
        CODEGEN_ASSERT(!isNonTerminatingJump(inst.cmd));

        visitVmRegDefsUses(state, function, inst);
        break;
    }
}

static void markDeadStoresInBlock(IrBuilder& build, IrBlock& block, RemoveDeadStoreState& state)
{
    IrFunction& function = build.function;

    for (uint32_t index = block.start; index <= block.finish; index++)
    {
        CODEGEN_ASSERT(index < function.instructions.size());
        IrInst& inst = function.instructions[index];

        markDeadStoresInInst(state, build, function, block, inst, index);
    }
}

static void markDeadStoresInBlockChain(IrBuilder& build, std::vector<uint8_t>& visited, IrBlock* block)
{
    IrFunction& function = build.function;

    RemoveDeadStoreState state{function};

    while (block)
    {
        uint32_t blockIdx = function.getBlockIndex(*block);
        CODEGEN_ASSERT(!visited[blockIdx]);
        visited[blockIdx] = true;

        markDeadStoresInBlock(build, *block, state);

        IrInst& termInst = function.instructions[block->finish];

        IrBlock* nextBlock = nullptr;

        // Unconditional jump into a block with a single user (current block) allows us to continue optimization
        // with the information we have gathered so far (unless we have already visited that block earlier)
        if (termInst.cmd == IrCmd::JUMP && termInst.a.kind == IrOpKind::Block)
        {
            IrBlock& target = function.blockOp(termInst.a);
            uint32_t targetIdx = function.getBlockIndex(target);

            if (target.useCount == 1 && !visited[targetIdx] && target.kind != IrBlockKind::Fallback)
                nextBlock = &target;
        }

        block = nextBlock;
    }
}

void markDeadStoresInBlockChains(IrBuilder& build)
{
    IrFunction& function = build.function;

    std::vector<uint8_t> visited(function.blocks.size(), false);

    for (IrBlock& block : function.blocks)
    {
        if (block.kind == IrBlockKind::Fallback || block.kind == IrBlockKind::Dead)
            continue;

        if (visited[function.getBlockIndex(block)])
            continue;

        markDeadStoresInBlockChain(build, visited, &block);
    }
}

} // namespace CodeGen
} // namespace Luau
