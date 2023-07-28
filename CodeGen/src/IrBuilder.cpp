// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrBuilder.h"

#include "Luau/IrAnalysis.h"
#include "Luau/IrUtils.h"

#include "IrTranslation.h"

#include "lapi.h"

#include <string.h>

namespace Luau
{
namespace CodeGen
{

constexpr unsigned kNoAssociatedBlockIndex = ~0u;

IrBuilder::IrBuilder()
    : constantMap({IrConstKind::Tag, ~0ull})
{
}

static void buildArgumentTypeChecks(IrBuilder& build, Proto* proto)
{
    if (!proto->typeinfo || proto->numparams == 0)
        return;

    for (int i = 0; i < proto->numparams; ++i)
    {
        uint8_t et = proto->typeinfo[2 + i];

        uint8_t tag = et & ~LBC_TYPE_OPTIONAL_BIT;
        uint8_t optional = et & LBC_TYPE_OPTIONAL_BIT;

        if (tag == LBC_TYPE_ANY)
            continue;

        IrOp load = build.inst(IrCmd::LOAD_TAG, build.vmReg(i));

        IrOp nextCheck;
        if (optional)
        {
            nextCheck = build.block(IrBlockKind::Internal);
            IrOp fallbackCheck = build.block(IrBlockKind::Internal);

            build.inst(IrCmd::JUMP_EQ_TAG, load, build.constTag(LUA_TNIL), nextCheck, fallbackCheck);

            build.beginBlock(fallbackCheck);
        }

        switch (tag)
        {
        case LBC_TYPE_NIL:
            build.inst(IrCmd::CHECK_TAG, load, build.constTag(LUA_TNIL), build.undef(), build.constInt(1));
            break;
        case LBC_TYPE_BOOLEAN:
            build.inst(IrCmd::CHECK_TAG, load, build.constTag(LUA_TBOOLEAN), build.undef(), build.constInt(1));
            break;
        case LBC_TYPE_NUMBER:
            build.inst(IrCmd::CHECK_TAG, load, build.constTag(LUA_TNUMBER), build.undef(), build.constInt(1));
            break;
        case LBC_TYPE_STRING:
            build.inst(IrCmd::CHECK_TAG, load, build.constTag(LUA_TSTRING), build.undef(), build.constInt(1));
            break;
        case LBC_TYPE_TABLE:
            build.inst(IrCmd::CHECK_TAG, load, build.constTag(LUA_TTABLE), build.undef(), build.constInt(1));
            break;
        case LBC_TYPE_FUNCTION:
            build.inst(IrCmd::CHECK_TAG, load, build.constTag(LUA_TFUNCTION), build.undef(), build.constInt(1));
            break;
        case LBC_TYPE_THREAD:
            build.inst(IrCmd::CHECK_TAG, load, build.constTag(LUA_TTHREAD), build.undef(), build.constInt(1));
            break;
        case LBC_TYPE_USERDATA:
            build.inst(IrCmd::CHECK_TAG, load, build.constTag(LUA_TUSERDATA), build.undef(), build.constInt(1));
            break;
        case LBC_TYPE_VECTOR:
            build.inst(IrCmd::CHECK_TAG, load, build.constTag(LUA_TVECTOR), build.undef(), build.constInt(1));
            break;
        }

        if (optional)
        {
            build.inst(IrCmd::JUMP, nextCheck);
            build.beginBlock(nextCheck);
        }
    }

    // If the last argument is optional, we can skip creating a new internal block since one will already have been created.
    if (!(proto->typeinfo[2 + proto->numparams - 1] & LBC_TYPE_OPTIONAL_BIT))
    {
        IrOp next = build.block(IrBlockKind::Internal);
        build.inst(IrCmd::JUMP, next);

        build.beginBlock(next);
    }
}

void IrBuilder::buildFunctionIr(Proto* proto)
{
    function.proto = proto;
    function.variadic = proto->is_vararg != 0;

    // Rebuild original control flow blocks
    rebuildBytecodeBasicBlocks(proto);

    function.bcMapping.resize(proto->sizecode, {~0u, ~0u});

    // Translate all instructions to IR inside blocks
    for (int i = 0; i < proto->sizecode;)
    {
        const Instruction* pc = &proto->code[i];
        LuauOpcode op = LuauOpcode(LUAU_INSN_OP(*pc));

        int nexti = i + getOpLength(op);
        LUAU_ASSERT(nexti <= proto->sizecode);

        function.bcMapping[i] = {uint32_t(function.instructions.size()), ~0u};

        // Begin new block at this instruction if it was in the bytecode or requested during translation
        if (instIndexToBlock[i] != kNoAssociatedBlockIndex)
            beginBlock(blockAtInst(i));

        if (i == 0)
            buildArgumentTypeChecks(*this, proto);

        // We skip dead bytecode instructions when they appear after block was already terminated
        if (!inTerminatedBlock)
        {
            translateInst(op, pc, i);

            if (fastcallSkipTarget != -1)
            {
                nexti = fastcallSkipTarget;
                fastcallSkipTarget = -1;
            }
        }

        i = nexti;
        LUAU_ASSERT(i <= proto->sizecode);

        // If we are going into a new block at the next instruction and it's a fallthrough, jump has to be placed to mark block termination
        if (i < int(instIndexToBlock.size()) && instIndexToBlock[i] != kNoAssociatedBlockIndex)
        {
            if (!isBlockTerminator(function.instructions.back().cmd))
                inst(IrCmd::JUMP, blockAtInst(i));
        }
    }

    // Now that all has been generated, compute use counts
    updateUseCounts(function);
}

void IrBuilder::rebuildBytecodeBasicBlocks(Proto* proto)
{
    instIndexToBlock.resize(proto->sizecode, kNoAssociatedBlockIndex);

    // Mark jump targets
    std::vector<uint8_t> jumpTargets(proto->sizecode, 0);

    for (int i = 0; i < proto->sizecode;)
    {
        const Instruction* pc = &proto->code[i];
        LuauOpcode op = LuauOpcode(LUAU_INSN_OP(*pc));

        int target = getJumpTarget(*pc, uint32_t(i));

        if (target >= 0 && !isFastCall(op))
            jumpTargets[target] = true;

        i += getOpLength(op);
        LUAU_ASSERT(i <= proto->sizecode);
    }


    // Bytecode blocks are created at bytecode jump targets and the start of a function
    jumpTargets[0] = true;

    for (int i = 0; i < proto->sizecode; i++)
    {
        if (jumpTargets[i])
        {
            IrOp b = block(IrBlockKind::Bytecode);
            instIndexToBlock[i] = b.index;
        }
    }
}

void IrBuilder::translateInst(LuauOpcode op, const Instruction* pc, int i)
{
    switch (op)
    {
    case LOP_NOP:
        break;
    case LOP_LOADNIL:
        translateInstLoadNil(*this, pc);
        break;
    case LOP_LOADB:
        translateInstLoadB(*this, pc, i);
        break;
    case LOP_LOADN:
        translateInstLoadN(*this, pc);
        break;
    case LOP_LOADK:
        translateInstLoadK(*this, pc);
        break;
    case LOP_LOADKX:
        translateInstLoadKX(*this, pc);
        break;
    case LOP_MOVE:
        translateInstMove(*this, pc);
        break;
    case LOP_GETGLOBAL:
        translateInstGetGlobal(*this, pc, i);
        break;
    case LOP_SETGLOBAL:
        translateInstSetGlobal(*this, pc, i);
        break;
    case LOP_CALL:
        inst(IrCmd::INTERRUPT, constUint(i));
        inst(IrCmd::SET_SAVEDPC, constUint(i + 1));

        inst(IrCmd::CALL, vmReg(LUAU_INSN_A(*pc)), constInt(LUAU_INSN_B(*pc) - 1), constInt(LUAU_INSN_C(*pc) - 1));

        if (activeFastcallFallback)
        {
            inst(IrCmd::JUMP, fastcallFallbackReturn);

            beginBlock(fastcallFallbackReturn);

            activeFastcallFallback = false;
        }
        break;
    case LOP_RETURN:
        inst(IrCmd::INTERRUPT, constUint(i));

        inst(IrCmd::RETURN, vmReg(LUAU_INSN_A(*pc)), constInt(LUAU_INSN_B(*pc) - 1));
        break;
    case LOP_GETTABLE:
        translateInstGetTable(*this, pc, i);
        break;
    case LOP_SETTABLE:
        translateInstSetTable(*this, pc, i);
        break;
    case LOP_GETTABLEKS:
        translateInstGetTableKS(*this, pc, i);
        break;
    case LOP_SETTABLEKS:
        translateInstSetTableKS(*this, pc, i);
        break;
    case LOP_GETTABLEN:
        translateInstGetTableN(*this, pc, i);
        break;
    case LOP_SETTABLEN:
        translateInstSetTableN(*this, pc, i);
        break;
    case LOP_JUMP:
        translateInstJump(*this, pc, i);
        break;
    case LOP_JUMPBACK:
        translateInstJumpBack(*this, pc, i);
        break;
    case LOP_JUMPIF:
        translateInstJumpIf(*this, pc, i, /* not_ */ false);
        break;
    case LOP_JUMPIFNOT:
        translateInstJumpIf(*this, pc, i, /* not_ */ true);
        break;
    case LOP_JUMPIFEQ:
        translateInstJumpIfEq(*this, pc, i, /* not_ */ false);
        break;
    case LOP_JUMPIFLE:
        translateInstJumpIfCond(*this, pc, i, IrCondition::LessEqual);
        break;
    case LOP_JUMPIFLT:
        translateInstJumpIfCond(*this, pc, i, IrCondition::Less);
        break;
    case LOP_JUMPIFNOTEQ:
        translateInstJumpIfEq(*this, pc, i, /* not_ */ true);
        break;
    case LOP_JUMPIFNOTLE:
        translateInstJumpIfCond(*this, pc, i, IrCondition::NotLessEqual);
        break;
    case LOP_JUMPIFNOTLT:
        translateInstJumpIfCond(*this, pc, i, IrCondition::NotLess);
        break;
    case LOP_JUMPX:
        translateInstJumpX(*this, pc, i);
        break;
    case LOP_JUMPXEQKNIL:
        translateInstJumpxEqNil(*this, pc, i);
        break;
    case LOP_JUMPXEQKB:
        translateInstJumpxEqB(*this, pc, i);
        break;
    case LOP_JUMPXEQKN:
        translateInstJumpxEqN(*this, pc, i);
        break;
    case LOP_JUMPXEQKS:
        translateInstJumpxEqS(*this, pc, i);
        break;
    case LOP_ADD:
        translateInstBinary(*this, pc, i, TM_ADD);
        break;
    case LOP_SUB:
        translateInstBinary(*this, pc, i, TM_SUB);
        break;
    case LOP_MUL:
        translateInstBinary(*this, pc, i, TM_MUL);
        break;
    case LOP_DIV:
        translateInstBinary(*this, pc, i, TM_DIV);
        break;
    case LOP_MOD:
        translateInstBinary(*this, pc, i, TM_MOD);
        break;
    case LOP_POW:
        translateInstBinary(*this, pc, i, TM_POW);
        break;
    case LOP_ADDK:
        translateInstBinaryK(*this, pc, i, TM_ADD);
        break;
    case LOP_SUBK:
        translateInstBinaryK(*this, pc, i, TM_SUB);
        break;
    case LOP_MULK:
        translateInstBinaryK(*this, pc, i, TM_MUL);
        break;
    case LOP_DIVK:
        translateInstBinaryK(*this, pc, i, TM_DIV);
        break;
    case LOP_MODK:
        translateInstBinaryK(*this, pc, i, TM_MOD);
        break;
    case LOP_POWK:
        translateInstBinaryK(*this, pc, i, TM_POW);
        break;
    case LOP_NOT:
        translateInstNot(*this, pc);
        break;
    case LOP_MINUS:
        translateInstMinus(*this, pc, i);
        break;
    case LOP_LENGTH:
        translateInstLength(*this, pc, i);
        break;
    case LOP_NEWTABLE:
        translateInstNewTable(*this, pc, i);
        break;
    case LOP_DUPTABLE:
        translateInstDupTable(*this, pc, i);
        break;
    case LOP_SETLIST:
        inst(IrCmd::SETLIST, constUint(i), vmReg(LUAU_INSN_A(*pc)), vmReg(LUAU_INSN_B(*pc)), constInt(LUAU_INSN_C(*pc) - 1), constUint(pc[1]));
        break;
    case LOP_GETUPVAL:
        translateInstGetUpval(*this, pc, i);
        break;
    case LOP_SETUPVAL:
        translateInstSetUpval(*this, pc, i);
        break;
    case LOP_CLOSEUPVALS:
        translateInstCloseUpvals(*this, pc);
        break;
    case LOP_FASTCALL:
        handleFastcallFallback(translateFastCallN(*this, pc, i, false, 0, {}), pc, i);
        break;
    case LOP_FASTCALL1:
        handleFastcallFallback(translateFastCallN(*this, pc, i, true, 1, undef()), pc, i);
        break;
    case LOP_FASTCALL2:
        handleFastcallFallback(translateFastCallN(*this, pc, i, true, 2, vmReg(pc[1])), pc, i);
        break;
    case LOP_FASTCALL2K:
        handleFastcallFallback(translateFastCallN(*this, pc, i, true, 2, vmConst(pc[1])), pc, i);
        break;
    case LOP_FORNPREP:
        translateInstForNPrep(*this, pc, i);
        break;
    case LOP_FORNLOOP:
        translateInstForNLoop(*this, pc, i);
        break;
    case LOP_FORGLOOP:
    {
        int aux = int(pc[1]);

        // We have a translation for ipairs-style traversal, general loop iteration is still too complex
        if (aux < 0)
        {
            translateInstForGLoopIpairs(*this, pc, i);
        }
        else
        {
            int ra = LUAU_INSN_A(*pc);

            IrOp loopRepeat = blockAtInst(i + 1 + LUAU_INSN_D(*pc));
            IrOp loopExit = blockAtInst(i + getOpLength(LOP_FORGLOOP));
            IrOp fallback = block(IrBlockKind::Fallback);

            inst(IrCmd::INTERRUPT, constUint(i));
            loadAndCheckTag(vmReg(ra), LUA_TNIL, fallback);

            inst(IrCmd::FORGLOOP, vmReg(ra), constInt(aux), loopRepeat, loopExit);

            beginBlock(fallback);
            inst(IrCmd::SET_SAVEDPC, constUint(i + 1));
            inst(IrCmd::FORGLOOP_FALLBACK, vmReg(ra), constInt(aux), loopRepeat, loopExit);

            beginBlock(loopExit);
        }
        break;
    }
    case LOP_FORGPREP_NEXT:
        translateInstForGPrepNext(*this, pc, i);
        break;
    case LOP_FORGPREP_INEXT:
        translateInstForGPrepInext(*this, pc, i);
        break;
    case LOP_AND:
        translateInstAndX(*this, pc, i, vmReg(LUAU_INSN_C(*pc)));
        break;
    case LOP_ANDK:
        translateInstAndX(*this, pc, i, vmConst(LUAU_INSN_C(*pc)));
        break;
    case LOP_OR:
        translateInstOrX(*this, pc, i, vmReg(LUAU_INSN_C(*pc)));
        break;
    case LOP_ORK:
        translateInstOrX(*this, pc, i, vmConst(LUAU_INSN_C(*pc)));
        break;
    case LOP_COVERAGE:
        inst(IrCmd::COVERAGE, constUint(i));
        break;
    case LOP_GETIMPORT:
        translateInstGetImport(*this, pc, i);
        break;
    case LOP_CONCAT:
        translateInstConcat(*this, pc, i);
        break;
    case LOP_CAPTURE:
        translateInstCapture(*this, pc, i);
        break;
    case LOP_NAMECALL:
        translateInstNamecall(*this, pc, i);
        break;
    case LOP_PREPVARARGS:
        inst(IrCmd::FALLBACK_PREPVARARGS, constUint(i), constInt(LUAU_INSN_A(*pc)));
        break;
    case LOP_GETVARARGS:
        inst(IrCmd::FALLBACK_GETVARARGS, constUint(i), vmReg(LUAU_INSN_A(*pc)), constInt(LUAU_INSN_B(*pc) - 1));
        break;
    case LOP_NEWCLOSURE:
        translateInstNewClosure(*this, pc, i);
        break;
    case LOP_DUPCLOSURE:
        inst(IrCmd::FALLBACK_DUPCLOSURE, constUint(i), vmReg(LUAU_INSN_A(*pc)), vmConst(LUAU_INSN_D(*pc)));
        break;
    case LOP_FORGPREP:
    {
        IrOp loopStart = blockAtInst(i + 1 + LUAU_INSN_D(*pc));

        inst(IrCmd::FALLBACK_FORGPREP, constUint(i), vmReg(LUAU_INSN_A(*pc)), loopStart);
        break;
    }
    default:
        LUAU_ASSERT(!"Unknown instruction");
    }
}

void IrBuilder::handleFastcallFallback(IrOp fallbackOrUndef, const Instruction* pc, int i)
{
    int skip = LUAU_INSN_C(*pc);

    if (fallbackOrUndef.kind != IrOpKind::Undef)
    {
        IrOp next = blockAtInst(i + skip + 2);
        inst(IrCmd::JUMP, next);
        beginBlock(fallbackOrUndef);

        activeFastcallFallback = true;
        fastcallFallbackReturn = next;
    }
    else
    {
        fastcallSkipTarget = i + skip + 2;
    }
}

bool IrBuilder::isInternalBlock(IrOp block)
{
    IrBlock& target = function.blocks[block.index];

    return target.kind == IrBlockKind::Internal;
}

void IrBuilder::beginBlock(IrOp block)
{
    IrBlock& target = function.blocks[block.index];
    activeBlockIdx = block.index;

    LUAU_ASSERT(target.start == ~0u || target.start == uint32_t(function.instructions.size()));

    target.start = uint32_t(function.instructions.size());
    target.sortkey = target.start;

    inTerminatedBlock = false;
}

void IrBuilder::loadAndCheckTag(IrOp loc, uint8_t tag, IrOp fallback)
{
    inst(IrCmd::CHECK_TAG, inst(IrCmd::LOAD_TAG, loc), constTag(tag), fallback);
}

void IrBuilder::clone(const IrBlock& source, bool removeCurrentTerminator)
{
    DenseHashMap<uint32_t, uint32_t> instRedir{~0u};

    auto redirect = [&instRedir](IrOp& op) {
        if (op.kind == IrOpKind::Inst)
        {
            if (const uint32_t* newIndex = instRedir.find(op.index))
                op.index = *newIndex;
            else
                LUAU_ASSERT(!"Values can only be used if they are defined in the same block");
        }
    };

    if (removeCurrentTerminator && inTerminatedBlock)
    {
        IrBlock& active = function.blocks[activeBlockIdx];
        IrInst& term = function.instructions[active.finish];

        kill(function, term);
        inTerminatedBlock = false;
    }

    for (uint32_t index = source.start; index <= source.finish; index++)
    {
        LUAU_ASSERT(index < function.instructions.size());
        IrInst clone = function.instructions[index];

        // Skip pseudo instructions to make clone more compact, but validate that they have no users
        if (isPseudo(clone.cmd))
        {
            LUAU_ASSERT(clone.useCount == 0);
            continue;
        }

        redirect(clone.a);
        redirect(clone.b);
        redirect(clone.c);
        redirect(clone.d);
        redirect(clone.e);
        redirect(clone.f);

        addUse(function, clone.a);
        addUse(function, clone.b);
        addUse(function, clone.c);
        addUse(function, clone.d);
        addUse(function, clone.e);
        addUse(function, clone.f);

        // Instructions that referenced the original will have to be adjusted to use the clone
        instRedir[index] = uint32_t(function.instructions.size());

        // Reconstruct the fresh clone
        inst(clone.cmd, clone.a, clone.b, clone.c, clone.d, clone.e, clone.f);
    }
}

IrOp IrBuilder::undef()
{
    return {IrOpKind::Undef, 0};
}

IrOp IrBuilder::constInt(int value)
{
    IrConst constant;
    constant.kind = IrConstKind::Int;
    constant.valueInt = value;
    return constAny(constant, uint64_t(value));
}

IrOp IrBuilder::constUint(unsigned value)
{
    IrConst constant;
    constant.kind = IrConstKind::Uint;
    constant.valueUint = value;
    return constAny(constant, uint64_t(value));
}

IrOp IrBuilder::constDouble(double value)
{
    IrConst constant;
    constant.kind = IrConstKind::Double;
    constant.valueDouble = value;

    uint64_t asCommonKey;
    static_assert(sizeof(asCommonKey) == sizeof(value), "Expecting double to be 64-bit");
    memcpy(&asCommonKey, &value, sizeof(value));

    return constAny(constant, asCommonKey);
}

IrOp IrBuilder::constTag(uint8_t value)
{
    IrConst constant;
    constant.kind = IrConstKind::Tag;
    constant.valueTag = value;
    return constAny(constant, uint64_t(value));
}

IrOp IrBuilder::constAny(IrConst constant, uint64_t asCommonKey)
{
    ConstantKey key{constant.kind, asCommonKey};

    if (uint32_t* cache = constantMap.find(key))
        return {IrOpKind::Constant, *cache};

    uint32_t index = uint32_t(function.constants.size());
    function.constants.push_back(constant);

    constantMap[key] = index;

    return {IrOpKind::Constant, index};
}

IrOp IrBuilder::cond(IrCondition cond)
{
    return {IrOpKind::Condition, uint32_t(cond)};
}

IrOp IrBuilder::inst(IrCmd cmd)
{
    return inst(cmd, {}, {}, {}, {}, {}, {});
}

IrOp IrBuilder::inst(IrCmd cmd, IrOp a)
{
    return inst(cmd, a, {}, {}, {}, {}, {});
}

IrOp IrBuilder::inst(IrCmd cmd, IrOp a, IrOp b)
{
    return inst(cmd, a, b, {}, {}, {}, {});
}

IrOp IrBuilder::inst(IrCmd cmd, IrOp a, IrOp b, IrOp c)
{
    return inst(cmd, a, b, c, {}, {}, {});
}

IrOp IrBuilder::inst(IrCmd cmd, IrOp a, IrOp b, IrOp c, IrOp d)
{
    return inst(cmd, a, b, c, d, {}, {});
}

IrOp IrBuilder::inst(IrCmd cmd, IrOp a, IrOp b, IrOp c, IrOp d, IrOp e)
{
    return inst(cmd, a, b, c, d, e, {});
}

IrOp IrBuilder::inst(IrCmd cmd, IrOp a, IrOp b, IrOp c, IrOp d, IrOp e, IrOp f)
{
    uint32_t index = uint32_t(function.instructions.size());
    function.instructions.push_back({cmd, a, b, c, d, e, f});

    LUAU_ASSERT(!inTerminatedBlock);

    if (isBlockTerminator(cmd))
    {
        function.blocks[activeBlockIdx].finish = index;
        inTerminatedBlock = true;
    }

    return {IrOpKind::Inst, index};
}

IrOp IrBuilder::block(IrBlockKind kind)
{
    if (kind == IrBlockKind::Internal && activeFastcallFallback)
        kind = IrBlockKind::Fallback;

    uint32_t index = uint32_t(function.blocks.size());
    function.blocks.push_back(IrBlock{kind});
    return IrOp{IrOpKind::Block, index};
}

IrOp IrBuilder::blockAtInst(uint32_t index)
{
    uint32_t blockIndex = instIndexToBlock[index];

    if (blockIndex != kNoAssociatedBlockIndex)
        return IrOp{IrOpKind::Block, blockIndex};

    return block(IrBlockKind::Internal);
}

IrOp IrBuilder::vmReg(uint8_t index)
{
    return {IrOpKind::VmReg, index};
}

IrOp IrBuilder::vmConst(uint32_t index)
{
    return {IrOpKind::VmConst, index};
}

IrOp IrBuilder::vmUpvalue(uint8_t index)
{
    return {IrOpKind::VmUpvalue, index};
}

IrOp IrBuilder::vmExit(uint32_t pcpos)
{
    return {IrOpKind::VmExit, pcpos};
}

} // namespace CodeGen
} // namespace Luau
