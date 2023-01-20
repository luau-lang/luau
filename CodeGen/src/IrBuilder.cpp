// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrBuilder.h"

#include "Luau/Common.h"

#include "CustomExecUtils.h"
#include "IrTranslation.h"
#include "IrUtils.h"

#include "lapi.h"

namespace Luau
{
namespace CodeGen
{

constexpr unsigned kNoAssociatedBlockIndex = ~0u;

void IrBuilder::buildFunctionIr(Proto* proto)
{
    function.proto = proto;

    // Rebuild original control flow blocks
    rebuildBytecodeBasicBlocks(proto);

    function.bcMapping.resize(proto->sizecode, {~0u, 0});

    // Translate all instructions to IR inside blocks
    for (int i = 0; i < proto->sizecode;)
    {
        const Instruction* pc = &proto->code[i];
        LuauOpcode op = LuauOpcode(LUAU_INSN_OP(*pc));

        int nexti = i + getOpLength(op);
        LUAU_ASSERT(nexti <= proto->sizecode);

        function.bcMapping[i] = {uint32_t(function.instructions.size()), 0};

        // Begin new block at this instruction if it was in the bytecode or requested during translation
        if (instIndexToBlock[i] != kNoAssociatedBlockIndex)
            beginBlock(blockAtInst(i));

        translateInst(op, pc, i);

        i = nexti;
        LUAU_ASSERT(i <= proto->sizecode);

        // If we are going into a new block at the next instruction and it's a fallthrough, jump has to be placed to mark block termination
        if (i < int(instIndexToBlock.size()) && instIndexToBlock[i] != kNoAssociatedBlockIndex)
        {
            if (!isBlockTerminator(function.instructions.back().cmd))
                inst(IrCmd::JUMP, blockAtInst(i));
        }
    }
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
        inst(IrCmd::LOP_CALL, constUint(i));

        if (activeFastcallFallback)
        {
            inst(IrCmd::JUMP, fastcallFallbackReturn);

            beginBlock(fastcallFallbackReturn);

            activeFastcallFallback = false;
        }
        break;
    case LOP_RETURN:
        inst(IrCmd::LOP_RETURN, constUint(i));
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
        inst(IrCmd::LOP_SETLIST, constUint(i));
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
    {
        IrOp fallback = block(IrBlockKind::Fallback);
        IrOp next = blockAtInst(i + LUAU_INSN_C(*pc) + 2);

        inst(IrCmd::LOP_FASTCALL, constUint(i), fallback);
        inst(IrCmd::JUMP, next);

        beginBlock(fallback);

        activeFastcallFallback = true;
        fastcallFallbackReturn = next;
        break;
    }
    case LOP_FASTCALL1:
    {
        IrOp fallback = block(IrBlockKind::Fallback);
        IrOp next = blockAtInst(i + LUAU_INSN_C(*pc) + 2);

        inst(IrCmd::LOP_FASTCALL1, constUint(i), fallback);
        inst(IrCmd::JUMP, next);

        beginBlock(fallback);

        activeFastcallFallback = true;
        fastcallFallbackReturn = next;
        break;
    }
    case LOP_FASTCALL2:
    {
        IrOp fallback = block(IrBlockKind::Fallback);
        IrOp next = blockAtInst(i + LUAU_INSN_C(*pc) + 2);

        inst(IrCmd::LOP_FASTCALL2, constUint(i), fallback);
        inst(IrCmd::JUMP, next);

        beginBlock(fallback);

        activeFastcallFallback = true;
        fastcallFallbackReturn = next;
        break;
    }
    case LOP_FASTCALL2K:
    {
        IrOp fallback = block(IrBlockKind::Fallback);
        IrOp next = blockAtInst(i + LUAU_INSN_C(*pc) + 2);

        inst(IrCmd::LOP_FASTCALL2K, constUint(i), fallback);
        inst(IrCmd::JUMP, next);

        beginBlock(fallback);

        activeFastcallFallback = true;
        fastcallFallbackReturn = next;
        break;
    }
    case LOP_FORNPREP:
    {
        IrOp loopExit = blockAtInst(i + 1 + LUAU_INSN_D(*pc));

        inst(IrCmd::LOP_FORNPREP, constUint(i), loopExit);
        break;
    }
    case LOP_FORNLOOP:
    {
        IrOp loopRepeat = blockAtInst(i + 1 + LUAU_INSN_D(*pc));
        IrOp loopExit = blockAtInst(i + getOpLength(LOP_FORNLOOP));

        inst(IrCmd::LOP_FORNLOOP, constUint(i), loopRepeat, loopExit);

        beginBlock(loopExit);
        break;
    }
    case LOP_FORGLOOP:
    {
        IrOp loopRepeat = blockAtInst(i + 1 + LUAU_INSN_D(*pc));
        IrOp loopExit = blockAtInst(i + getOpLength(LOP_FORGLOOP));
        IrOp fallback = block(IrBlockKind::Fallback);

        inst(IrCmd::LOP_FORGLOOP, constUint(i), loopRepeat, loopExit, fallback);

        beginBlock(fallback);
        inst(IrCmd::LOP_FORGLOOP_FALLBACK, constUint(i), loopRepeat, loopExit);

        beginBlock(loopExit);
        break;
    }
    case LOP_FORGPREP_NEXT:
    {
        IrOp target = blockAtInst(i + 1 + LUAU_INSN_D(*pc));
        IrOp fallback = block(IrBlockKind::Fallback);

        inst(IrCmd::LOP_FORGPREP_NEXT, constUint(i), target, fallback);

        beginBlock(fallback);
        inst(IrCmd::LOP_FORGPREP_XNEXT_FALLBACK, constUint(i), target);
        break;
    }
    case LOP_FORGPREP_INEXT:
    {
        IrOp target = blockAtInst(i + 1 + LUAU_INSN_D(*pc));
        IrOp fallback = block(IrBlockKind::Fallback);

        inst(IrCmd::LOP_FORGPREP_INEXT, constUint(i), target, fallback);

        beginBlock(fallback);
        inst(IrCmd::LOP_FORGPREP_XNEXT_FALLBACK, constUint(i), target);
        break;
    }
    case LOP_AND:
        inst(IrCmd::LOP_AND, constUint(i));
        break;
    case LOP_ANDK:
        inst(IrCmd::LOP_ANDK, constUint(i));
        break;
    case LOP_OR:
        inst(IrCmd::LOP_OR, constUint(i));
        break;
    case LOP_ORK:
        inst(IrCmd::LOP_ORK, constUint(i));
        break;
    case LOP_COVERAGE:
        inst(IrCmd::LOP_COVERAGE, constUint(i));
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
    {
        IrOp next = blockAtInst(i + getOpLength(LOP_NAMECALL));
        IrOp fallback = block(IrBlockKind::Fallback);

        inst(IrCmd::LOP_NAMECALL, constUint(i), next, fallback);

        beginBlock(fallback);
        inst(IrCmd::FALLBACK_NAMECALL, constUint(i));
        inst(IrCmd::JUMP, next);

        beginBlock(next);
        break;
    }
    case LOP_PREPVARARGS:
        inst(IrCmd::FALLBACK_PREPVARARGS, constUint(i));
        break;
    case LOP_GETVARARGS:
        inst(IrCmd::FALLBACK_GETVARARGS, constUint(i));
        break;
    case LOP_NEWCLOSURE:
        inst(IrCmd::FALLBACK_NEWCLOSURE, constUint(i));
        break;
    case LOP_DUPCLOSURE:
        inst(IrCmd::FALLBACK_DUPCLOSURE, constUint(i));
        break;
    case LOP_FORGPREP:
        inst(IrCmd::FALLBACK_FORGPREP, constUint(i));
        break;
    default:
        LUAU_ASSERT(!"unknown instruction");
        break;
    }
}

bool IrBuilder::isInternalBlock(IrOp block)
{
    IrBlock& target = function.blocks[block.index];

    return target.kind == IrBlockKind::Internal;
}

void IrBuilder::beginBlock(IrOp block)
{
    function.blocks[block.index].start = uint32_t(function.instructions.size());
}

IrOp IrBuilder::constBool(bool value)
{
    IrConst constant;
    constant.kind = IrConstKind::Bool;
    constant.valueBool = value;
    return constAny(constant);
}

IrOp IrBuilder::constInt(int value)
{
    IrConst constant;
    constant.kind = IrConstKind::Int;
    constant.valueInt = value;
    return constAny(constant);
}

IrOp IrBuilder::constUint(unsigned value)
{
    IrConst constant;
    constant.kind = IrConstKind::Uint;
    constant.valueUint = value;
    return constAny(constant);
}

IrOp IrBuilder::constDouble(double value)
{
    IrConst constant;
    constant.kind = IrConstKind::Double;
    constant.valueDouble = value;
    return constAny(constant);
}

IrOp IrBuilder::constTag(uint8_t value)
{
    IrConst constant;
    constant.kind = IrConstKind::Tag;
    constant.valueTag = value;
    return constAny(constant);
}

IrOp IrBuilder::constAny(IrConst constant)
{
    uint32_t index = uint32_t(function.constants.size());
    function.constants.push_back(constant);
    return {IrOpKind::Constant, index};
}

IrOp IrBuilder::cond(IrCondition cond)
{
    return {IrOpKind::Condition, uint32_t(cond)};
}

IrOp IrBuilder::inst(IrCmd cmd)
{
    return inst(cmd, {}, {}, {}, {}, {});
}

IrOp IrBuilder::inst(IrCmd cmd, IrOp a)
{
    return inst(cmd, a, {}, {}, {}, {});
}

IrOp IrBuilder::inst(IrCmd cmd, IrOp a, IrOp b)
{
    return inst(cmd, a, b, {}, {}, {});
}

IrOp IrBuilder::inst(IrCmd cmd, IrOp a, IrOp b, IrOp c)
{
    return inst(cmd, a, b, c, {}, {});
}

IrOp IrBuilder::inst(IrCmd cmd, IrOp a, IrOp b, IrOp c, IrOp d)
{
    return inst(cmd, a, b, c, d, {});
}

IrOp IrBuilder::inst(IrCmd cmd, IrOp a, IrOp b, IrOp c, IrOp d, IrOp e)
{
    uint32_t index = uint32_t(function.instructions.size());
    function.instructions.push_back({cmd, a, b, c, d, e});
    return {IrOpKind::Inst, index};
}

IrOp IrBuilder::block(IrBlockKind kind)
{
    if (kind == IrBlockKind::Internal && activeFastcallFallback)
        kind = IrBlockKind::Fallback;

    uint32_t index = uint32_t(function.blocks.size());
    function.blocks.push_back(IrBlock{kind, ~0u});
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

} // namespace CodeGen
} // namespace Luau
