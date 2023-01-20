// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Bytecode.h"
#include "Luau/Common.h"

#include "IrData.h"

namespace Luau
{
namespace CodeGen
{

inline bool isJumpD(LuauOpcode op)
{
    switch (op)
    {
    case LOP_JUMP:
    case LOP_JUMPIF:
    case LOP_JUMPIFNOT:
    case LOP_JUMPIFEQ:
    case LOP_JUMPIFLE:
    case LOP_JUMPIFLT:
    case LOP_JUMPIFNOTEQ:
    case LOP_JUMPIFNOTLE:
    case LOP_JUMPIFNOTLT:
    case LOP_FORNPREP:
    case LOP_FORNLOOP:
    case LOP_FORGPREP:
    case LOP_FORGLOOP:
    case LOP_FORGPREP_INEXT:
    case LOP_FORGPREP_NEXT:
    case LOP_JUMPBACK:
    case LOP_JUMPXEQKNIL:
    case LOP_JUMPXEQKB:
    case LOP_JUMPXEQKN:
    case LOP_JUMPXEQKS:
        return true;

    default:
        return false;
    }
}

inline bool isSkipC(LuauOpcode op)
{
    switch (op)
    {
    case LOP_LOADB:
        return true;

    default:
        return false;
    }
}

inline bool isFastCall(LuauOpcode op)
{
    switch (op)
    {
    case LOP_FASTCALL:
    case LOP_FASTCALL1:
    case LOP_FASTCALL2:
    case LOP_FASTCALL2K:
        return true;

    default:
        return false;
    }
}

inline int getJumpTarget(uint32_t insn, uint32_t pc)
{
    LuauOpcode op = LuauOpcode(LUAU_INSN_OP(insn));

    if (isJumpD(op))
        return int(pc + LUAU_INSN_D(insn) + 1);
    else if (isFastCall(op))
        return int(pc + LUAU_INSN_C(insn) + 2);
    else if (isSkipC(op) && LUAU_INSN_C(insn))
        return int(pc + LUAU_INSN_C(insn) + 1);
    else if (op == LOP_JUMPX)
        return int(pc + LUAU_INSN_E(insn) + 1);
    else
        return -1;
}

inline bool isBlockTerminator(IrCmd cmd)
{
    switch (cmd)
    {
    case IrCmd::JUMP:
    case IrCmd::JUMP_IF_TRUTHY:
    case IrCmd::JUMP_IF_FALSY:
    case IrCmd::JUMP_EQ_TAG:
    case IrCmd::JUMP_EQ_BOOLEAN:
    case IrCmd::JUMP_EQ_POINTER:
    case IrCmd::JUMP_CMP_NUM:
    case IrCmd::JUMP_CMP_STR:
    case IrCmd::JUMP_CMP_ANY:
    case IrCmd::LOP_NAMECALL:
    case IrCmd::LOP_RETURN:
    case IrCmd::LOP_FORNPREP:
    case IrCmd::LOP_FORNLOOP:
    case IrCmd::LOP_FORGLOOP:
    case IrCmd::LOP_FORGLOOP_FALLBACK:
    case IrCmd::LOP_FORGPREP_NEXT:
    case IrCmd::LOP_FORGPREP_INEXT:
    case IrCmd::LOP_FORGPREP_XNEXT_FALLBACK:
    case IrCmd::FALLBACK_FORGPREP:
        return true;
    default:
        break;
    }

    return false;
}

inline bool hasResult(IrCmd cmd)
{
    switch (cmd)
    {
    case IrCmd::LOAD_TAG:
    case IrCmd::LOAD_POINTER:
    case IrCmd::LOAD_DOUBLE:
    case IrCmd::LOAD_INT:
    case IrCmd::LOAD_TVALUE:
    case IrCmd::LOAD_NODE_VALUE_TV:
    case IrCmd::LOAD_ENV:
    case IrCmd::GET_ARR_ADDR:
    case IrCmd::GET_SLOT_NODE_ADDR:
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
    case IrCmd::TABLE_LEN:
    case IrCmd::NEW_TABLE:
    case IrCmd::DUP_TABLE:
    case IrCmd::NUM_TO_INDEX:
        return true;
    default:
        break;
    }

    return false;
}

inline bool hasSideEffects(IrCmd cmd)
{
    // Instructions that don't produce a result most likely have other side-effects to make them useful
    // Right now, a full switch would mirror the 'hasResult' function, so we use this simple condition
    return !hasResult(cmd);
}

} // namespace CodeGen
} // namespace Luau
