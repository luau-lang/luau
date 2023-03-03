// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Bytecode.h"
#include "Luau/Common.h"
#include "Luau/IrData.h"

namespace Luau
{
namespace CodeGen
{

struct IrBuilder;

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
    case IrCmd::JUMP_EQ_INT:
    case IrCmd::JUMP_EQ_POINTER:
    case IrCmd::JUMP_CMP_NUM:
    case IrCmd::JUMP_CMP_ANY:
    case IrCmd::LOP_NAMECALL:
    case IrCmd::LOP_RETURN:
    case IrCmd::LOP_FORGLOOP:
    case IrCmd::LOP_FORGLOOP_FALLBACK:
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
    case IrCmd::MIN_NUM:
    case IrCmd::MAX_NUM:
    case IrCmd::UNM_NUM:
    case IrCmd::NOT_ANY:
    case IrCmd::TABLE_LEN:
    case IrCmd::NEW_TABLE:
    case IrCmd::DUP_TABLE:
    case IrCmd::NUM_TO_INDEX:
    case IrCmd::INT_TO_NUM:
    case IrCmd::SUBSTITUTE:
    case IrCmd::INVOKE_FASTCALL:
        return true;
    default:
        break;
    }

    return false;
}

inline bool hasSideEffects(IrCmd cmd)
{
    if (cmd == IrCmd::INVOKE_FASTCALL)
        return true;

    // Instructions that don't produce a result most likely have other side-effects to make them useful
    // Right now, a full switch would mirror the 'hasResult' function, so we use this simple condition
    return !hasResult(cmd);
}

inline bool isPseudo(IrCmd cmd)
{
    // Instructions that are used for internal needs and are not a part of final lowering
    return cmd == IrCmd::NOP || cmd == IrCmd::SUBSTITUTE;
}

bool isGCO(uint8_t tag);

// Manually add or remove use of an operand
void addUse(IrFunction& function, IrOp op);
void removeUse(IrFunction& function, IrOp op);

// Remove a single instruction
void kill(IrFunction& function, IrInst& inst);

// Remove a range of instructions
void kill(IrFunction& function, uint32_t start, uint32_t end);

// Remove a block, including all instructions inside
void kill(IrFunction& function, IrBlock& block);

void removeUse(IrFunction& function, IrInst& inst);
void removeUse(IrFunction& function, IrBlock& block);

// Replace a single operand and update use counts (can cause chain removal of dead code)
void replace(IrFunction& function, IrOp& original, IrOp replacement);

// Replace a single instruction
// Target instruction index instead of reference is used to handle introduction of a new block terminator
void replace(IrFunction& function, IrBlock& block, uint32_t instIdx, IrInst replacement);

// Replace instruction with a different value (using IrCmd::SUBSTITUTE)
void substitute(IrFunction& function, IrInst& inst, IrOp replacement);

// Replace instruction arguments that point to substitutions with target values
void applySubstitutions(IrFunction& function, IrOp& op);
void applySubstitutions(IrFunction& function, IrInst& inst);

// Compare numbers using IR condition value
bool compare(double a, double b, IrCondition cond);

// Perform constant folding on instruction at index
// For most instructions, successful folding results in a IrCmd::SUBSTITUTE
// But it can also be successful on conditional control-flow, replacing it with an unconditional IrCmd::JUMP
void foldConstants(IrBuilder& build, IrFunction& function, IrBlock& block, uint32_t instIdx);

} // namespace CodeGen
} // namespace Luau
