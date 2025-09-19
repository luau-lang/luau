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
enum class HostMetamethod;

int getOpLength(LuauOpcode op);
bool isJumpD(LuauOpcode op);
bool isSkipC(LuauOpcode op);
bool isFastCall(LuauOpcode op);
int getJumpTarget(uint32_t insn, uint32_t pc);

inline bool isBlockTerminator(IrCmd cmd)
{
    switch (cmd)
    {
    case IrCmd::JUMP:
    case IrCmd::JUMP_IF_TRUTHY:
    case IrCmd::JUMP_IF_FALSY:
    case IrCmd::JUMP_EQ_TAG:
    case IrCmd::JUMP_CMP_INT:
    case IrCmd::JUMP_EQ_POINTER:
    case IrCmd::JUMP_CMP_NUM:
    case IrCmd::JUMP_FORN_LOOP_COND:
    case IrCmd::JUMP_SLOT_MATCH:
    case IrCmd::RETURN:
    case IrCmd::FORGLOOP:
    case IrCmd::FORGLOOP_FALLBACK:
    case IrCmd::FORGPREP_XNEXT_FALLBACK:
    case IrCmd::FALLBACK_FORGPREP:
        return true;
    default:
        break;
    }

    return false;
}

inline bool isNonTerminatingJump(IrCmd cmd)
{
    switch (cmd)
    {
    case IrCmd::TRY_NUM_TO_INDEX:
    case IrCmd::TRY_CALL_FASTGETTM:
    case IrCmd::CHECK_FASTCALL_RES:
    case IrCmd::CHECK_TAG:
    case IrCmd::CHECK_TRUTHY:
    case IrCmd::CHECK_READONLY:
    case IrCmd::CHECK_NO_METATABLE:
    case IrCmd::CHECK_SAFE_ENV:
    case IrCmd::CHECK_ARRAY_SIZE:
    case IrCmd::CHECK_SLOT_MATCH:
    case IrCmd::CHECK_NODE_NO_NEXT:
    case IrCmd::CHECK_NODE_VALUE:
    case IrCmd::CHECK_BUFFER_LEN:
    case IrCmd::CHECK_USERDATA_TAG:
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
    case IrCmd::LOAD_FLOAT:
    case IrCmd::LOAD_TVALUE:
    case IrCmd::LOAD_ENV:
    case IrCmd::GET_ARR_ADDR:
    case IrCmd::GET_SLOT_NODE_ADDR:
    case IrCmd::GET_HASH_NODE_ADDR:
    case IrCmd::GET_CLOSURE_UPVAL_ADDR:
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
    case IrCmd::SIGN_NUM:
    case IrCmd::SELECT_NUM:
    case IrCmd::ADD_VEC:
    case IrCmd::SUB_VEC:
    case IrCmd::MUL_VEC:
    case IrCmd::DIV_VEC:
    case IrCmd::DOT_VEC:
    case IrCmd::UNM_VEC:
    case IrCmd::NOT_ANY:
    case IrCmd::CMP_ANY:
    case IrCmd::CMP_INT:
    case IrCmd::CMP_TAG:
    case IrCmd::CMP_SPLIT_TVALUE:
    case IrCmd::TABLE_LEN:
    case IrCmd::TABLE_SETNUM:
    case IrCmd::STRING_LEN:
    case IrCmd::NEW_TABLE:
    case IrCmd::DUP_TABLE:
    case IrCmd::TRY_NUM_TO_INDEX:
    case IrCmd::TRY_CALL_FASTGETTM:
    case IrCmd::NEW_USERDATA:
    case IrCmd::INT_TO_NUM:
    case IrCmd::UINT_TO_NUM:
    case IrCmd::NUM_TO_INT:
    case IrCmd::NUM_TO_UINT:
    case IrCmd::NUM_TO_VEC:
    case IrCmd::TAG_VECTOR:
    case IrCmd::SUBSTITUTE:
    case IrCmd::INVOKE_FASTCALL:
    case IrCmd::BITAND_UINT:
    case IrCmd::BITXOR_UINT:
    case IrCmd::BITOR_UINT:
    case IrCmd::BITNOT_UINT:
    case IrCmd::BITLSHIFT_UINT:
    case IrCmd::BITRSHIFT_UINT:
    case IrCmd::BITARSHIFT_UINT:
    case IrCmd::BITLROTATE_UINT:
    case IrCmd::BITRROTATE_UINT:
    case IrCmd::BITCOUNTLZ_UINT:
    case IrCmd::BITCOUNTRZ_UINT:
    case IrCmd::INVOKE_LIBM:
    case IrCmd::GET_TYPE:
    case IrCmd::GET_TYPEOF:
    case IrCmd::NEWCLOSURE:
    case IrCmd::FINDUPVAL:
    case IrCmd::BUFFER_READI8:
    case IrCmd::BUFFER_READU8:
    case IrCmd::BUFFER_READI16:
    case IrCmd::BUFFER_READU16:
    case IrCmd::BUFFER_READI32:
    case IrCmd::BUFFER_READF32:
    case IrCmd::BUFFER_READF64:
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

IrValueKind getCmdValueKind(IrCmd cmd);

bool isGCO(uint8_t tag);

// Optional bit has to be cleared at call site, otherwise, this will return 'false' for 'userdata?'
bool isUserdataBytecodeType(uint8_t ty);
bool isCustomUserdataBytecodeType(uint8_t ty);

HostMetamethod tmToHostMetamethod(int tm);

// Manually add or remove use of an operand
void addUse(IrFunction& function, IrOp op);
void removeUse(IrFunction& function, IrOp op);

// Remove a single instruction
void kill(IrFunction& function, IrInst& inst);

// Remove a range of instructions
void kill(IrFunction& function, uint32_t start, uint32_t end);

// Remove a block, including all instructions inside
void kill(IrFunction& function, IrBlock& block);

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

uint32_t getNativeContextOffset(int bfid);

// Cleans up blocks that were created with no users
void killUnusedBlocks(IrFunction& function);

// Get blocks in order that tries to maximize fallthrough between them during lowering
// We want to mostly preserve build order with fallbacks outlined
// But we also use hints from optimization passes that chain blocks together where there's only one out-in edge between them
std::vector<uint32_t> getSortedBlockOrder(IrFunction& function);

// Returns first non-dead block that comes after block at index 'i' in the sorted blocks array
// 'dummy' block is returned if the end of array was reached
IrBlock& getNextBlock(IrFunction& function, const std::vector<uint32_t>& sortedBlocks, IrBlock& dummy, size_t i);

} // namespace CodeGen
} // namespace Luau
