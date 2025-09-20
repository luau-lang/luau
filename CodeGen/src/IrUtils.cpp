// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrUtils.h"

#include "Luau/CodeGenOptions.h"
#include "Luau/IrBuilder.h"

#include "BitUtils.h"
#include "NativeState.h"

#include "lua.h"
#include "lnumutils.h"

#include <algorithm>
#include <vector>

#include <limits.h>
#include <math.h>

LUAU_FASTFLAG(LuauCodeGenDirectBtest)
LUAU_FASTFLAG(LuauCodegenDirectCompare)

namespace Luau
{
namespace CodeGen
{

int getOpLength(LuauOpcode op)
{
    switch (int(op))
    {
    case LOP_GETGLOBAL:
    case LOP_SETGLOBAL:
    case LOP_GETIMPORT:
    case LOP_GETTABLEKS:
    case LOP_SETTABLEKS:
    case LOP_NAMECALL:
    case LOP_JUMPIFEQ:
    case LOP_JUMPIFLE:
    case LOP_JUMPIFLT:
    case LOP_JUMPIFNOTEQ:
    case LOP_JUMPIFNOTLE:
    case LOP_JUMPIFNOTLT:
    case LOP_NEWTABLE:
    case LOP_SETLIST:
    case LOP_FORGLOOP:
    case LOP_LOADKX:
    case LOP_FASTCALL2:
    case LOP_FASTCALL2K:
    case LOP_FASTCALL3:
    case LOP_JUMPXEQKNIL:
    case LOP_JUMPXEQKB:
    case LOP_JUMPXEQKN:
    case LOP_JUMPXEQKS:
        return 2;

    default:
        return 1;
    }
}

bool isJumpD(LuauOpcode op)
{
    switch (int(op))
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

bool isSkipC(LuauOpcode op)
{
    switch (int(op))
    {
    case LOP_LOADB:
        return true;

    default:
        return false;
    }
}

bool isFastCall(LuauOpcode op)
{
    switch (int(op))
    {
    case LOP_FASTCALL:
    case LOP_FASTCALL1:
    case LOP_FASTCALL2:
    case LOP_FASTCALL2K:
    case LOP_FASTCALL3:
        return true;

    default:
        return false;
    }
}

int getJumpTarget(uint32_t insn, uint32_t pc)
{
    LuauOpcode op = LuauOpcode(LUAU_INSN_OP(insn));

    if (isJumpD(op))
        return int(pc + LUAU_INSN_D(insn) + 1);
    else if (isFastCall(op))
        return int(pc + LUAU_INSN_C(insn) + 2);
    else if (isSkipC(op) && LUAU_INSN_C(insn))
        return int(pc + LUAU_INSN_C(insn) + 1);
    else if (int(op) == LOP_JUMPX)
        return int(pc + LUAU_INSN_E(insn) + 1);
    else
        return -1;
}

IrValueKind getCmdValueKind(IrCmd cmd)
{
    switch (cmd)
    {
    case IrCmd::NOP:
        return IrValueKind::None;
    case IrCmd::LOAD_TAG:
        return IrValueKind::Tag;
    case IrCmd::LOAD_POINTER:
        return IrValueKind::Pointer;
    case IrCmd::LOAD_DOUBLE:
        return IrValueKind::Double;
    case IrCmd::LOAD_INT:
        return IrValueKind::Int;
    case IrCmd::LOAD_FLOAT:
        return IrValueKind::Double;
    case IrCmd::LOAD_TVALUE:
        return IrValueKind::Tvalue;
    case IrCmd::LOAD_ENV:
    case IrCmd::GET_ARR_ADDR:
    case IrCmd::GET_SLOT_NODE_ADDR:
    case IrCmd::GET_HASH_NODE_ADDR:
    case IrCmd::GET_CLOSURE_UPVAL_ADDR:
        return IrValueKind::Pointer;
    case IrCmd::STORE_TAG:
    case IrCmd::STORE_EXTRA:
    case IrCmd::STORE_POINTER:
    case IrCmd::STORE_DOUBLE:
    case IrCmd::STORE_INT:
    case IrCmd::STORE_VECTOR:
    case IrCmd::STORE_TVALUE:
    case IrCmd::STORE_SPLIT_TVALUE:
        return IrValueKind::None;
    case IrCmd::ADD_INT:
    case IrCmd::SUB_INT:
        return IrValueKind::Int;
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
    case IrCmd::MULADD_NUM:
        return IrValueKind::Double;
    case IrCmd::ADD_VEC:
    case IrCmd::SUB_VEC:
    case IrCmd::MUL_VEC:
    case IrCmd::DIV_VEC:
    case IrCmd::UNM_VEC:
    case IrCmd::SELECT_VEC:
    case IrCmd::MULADD_VEC:
        return IrValueKind::Tvalue;
    case IrCmd::DOT_VEC:
        return IrValueKind::Double;
    case IrCmd::NOT_ANY:
    case IrCmd::CMP_ANY:
    case IrCmd::CMP_INT:
    case IrCmd::CMP_TAG:
    case IrCmd::CMP_SPLIT_TVALUE:
        return IrValueKind::Int;
    case IrCmd::JUMP:
    case IrCmd::JUMP_IF_TRUTHY:
    case IrCmd::JUMP_IF_FALSY:
    case IrCmd::JUMP_EQ_TAG:
    case IrCmd::JUMP_CMP_INT:
    case IrCmd::JUMP_EQ_POINTER:
    case IrCmd::JUMP_CMP_NUM:
    case IrCmd::JUMP_FORN_LOOP_COND:
    case IrCmd::JUMP_SLOT_MATCH:
        return IrValueKind::None;
    case IrCmd::TABLE_LEN:
        return IrValueKind::Int;
    case IrCmd::TABLE_SETNUM:
        return IrValueKind::Pointer;
    case IrCmd::STRING_LEN:
        return IrValueKind::Int;
    case IrCmd::NEW_TABLE:
    case IrCmd::DUP_TABLE:
        return IrValueKind::Pointer;
    case IrCmd::TRY_NUM_TO_INDEX:
        return IrValueKind::Int;
    case IrCmd::TRY_CALL_FASTGETTM:
    case IrCmd::NEW_USERDATA:
        return IrValueKind::Pointer;
    case IrCmd::INT_TO_NUM:
    case IrCmd::UINT_TO_NUM:
        return IrValueKind::Double;
    case IrCmd::NUM_TO_INT:
    case IrCmd::NUM_TO_UINT:
        return IrValueKind::Int;
    case IrCmd::NUM_TO_VEC:
    case IrCmd::TAG_VECTOR:
        return IrValueKind::Tvalue;
    case IrCmd::ADJUST_STACK_TO_REG:
    case IrCmd::ADJUST_STACK_TO_TOP:
        return IrValueKind::None;
    case IrCmd::FASTCALL:
        return IrValueKind::None;
    case IrCmd::INVOKE_FASTCALL:
        return IrValueKind::Int;
    case IrCmd::CHECK_FASTCALL_RES:
    case IrCmd::DO_ARITH:
    case IrCmd::DO_LEN:
    case IrCmd::GET_TABLE:
    case IrCmd::SET_TABLE:
    case IrCmd::GET_CACHED_IMPORT:
    case IrCmd::CONCAT:
    case IrCmd::GET_UPVALUE:
    case IrCmd::SET_UPVALUE:
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
    case IrCmd::INTERRUPT:
    case IrCmd::CHECK_GC:
    case IrCmd::BARRIER_OBJ:
    case IrCmd::BARRIER_TABLE_BACK:
    case IrCmd::BARRIER_TABLE_FORWARD:
    case IrCmd::SET_SAVEDPC:
    case IrCmd::CLOSE_UPVALS:
    case IrCmd::CAPTURE:
    case IrCmd::SETLIST:
    case IrCmd::CALL:
    case IrCmd::RETURN:
    case IrCmd::FORGLOOP:
    case IrCmd::FORGLOOP_FALLBACK:
    case IrCmd::FORGPREP_XNEXT_FALLBACK:
    case IrCmd::COVERAGE:
    case IrCmd::FALLBACK_GETGLOBAL:
    case IrCmd::FALLBACK_SETGLOBAL:
    case IrCmd::FALLBACK_GETTABLEKS:
    case IrCmd::FALLBACK_SETTABLEKS:
    case IrCmd::FALLBACK_NAMECALL:
    case IrCmd::FALLBACK_PREPVARARGS:
    case IrCmd::FALLBACK_GETVARARGS:
        return IrValueKind::None;
    case IrCmd::NEWCLOSURE:
        return IrValueKind::Pointer;
    case IrCmd::FALLBACK_DUPCLOSURE:
    case IrCmd::FALLBACK_FORGPREP:
        return IrValueKind::None;
    case IrCmd::SUBSTITUTE:
        return IrValueKind::Unknown;
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
    case IrCmd::BYTESWAP_UINT:
        return IrValueKind::Int;
    case IrCmd::INVOKE_LIBM:
        return IrValueKind::Double;
    case IrCmd::GET_TYPE:
    case IrCmd::GET_TYPEOF:
        return IrValueKind::Pointer;
    case IrCmd::FINDUPVAL:
        return IrValueKind::Pointer;
    case IrCmd::BUFFER_READI8:
    case IrCmd::BUFFER_READU8:
    case IrCmd::BUFFER_READI16:
    case IrCmd::BUFFER_READU16:
    case IrCmd::BUFFER_READI32:
        return IrValueKind::Int;
    case IrCmd::BUFFER_WRITEI8:
    case IrCmd::BUFFER_WRITEI16:
    case IrCmd::BUFFER_WRITEI32:
    case IrCmd::BUFFER_WRITEF32:
    case IrCmd::BUFFER_WRITEF64:
        return IrValueKind::None;
    case IrCmd::BUFFER_READF32:
    case IrCmd::BUFFER_READF64:
        return IrValueKind::Double;
    }

    LUAU_UNREACHABLE();
}

static void removeInstUse(IrFunction& function, uint32_t instIdx)
{
    IrInst& inst = function.instructions[instIdx];

    CODEGEN_ASSERT(inst.useCount);
    inst.useCount--;

    if (inst.useCount == 0)
        kill(function, inst);
}

static void removeBlockUse(IrFunction& function, uint32_t blockIdx)
{
    IrBlock& block = function.blocks[blockIdx];

    CODEGEN_ASSERT(block.useCount);
    block.useCount--;

    // Entry block is never removed because is has an implicit use
    if (block.useCount == 0 && blockIdx != 0)
        kill(function, block);
}

void addUse(IrFunction& function, IrOp op)
{
    if (op.kind == IrOpKind::Inst)
        function.instructions[op.index].useCount++;
    else if (op.kind == IrOpKind::Block)
        function.blocks[op.index].useCount++;
}

void removeUse(IrFunction& function, IrOp op)
{
    if (op.kind == IrOpKind::Inst)
        removeInstUse(function, op.index);
    else if (op.kind == IrOpKind::Block)
        removeBlockUse(function, op.index);
}

bool isGCO(uint8_t tag)
{
    CODEGEN_ASSERT(tag < LUA_T_COUNT);

    // mirrors iscollectable(o) from VM/lobject.h
    return tag >= LUA_TSTRING;
}

bool isUserdataBytecodeType(uint8_t ty)
{
    return ty == LBC_TYPE_USERDATA || isCustomUserdataBytecodeType(ty);
}

bool isCustomUserdataBytecodeType(uint8_t ty)
{
    return ty >= LBC_TYPE_TAGGED_USERDATA_BASE && ty < LBC_TYPE_TAGGED_USERDATA_END;
}

HostMetamethod tmToHostMetamethod(int tm)
{
    switch (TMS(tm))
    {
    case TM_ADD:
        return HostMetamethod::Add;
    case TM_SUB:
        return HostMetamethod::Sub;
    case TM_MUL:
        return HostMetamethod::Mul;
    case TM_DIV:
        return HostMetamethod::Div;
    case TM_IDIV:
        return HostMetamethod::Idiv;
    case TM_MOD:
        return HostMetamethod::Mod;
    case TM_POW:
        return HostMetamethod::Pow;
    case TM_UNM:
        return HostMetamethod::Minus;
    case TM_EQ:
        return HostMetamethod::Equal;
    case TM_LT:
        return HostMetamethod::LessThan;
    case TM_LE:
        return HostMetamethod::LessEqual;
    case TM_LEN:
        return HostMetamethod::Length;
    case TM_CONCAT:
        return HostMetamethod::Concat;
    default:
        CODEGEN_ASSERT(!"invalid tag method for host");
        break;
    }

    return HostMetamethod::Add;
}

void kill(IrFunction& function, IrInst& inst)
{
    CODEGEN_ASSERT(inst.useCount == 0);

    inst.cmd = IrCmd::NOP;

    removeUse(function, inst.a);
    removeUse(function, inst.b);
    removeUse(function, inst.c);
    removeUse(function, inst.d);
    removeUse(function, inst.e);
    removeUse(function, inst.f);
    removeUse(function, inst.g);

    inst.a = {};
    inst.b = {};
    inst.c = {};
    inst.d = {};
    inst.e = {};
    inst.f = {};
    inst.g = {};
}

void kill(IrFunction& function, uint32_t start, uint32_t end)
{
    // Kill instructions in reverse order to avoid killing instructions that are still marked as used
    for (int i = int(end); i >= int(start); i--)
    {
        CODEGEN_ASSERT(unsigned(i) < function.instructions.size());
        IrInst& curr = function.instructions[i];

        if (curr.cmd == IrCmd::NOP)
            continue;

        kill(function, curr);
    }
}

void kill(IrFunction& function, IrBlock& block)
{
    CODEGEN_ASSERT(block.useCount == 0);

    block.kind = IrBlockKind::Dead;

    kill(function, block.start, block.finish);
    block.start = ~0u;
    block.finish = ~0u;
}

void replace(IrFunction& function, IrOp& original, IrOp replacement)
{
    // Add use before removing new one if that's the last one keeping target operand alive
    addUse(function, replacement);
    removeUse(function, original);

    original = replacement;
}

void replace(IrFunction& function, IrBlock& block, uint32_t instIdx, IrInst replacement)
{
    IrInst& inst = function.instructions[instIdx];

    // Add uses before removing new ones if those are the last ones keeping target operand alive
    addUse(function, replacement.a);
    addUse(function, replacement.b);
    addUse(function, replacement.c);
    addUse(function, replacement.d);
    addUse(function, replacement.e);
    addUse(function, replacement.f);
    addUse(function, replacement.g);

    // An extra reference is added so block will not remove itself
    block.useCount++;

    // If we introduced an earlier terminating instruction, all following instructions become dead
    if (!isBlockTerminator(inst.cmd) && isBlockTerminator(replacement.cmd))
    {
        // Block has has to be fully constructed before replacement is performed
        CODEGEN_ASSERT(block.finish != ~0u);
        CODEGEN_ASSERT(instIdx + 1 <= block.finish);

        kill(function, instIdx + 1, block.finish);

        block.finish = instIdx;
    }

    removeUse(function, inst.a);
    removeUse(function, inst.b);
    removeUse(function, inst.c);
    removeUse(function, inst.d);
    removeUse(function, inst.e);
    removeUse(function, inst.f);
    removeUse(function, inst.g);

    // Inherit existing use count (last use is skipped as it will be defined later)
    replacement.useCount = inst.useCount;

    inst = replacement;

    // Removing the earlier extra reference, this might leave the block without users without marking it as dead
    // This will have to be handled by separate dead code elimination
    block.useCount--;
}

void substitute(IrFunction& function, IrInst& inst, IrOp replacement)
{
    CODEGEN_ASSERT(!isBlockTerminator(inst.cmd));

    inst.cmd = IrCmd::SUBSTITUTE;

    addUse(function, replacement);

    removeUse(function, inst.a);
    removeUse(function, inst.b);
    removeUse(function, inst.c);
    removeUse(function, inst.d);
    removeUse(function, inst.e);
    removeUse(function, inst.f);
    removeUse(function, inst.g);

    inst.a = replacement;
    inst.b = {};
    inst.c = {};
    inst.d = {};
    inst.e = {};
    inst.f = {};
    inst.g = {};
}

void applySubstitutions(IrFunction& function, IrOp& op)
{
    if (op.kind == IrOpKind::Inst)
    {
        IrInst& src = function.instructions[op.index];

        if (src.cmd == IrCmd::SUBSTITUTE)
        {
            op.kind = src.a.kind;
            op.index = src.a.index;

            // If we substitute with the result of a different instruction, update the use count
            if (op.kind == IrOpKind::Inst)
            {
                IrInst& dst = function.instructions[op.index];
                CODEGEN_ASSERT(dst.cmd != IrCmd::SUBSTITUTE && "chained substitutions are not allowed");

                dst.useCount++;
            }

            CODEGEN_ASSERT(src.useCount > 0);
            src.useCount--;

            if (src.useCount == 0)
            {
                src.cmd = IrCmd::NOP;
                removeUse(function, src.a);
                src.a = {};
            }
        }
    }
}

void applySubstitutions(IrFunction& function, IrInst& inst)
{
    applySubstitutions(function, inst.a);
    applySubstitutions(function, inst.b);
    applySubstitutions(function, inst.c);
    applySubstitutions(function, inst.d);
    applySubstitutions(function, inst.e);
    applySubstitutions(function, inst.f);
    applySubstitutions(function, inst.g);
}

bool compare(double a, double b, IrCondition cond)
{
    // Note: redundant bool() casts work around invalid MSVC optimization that merges cases in this switch, violating IEEE754 comparison semantics
    switch (cond)
    {
    case IrCondition::Equal:
        return a == b;
    case IrCondition::NotEqual:
        return a != b;
    case IrCondition::Less:
        return a < b;
    case IrCondition::NotLess:
        return !bool(a < b);
    case IrCondition::LessEqual:
        return a <= b;
    case IrCondition::NotLessEqual:
        return !bool(a <= b);
    case IrCondition::Greater:
        return a > b;
    case IrCondition::NotGreater:
        return !bool(a > b);
    case IrCondition::GreaterEqual:
        return a >= b;
    case IrCondition::NotGreaterEqual:
        return !bool(a >= b);
    default:
        CODEGEN_ASSERT(!"Unsupported condition");
    }

    return false;
}

bool compare(int a, int b, IrCondition cond)
{
    switch (cond)
    {
    case IrCondition::Equal:
        return a == b;
    case IrCondition::NotEqual:
        return a != b;
    case IrCondition::Less:
        return a < b;
    case IrCondition::NotLess:
        return !(a < b);
    case IrCondition::LessEqual:
        return a <= b;
    case IrCondition::NotLessEqual:
        return !(a <= b);
    case IrCondition::Greater:
        return a > b;
    case IrCondition::NotGreater:
        return !(a > b);
    case IrCondition::GreaterEqual:
        return a >= b;
    case IrCondition::NotGreaterEqual:
        return !(a >= b);
    case IrCondition::UnsignedLess:
        return unsigned(a) < unsigned(b);
    case IrCondition::UnsignedLessEqual:
        return unsigned(a) <= unsigned(b);
    case IrCondition::UnsignedGreater:
        return unsigned(a) > unsigned(b);
    case IrCondition::UnsignedGreaterEqual:
        return unsigned(a) >= unsigned(b);
    default:
        CODEGEN_ASSERT(!"Unsupported condition");
    }

    return false;
}

void foldConstants(IrBuilder& build, IrFunction& function, IrBlock& block, uint32_t index)
{
    IrInst& inst = function.instructions[index];

    switch (inst.cmd)
    {
    case IrCmd::ADD_INT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            // We need to avoid signed integer overflow, but we also have to produce a result
            // So we add numbers as unsigned and use fixed-width integer types to force a two's complement evaluation
            int32_t lhs = function.intOp(inst.a);
            int32_t rhs = function.intOp(inst.b);
            int sum = int32_t(uint32_t(lhs) + uint32_t(rhs));

            substitute(function, inst, build.constInt(sum));
        }
        break;
    case IrCmd::SUB_INT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            // We need to avoid signed integer overflow, but we also have to produce a result
            // So we subtract numbers as unsigned and use fixed-width integer types to force a two's complement evaluation
            int32_t lhs = function.intOp(inst.a);
            int32_t rhs = function.intOp(inst.b);
            int sum = int32_t(uint32_t(lhs) - uint32_t(rhs));

            substitute(function, inst, build.constInt(sum));
        }
        break;
    case IrCmd::ADD_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(function.doubleOp(inst.a) + function.doubleOp(inst.b)));
        break;
    case IrCmd::SUB_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(function.doubleOp(inst.a) - function.doubleOp(inst.b)));
        break;
    case IrCmd::MUL_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(function.doubleOp(inst.a) * function.doubleOp(inst.b)));
        break;
    case IrCmd::DIV_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(function.doubleOp(inst.a) / function.doubleOp(inst.b)));
        break;
    case IrCmd::IDIV_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(luai_numidiv(function.doubleOp(inst.a), function.doubleOp(inst.b))));
        break;
    case IrCmd::MOD_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(luai_nummod(function.doubleOp(inst.a), function.doubleOp(inst.b))));
        break;
    case IrCmd::MIN_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            double a1 = function.doubleOp(inst.a);
            double a2 = function.doubleOp(inst.b);

            substitute(function, inst, build.constDouble(a1 < a2 ? a1 : a2));
        }
        break;
    case IrCmd::MAX_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            double a1 = function.doubleOp(inst.a);
            double a2 = function.doubleOp(inst.b);

            substitute(function, inst, build.constDouble(a1 > a2 ? a1 : a2));
        }
        break;
    case IrCmd::UNM_NUM:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(-function.doubleOp(inst.a)));
        break;
    case IrCmd::FLOOR_NUM:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(floor(function.doubleOp(inst.a))));
        break;
    case IrCmd::CEIL_NUM:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(ceil(function.doubleOp(inst.a))));
        break;
    case IrCmd::ROUND_NUM:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(round(function.doubleOp(inst.a))));
        break;
    case IrCmd::SQRT_NUM:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(sqrt(function.doubleOp(inst.a))));
        break;
    case IrCmd::ABS_NUM:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(fabs(function.doubleOp(inst.a))));
        break;
    case IrCmd::SIGN_NUM:
        if (inst.a.kind == IrOpKind::Constant)
        {
            double v = function.doubleOp(inst.a);

            substitute(function, inst, build.constDouble(v > 0.0 ? 1.0 : v < 0.0 ? -1.0 : 0.0));
        }
        break;
    case IrCmd::SELECT_NUM:
        if (inst.c.kind == IrOpKind::Constant && inst.d.kind == IrOpKind::Constant)
        {
            double c = function.doubleOp(inst.c);
            double d = function.doubleOp(inst.d);

            substitute(function, inst, c == d ? inst.b : inst.a);
        }
        break;
    case IrCmd::NOT_ANY:
        if (inst.a.kind == IrOpKind::Constant)
        {
            uint8_t a = function.tagOp(inst.a);

            if (a == LUA_TNIL)
                substitute(function, inst, build.constInt(1));
            else if (a != LUA_TBOOLEAN)
                substitute(function, inst, build.constInt(0));
            else if (inst.b.kind == IrOpKind::Constant)
                substitute(function, inst, build.constInt(function.intOp(inst.b) == 1 ? 0 : 1));
        }
        break;
    case IrCmd::CMP_INT:
        CODEGEN_ASSERT(FFlag::LuauCodeGenDirectBtest);

        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            if (compare(function.intOp(inst.a), function.intOp(inst.b), conditionOp(inst.c)))
                substitute(function, inst, build.constInt(1));
            else
                substitute(function, inst, build.constInt(0));
        }
        break;
    case IrCmd::CMP_TAG:
        CODEGEN_ASSERT(FFlag::LuauCodegenDirectCompare);

        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            substitute(function, inst, build.constInt(function.tagOp(inst.a) == function.tagOp(inst.b) ? 1 : 0));
        }
        break;
    case IrCmd::CMP_SPLIT_TVALUE:
        CODEGEN_ASSERT(FFlag::LuauCodegenDirectCompare);

        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            IrCondition cond = conditionOp(inst.e);

            if (cond == IrCondition::Equal)
            {
                if (function.tagOp(inst.a) != function.tagOp(inst.b))
                {
                    substitute(function, inst, build.constInt(0));
                }
                else if (inst.c.kind == IrOpKind::Constant && inst.d.kind == IrOpKind::Constant)
                {
                    if (function.tagOp(inst.a) == LUA_TBOOLEAN)
                        substitute(function, inst, build.constInt(compare(function.intOp(inst.c), function.intOp(inst.d), cond) ? 1 : 0));
                    else if (function.tagOp(inst.a) == LUA_TNUMBER)
                        substitute(function, inst, build.constInt(compare(function.doubleOp(inst.c), function.doubleOp(inst.d), cond) ? 1 : 0));
                    else
                        CODEGEN_ASSERT(!"unsupported type");
                }
            }
            else if (cond == IrCondition::NotEqual)
            {
                if (function.tagOp(inst.a) != function.tagOp(inst.b))
                {
                    substitute(function, inst, build.constInt(1));
                }
                else if (inst.c.kind == IrOpKind::Constant && inst.d.kind == IrOpKind::Constant)
                {
                    if (function.tagOp(inst.a) == LUA_TBOOLEAN)
                        substitute(function, inst, build.constInt(compare(function.intOp(inst.c), function.intOp(inst.d), cond) ? 1 : 0));
                    else if (function.tagOp(inst.a) == LUA_TNUMBER)
                        substitute(function, inst, build.constInt(compare(function.doubleOp(inst.c), function.doubleOp(inst.d), cond) ? 1 : 0));
                    else
                        CODEGEN_ASSERT(!"unsupported type");
                }
            }
            else
            {
                CODEGEN_ASSERT(!"unsupported condition");
            }
        }
        break;
    case IrCmd::JUMP_EQ_TAG:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            if (function.tagOp(inst.a) == function.tagOp(inst.b))
                replace(function, block, index, {IrCmd::JUMP, inst.c});
            else
                replace(function, block, index, {IrCmd::JUMP, inst.d});
        }
        break;
    case IrCmd::JUMP_CMP_INT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            if (compare(function.intOp(inst.a), function.intOp(inst.b), conditionOp(inst.c)))
                replace(function, block, index, {IrCmd::JUMP, inst.d});
            else
                replace(function, block, index, {IrCmd::JUMP, inst.e});
        }
        break;
    case IrCmd::JUMP_CMP_NUM:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            if (compare(function.doubleOp(inst.a), function.doubleOp(inst.b), conditionOp(inst.c)))
                replace(function, block, index, {IrCmd::JUMP, inst.d});
            else
                replace(function, block, index, {IrCmd::JUMP, inst.e});
        }
        break;
    case IrCmd::TRY_NUM_TO_INDEX:
        if (inst.a.kind == IrOpKind::Constant)
        {
            double value = function.doubleOp(inst.a);

            // To avoid undefined behavior of casting a value not representable in the target type, we check the range
            if (value >= INT_MIN && value <= INT_MAX)
            {
                int arrIndex = int(value);

                if (double(arrIndex) == value)
                    substitute(function, inst, build.constInt(arrIndex));
                else
                    replace(function, block, index, {IrCmd::JUMP, inst.b});
            }
            else
            {
                replace(function, block, index, {IrCmd::JUMP, inst.b});
            }
        }
        break;
    case IrCmd::INT_TO_NUM:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(double(function.intOp(inst.a))));
        break;
    case IrCmd::UINT_TO_NUM:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constDouble(double(unsigned(function.intOp(inst.a)))));
        break;
    case IrCmd::NUM_TO_INT:
        if (inst.a.kind == IrOpKind::Constant)
        {
            double value = function.doubleOp(inst.a);

            // To avoid undefined behavior of casting a value not representable in the target type, we check the range
            if (value >= INT_MIN && value <= INT_MAX)
                substitute(function, inst, build.constInt(int(value)));
        }
        break;
    case IrCmd::NUM_TO_UINT:
        if (inst.a.kind == IrOpKind::Constant)
        {
            double value = function.doubleOp(inst.a);

            // To avoid undefined behavior of casting a value not representable in the target type, we check the range
            if (value >= 0 && value <= UINT_MAX)
                substitute(function, inst, build.constInt(unsigned(function.doubleOp(inst.a))));
        }
        break;
    case IrCmd::CHECK_TAG:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            if (function.tagOp(inst.a) == function.tagOp(inst.b))
                kill(function, inst);
            else
                replace(function, block, index, {IrCmd::JUMP, inst.c}); // Shows a conflict in assumptions on this path
        }
        break;
    case IrCmd::CHECK_TRUTHY:
        if (inst.a.kind == IrOpKind::Constant)
        {
            if (function.tagOp(inst.a) == LUA_TNIL)
            {
                replace(function, block, index, {IrCmd::JUMP, inst.c}); // Shows a conflict in assumptions on this path
            }
            else if (function.tagOp(inst.a) == LUA_TBOOLEAN)
            {
                if (inst.b.kind == IrOpKind::Constant)
                {
                    if (function.intOp(inst.b) == 0)
                        replace(function, block, index, {IrCmd::JUMP, inst.c}); // Shows a conflict in assumptions on this path
                    else
                        kill(function, inst);
                }
            }
            else
            {
                kill(function, inst);
            }
        }
        break;
    case IrCmd::BITAND_UINT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            unsigned op1 = unsigned(function.intOp(inst.a));
            unsigned op2 = unsigned(function.intOp(inst.b));
            substitute(function, inst, build.constInt(op1 & op2));
        }
        else
        {
            if (inst.a.kind == IrOpKind::Constant && function.intOp(inst.a) == 0) // (0 & b) -> 0
                substitute(function, inst, build.constInt(0));
            else if (inst.a.kind == IrOpKind::Constant && function.intOp(inst.a) == -1) // (-1 & b) -> b
                substitute(function, inst, inst.b);
            else if (inst.b.kind == IrOpKind::Constant && function.intOp(inst.b) == 0) // (a & 0) -> 0
                substitute(function, inst, build.constInt(0));
            else if (inst.b.kind == IrOpKind::Constant && function.intOp(inst.b) == -1) // (a & -1) -> a
                substitute(function, inst, inst.a);
        }
        break;
    case IrCmd::BITXOR_UINT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            unsigned op1 = unsigned(function.intOp(inst.a));
            unsigned op2 = unsigned(function.intOp(inst.b));
            substitute(function, inst, build.constInt(op1 ^ op2));
        }
        else
        {
            if (inst.a.kind == IrOpKind::Constant && function.intOp(inst.a) == 0) // (0 ^ b) -> b
                substitute(function, inst, inst.b);
            else if (inst.a.kind == IrOpKind::Constant && function.intOp(inst.a) == -1) // (-1 ^ b) -> ~b
                replace(function, block, index, {IrCmd::BITNOT_UINT, inst.b});
            else if (inst.b.kind == IrOpKind::Constant && function.intOp(inst.b) == 0) // (a ^ 0) -> a
                substitute(function, inst, inst.a);
            else if (inst.b.kind == IrOpKind::Constant && function.intOp(inst.b) == -1) // (a ^ -1) -> ~a
                replace(function, block, index, {IrCmd::BITNOT_UINT, inst.a});
        }
        break;
    case IrCmd::BITOR_UINT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            unsigned op1 = unsigned(function.intOp(inst.a));
            unsigned op2 = unsigned(function.intOp(inst.b));
            substitute(function, inst, build.constInt(op1 | op2));
        }
        else
        {
            if (inst.a.kind == IrOpKind::Constant && function.intOp(inst.a) == 0) // (0 | b) -> b
                substitute(function, inst, inst.b);
            else if (inst.a.kind == IrOpKind::Constant && function.intOp(inst.a) == -1) // (-1 | b) -> -1
                substitute(function, inst, build.constInt(-1));
            else if (inst.b.kind == IrOpKind::Constant && function.intOp(inst.b) == 0) // (a | 0) -> a
                substitute(function, inst, inst.a);
            else if (inst.b.kind == IrOpKind::Constant && function.intOp(inst.b) == -1) // (a | -1) -> -1
                substitute(function, inst, build.constInt(-1));
        }
        break;
    case IrCmd::BITNOT_UINT:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constInt(~unsigned(function.intOp(inst.a))));
        break;
    case IrCmd::BITLSHIFT_UINT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            unsigned op1 = unsigned(function.intOp(inst.a));
            int op2 = function.intOp(inst.b);

            substitute(function, inst, build.constInt(op1 << (op2 & 31)));
        }
        else if (inst.b.kind == IrOpKind::Constant && function.intOp(inst.b) == 0)
        {
            substitute(function, inst, inst.a);
        }
        break;
    case IrCmd::BITRSHIFT_UINT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            unsigned op1 = unsigned(function.intOp(inst.a));
            int op2 = function.intOp(inst.b);

            substitute(function, inst, build.constInt(op1 >> (op2 & 31)));
        }
        else if (inst.b.kind == IrOpKind::Constant && function.intOp(inst.b) == 0)
        {
            substitute(function, inst, inst.a);
        }
        break;
    case IrCmd::BITARSHIFT_UINT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
        {
            int op1 = function.intOp(inst.a);
            int op2 = function.intOp(inst.b);

            // note: technically right shift of negative values is UB, but this behavior is getting defined in C++20 and all compilers do the
            // right (shift) thing.
            substitute(function, inst, build.constInt(op1 >> (op2 & 31)));
        }
        else if (inst.b.kind == IrOpKind::Constant && function.intOp(inst.b) == 0)
        {
            substitute(function, inst, inst.a);
        }
        break;
    case IrCmd::BITLROTATE_UINT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constInt(lrotate(unsigned(function.intOp(inst.a)), function.intOp(inst.b))));
        else if (inst.b.kind == IrOpKind::Constant && function.intOp(inst.b) == 0)
            substitute(function, inst, inst.a);
        break;
    case IrCmd::BITRROTATE_UINT:
        if (inst.a.kind == IrOpKind::Constant && inst.b.kind == IrOpKind::Constant)
            substitute(function, inst, build.constInt(rrotate(unsigned(function.intOp(inst.a)), function.intOp(inst.b))));
        else if (inst.b.kind == IrOpKind::Constant && function.intOp(inst.b) == 0)
            substitute(function, inst, inst.a);
        break;
    case IrCmd::BITCOUNTLZ_UINT:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constInt(countlz(unsigned(function.intOp(inst.a)))));
        break;
    case IrCmd::BITCOUNTRZ_UINT:
        if (inst.a.kind == IrOpKind::Constant)
            substitute(function, inst, build.constInt(countrz(unsigned(function.intOp(inst.a)))));
        break;
    default:
        break;
    }
}

uint32_t getNativeContextOffset(int bfid)
{
    switch (bfid)
    {
    case LBF_MATH_ACOS:
        return offsetof(NativeContext, libm_acos);
    case LBF_MATH_ASIN:
        return offsetof(NativeContext, libm_asin);
    case LBF_MATH_ATAN2:
        return offsetof(NativeContext, libm_atan2);
    case LBF_MATH_ATAN:
        return offsetof(NativeContext, libm_atan);
    case LBF_MATH_COSH:
        return offsetof(NativeContext, libm_cosh);
    case LBF_MATH_COS:
        return offsetof(NativeContext, libm_cos);
    case LBF_MATH_EXP:
        return offsetof(NativeContext, libm_exp);
    case LBF_MATH_LOG10:
        return offsetof(NativeContext, libm_log10);
    case LBF_MATH_LOG:
        return offsetof(NativeContext, libm_log);
    case LBF_MATH_SINH:
        return offsetof(NativeContext, libm_sinh);
    case LBF_MATH_SIN:
        return offsetof(NativeContext, libm_sin);
    case LBF_MATH_TANH:
        return offsetof(NativeContext, libm_tanh);
    case LBF_MATH_TAN:
        return offsetof(NativeContext, libm_tan);
    case LBF_MATH_FMOD:
        return offsetof(NativeContext, libm_fmod);
    case LBF_MATH_POW:
        return offsetof(NativeContext, libm_pow);
    case LBF_IR_MATH_LOG2:
        return offsetof(NativeContext, libm_log2);
    case LBF_MATH_LDEXP:
        return offsetof(NativeContext, libm_ldexp);
    default:
        CODEGEN_ASSERT(!"Unsupported bfid");
    }

    return 0;
}

void killUnusedBlocks(IrFunction& function)
{
    // Start from 1 as the first block is the entry block
    for (unsigned i = 1; i < function.blocks.size(); i++)
    {
        IrBlock& block = function.blocks[i];

        if (block.kind != IrBlockKind::Dead && block.useCount == 0)
            kill(function, block);
    }
}

std::vector<uint32_t> getSortedBlockOrder(IrFunction& function)
{
    std::vector<uint32_t> sortedBlocks;
    sortedBlocks.reserve(function.blocks.size());
    for (uint32_t i = 0; i < function.blocks.size(); i++)
        sortedBlocks.push_back(i);

    std::sort(
        sortedBlocks.begin(),
        sortedBlocks.end(),
        [&](uint32_t idxA, uint32_t idxB)
        {
            const IrBlock& a = function.blocks[idxA];
            const IrBlock& b = function.blocks[idxB];

            // Place fallback blocks at the end
            if ((a.kind == IrBlockKind::Fallback) != (b.kind == IrBlockKind::Fallback))
                return (a.kind == IrBlockKind::Fallback) < (b.kind == IrBlockKind::Fallback);

            // Try to order by instruction order
            if (a.sortkey != b.sortkey)
                return a.sortkey < b.sortkey;

            // Chains of blocks are merged together by having the same sort key and consecutive chain key
            return a.chainkey < b.chainkey;
        }
    );

    return sortedBlocks;
}

IrBlock& getNextBlock(IrFunction& function, const std::vector<uint32_t>& sortedBlocks, IrBlock& dummy, size_t i)
{
    for (size_t j = i + 1; j < sortedBlocks.size(); ++j)
    {
        IrBlock& block = function.blocks[sortedBlocks[j]];
        if (block.kind != IrBlockKind::Dead)
            return block;
    }

    return dummy;
}

} // namespace CodeGen
} // namespace Luau
