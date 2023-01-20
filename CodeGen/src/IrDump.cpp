// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrDump.h"

#include "IrUtils.h"

#include "lua.h"

#include <stdarg.h>

namespace Luau
{
namespace CodeGen
{

static const char* textForCondition[] = {
    "eq", "not_eq", "lt", "not_lt", "le", "not_le", "gt", "not_gt", "ge", "not_ge", "u_lt", "u_le", "u_gt", "u_ge"};
static_assert(sizeof(textForCondition) / sizeof(textForCondition[0]) == size_t(IrCondition::Count), "all conditions have to be covered");

const int kDetailsAlignColumn = 60;

LUAU_PRINTF_ATTR(2, 3)
static void append(std::string& result, const char* fmt, ...)
{
    char buf[256];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    result.append(buf);
}

static const char* getTagName(uint8_t tag)
{
    switch (tag)
    {
    case LUA_TNIL:
        return "tnil";
    case LUA_TBOOLEAN:
        return "tboolean";
    case LUA_TLIGHTUSERDATA:
        return "tlightuserdata";
    case LUA_TNUMBER:
        return "tnumber";
    case LUA_TVECTOR:
        return "tvector";
    case LUA_TSTRING:
        return "tstring";
    case LUA_TTABLE:
        return "ttable";
    case LUA_TFUNCTION:
        return "tfunction";
    case LUA_TUSERDATA:
        return "tuserdata";
    case LUA_TTHREAD:
        return "tthread";
    default:
        LUAU_UNREACHABLE();
    }
}

const char* getCmdName(IrCmd cmd)
{
    switch (cmd)
    {
    case IrCmd::NOP:
        return "NOP";
    case IrCmd::LOAD_TAG:
        return "LOAD_TAG";
    case IrCmd::LOAD_POINTER:
        return "LOAD_POINTER";
    case IrCmd::LOAD_DOUBLE:
        return "LOAD_DOUBLE";
    case IrCmd::LOAD_INT:
        return "LOAD_INT";
    case IrCmd::LOAD_TVALUE:
        return "LOAD_TVALUE";
    case IrCmd::LOAD_NODE_VALUE_TV:
        return "LOAD_NODE_VALUE_TV";
    case IrCmd::LOAD_ENV:
        return "LOAD_ENV";
    case IrCmd::GET_ARR_ADDR:
        return "GET_ARR_ADDR";
    case IrCmd::GET_SLOT_NODE_ADDR:
        return "GET_SLOT_NODE_ADDR";
    case IrCmd::STORE_TAG:
        return "STORE_TAG";
    case IrCmd::STORE_POINTER:
        return "STORE_POINTER";
    case IrCmd::STORE_DOUBLE:
        return "STORE_DOUBLE";
    case IrCmd::STORE_INT:
        return "STORE_INT";
    case IrCmd::STORE_TVALUE:
        return "STORE_TVALUE";
    case IrCmd::STORE_NODE_VALUE_TV:
        return "STORE_NODE_VALUE_TV";
    case IrCmd::ADD_INT:
        return "ADD_INT";
    case IrCmd::SUB_INT:
        return "SUB_INT";
    case IrCmd::ADD_NUM:
        return "ADD_NUM";
    case IrCmd::SUB_NUM:
        return "SUB_NUM";
    case IrCmd::MUL_NUM:
        return "MUL_NUM";
    case IrCmd::DIV_NUM:
        return "DIV_NUM";
    case IrCmd::MOD_NUM:
        return "MOD_NUM";
    case IrCmd::POW_NUM:
        return "POW_NUM";
    case IrCmd::UNM_NUM:
        return "UNM_NUM";
    case IrCmd::NOT_ANY:
        return "NOT_ANY";
    case IrCmd::JUMP:
        return "JUMP";
    case IrCmd::JUMP_IF_TRUTHY:
        return "JUMP_IF_TRUTHY";
    case IrCmd::JUMP_IF_FALSY:
        return "JUMP_IF_FALSY";
    case IrCmd::JUMP_EQ_TAG:
        return "JUMP_EQ_TAG";
    case IrCmd::JUMP_EQ_BOOLEAN:
        return "JUMP_EQ_BOOLEAN";
    case IrCmd::JUMP_EQ_POINTER:
        return "JUMP_EQ_POINTER";
    case IrCmd::JUMP_CMP_NUM:
        return "JUMP_CMP_NUM";
    case IrCmd::JUMP_CMP_STR:
        return "JUMP_CMP_STR";
    case IrCmd::JUMP_CMP_ANY:
        return "JUMP_CMP_ANY";
    case IrCmd::TABLE_LEN:
        return "TABLE_LEN";
    case IrCmd::NEW_TABLE:
        return "NEW_TABLE";
    case IrCmd::DUP_TABLE:
        return "DUP_TABLE";
    case IrCmd::NUM_TO_INDEX:
        return "NUM_TO_INDEX";
    case IrCmd::DO_ARITH:
        return "DO_ARITH";
    case IrCmd::DO_LEN:
        return "DO_LEN";
    case IrCmd::GET_TABLE:
        return "GET_TABLE";
    case IrCmd::SET_TABLE:
        return "SET_TABLE";
    case IrCmd::GET_IMPORT:
        return "GET_IMPORT";
    case IrCmd::CONCAT:
        return "CONCAT";
    case IrCmd::GET_UPVALUE:
        return "GET_UPVALUE";
    case IrCmd::SET_UPVALUE:
        return "SET_UPVALUE";
    case IrCmd::CHECK_TAG:
        return "CHECK_TAG";
    case IrCmd::CHECK_READONLY:
        return "CHECK_READONLY";
    case IrCmd::CHECK_NO_METATABLE:
        return "CHECK_NO_METATABLE";
    case IrCmd::CHECK_SAFE_ENV:
        return "CHECK_SAFE_ENV";
    case IrCmd::CHECK_ARRAY_SIZE:
        return "CHECK_ARRAY_SIZE";
    case IrCmd::CHECK_SLOT_MATCH:
        return "CHECK_SLOT_MATCH";
    case IrCmd::INTERRUPT:
        return "INTERRUPT";
    case IrCmd::CHECK_GC:
        return "CHECK_GC";
    case IrCmd::BARRIER_OBJ:
        return "BARRIER_OBJ";
    case IrCmd::BARRIER_TABLE_BACK:
        return "BARRIER_TABLE_BACK";
    case IrCmd::BARRIER_TABLE_FORWARD:
        return "BARRIER_TABLE_FORWARD";
    case IrCmd::SET_SAVEDPC:
        return "SET_SAVEDPC";
    case IrCmd::CLOSE_UPVALS:
        return "CLOSE_UPVALS";
    case IrCmd::CAPTURE:
        return "CAPTURE";
    case IrCmd::LOP_SETLIST:
        return "LOP_SETLIST";
    case IrCmd::LOP_NAMECALL:
        return "LOP_NAMECALL";
    case IrCmd::LOP_CALL:
        return "LOP_CALL";
    case IrCmd::LOP_RETURN:
        return "LOP_RETURN";
    case IrCmd::LOP_FASTCALL:
        return "LOP_FASTCALL";
    case IrCmd::LOP_FASTCALL1:
        return "LOP_FASTCALL1";
    case IrCmd::LOP_FASTCALL2:
        return "LOP_FASTCALL2";
    case IrCmd::LOP_FASTCALL2K:
        return "LOP_FASTCALL2K";
    case IrCmd::LOP_FORNPREP:
        return "LOP_FORNPREP";
    case IrCmd::LOP_FORNLOOP:
        return "LOP_FORNLOOP";
    case IrCmd::LOP_FORGLOOP:
        return "LOP_FORGLOOP";
    case IrCmd::LOP_FORGLOOP_FALLBACK:
        return "LOP_FORGLOOP_FALLBACK";
    case IrCmd::LOP_FORGPREP_NEXT:
        return "LOP_FORGPREP_NEXT";
    case IrCmd::LOP_FORGPREP_INEXT:
        return "LOP_FORGPREP_INEXT";
    case IrCmd::LOP_FORGPREP_XNEXT_FALLBACK:
        return "LOP_FORGPREP_XNEXT_FALLBACK";
    case IrCmd::LOP_AND:
        return "LOP_AND";
    case IrCmd::LOP_ANDK:
        return "LOP_ANDK";
    case IrCmd::LOP_OR:
        return "LOP_OR";
    case IrCmd::LOP_ORK:
        return "LOP_ORK";
    case IrCmd::LOP_COVERAGE:
        return "LOP_COVERAGE";
    case IrCmd::FALLBACK_GETGLOBAL:
        return "FALLBACK_GETGLOBAL";
    case IrCmd::FALLBACK_SETGLOBAL:
        return "FALLBACK_SETGLOBAL";
    case IrCmd::FALLBACK_GETTABLEKS:
        return "FALLBACK_GETTABLEKS";
    case IrCmd::FALLBACK_SETTABLEKS:
        return "FALLBACK_SETTABLEKS";
    case IrCmd::FALLBACK_NAMECALL:
        return "FALLBACK_NAMECALL";
    case IrCmd::FALLBACK_PREPVARARGS:
        return "FALLBACK_PREPVARARGS";
    case IrCmd::FALLBACK_GETVARARGS:
        return "FALLBACK_GETVARARGS";
    case IrCmd::FALLBACK_NEWCLOSURE:
        return "FALLBACK_NEWCLOSURE";
    case IrCmd::FALLBACK_DUPCLOSURE:
        return "FALLBACK_DUPCLOSURE";
    case IrCmd::FALLBACK_FORGPREP:
        return "FALLBACK_FORGPREP";
    }

    LUAU_UNREACHABLE();
}

const char* getBlockKindName(IrBlockKind kind)
{
    switch (kind)
    {
    case IrBlockKind::Bytecode:
        return "bb_bytecode";
    case IrBlockKind::Fallback:
        return "bb_fallback";
    case IrBlockKind::Internal:
        return "bb";
    }

    LUAU_UNREACHABLE();
}

void toString(IrToStringContext& ctx, IrInst inst, uint32_t index)
{
    append(ctx.result, "  ");

    // Instructions with a result display target virtual register
    if (hasResult(inst.cmd))
        append(ctx.result, "%%%u = ", index);

    ctx.result.append(getCmdName(inst.cmd));

    if (inst.a.kind != IrOpKind::None)
    {
        append(ctx.result, " ");
        toString(ctx, inst.a);
    }

    if (inst.b.kind != IrOpKind::None)
    {
        append(ctx.result, ", ");
        toString(ctx, inst.b);
    }

    if (inst.c.kind != IrOpKind::None)
    {
        append(ctx.result, ", ");
        toString(ctx, inst.c);
    }

    if (inst.d.kind != IrOpKind::None)
    {
        append(ctx.result, ", ");
        toString(ctx, inst.d);
    }

    if (inst.e.kind != IrOpKind::None)
    {
        append(ctx.result, ", ");
        toString(ctx, inst.e);
    }
}

void toString(IrToStringContext& ctx, IrOp op)
{
    switch (op.kind)
    {
    case IrOpKind::None:
        break;
    case IrOpKind::Constant:
        toString(ctx.result, ctx.constants[op.index]);
        break;
    case IrOpKind::Condition:
        LUAU_ASSERT(op.index < uint32_t(IrCondition::Count));
        ctx.result.append(textForCondition[op.index]);
        break;
    case IrOpKind::Inst:
        append(ctx.result, "%%%u", op.index);
        break;
    case IrOpKind::Block:
        append(ctx.result, "%s_%u", getBlockKindName(ctx.blocks[op.index].kind), op.index);
        break;
    case IrOpKind::VmReg:
        append(ctx.result, "R%u", op.index);
        break;
    case IrOpKind::VmConst:
        append(ctx.result, "K%u", op.index);
        break;
    case IrOpKind::VmUpvalue:
        append(ctx.result, "U%u", op.index);
        break;
    }
}

void toString(std::string& result, IrConst constant)
{
    switch (constant.kind)
    {
    case IrConstKind::Bool:
        append(result, constant.valueBool ? "true" : "false");
        break;
    case IrConstKind::Int:
        append(result, "%di", constant.valueInt);
        break;
    case IrConstKind::Uint:
        append(result, "%uu", constant.valueUint);
        break;
    case IrConstKind::Double:
        append(result, "%.17g", constant.valueDouble);
        break;
    case IrConstKind::Tag:
        result.append(getTagName(constant.valueTag));
        break;
    }
}

void toStringDetailed(IrToStringContext& ctx, IrInst inst, uint32_t index)
{
    size_t start = ctx.result.size();

    toString(ctx, inst, index);

    int pad = kDetailsAlignColumn - int(ctx.result.size() - start);

    if (pad > 0)
        ctx.result.append(pad, ' ');

    LUAU_ASSERT(inst.useCount == 0 || inst.lastUse != 0);

    if (inst.useCount == 0 && hasSideEffects(inst.cmd))
        append(ctx.result, "; %%%u, has side-effects\n", index);
    else
        append(ctx.result, "; useCount: %d, lastUse: %%%u\n", inst.useCount, inst.lastUse);
}

std::string dump(IrFunction& function)
{
    std::string result;
    IrToStringContext ctx{result, function.blocks, function.constants};

    for (size_t i = 0; i < function.blocks.size(); i++)
    {
        IrBlock& block = function.blocks[i];

        append(ctx.result, "%s_%u:\n", getBlockKindName(block.kind), unsigned(i));

        if (block.start == ~0u)
        {
            append(ctx.result, " *empty*\n\n");
            continue;
        }

        for (uint32_t index = block.start; true; index++)
        {
            LUAU_ASSERT(index < function.instructions.size());

            IrInst& inst = function.instructions[index];

            // Nop is used to replace dead instructions in-place, so it's not that useful to see them
            if (inst.cmd == IrCmd::NOP)
                continue;

            append(ctx.result, " ");
            toStringDetailed(ctx, inst, index);

            if (isBlockTerminator(inst.cmd))
            {
                append(ctx.result, "\n");
                break;
            }
        }
    }

    printf("%s\n", result.c_str());

    return result;
}

} // namespace CodeGen
} // namespace Luau
