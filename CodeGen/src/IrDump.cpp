// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrDump.h"

#include "Luau/IrUtils.h"

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

static void padToDetailColumn(std::string& result, size_t lineStart)
{
    int pad = kDetailsAlignColumn - int(result.size() - lineStart);

    if (pad > 0)
        result.append(pad, ' ');
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
    case LUA_TBUFFER:
        return "tbuffer";
    case LUA_TPROTO:
        return "tproto";
    case LUA_TUPVAL:
        return "tupval";
    case LUA_TDEADKEY:
        return "tdeadkey";
    default:
        LUAU_ASSERT(!"Unknown type tag");
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
    case IrCmd::LOAD_ENV:
        return "LOAD_ENV";
    case IrCmd::GET_ARR_ADDR:
        return "GET_ARR_ADDR";
    case IrCmd::GET_SLOT_NODE_ADDR:
        return "GET_SLOT_NODE_ADDR";
    case IrCmd::GET_HASH_NODE_ADDR:
        return "GET_HASH_NODE_ADDR";
    case IrCmd::GET_CLOSURE_UPVAL_ADDR:
        return "GET_CLOSURE_UPVAL_ADDR";
    case IrCmd::STORE_TAG:
        return "STORE_TAG";
    case IrCmd::STORE_POINTER:
        return "STORE_POINTER";
    case IrCmd::STORE_DOUBLE:
        return "STORE_DOUBLE";
    case IrCmd::STORE_INT:
        return "STORE_INT";
    case IrCmd::STORE_VECTOR:
        return "STORE_VECTOR";
    case IrCmd::STORE_TVALUE:
        return "STORE_TVALUE";
    case IrCmd::STORE_SPLIT_TVALUE:
        return "STORE_SPLIT_TVALUE";
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
    case IrCmd::IDIV_NUM:
        return "IDIV_NUM";
    case IrCmd::MOD_NUM:
        return "MOD_NUM";
    case IrCmd::MIN_NUM:
        return "MIN_NUM";
    case IrCmd::MAX_NUM:
        return "MAX_NUM";
    case IrCmd::UNM_NUM:
        return "UNM_NUM";
    case IrCmd::FLOOR_NUM:
        return "FLOOR_NUM";
    case IrCmd::CEIL_NUM:
        return "CEIL_NUM";
    case IrCmd::ROUND_NUM:
        return "ROUND_NUM";
    case IrCmd::SQRT_NUM:
        return "SQRT_NUM";
    case IrCmd::ABS_NUM:
        return "ABS_NUM";
    case IrCmd::NOT_ANY:
        return "NOT_ANY";
    case IrCmd::CMP_ANY:
        return "CMP_ANY";
    case IrCmd::JUMP:
        return "JUMP";
    case IrCmd::JUMP_IF_TRUTHY:
        return "JUMP_IF_TRUTHY";
    case IrCmd::JUMP_IF_FALSY:
        return "JUMP_IF_FALSY";
    case IrCmd::JUMP_EQ_TAG:
        return "JUMP_EQ_TAG";
    case IrCmd::JUMP_CMP_INT:
        return "JUMP_CMP_INT";
    case IrCmd::JUMP_EQ_POINTER:
        return "JUMP_EQ_POINTER";
    case IrCmd::JUMP_CMP_NUM:
        return "JUMP_CMP_NUM";
    case IrCmd::JUMP_FORN_LOOP_COND:
        return "JUMP_FORN_LOOP_COND";
    case IrCmd::JUMP_SLOT_MATCH:
        return "JUMP_SLOT_MATCH";
    case IrCmd::TABLE_LEN:
        return "TABLE_LEN";
    case IrCmd::TABLE_SETNUM:
        return "TABLE_SETNUM";
    case IrCmd::STRING_LEN:
        return "STRING_LEN";
    case IrCmd::NEW_TABLE:
        return "NEW_TABLE";
    case IrCmd::DUP_TABLE:
        return "DUP_TABLE";
    case IrCmd::TRY_NUM_TO_INDEX:
        return "TRY_NUM_TO_INDEX";
    case IrCmd::TRY_CALL_FASTGETTM:
        return "TRY_CALL_FASTGETTM";
    case IrCmd::INT_TO_NUM:
        return "INT_TO_NUM";
    case IrCmd::UINT_TO_NUM:
        return "UINT_TO_NUM";
    case IrCmd::NUM_TO_INT:
        return "NUM_TO_INT";
    case IrCmd::NUM_TO_UINT:
        return "NUM_TO_UINT";
    case IrCmd::ADJUST_STACK_TO_REG:
        return "ADJUST_STACK_TO_REG";
    case IrCmd::ADJUST_STACK_TO_TOP:
        return "ADJUST_STACK_TO_TOP";
    case IrCmd::FASTCALL:
        return "FASTCALL";
    case IrCmd::INVOKE_FASTCALL:
        return "INVOKE_FASTCALL";
    case IrCmd::CHECK_FASTCALL_RES:
        return "CHECK_FASTCALL_RES";
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
    case IrCmd::CHECK_TRUTHY:
        return "CHECK_TRUTHY";
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
    case IrCmd::CHECK_NODE_NO_NEXT:
        return "CHECK_NODE_NO_NEXT";
    case IrCmd::CHECK_NODE_VALUE:
        return "CHECK_NODE_VALUE";
    case IrCmd::CHECK_BUFFER_LEN:
        return "CHECK_BUFFER_LEN";
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
    case IrCmd::SETLIST:
        return "SETLIST";
    case IrCmd::CALL:
        return "CALL";
    case IrCmd::RETURN:
        return "RETURN";
    case IrCmd::FORGLOOP:
        return "FORGLOOP";
    case IrCmd::FORGLOOP_FALLBACK:
        return "FORGLOOP_FALLBACK";
    case IrCmd::FORGPREP_XNEXT_FALLBACK:
        return "FORGPREP_XNEXT_FALLBACK";
    case IrCmd::COVERAGE:
        return "COVERAGE";
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
    case IrCmd::NEWCLOSURE:
        return "NEWCLOSURE";
    case IrCmd::FALLBACK_DUPCLOSURE:
        return "FALLBACK_DUPCLOSURE";
    case IrCmd::FALLBACK_FORGPREP:
        return "FALLBACK_FORGPREP";
    case IrCmd::SUBSTITUTE:
        return "SUBSTITUTE";
    case IrCmd::BITAND_UINT:
        return "BITAND_UINT";
    case IrCmd::BITXOR_UINT:
        return "BITXOR_UINT";
    case IrCmd::BITOR_UINT:
        return "BITOR_UINT";
    case IrCmd::BITNOT_UINT:
        return "BITNOT_UINT";
    case IrCmd::BITLSHIFT_UINT:
        return "BITLSHIFT_UINT";
    case IrCmd::BITRSHIFT_UINT:
        return "BITRSHIFT_UINT";
    case IrCmd::BITARSHIFT_UINT:
        return "BITARSHIFT_UINT";
    case IrCmd::BITLROTATE_UINT:
        return "BITLROTATE_UINT";
    case IrCmd::BITRROTATE_UINT:
        return "BITRROTATE_UINT";
    case IrCmd::BITCOUNTLZ_UINT:
        return "BITCOUNTLZ_UINT";
    case IrCmd::BITCOUNTRZ_UINT:
        return "BITCOUNTRZ_UINT";
    case IrCmd::BYTESWAP_UINT:
        return "BYTESWAP_UINT";
    case IrCmd::INVOKE_LIBM:
        return "INVOKE_LIBM";
    case IrCmd::GET_TYPE:
        return "GET_TYPE";
    case IrCmd::GET_TYPEOF:
        return "GET_TYPEOF";
    case IrCmd::FINDUPVAL:
        return "FINDUPVAL";
    case IrCmd::BUFFER_READI8:
        return "BUFFER_READI8";
    case IrCmd::BUFFER_READU8:
        return "BUFFER_READU8";
    case IrCmd::BUFFER_WRITEI8:
        return "BUFFER_WRITEI8";
    case IrCmd::BUFFER_READI16:
        return "BUFFER_READI16";
    case IrCmd::BUFFER_READU16:
        return "BUFFER_READU16";
    case IrCmd::BUFFER_WRITEI16:
        return "BUFFER_WRITEI16";
    case IrCmd::BUFFER_READI32:
        return "BUFFER_READI32";
    case IrCmd::BUFFER_WRITEI32:
        return "BUFFER_WRITEI32";
    case IrCmd::BUFFER_READF32:
        return "BUFFER_READF32";
    case IrCmd::BUFFER_WRITEF32:
        return "BUFFER_WRITEF32";
    case IrCmd::BUFFER_READF64:
        return "BUFFER_READF64";
    case IrCmd::BUFFER_WRITEF64:
        return "BUFFER_WRITEF64";
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
    case IrBlockKind::Linearized:
        return "bb_linear";
    case IrBlockKind::Dead:
        return "dead";
    }

    LUAU_UNREACHABLE();
}

void toString(IrToStringContext& ctx, const IrInst& inst, uint32_t index)
{
    append(ctx.result, "  ");

    // Instructions with a result display target virtual register
    if (hasResult(inst.cmd))
        append(ctx.result, "%%%u = ", index);

    ctx.result.append(getCmdName(inst.cmd));

    auto checkOp = [&ctx](IrOp op, const char* sep) {
        if (op.kind != IrOpKind::None)
        {
            ctx.result.append(sep);
            toString(ctx, op);
        }
    };

    checkOp(inst.a, " ");
    checkOp(inst.b, ", ");
    checkOp(inst.c, ", ");
    checkOp(inst.d, ", ");
    checkOp(inst.e, ", ");
    checkOp(inst.f, ", ");
}

void toString(IrToStringContext& ctx, const IrBlock& block, uint32_t index)
{
    append(ctx.result, "%s_%u", getBlockKindName(block.kind), index);
}

void toString(IrToStringContext& ctx, IrOp op)
{
    switch (op.kind)
    {
    case IrOpKind::None:
        break;
    case IrOpKind::Undef:
        append(ctx.result, "undef");
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
        append(ctx.result, "R%d", vmRegOp(op));
        break;
    case IrOpKind::VmConst:
        append(ctx.result, "K%d", vmConstOp(op));
        break;
    case IrOpKind::VmUpvalue:
        append(ctx.result, "U%d", vmUpvalueOp(op));
        break;
    case IrOpKind::VmExit:
        append(ctx.result, "exit(%d)", vmExitOp(op));
        break;
    }
}

void toString(std::string& result, IrConst constant)
{
    switch (constant.kind)
    {
    case IrConstKind::Int:
        append(result, "%di", constant.valueInt);
        break;
    case IrConstKind::Uint:
        append(result, "%uu", constant.valueUint);
        break;
    case IrConstKind::Double:
        if (constant.valueDouble != constant.valueDouble)
            append(result, "nan");
        else
            append(result, "%.17g", constant.valueDouble);
        break;
    case IrConstKind::Tag:
        result.append(getTagName(constant.valueTag));
        break;
    }
}

static void appendBlockSet(IrToStringContext& ctx, BlockIteratorWrapper blocks)
{
    bool comma = false;

    for (uint32_t target : blocks)
    {
        if (comma)
            append(ctx.result, ", ");
        comma = true;

        toString(ctx, ctx.blocks[target], target);
    }
}

static void appendRegisterSet(IrToStringContext& ctx, const RegisterSet& rs, const char* separator)
{
    bool comma = false;

    for (size_t i = 0; i < rs.regs.size(); i++)
    {
        if (rs.regs.test(i))
        {
            if (comma)
                ctx.result.append(separator);
            comma = true;

            append(ctx.result, "R%d", int(i));
        }
    }

    if (rs.varargSeq)
    {
        if (comma)
            ctx.result.append(separator);

        append(ctx.result, "R%d...", rs.varargStart);
    }
}

static RegisterSet getJumpTargetExtraLiveIn(IrToStringContext& ctx, const IrBlock& block, uint32_t blockIdx, const IrInst& inst)
{
    RegisterSet extraRs;

    if (blockIdx >= ctx.cfg.in.size())
        return extraRs;

    const RegisterSet& defRs = ctx.cfg.in[blockIdx];

    // Find first block argument, for guard instructions (isNonTerminatingJump), that's the first and only one
    LUAU_ASSERT(isNonTerminatingJump(inst.cmd));
    IrOp op = inst.a;

    if (inst.b.kind == IrOpKind::Block)
        op = inst.b;
    else if (inst.c.kind == IrOpKind::Block)
        op = inst.c;
    else if (inst.d.kind == IrOpKind::Block)
        op = inst.d;
    else if (inst.e.kind == IrOpKind::Block)
        op = inst.e;
    else if (inst.f.kind == IrOpKind::Block)
        op = inst.f;

    if (op.kind == IrOpKind::Block && op.index < ctx.cfg.in.size())
    {
        const RegisterSet& inRs = ctx.cfg.in[op.index];

        extraRs.regs = inRs.regs & ~defRs.regs;

        if (inRs.varargSeq)
            requireVariadicSequence(extraRs, defRs, inRs.varargStart);
    }

    return extraRs;
}

void toStringDetailed(IrToStringContext& ctx, const IrBlock& block, uint32_t blockIdx, const IrInst& inst, uint32_t instIdx, bool includeUseInfo)
{
    size_t start = ctx.result.size();

    toString(ctx, inst, instIdx);

    if (includeUseInfo)
    {
        padToDetailColumn(ctx.result, start);

        if (inst.useCount == 0 && hasSideEffects(inst.cmd))
        {
            if (isNonTerminatingJump(inst.cmd))
            {
                RegisterSet extraRs = getJumpTargetExtraLiveIn(ctx, block, blockIdx, inst);

                if (extraRs.regs.any() || extraRs.varargSeq)
                {
                    append(ctx.result, "; %%%u, extra in: ", instIdx);
                    appendRegisterSet(ctx, extraRs, ", ");
                    ctx.result.append("\n");
                }
                else
                {
                    append(ctx.result, "; %%%u\n", instIdx);
                }
            }
            else
            {
                append(ctx.result, "; %%%u\n", instIdx);
            }
        }
        else
        {
            append(ctx.result, "; useCount: %d, lastUse: %%%u\n", inst.useCount, inst.lastUse);
        }
    }
    else
    {
        ctx.result.append("\n");
    }
}

void toStringDetailed(IrToStringContext& ctx, const IrBlock& block, uint32_t index, bool includeUseInfo)
{
    // Report captured registers for entry block
    if (block.useCount == 0 && block.kind != IrBlockKind::Dead && ctx.cfg.captured.regs.any())
    {
        append(ctx.result, "; captured regs: ");
        appendRegisterSet(ctx, ctx.cfg.captured, ", ");
        append(ctx.result, "\n\n");
    }

    size_t start = ctx.result.size();

    toString(ctx, block, index);
    append(ctx.result, ":");

    if (includeUseInfo)
    {
        padToDetailColumn(ctx.result, start);

        append(ctx.result, "; useCount: %d\n", block.useCount);
    }
    else
    {
        ctx.result.append("\n");
    }

    // Predecessor list
    if (index < ctx.cfg.predecessorsOffsets.size())
    {
        BlockIteratorWrapper pred = predecessors(ctx.cfg, index);

        if (!pred.empty())
        {
            append(ctx.result, "; predecessors: ");

            appendBlockSet(ctx, pred);
            append(ctx.result, "\n");
        }
    }

    // Successor list
    if (index < ctx.cfg.successorsOffsets.size())
    {
        BlockIteratorWrapper succ = successors(ctx.cfg, index);

        if (!succ.empty())
        {
            append(ctx.result, "; successors: ");

            appendBlockSet(ctx, succ);
            append(ctx.result, "\n");
        }
    }

    // Live-in VM regs
    if (index < ctx.cfg.in.size())
    {
        const RegisterSet& in = ctx.cfg.in[index];

        if (in.regs.any() || in.varargSeq)
        {
            append(ctx.result, "; in regs: ");
            appendRegisterSet(ctx, in, ", ");
            append(ctx.result, "\n");
        }
    }

    // Live-out VM regs
    if (index < ctx.cfg.out.size())
    {
        const RegisterSet& out = ctx.cfg.out[index];

        if (out.regs.any() || out.varargSeq)
        {
            append(ctx.result, "; out regs: ");
            appendRegisterSet(ctx, out, ", ");
            append(ctx.result, "\n");
        }
    }
}

std::string toString(const IrFunction& function, bool includeUseInfo)
{
    std::string result;
    IrToStringContext ctx{result, function.blocks, function.constants, function.cfg};

    for (size_t i = 0; i < function.blocks.size(); i++)
    {
        const IrBlock& block = function.blocks[i];

        if (block.kind == IrBlockKind::Dead)
            continue;

        toStringDetailed(ctx, block, uint32_t(i), includeUseInfo);

        if (block.start == ~0u)
        {
            append(ctx.result, " *empty*\n\n");
            continue;
        }

        // To allow dumping blocks that are still being constructed, we can't rely on terminator and need a bounds check
        for (uint32_t index = block.start; index <= block.finish && index < uint32_t(function.instructions.size()); index++)
        {
            const IrInst& inst = function.instructions[index];

            // Skip pseudo instructions unless they are still referenced
            if (isPseudo(inst.cmd) && inst.useCount == 0)
                continue;

            append(ctx.result, " ");
            toStringDetailed(ctx, block, uint32_t(i), inst, index, includeUseInfo);
        }

        append(ctx.result, "\n");
    }

    return result;
}

std::string dump(const IrFunction& function)
{
    std::string result = toString(function, /* includeUseInfo */ true);

    printf("%s\n", result.c_str());

    return result;
}

static void appendLabelRegset(IrToStringContext& ctx, const std::vector<RegisterSet>& regSets, size_t blockIdx, const char* name)
{
    if (blockIdx < regSets.size())
    {
        const RegisterSet& rs = regSets[blockIdx];

        if (rs.regs.any() || rs.varargSeq)
        {
            append(ctx.result, "|{%s|", name);
            appendRegisterSet(ctx, rs, "|");
            append(ctx.result, "}");
        }
    }
}

static void appendBlocks(IrToStringContext& ctx, const IrFunction& function, bool includeInst, bool includeIn, bool includeOut, bool includeDef)
{
    for (size_t i = 0; i < function.blocks.size(); i++)
    {
        const IrBlock& block = function.blocks[i];

        append(ctx.result, "b%u [", unsigned(i));

        if (block.kind == IrBlockKind::Fallback)
            append(ctx.result, "style=filled;fillcolor=salmon;");
        else if (block.kind == IrBlockKind::Bytecode)
            append(ctx.result, "style=filled;fillcolor=palegreen;");

        append(ctx.result, "label=\"{");
        toString(ctx, block, uint32_t(i));

        if (includeIn)
            appendLabelRegset(ctx, ctx.cfg.in, i, "in");

        if (includeInst && block.start != ~0u)
        {
            for (uint32_t instIdx = block.start; instIdx <= block.finish; instIdx++)
            {
                const IrInst& inst = function.instructions[instIdx];

                // Skip pseudo instructions unless they are still referenced
                if (isPseudo(inst.cmd) && inst.useCount == 0)
                    continue;

                append(ctx.result, "|");
                toString(ctx, inst, instIdx);
            }
        }

        if (includeDef)
            appendLabelRegset(ctx, ctx.cfg.def, i, "def");

        if (includeOut)
            appendLabelRegset(ctx, ctx.cfg.out, i, "out");

        append(ctx.result, "}\"];\n");
    }
}

std::string toDot(const IrFunction& function, bool includeInst)
{
    std::string result;
    IrToStringContext ctx{result, function.blocks, function.constants, function.cfg};

    append(ctx.result, "digraph CFG {\n");
    append(ctx.result, "node[shape=record]\n");

    appendBlocks(ctx, function, includeInst, /* includeIn */ true, /* includeOut */ true, /* includeDef */ true);

    for (size_t i = 0; i < function.blocks.size(); i++)
    {
        const IrBlock& block = function.blocks[i];

        if (block.start == ~0u)
            continue;

        for (uint32_t instIdx = block.start; instIdx != ~0u && instIdx <= block.finish; instIdx++)
        {
            const IrInst& inst = function.instructions[instIdx];

            auto checkOp = [&](IrOp op) {
                if (op.kind == IrOpKind::Block)
                {
                    if (function.blocks[op.index].kind != IrBlockKind::Fallback)
                        append(ctx.result, "b%u -> b%u [weight=10];\n", unsigned(i), op.index);
                    else
                        append(ctx.result, "b%u -> b%u;\n", unsigned(i), op.index);
                }
            };

            checkOp(inst.a);
            checkOp(inst.b);
            checkOp(inst.c);
            checkOp(inst.d);
            checkOp(inst.e);
            checkOp(inst.f);
        }
    }

    append(ctx.result, "}\n");

    return result;
}

std::string toDotCfg(const IrFunction& function)
{
    std::string result;
    IrToStringContext ctx{result, function.blocks, function.constants, function.cfg};

    append(ctx.result, "digraph CFG {\n");
    append(ctx.result, "node[shape=record]\n");

    appendBlocks(ctx, function, /* includeInst */ false, /* includeIn */ false, /* includeOut */ false, /* includeDef */ true);

    for (size_t i = 0; i < function.blocks.size() && i < ctx.cfg.successorsOffsets.size(); i++)
    {
        BlockIteratorWrapper succ = successors(ctx.cfg, unsigned(i));

        for (uint32_t target : succ)
            append(ctx.result, "b%u -> b%u;\n", unsigned(i), target);
    }

    append(ctx.result, "}\n");

    return result;
}

std::string toDotDjGraph(const IrFunction& function)
{
    std::string result;
    IrToStringContext ctx{result, function.blocks, function.constants, function.cfg};

    append(ctx.result, "digraph CFG {\n");

    for (size_t i = 0; i < ctx.blocks.size(); i++)
    {
        const IrBlock& block = ctx.blocks[i];

        append(ctx.result, "b%u [", unsigned(i));

        if (block.kind == IrBlockKind::Fallback)
            append(ctx.result, "style=filled;fillcolor=salmon;");
        else if (block.kind == IrBlockKind::Bytecode)
            append(ctx.result, "style=filled;fillcolor=palegreen;");

        append(ctx.result, "label=\"");
        toString(ctx, block, uint32_t(i));
        append(ctx.result, "\"];\n");
    }

    // Layer by depth in tree
    uint32_t depth = 0;
    bool found = true;

    while (found)
    {
        found = false;

        append(ctx.result, "{rank = same;");
        for (size_t i = 0; i < ctx.cfg.domOrdering.size(); i++)
        {
            if (ctx.cfg.domOrdering[i].depth == depth)
            {
                append(ctx.result, "b%u;", unsigned(i));
                found = true;
            }
        }
        append(ctx.result, "}\n");

        depth++;
    }

    for (size_t i = 0; i < ctx.cfg.domChildrenOffsets.size(); i++)
    {
        BlockIteratorWrapper dom = domChildren(ctx.cfg, unsigned(i));

        for (uint32_t target : dom)
            append(ctx.result, "b%u -> b%u;\n", unsigned(i), target);

        // Join edges are all successor edges that do not strongly dominate
        BlockIteratorWrapper succ = successors(ctx.cfg, unsigned(i));

        for (uint32_t successor : succ)
        {
            bool found = false;

            for (uint32_t target : dom)
            {
                if (target == successor)
                {
                    found = true;
                    break;
                }
            }

            if (!found)
                append(ctx.result, "b%u -> b%u [style=dotted];\n", unsigned(i), successor);
        }
    }

    append(ctx.result, "}\n");

    return result;
}

std::string dumpDot(const IrFunction& function, bool includeInst)
{
    std::string result = toDot(function, includeInst);

    printf("%s\n", result.c_str());

    return result;
}

} // namespace CodeGen
} // namespace Luau
