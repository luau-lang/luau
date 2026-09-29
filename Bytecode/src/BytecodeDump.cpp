// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BytecodeDump.h"

namespace Luau
{
namespace Bytecode
{

const int kDetailsAlignColumn = 60;

LUAU_PRINTF_ATTR(2, 3)
void append(std::string& result, const char* fmt, ...)
{
    char buf[256];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    result.append(buf);
}

void padToDetailColumn(std::string& result, size_t lineStart)
{
    int pad = kDetailsAlignColumn - int(result.size() - lineStart);

    if (pad > 0)
        result.append(pad, ' ');
}

bool isPrintableStringConstant(const char* str, size_t len)
{
    for (size_t i = 0; i < len; ++i)
    {
        if (unsigned(str[i]) < ' ')
            return false;
    }

    return true;
}

int decomposeImportId(uint32_t ids, int32_t& id0, int32_t& id1, int32_t& id2)
{
    int count = ids >> 30;
    id0 = count > 0 ? int(ids >> 20) & 1023 : -1;
    id1 = count > 1 ? int(ids >> 10) & 1023 : -1;
    id2 = count > 2 ? int(ids) & 1023 : -1;
    return count;
}

const char* getBlockEdgeKindName(BcBlockEdgeKind kind)
{
    switch (kind)
    {
    case BcBlockEdgeKind::Branch:
        return "branch";
    case BcBlockEdgeKind::Fallthrough:
        return "fallthrough";
    case BcBlockEdgeKind::Loop:
        return "loop";
    }

    LUAU_ASSERT(!"unknown BcBlockEdgeKind");
    return "unknown";
}

const char* getLuauOpcodeName(LuauOpcode cmd)
{
    switch (cmd)
    {
    case LOP_LOADNIL:
        return "LOADNIL";
    case LOP_LOADB:
        return "LOADB";
    case LOP_LOADN:
        return "LOADN";
    case LOP_LOADK:
        return "LOADK";
    case LOP_MOVE:
        return "MOVE";
    case LOP_GETGLOBAL:
        return "GETGLOBAL";
    case LOP_SETGLOBAL:
        return "SETGLOBAL";
    case LOP_GETUPVAL:
        return "GETUPVAL";
    case LOP_SETUPVAL:
        return "SETUPVAL";
    case LOP_CLOSEUPVALS:
        return "CLOSEUPVALS";
    case LOP_GETIMPORT:
        return "GETIMPORT";
    case LOP_GETTABLE:
        return "GETTABLE";
    case LOP_SETTABLE:
        return "SETTABLE";
    case LOP_GETTABLEKS:
        return "GETTABLEKS";
    case LOP_SETTABLEKS:
        return "SETTABLEKS";
    case LOP_GETTABLEN:
        return "GETTABLEN";
    case LOP_SETTABLEN:
        return "SETTABLEN";
    case LOP_NEWCLOSURE:
        return "NEWCLOSURE";
    case LOP_NAMECALL:
        return "NAMECALL";
    case LOP_CALL:
        return "CALL";
    case LOP_CALLFB:
        return "CALLFB";
    case LOP_RETURN:
        return "RETURN";
    case LOP_JUMP:
        return "JUMP";
    case LOP_JUMPIF:
        return "JUMPIF";
    case LOP_JUMPIFNOT:
        return "JUMPIFNOT";
    case LOP_JUMPIFEQ:
        return "JUMPIFEQ";
    case LOP_JUMPIFLE:
        return "JUMPIFLE";
    case LOP_JUMPIFLT:
        return "JUMPIFLT";
    case LOP_JUMPIFNOTEQ:
        return "JUMPIFNOTEQ";
    case LOP_JUMPIFNOTLE:
        return "JUMPIFNOTLE";
    case LOP_JUMPIFNOTLT:
        return "JUMPIFNOTLT";
    case LOP_ADD:
        return "ADD";
    case LOP_SUB:
        return "SUB";
    case LOP_MUL:
        return "MUL";
    case LOP_DIV:
        return "DIV";
    case LOP_IDIV:
        return "IDIV";
    case LOP_MOD:
        return "MOD";
    case LOP_POW:
        return "POW";
    case LOP_ADDK:
        return "ADDK";
    case LOP_SUBK:
        return "SUBK";
    case LOP_MULK:
        return "MULK";
    case LOP_DIVK:
        return "DIVK";
    case LOP_IDIVK:
        return "IDIVK";
    case LOP_MODK:
        return "MODK";
    case LOP_POWK:
        return "POWK";
    case LOP_SUBRK:
        return "SUBRK";
    case LOP_DIVRK:
        return "DIVRK";
    case LOP_AND:
        return "AND";
    case LOP_OR:
        return "OR";
    case LOP_ANDK:
        return "ANDK";
    case LOP_ORK:
        return "ORK";
    case LOP_CONCAT:
        return "CONCAT";
    case LOP_NOT:
        return "NOT";
    case LOP_MINUS:
        return "MINUS";
    case LOP_LENGTH:
        return "LENGTH";
    case LOP_NEWTABLE:
        return "NEWTABLE";
    case LOP_DUPTABLE:
        return "DUPTABLE";
    case LOP_SETLIST:
        return "SETLIST";
    case LOP_FORNPREP:
        return "FORNPREP";
    case LOP_FORNLOOP:
        return "FORNLOOP";
    case LOP_FORGPREP:
        return "FORGPREP";
    case LOP_FORGLOOP:
        return "FORGLOOP";
    case LOP_FORGPREP_INEXT:
        return "FORGPREP_INEXT";
    case LOP_FORGPREP_NEXT:
        return "FORGPREP_NEXT";
    case LOP_GETVARARGS:
        return "GETVARARGS";
    case LOP_DUPCLOSURE:
        return "DUPCLOSURE";
    case LOP_PREPVARARGS:
        return "PREPVARARGS";
    case LOP_BREAK:
        return "BREAK";
    case LOP_JUMPBACK:
        return "JUMPBACK";
    case LOP_LOADKX:
        return "LOADKX";
    case LOP_JUMPX:
        return "JUMPX";
    case LOP_FASTCALL:
        return "FASTCALL";
    case LOP_FASTCALL1:
        return "FASTCALL1";
    case LOP_FASTCALL2:
        return "FASTCALL2";
    case LOP_FASTCALL2K:
        return "FASTCALL2K";
    case LOP_FASTCALL3:
        return "FASTCALL3";
    case LOP_COVERAGE:
        return "COVERAGE";
    case LOP_CAPTURE:
        return "CAPTURE";
    case LOP_JUMPXEQKNIL:
        return "JUMPXEQKNIL";
    case LOP_JUMPXEQKB:
        return "JUMPXEQKB";
    case LOP_JUMPXEQKN:
        return "JUMPXEQKN";
    case LOP_JUMPXEQKS:
        return "JUMPXEQKS";
    case LOP_GETUDATAKS:
        return "GETUDATAKS";
    case LOP_SETUDATAKS:
        return "SETUDATAKS";
    case LOP_NAMECALLUDATA:
        return "NAMECALLUDATA";
    case LOP_NEWCLASSMEMBER:
        return "NEWCLASSMEMBER";
    case LOP_CMPPROTO:
        return "CMPPROTO";
    case LOP_FASTPCALL:
        return "FASTPCALL";
    case LOP_NEWCLASS:
        return "NEWCLASS";
    default:
        LUAU_ASSERT(!"Unsupported opcode");
        return "unknown";
    }
}

void toString(ToStringContext& ctx, const BcImm& imm)
{
    switch (imm.kind)
    {
    case BcImmKind::Boolean:
        append(ctx.result, "%s", imm.valueBoolean ? "true" : "false");
        break;
    case BcImmKind::Int:
        append(ctx.result, "%d", imm.valueInt);
        break;
    case BcImmKind::Import:
        append(ctx.result, "%u", imm.valueImport);
        break;
    }
}

void toString(ToStringContext& ctx, BcFunction<BcVmConst>& function, const BcVmConst& data, bool detailed)
{
    switch (data.kind)
    {
    case BcVmConstKind::Nil:
        append(ctx.result, "nil");
        break;
    case BcVmConstKind::Boolean:
        append(ctx.result, "%s", data.valueBoolean ? "true" : "false");
        break;
    case BcVmConstKind::Number:
        append(ctx.result, "%.17g", data.valueNumber);
        break;
    case BcVmConstKind::Vectorf:
        if (data.valueVectorf[3] == 0.0f)
            append(ctx.result, "%.9g, %.9g, %.9g", data.valueVectorf[0], data.valueVectorf[1], data.valueVectorf[2]);
        else
            append(ctx.result, "%.9g, %.9g, %.9g, %.9g", data.valueVectorf[0], data.valueVectorf[1], data.valueVectorf[2], data.valueVectorf[3]);
        break;
    case BcVmConstKind::Vectord:
        if (data.valueVectord[3] == 0.0)
            append(ctx.result, "%.17g, %.17g, %.17g", data.valueVectord[0], data.valueVectord[1], data.valueVectord[2]);
        else
            append(ctx.result, "%.17g, %.17g, %.17g, %.17g", data.valueVectord[0], data.valueVectord[1], data.valueVectord[2], data.valueVectord[3]);
        break;
    case BcVmConstKind::String:
        if (isPrintableStringConstant(data.valueString.data(), data.valueString.length()))
        {
            if (data.valueString.length() < 32)
                append(ctx.result, "'%.*s'", int(data.valueString.length()), data.valueString.data());
            else
                append(ctx.result, "'%.*s'...", 32, data.valueString.data());
        }
        else
        {
            append(ctx.result, "'");

            for (size_t i = 0; i < data.valueString.length() && i < 32; ++i)
            {
                if (unsigned(data.valueString[i]) < ' ')
                    append(ctx.result, "\\x%02X", uint8_t(data.valueString[i]));
                else
                    append(ctx.result, "%c", data.valueString[i]);
            }

            if (data.valueString.length() >= 32)
                append(ctx.result, "'...");
            else
                append(ctx.result, "'");
        }
        break;
    case BcVmConstKind::Import:
    {
        int32_t id0 = -1, id1 = -1, id2 = -1;
        if (int count = decomposeImportId(data.valueImport, id0, id1, id2))
        {
            {
                const BcVmConst& id = function.constants[id0];
                LUAU_ASSERT(id.kind == BcVmConstKind::String);
                append(ctx.result, "%.*s", int(id.valueString.length()), id.valueString.data());
            }

            if (count > 1)
            {
                const BcVmConst& id = function.constants[id1];
                LUAU_ASSERT(id.kind == BcVmConstKind::String);
                append(ctx.result, ".%.*s", int(id.valueString.length()), id.valueString.data());
            }

            if (count > 2)
            {
                const BcVmConst& id = function.constants[id2];
                LUAU_ASSERT(id.kind == BcVmConstKind::String);
                append(ctx.result, ".%.*s", int(id.valueString.length()), id.valueString.data());
            }
        }
    }
    break;
    case BcVmConstKind::Table:
        // TODO: detailed mode
        append(ctx.result, "{...}");
        break;
    case BcVmConstKind::Closure:
        // TODO: detailed mode
        append(ctx.result, "%u", data.valueClosure);
        break;
    case BcVmConstKind::Integer:
        append(ctx.result, "%lld", (long long)data.valueInteger);
        break;
    case BcVmConstKind::ClassShape:
        const BytecodeBuilder::ClassShape& cs = function.classShapes[data.valueClassShape];
        const BcVmConst& className = function.constants[cs.className];
        LUAU_ASSERT(className.kind == BcVmConstKind::String);
        std::string_view str = className.valueString;
        LUAU_ASSERT(isPrintableStringConstant(className.valueString.data(), className.valueString.length()));
        append(ctx.result, "class %.*s (props: %zu, methods: %zu)", int(str.length()), str.data(), cs.propertyNames.size(), cs.methodNames.size());

        // TODO: detailed mode
        break;
    }
}

void toString(ToStringContext& ctx, BcFunction<TValue*>& function, TValue* data, bool detailed)
{
    // TODO: runtime representation like in dumpConstant from RuntimeBytecodeBuilder.h
}

std::string dump(BcFunction<BcVmConst>& function)
{
    std::string result = toString(function, true);

    printf("%s\n", result.c_str());

    return result;
}

std::string dump(BcFunction<TValue*>& function)
{
    std::string result = toString(function, true);

    printf("%s\n", result.c_str());

    return result;
}

} // namespace Bytecode
} // namespace Luau
