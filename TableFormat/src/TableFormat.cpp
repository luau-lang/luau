// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"

#include "Luau/Ast.h"
#include "Luau/Common.h"
#include "Luau/Lexer.h"
#include "Luau/NumPrint.h"
#include "Luau/Parser.h"
#include "Luau/StringUtils.h"

#include "lua.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <utility>

namespace Luau::TableFormat
{

// Like the parser, stop reporting errors once there are too many to be useful.
constexpr size_t kMaxErrors = 100;

static std::string depthError()
{
    return format("Exceeded the maximum table nesting depth of %zu", kMaxDepth);
}

static void pushVector(lua_State* L, float x, float y, float z)
{
#if LUA_VECTOR_SIZE == 4
    lua_pushvector(L, x, y, z, 0);
#else
    lua_pushvector(L, x, y, z);
#endif
}

static bool isGlobalMember(AstExpr* expr, const char* global, const char* member)
{
    AstExprIndexName* index = expr->as<AstExprIndexName>();
    if (!index || index->op != '.' || index->index != member)
        return false;

    AstExprGlobal* object = index->expr->as<AstExprGlobal>();
    return object && object->name == global;
}

// Numbers can be literals, math.huge or math.nan, with unary minus allowed on literals and math.huge.
static std::optional<double> readNumber(AstExpr* expr)
{
    if (AstExprConstantNumber* number = expr->as<AstExprConstantNumber>())
        return number->value;
    if (isGlobalMember(expr, "math", "huge"))
        return std::numeric_limits<double>::infinity();
    if (isGlobalMember(expr, "math", "nan"))
        return std::numeric_limits<double>::quiet_NaN();

    if (AstExprUnary* unary = expr->as<AstExprUnary>(); unary && unary->op == AstExprUnary::Op::Minus)
    {
        if (AstExprConstantNumber* number = unary->expr->as<AstExprConstantNumber>())
            return -number->value;
        if (isGlobalMember(unary->expr, "math", "huge"))
            return -std::numeric_limits<double>::infinity();
    }

    return std::nullopt;
}

// Writes values from the stack, failing at the first one a Luau Table file can't hold.
struct Writer
{
    lua_State* L;
    std::string out;
    std::string error;

    // The tables being written, outermost first, to detect tables that contain themselves.
    std::vector<const void*> path;

    bool fail(std::string message)
    {
        error = std::move(message);
        return false;
    }

    void writeNumber(double number)
    {
        if (std::isnan(number))
            out += "math.nan";
        else if (std::isinf(number))
            out += number > 0 ? "math.huge" : "-math.huge";
        else
        {
            char buffer[LUAI_MAXNUM2STR];
            out.append(buffer, luai_num2str(buffer, number));
        }
    }

    bool writeComponent(LUA_VECTOR_TYPE component)
    {
        float narrowed = float(component);
        if (!std::isnan(component) && double(narrowed) != double(component))
            return fail("Cannot serialize a vector component that isn't a float");

        if (std::isnan(narrowed) || std::isinf(narrowed))
            writeNumber(double(narrowed));
        else
        {
            char buffer[LUAI_MAXNUM2STR];
            out.append(buffer, luai_num2strf(buffer, narrowed));
        }

        return true;
    }

    bool writeVector(int idx)
    {
        const LUA_VECTOR_TYPE* v = lua_tovector(L, idx);

#if LUA_VECTOR_SIZE == 4
        if (v[3] != 0)
            return fail("Cannot serialize a vector with a nonzero fourth component");
#endif

        out += "vector.create(";
        for (int i = 0; i < 3; i++)
        {
            if (i != 0)
                out += ',';
            if (!writeComponent(v[i]))
                return false;
        }
        out += ')';
        return true;
    }

    void writeString(int idx)
    {
        size_t length = 0;
        const char* data = lua_tolstring(L, idx, &length);
        out += '"';
        out += escape(std::string_view(data, length));
        out += '"';
    }

    // Writes anything that can be a key: a boolean, number, string or vector.
    bool writePrimitive(int idx)
    {
        switch (lua_type(L, idx))
        {
        case LUA_TBOOLEAN:
            out += lua_toboolean(L, idx) ? "true" : "false";
            return true;
        case LUA_TNUMBER:
            writeNumber(lua_tonumber(L, idx));
            return true;
        case LUA_TSTRING:
            writeString(idx);
            return true;
        case LUA_TVECTOR:
            return writeVector(idx);
        default:
            return fail(format("Cannot serialize a value of type %s", lua_typename(L, lua_type(L, idx))));
        }
    }

    bool writeValue(int idx, size_t depth)
    {
        if (lua_istable(L, idx))
            return writeTable(idx, depth + 1);

        return writePrimitive(idx);
    }

    bool writeKey(int idx)
    {
        if (lua_istable(L, idx))
            return fail("Cannot use a table as a key");

        if (lua_type(L, idx) == LUA_TSTRING)
        {
            size_t length = 0;
            const char* data = lua_tolstring(L, idx, &length);
            if (Lexer::isName(std::string_view(data, length)))
            {
                out.append(data, length);
                out += '=';
                return true;
            }
        }

        out += '[';
        if (!writePrimitive(idx))
            return false;
        out += "]=";
        return true;
    }

    // The root table has a depth of 1.
    bool writeTable(int idx, size_t depth)
    {
        idx = lua_absindex(L, idx);

        if (depth > kMaxDepth)
            return fail(depthError());

        const void* table = lua_topointer(L, idx);
        if (std::find(path.begin(), path.end(), table) != path.end())
            return fail("Cannot serialize a table that contains itself");

        path.push_back(table);
        lua_rawcheckstack(L, 4);

        out += '{';
        bool first = true;

        // The array part: from 1 up to the first nil.
        int arrayLength = 0;
        while (lua_rawgeti(L, idx, arrayLength + 1) != LUA_TNIL)
        {
            if (!std::exchange(first, false))
                out += ',';

            bool written = writeValue(-1, depth);
            lua_pop(L, 1);
            if (!written)
                return false;

            arrayLength++;
        }
        lua_pop(L, 1);

        // Every other key, collected so they can be written in a fixed order rather than iteration order.
        struct SortedKey
        {
            int rank; // booleans, numbers, strings, vectors
            double number = 0;
            std::string string;
            float vector[3] = {};
            int slot = 0;
        };

        std::vector<SortedKey> keys;
        lua_createtable(L, 0, 0);
        int slots = lua_gettop(L);

        lua_pushnil(L);
        while (lua_next(L, idx))
        {
            lua_pop(L, 1);

            int type = lua_type(L, -1);
            if (type == LUA_TNUMBER)
            {
                double number = lua_tonumber(L, -1);
                if (number >= 1 && number <= arrayLength && number == std::floor(number))
                    continue;
            }

            SortedKey key;
            switch (type)
            {
            case LUA_TBOOLEAN:
                key.rank = 0;
                key.number = lua_toboolean(L, -1);
                break;
            case LUA_TNUMBER:
                key.rank = 1;
                key.number = lua_tonumber(L, -1);
                break;
            case LUA_TSTRING:
            {
                key.rank = 2;
                size_t length = 0;
                const char* data = lua_tolstring(L, -1, &length);
                key.string.assign(data, length);
                break;
            }
            case LUA_TVECTOR:
            {
                key.rank = 3;
                const LUA_VECTOR_TYPE* v = lua_tovector(L, -1);
                for (int i = 0; i < 3; i++)
                    key.vector[i] = float(v[i]);
                break;
            }
            default:
                lua_pop(L, 2);
                return fail(
                    type == LUA_TTABLE ? "Cannot use a table as a key" : format("Cannot use a value of type %s as a key", lua_typename(L, type))
                );
            }

            key.slot = int(keys.size()) + 1;
            lua_pushvalue(L, -1);
            lua_rawseti(L, slots, key.slot);
            keys.push_back(std::move(key));
        }

        std::sort(
            keys.begin(),
            keys.end(),
            [](const SortedKey& a, const SortedKey& b)
            {
                if (a.rank != b.rank)
                    return a.rank < b.rank;
                if (a.rank == 2)
                    return a.string < b.string;
                if (a.rank == 3)
                    return std::lexicographical_compare(a.vector, a.vector + 3, b.vector, b.vector + 3);
                return a.number < b.number;
            }
        );

        for (const SortedKey& key : keys)
        {
            if (!std::exchange(first, false))
                out += ',';

            lua_rawgeti(L, slots, key.slot);
            lua_pushvalue(L, -1);
            lua_rawget(L, idx);

            bool written = writeKey(-2) && writeValue(-1, depth);
            lua_pop(L, 2);
            if (!written)
            {
                lua_pop(L, 1);
                return false;
            }
        }

        lua_pop(L, 1);
        path.pop_back();
        out += '}';
        return true;
    }
};

static std::string describeKey(lua_State* L, int idx)
{
    Writer writer{L};
    writer.writePrimitive(idx);
    return std::move(writer.out);
}

// Walks the AST of a parsed file, accepting only what the Luau Table format allows, and pushes the values it reads.
// Each read function pushes exactly one value if it succeeds, and nothing if it fails.
struct Reader
{
    lua_State* L;
    std::vector<ParseError>& errors;

    void report(const Location& location, std::string message)
    {
        if (errors.size() > kMaxErrors)
            return;

        errors.emplace_back(location, std::move(message));

        if (errors.size() == kMaxErrors)
            errors.emplace_back(location, format("Reached error limit (%zu)", kMaxErrors));
    }

    bool readPrimitive(AstExpr* expr)
    {
        if (AstExprConstantBool* boolean = expr->as<AstExprConstantBool>())
        {
            lua_pushboolean(L, boolean->value);
            return true;
        }

        if (AstExprConstantString* string = expr->as<AstExprConstantString>())
        {
            lua_pushlstring(L, string->value.data, string->value.size);
            return true;
        }

        if (std::optional<double> number = readNumber(expr))
        {
            lua_pushnumber(L, *number);
            return true;
        }

        if (AstExprInterpString* string = expr->as<AstExprInterpString>())
        {
            if (string->expressions.size == 0)
            {
                lua_pushlstring(L, string->strings.data[0].data, string->strings.data[0].size);
                return true;
            }

            report(expr->location, "Cannot use string interpolation in Luau Table");
            return false;
        }

        if (AstExprCall* call = expr->as<AstExprCall>(); call && isGlobalMember(call->func, "vector", "create"))
            return readVector(call);

        report(expr->location, "Luau Table only allows literal strings, booleans, numbers, vectors and tables");
        return false;
    }

    bool readVector(AstExprCall* call)
    {
        if (call->args.size != 2 && call->args.size != 3)
        {
            report(call->location, "vector.create must be given 2 or 3 numbers");
            return false;
        }

        float components[3] = {};
        bool valid = true;

        for (size_t i = 0; i < call->args.size; i++)
        {
            if (std::optional<double> number = readNumber(call->args.data[i]))
                components[i] = float(*number);
            else
            {
                report(call->args.data[i]->location, "vector.create must be given numbers");
                valid = false;
            }
        }

        if (valid)
            pushVector(L, components[0], components[1], components[2]);

        return valid;
    }

    bool readKey(AstExpr* expr)
    {
        if (expr->is<AstExprTable>())
        {
            report(expr->location, "Cannot use a table as a key");
            return false;
        }

        if (!readPrimitive(expr))
            return false;

        bool isNaN = false;
        if (lua_type(L, -1) == LUA_TNUMBER)
            isNaN = std::isnan(lua_tonumber(L, -1));
        else if (lua_type(L, -1) == LUA_TVECTOR)
        {
            const LUA_VECTOR_TYPE* v = lua_tovector(L, -1);
            isNaN = std::isnan(v[0]) || std::isnan(v[1]) || std::isnan(v[2]);
        }

        if (isNaN)
        {
            lua_pop(L, 1);
            report(expr->location, "Cannot use NaN as a key");
            return false;
        }

        return true;
    }

    bool readValue(AstExpr* expr, size_t depth)
    {
        if (AstExprTable* table = expr->as<AstExprTable>())
            return readTable(table, depth + 1);

        return readPrimitive(expr);
    }

    bool readTable(AstExprTable* expr, size_t depth)
    {
        if (depth > kMaxDepth)
        {
            report(expr->location, depthError());
            return false;
        }

        int listItems = 0;
        for (const AstExprTable::Item& item : expr->items)
            listItems += item.kind == AstExprTable::Item::Kind::List;

        lua_rawcheckstack(L, 4);
        lua_createtable(L, listItems, int(expr->items.size) - listItems);
        int table = lua_gettop(L);
        int arrayLength = 0;

        for (const AstExprTable::Item& item : expr->items)
        {
            if (item.kind == AstExprTable::Item::Kind::List)
            {
                int key = ++arrayLength;
                if (!readValue(item.value, depth))
                    continue;

                if (lua_rawgeti(L, table, key) != LUA_TNIL)
                {
                    lua_pop(L, 2);
                    lua_pushnumber(L, key);
                    report(item.value->location, "Duplicate key " + describeKey(L, -1));
                    lua_pop(L, 1);
                    continue;
                }

                lua_pop(L, 1);
                lua_rawseti(L, table, key);
                continue;
            }

            // `name = value` keys are constant strings, so they read like any other key.
            bool hasKey = readKey(item.key);
            bool hasValue = readValue(item.value, depth);

            if (!hasKey || !hasValue)
            {
                lua_pop(L, int(hasKey) + int(hasValue));
                continue;
            }

            lua_pushvalue(L, -2);
            if (lua_rawget(L, table) != LUA_TNIL)
            {
                report(item.key->location, "Duplicate key " + describeKey(L, -3));
                lua_pop(L, 3);
                continue;
            }

            lua_pop(L, 1);
            lua_rawset(L, table);
        }

        return true;
    }
};

static Position positionOf(std::string_view source, size_t offset)
{
    unsigned int line = 0;
    size_t lineStart = 0;
    for (size_t i = 0; i < offset; i++)
    {
        if (source[i] == '\n')
        {
            line++;
            lineStart = i + 1;
        }
    }

    return Position{line, unsigned(offset - lineStart)};
}

static size_t offsetOf(std::string_view source, Position position)
{
    size_t offset = 0;
    for (unsigned int line = 0; line < position.line; line++)
        offset = source.find('\n', offset) + 1;

    return offset + position.column;
}

// Whether the source from an offset onwards is a single `;`, which is ignored at the end of a file.
static bool isTrailingSemicolon(std::string_view source, size_t offset, Position position, AstNameTable& names)
{
    Lexer lexer(source.data() + offset, source.size() - offset, names, position);
    lexer.setSkipComments(true);

    return lexer.next().type == ';' && lexer.next().type == Lexeme::Eof;
}

static AstExpr* parseExpr(std::string_view source, AstNameTable& names, Allocator& allocator, std::vector<ParseError>& errors)
{
    ParseNodeResult<AstExpr> result = Parser::parseExpr(source.data(), source.size(), names, allocator);

    // A single `;` after the table is ignored, so if the parser reports it as an error, parse again without it.
    if (result.errors.size() == 1)
    {
        Position position = result.errors[0].getLocation().begin;
        size_t offset = offsetOf(source, position);

        if (isTrailingSemicolon(source, offset, position, names))
            result = Parser::parseExpr(source.data(), offset, names, allocator);
    }

    errors = std::move(result.errors);
    return errors.empty() ? result.root : nullptr;
}

static AstExpr* parseReturn(std::string_view source, AstNameTable& names, Allocator& allocator, std::vector<ParseError>& errors)
{
    // The parser already allows a single `;` after `return`.
    ParseResult result = Parser::parse(source.data(), source.size(), names, allocator);
    errors = std::move(result.errors);
    if (!errors.empty())
        return nullptr;

    AstStatReturn* ret = result.root->body.size == 1 ? result.root->body.data[0]->as<AstStatReturn>() : nullptr;
    if (!ret || ret->list.size != 1)
    {
        errors.emplace_back(result.root->location, "Expected a single table after 'return'");
        return nullptr;
    }

    return ret->list.data[0];
}

// Finds the root table: either the whole file, or the single value returned by the file.
static AstExprTable* parseRoot(std::string_view source, AstNameTable& names, Allocator& allocator, std::vector<ParseError>& errors)
{
    Lexer lexer(source.data(), source.size(), names);
    lexer.setSkipComments(true);

    AstExpr* root =
        lexer.next().type == Lexeme::ReservedReturn ? parseReturn(source, names, allocator, errors) : parseExpr(source, names, allocator, errors);
    if (!root)
        return nullptr;

    AstExprTable* table = root->as<AstExprTable>();
    if (!table)
        errors.emplace_back(root->location, "Expected a table");

    return table;
}

DeserializeResult deserialize(lua_State* L, std::string_view source)
{
    Allocator allocator;
    AstNameTable names(allocator);

    DeserializeResult result;

    // The lexer treats a NUL byte as the end of the source, which would hide anything after it.
    if (size_t nul = source.find('\0'); nul != std::string_view::npos)
    {
        Position position = positionOf(source, nul);
        result.errors.emplace_back(Location{position, 1}, "NUL bytes must be escaped");
        return result;
    }

    AstExprTable* root = parseRoot(source, names, allocator, result.errors);
    if (!root)
        return result;

    int top = lua_gettop(L);
    Reader{L, result.errors}.readTable(root, 1);

    if (!result.errors.empty())
        lua_settop(L, top);

    return result;
}

SerializeResult serialize(lua_State* L, int idx)
{
    if (!lua_istable(L, idx))
        return {std::nullopt, "Expected a table"};

    int top = lua_gettop(L);
    Writer writer{L};
    bool written = writer.writeTable(idx, 1);
    lua_settop(L, top);

    if (!written)
        return {std::nullopt, std::move(writer.error)};

    return {std::move(writer.out), {}};
}

} // namespace Luau::TableFormat
