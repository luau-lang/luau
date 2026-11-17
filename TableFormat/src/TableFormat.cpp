// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"

#include "Luau/Ast.h"
#include "Luau/Cst.h"
#include "Luau/HashUtil.h"
#include "Luau/Lexer.h"
#include "Luau/Parser.h"
#include "Luau/StringUtils.h"

#include "lua.h"

#include <algorithm>
#include <cmath>
#include <stdio.h>
#include <stdlib.h>
#include <unordered_set>
#include <utility>

namespace Luau::TableFormat
{

// Like the parser, stop reporting errors once there are too many to be useful.
constexpr size_t kMaxErrors = 100;

static std::string depthError()
{
    return format("Exceeded the maximum table nesting depth of %zu", kMaxDepth);
}

static void report(std::vector<ParseError>& errors, const Location& location, std::string message)
{
    if (errors.size() > kMaxErrors)
        return;

    errors.emplace_back(location, std::move(message));

    if (errors.size() == kMaxErrors)
        errors.emplace_back(location, format("Reached error limit (%zu)", kMaxErrors));
}

static void pushVector(lua_State* L, LUA_VECTOR_TYPE x, LUA_VECTOR_TYPE y, LUA_VECTOR_TYPE z)
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
        return HUGE_VAL;
    if (isGlobalMember(expr, "math", "nan"))
        return NAN;

    if (AstExprUnary* unary = expr->as<AstExprUnary>(); unary && unary->op == AstExprUnary::Op::Minus)
    {
        if (AstExprConstantNumber* number = unary->expr->as<AstExprConstantNumber>())
            return -number->value;
        if (isGlobalMember(unary->expr, "math", "huge"))
            return -HUGE_VAL;
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

    // Writes the shortest representation that reads back as the same double, as tostring does. Vector components are
    // read as doubles and narrowed to the build's vector component type, so writing them as doubles reads back exactly too.
    void writeNumber(double number)
    {
        if (std::isnan(number))
            out += "math.nan";
        else if (std::isinf(number))
            out += number > 0 ? "math.huge" : "-math.huge";
        else
        {
            size_t length = 0;
            lua_pushnumber(L, number);
            const char* text = lua_tolstring(L, -1, &length);
            out.append(text, length);
            lua_pop(L, 1);
        }
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
            writeNumber(double(v[i]));
            out += i < 2 ? ',' : ')';
        }
        return true;
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
        {
            size_t length = 0;
            const char* data = lua_tolstring(L, idx, &length);
            out += '"' + escape(std::string_view(data, length)) + '"';
            return true;
        }
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

        switch (lua_type(L, idx))
        {
        case LUA_TBOOLEAN:
        case LUA_TNUMBER:
        case LUA_TSTRING:
        case LUA_TVECTOR:
            break;
        default:
            return fail(format("Cannot use a value of type %s as a key", lua_typename(L, lua_type(L, idx))));
        }

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

        // Every item is followed by a comma, and the last is replaced with the closing brace.
        out += '{';

        // The array part: from 1 up to the first nil.
        int arrayLength = 0;
        while (lua_rawgeti(L, idx, arrayLength + 1) != LUA_TNIL)
        {
            bool written = writeValue(-1, depth);
            lua_pop(L, 1);
            if (!written)
                return false;

            out += ',';
            arrayLength++;
        }
        lua_pop(L, 1);

        // Every other key, in iteration order.
        lua_pushnil(L);
        while (lua_next(L, idx))
        {
            if (lua_type(L, -2) == LUA_TNUMBER)
            {
                double number = lua_tonumber(L, -2);
                if (number >= 1 && number <= arrayLength && number == std::floor(number))
                {
                    lua_pop(L, 1);
                    continue;
                }
            }

            bool written = writeKey(-2) && writeValue(-1, depth);
            lua_pop(L, 1);
            if (!written)
                return false;

            out += ',';
        }

        path.pop_back();
        if (out.back() == ',')
            out.back() = '}';
        else
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
        TableFormat::report(errors, location, std::move(message));
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

        LUA_VECTOR_TYPE components[3] = {};
        bool valid = true;

        for (size_t i = 0; i < call->args.size; i++)
        {
            if (std::optional<double> number = readNumber(call->args.data[i]))
                components[i] = LUA_VECTOR_TYPE(*number);
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

        const LUA_VECTOR_TYPE* v = lua_isvector(L, -1) ? lua_tovector(L, -1) : nullptr;
        // Check the type rather than lua_isnumber, which accepts strings like "nan" too.
        bool isNumber = lua_type(L, -1) == LUA_TNUMBER;
        if ((isNumber && std::isnan(lua_tonumber(L, -1))) || (v && (std::isnan(v[0]) || std::isnan(v[1]) || std::isnan(v[2]))))
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
            // Positional items have implicit keys, and `name = value` keys are constant strings, read like any other key.
            bool hasKey = true;
            if (item.kind == AstExprTable::Item::Kind::List)
                lua_pushnumber(L, ++arrayLength);
            else
                hasKey = readKey(item.key);

            bool hasValue = readValue(item.value, depth);

            if (!hasKey || !hasValue)
            {
                lua_pop(L, int(hasKey) + int(hasValue));
                continue;
            }

            lua_pushvalue(L, -2);
            if (lua_rawget(L, table) != LUA_TNIL)
            {
                report((item.key ? item.key : item.value)->location, "Duplicate key " + describeKey(L, -3));
                lua_pop(L, 3);
                continue;
            }

            lua_pop(L, 1);
            lua_rawset(L, table);
        }

        return true;
    }
};

// A key's identity, compared the way Luau compares table keys: numbers by value, so 0 and -0 are the same key, and vectors
// by component.
struct KeyId
{
    int type = LUA_TNIL;
    double number = 0;
    std::string_view string;
    LUA_VECTOR_TYPE vector[3] = {};

    bool operator==(const KeyId& other) const
    {
        return type == other.type && number == other.number && string == other.string && vector[0] == other.vector[0] &&
               vector[1] == other.vector[1] && vector[2] == other.vector[2];
    }
};

struct KeyIdHash
{
    size_t operator()(const KeyId& key) const
    {
        // Adding 0 turns -0 into 0, since equal keys must hash equally.
        size_t seed = std::hash<std::string_view>()(key.string);
        for (double number : {key.number, double(key.vector[0]), double(key.vector[1]), double(key.vector[2])})
            hashCombine(seed, std::hash<double>()(number + 0.0));
        return seed;
    }
};

// Writes a number as tostring does, without needing a VM: the shortest digits that read back as the same number, in fixed
// notation unless the decimal point is far from them.
static std::string formatNumber(double number)
{
    if (std::isnan(number))
        return "math.nan";
    if (std::isinf(number))
        return number > 0 ? "math.huge" : "-math.huge";

    char buffer[32];
    for (int precision = 0; precision < 17; precision++)
    {
        snprintf(buffer, sizeof(buffer), "%.*e", precision, number);
        if (strtod(buffer, nullptr) == number)
            break;
    }

    std::string_view scientific = buffer;
    bool negative = scientific[0] == '-';
    size_t e = scientific.find('e');
    int dot = atoi(buffer + e + 1) + 1;
    if (dot < -5 || dot > 21)
        return buffer;

    std::string digits;
    for (char c : scientific.substr(negative, e - negative))
        if (c != '.')
            digits += c;

    std::string result = negative ? "-" : "";
    if (dot <= 0)
        result += "0." + std::string(-dot, '0') + digits;
    else if (size_t(dot) >= digits.size())
        result += digits + std::string(dot - digits.size(), '0');
    else
        result += digits.substr(0, dot) + "." + digits.substr(dot);

    return result;
}

static std::string describeKey(const KeyId& key)
{
    switch (key.type)
    {
    case LUA_TBOOLEAN:
        return key.number ? "true" : "false";
    case LUA_TNUMBER:
        return formatNumber(key.number);
    case LUA_TSTRING:
        return '"' + escape(key.string) + '"';
    default:
        return "vector.create(" + formatNumber(key.vector[0]) + "," + formatNumber(key.vector[1]) + "," + formatNumber(key.vector[2]) + ")";
    }
}

// Checks a parsed file against what the Luau Table format allows, without a VM. Reader makes the same checks while it builds
// Luau values.
struct Validator
{
    std::vector<ParseError>& errors;

    void report(const Location& location, std::string message)
    {
        TableFormat::report(errors, location, std::move(message));
    }

    std::optional<KeyId> checkPrimitive(AstExpr* expr)
    {
        if (AstExprConstantBool* boolean = expr->as<AstExprConstantBool>())
            return KeyId{LUA_TBOOLEAN, double(boolean->value)};
        if (AstExprConstantString* string = expr->as<AstExprConstantString>())
            return KeyId{LUA_TSTRING, 0, std::string_view(string->value.data, string->value.size)};
        if (std::optional<double> number = readNumber(expr))
            return KeyId{LUA_TNUMBER, *number};

        if (AstExprInterpString* string = expr->as<AstExprInterpString>())
        {
            if (string->expressions.size == 0)
                return KeyId{LUA_TSTRING, 0, std::string_view(string->strings.data[0].data, string->strings.data[0].size)};

            report(expr->location, "Cannot use string interpolation in Luau Table");
            return std::nullopt;
        }

        if (AstExprCall* call = expr->as<AstExprCall>(); call && isGlobalMember(call->func, "vector", "create"))
            return checkVector(call);

        report(expr->location, "Luau Table only allows literal strings, booleans, numbers, vectors and tables");
        return std::nullopt;
    }

    std::optional<KeyId> checkVector(AstExprCall* call)
    {
        if (call->args.size != 2 && call->args.size != 3)
        {
            report(call->location, "vector.create must be given 2 or 3 numbers");
            return std::nullopt;
        }

        KeyId key{LUA_TVECTOR};
        bool valid = true;

        for (size_t i = 0; i < call->args.size; i++)
        {
            if (std::optional<double> number = readNumber(call->args.data[i]))
                key.vector[i] = LUA_VECTOR_TYPE(*number);
            else
            {
                report(call->args.data[i]->location, "vector.create must be given numbers");
                valid = false;
            }
        }

        return valid ? std::optional(key) : std::nullopt;
    }

    std::optional<KeyId> checkKey(AstExpr* expr)
    {
        if (expr->is<AstExprTable>())
        {
            report(expr->location, "Cannot use a table as a key");
            return std::nullopt;
        }

        std::optional<KeyId> key = checkPrimitive(expr);
        if (key && (std::isnan(key->number) || std::isnan(key->vector[0]) || std::isnan(key->vector[1]) || std::isnan(key->vector[2])))
        {
            report(expr->location, "Cannot use NaN as a key");
            return std::nullopt;
        }

        return key;
    }

    bool checkValue(AstExpr* expr, size_t depth)
    {
        if (AstExprTable* table = expr->as<AstExprTable>())
            return checkTable(table, depth + 1);

        return checkPrimitive(expr).has_value();
    }

    bool checkTable(AstExprTable* expr, size_t depth)
    {
        if (depth > kMaxDepth)
        {
            report(expr->location, depthError());
            return false;
        }

        std::unordered_set<KeyId, KeyIdHash> keys;
        keys.reserve(expr->items.size);
        double arrayLength = 0;

        for (const AstExprTable::Item& item : expr->items)
        {
            // Positional items have implicit keys, and `name = value` keys are constant strings, read like any other key.
            std::optional<KeyId> key = item.kind == AstExprTable::Item::Kind::List ? KeyId{LUA_TNUMBER, ++arrayLength} : checkKey(item.key);
            bool hasValue = checkValue(item.value, depth);

            if (key && hasValue && !keys.insert(*key).second)
                report((item.key ? item.key : item.value)->location, "Duplicate key " + describeKey(*key));
        }

        return true;
    }
};

// The offset of the start of each line, counting lines by '\n' as the lexer does.
static std::vector<size_t> lineStarts(std::string_view source)
{
    std::vector<size_t> starts = {0};
    for (size_t i = 0; i < source.size(); i++)
        if (source[i] == '\n')
            starts.push_back(i + 1);

    return starts;
}

static Position positionAt(const std::vector<size_t>& starts, size_t offset)
{
    size_t line = std::upper_bound(starts.begin(), starts.end(), offset) - starts.begin() - 1;
    return Position{unsigned(line), unsigned(offset - starts[line])};
}

static size_t offsetAt(const std::vector<size_t>& starts, Position position)
{
    return starts[position.line] + position.column;
}

static bool isComment(Lexeme::Type type)
{
    return type == Lexeme::Comment || type == Lexeme::BlockComment || type == Lexeme::BrokenComment;
}

// Whether the source from an offset onwards is a single `;`, which is ignored at the end of a file.
static bool isTrailingSemicolon(std::string_view source, size_t offset, Position position, AstNameTable& names)
{
    Lexer lexer(source.data() + offset, source.size() - offset, names, position);
    lexer.setSkipComments(true);

    return lexer.next().type == ';' && lexer.next().type == Lexeme::Eof;
}

template<typename Root>
static void takeParse(ParseNodeResult<AstExprTable>& result, Root& parsed)
{
    result.lines = parsed.lines;
    result.hotcomments = std::move(parsed.hotcomments);
    result.errors = std::move(parsed.errors);
    result.cstNodeMap = std::move(parsed.cstNodeMap);
}

static AstExpr* parseExpr(
    std::string_view source,
    AstNameTable& names,
    Allocator& allocator,
    const ParseOptions& options,
    ParseNodeResult<AstExprTable>& result
)
{
    ParseNodeResult<AstExpr> parsed = Parser::parseExpr(source.data(), source.size(), names, allocator, options);

    // A single `;` after the table is ignored, so if the parser reports it as an error, parse again without it.
    if (parsed.errors.size() == 1)
    {
        Position position = parsed.errors[0].getLocation().begin;
        size_t offset = offsetAt(lineStarts(source), position);

        if (isTrailingSemicolon(source, offset, position, names))
            parsed = Parser::parseExpr(source.data(), offset, names, allocator, options);
    }

    takeParse(result, parsed);
    return result.errors.empty() ? parsed.root : nullptr;
}

static AstExpr* parseReturn(
    std::string_view source,
    AstNameTable& names,
    Allocator& allocator,
    const ParseOptions& options,
    ParseNodeResult<AstExprTable>& result
)
{
    // The parser already allows a single `;` after `return`.
    ParseResult parsed = Parser::parse(source.data(), source.size(), names, allocator, options);
    takeParse(result, parsed);
    if (!result.errors.empty())
        return nullptr;

    AstStatReturn* ret = parsed.root->body.size == 1 ? parsed.root->body.data[0]->as<AstStatReturn>() : nullptr;
    if (!ret || ret->list.size != 1)
    {
        result.errors.emplace_back(parsed.root->location, "Expected a single table after 'return'");
        return nullptr;
    }

    return ret->list.data[0];
}

// Parses the root table: either the whole file, or the single value returned by the file.
static ParseNodeResult<AstExprTable> parseRoot(std::string_view source, AstNameTable& names, Allocator& allocator, const ParseOptions& options)
{
    ParseNodeResult<AstExprTable> result{};

    // The lexer treats a NUL byte as the end of the source, which would hide anything after it.
    if (size_t nul = source.find('\0'); nul != std::string_view::npos)
    {
        result.errors.emplace_back(Location{positionAt(lineStarts(source), nul), 1}, "NUL bytes must be escaped");
        return result;
    }

    Lexer lexer(source.data(), source.size(), names);
    lexer.setSkipComments(true);

    AstExpr* root = lexer.next().type == Lexeme::ReservedReturn ? parseReturn(source, names, allocator, options, result)
                                                                : parseExpr(source, names, allocator, options, result);
    if (!root)
        return result;

    result.root = root->as<AstExprTable>();
    if (!result.root)
        result.errors.emplace_back(root->location, "Expected a table");

    return result;
}

DeserializeResult deserialize(lua_State* L, std::string_view source)
{
    Allocator allocator;
    AstNameTable names(allocator);

    ParseNodeResult<AstExprTable> parsed = parseRoot(source, names, allocator, {});

    DeserializeResult result{std::move(parsed.errors)};
    if (!parsed.root)
        return result;

    int top = lua_gettop(L);
    Reader{L, result.errors}.readTable(parsed.root, 1);

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

// Parses the root table and checks it, leaving the root unset if the file is malformed.
static ParseNodeResult<AstExprTable> parseValid(std::string_view source, AstNameTable& names, Allocator& allocator, const ParseOptions& options)
{
    ParseNodeResult<AstExprTable> result = parseRoot(source, names, allocator, options);

    if (result.root)
        Validator{result.errors}.checkTable(result.root, 1);

    if (!result.errors.empty())
        result.root = nullptr;

    return result;
}

ParseNodeResult<AstExprTable> parse(std::string_view source, AstNameTable& names, Allocator& allocator)
{
    ParseOptions options;
    options.storeCstData = true;

    ParseNodeResult<AstExprTable> result = parseValid(source, names, allocator, options);

    // The parser drops comments after the last token it reads, and a trailing `;` is cut off before parsing, so take
    // comments from the whole source instead.
    Lexer lexer(source.data(), source.size(), names);
    for (Lexeme lexeme = lexer.next(false, true); lexeme.type != Lexeme::Eof; lexeme = lexer.next(false, true))
        if (isComment(lexeme.type))
            result.commentLocations.push_back(Comment{lexeme.type, lexeme.location});

    return result;
}

std::vector<Token> tokenize(std::string_view source)
{
    Allocator allocator;
    AstNameTable names(allocator);

    std::vector<size_t> starts = lineStarts(source);
    auto locationOf = [&](size_t begin, size_t end)
    {
        return Location{positionAt(starts, begin), positionAt(starts, end)};
    };

    // The lexer stops at a NUL byte, so only what comes before one is lexed.
    std::string_view lexed = source.substr(0, source.find('\0'));
    Lexer lexer(lexed.data(), lexed.size(), names);

    std::vector<Token> tokens;
    std::vector<Trivia> trivia; // since the last token
    size_t cursor = 0;

    auto addWhitespace = [&](size_t end)
    {
        while (cursor < end)
        {
            size_t newline = source.substr(0, end).find('\n', cursor);
            size_t pieceEnd = newline == std::string_view::npos ? end : newline + 1;
            trivia.push_back(Trivia{Trivia::Kind::Whitespace, locationOf(cursor, pieceEnd), source.substr(cursor, pieceEnd - cursor)});
            cursor = pieceEnd;
        }
    };

    auto addToken = [&](Lexeme::Type type, const Location& location, size_t begin, size_t end)
    {
        // The previous token takes the trivia up to and including the first newline after it.
        auto split = trivia.begin();
        if (!tokens.empty())
        {
            split = std::find_if(
                trivia.begin(),
                trivia.end(),
                [](const Trivia& item)
                {
                    return item.kind == Trivia::Kind::Whitespace && item.text.find('\n') != std::string_view::npos;
                }
            );
            if (split != trivia.end())
                ++split;

            tokens.back().trailingTrivia.assign(trivia.begin(), split);
        }

        tokens.push_back(Token{type, location, source.substr(begin, end - begin), std::vector<Trivia>(split, trivia.end()), {}});
        trivia.clear();
        cursor = end;
    };

    for (Lexeme lexeme = lexer.next(false, true); lexeme.type != Lexeme::Eof; lexeme = lexer.next(false, true))
    {
        size_t begin = offsetAt(starts, lexeme.location.begin);
        size_t end = offsetAt(starts, lexeme.location.end);
        addWhitespace(begin);

        if (isComment(lexeme.type))
        {
            Trivia::Kind kind = lexeme.type == Lexeme::Comment ? Trivia::Kind::SingleLineComment : Trivia::Kind::MultiLineComment;
            trivia.push_back(Trivia{kind, lexeme.location, source.substr(begin, end - begin)});
            cursor = end;
        }
        else
            addToken(lexeme.type, lexeme.location, begin, end);
    }

    addWhitespace(lexed.size());
    if (lexed.size() < source.size())
        trivia.push_back(Trivia{Trivia::Kind::Unlexed, locationOf(lexed.size(), source.size()), source.substr(lexed.size())});

    addToken(Lexeme::Eof, locationOf(source.size(), source.size()), source.size(), source.size());
    return tokens;
}

Location itemLocation(const AstExprTable& table, size_t index, const CstNodeMap& cst)
{
    LUAU_ASSERT(index < table.items.size);
    const AstExprTable::Item& item = table.items.data[index];

    CstNode* const* node = cst.find(const_cast<AstExprTable*>(&table));
    LUAU_ASSERT(node && (*node)->is<CstExprTable>());
    const CstExprTable::Item& cstItem = (*node)->as<CstExprTable>()->items.data[index];

    Position begin =
        item.kind == AstExprTable::Item::Kind::General ? cstItem.indexerOpenPosition : (item.key ? item.key : item.value)->location.begin;

    if (cstItem.separator == CstExprTable::Separator::Missing)
        return Location{begin, item.value->location.end};

    return Location{begin, Position{cstItem.separatorPosition.line, cstItem.separatorPosition.column + 1}};
}

ReplaceResult replace(std::string_view source, const std::vector<Replacement>& replacements)
{
    std::vector<size_t> starts = lineStarts(source);
    ReplaceResult result;

    auto checkedOffset = [&](Position position) -> std::optional<size_t>
    {
        if (position.line >= starts.size())
            return std::nullopt;

        size_t lineEnd = position.line + 1 < starts.size() ? starts[position.line + 1] - 1 : source.size();
        if (offsetAt(starts, position) > lineEnd)
            return std::nullopt;

        return offsetAt(starts, position);
    };

    struct Range
    {
        size_t begin, end;
        const Replacement* replacement;
    };

    std::vector<Range> ranges;
    for (const Replacement& replacement : replacements)
    {
        std::optional<size_t> begin = checkedOffset(replacement.location.begin);
        std::optional<size_t> end = checkedOffset(replacement.location.end);

        if (!begin || !end)
            result.errors.emplace_back(replacement.location, "Replacement is outside the source");
        else if (*begin > *end)
            result.errors.emplace_back(replacement.location, "Replacement range is reversed");
        else
            ranges.push_back(Range{*begin, *end, &replacement});
    }

    std::sort(
        ranges.begin(),
        ranges.end(),
        [](const Range& a, const Range& b)
        {
            return a.begin < b.begin;
        }
    );

    // Ranges overlap if they share any characters, or start at the same place, which includes two insertions at once.
    for (size_t i = 1; i < ranges.size(); i++)
        if (ranges[i].begin < ranges[i - 1].end || ranges[i].begin == ranges[i - 1].begin)
            result.errors.emplace_back(ranges[i].replacement->location, "Replacement overlaps another");

    if (!result.errors.empty())
        return result;

    std::string out;
    size_t cursor = 0;
    for (const Range& range : ranges)
    {
        out.append(source.substr(cursor, range.begin - cursor));
        out += range.replacement->text;
        cursor = range.end;
    }
    out.append(source.substr(cursor));

    Allocator allocator;
    AstNameTable names(allocator);
    result.errors = parseValid(out, names, allocator, {}).errors;
    if (result.errors.empty())
        result.source = std::move(out);

    return result;
}

} // namespace Luau::TableFormat
