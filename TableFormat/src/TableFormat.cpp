// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"

#include "Luau/Ast.h"
#include "Luau/Common.h"
#include "Luau/HashUtil.h"
#include "Luau/Lexer.h"
#include "Luau/Parser.h"
#include "Luau/StringUtils.h"

#include <cmath>
#include <limits>
#include <stdio.h>
#include <stdlib.h>
#include <utility>

LUAU_FASTINT(LuauParseErrorLimit)

namespace Luau::TableFormat
{

static std::string describeKey(const Key& key);

static std::string depthError()
{
    return format("Exceeded the maximum table nesting depth of %zu", kMaxDepth);
}

static bool isValidKey(const Key& key)
{
    if (const double* number = key.get_if<double>())
        return !std::isnan(*number);
    if (const Vector* vector = key.get_if<Vector>())
        return !std::isnan(vector->x) && !std::isnan(vector->y) && !std::isnan(vector->z);
    return true;
}

// Returns where a key falls in the array part, if it does.
static std::optional<size_t> arrayIndex(const Key& key, size_t arraySize)
{
    const double* number = key.get_if<double>();
    if (!number || !(*number >= 1.0 && *number <= double(arraySize)) || *number != std::floor(*number))
        return std::nullopt;

    return size_t(*number) - 1;
}

// 0 and -0 are equal, so they must hash equally too.
template<typename T>
static size_t hashNumber(T number)
{
    return std::hash<T>{}(number == 0 ? T(0) : number);
}

size_t KeyHash::operator()(const Key& key) const
{
    if (const bool* boolean = key.get_if<bool>())
        return std::hash<bool>{}(*boolean);
    if (const double* number = key.get_if<double>())
        return hashNumber(*number);
    if (const std::string* string = key.get_if<std::string>())
        return std::hash<std::string>{}(*string);

    const Vector* vector = key.get_if<Vector>();
    LUAU_ASSERT(vector);

    size_t seed = hashNumber(vector->x);
    hashCombine(seed, hashNumber(vector->y));
    hashCombine(seed, hashNumber(vector->z));
    return seed;
}

bool KeyEqual::operator()(const Key& lhs, const Key& rhs) const
{
    if (lhs.index() != rhs.index())
        return false;

    if (const bool* boolean = lhs.get_if<bool>())
        return *boolean == *rhs.get_if<bool>();
    if (const double* number = lhs.get_if<double>())
        return *number == *rhs.get_if<double>();
    if (const std::string* string = lhs.get_if<std::string>())
        return *string == *rhs.get_if<std::string>();

    const Vector* a = lhs.get_if<Vector>();
    const Vector* b = rhs.get_if<Vector>();
    LUAU_ASSERT(a && b);

    return a->x == b->x && a->y == b->y && a->z == b->z;
}

Table::Table(const Table& other)
{
    // Copying a nested table recursively would recurse once per level of nesting, so instead we copy one table at a
    // time, leaving nested tables empty and filling them in later.
    std::vector<std::pair<const Table*, Table*>> pending = {{&other, this}};

    while (!pending.empty())
    {
        auto [from, to] = pending.back();
        pending.pop_back();

        to->arrayPart.reserve(from->arrayPart.size());
        for (const Value& value : from->arrayPart)
            to->arrayPart.push_back(value.get_if<Table>() ? Value(Table()) : value);

        to->mapPart.reserve(from->mapPart.size());
        for (const Entry& entry : from->mapPart)
            to->mapPart.push_back(Entry{entry.key, entry.value.get_if<Table>() ? Value(Table()) : entry.value});

        to->mapIndex = from->mapIndex;

        for (size_t i = 0; i < from->arrayPart.size(); i++)
            if (const Table* nested = from->arrayPart[i].get_if<Table>())
                pending.push_back({nested, to->arrayPart[i].get_if<Table>()});

        for (size_t i = 0; i < from->mapPart.size(); i++)
            if (const Table* nested = from->mapPart[i].value.get_if<Table>())
                pending.push_back({nested, to->mapPart[i].value.get_if<Table>()});
    }
}

Table::Table(Table&& other) noexcept = default;

Table& Table::operator=(const Table& other)
{
    if (this != &other)
        *this = Table(other);

    return *this;
}

Table& Table::operator=(Table&& other) noexcept
{
    // `other` may be nested inside this table, so it has to be moved out before this table's contents are destroyed.
    Table moved(std::move(other));
    arrayPart.swap(moved.arrayPart);
    mapPart.swap(moved.mapPart);
    std::swap(mapIndex, moved.mapIndex);
    return *this;
}

Table::~Table()
{
    // Destroying nested tables would recurse once per level of nesting too, so instead we move them out into a list
    // and destroy them one at a time, each after its own nested tables have been moved out.
    std::vector<Table> pending;

    auto takeNested = [&pending](Table& table)
    {
        for (Value& value : table.arrayPart)
            if (Table* nested = value.get_if<Table>())
                pending.push_back(std::move(*nested));

        for (Entry& entry : table.mapPart)
            if (Table* nested = entry.value.get_if<Table>())
                pending.push_back(std::move(*nested));
    };

    takeNested(*this);

    while (!pending.empty())
    {
        Table table = std::move(pending.back());
        pending.pop_back();
        takeNested(table);
    }
}

const Value* Table::find(const Key& key) const
{
    if (std::optional<size_t> index = arrayIndex(key, arrayPart.size()))
        return &arrayPart[*index];

    const size_t* position = mapIndex.find(key);
    return position ? &mapPart[*position].value : nullptr;
}

Value* Table::find(const Key& key)
{
    return const_cast<Value*>(std::as_const(*this).find(key));
}

bool Table::append(Value value)
{
    if (mapIndex.contains(Key(double(arrayPart.size() + 1))))
        return false;

    arrayPart.push_back(std::move(value));
    return true;
}

bool Table::insert(Key key, Value value)
{
    if (!isValidKey(key) || find(key))
        return false;

    mapIndex[key] = mapPart.size();
    mapPart.push_back(Entry{std::move(key), std::move(value)});
    return true;
}

bool Table::erase(const Key& key)
{
    if (std::optional<size_t> index = arrayIndex(key, arrayPart.size()))
    {
        if (*index != arrayPart.size() - 1)
            return false;

        arrayPart.pop_back();
        return true;
    }

    const size_t* found = mapIndex.find(key);
    if (!found)
        return false;

    size_t position = *found;
    mapIndex.erase(key);
    mapPart.erase(mapPart.begin() + position);

    // Erasing the last entry is common enough (and cheap enough) to skip renumbering the rest.
    if (position != mapPart.size())
    {
        for (auto& [_, later] : mapIndex)
            if (later > position)
                later--;
    }

    return true;
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

static Value toValue(Key key)
{
    if (bool* boolean = key.get_if<bool>())
        return *boolean;
    if (double* number = key.get_if<double>())
        return *number;
    if (std::string* string = key.get_if<std::string>())
        return std::move(*string);

    LUAU_ASSERT(key.get_if<Vector>());
    return *key.get_if<Vector>();
}

// Walks the AST of a parsed file, accepting only what the Luau Table format allows.
struct Reader
{
    std::vector<ParseError>& errors;

    void report(const Location& location, std::string message)
    {
        // Like the parser, stop reporting errors once there are too many to be useful.
        size_t limit = size_t(FInt::LuauParseErrorLimit);
        if (errors.size() > limit)
            return;

        errors.emplace_back(location, std::move(message));

        if (errors.size() == limit)
            errors.emplace_back(location, format("Reached error limit (%d)", int(FInt::LuauParseErrorLimit)));
    }

    std::optional<Key> readPrimitive(AstExpr* expr)
    {
        if (AstExprConstantBool* boolean = expr->as<AstExprConstantBool>())
            return boolean->value;
        if (AstExprConstantString* string = expr->as<AstExprConstantString>())
            return std::string(string->value.data, string->value.size);
        if (std::optional<double> number = readNumber(expr))
            return *number;

        if (AstExprInterpString* string = expr->as<AstExprInterpString>())
        {
            if (string->expressions.size == 0)
                return std::string(string->strings.data[0].data, string->strings.data[0].size);

            report(expr->location, "Cannot use string interpolation in Luau Table");
            return std::nullopt;
        }

        if (AstExprCall* call = expr->as<AstExprCall>(); call && isGlobalMember(call->func, "vector", "create"))
            return readVector(call);

        report(expr->location, "Luau Table only allows literal strings, booleans, numbers, vectors and tables");
        return std::nullopt;
    }

    std::optional<Key> readVector(AstExprCall* call)
    {
        if (call->args.size != 2 && call->args.size != 3)
        {
            report(call->location, "vector.create must be given 2 or 3 numbers");
            return std::nullopt;
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

        if (!valid)
            return std::nullopt;

        return Vector{components[0], components[1], components[2]};
    }

    std::optional<Key> readKey(AstExpr* expr)
    {
        if (expr->is<AstExprTable>())
        {
            report(expr->location, "Cannot use a table as a key");
            return std::nullopt;
        }

        std::optional<Key> key = readPrimitive(expr);
        if (key && !isValidKey(*key))
        {
            report(expr->location, "Cannot use NaN as a key");
            return std::nullopt;
        }

        return key;
    }

    std::optional<Value> readValue(AstExpr* expr, size_t depth)
    {
        if (AstExprTable* table = expr->as<AstExprTable>())
            return readTable(table, depth + 1);

        if (std::optional<Key> primitive = readPrimitive(expr))
            return toValue(std::move(*primitive));

        return std::nullopt;
    }

    Table readTable(AstExprTable* expr, size_t depth)
    {
        Table table;

        if (depth > kMaxDepth)
        {
            report(expr->location, depthError());
            return table;
        }

        for (const AstExprTable::Item& item : expr->items)
        {
            std::optional<Value> value = readValue(item.value, depth);

            if (item.kind == AstExprTable::Item::Kind::List)
            {
                Key implicitKey = double(table.getArray().size() + 1);
                if (value && !table.append(std::move(*value)))
                    report(item.value->location, "Duplicate key " + describeKey(implicitKey));

                continue;
            }

            std::optional<Key> key;
            if (item.kind == AstExprTable::Item::Kind::Record)
            {
                AstExprConstantString* name = item.key->as<AstExprConstantString>();
                LUAU_ASSERT(name);
                key = std::string(name->value.data, name->value.size);
            }
            else
                key = readKey(item.key);

            if (!key || !value)
                continue;

            if (table.find(*key))
            {
                report(item.key->location, "Duplicate key " + describeKey(*key));
                continue;
            }

            bool inserted = table.insert(std::move(*key), std::move(*value));
            LUAU_ASSERT(inserted);
        }

        return table;
    }
};

// Converts a position from the lexer, which counts lines by '\n' and columns in bytes, to an offset into the source.
static size_t offsetOf(std::string_view source, Position position)
{
    size_t offset = 0;
    for (unsigned int line = 0; line < position.line; line++)
        offset = source.find('\n', offset) + 1;

    return offset + position.column;
}

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

// Finds the root expression: either the whole file, or the single value returned by the file.
static AstExpr* parseRoot(std::string_view source, AstNameTable& names, Allocator& allocator, std::vector<ParseError>& errors)
{
    Lexer lexer(source.data(), source.size(), names);
    lexer.setSkipComments(true);

    // Parser::parseExpr doesn't reject every trailing token, so we find where the root expression should end ourselves.
    Lexeme first = lexer.next();
    Lexeme last = first;
    Lexeme beforeLast = first;
    for (Lexeme lexeme = first; lexeme.type != Lexeme::Eof; lexeme = lexer.next())
    {
        beforeLast = last;
        last = lexeme;
    }

    Position end = last.location.end;

    // A single `;` at the end of the file is ignored, so we parse only what comes before it.
    if (last.type == ';')
    {
        source = source.substr(0, offsetOf(source, last.location.begin));
        end = beforeLast.location.end;
    }

    AstExpr* root = nullptr;

    if (first.type == Lexeme::ReservedReturn)
    {
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

        root = ret->list.data[0];
    }
    else
    {
        ParseNodeResult<AstExpr> result = Parser::parseExpr(source.data(), source.size(), names, allocator);
        errors = std::move(result.errors);
        if (!errors.empty())
            return nullptr;

        root = result.root;
    }

    if (root->location.end != end)
    {
        errors.emplace_back(Location{root->location.end, end}, "Expected end of file");
        return nullptr;
    }

    if (!root->is<AstExprTable>())
    {
        errors.emplace_back(root->location, "Expected a table");
        return nullptr;
    }

    return root;
}

ReadResult read(std::string_view source)
{
    Allocator allocator;
    AstNameTable names(allocator);

    ReadResult result;

    // The lexer treats a NUL byte as the end of the source, which would hide anything after it.
    if (size_t nul = source.find('\0'); nul != std::string_view::npos)
    {
        Position position = positionOf(source, nul);
        result.errors.emplace_back(Location{position, 1}, "NUL bytes must be escaped");
        return result;
    }

    AstExpr* root = parseRoot(source, names, allocator, result.errors);
    if (!root)
        return result;

    Table table = Reader{result.errors}.readTable(root->as<AstExprTable>(), 1);
    if (result.errors.empty())
        result.table = std::move(table);

    return result;
}

// Matches what the lexer accepts as a name, which is always ASCII, regardless of locale.
static bool isNameStart(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static bool isName(const std::string& string)
{
    if (string.empty() || !isNameStart(string[0]))
        return false;

    for (char c : string)
        if (!isNameStart(c) && !(c >= '0' && c <= '9'))
            return false;

    return !Lexer::isReserved(string);
}

struct Writer
{
    std::string out;
    bool tooDeep = false;

    // The root table has a depth of 1, and its entries are indented once.

    // Writes the shortest number that reads back as exactly the same value.
    template<typename T>
    void writeNumber(T number, int maxPrecision)
    {
        if (std::isnan(number))
            out += "math.nan";
        else if (std::isinf(number))
            out += number > 0 ? "math.huge" : "-math.huge";
        else if (number == std::floor(number) && std::fabs(number) < 1e15)
        {
            // Whole numbers are written out in full, as %g would switch to an exponent for values like 100.
            char buffer[32];
            snprintf(buffer, sizeof(buffer), "%.0f", double(number));
            out += buffer;
        }
        else
        {
            char buffer[32];
            for (int precision = 1; precision <= maxPrecision; precision++)
            {
                snprintf(buffer, sizeof(buffer), "%.*g", precision, double(number));
                if (T(strtod(buffer, nullptr)) == number)
                    break;
            }
            out += buffer;
        }
    }

    void writeString(const std::string& string)
    {
        out += '"';

        for (unsigned char c : string)
        {
            switch (c)
            {
            case '"':
                out += "\\\"";
                break;
            case '\\':
                out += "\\\\";
                break;
            case '\n':
                out += "\\n";
                break;
            case '\r':
                out += "\\r";
                break;
            case '\t':
                out += "\\t";
                break;
            default:
                if (c < ' ' || c == 127)
                {
                    // Always three digits, so a digit that follows isn't read as part of the escape.
                    char buffer[8];
                    snprintf(buffer, sizeof(buffer), "\\%03d", c);
                    out += buffer;
                }
                else
                    out += char(c);
            }
        }

        out += '"';
    }

    template<typename T>
    void writePrimitive(const T& primitive)
    {
        if (const bool* boolean = primitive.template get_if<bool>())
            out += *boolean ? "true" : "false";
        else if (const double* number = primitive.template get_if<double>())
            writeNumber(*number, 17);
        else if (const std::string* string = primitive.template get_if<std::string>())
            writeString(*string);
        else if (const Vector* vector = primitive.template get_if<Vector>())
        {
            out += "vector.create(";
            writeNumber(vector->x, 9);
            out += ", ";
            writeNumber(vector->y, 9);
            out += ", ";
            writeNumber(vector->z, 9);
            out += ")";
        }
        else
            LUAU_ASSERT(!"Unknown primitive");
    }

    void writeKey(const Key& key)
    {
        if (const std::string* name = key.get_if<std::string>(); name && isName(*name))
            out += *name;
        else
        {
            out += '[';
            writePrimitive(key);
            out += ']';
        }

        out += " = ";
    }

    void writeValue(const Value& value, size_t depth)
    {
        if (const Table* table = value.get_if<Table>())
            writeTable(*table, depth + 1);
        else
            writePrimitive(value);
    }

    void indent(size_t depth)
    {
        out.append(depth * 4, ' ');
    }

    void writeTable(const Table& table, size_t depth)
    {
        if (depth > kMaxDepth)
        {
            tooDeep = true;
            return;
        }

        if (table.getArray().empty() && table.getMap().empty())
        {
            out += "{}";
            return;
        }

        out += "{\n";

        for (const Value& value : table.getArray())
        {
            indent(depth);
            writeValue(value, depth);
            out += ",\n";
        }

        for (const Entry& entry : table.getMap())
        {
            indent(depth);
            writeKey(entry.key);
            writeValue(entry.value, depth);
            out += ",\n";
        }

        indent(depth - 1);
        out += "}";
    }
};

static std::string describeKey(const Key& key)
{
    Writer writer;
    writer.writePrimitive(key);
    return std::move(writer.out);
}

WriteResult write(const Table& table)
{
    Writer writer;
    writer.writeTable(table, 1);
    if (writer.tooDeep)
        return {std::nullopt, depthError()};

    writer.out += "\n";
    return {std::move(writer.out), {}};
}

} // namespace Luau::TableFormat
