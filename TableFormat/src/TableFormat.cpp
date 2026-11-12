// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"

#include "Luau/Ast.h"
#include "Luau/HashUtil.h"
#include "Luau/Lexer.h"
#include "Luau/Parser.h"

#include <cmath>
#include <ctype.h>
#include <limits>
#include <stdio.h>
#include <stdlib.h>
#include <utility>

namespace Luau::TableFormat
{

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

const Value* Table::find(const Key& key) const
{
    if (std::optional<size_t> index = arrayIndex(key, arrayPart.size()))
        return &arrayPart[*index];

    auto it = mapIndex.find(key);
    return it == mapIndex.end() ? nullptr : &mapPart[it->second].value;
}

Value* Table::find(const Key& key)
{
    return const_cast<Value*>(std::as_const(*this).find(key));
}

bool Table::append(Value value)
{
    if (mapIndex.count(Key(double(arrayPart.size() + 1))))
        return false;

    arrayPart.push_back(std::move(value));
    return true;
}

bool Table::insert(Key key, Value value)
{
    if (!isValidKey(key) || find(key))
        return false;

    mapIndex.emplace(key, mapPart.size());
    mapPart.push_back(Entry{std::move(key), std::move(value)});
    return true;
}

bool Table::remove(const Key& key)
{
    if (std::optional<size_t> index = arrayIndex(key, arrayPart.size()))
    {
        if (*index != arrayPart.size() - 1)
            return false;

        arrayPart.pop_back();
        return true;
    }

    auto it = mapIndex.find(key);
    if (it == mapIndex.end())
        return false;

    size_t position = it->second;
    mapIndex.erase(it);
    mapPart.erase(mapPart.begin() + position);

    for (auto& [_, later] : mapIndex)
        if (later > position)
            later--;

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
        errors.emplace_back(location, std::move(message));
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

            report(expr->location, "String interpolation is not allowed in Luau Table");
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
            report(expr->location, "Tables can't be used as keys");
            return std::nullopt;
        }

        std::optional<Key> key = readPrimitive(expr);
        if (key && !isValidKey(*key))
        {
            report(expr->location, "NaN can't be used as a key");
            return std::nullopt;
        }

        return key;
    }

    std::optional<Value> readValue(AstExpr* expr)
    {
        if (AstExprTable* table = expr->as<AstExprTable>())
            return readTable(table);

        if (std::optional<Key> primitive = readPrimitive(expr))
            return toValue(std::move(*primitive));

        return std::nullopt;
    }

    Table readTable(AstExprTable* expr)
    {
        Table table;

        for (const AstExprTable::Item& item : expr->items)
        {
            std::optional<Value> value = readValue(item.value);

            if (item.kind == AstExprTable::Item::Kind::List)
            {
                if (value && !table.append(std::move(*value)))
                    report(item.value->location, "Duplicate key");

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

            if (key && value && !table.insert(std::move(*key), std::move(*value)))
                report(item.key->location, "Duplicate key");
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

    AstExpr* root = parseRoot(source, names, allocator, result.errors);
    if (!root)
        return result;

    Table table = Reader{result.errors}.readTable(root->as<AstExprTable>());
    if (result.errors.empty())
        result.table = std::move(table);

    return result;
}

static bool isName(const std::string& string)
{
    if (string.empty() || !(isalpha((unsigned char)string[0]) || string[0] == '_'))
        return false;

    for (char c : string)
        if (!(isalnum((unsigned char)c) || c == '_'))
            return false;

    return !Lexer::isReserved(string);
}

struct Writer
{
    std::string out;

    // Writes the shortest number that reads back as exactly the same value.
    template<typename T>
    void writeNumber(T number, int maxPrecision)
    {
        if (std::isnan(number))
            out += "math.nan";
        else if (std::isinf(number))
            out += number > 0 ? "math.huge" : "-math.huge";
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

    void writeValue(const Value& value, int depth)
    {
        if (const Table* table = value.get_if<Table>())
            writeTable(*table, depth);
        else
            writePrimitive(value);
    }

    void indent(int depth)
    {
        out.append(depth * 4, ' ');
    }

    void writeTable(const Table& table, int depth)
    {
        if (table.array().empty() && table.map().empty())
        {
            out += "{}";
            return;
        }

        out += "{\n";

        for (const Value& value : table.array())
        {
            indent(depth + 1);
            writeValue(value, depth + 1);
            out += ",\n";
        }

        for (const Entry& entry : table.map())
        {
            indent(depth + 1);
            writeKey(entry.key);
            writeValue(entry.value, depth + 1);
            out += ",\n";
        }

        indent(depth);
        out += "}";
    }
};

std::string write(const Table& table)
{
    Writer writer;
    writer.writeTable(table, 0);
    writer.out += "\n";
    return std::move(writer.out);
}

} // namespace Luau::TableFormat
