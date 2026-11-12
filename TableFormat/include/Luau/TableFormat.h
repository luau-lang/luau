// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/ParseResult.h"
#include "Luau/Variant.h"

#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

// Reads and writes the Luau Table format (`.table`): a subset of Luau that holds a single table of strings, booleans,
// numbers, vectors and nested tables, all written out literally.
//
// Reading parses the source and never evaluates it, so no Luau VM is involved. Well-formed files produce exactly the
// values a Luau VM would wherever the VM can evaluate them (Luau's own limits, such as register counts, can prevent
// that), and malformed files are always rejected. Writing always produces well-formed files.
namespace Luau::TableFormat
{

struct Vector
{
    float x = 0.0f;
    float y = 0.0f;
    float z = 0.0f;
};

// Anything that can be a key: every kind of value except tables, which have referential identity in Luau.
// Keys compare as they do in Luau, so 1 and 1.0 are the same key, as are 0 and -0. NaN is never a valid key, and nor is
// a vector containing NaN.
struct Key : Variant<bool, double, std::string, Vector>
{
    using Variant::Variant;
};

struct KeyHash
{
    size_t operator()(const Key& key) const;
};

struct KeyEqual
{
    bool operator()(const Key& lhs, const Key& rhs) const;
};

struct Value;
struct Entry;

// A table with an array part, whose values have the implicit keys 1, 2, 3 and so on, and a map part, whose values have
// explicit keys. Each key appears at most once across both parts, and the array part has no holes.
class Table
{
public:
    const std::vector<Value>& array() const
    {
        return arrayPart;
    }

    // In insertion order, which is also the order they're written in.
    const std::vector<Entry>& map() const
    {
        return mapPart;
    }

    // Finds the value for a key in either part.
    const Value* find(const Key& key) const;
    Value* find(const Key& key);

    // Adds a value to the end of the array part. Fails if its implicit key is already in the map part.
    bool append(Value value);

    // Adds a value to the map part. Fails if the key is invalid or already present in either part.
    bool insert(Key key, Value value);

    // Removes a key from either part. To keep the array part free of holes, only its last value can be removed. Removing
    // from the map part takes time proportional to its size, unless the key is the last one in it.
    bool remove(const Key& key);

private:
    std::vector<Value> arrayPart;
    std::vector<Entry> mapPart;
    std::unordered_map<Key, size_t, KeyHash, KeyEqual> mapIndex;
};

struct Value : Variant<bool, double, std::string, Vector, Table>
{
    using Variant::Variant;
};

struct Entry
{
    Key key;
    Value value;
};

// How deeply tables can nest, counting the root table as 1. Deeper files are malformed, and deeper tables can't be written.
constexpr size_t kMaxDepth = 255;

struct ReadResult
{
    // Present only if the source was well-formed.
    std::optional<Table> table;
    std::vector<ParseError> errors;
};

ReadResult read(std::string_view source);

// Returns the source of a well-formed file holding just the table, without `return`, or nothing if the table is nested
// more deeply than kMaxDepth. The source reads back as an identical table, except that NaN payloads aren't preserved. The
// output is formatted consistently, so identical tables (with their map parts in the same order) are written identically.
std::optional<std::string> write(const Table& table);

} // namespace Luau::TableFormat
