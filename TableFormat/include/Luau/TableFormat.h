// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/ParseResult.h"

#include <optional>
#include <string>
#include <string_view>
#include <vector>

struct lua_State;

// Reads and writes the Luau Table format (`.table`): a subset of Luau that holds a single table of strings, booleans,
// numbers, vectors and nested tables, all written out literally.
//
// Deserializing parses the source and never evaluates it. Well-formed files produce exactly the values a Luau VM would
// from running the same file, wherever the VM can evaluate them (Luau's own limits, such as register counts, can prevent
// that), and malformed files are always rejected. Serializing always produces well-formed files.
namespace Luau::TableFormat
{

// How deeply tables can nest, counting the root table as 1. Deeper files are malformed, and deeper tables can't be serialized.
constexpr size_t kMaxDepth = 255;

struct DeserializeResult
{
    // Empty if the source was well-formed.
    std::vector<ParseError> errors;
};

// Reads the table in a Luau Table file and pushes it onto the stack. If the source is malformed, pushes nothing and
// returns why.
DeserializeResult deserialize(lua_State* L, std::string_view source);

struct SerializeResult
{
    // Present only if the table could be serialized.
    std::optional<std::string> source;
    // Why the table couldn't be serialized, if it couldn't.
    std::string error;
};

// Writes the table at a stack index as a Luau Table file in minimal form, with no whitespace, no `return` and no trailing
// separators: {"a",[true]={1,2},name="x"}. Its array part comes first, from 1 up to the first nil, then its other keys,
// sorted: booleans, numbers, strings and vectors, each in ascending order. So tables with the same contents are always
// written identically. Tables are read with raw access, ignoring metatables.
//
// Fails for values a Luau Table file can't hold: functions, userdata, buffers, threads, integers, tables used as keys,
// tables that contain themselves, tables nested more deeply than kMaxDepth, and vectors that aren't three floats.
SerializeResult serialize(lua_State* L, int idx);

} // namespace Luau::TableFormat
