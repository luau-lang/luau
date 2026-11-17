// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/ParseResult.h"

#include <optional>
#include <string>
#include <string_view>
#include <vector>

struct lua_State;

namespace Luau
{
class AstExprTable;
}

// Reads and writes the Luau Table format (`.table`): a subset of Luau that holds a single table of strings, booleans,
// numbers, vectors and nested tables, all written out literally.
//
// Deserializing parses the source and never evaluates it. Well-formed files produce exactly the values a Luau VM would
// from running the same file, wherever the VM can evaluate them (Luau's own limits, such as register counts, can prevent
// that), and malformed files are always rejected. Serializing always produces well-formed files.
//
// For tools that work with the source itself, parse() gives the file's syntax tree, tokenize() gives every token with the
// whitespace and comments around it, and replace() edits the source while leaving everything else untouched.
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
// separators: {"a",name="x",[true]={1,2}}. Its array part comes first, from 1 up to the first nil, then its other keys in
// iteration order, so tables with the same contents can be written with their keys in different orders. Tables are read
// with raw access, ignoring metatables.
//
// Fails for values a Luau Table file can't hold: functions, userdata, buffers, threads, integers, tables used as keys,
// tables that contain themselves, tables nested more deeply than kMaxDepth, and vectors with a nonzero fourth component.
SerializeResult serialize(lua_State* L, int idx);

// Parses a Luau Table file without evaluating it or needing a VM. root is set only if the file is well-formed, and is the
// table whether or not the file starts with `return`. The result always includes the CST and the locations of comments.
ParseNodeResult<AstExprTable> parse(std::string_view source, AstNameTable& names, Allocator& allocator);

// Whitespace or a comment between tokens.
struct Trivia
{
    enum class Kind
    {
        // Each whitespace item ends at a newline, so blank lines and indentation are separate items.
        Whitespace,
        // Comments include their `--` and brackets.
        SingleLineComment,
        MultiLineComment,
        // Everything from a NUL byte to the end of the source, which the lexer doesn't read.
        Unlexed,
    };

    Kind kind;
    Location location;
    std::string_view text;
};

struct Token
{
    Lexeme::Type type;
    Location location;
    std::string_view text;
    std::vector<Trivia> leadingTrivia;
    // Trivia after the token up to and including the end of its line. Anything after that leads the next token.
    std::vector<Trivia> trailingTrivia;
};

// Splits any source, well-formed or not, into tokens, ending with an end-of-file token. Concatenating each token's leading
// trivia, text and trailing trivia reproduces the source exactly. Text is a view into the source, which must outlive the
// tokens.
std::vector<Token> tokenize(std::string_view source);

// The location of an item in a table from parse(), from its key (or the `[` before it) to its separator, if it has one.
Location itemLocation(const AstExprTable& table, size_t index, const CstNodeMap& cst);

struct Replacement
{
    Location location;
    std::string text;
};

struct ReplaceResult
{
    // Present only if the replacements could be made and the result is a well-formed Luau Table file.
    std::optional<std::string> source;
    // Why the replacements couldn't be made, if they couldn't, with locations in the original source for problems with the
    // replacements and in the new source for problems with the result.
    std::vector<ParseError> errors;
};

// Replaces each range of the source with new text, leaving everything else untouched; an empty range inserts text. Nothing
// is replaced if any range is reversed, outside the source or overlaps another, or if the result is malformed.
ReplaceResult replace(std::string_view source, const std::vector<Replacement>& replacements);

} // namespace Luau::TableFormat
