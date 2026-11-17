// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"

#include "Luau/Compiler.h"
#include "Luau/Cst.h"

#include "lua.h"
#include "lualib.h"

#include "ScopedFlags.h"
#include "doctest.h"

#include <cmath>
#include <cstring>
#include <memory>
#include <string>

using namespace Luau;
using namespace Luau::TableFormat;
using namespace std::literals;

LUAU_FASTFLAG(LuauParseRejectTrailingToken)

static const char* const kRfcExample = R"(-- Settings for the example service.
return {
    name = "example",
    enabled = true,
    retries = 3,
    timeout = 2.5e3,
    flags = 0b1010_0001,
    threshold = -math.huge,
    origin = vector.create(0, 10, -5),
    tags = { "fast", "beta", },
    [1] = "first numeric key";
    ["display name"] = `Example Service`,
    limits = {
        [true] = math.huge,
        [false] = 0,
    },
}
)";

static std::string nested(size_t depth)
{
    return std::string(depth, '{') + std::string(depth, '}');
}

static bool sameNumber(double a, double b)
{
    return (std::isnan(a) && std::isnan(b)) || (a == b && std::signbit(a) == std::signbit(b));
}

// Whether two values on the stack are identical: tables by contents, numbers and vector components by value and sign.
static bool sameValue(lua_State* L, int a, int b)
{
    a = lua_absindex(L, a);
    b = lua_absindex(L, b);

    if (lua_type(L, a) != lua_type(L, b))
        return false;

    switch (lua_type(L, a))
    {
    case LUA_TNUMBER:
        return sameNumber(lua_tonumber(L, a), lua_tonumber(L, b));
    case LUA_TVECTOR:
    {
        const LUA_VECTOR_TYPE* x = lua_tovector(L, a);
        const LUA_VECTOR_TYPE* y = lua_tovector(L, b);
        return sameNumber(x[0], y[0]) && sameNumber(x[1], y[1]) && sameNumber(x[2], y[2]);
    }
    case LUA_TTABLE:
    {
        size_t count = 0;
        lua_pushnil(L);
        while (lua_next(L, a))
        {
            count++;
            lua_pushvalue(L, -2);
            lua_rawget(L, b);
            bool same = sameValue(L, -2, -1);
            lua_pop(L, 2);
            if (!same)
            {
                lua_pop(L, 1);
                return false;
            }
        }

        lua_pushnil(L);
        while (lua_next(L, b))
        {
            count--;
            lua_pop(L, 1);
        }

        return count == 0;
    }
    default:
        return lua_rawequal(L, a, b);
    }
}

// The text of the source at a location.
static std::string_view textAt(std::string_view source, const Location& location)
{
    auto offsetOf = [&](Position position)
    {
        size_t offset = 0;
        for (unsigned line = 0; line < position.line; line++)
            offset = source.find('\n', offset) + 1;
        return offset + position.column;
    };

    size_t begin = offsetOf(location.begin);
    return source.substr(begin, offsetOf(location.end) - begin);
}

static std::vector<ParseError> parseErrors(std::string_view source)
{
    Allocator allocator;
    AstNameTable names(allocator);
    return parse(source, names, allocator).errors;
}

struct TableFormatFixture
{
    std::unique_ptr<lua_State, void (*)(lua_State*)> state{luaL_newstate(), lua_close};
    lua_State* L = state.get();

    TableFormatFixture()
    {
        luaL_openlibs(L);
    }

    // Runs Luau source and pushes the value it returns.
    void run(const std::string& source)
    {
        std::string bytecode = compile(source);
        REQUIRE(luau_load(L, "=test", bytecode.data(), bytecode.size(), 0) == 0);
        REQUIRE(lua_pcall(L, 0, 1, 0) == 0);
    }

    // Deserializes a well-formed file, pushing the table.
    void deserializeOk(const std::string& source)
    {
        INFO(source);
        int top = lua_gettop(L);
        DeserializeResult result = deserialize(L, source);
        REQUIRE_MESSAGE(result.errors.empty(), result.errors[0].getMessage());
        REQUIRE(lua_gettop(L) == top + 1);
        REQUIRE(lua_istable(L, -1));

        // Parsing accepts exactly what deserializing does.
        std::vector<ParseError> errors = parseErrors(source);
        CHECK_MESSAGE(errors.empty(), errors[0].getMessage());
    }

    void checkMalformed(const std::string& source)
    {
        INFO(source);
        int top = lua_gettop(L);
        DeserializeResult result = deserialize(L, source);
        CHECK(!result.errors.empty());
        CHECK(lua_gettop(L) == top);
        CHECK(!parseErrors(source).empty());
    }

    // Deserializing a well-formed file must produce what running it in a Luau VM does.
    void checkMatchesVm(const std::string& source)
    {
        INFO(source);
        deserializeOk(source);
        run(source.find("return") == std::string::npos ? "return " + source : source);
        CHECK(sameValue(L, -2, -1));
        lua_pop(L, 2);
    }

    // Serializes the table that Luau source returns, checking that the stack is left alone.
    SerializeResult serializeLuau(const std::string& source)
    {
        INFO(source);
        run(source);
        int top = lua_gettop(L);
        SerializeResult result = serialize(L, -1);
        CHECK(lua_gettop(L) == top);
        lua_pop(L, 1);
        return result;
    }

    // Checks that serializing the table on top of the stack reads back as an identical table, and as the same table when
    // run in the VM.
    void checkRoundTrip()
    {
        SerializeResult result = serialize(L, -1);
        REQUIRE_MESSAGE(result.source, result.error);
        INFO(*result.source);

        deserializeOk(*result.source);
        CHECK(sameValue(L, -2, -1));
        lua_pop(L, 1);

        checkMatchesVm(*result.source);
    }
};

TEST_SUITE_BEGIN("TableFormat");

TEST_CASE_FIXTURE(TableFormatFixture, "deserialize_matches_vm")
{
    checkMatchesVm(kRfcExample);
    checkMatchesVm(R"({
        'single', "double", [[raw]], [==[raw ]] level]==], `backtick`, "\q\w\X41", '\%', `\!`, "a\0b",
        "\n\t\\\"\x41\u{48}\65\z
             skipped", "\x00\255\u{10FFFF}",
        1_000, 0xf_f, 0B11, 1e-3, .5, 5., 0.1, -0, 1e999, 0xffffffffffffffffff, 0x7fffffffffffffff, 1e-320,
        math.nan, math.huge, -math.huge, vector.create(1, 2), vector.create(-1, math.huge, math.nan), vector.create(0.1, -0, 1e39),
    })");
    checkMatchesVm("{ [2] = 2, 1, [3.5] = 3, [-1] = 4, [true] = { { {} } }, [vector.create(1, 2, 3)] = \"v\", [\"nan\"] = 5 }");

    for (const char* source : {
             "return {}",
             "--[[ before ]] return --[==[ between ]==] {} -- after",
             "{ 1; 2, a = 3; }",
             "{};",
             "return {};",
             "return {} -- a\n; -- b",
             "{ a = {} }\n;\n",
         })
        checkMatchesVm(source);
}

TEST_CASE_FIXTURE(TableFormatFixture, "deserialize_rejects_malformed")
{
    // Evaluation of any kind.
    checkMalformed("{ a = 1 + 2 }");
    checkMalformed("{ a = x }");
    checkMalformed("{ a = not true }");
    checkMalformed("{ a = (1) }");
    checkMalformed("{ a = - -1 }");
    checkMalformed("{ a = #\"a\" }");
    checkMalformed("{ a = function() end }");
    checkMalformed("{ a = `{1}` }");
    checkMalformed("{ a = math.pi }");
    checkMalformed("{ a = string.nan }");
    checkMalformed("{ a = math[\"huge\"] }");
    checkMalformed("{ a = -math.nan }");
    checkMalformed("{ a = -vector.create(1, 2) }");
    checkMalformed("{ a = vector.create(1) }");
    checkMalformed("{ a = vector.create(1, 2, 3, 4) }");
    checkMalformed("{ a = vector.create(\"1\", 2) }");
    checkMalformed("{ a = vector.create{1, 2} }");
    checkMalformed("{ a = vector.create(vector.create(1, 2), 2) }");
    checkMalformed("{ a = if true then 1 else 2 }");

    // Nil.
    checkMalformed("{ a = nil }");
    checkMalformed("{ 1, nil, 3 }");

    // Type syntax.
    checkMalformed("{ a = 1 :: number }");

    // Invalid keys.
    checkMalformed("{ [{}] = 1 }");
    checkMalformed("{ [math.nan] = 1 }");
    checkMalformed("{ [vector.create(math.nan, 0, 0)] = 1 }");

    // Duplicate keys.
    checkMalformed("{ a = 1, a = 2 }");
    checkMalformed("{ a = 1, [\"a\"] = 2 }");
    checkMalformed("{ [1] = 1, [1.0] = 2 }");
    checkMalformed("{ [0] = 1, [-0] = 2 }");
    checkMalformed("{ \"a\", \"b\", [2] = \"c\" }");
    checkMalformed("{ [2] = \"c\", \"a\", \"b\" }");
    checkMalformed("{ [vector.create(0, 1)] = 1, [vector.create(-0, 1, 0)] = 2 }");
    checkMalformed("{ [math.huge] = 1, [1e999] = 2 }");

    // Anything other than a single table, optionally returned.
    checkMalformed("");
    checkMalformed("return");
    checkMalformed("1");
    checkMalformed("\"a\"");
    checkMalformed("{} {}");
    checkMalformed(";");
    checkMalformed("; {}");
    checkMalformed("return {};;");
    checkMalformed("return; {}");
    checkMalformed("return {}, {}");
    checkMalformed("return ({})");
    checkMalformed("local a = 1 return {}");
    checkMalformed("return {} return {}");
    checkMalformed("{ a = 1");

    // The lexer stops at a NUL byte, so whatever follows must not be ignored.
    checkMalformed(std::string("{}\0garbage", 10));
    checkMalformed(std::string("{}--\0garbage", 12));
    checkMalformed(std::string("{ a = \"\0\" }", 12));
}

TEST_CASE_FIXTURE(TableFormatFixture, "deserialize_rejects_trailing_tokens")
{
    // Parser::parseExpr only rejects a single trailing token with this flag.
    ScopedFastFlag sff{FFlag::LuauParseRejectTrailingToken, true};

    checkMalformed("{} 5");
    checkMalformed("{};;");
    checkMalformed("{}; ;");
}

TEST_CASE_FIXTURE(TableFormatFixture, "deserialize_reports_locations")
{
    DeserializeResult result = deserialize(L, "{\n    a = 1,\n    b = x,\n}");
    REQUIRE(result.errors.size() == 1);
    CHECK(result.errors[0].getLocation().begin == Position{2, 8});
}

TEST_CASE_FIXTURE(TableFormatFixture, "deserialize_reports_duplicate_keys")
{
    DeserializeResult result = deserialize(L, "{ \"a\", [1] = 1, b = 1, [\"b\"] = 2, [vector.create(1, 2)] = 1, [vector.create(1, 2, 0)] = 2 }");
    REQUIRE(result.errors.size() == 3);
    CHECK(result.errors[0].getMessage() == "Duplicate key 1");
    CHECK(result.errors[1].getMessage() == "Duplicate key \"b\"");
    CHECK(result.errors[2].getMessage() == "Duplicate key vector.create(1,2,0)");

    DeserializeResult implicit = deserialize(L, "{ [2] = 1, \"a\", \"b\" }");
    REQUIRE(implicit.errors.size() == 1);
    CHECK(implicit.errors[0].getMessage() == "Duplicate key 2");

    // Parsing reports the same errors without a VM.
    for (const char* source : {
             "{ \"a\", [1] = 1, b = 1, [\"b\"] = 2, [vector.create(1, 2)] = 1, [vector.create(1, 2, 0)] = 2 }",
             "{ [2] = 1, \"a\", \"b\" }",
             "{ [100] = 1, [1e2] = 2, [0.1] = 1, [0.1] = 2, [-0] = 1, [0] = 2, [1e300] = 1, [1e300] = 2, [true] = 1, [true] = 2 }",
             "{ [vector.create(0.5, -0, 1e20)] = 1, [vector.create(0.5, 0, 1e20)] = 2, [\"\\n\"] = 1, [\"\\n\"] = 2 }",
         })
    {
        INFO(source);
        std::vector<ParseError> deserialized = deserialize(L, source).errors;
        std::vector<ParseError> parsed = parseErrors(source);
        REQUIRE(parsed.size() == deserialized.size());
        for (size_t i = 0; i < parsed.size(); i++)
        {
            CHECK(parsed[i].getMessage() == deserialized[i].getMessage());
            CHECK(parsed[i].getLocation() == deserialized[i].getLocation());
        }
    }
}

TEST_CASE_FIXTURE(TableFormatFixture, "deserialize_limits_errors")
{
    std::string source = "{";
    for (int i = 0; i < 1000; i++)
        source += " x,";
    source += " }";

    DeserializeResult result = deserialize(L, source);
    CHECK(result.errors.size() == 101);
    CHECK(result.errors.back().getMessage() == "Reached error limit (100)");
}

TEST_CASE_FIXTURE(TableFormatFixture, "depth_limit")
{
    // The limit matches how deeply plain table constructors can nest in Luau.
    checkMatchesVm(nested(kMaxDepth));
    CHECK(compile("return {" + nested(kMaxDepth) + "}").front() == 0); // a compile error

    DeserializeResult tooDeep = deserialize(L, nested(kMaxDepth + 1));
    REQUIRE(tooDeep.errors.size() == 1);
    CHECK(tooDeep.errors[0].getMessage() == "Exceeded the maximum table nesting depth of 255");

    deserializeOk(nested(kMaxDepth));
    SerializeResult deepest = serialize(L, -1);
    lua_pop(L, 1);
    CHECK(deepest.source == nested(kMaxDepth));

    CHECK(serializeLuau("local t = {} for i = 1, 255 do t = { t } end return t").error == tooDeep.errors[0].getMessage());
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_format")
{
    CHECK(serializeLuau("return {}").source == "{}");
    CHECK(serializeLuau("return { a = { b = { 1, 2 } } }").source == "{a={b={1,2}}}");
    CHECK(serializeLuau("return { \"a\", true, {}, name = \"x\" }").source == "{\"a\",true,{},name=\"x\"}");

    // Keys are written as names where they can be, and in brackets otherwise.
    CHECK(serializeLuau("return { _1 = 1 }").source == "{_1=1}");
    CHECK(serializeLuau("return { continue = 1 }").source == "{continue=1}");
    CHECK(serializeLuau("return { ['end'] = 1 }").source == "{[\"end\"]=1}");
    CHECK(serializeLuau("return { ['1a'] = 1 }").source == "{[\"1a\"]=1}");
    CHECK(serializeLuau("return { ['not a name'] = 1 }").source == "{[\"not a name\"]=1}");
    CHECK(serializeLuau("return { [2.5] = -0 }").source == "{[2.5]=-0}");
    CHECK(serializeLuau("return { [-1] = math.huge }").source == "{[-1]=math.huge}");
    CHECK(serializeLuau("return { [false] = math.nan }").source == "{[false]=math.nan}");
    CHECK(serializeLuau("return { [vector.create(1, 0.5, -2)] = { 1 } }").source == "{[vector.create(1,0.5,-2)]={1}}");

    // Tables with several keys are written in iteration order, so check they read back instead.
    run("local t = {} for i = 1, 100 do t['k' .. i] = { i, [i + 0.5] = i } end return t");
    checkRoundTrip();
    lua_pop(L, 1);
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_array_part_stops_at_first_nil")
{
    CHECK(serializeLuau("local t = { 1, 2 } t[4] = 4 return t").source == "{1,2,[4]=4}");
    CHECK(serializeLuau("return { [2] = 2 }").source == "{[2]=2}");
    CHECK(serializeLuau("return { [1] = 1, [2] = 2 }").source == "{1,2}");
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_ignores_metatables")
{
    CHECK(serializeLuau("return setmetatable({ a = 1 }, { __index = { b = 2 } })").source == "{a=1}");
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_rejects_what_luau_table_cant_hold")
{
    CHECK(serializeLuau("return { print }").error == "Cannot serialize a value of type function");
    CHECK(serializeLuau("return { buffer.create(1) }").error == "Cannot serialize a value of type buffer");
    CHECK(serializeLuau("return { coroutine.create(print) }").error == "Cannot serialize a value of type thread");
    CHECK(serializeLuau("return { [{}] = 1 }").error == "Cannot use a table as a key");
    CHECK(serializeLuau("return { [print] = 1 }").error == "Cannot use a value of type function as a key");
    CHECK(serializeLuau("local t = {} t.self = t return t").error == "Cannot serialize a table that contains itself");
    CHECK(serializeLuau("local t = {} t.a = { b = { t } } return t").error == "Cannot serialize a table that contains itself");

    // The same table can appear more than once, as long as it doesn't contain itself.
    CHECK(serializeLuau("local shared = { 1 } return { shared, shared }").source == "{{1},{1}}");

    CHECK(serializeLuau("return { newproxy() }").error == "Cannot serialize a value of type userdata");

    lua_pushnumber(L, 1);
    CHECK(serialize(L, -1).error == "Expected a table");
    lua_pop(L, 1);
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_whole_numbers")
{
    CHECK(
        serializeLuau("return { 10, 100, 1e6, -1e14, 99999999999999.5, 1e15, vector.create(10, 20, 300) }").source ==
        "{10,100,1000000,-100000000000000,99999999999999.5,1000000000000000,vector.create(10,20,300)}"
    );
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_minimal_output_reads_back")
{
    // Without spaces, a minus sign follows `=`, `,`, `{` and `(`, so check none of those lex as something else.
    CHECK(serializeLuau("return { -1, -math.huge, a = -0 }").source == "{-1,-math.huge,a=-0}");
    CHECK(serializeLuau("return { [-1] = -2.5 }").source == "{[-1]=-2.5}");
    CHECK(serializeLuau("return { [vector.create(-1, -2, -3)] = { -1e-7 } }").source == "{[vector.create(-1,-2,-3)]={-1e-07}}");

    deserializeOk("{ -1, -math.huge, a = -0, [-1] = -2.5, [vector.create(-1, -2, -3)] = { -1e-7 } }");
    checkRoundTrip();
    lua_pop(L, 1);
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_numbers_and_strings_round_trip")
{
    run(R"(
        local t = {
            0.1, 1 / 3, 1e300, 5e-324, 2.2250738585072014e-308, 9007199254740993, 123456789012345680000, -1e-7, 100, -0,
            math.huge, -math.huge, vector.create(0.1, 1 / 3, 3.4028235e38), vector.create(1e-45, -16777217, -0.1),
        }
        for _, s in { "\x00\x001", "\a\b\f\v\x7f", "\"'`{}[[]]", "\xff\xfe", "multi\r\nline", "end", "" } do
            t[s] = s
        end
        return t
    )");
    checkRoundTrip();
    lua_pop(L, 1);
}

TEST_CASE("parse_returns_the_root_table")
{
    for (const char* source : {"{ 1 }", "return { 1 }", "-- a\nreturn { 1 }; -- b"})
    {
        INFO(source);
        Allocator allocator;
        AstNameTable names(allocator);
        ParseNodeResult<AstExprTable> result = parse(source, names, allocator);
        REQUIRE(result.root);
        CHECK(textAt(source, result.root->location) == "{ 1 }");
        CHECK(result.cstNodeMap.find(result.root));
    }
}

TEST_CASE("parse_captures_comments")
{
    // Including the ones after a trailing `;`, which the parser doesn't see.
    for (std::pair<const char*, const char*> test : {
             std::pair{"-- a\n{ --[[b]] }", "-- a|--[[b]]|"},
             std::pair{"-- a\n{ --[[b]] }; -- c\n--[==[d]==]", "-- a|--[[b]]|-- c|--[==[d]==]|"},
             std::pair{"return --[[a]] {} --[[b]];", "--[[a]]|--[[b]]|"},
         })
    {
        const char* source = test.first;
        INFO(source);
        Allocator allocator;
        AstNameTable names(allocator);
        ParseNodeResult<AstExprTable> result = parse(source, names, allocator);
        REQUIRE(result.root);

        std::string comments;
        for (const Comment& comment : result.commentLocations)
            comments += std::string(textAt(source, comment.location)) + "|";
        CHECK(comments == test.second);
    }
}

// Checks that the tokens of a source cover it exactly, in order, with locations that match their text.
static std::vector<Token> checkTokenize(std::string_view source)
{
    INFO(source);
    std::vector<Token> tokens = tokenize(source);
    REQUIRE(!tokens.empty());
    CHECK(tokens.back().type == Lexeme::Eof);
    CHECK(tokens.back().text.empty());

    std::string joined;
    auto add = [&](std::string_view text, const Location& location)
    {
        CHECK(!text.empty());
        CHECK(textAt(source, location) == text);
        CHECK(text.data() == source.data() + joined.size()); // a view into the source
        joined += text;
    };

    for (const Token& token : tokens)
    {
        for (const Trivia& trivia : token.leadingTrivia)
            add(trivia.text, trivia.location);
        if (token.type != Lexeme::Eof)
            add(token.text, token.location);
        for (const Trivia& trivia : token.trailingTrivia)
            add(trivia.text, trivia.location);
    }

    CHECK(joined == source);
    return tokens;
}

TEST_CASE("tokenize_round_trips")
{
    for (std::string_view source : {
             ""sv,
             " \n\t"sv,
             std::string_view(kRfcExample),
             "{\r\n\ta = 1,\r\n}\r\n"sv,
             "--[[ unterminated"sv,
             "\"unterminated\n{ a = 1 }"sv,
             "{ `a{1}b{ { 2 } }c`, [==[\n]==], 0x1p4, ... } @ $ !"sv,
             "{}\0garbage\n"sv,
             "{ \"\0\" }"sv,
             "\0"sv,
         })
        checkTokenize(source);
}

TEST_CASE("tokenize_splits_trivia")
{
    std::vector<Token> tokens = checkTokenize("a -- one\n\n  --[[two]] b --three");
    REQUIRE(tokens.size() == 3);

    auto describe = [](const std::vector<Trivia>& trivia)
    {
        std::string result;
        for (const Trivia& item : trivia)
            result += std::to_string(int(item.kind)) + ":" + std::string(item.text) + "|";
        return result;
    };

    // Each token takes the trivia up to and including the end of its line, and the next token takes the rest.
    CHECK(tokens[0].text == "a");
    CHECK(describe(tokens[0].leadingTrivia) == "");
    CHECK(describe(tokens[0].trailingTrivia) == "0: |1:-- one|0:\n|");
    CHECK(tokens[1].text == "b");
    CHECK(describe(tokens[1].leadingTrivia) == "0:\n|0:  |2:--[[two]]|0: |");
    CHECK(describe(tokens[1].trailingTrivia) == "0: |1:--three|");
    CHECK(describe(tokens[2].leadingTrivia) == "");

    std::vector<Token> unlexed = checkTokenize("{} \n\0rest"sv);
    REQUIRE(unlexed.size() == 3);
    CHECK(describe(unlexed[1].trailingTrivia) == "0: \n|");
    CHECK(describe(unlexed[2].leadingTrivia) == "3:\0rest|"s);
}

TEST_CASE("item_locations")
{
    std::string source = "{ 1, a = 2; [ \"b\" ] = { 3 } --[[x]] , 4 }";
    Allocator allocator;
    AstNameTable names(allocator);
    ParseNodeResult<AstExprTable> result = parse(source, names, allocator);
    REQUIRE(result.root);

    std::vector<std::string_view> items;
    for (size_t i = 0; i < result.root->items.size; i++)
        items.push_back(textAt(source, itemLocation(*result.root, i, result.cstNodeMap)));

    CHECK(items == std::vector<std::string_view>{"1,", "a = 2;", "[ \"b\" ] = { 3 } --[[x]] ,", "4"});
}

TEST_CASE("replace_edits_source")
{
    const std::string source = "-- settings\n{\n    a = 1, -- one\n    b = { 2 },\n}\n";
    CHECK(replace(source, {}).source == source);

    // Changing a value keeps the comments around it.
    CHECK(replace(source, {{Location{{2, 8}, {2, 9}}, "100"}}).source == "-- settings\n{\n    a = 100, -- one\n    b = { 2 },\n}\n");

    // Inserting and removing items.
    CHECK(
        replace(source, {{Location{{3, 14}, {3, 14}}, "\n    c = 3,"}, {Location{{2, 4}, {3, 4}}, ""}}).source ==
        "-- settings\n{\n    b = { 2 },\n    c = 3,\n}\n"
    );

    // Replacing the whole source, up to the end of its last line.
    CHECK(replace(source, {{Location{{0, 0}, {5, 0}}, "{}"}}).source == "{}");
}

TEST_CASE("replace_rejects_bad_replacements")
{
    const std::string source = "{\n    a = 1,\n}";

    auto errors = [&](std::vector<Replacement> replacements)
    {
        ReplaceResult result = replace(source, replacements);
        CHECK(!result.source);
        return result.errors;
    };

    CHECK(errors({{Location{{1, 9}, {1, 8}}, "2"}})[0].getMessage() == "Replacement range is reversed");
    CHECK(errors({{Location{{1, 8}, {1, 12}}, "2"}})[0].getMessage() == "Replacement is outside the source");
    CHECK(errors({{Location{{3, 0}, {3, 0}}, "2"}})[0].getMessage() == "Replacement is outside the source");
    CHECK(errors({{Location{{1, 4}, {1, 9}}, "b = 2"}, {Location{{1, 8}, {1, 9}}, "2"}})[0].getMessage() == "Replacement overlaps another");
    CHECK(errors({{Location{{1, 8}, {1, 8}}, "2"}, {Location{{1, 8}, {1, 8}}, "3"}})[0].getMessage() == "Replacement overlaps another");

    // Adjacent ranges don't overlap.
    CHECK(
        replace(source, {{Location{{1, 4}, {1, 5}}, "b"}, {Location{{1, 5}, {1, 5}}, "c"}, {Location{{1, 8}, {1, 9}}, "2"}}).source ==
        "{\n    bc = 2,\n}"
    );

    // The result must be well-formed, and its errors are located in it.
    std::vector<ParseError> malformed = errors({{Location{{1, 8}, {1, 9}}, "x"}});
    REQUIRE(malformed.size() == 1);
    CHECK(malformed[0].getLocation() == Location{{1, 8}, {1, 9}});
    CHECK(!errors({{Location{{1, 4}, {1, 4}}, "a = 0, "}}).empty());
}

TEST_SUITE_END();
