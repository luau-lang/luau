// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"

#include "Luau/Compiler.h"

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
    }

    void checkMalformed(const std::string& source)
    {
        INFO(source);
        int top = lua_gettop(L);
        DeserializeResult result = deserialize(L, source);
        CHECK(!result.errors.empty());
        CHECK(lua_gettop(L) == top);
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

    // Serializes the table that Luau source returns.
    SerializeResult serializeLuau(const std::string& source)
    {
        run(source);
        SerializeResult result = serialize(L, -1);
        lua_pop(L, 1);
        return result;
    }

    std::string serializeOk(const std::string& source)
    {
        INFO(source);
        SerializeResult result = serializeLuau(source);
        REQUIRE_MESSAGE(result.source, result.error);
        return *result.source;
    }

    std::string serializeError(const std::string& source)
    {
        INFO(source);
        SerializeResult result = serializeLuau(source);
        CHECK(!result.source);
        return result.error;
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
        CHECK(serialize(L, -1).source == result.source);
        lua_pop(L, 1);

        checkMatchesVm(*result.source);
    }
};

TEST_SUITE_BEGIN("TableFormat");

TEST_CASE_FIXTURE(TableFormatFixture, "deserialize_rfc_example")
{
    checkMatchesVm(kRfcExample);

    deserializeOk(kRfcExample);
    lua_getfield(L, -1, "name");
    CHECK(std::string(lua_tostring(L, -1)) == "example");
    lua_pop(L, 2);
}

TEST_CASE_FIXTURE(TableFormatFixture, "deserialize_literals")
{
    checkMatchesVm(R"({
        'single', "double", [[raw]], [==[raw ]] level]==], `backtick`,
        "\n\t\\\"\x41\u{48}\65\z
             skipped",
        1_000, 0xf_f, 0B11, 1e-3, .5, 5., 0.1, -0, 1e999, 0xffffffffffffffffff,
    })");

    deserializeOk(R"({ "\n\t\\\"\x41\u{48}\65\z
        skipped", -0 })");
    lua_rawgeti(L, -1, 1);
    CHECK(std::string(lua_tostring(L, -1)) == "\n\t\\\"AHAskipped");
    lua_rawgeti(L, -2, 2);
    CHECK(std::signbit(lua_tonumber(L, -1)));
    lua_pop(L, 3);
}

TEST_CASE_FIXTURE(TableFormatFixture, "deserialize_matches_vm")
{
    checkMatchesVm("{ math.nan, math.huge, -math.huge, vector.create(1, 2), vector.create(-1, math.huge, math.nan) }");
    checkMatchesVm(R"({ 'a', [[b]], `c`, "\x00\255\u{10FFFF}", 0x7fffffffffffffff, 1e-320, -0, 0.1 })");
    checkMatchesVm("{ -math.huge, math.nan, vector.create(0.1, -0, 1e39), vector.create(1, 2) }");
    checkMatchesVm("{ [2] = 2, 1, [3.5] = 3, [-1] = 4, [true] = { { {} } }, [vector.create(1, 2, 3)] = \"v\" }");
    checkMatchesVm(R"({ "\q\w\X41", '\%', `\!` })");
}

TEST_CASE_FIXTURE(TableFormatFixture, "deserialize_syntax_variations")
{
    for (const char* source : {
             "{}",
             "return {}",
             "--[[ before ]] return --[==[ between ]==] {} -- after",
             "{ 1; 2, a = 3; }",
             "{ [2] = 2, 1 }",
             "{};",
             "return {};",
             "return {} -- a\n; -- b",
             "{ a = {} }\n;\n",
             "{ [vector.create(1, 2, 3)] = 1, [vector.create(1, 2, 4)] = 2 }",
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

TEST_CASE_FIXTURE(TableFormatFixture, "deserialize_allows_escaped_nul")
{
    deserializeOk("{ \"a\\0b\" }");
    lua_rawgeti(L, -1, 1);
    size_t length = 0;
    const char* data = lua_tolstring(L, -1, &length);
    CHECK(std::string(data, length) == std::string("a\0b", 3));
    lua_pop(L, 2);
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

    checkMalformed(nested(kMaxDepth + 1));
    checkMalformed(nested(100000));

    DeserializeResult tooDeep = deserialize(L, nested(kMaxDepth + 1));
    REQUIRE(tooDeep.errors.size() == 1);
    CHECK(tooDeep.errors[0].getMessage() == "Exceeded the maximum table nesting depth of 255");

    deserializeOk(nested(kMaxDepth));
    SerializeResult deepest = serialize(L, -1);
    lua_pop(L, 1);
    CHECK(deepest.source == nested(kMaxDepth));

    CHECK(serializeError("local t = {} for i = 1, 255 do t = { t } end return t") == tooDeep.errors[0].getMessage());
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_format")
{
    CHECK(serializeOk("return {}") == "{}");
    CHECK(serializeOk("return { a = { b = { 1, 2 } } }") == "{a={b={1,2}}}");

    // The array part comes first, then the other keys: booleans, numbers, strings (by byte) and vectors.
    CHECK(
        serializeOk(R"(return {
            "a", true, {},
            name = "x", ["not a name"] = 1, ["end"] = 2, ["continue"] = 3, ["1a"] = 4, _1 = 5,
            [2.5] = -0, [-1] = math.huge, [false] = math.nan, [vector.create(1, 0.5, -2)] = { 1 },
        })") ==
        R"({"a",true,{},[false]=math.nan,[-1]=math.huge,[2.5]=-0,["1a"]=4,_1=5,continue=3,["end"]=2,name="x",["not a name"]=1,[vector.create(1,0.5,-2)]={1}})"
    );
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_is_independent_of_insertion_order")
{
    std::string forwards = serializeOk("local t = {} for i = 1, 100 do t['k' .. i] = i end t[1.5] = 0 t[true] = 1 return t");
    std::string backwards = serializeOk("local t = {} t[true] = 1 t[1.5] = 0 for i = 100, 1, -1 do t['k' .. i] = i end return t");
    CHECK(forwards == backwards);
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_array_part_stops_at_first_nil")
{
    CHECK(serializeOk("local t = { 1, 2 } t[4] = 4 return t") == "{1,2,[4]=4}");
    CHECK(serializeOk("return { [2] = 2 }") == "{[2]=2}");
    CHECK(serializeOk("return { [1] = 1, [2] = 2 }") == "{1,2}");
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_ignores_metatables")
{
    CHECK(serializeOk("return setmetatable({ a = 1 }, { __index = { b = 2 } })") == "{a=1}");
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_rejects_what_luau_table_cant_hold")
{
    CHECK(serializeError("return { print }") == "Cannot serialize a value of type function");
    CHECK(serializeError("return { buffer.create(1) }") == "Cannot serialize a value of type buffer");
    CHECK(serializeError("return { coroutine.create(print) }") == "Cannot serialize a value of type thread");
    CHECK(serializeError("return { [{}] = 1 }") == "Cannot use a table as a key");
    CHECK(serializeError("return { [print] = 1 }") == "Cannot use a value of type function as a key");
    CHECK(serializeError("local t = {} t.self = t return t") == "Cannot serialize a table that contains itself");
    CHECK(serializeError("local t = {} t.a = { b = { t } } return t") == "Cannot serialize a table that contains itself");

    // The same table can appear more than once, as long as it doesn't contain itself.
    CHECK(serializeOk("local shared = { 1 } return { shared, shared }") == "{{1},{1}}");

    lua_pushnumber(L, 1);
    CHECK(serialize(L, -1).error == "Expected a table");
    lua_pop(L, 1);

    lua_newtable(L);
    lua_newuserdata(L, 1);
    lua_rawseti(L, -2, 1);
    CHECK(serialize(L, -1).error == "Cannot serialize a value of type userdata");
    lua_pop(L, 1);
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_leaves_stack_alone")
{
    run("return { 1, { 2, a = print } }");
    int top = lua_gettop(L);
    CHECK(!serialize(L, -1).source);
    CHECK(lua_gettop(L) == top);
    lua_pop(L, 1);
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_names_are_ascii")
{
    CHECK(serializeOk("return { ['caf\\xc3\\xa9'] = 1, _x9 = 2 }") == "{_x9=2,[\"caf\xc3\xa9\"]=1}");
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_whole_numbers")
{
    CHECK(
        serializeOk("return { 10, 100, 1e6, -1e14, 99999999999999.5, 1e15, vector.create(10, 20, 300) }") ==
        "{10,100,1000000,-100000000000000,99999999999999.5,1000000000000000,vector.create(10,20,300)}"
    );
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_minimal_output_reads_back")
{
    // Without spaces, a minus sign follows `=`, `,`, `{` and `(`, so check none of those lex as something else.
    deserializeOk("{ -1, -math.huge, a = -0, [-1] = -2.5, [vector.create(-1, -2, -3)] = { -1e-7 } }");
    CHECK(serialize(L, -1).source == "{-1,-math.huge,[-1]=-2.5,a=-0,[vector.create(-1,-2,-3)]={-1e-07}}");
    checkRoundTrip();
    lua_pop(L, 1);
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_vector_components_read_back_through_doubles")
{
    // The shortest float representation of this component, 7.038531e-26, reads back as a different float when it's read
    // as a double first, as Luau does, so it's written with one more digit.
    uint32_t bits = 0x15ae43fd;
    float component;
    memcpy(&component, &bits, sizeof(component));

    lua_newtable(L);
#if LUA_VECTOR_SIZE == 4
    lua_pushvector(L, component, 0.1f, 0.0f, 0.0f);
#else
    lua_pushvector(L, component, 0.1f, 0.0f);
#endif
    lua_rawseti(L, -2, 1);

    CHECK(serialize(L, -1).source == "{vector.create(7.0385307e-26,0.1,0)}");
    checkRoundTrip();
    lua_pop(L, 1);
}

TEST_CASE_FIXTURE(TableFormatFixture, "serialize_numbers_and_strings_round_trip")
{
    const double numbers[] = {
        0.1, 1.0 / 3.0, 1e300, 5e-324, 2.2250738585072014e-308, 9007199254740993.0, 123456789012345680000.0, -1e-7, 100, -0.0, HUGE_VAL, -HUGE_VAL
    };
    const float floats[] = {0.1f, 1.0f / 3.0f, 3.4028235e38f, 1e-45f, -16777217.0f};
    const std::string strings[] = {std::string("\0\0001", 3), "\a\b\f\v\x7f", "\"'`{}[[]]", "\xff\xfe", "multi\r\nline", "end", ""};

    lua_newtable(L);
    int index = 1;
    for (double n : numbers)
    {
        lua_pushnumber(L, n);
        lua_rawseti(L, -2, index++);
    }
    for (float f : floats)
    {
#if LUA_VECTOR_SIZE == 4
        lua_pushvector(L, f, -f, 0.0f, 0.0f);
#else
        lua_pushvector(L, f, -f, 0.0f);
#endif
        lua_rawseti(L, -2, index++);
    }
    for (const std::string& s : strings)
    {
        lua_pushlstring(L, s.data(), s.size());
        lua_pushlstring(L, s.data(), s.size());
        lua_rawset(L, -3);
    }

    checkRoundTrip();
    lua_pop(L, 1);
}

TEST_SUITE_END();
