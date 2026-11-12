// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"

#include "Luau/Compiler.h"

#include "lua.h"
#include "lualib.h"

#include "doctest.h"

#include <cmath>
#include <memory>
#include <string>

using namespace Luau;
using namespace Luau::TableFormat;

static Table readOk(const std::string& source)
{
    ReadResult result = read(source);
    INFO(source);
    REQUIRE_MESSAGE(result.errors.empty(), result.errors[0].getMessage());
    REQUIRE(result.table);
    return std::move(*result.table);
}

static void checkMalformed(const std::string& source)
{
    ReadResult result = read(source);
    INFO(source);
    CHECK(!result.errors.empty());
    CHECK(!result.table);
}

static double number(const Table& table, const Key& key)
{
    const Value* value = table.find(key);
    REQUIRE(value);
    REQUIRE(value->get_if<double>());
    return *value->get_if<double>();
}

static std::string string(const Table& table, const Key& key)
{
    const Value* value = table.find(key);
    REQUIRE(value);
    REQUIRE(value->get_if<std::string>());
    return *value->get_if<std::string>();
}

static bool sameNumber(double a, double b)
{
    return (std::isnan(a) && std::isnan(b)) || (a == b && std::signbit(a) == std::signbit(b));
}

static Key toKey(lua_State* L, int index)
{
    switch (lua_type(L, index))
    {
    case LUA_TBOOLEAN:
        return bool(lua_toboolean(L, index));
    case LUA_TNUMBER:
        return lua_tonumber(L, index);
    case LUA_TSTRING:
    {
        size_t length = 0;
        const char* data = lua_tolstring(L, index, &length);
        return std::string(data, length);
    }
    case LUA_TVECTOR:
    {
        const float* v = lua_tovector(L, index);
        return Vector{v[0], v[1], v[2]};
    }
    default:
        FAIL("unexpected key type ", lua_typename(L, lua_type(L, index)));
        return false;
    }
}

// Checks that the Luau table at the top of the stack is identical to the table that was read.
static void checkSameAsVm(lua_State* L, const Table& table)
{
    size_t count = 0;

    lua_pushnil(L);
    while (lua_next(L, -2))
    {
        count++;

        const Value* value = table.find(toKey(L, -2));
        REQUIRE(value);

        switch (lua_type(L, -1))
        {
        case LUA_TBOOLEAN:
            REQUIRE(value->get_if<bool>());
            CHECK(*value->get_if<bool>() == bool(lua_toboolean(L, -1)));
            break;
        case LUA_TNUMBER:
            REQUIRE(value->get_if<double>());
            CHECK(sameNumber(*value->get_if<double>(), lua_tonumber(L, -1)));
            break;
        case LUA_TSTRING:
        {
            size_t length = 0;
            const char* data = lua_tolstring(L, -1, &length);
            REQUIRE(value->get_if<std::string>());
            CHECK(*value->get_if<std::string>() == std::string(data, length));
            break;
        }
        case LUA_TVECTOR:
        {
            REQUIRE(value->get_if<Vector>());
            const Vector& expected = *value->get_if<Vector>();
            const float* actual = lua_tovector(L, -1);
            CHECK(sameNumber(expected.x, actual[0]));
            CHECK(sameNumber(expected.y, actual[1]));
            CHECK(sameNumber(expected.z, actual[2]));
            break;
        }
        case LUA_TTABLE:
            REQUIRE(value->get_if<Table>());
            checkSameAsVm(L, *value->get_if<Table>());
            break;
        default:
            FAIL("unexpected value type ", lua_typename(L, lua_type(L, -1)));
        }

        lua_pop(L, 1);
    }

    CHECK(count == table.array().size() + table.map().size());
}

// Reading a well-formed file must produce what running it in a Luau VM does.
static void checkMatchesVm(const std::string& source)
{
    INFO(source);
    Table table = readOk(source);

    std::string module = source.find("return") == std::string::npos ? "return " + source : source;
    std::string bytecode = compile(module);

    std::unique_ptr<lua_State, void (*)(lua_State*)> state(luaL_newstate(), lua_close);
    lua_State* L = state.get();
    luaL_openlibs(L);

    REQUIRE(luau_load(L, "=test", bytecode.data(), bytecode.size(), 0) == 0);
    REQUIRE(lua_pcall(L, 0, 1, 0) == 0);
    REQUIRE(lua_istable(L, -1));

    checkSameAsVm(L, table);
}

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

TEST_SUITE_BEGIN("TableFormat");

TEST_CASE("read_rfc_example")
{
    Table table = readOk(kRfcExample);

    CHECK(table.array().empty());
    CHECK(table.map().size() == 11);
    CHECK(string(table, std::string("name")) == "example");
    CHECK(*table.find(std::string("enabled"))->get_if<bool>() == true);
    CHECK(number(table, std::string("retries")) == 3);
    CHECK(number(table, std::string("timeout")) == 2500);
    CHECK(number(table, std::string("flags")) == 0xa1);
    CHECK(number(table, std::string("threshold")) == -HUGE_VAL);
    CHECK(string(table, 1.0) == "first numeric key");
    CHECK(string(table, std::string("display name")) == "Example Service");

    const Vector* origin = table.find(std::string("origin"))->get_if<Vector>();
    REQUIRE(origin);
    CHECK(origin->x == 0.0f);
    CHECK(origin->y == 10.0f);
    CHECK(origin->z == -5.0f);

    const Table* tags = table.find(std::string("tags"))->get_if<Table>();
    REQUIRE(tags);
    REQUIRE(tags->array().size() == 2);
    CHECK(string(*tags, 2.0) == "beta");

    const Table* limits = table.find(std::string("limits"))->get_if<Table>();
    REQUIRE(limits);
    CHECK(number(*limits, true) == HUGE_VAL);
    CHECK(number(*limits, false) == 0);

    // The map part keeps the order the keys were written in.
    CHECK(*table.map()[0].key.get_if<std::string>() == "name");
    CHECK(*table.map()[10].key.get_if<std::string>() == "limits");
}

TEST_CASE("read_literals")
{
    Table table = readOk(R"({
        'single', "double", [[raw]], [==[raw ]] level]==], `backtick`,
        "\n\t\\\"\x41\u{48}\65\z
             skipped",
        1_000, 0xf_f, 0B11, 1e-3, .5, 5., 0.1, -0, 1e999, 0xffffffffffffffffff,
    })");

    REQUIRE(table.array().size() == 16);
    CHECK(string(table, 1.0) == "single");
    CHECK(string(table, 3.0) == "raw");
    CHECK(string(table, 4.0) == "raw ]] level");
    CHECK(string(table, 5.0) == "backtick");
    CHECK(string(table, 6.0) == "\n\t\\\"AHAskipped");
    CHECK(number(table, 7.0) == 1000);
    CHECK(number(table, 8.0) == 255);
    CHECK(number(table, 9.0) == 3);
    CHECK(number(table, 10.0) == 0.001);
    CHECK(number(table, 11.0) == 0.5);
    CHECK(number(table, 12.0) == 5);
    CHECK(number(table, 13.0) == 0.1);
    CHECK(std::signbit(number(table, 14.0)));
    CHECK(number(table, 15.0) == HUGE_VAL);
}

TEST_CASE("read_blessed_names")
{
    Table table = readOk("{ math.nan, math.huge, -math.huge, vector.create(1, 2), vector.create(-1, math.huge, math.nan) }");

    CHECK(std::isnan(number(table, 1.0)));
    CHECK(number(table, 2.0) == HUGE_VAL);
    CHECK(number(table, 3.0) == -HUGE_VAL);

    const Vector* two = table.find(4.0)->get_if<Vector>();
    REQUIRE(two);
    CHECK(two->z == 0.0f);

    const Vector* three = table.find(5.0)->get_if<Vector>();
    REQUIRE(three);
    CHECK(three->x == -1.0f);
    CHECK(three->y == HUGE_VALF);
    CHECK(std::isnan(three->z));
}

TEST_CASE("read_syntax_variations")
{
    readOk("{}");
    readOk("return {}");
    readOk("--[[ before ]] return --[==[ between ]==] {} -- after");
    readOk("{ 1; 2, a = 3; }");
    readOk("{ [2] = 2, 1 }");
    readOk("{};");
    readOk("return {};");
    readOk("return {} -- a\n; -- b");
    readOk("{ a = {} }\n;\n");
    readOk("{ [vector.create(1, 2, 3)] = 1, [vector.create(1, 2, 4)] = 2 }");
}

TEST_CASE("read_rejects_malformed")
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
    checkMalformed("{} 5");
    checkMalformed(";");
    checkMalformed("; {}");
    checkMalformed("{};;");
    checkMalformed("{}; ;");
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

TEST_CASE("read_allows_escaped_nul")
{
    CHECK(string(readOk("{ \"a\\0b\" }"), 1.0) == std::string("a\0b", 3));
}

static std::string nested(size_t depth)
{
    return std::string(depth, '{') + std::string(depth, '}');
}

static Table nestedTable(size_t depth)
{
    Table table;
    for (size_t i = 1; i < depth; i++)
    {
        Table outer;
        outer.append(std::move(table));
        table = std::move(outer);
    }
    return table;
}

TEST_CASE("depth_limit")
{
    readOk(nested(kMaxDepth));
    checkMalformed(nested(kMaxDepth + 1));
    checkMalformed(nested(100000));

    std::optional<std::string> deepest = write(nestedTable(kMaxDepth));
    REQUIRE(deepest);
    readOk(*deepest);

    CHECK(!write(nestedTable(kMaxDepth + 1)));
}

TEST_CASE("read_reports_locations")
{
    ReadResult result = read("{\n    a = 1,\n    b = x,\n}");
    REQUIRE(result.errors.size() == 1);
    CHECK(result.errors[0].getLocation().begin == Position{2, 8});
}

TEST_CASE("read_matches_vm")
{
    checkMatchesVm(kRfcExample);
    checkMatchesVm(R"({ 'a', [[b]], `c`, "\x00\255\u{10FFFF}", 0x7fffffffffffffff, 1e-320, -0, 0.1 })");
    checkMatchesVm("{ -math.huge, math.nan, vector.create(0.1, -0, 1e39), vector.create(1, 2) }");
    checkMatchesVm("{ [2] = 2, 1, [3.5] = 3, [-1] = 4, [true] = { { {} } }, [vector.create(1, 2, 3)] = \"v\" }");
}

TEST_CASE("table_keys")
{
    Table table;

    CHECK(table.append(std::string("a")));
    CHECK(table.insert(3.0, std::string("c")));
    CHECK(table.append(std::string("b")));
    CHECK(!table.append(std::string("x"))); // 3 is already in the map part

    CHECK(!table.insert(1.0, true));
    CHECK(!table.insert(3.0, true));
    CHECK(!table.insert(std::nan(""), true));
    CHECK(!table.insert(Vector{0.0f, std::nanf(""), 0.0f}, true));

    CHECK(table.insert(0.0, true));
    CHECK(!table.insert(-0.0, true));
    CHECK(table.insert(Vector{-0.0f, 1.0f, 0.0f}, true));
    CHECK(table.find(Vector{0.0f, 1.0f, -0.0f}));
    CHECK(table.insert(std::string("1"), true));
    CHECK(table.insert(false, true));

    CHECK(string(table, 2.0) == "b");
    CHECK(string(table, 3.0) == "c");
    CHECK(!table.find(4.0));
    CHECK(!table.find(1.5));
    CHECK(!table.find(std::nan("")));
}

TEST_CASE("table_remove")
{
    Table table = readOk("{ 1, 2, a = 1, b = 2, c = 3 }");

    CHECK(!table.remove(1.0)); // would leave a hole
    CHECK(table.remove(2.0));
    CHECK(table.remove(1.0));
    CHECK(table.array().empty());

    CHECK(table.remove(std::string("b")));
    CHECK(!table.remove(std::string("b")));
    CHECK(number(table, std::string("a")) == 1);
    CHECK(number(table, std::string("c")) == 3);
    CHECK(table.map().size() == 2);

    // Removing the last map entry keeps the rest findable.
    CHECK(table.remove(std::string("c")));
    CHECK(number(table, std::string("a")) == 1);
    CHECK(table.insert(std::string("d"), 4.0));
    CHECK(number(table, std::string("d")) == 4);

    // The array part can grow back into keys that were removed.
    CHECK(table.append(true));
}

TEST_CASE("table_edit_in_place")
{
    Table table = readOk("{ nested = { count = 1 } }");

    Table* nested = table.find(std::string("nested"))->get_if<Table>();
    REQUIRE(nested);
    *nested->find(std::string("count")) = 2.0;

    CHECK(write(table) == "{\n    nested = {\n        count = 2,\n    },\n}\n");
}

TEST_CASE("write_format")
{
    Table table = readOk(R"({
        "a", true, {},
        name = "x", ["not a name"] = 1, ["end"] = 2, ["continue"] = 3, ["1a"] = 4, _1 = 5,
        [2.5] = -0, [-1] = math.huge, [false] = math.nan, [vector.create(1, 0.5, -2)] = { 1 },
    })");

    CHECK(write(table) == R"({
    "a",
    true,
    {},
    name = "x",
    ["not a name"] = 1,
    ["end"] = 2,
    continue = 3,
    ["1a"] = 4,
    _1 = 5,
    [2.5] = -0,
    [-1] = math.huge,
    [false] = math.nan,
    [vector.create(1, 0.5, -2)] = {
        1,
    },
}
)");

    readOk(*write(table));
    CHECK(write(Table{}) == "{}\n");
}

TEST_CASE("write_whole_numbers")
{
    Table table = readOk("{ 10, 100, 1e6, -1e14, 99999999999999.5, 1e15, vector.create(10, 20, 300) }");

    CHECK(write(table) == R"({
    10,
    100,
    1000000,
    -100000000000000,
    99999999999999.5,
    1e+15,
    vector.create(10, 20, 300),
}
)");
}

TEST_CASE("write_numbers_and_strings_round_trip")
{
    const double numbers[] = {0.1, 1.0 / 3.0, 1e300, 5e-324, 2.2250738585072014e-308, 9007199254740993.0, 123456789012345680000.0, -1e-7};
    const float floats[] = {0.1f, 1.0f / 3.0f, 3.4028235e38f, 1e-45f, -16777217.0f};
    const std::string strings[] = {std::string("\0\0001", 3), "\a\b\f\v\x7f", "\"'`{}[[]]", "\xff\xfe", "multi\r\nline"};

    Table table;
    for (double n : numbers)
        CHECK(table.append(n));
    for (float f : floats)
        CHECK(table.append(Vector{f, -f, 0.0f}));
    for (const std::string& s : strings)
        CHECK(table.insert(s, s));

    std::string source = *write(table);
    INFO(source);
    Table copy = readOk(source);

    CHECK(write(copy) == source);
    checkMatchesVm(source);

    for (size_t i = 0; i < std::size(numbers); i++)
        CHECK(number(copy, double(i + 1)) == numbers[i]);
    for (size_t i = 0; i < std::size(floats); i++)
        CHECK(copy.find(double(std::size(numbers) + i + 1))->get_if<Vector>()->x == floats[i]);
    for (const std::string& s : strings)
        CHECK(string(copy, s) == s);
}

TEST_SUITE_END();
