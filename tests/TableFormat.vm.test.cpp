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
        const LUA_VECTOR_TYPE* v = lua_tovector(L, index);
        return Vector{float(v[0]), float(v[1]), float(v[2])};
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
            const LUA_VECTOR_TYPE* actual = lua_tovector(L, -1);
            CHECK(sameNumber(expected.x, float(actual[0])));
            CHECK(sameNumber(expected.y, float(actual[1])));
            CHECK(sameNumber(expected.z, float(actual[2])));
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

    CHECK(count == table.getArray().size() + table.getMap().size());
}

// Reading a well-formed file must produce what running it in a Luau VM does.
static void checkMatchesVm(const std::string& source)
{
    INFO(source);

    ReadResult result = read(source);
    REQUIRE_MESSAGE(result.errors.empty(), result.errors[0].getMessage());
    REQUIRE(result.table);

    std::string module = source.find("return") == std::string::npos ? "return " + source : source;
    std::string bytecode = compile(module);

    std::unique_ptr<lua_State, void (*)(lua_State*)> state(luaL_newstate(), lua_close);
    lua_State* L = state.get();
    luaL_openlibs(L);

    REQUIRE(luau_load(L, "=test", bytecode.data(), bytecode.size(), 0) == 0);
    REQUIRE(lua_pcall(L, 0, 1, 0) == 0);
    REQUIRE(lua_istable(L, -1));

    checkSameAsVm(L, *result.table);
}

TEST_SUITE_BEGIN("TableFormatVm");

TEST_CASE("read_matches_vm")
{
    checkMatchesVm(R"(-- Settings for the example service.
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
)");
    checkMatchesVm(R"({ 'a', [[b]], `c`, "\x00\255\u{10FFFF}", 0x7fffffffffffffff, 1e-320, -0, 0.1 })");
    checkMatchesVm("{ -math.huge, math.nan, vector.create(0.1, -0, 1e39), vector.create(1, 2) }");
    checkMatchesVm("{ [2] = 2, 1, [3.5] = 3, [-1] = 4, [true] = { { {} } }, [vector.create(1, 2, 3)] = \"v\" }");
    checkMatchesVm(R"({ "\q\w\X41", '\%', `\!` })");
}

TEST_CASE("depth_limit_matches_vm")
{
    // The limit matches how deeply plain table constructors can nest in Luau.
    std::string deepest = std::string(kMaxDepth, '{') + std::string(kMaxDepth, '}');
    checkMatchesVm(deepest);
    CHECK(compile("return {" + deepest + "}").front() == 0); // a compile error
}

TEST_CASE("write_matches_vm")
{
    const double numbers[] = {0.1, 1.0 / 3.0, 1e300, 5e-324, 2.2250738585072014e-308, 123456789012345680000.0, -1e-7, 100, -0.0};
    const float floats[] = {0.1f, 1.0f / 3.0f, 3.4028235e38f, 1e-45f, -16777217.0f};
    const std::string strings[] = {std::string("\0\0001", 3), "\a\b\f\v\x7f", "\"'`{}[[]]", "\xff\xfe", "multi\r\nline", "end"};

    Table table;
    for (double n : numbers)
        CHECK(table.append(n));
    for (float f : floats)
        CHECK(table.append(Vector{f, -f, 0.0f}));
    for (const std::string& s : strings)
        CHECK(table.insert(s, s));

    WriteResult written = write(table);
    REQUIRE(written.source);
    checkMatchesVm(*written.source);
}

TEST_SUITE_END();
