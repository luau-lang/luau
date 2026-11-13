// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"

#include "Luau/Compiler.h"

#include "lua.h"
#include "lualib.h"

#include "TableFormatExamples.h"

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

static bool samePrimitive(const Value& value, const Key& key)
{
    if (const bool* boolean = key.get_if<bool>())
        return value.get_if<bool>() && *value.get_if<bool>() == *boolean;
    if (const double* number = key.get_if<double>())
        return value.get_if<double>() && sameNumber(*value.get_if<double>(), *number);
    if (const std::string* string = key.get_if<std::string>())
        return value.get_if<std::string>() && *value.get_if<std::string>() == *string;

    const Vector& vector = *key.get_if<Vector>();
    const Vector* other = value.get_if<Vector>();
    return other && sameNumber(other->x, vector.x) && sameNumber(other->y, vector.y) && sameNumber(other->z, vector.z);
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

        if (lua_istable(L, -1))
        {
            REQUIRE(value->get_if<Table>());
            checkSameAsVm(L, *value->get_if<Table>());
        }
        else
            CHECK(samePrimitive(*value, toKey(L, -1)));

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
    checkMatchesVm(kTableFormatRfcExample);
    checkMatchesVm(R"({ 'a', [[b]], `c`, "\x00\255\u{10FFFF}", 0x7fffffffffffffff, 1e-320, -0, 0.1 })");
    checkMatchesVm("{ -math.huge, math.nan, vector.create(0.1, -0, 1e39), vector.create(1, 2) }");
    checkMatchesVm("{ [2] = 2, 1, [3.5] = 3, [-1] = 4, [true] = { { {} } }, [vector.create(1, 2, 3)] = \"v\" }");
    checkMatchesVm(R"({ "\q\w\X41", '\%', `\!` })");
}

TEST_CASE("depth_limit_matches_vm")
{
    // The limit matches how deeply plain table constructors can nest in Luau.
    std::string deepest = tableFormatNested(kMaxDepth);
    checkMatchesVm(deepest);
    CHECK(compile("return {" + deepest + "}").front() == 0); // a compile error
}

TEST_CASE("write_matches_vm")
{
    Table table;
    for (double n : kTableFormatRoundTripNumbers)
        CHECK(table.append(n));
    for (float f : kTableFormatRoundTripFloats)
        CHECK(table.append(Vector{f, -f, 0.0f}));
    for (const std::string& s : kTableFormatRoundTripStrings)
        CHECK(table.insert(s, s));

    WriteResult written = write(table);
    REQUIRE(written.source);
    checkMatchesVm(*written.source);
}

TEST_SUITE_END();
