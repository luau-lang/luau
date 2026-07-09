// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Compiler.h"

#include "ScopedFlags.h"
#include "lua.h"
#include "luacodegen.h"
#include "lualib.h"

#include "doctest.h"

#include <memory>
#include <string>

LUAU_FASTFLAG(LuauDirectFieldGet)

// For ease of testing, this counter is static. Tests that use it should reset
// its value to 0 at startup and verify its value after code has run.
static int handlerHitCount = 0;

static constexpr int kTagVec2 = 42;
static constexpr int kTagOther = 43;

struct Vec2
{
    double x;
    double y;
};

static int lua_createVec2(lua_State* L)
{
    double x = luaL_checknumber(L, 1);
    double y = luaL_checknumber(L, 2);
    Vec2* p = static_cast<Vec2*>(lua_newuserdatatagged(L, sizeof(Vec2), kTagVec2));
    p->x = x;
    p->y = y;
    return 1;
}

static int lua_createOtherWithMt(lua_State* L)
{
    lua_newuserdatataggedwithmetatable(L, sizeof(Vec2), kTagOther);
    return 1;
}

static int lua_createOtherWithoutMt(lua_State* L)
{
    lua_newuserdatatagged(L, sizeof(Vec2), kTagOther);
    return 1;
}

static int runCode(lua_State* L, const std::string& source)
{
    std::string bytecode = Luau::compile(source, {});
    if (luau_load(L, "test", bytecode.data(), bytecode.size(), 0) != 0)
        return -1; // load failed

    return lua_pcall(L, 0, LUA_MULTRET, 0);
}

TEST_SUITE_BEGIN("DirectFieldAccess");

TEST_CASE("handler_setnumber_result")
{
    ScopedFastFlag sff{FFlag::LuauDirectFieldGet, true};

    std::unique_ptr<lua_State, void (*)(lua_State*)> state(luaL_newstate(), lua_close);
    lua_State* L = state.get();

    lua_registeruserdatadirectfieldget(
        L,
        kTagVec2,
        "X",
        [](void* ud, void* res)
        {
            lua_userdatadirectfield_setnumber(res, static_cast<Vec2*>(ud)->x);
        }
    );

    lua_pushcfunction(L, lua_createVec2, "createVec2");
    lua_setglobal(L, "createVec2");

    int status = runCode(L, R"(
        local v = createVec2(3.5, 0)
        return v.X
    )");
    REQUIRE(status == LUA_OK);

    REQUIRE(lua_isnumber(L, -1));
    CHECK(lua_tonumber(L, -1) == 3.5);
}

TEST_CASE("handler_setboolean_result")
{
    ScopedFastFlag sff{FFlag::LuauDirectFieldGet, true};

    std::unique_ptr<lua_State, void (*)(lua_State*)> state(luaL_newstate(), lua_close);
    lua_State* L = state.get();

    lua_registeruserdatadirectfieldget(
        L,
        kTagVec2,
        "NonZero",
        [](void* ud, void* r)
        {
            lua_userdatadirectfield_setboolean(r, static_cast<int>(static_cast<Vec2*>(ud)->x != 0 || static_cast<Vec2*>(ud)->y != 0));
        }
    );

    lua_pushcfunction(L, lua_createVec2, "createVec2");
    lua_setglobal(L, "createVec2");

    {
        int status = runCode(L, R"(
            local v = createVec2(1, 0)
            return v.NonZero
        )");
        REQUIRE(status == LUA_OK);
        REQUIRE(lua_isboolean(L, -1));
        CHECK(lua_toboolean(L, -1) == 1);
    }
    {
        int status = runCode(L, R"(
            local v = createVec2(0, 0)
            return v.NonZero
        )");
        REQUIRE(status == LUA_OK);
        REQUIRE(lua_isboolean(L, -1));
        CHECK(lua_toboolean(L, -1) == 0);
    }
}

TEST_CASE("repeated_access_handler_called_every_iteration")
{
    ScopedFastFlag sff{FFlag::LuauDirectFieldGet, true};

    std::unique_ptr<lua_State, void (*)(lua_State*)> state(luaL_newstate(), lua_close);
    lua_State* L = state.get();

    handlerHitCount = 0;
    lua_registeruserdatadirectfieldget(
        L,
        kTagVec2,
        "X",
        [](void* ud, void* r)
        {
            handlerHitCount++;
            lua_userdatadirectfield_setnumber(r, static_cast<Vec2*>(ud)->x);
        }
    );

    lua_pushcfunction(L, lua_createVec2, "createVec2");
    lua_setglobal(L, "createVec2");

    int status = runCode(L, R"(
        local v = createVec2(7, 0)
        local sum = 0
        for i = 1, 5 do
            sum = sum + v.X
        end
        return sum
    )");
    REQUIRE(status == LUA_OK);
    REQUIRE(lua_isnumber(L, -1));
    CHECK(lua_tonumber(L, -1) == 35);

    CHECK(handlerHitCount == 5);
}

TEST_CASE("unregistered_tag_falls_through_to_index_metamethod")
{
    ScopedFastFlag sff{FFlag::LuauDirectFieldGet, true};

    std::unique_ptr<lua_State, void (*)(lua_State*)> state(luaL_newstate(), lua_close);
    lua_State* L = state.get();
    luaL_openlibs(L);

    handlerHitCount = 0;
    lua_registeruserdatadirectfieldget(
        L,
        kTagVec2,
        "X",
        [](void* ud, void* r)
        {
            handlerHitCount++;
            lua_userdatadirectfield_setnumber(r, static_cast<Vec2*>(ud)->x);
        }
    );

    // Give kTagOther a metatable whose __index returns -1 for any field.
    luaL_newmetatable(L, "metaOther");
    lua_pushcfunction(
        L,
        [](lua_State* L) -> int
        {
            lua_pushnumber(L, -1);
            return 1;
        },
        "__index"
    );
    lua_setfield(L, -2, "__index");
    lua_setuserdatametatable(L, kTagOther);

    lua_pushcfunction(L, lua_createVec2, "createVec2");
    lua_setglobal(L, "createVec2");
    lua_pushcfunction(L, lua_createOtherWithMt, "createOther");
    lua_setglobal(L, "createOther");

    int status = runCode(L, R"(
        local uds = {createVec2(1, 0), createOther()}
        local results = {}
        for _, v in uds do
            results[#results + 1] = v.X
        end
        return table.unpack(results)
    )");
    REQUIRE(status == LUA_OK);
    REQUIRE(lua_gettop(L) == 2);

    CHECK(lua_tonumber(L, -2) == 1);  // direct dispatch worked
    CHECK(lua_tonumber(L, -1) == -1); // kTagOther has no dispatch table, fell back to __index

    CHECK(handlerHitCount == 1); // handler was only hit for the Vec2
}

TEST_CASE("multiple_fields_same_type_dispatch_independently")
{
    ScopedFastFlag sff{FFlag::LuauDirectFieldGet, true};

    std::unique_ptr<lua_State, void (*)(lua_State*)> state(luaL_newstate(), lua_close);
    lua_State* L = state.get();

    lua_registeruserdatadirectfieldget(
        L,
        kTagVec2,
        "X",
        [](void* ud, void* r)
        {
            lua_userdatadirectfield_setnumber(r, static_cast<Vec2*>(ud)->x);
        }
    );
    lua_registeruserdatadirectfieldget(
        L,
        kTagVec2,
        "Y",
        [](void* ud, void* r)
        {
            lua_userdatadirectfield_setnumber(r, static_cast<Vec2*>(ud)->y);
        }
    );

    lua_pushcfunction(L, lua_createVec2, "createVec2");
    lua_setglobal(L, "createVec2");

    int status = runCode(L, R"(
        local v = createVec2(1.5, 2.5)
        return v.X, v.Y
    )");
    REQUIRE(status == LUA_OK);
    REQUIRE(lua_gettop(L) == 2);

    CHECK(lua_tonumber(L, -2) == 1.5);
    CHECK(lua_tonumber(L, -1) == 2.5);
}

TEST_CASE("same_field_name_different_tags_dispatch_independently")
{
    ScopedFastFlag sff{FFlag::LuauDirectFieldGet, true};

    std::unique_ptr<lua_State, void (*)(lua_State*)> state(luaL_newstate(), lua_close);
    lua_State* L = state.get();
    luaL_openlibs(L);

    handlerHitCount = 0;
    lua_registeruserdatadirectfieldget(
        L,
        kTagVec2,
        "X",
        [](void* ud, void* r)
        {
            lua_userdatadirectfield_setnumber(r, static_cast<Vec2*>(ud)->x);
            handlerHitCount++;
        }
    );
    lua_registeruserdatadirectfieldget(
        L,
        kTagOther,
        "X",
        [](void* ud, void* r)
        {
            lua_userdatadirectfield_setnumber(r, 999);
            handlerHitCount++;
        }
    );

    lua_pushcfunction(L, lua_createVec2, "createVec2");
    lua_setglobal(L, "createVec2");

    lua_pushcfunction(L, lua_createOtherWithoutMt, "createOther");
    lua_setglobal(L, "createOther");

    int status = runCode(L, R"(
        return createVec2(3, 0).X, createOther().X
    )");
    REQUIRE(status == LUA_OK);
    REQUIRE(lua_gettop(L) == 2);

    CHECK(lua_tonumber(L, -2) == 3);
    CHECK(lua_tonumber(L, -1) == 999);
    CHECK(handlerHitCount == 2);
}

TEST_SUITE_END();
