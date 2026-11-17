// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"
#include "Luau/Common.h"

#include "lua.h"
#include "lualib.h"

#include <math.h>
#include <stdint.h>
#include <string.h>

using namespace Luau::TableFormat;

static bool sameNumber(double a, double b)
{
    return (isnan(a) && isnan(b)) || (a == b && signbit(a) == signbit(b));
}

// Whether two values on the stack are identical: tables by contents, numbers and vector components by value and sign.
static bool sameValue(lua_State* L, int a, int b)
{
    a = lua_absindex(L, a);
    b = lua_absindex(L, b);

    if (lua_type(L, a) != lua_type(L, b))
        return false;

    if (lua_type(L, a) == LUA_TNUMBER)
        return sameNumber(lua_tonumber(L, a), lua_tonumber(L, b));

    if (lua_type(L, a) == LUA_TVECTOR)
    {
        const LUA_VECTOR_TYPE* x = lua_tovector(L, a);
        const LUA_VECTOR_TYPE* y = lua_tovector(L, b);
        return sameNumber(x[0], y[0]) && sameNumber(x[1], y[1]) && sameNumber(x[2], y[2]);
    }

    if (lua_type(L, a) != LUA_TTABLE)
        return lua_rawequal(L, a, b);

    int count = 0;
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

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
    {
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;
    }

    lua_State* L = luaL_newstate();

    DeserializeResult result = deserialize(L, std::string_view(reinterpret_cast<const char*>(Data), Size));
    LUAU_ASSERT(lua_gettop(L) == (result.errors.empty() ? 1 : 0));

    if (result.errors.empty())
    {
        // Anything that can be read can be written, and what was written reads back as an identical table.
        SerializeResult written = serialize(L, -1);
        LUAU_ASSERT(written.source);

        LUAU_ASSERT(deserialize(L, *written.source).errors.empty());
        LUAU_ASSERT(sameValue(L, -2, -1));
    }

    lua_close(L);
    return 0;
}
