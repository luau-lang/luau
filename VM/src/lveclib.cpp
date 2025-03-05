// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lualib.h"

#include "lcommon.h"
#include "lnumutils.h"

#include <math.h>

static int vector_create(lua_State* L)
{
    // checking argument count to avoid accepting 'nil' as a valid value
    int count = lua_gettop(L);

    double x = luaL_checknumber(L, 1);
    double y = luaL_checknumber(L, 2);
    double z = count >= 3 ? luaL_checknumber(L, 3) : 0.0;

#if LUA_VECTOR_SIZE == 4
    double w = count >= 4 ? luaL_checknumber(L, 4) : 0.0;

    lua_pushvector(L, float(x), float(y), float(z), float(w));
#else
    lua_pushvector(L, float(x), float(y), float(z));
#endif

    return 1;
}

static int vector_magnitude(lua_State* L)
{
    const float* v = luaL_checkvector(L, 1);

#if LUA_VECTOR_SIZE == 4
    lua_pushnumber(L, sqrtf(v[0] * v[0] + v[1] * v[1] + v[2] * v[2] + v[3] * v[3]));
#else
    lua_pushnumber(L, sqrtf(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]));
#endif

    return 1;
}

static int vector_normalize(lua_State* L)
{
    const float* v = luaL_checkvector(L, 1);

#if LUA_VECTOR_SIZE == 4
    float invSqrt = 1.0f / sqrtf(v[0] * v[0] + v[1] * v[1] + v[2] * v[2] + v[3] * v[3]);

    lua_pushvector(L, v[0] * invSqrt, v[1] * invSqrt, v[2] * invSqrt, v[3] * invSqrt);
#else
    float invSqrt = 1.0f / sqrtf(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);

    lua_pushvector(L, v[0] * invSqrt, v[1] * invSqrt, v[2] * invSqrt);
#endif

    return 1;
}

static int vector_cross(lua_State* L)
{
    const float* a = luaL_checkvector(L, 1);
    const float* b = luaL_checkvector(L, 2);

#if LUA_VECTOR_SIZE == 4
    lua_pushvector(L, a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0], 0.0f);
#else
    lua_pushvector(L, a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]);
#endif

    return 1;
}

static int vector_dot(lua_State* L)
{
    const float* a = luaL_checkvector(L, 1);
    const float* b = luaL_checkvector(L, 2);

#if LUA_VECTOR_SIZE == 4
    lua_pushnumber(L, a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3]);
#else
    lua_pushnumber(L, a[0] * b[0] + a[1] * b[1] + a[2] * b[2]);
#endif

    return 1;
}

static int vector_angle(lua_State* L)
{
    const float* a = luaL_checkvector(L, 1);
    const float* b = luaL_checkvector(L, 2);
    const float* axis = luaL_optvector(L, 3, nullptr);

    // cross(a, b)
    float cross[] = {a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]};

    double sinA = sqrt(cross[0] * cross[0] + cross[1] * cross[1] + cross[2] * cross[2]);
    double cosA = a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
    double angle = atan2(sinA, cosA);

    if (axis)
    {
        if (cross[0] * axis[0] + cross[1] * axis[1] + cross[2] * axis[2] < 0.0f)
            angle = -angle;
    }

    lua_pushnumber(L, angle);
    return 1;
}

static int vector_floor(lua_State* L)
{
    const float* v = luaL_checkvector(L, 1);

#if LUA_VECTOR_SIZE == 4
    lua_pushvector(L, floorf(v[0]), floorf(v[1]), floorf(v[2]), floorf(v[3]));
#else
    lua_pushvector(L, floorf(v[0]), floorf(v[1]), floorf(v[2]));
#endif

    return 1;
}

static int vector_ceil(lua_State* L)
{
    const float* v = luaL_checkvector(L, 1);

#if LUA_VECTOR_SIZE == 4
    lua_pushvector(L, ceilf(v[0]), ceilf(v[1]), ceilf(v[2]), ceilf(v[3]));
#else
    lua_pushvector(L, ceilf(v[0]), ceilf(v[1]), ceilf(v[2]));
#endif

    return 1;
}

static int vector_abs(lua_State* L)
{
    const float* v = luaL_checkvector(L, 1);

#if LUA_VECTOR_SIZE == 4
    lua_pushvector(L, fabsf(v[0]), fabsf(v[1]), fabsf(v[2]), fabsf(v[3]));
#else
    lua_pushvector(L, fabsf(v[0]), fabsf(v[1]), fabsf(v[2]));
#endif

    return 1;
}

static int vector_sign(lua_State* L)
{
    const float* v = luaL_checkvector(L, 1);

#if LUA_VECTOR_SIZE == 4
    lua_pushvector(L, luaui_signf(v[0]), luaui_signf(v[1]), luaui_signf(v[2]), luaui_signf(v[3]));
#else
    lua_pushvector(L, luaui_signf(v[0]), luaui_signf(v[1]), luaui_signf(v[2]));
#endif

    return 1;
}

static int vector_clamp(lua_State* L)
{
    const float* v = luaL_checkvector(L, 1);
    const float* min = luaL_checkvector(L, 2);
    const float* max = luaL_checkvector(L, 3);

    luaL_argcheck(L, min[0] <= max[0], 3, "max.x must be greater than or equal to min.x");
    luaL_argcheck(L, min[1] <= max[1], 3, "max.y must be greater than or equal to min.y");
    luaL_argcheck(L, min[2] <= max[2], 3, "max.z must be greater than or equal to min.z");

#if LUA_VECTOR_SIZE == 4
    lua_pushvector(
        L,
        luaui_clampf(v[0], min[0], max[0]),
        luaui_clampf(v[1], min[1], max[1]),
        luaui_clampf(v[2], min[2], max[2]),
        luaui_clampf(v[3], min[3], max[3])
    );
#else
    lua_pushvector(L, luaui_clampf(v[0], min[0], max[0]), luaui_clampf(v[1], min[1], max[1]), luaui_clampf(v[2], min[2], max[2]));
#endif

    return 1;
}

static int vector_min(lua_State* L)
{
    int n = lua_gettop(L);
    const float* v = luaL_checkvector(L, 1);

#if LUA_VECTOR_SIZE == 4
    float result[] = {v[0], v[1], v[2], v[3]};
#else
    float result[] = {v[0], v[1], v[2]};
#endif

    for (int i = 2; i <= n; i++)
    {
        const float* b = luaL_checkvector(L, i);

        if (b[0] < result[0])
            result[0] = b[0];
        if (b[1] < result[1])
            result[1] = b[1];
        if (b[2] < result[2])
            result[2] = b[2];
#if LUA_VECTOR_SIZE == 4
        if (b[3] < result[3])
            result[3] = b[3];
#endif
    }

#if LUA_VECTOR_SIZE == 4
    lua_pushvector(L, result[0], result[1], result[2], result[3]);
#else
    lua_pushvector(L, result[0], result[1], result[2]);
#endif

    return 1;
}

static int vector_max(lua_State* L)
{
    int n = lua_gettop(L);
    const float* v = luaL_checkvector(L, 1);

#if LUA_VECTOR_SIZE == 4
    float result[] = {v[0], v[1], v[2], v[3]};
#else
    float result[] = {v[0], v[1], v[2]};
#endif

    for (int i = 2; i <= n; i++)
    {
        const float* b = luaL_checkvector(L, i);

        if (b[0] > result[0])
            result[0] = b[0];
        if (b[1] > result[1])
            result[1] = b[1];
        if (b[2] > result[2])
            result[2] = b[2];
#if LUA_VECTOR_SIZE == 4
        if (b[3] > result[3])
            result[3] = b[3];
#endif
    }

#if LUA_VECTOR_SIZE == 4
    lua_pushvector(L, result[0], result[1], result[2], result[3]);
#else
    lua_pushvector(L, result[0], result[1], result[2]);
#endif

    return 1;
}

static int vector_index(lua_State* L)
{
    const float* v = luaL_checkvector(L, 1);
    size_t namelen = 0;
    const char* name = luaL_checklstring(L, 2, &namelen);

    // field access implementation mirrors the fast-path we have in the VM
    if (namelen == 1)
    {
        int ic = (name[0] | ' ') - 'x';

#if LUA_VECTOR_SIZE == 4
        // 'w' is before 'x' in ascii, so ic is -1 when indexing with 'w'
        if (ic == -1)
            ic = 3;
#endif

        if (unsigned(ic) < LUA_VECTOR_SIZE)
        {
            lua_pushnumber(L, v[ic]);
            return 1;
        }
    }

    luaL_error(L, "attempt to index vector with '%s'", name);
}

static const luaL_Reg vectorlib[] = {
    {"create", vector_create},
    {"magnitude", vector_magnitude},
    {"normalize", vector_normalize},
    {"cross", vector_cross},
    {"dot", vector_dot},
    {"angle", vector_angle},
    {"floor", vector_floor},
    {"ceil", vector_ceil},
    {"abs", vector_abs},
    {"sign", vector_sign},
    {"clamp", vector_clamp},
    {"max", vector_max},
    {"min", vector_min},
    {NULL, NULL},
};

static void createmetatable(lua_State* L)
{
    lua_createtable(L, 0, 1); // create metatable for vectors

    // push dummy vector
#if LUA_VECTOR_SIZE == 4
    lua_pushvector(L, 0.0f, 0.0f, 0.0f, 0.0f);
#else
    lua_pushvector(L, 0.0f, 0.0f, 0.0f);
#endif

    lua_pushvalue(L, -2);
    lua_setmetatable(L, -2); // set vector metatable
    lua_pop(L, 1);           // pop dummy vector

    lua_pushcfunction(L, vector_index, nullptr);
    lua_setfield(L, -2, "__index");

    lua_setreadonly(L, -1, true);
    lua_pop(L, 1); // pop the metatable
}

int luaopen_vector(lua_State* L)
{
    luaL_register(L, LUA_VECLIBNAME, vectorlib);

#if LUA_VECTOR_SIZE == 4
    lua_pushvector(L, 0.0f, 0.0f, 0.0f, 0.0f);
    lua_setfield(L, -2, "zero");
    lua_pushvector(L, 1.0f, 1.0f, 1.0f, 1.0f);
    lua_setfield(L, -2, "one");
#else
    lua_pushvector(L, 0.0f, 0.0f, 0.0f);
    lua_setfield(L, -2, "zero");
    lua_pushvector(L, 1.0f, 1.0f, 1.0f);
    lua_setfield(L, -2, "one");
#endif

    createmetatable(L);

    return 1;
}
