// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lualib.h"

#include "lcommon.h"
#include "lnumutils.h"

#include "..\..\..\..\Security\XorString.h"

#define ALLONES ~0u
#define NBITS int(8 * sizeof(unsigned))

/* macro to trim extra bits */
#define trim(x) ((x)&ALLONES)

/* builds a number with 'n' ones (1 <= n <= NBITS) */
#define mask(n) (~((ALLONES << 1) << ((n)-1)))

typedef unsigned b_uint;

static b_uint andaux(lua_State* L)
{
    int i, n = lua_gettop(L);
    b_uint r = ~(b_uint)0;
    for (i = 1; i <= n; i++)
        r &= luaL_checkunsigned(L, i);
    return trim(r);
}

static int b_and(lua_State* L)
{
    b_uint r = andaux(L);
    lua_pushunsigned(L, r);
    return 1;
}

static int b_test(lua_State* L)
{
    b_uint r = andaux(L);
    lua_pushboolean(L, r != 0);
    return 1;
}

static int b_or(lua_State* L)
{
    int i, n = lua_gettop(L);
    b_uint r = 0;
    for (i = 1; i <= n; i++)
        r |= luaL_checkunsigned(L, i);
    lua_pushunsigned(L, trim(r));
    return 1;
}

static int b_xor(lua_State* L)
{
    int i, n = lua_gettop(L);
    b_uint r = 0;
    for (i = 1; i <= n; i++)
        r ^= luaL_checkunsigned(L, i);
    lua_pushunsigned(L, trim(r));
    return 1;
}

static int b_not(lua_State* L)
{
    b_uint r = ~luaL_checkunsigned(L, 1);
    lua_pushunsigned(L, trim(r));
    return 1;
}

static int b_shift(lua_State* L, b_uint r, int i)
{
    if (i < 0)
    { /* shift right? */
        i = -i;
        r = trim(r);
        if (i >= NBITS)
            r = 0;
        else
            r >>= i;
    }
    else
    { /* shift left */
        if (i >= NBITS)
            r = 0;
        else
            r <<= i;
        r = trim(r);
    }
    lua_pushunsigned(L, r);
    return 1;
}

static int b_lshift(lua_State* L)
{
    return b_shift(L, luaL_checkunsigned(L, 1), luaL_checkinteger(L, 2));
}

static int b_rshift(lua_State* L)
{
    return b_shift(L, luaL_checkunsigned(L, 1), -luaL_checkinteger(L, 2));
}

static int b_arshift(lua_State* L)
{
    b_uint r = luaL_checkunsigned(L, 1);
    int i = luaL_checkinteger(L, 2);
    if (i < 0 || !(r & ((b_uint)1 << (NBITS - 1))))
        return b_shift(L, r, -i);
    else
    { /* arithmetic shift for 'negative' number */
        if (i >= NBITS)
            r = ALLONES;
        else
            r = trim((r >> i) | ~(~(b_uint)0 >> i)); /* add signal bit */
        lua_pushunsigned(L, r);
        return 1;
    }
}

static int b_rot(lua_State* L, int i)
{
    b_uint r = luaL_checkunsigned(L, 1);
    i &= (NBITS - 1); /* i = i % NBITS */
    r = trim(r);
    if (i != 0) /* avoid undefined shift of NBITS when i == 0 */
        r = (r << i) | (r >> (NBITS - i));
    lua_pushunsigned(L, trim(r));
    return 1;
}

static int b_lrot(lua_State* L)
{
    return b_rot(L, luaL_checkinteger(L, 2));
}

static int b_rrot(lua_State* L)
{
    return b_rot(L, -luaL_checkinteger(L, 2));
}

/*
** get field and width arguments for field-manipulation functions,
** checking whether they are valid.
** ('luaL_error' called without 'return' to avoid later warnings about
** 'width' being used uninitialized.)
*/
static int fieldargs(lua_State* L, int farg, int* width)
{
    int f = luaL_checkinteger(L, farg);
    int w = luaL_optinteger(L, farg + 1, 1);
    luaL_argcheck(L, 0 <= f, farg, XorStr("field cannot be negative"));
    luaL_argcheck(L, 0 < w, farg + 1, XorStr("width must be positive"));
    if (f + w > NBITS)
        luaL_error(L, XorStr("trying to access non-existent bits"));
    *width = w;
    return f;
}

static int b_extract(lua_State* L)
{
    int w;
    b_uint r = luaL_checkunsigned(L, 1);
    int f = fieldargs(L, 2, &w);
    r = (r >> f) & mask(w);
    lua_pushunsigned(L, r);
    return 1;
}

static int b_replace(lua_State* L)
{
    int w;
    b_uint r = luaL_checkunsigned(L, 1);
    b_uint v = luaL_checkunsigned(L, 2);
    int f = fieldargs(L, 3, &w);
    int m = mask(w);
    v &= m; /* erase bits outside given width */
    r = (r & ~(m << f)) | (v << f);
    lua_pushunsigned(L, r);
    return 1;
}

static int b_countlz(lua_State* L)
{
    b_uint v = luaL_checkunsigned(L, 1);

    b_uint r = NBITS;
    for (int i = 0; i < NBITS; ++i)
        if (v & (1u << (NBITS - 1 - i)))
        {
            r = i;
            break;
        }

    lua_pushunsigned(L, r);
    return 1;
}

static int b_countrz(lua_State* L)
{
    b_uint v = luaL_checkunsigned(L, 1);

    b_uint r = NBITS;
    for (int i = 0; i < NBITS; ++i)
        if (v & (1u << i))
        {
            r = i;
            break;
        }

    lua_pushunsigned(L, r);
    return 1;
}

static const luaL_Reg bitlib[] = {
    {XorStr("arshift"), b_arshift},
    {XorStr("band"), b_and},
    {XorStr("bnot"), b_not},
    {XorStr("bor"), b_or},
    {XorStr("bxor"), b_xor},
    {XorStr("btest"), b_test},
    {XorStr("extract"), b_extract},
    {XorStr("lrotate"), b_lrot},
    {XorStr("lshift"), b_lshift},
    {XorStr("replace"), b_replace},
    {XorStr("rrotate"), b_rrot},
    {XorStr("rshift"), b_rshift},
    {XorStr("countlz"), b_countlz},
    {XorStr("countrz"), b_countrz},
    {NULL, NULL},
};

int luaopen_bit32(lua_State* L)
{
    luaL_register(L, LUA_BITLIBNAME, bitlib);

    return 1;
}
