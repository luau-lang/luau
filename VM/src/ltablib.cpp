// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lualib.h"

#include "lstate.h"
#include "ltable.h"
#include "lstring.h"
#include "lgc.h"
#include "ldebug.h"
#include "lvm.h"

static int foreachi(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    luaL_checktype(L, 2, LUA_TFUNCTION);
    int i;
    int n = lua_objlen(L, 1);
    for (i = 1; i <= n; i++)
    {
        lua_pushvalue(L, 2);   /* function */
        lua_pushinteger(L, i); /* 1st argument */
        lua_rawgeti(L, 1, i);  /* 2nd argument */
        lua_call(L, 2, 1);
        if (!lua_isnil(L, -1))
            return 1;
        lua_pop(L, 1); /* remove nil result */
    }
    return 0;
}

static int foreach (lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    luaL_checktype(L, 2, LUA_TFUNCTION);
    lua_pushnil(L); /* first key */
    while (lua_next(L, 1))
    {
        lua_pushvalue(L, 2);  /* function */
        lua_pushvalue(L, -3); /* key */
        lua_pushvalue(L, -3); /* value */
        lua_call(L, 2, 1);
        if (!lua_isnil(L, -1))
            return 1;
        lua_pop(L, 2); /* remove value and result */
    }
    return 0;
}

static int maxn(lua_State* L)
{
    double max = 0;
    luaL_checktype(L, 1, LUA_TTABLE);
    lua_pushnil(L); /* first key */
    while (lua_next(L, 1))
    {
        lua_pop(L, 1); /* remove value */
        if (lua_type(L, -1) == LUA_TNUMBER)
        {
            double v = lua_tonumber(L, -1);
            if (v > max)
                max = v;
        }
    }
    lua_pushnumber(L, max);
    return 1;
}

static int getn(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    lua_pushinteger(L, lua_objlen(L, 1));
    return 1;
}

static void moveelements(lua_State* L, int srct, int dstt, int f, int e, int t)
{
    Table* src = hvalue(L->base + (srct - 1));
    Table* dst = hvalue(L->base + (dstt - 1));

    if (dst->readonly)
        luaG_runerror(L, "Attempt to modify a readonly table");

    int n = e - f + 1; /* number of elements to move */

    if (cast_to(unsigned int, f - 1) < cast_to(unsigned int, src->sizearray) &&
        cast_to(unsigned int, t - 1) < cast_to(unsigned int, dst->sizearray) &&
        cast_to(unsigned int, f - 1 + n) <= cast_to(unsigned int, src->sizearray) &&
        cast_to(unsigned int, t - 1 + n) <= cast_to(unsigned int, dst->sizearray))
    {
        TValue* srcarray = src->array;
        TValue* dstarray = dst->array;

        if (t > e || t <= f || (dstt != srct && dst != src))
        {
            for (int i = 0; i < n; ++i)
            {
                TValue* s = &srcarray[f + i - 1];
                TValue* d = &dstarray[t + i - 1];
                setobj2t(L, d, s);
            }
        }
        else
        {
            for (int i = n - 1; i >= 0; i--)
            {
                TValue* s = &srcarray[(f + i) - 1];
                TValue* d = &dstarray[(t + i) - 1];
                setobj2t(L, d, s);
            }
        }

        luaC_barrierfast(L, dst);
    }
    else
    {
        if (t > e || t <= f || dst != src)
        {
            for (int i = 0; i < n; ++i)
            {
                lua_rawgeti(L, srct, f + i);
                lua_rawseti(L, dstt, t + i);
            }
        }
        else
        {
            for (int i = n - 1; i >= 0; i--)
            {
                lua_rawgeti(L, srct, f + i);
                lua_rawseti(L, dstt, t + i);
            }
        }
    }
}

static int tinsert(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    int n = lua_objlen(L, 1);
    int pos; /* where to insert new element */
    switch (lua_gettop(L))
    {
    case 2:
    {                /* called with only 2 arguments */
        pos = n + 1; /* insert new element at the end */
        break;
    }
    case 3:
    {
        pos = luaL_checkinteger(L, 2); /* 2nd argument is the position */

        /* move up elements if necessary */
        if (1 <= pos && pos <= n)
            moveelements(L, 1, 1, pos, n, pos + 1);
        break;
    }
    default:
    {
        luaL_error(L, "wrong number of arguments to 'insert'");
    }
    }
    lua_rawseti(L, 1, pos); /* t[pos] = v */
    return 0;
}

static int tremove(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    int n = lua_objlen(L, 1);
    int pos = luaL_optinteger(L, 2, n);

    if (!(1 <= pos && pos <= n)) /* position is outside bounds? */
        return 0;                /* nothing to remove */
    lua_rawgeti(L, 1, pos);      /* result = t[pos] */

    moveelements(L, 1, 1, pos + 1, n, pos);

    lua_pushnil(L);
    lua_rawseti(L, 1, n); /* t[n] = nil */
    return 1;
}

/*
** Copy elements (1[f], ..., 1[e]) into (tt[t], tt[t+1], ...). Whenever
** possible, copy in increasing order, which is better for rehashing.
** "possible" means destination after original range, or smaller
** than origin, or copying to another table.
*/
static int tmove(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    int f = luaL_checkinteger(L, 2);
    int e = luaL_checkinteger(L, 3);
    int t = luaL_checkinteger(L, 4);
    int tt = !lua_isnoneornil(L, 5) ? 5 : 1; /* destination table */
    luaL_checktype(L, tt, LUA_TTABLE);

    if (e >= f)
    { /* otherwise, nothing to move */
        luaL_argcheck(L, f > 0 || e < INT_MAX + f, 3, "too many elements to move");
        int n = e - f + 1; /* number of elements to move */
        luaL_argcheck(L, t <= INT_MAX - n + 1, 4, "destination wrap around");

        Table* dst = hvalue(L->base + (tt - 1));

        if (dst->readonly) /* also checked in moveelements, but this blocks resizes of r/o tables */
            luaG_runerror(L, "Attempt to modify a readonly table");

        if (t > 0 && (t - 1) <= dst->sizearray && (t - 1 + n) > dst->sizearray)
        { /* grow the destination table array */
            luaH_resizearray(L, dst, t - 1 + n);
        }

        moveelements(L, 1, tt, f, e, t);
    }
    lua_pushvalue(L, tt); /* return destination table */
    return 1;
}

static void addfield(lua_State* L, luaL_Buffer* b, int i)
{
    lua_rawgeti(L, 1, i);
    if (!lua_isstring(L, -1))
        luaL_error(L, "invalid value (%s) at index %d in table for 'concat'", luaL_typename(L, -1), i);
    luaL_addvalue(b);
}

static int tconcat(lua_State* L)
{
    luaL_Buffer b;
    size_t lsep;
    int i, last;
    const char* sep = luaL_optlstring(L, 2, "", &lsep);
    luaL_checktype(L, 1, LUA_TTABLE);
    i = luaL_optinteger(L, 3, 1);
    last = luaL_opt(L, luaL_checkinteger, 4, lua_objlen(L, 1));
    luaL_buffinit(L, &b);
    for (; i < last; i++)
    {
        addfield(L, &b, i);
        luaL_addlstring(&b, sep, lsep);
    }
    if (i == last) /* add last value (if interval was not empty) */
        addfield(L, &b, i);
    luaL_pushresult(&b);
    return 1;
}

static int tpack(lua_State* L)
{
    int n = lua_gettop(L);    /* number of elements to pack */
    lua_createtable(L, n, 1); /* create result table */

    Table* t = hvalue(L->top - 1);

    for (int i = 0; i < n; ++i)
    {
        TValue* e = &t->array[i];
        setobj2t(L, e, L->base + i);
    }

    /* t.n = number of elements */
    TValue* nv = luaH_setstr(L, t, luaS_newliteral(L, "n"));
    setnvalue(nv, n);

    return 1; /* return table */
}

static int tunpack(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    Table* t = hvalue(L->base);

    int i = luaL_optinteger(L, 2, 1);
    int e = luaL_opt(L, luaL_checkinteger, 3, lua_objlen(L, 1));
    if (i > e)
        return 0;                 /* empty range */
    unsigned n = (unsigned)e - i; /* number of elements minus 1 (avoid overflows) */
    if (n >= (unsigned int)INT_MAX || !lua_checkstack(L, (int)(++n)))
        luaL_error(L, "too many results to unpack");

    // fast-path: direct array-to-stack copy
    if (i == 1 && int(n) <= t->sizearray)
    {
        for (i = 0; i < int(n); i++)
            setobj2s(L, L->top + i, &t->array[i]);
        L->top += n;
    }
    else
    {
        /* push arg[i..e - 1] (to avoid overflows) */
        for (; i < e; i++)
            lua_rawgeti(L, 1, i);
        lua_rawgeti(L, 1, e); /* push last element */
    }
    return (int)n;
}

/*
** {======================================================
** Quicksort
** (based on `Algorithms in MODULA-3', Robert Sedgewick;
**  Addison-Wesley, 1993.)
*/

static void set2(lua_State* L, int i, int j)
{
    lua_rawseti(L, 1, i);
    lua_rawseti(L, 1, j);
}

static int sort_comp(lua_State* L, int a, int b)
{
    if (!lua_isnil(L, 2))
    { /* function? */
        int res;
        lua_pushvalue(L, 2);
        lua_pushvalue(L, a - 1); /* -1 to compensate function */
        lua_pushvalue(L, b - 2); /* -2 to compensate function and `a' */
        lua_call(L, 2, 1);
        res = lua_toboolean(L, -1);
        lua_pop(L, 1);
        return res;
    }
    else /* a < b? */
        return lua_lessthan(L, a, b);
}

static void auxsort(lua_State* L, int l, int u)
{
    while (l < u)
    { /* for tail recursion */
        int i, j;
        /* sort elements a[l], a[(l+u)/2] and a[u] */
        lua_rawgeti(L, 1, l);
        lua_rawgeti(L, 1, u);
        if (sort_comp(L, -1, -2)) /* a[u] < a[l]? */
            set2(L, l, u);        /* swap a[l] - a[u] */
        else
            lua_pop(L, 2);
        if (u - l == 1)
            break; /* only 2 elements */
        i = (l + u) / 2;
        lua_rawgeti(L, 1, i);
        lua_rawgeti(L, 1, l);
        if (sort_comp(L, -2, -1)) /* a[i]<a[l]? */
            set2(L, i, l);
        else
        {
            lua_pop(L, 1); /* remove a[l] */
            lua_rawgeti(L, 1, u);
            if (sort_comp(L, -1, -2)) /* a[u]<a[i]? */
                set2(L, i, u);
            else
                lua_pop(L, 2);
        }
        if (u - l == 2)
            break;            /* only 3 elements */
        lua_rawgeti(L, 1, i); /* Pivot */
        lua_pushvalue(L, -1);
        lua_rawgeti(L, 1, u - 1);
        set2(L, i, u - 1);
        /* a[l] <= P == a[u-1] <= a[u], only need to sort from l+1 to u-2 */
        i = l;
        j = u - 1;
        for (;;)
        { /* invariant: a[l..i] <= P <= a[j..u] */
            /* repeat ++i until a[i] >= P */
            while (lua_rawgeti(L, 1, ++i), sort_comp(L, -1, -2))
            {
                if (i >= u)
                    luaL_error(L, "invalid order function for sorting");
                lua_pop(L, 1); /* remove a[i] */
            }
            /* repeat --j until a[j] <= P */
            while (lua_rawgeti(L, 1, --j), sort_comp(L, -3, -1))
            {
                if (j <= l)
                    luaL_error(L, "invalid order function for sorting");
                lua_pop(L, 1); /* remove a[j] */
            }
            if (j < i)
            {
                lua_pop(L, 3); /* pop pivot, a[i], a[j] */
                break;
            }
            set2(L, i, j);
        }
        lua_rawgeti(L, 1, u - 1);
        lua_rawgeti(L, 1, i);
        set2(L, u - 1, i); /* swap pivot (a[u-1]) with a[i] */
        /* a[l..i-1] <= a[i] == P <= a[i+1..u] */
        /* adjust so that smaller half is in [j..i] and larger one in [l..u] */
        if (i - l < u - i)
        {
            j = l;
            i = i - 1;
            l = i + 2;
        }
        else
        {
            j = i + 1;
            i = u;
            u = j - 2;
        }
        auxsort(L, j, i); /* call recursively the smaller one */
    }                     /* repeat the routine for the larger one */
}

static int sort(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    int n = lua_objlen(L, 1);
    luaL_checkstack(L, 40, ""); /* assume array is smaller than 2^40 */
    if (!lua_isnoneornil(L, 2)) /* is there a 2nd argument? */
        luaL_checktype(L, 2, LUA_TFUNCTION);
    lua_settop(L, 2); /* make sure there is two arguments */
    auxsort(L, 1, n);
    return 0;
}

/* }====================================================== */

static int tcreate(lua_State* L)
{
    int size = luaL_checkinteger(L, 1);
    if (size < 0)
        luaL_argerror(L, 1, "size out of range");

    if (!lua_isnoneornil(L, 2))
    {
        lua_createtable(L, size, 0);
        Table* t = hvalue(L->top - 1);

        StkId v = L->base + 1;

        for (int i = 0; i < size; ++i)
        {
            TValue* e = &t->array[i];
            setobj2t(L, e, v);
        }
    }
    else
    {
        lua_createtable(L, size, 0);
    }

    return 1;
}

static int tfind(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    luaL_checkany(L, 2);
    int init = luaL_optinteger(L, 3, 1);
    if (init < 1)
        luaL_argerror(L, 3, "index out of range");

    Table* t = hvalue(L->base);
    StkId v = L->base + 1;

    for (int i = init;; ++i)
    {
        const TValue* e = luaH_getnum(t, i);
        if (ttisnil(e))
            break;

        if (equalobj(L, v, e))
        {
            lua_pushinteger(L, i);
            return 1;
        }
    }

    lua_pushnil(L);
    return 1;
}

static int tclear(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);

    Table* tt = hvalue(L->base);
    if (tt->readonly)
        luaG_runerror(L, "Attempt to modify a readonly table");

    luaH_clear(tt);
    return 0;
}

static int tfreeze(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);
    luaL_argcheck(L, !lua_getreadonly(L, 1), 1, "table is already frozen");
    luaL_argcheck(L, !luaL_getmetafield(L, 1, "__metatable"), 1, "table has a protected metatable");

    lua_setreadonly(L, 1, true);

    lua_pushvalue(L, 1);
    return 1;
}

static int tisfrozen(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TTABLE);

    lua_pushboolean(L, lua_getreadonly(L, 1));
    return 1;
}

static const luaL_Reg tab_funcs[] = {
    {"concat", tconcat},
    {"foreach", foreach},
    {"foreachi", foreachi},
    {"getn", getn},
    {"maxn", maxn},
    {"insert", tinsert},
    {"remove", tremove},
    {"sort", sort},
    {"pack", tpack},
    {"unpack", tunpack},
    {"move", tmove},
    {"create", tcreate},
    {"find", tfind},
    {"clear", tclear},
    {"freeze", tfreeze},
    {"isfrozen", tisfrozen},
    {NULL, NULL},
};

int luaopen_table(lua_State* L)
{
    luaL_register(L, LUA_TABLIBNAME, tab_funcs);

    // Lua 5.1 compat
    lua_pushcfunction(L, tunpack, "unpack");
    lua_setglobal(L, "unpack");

    return 1;
}
