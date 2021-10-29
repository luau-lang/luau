// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lapi.h"

#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "lfunc.h"
#include "lgc.h"
#include "ldo.h"
#include "lvm.h"
#include "lnumutils.h"

#include <string.h>

LUAU_FASTFLAG(LuauGcFullSkipInactiveThreads)

const char* lua_ident = "$Lua: Lua 5.1.4 Copyright (C) 1994-2008 Lua.org, PUC-Rio $\n"
                        "$Authors: R. Ierusalimschy, L. H. de Figueiredo & W. Celes $\n"
                        "$URL: www.lua.org $\n";

const char* luau_ident = "$Luau: Copyright (C) 2019-2021 Roblox Corporation $\n"
                         "$URL: luau-lang.org $\n";

#define api_checknelems(L, n) api_check(L, (n) <= (L->top - L->base))

#define api_checkvalidindex(L, i) api_check(L, (i) != luaO_nilobject)

#define api_incr_top(L) \
    { \
        api_check(L, L->top < L->ci->top); \
        L->top++; \
    }

static Table* getcurrenv(lua_State* L)
{
    if (L->ci == L->base_ci)  /* no enclosing function? */
        return hvalue(gt(L)); /* use global table as environment */
    else
    {
        Closure* func = curr_func(L);
        return func->env;
    }
}

static LUAU_NOINLINE TValue* index2adrslow(lua_State* L, int idx)
{
    api_check(L, idx <= 0);
    if (idx > LUA_REGISTRYINDEX)
    {
        api_check(L, idx != 0 && -idx <= L->top - L->base);
        return L->top + idx;
    }
    else
        switch (idx)
        { /* pseudo-indices */
        case LUA_REGISTRYINDEX:
            return registry(L);
        case LUA_ENVIRONINDEX:
        {
            sethvalue(L, &L->env, getcurrenv(L));
            return &L->env;
        }
        case LUA_GLOBALSINDEX:
            return gt(L);
        default:
        {
            Closure* func = curr_func(L);
            idx = LUA_GLOBALSINDEX - idx;
            return (idx <= func->nupvalues) ? &func->c.upvals[idx - 1] : cast_to(TValue*, luaO_nilobject);
        }
        }
}

static LUAU_FORCEINLINE TValue* index2adr(lua_State* L, int idx)
{
    if (idx > 0)
    {
        TValue* o = L->base + (idx - 1);
        api_check(L, idx <= L->ci->top - L->base);
        if (o >= L->top)
            return cast_to(TValue*, luaO_nilobject);
        else
            return o;
    }
    else
    {
        return index2adrslow(L, idx);
    }
}

const TValue* luaA_toobject(lua_State* L, int idx)
{
    StkId p = index2adr(L, idx);
    return (p == luaO_nilobject) ? NULL : p;
}

void luaA_pushobject(lua_State* L, const TValue* o)
{
    setobj2s(L, L->top, o);
    api_incr_top(L);
}

int lua_checkstack(lua_State* L, int size)
{
    int res = 1;
    if (size > LUAI_MAXCSTACK || (L->top - L->base + size) > LUAI_MAXCSTACK)
        res = 0; /* stack overflow */
    else if (size > 0)
    {
        luaD_checkstack(L, size);
        expandstacklimit(L, L->top + size);
    }
    return res;
}

void lua_rawcheckstack(lua_State* L, int size)
{
    luaD_checkstack(L, size);
    expandstacklimit(L, L->top + size);
    return;
}

void lua_xmove(lua_State* from, lua_State* to, int n)
{
    if (from == to)
        return;
    api_checknelems(from, n);
    api_check(from, from->global == to->global);
    api_check(from, to->ci->top - to->top >= n);
    luaC_checkthreadsleep(to);

    StkId ttop = to->top;
    StkId ftop = from->top - n;
    for (int i = 0; i < n; i++)
        setobj2s(to, ttop + i, ftop + i);

    from->top = ftop;
    to->top = ttop + n;

    return;
}

void lua_xpush(lua_State* from, lua_State* to, int idx)
{
    api_check(from, from->global == to->global);
    luaC_checkthreadsleep(to);
    setobj2s(to, to->top, index2adr(from, idx));
    api_incr_top(to);
    return;
}

lua_State* lua_newthread(lua_State* L)
{
    luaC_checkGC(L);
    luaC_checkthreadsleep(L);
    lua_State* L1 = luaE_newthread(L);
    setthvalue(L, L->top, L1);
    api_incr_top(L);
    global_State* g = L->global;
    if (g->cb.userthread)
        g->cb.userthread(L, L1);
    return L1;
}

lua_State* lua_mainthread(lua_State* L)
{
    return L->global->mainthread;
}

/*
** basic stack manipulation
*/

int lua_gettop(lua_State* L)
{
    return cast_int(L->top - L->base);
}

void lua_settop(lua_State* L, int idx)
{
    if (idx >= 0)
    {
        api_check(L, idx <= L->stack_last - L->base);
        while (L->top < L->base + idx)
            setnilvalue(L->top++);
        L->top = L->base + idx;
    }
    else
    {
        api_check(L, -(idx + 1) <= (L->top - L->base));
        L->top += idx + 1; /* `subtract' index (index is negative) */
    }
    return;
}

void lua_remove(lua_State* L, int idx)
{
    StkId p = index2adr(L, idx);
    api_checkvalidindex(L, p);
    while (++p < L->top)
        setobjs2s(L, p - 1, p);
    L->top--;
    return;
}

void lua_insert(lua_State* L, int idx)
{
    luaC_checkthreadsleep(L);
    StkId p = index2adr(L, idx);
    api_checkvalidindex(L, p);
    for (StkId q = L->top; q > p; q--)
        setobjs2s(L, q, q - 1);
    setobjs2s(L, p, L->top);
    return;
}

void lua_replace(lua_State* L, int idx)
{
    /* explicit test for incompatible code */
    if (idx == LUA_ENVIRONINDEX && L->ci == L->base_ci)
        luaG_runerror(L, "no calling environment");
    api_checknelems(L, 1);
    luaC_checkthreadsleep(L);
    StkId o = index2adr(L, idx);
    api_checkvalidindex(L, o);
    if (idx == LUA_ENVIRONINDEX)
    {
        Closure* func = curr_func(L);
        api_check(L, ttistable(L->top - 1));
        func->env = hvalue(L->top - 1);
        luaC_barrier(L, func, L->top - 1);
    }
    else
    {
        setobj(L, o, L->top - 1);
        if (idx < LUA_GLOBALSINDEX) /* function upvalue? */
            luaC_barrier(L, curr_func(L), L->top - 1);
    }
    L->top--;
    return;
}

void lua_pushvalue(lua_State* L, int idx)
{
    luaC_checkthreadsleep(L);
    StkId o = index2adr(L, idx);
    setobj2s(L, L->top, o);
    api_incr_top(L);
    return;
}

/*
** access functions (stack -> C)
*/

int lua_type(lua_State* L, int idx)
{
    StkId o = index2adr(L, idx);
    return (o == luaO_nilobject) ? LUA_TNONE : ttype(o);
}

const char* lua_typename(lua_State* L, int t)
{
    return (t == LUA_TNONE) ? "no value" : luaT_typenames[t];
}

int lua_iscfunction(lua_State* L, int idx)
{
    StkId o = index2adr(L, idx);
    return iscfunction(o);
}

int lua_isLfunction(lua_State* L, int idx)
{
    StkId o = index2adr(L, idx);
    return isLfunction(o);
}

int lua_isnumber(lua_State* L, int idx)
{
    TValue n;
    const TValue* o = index2adr(L, idx);
    return tonumber(o, &n);
}

int lua_isstring(lua_State* L, int idx)
{
    int t = lua_type(L, idx);
    return (t == LUA_TSTRING || t == LUA_TNUMBER);
}

int lua_isuserdata(lua_State* L, int idx)
{
    const TValue* o = index2adr(L, idx);
    return (ttisuserdata(o) || ttislightuserdata(o));
}

int lua_rawequal(lua_State* L, int index1, int index2)
{
    StkId o1 = index2adr(L, index1);
    StkId o2 = index2adr(L, index2);
    return (o1 == luaO_nilobject || o2 == luaO_nilobject) ? 0 : luaO_rawequalObj(o1, o2);
}

int lua_equal(lua_State* L, int index1, int index2)
{
    StkId o1, o2;
    int i;
    o1 = index2adr(L, index1);
    o2 = index2adr(L, index2);
    i = (o1 == luaO_nilobject || o2 == luaO_nilobject) ? 0 : equalobj(L, o1, o2);
    return i;
}

int lua_lessthan(lua_State* L, int index1, int index2)
{
    StkId o1, o2;
    int i;
    o1 = index2adr(L, index1);
    o2 = index2adr(L, index2);
    i = (o1 == luaO_nilobject || o2 == luaO_nilobject) ? 0 : luaV_lessthan(L, o1, o2);
    return i;
}

double lua_tonumberx(lua_State* L, int idx, int* isnum)
{
    TValue n;
    const TValue* o = index2adr(L, idx);
    if (tonumber(o, &n))
    {
        if (isnum)
            *isnum = 1;
        return nvalue(o);
    }
    else
    {
        if (isnum)
            *isnum = 0;
        return 0;
    }
}

int lua_tointegerx(lua_State* L, int idx, int* isnum)
{
    TValue n;
    const TValue* o = index2adr(L, idx);
    if (tonumber(o, &n))
    {
        int res;
        double num = nvalue(o);
        luai_num2int(res, num);
        if (isnum)
            *isnum = 1;
        return res;
    }
    else
    {
        if (isnum)
            *isnum = 0;
        return 0;
    }
}

unsigned lua_tounsignedx(lua_State* L, int idx, int* isnum)
{
    TValue n;
    const TValue* o = index2adr(L, idx);
    if (tonumber(o, &n))
    {
        unsigned res;
        double num = nvalue(o);
        luai_num2unsigned(res, num);
        if (isnum)
            *isnum = 1;
        return res;
    }
    else
    {
        if (isnum)
            *isnum = 0;
        return 0;
    }
}

int lua_toboolean(lua_State* L, int idx)
{
    const TValue* o = index2adr(L, idx);
    return !l_isfalse(o);
}

const char* lua_tolstring(lua_State* L, int idx, size_t* len)
{
    StkId o = index2adr(L, idx);
    if (!ttisstring(o))
    {
        luaC_checkthreadsleep(L);
        if (!luaV_tostring(L, o))
        { /* conversion failed? */
            if (len != NULL)
                *len = 0;
            return NULL;
        }
        luaC_checkGC(L);
        o = index2adr(L, idx); /* previous call may reallocate the stack */
    }
    if (len != NULL)
        *len = tsvalue(o)->len;
    return svalue(o);
}

const char* lua_tostringatom(lua_State* L, int idx, int* atom)
{
    StkId o = index2adr(L, idx);
    if (!ttisstring(o))
        return NULL;
    const TString* s = tsvalue(o);
    if (atom)
        *atom = s->atom;
    return getstr(s);
}

const char* lua_namecallatom(lua_State* L, int* atom)
{
    const TString* s = L->namecall;
    if (!s)
        return NULL;
    if (atom)
        *atom = s->atom;
    return getstr(s);
}

const float* lua_tovector(lua_State* L, int idx)
{
    StkId o = index2adr(L, idx);
    if (!ttisvector(o))
    {
        return NULL;
    }
    return vvalue(o);
}

int lua_objlen(lua_State* L, int idx)
{
    StkId o = index2adr(L, idx);
    switch (ttype(o))
    {
    case LUA_TSTRING:
        return tsvalue(o)->len;
    case LUA_TUSERDATA:
        return uvalue(o)->len;
    case LUA_TTABLE:
        return luaH_getn(hvalue(o));
    case LUA_TNUMBER:
    {
        int l = (luaV_tostring(L, o) ? tsvalue(o)->len : 0);
        return l;
    }
    default:
        return 0;
    }
}

lua_CFunction lua_tocfunction(lua_State* L, int idx)
{
    StkId o = index2adr(L, idx);
    return (!iscfunction(o)) ? NULL : cast_to(lua_CFunction, clvalue(o)->c.f);
}

void* lua_touserdata(lua_State* L, int idx)
{
    StkId o = index2adr(L, idx);
    switch (ttype(o))
    {
    case LUA_TUSERDATA:
        return uvalue(o)->data;
    case LUA_TLIGHTUSERDATA:
        return pvalue(o);
    default:
        return NULL;
    }
}

void* lua_touserdatatagged(lua_State* L, int idx, int tag)
{
    StkId o = index2adr(L, idx);
    return (ttisuserdata(o) && uvalue(o)->tag == tag) ? uvalue(o)->data : NULL;
}

int lua_userdatatag(lua_State* L, int idx)
{
    StkId o = index2adr(L, idx);
    if (ttisuserdata(o))
        return uvalue(o)->tag;
    return -1;
}

lua_State* lua_tothread(lua_State* L, int idx)
{
    StkId o = index2adr(L, idx);
    return (!ttisthread(o)) ? NULL : thvalue(o);
}

const void* lua_topointer(lua_State* L, int idx)
{
    StkId o = index2adr(L, idx);
    switch (ttype(o))
    {
    case LUA_TTABLE:
        return hvalue(o);
    case LUA_TFUNCTION:
        return clvalue(o);
    case LUA_TTHREAD:
        return thvalue(o);
    case LUA_TUSERDATA:
    case LUA_TLIGHTUSERDATA:
        return lua_touserdata(L, idx);
    default:
        return NULL;
    }
}

/*
** push functions (C -> stack)
*/

void lua_pushnil(lua_State* L)
{
    setnilvalue(L->top);
    api_incr_top(L);
    return;
}

void lua_pushnumber(lua_State* L, double n)
{
    setnvalue(L->top, n);
    api_incr_top(L);
    return;
}

void lua_pushinteger(lua_State* L, int n)
{
    setnvalue(L->top, cast_num(n));
    api_incr_top(L);
    return;
}

void lua_pushunsigned(lua_State* L, unsigned u)
{
    setnvalue(L->top, cast_num(u));
    api_incr_top(L);
    return;
}

void lua_pushvector(lua_State* L, float x, float y, float z)
{
    setvvalue(L->top, x, y, z);
    api_incr_top(L);
    return;
}

void lua_pushlstring(lua_State* L, const char* s, size_t len)
{
    luaC_checkGC(L);
    luaC_checkthreadsleep(L);
    setsvalue2s(L, L->top, luaS_newlstr(L, s, len));
    api_incr_top(L);
    return;
}

void lua_pushstring(lua_State* L, const char* s)
{
    if (s == NULL)
        lua_pushnil(L);
    else
        lua_pushlstring(L, s, strlen(s));
}

const char* lua_pushvfstring(lua_State* L, const char* fmt, va_list argp)
{
    luaC_checkGC(L);
    luaC_checkthreadsleep(L);
    const char* ret = luaO_pushvfstring(L, fmt, argp);
    return ret;
}

const char* lua_pushfstringL(lua_State* L, const char* fmt, ...)
{
    luaC_checkGC(L);
    luaC_checkthreadsleep(L);
    va_list argp;
    va_start(argp, fmt);
    const char* ret = luaO_pushvfstring(L, fmt, argp);
    va_end(argp);
    return ret;
}

void lua_pushcfunction(lua_State* L, lua_CFunction fn, const char* debugname, int nup, lua_Continuation cont)
{
    luaC_checkGC(L);
    luaC_checkthreadsleep(L);
    api_checknelems(L, nup);
    Closure* cl = luaF_newCclosure(L, nup, getcurrenv(L));
    cl->c.f = fn;
    cl->c.cont = cont;
    cl->c.debugname = debugname;
    L->top -= nup;
    while (nup--)
        setobj2n(L, &cl->c.upvals[nup], L->top + nup);
    setclvalue(L, L->top, cl);
    LUAU_ASSERT(iswhite(obj2gco(cl)));
    api_incr_top(L);
    return;
}

void lua_pushboolean(lua_State* L, int b)
{
    setbvalue(L->top, (b != 0)); /* ensure that true is 1 */
    api_incr_top(L);
    return;
}

void lua_pushlightuserdata(lua_State* L, void* p)
{
    setpvalue(L->top, p);
    api_incr_top(L);
    return;
}

int lua_pushthread(lua_State* L)
{
    luaC_checkthreadsleep(L);
    setthvalue(L, L->top, L);
    api_incr_top(L);
    return L->global->mainthread == L;
}

/*
** get functions (Lua -> stack)
*/

void lua_gettable(lua_State* L, int idx)
{
    luaC_checkthreadsleep(L);
    StkId t = index2adr(L, idx);
    api_checkvalidindex(L, t);
    luaV_gettable(L, t, L->top - 1, L->top - 1);
    return;
}

void lua_getfield(lua_State* L, int idx, const char* k)
{
    luaC_checkthreadsleep(L);
    StkId t = index2adr(L, idx);
    api_checkvalidindex(L, t);
    TValue key;
    setsvalue(L, &key, luaS_new(L, k));
    luaV_gettable(L, t, &key, L->top);
    api_incr_top(L);
    return;
}

void lua_rawgetfield(lua_State* L, int idx, const char* k)
{
    luaC_checkthreadsleep(L);
    StkId t = index2adr(L, idx);
    api_check(L, ttistable(t));
    TValue key;
    setsvalue(L, &key, luaS_new(L, k));
    setobj2s(L, L->top, luaH_getstr(hvalue(t), tsvalue(&key)));
    api_incr_top(L);
    return;
}

void lua_rawget(lua_State* L, int idx)
{
    luaC_checkthreadsleep(L);
    StkId t = index2adr(L, idx);
    api_check(L, ttistable(t));
    setobj2s(L, L->top - 1, luaH_get(hvalue(t), L->top - 1));
    return;
}

void lua_rawgeti(lua_State* L, int idx, int n)
{
    luaC_checkthreadsleep(L);
    StkId t = index2adr(L, idx);
    api_check(L, ttistable(t));
    setobj2s(L, L->top, luaH_getnum(hvalue(t), n));
    api_incr_top(L);
    return;
}

void lua_createtable(lua_State* L, int narray, int nrec)
{
    luaC_checkGC(L);
    luaC_checkthreadsleep(L);
    sethvalue(L, L->top, luaH_new(L, narray, nrec));
    api_incr_top(L);
    return;
}

void lua_setreadonly(lua_State* L, int objindex, bool value)
{
    const TValue* o = index2adr(L, objindex);
    api_check(L, ttistable(o));
    Table* t = hvalue(o);
    t->readonly = value;
    return;
}

int lua_getreadonly(lua_State* L, int objindex)
{
    const TValue* o = index2adr(L, objindex);
    api_check(L, ttistable(o));
    Table* t = hvalue(o);
    int res = t->readonly;
    return res;
}

void lua_setsafeenv(lua_State* L, int objindex, bool value)
{
    const TValue* o = index2adr(L, objindex);
    api_check(L, ttistable(o));
    Table* t = hvalue(o);
    t->safeenv = value;
    return;
}

int lua_getmetatable(lua_State* L, int objindex)
{
    const TValue* obj;
    Table* mt = NULL;
    int res;
    obj = index2adr(L, objindex);
    switch (ttype(obj))
    {
    case LUA_TTABLE:
        mt = hvalue(obj)->metatable;
        break;
    case LUA_TUSERDATA:
        mt = uvalue(obj)->metatable;
        break;
    default:
        mt = L->global->mt[ttype(obj)];
        break;
    }
    if (mt == NULL)
        res = 0;
    else
    {
        sethvalue(L, L->top, mt);
        api_incr_top(L);
        res = 1;
    }
    return res;
}

void lua_getfenv(lua_State* L, int idx)
{
    StkId o;
    o = index2adr(L, idx);
    api_checkvalidindex(L, o);
    switch (ttype(o))
    {
    case LUA_TFUNCTION:
        sethvalue(L, L->top, clvalue(o)->env);
        break;
    case LUA_TTHREAD:
        setobj2s(L, L->top, gt(thvalue(o)));
        break;
    default:
        setnilvalue(L->top);
        break;
    }
    api_incr_top(L);
    return;
}

/*
** set functions (stack -> Lua)
*/

void lua_settable(lua_State* L, int idx)
{
    StkId t;
    api_checknelems(L, 2);
    t = index2adr(L, idx);
    api_checkvalidindex(L, t);
    luaV_settable(L, t, L->top - 2, L->top - 1);
    L->top -= 2; /* pop index and value */
    return;
}

void lua_setfield(lua_State* L, int idx, const char* k)
{
    StkId t;
    TValue key;
    api_checknelems(L, 1);
    t = index2adr(L, idx);
    api_checkvalidindex(L, t);
    setsvalue(L, &key, luaS_new(L, k));
    luaV_settable(L, t, &key, L->top - 1);
    L->top--; /* pop value */
    return;
}

void lua_rawset(lua_State* L, int idx)
{
    StkId t;
    api_checknelems(L, 2);
    t = index2adr(L, idx);
    api_check(L, ttistable(t));
    if (hvalue(t)->readonly)
        luaG_runerror(L, "Attempt to modify a readonly table");
    setobj2t(L, luaH_set(L, hvalue(t), L->top - 2), L->top - 1);
    luaC_barriert(L, hvalue(t), L->top - 1);
    L->top -= 2;
    return;
}

void lua_rawseti(lua_State* L, int idx, int n)
{
    StkId o;
    api_checknelems(L, 1);
    o = index2adr(L, idx);
    api_check(L, ttistable(o));
    if (hvalue(o)->readonly)
        luaG_runerror(L, "Attempt to modify a readonly table");
    setobj2t(L, luaH_setnum(L, hvalue(o), n), L->top - 1);
    luaC_barriert(L, hvalue(o), L->top - 1);
    L->top--;
    return;
}

int lua_setmetatable(lua_State* L, int objindex)
{
    TValue* obj;
    Table* mt;
    api_checknelems(L, 1);
    obj = index2adr(L, objindex);
    api_checkvalidindex(L, obj);
    if (ttisnil(L->top - 1))
        mt = NULL;
    else
    {
        api_check(L, ttistable(L->top - 1));
        mt = hvalue(L->top - 1);
    }
    switch (ttype(obj))
    {
    case LUA_TTABLE:
    {
        if (hvalue(obj)->readonly)
            luaG_runerror(L, "Attempt to modify a readonly table");
        hvalue(obj)->metatable = mt;
        if (mt)
            luaC_objbarriert(L, hvalue(obj), mt);
        break;
    }
    case LUA_TUSERDATA:
    {
        uvalue(obj)->metatable = mt;
        if (mt)
            luaC_objbarrier(L, uvalue(obj), mt);
        break;
    }
    default:
    {
        L->global->mt[ttype(obj)] = mt;
        break;
    }
    }
    L->top--;
    return 1;
}

int lua_setfenv(lua_State* L, int idx)
{
    StkId o;
    int res = 1;
    api_checknelems(L, 1);
    o = index2adr(L, idx);
    api_checkvalidindex(L, o);
    api_check(L, ttistable(L->top - 1));
    switch (ttype(o))
    {
    case LUA_TFUNCTION:
        clvalue(o)->env = hvalue(L->top - 1);
        break;
    case LUA_TTHREAD:
        sethvalue(L, gt(thvalue(o)), hvalue(L->top - 1));
        break;
    default:
        res = 0;
        break;
    }
    if (res)
    {
        luaC_objbarrier(L, &gcvalue(o)->gch, hvalue(L->top - 1));
    }
    L->top--;
    return res;
}

/*
** `load' and `call' functions (run Lua code)
*/

#define adjustresults(L, nres) \
    { \
        if (nres == LUA_MULTRET && L->top >= L->ci->top) \
            L->ci->top = L->top; \
    }

#define checkresults(L, na, nr) api_check(L, (nr) == LUA_MULTRET || (L->ci->top - L->top >= (nr) - (na)))

void lua_call(lua_State* L, int nargs, int nresults)
{
    StkId func;
    api_checknelems(L, nargs + 1);
    api_check(L, L->status == 0);
    checkresults(L, nargs, nresults);
    func = L->top - (nargs + 1);

    int wasActive = luaC_threadactive(L);
    l_setbit(L->stackstate, THREAD_ACTIVEBIT);
    luaC_checkthreadsleep(L);

    luaD_call(L, func, nresults);

    if (!wasActive)
        resetbit(L->stackstate, THREAD_ACTIVEBIT);

    adjustresults(L, nresults);
    return;
}

/*
** Execute a protected call.
*/
struct CallS
{ /* data to `f_call' */
    StkId func;
    int nresults;
};

static void f_call(lua_State* L, void* ud)
{
    struct CallS* c = cast_to(struct CallS*, ud);
    luaD_call(L, c->func, c->nresults);
    return;
}

int lua_pcall(lua_State* L, int nargs, int nresults, int errfunc)
{
    struct CallS c;
    int status;
    ptrdiff_t func;
    api_checknelems(L, nargs + 1);
    api_check(L, L->status == 0);
    checkresults(L, nargs, nresults);
    if (errfunc == 0)
        func = 0;
    else
    {
        StkId o = index2adr(L, errfunc);
        api_checkvalidindex(L, o);
        func = savestack(L, o);
    }
    c.func = L->top - (nargs + 1); /* function to be called */
    c.nresults = nresults;

    int wasActive = luaC_threadactive(L);
    l_setbit(L->stackstate, THREAD_ACTIVEBIT);
    luaC_checkthreadsleep(L);

    status = luaD_pcall(L, f_call, &c, savestack(L, c.func), func);

    if (!wasActive)
        resetbit(L->stackstate, THREAD_ACTIVEBIT);

    adjustresults(L, nresults);
    return status;
}

int lua_status(lua_State* L)
{
    return L->status;
}

/*
** Garbage-collection function
*/

int lua_gc(lua_State* L, int what, int data)
{
    int res = 0;
    condhardmemtests(luaC_validate(L), 1);
    global_State* g = L->global;
    switch (what)
    {
    case LUA_GCSTOP:
    {
        g->GCthreshold = SIZE_MAX;
        break;
    }
    case LUA_GCRESTART:
    {
        g->GCthreshold = g->totalbytes;
        break;
    }
    case LUA_GCCOLLECT:
    {
        luaC_fullgc(L);
        break;
    }
    case LUA_GCCOUNT:
    {
        /* GC values are expressed in Kbytes: #bytes/2^10 */
        res = cast_int(g->totalbytes >> 10);
        break;
    }
    case LUA_GCISRUNNING:
    {
        res = (g->GCthreshold != SIZE_MAX);
        break;
    }
    case LUA_GCSTEP:
    {
        size_t prevthreshold = g->GCthreshold;
        size_t amount = (cast_to(size_t, data) << 10);

        // temporarily adjust the threshold so that we can perform GC work
        if (amount <= g->totalbytes)
            g->GCthreshold = g->totalbytes - amount;
        else
            g->GCthreshold = 0;

        bool waspaused = g->gcstate == GCSpause;

        // track how much work the loop will actually perform
        size_t actualwork = 0;

        while (g->GCthreshold <= g->totalbytes)
        {
            luaC_step(L, false);

            actualwork += g->gcstepsize;

            if (g->gcstate == GCSpause)
            {            /* end of cycle? */
                res = 1; /* signal it */
                break;
            }
        }

        // if cycle hasn't finished, advance threshold forward for the amount of extra work performed
        if (g->gcstate != GCSpause)
        {
            // if a new cycle was triggered by explicit step, we ignore old threshold as that shows an incorrect 'credit' of GC work
            if (waspaused)
                g->GCthreshold = g->totalbytes + actualwork;
            else
                g->GCthreshold = prevthreshold + actualwork;
        }
        break;
    }
    case LUA_GCSETGOAL:
    {
        res = g->gcgoal;
        g->gcgoal = data;
        break;
    }
    case LUA_GCSETSTEPMUL:
    {
        res = g->gcstepmul;
        g->gcstepmul = data;
        break;
    }
    case LUA_GCSETSTEPSIZE:
    {
        /* GC values are expressed in Kbytes: #bytes/2^10 */
        res = g->gcstepsize >> 10;
        g->gcstepsize = data << 10;
        break;
    }
    default:
        res = -1; /* invalid option */
    }
    return res;
}

/*
** miscellaneous functions
*/

l_noret lua_error(lua_State* L)
{
    api_checknelems(L, 1);

    luaD_throw(L, LUA_ERRRUN);
}

int lua_next(lua_State* L, int idx)
{
    luaC_checkthreadsleep(L);
    StkId t = index2adr(L, idx);
    api_check(L, ttistable(t));
    int more = luaH_next(L, hvalue(t), L->top - 1);
    if (more)
    {
        api_incr_top(L);
    }
    else             /* no more elements */
        L->top -= 1; /* remove key */
    return more;
}

void lua_concat(lua_State* L, int n)
{
    api_checknelems(L, n);
    if (n >= 2)
    {
        luaC_checkGC(L);
        luaC_checkthreadsleep(L);
        luaV_concat(L, n, cast_int(L->top - L->base) - 1);
        L->top -= (n - 1);
    }
    else if (n == 0)
    { /* push empty string */
        luaC_checkthreadsleep(L);
        setsvalue2s(L, L->top, luaS_newlstr(L, "", 0));
        api_incr_top(L);
    }
    /* else n == 1; nothing to do */
    return;
}

void* lua_newuserdata(lua_State* L, size_t sz, int tag)
{
    api_check(L, unsigned(tag) < LUA_UTAG_LIMIT);
    luaC_checkGC(L);
    luaC_checkthreadsleep(L);
    Udata* u = luaS_newudata(L, sz, tag);
    setuvalue(L, L->top, u);
    api_incr_top(L);
    return u->data;
}

void* lua_newuserdatadtor(lua_State* L, size_t sz, void (*dtor)(void*))
{
    luaC_checkGC(L);
    luaC_checkthreadsleep(L);
    Udata* u = luaS_newudata(L, sz + sizeof(dtor), UTAG_IDTOR);
    memcpy(u->data + sz, &dtor, sizeof(dtor));
    setuvalue(L, L->top, u);
    api_incr_top(L);
    return u->data;
}

static const char* aux_upvalue(StkId fi, int n, TValue** val)
{
    Closure* f;
    if (!ttisfunction(fi))
        return NULL;
    f = clvalue(fi);
    if (f->isC)
    {
        if (!(1 <= n && n <= f->nupvalues))
            return NULL;
        *val = &f->c.upvals[n - 1];
        return "";
    }
    else
    {
        Proto* p = f->l.p;
        if (!(1 <= n && n <= p->sizeupvalues))
            return NULL;
        TValue* r = &f->l.uprefs[n - 1];
        *val = ttisupval(r) ? upvalue(r)->v : r;
        return getstr(p->upvalues[n - 1]);
    }
}

const char* lua_getupvalue(lua_State* L, int funcindex, int n)
{
    luaC_checkthreadsleep(L);
    TValue* val;
    const char* name = aux_upvalue(index2adr(L, funcindex), n, &val);
    if (name)
    {
        setobj2s(L, L->top, val);
        api_incr_top(L);
    }
    return name;
}

const char* lua_setupvalue(lua_State* L, int funcindex, int n)
{
    const char* name;
    TValue* val;
    StkId fi;
    fi = index2adr(L, funcindex);
    api_checknelems(L, 1);
    name = aux_upvalue(fi, n, &val);
    if (name)
    {
        L->top--;
        setobj(L, val, L->top);
        luaC_barrier(L, clvalue(fi), L->top);
        luaC_upvalbarrier(L, NULL, val);
    }
    return name;
}

uintptr_t lua_encodepointer(lua_State* L, uintptr_t p)
{
    global_State* g = L->global;
    return uintptr_t((g->ptrenckey[0] * p + g->ptrenckey[2]) ^ (g->ptrenckey[1] * p + g->ptrenckey[3]));
}

int lua_ref(lua_State* L, int idx)
{
    int ref = LUA_REFNIL;
    global_State* g = L->global;
    StkId p = index2adr(L, idx);
    if (!ttisnil(p))
    {
        Table* reg = hvalue(registry(L));

        if (g->registryfree != 0)
        { /* reuse existing slot */
            ref = g->registryfree;
        }
        else
        { /* no free elements */
            ref = luaH_getn(reg);
            ref++; /* create new reference */
        }

        TValue* slot = luaH_setnum(L, reg, ref);
        if (g->registryfree != 0)
            g->registryfree = int(nvalue(slot));
        setobj2t(L, slot, p);
        luaC_barriert(L, reg, p);
    }
    return ref;
}

void lua_unref(lua_State* L, int ref)
{
    if (ref <= LUA_REFNIL)
        return;

    global_State* g = L->global;
    Table* reg = hvalue(registry(L));
    TValue* slot = luaH_setnum(L, reg, ref);
    setnvalue(slot, g->registryfree); /* NB: no barrier needed because value isn't collectable */
    g->registryfree = ref;
    return;
}

void lua_setuserdatadtor(lua_State* L, int tag, void (*dtor)(void*))
{
    api_check(L, unsigned(tag) < LUA_UTAG_LIMIT);
    L->global->udatagc[tag] = dtor;
}

lua_Callbacks* lua_callbacks(lua_State* L)
{
    return &L->global->cb;
}
