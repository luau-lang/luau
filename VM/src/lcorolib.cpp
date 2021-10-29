// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lualib.h"

#include "lstate.h"
#include "lvm.h"

LUAU_FASTFLAGVARIABLE(LuauPreferXpush, false)

#define CO_RUN 0 /* running */
#define CO_SUS 1 /* suspended */
#define CO_NOR 2 /* 'normal' (it resumed another coroutine) */
#define CO_DEAD 3

#define CO_STATUS_ERROR -1
#define CO_STATUS_BREAK -2

static const char* const statnames[] = {"running", "suspended", "normal", "dead"};

static int costatus(lua_State* L, lua_State* co)
{
    if (co == L)
        return CO_RUN;
    if (co->status == LUA_YIELD)
        return CO_SUS;
    if (co->status == LUA_BREAK)
        return CO_NOR;
    if (co->status != 0) /* some error occured */
        return CO_DEAD;
    if (co->ci != co->base_ci) /* does it have frames? */
        return CO_NOR;
    if (co->top == co->base)
        return CO_DEAD;
    return CO_SUS; /* initial state */
}

static int luaB_costatus(lua_State* L)
{
    lua_State* co = lua_tothread(L, 1);
    luaL_argexpected(L, co, 1, "thread");
    lua_pushstring(L, statnames[costatus(L, co)]);
    return 1;
}

static int auxresume(lua_State* L, lua_State* co, int narg)
{
    // error handling for edge cases
    if (co->status != LUA_YIELD)
    {
        int status = costatus(L, co);
        if (status != CO_SUS)
        {
            lua_pushfstring(L, "cannot resume %s coroutine", statnames[status]);
            return CO_STATUS_ERROR;
        }
    }

    if (narg)
    {
        if (!lua_checkstack(co, narg))
            luaL_error(L, "too many arguments to resume");
        lua_xmove(L, co, narg);
    }

    co->singlestep = L->singlestep;

    int status = lua_resume(co, L, narg);
    if (status == 0 || status == LUA_YIELD)
    {
        int nres = cast_int(co->top - co->base);
        if (nres)
        {
            /* +1 accounts for true/false status in resumefinish */
            if (nres + 1 > LUA_MINSTACK && !lua_checkstack(L, nres + 1))
                luaL_error(L, "too many results to resume");
            lua_xmove(co, L, nres); /* move yielded values */
        }
        return nres;
    }
    else if (status == LUA_BREAK)
    {
        return CO_STATUS_BREAK;
    }
    else
    {
        lua_xmove(co, L, 1); /* move error message */
        return CO_STATUS_ERROR;
    }
}

static int interruptThread(lua_State* L, lua_State* co)
{
    // notify the debugger that the thread was suspended
    if (L->global->cb.debuginterrupt)
        luau_callhook(L, L->global->cb.debuginterrupt, co);

    return lua_break(L);
}

static int auxresumecont(lua_State* L, lua_State* co)
{
    if (co->status == 0 || co->status == LUA_YIELD)
    {
        int nres = cast_int(co->top - co->base);
        if (!lua_checkstack(L, nres + 1))
            luaL_error(L, "too many results to resume");
        lua_xmove(co, L, nres); /* move yielded values */
        return nres;
    }
    else
    {
        lua_rawcheckstack(L, 2);
        lua_xmove(co, L, 1); /* move error message */
        return CO_STATUS_ERROR;
    }
}

static int luaB_coresumefinish(lua_State* L, int r)
{
    if (r < 0)
    {
        lua_pushboolean(L, 0);
        lua_insert(L, -2);
        return 2; /* return false + error message */
    }
    else
    {
        lua_pushboolean(L, 1);
        lua_insert(L, -(r + 1));
        return r + 1; /* return true + `resume' returns */
    }
}

static int luaB_coresumey(lua_State* L)
{
    lua_State* co = lua_tothread(L, 1);
    luaL_argexpected(L, co, 1, "thread");
    int narg = cast_int(L->top - L->base) - 1;
    int r = auxresume(L, co, narg);

    if (r == CO_STATUS_BREAK)
        return interruptThread(L, co);

    return luaB_coresumefinish(L, r);
}

static int luaB_coresumecont(lua_State* L, int status)
{
    lua_State* co = lua_tothread(L, 1);
    luaL_argexpected(L, co, 1, "thread");

    // if coroutine still hasn't yielded after the break, break current thread again
    if (co->status == LUA_BREAK)
        return interruptThread(L, co);

    int r = auxresumecont(L, co);

    return luaB_coresumefinish(L, r);
}

static int luaB_auxwrapfinish(lua_State* L, int r)
{
    if (r < 0)
    {
        if (lua_isstring(L, -1))
        {                     /* error object is a string? */
            luaL_where(L, 1); /* add extra info */
            lua_insert(L, -2);
            lua_concat(L, 2);
        }
        lua_error(L); /* propagate error */
    }
    return r;
}

static int luaB_auxwrapy(lua_State* L)
{
    lua_State* co = lua_tothread(L, lua_upvalueindex(1));
    int narg = cast_int(L->top - L->base);
    int r = auxresume(L, co, narg);

    if (r == CO_STATUS_BREAK)
        return interruptThread(L, co);

    return luaB_auxwrapfinish(L, r);
}

static int luaB_auxwrapcont(lua_State* L, int status)
{
    lua_State* co = lua_tothread(L, lua_upvalueindex(1));

    // if coroutine still hasn't yielded after the break, break current thread again
    if (co->status == LUA_BREAK)
        return interruptThread(L, co);

    int r = auxresumecont(L, co);

    return luaB_auxwrapfinish(L, r);
}

static int luaB_cocreate(lua_State* L)
{
    luaL_checktype(L, 1, LUA_TFUNCTION);
    lua_State* NL = lua_newthread(L);

    if (FFlag::LuauPreferXpush)
    {
        lua_xpush(L, NL, 1); // push function on top of NL
    }
    else
    {
        lua_pushvalue(L, 1); /* move function to top */
        lua_xmove(L, NL, 1); /* move function from L to NL */
    }

    return 1;
}

static int luaB_cowrap(lua_State* L)
{
    luaB_cocreate(L);

    lua_pushcfunction(L, luaB_auxwrapy, NULL, 1, luaB_auxwrapcont);

    return 1;
}

static int luaB_yield(lua_State* L)
{
    int nres = cast_int(L->top - L->base);
    return lua_yield(L, nres);
}

static int luaB_corunning(lua_State* L)
{
    if (lua_pushthread(L))
        lua_pushnil(L); /* main thread is not a coroutine */
    return 1;
}

static int luaB_yieldable(lua_State* L)
{
    lua_pushboolean(L, lua_isyieldable(L));
    return 1;
}

static const luaL_Reg co_funcs[] = {
    {"create", luaB_cocreate},
    {"running", luaB_corunning},
    {"status", luaB_costatus},
    {"wrap", luaB_cowrap},
    {"yield", luaB_yield},
    {"isyieldable", luaB_yieldable},
    {NULL, NULL},
};

LUALIB_API int luaopen_coroutine(lua_State* L)
{
    luaL_register(L, LUA_COLIBNAME, co_funcs);

    lua_pushcfunction(L, luaB_coresumey, "resume", 0, luaB_coresumecont);
    lua_setfield(L, -2, "resume");

    return 1;
}
