// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lualib.h"

#include "lvm.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

static lua_State* getthread(lua_State* L, int* arg)
{
    if (lua_isthread(L, 1))
    {
        *arg = 1;
        return lua_tothread(L, 1);
    }
    else
    {
        *arg = 0;
        return L;
    }
}

static int db_info(lua_State* L)
{
    int arg;
    lua_State* L1 = getthread(L, &arg);

    // If L1 != L, L1 can be in any state, and therefore there are no guarantees about its stack space
    if (L != L1)
        lua_rawcheckstack(L1, 1); // for 'f' option

    int level;
    if (lua_isnumber(L, arg + 1))
    {
        level = (int)lua_tointeger(L, arg + 1);
        luaL_argcheck(L, level >= 0, arg + 1, "level can't be negative");
    }
    else if (arg == 0 && lua_isfunction(L, 1))
    {
        // convert absolute index to relative index
        level = -lua_gettop(L);
    }
    else
        luaL_argerror(L, arg + 1, "function or level expected");

    const char* options = luaL_checkstring(L, arg + 2);

    lua_Debug ar;
    if (!lua_getinfo(L1, level, options, &ar))
        return 0;

    int results = 0;
    bool occurs[26] = {};

    for (const char* it = options; *it; ++it)
    {
        if (unsigned(*it - 'a') < 26)
        {
            if (occurs[*it - 'a'])
                luaL_argerror(L, arg + 2, "duplicate option");
            occurs[*it - 'a'] = true;
        }

        switch (*it)
        {
        case 's':
            lua_pushstring(L, ar.short_src);
            results++;
            break;

        case 'l':
            lua_pushinteger(L, ar.currentline);
            results++;
            break;

        case 'n':
            lua_pushstring(L, ar.name ? ar.name : "");
            results++;
            break;

        case 'f':
            if (L1 == L)
                lua_pushvalue(L, -1 - results); // function is right before results
            else
                lua_xmove(L1, L, 1); // function is at top of L1
            results++;
            break;

        case 'a':
            lua_pushinteger(L, ar.nparams);
            lua_pushboolean(L, ar.isvararg);
            results += 2;
            break;

        default:
            luaL_argerror(L, arg + 2, "invalid option");
        }
    }

    return results;
}

static int db_traceback(lua_State* L)
{
    int arg;
    lua_State* L1 = getthread(L, &arg);
    const char* msg = luaL_optstring(L, arg + 1, NULL);
    int level = luaL_optinteger(L, arg + 2, (L == L1) ? 1 : 0);
    luaL_argcheck(L, level >= 0, arg + 2, "level can't be negative");

    luaL_Buffer buf;
    luaL_buffinit(L, &buf);

    if (msg)
    {
        luaL_addstring(&buf, msg);
        luaL_addstring(&buf, "\n");
    }

    lua_Debug ar;
    for (int i = level; lua_getinfo(L1, i, "sln", &ar); ++i)
    {
        if (strcmp(ar.what, "C") == 0)
            continue;

        if (ar.source)
            luaL_addstring(&buf, ar.short_src);

        if (ar.currentline > 0)
        {
            char line[32]; // manual conversion for performance
            char* lineend = line + sizeof(line);
            char* lineptr = lineend;
            for (unsigned int r = ar.currentline; r > 0; r /= 10)
                *--lineptr = '0' + (r % 10);

            luaL_addchar(&buf, ':');
            luaL_addlstring(&buf, lineptr, lineend - lineptr, -1);
        }

        if (ar.name)
        {
            luaL_addstring(&buf, " function ");
            luaL_addstring(&buf, ar.name);
        }

        luaL_addchar(&buf, '\n');
    }

    luaL_pushresult(&buf);
    return 1;
}

static const luaL_Reg dblib[] = {
    {"info", db_info},
    {"traceback", db_traceback},
    {NULL, NULL},
};

int luaopen_debug(lua_State* L)
{
    luaL_register(L, LUA_DBLIBNAME, dblib);
    return 1;
}
