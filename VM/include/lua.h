// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#pragma once

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>

#include "luaconf.h"




/* option for multiple returns in `lua_pcall' and `lua_call' */
#define LUA_MULTRET (-1)

/*
** pseudo-indices
*/
#define LUA_REGISTRYINDEX (-10000)
#define LUA_ENVIRONINDEX (-10001)
#define LUA_GLOBALSINDEX (-10002)
#define lua_upvalueindex(i) (LUA_GLOBALSINDEX - (i))

/* thread status; 0 is OK */
enum lua_Status
{
    LUA_OK = 0,
    LUA_YIELD,
    LUA_ERRRUN,
    LUA_ERRSYNTAX,
    LUA_ERRMEM,
    LUA_ERRERR,
    LUA_BREAK, /* yielded for a debug breakpoint */
};

typedef struct lua_State lua_State;

typedef int (*lua_CFunction)(lua_State* L);
typedef int (*lua_Continuation)(lua_State* L, int status);

/*
** prototype for memory-allocation functions
*/

typedef void* (*lua_Alloc)(lua_State* L, void* ud, void* ptr, size_t osize, size_t nsize);

/* non-return type */
#define l_noret void LUA_NORETURN

/*
** basic types
*/
#define LUA_TNONE (-1)

/*
 * WARNING: if you change the order of this enumeration,
 * grep "ORDER TYPE"
 */
// clang-format off
enum lua_Type
{
    LUA_TNIL = 0,     /* must be 0 due to lua_isnoneornil */
    LUA_TBOOLEAN = 1, /* must be 1 due to l_isfalse */

    
    LUA_TLIGHTUSERDATA,
    LUA_TNUMBER,
    LUA_TVECTOR,

    LUA_TSTRING, /* all types above this must be value types, all types below this must be GC types - see iscollectable */

    
    LUA_TTABLE,
    LUA_TFUNCTION,
    LUA_TUSERDATA,
    LUA_TTHREAD,

    /* values below this line are used in GCObject tags but may never show up in TValue type tags */
    LUA_TPROTO,
    LUA_TUPVAL,
    LUA_TDEADKEY,

    /* the count of TValue type tags */
    LUA_T_COUNT = LUA_TPROTO
};
// clang-format on

/* type of numbers in Luau */
typedef double lua_Number;

/* type for integer functions */
typedef int lua_Integer;

/* unsigned integer type */
typedef unsigned lua_Unsigned;

/*
** state manipulation
*/
LUA_API lua_State* lua_newstate(lua_Alloc f, void* ud);
LUA_API void lua_close(lua_State* L);
LUA_API lua_State* lua_newthread(lua_State* L);
LUA_API lua_State* lua_mainthread(lua_State* L);

/*
** basic stack manipulation
*/
LUA_API int lua_gettop(lua_State* L);
LUA_API void lua_settop(lua_State* L, int idx);
LUA_API void lua_pushvalue(lua_State* L, int idx);
LUA_API void lua_remove(lua_State* L, int idx);
LUA_API void lua_insert(lua_State* L, int idx);
LUA_API void lua_replace(lua_State* L, int idx);
LUA_API int lua_checkstack(lua_State* L, int sz);
LUA_API void lua_rawcheckstack(lua_State* L, int sz); /* allows for unlimited stack frames */

LUA_API void lua_xmove(lua_State* from, lua_State* to, int n);
LUA_API void lua_xpush(lua_State* from, lua_State* to, int idx);

/*
** access functions (stack -> C)
*/

LUA_API int lua_isnumber(lua_State* L, int idx);
LUA_API int lua_isstring(lua_State* L, int idx);
LUA_API int lua_iscfunction(lua_State* L, int idx);
LUA_API int lua_isLfunction(lua_State* L, int idx);
LUA_API int lua_isuserdata(lua_State* L, int idx);
LUA_API int lua_type(lua_State* L, int idx);
LUA_API const char* lua_typename(lua_State* L, int tp);

LUA_API int lua_equal(lua_State* L, int idx1, int idx2);
LUA_API int lua_rawequal(lua_State* L, int idx1, int idx2);
LUA_API int lua_lessthan(lua_State* L, int idx1, int idx2);

LUA_API double lua_tonumberx(lua_State* L, int idx, int* isnum);
LUA_API int lua_tointegerx(lua_State* L, int idx, int* isnum);
LUA_API unsigned lua_tounsignedx(lua_State* L, int idx, int* isnum);
LUA_API const float* lua_tovector(lua_State* L, int idx);
LUA_API int lua_toboolean(lua_State* L, int idx);
LUA_API const char* lua_tolstring(lua_State* L, int idx, size_t* len);
LUA_API const char* lua_tostringatom(lua_State* L, int idx, int* atom);
LUA_API const char* lua_namecallatom(lua_State* L, int* atom);
LUA_API int lua_objlen(lua_State* L, int idx);
LUA_API lua_CFunction lua_tocfunction(lua_State* L, int idx);
LUA_API void* lua_touserdata(lua_State* L, int idx);
LUA_API void* lua_touserdatatagged(lua_State* L, int idx, int tag);
LUA_API int lua_userdatatag(lua_State* L, int idx);
LUA_API lua_State* lua_tothread(lua_State* L, int idx);
LUA_API const void* lua_topointer(lua_State* L, int idx);

/*
** push functions (C -> stack)
*/
LUA_API void lua_pushnil(lua_State* L);
LUA_API void lua_pushnumber(lua_State* L, double n);
LUA_API void lua_pushinteger(lua_State* L, int n);
LUA_API void lua_pushunsigned(lua_State* L, unsigned n);
LUA_API void lua_pushvector(lua_State* L, float x, float y, float z);
LUA_API void lua_pushlstring(lua_State* L, const char* s, size_t l);
LUA_API void lua_pushstring(lua_State* L, const char* s);
LUA_API const char* lua_pushvfstring(lua_State* L, const char* fmt, va_list argp);
LUA_API LUA_PRINTF_ATTR(2, 3) const char* lua_pushfstringL(lua_State* L, const char* fmt, ...);
LUA_API void lua_pushcfunction(
    lua_State* L, lua_CFunction fn, const char* debugname = NULL, int nup = 0, lua_Continuation cont = NULL);
LUA_API void lua_pushboolean(lua_State* L, int b);
LUA_API void lua_pushlightuserdata(lua_State* L, void* p);
LUA_API int lua_pushthread(lua_State* L);

/*
** get functions (Lua -> stack)
*/
LUA_API void lua_gettable(lua_State* L, int idx);
LUA_API void lua_getfield(lua_State* L, int idx, const char* k);
LUA_API void lua_rawgetfield(lua_State* L, int idx, const char* k);
LUA_API void lua_rawget(lua_State* L, int idx);
LUA_API void lua_rawgeti(lua_State* L, int idx, int n);
LUA_API void lua_createtable(lua_State* L, int narr, int nrec);

LUA_API void lua_setreadonly(lua_State* L, int idx, bool value);
LUA_API int lua_getreadonly(lua_State* L, int idx);
LUA_API void lua_setsafeenv(lua_State* L, int idx, bool value);

LUA_API void* lua_newuserdata(lua_State* L, size_t sz, int tag);
LUA_API void* lua_newuserdatadtor(lua_State* L, size_t sz, void (*dtor)(void*));
LUA_API int lua_getmetatable(lua_State* L, int objindex);
LUA_API void lua_getfenv(lua_State* L, int idx);

/*
** set functions (stack -> Lua)
*/
LUA_API void lua_settable(lua_State* L, int idx);
LUA_API void lua_setfield(lua_State* L, int idx, const char* k);
LUA_API void lua_rawset(lua_State* L, int idx);
LUA_API void lua_rawseti(lua_State* L, int idx, int n);
LUA_API int lua_setmetatable(lua_State* L, int objindex);
LUA_API int lua_setfenv(lua_State* L, int idx);

/*
** `load' and `call' functions (load and run Luau bytecode)
*/
LUA_API int luau_load(lua_State* L, const char* chunkname, const char* data, size_t size, int env = 0);
LUA_API void lua_call(lua_State* L, int nargs, int nresults);
LUA_API int lua_pcall(lua_State* L, int nargs, int nresults, int errfunc);

/*
** coroutine functions
*/
LUA_API int lua_yield(lua_State* L, int nresults);
LUA_API int lua_break(lua_State* L);
LUA_API int lua_resume(lua_State* L, lua_State* from, int narg);
LUA_API int lua_resumeerror(lua_State* L, lua_State* from);
LUA_API int lua_status(lua_State* L);
LUA_API int lua_isyieldable(lua_State* L);

/*
** garbage-collection function and options
*/

enum lua_GCOp
{
    LUA_GCSTOP,
    LUA_GCRESTART,
    LUA_GCCOLLECT,
    LUA_GCCOUNT,
    LUA_GCISRUNNING,

    // garbage collection is handled by 'assists' that perform some amount of GC work matching pace of allocation
    // explicit GC steps allow to perform some amount of work at custom points to offset the need for GC assists
    // note that GC might also be paused for some duration (until bytes allocated meet the threshold)
    // if an explicit step is performed during this pause, it will trigger the start of the next collection cycle
    LUA_GCSTEP,

    LUA_GCSETGOAL,
    LUA_GCSETSTEPMUL,
    LUA_GCSETSTEPSIZE,
};

LUA_API int lua_gc(lua_State* L, int what, int data);

/*
** miscellaneous functions
*/

LUA_API l_noret lua_error(lua_State* L);

LUA_API int lua_next(lua_State* L, int idx);

LUA_API void lua_concat(lua_State* L, int n);

LUA_API uintptr_t lua_encodepointer(lua_State* L, uintptr_t p);

LUA_API double lua_clock();

LUA_API void lua_setuserdatadtor(lua_State* L, int tag, void (*dtor)(void*));

/*
** reference system, can be used to pin objects
*/
#define LUA_NOREF -1
#define LUA_REFNIL 0

LUA_API int lua_ref(lua_State* L, int idx);
LUA_API void lua_unref(lua_State* L, int ref);

#define lua_getref(L, ref) lua_rawgeti(L, LUA_REGISTRYINDEX, (ref))

/*
** ===============================================================
** some useful macros
** ===============================================================
*/
#define lua_tonumber(L, i) lua_tonumberx(L, i, NULL)
#define lua_tointeger(L, i) lua_tointegerx(L, i, NULL)
#define lua_tounsigned(L, i) lua_tounsignedx(L, i, NULL)

#define lua_pop(L, n) lua_settop(L, -(n)-1)

#define lua_newtable(L) lua_createtable(L, 0, 0)

#define lua_strlen(L, i) lua_objlen(L, (i))

#define lua_isfunction(L, n) (lua_type(L, (n)) == LUA_TFUNCTION)
#define lua_istable(L, n) (lua_type(L, (n)) == LUA_TTABLE)
#define lua_islightuserdata(L, n) (lua_type(L, (n)) == LUA_TLIGHTUSERDATA)
#define lua_isnil(L, n) (lua_type(L, (n)) == LUA_TNIL)
#define lua_isboolean(L, n) (lua_type(L, (n)) == LUA_TBOOLEAN)
#define lua_isthread(L, n) (lua_type(L, (n)) == LUA_TTHREAD)
#define lua_isnone(L, n) (lua_type(L, (n)) == LUA_TNONE)
#define lua_isnoneornil(L, n) (lua_type(L, (n)) <= LUA_TNIL)

#define lua_pushliteral(L, s) lua_pushlstring(L, "" s, (sizeof(s) / sizeof(char)) - 1)

#define lua_setglobal(L, s) lua_setfield(L, LUA_GLOBALSINDEX, (s))
#define lua_getglobal(L, s) lua_getfield(L, LUA_GLOBALSINDEX, (s))

#define lua_tostring(L, i) lua_tolstring(L, (i), NULL)

#define lua_pushfstring(L, fmt, ...) lua_pushfstringL(L, fmt, ##__VA_ARGS__)

/*
** {======================================================================
** Debug API
** =======================================================================
*/

typedef struct lua_Debug lua_Debug; /* activation record */

/* Functions to be called by the debugger in specific events */
typedef void (*lua_Hook)(lua_State* L, lua_Debug* ar);

LUA_API int lua_getinfo(lua_State* L, int level, const char* what, lua_Debug* ar);
LUA_API int lua_getargument(lua_State* L, int level, int n);
LUA_API const char* lua_getlocal(lua_State* L, int level, int n);
LUA_API const char* lua_setlocal(lua_State* L, int level, int n);
LUA_API const char* lua_getupvalue(lua_State* L, int funcindex, int n);
LUA_API const char* lua_setupvalue(lua_State* L, int funcindex, int n);

LUA_API void lua_singlestep(lua_State* L, bool singlestep);
LUA_API void lua_breakpoint(lua_State* L, int funcindex, int line, bool enable);

/* Warning: this function is not thread-safe since it stores the result in a shared global array! Only use for debugging. */
LUA_API const char* lua_debugtrace(lua_State* L);

struct lua_Debug
{
    const char* name;           /* (n) */
    const char* what;           /* (s) `Lua', `C', `main', `tail' */
    const char* source;         /* (s) */
    int linedefined;            /* (s) */
    int currentline;            /* (l) */
    unsigned char nupvals;      /* (u) number of upvalues */
    unsigned char nparams;      /* (a) number of parameters */
    char isvararg;              /* (a) */
    char short_src[LUA_IDSIZE]; /* (s) */
    void* userdata;             /* only valid in luau_callhook */
};

/* }====================================================================== */

/* Callbacks that can be used to reconfigure behavior of the VM dynamically.
 * These are shared between all coroutines.
 *
 * Note: interrupt is safe to set from an arbitrary thread but all other callbacks
 * can only be changed when the VM is not running any code */
struct lua_Callbacks
{
    void (*interrupt)(lua_State* L, int gc);  /* gets called at safepoints (loop back edges, call/ret, gc) if set */
    void (*panic)(lua_State* L, int errcode); /* gets called when an unprotected error is raised (if longjmp is used) */

    void (*userthread)(lua_State* LP, lua_State* L); /* gets called when L is created (LP == parent) or destroyed (LP == NULL) */
    int16_t (*useratom)(const char* s, size_t l);    /* gets called when a string is created; returned atom can be retrieved via tostringatom */

    void (*debugbreak)(lua_State* L, lua_Debug* ar);     /* gets called when BREAK instruction is encountered */
    void (*debugstep)(lua_State* L, lua_Debug* ar);      /* gets called after each instruction in single step mode */
    void (*debuginterrupt)(lua_State* L, lua_Debug* ar); /* gets called when thread execution is interrupted by break in another thread */
    void (*debugprotectederror)(lua_State* L);           /* gets called when protected call results in an error */
};

LUA_API lua_Callbacks* lua_callbacks(lua_State* L);

/******************************************************************************
 * Copyright (c) 2019-2021 Roblox Corporation
 * Copyright (C) 1994-2008 Lua.org, PUC-Rio.  All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 ******************************************************************************/
