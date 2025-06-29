// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#pragma once

#include "lua.h"

#define luaL_error(L, fmt, ...) luaL_errorL(L, fmt, ##__VA_ARGS__)
#define luaL_typeerror(L, narg, tname) luaL_typeerrorL(L, narg, tname)
#define luaL_argerror(L, narg, extramsg) luaL_argerrorL(L, narg, extramsg)

struct luaL_Reg
{
    const char* name;
    lua_CFunction func;
};
typedef struct luaL_Reg luaL_Reg;

LUALIB_API void luaL_register(lua_State* L, const char* libname, const luaL_Reg* l);
LUALIB_API int luaL_getmetafield(lua_State* L, int obj, const char* e);
LUALIB_API int luaL_callmeta(lua_State* L, int obj, const char* e);
LUALIB_API l_noret luaL_typeerrorL(lua_State* L, int narg, const char* tname);
LUALIB_API l_noret luaL_argerrorL(lua_State* L, int narg, const char* extramsg);
LUALIB_API const char* luaL_checklstring(lua_State* L, int numArg, size_t* l);
LUALIB_API const char* luaL_optlstring(lua_State* L, int numArg, const char* def, size_t* l);
LUALIB_API double luaL_checknumber(lua_State* L, int numArg);
LUALIB_API double luaL_optnumber(lua_State* L, int nArg, double def);

LUALIB_API int luaL_checkboolean(lua_State* L, int narg);
LUALIB_API int luaL_optboolean(lua_State* L, int narg, int def);

LUALIB_API int luaL_checkinteger(lua_State* L, int numArg);
LUALIB_API int luaL_optinteger(lua_State* L, int nArg, int def);
LUALIB_API unsigned luaL_checkunsigned(lua_State* L, int numArg);
LUALIB_API unsigned luaL_optunsigned(lua_State* L, int numArg, unsigned def);

LUALIB_API const float* luaL_checkvector(lua_State* L, int narg);
LUALIB_API const float* luaL_optvector(lua_State* L, int narg, const float* def);

LUALIB_API void luaL_checkstack(lua_State* L, int sz, const char* msg);
LUALIB_API void luaL_checktype(lua_State* L, int narg, int t);
LUALIB_API void luaL_checkany(lua_State* L, int narg);

LUALIB_API int luaL_newmetatable(lua_State* L, const char* tname);
LUALIB_API void* luaL_checkudata(lua_State* L, int ud, const char* tname);

LUALIB_API void* luaL_checkbuffer(lua_State* L, int narg, size_t* len);

LUALIB_API void luaL_where(lua_State* L, int lvl);
LUALIB_API LUA_PRINTF_ATTR(2, 3) l_noret luaL_errorL(lua_State* L, const char* fmt, ...);

LUALIB_API int luaL_checkoption(lua_State* L, int narg, const char* def, const char* const lst[]);

LUALIB_API const char* luaL_tolstring(lua_State* L, int idx, size_t* len);

LUALIB_API lua_State* luaL_newstate(void);

LUALIB_API const char* luaL_findtable(lua_State* L, int idx, const char* fname, int szhint);

LUALIB_API const char* luaL_typename(lua_State* L, int idx);

// wrapper for making calls from yieldable C functions
LUALIB_API int luaL_callyieldable(lua_State* L, int nargs, int nresults);

/*
** ===============================================================
** some useful macros
** ===============================================================
*/

#define luaL_argcheck(L, cond, arg, extramsg) ((void)((cond) ? (void)0 : luaL_argerror(L, arg, extramsg)))
#define luaL_argexpected(L, cond, arg, tname) ((void)((cond) ? (void)0 : luaL_typeerror(L, arg, tname)))

#define luaL_checkstring(L, n) (luaL_checklstring(L, (n), NULL))
#define luaL_optstring(L, n, d) (luaL_optlstring(L, (n), (d), NULL))

#define luaL_getmetatable(L, n) (lua_getfield(L, LUA_REGISTRYINDEX, (n)))

#define luaL_opt(L, f, n, d) (lua_isnoneornil(L, (n)) ? (d) : f(L, (n)))

// generic buffer manipulation

struct luaL_Strbuf
{
    char* p;   // current position in buffer
    char* end; // end of the current buffer
    lua_State* L;
    struct TString* storage;
    char buffer[LUA_BUFFERSIZE];
};
typedef struct luaL_Strbuf luaL_Strbuf;

// compatibility typedef: this type is called luaL_Buffer in Lua headers
// renamed to luaL_Strbuf to reduce confusion with internal VM buffer type
typedef struct luaL_Strbuf luaL_Buffer;

// when internal buffer storage is exhausted, a mutable string value 'storage' will be placed on the stack
// in general, functions expect the mutable string buffer to be placed on top of the stack (top-1)
// with the exception of luaL_addvalue that expects the value at the top and string buffer further away (top-2)

#define luaL_addchar(B, c) ((void)((B)->p < (B)->end || luaL_prepbuffsize(B, 1)), (*(B)->p++ = (char)(c)))
#define luaL_addstring(B, s) luaL_addlstring(B, s, strlen(s))

LUALIB_API void luaL_buffinit(lua_State* L, luaL_Strbuf* B);
LUALIB_API char* luaL_buffinitsize(lua_State* L, luaL_Strbuf* B, size_t size);
LUALIB_API char* luaL_prepbuffsize(luaL_Buffer* B, size_t size);
LUALIB_API void luaL_addlstring(luaL_Strbuf* B, const char* s, size_t l);
LUALIB_API void luaL_addvalue(luaL_Strbuf* B);
LUALIB_API void luaL_addvalueany(luaL_Strbuf* B, int idx);
LUALIB_API void luaL_pushresult(luaL_Strbuf* B);
LUALIB_API void luaL_pushresultsize(luaL_Strbuf* B, size_t size);

// builtin libraries
LUALIB_API int luaopen_base(lua_State* L);

#define LUA_COLIBNAME "coroutine"
LUALIB_API int luaopen_coroutine(lua_State* L);

#define LUA_TABLIBNAME "table"
LUALIB_API int luaopen_table(lua_State* L);

#define LUA_OSLIBNAME "os"
LUALIB_API int luaopen_os(lua_State* L);

#define LUA_STRLIBNAME "string"
LUALIB_API int luaopen_string(lua_State* L);

#define LUA_BITLIBNAME "bit32"
LUALIB_API int luaopen_bit32(lua_State* L);

#define LUA_BUFFERLIBNAME "buffer"
LUALIB_API int luaopen_buffer(lua_State* L);

#define LUA_UTF8LIBNAME "utf8"
LUALIB_API int luaopen_utf8(lua_State* L);

#define LUA_MATHLIBNAME "math"
LUALIB_API int luaopen_math(lua_State* L);

#define LUA_DBLIBNAME "debug"
LUALIB_API int luaopen_debug(lua_State* L);

#define LUA_VECLIBNAME "vector"
LUALIB_API int luaopen_vector(lua_State* L);

// open all builtin libraries
LUALIB_API void luaL_openlibs(lua_State* L);

// sandbox libraries and globals
LUALIB_API void luaL_sandbox(lua_State* L);
LUALIB_API void luaL_sandboxthread(lua_State* L);

// lbaselib.cpp
LUALIB_API int luaB_assert(lua_State* L);
LUALIB_API int luaB_error(lua_State* L);
LUALIB_API int luaB_print(lua_State* L);

LUALIB_API int luaB_type(lua_State* L);
LUALIB_API int luaB_typeof(lua_State* L);

LUALIB_API int luaB_tonumber(lua_State* L);
LUALIB_API int luaB_tostring(lua_State* L);

LUALIB_API int luaB_getfenv(lua_State* L);
LUALIB_API int luaB_setfenv(lua_State* L);

LUALIB_API int luaB_getmetatable(lua_State* L);
LUALIB_API int luaB_setmetatable(lua_State* L);

LUALIB_API int luaB_rawequal(lua_State* L);
LUALIB_API int luaB_rawget(lua_State* L);
LUALIB_API int luaB_rawset(lua_State* L);
LUALIB_API int luaB_rawlen(lua_State* L);

LUALIB_API int luaB_next(lua_State* L);
LUALIB_API int luaB_inext(lua_State* L);

LUALIB_API int luaB_pairs(lua_State* L);
LUALIB_API int luaB_ipairs(lua_State* L);

LUALIB_API int luaB_pcally(lua_State* L);
LUALIB_API int luaB_pcallcont(lua_State* L, int status);
LUALIB_API void luaB_pcallrun(lua_State* L, void* ud);

LUALIB_API int luaB_xpcally(lua_State* L);
LUALIB_API int luaB_xpcallcont(lua_State* L, int status);
LUALIB_API void luaB_xpcallerr(lua_State* L, void* ud);

LUALIB_API int luaB_type(lua_State* L);
LUALIB_API int luaB_typeof(lua_State* L);

LUALIB_API int luaB_gcinfo(lua_State* L);
LUALIB_API int luaB_select(lua_State* L);
LUALIB_API int luaB_newproxy(lua_State* L);

// lbitlib.cpp
LUALIB_API unsigned int andaux(lua_State* L);
LUALIB_API int b_arshift(lua_State* L);
LUALIB_API int b_and(lua_State* L);
LUALIB_API int b_not(lua_State* L);
LUALIB_API int b_or(lua_State* L);
LUALIB_API int b_xor(lua_State* L);
LUALIB_API int b_test(lua_State* L);
LUALIB_API int b_extract(lua_State* L);
LUALIB_API int b_lrot(lua_State* L);
LUALIB_API int b_lshift(lua_State* L);
LUALIB_API int b_replace(lua_State* L);
LUALIB_API int b_rrot(lua_State* L);
LUALIB_API int b_rshift(lua_State* L);
LUALIB_API int b_countlz(lua_State* L);
LUALIB_API int b_countrz(lua_State* L);
LUALIB_API int b_swap(lua_State* L);

// lbuflib.cpp
LUALIB_API int buffer_create(lua_State* L);
LUALIB_API int buffer_fromstring(lua_State* L);
LUALIB_API int buffer_tostring(lua_State* L);
LUALIB_API int buffer_readstring(lua_State* L);
LUALIB_API int buffer_writestring(lua_State* L);

template<typename T>
LUALIB_API int buffer_readinteger(lua_State* L);

template<typename T>
LUALIB_API int buffer_writeinteger(lua_State* L);

template<typename T, typename StorageType>
LUALIB_API int buffer_readfp(lua_State* L);

template<typename T, typename StorageType>
LUALIB_API int buffer_writefp(lua_State* L);

LUALIB_API int buffer_len(lua_State* L);
LUALIB_API int buffer_copy(lua_State* L);
LUALIB_API int buffer_fill(lua_State* L);
LUALIB_API int buffer_readbits(lua_State* L);
LUALIB_API int buffer_writebits(lua_State* L);

#if defined(LUAU_BIG_ENDIAN)
template<typename T>
LUALIB_API inline T buffer_swapbe(T v)
#endif

// lcorolib.cpp
LUALIB_API int cocreate(lua_State* L);
LUALIB_API int corunning(lua_State* L);
LUALIB_API int costatus(lua_State* L);
LUALIB_API int cowrap(lua_State* L);
LUALIB_API int coyield(lua_State* L);
LUALIB_API int coyieldable(lua_State* L);
LUALIB_API int coclose(lua_State* L);
LUALIB_API int coresumey(lua_State* L);
LUALIB_API int coresumecont(lua_State* L, int status);

// ldblib.cpp
LUALIB_API int db_info(lua_State* L);
LUALIB_API int db_traceback(lua_State* L);

// lmathlib.cpp
LUALIB_API int math_abs(lua_State* L);
LUALIB_API int math_acos(lua_State* L);
LUALIB_API int math_asin(lua_State* L);
LUALIB_API int math_atan2(lua_State* L);
LUALIB_API int math_atan(lua_State* L);
LUALIB_API int math_ceil(lua_State* L);
LUALIB_API int math_cosh(lua_State* L);
LUALIB_API int math_cos(lua_State* L);
LUALIB_API int math_deg(lua_State* L);
LUALIB_API int math_exp(lua_State* L);
LUALIB_API int math_floor(lua_State* L);
LUALIB_API int math_fmod(lua_State* L);
LUALIB_API int math_frexp(lua_State* L);
LUALIB_API int math_ldexp(lua_State* L);
LUALIB_API int math_log10(lua_State* L);
LUALIB_API int math_log(lua_State* L);
LUALIB_API int math_min(lua_State* L);
LUALIB_API int math_max(lua_State* L);
LUALIB_API int math_modf(lua_State* L);
LUALIB_API int math_pow(lua_State* L);
LUALIB_API int math_rad(lua_State* L);
LUALIB_API int math_random(lua_State* L);
LUALIB_API int math_randomseed(lua_State* L);
LUALIB_API int math_sinh(lua_State* L);
LUALIB_API int math_sin(lua_State* L);
LUALIB_API int math_sqrt(lua_State* L);
LUALIB_API int math_tanh(lua_State* L);
LUALIB_API int math_tan(lua_State* L);
LUALIB_API int math_noise(lua_State* L);
LUALIB_API int math_sign(lua_State* L);
LUALIB_API int math_round(lua_State* L);
LUALIB_API int math_map(lua_State* L);
LUALIB_API int math_lerp(lua_State* L);

// loslib.cpp
LUALIB_API int os_clock(lua_State* L);
LUALIB_API int os_date(lua_State* L);
LUALIB_API int os_difftime(lua_State* L);
LUALIB_API int os_time(lua_State* L);

// lstrlib.cpp
LUALIB_API int str_byte(lua_State* L);
LUALIB_API int str_char(lua_State* L);
LUALIB_API int str_find(lua_State* L);
LUALIB_API int str_format(lua_State* L);
LUALIB_API int gmatch(lua_State* L);
LUALIB_API int str_gsub(lua_State* L);
LUALIB_API int str_len(lua_State* L);
LUALIB_API int str_lower(lua_State* L);
LUALIB_API int str_match(lua_State* L);
LUALIB_API int str_rep(lua_State* L);
LUALIB_API int str_reverse(lua_State* L);
LUALIB_API int str_sub(lua_State* L);
LUALIB_API int str_upper(lua_State* L);
LUALIB_API int str_split(lua_State* L);
LUALIB_API int str_pack(lua_State* L);
LUALIB_API int str_packsize(lua_State* L);
LUALIB_API int str_unpack(lua_State* L);

// ltablib.cpp
LUALIB_API int tconcat(lua_State* L);
LUALIB_API int foreach(lua_State* L);
LUALIB_API int foreachi(lua_State* L);
LUALIB_API int getn(lua_State* L);
LUALIB_API int maxn(lua_State* L);
LUALIB_API int tinsert(lua_State* L);
LUALIB_API int tremove(lua_State* L);
LUALIB_API int tunpack(lua_State* L);
LUALIB_API int tmove(lua_State* L);
LUALIB_API int tcreate(lua_State* L);
LUALIB_API int tfind(lua_State* L);
LUALIB_API int tclear(lua_State* L);
LUALIB_API int tfreeze(lua_State* L);
LUALIB_API int tisfrozen(lua_State* L);
LUALIB_API int tclone(lua_State* L);

// lutf8lib.cpp
LUALIB_API int byteoffset(lua_State* L);
LUALIB_API int codepoint(lua_State* L);
LUALIB_API int utfchar(lua_State* L);
LUALIB_API int utflen(lua_State* L);
LUALIB_API int iter_codes(lua_State* L);

// lveclib.cpp
LUALIB_API int vector_create(lua_State* L);
LUALIB_API int vector_magnitude(lua_State* L);
LUALIB_API int vector_normalize(lua_State* L);
LUALIB_API int vector_cross(lua_State* L);
LUALIB_API int vector_dot(lua_State* L);
LUALIB_API int vector_angle(lua_State* L);
LUALIB_API int vector_floor(lua_State* L);
LUALIB_API int vector_ceil(lua_State* L);
LUALIB_API int vector_abs(lua_State* L);
LUALIB_API int vector_sign(lua_State* L);
LUALIB_API int vector_clamp(lua_State* L);
LUALIB_API int vector_max(lua_State* L);
LUALIB_API int vector_min(lua_State* L);
LUALIB_API int vector_index(lua_State* L);