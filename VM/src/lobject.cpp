// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#include "lobject.h"

#include "lstate.h"
#include "lstring.h"
#include "lgc.h"
#include "ldo.h"
#include "lnumutils.h"

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>



const TValue luaO_nilobject_ = {{NULL}, {0}, LUA_TNIL};

int luaO_log2(unsigned int x)
{
    static const uint8_t log_2[256] = {0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8};
    int l = -1;
    while (x >= 256)
    {
        l += 8;
        x >>= 8;
    }
    return l + log_2[x];
}

int luaO_rawequalObj(const TValue* t1, const TValue* t2)
{
    if (ttype(t1) != ttype(t2))
        return 0;
    else
        switch (ttype(t1))
        {
        case LUA_TNIL:
            return 1;
        case LUA_TNUMBER:
            return luai_numeq(nvalue(t1), nvalue(t2));
        case LUA_TVECTOR:
            return luai_veceq(vvalue(t1), vvalue(t2));
        case LUA_TBOOLEAN:
            return bvalue(t1) == bvalue(t2); /* boolean true must be 1 !! */
        case LUA_TLIGHTUSERDATA:
            return pvalue(t1) == pvalue(t2);
        default:
            LUAU_ASSERT(iscollectable(t1));
            return gcvalue(t1) == gcvalue(t2);
        }
}

int luaO_rawequalKey(const TKey* t1, const TValue* t2)
{
    if (ttype(t1) != ttype(t2))
        return 0;
    else
        switch (ttype(t1))
        {
        case LUA_TNIL:
            return 1;
        case LUA_TNUMBER:
            return luai_numeq(nvalue(t1), nvalue(t2));
        case LUA_TVECTOR:
            return luai_veceq(vvalue(t1), vvalue(t2));
        case LUA_TBOOLEAN:
            return bvalue(t1) == bvalue(t2); /* boolean true must be 1 !! */
        case LUA_TLIGHTUSERDATA:
            return pvalue(t1) == pvalue(t2);
        default:
            LUAU_ASSERT(iscollectable(t1));
            return gcvalue(t1) == gcvalue(t2);
        }
}

int luaO_str2d(const char* s, double* result)
{
    char* endptr;
    *result = luai_str2num(s, &endptr);
    if (endptr == s)
        return 0;                         /* conversion failed */
    if (*endptr == 'x' || *endptr == 'X') /* maybe an hexadecimal constant? */
        *result = cast_num(strtoul(s, &endptr, 16));
    if (*endptr == '\0')
        return 1; /* most common case */
    while (isspace(cast_to(unsigned char, *endptr)))
        endptr++;
    if (*endptr != '\0')
        return 0; /* invalid trailing characters? */
    return 1;
}

const char* luaO_pushvfstring(lua_State* L, const char* fmt, va_list argp)
{
    char result[LUA_BUFFERSIZE];
    vsnprintf(result, sizeof(result), fmt, argp);

    setsvalue2s(L, L->top, luaS_new(L, result));
    incr_top(L);
    return svalue(L->top - 1);
}

const char* luaO_pushfstring(lua_State* L, const char* fmt, ...)
{
    const char* msg;
    va_list argp;
    va_start(argp, fmt);
    msg = luaO_pushvfstring(L, fmt, argp);
    va_end(argp);
    return msg;
}

void luaO_chunkid(char* out, const char* source, size_t bufflen)
{
    if (*source == '=')
    {
        source++; /* skip the `=' */
        size_t srclen = strlen(source);
        size_t dstlen = srclen < bufflen ? srclen : bufflen - 1;
        memcpy(out, source, dstlen);
        out[dstlen] = '\0';
    }
    else if (*source == '@')
    {
        size_t l;
        source++; /* skip the `@' */
        bufflen -= sizeof(" '...' ");
        l = strlen(source);
        strcpy(out, "");
        if (l > bufflen)
        {
            source += (l - bufflen); /* get last part of file name */
            strcat(out, "...");
        }
        strcat(out, source);
    }
    else
    {                                         /* out = [string "string"] */
        size_t len = strcspn(source, "\n\r"); /* stop at first newline */
        bufflen -= sizeof(" [string \"...\"] ");
        if (len > bufflen)
            len = bufflen;
        strcpy(out, "[string \"");
        if (source[len] != '\0')
        { /* must truncate? */
            strncat(out, source, len);
            strcat(out, "...");
        }
        else
            strcat(out, source);
        strcat(out, "\"]");
    }
}
