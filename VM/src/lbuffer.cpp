// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lbuffer.h"

#include "lgc.h"
#include "lmem.h"

#include <string.h>

LUAU_FASTFLAG(LuauBufferCage)
LUAU_FASTFLAG(LuauBufferSweep)

Buffer* luaB_newbuffer(lua_State* L, size_t s)
{
    if (s > MAX_BUFFER_SIZE)
        luaM_toobig(L);

    global_State* g = L->global;

    if (FFlag::LuauBufferSweep)
    {
        Buffer* b = (FFlag::LuauBufferCage && g->cagealloc)
                        ? luaM_newgcotagged(L, Buffer, sizebuffer(s), L->activememcat, AllocationPath::CagedRealloc, LUA_TBUFFER)
                        : luaM_newgcotagged(L, Buffer, sizebuffer(s), L->activememcat, AllocationPath::Frealloc, LUA_TBUFFER);

        luaC_init(L, b, LUA_TBUFFER);
        b->len = unsigned(s);
        memset(b->data, 0, b->len);
        return b;
    }
    else
    {
        Buffer* b = (FFlag::LuauBufferCage && g->cagealloc) ? luaM_newgcocaged(L, Buffer, sizebuffer(s), L->activememcat, LUA_TBUFFER)
                                                            : luaM_newgco(L, Buffer, sizebuffer(s), L->activememcat, LUA_TBUFFER);

        luaC_init(L, b, LUA_TBUFFER);
        b->len = unsigned(s);
        memset(b->data, 0, b->len);
        return b;
    }
}

void luaB_freebuffer(lua_State* L, Buffer* b, lua_Page* page)
{
    global_State* g = L->global;

    if (FFlag::LuauBufferSweep)
    {
        if (FFlag::LuauBufferCage && g->cagealloc)
            luaM_freegcotagged(L, b, sizebuffer(b->len), b->memcat, AllocationPath::CagedRealloc, LUA_TBUFFER, page);
        else
            luaM_freegcotagged(L, b, sizebuffer(b->len), b->memcat, AllocationPath::Frealloc, LUA_TBUFFER, page);
    }
    else
    {
        if (FFlag::LuauBufferCage && g->cagealloc)
            luaM_freegcocaged(L, b, sizebuffer(b->len), b->memcat, page);
        else
            luaM_freegco(L, b, sizebuffer(b->len), b->memcat, page);
    }
}
