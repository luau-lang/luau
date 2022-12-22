// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#pragma once

#include "lobject.h"

// special tag value is used for user data with inline dtors
#define UTAG_IDTOR LUA_UTAG_LIMIT

// special tag value is used for newproxy-created user data (all other user data objects are host-exposed)
#define UTAG_PROXY (LUA_UTAG_LIMIT + 1)

#define sizeudata(len) (offsetof(Udata, data) + len)

LUAI_FUNC Udata* luaU_newudata(lua_State* L, size_t s, int tag);
LUAI_FUNC void luaU_freeudata(lua_State* L, Udata* u, struct lua_Page* page);
