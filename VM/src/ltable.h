// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#pragma once

#include "lobject.h"

#define gnode(t, i) (&(t)->node[i])
#define gkey(n) (&(n)->key)
#define gval(n) (&(n)->val)
#define gnext(n) ((n)->key.next)

#define gval2slot(t, v) int(cast_to(LuaNode*, static_cast<const TValue*>(v)) - t->node)

LUAI_FUNC const TValue* luaH_getnum(Table* t, int key);
LUAI_FUNC TValue* luaH_setnum(lua_State* L, Table* t, int key);
LUAI_FUNC const TValue* luaH_getstr(Table* t, TString* key);
LUAI_FUNC TValue* luaH_setstr(lua_State* L, Table* t, TString* key);
LUAI_FUNC const TValue* luaH_get(Table* t, const TValue* key);
LUAI_FUNC TValue* luaH_set(lua_State* L, Table* t, const TValue* key);
LUAI_FUNC Table* luaH_new(lua_State* L, int narray, int lnhash);
LUAI_FUNC void luaH_resizearray(lua_State* L, Table* t, int nasize);
LUAI_FUNC void luaH_resizehash(lua_State* L, Table* t, int nhsize);
LUAI_FUNC void luaH_free(lua_State* L, Table* t, struct lua_Page* page);
LUAI_FUNC int luaH_next(lua_State* L, Table* t, StkId key);
LUAI_FUNC int luaH_getn(Table* t);
LUAI_FUNC Table* luaH_clone(lua_State* L, Table* tt);
LUAI_FUNC void luaH_clear(Table* tt);

extern const LuaNode luaH_dummynode;
