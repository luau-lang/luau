// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#pragma once

#include "lobject.h"

LUAI_FUNC const TValue* luaA_toobject(lua_State* L, int idx);
LUAI_FUNC void luaA_pushobject(lua_State* L, const TValue* o);
