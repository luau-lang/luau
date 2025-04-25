// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

struct lua_State;

namespace Luau::Require
{

int lua_require(lua_State* L);
int lua_proxyrequire(lua_State* L);

int registerModuleImpl(lua_State* L);

} // namespace Luau::Require
