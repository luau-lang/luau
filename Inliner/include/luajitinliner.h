// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

// Can be used to reconfigure visibility/exports for public APIs
#ifndef LUAJITINLINER_API
#define LUAJITINLINER_API extern
#endif

typedef struct lua_State lua_State;

LUAJITINLINER_API void luau_enable_jit_inliner(lua_State* L);

LUAJITINLINER_API void luau_disable_jit_inliner(lua_State* L);
