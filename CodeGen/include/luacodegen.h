// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

// define LUACODEGEN_API to override public api linkage and visibility
#ifndef LUACODEGEN_API
#if defined(LUAU_EXTERN_C) && LUAU_EXTERN_C && defined(__cplusplus)
#define LUACODEGEN_API_LINKAGE extern "C"
#else
#define LUACODEGEN_API_LINKAGE extern
#endif

#ifndef LUACODEGEN_API_VISIBILITY
#define LUACODEGEN_API_VISIBILITY
#endif

#define LUACODEGEN_API LUACODEGEN_API_LINKAGE LUACODEGEN_API_VISIBILITY
#endif

typedef struct lua_State lua_State;

// returns 1 if Luau code generator is supported, 0 otherwise
LUACODEGEN_API int luau_codegen_supported(void);

// create an instance of Luau code generator. you must check that this feature is supported using luau_codegen_supported().
LUACODEGEN_API void luau_codegen_create(lua_State* L);

// build target function and all inner functions
LUACODEGEN_API void luau_codegen_compile(lua_State* L, int idx);
