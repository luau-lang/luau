// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "luacodegen.h"

#include "Luau/CodeGen.h"

#include "lapi.h"

LUAU_FASTFLAG(LuauCodegenDetailedCompilationResult)

int luau_codegen_supported()
{
    return Luau::CodeGen::isSupported();
}

void luau_codegen_create(lua_State* L)
{
    Luau::CodeGen::create(L);
}

void luau_codegen_compile(lua_State* L, int idx)
{
    if (FFlag::LuauCodegenDetailedCompilationResult)
        Luau::CodeGen::compile(L, idx);
    else
        Luau::CodeGen::compile_DEPRECATED(L, idx);
}
