// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "luajitinliner.h"

#include "Luau/JitInliner.h"

void luau_enable_jit_inliner(lua_State* L)
{
    Luau::JitInliner::setup(L);
}

void luau_disable_jit_inliner(lua_State* L)
{
    Luau::JitInliner::disable(L);
}
