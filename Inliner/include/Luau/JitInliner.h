// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"

struct lua_State;

namespace Luau
{
namespace JitInliner
{

void setup(lua_State* L);
void disable(lua_State* L);

} // namespace JitInliner
} // namespace Luau
