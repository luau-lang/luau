// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lobject.h"

namespace Luau
{
namespace CodeGen
{

bool forgLoopNodeIter(lua_State* L, Table* h, int index, TValue* ra);
bool forgLoopNonTableFallback(lua_State* L, int insnA, int aux);

void forgPrepXnextFallback(lua_State* L, TValue* ra, int pc);

} // namespace CodeGen
} // namespace Luau
