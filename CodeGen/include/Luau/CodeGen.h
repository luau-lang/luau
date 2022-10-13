// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>

struct lua_State;

namespace Luau
{
namespace CodeGen
{

bool isSupported();

void create(lua_State* L);

// Builds target function and all inner functions
void compile(lua_State* L, int idx);

// Generates assembly text for target function and all inner functions
std::string getAssemblyText(lua_State* L, int idx);

} // namespace CodeGen
} // namespace Luau
