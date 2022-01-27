// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lua.h"

#include <string>

// Note: These are internal functions which are being exposed in a header
// so they can be included by unit tests.
int replMain(int argc, char** argv);
void setupState(lua_State* L);
std::string runCode(lua_State* L, const std::string& source);
