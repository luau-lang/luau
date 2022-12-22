// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lua.h"

#include <functional>
#include <string>

using AddCompletionCallback = std::function<void(const std::string& completion, const std::string& display)>;

// Note: These are internal functions which are being exposed in a header
// so they can be included by unit tests.
void setupState(lua_State* L);
std::string runCode(lua_State* L, const std::string& source);
void getCompletions(lua_State* L, const std::string& editBuffer, const AddCompletionCallback& addCompletionCallback);

int replMain(int argc, char** argv);
