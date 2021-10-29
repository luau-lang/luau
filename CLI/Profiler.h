// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

struct lua_State;

void profilerStart(lua_State* L, int frequency);
void profilerStop();
void profilerDump(const char* name);