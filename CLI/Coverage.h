// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

struct lua_State;

void coverageInit(lua_State* L);
bool coverageActive();

void coverageTrack(lua_State* L, int funcindex);
void coverageDump(const char* path);
