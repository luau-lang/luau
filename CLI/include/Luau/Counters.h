// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

struct lua_State;

void countersInit(lua_State* L);
bool countersActive();

void countersTrack(lua_State* L, int funcindex);
void countersDump(const char* path);
