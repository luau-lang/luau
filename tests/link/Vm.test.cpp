// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

// Check that VM can be used without any other libraries

#include "lua.h"
#include "lualib.h"

int main()
{
    lua_State* L = luaL_newstate();
    luaL_openlibs(L);
    lua_close(L);
    return 0;
}
