// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

// Check that VM with CodeGen can be used without any other libraries

#include "lua.h"
#include "lualib.h"

#include "Luau/CodeGen.h"

int main()
{
    if (Luau::CodeGen::isSupported())
        printf("NCG supported\n");

    lua_State* L = luaL_newstate();
    luaL_openlibs(L);
    lua_close(L);
    return 0;
}
