// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Require.h"

#include "Luau/Compiler.h"

#include "lua.h"

#include <functional>
#include <string>

void requireConfigInit(luarequire_Configuration* config);

struct ReplRequirer
{
    ReplRequirer(
        std::function<Luau::CompileOptions()> copts,
        std::function<bool()> coverageActive,
        std::function<bool()> codegenEnabled,
        std::function<void(lua_State*, int)> coverageTrack
    );

    std::function<Luau::CompileOptions()> copts;
    std::function<bool()> coverageActive;
    std::function<bool()> codegenEnabled;
    std::function<void(lua_State*, int)> coverageTrack;

    std::string absPath;
    std::string relPath;
    std::string suffix;
};
