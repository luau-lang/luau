// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Require.h"

#include "Luau/Compiler.h"
#include "Luau/VfsNavigator.h"

#include "lua.h"

#include <string>

void requireConfigInit(luarequire_Configuration* config);

struct ReplRequirer
{
    using CompileOptions = Luau::CompileOptions (*)();
    using BoolCheck = bool (*)();
    using Coverage = void (*)(lua_State*, int);

    ReplRequirer(
        CompileOptions copts,
        BoolCheck coverageActive,
        BoolCheck codegenEnabled,
        Coverage coverageTrack,
        BoolCheck countersActive,
        Coverage countersTrack
    );

    CompileOptions copts;
    BoolCheck coverageActive;
    BoolCheck codegenEnabled;
    Coverage coverageTrack;
    BoolCheck countersActive;
    Coverage countersTrack;

    VfsNavigator vfs;
};
