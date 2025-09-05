// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Module.h"
#include "Luau/NotNull.h"
#include "Luau/Scope.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeFwd.h"

namespace Luau
{

struct GlobalTypes
{
    explicit GlobalTypes(NotNull<BuiltinTypes> builtinTypes, SolverMode mode);

    NotNull<BuiltinTypes> builtinTypes; // Global types are based on builtin types

    TypeArena globalTypes;
    SourceModule globalNames; // names for symbols entered into globalScope

    ScopePtr globalScope;             // shared by all modules
    ScopePtr globalTypeFunctionScope; // shared by all modules

    SolverMode mode = SolverMode::Old;
};

} // namespace Luau
