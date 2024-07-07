// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Scope.h"
#include "Luau/NotNull.h"
#include "Luau/TypeFwd.h"

namespace Luau
{

std::optional<TypeId> generalize(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<Scope> scope,
    NotNull<DenseHashSet<TypeId>> bakedTypes, TypeId ty, /* avoid sealing tables*/ bool avoidSealingTables = false);
}
