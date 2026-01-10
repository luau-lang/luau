// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/NotNull.h"
#include "Luau/TypeFwd.h"


// Clip this file (and InferPolarity.cpp) with LuauStorePolarityInline

namespace Luau
{

struct Scope;
struct TypeArena;

void inferGenericPolarities_DEPRECATED(NotNull<TypeArena> arena, NotNull<Scope> scope, TypeId ty);
void inferGenericPolarities_DEPRECATED(NotNull<TypeArena> arena, NotNull<Scope> scope, TypePackId tp);

} // namespace Luau
