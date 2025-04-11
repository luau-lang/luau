// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/NotNull.h"
#include "Luau/TypeFwd.h"

namespace Luau
{

struct Scope;
struct TypeArena;

void inferGenericPolarities(NotNull<TypeArena> arena, NotNull<Scope> scope, TypeId ty);
void inferGenericPolarities(NotNull<TypeArena> arena, NotNull<Scope> scope, TypePackId tp);

} // namespace Luau
