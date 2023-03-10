// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Type.h"

namespace Luau
{

struct TypeArena;
struct Scope;

void quantify(TypeId ty, TypeLevel level);
std::optional<TypeId> quantify(TypeArena* arena, TypeId ty, Scope* scope);

} // namespace Luau
