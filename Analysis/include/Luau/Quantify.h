// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeVar.h"

namespace Luau
{

struct TypeArena;
struct Scope2;

void quantify(TypeId ty, TypeLevel level);
TypeId quantify(TypeArena* arena, TypeId ty, Scope2* scope);

} // namespace Luau
