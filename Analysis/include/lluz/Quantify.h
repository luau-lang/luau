// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/TypeVar.h"

namespace lluz
{

struct TypeArena;
struct Scope2;

void quantify(TypeId ty, TypeLevel level);
TypeId quantify(TypeArena* arena, TypeId ty, Scope2* scope);

} // namespace lluz
