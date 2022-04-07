// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeVar.h"

#include <unordered_map>

namespace Luau
{

// Only exposed so they can be unit tested.
using SeenTypes = std::unordered_map<TypeId, TypeId>;
using SeenTypePacks = std::unordered_map<TypePackId, TypePackId>;

struct CloneState
{
    int recursionCount = 0;
    bool encounteredFreeType = false;
};

TypePackId clone(TypePackId tp, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, CloneState& cloneState);
TypeId clone(TypeId tp, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, CloneState& cloneState);
TypeFun clone(const TypeFun& typeFun, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, CloneState& cloneState);

} // namespace Luau
