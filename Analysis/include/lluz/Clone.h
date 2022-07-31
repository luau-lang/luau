// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/TypeArena.h"
#include "lluz/TypeVar.h"

#include <unordered_map>

namespace lluz
{

// Only exposed so they can be unit tested.
using SeenTypes = std::unordered_map<TypeId, TypeId>;
using SeenTypePacks = std::unordered_map<TypePackId, TypePackId>;

struct CloneState
{
    SeenTypes seenTypes;
    SeenTypePacks seenTypePacks;

    int recursionCount = 0;
};

TypePackId clone(TypePackId tp, TypeArena& dest, CloneState& cloneState);
TypeId clone(TypeId tp, TypeArena& dest, CloneState& cloneState);
TypeFun clone(const TypeFun& typeFun, TypeArena& dest, CloneState& cloneState);

TypeId shallowClone(TypeId ty, TypeArena& dest, const TxnLog* log);

} // namespace lluz
