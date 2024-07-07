// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/DenseHash.h"
#include "Luau/NotNull.h"
#include "Luau/TypeFwd.h"
#include <set>

namespace Luau
{

struct TypeArena;

struct SimplifyResult
{
    TypeId result;

    DenseHashSet<TypeId> blockedTypes;
};

SimplifyResult simplifyIntersection(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, TypeId ty, TypeId discriminant);
SimplifyResult simplifyIntersection(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, std::set<TypeId> parts);

SimplifyResult simplifyUnion(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, TypeId ty, TypeId discriminant);

enum class Relation
{
    Disjoint,   // No A is a B or vice versa
    Coincident, // Every A is in B and vice versa
    Intersects, // Some As are in B and some Bs are in A.  ex (number | string) <-> (string | boolean)
    Subset,     // Every A is in B
    Superset,   // Every B is in A
};

Relation relate(TypeId left, TypeId right);

} // namespace Luau
