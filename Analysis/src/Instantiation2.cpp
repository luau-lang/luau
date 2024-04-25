// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Instantiation2.h"

namespace Luau
{

bool Instantiation2::ignoreChildren(TypeId ty)
{
    if (get<ClassType>(ty))
        return true;
    return false;
}

bool Instantiation2::isDirty(TypeId ty)
{
    return get<GenericType>(ty) && genericSubstitutions.contains(ty);
}

bool Instantiation2::isDirty(TypePackId tp)
{
    return get<GenericTypePack>(tp) && genericPackSubstitutions.contains(tp);
}

TypeId Instantiation2::clean(TypeId ty)
{
    TypeId substTy = follow(genericSubstitutions[ty]);
    const FreeType* ft = get<FreeType>(substTy);

    // violation of the substitution invariant if this is not a free type.
    LUAU_ASSERT(ft);

    // if we didn't learn anything about the lower bound, we pick the upper bound instead.
    // we default to the lower bound which represents the most specific type for the free type.
    TypeId res = get<NeverType>(ft->lowerBound) ? ft->upperBound : ft->lowerBound;

    // Instantiation should not traverse into the type that we are substituting for.
    dontTraverseInto(res);

    return res;
}

TypePackId Instantiation2::clean(TypePackId tp)
{
    TypePackId res = genericPackSubstitutions[tp];
    dontTraverseInto(res);
    return res;
}

std::optional<TypeId> instantiate2(
    TypeArena* arena, DenseHashMap<TypeId, TypeId> genericSubstitutions, DenseHashMap<TypePackId, TypePackId> genericPackSubstitutions, TypeId ty)
{
    Instantiation2 instantiation{arena, std::move(genericSubstitutions), std::move(genericPackSubstitutions)};
    return instantiation.substitute(ty);
}

std::optional<TypePackId> instantiate2(
    TypeArena* arena, DenseHashMap<TypeId, TypeId> genericSubstitutions, DenseHashMap<TypePackId, TypePackId> genericPackSubstitutions, TypePackId tp)
{
    Instantiation2 instantiation{arena, std::move(genericSubstitutions), std::move(genericPackSubstitutions)};
    return instantiation.substitute(tp);
}

} // namespace Luau
