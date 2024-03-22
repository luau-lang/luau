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
    TypeId substTy = genericSubstitutions[ty];
    const FreeType* ft = get<FreeType>(substTy);

    // violation of the substitution invariant if this is not a free type.
    LUAU_ASSERT(ft);

    // if we didn't learn anything about the lower bound, we pick the upper bound instead.
    if (get<NeverType>(ft->lowerBound))
        return ft->upperBound;

    // we default to the lower bound which represents the most specific type for the free type.
    return ft->lowerBound;
}

TypePackId Instantiation2::clean(TypePackId tp)
{
    return genericPackSubstitutions[tp];
}

} // namespace Luau
