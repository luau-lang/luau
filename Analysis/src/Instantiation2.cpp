// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"
#include "Luau/Polarity.h"
#include "Luau/Scope.h"
#include "Luau/Instantiation2.h"

LUAU_FASTFLAGVARIABLE(LuauInstantiationUsesGenericPolarity2)
namespace Luau
{

bool Instantiation2::ignoreChildren(TypeId ty)
{
    if (get<ExternType>(ty))
        return true;

    if (auto ftv = get<FunctionType>(ty))
    {
        if (ftv->hasNoFreeOrGenericTypes)
            return false;

        // If this function type quantifies over these generics, we don't want substitution to
        // go any further into them because it's being shadowed in this case.
        for (auto generic : ftv->generics)
            if (genericSubstitutions.contains(generic))
                return true;

        for (auto generic : ftv->genericPacks)
            if (genericPackSubstitutions.contains(generic))
                return true;
    }

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
    if (FFlag::LuauInstantiationUsesGenericPolarity2)
    {
        LUAU_ASSERT(subtyping && scope);
        auto generic = get<GenericType>(ty);
        LUAU_ASSERT(generic);
        TypeId substTy = follow(genericSubstitutions[ty]);
        const FreeType* ft = get<FreeType>(substTy);

        // violation of the substitution invariant if this is not a free type.
        LUAU_ASSERT(ft);

        TypeId res;
        if (is<NeverType>(ft->lowerBound))
        {
            // If the lower bound is never, assume that we can pick the
            // upper bound, and that this will provide a reasonable type.
            //
            // If we have a mixed generic who's free type is totally
            // unbound (the upper bound is `unknown` and the lower
            // bound is `never`), then we instantiate it to `unknown`.
            // This seems ... fine.
            res = ft->upperBound;
        }
        else if (is<UnknownType>(ft->upperBound))
        {
            // If the upper bound is unknown, assume we can pick the
            // lower bound, and that this will provide a reasonable
            // type.
            res = ft->lowerBound;
        }
        else
        {
            // Imagine that we have some set of bounds on a free type:
            //
            //  Q <: 'a <: Z
            //
            // If we have a mixed generic, then the upper and lower bounds
            // should inform what type to instantiate. In fact, we should
            // pick the intersection between the two. If our bounds are
            // coherent, then Q <: Z, meaning that Q & Z == Q.
            //
            // If `Q </: Z`, then try the upper bound. We _might_ error
            // later.
            auto r = subtyping->isSubtype(ft->lowerBound, ft->upperBound, NotNull{scope});
            res = r.isSubtype ? ft->lowerBound : ft->upperBound;
        }

        // Instantiation should not traverse into the type that we are substituting for.
        dontTraverseInto(res);

        return res;
    }
    else
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
}

TypePackId Instantiation2::clean(TypePackId tp)
{
    TypePackId res = genericPackSubstitutions[tp];
    dontTraverseInto(res);
    return res;
}

std::optional<TypeId> instantiate2_DEPRECATED(
    TypeArena* arena,
    DenseHashMap<TypeId, TypeId> genericSubstitutions,
    DenseHashMap<TypePackId, TypePackId> genericPackSubstitutions,
    TypeId ty
)
{
    Instantiation2 instantiation{arena, std::move(genericSubstitutions), std::move(genericPackSubstitutions)};
    return instantiation.substitute(ty);
}

std::optional<TypePackId> instantiate2_DEPRECATED(
    TypeArena* arena,
    DenseHashMap<TypeId, TypeId> genericSubstitutions,
    DenseHashMap<TypePackId, TypePackId> genericPackSubstitutions,
    TypePackId tp
)
{
    Instantiation2 instantiation{arena, std::move(genericSubstitutions), std::move(genericPackSubstitutions)};
    return instantiation.substitute(tp);
}

std::optional<TypeId> instantiate2(
    TypeArena* arena,
    DenseHashMap<TypeId, TypeId> genericSubstitutions,
    DenseHashMap<TypePackId, TypePackId> genericPackSubstitutions,
    NotNull<Subtyping> subtyping,
    NotNull<Scope> scope,
    TypeId ty
)
{
    Instantiation2 instantiation{arena, std::move(genericSubstitutions), std::move(genericPackSubstitutions), subtyping, scope};
    return instantiation.substitute(ty);
}

std::optional<TypePackId> instantiate2(
    TypeArena* arena,
    DenseHashMap<TypeId, TypeId> genericSubstitutions,
    DenseHashMap<TypePackId, TypePackId> genericPackSubstitutions,
    NotNull<Subtyping> subtyping,
    NotNull<Scope> scope,
    TypePackId tp
)
{
    Instantiation2 instantiation{arena, std::move(genericSubstitutions), std::move(genericPackSubstitutions), subtyping, scope};
    return instantiation.substitute(tp);
}

} // namespace Luau
