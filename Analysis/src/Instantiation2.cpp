// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"
#include "Luau/Polarity.h"
#include "Luau/Scope.h"
#include "Luau/Instantiation2.h"

LUAU_FASTFLAGVARIABLE(LuauHigherOrderGenericInference)

namespace Luau
{

Replacer::Replacer(
    NotNull<TypeArena> arena,
    NotNull<DenseHashMap<TypeId, TypeId>> replacements,
    NotNull<DenseHashMap<TypePackId, TypePackId>> replacementPacks
)
    : Substitution(TxnLog::empty(), arena)
    , replacements(replacements)
    , replacementPacks(replacementPacks)
{
    LUAU_ASSERT(checkReplacementKeys());
}

bool Replacer::isDirty(TypeId ty)
{
    return replacements->contains(ty);
}

bool Replacer::isDirty(TypePackId tp)
{
    return replacementPacks->contains(tp);
}

TypeId Replacer::clean(TypeId ty)
{
    const auto res = replacements->find(ty);
    LUAU_ASSERT(res);
    dontTraverseInto(*res);
    return *res;
}

TypePackId Replacer::clean(TypePackId tp)
{
    const auto res = replacementPacks->find(tp);
    LUAU_ASSERT(res);
    dontTraverseInto(*res);
    return *res;
}

bool Replacer::ignoreChildren(TypeId ty)
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
            if (replacements->contains(generic))
                return true;

        for (auto generic : ftv->genericPacks)
            if (replacementPacks->contains(generic))
                return true;
    }

    return false;
}

bool Replacer::checkReplacementKeys() const
{
    for (const auto& [k, _] : *replacements)
    {
        if (k != follow(k))
            return false;
    }

    for (const auto& [k, _] : *replacementPacks)
    {
        if (k != follow(k))
            return false;
    }

    return true;
}


bool Instantiation2_DEPRECATED::ignoreChildren(TypeId ty)
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

bool Instantiation2_DEPRECATED::isDirty(TypeId ty)
{
    return get<GenericType>(ty) && genericSubstitutions.contains(ty);
}

bool Instantiation2_DEPRECATED::isDirty(TypePackId tp)
{
    return get<GenericTypePack>(tp) && genericPackSubstitutions.contains(tp);
}

TypeId Instantiation2_DEPRECATED::clean(TypeId ty)
{
    LUAU_ASSERT(subtyping && scope);
    auto generic = get<GenericType>(ty);
    LUAU_ASSERT(generic);
    TypeId substTy = follow(genericSubstitutions[ty]);
    const FreeType* ft = get<FreeType>(substTy);

    // violation of the substitution invariant if this is not a free type.
    LUAU_ASSERT(ft);

    TypeId res;
    if (is<NeverType>(follow(ft->lowerBound)))
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
    else if (is<UnknownType>(follow(ft->upperBound)))
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

TypePackId Instantiation2_DEPRECATED::clean(TypePackId tp)
{
    TypePackId res = genericPackSubstitutions[tp];
    dontTraverseInto(res);
    return res;
}

void resolveGenericSubstitutions(
    TypeArena* arena,
    DenseHashMap<TypeId, TypeId>& genericSubstitutions,
    DenseHashMap<TypePackId, TypePackId>& genericPackSubstitutions,
    NotNull<Subtyping> subtyping,
    NotNull<Scope> scope
)
{
    // Collect the set of original free type IDs from genericSubstitutions
    // before we overwrite the map values.  These are the types that may
    // appear inside type pack substitutions and need to be resolved.
    DenseHashSet<TypeId> originalFreeTypes{nullptr};
    for (auto& [_, v] : genericSubstitutions)
    {
        TypeId followed = follow(v);
        if (get<FreeType>(followed))
            originalFreeTypes.insert(followed);
    }

    auto pickBound = [&](const FreeType* ft)
    {
        if (is<NeverType>(follow(ft->lowerBound)))
            return ft->upperBound;
        else if (is<UnknownType>(follow(ft->upperBound)))
            return ft->lowerBound;
        else
        {
            auto r = subtyping->isSubtype(ft->lowerBound, ft->upperBound, scope);
            return r.isSubtype ? ft->lowerBound : ft->upperBound;
        }
    };

    // Resolve each generic type substitution: update the map entry to the
    // best concrete type, leaving the original free type node untouched.
    // Other parts of the solver may still reference it.
    for (auto& [_, ty] : genericSubstitutions)
    {
        ty = follow(ty);
        if (auto ft = get<FreeType>(ty))
            ty = pickBound(ft);
    }

    // Resolve free types that appear inside type pack substitutions.
    // We build new packs rather than mutating existing ones so that other
    // references to the same pack are not affected.
    for (auto& [_, packSubst] : genericPackSubstitutions)
    {
        TypePackId followed = follow(packSubst);
        if (auto pack = get<TypePack>(followed))
        {
            bool changed = false;
            std::vector<TypeId> newHead;
            newHead.reserve(pack->head.size());
            auto iter = begin(followed);
            auto endIter = end(followed);

            while (iter != endIter)
            {
                TypeId ty = follow(*iter);
                if (auto ft = get<FreeType>(ty); ft && originalFreeTypes.contains(ty))
                {
                    newHead.push_back(pickBound(ft));
                    changed = true;
                }
                else
                    newHead.push_back(ty);

                ++iter;
            }

            if (changed)
                packSubst = arena->addTypePack(TypePack{std::move(newHead), iter.tail()});
        }
    }
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
    if (FFlag::LuauHigherOrderGenericInference)
    {
        resolveGenericSubstitutions(arena, genericSubstitutions, genericPackSubstitutions, subtyping, scope);
        Replacer r{NotNull{arena}, NotNull{&genericSubstitutions}, NotNull{&genericPackSubstitutions}};
        return r.substitute(ty);
    }

    Instantiation2_DEPRECATED instantiation{arena, std::move(genericSubstitutions), std::move(genericPackSubstitutions), subtyping, scope};
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
    if (FFlag::LuauHigherOrderGenericInference)
    {
        resolveGenericSubstitutions(arena, genericSubstitutions, genericPackSubstitutions, subtyping, scope);
        Replacer r{NotNull{arena}, NotNull{&genericSubstitutions}, NotNull{&genericPackSubstitutions}};
        return r.substitute(tp);
    }

    Instantiation2_DEPRECATED instantiation{arena, std::move(genericSubstitutions), std::move(genericPackSubstitutions), subtyping, scope};
    return instantiation.substitute(tp);
}

} // namespace Luau
