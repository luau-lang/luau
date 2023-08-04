// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Unifier2.h"

#include "Luau/Scope.h"
#include "Luau/Simplify.h"
#include "Luau/Substitution.h"
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeUtils.h"
#include "Luau/VisitType.h"

#include <algorithm>
#include <unordered_set>

LUAU_FASTINT(LuauTypeInferRecursionLimit)

namespace Luau
{

Unifier2::Unifier2(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<InternalErrorReporter> ice)
    : arena(arena)
    , builtinTypes(builtinTypes)
    , ice(ice)
    , recursionLimit(FInt::LuauTypeInferRecursionLimit)
{

}

bool Unifier2::unify(TypeId subTy, TypeId superTy)
{
    subTy = follow(subTy);
    superTy = follow(superTy);

    if (subTy == superTy)
        return true;

    FreeType* subFree = getMutable<FreeType>(subTy);
    FreeType* superFree = getMutable<FreeType>(superTy);

    if (subFree)
        subFree->upperBound = mkIntersection(subFree->upperBound, superTy);

    if (superFree)
        superFree->lowerBound = mkUnion(superFree->lowerBound, subTy);

    if (subFree || superFree)
        return true;

    const FunctionType* subFn = get<FunctionType>(subTy);
    const FunctionType* superFn = get<FunctionType>(superTy);

    if (subFn && superFn)
    {
        bool argResult = unify(superFn->argTypes, subFn->argTypes);
        bool retResult = unify(subFn->retTypes, superFn->retTypes);
        return argResult && retResult;
    }

    // The unification failed, but we're not doing type checking.
    return true;
}

// FIXME?  This should probably return an ErrorVec or an optional<TypeError>
// rather than a boolean to signal an occurs check failure.
bool Unifier2::unify(TypePackId subTp, TypePackId superTp)
{
    subTp = follow(subTp);
    superTp = follow(superTp);

    const FreeTypePack* subFree = get<FreeTypePack>(subTp);
    const FreeTypePack* superFree = get<FreeTypePack>(superTp);

    if (subFree)
    {
        DenseHashSet<TypePackId> seen{nullptr};
        if (OccursCheckResult::Fail == occursCheck(seen, subTp, superTp))
        {
            asMutable(subTp)->ty.emplace<BoundTypePack>(builtinTypes->errorRecoveryTypePack());
            return false;
        }

        asMutable(subTp)->ty.emplace<BoundTypePack>(superTp);
        return true;
    }

    if (superFree)
    {
        DenseHashSet<TypePackId> seen{nullptr};
        if (OccursCheckResult::Fail == occursCheck(seen, superTp, subTp))
        {
            asMutable(superTp)->ty.emplace<BoundTypePack>(builtinTypes->errorRecoveryTypePack());
            return false;
        }

        asMutable(superTp)->ty.emplace<BoundTypePack>(subTp);
        return true;
    }

    size_t maxLength = std::max(
        flatten(subTp).first.size(),
        flatten(superTp).first.size()
    );

    auto [subTypes, subTail] = extendTypePack(*arena, builtinTypes, subTp, maxLength);
    auto [superTypes, superTail] = extendTypePack(*arena, builtinTypes, superTp, maxLength);

    if (subTypes.size() < maxLength || superTypes.size() < maxLength)
        return true;

    for (size_t i = 0; i < maxLength; ++i)
        unify(subTypes[i], superTypes[i]);

    return true;
}

struct FreeTypeSearcher : TypeVisitor
{
    NotNull<Scope> scope;

    explicit FreeTypeSearcher(NotNull<Scope> scope)
        : TypeVisitor(/*skipBoundTypes*/ true)
        , scope(scope)
    {}

    enum { Positive, Negative } polarity = Positive;

    void flip()
    {
        switch (polarity)
        {
            case Positive: polarity = Negative; break;
            case Negative: polarity = Positive; break;
        }
    }

    std::unordered_set<TypeId> negativeTypes;
    std::unordered_set<TypeId> positiveTypes;

    bool visit(TypeId ty) override
    {
        LUAU_ASSERT(ty);
        return true;
    }

    bool visit(TypeId ty, const FreeType& ft) override
    {
        if (!subsumes(scope, ft.scope))
            return true;

        switch (polarity)
        {
            case Positive: positiveTypes.insert(ty); break;
            case Negative: negativeTypes.insert(ty); break;
        }

        return true;
    }

    bool visit(TypeId ty, const FunctionType& ft) override
    {
        flip();
        traverse(ft.argTypes);
        flip();

        traverse(ft.retTypes);

        return false;
    }
};

struct MutatingGeneralizer : TypeOnceVisitor
{
    NotNull<BuiltinTypes> builtinTypes;

    NotNull<Scope> scope;
    std::unordered_set<TypeId> positiveTypes;
    std::unordered_set<TypeId> negativeTypes;
    std::vector<TypeId> generics;

    MutatingGeneralizer(NotNull<BuiltinTypes> builtinTypes, NotNull<Scope> scope, std::unordered_set<TypeId> positiveTypes, std::unordered_set<TypeId> negativeTypes)
        : TypeOnceVisitor(/* skipBoundTypes */ true)
        , builtinTypes(builtinTypes)
        , scope(scope)
        , positiveTypes(std::move(positiveTypes))
        , negativeTypes(std::move(negativeTypes))
    {}

    static void replace(DenseHashSet<TypeId>& seen, TypeId haystack, TypeId needle, TypeId replacement)
    {
        haystack = follow(haystack);

        if (seen.find(haystack))
            return;
        seen.insert(haystack);

        std::vector<TypeId>* parts = nullptr;
        if (UnionType* ut = getMutable<UnionType>(haystack))
            parts = &ut->options;
        else if (IntersectionType* it = getMutable<IntersectionType>(needle))
            parts = &it->parts;
        else
            return;

        LUAU_ASSERT(parts);

        for (TypeId& option : *parts)
        {
            // FIXME: I bet this function has reentrancy problems
            option = follow(option);
            if (option == needle)
            {
                LUAU_ASSERT(!seen.find(option));
                option = replacement;
            }

            // TODO seen set
            else if (get<UnionType>(option))
                replace(seen, option, needle, haystack);
            else if (get<IntersectionType>(option))
                replace(seen, option, needle, haystack);
        }
    }

    bool visit (TypeId ty, const FreeType&) override
    {
        const FreeType* ft = get<FreeType>(ty);
        LUAU_ASSERT(ft);

        traverse(ft->lowerBound);
        traverse(ft->upperBound);

        // ft is potentially invalid now.
        ty = follow(ty);
        ft = get<FreeType>(ty);
        if (!ft)
            return false;

        const bool isPositive = positiveTypes.count(ty);
        const bool isNegative = negativeTypes.count(ty);

        if (!isPositive && !isNegative)
            return false;

        const bool hasLowerBound = !get<NeverType>(follow(ft->lowerBound));
        const bool hasUpperBound = !get<UnknownType>(follow(ft->upperBound));

        DenseHashSet<TypeId> seen{nullptr};
        seen.insert(ty);

        if (!hasLowerBound && !hasUpperBound)
        {
            emplaceType<GenericType>(asMutable(ty), scope);
            generics.push_back(ty);
        }

        // It is possible that this free type has other free types in its upper
        // or lower bounds.  If this is the case, we must replace those
        // references with never (for the lower bound) or unknown (for the upper
        // bound).
        //
        // If we do not do this, we get tautological bounds like a <: a <: unknown.
        else if (isPositive && !hasUpperBound)
        {
            if (FreeType* lowerFree = getMutable<FreeType>(ft->lowerBound); lowerFree && lowerFree->upperBound == ty)
                lowerFree->upperBound = builtinTypes->unknownType;
            else
                replace(seen, ft->lowerBound, ty, builtinTypes->unknownType);
            emplaceType<BoundType>(asMutable(ty), ft->lowerBound);
        }
        else
        {
            if (FreeType* upperFree = getMutable<FreeType>(ft->upperBound); upperFree && upperFree->lowerBound == ty)
                upperFree->lowerBound = builtinTypes->neverType;
            else
                replace(seen, ft->upperBound, ty, builtinTypes->neverType);
            emplaceType<BoundType>(asMutable(ty), ft->upperBound);
        }

        return false;
    }
};

std::optional<TypeId> Unifier2::generalize(NotNull<Scope> scope, TypeId ty)
{
    ty = follow(ty);

    if (ty->owningArena != arena)
        return ty;

    if (ty->persistent)
        return ty;

    if (const FunctionType* ft = get<FunctionType>(ty); ft && (!ft->generics.empty() || !ft->genericPacks.empty()))
        return ty;

    FreeTypeSearcher fts{scope};
    fts.traverse(ty);

    MutatingGeneralizer gen{builtinTypes, scope, std::move(fts.positiveTypes), std::move(fts.negativeTypes)};

    gen.traverse(ty);

    std::optional<TypeId> res = ty;

    FunctionType* ftv = getMutable<FunctionType>(follow(*res));
    if (ftv)
        ftv->generics = std::move(gen.generics);

    return res;
}

TypeId Unifier2::mkUnion(TypeId left, TypeId right)
{
    left = follow(left);
    right = follow(right);

    return simplifyUnion(builtinTypes, arena, left, right).result;
}

TypeId Unifier2::mkIntersection(TypeId left, TypeId right)
{
    left = follow(left);
    right = follow(right);

    return simplifyIntersection(builtinTypes, arena, left, right).result;
}

OccursCheckResult Unifier2::occursCheck(DenseHashSet<TypePackId>& seen, TypePackId needle, TypePackId haystack)
{
    needle = follow(needle);
    haystack = follow(haystack);

    if (seen.find(haystack))
        return OccursCheckResult::Pass;

    seen.insert(haystack);

    if (getMutable<ErrorTypePack>(needle))
        return OccursCheckResult::Pass;

    if (!getMutable<FreeTypePack>(needle))
        ice->ice("Expected needle pack to be free");

    RecursionLimiter _ra(&recursionCount, recursionLimit);

    while (!getMutable<Unifiable::Error>(haystack))
    {
        if (needle == haystack)
            return OccursCheckResult::Fail;

        if (auto a = get<TypePack>(haystack); a && a->tail)
        {
            haystack = follow(*a->tail);
            continue;
        }

        break;
    }

    return OccursCheckResult::Pass;
}

}
