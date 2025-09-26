// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Unifier2.h"

#include "Luau/Instantiation.h"
#include "Luau/Scope.h"
#include "Luau/Simplify.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/VisitType.h"

#include <algorithm>
#include <optional>

LUAU_FASTINT(LuauTypeInferIterationLimit)
LUAU_FASTINT(LuauTypeInferRecursionLimit)

LUAU_FASTFLAG(LuauIndividualRecursionLimits)
LUAU_DYNAMIC_FASTINTVARIABLE(LuauUnifierRecursionLimit, 100)

LUAU_FASTFLAG(LuauEmplaceNotPushBack)
LUAU_FASTFLAGVARIABLE(LuauLimitUnification)
LUAU_FASTFLAGVARIABLE(LuauUnifyShortcircuitSomeIntersectionsAndUnions)
LUAU_FASTFLAGVARIABLE(LuauTryToOptimizeSetTypeUnification)
LUAU_FASTFLAGVARIABLE(LuauFixNilRightPad)

namespace Luau
{

static bool isOptionalOrFree(TypeId ty)
{
    ty = follow(ty);
    return isOptional(ty) || (get<FreeType>(ty) != nullptr);
}

static bool areCompatible(TypeId left, TypeId right)
{
    auto p = get2<TableType, TableType>(follow(left), follow(right));
    if (!p)
        return true;

    const TableType* leftTable = p.first;
    LUAU_ASSERT(leftTable);
    const TableType* rightTable = p.second;
    LUAU_ASSERT(rightTable);

    const auto missingPropIsCompatible = [](const Property& leftProp, const TableType* rightTable)
    {
        // Two tables may be compatible even if their shapes aren't exactly the
        // same if the extra property is optional, free (and therefore
        // potentially optional), or if the right table has an indexer.  Or if
        // the right table is free (and therefore potentially has an indexer or
        // a compatible property)

        if (rightTable->state == TableState::Free || rightTable->indexer.has_value())
            return true;

        if (leftProp.isReadOnly() || leftProp.isShared())
        {
            if (isOptionalOrFree(*leftProp.readTy))
                return true;
        }

        // FIXME: Could this create an issue for write only / divergent properties?
        return false;
    };

    for (const auto& [name, leftProp] : leftTable->props)
    {
        auto it = rightTable->props.find(name);
        if (it == rightTable->props.end())
        {
            if (!missingPropIsCompatible(leftProp, rightTable))
                return false;
        }
    }

    for (const auto& [name, rightProp] : rightTable->props)
    {
        auto it = leftTable->props.find(name);
        if (it == leftTable->props.end())
        {
            if (!missingPropIsCompatible(rightProp, leftTable))
                return false;
        }
    }

    return true;
}

// returns `true` if `ty` is irressolvable and should be added to `incompleteSubtypes`.
static bool isIrresolvable(TypeId ty)
{
    if (auto tfit = get<TypeFunctionInstanceType>(ty); tfit && tfit->state != TypeFunctionInstanceState::Unsolved)
        return false;

    return get<BlockedType>(ty) || get<TypeFunctionInstanceType>(ty);
}

// returns `true` if `tp` is irressolvable and should be added to `incompleteSubtypes`.
static bool isIrresolvable(TypePackId tp)
{
    return get<BlockedTypePack>(tp) || get<TypeFunctionInstanceTypePack>(tp);
}

Unifier2::Unifier2(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<Scope> scope, NotNull<InternalErrorReporter> ice)
    : arena(arena)
    , builtinTypes(builtinTypes)
    , scope(scope)
    , ice(ice)
    , limits(TypeCheckLimits{}) // TODO: typecheck limits in unifier2
    , recursionLimit(FFlag::LuauIndividualRecursionLimits ? DFInt::LuauUnifierRecursionLimit : FInt::LuauTypeInferRecursionLimit)
    , uninhabitedTypeFunctions(nullptr)
{
}

Unifier2::Unifier2(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    NotNull<InternalErrorReporter> ice,
    DenseHashSet<const void*>* uninhabitedTypeFunctions
)
    : arena(arena)
    , builtinTypes(builtinTypes)
    , scope(scope)
    , ice(ice)
    , limits(TypeCheckLimits{}) // TODO: typecheck limits in unifier2
    , recursionLimit(FFlag::LuauIndividualRecursionLimits ? DFInt::LuauUnifierRecursionLimit : FInt::LuauTypeInferRecursionLimit)
    , uninhabitedTypeFunctions(uninhabitedTypeFunctions)
{
}

UnifyResult Unifier2::unify(TypeId subTy, TypeId superTy)
{
    iterationCount = 0;
    return unify_(subTy, superTy);
}

UnifyResult Unifier2::unify(TypePackId subTp, TypePackId superTp)
{
    iterationCount = 0;
    return unify_(subTp, superTp);
}

UnifyResult Unifier2::unify_(TypeId subTy, TypeId superTy)
{
    if (FFlag::LuauLimitUnification)
    {
        if (FInt::LuauTypeInferIterationLimit > 0 && iterationCount >= FInt::LuauTypeInferIterationLimit)
            return UnifyResult::TooComplex;

        ++iterationCount;
    }

    subTy = follow(subTy);
    superTy = follow(superTy);

    if (auto subGen = genericSubstitutions.find(subTy))
        return unify_(*subGen, superTy);

    if (auto superGen = genericSubstitutions.find(superTy))
        return unify_(subTy, *superGen);

    if (seenTypePairings.contains({subTy, superTy}))
        return UnifyResult::Ok;
    seenTypePairings.insert({subTy, superTy});

    if (subTy == superTy)
        return UnifyResult::Ok;

    // We have potentially done some unifications while dispatching either `SubtypeConstraint` or `PackSubtypeConstraint`,
    // so rather than implementing backtracking or traversing the entire type graph multiple times, we could push
    // additional constraints as we discover blocked types along with their proper bounds.
    //
    // But we exclude these two subtyping patterns, they are tautological:
    //   - never <: *blocked*
    //   - *blocked* <: unknown
    if ((isIrresolvable(subTy) || isIrresolvable(superTy)) && !get<NeverType>(subTy) && !get<UnknownType>(superTy))
    {
        if (uninhabitedTypeFunctions && (uninhabitedTypeFunctions->contains(subTy) || uninhabitedTypeFunctions->contains(superTy)))
            return UnifyResult::Ok;

        if (FFlag::LuauEmplaceNotPushBack)
            incompleteSubtypes.emplace_back(SubtypeConstraint{subTy, superTy});
        else
            incompleteSubtypes.push_back(SubtypeConstraint{subTy, superTy});
        return UnifyResult::Ok;
    }

    FreeType* subFree = getMutable<FreeType>(subTy);
    FreeType* superFree = getMutable<FreeType>(superTy);

    if (superFree)
    {
        superFree->lowerBound = mkUnion(superFree->lowerBound, subTy);
    }

    if (subFree)
    {
        return unifyFreeWithType(subTy, superTy);
    }

    if (subFree || superFree)
        return UnifyResult::Ok;

    auto subFn = get<FunctionType>(subTy);
    auto superFn = get<FunctionType>(superTy);
    if (subFn && superFn)
        return unify_(subTy, superFn);

    if (FFlag::LuauTryToOptimizeSetTypeUnification)
    {
        auto subUnion = get<UnionType>(subTy);
        auto superUnion = get<UnionType>(superTy);

        auto subIntersection = get<IntersectionType>(subTy);
        auto superIntersection = get<IntersectionType>(superTy);

        // This is, effectively, arranged to avoid the following:
        //
        //  'a & T <: U | V => 'a & T <: U and 'a & T <: V
        //

        // For T <: U & V and T | U <: V, these two cases are entirely correct.

        // We decompose T <: U & V above into T <: U and T <: V ...
        if (superIntersection)
            return unify_(subTy, superIntersection);

        // ... and T | U <: V into T <: V and U <: V.
        if (subUnion)
            return unify_(subUnion, superTy);

        // This, T & U <: V, erroneously is decomposed into T <: U and T <: V,
        // even though technically we only need one of the above to hold.
        // However, this ordering means that we avoid ...
        if (subIntersection)
            return unify_(subIntersection, superTy);

        // T <: U | V decomposing into T <: U and T <: V is incorrect, and
        // can result in some really strange user-visible bugs. Consider:
        //
        //  'a & ~(false?) <: string | number
        //
        // Intuitively, this should place a constraint of `string | number`
        // on the upper bound of `'a`. But if we hit this case, then we
        // end up with something like:
        //
        //  'a & ~(false?) <: string and 'a & ~(false?) <: number
        //
        // ... which will result in `'a` having `string & number` as its
        // upper bound, and being inferred to `never`.
        if (superUnion)
            return unify_(subTy, superUnion);
    }
    else
    {
        auto subUnion = get<UnionType>(subTy);
        auto superUnion = get<UnionType>(superTy);
        if (subUnion)
            return unify_(subUnion, superTy);
        else if (superUnion)
            return unify_(subTy, superUnion);

        auto subIntersection = get<IntersectionType>(subTy);
        auto superIntersection = get<IntersectionType>(superTy);
        if (subIntersection)
            return unify_(subIntersection, superTy);
        else if (superIntersection)
            return unify_(subTy, superIntersection);
    }

    auto subNever = get<NeverType>(subTy);
    auto superNever = get<NeverType>(superTy);
    if (subNever && superNever)
        return UnifyResult::Ok;
    else if (subNever && superFn)
    {
        // If `never` is the subtype, then we can propagate that inward.
        UnifyResult argResult = unify_(superFn->argTypes, builtinTypes->neverTypePack);
        UnifyResult retResult = unify_(builtinTypes->neverTypePack, superFn->retTypes);
        return argResult & retResult;
    }
    else if (subFn && superNever)
    {
        // If `never` is the supertype, then we can propagate that inward.
        UnifyResult argResult = unify_(builtinTypes->neverTypePack, subFn->argTypes);
        UnifyResult retResult = unify_(subFn->retTypes, builtinTypes->neverTypePack);
        return argResult & retResult;
    }

    auto subAny = get<AnyType>(subTy);
    auto superAny = get<AnyType>(superTy);

    auto subTable = getMutable<TableType>(subTy);
    auto superTable = get<TableType>(superTy);

    if (subAny && superAny)
        return UnifyResult::Ok;
    else if (subAny && superFn)
        return unify_(subAny, superFn);
    else if (subFn && superAny)
        return unify_(subFn, superAny);
    else if (subAny && superTable)
        return unify_(subAny, superTable);
    else if (subTable && superAny)
        return unify_(subTable, superAny);

    if (subTable && superTable)
    {
        // `boundTo` works like a bound type, and therefore we'd replace it
        // with the `boundTo` and try unification again.
        //
        // However, these pointers should have been chased already by follow().
        LUAU_ASSERT(!subTable->boundTo);
        LUAU_ASSERT(!superTable->boundTo);

        return unify_(subTable, superTable);
    }

    auto subMetatable = get<MetatableType>(subTy);
    auto superMetatable = get<MetatableType>(superTy);
    if (subMetatable && superMetatable)
        return unify_(subMetatable, superMetatable);
    else if (subMetatable && superAny)
        return unify_(subMetatable, superAny);
    else if (subAny && superMetatable)
        return unify_(subAny, superMetatable);
    else if (subMetatable) // if we only have one metatable, unify with the inner table
        return unify_(subMetatable->table, superTy);
    else if (superMetatable) // if we only have one metatable, unify with the inner table
        return unify_(subTy, superMetatable->table);

    auto [subNegation, superNegation] = get2<NegationType, NegationType>(subTy, superTy);
    if (subNegation && superNegation)
        return unify_(subNegation->ty, superNegation->ty);

    // The unification failed, but we're not doing type checking.
    return UnifyResult::Ok;
}

// If superTy is a function and subTy already has a
// potentially-compatible function in its upper bound, we assume that
// the function is not overloaded and attempt to combine superTy into
// subTy's existing function bound.
UnifyResult Unifier2::unifyFreeWithType(TypeId subTy, TypeId superTy)
{
    FreeType* subFree = getMutable<FreeType>(subTy);
    LUAU_ASSERT(subFree);

    auto doDefault = [&]()
    {
        subFree->upperBound = mkIntersection(subFree->upperBound, superTy);
        expandedFreeTypes[subTy].push_back(superTy);
        return UnifyResult::Ok;
    };

    TypeId upperBound = follow(subFree->upperBound);

    if (get<FunctionType>(upperBound))
        return unify_(subFree->upperBound, superTy);

    const FunctionType* superFunction = get<FunctionType>(superTy);
    if (!superFunction)
        return doDefault();

    const auto [superArgHead, superArgTail] = flatten(superFunction->argTypes);
    if (superArgTail)
        return doDefault();

    const IntersectionType* upperBoundIntersection = get<IntersectionType>(upperBound);
    if (!upperBoundIntersection)
        return doDefault();

    UnifyResult result = UnifyResult::Ok;
    bool foundOne = false;

    for (TypeId part : upperBoundIntersection->parts)
    {
        const FunctionType* ft = get<FunctionType>(follow(part));
        if (!ft)
            continue;

        const auto [subArgHead, subArgTail] = flatten(ft->argTypes);

        if (!subArgTail && subArgHead.size() == superArgHead.size())
        {
            foundOne = true;
            result &= unify_(part, superTy);
        }
    }

    if (foundOne)
        return result;
    else
        return doDefault();
}

UnifyResult Unifier2::unify_(TypeId subTy, const FunctionType* superFn)
{
    const FunctionType* subFn = get<FunctionType>(subTy);

    bool shouldInstantiate =
        (superFn->generics.empty() && !subFn->generics.empty()) || (superFn->genericPacks.empty() && !subFn->genericPacks.empty());

    if (shouldInstantiate)
    {
        for (TypeId generic : subFn->generics)
        {
            const GenericType* gen = get<GenericType>(follow(generic));
            if (gen)
                genericSubstitutions[generic] = freshType(scope, gen->polarity);
        }

        for (TypePackId genericPack : subFn->genericPacks)
        {
            genericPack = follow(genericPack);

            const GenericTypePack* gen = get<GenericTypePack>(genericPack);
            if (gen)
                genericPackSubstitutions[genericPack] = freshTypePack(scope, gen->polarity);
        }
    }

    UnifyResult argResult = unify_(superFn->argTypes, subFn->argTypes);
    UnifyResult retResult = unify_(subFn->retTypes, superFn->retTypes);
    return argResult & retResult;
}

UnifyResult Unifier2::unify_(const UnionType* subUnion, TypeId superTy)
{
    UnifyResult result = UnifyResult::Ok;

    // if the occurs check fails for any option, it fails overall
    for (auto subOption : subUnion->options)
    {
        if (areCompatible(subOption, superTy))
            result &= unify_(subOption, superTy);
    }

    return result;
}

UnifyResult Unifier2::unify_(TypeId subTy, const UnionType* superUnion)
{
    if (FFlag::LuauUnifyShortcircuitSomeIntersectionsAndUnions)
    {
        subTy = follow(subTy);
        // T <: T | U1 | U2 | ... | Un is trivially true, so we don't gain any information by unifying
        for (const auto superOption : superUnion)
        {
            if (subTy == superOption)
                return UnifyResult::Ok;
        }
    }

    UnifyResult result = UnifyResult::Ok;

    // if the occurs check fails for any option, it fails overall
    for (auto superOption : superUnion->options)
    {
        if (areCompatible(subTy, superOption))
            result &= unify_(subTy, superOption);
    }

    return result;
}

UnifyResult Unifier2::unify_(const IntersectionType* subIntersection, TypeId superTy)
{
    if (FFlag::LuauUnifyShortcircuitSomeIntersectionsAndUnions)
    {
        superTy = follow(superTy);
        // T & I1 & I2 & ... & In <: T is trivially true, so we don't gain any information by unifying
        for (const auto subOption : subIntersection)
        {
            if (superTy == subOption)
                return UnifyResult::Ok;
        }
    }

    UnifyResult result = UnifyResult::Ok;

    // if the occurs check fails for any part, it fails overall
    for (auto subPart : subIntersection->parts)
        result &= unify_(subPart, superTy);

    return result;
}

UnifyResult Unifier2::unify_(TypeId subTy, const IntersectionType* superIntersection)
{
    UnifyResult result = UnifyResult::Ok;

    // if the occurs check fails for any part, it fails overall
    for (auto superPart : superIntersection->parts)
        result &= unify_(subTy, superPart);

    return result;
}

UnifyResult Unifier2::unify_(TableType* subTable, const TableType* superTable)
{
    UnifyResult result = UnifyResult::Ok;

    // It suffices to only check one direction of properties since we'll only ever have work to do during unification
    // if the property is present in both table types.
    for (const auto& [propName, subProp] : subTable->props)
    {
        auto superPropOpt = superTable->props.find(propName);

        if (superPropOpt != superTable->props.end())
        {
            const Property& superProp = superPropOpt->second;

            if (subProp.readTy && superProp.readTy)
                result &= unify_(*subProp.readTy, *superProp.readTy);

            if (subProp.writeTy && superProp.writeTy)
                result &= unify_(*superProp.writeTy, *subProp.writeTy);
        }
    }

    auto subTypeParamsIter = subTable->instantiatedTypeParams.begin();
    auto superTypeParamsIter = superTable->instantiatedTypeParams.begin();

    while (subTypeParamsIter != subTable->instantiatedTypeParams.end() && superTypeParamsIter != superTable->instantiatedTypeParams.end())
    {
        result &= unify_(*subTypeParamsIter, *superTypeParamsIter);

        subTypeParamsIter++;
        superTypeParamsIter++;
    }

    auto subTypePackParamsIter = subTable->instantiatedTypePackParams.begin();
    auto superTypePackParamsIter = superTable->instantiatedTypePackParams.begin();

    while (subTypePackParamsIter != subTable->instantiatedTypePackParams.end() &&
           superTypePackParamsIter != superTable->instantiatedTypePackParams.end())
    {
        result &= unify_(*subTypePackParamsIter, *superTypePackParamsIter);

        subTypePackParamsIter++;
        superTypePackParamsIter++;
    }

    if (subTable->indexer && superTable->indexer)
    {
        result &= unify_(subTable->indexer->indexType, superTable->indexer->indexType);
        result &= unify_(subTable->indexer->indexResultType, superTable->indexer->indexResultType);

        // FIXME: We can probably do something more efficient here.
        result &= unify_(superTable->indexer->indexType, subTable->indexer->indexType);
        result &= unify_(superTable->indexer->indexResultType, subTable->indexer->indexResultType);
    }

    if (!subTable->indexer && subTable->state == TableState::Unsealed && superTable->indexer)
    {
        /*
         * Unsealed tables are always created from literal table expressions. We
         * can't be completely certain whether such a table has an indexer just
         * by the content of the expression itself, so we need to be a bit more
         * flexible here.
         *
         * If we are trying to reconcile an unsealed table with a table that has
         * an indexer, we therefore conclude that the unsealed table has the
         * same indexer.
         */

        TypeId indexType = superTable->indexer->indexType;
        if (TypeId* subst = genericSubstitutions.find(indexType))
            indexType = *subst;

        TypeId indexResultType = superTable->indexer->indexResultType;
        if (TypeId* subst = genericSubstitutions.find(indexResultType))
            indexResultType = *subst;

        subTable->indexer = TableIndexer{indexType, indexResultType};
    }

    return result;
}

UnifyResult Unifier2::unify_(const MetatableType* subMetatable, const MetatableType* superMetatable)
{
    UnifyResult metatableResult = unify_(subMetatable->metatable, superMetatable->metatable);
    if (metatableResult != UnifyResult::Ok)
        return metatableResult;
    return unify_(subMetatable->table, superMetatable->table);
}

UnifyResult Unifier2::unify_(const AnyType* subAny, const FunctionType* superFn)
{
    // If `any` is the subtype, then we can propagate that inward.
    UnifyResult argResult = unify_(superFn->argTypes, builtinTypes->anyTypePack);
    UnifyResult retResult = unify_(builtinTypes->anyTypePack, superFn->retTypes);
    return argResult & retResult;
}

UnifyResult Unifier2::unify_(const FunctionType* subFn, const AnyType* superAny)
{
    // If `any` is the supertype, then we can propagate that inward.
    UnifyResult argResult = unify_(builtinTypes->anyTypePack, subFn->argTypes);
    UnifyResult retResult = unify_(subFn->retTypes, builtinTypes->anyTypePack);
    return argResult & retResult;
}

UnifyResult Unifier2::unify_(const AnyType* subAny, const TableType* superTable)
{
    for (const auto& [propName, prop] : superTable->props)
    {
        if (prop.readTy)
            unify_(builtinTypes->anyType, *prop.readTy);

        if (prop.writeTy)
            unify_(*prop.writeTy, builtinTypes->anyType);
    }

    if (superTable->indexer)
    {
        unify_(builtinTypes->anyType, superTable->indexer->indexType);
        unify_(builtinTypes->anyType, superTable->indexer->indexResultType);
    }

    return UnifyResult::Ok;
}

UnifyResult Unifier2::unify_(const TableType* subTable, const AnyType* superAny)
{
    for (const auto& [propName, prop] : subTable->props)
    {
        if (prop.readTy)
            unify_(*prop.readTy, builtinTypes->anyType);

        if (prop.writeTy)
            unify_(builtinTypes->anyType, *prop.writeTy);
    }

    if (subTable->indexer)
    {
        unify_(subTable->indexer->indexType, builtinTypes->anyType);
        unify_(subTable->indexer->indexResultType, builtinTypes->anyType);
    }

    return UnifyResult::Ok;
}

UnifyResult Unifier2::unify_(const MetatableType* subMetatable, const AnyType*)
{
    UnifyResult metatableResult = unify_(subMetatable->metatable, builtinTypes->anyType);
    if (metatableResult != UnifyResult::Ok)
        return metatableResult;

    return unify_(subMetatable->table, builtinTypes->anyType);
}

UnifyResult Unifier2::unify_(const AnyType*, const MetatableType* superMetatable)
{
    UnifyResult metatableResult = unify_(builtinTypes->anyType, superMetatable->metatable);
    if (metatableResult != UnifyResult::Ok)
        return metatableResult;

    return unify_(builtinTypes->anyType, superMetatable->table);
}

// FIXME?  This should probably return an ErrorVec or an optional<TypeError>
// rather than a boolean to signal an occurs check failure.
UnifyResult Unifier2::unify_(TypePackId subTp, TypePackId superTp)
{
    if (FFlag::LuauLimitUnification)
    {
        if (FInt::LuauTypeInferIterationLimit > 0 && iterationCount >= FInt::LuauTypeInferIterationLimit)
            return UnifyResult::TooComplex;

        ++iterationCount;
    }

    subTp = follow(subTp);
    superTp = follow(superTp);

    if (auto subGen = genericPackSubstitutions.find(subTp))
        return unify_(*subGen, superTp);

    if (auto superGen = genericPackSubstitutions.find(superTp))
        return unify_(subTp, *superGen);

    if (seenTypePackPairings.contains({subTp, superTp}))
        return UnifyResult::Ok;
    seenTypePackPairings.insert({subTp, superTp});

    if (subTp == superTp)
        return UnifyResult::Ok;

    if (isIrresolvable(subTp) || isIrresolvable(superTp))
    {
        if (uninhabitedTypeFunctions && (uninhabitedTypeFunctions->contains(subTp) || uninhabitedTypeFunctions->contains(superTp)))
            return UnifyResult::Ok;

        if (FFlag::LuauEmplaceNotPushBack)
            incompleteSubtypes.emplace_back(PackSubtypeConstraint{subTp, superTp});
        else
            incompleteSubtypes.push_back(PackSubtypeConstraint{subTp, superTp});
        return UnifyResult::Ok;
    }

    const FreeTypePack* subFree = get<FreeTypePack>(subTp);
    const FreeTypePack* superFree = get<FreeTypePack>(superTp);

    if (subFree)
    {
        DenseHashSet<TypePackId> seen{nullptr};
        if (OccursCheckResult::Fail == occursCheck(seen, subTp, superTp))
        {
            emplaceTypePack<BoundTypePack>(asMutable(subTp), builtinTypes->errorTypePack);
            return UnifyResult::OccursCheckFailed;
        }

        emplaceTypePack<BoundTypePack>(asMutable(subTp), superTp);
        return UnifyResult::Ok;
    }

    if (superFree)
    {
        DenseHashSet<TypePackId> seen{nullptr};
        if (OccursCheckResult::Fail == occursCheck(seen, superTp, subTp))
        {
            emplaceTypePack<BoundTypePack>(asMutable(superTp), builtinTypes->errorTypePack);
            return UnifyResult::OccursCheckFailed;
        }

        emplaceTypePack<BoundTypePack>(asMutable(superTp), subTp);
        return UnifyResult::Ok;
    }

    size_t maxLength = std::max(flatten(subTp).first.size(), flatten(superTp).first.size());

    auto [subTypes, subTail] = extendTypePack(*arena, builtinTypes, subTp, maxLength);
    auto [superTypes, superTail] = extendTypePack(*arena, builtinTypes, superTp, maxLength);

    // right-pad the subpack with nils if `superPack` is larger since that's what a function call does
    if (FFlag::LuauFixNilRightPad)
    {
        if (subTypes.size() < maxLength)
            subTypes.resize(maxLength, builtinTypes->nilType);
    }
    else
    {
        if (subTypes.size() < maxLength)
        {
            for (size_t i = 0; i <= maxLength - subTypes.size(); i++)
                subTypes.push_back(builtinTypes->nilType);
        }
    }

    if (subTypes.size() < maxLength || superTypes.size() < maxLength)
        return UnifyResult::Ok;

    for (size_t i = 0; i < maxLength; ++i)
        unify_(subTypes[i], superTypes[i]);

    if (subTail && superTail)
    {
        TypePackId followedSubTail = follow(*subTail);
        TypePackId followedSuperTail = follow(*superTail);

        if (get<FreeTypePack>(followedSubTail) || get<FreeTypePack>(followedSuperTail))
            return unify_(followedSubTail, followedSuperTail);
    }
    else if (subTail)
    {
        TypePackId followedSubTail = follow(*subTail);
        if (get<FreeTypePack>(followedSubTail))
            emplaceTypePack<BoundTypePack>(asMutable(followedSubTail), builtinTypes->emptyTypePack);
    }
    else if (superTail)
    {
        TypePackId followedSuperTail = follow(*superTail);
        if (get<FreeTypePack>(followedSuperTail))
            emplaceTypePack<BoundTypePack>(asMutable(followedSuperTail), builtinTypes->emptyTypePack);
    }

    return UnifyResult::Ok;
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

OccursCheckResult Unifier2::occursCheck(DenseHashSet<TypeId>& seen, TypeId needle, TypeId haystack)
{
    RecursionLimiter _ra("Unifier2::occursCheck", &recursionCount, recursionLimit);

    OccursCheckResult occurrence = OccursCheckResult::Pass;

    auto check = [&](TypeId ty)
    {
        if (occursCheck(seen, needle, ty) == OccursCheckResult::Fail)
            occurrence = OccursCheckResult::Fail;
    };

    needle = follow(needle);
    haystack = follow(haystack);

    if (seen.find(haystack))
        return OccursCheckResult::Pass;

    seen.insert(haystack);

    if (get<ErrorType>(needle))
        return OccursCheckResult::Pass;

    if (!get<FreeType>(needle))
        ice->ice("Expected needle to be free");

    if (needle == haystack)
        return OccursCheckResult::Fail;

    if (auto haystackFree = get<FreeType>(haystack))
    {
        check(haystackFree->lowerBound);
        check(haystackFree->upperBound);
    }
    else if (auto ut = get<UnionType>(haystack))
    {
        for (TypeId ty : ut->options)
            check(ty);
    }
    else if (auto it = get<IntersectionType>(haystack))
    {
        for (TypeId ty : it->parts)
            check(ty);
    }

    return occurrence;
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

    RecursionLimiter _ra("Unifier2::occursCheck", &recursionCount, recursionLimit);

    while (!getMutable<ErrorTypePack>(haystack))
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

TypeId Unifier2::freshType(NotNull<Scope> scope, Polarity polarity)
{
    TypeId result = ::Luau::freshType(arena, builtinTypes, scope.get(), polarity);
    newFreshTypes.emplace_back(result);
    return result;
}

TypePackId Unifier2::freshTypePack(NotNull<Scope> scope, Polarity polarity)
{
    TypePackId result = arena->freshTypePack(scope.get());

    auto ftp = getMutable<FreeTypePack>(result);
    LUAU_ASSERT(ftp);
    ftp->polarity = polarity;

    newFreshTypePacks.emplace_back(result);
    return result;
}

} // namespace Luau
