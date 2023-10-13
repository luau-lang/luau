// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Subtyping.h"

#include "Luau/Common.h"
#include "Luau/Error.h"
#include "Luau/Normalize.h"
#include "Luau/Scope.h"
#include "Luau/StringUtils.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"

#include <algorithm>

namespace Luau
{

struct VarianceFlipper
{
    Subtyping::Variance* variance;
    Subtyping::Variance oldValue;

    VarianceFlipper(Subtyping::Variance* v)
        : variance(v)
        , oldValue(*v)
    {
        switch (oldValue)
        {
        case Subtyping::Variance::Covariant:
            *variance = Subtyping::Variance::Contravariant;
            break;
        case Subtyping::Variance::Contravariant:
            *variance = Subtyping::Variance::Covariant;
            break;
        }
    }

    ~VarianceFlipper()
    {
        *variance = oldValue;
    }
};

SubtypingResult& SubtypingResult::andAlso(const SubtypingResult& other)
{
    isSubtype &= other.isSubtype;
    // `|=` is intentional here, we want to preserve error related flags.
    isErrorSuppressing |= other.isErrorSuppressing;
    normalizationTooComplex |= other.normalizationTooComplex;
    isCacheable &= other.isCacheable;

    return *this;
}

SubtypingResult& SubtypingResult::orElse(const SubtypingResult& other)
{
    isSubtype |= other.isSubtype;
    isErrorSuppressing |= other.isErrorSuppressing;
    normalizationTooComplex |= other.normalizationTooComplex;
    isCacheable &= other.isCacheable;

    return *this;
}

SubtypingResult SubtypingResult::negate(const SubtypingResult& result)
{
    return SubtypingResult{
        !result.isSubtype,
        result.isErrorSuppressing,
        result.normalizationTooComplex,
    };
}

SubtypingResult SubtypingResult::all(const std::vector<SubtypingResult>& results)
{
    SubtypingResult acc{true, false};
    for (const SubtypingResult& current : results)
        acc.andAlso(current);
    return acc;
}

SubtypingResult SubtypingResult::any(const std::vector<SubtypingResult>& results)
{
    SubtypingResult acc{false, false};
    for (const SubtypingResult& current : results)
        acc.orElse(current);
    return acc;
}

Subtyping::Subtyping(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> typeArena, NotNull<Normalizer> normalizer,
    NotNull<InternalErrorReporter> iceReporter, NotNull<Scope> scope)
    : builtinTypes(builtinTypes)
    , arena(typeArena)
    , normalizer(normalizer)
    , iceReporter(iceReporter)
    , scope(scope)
{
}

SubtypingResult Subtyping::isSubtype(TypeId subTy, TypeId superTy)
{
    SubtypingEnvironment env;

    SubtypingResult result = isCovariantWith(env, subTy, superTy);

    for (const auto& [subTy, bounds] : env.mappedGenerics)
    {
        const auto& lb = bounds.lowerBound;
        const auto& ub = bounds.upperBound;

        TypeId lowerBound = makeAggregateType<UnionType>(lb, builtinTypes->neverType);
        TypeId upperBound = makeAggregateType<IntersectionType>(ub, builtinTypes->unknownType);

        const NormalizedType* nt = normalizer->normalize(upperBound);
        if (!nt)
            result.normalizationTooComplex = true;
        else if (!normalizer->isInhabited(nt))
        {
            /* If the normalized upper bound we're mapping to a generic is
             * uninhabited, then we must consider the subtyping relation not to
             * hold.
             *
             * This happens eg in <T>() -> (T, T) <: () -> (string, number)
             *
             * T appears in covariant position and would have to be both string
             * and number at once.
             *
             * No actual value is both a string and a number, so the test fails.
             *
             * TODO: We'll need to add explanitory context here.
             */
            result.isSubtype = false;
        }

        result.andAlso(isCovariantWith(env, lowerBound, upperBound));
    }

    /* TODO: We presently don't store subtype test results in the persistent
     * cache if the left-side type is a generic function.
     *
     * The implementation would be a bit tricky and we haven't seen any material
     * impact on benchmarks.
     *
     * What we would want to do is to remember points within the type where
     * mapped generics are introduced.  When all the contingent generics are
     * introduced at which we're doing the test, we can mark the result as
     * cacheable.
     */

    if (result.isCacheable)
        resultCache[{subTy, superTy}] = result;

    return result;
}

SubtypingResult Subtyping::isSubtype(TypePackId subTp, TypePackId superTp)
{
    SubtypingEnvironment env;
    return isCovariantWith(env, subTp, superTp);
}

SubtypingResult Subtyping::cache(SubtypingEnvironment& env, SubtypingResult result, TypeId subTy, TypeId superTy)
{
    const std::pair<TypeId, TypeId> p{subTy, superTy};
    if (result.isCacheable)
        resultCache[p] = result;
    else
        env.ephemeralCache[p] = result;

    return result;
}

namespace
{
struct SeenSetPopper
{
    Subtyping::SeenSet* seenTypes;
    std::pair<TypeId, TypeId> pair;

    SeenSetPopper(Subtyping::SeenSet* seenTypes, std::pair<TypeId, TypeId> pair)
        : seenTypes(seenTypes)
        , pair(pair)
    {
    }

    ~SeenSetPopper()
    {
        seenTypes->erase(pair);
    }
};
} // namespace

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, TypeId subTy, TypeId superTy)
{
    subTy = follow(subTy);
    superTy = follow(superTy);

    SubtypingResult* cachedResult = resultCache.find({subTy, superTy});
    if (cachedResult)
        return *cachedResult;

    cachedResult = env.ephemeralCache.find({subTy, superTy});
    if (cachedResult)
        return *cachedResult;

    // TODO: Do we care about returning a proof that this is error-suppressing?
    // e.g. given `a | error <: a | error` where both operands are pointer equal,
    // then should it also carry the information that it's error-suppressing?
    // If it should, then `error <: error` should also do the same.
    if (subTy == superTy)
        return {true};

    std::pair<TypeId, TypeId> typePair{subTy, superTy};
    if (!seenTypes.insert(typePair).second)
    {
        /* TODO: Caching results for recursive types is really tricky to think
         * about.
         *
         * We'd like to cache at the outermost level where we encounter the
         * recursive type, but we do not want to cache interior results that
         * involve the cycle.
         *
         * Presently, we stop at cycles and assume that the subtype check will
         * succeed because we'll eventually get there if it won't. However, if
         * that cyclic type turns out not to have the asked-for subtyping
         * relation, then all the intermediate cached results that were
         * contingent on that assumption need to be evicted from the cache, or
         * not entered into the cache, or something.
         *
         * For now, we do the conservative thing and refuse to cache anything
         * that touches a cycle.
         */
        SubtypingResult res;
        res.isSubtype = true;
        res.isCacheable = false;
        return res;
    }

    SeenSetPopper ssp{&seenTypes, typePair};

    // Within the scope to which a generic belongs, that generic should be
    // tested as though it were its upper bounds.  We do not yet support bounded
    // generics, so the upper bound is always unknown.
    if (auto subGeneric = get<GenericType>(subTy); subGeneric && subsumes(subGeneric->scope, scope))
        return isCovariantWith(env, builtinTypes->unknownType, superTy);
    if (auto superGeneric = get<GenericType>(superTy); superGeneric && subsumes(superGeneric->scope, scope))
        return isCovariantWith(env, subTy, builtinTypes->unknownType);

    SubtypingResult result;

    if (auto subUnion = get<UnionType>(subTy))
        result = isCovariantWith(env, subUnion, superTy);
    else if (auto superUnion = get<UnionType>(superTy))
        result = isCovariantWith(env, subTy, superUnion);
    else if (auto superIntersection = get<IntersectionType>(superTy))
        result = isCovariantWith(env, subTy, superIntersection);
    else if (auto subIntersection = get<IntersectionType>(subTy))
    {
        result = isCovariantWith(env, subIntersection, superTy);
        if (!result.isSubtype && !result.isErrorSuppressing && !result.normalizationTooComplex)
            result = isCovariantWith(env, normalizer->normalize(subTy), normalizer->normalize(superTy));
    }
    else if (get<AnyType>(superTy))
        result = {true};
    else if (get<AnyType>(subTy))
    {
        // any = unknown | error, so we rewrite this to match.
        // As per TAPL: A | B <: T iff A <: T && B <: T
        result = isCovariantWith(env, builtinTypes->unknownType, superTy).andAlso(isCovariantWith(env, builtinTypes->errorType, superTy));
    }
    else if (get<UnknownType>(superTy))
    {
        LUAU_ASSERT(!get<AnyType>(subTy));          // TODO: replace with ice.
        LUAU_ASSERT(!get<UnionType>(subTy));        // TODO: replace with ice.
        LUAU_ASSERT(!get<IntersectionType>(subTy)); // TODO: replace with ice.

        bool errorSuppressing = get<ErrorType>(subTy);
        result = {!errorSuppressing, errorSuppressing};
    }
    else if (get<NeverType>(subTy))
        result = {true};
    else if (get<ErrorType>(superTy))
        result = {false, true};
    else if (get<ErrorType>(subTy))
        result = {false, true};
    else if (auto p = get2<NegationType, NegationType>(subTy, superTy))
        result = isCovariantWith(env, p.first->ty, p.second->ty);
    else if (auto subNegation = get<NegationType>(subTy))
        result = isCovariantWith(env, subNegation, superTy);
    else if (auto superNegation = get<NegationType>(superTy))
        result = isCovariantWith(env, subTy, superNegation);
    else if (auto subGeneric = get<GenericType>(subTy); subGeneric && variance == Variance::Covariant)
    {
        bool ok = bindGeneric(env, subTy, superTy);
        result.isSubtype = ok;
        result.isCacheable = false;
    }
    else if (auto superGeneric = get<GenericType>(superTy); superGeneric && variance == Variance::Contravariant)
    {
        bool ok = bindGeneric(env, subTy, superTy);
        result.isSubtype = ok;
        result.isCacheable = false;
    }
    else if (auto p = get2<PrimitiveType, PrimitiveType>(subTy, superTy))
        result = isCovariantWith(env, p);
    else if (auto p = get2<SingletonType, PrimitiveType>(subTy, superTy))
        result = isCovariantWith(env, p);
    else if (auto p = get2<SingletonType, SingletonType>(subTy, superTy))
        result = isCovariantWith(env, p);
    else if (auto p = get2<FunctionType, FunctionType>(subTy, superTy))
        result = isCovariantWith(env, p);
    else if (auto p = get2<TableType, TableType>(subTy, superTy))
        result = isCovariantWith(env, p);
    else if (auto p = get2<MetatableType, MetatableType>(subTy, superTy))
        result = isCovariantWith(env, p);
    else if (auto p = get2<MetatableType, TableType>(subTy, superTy))
        result = isCovariantWith(env, p);
    else if (auto p = get2<ClassType, ClassType>(subTy, superTy))
        result = isCovariantWith(env, p);
    else if (auto p = get2<ClassType, TableType>(subTy, superTy))
        result = isCovariantWith(env, p);
    else if (auto p = get2<PrimitiveType, TableType>(subTy, superTy))
        result = isCovariantWith(env, p);
    else if (auto p = get2<SingletonType, TableType>(subTy, superTy))
        result = isCovariantWith(env, p);

    return cache(env, result, subTy, superTy);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, TypePackId subTp, TypePackId superTp)
{
    subTp = follow(subTp);
    superTp = follow(superTp);

    auto [subHead, subTail] = flatten(subTp);
    auto [superHead, superTail] = flatten(superTp);

    const size_t headSize = std::min(subHead.size(), superHead.size());

    std::vector<SubtypingResult> results;
    results.reserve(std::max(subHead.size(), superHead.size()) + 1);

    if (subTp == superTp)
        return {true};

    // Match head types pairwise

    for (size_t i = 0; i < headSize; ++i)
    {
        results.push_back(isCovariantWith(env, subHead[i], superHead[i]));
        if (!results.back().isSubtype)
            return {false};
    }

    // Handle mismatched head sizes

    if (subHead.size() < superHead.size())
    {
        if (subTail)
        {
            if (auto vt = get<VariadicTypePack>(*subTail))
            {
                for (size_t i = headSize; i < superHead.size(); ++i)
                    results.push_back(isCovariantWith(env, vt->ty, superHead[i]));
            }
            else if (auto gt = get<GenericTypePack>(*subTail))
            {
                if (variance == Variance::Covariant)
                {
                    // For any non-generic type T:
                    //
                    // <X>(X) -> () <: (T) -> ()

                    // Possible optimization: If headSize == 0 then we can just use subTp as-is.
                    std::vector<TypeId> headSlice(begin(superHead), begin(superHead) + headSize);
                    TypePackId superTailPack = arena->addTypePack(std::move(headSlice), superTail);

                    if (TypePackId* other = env.mappedGenericPacks.find(*subTail))
                        results.push_back(isCovariantWith(env, *other, superTailPack));
                    else
                        env.mappedGenericPacks.try_insert(*subTail, superTailPack);

                    // FIXME? Not a fan of the early return here.  It makes the
                    // control flow harder to reason about.
                    return SubtypingResult::all(results);
                }
                else
                {
                    // For any non-generic type T:
                    //
                    // (T) -> () </: <X>(X) -> ()
                    //
                    return {false};
                }
            }
            else
                unexpected(*subTail);
        }
        else
            return {false};
    }
    else if (subHead.size() > superHead.size())
    {
        if (superTail)
        {
            if (auto vt = get<VariadicTypePack>(*superTail))
            {
                for (size_t i = headSize; i < subHead.size(); ++i)
                    results.push_back(isCovariantWith(env, subHead[i], vt->ty));
            }
            else if (auto gt = get<GenericTypePack>(*superTail))
            {
                if (variance == Variance::Contravariant)
                {
                    // For any non-generic type T:
                    //
                    // <X...>(X...) -> () <: (T) -> ()

                    // Possible optimization: If headSize == 0 then we can just use subTp as-is.
                    std::vector<TypeId> headSlice(begin(subHead), begin(subHead) + headSize);
                    TypePackId subTailPack = arena->addTypePack(std::move(headSlice), subTail);

                    if (TypePackId* other = env.mappedGenericPacks.find(*superTail))
                        results.push_back(isCovariantWith(env, *other, subTailPack));
                    else
                        env.mappedGenericPacks.try_insert(*superTail, subTailPack);

                    // FIXME? Not a fan of the early return here.  It makes the
                    // control flow harder to reason about.
                    return SubtypingResult::all(results);
                }
                else
                {
                    // For any non-generic type T:
                    //
                    // () -> T </: <X...>() -> X...
                    return {false};
                }
            }
            else
                unexpected(*superTail);
        }
        else
            return {false};
    }

    // Handle tails

    if (subTail && superTail)
    {
        if (auto p = get2<VariadicTypePack, VariadicTypePack>(*subTail, *superTail))
        {
            results.push_back(isCovariantWith(env, p));
        }
        else if (auto p = get2<GenericTypePack, GenericTypePack>(*subTail, *superTail))
        {
            bool ok = bindGeneric(env, *subTail, *superTail);
            results.push_back({ok});
        }
        else if (get2<VariadicTypePack, GenericTypePack>(*subTail, *superTail))
        {
            if (variance == Variance::Contravariant)
            {
                // <A...>(A...) -> number <: (...number) -> number
                bool ok = bindGeneric(env, *subTail, *superTail);
                results.push_back({ok});
            }
            else
            {
                // (number) -> ...number </: <A...>(number) -> A...
                results.push_back({false});
            }
        }
        else if (get2<GenericTypePack, VariadicTypePack>(*subTail, *superTail))
        {
            if (variance == Variance::Contravariant)
            {
                // (...number) -> number </: <A...>(A...) -> number
                results.push_back({false});
            }
            else
            {
                // <A...>() -> A... <: () -> ...number
                bool ok = bindGeneric(env, *subTail, *superTail);
                results.push_back({ok});
            }
        }
        else
            iceReporter->ice(
                format("Subtyping::isSubtype got unexpected type packs %s and %s", toString(*subTail).c_str(), toString(*superTail).c_str()));
    }
    else if (subTail)
    {
        if (get<VariadicTypePack>(*subTail))
        {
            return {false};
        }
        else if (get<GenericTypePack>(*subTail))
        {
            bool ok = bindGeneric(env, *subTail, builtinTypes->emptyTypePack);
            return {ok};
        }
        else
            unexpected(*subTail);
    }
    else if (superTail)
    {
        if (get<VariadicTypePack>(*superTail))
        {
            /*
             * A variadic type pack ...T can be thought of as an infinite union of finite type packs.
             *     () | (T) | (T, T) | (T, T, T) | ...
             *
             * And, per TAPL:
             *     T <: A | B iff T <: A or T <: B
             *
             * All variadic type packs are therefore supertypes of the empty type pack.
             */
        }
        else if (get<GenericTypePack>(*superTail))
        {
            if (variance == Variance::Contravariant)
            {
                bool ok = bindGeneric(env, builtinTypes->emptyTypePack, *superTail);
                results.push_back({ok});
            }
            else
                results.push_back({false});
        }
        else
            iceReporter->ice("Subtyping test encountered the unexpected type pack: " + toString(*superTail));
    }

    return SubtypingResult::all(results);
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isContravariantWith(SubtypingEnvironment& env, SubTy&& subTy, SuperTy&& superTy)
{
    return isCovariantWith(env, superTy, subTy);
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isInvariantWith(SubtypingEnvironment& env, SubTy&& subTy, SuperTy&& superTy)
{
    return isCovariantWith(env, subTy, superTy).andAlso(isContravariantWith(env, subTy, superTy));
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair)
{
    return isCovariantWith(env, pair.first, pair.second);
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isContravariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair)
{
    return isCovariantWith(env, pair.second, pair.first);
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isInvariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair)
{
    return isCovariantWith(env, pair).andAlso(isContravariantWith(pair));
}

/*
 * This is much simpler than the Unifier implementation because we don't
 * actually care about potential "cross-talk" between union parts that match the
 * left side.
 *
 * In fact, we're very limited in what we can do: If multiple choices match, but
 * all of them have non-overlapping constraints, then we're stuck with an "or"
 * conjunction of constraints.  Solving this in the general case is quite
 * difficult.
 *
 * For example, we cannot dispatch anything from this constraint:
 *
 * {x: number, y: string} <: {x: number, y: 'a} | {x: 'b, y: string}
 *
 * From this constraint, we can know that either string <: 'a or number <: 'b,
 * but we don't know which!
 *
 * However:
 *
 * {x: number, y: string} <: {x: number, y: 'a} | {x: number, y: string}
 *
 * We can dispatch this constraint because there is no 'or' conjunction.  One of
 * the arms requires 0 matches.
 *
 * {x: number, y: string, z: boolean} | {x: number, y: 'a, z: 'b} | {x: number,
 * y: string, z: 'b}
 *
 * Here, we have two matches.  One asks for string ~ 'a and boolean ~ 'b.  The
 * other just asks for boolean ~ 'b. We can dispatch this and only commit
 * boolean ~ 'b.  This constraint does not teach us anything about 'a.
 */
SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, TypeId subTy, const UnionType* superUnion)
{
    // As per TAPL: T <: A | B iff T <: A || T <: B
    std::vector<SubtypingResult> subtypings;
    for (TypeId ty : superUnion)
        subtypings.push_back(isCovariantWith(env, subTy, ty));
    return SubtypingResult::any(subtypings);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const UnionType* subUnion, TypeId superTy)
{
    // As per TAPL: A | B <: T iff A <: T && B <: T
    std::vector<SubtypingResult> subtypings;
    for (TypeId ty : subUnion)
        subtypings.push_back(isCovariantWith(env, ty, superTy));
    return SubtypingResult::all(subtypings);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, TypeId subTy, const IntersectionType* superIntersection)
{
    // As per TAPL: T <: A & B iff T <: A && T <: B
    std::vector<SubtypingResult> subtypings;
    for (TypeId ty : superIntersection)
        subtypings.push_back(isCovariantWith(env, subTy, ty));
    return SubtypingResult::all(subtypings);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const IntersectionType* subIntersection, TypeId superTy)
{
    // As per TAPL: A & B <: T iff A <: T || B <: T
    std::vector<SubtypingResult> subtypings;
    for (TypeId ty : subIntersection)
        subtypings.push_back(isCovariantWith(env, ty, superTy));
    return SubtypingResult::any(subtypings);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const NegationType* subNegation, TypeId superTy)
{
    TypeId negatedTy = follow(subNegation->ty);

    // In order to follow a consistent codepath, rather than folding the
    // isCovariantWith test down to its conclusion here, we test the subtyping test
    // of the result of negating the type for never, unknown, any, and error.
    if (is<NeverType>(negatedTy))
    {
        // ¬never ~ unknown
        return isCovariantWith(env, builtinTypes->unknownType, superTy);
    }
    else if (is<UnknownType>(negatedTy))
    {
        // ¬unknown ~ never
        return isCovariantWith(env, builtinTypes->neverType, superTy);
    }
    else if (is<AnyType>(negatedTy))
    {
        // ¬any ~ any
        return isCovariantWith(env, negatedTy, superTy);
    }
    else if (auto u = get<UnionType>(negatedTy))
    {
        // ¬(A ∪ B) ~ ¬A ∩ ¬B
        // follow intersection rules: A & B <: T iff A <: T && B <: T
        std::vector<SubtypingResult> subtypings;

        for (TypeId ty : u)
        {
            NegationType negatedTmp{ty};
            subtypings.push_back(isCovariantWith(env, &negatedTmp, superTy));
        }

        return SubtypingResult::all(subtypings);
    }
    else if (auto i = get<IntersectionType>(negatedTy))
    {
        // ¬(A ∩ B) ~ ¬A ∪ ¬B
        // follow union rules: A | B <: T iff A <: T || B <: T
        std::vector<SubtypingResult> subtypings;

        for (TypeId ty : i)
        {
            if (auto negatedPart = get<NegationType>(follow(ty)))
                subtypings.push_back(isCovariantWith(env, negatedPart->ty, superTy));
            else
            {
                NegationType negatedTmp{ty};
                subtypings.push_back(isCovariantWith(env, &negatedTmp, superTy));
            }
        }

        return SubtypingResult::any(subtypings);
    }
    else if (is<ErrorType, FunctionType, TableType, MetatableType>(negatedTy))
    {
        iceReporter->ice("attempting to negate a non-testable type");
    }
    // negating a different subtype will get you a very wide type that's not a
    // subtype of other stuff.
    else
    {
        return {false};
    }
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TypeId subTy, const NegationType* superNegation)
{
    TypeId negatedTy = follow(superNegation->ty);

    if (is<NeverType>(negatedTy))
    {
        // ¬never ~ unknown
        return isCovariantWith(env, subTy, builtinTypes->unknownType);
    }
    else if (is<UnknownType>(negatedTy))
    {
        // ¬unknown ~ never
        return isCovariantWith(env, subTy, builtinTypes->neverType);
    }
    else if (is<AnyType>(negatedTy))
    {
        // ¬any ~ any
        return isSubtype(subTy, negatedTy);
    }
    else if (auto u = get<UnionType>(negatedTy))
    {
        // ¬(A ∪ B) ~ ¬A ∩ ¬B
        // follow intersection rules: A & B <: T iff A <: T && B <: T
        std::vector<SubtypingResult> subtypings;

        for (TypeId ty : u)
        {
            if (auto negatedPart = get<NegationType>(follow(ty)))
                subtypings.push_back(isCovariantWith(env, subTy, negatedPart->ty));
            else
            {
                NegationType negatedTmp{ty};
                subtypings.push_back(isCovariantWith(env, subTy, &negatedTmp));
            }
        }

        return SubtypingResult::all(subtypings);
    }
    else if (auto i = get<IntersectionType>(negatedTy))
    {
        // ¬(A ∩ B) ~ ¬A ∪ ¬B
        // follow union rules: A | B <: T iff A <: T || B <: T
        std::vector<SubtypingResult> subtypings;

        for (TypeId ty : i)
        {
            if (auto negatedPart = get<NegationType>(follow(ty)))
                subtypings.push_back(isCovariantWith(env, subTy, negatedPart->ty));
            else
            {
                NegationType negatedTmp{ty};
                subtypings.push_back(isCovariantWith(env, subTy, &negatedTmp));
            }
        }

        return SubtypingResult::any(subtypings);
    }
    else if (auto p = get2<PrimitiveType, PrimitiveType>(subTy, negatedTy))
    {
        // number <: ¬boolean
        // number </: ¬number
        return {p.first->type != p.second->type};
    }
    else if (auto p = get2<SingletonType, PrimitiveType>(subTy, negatedTy))
    {
        // "foo" </: ¬string
        if (get<StringSingleton>(p.first) && p.second->type == PrimitiveType::String)
            return {false};
        // false </: ¬boolean
        else if (get<BooleanSingleton>(p.first) && p.second->type == PrimitiveType::Boolean)
            return {false};
        // other cases are true
        else
            return {true};
    }
    else if (auto p = get2<PrimitiveType, SingletonType>(subTy, negatedTy))
    {
        if (p.first->type == PrimitiveType::String && get<StringSingleton>(p.second))
            return {false};
        else if (p.first->type == PrimitiveType::Boolean && get<BooleanSingleton>(p.second))
            return {false};
        else
            return {true};
    }
    // the top class type is not actually a primitive type, so the negation of
    // any one of them includes the top class type.
    else if (auto p = get2<ClassType, PrimitiveType>(subTy, negatedTy))
        return {true};
    else if (auto p = get<PrimitiveType>(negatedTy); p && is<TableType, MetatableType>(subTy))
        return {p->type != PrimitiveType::Table};
    else if (auto p = get2<FunctionType, PrimitiveType>(subTy, negatedTy))
        return {p.second->type != PrimitiveType::Function};
    else if (auto p = get2<SingletonType, SingletonType>(subTy, negatedTy))
        return {*p.first != *p.second};
    else if (auto p = get2<ClassType, ClassType>(subTy, negatedTy))
        return SubtypingResult::negate(isCovariantWith(env, p.first, p.second));
    else if (get2<FunctionType, ClassType>(subTy, negatedTy))
        return {true};
    else if (is<ErrorType, FunctionType, TableType, MetatableType>(negatedTy))
        iceReporter->ice("attempting to negate a non-testable type");

    return {false};
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const PrimitiveType* subPrim, const PrimitiveType* superPrim)
{
    return {subPrim->type == superPrim->type};
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const SingletonType* subSingleton, const PrimitiveType* superPrim)
{
    if (get<StringSingleton>(subSingleton) && superPrim->type == PrimitiveType::String)
        return {true};
    else if (get<BooleanSingleton>(subSingleton) && superPrim->type == PrimitiveType::Boolean)
        return {true};
    else
        return {false};
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const SingletonType* subSingleton, const SingletonType* superSingleton)
{
    return {*subSingleton == *superSingleton};
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TableType* subTable, const TableType* superTable)
{
    SubtypingResult result{true};

    if (subTable->props.empty() && !subTable->indexer && superTable->indexer)
        return {false};

    for (const auto& [name, prop] : superTable->props)
    {
        std::vector<SubtypingResult> results;
        if (auto it = subTable->props.find(name); it != subTable->props.end())
            results.push_back(isInvariantWith(env, it->second.type(), prop.type()));

        if (subTable->indexer)
        {
            if (isInvariantWith(env, subTable->indexer->indexType, builtinTypes->stringType).isSubtype)
                results.push_back(isInvariantWith(env, subTable->indexer->indexResultType, prop.type()));
        }

        if (results.empty())
            return {false};

        result.andAlso(SubtypingResult::all(results));
    }

    if (superTable->indexer)
    {
        if (subTable->indexer)
            result.andAlso(isInvariantWith(env, *subTable->indexer, *superTable->indexer));
        else
            return {false};
    }

    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const MetatableType* subMt, const MetatableType* superMt)
{
    return isCovariantWith(env, subMt->table, superMt->table).andAlso(isCovariantWith(env, subMt->metatable, superMt->metatable));
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const MetatableType* subMt, const TableType* superTable)
{
    if (auto subTable = get<TableType>(subMt->table))
    {
        // Metatables cannot erase properties from the table they're attached to, so
        // the subtyping rule for this is just if the table component is a subtype
        // of the supertype table.
        //
        // There's a flaw here in that if the __index metamethod contributes a new
        // field that would satisfy the subtyping relationship, we'll erronously say
        // that the metatable isn't a subtype of the table, even though they have
        // compatible properties/shapes. We'll revisit this later when we have a
        // better understanding of how important this is.
        return isCovariantWith(env, subTable, superTable);
    }
    else
    {
        // TODO: This may be a case we actually hit?
        return {false};
    }
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const ClassType* subClass, const ClassType* superClass)
{
    return {isSubclass(subClass, superClass)};
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const ClassType* subClass, const TableType* superTable)
{
    SubtypingResult result{true};

    for (const auto& [name, prop] : superTable->props)
    {
        if (auto classProp = lookupClassProp(subClass, name))
            result.andAlso(isInvariantWith(env, prop.type(), classProp->type()));
        else
            return SubtypingResult{false};
    }

    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const FunctionType* subFunction, const FunctionType* superFunction)
{
    SubtypingResult result;
    {
        VarianceFlipper vf{&variance};
        result.orElse(isContravariantWith(env, subFunction->argTypes, superFunction->argTypes));
    }

    result.andAlso(isCovariantWith(env, subFunction->retTypes, superFunction->retTypes));

    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const PrimitiveType* subPrim, const TableType* superTable)
{
    SubtypingResult result{false};
    if (subPrim->type == PrimitiveType::String)
    {
        if (auto metatable = getMetatable(builtinTypes->stringType, builtinTypes))
        {
            if (auto mttv = get<TableType>(follow(metatable)))
            {
                if (auto it = mttv->props.find("__index"); it != mttv->props.end())
                {
                    if (auto stringTable = get<TableType>(it->second.type()))
                        result.orElse(isCovariantWith(env, stringTable, superTable));
                }
            }
        }
    }

    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const SingletonType* subSingleton, const TableType* superTable)
{
    SubtypingResult result{false};
    if (auto stringleton = get<StringSingleton>(subSingleton))
    {
        if (auto metatable = getMetatable(builtinTypes->stringType, builtinTypes))
        {
            if (auto mttv = get<TableType>(follow(metatable)))
            {
                if (auto it = mttv->props.find("__index"); it != mttv->props.end())
                {
                    if (auto stringTable = get<TableType>(it->second.type()))
                        result.orElse(isCovariantWith(env, stringTable, superTable));
                }
            }
        }
    }
    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TableIndexer& subIndexer, const TableIndexer& superIndexer)
{
    return isInvariantWith(env, subIndexer.indexType, superIndexer.indexType)
        .andAlso(isInvariantWith(env, superIndexer.indexResultType, subIndexer.indexResultType));
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const NormalizedType* subNorm, const NormalizedType* superNorm)
{
    if (!subNorm || !superNorm)
        return {false, true, true};

    SubtypingResult result = isCovariantWith(env, subNorm->tops, superNorm->tops);
    result.andAlso(isCovariantWith(env, subNorm->booleans, superNorm->booleans));
    result.andAlso(isCovariantWith(env, subNorm->classes, superNorm->classes).orElse(isCovariantWith(env, subNorm->classes, superNorm->tables)));
    result.andAlso(isCovariantWith(env, subNorm->errors, superNorm->errors));
    result.andAlso(isCovariantWith(env, subNorm->nils, superNorm->nils));
    result.andAlso(isCovariantWith(env, subNorm->numbers, superNorm->numbers));
    result.andAlso(isCovariantWith(env, subNorm->strings, superNorm->strings));
    result.andAlso(isCovariantWith(env, subNorm->strings, superNorm->tables));
    result.andAlso(isCovariantWith(env, subNorm->threads, superNorm->threads));
    result.andAlso(isCovariantWith(env, subNorm->tables, superNorm->tables));
    result.andAlso(isCovariantWith(env, subNorm->functions, superNorm->functions));
    // isCovariantWith(subNorm->tyvars, superNorm->tyvars);
    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const NormalizedClassType& subClass, const NormalizedClassType& superClass)
{
    for (const auto& [subClassTy, _] : subClass.classes)
    {
        SubtypingResult result;

        for (const auto& [superClassTy, superNegations] : superClass.classes)
        {
            result.orElse(isCovariantWith(env, subClassTy, superClassTy));
            if (!result.isSubtype)
                continue;

            for (TypeId negation : superNegations)
            {
                result.andAlso(SubtypingResult::negate(isCovariantWith(env, subClassTy, negation)));
                if (result.isSubtype)
                    break;
            }
        }

        if (!result.isSubtype)
            return result;
    }

    return {true};
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const NormalizedClassType& subClass, const TypeIds& superTables)
{
    for (const auto& [subClassTy, _] : subClass.classes)
    {
        SubtypingResult result;

        for (TypeId superTableTy : superTables)
            result.orElse(isCovariantWith(env, subClassTy, superTableTy));

        if (!result.isSubtype)
            return result;
    }

    return {true};
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const NormalizedStringType& subString, const NormalizedStringType& superString)
{
    bool isSubtype = Luau::isSubtype(subString, superString);
    return {isSubtype};
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const NormalizedStringType& subString, const TypeIds& superTables)
{
    if (subString.isNever())
        return {true};

    if (subString.isCofinite)
    {
        SubtypingResult result;
        for (const auto& superTable : superTables)
        {
            result.orElse(isCovariantWith(env, builtinTypes->stringType, superTable));
            if (result.isSubtype)
                return result;
        }
        return result;
    }

    // Finite case
    // S = s1 | s2 | s3 ... sn <: t1 | t2 | ... | tn
    // iff for some ti, S <: ti
    // iff for all sj, sj <: ti
    for (const auto& superTable : superTables)
    {
        SubtypingResult result{true};
        for (const auto& [_, subString] : subString.singletons)
        {
            result.andAlso(isCovariantWith(env, subString, superTable));
            if (!result.isSubtype)
                break;
        }

        if (!result.isSubtype)
            continue;
        else
            return result;
    }

    return {false};
}

SubtypingResult Subtyping::isCovariantWith(
    SubtypingEnvironment& env, const NormalizedFunctionType& subFunction, const NormalizedFunctionType& superFunction)
{
    if (subFunction.isNever())
        return {true};
    else if (superFunction.isTop)
        return {true};
    else
        return isCovariantWith(env, subFunction.parts, superFunction.parts);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TypeIds& subTypes, const TypeIds& superTypes)
{
    std::vector<SubtypingResult> results;

    for (TypeId subTy : subTypes)
    {
        results.emplace_back();
        for (TypeId superTy : superTypes)
            results.back().orElse(isCovariantWith(env, subTy, superTy));
    }

    return SubtypingResult::all(results);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const VariadicTypePack* subVariadic, const VariadicTypePack* superVariadic)
{
    return isCovariantWith(env, subVariadic->ty, superVariadic->ty);
}

bool Subtyping::bindGeneric(SubtypingEnvironment& env, TypeId subTy, TypeId superTy)
{
    if (variance == Variance::Covariant)
    {
        if (!get<GenericType>(subTy))
            return false;

        env.mappedGenerics[subTy].upperBound.insert(superTy);
    }
    else
    {
        if (!get<GenericType>(superTy))
            return false;

        env.mappedGenerics[superTy].lowerBound.insert(subTy);
    }

    return true;
}

/*
 * If, when performing a subtyping test, we encounter a generic on the left
 * side, it is permissible to tentatively bind that generic to the right side
 * type.
 */
bool Subtyping::bindGeneric(SubtypingEnvironment& env, TypePackId subTp, TypePackId superTp)
{
    if (variance == Variance::Contravariant)
        std::swap(superTp, subTp);

    if (!get<GenericTypePack>(subTp))
        return false;

    if (TypePackId* m = env.mappedGenericPacks.find(subTp))
        return *m == superTp;

    env.mappedGenericPacks[subTp] = superTp;

    return true;
}

template<typename T, typename Container>
TypeId Subtyping::makeAggregateType(const Container& container, TypeId orElse)
{
    if (container.empty())
        return orElse;
    else if (container.size() == 1)
        return *begin(container);
    else
        return arena->addType(T{std::vector<TypeId>(begin(container), end(container))});
}

void Subtyping::unexpected(TypePackId tp)
{
    iceReporter->ice(format("Unexpected type pack %s", toString(tp).c_str()));
}

} // namespace Luau
