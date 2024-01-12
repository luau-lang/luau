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
#include "Luau/TypePath.h"
#include "Luau/TypeUtils.h"

#include <algorithm>

LUAU_FASTFLAGVARIABLE(DebugLuauSubtypingCheckPathValidity, false);

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

bool SubtypingReasoning::operator==(const SubtypingReasoning& other) const
{
    return subPath == other.subPath && superPath == other.superPath && variance == other.variance;
}

size_t SubtypingReasoningHash::operator()(const SubtypingReasoning& r) const
{
    return TypePath::PathHash()(r.subPath) ^ (TypePath::PathHash()(r.superPath) << 1) ^ (static_cast<size_t>(r.variance) << 1);
}

template<typename TID>
static void assertReasoningValid(TID subTy, TID superTy, const SubtypingResult& result, NotNull<BuiltinTypes> builtinTypes)
{
    if (!FFlag::DebugLuauSubtypingCheckPathValidity)
        return;

    for (const SubtypingReasoning& reasoning : result.reasoning)
    {
        LUAU_ASSERT(traverse(subTy, reasoning.subPath, builtinTypes));
        LUAU_ASSERT(traverse(superTy, reasoning.superPath, builtinTypes));
    }
}

template<>
void assertReasoningValid<TableIndexer>(TableIndexer subIdx, TableIndexer superIdx, const SubtypingResult& result, NotNull<BuiltinTypes> builtinTypes)
{
    // Empty method to satisfy the compiler.
}

static SubtypingReasonings mergeReasonings(const SubtypingReasonings& a, const SubtypingReasonings& b)
{
    SubtypingReasonings result{kEmptyReasoning};

    for (const SubtypingReasoning& r : a)
    {
        if (r.variance == SubtypingVariance::Invariant)
            result.insert(r);
        else if (r.variance == SubtypingVariance::Covariant || r.variance == SubtypingVariance::Contravariant)
        {
            SubtypingReasoning inverseReasoning = SubtypingReasoning{
                r.subPath, r.superPath, r.variance == SubtypingVariance::Covariant ? SubtypingVariance::Contravariant : SubtypingVariance::Covariant};
            if (b.contains(inverseReasoning))
                result.insert(SubtypingReasoning{r.subPath, r.superPath, SubtypingVariance::Invariant});
            else
                result.insert(r);
        }
    }

    for (const SubtypingReasoning& r : b)
    {
        if (r.variance == SubtypingVariance::Invariant)
            result.insert(r);
        else if (r.variance == SubtypingVariance::Covariant || r.variance == SubtypingVariance::Contravariant)
        {
            SubtypingReasoning inverseReasoning = SubtypingReasoning{
                r.subPath, r.superPath, r.variance == SubtypingVariance::Covariant ? SubtypingVariance::Contravariant : SubtypingVariance::Covariant};
            if (a.contains(inverseReasoning))
                result.insert(SubtypingReasoning{r.subPath, r.superPath, SubtypingVariance::Invariant});
            else
                result.insert(r);
        }
    }

    return result;
}

SubtypingResult& SubtypingResult::andAlso(const SubtypingResult& other)
{
    // If the other result is not a subtype, we want to join all of its
    // reasonings to this one. If this result already has reasonings of its own,
    // those need to be attributed here.
    if (!other.isSubtype)
        reasoning = mergeReasonings(reasoning, other.reasoning);

    isSubtype &= other.isSubtype;
    // `|=` is intentional here, we want to preserve error related flags.
    isErrorSuppressing |= other.isErrorSuppressing;
    normalizationTooComplex |= other.normalizationTooComplex;
    isCacheable &= other.isCacheable;

    return *this;
}

SubtypingResult& SubtypingResult::orElse(const SubtypingResult& other)
{
    // If this result is a subtype, we do not join the reasoning lists. If this
    // result is not a subtype, but the other is a subtype, we want to _clear_
    // our reasoning list. If both results are not subtypes, we join the
    // reasoning lists.
    if (!isSubtype)
    {
        if (other.isSubtype)
            reasoning.clear();
        else
            reasoning = mergeReasonings(reasoning, other.reasoning);
    }

    isSubtype |= other.isSubtype;
    isErrorSuppressing |= other.isErrorSuppressing;
    normalizationTooComplex |= other.normalizationTooComplex;
    isCacheable &= other.isCacheable;

    return *this;
}

SubtypingResult& SubtypingResult::withBothComponent(TypePath::Component component)
{
    return withSubComponent(component).withSuperComponent(component);
}

SubtypingResult& SubtypingResult::withSubComponent(TypePath::Component component)
{
    if (reasoning.empty())
        reasoning.insert(SubtypingReasoning{Path(component), TypePath::kEmpty});
    else
    {
        for (auto& r : reasoning)
            r.subPath = r.subPath.push_front(component);
    }

    return *this;
}

SubtypingResult& SubtypingResult::withSuperComponent(TypePath::Component component)
{
    if (reasoning.empty())
        reasoning.insert(SubtypingReasoning{TypePath::kEmpty, Path(component)});
    else
    {
        for (auto& r : reasoning)
            r.superPath = r.superPath.push_front(component);
    }

    return *this;
}

SubtypingResult& SubtypingResult::withBothPath(TypePath::Path path)
{
    return withSubPath(path).withSuperPath(path);
}

SubtypingResult& SubtypingResult::withSubPath(TypePath::Path path)
{
    if (reasoning.empty())
        reasoning.insert(SubtypingReasoning{path, TypePath::kEmpty});
    else
    {
        for (auto& r : reasoning)
            r.subPath = path.append(r.subPath);
    }

    return *this;
}

SubtypingResult& SubtypingResult::withSuperPath(TypePath::Path path)
{
    if (reasoning.empty())
        reasoning.insert(SubtypingReasoning{TypePath::kEmpty, path});
    else
    {
        for (auto& r : reasoning)
            r.superPath = path.append(r.superPath);
    }

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

        SubtypingResult boundsResult = isCovariantWith(env, lowerBound, upperBound);
        boundsResult.reasoning.clear();

        result.andAlso(boundsResult);
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
    if (!seenTypes.insert(typePair))
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
    {
        result = isCovariantWith(env, subTy, superUnion);
        if (!result.isSubtype && !result.isErrorSuppressing && !result.normalizationTooComplex)
        {
            SubtypingResult semantic = isCovariantWith(env, normalizer->normalize(subTy), normalizer->normalize(superTy));
            if (semantic.isSubtype)
            {
                semantic.reasoning.clear();
                result = semantic;
            }
        }
    }
    else if (auto superIntersection = get<IntersectionType>(superTy))
        result = isCovariantWith(env, subTy, superIntersection);
    else if (auto subIntersection = get<IntersectionType>(subTy))
    {
        result = isCovariantWith(env, subIntersection, superTy);
        if (!result.isSubtype && !result.isErrorSuppressing && !result.normalizationTooComplex)
        {
            SubtypingResult semantic = isCovariantWith(env, normalizer->normalize(subTy), normalizer->normalize(superTy));
            if (semantic.isSubtype)
            {
                // Clear the semantic reasoning, as any reasonings within
                // potentially contain invalid paths.
                semantic.reasoning.clear();
                result = semantic;
            }
        }
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
        result = isCovariantWith(env, p.first->ty, p.second->ty).withBothComponent(TypePath::TypeField::Negated);
    else if (auto subNegation = get<NegationType>(subTy))
    {
        result = isCovariantWith(env, subNegation, superTy);
        if (!result.isSubtype && !result.isErrorSuppressing && !result.normalizationTooComplex)
        {
            SubtypingResult semantic = isCovariantWith(env, normalizer->normalize(subTy), normalizer->normalize(superTy));
            if (semantic.isSubtype)
            {
                semantic.reasoning.clear();
                result = semantic;
            }
        }
    }
    else if (auto superNegation = get<NegationType>(superTy))
    {
        result = isCovariantWith(env, subTy, superNegation);
        if (!result.isSubtype && !result.isErrorSuppressing && !result.normalizationTooComplex)
        {
            SubtypingResult semantic = isCovariantWith(env, normalizer->normalize(subTy), normalizer->normalize(superTy));
            if (semantic.isSubtype)
            {
                semantic.reasoning.clear();
                result = semantic;
            }
        }
    }
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

    assertReasoningValid(subTy, superTy, result, builtinTypes);

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
        results.push_back(isCovariantWith(env, subHead[i], superHead[i]).withBothComponent(TypePath::Index{i}));

    // Handle mismatched head sizes

    if (subHead.size() < superHead.size())
    {
        if (subTail)
        {
            if (auto vt = get<VariadicTypePack>(*subTail))
            {
                for (size_t i = headSize; i < superHead.size(); ++i)
                    results.push_back(isCovariantWith(env, vt->ty, superHead[i])
                                          .withSubComponent(TypePath::TypeField::Variadic)
                                          .withSuperComponent(TypePath::Index{i}));
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
                        // TODO: TypePath can't express "slice of a pack + its tail".
                        results.push_back(isCovariantWith(env, *other, superTailPack).withSubComponent(TypePath::PackField::Tail));
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
                    return SubtypingResult{false}.withSubComponent(TypePath::PackField::Tail);
                }
            }
            else
                unexpected(*subTail);
        }
        else
        {
            results.push_back({false});
            return SubtypingResult::all(results);
        }
    }
    else if (subHead.size() > superHead.size())
    {
        if (superTail)
        {
            if (auto vt = get<VariadicTypePack>(*superTail))
            {
                for (size_t i = headSize; i < subHead.size(); ++i)
                    results.push_back(isCovariantWith(env, subHead[i], vt->ty)
                                          .withSubComponent(TypePath::Index{i})
                                          .withSuperPath(TypePath::PathBuilder().tail().variadic().build()));
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
                        // TODO: TypePath can't express "slice of a pack + its tail".
                        results.push_back(isCovariantWith(env, *other, subTailPack).withSuperComponent(TypePath::PackField::Tail));
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
                    return SubtypingResult{false}.withSuperComponent(TypePath::PackField::Tail);
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
            // Variadic component is added by the isCovariantWith
            // implementation; no need to add it here.
            results.push_back(isCovariantWith(env, p).withBothComponent(TypePath::PackField::Tail));
        }
        else if (auto p = get2<GenericTypePack, GenericTypePack>(*subTail, *superTail))
        {
            bool ok = bindGeneric(env, *subTail, *superTail);
            results.push_back(SubtypingResult{ok}.withBothComponent(TypePath::PackField::Tail));
        }
        else if (auto p = get2<VariadicTypePack, GenericTypePack>(*subTail, *superTail))
        {
            if (variance == Variance::Contravariant)
            {
                // <A...>(A...) -> number <: (...number) -> number
                bool ok = bindGeneric(env, *subTail, *superTail);
                results.push_back(SubtypingResult{ok}.withBothComponent(TypePath::PackField::Tail));
            }
            else
            {
                // (number) -> ...number </: <A...>(number) -> A...
                results.push_back(SubtypingResult{false}.withBothComponent(TypePath::PackField::Tail));
            }
        }
        else if (auto p = get2<GenericTypePack, VariadicTypePack>(*subTail, *superTail))
        {
            if (TypeId t = follow(p.second->ty); get<AnyType>(t) || get<UnknownType>(t))
            {
                // Extra magic rule:
                // T... <: ...any
                // T... <: ...unknown
                //
                // See https://github.com/luau-lang/luau/issues/767
            }
            else if (variance == Variance::Contravariant)
            {
                // (...number) -> number </: <A...>(A...) -> number
                results.push_back(SubtypingResult{false}.withBothComponent(TypePath::PackField::Tail));
            }
            else
            {
                // <A...>() -> A... <: () -> ...number
                bool ok = bindGeneric(env, *subTail, *superTail);
                results.push_back(SubtypingResult{ok}.withBothComponent(TypePath::PackField::Tail));
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
            return SubtypingResult{false}.withSubComponent(TypePath::PackField::Tail);
        }
        else if (get<GenericTypePack>(*subTail))
        {
            bool ok = bindGeneric(env, *subTail, builtinTypes->emptyTypePack);
            return SubtypingResult{ok}.withSubComponent(TypePath::PackField::Tail);
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
                results.push_back(SubtypingResult{ok}.withSuperComponent(TypePath::PackField::Tail));
            }
            else
                results.push_back(SubtypingResult{false}.withSuperComponent(TypePath::PackField::Tail));
        }
        else
            iceReporter->ice("Subtyping test encountered the unexpected type pack: " + toString(*superTail));
    }

    SubtypingResult result = SubtypingResult::all(results);
    assertReasoningValid(subTp, superTp, result, builtinTypes);

    return result;
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isContravariantWith(SubtypingEnvironment& env, SubTy&& subTy, SuperTy&& superTy)
{
    VarianceFlipper vf{&variance};

    SubtypingResult result = isCovariantWith(env, superTy, subTy);
    if (result.reasoning.empty())
        result.reasoning.insert(SubtypingReasoning{TypePath::kEmpty, TypePath::kEmpty, SubtypingVariance::Contravariant});
    else
    {
        // If we don't swap the paths here, we will end up producing an invalid path
        // whenever we involve contravariance. We'll end up appending path
        // components that should belong to the supertype to the subtype, and vice
        // versa.
        for (auto& reasoning : result.reasoning)
        {
            std::swap(reasoning.subPath, reasoning.superPath);

            // Also swap covariant/contravariant, since those are also the other way
            // around.
            if (reasoning.variance == SubtypingVariance::Covariant)
                reasoning.variance = SubtypingVariance::Contravariant;
            else if (reasoning.variance == SubtypingVariance::Contravariant)
                reasoning.variance = SubtypingVariance::Covariant;
        }
    }

    assertReasoningValid(subTy, superTy, result, builtinTypes);

    return result;
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isInvariantWith(SubtypingEnvironment& env, SubTy&& subTy, SuperTy&& superTy)
{
    SubtypingResult result = isCovariantWith(env, subTy, superTy).andAlso(isContravariantWith(env, subTy, superTy));

    if (result.reasoning.empty())
        result.reasoning.insert(SubtypingReasoning{TypePath::kEmpty, TypePath::kEmpty, SubtypingVariance::Invariant});
    else
    {
        for (auto& reasoning : result.reasoning)
            reasoning.variance = SubtypingVariance::Invariant;
    }

    assertReasoningValid(subTy, superTy, result, builtinTypes);
    return result;
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair)
{
    return isCovariantWith(env, pair.first, pair.second);
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isContravariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair)
{
    return isContravariantWith(env, pair.first, pair.second);
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isInvariantWith(SubtypingEnvironment& env, const TryPair<const SubTy*, const SuperTy*>& pair)
{
    return isInvariantWith(env, pair.first, pair.second);
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

    for (TypeId ty : superUnion)
    {
        SubtypingResult next = isCovariantWith(env, subTy, ty);
        if (next.isSubtype)
            return SubtypingResult{true};
    }

    /*
     * TODO: Is it possible here to use the context produced by the above
     * isCovariantWith() calls to produce a richer, more helpful result in the
     * case that the subtyping relation does not hold?
     */
    return SubtypingResult{false};
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const UnionType* subUnion, TypeId superTy)
{
    // As per TAPL: A | B <: T iff A <: T && B <: T
    std::vector<SubtypingResult> subtypings;
    size_t i = 0;
    for (TypeId ty : subUnion)
        subtypings.push_back(isCovariantWith(env, ty, superTy).withSubComponent(TypePath::Index{i++}));
    return SubtypingResult::all(subtypings);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, TypeId subTy, const IntersectionType* superIntersection)
{
    // As per TAPL: T <: A & B iff T <: A && T <: B
    std::vector<SubtypingResult> subtypings;
    size_t i = 0;
    for (TypeId ty : superIntersection)
        subtypings.push_back(isCovariantWith(env, subTy, ty).withSuperComponent(TypePath::Index{i++}));
    return SubtypingResult::all(subtypings);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const IntersectionType* subIntersection, TypeId superTy)
{
    // As per TAPL: A & B <: T iff A <: T || B <: T
    std::vector<SubtypingResult> subtypings;
    size_t i = 0;
    for (TypeId ty : subIntersection)
        subtypings.push_back(isCovariantWith(env, ty, superTy).withSubComponent(TypePath::Index{i++}));
    return SubtypingResult::any(subtypings);
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const NegationType* subNegation, TypeId superTy)
{
    TypeId negatedTy = follow(subNegation->ty);

    SubtypingResult result;

    // In order to follow a consistent codepath, rather than folding the
    // isCovariantWith test down to its conclusion here, we test the subtyping test
    // of the result of negating the type for never, unknown, any, and error.
    if (is<NeverType>(negatedTy))
    {
        // ¬never ~ unknown
        result = isCovariantWith(env, builtinTypes->unknownType, superTy).withSubComponent(TypePath::TypeField::Negated);
    }
    else if (is<UnknownType>(negatedTy))
    {
        // ¬unknown ~ never
        result = isCovariantWith(env, builtinTypes->neverType, superTy).withSubComponent(TypePath::TypeField::Negated);
    }
    else if (is<AnyType>(negatedTy))
    {
        // ¬any ~ any
        result = isCovariantWith(env, negatedTy, superTy).withSubComponent(TypePath::TypeField::Negated);
    }
    else if (auto u = get<UnionType>(negatedTy))
    {
        // ¬(A ∪ B) ~ ¬A ∩ ¬B
        // follow intersection rules: A & B <: T iff A <: T && B <: T
        std::vector<SubtypingResult> subtypings;

        for (TypeId ty : u)
        {
            if (auto negatedPart = get<NegationType>(follow(ty)))
                subtypings.push_back(isCovariantWith(env, negatedPart->ty, superTy).withSubComponent(TypePath::TypeField::Negated));
            else
            {
                NegationType negatedTmp{ty};
                subtypings.push_back(isCovariantWith(env, &negatedTmp, superTy));
            }
        }

        result = SubtypingResult::all(subtypings);
    }
    else if (auto i = get<IntersectionType>(negatedTy))
    {
        // ¬(A ∩ B) ~ ¬A ∪ ¬B
        // follow union rules: A | B <: T iff A <: T || B <: T
        std::vector<SubtypingResult> subtypings;

        for (TypeId ty : i)
        {
            if (auto negatedPart = get<NegationType>(follow(ty)))
                subtypings.push_back(isCovariantWith(env, negatedPart->ty, superTy).withSubComponent(TypePath::TypeField::Negated));
            else
            {
                NegationType negatedTmp{ty};
                subtypings.push_back(isCovariantWith(env, &negatedTmp, superTy));
            }
        }

        result = SubtypingResult::any(subtypings);
    }
    else if (is<ErrorType, FunctionType, TableType, MetatableType>(negatedTy))
    {
        iceReporter->ice("attempting to negate a non-testable type");
    }
    // negating a different subtype will get you a very wide type that's not a
    // subtype of other stuff.
    else
    {
        result = SubtypingResult{false}.withSubComponent(TypePath::TypeField::Negated);
    }

    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TypeId subTy, const NegationType* superNegation)
{
    TypeId negatedTy = follow(superNegation->ty);

    SubtypingResult result;

    if (is<NeverType>(negatedTy))
    {
        // ¬never ~ unknown
        result = isCovariantWith(env, subTy, builtinTypes->unknownType);
    }
    else if (is<UnknownType>(negatedTy))
    {
        // ¬unknown ~ never
        result = isCovariantWith(env, subTy, builtinTypes->neverType);
    }
    else if (is<AnyType>(negatedTy))
    {
        // ¬any ~ any
        result = isSubtype(subTy, negatedTy);
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
        result = {p.first->type != p.second->type};
    }
    else if (auto p = get2<SingletonType, PrimitiveType>(subTy, negatedTy))
    {
        // "foo" </: ¬string
        if (get<StringSingleton>(p.first) && p.second->type == PrimitiveType::String)
            result = {false};
        // false </: ¬boolean
        else if (get<BooleanSingleton>(p.first) && p.second->type == PrimitiveType::Boolean)
            result = {false};
        // other cases are true
        else
            result = {true};
    }
    else if (auto p = get2<PrimitiveType, SingletonType>(subTy, negatedTy))
    {
        if (p.first->type == PrimitiveType::String && get<StringSingleton>(p.second))
            result = {false};
        else if (p.first->type == PrimitiveType::Boolean && get<BooleanSingleton>(p.second))
            result = {false};
        else
            result = {true};
    }
    // the top class type is not actually a primitive type, so the negation of
    // any one of them includes the top class type.
    else if (auto p = get2<ClassType, PrimitiveType>(subTy, negatedTy))
        result = {true};
    else if (auto p = get<PrimitiveType>(negatedTy); p && is<TableType, MetatableType>(subTy))
        result = {p->type != PrimitiveType::Table};
    else if (auto p = get2<FunctionType, PrimitiveType>(subTy, negatedTy))
        result = {p.second->type != PrimitiveType::Function};
    else if (auto p = get2<SingletonType, SingletonType>(subTy, negatedTy))
        result = {*p.first != *p.second};
    else if (auto p = get2<ClassType, ClassType>(subTy, negatedTy))
        result = SubtypingResult::negate(isCovariantWith(env, p.first, p.second));
    else if (get2<FunctionType, ClassType>(subTy, negatedTy))
        result = {true};
    else if (is<ErrorType, FunctionType, TableType, MetatableType>(negatedTy))
        iceReporter->ice("attempting to negate a non-testable type");
    else
        result = {false};

    return result.withSuperComponent(TypePath::TypeField::Negated);
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
            results.push_back(isInvariantWith(env, it->second.type(), prop.type()).withBothComponent(TypePath::Property(name)));

        if (subTable->indexer)
        {
            if (isInvariantWith(env, subTable->indexer->indexType, builtinTypes->stringType).isSubtype)
                results.push_back(isInvariantWith(env, subTable->indexer->indexResultType, prop.type())
                                      .withSubComponent(TypePath::TypeField::IndexResult)
                                      .withSuperComponent(TypePath::Property(name)));
        }

        if (results.empty())
            return SubtypingResult{false};

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
    return isCovariantWith(env, subMt->table, superMt->table)
        .andAlso(isCovariantWith(env, subMt->metatable, superMt->metatable).withBothComponent(TypePath::TypeField::Metatable));
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const MetatableType* subMt, const TableType* superTable)
{
    if (auto subTable = get<TableType>(follow(subMt->table)))
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
            result.andAlso(isInvariantWith(env, prop.type(), classProp->type()).withBothComponent(TypePath::Property(name)));
        else
            return SubtypingResult{false};
    }

    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const FunctionType* subFunction, const FunctionType* superFunction)
{
    SubtypingResult result;
    {
        result.orElse(isContravariantWith(env, subFunction->argTypes, superFunction->argTypes).withBothComponent(TypePath::PackField::Arguments));
    }

    result.andAlso(isCovariantWith(env, subFunction->retTypes, superFunction->retTypes).withBothComponent(TypePath::PackField::Returns));

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
                        result.orElse(
                            isCovariantWith(env, stringTable, superTable).withSubPath(TypePath::PathBuilder().mt().prop("__index").build()));
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
                        result.orElse(
                            isCovariantWith(env, stringTable, superTable).withSubPath(TypePath::PathBuilder().mt().prop("__index").build()));
                }
            }
        }
    }
    return result;
}

SubtypingResult Subtyping::isCovariantWith(SubtypingEnvironment& env, const TableIndexer& subIndexer, const TableIndexer& superIndexer)
{
    return isInvariantWith(env, subIndexer.indexType, superIndexer.indexType)
        .withBothComponent(TypePath::TypeField::IndexLookup)
        .andAlso(isInvariantWith(env, subIndexer.indexResultType, superIndexer.indexResultType).withBothComponent(TypePath::TypeField::IndexResult));
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
    return isCovariantWith(env, subVariadic->ty, superVariadic->ty).withBothComponent(TypePath::TypeField::Variadic);
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
