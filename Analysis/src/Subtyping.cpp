// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Subtyping.h"

#include "Luau/Common.h"
#include "Luau/Normalize.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"

#include <algorithm>

namespace Luau
{

SubtypingGraph SubtypingGraph::and_(const SubtypingGraph& other)
{
    return SubtypingGraph{
        isSubtype && other.isSubtype,
        // `||` is intentional here, we want to preserve error-suppressing flag.
        isErrorSuppressing || other.isErrorSuppressing,
        normalizationTooComplex || other.normalizationTooComplex,
    };
}

SubtypingGraph SubtypingGraph::or_(const SubtypingGraph& other)
{
    return SubtypingGraph{
        isSubtype || other.isSubtype,
        isErrorSuppressing || other.isErrorSuppressing,
        normalizationTooComplex || other.normalizationTooComplex,
    };
}

SubtypingGraph SubtypingGraph::and_(const std::vector<SubtypingGraph>& results)
{
    SubtypingGraph acc{true, false};
    for (const SubtypingGraph& current : results)
        acc = acc.and_(current);
    return acc;
}

SubtypingGraph SubtypingGraph::or_(const std::vector<SubtypingGraph>& results)
{
    SubtypingGraph acc{false, false};
    for (const SubtypingGraph& current : results)
        acc = acc.or_(current);
    return acc;
}

SubtypingGraph Subtyping::isSubtype(TypeId subTy, TypeId superTy)
{
    subTy = follow(subTy);
    superTy = follow(superTy);

    // TODO: Do we care about returning a proof that this is error-suppressing?
    // e.g. given `a | error <: a | error` where both operands are pointer equal,
    // then should it also carry the information that it's error-suppressing?
    // If it should, then `error <: error` should also do the same.
    if (subTy == superTy)
        return {true};


    if (auto superUnion = get<UnionType>(superTy))
        return isSubtype(subTy, superUnion);
    else if (auto subUnion = get<UnionType>(subTy))
        return isSubtype(subUnion, superTy);
    else if (auto superIntersection = get<IntersectionType>(superTy))
        return isSubtype(subTy, superIntersection);
    else if (auto subIntersection = get<IntersectionType>(subTy))
    {
        SubtypingGraph result = isSubtype(subIntersection, superTy);
        if (result.isSubtype || result.isErrorSuppressing || result.normalizationTooComplex)
            return result;
        else
            return isSubtype(normalizer->normalize(subTy), normalizer->normalize(superTy));
    }
    else if (get<AnyType>(superTy))
        return {true}; // This is always true.
    else if (get<AnyType>(subTy))
    {
        // any = unknown | error, so we rewrite this to match.
        // As per TAPL: A | B <: T iff A <: T && B <: T
        return isSubtype(builtinTypes->unknownType, superTy).and_(isSubtype(builtinTypes->errorType, superTy));
    }
    else if (auto superUnknown = get<UnknownType>(superTy))
    {
        LUAU_ASSERT(!get<AnyType>(subTy)); // TODO: replace with ice.
        LUAU_ASSERT(!get<UnionType>(subTy)); // TODO: replace with ice.
        LUAU_ASSERT(!get<IntersectionType>(subTy)); // TODO: replace with ice.

        bool errorSuppressing = get<ErrorType>(subTy);
        return {!errorSuppressing, errorSuppressing};
    }
    else if (get<NeverType>(subTy))
        return {true};
    else if (get<ErrorType>(superTy))
        return {false, true};
    else if (get<ErrorType>(subTy))
        return {false, true};
    else if (auto p = get2<PrimitiveType, PrimitiveType>(subTy, superTy))
        return isSubtype(p);
    else if (auto p = get2<SingletonType, PrimitiveType>(subTy, superTy))
        return isSubtype(p);
    else if (auto p = get2<SingletonType, SingletonType>(subTy, superTy))
        return isSubtype(p);
    else if (auto p = get2<FunctionType, FunctionType>(subTy, superTy))
        return isSubtype(p);

    return {false};
}

SubtypingGraph Subtyping::isSubtype(TypePackId subTp, TypePackId superTp)
{
    subTp = follow(subTp);
    superTp = follow(superTp);

    auto [subHead, subTail] = flatten(subTp);
    auto [superHead, superTail] = flatten(superTp);

    const size_t headSize = std::min(subHead.size(), superHead.size());

    std::vector<SubtypingGraph> results;
    results.reserve(std::max(subHead.size(), superHead.size()) + 1);

    // Match head types pairwise

    for (size_t i = 0; i < headSize; ++i)
    {
        results.push_back(isSubtype(subHead[i], superHead[i]));
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
                {
                    results.push_back(isSubtype(vt->ty, superHead[i]));
                }
            }
            else
                LUAU_ASSERT(0); // TODO
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
                {
                    results.push_back(isSubtype(subHead[i], vt->ty));
                }
            }
            else
                LUAU_ASSERT(0); // TODO
        }
        else
            return {false};
    }
    else
    {
        // subHead and superHead are the same size.  Nothing more must be done.
    }

    // Handle tails

    if (subTail && superTail)
    {
        if (auto p = get2<VariadicTypePack, VariadicTypePack>(*subTail, *superTail))
        {
            results.push_back(isSubtype(p.first->ty, p.second->ty));
        }
        else
            LUAU_ASSERT(0); // TODO
    }
    else if (subTail)
    {
        if (get<VariadicTypePack>(*subTail))
        {
            return {false};
        }

        LUAU_ASSERT(0); // TODO
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
        else
            LUAU_ASSERT(0); // TODO
    }

    return SubtypingGraph::and_(results);
}

template<typename SubTy, typename SuperTy>
SubtypingGraph Subtyping::isSubtype(const TryPair<const SubTy*, const SuperTy*>& pair)
{
    return isSubtype(pair.first, pair.second);
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
SubtypingGraph Subtyping::isSubtype(TypeId subTy, const UnionType* superUnion)
{
    // As per TAPL: T <: A | B iff T <: A || T <: B
    std::vector<SubtypingGraph> subtypings;
    for (TypeId ty : superUnion)
        subtypings.push_back(isSubtype(subTy, ty));
    return SubtypingGraph::or_(subtypings);
}

SubtypingGraph Subtyping::isSubtype(const UnionType* subUnion, TypeId superTy)
{
    // As per TAPL: A | B <: T iff A <: T && B <: T
    std::vector<SubtypingGraph> subtypings;
    for (TypeId ty : subUnion)
        subtypings.push_back(isSubtype(ty, superTy));
    return SubtypingGraph::and_(subtypings);
}

SubtypingGraph Subtyping::isSubtype(TypeId subTy, const IntersectionType* superIntersection)
{
    // As per TAPL: T <: A & B iff T <: A && T <: B
    std::vector<SubtypingGraph> subtypings;
    for (TypeId ty : superIntersection)
        subtypings.push_back(isSubtype(subTy, ty));
    return SubtypingGraph::and_(subtypings);
}

SubtypingGraph Subtyping::isSubtype(const IntersectionType* subIntersection, TypeId superTy)
{
    // TODO: Semantic subtyping here.
    // As per TAPL: A & B <: T iff A <: T || B <: T
    std::vector<SubtypingGraph> subtypings;
    for (TypeId ty : subIntersection)
        subtypings.push_back(isSubtype(ty, superTy));
    return SubtypingGraph::or_(subtypings);
}

SubtypingGraph Subtyping::isSubtype(const PrimitiveType* subPrim, const PrimitiveType* superPrim)
{
    return {subPrim->type == superPrim->type};
}

SubtypingGraph Subtyping::isSubtype(const SingletonType* subSingleton, const PrimitiveType* superPrim)
{
    if (get<StringSingleton>(subSingleton) && superPrim->type == PrimitiveType::String)
        return {true};
    else if (get<BooleanSingleton>(subSingleton) && superPrim->type == PrimitiveType::Boolean)
        return {true};
    else
        return {false};
}

SubtypingGraph Subtyping::isSubtype(const SingletonType* subSingleton, const SingletonType* superSingleton)
{
    return {*subSingleton == *superSingleton};
}

SubtypingGraph Subtyping::isSubtype(const FunctionType* subFunction, const FunctionType* superFunction)
{
    SubtypingGraph argResult = isSubtype(superFunction->argTypes, subFunction->argTypes);
    SubtypingGraph retResult = isSubtype(subFunction->retTypes, superFunction->retTypes);

    return argResult.and_(retResult);
}

SubtypingGraph Subtyping::isSubtype(const NormalizedType* subNorm, const NormalizedType* superNorm)
{
    if (!subNorm || !superNorm)
        return {false, true, true};

    SubtypingGraph result{true};
    result = result.and_(isSubtype(subNorm->tops, superNorm->tops));
    result = result.and_(isSubtype(subNorm->booleans, superNorm->booleans));
    // isSubtype(subNorm->classes, superNorm->classes);
    // isSubtype(subNorm->classes, superNorm->tables);
    result = result.and_(isSubtype(subNorm->errors, superNorm->errors));
    result = result.and_(isSubtype(subNorm->nils, superNorm->nils));
    result = result.and_(isSubtype(subNorm->numbers, superNorm->numbers));
    result.isSubtype &= Luau::isSubtype(subNorm->strings, superNorm->strings);
    // isSubtype(subNorm->strings, superNorm->tables);
    result = result.and_(isSubtype(subNorm->threads, superNorm->threads));
    // isSubtype(subNorm->tables, superNorm->tables);
    // isSubtype(subNorm->tables, superNorm->strings);
    // isSubtype(subNorm->tables, superNorm->classes);
    // isSubtype(subNorm->functions, superNorm->functions);
    // isSubtype(subNorm->tyvars, superNorm->tyvars);

    return result;
}

} // namespace Luau
