// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Subtyping.h"

#include "Luau/Common.h"
#include "Luau/Error.h"
#include "Luau/Normalize.h"
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

void SubtypingResult::andAlso(const SubtypingResult& other)
{
    isSubtype &= other.isSubtype;
    // `|=` is intentional here, we want to preserve error related flags.
    isErrorSuppressing |= other.isErrorSuppressing;
    normalizationTooComplex |= other.normalizationTooComplex;
}

void SubtypingResult::orElse(const SubtypingResult& other)
{
    isSubtype |= other.isSubtype;
    isErrorSuppressing |= other.isErrorSuppressing;
    normalizationTooComplex |= other.normalizationTooComplex;
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

SubtypingResult Subtyping::isSubtype(TypeId subTy, TypeId superTy)
{
    mappedGenerics.clear();
    mappedGenericPacks.clear();

    SubtypingResult result = isSubtype_(subTy, superTy);

    for (const auto& [subTy, bounds]: mappedGenerics)
    {
        const auto& lb = bounds.lowerBound;
        const auto& ub = bounds.upperBound;

        TypeId lowerBound = makeAggregateType<UnionType>(lb, builtinTypes->neverType);
        TypeId upperBound = makeAggregateType<IntersectionType>(ub, builtinTypes->unknownType);

        result.andAlso(isSubtype_(lowerBound, upperBound));
    }

    return result;
}

SubtypingResult Subtyping::isSubtype(TypePackId subTp, TypePackId superTp)
{
    return isSubtype_(subTp, superTp);
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
    {}

    ~SeenSetPopper()
    {
        seenTypes->erase(pair);
    }
};
}

SubtypingResult Subtyping::isSubtype_(TypeId subTy, TypeId superTy)
{
    subTy = follow(subTy);
    superTy = follow(superTy);

    // TODO: Do we care about returning a proof that this is error-suppressing?
    // e.g. given `a | error <: a | error` where both operands are pointer equal,
    // then should it also carry the information that it's error-suppressing?
    // If it should, then `error <: error` should also do the same.
    if (subTy == superTy)
        return {true};

    std::pair<TypeId, TypeId> typePair{subTy, superTy};
    if (!seenTypes.insert(typePair).second)
        return {true};

    SeenSetPopper ssp{&seenTypes, typePair};

    if (auto superUnion = get<UnionType>(superTy))
        return isSubtype_(subTy, superUnion);
    else if (auto subUnion = get<UnionType>(subTy))
        return isSubtype_(subUnion, superTy);
    else if (auto superIntersection = get<IntersectionType>(superTy))
        return isSubtype_(subTy, superIntersection);
    else if (auto subIntersection = get<IntersectionType>(subTy))
    {
        SubtypingResult result = isSubtype_(subIntersection, superTy);
        if (result.isSubtype || result.isErrorSuppressing || result.normalizationTooComplex)
            return result;
        else
            return isSubtype_(normalizer->normalize(subTy), normalizer->normalize(superTy));
    }
    else if (get<AnyType>(superTy))
        return {true}; // This is always true.
    else if (get<AnyType>(subTy))
    {
        // any = unknown | error, so we rewrite this to match.
        // As per TAPL: A | B <: T iff A <: T && B <: T
        SubtypingResult result = isSubtype_(builtinTypes->unknownType, superTy);
        result.andAlso(isSubtype_(builtinTypes->errorType, superTy));
        return result;
    }
    else if (get<UnknownType>(superTy))
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
    else if (auto subGeneric = get<GenericType>(subTy); subGeneric && variance == Variance::Covariant)
    {
        bool ok = bindGeneric(subTy, superTy);
        return {ok};
    }
    else if (auto superGeneric = get<GenericType>(superTy); superGeneric && variance == Variance::Contravariant)
    {
        bool ok = bindGeneric(subTy, superTy);
        return {ok};
    }
    else if (auto p = get2<PrimitiveType, PrimitiveType>(subTy, superTy))
        return isSubtype_(p);
    else if (auto p = get2<SingletonType, PrimitiveType>(subTy, superTy))
        return isSubtype_(p);
    else if (auto p = get2<SingletonType, SingletonType>(subTy, superTy))
        return isSubtype_(p);
    else if (auto p = get2<FunctionType, FunctionType>(subTy, superTy))
        return isSubtype_(p);
    else if (auto p = get2<TableType, TableType>(subTy, superTy))
        return isSubtype_(p);

    return {false};
}

SubtypingResult Subtyping::isSubtype_(TypePackId subTp, TypePackId superTp)
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
        results.push_back(isSubtype_(subHead[i], superHead[i]));
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
                    results.push_back(isSubtype_(vt->ty, superHead[i]));
            }
            else if (auto gt = get<GenericTypePack>(*subTail))
            {
                if (variance == Variance::Covariant)
                {
                    // For any non-generic type T:
                    //
                    // <X>(X) -> () <: (T) -> ()

                    // Possible optimization: If headSize == 0 then we can just use subTp as-is.
                    std::vector<TypeId> headSlice(begin(superHead), end(superHead) + headSize);
                    TypePackId superTailPack = arena->addTypePack(std::move(headSlice), superTail);

                    if (TypePackId* other = mappedGenericPacks.find(*subTail))
                        results.push_back(isSubtype_(*other, superTailPack));
                    else
                        mappedGenericPacks.try_insert(*subTail, superTailPack);

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
                    results.push_back(isSubtype_(subHead[i], vt->ty));
            }
            else if (auto gt = get<GenericTypePack>(*superTail))
            {
                if (variance == Variance::Contravariant)
                {
                    // For any non-generic type T:
                    //
                    // <X...>(X...) -> () <: (T) -> ()

                    // Possible optimization: If headSize == 0 then we can just use subTp as-is.
                    std::vector<TypeId> headSlice(begin(subHead), end(subHead) + headSize);
                    TypePackId subTailPack = arena->addTypePack(std::move(headSlice), subTail);

                    if (TypePackId* other = mappedGenericPacks.find(*superTail))
                        results.push_back(isSubtype_(*other, subTailPack));
                    else
                        mappedGenericPacks.try_insert(*superTail, subTailPack);

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
            results.push_back(isSubtype_(p.first->ty, p.second->ty));
        }
        else if (auto p = get2<GenericTypePack, GenericTypePack>(*subTail, *superTail))
        {
            bool ok = bindGeneric(*subTail, *superTail);
            results.push_back({ok});
        }
        else if (get2<VariadicTypePack, GenericTypePack>(*subTail, *superTail))
        {
            if (variance == Variance::Contravariant)
            {
                // <A...>(A...) -> number <: (...number) -> number
                bool ok = bindGeneric(*subTail, *superTail);
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
                bool ok = bindGeneric(*subTail, *superTail);
                results.push_back({ok});
            }
        }
        else
            iceReporter->ice(format("Subtyping::isSubtype got unexpected type packs %s and %s", toString(*subTail).c_str(), toString(*superTail).c_str()));
    }
    else if (subTail)
    {
        if (get<VariadicTypePack>(*subTail))
        {
            return {false};
        }
        else if (get<GenericTypePack>(*subTail))
        {
            bool ok = bindGeneric(*subTail, builtinTypes->emptyTypePack);
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
                bool ok = bindGeneric(builtinTypes->emptyTypePack, *superTail);
                results.push_back({ok});
            }
            else
                results.push_back({false});
        }
        else
            LUAU_ASSERT(0); // TODO
    }

    return SubtypingResult::all(results);
}

template<typename SubTy, typename SuperTy>
SubtypingResult Subtyping::isSubtype_(const TryPair<const SubTy*, const SuperTy*>& pair)
{
    return isSubtype_(pair.first, pair.second);
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
SubtypingResult Subtyping::isSubtype_(TypeId subTy, const UnionType* superUnion)
{
    // As per TAPL: T <: A | B iff T <: A || T <: B
    std::vector<SubtypingResult> subtypings;
    for (TypeId ty : superUnion)
        subtypings.push_back(isSubtype_(subTy, ty));
    return SubtypingResult::any(subtypings);
}

SubtypingResult Subtyping::isSubtype_(const UnionType* subUnion, TypeId superTy)
{
    // As per TAPL: A | B <: T iff A <: T && B <: T
    std::vector<SubtypingResult> subtypings;
    for (TypeId ty : subUnion)
        subtypings.push_back(isSubtype_(ty, superTy));
    return SubtypingResult::all(subtypings);
}

SubtypingResult Subtyping::isSubtype_(TypeId subTy, const IntersectionType* superIntersection)
{
    // As per TAPL: T <: A & B iff T <: A && T <: B
    std::vector<SubtypingResult> subtypings;
    for (TypeId ty : superIntersection)
        subtypings.push_back(isSubtype_(subTy, ty));
    return SubtypingResult::all(subtypings);
}

SubtypingResult Subtyping::isSubtype_(const IntersectionType* subIntersection, TypeId superTy)
{
    // TODO: Semantic subtyping here.
    // As per TAPL: A & B <: T iff A <: T || B <: T
    std::vector<SubtypingResult> subtypings;
    for (TypeId ty : subIntersection)
        subtypings.push_back(isSubtype_(ty, superTy));
    return SubtypingResult::any(subtypings);
}

SubtypingResult Subtyping::isSubtype_(const PrimitiveType* subPrim, const PrimitiveType* superPrim)
{
    return {subPrim->type == superPrim->type};
}

SubtypingResult Subtyping::isSubtype_(const SingletonType* subSingleton, const PrimitiveType* superPrim)
{
    if (get<StringSingleton>(subSingleton) && superPrim->type == PrimitiveType::String)
        return {true};
    else if (get<BooleanSingleton>(subSingleton) && superPrim->type == PrimitiveType::Boolean)
        return {true};
    else
        return {false};
}

SubtypingResult Subtyping::isSubtype_(const SingletonType* subSingleton, const SingletonType* superSingleton)
{
    return {*subSingleton == *superSingleton};
}

SubtypingResult Subtyping::isSubtype_(const TableType* subTable, const TableType* superTable)
{
    SubtypingResult result{true};

    for (const auto& [name, prop]: superTable->props)
    {
        auto it = subTable->props.find(name);
        if (it != subTable->props.end())
        {
            // Table properties are invariant
            result.andAlso(isSubtype(it->second.type(), prop.type()));
            result.andAlso(isSubtype(prop.type(), it->second.type()));
        }
        else
            return SubtypingResult{false};
    }

    return result;
}

SubtypingResult Subtyping::isSubtype_(const FunctionType* subFunction, const FunctionType* superFunction)
{
    SubtypingResult result;
    {
        VarianceFlipper vf{&variance};
        result.orElse(isSubtype_(superFunction->argTypes, subFunction->argTypes));
    }

    result.andAlso(isSubtype_(subFunction->retTypes, superFunction->retTypes));

    return result;
}

SubtypingResult Subtyping::isSubtype_(const NormalizedType* subNorm, const NormalizedType* superNorm)
{
    if (!subNorm || !superNorm)
        return {false, true, true};

    SubtypingResult result = isSubtype_(subNorm->tops, superNorm->tops);
    result.andAlso(isSubtype_(subNorm->booleans, superNorm->booleans));
    // isSubtype_(subNorm->classes, superNorm->classes);
    // isSubtype_(subNorm->classes, superNorm->tables);
    result.andAlso(isSubtype_(subNorm->errors, superNorm->errors));
    result.andAlso(isSubtype_(subNorm->nils, superNorm->nils));
    result.andAlso(isSubtype_(subNorm->numbers, superNorm->numbers));
    result.isSubtype &= Luau::isSubtype(subNorm->strings, superNorm->strings);
    // isSubtype_(subNorm->strings, superNorm->tables);
    result.andAlso(isSubtype_(subNorm->threads, superNorm->threads));
    // isSubtype_(subNorm->tables, superNorm->tables);
    // isSubtype_(subNorm->tables, superNorm->strings);
    // isSubtype_(subNorm->tables, superNorm->classes);
    // isSubtype_(subNorm->functions, superNorm->functions);
    // isSubtype_(subNorm->tyvars, superNorm->tyvars);

    return result;
}

bool Subtyping::bindGeneric(TypeId subTy, TypeId superTy)
{
    if (variance == Variance::Covariant)
    {
        if (!get<GenericType>(subTy))
            return false;

        mappedGenerics[subTy].upperBound.insert(superTy);
    }
    else
    {
        if (!get<GenericType>(superTy))
            return false;

        mappedGenerics[superTy].lowerBound.insert(subTy);
    }

    return true;
}

/*
 * If, when performing a subtyping test, we encounter a generic on the left
 * side, it is permissible to tentatively bind that generic to the right side
 * type.
 */
bool Subtyping::bindGeneric(TypePackId subTp, TypePackId superTp)
{
    if (variance == Variance::Contravariant)
        std::swap(superTp, subTp);

    if (!get<GenericTypePack>(subTp))
        return false;

    if (TypePackId* m = mappedGenericPacks.find(subTp))
        return *m == superTp;

    mappedGenericPacks[subTp] = superTp;

    return true;
}

template <typename T, typename Container>
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
