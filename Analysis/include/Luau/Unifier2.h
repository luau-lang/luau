// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Constraint.h"
#include "Luau/DenseHash.h"
#include "Luau/NotNull.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypePairHash.h"

#include <optional>
#include <vector>
#include <utility>

namespace Luau
{

struct InternalErrorReporter;
struct Scope;
struct TypeArena;

enum class OccursCheckResult
{
    Pass,
    Fail
};

enum class UnifyResult
{
    Ok,
    OccursCheckFailed,
    TooComplex
};

inline UnifyResult operator&(UnifyResult lhs, UnifyResult rhs)
{
    if (lhs == UnifyResult::Ok)
        return rhs;
    return lhs;
}

inline UnifyResult& operator&=(UnifyResult& lhs, UnifyResult rhs)
{
    if (lhs == UnifyResult::Ok)
        lhs = rhs;
    return lhs;
}

struct Unifier2
{
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<Scope> scope;
    NotNull<InternalErrorReporter> ice;
    TypeCheckLimits limits;

    DenseHashSet<std::pair<TypeId, TypeId>, TypePairHash> seenTypePairings{{nullptr, nullptr}};
    DenseHashSet<std::pair<TypePackId, TypePackId>, TypePairHash> seenTypePackPairings{{nullptr, nullptr}};

    DenseHashMap<TypeId, std::vector<TypeId>> expandedFreeTypes{nullptr};

    // Mapping from generic types to free types to be used in instantiation.
    DenseHashMap<TypeId, TypeId> genericSubstitutions{nullptr};
    // Mapping from generic type packs to `TypePack`s of free types to be used in instantiation.
    DenseHashMap<TypePackId, TypePackId> genericPackSubstitutions{nullptr};

    // Unification sometimes results in the creation of new free types.
    // We collect them here so that other systems can perform necessary
    // bookkeeping.
    std::vector<TypeId> newFreshTypes;
    std::vector<TypePackId> newFreshTypePacks;

    int iterationCount = 0;
    int recursionCount = 0;
    int recursionLimit = 0;

    std::vector<ConstraintV> incompleteSubtypes;
    // null if not in a constraint solving context
    DenseHashSet<const void*>* uninhabitedTypeFunctions;

    Unifier2(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<Scope> scope, NotNull<InternalErrorReporter> ice);
    Unifier2(
        NotNull<TypeArena> arena,
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<Scope> scope,
        NotNull<InternalErrorReporter> ice,
        DenseHashSet<const void*>* uninhabitedTypeFunctions
    );

    UnifyResult unify(TypeId subTy, TypeId superTy);
    UnifyResult unify(TypePackId subTp, TypePackId superTp);

private:
    /** Attempt to commit the subtype relation subTy <: superTy to the type
     * graph.
     *
     * @returns true if successful.
     *
     * Note that incoherent types can and will successfully be unified.  We stop
     * when we *cannot know* how to relate the provided types, not when doing so
     * would narrow something down to never or broaden it to unknown.
     *
     * Presently, the only way unification can fail is if we attempt to bind one
     * free TypePack to another and encounter an occurs check violation.
     */
    UnifyResult unify_(TypeId subTy, TypeId superTy);
    UnifyResult unifyFreeWithType(TypeId subTy, TypeId superTy);
    UnifyResult unify_(TypeId subTy, const FunctionType* superFn);
    UnifyResult unify_(const UnionType* subUnion, TypeId superTy);
    UnifyResult unify_(TypeId subTy, const UnionType* superUnion);
    UnifyResult unify_(const IntersectionType* subIntersection, TypeId superTy);
    UnifyResult unify_(TypeId subTy, const IntersectionType* superIntersection);
    UnifyResult unify_(TableType* subTable, const TableType* superTable);
    UnifyResult unify_(const MetatableType* subMetatable, const MetatableType* superMetatable);

    UnifyResult unify_(const AnyType* subAny, const FunctionType* superFn);
    UnifyResult unify_(const FunctionType* subFn, const AnyType* superAny);
    UnifyResult unify_(const AnyType* subAny, const TableType* superTable);
    UnifyResult unify_(const TableType* subTable, const AnyType* superAny);

    UnifyResult unify_(const MetatableType* subMetatable, const AnyType*);
    UnifyResult unify_(const AnyType*, const MetatableType* superMetatable);

    UnifyResult unify_(TypePackId subTp, TypePackId superTp);

    std::optional<TypeId> generalize(TypeId ty);

    /**
     * @returns simplify(left | right)
     */
    TypeId mkUnion(TypeId left, TypeId right);

    /**
     * @returns simplify(left & right)
     */
    TypeId mkIntersection(TypeId left, TypeId right);

    // Returns true if needle occurs within haystack already.  ie if we bound
    // needle to haystack, would a cyclic type result?
    OccursCheckResult occursCheck(DenseHashSet<TypeId>& seen, TypeId needle, TypeId haystack);

    // Returns true if needle occurs within haystack already.  ie if we bound
    // needle to haystack, would a cyclic TypePack result?
    OccursCheckResult occursCheck(DenseHashSet<TypePackId>& seen, TypePackId needle, TypePackId haystack);

    TypeId freshType(NotNull<Scope> scope, Polarity polarity);
    TypePackId freshTypePack(NotNull<Scope> scope, Polarity polarity);
};

} // namespace Luau
