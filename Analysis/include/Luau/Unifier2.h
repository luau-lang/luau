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

    int recursionCount = 0;
    int recursionLimit = 0;

    std::vector<ConstraintV> incompleteSubtypes;
    // null if not in a constraint solving context
    DenseHashSet<const void*>* uninhabitedTypeFunctions;

    Unifier2(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<Scope> scope, NotNull<InternalErrorReporter> ice);
    Unifier2(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<Scope> scope, NotNull<InternalErrorReporter> ice,
        DenseHashSet<const void*>* uninhabitedTypeFunctions);

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
    bool unify(TypeId subTy, TypeId superTy);
    bool unifyFreeWithType(TypeId subTy, TypeId superTy);
    bool unify(TypeId subTy, const FunctionType* superFn);
    bool unify(const UnionType* subUnion, TypeId superTy);
    bool unify(TypeId subTy, const UnionType* superUnion);
    bool unify(const IntersectionType* subIntersection, TypeId superTy);
    bool unify(TypeId subTy, const IntersectionType* superIntersection);
    bool unify(TableType* subTable, const TableType* superTable);
    bool unify(const MetatableType* subMetatable, const MetatableType* superMetatable);

    bool unify(const AnyType* subAny, const FunctionType* superFn);
    bool unify(const FunctionType* subFn, const AnyType* superAny);
    bool unify(const AnyType* subAny, const TableType* superTable);
    bool unify(const TableType* subTable, const AnyType* superAny);

    // TODO think about this one carefully.  We don't do unions or intersections of type packs
    bool unify(TypePackId subTp, TypePackId superTp);

    std::optional<TypeId> generalize(TypeId ty);

private:
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
};

} // namespace Luau
