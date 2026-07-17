// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Constraint.h"
#include "Luau/Set.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"

/**
 * It is said that there are only so many unique problems in computer science.
 *
 * We have stumbled upon one of them: build systems.
 */

namespace Luau
{

using BlockedConstraintId = Variant<TypeId, TypePackId, const Constraint*>;

struct HashBlockedConstraintId
{
    size_t operator()(const BlockedConstraintId& bci) const;
};


using ConstraintVertex = BlockedConstraintId;

/**
 * Helper data structure for `ConstraintGraph`: an ordered, mutable set.
 */
struct ConstraintList
{
    bool contains(ConstraintVertex vertex) const;
    void insert(ConstraintVertex vertex);
    void remove(ConstraintVertex vertex);
    void clear();
    size_t size() const;

    struct Iterator
    {
        using value_type = const ConstraintVertex;
        using pointer = value_type*;
        using reference = value_type&;
        using iterator_category = std::input_iterator_tag;

        explicit Iterator(NotNull<ConstraintList> cl, size_t index);

        Iterator& operator++();

        bool operator==(const Iterator& rhs) const;
        bool operator!=(const Iterator& rhs) const;

        value_type operator*();

    private:
        NotNull<ConstraintList> cl;
        size_t index;

        void advanceUntilPresentOrEnd();
    };

    Iterator begin();
    Iterator end();

private:
    DenseHashMap<ConstraintVertex, bool, HashBlockedConstraintId> present{(TypeId) nullptr};
    std::vector<ConstraintVertex> order;
    size_t entries = 0;
};


/**
 * Represents an (aspirationally) acyclic graph of constraints, types, and
 * type packs. Edges in the graph represent dependencies. For example, a
 * GeneralizationConstraint depends on all of the constraints of the
 * function it is generalizing, and the blocked type representing the
 * generalized function depends on said GeneralizationConstraint.
 *
 * We need to consider six (P(3, 2)) scenarios, though many overlap:
 * - If constraint A depends on constraint B, we must dispatch B before A.
 * - If constraint A depends on a type or type pack, we must wait for those
 *   types to be "unblocked." This could be via generalization, a blocked
 *   type being replaced by another type, or a type function being solved.
 * - If a type or type pack depends on a constraint, then at present it must
 *   be a free type. Blocked types (and similar types such as type functions
 *   and pending expansion types) are not represented in the graph in this way.
 * - Types should never depend on other types or type packs.
 *
 * For now, ConstraintGraph is not responsible for determining which constraint
 * to dispatch next.
 */
struct ConstraintGraph
{
    using ConstraintMap = DenseHashMap<ConstraintVertex, ConstraintList*, HashBlockedConstraintId>;

    ConstraintGraph(NotNull<BuiltinTypes> builtinTypes);

    /**
     * Add [dependency] as a blocker for [target]
     *
     * Returns whether this is a fresh relationship (were we already tracking
     * it).
     */
    bool addDependencyOf(ConstraintVertex dependency, ConstraintVertex target);

    /**
     * Semantically the same as [block(ConstraintVertex, ConstraintVertex)],
     * this is a helper overload for ConstraintGenerator.
     */
    bool addDependencyOf(Constraint* dependency, Constraint* target);

    /**
     * Take all of the reverse dependencies of [existingVertex] and *also*
     * make them reverse dependencies of [newVertex].
     */
    void inheritBlocks(ConstraintVertex existingVertex, ConstraintVertex newVertex);

    struct UnblockedTypes
    {
        TypeIds types;
        TypePackIds packs;
    };

    /**
     * Unblock constraint [c]:
     *  1. Iterate over the reverse dependencies of [c] and remove [c] from their dep list.
     *  2. Collect all of the reverse dependencies of [c] that are types or type packs.
     *  3. Repair any bound types to ensure the constraint graph remains accurate.
     *  4. Return unblocked types and type packs.
     */
    UnblockedTypes unblockConstraint(NotNull<const Constraint> c);

    /**
     * Unblock type [vertex].
     * 1. If [vertex] is now a bound type, walk the chain of bound types and
     *    repair references to said type in the graph (see: `repairTypeReferneces`).
     * 2. After references have been repaired, walk the reverse dependencies of
     *    [vertex] and remove [vertex] from each dependency list, and then clear
     *    the reverse dependency list of [vertex].
     */
    void unblockTypeOrPack(TypeId vertex);

    /**
     * Unblock type *pack* [vertex].
     * 1. If [vertex] is now a bound type, walk the chain of bound types and
     *    repair references to said type in the graph (see: `repairTypeReferneces`).
     * 2. After references have been repaired, walk the reverse dependencies of
     *    [vertex] and remove [vertex] from each dependency list, and then clear
     *    the reverse dependency list of [vertex].
     */
    void unblockTypeOrPack(TypePackId vertex);

    /**
     * Return whether the vertex has any unsolved dependencies.
     *
     * HACK: For `PrimitiveTypeConstraint` we consider it unblocked if there is
     * a single dependency.
     */
    bool hasUnsolvedDependencies(ConstraintVertex vertex);

    /**
     * HACK: Used for `PrimitiveTypeConstraint` to check whether the free type
     * it "controls" has other outstanding dependencies.
     */
    bool DEPRECATED_hasStrictlyMoreThanOneDependency(ConstraintVertex vertex);

    /**
     * Find all of the reference counted types that are reachable from `target`
     * and shift the dependencies (and reverse dependencies) of source over
     * without rebinding source to target.
     */
    template<typename T>
    void copyDependenciesOf(T source, T target);

    /**
     * NOTE: You probably do not want to call this function directly.
     *
     * This attempts to find all the reachable mutable types from [target] and
     * shift all references from the type [source] to [target]. You probably
     * intend to use [copyDependenciesOf], the non-destructive version.
     */
    template<typename T>
    void shiftReferences(T source, T target);

    [[maybe_unused]]
    void dumpWith(const std::vector<NotNull<const Constraint>>& unsolvedConstraints, ToStringOptions& opts);

    [[maybe_unused]]
    void dumpBlocked(NotNull<const Constraint> c, ToStringOptions& opts);

private:
    NotNull<BuiltinTypes> builtinTypes;

    /**
     * We need to handle arbitrary cases of types being rebound in the type
     * graph.
     *
     * If [ty] is not bound, exit immediately. Otherwise, traverse the bound
     * type chain from [ty] to its root and, for each type in the chain [ty']
     * that is not the root, shift the references *to* the root type.
     */
    template<typename T>
    void repairTypeReferences(T ty);

    /**
     * For all types and type packs [t] in the params [mutatedTypes] and [mutatedTypePacks],
     * and vertex [v] in [originalSource],
     * 1. Add [v] as a dependency of [t]
     * 2. Add [t] to the reverse deps of [v]
     * 3. If we have provided an [originalSource], remove it from the
     *    reverse dependencies of [v].
     *
     * We use this function to either constructively or destructively copy
     * references from one type to another as part of [repairTypeReferences]
     * and [shiftReferences].
     */
    void copyDependenciesToReachableTypes(
        std::optional<ConstraintVertex> originalVertex,
        NotNull<ConstraintList> source,
        TypeIds mutatedTypes,
        TypePackIds mutatedTypePacks
    );

    /**
     * For all the reverse dependencies of [vertex], remove [vertex] from their
     * dependency list. Finally, remove the [vertex] entry from `reverseDependencies`.
     */
    void clearReverseDependenciesOf(ConstraintVertex vertex);

    /**
     * Mapping from vertices to their dependencies. A missing entry or an entry
     * pointing to the empty set indicates no dependencies:
     * - Any free type with no dependencies can be generalized;
     * - Any free type pack with no dependencies can be generalized;
     * - Any constraint with no dependencies can be dispatched.
     */
    ConstraintMap dependencies{(TypeId) nullptr};


    NotNull<ConstraintList> findDependencyList(ConstraintVertex vertex);

    /**
     * Inverse of the above mapping. Yes, the proper name for this is
     * "dependents," but naming it such will result in hellish typos.
     */
    ConstraintMap reverseDependencies{(TypeId) nullptr};
    NotNull<ConstraintList> findReverseDependencyList(ConstraintVertex vertex);

    /**
     * We do the same pseudo-arena trick as constraints do right now.
     */
    std::vector<std::unique_ptr<ConstraintList>> constraintLists;

    [[maybe_unused]]
    void dump();
};

std::string dump(ConstraintVertex vertex);

} // namespace Luau