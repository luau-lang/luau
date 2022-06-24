// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Error.h"
#include "Luau/Variant.h"
#include "Luau/Constraint.h"
#include "Luau/ConstraintSolverLogger.h"
#include "Luau/TypeVar.h"

#include <vector>

namespace Luau
{

// TypeId, TypePackId, or Constraint*. It is impossible to know which, but we
// never dereference this pointer.
using BlockedConstraintId = const void*;

struct ConstraintSolver
{
    TypeArena* arena;
    InternalErrorReporter iceReporter;
    // The entire set of constraints that the solver is trying to resolve. It
    // is important to not add elements to this vector, lest the underlying
    // storage that we retain pointers to be mutated underneath us.
    const std::vector<NotNull<Constraint>> constraints;
    Scope2* rootScope;

    // This includes every constraint that has not been fully solved.
    // A constraint can be both blocked and unsolved, for instance.
    std::vector<NotNull<const Constraint>> unsolvedConstraints;

    // A mapping of constraint pointer to how many things the constraint is
    // blocked on. Can be empty or 0 for constraints that are not blocked on
    // anything.
    std::unordered_map<NotNull<const Constraint>, size_t> blockedConstraints;
    // A mapping of type/pack pointers to the constraints they block.
    std::unordered_map<BlockedConstraintId, std::vector<NotNull<const Constraint>>> blocked;

    ConstraintSolverLogger logger;

    explicit ConstraintSolver(TypeArena* arena, Scope2* rootScope);

    /**
     * Attempts to dispatch all pending constraints and reach a type solution
     * that satisfies all of the constraints.
     **/
    void run();

    bool done();

    bool tryDispatch(NotNull<const Constraint> c, bool force);
    bool tryDispatch(const SubtypeConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const PackSubtypeConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const GeneralizationConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const InstantiationConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const NameConstraint& c, NotNull<const Constraint> constraint);

    void block(NotNull<const Constraint> target, NotNull<const Constraint> constraint);
    /**
     * Block a constraint on the resolution of a TypeVar.
     * @returns false always.  This is just to allow tryDispatch to return the result of block()
     */
    bool block(TypeId target, NotNull<const Constraint> constraint);
    bool block(TypePackId target, NotNull<const Constraint> constraint);

    void unblock(NotNull<const Constraint> progressed);
    void unblock(TypeId progressed);
    void unblock(TypePackId progressed);

    /**
     * @returns true if the TypeId is in a blocked state.
     */
    bool isBlocked(TypeId ty);

    /**
     * Returns whether the constraint is blocked on anything.
     * @param constraint the constraint to check.
     */
    bool isBlocked(NotNull<const Constraint> constraint);

    /**
     * Creates a new Unifier and performs a single unification operation. Commits
     * the result.
     * @param subType the sub-type to unify.
     * @param superType the super-type to unify.
     */
    void unify(TypeId subType, TypeId superType);

    /**
     * Creates a new Unifier and performs a single unification operation. Commits
     * the result.
     * @param subPack the sub-type pack to unify.
     * @param superPack the super-type pack to unify.
     */
    void unify(TypePackId subPack, TypePackId superPack);

private:
    /**
     * Marks a constraint as being blocked on a type or type pack. The constraint
     * solver will not attempt to dispatch blocked constraints until their
     * dependencies have made progress.
     * @param target the type or type pack pointer that the constraint is blocked on.
     * @param constraint the constraint to block.
     **/
    void block_(BlockedConstraintId target, NotNull<const Constraint> constraint);

    /**
     * Informs the solver that progress has been made on a type or type pack. The
     * solver will wake up all constraints that are blocked on the type or type pack,
     * and will resume attempting to dispatch them.
     * @param progressed the type or type pack pointer that has progressed.
     **/
    void unblock_(BlockedConstraintId progressed);
};

void dump(Scope2* rootScope, struct ToStringOptions& opts);

} // namespace Luau
