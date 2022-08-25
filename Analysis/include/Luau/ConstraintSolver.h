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

struct InstantiationSignature
{
    TypeFun fn;
    std::vector<TypeId> arguments;
    std::vector<TypePackId> packArguments;

    bool operator==(const InstantiationSignature& rhs) const;
    bool operator!=(const InstantiationSignature& rhs) const
    {
        return !((*this) == rhs);
    }
};

struct HashInstantiationSignature
{
    size_t operator()(const InstantiationSignature& signature) const;
};

struct ConstraintSolver
{
    TypeArena* arena;
    InternalErrorReporter iceReporter;
    // The entire set of constraints that the solver is trying to resolve.
    std::vector<NotNull<Constraint>> constraints;
    NotNull<Scope> rootScope;

    // Constraints that the solver has generated, rather than sourcing from the
    // scope tree.
    std::vector<std::unique_ptr<Constraint>> solverConstraints;

    // This includes every constraint that has not been fully solved.
    // A constraint can be both blocked and unsolved, for instance.
    std::vector<NotNull<const Constraint>> unsolvedConstraints;

    // A mapping of constraint pointer to how many things the constraint is
    // blocked on. Can be empty or 0 for constraints that are not blocked on
    // anything.
    std::unordered_map<NotNull<const Constraint>, size_t> blockedConstraints;
    // A mapping of type/pack pointers to the constraints they block.
    std::unordered_map<BlockedConstraintId, std::vector<NotNull<const Constraint>>> blocked;
    // Memoized instantiations of type aliases.
    DenseHashMap<InstantiationSignature, TypeId, HashInstantiationSignature> instantiatedAliases{{}};

    // Recorded errors that take place within the solver.
    ErrorVec errors;

    ConstraintSolverLogger logger;

    explicit ConstraintSolver(TypeArena* arena, NotNull<Scope> rootScope);

    /**
     * Attempts to dispatch all pending constraints and reach a type solution
     * that satisfies all of the constraints.
     **/
    void run();

    bool done();

    /** Attempt to dispatch a constraint.  Returns true if it was successful.
     * If tryDispatch() returns false, the constraint remains in the unsolved set and will be retried later.
     */
    bool tryDispatch(NotNull<const Constraint> c, bool force);

    bool tryDispatch(const SubtypeConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const PackSubtypeConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const GeneralizationConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const InstantiationConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const UnaryConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const BinaryConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const NameConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const TypeAliasExpansionConstraint& c, NotNull<const Constraint> constraint);

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
    void unblock(const std::vector<TypeId>& types);
    void unblock(const std::vector<TypePackId>& packs);

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
    void unify(TypeId subType, TypeId superType, NotNull<Scope> scope);

    /**
     * Creates a new Unifier and performs a single unification operation. Commits
     * the result.
     * @param subPack the sub-type pack to unify.
     * @param superPack the super-type pack to unify.
     */
    void unify(TypePackId subPack, TypePackId superPack, NotNull<Scope> scope);

    /** Pushes a new solver constraint to the solver.
     * @param cv the body of the constraint.
     **/
    void pushConstraint(ConstraintV cv, NotNull<Scope> scope);

    void reportError(TypeErrorData&& data, const Location& location);
    void reportError(TypeError e);
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

void dump(NotNull<Scope> rootScope, struct ToStringOptions& opts);

} // namespace Luau
