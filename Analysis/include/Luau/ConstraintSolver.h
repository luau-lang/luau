// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Error.h"
#include "Luau/Variant.h"
#include "Luau/Constraint.h"
#include "Luau/TypeVar.h"
#include "Luau/ToString.h"
#include "Luau/Normalize.h"

#include <vector>

namespace Luau
{

struct DcrLogger;

// TypeId, TypePackId, or Constraint*. It is impossible to know which, but we
// never dereference this pointer.
using BlockedConstraintId = const void*;

struct ModuleResolver;

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
    NotNull<SingletonTypes> singletonTypes;
    InternalErrorReporter iceReporter;
    NotNull<Normalizer> normalizer;
    // The entire set of constraints that the solver is trying to resolve.
    std::vector<NotNull<Constraint>> constraints;
    NotNull<Scope> rootScope;
    ModuleName currentModuleName;

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

    NotNull<ModuleResolver> moduleResolver;
    std::vector<RequireCycle> requireCycles;

    DcrLogger* logger;

    explicit ConstraintSolver(NotNull<Normalizer> normalizer, NotNull<Scope> rootScope, ModuleName moduleName, NotNull<ModuleResolver> moduleResolver,
        std::vector<RequireCycle> requireCycles, DcrLogger* logger);

    // Randomize the order in which to dispatch constraints
    void randomize(unsigned seed);

    /**
     * Attempts to dispatch all pending constraints and reach a type solution
     * that satisfies all of the constraints.
     **/
    void run();

    bool isDone();

    void finalizeModule();

    /** Attempt to dispatch a constraint.  Returns true if it was successful. If
     * tryDispatch() returns false, the constraint remains in the unsolved set
     * and will be retried later.
     */
    bool tryDispatch(NotNull<const Constraint> c, bool force);

    bool tryDispatch(const SubtypeConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const PackSubtypeConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const GeneralizationConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const InstantiationConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const UnaryConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const BinaryConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const IterableConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const NameConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const TypeAliasExpansionConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const FunctionCallConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const PrimitiveTypeConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const HasPropConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const SingletonOrTopTypeConstraint& c, NotNull<const Constraint> constraint);

    // for a, ... in some_table do
    // also handles __iter metamethod
    bool tryDispatchIterableTable(TypeId iteratorTy, const IterableConstraint& c, NotNull<const Constraint> constraint, bool force);

    // for a, ... in next_function, t, ... do
    bool tryDispatchIterableFunction(
        TypeId nextTy, TypeId tableTy, TypeId firstIndexTy, const IterableConstraint& c, NotNull<const Constraint> constraint, bool force);

    void block(NotNull<const Constraint> target, NotNull<const Constraint> constraint);
    /**
     * Block a constraint on the resolution of a TypeVar.
     * @returns false always.  This is just to allow tryDispatch to return the result of block()
     */
    bool block(TypeId target, NotNull<const Constraint> constraint);
    bool block(TypePackId target, NotNull<const Constraint> constraint);

    // Traverse the type.  If any blocked or pending typevars are found, block
    // the constraint on them.
    //
    // Returns false if a type blocks the constraint.
    //
    // FIXME: This use of a boolean for the return result is an appalling
    // interface.
    bool recursiveBlock(TypeId target, NotNull<const Constraint> constraint);
    bool recursiveBlock(TypePackId target, NotNull<const Constraint> constraint);

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
     * @returns true if the TypePackId is in a blocked state.
     */
    bool isBlocked(TypePackId tp);

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
    void pushConstraint(NotNull<Scope> scope, const Location& location, ConstraintV cv);

    /**
     * Attempts to resolve a module from its module information. Returns the
     * module-level return type of the module, or the error type if one cannot
     * be found. Reports errors to the solver if the module cannot be found or
     * the require is illegal.
     * @param module the module information to look up.
     * @param location the location where the require is taking place; used for
     * error locations.
     **/
    TypeId resolveModule(const ModuleInfo& module, const Location& location);

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

    TypeId errorRecoveryType() const;
    TypePackId errorRecoveryTypePack() const;

    TypeId unionOfTypes(TypeId a, TypeId b, NotNull<Scope> scope, bool unifyFreeTypes);

    ToStringOptions opts;
};

void dump(NotNull<Scope> rootScope, struct ToStringOptions& opts);

} // namespace Luau
