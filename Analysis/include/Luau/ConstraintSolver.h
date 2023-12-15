// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Constraint.h"
#include "Luau/DenseHash.h"
#include "Luau/Error.h"
#include "Luau/Location.h"
#include "Luau/Module.h"
#include "Luau/Normalize.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeFwd.h"
#include "Luau/Variant.h"

#include <utility>
#include <vector>

namespace Luau
{

struct DcrLogger;

// TypeId, TypePackId, or Constraint*. It is impossible to know which, but we
// never dereference this pointer.
using BlockedConstraintId = Variant<TypeId, TypePackId, const Constraint*>;

struct HashBlockedConstraintId
{
    size_t operator()(const BlockedConstraintId& bci) const;
};

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
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
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
    std::unordered_map<BlockedConstraintId, std::vector<NotNull<const Constraint>>, HashBlockedConstraintId> blocked;
    // Memoized instantiations of type aliases.
    DenseHashMap<InstantiationSignature, TypeId, HashInstantiationSignature> instantiatedAliases{{}};
    // Breadcrumbs for where a free type's upper bound was expanded. We use
    // these to provide more helpful error messages when a free type is solved
    // as never unexpectedly.
    DenseHashMap<TypeId, std::vector<std::pair<Location, TypeId>>> upperBoundContributors{nullptr};

    // A mapping from free types to the number of unresolved constraints that mention them.
    DenseHashMap<TypeId, size_t> unresolvedConstraints{{}};

    // Recorded errors that take place within the solver.
    ErrorVec errors;

    NotNull<ModuleResolver> moduleResolver;
    std::vector<RequireCycle> requireCycles;

    DcrLogger* logger;
    TypeCheckLimits limits;

    explicit ConstraintSolver(NotNull<Normalizer> normalizer, NotNull<Scope> rootScope, std::vector<NotNull<Constraint>> constraints,
        ModuleName moduleName, NotNull<ModuleResolver> moduleResolver, std::vector<RequireCycle> requireCycles, DcrLogger* logger,
        TypeCheckLimits limits);

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
    bool tryDispatch(const IterableConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const NameConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const TypeAliasExpansionConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const FunctionCallConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const PrimitiveTypeConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const HasPropConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const SetPropConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const SetIndexerConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const SingletonOrTopTypeConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const UnpackConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const SetOpConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const ReduceConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const ReducePackConstraint& c, NotNull<const Constraint> constraint, bool force);

    // for a, ... in some_table do
    // also handles __iter metamethod
    bool tryDispatchIterableTable(TypeId iteratorTy, const IterableConstraint& c, NotNull<const Constraint> constraint, bool force);

    // for a, ... in next_function, t, ... do
    bool tryDispatchIterableFunction(
        TypeId nextTy, TypeId tableTy, TypeId firstIndexTy, const IterableConstraint& c, NotNull<const Constraint> constraint, bool force);

    std::pair<std::vector<TypeId>, std::optional<TypeId>> lookupTableProp(
        TypeId subjectType, const std::string& propName, bool suppressSimplification = false);
    std::pair<std::vector<TypeId>, std::optional<TypeId>> lookupTableProp(
        TypeId subjectType, const std::string& propName, bool suppressSimplification, DenseHashSet<TypeId>& seen);

    void block(NotNull<const Constraint> target, NotNull<const Constraint> constraint);
    /**
     * Block a constraint on the resolution of a Type.
     * @returns false always.  This is just to allow tryDispatch to return the result of block()
     */
    bool block(TypeId target, NotNull<const Constraint> constraint);
    bool block(TypePackId target, NotNull<const Constraint> constraint);

    // Block on every target
    template<typename T>
    bool block(const T& targets, NotNull<const Constraint> constraint)
    {
        for (TypeId target : targets)
            block(target, constraint);

        return false;
    }

    /**
     * For all constraints that are blocked on one constraint, make them block
     * on a new constraint.
     * @param source the constraint to copy blocks from.
     * @param addition the constraint that other constraints should now block on.
     */
    void inheritBlocks(NotNull<const Constraint> source, NotNull<const Constraint> addition);

    // Traverse the type.  If any pending types are found, block the constraint
    // on them.
    //
    // Returns false if a type blocks the constraint.
    //
    // FIXME: This use of a boolean for the return result is an appalling
    // interface.
    bool blockOnPendingTypes(TypeId target, NotNull<const Constraint> constraint);
    bool blockOnPendingTypes(TypePackId target, NotNull<const Constraint> constraint);

    void unblock(NotNull<const Constraint> progressed);
    void unblock(TypeId progressed, Location location);
    void unblock(TypePackId progressed, Location location);
    void unblock(const std::vector<TypeId>& types, Location location);
    void unblock(const std::vector<TypePackId>& packs, Location location);

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
     * @returns optionally a unification too complex error if unification failed
     */
    std::optional<TypeError> unify(NotNull<Scope> scope, Location location, TypeId subType, TypeId superType);

    /**
     * Creates a new Unifier and performs a single unification operation. Commits
     * the result.
     * @param subPack the sub-type pack to unify.
     * @param superPack the super-type pack to unify.
     */
    ErrorVec unify(NotNull<Scope> scope, Location location, TypePackId subPack, TypePackId superPack);

    /** Pushes a new solver constraint to the solver.
     * @param cv the body of the constraint.
     **/
    NotNull<Constraint> pushConstraint(NotNull<Scope> scope, const Location& location, ConstraintV cv);

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

    /**
     * Checks the existing set of constraints to see if there exist any that contain
     * the provided free type, indicating that it is not yet ready to be replaced by
     * one of its bounds.
     * @param ty the free type that to check for related constraints
     * @returns whether or not it is unsafe to replace the free type by one of its bounds
     */
    bool hasUnresolvedConstraints(TypeId ty);

private:

    /** Helper used by tryDispatch(SubtypeConstraint) and
     * tryDispatch(PackSubtypeConstraint)
     *
     * Attempts to unify subTy with superTy.  If doing so would require unifying
     * BlockedTypes, fail and block the constraint on those BlockedTypes.
     *
     * If unification fails, replace all free types with errorType.
     *
     * If unification succeeds, unblock every type changed by the unification.
     */
    template <typename TID>
    bool tryUnify(NotNull<const Constraint> constraint, TID subTy, TID superTy);

    /**
     * Bind a BlockedType to another type while taking care not to bind it to
     * itself in the case that resultTy == blockedTy.  This can happen if we
     * have a tautological constraint.  When it does, we must instead bind
     * blockedTy to a fresh type belonging to an appropriate scope.
     *
     * To determine which scope is appropriate, we also accept rootTy, which is
     * to be the type that contains blockedTy.
     */
    void bindBlockedType(TypeId blockedTy, TypeId resultTy, TypeId rootTy, Location location);

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

    TypePackId anyifyModuleReturnTypePackGenerics(TypePackId tp);

    void throwTimeLimitError();
    void throwUserCancelError();

    ToStringOptions opts;
};

void dump(NotNull<Scope> rootScope, struct ToStringOptions& opts);

} // namespace Luau
