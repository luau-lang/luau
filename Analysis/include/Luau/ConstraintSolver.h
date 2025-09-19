// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Constraint.h"
#include "Luau/ConstraintSet.h"
#include "Luau/DataFlowGraph.h"
#include "Luau/DenseHash.h"
#include "Luau/EqSatSimplification.h"
#include "Luau/Error.h"
#include "Luau/Location.h"
#include "Luau/Module.h"
#include "Luau/Normalize.h"
#include "Luau/OrderedSet.h"
#include "Luau/Substitution.h"
#include "Luau/SubtypingVariance.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeFwd.h"
#include "Luau/Variant.h"

#include <utility>
#include <vector>

namespace Luau
{

enum class ValueContext;

struct DcrLogger;

class AstExpr;

// TypeId, TypePackId, or Constraint*. It is impossible to know which, but we
// never dereference this pointer.
using BlockedConstraintId = Variant<TypeId, TypePackId, const Constraint*>;

struct HashBlockedConstraintId
{
    size_t operator()(const BlockedConstraintId& bci) const;
};

struct SubtypeConstraintRecord
{
    TypeId subTy = nullptr;
    TypeId superTy = nullptr;
    SubtypingVariance variance = SubtypingVariance::Invalid;

    bool operator==(const SubtypeConstraintRecord& other) const;
};

struct HashSubtypeConstraintRecord
{
    size_t operator()(const SubtypeConstraintRecord& c) const;
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


struct TablePropLookupResult
{
    // What types are we blocked on for determining this type?
    std::vector<TypeId> blockedTypes;
    // The type of the property (if we were able to determine it).
    std::optional<TypeId> propType;
    // Whether or not this is _definitely_ derived as the result of an indexer.
    // We use this to determine whether or not code like:
    //
    //   t.lol = nil;
    //
    // ... is legal. If `t: { [string]: ~nil }` then this is legal as
    // there's no guarantee on whether "lol" specifically exists.
    // However, if `t: { lol: ~nil }`, then we cannot allow assignment as
    // that would remove "lol" from the table entirely.
    bool isIndex = false;
};

struct ConstraintSolver
{
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
    InternalErrorReporter iceReporter;
    NotNull<Normalizer> normalizer;
    NotNull<Simplifier> simplifier;
    NotNull<TypeFunctionRuntime> typeFunctionRuntime;
    // The entire set of constraints that the solver is trying to resolve.
    ConstraintSet constraintSet;
    std::vector<NotNull<Constraint>> constraints;
    NotNull<DenseHashMap<Scope*, TypeId>> scopeToFunction;
    NotNull<Scope> rootScope;
    ModulePtr module;

    // The dataflow graph of the program, used in constraint generation and for magic functions.
    NotNull<const DataFlowGraph> dfg;

    // Constraints that the solver has generated, rather than sourcing from the
    // scope tree.
    std::vector<std::unique_ptr<Constraint>> solverConstraints;

    // Ticks downward toward zero each time a new constraint is pushed into
    // solverConstraints. When this counter reaches zero, the type inference
    // engine reports a CodeTooComplex error and aborts.
    size_t solverConstraintLimit = 0;

    // This includes every constraint that has not been fully solved.
    // A constraint can be both blocked and unsolved, for instance.
    std::vector<NotNull<const Constraint>> unsolvedConstraints;

    // A mapping of constraint pointer to how many things the constraint is
    // blocked on. Can be empty or 0 for constraints that are not blocked on
    // anything.
    std::unordered_map<NotNull<const Constraint>, size_t> blockedConstraints;
    // A mapping of type/pack pointers to the constraints they block.
    std::unordered_map<BlockedConstraintId, DenseHashSet<const Constraint*>, HashBlockedConstraintId> blocked;
    // Memoized instantiations of type aliases.
    DenseHashMap<InstantiationSignature, TypeId, HashInstantiationSignature> instantiatedAliases{{}};
    // Breadcrumbs for where a free type's upper bound was expanded. We use
    // these to provide more helpful error messages when a free type is solved
    // as never unexpectedly.
    DenseHashMap<TypeId, std::vector<std::pair<Location, TypeId>>> upperBoundContributors{nullptr};

    // A mapping from free types to the number of unresolved constraints that mention them.
    DenseHashMap<TypeId, size_t> unresolvedConstraints{{}};

    std::unordered_map<NotNull<const Constraint>, TypeIds> maybeMutatedFreeTypes;
    std::unordered_map<TypeId, OrderedSet<const Constraint*>> mutatedFreeTypeToConstraint;

    // Irreducible/uninhabited type functions or type pack functions.
    DenseHashSet<const void*> uninhabitedTypeFunctions{{}};

    DenseHashMap<SubtypeConstraintRecord, Constraint*, HashSubtypeConstraintRecord> seenConstraints{{}};

    // The set of types that will definitely be unchanged by generalization.
    DenseHashSet<TypeId> generalizedTypes_{nullptr};
    const NotNull<DenseHashSet<TypeId>> generalizedTypes{&generalizedTypes_};

    // Recorded errors that take place within the solver.
    ErrorVec errors;

    NotNull<ModuleResolver> moduleResolver;
    std::vector<RequireCycle> requireCycles;

    DcrLogger* logger;
    TypeCheckLimits limits;

    DenseHashMap<TypeId, const Constraint*> typeFunctionsToFinalize{nullptr};

    explicit ConstraintSolver(
        NotNull<Normalizer> normalizer,
        NotNull<Simplifier> simplifier,
        NotNull<TypeFunctionRuntime> typeFunctionRuntime,
        ModulePtr module,
        NotNull<ModuleResolver> moduleResolver,
        std::vector<RequireCycle> requireCycles,
        DcrLogger* logger,
        NotNull<const DataFlowGraph> dfg,
        TypeCheckLimits limits,
        ConstraintSet constraintSet
    );

    // TODO CLI-169086: Replace all uses of this constructor with the ConstraintSet constructor, above.
    explicit ConstraintSolver(
        NotNull<Normalizer> normalizer,
        NotNull<Simplifier> simplifier,
        NotNull<TypeFunctionRuntime> typeFunctionRuntime,
        NotNull<Scope> rootScope,
        std::vector<NotNull<Constraint>> constraints,
        NotNull<DenseHashMap<Scope*, TypeId>> scopeToFunction,
        ModulePtr module,
        NotNull<ModuleResolver> moduleResolver,
        std::vector<RequireCycle> requireCycles,
        DcrLogger* logger,
        NotNull<const DataFlowGraph> dfg,
        TypeCheckLimits limits
    );

    // Randomize the order in which to dispatch constraints
    void randomize(unsigned seed);

    /**
     * Attempts to dispatch all pending constraints and reach a type solution
     * that satisfies all of the constraints.
     **/
    void run();


    /**
     * Attempts to perform one final reduction on type functions after every constraint has been completed
     *
     **/
    void finalizeTypeFunctions();

    bool isDone() const;

private:
    /// A helper that does most of the setup work that is shared between the two constructors.
    void initFreeTypeTracking();

    void generalizeOneType(TypeId ty);

    /**
     * Bind a type variable to another type.
     *
     * A constraint is required and will validate that blockedTy is owned by this
     * constraint. This prevents one constraint from interfering with another's
     * blocked types.
     *
     * Bind will also unblock the type variable for you.
     */
    void bind(NotNull<const Constraint> constraint, TypeId ty, TypeId boundTo);
    void bind(NotNull<const Constraint> constraint, TypePackId tp, TypePackId boundTo);

    template<typename T, typename... Args>
    void emplace(NotNull<const Constraint> constraint, TypeId ty, Args&&... args);

    template<typename T, typename... Args>
    void emplace(NotNull<const Constraint> constraint, TypePackId tp, Args&&... args);

public:
    /** Attempt to dispatch a constraint.  Returns true if it was successful. If
     * tryDispatch() returns false, the constraint remains in the unsolved set
     * and will be retried later.
     */
    bool tryDispatch(NotNull<const Constraint> c, bool force);

    bool tryDispatch(const SubtypeConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const PackSubtypeConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const GeneralizationConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const IterableConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const NameConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const TypeAliasExpansionConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const FunctionCallConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const FunctionCheckConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const PrimitiveTypeConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const HasPropConstraint& c, NotNull<const Constraint> constraint);


    bool tryDispatchHasIndexer(
        int& recursionDepth,
        NotNull<const Constraint> constraint,
        TypeId subjectType,
        TypeId indexType,
        TypeId resultType,
        Set<TypeId>& seen
    );
    bool tryDispatch(const HasIndexerConstraint& c, NotNull<const Constraint> constraint);

    bool tryDispatch(const AssignPropConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const AssignIndexConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const UnpackConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const ReduceConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const ReducePackConstraint& c, NotNull<const Constraint> constraint, bool force);
    bool tryDispatch(const EqualityConstraint& c, NotNull<const Constraint> constraint);

    bool tryDispatch(const SimplifyConstraint& c, NotNull<const Constraint> constraint, bool force);

    bool tryDispatch(const PushFunctionTypeConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const PushTypeConstraint& c, NotNull<const Constraint> constraint, bool force);

    // for a, ... in some_table do
    // also handles __iter metamethod
    bool tryDispatchIterableTable(TypeId iteratorTy, const IterableConstraint& c, NotNull<const Constraint> constraint, bool force);

    // for a, ... in next_function, t, ... do
    bool tryDispatchIterableFunction(TypeId nextTy, TypeId tableTy, const IterableConstraint& c, NotNull<const Constraint> constraint);

    TablePropLookupResult lookupTableProp(
        NotNull<const Constraint> constraint,
        TypeId subjectType,
        const std::string& propName,
        ValueContext context,
        bool inConditional = false,
        bool suppressSimplification = false
    );

    TablePropLookupResult lookupTableProp(
        NotNull<const Constraint> constraint,
        TypeId subjectType,
        const std::string& propName,
        ValueContext context,
        bool inConditional,
        bool suppressSimplification,
        Set<TypeId>& seen
    );

    /**
     * Generate constraints to unpack the types of srcTypes and assign each
     * value to the corresponding BlockedType in destTypes.
     *
     * This function also overwrites the owners of each BlockedType.  This is
     * okay because this function is only used to decompose IterableConstraint
     * into an UnpackConstraint.
     *
     * @param destTypes A vector of types comprised of BlockedTypes.
     * @param srcTypes A TypePack that represents rvalues to be assigned.
     * @returns The underlying UnpackConstraint.  There's a bit of code in
     * iteration that needs to pass blocks on to this constraint.
     */
    NotNull<const Constraint> unpackAndAssign(const std::vector<TypeId> destTypes, TypePackId srcTypes, NotNull<const Constraint> constraint);

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
    bool blockOnPendingTypes(TypePackId targetPack, NotNull<const Constraint> constraint);

    void unblock(NotNull<const Constraint> progressed);
    void unblock(TypeId ty, Location location);
    void unblock(TypePackId progressed, Location location);
    void unblock(const std::vector<TypeId>& types, Location location);
    void unblock(const std::vector<TypePackId>& packs, Location location);

    /**
     * @returns true if the TypeId is in a blocked state.
     */
    bool isBlocked(TypeId ty) const;

    /**
     * @returns true if the TypePackId is in a blocked state.
     */
    bool isBlocked(TypePackId tp) const;

    /**
     * Returns whether the constraint is blocked on anything.
     * @param constraint the constraint to check.
     */
    bool isBlocked(NotNull<const Constraint> constraint) const;

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
    TypeId resolveModule(const ModuleInfo& info, const Location& location);

    void reportError(TypeErrorData&& data, const Location& location);
    void reportError(TypeError e);

    /**
     * Shifts the count of references from `source` to `target`. This should be paired
     * with any instance of binding a free type in order to maintain accurate refcounts.
     * If `target` is not a free type, this is a noop.
     * @param source the free type which is being bound
     * @param target the type which the free type is being bound to
     */
    void shiftReferences(TypeId source, TypeId target);

    /**
     * Generalizes the given free type if the reference counting allows it.
     * @param the scope to generalize in
     * @param type the free type we want to generalize
     * @returns a non-free type that generalizes the argument, or `std::nullopt` if one
     * does not exist
     */
    std::optional<TypeId> generalizeFreeType(NotNull<Scope> scope, TypeId type);

    /**
     * Checks the existing set of constraints to see if there exist any that contain
     * the provided free type, indicating that it is not yet ready to be replaced by
     * one of its bounds.
     * @param ty the free type that to check for related constraints
     * @returns whether or not it is unsafe to replace the free type by one of its bounds
     */
    bool hasUnresolvedConstraints(TypeId ty);

    /** Attempts to unify subTy with superTy.  If doing so would require unifying
     * BlockedTypes, fail and block the constraint on those BlockedTypes.
     *
     * Note: TID can only be TypeId or TypePackId.
     *
     * If unification fails, replace all free types with errorType.
     *
     * If unification succeeds, unblock every type changed by the unification.
     *
     * @returns true if the unification succeeded.  False if the unification was
     * too complex.
     */
    template<typename TID>
    bool unify(NotNull<const Constraint> constraint, TID subTy, TID superTy);

    /**
     * Marks a constraint as being blocked on a type or type pack. The constraint
     * solver will not attempt to dispatch blocked constraints until their
     * dependencies have made progress.
     * @param target the type or type pack pointer that the constraint is blocked on.
     * @param constraint the constraint to block.
     **/
    bool block_(BlockedConstraintId target, NotNull<const Constraint> constraint);

    /**
     * Informs the solver that progress has been made on a type or type pack. The
     * solver will wake up all constraints that are blocked on the type or type pack,
     * and will resume attempting to dispatch them.
     * @param progressed the type or type pack pointer that has progressed.
     **/
    void unblock_(BlockedConstraintId progressed);

    /**
     * Reproduces any constraints necessary for new types that are copied when applying a substitution.
     * At the time of writing, this pertains only to type functions.
     * @param subst the substitution that was applied
     **/
    void reproduceConstraints(NotNull<Scope> scope, const Location& location, const Substitution& subst);

    TypeId simplifyIntersection(NotNull<Scope> scope, Location location, TypeId left, TypeId right);
    TypeId simplifyIntersection(NotNull<Scope> scope, Location location, std::set<TypeId> parts);
    TypeId simplifyUnion(NotNull<Scope> scope, Location location, TypeId left, TypeId right);

    TypePackId anyifyModuleReturnTypePackGenerics(TypePackId tp);

    void throwTimeLimitError() const;
    void throwUserCancelError() const;

    ToStringOptions opts;

    void fillInDiscriminantTypes(NotNull<const Constraint> constraint, const std::vector<std::optional<TypeId>>& discriminantTypes);
};

/** Borrow a vector of pointers from a vector of owning pointers to constraints.
 */
std::vector<NotNull<Constraint>> borrowConstraints(const std::vector<ConstraintPtr>& constraints);

std::pair<std::vector<TypeId>, std::vector<TypePackId>> saturateArguments(
    TypeArena* arena,
    NotNull<BuiltinTypes> builtinTypes,
    const TypeFun& fn,
    const std::vector<TypeId>& rawTypeArguments,
    const std::vector<TypePackId>& rawPackArguments
);

void dump(NotNull<Scope> rootScope, struct ToStringOptions& opts);

} // namespace Luau
