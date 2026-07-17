// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Constraint.h"
#include "Luau/ConstraintGraph.h"
#include "Luau/ConstraintSet.h"
#include "Luau/DataFlowGraph.h"
#include "Luau/DenseHash.h"
#include "Luau/Error.h"
#include "Luau/Location.h"
#include "Luau/Module.h"
#include "Luau/Normalize.h"
#include "Luau/Substitution.h"
#include "Luau/Subtyping.h"
#include "Luau/SubtypingVariance.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeFwd.h"

#include <utility>
#include <vector>

namespace Luau
{

enum class ValueContext;

struct DcrLogger;

class AstExpr;

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

    // Memoized instantiations of type aliases.
    DenseHashMap<InstantiationSignature, TypeId, HashInstantiationSignature> instantiatedAliases{{}};
    // Breadcrumbs for where a free type's upper bound was expanded. We use
    // these to provide more helpful error messages when a free type is solved
    // as never unexpectedly.
    DenseHashMap<TypeId, std::vector<std::pair<Location, TypeId>>> upperBoundContributors{nullptr};

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
        NotNull<TypeFunctionRuntime> typeFunctionRuntime,
        ModulePtr module,
        NotNull<ModuleResolver> moduleResolver,
        std::vector<RequireCycle> requireCycles,
        DcrLogger* logger,
        NotNull<const DataFlowGraph> dfg,
        TypeCheckLimits limits,
        ConstraintSet constraintSet,
        NotNull<ConstraintGraph> cgraph,
        NotNull<Subtyping> subtyping
    );

    // TODO CLI-169086: Replace all uses of this constructor with the ConstraintSet constructor, above.
    explicit ConstraintSolver(
        NotNull<Normalizer> normalizer,
        NotNull<TypeFunctionRuntime> typeFunctionRuntime,
        NotNull<Scope> rootScope,
        std::vector<NotNull<Constraint>> constraints,
        NotNull<DenseHashMap<Scope*, TypeId>> scopeToFunction,
        ModulePtr module,
        NotNull<ModuleResolver> moduleResolver,
        std::vector<RequireCycle> requireCycles,
        DcrLogger* logger,
        NotNull<const DataFlowGraph> dfg,
        TypeCheckLimits limits,
        NotNull<ConstraintGraph> cgraph,
        NotNull<Subtyping> subtyping
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

    // Clip with LuauRemoveConstraintSolverEmplace
    template<typename T, typename... Args>
    void DEPRECATED_emplace(NotNull<const Constraint> constraint, TypeId ty, Args&&... args);

    // Clip with LuauRemoveConstraintSolverEmplace
    template<typename T, typename... Args>
    void DEPRECATED_emplace(NotNull<const Constraint> constraint, TypePackId tp, Args&&... args);

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
    // Clip with LuauRemovePrimitiveTypeConstraint
    bool DEPRECATED_tryDispatch(const DEPRECATED_PrimitiveTypeConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const HasPropConstraint& c, NotNull<const Constraint> constraint);
    bool tryDispatch(const TypeInstantiationConstraint& c, NotNull<const Constraint> constraint);

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

    void unblock(TypeId ty, Location location);
    void unblock(TypePackId progressed, Location location);

    /**
     * @returns true if the TypeId is in a blocked state.
     */
    bool isBlocked(TypeId ty) const;

    /**
     * @returns true if the TypePackId is in a blocked state.
     */
    bool isBlocked(TypePackId tp) const;

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
     * Reproduces any constraints necessary for new types that are copied when applying a substitution.
     * At the time of writing, this pertains only to type functions.
     * @param subst the substitution that was applied
     **/
    void reproduceConstraints(NotNull<Scope> scope, const Location& location, const Substitution& subst);

    TypeId simplifyIntersection(NotNull<Scope> scope, Location location, TypeId left, TypeId right);

    TypeId simplifyIntersection(NotNull<Scope> scope, Location location, TypeIds parts);

    TypeId simplifyUnion(NotNull<Scope> scope, Location location, TypeId left, TypeId right);

    TypeId instantiateFunctionType(
        TypeId functionTypeId,
        const std::vector<TypeId>& typeArguments,
        const std::vector<TypePackId>& typePackArguments,
        NotNull<Scope> scope,
        const Location& location
    );

    TypePackId anyifyModuleReturnTypePackGenerics(TypePackId tp);

    void throwTimeLimitError() const;
    void throwUserCancelError() const;

    ToStringOptions opts;

    NotNull<ConstraintGraph> cgraph;

    NotNull<Subtyping> subtyping;

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
