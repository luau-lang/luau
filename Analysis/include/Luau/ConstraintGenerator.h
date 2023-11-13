// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Constraint.h"
#include "Luau/ControlFlow.h"
#include "Luau/DataFlowGraph.h"
#include "Luau/InsertionOrderedMap.h"
#include "Luau/Module.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Normalize.h"
#include "Luau/NotNull.h"
#include "Luau/Refinement.h"
#include "Luau/Symbol.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeUtils.h"
#include "Luau/Variant.h"
#include "Luau/Normalize.h"

#include <memory>
#include <vector>
#include <unordered_map>

namespace Luau
{

struct Scope;
using ScopePtr = std::shared_ptr<Scope>;

struct DcrLogger;

struct Inference
{
    TypeId ty = nullptr;
    RefinementId refinement = nullptr;

    Inference() = default;

    explicit Inference(TypeId ty, RefinementId refinement = nullptr)
        : ty(ty)
        , refinement(refinement)
    {
    }
};

struct InferencePack
{
    TypePackId tp = nullptr;
    std::vector<RefinementId> refinements;

    InferencePack() = default;

    explicit InferencePack(TypePackId tp, const std::vector<RefinementId>& refinements = {})
        : tp(tp)
        , refinements(refinements)
    {
    }
};

struct ConstraintGenerator
{
    // A list of all the scopes in the module. This vector holds ownership of the
    // scope pointers; the scopes themselves borrow pointers to other scopes to
    // define the scope hierarchy.
    std::vector<std::pair<Location, ScopePtr>> scopes;

    ModulePtr module;
    NotNull<BuiltinTypes> builtinTypes;
    const NotNull<TypeArena> arena;
    // The root scope of the module we're generating constraints for.
    // This is null when the CG is initially constructed.
    Scope* rootScope;

    struct InferredBinding
    {
        Scope* scope;
        Location location;
        TypeIds types;
    };

    // During constraint generation, we only populate the Scope::bindings
    // property for annotated symbols.  Unannotated symbols must be handled in a
    // postprocessing step because we have not yet allocated the types that will
    // be assigned to those unannotated symbols, so we queue them up here.
    std::map<Symbol, InferredBinding> inferredBindings;

    // Constraints that go straight to the solver.
    std::vector<ConstraintPtr> constraints;

    // Constraints that do not go to the solver right away.  Other constraints
    // will enqueue them during solving.
    std::vector<ConstraintPtr> unqueuedConstraints;

    // The private scope of type aliases for which the type parameters belong to.
    DenseHashMap<const AstStatTypeAlias*, ScopePtr> astTypeAliasDefiningScopes{nullptr};

    NotNull<const DataFlowGraph> dfg;
    RefinementArena refinementArena;

    int recursionCount = 0;

    // It is pretty uncommon for constraint generation to itself produce errors, but it can happen.
    std::vector<TypeError> errors;

    // Needed to be able to enable error-suppression preservation for immediate refinements.
    NotNull<Normalizer> normalizer;
    // Needed to resolve modules to make 'require' import types properly.
    NotNull<ModuleResolver> moduleResolver;
    // Occasionally constraint generation needs to produce an ICE.
    const NotNull<InternalErrorReporter> ice;

    ScopePtr globalScope;

    std::function<void(const ModuleName&, const ScopePtr&)> prepareModuleScope;
    std::vector<RequireCycle> requireCycles;

    DcrLogger* logger;

    ConstraintGenerator(ModulePtr module, NotNull<Normalizer> normalizer, NotNull<ModuleResolver> moduleResolver,
        NotNull<BuiltinTypes> builtinTypes, NotNull<InternalErrorReporter> ice, const ScopePtr& globalScope,
        std::function<void(const ModuleName&, const ScopePtr&)> prepareModuleScope, DcrLogger* logger, NotNull<DataFlowGraph> dfg,
        std::vector<RequireCycle> requireCycles);

    /**
     * The entry point to the ConstraintGenerator. This will construct a set
     * of scopes, constraints, and free types that can be solved later.
     * @param block the root block to generate constraints for.
     */
    void visitModuleRoot(AstStatBlock* block);

private:
    /**
     * Fabricates a new free type belonging to a given scope.
     * @param scope the scope the free type belongs to.
     */
    TypeId freshType(const ScopePtr& scope);

    /**
     * Fabricates a new free type pack belonging to a given scope.
     * @param scope the scope the free type pack belongs to.
     */
    TypePackId freshTypePack(const ScopePtr& scope);

    /**
     * Fabricates a scope that is a child of another scope.
     * @param node the lexical node that the scope belongs to.
     * @param parent the parent scope of the new scope. Must not be null.
     */
    ScopePtr childScope(AstNode* node, const ScopePtr& parent);

    std::optional<TypeId> lookup(Scope* scope, DefId def);

    /**
     * Adds a new constraint with no dependencies to a given scope.
     * @param scope the scope to add the constraint to.
     * @param cv the constraint variant to add.
     * @return the pointer to the inserted constraint
     */
    NotNull<Constraint> addConstraint(const ScopePtr& scope, const Location& location, ConstraintV cv);

    /**
     * Adds a constraint to a given scope.
     * @param scope the scope to add the constraint to. Must not be null.
     * @param c the constraint to add.
     * @return the pointer to the inserted constraint
     */
    NotNull<Constraint> addConstraint(const ScopePtr& scope, std::unique_ptr<Constraint> c);

    struct RefinementPartition
    {
        // Types that we want to intersect against the type of the expression.
        std::vector<TypeId> discriminantTypes;

        // Sometimes the type we're discriminating against is implicitly nil.
        bool shouldAppendNilType = false;
    };

    using RefinementContext = InsertionOrderedMap<DefId, RefinementPartition>;
    void unionRefinements(const RefinementContext& lhs, const RefinementContext& rhs, RefinementContext& dest, std::vector<ConstraintV>* constraints);
    void computeRefinement(const ScopePtr& scope, RefinementId refinement, RefinementContext* refis, bool sense, bool eq, std::vector<ConstraintV>* constraints);
    void applyRefinements(const ScopePtr& scope, Location location, RefinementId refinement);

    ControlFlow visitBlockWithoutChildScope(const ScopePtr& scope, AstStatBlock* block);

    ControlFlow visit(const ScopePtr& scope, AstStat* stat);
    ControlFlow visit(const ScopePtr& scope, AstStatBlock* block);
    ControlFlow visit(const ScopePtr& scope, AstStatLocal* local);
    ControlFlow visit(const ScopePtr& scope, AstStatFor* for_);
    ControlFlow visit(const ScopePtr& scope, AstStatForIn* forIn);
    ControlFlow visit(const ScopePtr& scope, AstStatWhile* while_);
    ControlFlow visit(const ScopePtr& scope, AstStatRepeat* repeat);
    ControlFlow visit(const ScopePtr& scope, AstStatLocalFunction* function);
    ControlFlow visit(const ScopePtr& scope, AstStatFunction* function);
    ControlFlow visit(const ScopePtr& scope, AstStatReturn* ret);
    ControlFlow visit(const ScopePtr& scope, AstStatAssign* assign);
    ControlFlow visit(const ScopePtr& scope, AstStatCompoundAssign* assign);
    ControlFlow visit(const ScopePtr& scope, AstStatIf* ifStatement);
    ControlFlow visit(const ScopePtr& scope, AstStatTypeAlias* alias);
    ControlFlow visit(const ScopePtr& scope, AstStatDeclareGlobal* declareGlobal);
    ControlFlow visit(const ScopePtr& scope, AstStatDeclareClass* declareClass);
    ControlFlow visit(const ScopePtr& scope, AstStatDeclareFunction* declareFunction);
    ControlFlow visit(const ScopePtr& scope, AstStatError* error);

    InferencePack checkPack(const ScopePtr& scope, AstArray<AstExpr*> exprs, const std::vector<std::optional<TypeId>>& expectedTypes = {});
    InferencePack checkPack(
        const ScopePtr& scope, AstExpr* expr, const std::vector<std::optional<TypeId>>& expectedTypes = {}, bool generalize = true);

    InferencePack checkPack(const ScopePtr& scope, AstExprCall* call);

    /**
     * Checks an expression that is expected to evaluate to one type.
     * @param scope the scope the expression is contained within.
     * @param expr the expression to check.
     * @param expectedType the type of the expression that is expected from its
     *      surrounding context.  Used to implement bidirectional type checking.
     * @param generalize If true, generalize any lambdas that are encountered.
     * @return the type of the expression.
     */
    Inference check(
        const ScopePtr& scope, AstExpr* expr, std::optional<TypeId> expectedType = {}, bool forceSingleton = false, bool generalize = true);

    Inference check(const ScopePtr& scope, AstExprConstantString* string, std::optional<TypeId> expectedType, bool forceSingleton);
    Inference check(const ScopePtr& scope, AstExprConstantBool* bool_, std::optional<TypeId> expectedType, bool forceSingleton);
    Inference check(const ScopePtr& scope, AstExprLocal* local);
    Inference check(const ScopePtr& scope, AstExprGlobal* global);
    Inference check(const ScopePtr& scope, AstExprIndexName* indexName);
    Inference check(const ScopePtr& scope, AstExprIndexExpr* indexExpr);
    Inference check(const ScopePtr& scope, AstExprFunction* func, std::optional<TypeId> expectedType, bool generalize);
    Inference check(const ScopePtr& scope, AstExprUnary* unary);
    Inference check(const ScopePtr& scope, AstExprBinary* binary, std::optional<TypeId> expectedType);
    Inference check(const ScopePtr& scope, AstExprIfElse* ifElse, std::optional<TypeId> expectedType);
    Inference check(const ScopePtr& scope, AstExprTypeAssertion* typeAssert);
    Inference check(const ScopePtr& scope, AstExprInterpString* interpString);
    Inference check(const ScopePtr& scope, AstExprTable* expr, std::optional<TypeId> expectedType);
    std::tuple<TypeId, TypeId, RefinementId> checkBinary(const ScopePtr& scope, AstExprBinary* binary, std::optional<TypeId> expectedType);

    /**
     * Generate constraints to assign assignedTy to the expression expr
     * @returns the type of the expression.  This may or may not be assignedTy itself.
     */
    std::optional<TypeId> checkLValue(const ScopePtr& scope, AstExpr* expr, TypeId assignedTy);
    std::optional<TypeId> checkLValue(const ScopePtr& scope, AstExprLocal* local, TypeId assignedTy);
    std::optional<TypeId> checkLValue(const ScopePtr& scope, AstExprGlobal* global, TypeId assignedTy);
    std::optional<TypeId> checkLValue(const ScopePtr& scope, AstExprIndexName* indexName, TypeId assignedTy);
    std::optional<TypeId> checkLValue(const ScopePtr& scope, AstExprIndexExpr* indexExpr, TypeId assignedTy);
    TypeId updateProperty(const ScopePtr& scope, AstExpr* expr, TypeId assignedTy);

    void updateLValueType(AstExpr* lvalue, TypeId ty);

    struct FunctionSignature
    {
        // The type of the function.
        TypeId signature;
        // The scope that encompasses the function's signature. May be nullptr
        // if there was no need for a signature scope (the function has no
        // generics).
        ScopePtr signatureScope;
        // The scope that encompasses the function's body. Is a child scope of
        // signatureScope, if present.
        ScopePtr bodyScope;
    };

    FunctionSignature checkFunctionSignature(
        const ScopePtr& parent, AstExprFunction* fn, std::optional<TypeId> expectedType = {}, std::optional<Location> originalName = {});

    /**
     * Checks the body of a function expression.
     * @param scope the interior scope of the body of the function.
     * @param fn the function expression to check.
     */
    void checkFunctionBody(const ScopePtr& scope, AstExprFunction* fn);

    /**
     * Resolves a type from its AST annotation.
     * @param scope the scope that the type annotation appears within.
     * @param ty the AST annotation to resolve.
     * @param inTypeArguments whether we are resolving a type that's contained within type arguments, `<...>`.
     * @return the type of the AST annotation.
     **/
    TypeId resolveType(const ScopePtr& scope, AstType* ty, bool inTypeArguments, bool replaceErrorWithFresh = false);

    /**
     * Resolves a type pack from its AST annotation.
     * @param scope the scope that the type annotation appears within.
     * @param tp the AST annotation to resolve.
     * @param inTypeArguments whether we are resolving a type that's contained within type arguments, `<...>`.
     * @return the type pack of the AST annotation.
     **/
    TypePackId resolveTypePack(const ScopePtr& scope, AstTypePack* tp, bool inTypeArguments, bool replaceErrorWithFresh = false);

    /**
     * Resolves a type pack from its AST annotation.
     * @param scope the scope that the type annotation appears within.
     * @param list the AST annotation to resolve.
     * @param inTypeArguments whether we are resolving a type that's contained within type arguments, `<...>`.
     * @return the type pack of the AST annotation.
     **/
    TypePackId resolveTypePack(const ScopePtr& scope, const AstTypeList& list, bool inTypeArguments, bool replaceErrorWithFresh = false);

    /**
     * Creates generic types given a list of AST definitions, resolving default
     * types as required.
     * @param scope the scope that the generics should belong to.
     * @param generics the AST generics to create types for.
     * @param useCache whether to use the generic type cache for the given
     * scope.
     * @param addTypes whether to add the types to the scope's
     * privateTypeBindings map.
     **/
    std::vector<std::pair<Name, GenericTypeDefinition>> createGenerics(
        const ScopePtr& scope, AstArray<AstGenericType> generics, bool useCache = false, bool addTypes = true);

    /**
     * Creates generic type packs given a list of AST definitions, resolving
     * default type packs as required.
     * @param scope the scope that the generic packs should belong to.
     * @param generics the AST generics to create type packs for.
     * @param useCache whether to use the generic type pack cache for the given
     * scope.
     * @param addTypes whether to add the types to the scope's
     * privateTypePackBindings map.
     **/
    std::vector<std::pair<Name, GenericTypePackDefinition>> createGenericPacks(
        const ScopePtr& scope, AstArray<AstGenericTypePack> packs, bool useCache = false, bool addTypes = true);

    Inference flattenPack(const ScopePtr& scope, Location location, InferencePack pack);

    void reportError(Location location, TypeErrorData err);
    void reportCodeTooComplex(Location location);

    /** Scan the program for global definitions.
     *
     * ConstraintGenerator needs to differentiate between globals and accesses to undefined symbols. Doing this "for
     * real" in a general way is going to be pretty hard, so we are choosing not to tackle that yet. For now, we do an
     * initial scan of the AST and note what globals are defined.
     */
    void prepopulateGlobalScope(const ScopePtr& globalScope, AstStatBlock* program);

    void fillInInferredBindings(const ScopePtr& globalScope, AstStatBlock* block);

    /** Given a function type annotation, return a vector describing the expected types of the calls to the function
     *  For example, calling a function with annotation ((number) -> string & ((string) -> number))
     *  yields a vector of size 1, with value: [number | string]
     */
    std::vector<std::optional<TypeId>> getExpectedCallTypesForFunctionOverloads(const TypeId fnType);
};

/** Borrow a vector of pointers from a vector of owning pointers to constraints.
 */
std::vector<NotNull<Constraint>> borrowConstraints(const std::vector<ConstraintPtr>& constraints);


} // namespace Luau
