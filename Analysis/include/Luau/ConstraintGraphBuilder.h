// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Connective.h"
#include "Luau/Constraint.h"
#include "Luau/DataFlowGraph.h"
#include "Luau/Module.h"
#include "Luau/ModuleResolver.h"
#include "Luau/NotNull.h"
#include "Luau/Symbol.h"
#include "Luau/Type.h"
#include "Luau/Variant.h"

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
    ConnectiveId connective = nullptr;

    Inference() = default;

    explicit Inference(TypeId ty, ConnectiveId connective = nullptr)
        : ty(ty)
        , connective(connective)
    {
    }
};

struct InferencePack
{
    TypePackId tp = nullptr;
    std::vector<ConnectiveId> connectives;

    InferencePack() = default;

    explicit InferencePack(TypePackId tp, const std::vector<ConnectiveId>& connectives = {})
        : tp(tp)
        , connectives(connectives)
    {
    }
};

struct ConstraintGraphBuilder
{
    // A list of all the scopes in the module. This vector holds ownership of the
    // scope pointers; the scopes themselves borrow pointers to other scopes to
    // define the scope hierarchy.
    std::vector<std::pair<Location, ScopePtr>> scopes;

    ModuleName moduleName;
    ModulePtr module;
    NotNull<BuiltinTypes> builtinTypes;
    const NotNull<TypeArena> arena;
    // The root scope of the module we're generating constraints for.
    // This is null when the CGB is initially constructed.
    Scope* rootScope;

    // Constraints that go straight to the solver.
    std::vector<ConstraintPtr> constraints;

    // Constraints that do not go to the solver right away.  Other constraints
    // will enqueue them during solving.
    std::vector<ConstraintPtr> unqueuedConstraints;

    // A mapping of AST node to TypeId.
    DenseHashMap<const AstExpr*, TypeId> astTypes{nullptr};

    // A mapping of AST node to TypePackId.
    DenseHashMap<const AstExpr*, TypePackId> astTypePacks{nullptr};

    // If the node was applied as a function, this is the unspecialized type of
    // that expression.
    DenseHashMap<const void*, TypeId> astOriginalCallTypes{nullptr};

    // If overload resolution was performed on this element, this is the
    // overload that was selected.
    DenseHashMap<const void*, TypeId> astOverloadResolvedTypes{nullptr};

    // Types resolved from type annotations. Analogous to astTypes.
    DenseHashMap<const AstType*, TypeId> astResolvedTypes{nullptr};

    // Type packs resolved from type annotations. Analogous to astTypePacks.
    DenseHashMap<const AstTypePack*, TypePackId> astResolvedTypePacks{nullptr};

    // Defining scopes for AST nodes.
    DenseHashMap<const AstStatTypeAlias*, ScopePtr> astTypeAliasDefiningScopes{nullptr};

    NotNull<const DataFlowGraph> dfg;
    ConnectiveArena connectiveArena;

    int recursionCount = 0;

    // It is pretty uncommon for constraint generation to itself produce errors, but it can happen.
    std::vector<TypeError> errors;

    // Needed to resolve modules to make 'require' import types properly.
    NotNull<ModuleResolver> moduleResolver;
    // Occasionally constraint generation needs to produce an ICE.
    const NotNull<InternalErrorReporter> ice;

    ScopePtr globalScope;
    DcrLogger* logger;

    ConstraintGraphBuilder(const ModuleName& moduleName, ModulePtr module, TypeArena* arena, NotNull<ModuleResolver> moduleResolver,
        NotNull<BuiltinTypes> builtinTypes, NotNull<InternalErrorReporter> ice, const ScopePtr& globalScope, DcrLogger* logger,
        NotNull<DataFlowGraph> dfg);

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

    void applyRefinements(const ScopePtr& scope, Location location, ConnectiveId connective);

    /**
     * The entry point to the ConstraintGraphBuilder. This will construct a set
     * of scopes, constraints, and free types that can be solved later.
     * @param block the root block to generate constraints for.
     */
    void visit(AstStatBlock* block);

    void visitBlockWithoutChildScope(const ScopePtr& scope, AstStatBlock* block);

    void visit(const ScopePtr& scope, AstStat* stat);
    void visit(const ScopePtr& scope, AstStatBlock* block);
    void visit(const ScopePtr& scope, AstStatLocal* local);
    void visit(const ScopePtr& scope, AstStatFor* for_);
    void visit(const ScopePtr& scope, AstStatForIn* forIn);
    void visit(const ScopePtr& scope, AstStatWhile* while_);
    void visit(const ScopePtr& scope, AstStatRepeat* repeat);
    void visit(const ScopePtr& scope, AstStatLocalFunction* function);
    void visit(const ScopePtr& scope, AstStatFunction* function);
    void visit(const ScopePtr& scope, AstStatReturn* ret);
    void visit(const ScopePtr& scope, AstStatAssign* assign);
    void visit(const ScopePtr& scope, AstStatCompoundAssign* assign);
    void visit(const ScopePtr& scope, AstStatIf* ifStatement);
    void visit(const ScopePtr& scope, AstStatTypeAlias* alias);
    void visit(const ScopePtr& scope, AstStatDeclareGlobal* declareGlobal);
    void visit(const ScopePtr& scope, AstStatDeclareClass* declareClass);
    void visit(const ScopePtr& scope, AstStatDeclareFunction* declareFunction);
    void visit(const ScopePtr& scope, AstStatError* error);

    InferencePack checkPack(const ScopePtr& scope, AstArray<AstExpr*> exprs, const std::vector<TypeId>& expectedTypes = {});
    InferencePack checkPack(const ScopePtr& scope, AstExpr* expr, const std::vector<TypeId>& expectedTypes = {});

    InferencePack checkPack(const ScopePtr& scope, AstExprCall* call, const std::vector<TypeId>& expectedTypes);

    /**
     * Checks an expression that is expected to evaluate to one type.
     * @param scope the scope the expression is contained within.
     * @param expr the expression to check.
     * @param expectedType the type of the expression that is expected from its
     *      surrounding context.  Used to implement bidirectional type checking.
     * @return the type of the expression.
     */
    Inference check(const ScopePtr& scope, AstExpr* expr, std::optional<TypeId> expectedType = {}, bool forceSingleton = false);

    Inference check(const ScopePtr& scope, AstExprConstantString* string, std::optional<TypeId> expectedType, bool forceSingleton);
    Inference check(const ScopePtr& scope, AstExprConstantBool* bool_, std::optional<TypeId> expectedType, bool forceSingleton);
    Inference check(const ScopePtr& scope, AstExprLocal* local);
    Inference check(const ScopePtr& scope, AstExprGlobal* global);
    Inference check(const ScopePtr& scope, AstExprIndexName* indexName);
    Inference check(const ScopePtr& scope, AstExprIndexExpr* indexExpr);
    Inference check(const ScopePtr& scope, AstExprUnary* unary);
    Inference check(const ScopePtr& scope, AstExprBinary* binary, std::optional<TypeId> expectedType);
    Inference check(const ScopePtr& scope, AstExprIfElse* ifElse, std::optional<TypeId> expectedType);
    Inference check(const ScopePtr& scope, AstExprTypeAssertion* typeAssert);
    Inference check(const ScopePtr& scope, AstExprTable* expr, std::optional<TypeId> expectedType);
    std::tuple<TypeId, TypeId, ConnectiveId> checkBinary(const ScopePtr& scope, AstExprBinary* binary, std::optional<TypeId> expectedType);

    TypePackId checkLValues(const ScopePtr& scope, AstArray<AstExpr*> exprs);

    TypeId checkLValue(const ScopePtr& scope, AstExpr* expr);

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

    FunctionSignature checkFunctionSignature(const ScopePtr& parent, AstExprFunction* fn, std::optional<TypeId> expectedType = {});

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
    TypeId resolveType(const ScopePtr& scope, AstType* ty, bool inTypeArguments);

    /**
     * Resolves a type pack from its AST annotation.
     * @param scope the scope that the type annotation appears within.
     * @param tp the AST annotation to resolve.
     * @param inTypeArguments whether we are resolving a type that's contained within type arguments, `<...>`.
     * @return the type pack of the AST annotation.
     **/
    TypePackId resolveTypePack(const ScopePtr& scope, AstTypePack* tp, bool inTypeArguments);

    /**
     * Resolves a type pack from its AST annotation.
     * @param scope the scope that the type annotation appears within.
     * @param list the AST annotation to resolve.
     * @param inTypeArguments whether we are resolving a type that's contained within type arguments, `<...>`.
     * @return the type pack of the AST annotation.
     **/
    TypePackId resolveTypePack(const ScopePtr& scope, const AstTypeList& list, bool inTypeArguments);

    std::vector<std::pair<Name, GenericTypeDefinition>> createGenerics(const ScopePtr& scope, AstArray<AstGenericType> generics);
    std::vector<std::pair<Name, GenericTypePackDefinition>> createGenericPacks(const ScopePtr& scope, AstArray<AstGenericTypePack> packs);

    Inference flattenPack(const ScopePtr& scope, Location location, InferencePack pack);

    void reportError(Location location, TypeErrorData err);
    void reportCodeTooComplex(Location location);

    /** Scan the program for global definitions.
     *
     * ConstraintGraphBuilder needs to differentiate between globals and accesses to undefined symbols. Doing this "for
     * real" in a general way is going to be pretty hard, so we are choosing not to tackle that yet. For now, we do an
     * initial scan of the AST and note what globals are defined.
     */
    void prepopulateGlobalScope(const ScopePtr& globalScope, AstStatBlock* program);
};

/** Borrow a vector of pointers from a vector of owning pointers to constraints.
 */
std::vector<NotNull<Constraint>> borrowConstraints(const std::vector<ConstraintPtr>& constraints);

} // namespace Luau
