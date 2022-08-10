// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include <memory>
#include <vector>
#include <unordered_map>

#include "Luau/Ast.h"
#include "Luau/Constraint.h"
#include "Luau/Module.h"
#include "Luau/NotNull.h"
#include "Luau/Symbol.h"
#include "Luau/TypeVar.h"
#include "Luau/Variant.h"

namespace Luau
{

struct Scope;
using ScopePtr = std::shared_ptr<Scope>;

struct ConstraintGraphBuilder
{
    // A list of all the scopes in the module. This vector holds ownership of the
    // scope pointers; the scopes themselves borrow pointers to other scopes to
    // define the scope hierarchy.
    std::vector<std::pair<Location, ScopePtr>> scopes;

    ModuleName moduleName;
    SingletonTypes& singletonTypes;
    const NotNull<TypeArena> arena;
    // The root scope of the module we're generating constraints for.
    // This is null when the CGB is initially constructed.
    Scope* rootScope;
    // A mapping of AST node to TypeId.
    DenseHashMap<const AstExpr*, TypeId> astTypes{nullptr};
    // A mapping of AST node to TypePackId.
    DenseHashMap<const AstExpr*, TypePackId> astTypePacks{nullptr};
    DenseHashMap<const AstExpr*, TypeId> astOriginalCallTypes{nullptr};
    // Types resolved from type annotations. Analogous to astTypes.
    DenseHashMap<const AstType*, TypeId> astResolvedTypes{nullptr};
    // Type packs resolved from type annotations. Analogous to astTypePacks.
    DenseHashMap<const AstTypePack*, TypePackId> astResolvedTypePacks{nullptr};
    // Defining scopes for AST nodes.
    DenseHashMap<const AstStatTypeAlias*, ScopePtr> astTypeAliasDefiningScopes{nullptr};

    int recursionCount = 0;

    // It is pretty uncommon for constraint generation to itself produce errors, but it can happen.
    std::vector<TypeError> errors;

    // Occasionally constraint generation needs to produce an ICE.
    const NotNull<InternalErrorReporter> ice;

    NotNull<Scope> globalScope;

    ConstraintGraphBuilder(const ModuleName& moduleName, TypeArena* arena, NotNull<InternalErrorReporter> ice, NotNull<Scope> globalScope);

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
     * @param location the lexical extent of the scope in the source code.
     * @param parent the parent scope of the new scope. Must not be null.
     */
    ScopePtr childScope(Location location, const ScopePtr& parent);

    /**
     * Adds a new constraint with no dependencies to a given scope.
     * @param scope the scope to add the constraint to.
     * @param cv the constraint variant to add.
     */
    void addConstraint(const ScopePtr& scope, ConstraintV cv);

    /**
     * Adds a constraint to a given scope.
     * @param scope the scope to add the constraint to. Must not be null.
     * @param c the constraint to add.
     */
    void addConstraint(const ScopePtr& scope, std::unique_ptr<Constraint> c);

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
    void visit(const ScopePtr& scope, AstStatLocalFunction* function);
    void visit(const ScopePtr& scope, AstStatFunction* function);
    void visit(const ScopePtr& scope, AstStatReturn* ret);
    void visit(const ScopePtr& scope, AstStatAssign* assign);
    void visit(const ScopePtr& scope, AstStatIf* ifStatement);
    void visit(const ScopePtr& scope, AstStatTypeAlias* alias);
    void visit(const ScopePtr& scope, AstStatDeclareGlobal* declareGlobal);
    void visit(const ScopePtr& scope, AstStatDeclareClass* declareClass);
    void visit(const ScopePtr& scope, AstStatDeclareFunction* declareFunction);

    TypePackId checkExprList(const ScopePtr& scope, const AstArray<AstExpr*>& exprs);

    TypePackId checkPack(const ScopePtr& scope, AstArray<AstExpr*> exprs);
    TypePackId checkPack(const ScopePtr& scope, AstExpr* expr);

    /**
     * Checks an expression that is expected to evaluate to one type.
     * @param scope the scope the expression is contained within.
     * @param expr the expression to check.
     * @return the type of the expression.
     */
    TypeId check(const ScopePtr& scope, AstExpr* expr);

    TypeId checkExprTable(const ScopePtr& scope, AstExprTable* expr);
    TypeId check(const ScopePtr& scope, AstExprIndexName* indexName);
    TypeId check(const ScopePtr& scope, AstExprIndexExpr* indexExpr);
    TypeId check(const ScopePtr& scope, AstExprUnary* unary);
    TypeId check(const ScopePtr& scope, AstExprBinary* binary);

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

    FunctionSignature checkFunctionSignature(const ScopePtr& parent, AstExprFunction* fn);

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
     * @param topLevel whether the annotation is a "top-level" annotation.
     * @return the type of the AST annotation.
     **/
    TypeId resolveType(const ScopePtr& scope, AstType* ty, bool topLevel = false);

    /**
     * Resolves a type pack from its AST annotation.
     * @param scope the scope that the type annotation appears within.
     * @param tp the AST annotation to resolve.
     * @return the type pack of the AST annotation.
     **/
    TypePackId resolveTypePack(const ScopePtr& scope, AstTypePack* tp);

    TypePackId resolveTypePack(const ScopePtr& scope, const AstTypeList& list);

    std::vector<std::pair<Name, GenericTypeDefinition>> createGenerics(const ScopePtr& scope, AstArray<AstGenericType> generics);
    std::vector<std::pair<Name, GenericTypePackDefinition>> createGenericPacks(const ScopePtr& scope, AstArray<AstGenericTypePack> packs);

    TypeId flattenPack(const ScopePtr& scope, Location location, TypePackId tp);

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

/**
 * Collects a vector of borrowed constraints from the scope and all its child
 * scopes. It is important to only call this function when you're done adding
 * constraints to the scope or its descendants, lest the borrowed pointers
 * become invalid due to a container reallocation.
 * @param rootScope the root scope of the scope graph to collect constraints
 * from.
 * @return a list of pointers to constraints contained within the scope graph.
 * None of these pointers should be null.
 */
std::vector<NotNull<Constraint>> collectConstraints(NotNull<Scope> rootScope);

} // namespace Luau
