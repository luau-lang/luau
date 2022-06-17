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

struct Scope2
{
    // The parent scope of this scope. Null if there is no parent (i.e. this
    // is the module-level scope).
    Scope2* parent = nullptr;
    // All the children of this scope.
    std::vector<Scope2*> children;
    std::unordered_map<Symbol, TypeId> bindings; // TODO: I think this can be a DenseHashMap
    TypePackId returnType;
    // All constraints belonging to this scope.
    std::vector<ConstraintPtr> constraints;

    std::optional<TypeId> lookup(Symbol sym);
};

struct ConstraintGraphBuilder
{
    // A list of all the scopes in the module. This vector holds ownership of the
    // scope pointers; the scopes themselves borrow pointers to other scopes to
    // define the scope hierarchy.
    std::vector<std::pair<Location, std::unique_ptr<Scope2>>> scopes;
    SingletonTypes& singletonTypes;
    TypeArena* const arena;
    // The root scope of the module we're generating constraints for.
    Scope2* rootScope;
    // A mapping of AST node to TypeId.
    DenseHashMap<const AstExpr*, TypeId> astTypes{nullptr};
    // A mapping of AST node to TypePackId.
    DenseHashMap<const AstExpr*, TypePackId> astTypePacks{nullptr};
    DenseHashMap<const AstExpr*, TypeId> astOriginalCallTypes{nullptr};

    explicit ConstraintGraphBuilder(TypeArena* arena);

    /**
     * Fabricates a new free type belonging to a given scope.
     * @param scope the scope the free type belongs to. Must not be null.
     */
    TypeId freshType(Scope2* scope);

    /**
     * Fabricates a new free type pack belonging to a given scope.
     * @param scope the scope the free type pack belongs to. Must not be null.
     */
    TypePackId freshTypePack(Scope2* scope);

    /**
     * Fabricates a scope that is a child of another scope.
     * @param location the lexical extent of the scope in the source code.
     * @param parent the parent scope of the new scope. Must not be null.
     */
    Scope2* childScope(Location location, Scope2* parent);

    /**
     * Adds a new constraint with no dependencies to a given scope.
     * @param scope the scope to add the constraint to. Must not be null.
     * @param cv the constraint variant to add.
     * @param location the location to attribute to the constraint.
     */
    void addConstraint(Scope2* scope, ConstraintV cv, Location location);

    /**
     * Adds a constraint to a given scope.
     * @param scope the scope to add the constraint to. Must not be null.
     * @param c the constraint to add.
     */
    void addConstraint(Scope2* scope, std::unique_ptr<Constraint> c);

    /**
     * The entry point to the ConstraintGraphBuilder. This will construct a set
     * of scopes, constraints, and free types that can be solved later.
     * @param block the root block to generate constraints for.
     */
    void visit(AstStatBlock* block);

    void visit(Scope2* scope, AstStat* stat);
    void visit(Scope2* scope, AstStatBlock* block);
    void visit(Scope2* scope, AstStatLocal* local);
    void visit(Scope2* scope, AstStatLocalFunction* function);
    void visit(Scope2* scope, AstStatFunction* function);
    void visit(Scope2* scope, AstStatReturn* ret);
    void visit(Scope2* scope, AstStatAssign* assign);
    void visit(Scope2* scope, AstStatIf* ifStatement);

    TypePackId checkExprList(Scope2* scope, const AstArray<AstExpr*>& exprs);

    TypePackId checkPack(Scope2* scope, AstArray<AstExpr*> exprs);
    TypePackId checkPack(Scope2* scope, AstExpr* expr);

    /**
     * Checks an expression that is expected to evaluate to one type.
     * @param scope the scope the expression is contained within.
     * @param expr the expression to check.
     * @return the type of the expression.
     */
    TypeId check(Scope2* scope, AstExpr* expr);

    TypeId checkExprTable(Scope2* scope, AstExprTable* expr);
    TypeId check(Scope2* scope, AstExprIndexName* indexName);

    std::pair<TypeId, Scope2*> checkFunctionSignature(Scope2* parent, AstExprFunction* fn);

    /**
     * Checks the body of a function expression.
     * @param scope the interior scope of the body of the function.
     * @param fn the function expression to check.
     */
    void checkFunctionBody(Scope2* scope, AstExprFunction* fn);
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
std::vector<NotNull<Constraint>> collectConstraints(Scope2* rootScope);

} // namespace Luau
