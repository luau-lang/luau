// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include <memory>
#include <vector>

#include "Luau/Ast.h"
#include "Luau/Module.h"
#include "Luau/Symbol.h"
#include "Luau/TypeVar.h"
#include "Luau/Variant.h"

namespace Luau
{

struct Scope2;

// subType <: superType
struct SubtypeConstraint
{
    TypeId subType;
    TypeId superType;
};

// subPack <: superPack
struct PackSubtypeConstraint
{
    TypePackId subPack;
    TypePackId superPack;
};

// subType ~ gen superType
struct GeneralizationConstraint
{
    TypeId subType;
    TypeId superType;
    Scope2* scope;
};

// subType ~ inst superType
struct InstantiationConstraint
{
    TypeId subType;
    TypeId superType;
};

using ConstraintV = Variant<SubtypeConstraint, PackSubtypeConstraint, GeneralizationConstraint, InstantiationConstraint>;
using ConstraintPtr = std::unique_ptr<struct Constraint>;

struct Constraint
{
    Constraint(ConstraintV&& c);
    Constraint(ConstraintV&& c, std::vector<Constraint*> dependencies);

    Constraint(const Constraint&) = delete;
    Constraint& operator=(const Constraint&) = delete;

    ConstraintV c;
    std::vector<Constraint*> dependencies;
};

inline Constraint& asMutable(const Constraint& c)
{
    return const_cast<Constraint&>(c);
}

template<typename T>
T* getMutable(Constraint& c)
{
    return ::Luau::get_if<T>(&c.c);
}

template<typename T>
const T* get(const Constraint& c)
{
    return getMutable<T>(asMutable(c));
}

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
     */
    void addConstraint(Scope2* scope, ConstraintV cv);

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
    void visit(Scope2* scope, AstStatLocalFunction* local);
    void visit(Scope2* scope, AstStatReturn* local);

    TypePackId checkPack(Scope2* scope, AstArray<AstExpr*> exprs);
    TypePackId checkPack(Scope2* scope, AstExpr* expr);

    TypeId check(Scope2* scope, AstExpr* expr);
};

std::vector<const Constraint*> collectConstraints(Scope2* rootScope);

} // namespace Luau
