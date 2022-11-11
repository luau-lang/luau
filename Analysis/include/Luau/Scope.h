// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Constraint.h"
#include "Luau/Location.h"
#include "Luau/NotNull.h"
#include "Luau/TypeVar.h"

#include <unordered_map>
#include <optional>
#include <memory>

namespace Luau
{

struct Scope;

using ScopePtr = std::shared_ptr<Scope>;

struct Binding
{
    TypeId typeId;
    Location location;
    bool deprecated = false;
    std::string deprecatedSuggestion;
    std::optional<std::string> documentationSymbol;
};

struct Scope
{
    explicit Scope(TypePackId returnType);                    // root scope
    explicit Scope(const ScopePtr& parent, int subLevel = 0); // child scope.  Parent must not be nullptr.

    const ScopePtr parent; // null for the root

    // All the children of this scope.
    std::vector<NotNull<Scope>> children;
    std::unordered_map<Symbol, Binding> bindings;
    TypePackId returnType;
    std::optional<TypePackId> varargPack;

    TypeLevel level;

    std::unordered_map<Name, TypeFun> exportedTypeBindings;
    std::unordered_map<Name, TypeFun> privateTypeBindings;
    std::unordered_map<Name, Location> typeAliasLocations;
    std::unordered_map<Name, std::unordered_map<Name, TypeFun>> importedTypeBindings;

    DenseHashSet<Name> builtinTypeNames{""};
    void addBuiltinTypeBinding(const Name& name, const TypeFun& tyFun);

    std::optional<TypeId> lookup(Symbol sym) const;
    std::optional<TypeId> lookup(DefId def) const;
    std::optional<std::pair<TypeId, Scope*>> lookupEx(Symbol sym);

    std::optional<TypeFun> lookupType(const Name& name);
    std::optional<TypeFun> lookupImportedType(const Name& moduleAlias, const Name& name);

    std::unordered_map<Name, TypePackId> privateTypePackBindings;
    std::optional<TypePackId> lookupPack(const Name& name);

    // WARNING: This function linearly scans for a string key of equal value!  It is thus O(n**2)
    std::optional<Binding> linearSearchForBinding(const std::string& name, bool traverseScopeChain = true) const;

    RefinementMap refinements;
    DenseHashMap<const Def*, TypeId> dcrRefinements{nullptr};

    // For mutually recursive type aliases, it's important that
    // they use the same types for the same names.
    // For instance, in `type Tree<T> { data: T, children: Forest<T> } type Forest<T> = {Tree<T>}`
    // we need that the generic type `T` in both cases is the same, so we use a cache.
    std::unordered_map<Name, TypeId> typeAliasTypeParameters;
    std::unordered_map<Name, TypePackId> typeAliasTypePackParameters;
};

// Returns true iff the left scope encloses the right scope.  A Scope* equal to
// nullptr is considered to be the outermost-possible scope.
bool subsumesStrict(Scope* left, Scope* right);

// Returns true if the left scope encloses the right scope, or if they are the
// same scope.  As in subsumesStrict(), nullptr is considered to be the
// outermost-possible scope.
bool subsumes(Scope* left, Scope* right);

} // namespace Luau
