// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Def.h"
#include "Luau/LValue.h"
#include "Luau/Location.h"
#include "Luau/NotNull.h"
#include "Luau/Type.h"
#include "Luau/DenseHash.h"
#include "Luau/Symbol.h"
#include "Luau/Unifiable.h"

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

    Location location; // the spanning location associated with this scope

    std::unordered_map<Name, TypeFun> exportedTypeBindings;
    std::unordered_map<Name, TypeFun> privateTypeBindings;
    std::unordered_map<Name, Location> typeAliasLocations;
    std::unordered_map<Name, Location> typeAliasNameLocations;
    std::unordered_map<Name, ModuleName> importedModules; // Mapping from the name in the require statement to the internal moduleName.
    std::unordered_map<Name, std::unordered_map<Name, TypeFun>> importedTypeBindings;

    DenseHashSet<Name> builtinTypeNames{""};
    void addBuiltinTypeBinding(const Name& name, const TypeFun& tyFun);

    std::optional<TypeId> lookup(Symbol sym) const;
    std::optional<TypeId> lookupUnrefinedType(DefId def) const;
    std::optional<TypeId> lookup(DefId def) const;
    std::optional<std::pair<TypeId, Scope*>> lookupEx(DefId def);
    std::optional<std::pair<Binding*, Scope*>> lookupEx(Symbol sym);

    std::optional<TypeFun> lookupType(const Name& name) const;
    std::optional<TypeFun> lookupImportedType(const Name& moduleAlias, const Name& name) const;

    std::unordered_map<Name, TypePackId> privateTypePackBindings;
    std::optional<TypePackId> lookupPack(const Name& name) const;

    // WARNING: This function linearly scans for a string key of equal value!  It is thus O(n**2)
    std::optional<Binding> linearSearchForBinding(const std::string& name, bool traverseScopeChain = true) const;

    RefinementMap refinements;

    // This can be viewed as the "unrefined" type of each binding.
    DenseHashMap<const Def*, TypeId> lvalueTypes{nullptr};

    // Luau values are routinely refined more narrowly than their actual
    // inferred type through control flow statements.  We retain those refined
    // types here.
    DenseHashMap<const Def*, TypeId> rvalueRefinements{nullptr};

    void inheritAssignments(const ScopePtr& childScope);
    void inheritRefinements(const ScopePtr& childScope);

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

inline Scope* max(Scope* left, Scope* right)
{
    if (subsumes(left, right))
        return right;
    else
        return left;
}

} // namespace Luau
