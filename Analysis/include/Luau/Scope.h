// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Location.h"
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
    std::unordered_map<Symbol, Binding> bindings;
    TypePackId returnType;
    bool breakOk = false;
    std::optional<TypePackId> varargPack;

    TypeLevel level;

    std::unordered_map<Name, TypeFun> exportedTypeBindings;
    std::unordered_map<Name, TypeFun> privateTypeBindings;
    std::unordered_map<Name, Location> typeAliasLocations;

    std::unordered_map<Name, std::unordered_map<Name, TypeFun>> importedTypeBindings;

    std::optional<TypeId> lookup(const Symbol& name);

    std::optional<TypeFun> lookupType(const Name& name);
    std::optional<TypeFun> lookupImportedType(const Name& moduleAlias, const Name& name);

    std::unordered_map<Name, TypePackId> privateTypePackBindings;
    std::optional<TypePackId> lookupPack(const Name& name);

    // WARNING: This function linearly scans for a string key of equal value!  It is thus O(n**2)
    std::optional<Binding> linearSearchForBinding(const std::string& name, bool traverseScopeChain = true);

    RefinementMap refinements;

    // For mutually recursive type aliases, it's important that
    // they use the same types for the same names.
    // For instance, in `type Tree<T> { data: T, children: Forest<T> } type Forest<T> = {Tree<T>}`
    // we need that the generic type `T` in both cases is the same, so we use a cache.
    std::unordered_map<Name, TypeId> typeAliasTypeParameters;
    std::unordered_map<Name, TypePackId> typeAliasTypePackParameters;
};

} // namespace Luau
