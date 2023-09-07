// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Scope.h"
#include "Luau/Type.h"

#include <optional>

namespace Luau
{

struct Frontend;
struct GlobalTypes;
struct TypeChecker;
struct TypeArena;

void registerBuiltinGlobals(Frontend& frontend, GlobalTypes& globals, bool typeCheckForAutocomplete = false);
TypeId makeUnion(TypeArena& arena, std::vector<TypeId>&& types);
TypeId makeIntersection(TypeArena& arena, std::vector<TypeId>&& types);

/** Build an optional 't'
 */
TypeId makeOption(NotNull<BuiltinTypes> builtinTypes, TypeArena& arena, TypeId t);

/** Small utility function for building up type definitions from C++.
 */
TypeId makeFunction( // Monomorphic
    TypeArena& arena, std::optional<TypeId> selfType, std::initializer_list<TypeId> paramTypes, std::initializer_list<TypeId> retTypes);

TypeId makeFunction( // Polymorphic
    TypeArena& arena, std::optional<TypeId> selfType, std::initializer_list<TypeId> generics, std::initializer_list<TypePackId> genericPacks,
    std::initializer_list<TypeId> paramTypes, std::initializer_list<TypeId> retTypes);

TypeId makeFunction( // Monomorphic
    TypeArena& arena, std::optional<TypeId> selfType, std::initializer_list<TypeId> paramTypes, std::initializer_list<std::string> paramNames,
    std::initializer_list<TypeId> retTypes);

TypeId makeFunction( // Polymorphic
    TypeArena& arena, std::optional<TypeId> selfType, std::initializer_list<TypeId> generics, std::initializer_list<TypePackId> genericPacks,
    std::initializer_list<TypeId> paramTypes, std::initializer_list<std::string> paramNames, std::initializer_list<TypeId> retTypes);

void attachMagicFunction(TypeId ty, MagicFunction fn);
void attachDcrMagicFunction(TypeId ty, DcrMagicFunction fn);
void attachDcrMagicRefinement(TypeId ty, DcrMagicRefinement fn);

Property makeProperty(TypeId ty, std::optional<std::string> documentationSymbol = std::nullopt);
void assignPropDocumentationSymbols(TableType::Props& props, const std::string& baseName);

std::string getBuiltinDefinitionSource();

void addGlobalBinding(GlobalTypes& globals, const std::string& name, TypeId ty, const std::string& packageName);
void addGlobalBinding(GlobalTypes& globals, const std::string& name, Binding binding);
void addGlobalBinding(GlobalTypes& globals, const ScopePtr& scope, const std::string& name, TypeId ty, const std::string& packageName);
void addGlobalBinding(GlobalTypes& globals, const ScopePtr& scope, const std::string& name, Binding binding);
std::optional<Binding> tryGetGlobalBinding(GlobalTypes& globals, const std::string& name);
Binding* tryGetGlobalBindingRef(GlobalTypes& globals, const std::string& name);
TypeId getGlobalBinding(GlobalTypes& globals, const std::string& name);

} // namespace Luau
