// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Scope.h"
#include "Luau/Type.h"

#include <optional>

namespace Luau
{

struct Frontend;
struct TypeChecker;
struct TypeArena;

void registerBuiltinTypes(Frontend& frontend);

void registerBuiltinGlobals(TypeChecker& typeChecker);
void registerBuiltinGlobals(Frontend& frontend);

TypeId makeUnion(TypeArena& arena, std::vector<TypeId>&& types);
TypeId makeIntersection(TypeArena& arena, std::vector<TypeId>&& types);

/** Build an optional 't'
 */
TypeId makeOption(TypeChecker& typeChecker, TypeArena& arena, TypeId t);
TypeId makeOption(Frontend& frontend, TypeArena& arena, TypeId t);

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

void addGlobalBinding(TypeChecker& typeChecker, const std::string& name, Binding binding);
void addGlobalBinding(TypeChecker& typeChecker, const std::string& name, TypeId ty, const std::string& packageName);
void addGlobalBinding(TypeChecker& typeChecker, const ScopePtr& scope, const std::string& name, TypeId ty, const std::string& packageName);
void addGlobalBinding(TypeChecker& typeChecker, const ScopePtr& scope, const std::string& name, Binding binding);
void addGlobalBinding(Frontend& frontend, const std::string& name, TypeId ty, const std::string& packageName);
void addGlobalBinding(Frontend& frontend, const std::string& name, Binding binding);
void addGlobalBinding(Frontend& frontend, const ScopePtr& scope, const std::string& name, TypeId ty, const std::string& packageName);
void addGlobalBinding(Frontend& frontend, const ScopePtr& scope, const std::string& name, Binding binding);
std::optional<Binding> tryGetGlobalBinding(Frontend& frontend, const std::string& name);
Binding* tryGetGlobalBindingRef(TypeChecker& typeChecker, const std::string& name);
TypeId getGlobalBinding(Frontend& frontend, const std::string& name);
TypeId getGlobalBinding(TypeChecker& typeChecker, const std::string& name);

} // namespace Luau
