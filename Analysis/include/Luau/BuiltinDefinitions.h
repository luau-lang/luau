// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "TypeInfer.h"

namespace Luau
{

void registerBuiltinTypes(TypeChecker& typeChecker);

TypeId makeUnion(TypeArena& arena, std::vector<TypeId>&& types);
TypeId makeIntersection(TypeArena& arena, std::vector<TypeId>&& types);

/** Build an optional 't'
 */
TypeId makeOption(TypeChecker& typeChecker, TypeArena& arena, TypeId t);

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
void attachFunctionTag(TypeId ty, std::string constraint);

Property makeProperty(TypeId ty, std::optional<std::string> documentationSymbol = std::nullopt);
void assignPropDocumentationSymbols(TableTypeVar::Props& props, const std::string& baseName);

std::string getBuiltinDefinitionSource();

void addGlobalBinding(TypeChecker& typeChecker, const std::string& name, TypeId ty, const std::string& packageName);
void addGlobalBinding(TypeChecker& typeChecker, const std::string& name, Binding binding);
void addGlobalBinding(TypeChecker& typeChecker, const ScopePtr& scope, const std::string& name, TypeId ty, const std::string& packageName);
void addGlobalBinding(TypeChecker& typeChecker, const ScopePtr& scope, const std::string& name, Binding binding);
std::optional<Binding> tryGetGlobalBinding(TypeChecker& typeChecker, const std::string& name);
Binding* tryGetGlobalBindingRef(TypeChecker& typeChecker, const std::string& name);
TypeId getGlobalBinding(TypeChecker& typeChecker, const std::string& name);

} // namespace Luau
