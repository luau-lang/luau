// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Variant.h"

#include <string>

namespace Luau
{

// So... why `const T*` here rather than `T*`?
// It's because we've had problems caused by the type graph being mutated
// in ways it shouldn't be, for example mutating types from other modules.
// To try to control this, we make the use of types immutable by default,
// then provide explicit mutable access via getMutable and asMutable.
// This means we can grep for all the places we're mutating the type graph,
// and it makes it possible to provide other APIs (e.g. the txn log)
// which control mutable access to the type graph.

struct Type;
using TypeId = const Type*;

struct FreeType;
struct GenericType;
struct PrimitiveType;
struct BlockedType;
struct PendingExpansionType;
struct SingletonType;
struct FunctionType;
struct TableType;
struct MetatableType;
struct ExternType;
struct AnyType;
struct UnionType;
struct IntersectionType;
struct LazyType;
struct UnknownType;
struct NeverType;
struct NegationType;
struct TypeFunctionInstanceType;

struct TypePackVar;
using TypePackId = const TypePackVar*;

struct FreeTypePack;
struct GenericTypePack;
struct TypePack;
struct VariadicTypePack;
struct BlockedTypePack;
struct TypeFunctionInstanceTypePack;

using Name = std::string;
using ModuleName = std::string;

struct BuiltinTypes;

using TypeOrPack = Variant<TypeId, TypePackId>;

struct TypeFunctionType;
using TypeFunctionTypeId = const TypeFunctionType*;

struct TypeFunctionTypePackVar;
using TypeFunctionTypePackId = const TypeFunctionTypePackVar*;

} // namespace Luau
