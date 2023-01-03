// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Error.h"
#include "Luau/Location.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"

#include <memory>
#include <optional>

namespace Luau
{

struct TxnLog;
struct TypeArena;

using ScopePtr = std::shared_ptr<struct Scope>;

std::optional<TypeId> findMetatableEntry(
    NotNull<BuiltinTypes> builtinTypes, ErrorVec& errors, TypeId type, const std::string& entry, Location location);
std::optional<TypeId> findTablePropertyRespectingMeta(
    NotNull<BuiltinTypes> builtinTypes, ErrorVec& errors, TypeId ty, const std::string& name, Location location);

// Returns the minimum and maximum number of types the argument list can accept.
std::pair<size_t, std::optional<size_t>> getParameterExtents(const TxnLog* log, TypePackId tp, bool includeHiddenVariadics = false);

// Extend the provided pack to at least `length` types.
// Returns a temporary TypePack that contains those types plus a tail.
TypePack extendTypePack(TypeArena& arena, NotNull<BuiltinTypes> builtinTypes, TypePackId pack, size_t length);

/**
 * Reduces a union by decomposing to the any/error type if it appears in the
 * type list, and by merging child unions. Also strips out duplicate (by pointer
 * identity) types.
 * @param types the input type list to reduce.
 * @returns the reduced type list.
 */
std::vector<TypeId> reduceUnion(const std::vector<TypeId>& types);

/**
 * Tries to remove nil from a union type, if there's another option. T | nil
 * reduces to T, but nil itself does not reduce.
 * @param builtinTypes the singleton types to use
 * @param arena the type arena to allocate the new type in, if necessary
 * @param ty the type to remove nil from
 * @returns a type with nil removed, or nil itself if that were the only option.
 */
TypeId stripNil(NotNull<BuiltinTypes> builtinTypes, TypeArena& arena, TypeId ty);

} // namespace Luau
