// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Error.h"
#include "Luau/Location.h"
#include "Luau/TypeVar.h"

#include <memory>
#include <optional>

namespace Luau
{

struct TxnLog;

using ScopePtr = std::shared_ptr<struct Scope>;

std::optional<TypeId> findMetatableEntry(
    NotNull<SingletonTypes> singletonTypes, ErrorVec& errors, TypeId type, const std::string& entry, Location location);
std::optional<TypeId> findTablePropertyRespectingMeta(
    NotNull<SingletonTypes> singletonTypes, ErrorVec& errors, TypeId ty, const std::string& name, Location location);
std::optional<TypeId> getIndexTypeFromType(const ScopePtr& scope, ErrorVec& errors, TypeArena* arena, NotNull<SingletonTypes> singletonTypes,
    TypeId type, const std::string& prop, const Location& location, bool addErrors, InternalErrorReporter& handle);

// Returns the minimum and maximum number of types the argument list can accept.
std::pair<size_t, std::optional<size_t>> getParameterExtents(const TxnLog* log, TypePackId tp);

} // namespace Luau
