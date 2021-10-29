// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Error.h"
#include "Luau/Location.h"
#include "Luau/TypeVar.h"

#include <memory>
#include <optional>

namespace Luau
{

using ScopePtr = std::shared_ptr<struct Scope>;

std::optional<TypeId> findMetatableEntry(ErrorVec& errors, const ScopePtr& globalScope, TypeId type, std::string entry, Location location);
std::optional<TypeId> findTablePropertyRespectingMeta(ErrorVec& errors, const ScopePtr& globalScope, TypeId ty, Name name, Location location);

} // namespace Luau
