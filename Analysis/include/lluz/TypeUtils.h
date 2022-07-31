// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/Error.h"
#include "lluz/Location.h"
#include "lluz/TypeVar.h"

#include <memory>
#include <optional>

namespace lluz
{

using ScopePtr = std::shared_ptr<struct Scope>;

std::optional<TypeId> findMetatableEntry(ErrorVec& errors, TypeId type, std::string entry, Location location);
std::optional<TypeId> findTablePropertyRespectingMeta(ErrorVec& errors, TypeId ty, Name name, Location location);

} // namespace lluz
