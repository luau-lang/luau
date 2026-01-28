// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeFwd.h"

#include <set>
#include <utility>

namespace Luau
{

using SeenSet = std::set<std::pair<const void*, const void*>>;

bool areEqual(SeenSet& seen, const Type& lhs, const Type& rhs);
bool areEqual(SeenSet& seen, const TypePackVar& lhs, const TypePackVar& rhs);
bool areEqual(SeenSet& seen, TypeId lhs, TypeId rhs);

} // namespace Luau
