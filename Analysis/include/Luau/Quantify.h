// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeVar.h"

namespace Luau
{

struct Module;
using ModulePtr = std::shared_ptr<Module>;

void quantify(ModulePtr module, TypeId ty, TypeLevel level);

} // namespace Luau
