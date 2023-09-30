// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Module.h"
#include "Luau/NotNull.h"

namespace Luau
{

struct BuiltinTypes;


void checkNonStrict(NotNull<BuiltinTypes> builtinTypes, Module* module);

} // namespace Luau
