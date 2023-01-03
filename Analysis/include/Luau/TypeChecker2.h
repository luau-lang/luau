// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Ast.h"
#include "Luau/Module.h"
#include "Luau/NotNull.h"

namespace Luau
{

struct DcrLogger;
struct BuiltinTypes;

void check(NotNull<BuiltinTypes> builtinTypes, DcrLogger* logger, const SourceModule& sourceModule, Module* module);

} // namespace Luau
