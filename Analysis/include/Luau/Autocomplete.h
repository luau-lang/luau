// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/AutocompleteTypes.h"
#include "Luau/Location.h"
#include "Luau/Type.h"

#include <string>
#include <memory>
#include <optional>

namespace Luau
{

struct Frontend;
struct SourceModule;
struct Module;
struct TypeChecker;
struct FileResolver;

AutocompleteResult autocomplete(Frontend& frontend, const ModuleName& moduleName, Position position, StringCompletionCallback callback);

} // namespace Luau
