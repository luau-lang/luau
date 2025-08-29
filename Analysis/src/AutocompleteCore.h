// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/AutocompleteTypes.h"

namespace Luau
{
struct Module;
struct FileResolver;

using ModulePtr = std::shared_ptr<Module>;
using ModuleName = std::string;


AutocompleteResult autocomplete_(
    const ModulePtr& module,
    NotNull<BuiltinTypes> builtinTypes,
    TypeArena* typeArena,
    std::vector<AstNode*>& ancestry,
    Scope* globalScope,
    const ScopePtr& scopeAtPosition,
    Position position,
    FileResolver* fileResolver,
    StringCompletionCallback callback,
    bool isInHotComment = false
);

} // namespace Luau
