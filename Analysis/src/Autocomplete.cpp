// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Autocomplete.h"

#include "Luau/AstQuery.h"
#include "Luau/TypeArena.h"
#include "Luau/Module.h"
#include "Luau/Frontend.h"

#include "AutocompleteCore.h"

LUAU_FASTFLAG(LuauSolverV2)

namespace Luau
{

AutocompleteResult autocomplete(Frontend& frontend, const ModuleName& moduleName, Position position, StringCompletionCallback callback)
{
    const SourceModule* sourceModule = frontend.getSourceModule(moduleName);
    if (!sourceModule)
        return {};

    ModulePtr module;
    if (FFlag::LuauSolverV2)
        module = frontend.moduleResolver.getModule(moduleName);
    else
        module = frontend.moduleResolverForAutocomplete.getModule(moduleName);

    if (!module)
        return {};

    NotNull<BuiltinTypes> builtinTypes = frontend.builtinTypes;
    Scope* globalScope;
    if (FFlag::LuauSolverV2)
        globalScope = frontend.globals.globalScope.get();
    else
        globalScope = frontend.globalsForAutocomplete.globalScope.get();

    TypeArena typeArena;
    if (isWithinComment(*sourceModule, position))
        return {};

    std::vector<AstNode*> ancestry = findAncestryAtPositionForAutocomplete(*sourceModule, position);
    LUAU_ASSERT(!ancestry.empty());
    ScopePtr startScope = findScopeAtPosition(*module, position);
    return autocomplete_(module, builtinTypes, &typeArena, ancestry, globalScope, startScope, position, frontend.fileResolver, callback);
}

} // namespace Luau
