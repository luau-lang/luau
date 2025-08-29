// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Autocomplete.h"

#include "Luau/AstQuery.h"
#include "Luau/TimeTrace.h"
#include "Luau/TypeArena.h"
#include "Luau/Module.h"
#include "Luau/Frontend.h"

#include "AutocompleteCore.h"

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauSuggestHotComments)

namespace Luau
{

AutocompleteResult autocomplete(Frontend& frontend, const ModuleName& moduleName, Position position, StringCompletionCallback callback)
{
    LUAU_TIMETRACE_SCOPE("Luau::autocomplete", "Autocomplete");
    LUAU_TIMETRACE_ARGUMENT("name", moduleName.c_str());

    const SourceModule* sourceModule = frontend.getSourceModule(moduleName);
    if (!sourceModule)
        return {};

    ModulePtr module;
    if (frontend.getLuauSolverMode() == SolverMode::New)
        module = frontend.moduleResolver.getModule(moduleName);
    else
        module = frontend.moduleResolverForAutocomplete.getModule(moduleName);

    if (!module)
        return {};

    NotNull<BuiltinTypes> builtinTypes = frontend.builtinTypes;
    Scope* globalScope;
    if (frontend.getLuauSolverMode() == SolverMode::New)
        globalScope = frontend.globals.globalScope.get();
    else
        globalScope = frontend.globalsForAutocomplete.globalScope.get();

    TypeArena typeArena;
    if (FFlag::LuauSuggestHotComments)
    {
        bool isInHotComment = isWithinHotComment(*sourceModule, position);
        if (isWithinComment(*sourceModule, position) && !isInHotComment)
            return {};

        std::vector<AstNode*> ancestry = findAncestryAtPositionForAutocomplete(*sourceModule, position);
        LUAU_ASSERT(!ancestry.empty());
        ScopePtr startScope = findScopeAtPosition(*module, position);

        return autocomplete_(
            module, builtinTypes, &typeArena, ancestry, globalScope, startScope, position, frontend.fileResolver, std::move(callback), isInHotComment
        );
    }
    else
    {
        if (isWithinComment(*sourceModule, position))
            return {};

        std::vector<AstNode*> ancestry = findAncestryAtPositionForAutocomplete(*sourceModule, position);
        LUAU_ASSERT(!ancestry.empty());
        ScopePtr startScope = findScopeAtPosition(*module, position);

        return autocomplete_(
            module, builtinTypes, &typeArena, ancestry, globalScope, startScope, position, frontend.fileResolver, std::move(callback)
        );
    }
}

} // namespace Luau
