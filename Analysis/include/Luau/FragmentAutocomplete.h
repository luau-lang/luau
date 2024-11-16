// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Parser.h"
#include "Luau/AutocompleteTypes.h"
#include "Luau/DenseHash.h"
#include "Luau/Module.h"
#include "Luau/Frontend.h"

#include <memory>
#include <vector>

namespace Luau
{
struct FrontendOptions;

struct FragmentAutocompleteAncestryResult
{
    DenseHashMap<AstName, AstLocal*> localMap{AstName()};
    std::vector<AstLocal*> localStack;
    std::vector<AstNode*> ancestry;
    AstStat* nearestStatement = nullptr;
};

struct FragmentParseResult
{
    std::string fragmentToParse;
    AstStatBlock* root = nullptr;
    std::vector<AstNode*> ancestry;
    AstStat* nearestStatement = nullptr;
    std::unique_ptr<Allocator> alloc = std::make_unique<Allocator>();
};

struct FragmentTypeCheckResult
{
    ModulePtr incrementalModule = nullptr;
    ScopePtr freshScope;
    std::vector<AstNode*> ancestry;
};

struct FragmentAutocompleteResult
{
    ModulePtr incrementalModule;
    Scope* freshScope;
    TypeArena arenaForAutocomplete;
    AutocompleteResult acResults;
};

FragmentAutocompleteAncestryResult findAncestryForFragmentParse(AstStatBlock* root, const Position& cursorPos);

FragmentParseResult parseFragment(
    const SourceModule& srcModule,
    std::string_view src,
    const Position& cursorPos,
    std::optional<Position> fragmentEndPosition
);

FragmentTypeCheckResult typecheckFragment(
    Frontend& frontend,
    const ModuleName& moduleName,
    const Position& cursorPos,
    std::optional<FrontendOptions> opts,
    std::string_view src,
    std::optional<Position> fragmentEndPosition
);

FragmentAutocompleteResult fragmentAutocomplete(
    Frontend& frontend,
    std::string_view src,
    const ModuleName& moduleName,
    Position cursorPosition,
    std::optional<FrontendOptions> opts,
    StringCompletionCallback callback,
    std::optional<Position> fragmentEndPosition = std::nullopt
);


} // namespace Luau
