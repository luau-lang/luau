// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Parser.h"
#include "Luau/Autocomplete.h"
#include "Luau/DenseHash.h"
#include "Luau/Module.h"

#include <memory>
#include <vector>

namespace Luau
{

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
    std::unique_ptr<Allocator> alloc = std::make_unique<Allocator>();
};

FragmentAutocompleteAncestryResult findAncestryForFragmentParse(AstStatBlock* root, const Position& cursorPos);

FragmentParseResult parseFragment(const SourceModule& srcModule, std::string_view src, const Position& cursorPos);

AutocompleteResult fragmentAutocomplete(
    Frontend& frontend,
    std::string_view src,
    const ModuleName& moduleName,
    Position& cursorPosition,
    StringCompletionCallback callback
);


} // namespace Luau
