// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash.h"
#include "Luau/Ast.h"

#include <vector>


namespace Luau
{

struct FragmentAutocompleteAncestryResult
{
    DenseHashMap<AstName, AstLocal*> localMap{AstName()};
    std::vector<AstLocal*> localStack;
    std::vector<AstNode*> ancestry;
    AstStat* nearestStatement;
};

FragmentAutocompleteAncestryResult findAncestryForFragmentParse(AstStatBlock* root, const Position& cursorPos);

} // namespace Luau
