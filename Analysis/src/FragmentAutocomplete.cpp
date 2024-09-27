// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/FragmentAutocomplete.h"

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"

namespace Luau
{

FragmentAutocompleteAncestryResult findAncestryForFragmentParse(AstStatBlock* root, const Position& cursorPos)
{
    std::vector<AstNode*> ancestry = findAncestryAtPositionForAutocomplete(root, cursorPos);
    DenseHashMap<AstName, AstLocal*> localMap{AstName()};
    std::vector<AstLocal*> localStack;
    AstStat* nearestStatement = nullptr;
    for (AstNode* node : ancestry)
    {
        if (auto block = node->as<AstStatBlock>())
        {
            for (auto stat : block->body)
            {
                if (stat->location.begin <= cursorPos)
                    nearestStatement = stat;
                if (stat->location.begin <= cursorPos)
                {
                    // This statement precedes the current one
                    if (auto loc = stat->as<AstStatLocal>())
                    {
                        for (auto v : loc->vars)
                        {
                            localStack.push_back(v);
                            localMap[v->name] = v;
                        }
                    }
                    else if (auto locFun = stat->as<AstStatLocalFunction>())
                    {
                        localStack.push_back(locFun->name);
                        localMap[locFun->name->name] = locFun->name;
                    }
                }
            }
        }
    }

    return {std::move(localMap), std::move(localStack), std::move(ancestry), std::move(nearestStatement)};
}

} // namespace Luau
