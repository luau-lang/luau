// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/FragmentAutocomplete.h"

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Common.h"
#include "Luau/Frontend.h"
#include "Luau/Parser.h"
#include "Luau/ParseOptions.h"
#include "Luau/Module.h"

namespace Luau
{

FragmentAutocompleteAncestryResult findAncestryForFragmentParse(AstStatBlock* root, const Position& cursorPos)
{
    std::vector<AstNode*> ancestry = findAncestryAtPositionForAutocomplete(root, cursorPos);
    // Should always contain the root AstStat
    LUAU_ASSERT(ancestry.size() >= 1);
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
                if (stat->location.begin < cursorPos && stat->location.begin.line < cursorPos.line)
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

    if (!nearestStatement)
        nearestStatement = ancestry[0]->asStat();
    LUAU_ASSERT(nearestStatement);
    return {std::move(localMap), std::move(localStack), std::move(ancestry), std::move(nearestStatement)};
}

std::pair<unsigned int, unsigned int> getDocumentOffsets(const std::string_view& src, const Position& startPos, const Position& endPos)
{
    unsigned int lineCount = 0;
    unsigned int colCount = 0;

    unsigned int docOffset = 0;
    unsigned int startOffset = 0;
    unsigned int endOffset = 0;
    bool foundStart = false;
    bool foundEnd = false;
    for (char c : src)
    {
        if (foundStart && foundEnd)
            break;

        if (startPos.line == lineCount && startPos.column == colCount)
        {
            foundStart = true;
            startOffset = docOffset;
        }

        if (endPos.line == lineCount && endPos.column == colCount)
        {
            endOffset = docOffset;
            foundEnd = true;
        }

        if (c == '\n')
        {
            lineCount++;
            colCount = 0;
        }
        else
            colCount++;
        docOffset++;
    }


    unsigned int min = std::min(startOffset, endOffset);
    unsigned int len = std::max(startOffset, endOffset) - min;
    return {min, len};
}

ScopePtr findClosestScope(const ModulePtr& module, const Position& cursorPos)
{
    LUAU_ASSERT(module->hasModuleScope());

    ScopePtr closest = module->getModuleScope();
    for (auto [loc, sc] : module->scopes)
    {
        if (loc.begin <= cursorPos && closest->location.begin <= loc.begin)
            closest = sc;
    }

    return closest;
}

FragmentParseResult parseFragment(const SourceModule& srcModule, std::string_view src, const Position& cursorPos)
{
    FragmentAutocompleteAncestryResult result = findAncestryForFragmentParse(srcModule.root, cursorPos);
    ParseOptions opts;
    opts.allowDeclarationSyntax = false;
    opts.captureComments = false;
    opts.parseFragment = FragmentParseResumeSettings{std::move(result.localMap), std::move(result.localStack)};
    AstStat* enclosingStatement = result.nearestStatement;

    const Position& endPos = cursorPos;
    // If the statement starts on a previous line, grab the statement beginning
    // otherwise, grab the statement end to whatever is being typed right now
    const Position& startPos =
        enclosingStatement->location.begin.line == cursorPos.line ? enclosingStatement->location.begin : enclosingStatement->location.end;

    auto [offsetStart, parseLength] = getDocumentOffsets(src, startPos, endPos);

    const char* srcStart = src.data() + offsetStart;
    std::string_view dbg = src.substr(offsetStart, parseLength);
    const std::shared_ptr<AstNameTable>& nameTbl = srcModule.names;
    FragmentParseResult fragmentResult;
    fragmentResult.fragmentToParse = std::string(dbg.data(), parseLength);
    // For the duration of the incremental parse, we want to allow the name table to re-use duplicate names
    ParseResult p = Luau::Parser::parse(srcStart, parseLength, *nameTbl, *fragmentResult.alloc.get(), opts);

    std::vector<AstNode*> fabricatedAncestry = std::move(result.ancestry);
    std::vector<AstNode*> fragmentAncestry = findAncestryAtPositionForAutocomplete(p.root, p.root->location.end);
    fabricatedAncestry.insert(fabricatedAncestry.end(), fragmentAncestry.begin(), fragmentAncestry.end());
    if (enclosingStatement == nullptr)
        enclosingStatement = p.root;
    fragmentResult.root = std::move(p.root);
    fragmentResult.ancestry = std::move(fabricatedAncestry);
    return fragmentResult;
}


AutocompleteResult fragmentAutocomplete(
    Frontend& frontend,
    std::string_view src,
    const ModuleName& moduleName,
    Position& cursorPosition,
    StringCompletionCallback callback
)
{
    LUAU_ASSERT(FFlag::LuauSolverV2);
    // TODO
    return {};
}

} // namespace Luau
