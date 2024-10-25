// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/FragmentAutocomplete.h"

#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Common.h"
#include "Luau/Parser.h"
#include "Luau/ParseOptions.h"
#include "Luau/Module.h"
#include "Luau/TimeTrace.h"
#include "Luau/UnifierSharedState.h"
#include "Luau/TypeFunction.h"
#include "Luau/DataFlowGraph.h"
#include "Luau/ConstraintGenerator.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/Frontend.h"
#include "Luau/Parser.h"
#include "Luau/ParseOptions.h"
#include "Luau/Module.h"

LUAU_FASTINT(LuauTypeInferRecursionLimit);
LUAU_FASTINT(LuauTypeInferIterationLimit);
LUAU_FASTINT(LuauTarjanChildLimit)
LUAU_FASTFLAG(LuauAllowFragmentParsing);
LUAU_FASTFLAG(LuauStoreDFGOnModule2);

namespace
{
template<typename T>
void copyModuleVec(std::vector<T>& result, const std::vector<T>& input)
{
    result.insert(result.end(), input.begin(), input.end());
}

template<typename K, typename V>
void copyModuleMap(Luau::DenseHashMap<K, V>& result, const Luau::DenseHashMap<K, V>& input)
{
    for (auto [k, v] : input)
        result[k] = v;
}

} // namespace


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

ModulePtr copyModule(const ModulePtr& result, std::unique_ptr<Allocator> alloc)
{
    freeze(result->internalTypes);
    freeze(result->interfaceTypes);
    ModulePtr incrementalModule = std::make_shared<Module>();
    incrementalModule->name = result->name;
    incrementalModule->humanReadableName = result->humanReadableName;
    incrementalModule->allocator = std::move(alloc);
    // Don't need to keep this alive (it's already on the source module)
    copyModuleVec(incrementalModule->scopes, result->scopes);
    copyModuleMap(incrementalModule->astTypes, result->astTypes);
    copyModuleMap(incrementalModule->astTypePacks, result->astTypePacks);
    copyModuleMap(incrementalModule->astExpectedTypes, result->astExpectedTypes);
    // Don't need to clone astOriginalCallTypes
    copyModuleMap(incrementalModule->astOverloadResolvedTypes, result->astOverloadResolvedTypes);
    // Don't need to clone astForInNextTypes
    copyModuleMap(incrementalModule->astForInNextTypes, result->astForInNextTypes);
    // Don't need to clone astResolvedTypes
    // Don't need to clone astResolvedTypePacks
    // Don't need to clone upperBoundContributors
    copyModuleMap(incrementalModule->astScopes, result->astScopes);
    // Don't need to clone declared Globals;
    return incrementalModule;
}

FragmentTypeCheckResult typeCheckFragmentHelper(
    Frontend& frontend,
    AstStatBlock* root,
    const ModulePtr& stale,
    const ScopePtr& closestScope,
    const Position& cursorPos,
    std::unique_ptr<Allocator> astAllocator,
    const FrontendOptions& opts
)
{
    freeze(stale->internalTypes);
    freeze(stale->interfaceTypes);
    ModulePtr incrementalModule = copyModule(stale, std::move(astAllocator));
    unfreeze(incrementalModule->internalTypes);
    unfreeze(incrementalModule->interfaceTypes);

    /// Setup typecheck limits
    TypeCheckLimits limits;
    if (opts.moduleTimeLimitSec)
        limits.finishTime = TimeTrace::getClock() + *opts.moduleTimeLimitSec;
    else
        limits.finishTime = std::nullopt;
    limits.cancellationToken = opts.cancellationToken;

    /// Icehandler
    NotNull<InternalErrorReporter> iceHandler{&frontend.iceHandler};
    /// Make the shared state for the unifier (recursion + iteration limits)
    UnifierSharedState unifierState{iceHandler};
    unifierState.counters.recursionLimit = FInt::LuauTypeInferRecursionLimit;
    unifierState.counters.iterationLimit = limits.unifierIterationLimit.value_or(FInt::LuauTypeInferIterationLimit);

    /// Initialize the normalizer
    Normalizer normalizer{&incrementalModule->internalTypes, frontend.builtinTypes, NotNull{&unifierState}};

    /// User defined type functions runtime
    TypeFunctionRuntime typeFunctionRuntime(iceHandler, NotNull{&limits});

    /// Create a DataFlowGraph just for the surrounding context
    auto updatedDfg = DataFlowGraphBuilder::updateGraph(*stale->dataFlowGraph.get(), stale->dfgScopes, root, cursorPos, iceHandler);

    /// Contraint Generator
    ConstraintGenerator cg{
        incrementalModule,
        NotNull{&normalizer},
        NotNull{&typeFunctionRuntime},
        NotNull{&frontend.moduleResolver},
        frontend.builtinTypes,
        iceHandler,
        frontend.globals.globalScope,
        nullptr,
        nullptr,
        NotNull{&updatedDfg},
        {}
    };
    cg.rootScope = stale->getModuleScope().get();
    // Any additions to the scope must occur in a fresh scope
    auto freshChildOfNearestScope = std::make_shared<Scope>(closestScope);
    incrementalModule->scopes.push_back({root->location, freshChildOfNearestScope});

    // closest Scope -> children = { ...., freshChildOfNearestScope}
    // We need to trim nearestChild from the scope hierarcy
    closestScope->children.push_back(NotNull{freshChildOfNearestScope.get()});
    // Visit just the root - we know the scope it should be in
    cg.visitFragmentRoot(freshChildOfNearestScope, root);
    // Trim nearestChild from the closestScope
    Scope* back = closestScope->children.back().get();
    LUAU_ASSERT(back == freshChildOfNearestScope.get());
    closestScope->children.pop_back();

    /// Initialize the constraint solver and run it
    ConstraintSolver cs{
        NotNull{&normalizer},
        NotNull{&typeFunctionRuntime},
        NotNull(cg.rootScope),
        borrowConstraints(cg.constraints),
        incrementalModule->name,
        NotNull{&frontend.moduleResolver},
        {},
        nullptr,
        NotNull{&updatedDfg},
        limits
    };

    try
    {
        cs.run();
    }
    catch (const TimeLimitError&)
    {
        stale->timeout = true;
    }
    catch (const UserCancelError&)
    {
        stale->cancelled = true;
    }

    // In frontend we would forbid internal types
    // because this is just for autocomplete, we don't actually care
    // We also don't even need to typecheck - just synthesize types as best as we can

    freeze(incrementalModule->internalTypes);
    freeze(incrementalModule->interfaceTypes);
    return {std::move(incrementalModule), freshChildOfNearestScope.get()};
}


FragmentTypeCheckResult typecheckFragment(
    Frontend& frontend,
    const ModuleName& moduleName,
    const Position& cursorPos,
    std::optional<FrontendOptions> opts,
    std::string_view src
)
{
    const SourceModule* sourceModule = frontend.getSourceModule(moduleName);
    if (!sourceModule)
    {
        LUAU_ASSERT(!"Expected Source Module for fragment typecheck");
        return {};
    }

    ModulePtr module = frontend.moduleResolver.getModule(moduleName);
    const ScopePtr& closestScope = findClosestScope(module, cursorPos);


    FragmentParseResult r = parseFragment(*sourceModule, src, cursorPos);
    FrontendOptions frontendOptions = opts.value_or(frontend.options);
    return typeCheckFragmentHelper(frontend, r.root, module, closestScope, cursorPos, std::move(r.alloc), frontendOptions);
}

AutocompleteResult fragmentAutocomplete(
    Frontend& frontend,
    std::string_view src,
    const ModuleName& moduleName,
    Position& cursorPosition,
    const FrontendOptions& opts,
    StringCompletionCallback callback
)
{
    LUAU_ASSERT(FFlag::LuauSolverV2);
    LUAU_ASSERT(FFlag::LuauAllowFragmentParsing);
    LUAU_ASSERT(FFlag::LuauStoreDFGOnModule2);
    return {};
}

} // namespace Luau
