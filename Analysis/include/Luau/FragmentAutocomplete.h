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

enum class FragmentTypeCheckStatus
{
    SkipAutocomplete,
    Success,
};

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
    std::vector<Comment> commentLocations;
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

std::optional<FragmentParseResult> parseFragment(
    const SourceModule& srcModule,
    std::string_view src,
    const Position& cursorPos,
    std::optional<Position> fragmentEndPosition
);

std::pair<FragmentTypeCheckStatus, FragmentTypeCheckResult> typecheckFragment(
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

enum class FragmentAutocompleteStatus
{
    Success,
    FragmentTypeCheckFail,
    InternalIce
};

struct FragmentAutocompleteStatusResult
{
    FragmentAutocompleteStatus status;
    std::optional<FragmentAutocompleteResult> result;
};

struct FragmentContext
{
    std::string_view newSrc;
    const ParseResult& newAstRoot;
    std::optional<FrontendOptions> opts;
    std::optional<Position> DEPRECATED_fragmentEndPosition;
};

/**
 * @brief Attempts to compute autocomplete suggestions from the fragment context.
 *
 * This function computes autocomplete suggestions using outdated frontend typechecking data
 * by patching the fragment context of the new script source content.
 *
 * @param frontend The Luau Frontend data structure, which may contain outdated typechecking data.
 *
 * @param moduleName The name of the target module, specifying which script the caller wants to request autocomplete for.
 *
 * @param cursorPosition The position in the script where the caller wants to trigger autocomplete.
 *
 * @param context The fragment context that this API will use to patch the outdated typechecking data.
 *
 * @param stringCompletionCB A callback function that provides autocomplete suggestions for string contexts.
 *
 * @return
 * The status indicating whether `fragmentAutocomplete` ran successfully or failed, along with the reason for failure.
 * Also includes autocomplete suggestions if the status is successful.
 *
 * @usage
 * FragmentAutocompleteStatusResult acStatusResult;
 * if (shouldFragmentAC)
 *     acStatusResult = Luau::tryFragmentAutocomplete(...);
 *
 * if (acStatusResult.status != Successful)
 * {
 *     frontend.check(moduleName, options);
 *     acStatusResult.acResult = Luau::autocomplete(...);
 * }
 * return convertResultWithContext(acStatusResult.acResult);
 */
FragmentAutocompleteStatusResult tryFragmentAutocomplete(
    Frontend& frontend,
    const ModuleName& moduleName,
    Position cursorPosition,
    FragmentContext context,
    StringCompletionCallback stringCompletionCB
);

} // namespace Luau
