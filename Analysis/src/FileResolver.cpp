// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/FileResolver.h"

#include "Luau/Common.h"
#include "Luau/StringUtils.h"

#include <memory>
#include <optional>
#include <string_view>
#include <utility>

namespace Luau
{

static std::optional<RequireSuggestions> processRequireSuggestions(std::optional<RequireSuggestions> suggestions)
{
    if (!suggestions)
        return suggestions;

    for (RequireSuggestion& suggestion : *suggestions)
    {
        suggestion.fullPath = escape(suggestion.fullPath);
    }

    return suggestions;
}

static RequireSuggestions makeSuggestionsFromAliases(std::vector<RequireAlias> aliases)
{
    RequireSuggestions result;
    for (RequireAlias& alias : aliases)
    {
        RequireSuggestion suggestion;
        suggestion.label = "@" + std::move(alias.alias);
        suggestion.fullPath = suggestion.label;
        suggestion.tags = std::move(alias.tags);
        result.push_back(std::move(suggestion));
    }
    return result;
}

static RequireSuggestions makeSuggestionsForFirstComponent(std::unique_ptr<RequireNode> node)
{
    RequireSuggestions result = makeSuggestionsFromAliases(node->getAvailableAliases());
    result.push_back(RequireSuggestion{"./", "./", {}});
    result.push_back(RequireSuggestion{"../", "../", {}});
    return result;
}

static RequireSuggestions makeSuggestionsFromNode(std::unique_ptr<RequireNode> node, const std::string_view path, bool isPartialPath)
{
    LUAU_ASSERT(!path.empty());

    RequireSuggestions result;

    const size_t lastSlashInPath = path.find_last_of('/');

    if (lastSlashInPath != std::string_view::npos)
    {
        // Add a suggestion for the parent directory
        RequireSuggestion parentSuggestion;
        parentSuggestion.label = "..";

        // TODO: after exposing require-by-string's path normalization API, this
        // if-else can be replaced. Instead, we can simply normalize the result
        // of inserting ".." at the end of the current path.
        if (lastSlashInPath >= 2 && path.substr(lastSlashInPath - 2, 3) == "../")
        {
            parentSuggestion.fullPath = path.substr(0, lastSlashInPath + 1);
            parentSuggestion.fullPath += "..";
        }
        else
        {
            parentSuggestion.fullPath = path.substr(0, lastSlashInPath);
        }

        result.push_back(std::move(parentSuggestion));
    }

    std::string fullPathPrefix;
    if (isPartialPath)
    {
        // ./path/to/chi -> ./path/to/
        fullPathPrefix += path.substr(0, lastSlashInPath + 1);
    }
    else
    {
        if (path.back() == '/')
        {
            // ./path/to/ -> ./path/to/
            fullPathPrefix += path;
        }
        else
        {
            // ./path/to -> ./path/to/
            fullPathPrefix += path;
            fullPathPrefix += "/";
        }
    }

    for (const std::unique_ptr<RequireNode>& child : node->getChildren())
    {
        if (!child)
            continue;

        std::string pathComponent = child->getPathComponent();

        // If path component contains a slash, it cannot be required by string.
        // There's no point suggesting it.
        if (pathComponent.find('/') != std::string::npos)
            continue;

        RequireSuggestion suggestion;
        suggestion.label = isPartialPath || path.back() == '/' ? child->getLabel() : "/" + child->getLabel();
        suggestion.fullPath = fullPathPrefix + std::move(pathComponent);
        suggestion.tags = child->getTags();
        result.push_back(std::move(suggestion));
    }

    return result;
}

std::optional<RequireSuggestions> RequireSuggester::getRequireSuggestionsImpl(
    const ModuleName& requirer,
    const std::optional<std::string>& path
) const
{
    if (!path)
        return std::nullopt;

    std::unique_ptr<RequireNode> requirerNode = getNode(requirer);
    if (!requirerNode)
        return std::nullopt;

    const size_t slashPos = path->find_last_of('/');

    if (slashPos == std::string::npos)
        return makeSuggestionsForFirstComponent(std::move(requirerNode));

    // If path already points at a Node, return the Node's children as paths.
    if (std::unique_ptr<RequireNode> node = requirerNode->resolvePathToNode(*path))
        return makeSuggestionsFromNode(std::move(node), *path, /* isPartialPath = */ false);

    // Otherwise, recover a partial path and use this to generate suggestions.
    if (std::unique_ptr<RequireNode> partialNode = requirerNode->resolvePathToNode(path->substr(0, slashPos)))
        return makeSuggestionsFromNode(std::move(partialNode), *path, /* isPartialPath = */ true);

    return std::nullopt;
}

std::optional<RequireSuggestions> RequireSuggester::getRequireSuggestions(const ModuleName& requirer, const std::optional<std::string>& path) const
{
    return processRequireSuggestions(getRequireSuggestionsImpl(requirer, path));
}

std::optional<RequireSuggestions> FileResolver::getRequireSuggestions(const ModuleName& requirer, const std::optional<std::string>& path) const
{
    return requireSuggester ? requireSuggester->getRequireSuggestions(requirer, path) : std::nullopt;
}

} // namespace Luau
