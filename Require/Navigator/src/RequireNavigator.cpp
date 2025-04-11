// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/RequireNavigator.h"

#include "Luau/PathUtilities.h"

#include "Luau/Config.h"

#include <algorithm>
#include <optional>
#include <utility>

static constexpr char kRequireErrorAmbiguous[] = "require path could not be resolved to a unique file";
static constexpr char kRequireErrorGeneric[] = "error requiring module";

namespace Luau::Require
{

using Error = std::optional<std::string>;

static Error toError(NavigationContext::NavigateResult result)
{
    if (result == NavigationContext::NavigateResult::Success)
        return std::nullopt;
    if (result == NavigationContext::NavigateResult::Ambiguous)
        return kRequireErrorAmbiguous;
    else
        return kRequireErrorGeneric;
}

static std::string extractAlias(std::string_view path)
{
    // To ignore the '@' alias prefix when processing the alias
    const size_t aliasStartPos = 1;

    // If a directory separator was found, the length of the alias is the
    // distance between the start of the alias and the separator. Otherwise,
    // the whole string after the alias symbol is the alias.
    size_t aliasLen = path.find_first_of('/');
    if (aliasLen != std::string::npos)
        aliasLen -= aliasStartPos;

    return std::string{path.substr(aliasStartPos, aliasLen)};
}

Navigator::Navigator(NavigationContext& navigationContext, ErrorHandler& errorHandler)
    : navigationContext(navigationContext)
    , errorHandler(errorHandler)
{
}

Navigator::Status Navigator::navigate(std::string path)
{
    std::replace(path.begin(), path.end(), '\\', '/');

    if (Error error = toError(navigationContext.reset(navigationContext.getRequirerIdentifier())))
    {
        errorHandler.reportError(*error);
        return Status::ErrorReported;
    }

    if (Error error = navigateImpl(path))
    {
        errorHandler.reportError(*error);
        return Status::ErrorReported;
    }

    return Status::Success;
}

Error Navigator::navigateImpl(std::string_view path)
{
    PathType pathType = getPathType(path);

    if (pathType == PathType::Unsupported)
        return "require path must start with a valid prefix: ./, ../, or @";

    if (pathType == PathType::Aliased)
    {
        std::string alias = extractAlias(path);
        std::transform(
            alias.begin(),
            alias.end(),
            alias.begin(),
            [](unsigned char c)
            {
                return ('A' <= c && c <= 'Z') ? (c + ('a' - 'A')) : c;
            }
        );

        if (Error error = navigateToAndPopulateConfig(alias))
            return error;

        if (!config.aliases.contains(alias))
        {
            if (alias != "self")
                return "@" + alias + " is not a valid alias";

            // If the alias is "@self", we reset to the requirer's context and
            // navigate directly from there.
            if (Error error = toError(navigationContext.reset(navigationContext.getRequirerIdentifier())))
                return error;
            if (Error error = navigateThroughPath(path))
                return error;

            return std::nullopt;
        }

        if (Error error = navigateToAlias(alias, config.aliases[alias].value))
            return error;
        if (Error error = navigateThroughPath(path))
            return error;
    }

    if (pathType == PathType::RelativeToCurrent || pathType == PathType::RelativeToParent)
    {
        if (Error error = toError(navigationContext.toParent()))
            return error;
        if (Error error = navigateThroughPath(path))
            return error;
    }

    return std::nullopt;
}

Error Navigator::navigateThroughPath(std::string_view path)
{
    std::pair<std::string_view, std::string_view> components = splitPath(path);
    if (path.size() >= 1 && path[0] == '@')
    {
        // If the path is aliased, we ignore the alias: this function assumes
        // that navigation to an alias is handled by the caller.
        components = splitPath(components.second);
    }

    while (!(components.first.empty() && components.second.empty()))
    {
        if (components.first == "." || components.first.empty())
        {
            components = splitPath(components.second);
            continue;
        }
        else if (components.first == "..")
        {
            if (Error error = toError(navigationContext.toParent()))
                return error;
        }
        else
        {
            if (Error error = toError(navigationContext.toChild(std::string{components.first})))
                return error;
        }
        components = splitPath(components.second);
    }

    return std::nullopt;
}

Error Navigator::navigateToAlias(const std::string& alias, const std::string& value)
{
    PathType pathType = getPathType(value);

    if (pathType == PathType::RelativeToCurrent || pathType == PathType::RelativeToParent)
    {
        if (Error error = navigateThroughPath(value))
            return error;
    }
    else if (pathType == PathType::Aliased)
    {
        return "@" + alias + " cannot point to other aliases";
    }
    else
    {
        if (Error error = toError(navigationContext.jumpToAlias(value)))
            return error;
    }

    return std::nullopt;
}

Error Navigator::navigateToAndPopulateConfig(const std::string& desiredAlias)
{
    while (!config.aliases.contains(desiredAlias))
    {
        if (navigationContext.toParent() != NavigationContext::NavigateResult::Success)
            break;

        if (navigationContext.isConfigPresent())
        {
            std::optional<std::string> configContents = navigationContext.getConfig();
            if (!configContents)
                return "could not get configuration file contents";

            Luau::ConfigOptions opts;
            Luau::ConfigOptions::AliasOptions aliasOpts;
            aliasOpts.configLocation = "unused";
            aliasOpts.overwriteAliases = false;
            opts.aliasOptions = std::move(aliasOpts);

            if (Error error = Luau::parseConfig(*configContents, config, opts))
                return error;
        }
    };

    return std::nullopt;
}

} // namespace Luau::Require
