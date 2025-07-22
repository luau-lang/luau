// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/RequireNavigator.h"

#include "Luau/PathUtilities.h"

#include "Luau/Config.h"

#include <algorithm>
#include <optional>
#include <utility>

namespace Luau::Require
{

using Error = std::optional<std::string>;

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

    if (Error error = resetToRequirer())
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

        if (!foundAliasValue)
        {
            if (alias != "self")
                return "@" + alias + " is not a valid alias";

            // If the alias is "@self", we reset to the requirer's context and
            // navigate directly from there.
            if (Error error = resetToRequirer())
                return error;
            if (Error error = navigateThroughPath(path))
                return error;

            return std::nullopt;
        }

        if (Error error = navigateToAlias(alias, *foundAliasValue))
            return error;
        if (Error error = navigateThroughPath(path))
            return error;
    }

    if (pathType == PathType::RelativeToCurrent || pathType == PathType::RelativeToParent)
    {
        if (Error error = navigateToParent(std::nullopt))
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

    std::optional<std::string> previousComponent;
    while (!(components.first.empty() && components.second.empty()))
    {
        if (components.first == "." || components.first.empty())
        {
            components = splitPath(components.second);
            continue;
        }
        else if (components.first == "..")
        {
            if (Error error = navigateToParent(previousComponent))
                return error;
        }
        else
        {
            if (Error error = navigateToChild(std::string{components.first}))
                return error;
        }
        previousComponent = components.first;
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
        return "alias \"@" + alias + "\" cannot point to an aliased path (\"" + value + "\")";
    }
    else
    {
        if (Error error = jumpToAlias(value))
            return error;
    }

    return std::nullopt;
}

Error Navigator::navigateToAndPopulateConfig(const std::string& desiredAlias)
{
    Luau::Config config;

    while (!foundAliasValue)
    {
        if (navigationContext.toParent() != NavigationContext::NavigateResult::Success)
            break;

        if (navigationContext.isConfigPresent())
        {
            if (navigationContext.getConfigBehavior() == NavigationContext::ConfigBehavior::GetAlias)
            {
                foundAliasValue = navigationContext.getAlias(desiredAlias);
            }
            else
            {
                std::optional<std::string> configContents = navigationContext.getConfig();
                if (!configContents)
                    return "could not get configuration file contents to resolve alias \"" + desiredAlias + "\"";

                Luau::ConfigOptions opts;
                Luau::ConfigOptions::AliasOptions aliasOpts;
                aliasOpts.configLocation = "unused";
                aliasOpts.overwriteAliases = false;
                opts.aliasOptions = std::move(aliasOpts);

                if (Error error = Luau::parseConfig(*configContents, config, opts))
                    return error;

                if (config.aliases.contains(desiredAlias))
                    foundAliasValue = config.aliases[desiredAlias].value;
            }
        }
    };

    return std::nullopt;
}

Error Navigator::resetToRequirer()
{
    NavigationContext::NavigateResult result = navigationContext.reset(navigationContext.getRequirerIdentifier());
    if (result == NavigationContext::NavigateResult::Success)
        return std::nullopt;

    std::string errorMessage = "could not reset to requiring context";
    if (result == NavigationContext::NavigateResult::Ambiguous)
        errorMessage += " (ambiguous)";
    return errorMessage;
}

Error Navigator::jumpToAlias(const std::string& aliasPath)
{
    NavigationContext::NavigateResult result = navigationContext.jumpToAlias(aliasPath);
    if (result == NavigationContext::NavigateResult::Success)
        return std::nullopt;

    std::string errorMessage = "could not jump to alias \"" + aliasPath + "\"";
    if (result == NavigationContext::NavigateResult::Ambiguous)
        errorMessage += " (ambiguous)";
    return errorMessage;
}

Error Navigator::navigateToParent(std::optional<std::string> previousComponent)
{
    NavigationContext::NavigateResult result = navigationContext.toParent();
    if (result == NavigationContext::NavigateResult::Success)
        return std::nullopt;

    std::string errorMessage;
    if (previousComponent)
        errorMessage = "could not get parent of component \"" + *previousComponent + "\"";
    else
        errorMessage = "could not get parent of requiring context";
    if (result == NavigationContext::NavigateResult::Ambiguous)
        errorMessage += " (ambiguous)";
    return errorMessage;
}

Error Navigator::navigateToChild(const std::string& component)
{
    NavigationContext::NavigateResult result = navigationContext.toChild(component);
    if (result == NavigationContext::NavigateResult::Success)
        return std::nullopt;

    std::string errorMessage = "could not resolve child component \"" + component + "\"";
    if (result == NavigationContext::NavigateResult::Ambiguous)
        errorMessage += " (ambiguous)";
    return errorMessage;
}

} // namespace Luau::Require
