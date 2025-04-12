// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AnalyzeRequirer.h"

#include "Luau/RequireNavigator.h"
#include "Luau/RequirerUtils.h"

#include <string>
#include <string_view>

Luau::Require::NavigationContext::NavigateResult FileNavigationContext::storePathResult(PathResult result)
{
    if (result.status == PathResult::Status::AMBIGUOUS)
        return Luau::Require::NavigationContext::NavigateResult::Ambiguous;

    if (result.status == PathResult::Status::NOT_FOUND)
        return Luau::Require::NavigationContext::NavigateResult::NotFound;

    path = result.absPath;
    suffix = result.suffix;

    return Luau::Require::NavigationContext::NavigateResult::Success;
}

FileNavigationContext::FileNavigationContext(std::string requirerPath)
{
    std::string_view path = requirerPath;
    if (path.size() >= 10 && path.substr(path.size() - 10) == "/init.luau")
    {
        path.remove_suffix(10);
    }
    else if (path.size() >= 9 && path.substr(path.size() - 9) == "/init.lua")
    {
        path.remove_suffix(9);
    }
    else if (path.size() >= 5 && path.substr(path.size() - 5) == ".luau")
    {
        path.remove_suffix(5);
    }
    else if (path.size() >= 4 && path.substr(path.size() - 4) == ".lua")
    {
        path.remove_suffix(4);
    }

    this->requirerPath = path;
}

std::string FileNavigationContext::getRequirerIdentifier() const
{
    return requirerPath;
}

Luau::Require::NavigationContext::NavigateResult FileNavigationContext::reset(const std::string& requirerChunkname)
{
    if (requirerChunkname == "-")
    {
        return storePathResult(getStdInResult());
    }

    return storePathResult(tryGetRelativePathResult(requirerChunkname));
}

Luau::Require::NavigationContext::NavigateResult FileNavigationContext::jumpToAlias(const std::string& path)
{
    Luau::Require::NavigationContext::NavigateResult result = storePathResult(getAbsolutePathResult(path));
    if (result != Luau::Require::NavigationContext::NavigateResult::Success)
        return result;

    return Luau::Require::NavigationContext::NavigateResult::Success;
}

Luau::Require::NavigationContext::NavigateResult FileNavigationContext::toParent()
{
    return storePathResult(getParent(path, path));
}

Luau::Require::NavigationContext::NavigateResult FileNavigationContext::toChild(const std::string& component)
{
    return storePathResult(getChild(path, path, component));
}

bool FileNavigationContext::isModulePresent() const
{
    return isFilePresent(path, suffix);
}

std::optional<std::string> FileNavigationContext::getIdentifier() const
{
    return path + suffix;
}

bool FileNavigationContext::isConfigPresent() const
{
    return isFilePresent(path, "/.luaurc");
}

std::optional<std::string> FileNavigationContext::getConfig() const
{
    return getFileContents(path, "/.luaurc");
}
