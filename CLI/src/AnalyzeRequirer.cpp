// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AnalyzeRequirer.h"

#include "Luau/FileUtils.h"
#include "Luau/RequireNavigator.h"
#include "Luau/VfsNavigator.h"

#include <string>

static Luau::Require::NavigationContext::NavigateResult convert(NavigationStatus status)
{
    if (status == NavigationStatus::Success)
        return Luau::Require::NavigationContext::NavigateResult::Success;
    else if (status == NavigationStatus::Ambiguous)
        return Luau::Require::NavigationContext::NavigateResult::Ambiguous;
    else
        return Luau::Require::NavigationContext::NavigateResult::NotFound;
}

FileNavigationContext::FileNavigationContext(std::string requirerPath)
    : requirerPath(std::move(requirerPath))
{
}

std::string FileNavigationContext::getRequirerIdentifier() const
{
    return requirerPath;
}

Luau::Require::NavigationContext::NavigateResult FileNavigationContext::reset(const std::string& identifier)
{
    if (identifier == "-")
        return convert(vfs.resetToStdIn());

    return convert(vfs.resetToPath(identifier));
}

Luau::Require::NavigationContext::NavigateResult FileNavigationContext::jumpToAlias(const std::string& path)
{
    if (!isAbsolutePath(path))
        return Luau::Require::NavigationContext::NavigateResult::NotFound;

    return convert(vfs.resetToPath(path));
}

Luau::Require::NavigationContext::NavigateResult FileNavigationContext::toParent()
{
    return convert(vfs.toParent());
}

Luau::Require::NavigationContext::NavigateResult FileNavigationContext::toChild(const std::string& component)
{
    return convert(vfs.toChild(component));
}

bool FileNavigationContext::isModulePresent() const
{
    return isFile(vfs.getAbsoluteFilePath());
}

std::optional<std::string> FileNavigationContext::getIdentifier() const
{
    return vfs.getAbsoluteFilePath();
}

bool FileNavigationContext::isConfigPresent() const
{
    return isFile(vfs.getLuaurcPath());
}

Luau::Require::NavigationContext::ConfigBehavior FileNavigationContext::getConfigBehavior() const
{
    return Luau::Require::NavigationContext::ConfigBehavior::GetConfig;
}

std::optional<std::string> FileNavigationContext::getAlias(const std::string& alias) const
{
    return std::nullopt;
}

std::optional<std::string> FileNavigationContext::getConfig() const
{
    return readFile(vfs.getLuaurcPath());
}
