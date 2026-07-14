// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/RequireNavigator.h"
#include "Luau/VfsNavigator.h"

struct FileNavigationContext : Luau::Require::NavigationContext
{
    using NavigateResult = Luau::Require::NavigationContext::NavigateResult;
    using ConfigStatus = Luau::Require::NavigationContext::ConfigStatus;

    FileNavigationContext(std::string requirerPath);

    // Navigation interface
    NavigateResult resetToRequirer() override;
    NavigateResult jumpToAlias(const std::string& path) override;

    NavigateResult toParent() override;
    NavigateResult toChild(const std::string& component) override;

    ConfigStatus getConfigStatus() const override;
    ConfigBehavior getConfigBehavior() const override;
    std::optional<std::string> getAlias(const std::string& alias) const override;
    std::optional<std::string> getConfig() const override;

    // Custom capabilities
    bool isModulePresent() const;
    std::optional<std::string> getIdentifier() const;

private:
    std::string requirerPath;
    VfsNavigator vfs;
};
