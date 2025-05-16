// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/RequireNavigator.h"
#include "Luau/VfsNavigator.h"

struct FileNavigationContext : Luau::Require::NavigationContext
{
    using NavigateResult = Luau::Require::NavigationContext::NavigateResult;

    FileNavigationContext(std::string requirerPath);

    std::string getRequirerIdentifier() const override;

    // Navigation interface
    NavigateResult reset(const std::string& identifier) override;
    NavigateResult jumpToAlias(const std::string& path) override;

    NavigateResult toParent() override;
    NavigateResult toChild(const std::string& component) override;

    bool isConfigPresent() const override;
    virtual ConfigBehavior getConfigBehavior() const override;
    virtual std::optional<std::string> getAlias(const std::string& alias) const override;
    virtual std::optional<std::string> getConfig() const override;

    // Custom capabilities
    bool isModulePresent() const;
    std::optional<std::string> getIdentifier() const;

private:
    std::string requirerPath;
    VfsNavigator vfs;
};
