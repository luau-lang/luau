// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Config.h"
#include "Luau/LuauConfig.h"

#include <functional>
#include <optional>
#include <string>
#include <string_view>

struct lua_State;

////////////////////////////////////////////////////////////////////////////////
//
// This file provides a C++ interface for navigating the context in which
// require-by-string operates. This is used internally by the require-by-string
// runtime library to resolve paths based on the rules defined by its consumers.
//
// Including this file directly allows for inspection of the require-by-string
// path resolution algorithm's behavior without enabling the runtime library,
// which can be useful for static tooling.
//
////////////////////////////////////////////////////////////////////////////////

namespace Luau::Require
{

class AliasCycleTracker;

// The ErrorHandler interface is used to report errors during navigation.
// The default implementation does nothing but can be overridden to enable
// custom error handling behavior.
class ErrorHandler
{
public:
    virtual ~ErrorHandler() = default;
    virtual void reportError(std::string message) {}
};

// NavigationContext is an pure virtual class that is intended to be implemented
// and injected into a Navigator.
//
// When a Navigator traverses a require path, its NavigationContext's methods
// are invoked, with the expectation that the NavigationContext will keep track
// of the current state of the navigation and provide information about the
// current context as needed.
class NavigationContext
{
public:
    virtual ~NavigationContext() = default;
    virtual std::string getRequirerIdentifier() const = 0;

    enum class NavigateResult
    {
        Success,
        Ambiguous,
        NotFound
    };

    virtual NavigateResult reset(const std::string& identifier) = 0;
    virtual NavigateResult jumpToAlias(const std::string& path) = 0;

    virtual NavigateResult toAliasOverride(const std::string& aliasUnprefixed)
    {
        return NavigateResult::NotFound;
    };
    virtual NavigateResult toAliasFallback(const std::string& aliasUnprefixed)
    {
        return NavigateResult::NotFound;
    };

    virtual NavigateResult toParent() = 0;
    virtual NavigateResult toChild(const std::string& component) = 0;

    enum class ConfigBehavior
    {
        GetAlias,
        GetConfig
    };

    enum class ConfigStatus
    {
        Absent,
        Ambiguous,
        PresentJson,
        PresentLuau
    };

    virtual ConfigStatus getConfigStatus() const = 0;

    std::function<void(lua_State*)> luauConfigInit = nullptr;
    void (*luauConfigInterrupt)(lua_State* L, int gc) = nullptr;

    // The result of getConfigBehavior determines whether getAlias or getConfig
    // is called when getConfigStatus indicates a configuration is present.
    virtual ConfigBehavior getConfigBehavior() const = 0;
    virtual std::optional<std::string> getAlias(const std::string& alias) const = 0;
    virtual std::optional<std::string> getConfig() const = 0;
};

// The Navigator class is responsible for traversing a given require path in the
// context of a given NavigationContext.
//
// The Navigator is not intended to be overridden. Rather, it expects a custom
// injected NavigationContext that provides the desired navigation behavior.
class Navigator
{
public:
    enum class Status
    {
        Success,
        ErrorReported
    };

    Navigator(NavigationContext& navigationContext, ErrorHandler& errorHandler);
    [[nodiscard]] Status navigate(std::string path);

private:
    using Error = std::optional<std::string>;
    [[nodiscard]] Error navigateImpl(std::string_view path);
    [[nodiscard]] Error navigateThroughPath(std::string_view path);
    [[nodiscard]] Error navigateToAlias(const std::string& alias, const Config& config, AliasCycleTracker cycleTracker);
    [[nodiscard]] Error navigateToAndPopulateConfig(const std::string& desiredAlias, Config& config);

    [[nodiscard]] Error resetToRequirer();
    [[nodiscard]] Error jumpToAlias(const std::string& aliasPath);
    [[nodiscard]] Error navigateToParent(std::optional<std::string> previousComponent);
    [[nodiscard]] Error navigateToChild(const std::string& component);

    [[nodiscard]] std::pair<Error, bool> toAliasOverride(const std::string& aliasUnprefixed);
    [[nodiscard]] Error toAliasFallback(const std::string& aliasUnprefixed);

    NavigationContext& navigationContext;
    ErrorHandler& errorHandler;

    std::optional<std::string> foundAliasValue;
};

} // namespace Luau::Require
