// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/RequireNavigator.h"
#include "Luau/Require.h"

#include <string>

struct lua_State;
struct luarequire_Configuration;

namespace Luau::Require
{

class RuntimeNavigationContext : public NavigationContext
{
public:
    RuntimeNavigationContext(luarequire_Configuration* config, lua_State* L, void* ctx, std::string requirerChunkname);

    std::string getRequirerIdentifier() const override;

    // Navigation interface
    NavigateResult reset(const std::string& requirerChunkname) override;
    NavigateResult jumpToAlias(const std::string& path) override;

    NavigateResult toParent() override;
    NavigateResult toChild(const std::string& component) override;

    bool isConfigPresent() const override;
    NavigationContext::ConfigBehavior getConfigBehavior() const override;
    std::optional<std::string> getAlias(const std::string& alias) const override;
    std::optional<std::string> getConfig() const override;

    // Custom capabilities
    bool isModulePresent() const;
    std::optional<std::string> getChunkname() const;
    std::optional<std::string> getLoadname() const;
    std::optional<std::string> getCacheKey() const;

private:
    std::optional<std::string> getStringFromCWriter(
        luarequire_WriteResult (*writer)(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out),
        size_t initalBufferSize
    ) const;
    std::optional<std::string> getStringFromCWriterWithInput(
        luarequire_WriteResult (*writer)(lua_State* L, void* ctx, const char* input, char* buffer, size_t buffer_size, size_t* size_out),
        std::string input,
        size_t initalBufferSize
    ) const;

    luarequire_Configuration* config;
    lua_State* L;
    void* ctx;
    std::string requirerChunkname;
};

// Non-throwing error reporter
class RuntimeErrorHandler : public ErrorHandler
{
public:
    RuntimeErrorHandler(std::string requiredPath);
    void reportError(std::string message) override;

    const std::string& getReportedError() const;

private:
    std::string errorPrefix;
    std::string errorMessage;
};

} // namespace Luau::Require
