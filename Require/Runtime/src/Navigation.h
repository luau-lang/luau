// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/RequireNavigator.h"
#include "Luau/Require.h"

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
    std::optional<std::string> getConfig() const override;

    // Custom capabilities
    bool isModulePresent() const;
    std::optional<std::string> getContents() const;
    std::optional<std::string> getChunkname() const;
    std::optional<std::string> getCacheKey() const;

private:
    std::optional<std::string> getStringFromCWriter(
        luarequire_WriteResult (*writer)(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out),
        size_t initalBufferSize
    ) const;

    luarequire_Configuration* config;
    lua_State* L;
    void* ctx;
    std::string requirerChunkname;
};

class RuntimeErrorHandler : public ErrorHandler
{
public:
    RuntimeErrorHandler(lua_State* L);
    void reportError(std::string message) override;

private:
    lua_State* L;
};

} // namespace Luau::Require
