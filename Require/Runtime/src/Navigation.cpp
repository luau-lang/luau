// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Navigation.h"

#include "Luau/Require.h"
#include "lua.h"
#include "lualib.h"

#include <string>

static constexpr size_t initalFileBufferSize = 1024;
static constexpr size_t initalIdentifierBufferSize = 64;

namespace Luau::Require
{

static NavigationContext::NavigateResult convertNavigateResult(luarequire_NavigateResult result)
{
    if (result == NAVIGATE_SUCCESS)
        return NavigationContext::NavigateResult::Success;
    if (result == NAVIGATE_AMBIGUOUS)
        return NavigationContext::NavigateResult::Ambiguous;

    return NavigationContext::NavigateResult::NotFound;
}

RuntimeNavigationContext::RuntimeNavigationContext(luarequire_Configuration* config, lua_State* L, void* ctx, std::string requirerChunkname)
    : config(config)
    , L(L)
    , ctx(ctx)
    , requirerChunkname(std::move(requirerChunkname))
{
}

std::string RuntimeNavigationContext::getRequirerIdentifier() const
{
    return requirerChunkname;
}

NavigationContext::NavigateResult RuntimeNavigationContext::reset(const std::string& requirerChunkname)
{
    return convertNavigateResult(config->reset(L, ctx, requirerChunkname.c_str()));
}

NavigationContext::NavigateResult RuntimeNavigationContext::jumpToAlias(const std::string& path)
{
    return convertNavigateResult(config->jump_to_alias(L, ctx, path.c_str()));
}

NavigationContext::NavigateResult RuntimeNavigationContext::toParent()
{
    return convertNavigateResult(config->to_parent(L, ctx));
}

NavigationContext::NavigateResult RuntimeNavigationContext::toChild(const std::string& component)
{
    return convertNavigateResult(config->to_child(L, ctx, component.c_str()));
}

bool RuntimeNavigationContext::isModulePresent() const
{
    return config->is_module_present(L, ctx);
}

std::optional<std::string> RuntimeNavigationContext::getChunkname() const
{
    return getStringFromCWriter(config->get_chunkname, initalIdentifierBufferSize);
}

std::optional<std::string> RuntimeNavigationContext::getLoadname() const
{
    return getStringFromCWriter(config->get_loadname, initalIdentifierBufferSize);
}

std::optional<std::string> RuntimeNavigationContext::getCacheKey() const
{
    return getStringFromCWriter(config->get_cache_key, initalIdentifierBufferSize);
}

bool RuntimeNavigationContext::isConfigPresent() const
{
    return config->is_config_present(L, ctx);
}

NavigationContext::ConfigBehavior RuntimeNavigationContext::getConfigBehavior() const
{
    if (config->get_alias)
        return ConfigBehavior::GetAlias;
    return ConfigBehavior::GetConfig;
}

std::optional<std::string> RuntimeNavigationContext::getAlias(const std::string& alias) const
{
    return getStringFromCWriterWithInput(config->get_alias, alias, initalIdentifierBufferSize);
}

std::optional<std::string> RuntimeNavigationContext::getConfig() const
{
    return getStringFromCWriter(config->get_config, initalFileBufferSize);
}

std::optional<std::string> RuntimeNavigationContext::getStringFromCWriter(
    luarequire_WriteResult (*writer)(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out),
    size_t initalBufferSize
) const
{
    std::string buffer;
    buffer.resize(initalBufferSize);

    size_t size;
    luarequire_WriteResult result = writer(L, ctx, buffer.data(), buffer.size(), &size);
    if (result == WRITE_BUFFER_TOO_SMALL)
    {
        buffer.resize(size);
        result = writer(L, ctx, buffer.data(), buffer.size(), &size);
    }

    if (result == WRITE_SUCCESS)
    {
        buffer.resize(size);
        return buffer;
    }

    return std::nullopt;
}

std::optional<std::string> RuntimeNavigationContext::getStringFromCWriterWithInput(
    luarequire_WriteResult (*writer)(lua_State* L, void* ctx, const char* input, char* buffer, size_t buffer_size, size_t* size_out),
    std::string input,
    size_t initalBufferSize
) const
{
    std::string buffer;
    buffer.resize(initalBufferSize);

    size_t size;
    luarequire_WriteResult result = writer(L, ctx, input.c_str(), buffer.data(), buffer.size(), &size);
    if (result == WRITE_BUFFER_TOO_SMALL)
    {
        buffer.resize(size);
        result = writer(L, ctx, input.c_str(), buffer.data(), buffer.size(), &size);
    }

    if (result == WRITE_SUCCESS)
    {
        buffer.resize(size);
        return buffer;
    }

    return std::nullopt;
}

RuntimeErrorHandler::RuntimeErrorHandler(std::string requiredPath)
    : errorPrefix("error requiring module \"" + std::move(requiredPath) + "\": ")
{
}

void RuntimeErrorHandler::reportError(std::string message)
{
    errorMessage = errorPrefix + std::move(message);
}

const std::string& RuntimeErrorHandler::getReportedError() const
{
    return errorMessage;
}

} // namespace Luau::Require
