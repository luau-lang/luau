// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lua.h"
#include "lualib.h"

#include "Luau/Config.h"

#include <string>
#include <string_view>

class RequireResolver
{
public:
    std::string chunkname;
    std::string absolutePath;
    std::string sourceCode;

    enum class ModuleStatus
    {
        Cached,
        FileRead,
        NotFound
    };

    struct ResolvedRequire
    {
        ModuleStatus status;
        std::string chunkName;
        std::string absolutePath;
        std::string sourceCode;
    };

    [[nodiscard]] ResolvedRequire static resolveRequire(lua_State* L, std::string path);

private:
    std::string pathToResolve;
    std::string_view sourceChunkname;

    RequireResolver(lua_State* L, std::string path);

    ModuleStatus findModule();
    lua_State* L;
    Luau::Config config;
    std::string lastSearchedDir;
    bool isConfigFullyResolved = false;

    bool isRequireAllowed(std::string_view sourceChunkname);
    bool shouldSearchPathsArray();

    void resolveAndStoreDefaultPaths();
    ModuleStatus findModuleImpl();

    std::optional<std::string> getRequiringContextAbsolute();
    std::string getRequiringContextRelative();

    void substituteAliasIfPresent(std::string& path);
    std::optional<std::string> getAlias(std::string alias);

    void parseNextConfig();
    void parseConfigInDirectory(const std::string& path);
};
