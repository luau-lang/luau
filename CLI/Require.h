// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Config.h"

#include <functional>
#include <string>
#include <string_view>

class RequireResolver
{
public:
    enum class ModuleStatus
    {
        Cached,
        FileRead,
        ErrorReported
    };

    struct ResolvedRequire
    {
        ModuleStatus status;
        std::string identifier;
        std::string absolutePath;
        std::string sourceCode;
    };

    struct RequireContext
    {
        virtual ~RequireContext() = default;
        virtual std::string getPath() = 0;
        virtual bool isRequireAllowed() = 0;
        virtual bool isStdin() = 0;
        virtual std::string createNewIdentifer(const std::string& path) = 0;
    };

    struct CacheManager
    {
        virtual ~CacheManager() = default;
        virtual bool isCached(const std::string& path)
        {
            return false;
        }
    };

    struct ErrorHandler
    {
        virtual ~ErrorHandler() = default;
        virtual void reportError(const std::string message) {}
    };

    RequireResolver(std::string pathToResolve, RequireContext& requireContext, CacheManager& cacheManager, ErrorHandler& errorHandler);

    [[nodiscard]] ResolvedRequire resolveRequire(std::function<void(const ModuleStatus)> completionCallback = nullptr);

private:
    std::string pathToResolve;

    RequireContext& requireContext;
    CacheManager& cacheManager;
    ErrorHandler& errorHandler;

    ResolvedRequire resolvedRequire;
    bool isRequireResolved = false;

    Luau::Config config;
    std::string lastSearchedDir;
    bool isConfigFullyResolved = false;

    [[nodiscard]] bool initialize();

    ModuleStatus findModule();
    ModuleStatus findModuleImpl();

    [[nodiscard]] bool resolveAndStoreDefaultPaths();
    std::optional<std::string> getRequiringContextAbsolute();
    std::string getRequiringContextRelative();

    [[nodiscard]] bool substituteAliasIfPresent(std::string& path);
    std::optional<std::string> getAlias(std::string alias);

    [[nodiscard]] bool parseNextConfig();
    [[nodiscard]] bool parseConfigInDirectory(const std::string& directory);
};