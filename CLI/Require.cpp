// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Require.h"

#include "FileUtils.h"
#include "Luau/Common.h"

#include <algorithm>
#include <array>
#include <utility>

RequireResolver::RequireResolver(lua_State* L, std::string path)
    : pathToResolve(std::move(path))
    , L(L)
{
    lua_Debug ar;
    lua_getinfo(L, 1, "s", &ar);
    sourceChunkname = ar.source;

    if (!isRequireAllowed(sourceChunkname))
        luaL_errorL(L, "require is not supported in this context");

    if (isAbsolutePath(pathToResolve))
        luaL_argerrorL(L, 1, "cannot require an absolute path");

    std::replace(pathToResolve.begin(), pathToResolve.end(), '\\', '/');

    substituteAliasIfPresent(pathToResolve);
}

[[nodiscard]] RequireResolver::ResolvedRequire RequireResolver::resolveRequire(lua_State* L, std::string path)
{
    RequireResolver resolver(L, std::move(path));
    ModuleStatus status = resolver.findModule();
    if (status != ModuleStatus::FileRead)
        return ResolvedRequire{status};
    else
        return ResolvedRequire{status, std::move(resolver.chunkname), std::move(resolver.absolutePath), std::move(resolver.sourceCode)};
}

RequireResolver::ModuleStatus RequireResolver::findModule()
{
    resolveAndStoreDefaultPaths();

    // Put _MODULES table on stack for checking and saving to the cache
    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);

    RequireResolver::ModuleStatus moduleStatus = findModuleImpl();

    if (moduleStatus != RequireResolver::ModuleStatus::NotFound)
        return moduleStatus;

    if (!shouldSearchPathsArray())
        return moduleStatus;

    if (!isConfigFullyResolved)
        parseNextConfig();

    // Index-based iteration because std::iterator may be invalidated if config.paths is reallocated
    for (size_t i = 0; i < config.paths.size(); ++i)
    {
        // "placeholder" acts as a requiring file in the relevant directory
        std::optional<std::string> absolutePathOpt = resolvePath(pathToResolve, joinPaths(config.paths[i], "placeholder"));

        if (!absolutePathOpt)
            luaL_errorL(L, "error requiring module");

        chunkname = *absolutePathOpt;
        absolutePath = *absolutePathOpt;

        moduleStatus = findModuleImpl();

        if (moduleStatus != RequireResolver::ModuleStatus::NotFound)
            return moduleStatus;

        // Before finishing the loop, parse more config files if there are any
        if (i == config.paths.size() - 1 && !isConfigFullyResolved)
            parseNextConfig(); // could reallocate config.paths when paths are parsed and added
    }

    return RequireResolver::ModuleStatus::NotFound;
}

RequireResolver::ModuleStatus RequireResolver::findModuleImpl()
{
    static const std::array<const char*, 4> possibleSuffixes = {".luau", ".lua", "/init.luau", "/init.lua"};

    size_t unsuffixedAbsolutePathSize = absolutePath.size();

    for (const char* possibleSuffix : possibleSuffixes)
    {
        absolutePath += possibleSuffix;

        // Check cache for module
        lua_getfield(L, -1, absolutePath.c_str());
        if (!lua_isnil(L, -1))
        {
            return ModuleStatus::Cached;
        }
        lua_pop(L, 1);

        // Try to read the matching file
        std::optional<std::string> source = readFile(absolutePath);
        if (source)
        {
            chunkname = "=" + chunkname + possibleSuffix;
            sourceCode = *source;
            return ModuleStatus::FileRead;
        }

        absolutePath.resize(unsuffixedAbsolutePathSize); // truncate to remove suffix
    }

    return ModuleStatus::NotFound;
}

bool RequireResolver::isRequireAllowed(std::string_view sourceChunkname)
{
    LUAU_ASSERT(!sourceChunkname.empty());
    return (sourceChunkname[0] == '=' || sourceChunkname[0] == '@');
}

bool RequireResolver::shouldSearchPathsArray()
{
    return !isAbsolutePath(pathToResolve) && !isExplicitlyRelative(pathToResolve);
}

void RequireResolver::resolveAndStoreDefaultPaths()
{
    if (!isAbsolutePath(pathToResolve))
    {
        std::string chunknameContext = getRequiringContextRelative();
        std::optional<std::string> absolutePathContext = getRequiringContextAbsolute();

        if (!absolutePathContext)
            luaL_errorL(L, "error requiring module");

        // resolvePath automatically sanitizes/normalizes the paths
        std::optional<std::string> chunknameOpt = resolvePath(pathToResolve, chunknameContext);
        std::optional<std::string> absolutePathOpt = resolvePath(pathToResolve, *absolutePathContext);

        if (!chunknameOpt || !absolutePathOpt)
            luaL_errorL(L, "error requiring module");

        chunkname = std::move(*chunknameOpt);
        absolutePath = std::move(*absolutePathOpt);
    }
    else
    {
        // Here we must explicitly sanitize, as the path is taken as is
        std::optional<std::string> sanitizedPath = normalizePath(pathToResolve);
        if (!sanitizedPath)
            luaL_errorL(L, "error requiring module");

        chunkname = *sanitizedPath;
        absolutePath = std::move(*sanitizedPath);
    }
}

std::optional<std::string> RequireResolver::getRequiringContextAbsolute()
{
    std::string requiringFile;
    if (isAbsolutePath(sourceChunkname.substr(1)))
    {
        // We already have an absolute path for the requiring file
        requiringFile = sourceChunkname.substr(1);
    }
    else
    {
        // Requiring file's stored path is relative to the CWD, must make absolute
        std::optional<std::string> cwd = getCurrentWorkingDirectory();
        if (!cwd)
            return std::nullopt;

        if (sourceChunkname.substr(1) == "stdin")
        {
            // Require statement is being executed from REPL input prompt
            // The requiring context is the pseudo-file "stdin" in the CWD
            requiringFile = joinPaths(*cwd, "stdin");
        }
        else
        {
            // Require statement is being executed in a file, must resolve relative to CWD
            std::optional<std::string> requiringFileOpt = resolvePath(sourceChunkname.substr(1), joinPaths(*cwd, "stdin"));
            if (!requiringFileOpt)
                return std::nullopt;

            requiringFile = *requiringFileOpt;
        }
    }
    std::replace(requiringFile.begin(), requiringFile.end(), '\\', '/');
    return requiringFile;
}

std::string RequireResolver::getRequiringContextRelative()
{
    std::string baseFilePath;
    if (sourceChunkname.substr(1) != "stdin")
        baseFilePath = sourceChunkname.substr(1);

    return baseFilePath;
}

void RequireResolver::substituteAliasIfPresent(std::string& path)
{
    if (path.size() < 1 || path[0] != '@')
        return;
    std::string potentialAlias = path.substr(1, path.find_first_of("\\/"));

    // Not worth searching when potentialAlias cannot be an alias
    if (!Luau::isValidAlias(potentialAlias))
        luaL_errorL(L, "@%s is not a valid alias", potentialAlias.c_str());

    std::optional<std::string> alias = getAlias(potentialAlias);
    if (alias)
    {
        path = *alias + path.substr(potentialAlias.size() + 1);
    }
    else
    {
        luaL_errorL(L, "@%s is not a valid alias", potentialAlias.c_str());
    }
}

std::optional<std::string> RequireResolver::getAlias(std::string alias)
{
    std::transform(alias.begin(), alias.end(), alias.begin(), [](unsigned char c) {
        return ('A' <= c && c <= 'Z') ? (c + ('a' - 'A')) : c;
    });
    while (!config.aliases.count(alias) && !isConfigFullyResolved)
    {
        parseNextConfig();
    }
    if (!config.aliases.count(alias) && isConfigFullyResolved)
        return std::nullopt; // could not find alias

    return resolvePath(config.aliases[alias], joinPaths(lastSearchedDir, Luau::kConfigName));
}

void RequireResolver::parseNextConfig()
{
    if (isConfigFullyResolved)
        return; // no config files left to parse

    std::optional<std::string> directory;
    if (lastSearchedDir.empty())
    {
        std::optional<std::string> requiringFile = getRequiringContextAbsolute();
        if (!requiringFile)
            luaL_errorL(L, "error requiring module");

        directory = getParentPath(*requiringFile);
    }
    else
        directory = getParentPath(lastSearchedDir);

    if (directory)
    {
        lastSearchedDir = *directory;
        parseConfigInDirectory(*directory);
    }
    else
        isConfigFullyResolved = true;
}

void RequireResolver::parseConfigInDirectory(const std::string& directory)
{
    std::string configPath = joinPaths(directory, Luau::kConfigName);

    size_t numPaths = config.paths.size();

    if (std::optional<std::string> contents = readFile(configPath))
    {
        std::optional<std::string> error = Luau::parseConfig(*contents, config);
        if (error)
            luaL_errorL(L, "error parsing %s (%s)", configPath.c_str(), (*error).c_str());
    }

    // Resolve any newly obtained relative paths in "paths" in relation to configPath
    for (auto it = config.paths.begin() + numPaths; it != config.paths.end(); ++it)
    {
        if (!isAbsolutePath(*it))
        {
            if (std::optional<std::string> resolvedPath = resolvePath(*it, configPath))
                *it = std::move(*resolvedPath);
            else
                luaL_errorL(L, "error requiring module");
        }
    }
}
