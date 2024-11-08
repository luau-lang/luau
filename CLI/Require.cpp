// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Require.h"

#include "FileUtils.h"
#include "Luau/Common.h"
#include "Luau/Config.h"

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

    if (!isPrefixValid())
        luaL_argerrorL(L, 1, "require path must start with a valid prefix: ./, ../, or @");

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

    return findModuleImpl();
}

RequireResolver::ModuleStatus RequireResolver::findModuleImpl()
{
    if (isPathAmbiguous(absolutePath))
        return ModuleStatus::Ambiguous;

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

    if (hasFileExtension(absolutePath, {".luau", ".lua"}) && isFile(absolutePath))
        luaL_argerrorL(L, 1, "error requiring module: consider removing the file extension");

    return ModuleStatus::NotFound;
}

bool RequireResolver::isPathAmbiguous(const std::string& path)
{
    bool found = false;
    for (const char* suffix : {".luau", ".lua"})
    {
        if (isFile(path + suffix))
        {
            if (found)
                return true;
            else
                found = true;
        }
    }
    if (isDirectory(path) && found)
        return true;

    return false;
}

bool RequireResolver::isRequireAllowed(std::string_view sourceChunkname)
{
    LUAU_ASSERT(!sourceChunkname.empty());
    return (sourceChunkname[0] == '=' || sourceChunkname[0] == '@');
}

bool RequireResolver::isPrefixValid()
{
    return pathToResolve.compare(0, 2, "./") == 0 || pathToResolve.compare(0, 3, "../") == 0 || pathToResolve.compare(0, 1, "@") == 0;
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

    // To ignore the '@' alias prefix when processing the alias
    const size_t aliasStartPos = 1;

    // If a directory separator was found, the length of the alias is the
    // distance between the start of the alias and the separator. Otherwise,
    // the whole string after the alias symbol is the alias.
    size_t aliasLen = path.find_first_of("\\/");
    if (aliasLen != std::string::npos)
        aliasLen -= aliasStartPos;

    const std::string potentialAlias = path.substr(aliasStartPos, aliasLen);

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
    std::transform(
        alias.begin(),
        alias.end(),
        alias.begin(),
        [](unsigned char c)
        {
            return ('A' <= c && c <= 'Z') ? (c + ('a' - 'A')) : c;
        }
    );
    while (!config.aliases.contains(alias) && !isConfigFullyResolved)
    {
        parseNextConfig();
    }
    if (!config.aliases.contains(alias) && isConfigFullyResolved)
        return std::nullopt; // could not find alias

    const Luau::Config::AliasInfo& aliasInfo = config.aliases[alias];
    return resolvePath(aliasInfo.value, aliasInfo.configLocation);
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

    Luau::ConfigOptions::AliasOptions aliasOpts;
    aliasOpts.configLocation = configPath;
    aliasOpts.overwriteAliases = false;

    Luau::ConfigOptions opts;
    opts.aliasOptions = std::move(aliasOpts);

    if (std::optional<std::string> contents = readFile(configPath))
    {
        std::optional<std::string> error = Luau::parseConfig(*contents, config, opts);
        if (error)
            luaL_errorL(L, "error parsing %s (%s)", configPath.c_str(), (*error).c_str());
    }
}
