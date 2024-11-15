// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Require.h"

#include "FileUtils.h"
#include "Luau/Common.h"
#include "Luau/Config.h"

#include <algorithm>
#include <array>
#include <utility>

static constexpr char kRequireErrorGeneric[] = "error requiring module";

RequireResolver::RequireResolver(std::string path, RequireContext& requireContext, CacheManager& cacheManager, ErrorHandler& errorHandler)
    : pathToResolve(std::move(path))
    , requireContext(requireContext)
    , cacheManager(cacheManager)
    , errorHandler(errorHandler)
{
}

RequireResolver::ResolvedRequire RequireResolver::resolveRequire(std::function<void(const ModuleStatus)> completionCallback)
{
    if (isRequireResolved)
    {
        errorHandler.reportError("require statement has already been resolved");
        return ResolvedRequire{ModuleStatus::ErrorReported};
    }

    if (!initialize())
        return ResolvedRequire{ModuleStatus::ErrorReported};

    resolvedRequire.status = findModule();

    if (completionCallback)
        completionCallback(resolvedRequire.status);

    isRequireResolved = true;
    return resolvedRequire;
}

static bool hasValidPrefix(std::string_view path)
{
    return path.compare(0, 2, "./") == 0 || path.compare(0, 3, "../") == 0 || path.compare(0, 1, "@") == 0;
}

static bool isPathAmbiguous(const std::string& path)
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

bool RequireResolver::initialize()
{
    if (!requireContext.isRequireAllowed())
    {
        errorHandler.reportError("require is not supported in this context");
        return false;
    }

    if (isAbsolutePath(pathToResolve))
    {
        errorHandler.reportError("cannot require an absolute path");
        return false;
    }

    std::replace(pathToResolve.begin(), pathToResolve.end(), '\\', '/');

    if (!hasValidPrefix(pathToResolve))
    {
        errorHandler.reportError("require path must start with a valid prefix: ./, ../, or @");
        return false;
    }

    return substituteAliasIfPresent(pathToResolve);
}

RequireResolver::ModuleStatus RequireResolver::findModule()
{
    if (!resolveAndStoreDefaultPaths())
        return ModuleStatus::ErrorReported;

    if (isPathAmbiguous(resolvedRequire.absolutePath))
    {
        errorHandler.reportError("require path could not be resolved to a unique file");
        return ModuleStatus::ErrorReported;
    }

    static constexpr std::array<const char*, 4> possibleSuffixes = {".luau", ".lua", "/init.luau", "/init.lua"};
    size_t unsuffixedAbsolutePathSize = resolvedRequire.absolutePath.size();

    for (const char* possibleSuffix : possibleSuffixes)
    {
        resolvedRequire.absolutePath += possibleSuffix;

        if (cacheManager.isCached(resolvedRequire.absolutePath))
            return ModuleStatus::Cached;

        // Try to read the matching file
        if (std::optional<std::string> source = readFile(resolvedRequire.absolutePath))
        {
            resolvedRequire.identifier = requireContext.createNewIdentifer(resolvedRequire.identifier + possibleSuffix);
            resolvedRequire.sourceCode = *source;
            return ModuleStatus::FileRead;
        }

        resolvedRequire.absolutePath.resize(unsuffixedAbsolutePathSize); // truncate to remove suffix
    }

    if (hasFileExtension(resolvedRequire.absolutePath, {".luau", ".lua"}) && isFile(resolvedRequire.absolutePath))
    {
        errorHandler.reportError("error requiring module: consider removing the file extension");
        return ModuleStatus::ErrorReported;
    }

    errorHandler.reportError(kRequireErrorGeneric);
    return ModuleStatus::ErrorReported;
}

bool RequireResolver::resolveAndStoreDefaultPaths()
{
    if (!isAbsolutePath(pathToResolve))
    {
        std::string identifierContext = getRequiringContextRelative();
        std::optional<std::string> absolutePathContext = getRequiringContextAbsolute();

        if (!absolutePathContext)
            return false;

        // resolvePath automatically sanitizes/normalizes the paths
        resolvedRequire.identifier = resolvePath(pathToResolve, identifierContext);
        resolvedRequire.absolutePath = resolvePath(pathToResolve, *absolutePathContext);
    }
    else
    {
        // Here we must explicitly sanitize, as the path is taken as is
        std::string sanitizedPath = normalizePath(pathToResolve);
        resolvedRequire.identifier = sanitizedPath;
        resolvedRequire.absolutePath = std::move(sanitizedPath);
    }
    return true;
}

std::optional<std::string> RequireResolver::getRequiringContextAbsolute()
{
    std::string requiringFile;
    if (isAbsolutePath(requireContext.getPath()))
    {
        // We already have an absolute path for the requiring file
        requiringFile = requireContext.getPath();
    }
    else
    {
        // Requiring file's stored path is relative to the CWD, must make absolute
        std::optional<std::string> cwd = getCurrentWorkingDirectory();
        if (!cwd)
        {
            errorHandler.reportError("could not determine current working directory");
            return std::nullopt;
        }

        if (requireContext.isStdin())
        {
            // Require statement is being executed from REPL input prompt
            // The requiring context is the pseudo-file "stdin" in the CWD
            requiringFile = joinPaths(*cwd, "stdin");
        }
        else
        {
            // Require statement is being executed in a file, must resolve relative to CWD
            requiringFile = resolvePath(requireContext.getPath(), joinPaths(*cwd, "stdin"));
        }
    }
    std::replace(requiringFile.begin(), requiringFile.end(), '\\', '/');
    return requiringFile;
}

std::string RequireResolver::getRequiringContextRelative()
{
    return requireContext.isStdin() ? "" : requireContext.getPath();
}

bool RequireResolver::substituteAliasIfPresent(std::string& path)
{
    if (path.size() < 1 || path[0] != '@')
        return true;

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
    {
        errorHandler.reportError("@" + potentialAlias + " is not a valid alias");
        return false;
    }

    if (std::optional<std::string> alias = getAlias(potentialAlias))
    {
        path = *alias + path.substr(potentialAlias.size() + 1);
        return true;
    }

    errorHandler.reportError("@" + potentialAlias + " is not a valid alias");
    return false;
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
        if (!parseNextConfig())
            return std::nullopt; // error parsing config
    }
    if (!config.aliases.contains(alias) && isConfigFullyResolved)
        return std::nullopt; // could not find alias

    const Luau::Config::AliasInfo& aliasInfo = config.aliases[alias];
    return resolvePath(aliasInfo.value, aliasInfo.configLocation);
}

bool RequireResolver::parseNextConfig()
{
    if (isConfigFullyResolved)
        return true; // no config files left to parse

    std::optional<std::string> directory;
    if (lastSearchedDir.empty())
    {
        std::optional<std::string> requiringFile = getRequiringContextAbsolute();
        if (!requiringFile)
            return false;

        directory = getParentPath(*requiringFile);
    }
    else
        directory = getParentPath(lastSearchedDir);

    if (directory)
    {
        lastSearchedDir = *directory;
        if (!parseConfigInDirectory(*directory))
            return false;
    }
    else
        isConfigFullyResolved = true;

    return true;
}

bool RequireResolver::parseConfigInDirectory(const std::string& directory)
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
        {
            errorHandler.reportError("error parsing " + configPath + "(" + *error + ")");
            return false;
        }
    }

    return true;
}