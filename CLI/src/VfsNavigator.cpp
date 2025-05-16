// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/VfsNavigator.h"

#include "Luau/Common.h"
#include "Luau/FileUtils.h"

#include <array>
#include <string>
#include <string_view>

const std::array<std::string_view, 2> kSuffixes = {".luau", ".lua"};
const std::array<std::string_view, 2> kInitSuffixes = {"/init.luau", "/init.lua"};

struct ResolvedRealPath
{
    NavigationStatus status;
    std::string realPath;
};

static ResolvedRealPath getRealPath(std::string modulePath)
{
    bool found = false;
    std::string suffix;

    size_t lastSlash = modulePath.find_last_of('/');
    LUAU_ASSERT(lastSlash != std::string::npos);
    std::string lastComponent = modulePath.substr(lastSlash + 1);

    if (lastComponent != "init")
    {
        for (std::string_view potentialSuffix : kSuffixes)
        {
            if (isFile(modulePath + std::string(potentialSuffix)))
            {
                if (found)
                    return {NavigationStatus::Ambiguous};

                suffix = potentialSuffix;
                found = true;
            }
        }
    }
    if (isDirectory(modulePath))
    {
        if (found)
            return {NavigationStatus::Ambiguous};

        for (std::string_view potentialSuffix : kInitSuffixes)
        {
            if (isFile(modulePath + std::string(potentialSuffix)))
            {
                if (found)
                    return {NavigationStatus::Ambiguous};

                suffix = potentialSuffix;
                found = true;
            }
        }

        found = true;
    }

    if (!found)
        return {NavigationStatus::NotFound};

    return {NavigationStatus::Success, modulePath + suffix};
}

static bool hasSuffix(std::string_view str, std::string_view suffix)
{
    return str.size() >= suffix.size() && str.substr(str.size() - suffix.size()) == suffix;
}

static std::string getModulePath(std::string filePath)
{
    for (char& c : filePath)
    {
        if (c == '\\')
            c = '/';
    }

    std::string_view pathView = filePath;

    if (isAbsolutePath(pathView))
    {
        size_t firstSlash = pathView.find_first_of('/');
        LUAU_ASSERT(firstSlash != std::string::npos);
        pathView.remove_prefix(firstSlash);
    }

    for (std::string_view suffix : kInitSuffixes)
    {
        if (hasSuffix(pathView, suffix))
        {
            pathView.remove_suffix(suffix.size());
            return std::string(pathView);
        }
    }
    for (std::string_view suffix : kSuffixes)
    {
        if (hasSuffix(pathView, suffix))
        {
            pathView.remove_suffix(suffix.size());
            return std::string(pathView);
        }
    }

    return std::string(pathView);
}

NavigationStatus VfsNavigator::updateRealPaths()
{
    ResolvedRealPath result = getRealPath(modulePath);
    ResolvedRealPath absoluteResult = getRealPath(absoluteModulePath);
    if (result.status != NavigationStatus::Success || absoluteResult.status != NavigationStatus::Success)
        return result.status;

    realPath = isAbsolutePath(result.realPath) ? absolutePathPrefix + result.realPath : result.realPath;
    absoluteRealPath = absolutePathPrefix + absoluteResult.realPath;
    return NavigationStatus::Success;
}

NavigationStatus VfsNavigator::resetToStdIn()
{
    std::optional<std::string> cwd = getCurrentWorkingDirectory();
    if (!cwd)
        return NavigationStatus::NotFound;

    realPath = "./stdin";
    absoluteRealPath = normalizePath(*cwd + "/stdin");
    modulePath = "./stdin";
    absoluteModulePath = getModulePath(absoluteRealPath);

    size_t firstSlash = absoluteRealPath.find_first_of('/');
    LUAU_ASSERT(firstSlash != std::string::npos);
    absolutePathPrefix = absoluteRealPath.substr(0, firstSlash);

    return NavigationStatus::Success;
}

NavigationStatus VfsNavigator::resetToPath(const std::string& path)
{
    std::string normalizedPath = normalizePath(path);

    if (isAbsolutePath(normalizedPath))
    {
        modulePath = getModulePath(normalizedPath);
        absoluteModulePath = modulePath;

        size_t firstSlash = normalizedPath.find_first_of('/');
        LUAU_ASSERT(firstSlash != std::string::npos);
        absolutePathPrefix = normalizedPath.substr(0, firstSlash);
    }
    else
    {
        std::optional<std::string> cwd = getCurrentWorkingDirectory();
        if (!cwd)
            return NavigationStatus::NotFound;

        modulePath = getModulePath(normalizedPath);
        std::string joinedPath = normalizePath(*cwd + "/" + normalizedPath);
        absoluteModulePath = getModulePath(joinedPath);

        size_t firstSlash = joinedPath.find_first_of('/');
        LUAU_ASSERT(firstSlash != std::string::npos);
        absolutePathPrefix = joinedPath.substr(0, firstSlash);
    }

    return updateRealPaths();
}

NavigationStatus VfsNavigator::toParent()
{
    if (absoluteModulePath == "/")
        return NavigationStatus::NotFound;

    size_t numSlashes = 0;
    for (char c : absoluteModulePath)
    {
        if (c == '/')
            numSlashes++;
    }
    LUAU_ASSERT(numSlashes > 0);

    if (numSlashes == 1)
        return NavigationStatus::NotFound;

    modulePath = normalizePath(modulePath + "/..");
    absoluteModulePath = normalizePath(absoluteModulePath + "/..");

    return updateRealPaths();
}

NavigationStatus VfsNavigator::toChild(const std::string& name)
{
    modulePath = normalizePath(modulePath + "/" + name);
    absoluteModulePath = normalizePath(absoluteModulePath + "/" + name);

    return updateRealPaths();
}

std::string VfsNavigator::getFilePath() const
{
    return realPath;
}

std::string VfsNavigator::getAbsoluteFilePath() const
{
    return absoluteRealPath;
}

std::string VfsNavigator::getLuaurcPath() const
{
    std::string_view directory = realPath;

    for (std::string_view suffix : kInitSuffixes)
    {
        if (hasSuffix(directory, suffix))
        {
            directory.remove_suffix(suffix.size());
            return std::string(directory) + "/.luaurc";
        }
    }
    for (std::string_view suffix : kSuffixes)
    {
        if (hasSuffix(directory, suffix))
        {
            directory.remove_suffix(suffix.size());
            return std::string(directory) + "/.luaurc";
        }
    }

    return std::string(directory) + "/.luaurc";
}
