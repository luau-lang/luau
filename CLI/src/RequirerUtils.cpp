// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/RequirerUtils.h"

#include "Luau/FileUtils.h"

#include <algorithm>
#include <string>
#include <string_view>

static std::pair<PathResult::Status, std::string> getSuffixWithAmbiguityCheck(const std::string& path)
{
    bool found = false;
    std::string suffix;

    for (const char* potentialSuffix : {".luau", ".lua"})
    {
        if (isFile(path + potentialSuffix))
        {
            if (found)
                return {PathResult::Status::AMBIGUOUS, ""};

            suffix = potentialSuffix;
            found = true;
        }
    }
    if (isDirectory(path))
    {
        if (found)
            return {PathResult::Status::AMBIGUOUS, ""};

        for (const char* potentialSuffix : {"/init.luau", "/init.lua"})
        {
            if (isFile(path + potentialSuffix))
            {
                if (found)
                    return {PathResult::Status::AMBIGUOUS, ""};

                suffix = potentialSuffix;
                found = true;
            }
        }

        found = true;
    }

    if (!found)
        return {PathResult::Status::NOT_FOUND, ""};

    return {PathResult::Status::SUCCESS, suffix};
}

static PathResult addSuffix(PathResult partialResult)
{
    if (partialResult.status != PathResult::Status::SUCCESS)
        return partialResult;

    auto [status, suffix] = getSuffixWithAmbiguityCheck(partialResult.absPath);
    if (status != PathResult::Status::SUCCESS)
        return PathResult{status};

    partialResult.suffix = std::move(suffix);
    return partialResult;
}

PathResult getStdInResult()
{
    std::optional<std::string> cwd = getCurrentWorkingDirectory();
    if (!cwd)
        return PathResult{PathResult::Status::NOT_FOUND};

    std::replace(cwd->begin(), cwd->end(), '\\', '/');

    return PathResult{PathResult::Status::SUCCESS, *cwd + "/stdin", "./stdin", ""};
}

PathResult getAbsolutePathResult(const std::string& path)
{
    return addSuffix(PathResult{PathResult::Status::SUCCESS, path});
}

PathResult tryGetRelativePathResult(const std::string& path)
{
    if (isAbsolutePath(path))
        return getAbsolutePathResult(path);

    std::optional<std::string> cwd = getCurrentWorkingDirectory();
    if (!cwd)
        return PathResult{PathResult::Status::NOT_FOUND};

    std::optional<std::string> resolvedAbsPath = resolvePath(path, *cwd + "/stdin");
    if (!resolvedAbsPath)
        return PathResult{PathResult::Status::NOT_FOUND};

    return addSuffix(PathResult{PathResult::Status::SUCCESS, std::move(*resolvedAbsPath), path});
}

PathResult getParent(const std::string& absPath, const std::string& relPath)
{
    std::optional<std::string> parent = getParentPath(absPath);
    if (!parent)
        return PathResult{PathResult::Status::NOT_FOUND};

    return addSuffix(PathResult{PathResult::Status::SUCCESS, *parent, normalizePath(relPath + "/..")});
}

PathResult getChild(const std::string& absPath, const std::string& relPath, const std::string& name)
{
    return addSuffix(PathResult{PathResult::Status::SUCCESS, joinPaths(absPath, name), joinPaths(relPath, name)});
}

bool isFilePresent(const std::string& path, const std::string& suffix)
{
    return isFile(path + suffix);
}

std::optional<std::string> getFileContents(const std::string& path, const std::string& suffix)
{
    return readFile(path + suffix);
}
