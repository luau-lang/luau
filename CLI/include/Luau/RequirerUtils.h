// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <optional>
#include <string>
#include <string_view>

struct PathResult
{
    enum class Status
    {
        SUCCESS,
        AMBIGUOUS,
        NOT_FOUND
    };

    Status status;
    std::string absPath;
    std::string relPath;
    std::string suffix;
};

PathResult getStdInResult();

PathResult getAbsolutePathResult(const std::string& path);

// If given an absolute path, this will implicitly call getAbsolutePathResult.
// Aliases prevent us from solely operating on relative paths, so we need to
// be able to fall back to operating on absolute paths if needed.
PathResult tryGetRelativePathResult(const std::string& path);

PathResult getParent(const std::string& absPath, const std::string& relPath);
PathResult getChild(const std::string& absPath, const std::string& relPath, const std::string& name);

bool isFilePresent(const std::string& path, const std::string& suffix);
std::optional<std::string> getFileContents(const std::string& path, const std::string& suffix);
