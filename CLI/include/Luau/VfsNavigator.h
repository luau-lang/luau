// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>

enum class NavigationStatus
{
    Success,
    Ambiguous,
    NotFound
};

class VfsNavigator
{
public:
    NavigationStatus resetToStdIn();
    NavigationStatus resetToPath(const std::string& path);

    NavigationStatus toParent();
    NavigationStatus toChild(const std::string& name);

    std::string getFilePath() const;
    std::string getAbsoluteFilePath() const;
    std::string getLuaurcPath() const;

private:
    NavigationStatus updateRealPaths();

    std::string realPath;
    std::string absoluteRealPath;
    std::string absolutePathPrefix;

    std::string modulePath;
    std::string absoluteModulePath;
};
