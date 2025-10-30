// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <optional>
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

    enum class ConfigStatus
    {
        Absent,
        Ambiguous,
        PresentJson,
        PresentLuau
    };

    ConfigStatus getConfigStatus() const;
    std::optional<std::string> getConfig() const;

private:
    std::string getConfigPath(const std::string& filename) const;

    NavigationStatus updateRealPaths();

    std::string realPath;
    std::string absoluteRealPath;
    std::string absolutePathPrefix;

    std::string modulePath;
    std::string absoluteModulePath;
};
