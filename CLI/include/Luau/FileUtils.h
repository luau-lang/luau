// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <functional>
#include <vector>

std::optional<std::string> getCurrentWorkingDirectory();

std::string normalizePath(std::string_view path);
std::optional<std::string> resolvePath(std::string_view relativePath, std::string_view baseFilePath);

std::optional<std::string> readFile(const std::string& name);
std::optional<std::string> readStdin();

bool hasFileExtension(std::string_view name, const std::vector<std::string>& extensions);

bool isAbsolutePath(std::string_view path);
bool isFile(const std::string& path);
bool isDirectory(const std::string& path);
bool traverseDirectory(const std::string& path, const std::function<void(const std::string& name)>& callback);

std::vector<std::string_view> splitPath(std::string_view path);
std::string joinPaths(std::string_view lhs, std::string_view rhs);
std::optional<std::string> getParentPath(std::string_view path);

std::vector<std::string> getSourceFiles(int argc, char** argv);
