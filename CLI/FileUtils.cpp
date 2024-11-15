// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "FileUtils.h"

#include "Luau/Common.h"

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <direct.h>
#include <windows.h>
#else
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#endif

#include <string.h>

#ifdef _WIN32
static std::wstring fromUtf8(const std::string& path)
{
    size_t result = MultiByteToWideChar(CP_UTF8, 0, path.data(), int(path.size()), nullptr, 0);
    LUAU_ASSERT(result);

    std::wstring buf(result, L'\0');
    MultiByteToWideChar(CP_UTF8, 0, path.data(), int(path.size()), &buf[0], int(buf.size()));

    return buf;
}

static std::string toUtf8(const std::wstring& path)
{
    size_t result = WideCharToMultiByte(CP_UTF8, 0, path.data(), int(path.size()), nullptr, 0, nullptr, nullptr);
    LUAU_ASSERT(result);

    std::string buf(result, '\0');
    WideCharToMultiByte(CP_UTF8, 0, path.data(), int(path.size()), &buf[0], int(buf.size()), nullptr, nullptr);

    return buf;
}
#endif

bool isAbsolutePath(std::string_view path)
{
#ifdef _WIN32
    // Must either begin with "X:/", "X:\", "/", or "\", where X is a drive letter
    return (path.size() >= 3 && isalpha(path[0]) && path[1] == ':' && (path[2] == '/' || path[2] == '\\')) ||
           (path.size() >= 1 && (path[0] == '/' || path[0] == '\\'));
#else
    // Must begin with '/'
    return path.size() >= 1 && path[0] == '/';
#endif
}

std::optional<std::string> getCurrentWorkingDirectory()
{
    // 2^17 - derived from the Windows path length limit
    constexpr size_t maxPathLength = 131072;
    constexpr size_t initialPathLength = 260;

    std::string directory(initialPathLength, '\0');
    char* cstr = nullptr;

    while (!cstr && directory.size() <= maxPathLength)
    {
#ifdef _WIN32
        cstr = _getcwd(directory.data(), static_cast<int>(directory.size()));
#else
        cstr = getcwd(directory.data(), directory.size());
#endif
        if (cstr)
        {
            directory.resize(strlen(cstr));
            return directory;
        }
        else if (errno != ERANGE || directory.size() * 2 > maxPathLength)
        {
            return std::nullopt;
        }
        else
        {
            directory.resize(directory.size() * 2);
        }
    }
    return std::nullopt;
}

// Returns the normal/canonical form of a path (e.g. "../subfolder/../module.luau" -> "../module.luau")
std::string normalizePath(std::string_view path)
{
    return resolvePath(path, "");
}

// Takes a path that is relative to the file at baseFilePath and returns the path explicitly rebased onto baseFilePath.
// For absolute paths, baseFilePath will be ignored, and this function will resolve the path to a canonical path:
// (e.g. "/Users/.././Users/johndoe" -> "/Users/johndoe").
std::string resolvePath(std::string_view path, std::string_view baseFilePath)
{
    std::vector<std::string_view> pathComponents;
    std::vector<std::string_view> baseFilePathComponents;

    // Dependent on whether the final resolved path is absolute or relative
    // - if relative (when path and baseFilePath are both relative), resolvedPathPrefix remains empty
    // - if absolute (if either path or baseFilePath are absolute), resolvedPathPrefix is "C:\", "/", etc.
    std::string resolvedPathPrefix;
    bool isResolvedPathRelative = false;

    if (isAbsolutePath(path))
    {
        // path is absolute, we use path's prefix and ignore baseFilePath
        size_t afterPrefix = path.find_first_of("\\/") + 1;
        resolvedPathPrefix = path.substr(0, afterPrefix);
        pathComponents = splitPath(path.substr(afterPrefix));
    }
    else
    {
        size_t afterPrefix = baseFilePath.find_first_of("\\/") + 1;
        baseFilePathComponents = splitPath(baseFilePath.substr(afterPrefix));
        if (isAbsolutePath(baseFilePath))
        {
            // path is relative and baseFilePath is absolute, we use baseFilePath's prefix
            resolvedPathPrefix = baseFilePath.substr(0, afterPrefix);
        }
        else
        {
            // path and baseFilePath are both relative, we do not set a prefix (resolved path will be relative)
            isResolvedPathRelative = true;
        }
        pathComponents = splitPath(path);
    }

    // Remove filename from components
    if (!baseFilePathComponents.empty())
        baseFilePathComponents.pop_back();

    // Resolve the path by applying pathComponents to baseFilePathComponents
    int numPrependedParents = 0;
    for (std::string_view component : pathComponents)
    {
        if (component == "..")
        {
            if (baseFilePathComponents.empty())
            {
                if (isResolvedPathRelative)
                    numPrependedParents++;      // "../" will later be added to the beginning of the resolved path
            }
            else if (baseFilePathComponents.back() != "..")
            {
                baseFilePathComponents.pop_back(); // Resolve cases like "folder/subfolder/../../file" to "file"
            }
        }
        else if (component != "." && !component.empty())
        {
            baseFilePathComponents.push_back(component);
        }
    }

    // Create resolved path prefix for relative paths
    if (isResolvedPathRelative)
    {
        if (numPrependedParents > 0)
        {
            resolvedPathPrefix.reserve(numPrependedParents * 3);
            for (int i = 0; i < numPrependedParents; i++)
            {
                resolvedPathPrefix += "../";
            }
        }
        else
        {
            resolvedPathPrefix = "./";
        }
    }

    // Join baseFilePathComponents to form the resolved path
    std::string resolvedPath = resolvedPathPrefix;
    for (auto iter = baseFilePathComponents.begin(); iter != baseFilePathComponents.end(); ++iter)
    {
        if (iter != baseFilePathComponents.begin())
            resolvedPath += "/";

        resolvedPath += *iter;
    }
    if (resolvedPath.size() > resolvedPathPrefix.size() && resolvedPath.back() == '/')
    {
        // Remove trailing '/' if present
        resolvedPath.pop_back();
    }
    return resolvedPath;
}

bool hasFileExtension(std::string_view name, const std::vector<std::string>& extensions)
{
    for (const std::string& extension : extensions)
    {
        if (name.size() >= extension.size() && name.substr(name.size() - extension.size()) == extension)
            return true;
    }
    return false;
}

std::optional<std::string> readFile(const std::string& name)
{
#ifdef _WIN32
    FILE* file = _wfopen(fromUtf8(name).c_str(), L"rb");
#else
    FILE* file = fopen(name.c_str(), "rb");
#endif

    if (!file)
        return std::nullopt;

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    if (length < 0)
    {
        fclose(file);
        return std::nullopt;
    }
    fseek(file, 0, SEEK_SET);

    std::string result(length, 0);

    size_t read = fread(result.data(), 1, length, file);
    fclose(file);

    if (read != size_t(length))
        return std::nullopt;

    // Skip first line if it's a shebang
    if (length > 2 && result[0] == '#' && result[1] == '!')
        result.erase(0, result.find('\n'));

    return result;
}

std::optional<std::string> readStdin()
{
    std::string result;
    char buffer[4096] = {};

    while (fgets(buffer, sizeof(buffer), stdin) != nullptr)
        result.append(buffer);

    // If eof was not reached for stdin, then a read error occurred
    if (!feof(stdin))
        return std::nullopt;

    return result;
}

template<typename Ch>
static void joinPaths(std::basic_string<Ch>& str, const Ch* lhs, const Ch* rhs)
{
    str = lhs;
    if (!str.empty() && str.back() != '/' && str.back() != '\\' && *rhs != '/' && *rhs != '\\')
        str += '/';
    str += rhs;
}

#ifdef _WIN32
static bool traverseDirectoryRec(const std::wstring& path, const std::function<void(const std::string& name)>& callback)
{
    std::wstring query = path + std::wstring(L"/*");

    WIN32_FIND_DATAW data;
    HANDLE h = FindFirstFileW(query.c_str(), &data);

    if (h == INVALID_HANDLE_VALUE)
        return false;

    std::wstring buf;

    do
    {
        if (wcscmp(data.cFileName, L".") != 0 && wcscmp(data.cFileName, L"..") != 0)
        {
            joinPaths(buf, path.c_str(), data.cFileName);

            if (data.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT)
            {
                // Skip reparse points to avoid handling cycles
            }
            else if (data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
            {
                traverseDirectoryRec(buf, callback);
            }
            else
            {
                callback(toUtf8(buf));
            }
        }
    } while (FindNextFileW(h, &data));

    FindClose(h);

    return true;
}

bool traverseDirectory(const std::string& path, const std::function<void(const std::string& name)>& callback)
{
    return traverseDirectoryRec(fromUtf8(path), callback);
}
#else
static bool traverseDirectoryRec(const std::string& path, const std::function<void(const std::string& name)>& callback)
{
    int fd = open(path.c_str(), O_DIRECTORY);
    DIR* dir = fdopendir(fd);

    if (!dir)
        return false;

    std::string buf;

    while (dirent* entry = readdir(dir))
    {
        const dirent& data = *entry;

        if (strcmp(data.d_name, ".") != 0 && strcmp(data.d_name, "..") != 0)
        {
            joinPaths(buf, path.c_str(), data.d_name);

#if defined(DTTOIF)
            mode_t mode = DTTOIF(data.d_type);
#else
            mode_t mode = 0;
#endif

            // we need to stat an UNKNOWN to be able to tell the type
            if ((mode & S_IFMT) == 0)
            {
                struct stat st = {};
#ifdef _ATFILE_SOURCE
                fstatat(fd, data.d_name, &st, 0);
#else
                lstat(buf.c_str(), &st);
#endif

                mode = st.st_mode;
            }

            if (mode == S_IFDIR)
            {
                traverseDirectoryRec(buf, callback);
            }
            else if (mode == S_IFREG)
            {
                callback(buf);
            }
            else if (mode == S_IFLNK)
            {
                // Skip symbolic links to avoid handling cycles
            }
        }
    }

    closedir(dir);

    return true;
}

bool traverseDirectory(const std::string& path, const std::function<void(const std::string& name)>& callback)
{
    return traverseDirectoryRec(path, callback);
}
#endif

bool isFile(const std::string& path)
{
#ifdef _WIN32
    DWORD fileAttributes = GetFileAttributesW(fromUtf8(path).c_str());
    if (fileAttributes == INVALID_FILE_ATTRIBUTES)
        return false;
    return (fileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0;
#else
    struct stat st = {};
    lstat(path.c_str(), &st);
    return (st.st_mode & S_IFMT) == S_IFREG;
#endif
}

bool isDirectory(const std::string& path)
{
#ifdef _WIN32
    DWORD fileAttributes = GetFileAttributesW(fromUtf8(path).c_str());
    if (fileAttributes == INVALID_FILE_ATTRIBUTES)
        return false;
    return (fileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
#else
    struct stat st = {};
    lstat(path.c_str(), &st);
    return (st.st_mode & S_IFMT) == S_IFDIR;
#endif
}

std::vector<std::string_view> splitPath(std::string_view path)
{
    std::vector<std::string_view> components;

    size_t pos = 0;
    size_t nextPos = path.find_first_of("\\/", pos);

    while (nextPos != std::string::npos)
    {
        components.push_back(path.substr(pos, nextPos - pos));
        pos = nextPos + 1;
        nextPos = path.find_first_of("\\/", pos);
    }
    components.push_back(path.substr(pos));

    return components;
}

std::string joinPaths(const std::string& lhs, const std::string& rhs)
{
    std::string result = lhs;
    if (!result.empty() && result.back() != '/' && result.back() != '\\')
        result += '/';
    result += rhs;
    return result;
}

std::optional<std::string> getParentPath(const std::string& path)
{
    if (path == "" || path == "." || path == "/")
        return std::nullopt;

#ifdef _WIN32
    if (path.size() == 2 && path.back() == ':')
        return std::nullopt;
#endif

    size_t slash = path.find_last_of("\\/", path.size() - 1);

    if (slash == 0)
        return "/";

    if (slash != std::string::npos)
        return path.substr(0, slash);

    return "";
}

static std::string getExtension(const std::string& path)
{
    size_t dot = path.find_last_of(".\\/");

    if (dot == std::string::npos || path[dot] != '.')
        return "";

    return path.substr(dot);
}

std::vector<std::string> getSourceFiles(int argc, char** argv)
{
    std::vector<std::string> files;

    for (int i = 1; i < argc; ++i)
    {
        // Early out once we reach --program-args,-a since the remaining args are passed to lua
        if (strcmp(argv[i], "--program-args") == 0 || strcmp(argv[i], "-a") == 0)
            return files;

        // Treat '-' as a special file whose source is read from stdin
        // All other arguments that start with '-' are skipped
        if (argv[i][0] == '-' && argv[i][1] != '\0')
            continue;

        if (isDirectory(argv[i]))
        {
            traverseDirectory(
                argv[i],
                [&](const std::string& name)
                {
                    std::string ext = getExtension(name);

                    if (ext == ".lua" || ext == ".luau")
                        files.push_back(name);
                }
            );
        }
        else
        {
            files.push_back(argv[i]);
        }
    }

    return files;
}
