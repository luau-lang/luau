// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "ReplWithPathFixture.h"

#include "lua.h"
#include "lualib.h"

#include "Luau/FileUtils.h"
#include "Luau/Repl.h"

#include "doctest.h"

#include <algorithm>


#if __APPLE__
#include <TargetConditionals.h>
#if TARGET_OS_IPHONE
#include <CoreFoundation/CoreFoundation.h>
#include <cstdlib>
#include <unistd.h>

std::optional<std::string> getResourcePath0()
{
    CFBundleRef mainBundle = CFBundleGetMainBundle();
    if (mainBundle == NULL)
    {
        return std::nullopt;
    }
    CFURLRef mainBundleURL = CFBundleCopyBundleURL(mainBundle);
    if (mainBundleURL == NULL)
    {
        CFRelease(mainBundle);
        return std::nullopt;
    }

    char pathBuffer[PATH_MAX];
    if (!CFURLGetFileSystemRepresentation(mainBundleURL, true, (UInt8*)pathBuffer, PATH_MAX))
    {
        CFRelease(mainBundleURL);
        CFRelease(mainBundle);
        return std::nullopt;
    }

    CFRelease(mainBundleURL);
    CFRelease(mainBundle);
    return std::string(pathBuffer);
}

std::optional<std::string> getResourcePath()
{
    static std::optional<std::string> path0 = getResourcePath0();
    return path0;
}
#endif
#endif

namespace Luau
{
ReplWithPathFixture::ReplWithPathFixture()
    : luaState(luaL_newstate(), lua_close)
{
    L = luaState.get();
    setupState(L);
    luaL_sandboxthread(L);

    runCode(L, prettyPrintSource);
}

std::string ReplWithPathFixture::getCapturedOutput()
{
    lua_getglobal(L, "capturedoutput");
    const char* str = lua_tolstring(L, -1, nullptr);
    std::string result(str);
    lua_pop(L, 1);
    return result;
}

std::string ReplWithPathFixture::getLuauDirectory(PathType type)
{
    std::string luauDirRel = ".";
    std::string luauDirAbs;

#if TARGET_OS_IPHONE
    std::optional<std::string> cwd0 = getCurrentWorkingDirectory();
    std::optional<std::string> cwd = getResourcePath();
    if (cwd && cwd0)
    {
        // when running in xcode cwd0 is "/", however that is not always the case
        const auto& _res = *cwd;
        const auto& _cwd = *cwd0;
        if (_res.find(_cwd) == 0)
        {
            // we need relative path so we subtract cwd0 from cwd
            luauDirRel = "./" + _res.substr(_cwd.length());
        }
    }
    if (const char* repoRoot = std::getenv("TEST_SOURCE_ROOT"))
    {
        (void)chdir(repoRoot);
        cwd = getCurrentWorkingDirectory();
        luauDirRel = ".";
    }
#else
    std::optional<std::string> cwd = getCurrentWorkingDirectory();
#endif

    REQUIRE_MESSAGE(cwd, "Error getting Luau path");
    std::replace((*cwd).begin(), (*cwd).end(), '\\', '/');
    luauDirAbs = *cwd;

    for (int i = 0; i < 20; ++i)
    {
        bool engineTestDir = isDirectory(luauDirAbs + "/Client/Luau/tests");
        bool luauTestDir = isDirectory(luauDirAbs + "/tests/require");

        if (engineTestDir || luauTestDir)
        {
            if (engineTestDir)
            {
                luauDirRel += "/Client/Luau";
                luauDirAbs += "/Client/Luau";
            }

            if (type == PathType::Relative)
                return luauDirRel;
            if (type == PathType::Absolute)
                return luauDirAbs;
        }

        if (luauDirRel == ".")
            luauDirRel = "..";
        else
            luauDirRel += "/..";

        std::optional<std::string> parentPath = getParentPath(luauDirAbs);
        REQUIRE_MESSAGE(parentPath, "Error getting Luau path");
        luauDirAbs = *parentPath;
    }

    // Could not find the directory
    REQUIRE_MESSAGE(false, "Error getting Luau path");
    return {};
}

void ReplWithPathFixture::runProtectedRequire(const std::string& path)
{
    runCode(L, "return pcall(function() return require(\"" + path + "\") end)");
}

void ReplWithPathFixture::assertOutputContainsAll(const std::initializer_list<std::string>& list)
{
    const std::string capturedOutput = getCapturedOutput();
    for (const std::string& elem : list)
    {
        CHECK_MESSAGE(capturedOutput.find(elem) != std::string::npos, "Captured output: ", capturedOutput);
    }
}

} // namespace Luau
