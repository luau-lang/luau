// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"
#include "Luau/Config.h"

#include "ScopedFlags.h"
#include "lua.h"
#include "lualib.h"

#include "Luau/Repl.h"
#include "Luau/ReplRequirer.h"
#include "Luau/Require.h"
#include "Luau/FileUtils.h"

#include "doctest.h"

#include <algorithm>
#include <cstring>
#include <initializer_list>
#include <memory>
#include <optional>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#if __APPLE__
#include <TargetConditionals.h>
#if TARGET_OS_IPHONE
#include <CoreFoundation/CoreFoundation.h>

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

class ReplWithPathFixture
{
public:
    ReplWithPathFixture()
        : luaState(luaL_newstate(), lua_close)
    {
        L = luaState.get();
        setupState(L);
        luaL_sandboxthread(L);

        runCode(L, prettyPrintSource);
    }

    // Returns all of the output captured from the pretty printer
    std::string getCapturedOutput()
    {
        lua_getglobal(L, "capturedoutput");
        const char* str = lua_tolstring(L, -1, nullptr);
        std::string result(str);
        lua_pop(L, 1);
        return result;
    }

    enum class PathType
    {
        Absolute,
        Relative
    };

    std::string getLuauDirectory(PathType type)
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

    void runProtectedRequire(const std::string& path)
    {
        runCode(L, "return pcall(function() return require(\"" + path + "\") end)");
    }

    void assertOutputContainsAll(const std::initializer_list<std::string>& list)
    {
        const std::string capturedOutput = getCapturedOutput();
        for (const std::string& elem : list)
        {
            CHECK_MESSAGE(capturedOutput.find(elem) != std::string::npos, "Captured output: ", capturedOutput);
        }
    }

    lua_State* L;

private:
    std::unique_ptr<lua_State, void (*)(lua_State*)> luaState;

    // This is a simplistic and incomplete pretty printer.
    // It is included here to test that the pretty printer hook is being called.
    // More elaborate tests to ensure correct output can be added if we introduce
    // a more feature rich pretty printer.
    std::string prettyPrintSource = R"(
-- Accumulate pretty printer output in `capturedoutput`
capturedoutput = ""

function arraytostring(arr)
    local strings = {}
    table.foreachi(arr, function(k,v) table.insert(strings, pptostring(v)) end )
    return "{" .. table.concat(strings, ", ") .. "}"
end

function pptostring(x)
    if type(x) == "table" then
        -- Just assume array-like tables for now.
        return arraytostring(x)
    elseif type(x) == "string" then
        return '"' .. x .. '"'
    else
        return tostring(x)
    end
end

-- Note: Instead of calling print, the pretty printer just stores the output
-- in `capturedoutput` so we can check for the correct results.
function _PRETTYPRINT(...)
    local args = table.pack(...)
    local strings = {}
    for i=1, args.n do
        local item = args[i]
        local str = pptostring(item, customoptions)
        if i == 1 then
            capturedoutput = capturedoutput .. str
        else
            capturedoutput = capturedoutput .. "\t" .. str
        end
    end
end
)";
};

TEST_SUITE_BEGIN("RequireByStringTests");

TEST_CASE("PathResolution")
{
#ifdef _WIN32
    std::string prefix = "C:/";
#else
    std::string prefix = "/";
#endif

    // tuple format: {inputPath, inputBaseFilePath, expected}
    std::vector<std::tuple<std::string, std::string, std::string>> tests = {
        // 1. Basic path resolution
        // a. Relative to a relative path that begins with './'
        {"./dep", "./src/modules/module.luau", "./src/modules/dep"},
        {"../dep", "./src/modules/module.luau", "./src/dep"},
        {"../../dep", "./src/modules/module.luau", "./dep"},
        {"../../", "./src/modules/module.luau", "./"},

        // b. Relative to a relative path that begins with '../'
        {"./dep", "../src/modules/module.luau", "../src/modules/dep"},
        {"../dep", "../src/modules/module.luau", "../src/dep"},
        {"../../dep", "../src/modules/module.luau", "../dep"},
        {"../../", "../src/modules/module.luau", "../"},

        // c. Relative to an absolute path
        {"./dep", prefix + "src/modules/module.luau", prefix + "src/modules/dep"},
        {"../dep", prefix + "src/modules/module.luau", prefix + "src/dep"},
        {"../../dep", prefix + "src/modules/module.luau", prefix + "dep"},
        {"../../", prefix + "src/modules/module.luau", prefix},


        // 2. Check behavior for extraneous ".."
        // a. Relative paths retain '..' and append if needed
        {"../../../", "./src/modules/module.luau", "../"},
        {"../../../", "../src/modules/module.luau", "../../"},

        // b. Absolute paths ignore '..' if already at root
        {"../../../", prefix + "src/modules/module.luau", prefix},
    };

    for (const auto& [inputPath, inputBaseFilePath, expected] : tests)
    {
        std::optional<std::string> resolved = resolvePath(inputPath, inputBaseFilePath);
        CHECK(resolved);
        CHECK_EQ(resolved, expected);
    }
}

TEST_CASE("PathNormalization")
{
#ifdef _WIN32
    std::string prefix = "C:/";
#else
    std::string prefix = "/";
#endif

    // pair format: {input, expected}
    std::vector<std::pair<std::string, std::string>> tests = {
        // 1. Basic formatting checks
        {"", "./"},
        {".", "./"},
        {"..", "../"},
        {"a/relative/path", "./a/relative/path"},


        // 2. Paths containing extraneous '.' and '/' symbols
        {"./remove/extraneous/symbols/", "./remove/extraneous/symbols"},
        {"./remove/extraneous//symbols", "./remove/extraneous/symbols"},
        {"./remove/extraneous/symbols/.", "./remove/extraneous/symbols"},
        {"./remove/extraneous/./symbols", "./remove/extraneous/symbols"},

        {"../remove/extraneous/symbols/", "../remove/extraneous/symbols"},
        {"../remove/extraneous//symbols", "../remove/extraneous/symbols"},
        {"../remove/extraneous/symbols/.", "../remove/extraneous/symbols"},
        {"../remove/extraneous/./symbols", "../remove/extraneous/symbols"},

        {prefix + "remove/extraneous/symbols/", prefix + "remove/extraneous/symbols"},
        {prefix + "remove/extraneous//symbols", prefix + "remove/extraneous/symbols"},
        {prefix + "remove/extraneous/symbols/.", prefix + "remove/extraneous/symbols"},
        {prefix + "remove/extraneous/./symbols", prefix + "remove/extraneous/symbols"},


        // 3. Paths containing '..'
        // a. '..' removes the erasable component before it
        {"./remove/me/..", "./remove"},
        {"./remove/me/../", "./remove"},

        {"../remove/me/..", "../remove"},
        {"../remove/me/../", "../remove"},

        {prefix + "remove/me/..", prefix + "remove"},
        {prefix + "remove/me/../", prefix + "remove"},

        // b. '..' stays if path is relative and component is non-erasable
        {"./..", "../"},
        {"./../", "../"},

        {"../..", "../../"},
        {"../../", "../../"},

        // c. '..' disappears if path is absolute and component is non-erasable
        {prefix + "..", prefix},
    };

    for (const auto& [input, expected] : tests)
    {
        CHECK_EQ(normalizePath(input), expected);
    }
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireSimpleRelativePath")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/dependency";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from dependency"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireSimpleRelativePathWithinPcall")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/dependency";
    runCode(L, "return pcall(require, \"" + path + "\")");
    assertOutputContainsAll({"true", "result from dependency"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireRelativeToRequiringFile")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/module";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from dependency", "required into module"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireLua")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/lua_dependency";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from lua_dependency"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireInitLuau")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/luau";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from init.luau"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireInitLua")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/lua";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from init.lua"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireSubmoduleUsingSelfIndirectly")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/nested_module_requirer";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from submodule"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireSubmoduleUsingSelfDirectly")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/nested";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from submodule"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "CannotRequireInitLuauDirectly")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/nested/init";
    runProtectedRequire(path);
    assertOutputContainsAll({"false", "could not resolve child component \"init\""});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireNestedInits")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/nested_inits_requirer";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from nested_inits/init", "required into module"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireWithFileAmbiguity")
{
    std::string ambiguousPath = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/ambiguous_file_requirer";

    runProtectedRequire(ambiguousPath);
    assertOutputContainsAll(
        {"false", "error requiring module \"./ambiguous/file/dependency\": could not resolve child component \"dependency\" (ambiguous)"}
    );
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireWithDirectoryAmbiguity")
{
    std::string ambiguousPath = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/ambiguous_directory_requirer";

    runProtectedRequire(ambiguousPath);
    assertOutputContainsAll(
        {"false", "error requiring module \"./ambiguous/directory/dependency\": could not resolve child component \"dependency\" (ambiguous)"}
    );
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "CheckCacheAfterRequireLuau")
{
    std::string relativePath = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/module";
    std::string absolutePath = getLuauDirectory(PathType::Absolute) + "/tests/require/without_config/module";

    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, (absolutePath + ".luau").c_str());
    REQUIRE_MESSAGE(lua_isnil(L, -1), "Cache already contained module result");

    runProtectedRequire(relativePath);

    assertOutputContainsAll({"true", "result from dependency", "required into module"});

    // Check cache for the absolute path as a cache key
    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, (absolutePath + ".luau").c_str());
    REQUIRE_FALSE_MESSAGE(lua_isnil(L, -1), "Cache did not contain module result");
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "CheckCacheAfterRequireLua")
{
    std::string relativePath = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/lua_dependency";
    std::string absolutePath = getLuauDirectory(PathType::Absolute) + "/tests/require/without_config/lua_dependency";

    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, (absolutePath + ".luau").c_str());
    REQUIRE_MESSAGE(lua_isnil(L, -1), "Cache already contained module result");

    runProtectedRequire(relativePath);

    assertOutputContainsAll({"true", "result from lua_dependency"});

    // Check cache for the absolute path as a cache key
    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, (absolutePath + ".lua").c_str());
    REQUIRE_FALSE_MESSAGE(lua_isnil(L, -1), "Cache did not contain module result");
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "CheckCacheAfterRequireInitLuau")
{
    std::string relativePath = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/luau";
    std::string absolutePath = getLuauDirectory(PathType::Absolute) + "/tests/require/without_config/luau";

    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, (absolutePath + "/init.luau").c_str());
    REQUIRE_MESSAGE(lua_isnil(L, -1), "Cache already contained module result");

    runProtectedRequire(relativePath);

    assertOutputContainsAll({"true", "result from init.luau"});

    // Check cache for the absolute path as a cache key
    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, (absolutePath + "/init.luau").c_str());
    REQUIRE_FALSE_MESSAGE(lua_isnil(L, -1), "Cache did not contain module result");
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "CheckCacheAfterRequireInitLua")
{
    std::string relativePath = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/lua";
    std::string absolutePath = getLuauDirectory(PathType::Absolute) + "/tests/require/without_config/lua";

    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, (absolutePath + "/init.lua").c_str());
    REQUIRE_MESSAGE(lua_isnil(L, -1), "Cache already contained module result");

    runProtectedRequire(relativePath);

    assertOutputContainsAll({"true", "result from init.lua"});

    // Check cache for the absolute path as a cache key
    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, (absolutePath + "/init.lua").c_str());
    REQUIRE_FALSE_MESSAGE(lua_isnil(L, -1), "Cache did not contain module result");
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "CheckCachedResult")
{
    std::string relativePath = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/validate_cache";
    runProtectedRequire(relativePath);
    assertOutputContainsAll({"true"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "CheckClearCacheEntry")
{
    std::string relativePath = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/module";
    std::string absolutePath = getLuauDirectory(PathType::Absolute) + "/tests/require/without_config/module";
    std::string cacheKey = absolutePath + ".luau";

    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, cacheKey.c_str());
    REQUIRE_MESSAGE(lua_isnil(L, -1), "Cache already contained module result");

    runProtectedRequire(relativePath);

    assertOutputContainsAll({"true", "result from dependency", "required into module"});

    // Check cache for the absolute path as a cache key
    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, cacheKey.c_str());
    REQUIRE_FALSE_MESSAGE(lua_isnil(L, -1), "Cache did not contain module result");

    lua_pushcfunction(L, luarequire_clearcacheentry, nullptr);
    lua_pushstring(L, cacheKey.c_str());
    lua_call(L, 1, 0);

    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, cacheKey.c_str());
    REQUIRE_MESSAGE(lua_isnil(L, -1), "Cache was not cleared");
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "CheckClearCache")
{
    std::string relativePath = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/module";
    std::string absolutePath = getLuauDirectory(PathType::Absolute) + "/tests/require/without_config/module";
    std::string cacheKey = absolutePath + ".luau";

    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, cacheKey.c_str());
    REQUIRE_MESSAGE(lua_isnil(L, -1), "Cache already contained module result");

    runProtectedRequire(relativePath);

    assertOutputContainsAll({"true", "result from dependency", "required into module"});

    // Check cache for the absolute path as a cache key
    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, cacheKey.c_str());
    REQUIRE_FALSE_MESSAGE(lua_isnil(L, -1), "Cache did not contain module result");

    lua_pushcfunction(L, luarequire_clearcache, nullptr);
    lua_call(L, 0, 0);

    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);
    lua_getfield(L, -1, cacheKey.c_str());
    REQUIRE_MESSAGE(lua_isnil(L, -1), "Cache was not cleared");
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RegisterRuntimeModule")
{
    lua_pushcfunction(L, luarequire_registermodule, nullptr);
    lua_pushstring(L, "@test/helloworld");
    lua_newtable(L);
    lua_pushstring(L, "hello");
    lua_pushstring(L, "world");
    lua_settable(L, -3);
    lua_call(L, 2, 0);

    runCode(L, "return require('@test/helloworld').hello == 'world'");
    assertOutputContainsAll({"true"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RegisterRuntimeModuleCaseInsensitive")
{
    lua_pushcfunction(L, luarequire_registermodule, nullptr);
    lua_pushstring(L, "@test/helloworld");
    lua_newtable(L);
    lua_pushstring(L, "hello");
    lua_pushstring(L, "world");
    lua_settable(L, -3);
    lua_call(L, 2, 0);

    runCode(L, "return require('@TeSt/heLLoWoRld').hello == 'world'");
    assertOutputContainsAll({"true"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "ProxyRequire")
{
    luarequire_pushproxyrequire(L, requireConfigInit, createCliRequireContext(L));
    lua_setglobal(L, "proxyrequire");

    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/proxy_requirer";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from dependency", "required into proxy_requirer"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "LoadStringRelative")
{
    runCode(L, "return pcall(function() return loadstring(\"require('a/relative/path')\")() end)");
    assertOutputContainsAll({"false", "require is not supported in this context"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireAbsolutePath")
{
    std::string absolutePath = "/an/absolute/path";

    runProtectedRequire(absolutePath);
    assertOutputContainsAll({"false", "require path must start with a valid prefix: ./, ../, or @"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireUnprefixedPath")
{
    std::string path = "an/unprefixed/path";
    runProtectedRequire(path);
    assertOutputContainsAll({"false", "require path must start with a valid prefix: ./, ../, or @"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequirePathWithAlias")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/with_config/src/alias_requirer";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from dependency"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequirePathWithParentAlias")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/with_config/src/parent_alias_requirer";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from other_dependency"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequirePathWithAliasPointingToDirectory")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/with_config/src/directory_alias_requirer";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from subdirectory_dependency"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireAliasThatDoesNotExist")
{
    std::string nonExistentAlias = "@this.alias.does.not.exist";

    runProtectedRequire(nonExistentAlias);
    assertOutputContainsAll({"false", "@this.alias.does.not.exist is not a valid alias"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "AliasHasIllegalFormat")
{
    std::string illegalCharacter = "@@";

    runProtectedRequire(illegalCharacter);
    assertOutputContainsAll({"false", "@@ is not a valid alias"});

    std::string pathAlias1 = "@.";

    runProtectedRequire(pathAlias1);
    assertOutputContainsAll({"false", ". is not a valid alias"});


    std::string pathAlias2 = "@..";

    runProtectedRequire(pathAlias2);
    assertOutputContainsAll({"false", ".. is not a valid alias"});

    std::string emptyAlias = "@";

    runProtectedRequire(emptyAlias);
    assertOutputContainsAll({"false", " is not a valid alias"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireFromLuauBinary")
{
    char executable[] = "luau";
    std::vector<std::string> paths = {
        getLuauDirectory(PathType::Relative) + "/tests/require/without_config/dependency.luau",
        getLuauDirectory(PathType::Absolute) + "/tests/require/without_config/dependency.luau",
        getLuauDirectory(PathType::Relative) + "/tests/require/without_config/module.luau",
        getLuauDirectory(PathType::Absolute) + "/tests/require/without_config/module.luau",
        getLuauDirectory(PathType::Relative) + "/tests/require/without_config/nested/init.luau",
        getLuauDirectory(PathType::Absolute) + "/tests/require/without_config/nested/init.luau",
        getLuauDirectory(PathType::Relative) + "/tests/require/with_config/src/submodule/init.luau",
        getLuauDirectory(PathType::Absolute) + "/tests/require/with_config/src/submodule/init.luau",
    };

    for (const std::string& path : paths)
    {
        std::vector<char> pathStr(path.size() + 1);
        strncpy(pathStr.data(), path.c_str(), path.size());
        pathStr[path.size()] = '\0';

        char* argv[2] = {executable, pathStr.data()};
        CHECK_EQ(replMain(2, argv), 0);
    }
}

TEST_CASE("ParseAliases")
{
    std::string configJson = R"({
    "aliases": {
        "MyAlias": "/my/alias/path",
    }
})";

    Luau::Config config;

    Luau::ConfigOptions::AliasOptions aliasOptions;
    aliasOptions.configLocation = "/default/location";
    aliasOptions.overwriteAliases = true;

    Luau::ConfigOptions options{false, aliasOptions};

    std::optional<std::string> error = Luau::parseConfig(configJson, config, options);
    REQUIRE(!error);

    auto checkContents = [](Luau::Config& config) -> void
    {
        CHECK(config.aliases.size() == 1);
        REQUIRE(config.aliases.contains("myalias"));

        Luau::Config::AliasInfo& aliasInfo = config.aliases["myalias"];
        CHECK(aliasInfo.value == "/my/alias/path");
        CHECK(aliasInfo.originalCase == "MyAlias");
    };

    checkContents(config);

    // Ensure that copied Configs retain the same information
    Luau::Config copyConstructedConfig = config;
    checkContents(copyConstructedConfig);

    Luau::Config copyAssignedConfig;
    copyAssignedConfig = config;
    checkContents(copyAssignedConfig);
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireBoolean")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/types/boolean";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "false"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireBuffer")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/types/buffer";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "buffer"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireFunction")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/types/function";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "function"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireNil")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/types/nil";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "nil"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireNumber")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/types/number";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "12345"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireString")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/types/string";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "\"foo\""});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireTable")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/types/table";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "{\"foo\", \"bar\"}"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireThread")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/types/thread";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "thread"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireUserdata")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/types/userdata";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "userdata"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireVector")
{
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/types/vector";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "1, 2, 3"});
}

TEST_SUITE_END();
