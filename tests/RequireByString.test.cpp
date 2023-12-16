// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"
#include "ScopedFlags.h"
#include "lua.h"
#include "lualib.h"

#include "Repl.h"
#include "FileUtils.h"

#include "doctest.h"

#include <algorithm>
#include <initializer_list>
#include <memory>

LUAU_FASTFLAG(LuauUpdatedRequireByStringSemantics)

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

        std::optional<std::string> cwd = getCurrentWorkingDirectory();
        REQUIRE_MESSAGE(cwd, "Error getting Luau path");
        std::replace((*cwd).begin(), (*cwd).end(), '\\', '/');
        luauDirAbs = *cwd;

        for (int i = 0; i < 20; ++i)
        {
            bool engineTestDir = isDirectory(luauDirAbs + "/Client/Luau/tests");
            bool luauTestDir = isDirectory(luauDirAbs + "/luau/tests");
            if (engineTestDir || luauTestDir)
            {
                if (engineTestDir)
                {
                    luauDirRel += "/Client/Luau";
                    luauDirAbs += "/Client/Luau";
                }
                else
                {
                    luauDirRel += "/luau";
                    luauDirAbs += "/luau";
                }


                if (type == PathType::Relative)
                    return luauDirRel;
                if (type == PathType::Absolute)
                    return luauDirAbs;
            }

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

    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    CHECK(resolvePath(prefix + "Users/modules/module.luau", "") == prefix + "Users/modules/module.luau");
    CHECK(resolvePath(prefix + "Users/modules/module.luau", "a/string/that/should/be/ignored") == prefix + "Users/modules/module.luau");
    CHECK(resolvePath(prefix + "Users/modules/module.luau", "./a/string/that/should/be/ignored") == prefix + "Users/modules/module.luau");
    CHECK(resolvePath(prefix + "Users/modules/module.luau", "/a/string/that/should/be/ignored") == prefix + "Users/modules/module.luau");
    CHECK(resolvePath(prefix + "Users/modules/module.luau", "/Users/modules") == prefix + "Users/modules/module.luau");

    CHECK(resolvePath("../module", "") == "../module");
    CHECK(resolvePath("../../module", "") == "../../module");
    CHECK(resolvePath("../module/..", "") == "..");
    CHECK(resolvePath("../module/../..", "") == "../..");

    CHECK(resolvePath("../dependency", prefix + "Users/modules/module.luau") == prefix + "Users/dependency");
    CHECK(resolvePath("../dependency/", prefix + "Users/modules/module.luau") == prefix + "Users/dependency");
    CHECK(resolvePath("../../../../../Users/dependency", prefix + "Users/modules/module.luau") == prefix + "Users/dependency");
    CHECK(resolvePath("../..", prefix + "Users/modules/module.luau") == prefix);
}

TEST_CASE("PathNormalization")
{
#ifdef _WIN32
    std::string prefix = "C:/";
#else
    std::string prefix = "/";
#endif

    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    // Relative path
    std::optional<std::string> result = normalizePath("../../modules/module");
    CHECK(result);
    std::string normalized = *result;
    std::vector<std::string> variants = {
        "./.././.././modules/./module/", "placeholder/../../../modules/module", "../placeholder/placeholder2/../../../modules/module"};
    for (const std::string& variant : variants)
    {
        result = normalizePath(variant);
        CHECK(result);
        CHECK(normalized == *result);
    }

    // Absolute path
    result = normalizePath(prefix + "Users/modules/module");
    CHECK(result);
    normalized = *result;
    variants = {"Users/Users/Users/.././.././modules/./module/", "placeholder/../Users/..//Users/modules/module",
        "Users/../placeholder/placeholder2/../../Users/modules/module"};
    for (const std::string& variant : variants)
    {
        result = normalizePath(prefix + variant);
        CHECK(result);
        CHECK(normalized == *result);
    }
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireSimpleRelativePath")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/dependency";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from dependency"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireRelativeToRequiringFile")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/module";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from dependency", "required into module"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireLua")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/lua_dependency";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from lua_dependency"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireInitLuau")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/luau";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from init.luau"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireInitLua")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/without_config/lua";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from init.lua"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "CheckCacheAfterRequireLuau")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
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
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
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
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
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
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
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

TEST_CASE_FIXTURE(ReplWithPathFixture, "LoadStringRelative")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    runCode(L, "return pcall(function() return loadstring(\"require('a/relative/path')\")() end)");
    assertOutputContainsAll({"false", "require is not supported in this context"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireAbsolutePath")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
#ifdef _WIN32
    std::string absolutePath = "C:/an/absolute/path";
#else
    std::string absolutePath = "/an/absolute/path";
#endif
    runProtectedRequire(absolutePath);
    assertOutputContainsAll({"false", "cannot require an absolute path"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "PathsArrayRelativePath")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/with_config/src/requirer";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from library"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "PathsArrayExplicitlyRelativePath")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/with_config/src/fail_requirer";
    runProtectedRequire(path);
    assertOutputContainsAll({"false", "error requiring module"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "PathsArrayFromParent")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/with_config/src/global_library_requirer";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from global_library"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequirePathWithAlias")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/with_config/src/alias_requirer";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from dependency"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequirePathWithParentAlias")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/require/with_config/src/parent_alias_requirer";
    runProtectedRequire(path);
    assertOutputContainsAll({"true", "result from other_dependency"});
}


TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireAliasThatDoesNotExist")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
    std::string nonExistentAlias = "@this.alias.does.not.exist";

    runProtectedRequire(nonExistentAlias);
    assertOutputContainsAll({"false", "@this.alias.does.not.exist is not a valid alias"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "AliasHasIllegalFormat")
{
    ScopedFastFlag sff{FFlag::LuauUpdatedRequireByStringSemantics, true};
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

TEST_SUITE_END();
