// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "RequireImpl.h"

#include "Navigation.h"

#include "Luau/RequireNavigator.h"
#include "Luau/Require.h"

#include "lua.h"
#include "lualib.h"

namespace Luau::Require
{

// Stores explicitly registered modules.
static const char* registeredCacheTableKey = "_REGISTEREDMODULES";

// Stores the results of require calls.
static const char* requiredCacheTableKey = "_MODULES";

struct ResolvedRequire
{
    enum class Status
    {
        Cached,
        ModuleRead,
        ErrorReported
    };

    Status status;
    std::string contents;
    std::string chunkname;
    std::string cacheKey;
};

static bool isCached(lua_State* L, const std::string& key)
{
    luaL_findtable(L, LUA_REGISTRYINDEX, requiredCacheTableKey, 1);
    lua_getfield(L, -1, key.c_str());
    bool cached = !lua_isnil(L, -1);
    lua_pop(L, 2);

    return cached;
}

static ResolvedRequire resolveRequire(luarequire_Configuration* lrc, lua_State* L, void* ctx, const char* requirerChunkname, std::string path)
{
    if (!lrc->is_require_allowed(L, ctx, requirerChunkname))
        luaL_error(L, "require is not supported in this context");

    RuntimeNavigationContext navigationContext{lrc, L, ctx, requirerChunkname};
    RuntimeErrorHandler errorHandler{L}; // Errors reported directly to lua_State.

    Navigator navigator(navigationContext, errorHandler);

    // Updates navigationContext while navigating through the given path.
    Navigator::Status status = navigator.navigate(std::move(path));
    if (status == Navigator::Status::ErrorReported)
        return {ResolvedRequire::Status::ErrorReported};

    if (!navigationContext.isModulePresent())
    {
        luaL_errorL(L, "no module present at resolved path");
        return ResolvedRequire{ResolvedRequire::Status::ErrorReported};
    }

    std::optional<std::string> cacheKey = navigationContext.getCacheKey();
    if (!cacheKey)
    {
        errorHandler.reportError("could not get cache key for module");
        return ResolvedRequire{ResolvedRequire::Status::ErrorReported};
    }

    if (isCached(L, *cacheKey))
    {
        // Put cached result on top of stack before returning.
        lua_getfield(L, LUA_REGISTRYINDEX, requiredCacheTableKey);
        lua_getfield(L, -1, cacheKey->c_str());
        lua_remove(L, -2);

        return ResolvedRequire{ResolvedRequire::Status::Cached};
    }

    std::optional<std::string> chunkname = navigationContext.getChunkname();
    if (!chunkname)
    {
        errorHandler.reportError("could not get chunkname for module");
        return ResolvedRequire{ResolvedRequire::Status::ErrorReported};
    }

    std::optional<std::string> contents = navigationContext.getContents();
    if (!contents)
    {
        errorHandler.reportError("could not get contents for module");
        return ResolvedRequire{ResolvedRequire::Status::ErrorReported};
    }

    return ResolvedRequire{
        ResolvedRequire::Status::ModuleRead,
        std::move(*contents),
        std::move(*chunkname),
        std::move(*cacheKey),
    };
}

static int checkRegisteredModules(lua_State* L, const char* path)
{
    luaL_findtable(L, LUA_REGISTRYINDEX, registeredCacheTableKey, 1);
    lua_getfield(L, -1, path);
    if (lua_isnil(L, -1))
    {
        lua_pop(L, 2);
        return 0;
    }

    lua_remove(L, -2);
    return 1;
}

int lua_requireinternal(lua_State* L, const char* requirerChunkname)
{
    luarequire_Configuration* lrc = static_cast<luarequire_Configuration*>(lua_touserdata(L, lua_upvalueindex(1)));
    if (!lrc)
        luaL_error(L, "unable to find require configuration");

    void* ctx = lua_tolightuserdata(L, lua_upvalueindex(2));

    const char* path = luaL_checkstring(L, 1);

    if (checkRegisteredModules(L, path) == 1)
        return 1;

    ResolvedRequire resolvedRequire = resolveRequire(lrc, L, ctx, requirerChunkname, path);
    if (resolvedRequire.status == ResolvedRequire::Status::Cached)
        return 1;

    int numResults = lrc->load(L, ctx, path, resolvedRequire.chunkname.c_str(), resolvedRequire.contents.c_str());
    if (numResults > 1)
        luaL_error(L, "module must return a single value");

    // Cache the result
    if (numResults == 1)
    {
        // Initial stack state
        // (-1) result

        lua_getfield(L, LUA_REGISTRYINDEX, requiredCacheTableKey);
        // (-2) result, (-1) cache table

        lua_pushvalue(L, -2);
        // (-3) result, (-2) cache table, (-1) result

        lua_setfield(L, -2, resolvedRequire.cacheKey.c_str());
        // (-2) result, (-1) cache table

        lua_pop(L, 1);
        // (-1) result
    }

    return numResults;
}

int lua_proxyrequire(lua_State* L)
{
    const char* requirerChunkname = luaL_checkstring(L, 2);
    return lua_requireinternal(L, requirerChunkname);
}

int lua_require(lua_State* L)
{
    lua_Debug ar;
    lua_getinfo(L, 1, "s", &ar);
    return lua_requireinternal(L, ar.source);
}

int registerModuleImpl(lua_State* L)
{
    if (lua_gettop(L) != 2)
        luaL_error(L, "expected 2 arguments: aliased require path and desired result");

    size_t len;
    const char* path = luaL_checklstring(L, 1, &len);
    std::string_view pathView(path, len);
    if (pathView.empty() || pathView[0] != '@')
        luaL_argerrorL(L, 1, "path must begin with '@'");

    luaL_findtable(L, LUA_REGISTRYINDEX, registeredCacheTableKey, 1);
    // (1) path, (2) result, (3) cache table

    lua_insert(L, 1);
    // (1) cache table, (2) path, (3) result

    lua_settable(L, 1);
    // (1) cache table

    lua_pop(L, 1);

    return 0;
}

} // namespace Luau::Require
