// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "RequireImpl.h"

#include "Navigation.h"

#include "Luau/RequireNavigator.h"
#include "Luau/Require.h"

#include "lua.h"
#include "lualib.h"

LUAU_FASTFLAGVARIABLE(LuauCyclicRequireShortCircuit)

namespace Luau::Require
{

// Stores explicitly registered modules.
static const char* registeredCacheTableKey = "_REGISTEREDMODULES";

// Stores the results of require calls.
static const char* requiredCacheTableKey = "_MODULES";

// Stores placeholders for currently-loading modules, keyed by module chunkname.
// Populated just before a module's chunk executes; removed after it completes.
static const char* modulePlaceholdersKey = "_MODULEPLACEHOLDERS";

struct ResolvedRequire
{
    static ResolvedRequire fromErrorHandler(const RuntimeErrorHandler& errorHandler)
    {
        return {ResolvedRequire::Status::ErrorReported, "", "", "", errorHandler.getReportedError()};
    }

    static ResolvedRequire fromErrorMessage(const char* message)
    {
        return {ResolvedRequire::Status::ErrorReported, "", "", "", message};
    }

    enum class Status
    {
        Cached,
        ModuleRead,
        ErrorReported
    };

    Status status;
    std::string chunkname;
    std::string loadname;
    std::string cacheKey;
    std::string error;
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
        return ResolvedRequire::fromErrorMessage("require is not supported in this context");

    RuntimeNavigationContext navigationContext{lrc, L, ctx, requirerChunkname};
    RuntimeErrorHandler errorHandler{path};

    Navigator navigator(navigationContext, errorHandler);

    // Updates navigationContext while navigating through the given path.
    Navigator::Status status = navigator.navigate(std::move(path));
    if (status == Navigator::Status::ErrorReported)
        return ResolvedRequire::fromErrorHandler(errorHandler);

    if (!navigationContext.isModulePresent())
        return ResolvedRequire::fromErrorMessage("no module present at resolved path");

    std::optional<std::string> cacheKey = navigationContext.getCacheKey();
    if (!cacheKey)
        return ResolvedRequire::fromErrorMessage("could not get cache key for module");

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
        return ResolvedRequire::fromErrorMessage("could not get chunkname for module");

    std::optional<std::string> loadname = navigationContext.getLoadname();
    if (!loadname)
        return ResolvedRequire::fromErrorMessage("could not get loadname for module");

    return ResolvedRequire{
        ResolvedRequire::Status::ModuleRead,
        std::move(*chunkname),
        std::move(*loadname),
        std::move(*cacheKey),
    };
}

static int checkRegisteredModules(lua_State* L, const char* path)
{
    luaL_findtable(L, LUA_REGISTRYINDEX, registeredCacheTableKey, 1);

    std::string pathLower = std::string(path);
    for (char& c : pathLower)
    {
        if (c >= 'A' && c <= 'Z')
            c -= ('A' - 'a');
    }

    lua_getfield(L, -1, pathLower.c_str());
    if (lua_isnil(L, -1))
    {
        lua_pop(L, 2);
        return 0;
    }

    lua_remove(L, -2);
    return 1;
}

static int CyclicDependencyIndexError(lua_State* L)
{
    const char* key = lua_tostring(L, 2);
    luaL_error(L, "Cannot access the exported field '%s' because it has a cyclic dependency on its requiring module", key ? key : "unknown");
    return 0;
}

static int CyclicDependencyNewIndexError(lua_State* L)
{
    const char* key = lua_tostring(L, 2);
    luaL_error(L, "Cannot set the exported field '%s' because it has a cyclic dependency on its requiring module", key ? key : "unknown");
    return 0;
}

static void invalidateModulePlaceholder(lua_State* L, int idx)
{
    idx = lua_absindex(L, idx);
    lua_newtable(L);
    if (lua_getmetatable(L, idx))
        lua_setfield(L, -2, "__prev_metatable");
    lua_pushcfunction(L, CyclicDependencyIndexError, "CyclicDependencyIndexError");
    lua_setfield(L, -2, "__index");
    lua_pushcfunction(L, CyclicDependencyNewIndexError, "CyclicDependencyNewIndexError");
    lua_setfield(L, -2, "__newindex");
    lua_pushliteral(L, "The metatable is locked");
    lua_setfield(L, -2, "__metatable");
    lua_setmetatable(L, idx);
}

// Fixed stack slots below the load results (LuauCyclicRequireShortCircuit on):
//   (1) path, (2) cacheKey, (3) chunkname, (4) loadname,
//   (5) requirer's chunkname, (6) module placeholder
static const int kRequireStackValues = 6;
static const int kRequireStackValues_DEPRECATED = 4;

int lua_requirecont(lua_State* L, int status)
{
    // LuauCyclicRequireShortCircuit on: 6 fixed slots (path, cacheKey, chunkname, loadname, requirer's chunkname, module placeholder).
    // off: 4 fixed slots (path, cacheKey, chunkname, loadname).
    const int numFixedSlots = FFlag::LuauCyclicRequireShortCircuit ? kRequireStackValues : kRequireStackValues_DEPRECATED;
    LUAU_ASSERT(lua_gettop(L) >= numFixedSlots);
    const int numResults = lua_gettop(L) - numFixedSlots;
    const char* cacheKey = luaL_checkstring(L, 2);
    const char* chunkname = luaL_checkstring(L, 3);

    if (numResults > 1)
        luaL_error(L, "module must return a single value");

    if (FFlag::LuauCyclicRequireShortCircuit)
    {
        const char* requirerChunkname = luaL_checkstring(L, 5);

        // Slot 6 holds the module placeholder; results (if any) start at slot 7.
        const int modulePlaceholderIdx = kRequireStackValues;

        // Check whether the module returned the module placeholder; if not, invalidate it,
        // freeze it, and update the cache with the actual result.
        if (numResults != 1 || lua_rawequal(L, modulePlaceholderIdx, modulePlaceholderIdx + 1) == 0)
        {
            invalidateModulePlaceholder(L, modulePlaceholderIdx);
            lua_setreadonly(L, modulePlaceholderIdx, 1);

            luaL_findtable(L, LUA_REGISTRYINDEX, requiredCacheTableKey, 1);
            numResults == 1 ? lua_pushvalue(L, modulePlaceholderIdx + 1) : lua_pushnil(L);
            lua_setfield(L, -2, cacheKey);
            lua_pop(L, 1);
        }

        luaL_findtable(L, LUA_REGISTRYINDEX, modulePlaceholdersKey, 1);

        // Deregister the loaded module now that loading is complete.
        lua_pushnil(L);
        lua_setfield(L, -2, chunkname);

        // Restore the requirer's module placeholder now that the cycle is resolved.
        lua_getfield(L, -1, requirerChunkname);
        if (!lua_isnil(L, -1))
        {
            if (lua_getmetatable(L, -1))
            {
                lua_getfield(L, -1, "__prev_metatable");
                lua_setmetatable(L, -3);
                lua_pop(L, 1);
            }
        }
        lua_pop(L, 2);
    }
    else
    {
        if (numResults == 1)
        {
            // Initial stack state
            // (-1) result
            lua_getfield(L, LUA_REGISTRYINDEX, requiredCacheTableKey);
            // (-2) result, (-1) cache table

            lua_pushvalue(L, -2);
            // (-3) result, (-2) cache table, (-1) result

            lua_setfield(L, -2, cacheKey);
            // (-2) result, (-1) cache table

            lua_pop(L, 1);
            // (-1) result
        }
    }

    return numResults;
}

int lua_requireinternal(lua_State* L, const char* requirerChunkname)
{
    // Discard extra arguments, we only use path
    lua_settop(L, 1);

    luarequire_Configuration* lrc = static_cast<luarequire_Configuration*>(lua_touserdata(L, lua_upvalueindex(1)));
    if (!lrc)
        luaL_error(L, "unable to find require configuration");

    void* ctx = lua_tolightuserdata(L, lua_upvalueindex(2));

    // (1) path
    const char* path = luaL_checkstring(L, 1);

    if (checkRegisteredModules(L, path) == 1)
        return 1;

    // ResolvedRequire will be destroyed and any string will be pinned to Luau stack, so that luaL_error doesn't need destructors
    bool resolveError = false;

    {
        ResolvedRequire resolvedRequire = resolveRequire(lrc, L, ctx, requirerChunkname, path);

        if (resolvedRequire.status == ResolvedRequire::Status::Cached)
            return 1;

        if (resolvedRequire.status == ResolvedRequire::Status::ErrorReported)
        {
            lua_pushstring(L, resolvedRequire.error.c_str());
            resolveError = true;
        }
        else
        {
            // (1) path, ..., cacheKey, chunkname, loadname
            lua_pushstring(L, resolvedRequire.cacheKey.c_str());
            lua_pushstring(L, resolvedRequire.chunkname.c_str());
            lua_pushstring(L, resolvedRequire.loadname.c_str());
        }
    }

    if (resolveError)
        lua_error(L); // Error already on top of the stack

    const char* chunkname = FFlag::LuauCyclicRequireShortCircuit ? lua_tostring(L, 3) : lua_tostring(L, -2);

    if (FFlag::LuauCyclicRequireShortCircuit)
    {
        const char* cacheKey = lua_tostring(L, 2);

        // (5) requirer's chunkname — needed by lua_requirecont to restore the requirer's placeholder after loading.
        lua_pushstring(L, requirerChunkname);

        // Don't allow reads and writes to a module's exports table if it has a cyclic dependency on its requiring module.
        luaL_findtable(L, LUA_REGISTRYINDEX, modulePlaceholdersKey, 1);
        lua_getfield(L, -1, requirerChunkname);
        if (!lua_isnil(L, -1))
            invalidateModulePlaceholder(L, -1);
        lua_pop(L, 2);

        // Pre-populate the cache so cyclic requires short-circuit instead of re-entering module loading.
        lua_newtable(L); // (6) module placeholder

        luaL_findtable(L, LUA_REGISTRYINDEX, requiredCacheTableKey, 1);
        lua_pushvalue(L, kRequireStackValues);
        lua_setfield(L, -2, cacheKey);
        lua_pop(L, 1);

        // Register the module placeholder so it can be used by cyclic importers and cleaned up after loading completes.
        luaL_findtable(L, LUA_REGISTRYINDEX, modulePlaceholdersKey, 1);
        lua_pushvalue(L, kRequireStackValues);
        lua_setfield(L, -2, chunkname);
        lua_pop(L, 1);
    }

    int stackValues = lua_gettop(L);
    LUAU_ASSERT(stackValues == (FFlag::LuauCyclicRequireShortCircuit ? kRequireStackValues : kRequireStackValues_DEPRECATED));

    const char* loadname = FFlag::LuauCyclicRequireShortCircuit ? lua_tostring(L, 4) : lua_tostring(L, -1);

    int numResults = lrc->load(L, ctx, path, chunkname, loadname);
    if (numResults == -1)
    {
        if (lua_gettop(L) != stackValues)
            luaL_error(L, "stack cannot be modified when require yields");

        return lua_yield(L, 0);
    }

    return lua_requirecont(L, LUA_OK);
}

int lua_proxyrequire(lua_State* L)
{
    const char* requirerChunkname = luaL_checkstring(L, 2);
    return lua_requireinternal(L, requirerChunkname);
}

int lua_require(lua_State* L)
{
    lua_Debug ar;
    int level = 1;

    do
    {
        if (!lua_getinfo(L, level++, "s", &ar))
            luaL_error(L, "require is not supported in this context");
    } while (ar.what[0] == 'C');

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

    // Make path lowercase to ensure case-insensitive matching.
    std::string pathLower = std::string(path, len);
    for (char& c : pathLower)
    {
        if (c >= 'A' && c <= 'Z')
            c -= ('A' - 'a');
    }
    lua_pushstring(L, pathLower.c_str());
    lua_replace(L, 1);

    luaL_findtable(L, LUA_REGISTRYINDEX, registeredCacheTableKey, 1);
    // (1) path, (2) result, (3) cache table

    lua_insert(L, 1);
    // (1) cache table, (2) path, (3) result

    lua_settable(L, 1);
    // (1) cache table

    lua_pop(L, 1);

    return 0;
}

int clearCacheEntry(lua_State* L)
{
    const char* cacheKey = luaL_checkstring(L, 1);
    luaL_findtable(L, LUA_REGISTRYINDEX, requiredCacheTableKey, 1);
    lua_pushnil(L);
    lua_setfield(L, -2, cacheKey);
    lua_pop(L, 1);
    return 0;
}

int clearCache(lua_State* L)
{
    lua_newtable(L);
    lua_setfield(L, LUA_REGISTRYINDEX, requiredCacheTableKey);
    return 0;
}

} // namespace Luau::Require
