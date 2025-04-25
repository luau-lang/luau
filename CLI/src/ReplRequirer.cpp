// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ReplRequirer.h"

#include "Luau/CodeGen.h"
#include "Luau/CodeGenOptions.h"
#include "Luau/Require.h"

#include "Luau/RequirerUtils.h"
#include "lua.h"
#include "lualib.h"

#include <string>
#include <string_view>
#include <utility>

static luarequire_WriteResult write(std::optional<std::string> contents, char* buffer, size_t bufferSize, size_t* sizeOut)
{
    if (!contents)
        return luarequire_WriteResult::WRITE_FAILURE;

    size_t nullTerminatedSize = contents->size() + 1;

    if (bufferSize < nullTerminatedSize)
    {
        *sizeOut = nullTerminatedSize;
        return luarequire_WriteResult::WRITE_BUFFER_TOO_SMALL;
    }

    *sizeOut = nullTerminatedSize;
    memcpy(buffer, contents->c_str(), nullTerminatedSize);
    return luarequire_WriteResult::WRITE_SUCCESS;
}

static luarequire_NavigateResult storePathResult(ReplRequirer* req, PathResult result)
{
    if (result.status == PathResult::Status::AMBIGUOUS)
        return NAVIGATE_AMBIGUOUS;

    if (result.status == PathResult::Status::NOT_FOUND)
        return NAVIGATE_NOT_FOUND;

    req->absPath = result.absPath;
    req->relPath = result.relPath;
    req->suffix = result.suffix;

    return NAVIGATE_SUCCESS;
}

static bool is_require_allowed(lua_State* L, void* ctx, const char* requirer_chunkname)
{
    std::string_view chunkname = requirer_chunkname;
    return chunkname == "=stdin" || (!chunkname.empty() && chunkname[0] == '@');
}

static luarequire_NavigateResult reset(lua_State* L, void* ctx, const char* requirer_chunkname)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);

    std::string chunkname = requirer_chunkname;
    if (chunkname == "=stdin")
    {
        return storePathResult(req, getStdInResult());
    }
    else if (!chunkname.empty() && chunkname[0] == '@')
    {
        return storePathResult(req, tryGetRelativePathResult(chunkname.substr(1)));
    }

    return NAVIGATE_NOT_FOUND;
}

static luarequire_NavigateResult jump_to_alias(lua_State* L, void* ctx, const char* path)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);

    luarequire_NavigateResult result = storePathResult(req, getAbsolutePathResult(path));
    if (result != NAVIGATE_SUCCESS)
        return result;

    // Jumping to an absolute path breaks the relative-require chain. The best
    // we can do is to store the absolute path itself.
    req->relPath = req->absPath;
    return NAVIGATE_SUCCESS;
}

static luarequire_NavigateResult to_parent(lua_State* L, void* ctx)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return storePathResult(req, getParent(req->absPath, req->relPath));
}

static luarequire_NavigateResult to_child(lua_State* L, void* ctx, const char* name)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return storePathResult(req, getChild(req->absPath, req->relPath, name));
}

static bool is_module_present(lua_State* L, void* ctx)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return isFilePresent(req->absPath, req->suffix);
}

static luarequire_WriteResult get_contents(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return write(getFileContents(req->absPath, req->suffix), buffer, buffer_size, size_out);
}

static luarequire_WriteResult get_chunkname(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return write("@" + req->relPath, buffer, buffer_size, size_out);
}

static luarequire_WriteResult get_cache_key(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return write(req->absPath + req->suffix, buffer, buffer_size, size_out);
}

static bool is_config_present(lua_State* L, void* ctx)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return isFilePresent(req->absPath, "/.luaurc");
}

static luarequire_WriteResult get_config(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return write(getFileContents(req->absPath, "/.luaurc"), buffer, buffer_size, size_out);
}

static int load(lua_State* L, void* ctx, const char* path, const char* chunkname, const char* contents)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);

    // module needs to run in a new thread, isolated from the rest
    // note: we create ML on main thread so that it doesn't inherit environment of L
    lua_State* GL = lua_mainthread(L);
    lua_State* ML = lua_newthread(GL);
    lua_xmove(GL, L, 1);

    // new thread needs to have the globals sandboxed
    luaL_sandboxthread(ML);

    // now we can compile & run module on the new thread
    std::string bytecode = Luau::compile(contents, req->copts());
    if (luau_load(ML, chunkname, bytecode.data(), bytecode.size(), 0) == 0)
    {
        if (req->codegenEnabled())
        {
            Luau::CodeGen::CompilationOptions nativeOptions;
            Luau::CodeGen::compile(ML, -1, nativeOptions);
        }

        if (req->coverageActive())
            req->coverageTrack(ML, -1);

        int status = lua_resume(ML, L, 0);

        if (status == 0)
        {
            if (lua_gettop(ML) == 0)
                lua_pushstring(ML, "module must return a value");
            else if (!lua_istable(ML, -1) && !lua_isfunction(ML, -1))
                lua_pushstring(ML, "module must return a table or function");
        }
        else if (status == LUA_YIELD)
        {
            lua_pushstring(ML, "module can not yield");
        }
        else if (!lua_isstring(ML, -1))
        {
            lua_pushstring(ML, "unknown error while running module");
        }
    }

    // add ML result to L stack
    lua_xmove(ML, L, 1);
    if (lua_isstring(L, -1))
        lua_error(L);

    // remove ML thread from L stack
    lua_remove(L, -2);

    // added one value to L stack: module result
    return 1;
}

void requireConfigInit(luarequire_Configuration* config)
{
    if (config == nullptr)
        return;

    config->is_require_allowed = is_require_allowed;
    config->reset = reset;
    config->jump_to_alias = jump_to_alias;
    config->to_parent = to_parent;
    config->to_child = to_child;
    config->is_module_present = is_module_present;
    config->get_contents = get_contents;
    config->is_config_present = is_config_present;
    config->get_chunkname = get_chunkname;
    config->get_cache_key = get_cache_key;
    config->get_config = get_config;
    config->load = load;
}

ReplRequirer::ReplRequirer(
    std::function<Luau::CompileOptions()> copts,
    std::function<bool()> coverageActive,
    std::function<bool()> codegenEnabled,
    std::function<void(lua_State*, int)> coverageTrack
)
    : copts(std::move(copts))
    , coverageActive(std::move(coverageActive))
    , codegenEnabled(std::move(codegenEnabled))
    , coverageTrack(std::move(coverageTrack))
{
}
