// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ReplRequirer.h"

#include "Luau/CodeGen.h"
#include "Luau/CodeGenOptions.h"
#include "Luau/FileUtils.h"
#include "Luau/Require.h"
#include "Luau/VfsNavigator.h"

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

static luarequire_NavigateResult convert(NavigationStatus status)
{
    if (status == NavigationStatus::Success)
        return NAVIGATE_SUCCESS;
    else if (status == NavigationStatus::Ambiguous)
        return NAVIGATE_AMBIGUOUS;
    else
        return NAVIGATE_NOT_FOUND;
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
        return convert(req->vfs.resetToStdIn());
    else if (!chunkname.empty() && chunkname[0] == '@')
        return convert(req->vfs.resetToPath(chunkname.substr(1)));

    return NAVIGATE_NOT_FOUND;
}

static luarequire_NavigateResult jump_to_alias(lua_State* L, void* ctx, const char* path)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);

    if (!isAbsolutePath(path))
        return NAVIGATE_NOT_FOUND;

    return convert(req->vfs.resetToPath(path));
}

static luarequire_NavigateResult to_parent(lua_State* L, void* ctx)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return convert(req->vfs.toParent());
}

static luarequire_NavigateResult to_child(lua_State* L, void* ctx, const char* name)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return convert(req->vfs.toChild(name));
}

static bool is_module_present(lua_State* L, void* ctx)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return isFile(req->vfs.getFilePath());
}

static luarequire_WriteResult get_chunkname(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return write("@" + req->vfs.getFilePath(), buffer, buffer_size, size_out);
}

static luarequire_WriteResult get_loadname(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return write(req->vfs.getAbsoluteFilePath(), buffer, buffer_size, size_out);
}

static luarequire_WriteResult get_cache_key(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return write(req->vfs.getAbsoluteFilePath(), buffer, buffer_size, size_out);
}

static bool is_config_present(lua_State* L, void* ctx)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return isFile(req->vfs.getLuaurcPath());
}

static luarequire_WriteResult get_config(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);
    return write(readFile(req->vfs.getLuaurcPath()), buffer, buffer_size, size_out);
}

static int load(lua_State* L, void* ctx, const char* path, const char* chunkname, const char* loadname)
{
    ReplRequirer* req = static_cast<ReplRequirer*>(ctx);

    // module needs to run in a new thread, isolated from the rest
    // note: we create ML on main thread so that it doesn't inherit environment of L
    lua_State* GL = lua_mainthread(L);
    lua_State* ML = lua_newthread(GL);
    lua_xmove(GL, L, 1);

    // new thread needs to have the globals sandboxed
    luaL_sandboxthread(ML);

    bool hadContents = false;
    int status = LUA_OK;

    // Handle C++ RAII objects in a scope which doesn't cause a Luau error
    {
        std::optional<std::string> contents = readFile(loadname);
        hadContents = contents.has_value();

        if (contents)
        {
            // now we can compile & run module on the new thread
            std::string bytecode = Luau::compile(*contents, req->copts());
            status = luau_load(ML, chunkname, bytecode.data(), bytecode.size(), 0);
        }
    }

    if (!hadContents)
        luaL_error(L, "could not read file '%s'", loadname);

    if (status == 0)
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
            if (lua_gettop(ML) != 1)
                luaL_error(L, "module must return a single value");
        }
        else if (status == LUA_YIELD)
        {
            luaL_error(L, "module can not yield");
        }
        else if (!lua_isstring(ML, -1))
        {
            luaL_error(L, "unknown error while running module");
        }
        else
        {
            luaL_error(L, "error while running module: %s", lua_tostring(ML, -1));
        }
    }

    // add ML result to L stack
    lua_xmove(ML, L, 1);

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
    config->is_config_present = is_config_present;
    config->get_chunkname = get_chunkname;
    config->get_loadname = get_loadname;
    config->get_cache_key = get_cache_key;
    config->get_alias = nullptr;
    config->get_config = get_config;
    config->load = load;
}

ReplRequirer::ReplRequirer(CompileOptions copts, BoolCheck coverageActive, BoolCheck codegenEnabled, Coverage coverageTrack)
    : copts(copts)
    , coverageActive(coverageActive)
    , codegenEnabled(codegenEnabled)
    , coverageTrack(coverageTrack)
{
}
