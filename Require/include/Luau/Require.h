// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lua.h"

#include <stdbool.h>
#include <stddef.h>

////////////////////////////////////////////////////////////////////////////////
//
// Require-by-string assumes that the context in which it is embedded adheres to
// a particular structure.
//
// Each component in a require path either represents a module or a directory.
// Modules contain Luau code, whereas directories serve solely as organizational
// units. For the purposes of navigation, both modules and directories are
// functionally identical: modules and directories can both have children, which
// could themselves be modules or directories, and both types can have at most
// one parent, which could also be either a module or a directory.
//
// Without more context, it is impossible to tell which components in a given
// path "./foo/bar/baz" are modules and which are directories. To provide this
// context, the require-by-string runtime library must be opened with a
// luarequire_Configuration object, which defines the navigation behavior of the
// context in which Luau is embedded.
//
// Calls to to_parent and to_child signal a move up or down the context's
// hierarchy. The context is expected to maintain an internal state so that
// when is_module_present is called, require-by-string can determine whether it
// is currently pointing at a module or a directory.
//
// In a conventional filesystem context, "modules" map either to *.luau files or
// to directories on disk containing an init.luau file, whereas "directories"
// map to directories on disk not containing an init.luau file. In a more
// abstract context, a module and a directory could be represented by any
// nestable code unit and organizational unit, respectively.
//
// Require-by-string's runtime behavior can be additionally be configured in
// configuration files, such as .luaurc or .config.luau files in a filesystem
// context. The presence of a configuration file in the current context is
// signaled by the get_config_status function. Both modules and directories can
// contain configuration files; however, note that a given configuration file's
// scope is limited to the descendants of the module or directory in which it
// resides. In other words, when searching for a relevant configuration file for
// a given module, the search begins at the module's parent context and proceeds
// up the hierarchy from there, resolving to the first configuration file found.
//
////////////////////////////////////////////////////////////////////////////////

typedef enum luarequire_NavigateResult
{
    NAVIGATE_SUCCESS,
    NAVIGATE_AMBIGUOUS,
    NAVIGATE_NOT_FOUND
} luarequire_NavigateResult;

// Functions returning WRITE_SUCCESS are expected to set their size_out argument
// to the number of bytes written to the buffer. If WRITE_BUFFER_TOO_SMALL is
// returned, size_out should be set to the required buffer size.
typedef enum luarequire_WriteResult
{
    WRITE_SUCCESS,
    WRITE_BUFFER_TOO_SMALL,
    WRITE_FAILURE
} luarequire_WriteResult;

// Represents whether a configuration file is present, and if so, its syntax.
typedef enum luarequire_ConfigStatus
{
    CONFIG_ABSENT,
    CONFIG_AMBIGUOUS, // Signals the presence of multiple configuration files.
    CONFIG_PRESENT_JSON,
    CONFIG_PRESENT_LUAU,
} luarequire_ConfigStatus;

typedef struct luarequire_Configuration
{
    // Returns whether requires are permitted from the given chunkname.
    bool (*is_require_allowed)(lua_State* L, void* ctx, const char* requirer_chunkname);

    // Resets the internal state to point at the requirer module.
    luarequire_NavigateResult (*reset)(lua_State* L, void* ctx, const char* requirer_chunkname);

    // Resets the internal state to point at an aliased module, given its exact
    // path from a configuration file. This function is only called when an
    // alias's path cannot be resolved relative to its configuration file.
    luarequire_NavigateResult (*jump_to_alias)(lua_State* L, void* ctx, const char* path);

    // Provides an initial alias override opportunity prior to searching for
    // configuration files. If NAVIGATE_SUCCESS is returned, the internal state
    // must be updated to point at the aliased location. Can be left undefined.
    luarequire_NavigateResult (*to_alias_override)(lua_State* L, void* ctx, const char* alias_unprefixed);

    // Provides a final opportunity to resolve an alias if it cannot be found in
    // configuration files. If NAVIGATE_SUCCESS is returned, the internal state
    // must be updated to point at the aliased location. Can be left undefined.
    luarequire_NavigateResult (*to_alias_fallback)(lua_State* L, void* ctx, const char* alias_unprefixed);

    // Navigates through the context by making mutations to the internal state.
    luarequire_NavigateResult (*to_parent)(lua_State* L, void* ctx);
    luarequire_NavigateResult (*to_child)(lua_State* L, void* ctx, const char* name);

    // Returns whether the context is currently pointing at a module.
    bool (*is_module_present)(lua_State* L, void* ctx);

    // Provides a chunkname for the current module. This will be accessible
    // through the debug library. This function is only called if
    // is_module_present returns true.
    luarequire_WriteResult (*get_chunkname)(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out);

    // Provides a loadname that identifies the current module and is passed to
    // load. This function is only called if is_module_present returns true.
    luarequire_WriteResult (*get_loadname)(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out);

    // Provides a cache key representing the current module. This function is
    // only called if is_module_present returns true.
    luarequire_WriteResult (*get_cache_key)(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out);

    // Returns whether a configuration file is present in the current context,
    // and if so, its syntax. If not present, require-by-string will call
    // to_parent until either a configuration file is present or
    // NAVIGATE_FAILURE is returned (at root).
    luarequire_ConfigStatus (*get_config_status)(lua_State* L, void* ctx);

    // Parses the configuration file in the current context for the given alias
    // and returns its value or WRITE_FAILURE if not found. This function is
    // only called if get_config_status returns true. If this function pointer
    // is set, get_config must not be set. Opting in to this function pointer
    // disables parsing configuration files internally and can be used for finer
    // control over the configuration file parsing process.
    luarequire_WriteResult (*get_alias)(lua_State* L, void* ctx, const char* alias, char* buffer, size_t buffer_size, size_t* size_out);

    // Provides the contents of the configuration file in the current context.
    // This function is only called if get_config_status does not return
    // CONFIG_ABSENT. If this function pointer is set, get_alias must not be
    // set. Opting in to this function pointer enables parsing configuration
    // files internally.
    luarequire_WriteResult (*get_config)(lua_State* L, void* ctx, char* buffer, size_t buffer_size, size_t* size_out);

    // Returns the maximum number of milliseconds to allow for executing a given
    // Luau-syntax configuration file. This function is only called if
    // get_config_status returns CONFIG_PRESENT_LUAU and can be left undefined
    // if support for Luau-syntax configuration files is not needed. A default
    // value of 2000ms is used. Negative values are treated as infinite.
    int (*get_luau_config_timeout)(lua_State* L, void* ctx);

    // Executes the module and places the result on the stack. Returns the
    // number of results placed on the stack. Returning -1 directs the requiring
    // thread to yield. In this case, this thread should be resumed with the
    // module result pushed onto its stack.
    int (*load)(lua_State* L, void* ctx, const char* path, const char* chunkname, const char* loadname);
} luarequire_Configuration;

// Populates function pointers in the given luarequire_Configuration.
typedef void (*luarequire_Configuration_init)(luarequire_Configuration* config);

// Initializes and pushes the require closure onto the stack without
// registration.
LUALIB_API int luarequire_pushrequire(lua_State* L, luarequire_Configuration_init config_init, void* ctx);

// Initializes the require library and registers it globally.
LUALIB_API void luaopen_require(lua_State* L, luarequire_Configuration_init config_init, void* ctx);

// Initializes and pushes a "proxyrequire" closure onto the stack. This function
// takes two parameters: the string path to resolve and the chunkname of an
// existing module. The path is resolved as if it were being required from the
// module that the chunkname represents.
LUALIB_API int luarequire_pushproxyrequire(lua_State* L, luarequire_Configuration_init config_init, void* ctx);

// Registers an aliased require path to a result. After registration, the given
// result will always be immediately returned when the given path is required.
// Expects the path and table to be passed as arguments on the stack.
LUALIB_API int luarequire_registermodule(lua_State* L);

// Clears the entry associated with the given cache key from the require cache.
// Expects the cache key to be passed as an argument on the stack.
LUALIB_API int luarequire_clearcacheentry(lua_State* L);

// Clears all entries from the require cache.
LUALIB_API int luarequire_clearcache(lua_State* L);
