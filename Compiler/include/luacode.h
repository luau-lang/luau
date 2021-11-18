// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <stddef.h>

/* Can be used to reconfigure visibility/exports for public APIs */
#ifndef LUACODE_API
#define LUACODE_API extern
#endif

typedef struct lua_CompileOptions lua_CompileOptions;

struct lua_CompileOptions
{
    // 0 - no optimization
    // 1 - baseline optimization level that doesn't prevent debuggability
    // 2 - includes optimizations that harm debuggability such as inlining
    int optimizationLevel; // default=1

    // 0 - no debugging support
    // 1 - line info & function names only; sufficient for backtraces
    // 2 - full debug info with local & upvalue names; necessary for debugger
    int debugLevel; // default=1

    // 0 - no code coverage support
    // 1 - statement coverage
    // 2 - statement and expression coverage (verbose)
    int coverageLevel; // default=0

    // global builtin to construct vectors; disabled by default
    const char* vectorLib;
    const char* vectorCtor;

    // null-terminated array of globals that are mutable; disables the import optimization for fields accessed through these
    const char** mutableGlobals;
};

/* compile source to bytecode; when source compilation fails, the resulting bytecode contains the encoded error. use free() to destroy */
LUACODE_API char* luau_compile(const char* source, size_t size, lua_CompileOptions* options, size_t* outsize);
