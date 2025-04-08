// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

// Can be used to reconfigure visibility/exports for public APIs
#ifndef LUACOMMON_API
#define LUACOMMON_API extern
#endif

// sets a bool fast flag value (returns 1 if the flag was found, 0 otherwise)
LUACOMMON_API int luau_setfflag(const char* name, int value);
