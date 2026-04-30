// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"

// Define fast flags here that will be used across various Luau targets such as
// VM and Ast - by putting the flag definition here, you prevent dependent projects
// from breaking when they statically bind to VM but not Ast for example.

LUAU_FASTFLAGVARIABLE(LuauIntegerType)