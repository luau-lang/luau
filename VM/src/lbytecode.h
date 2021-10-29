// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
// This code is based on Lua 5.x implementation licensed under MIT License; see lua_LICENSE.txt for details
#pragma once

// This is a forwarding header for Luau bytecode definition
// Luau consists of several components, including compiler (Ast, Compiler) and VM (virtual machine)
// These components are fully independent, but they both need the bytecode format defined in this header
// so it needs to be shared.
#include "../../Compiler/include/Luau/Bytecode.h"
