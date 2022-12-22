// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>

struct lua_State;

namespace Luau
{
namespace CodeGen
{

bool isSupported();

void create(lua_State* L);

// Builds target function and all inner functions
void compile(lua_State* L, int idx);

using annotatorFn = void (*)(void* context, std::string& result, int fid, int instpos);

struct AssemblyOptions
{
    bool outputBinary = false;
    bool skipOutlinedCode = false;

    // Optional annotator function can be provided to describe each instruction, it takes function id and sequential instruction id
    annotatorFn annotator = nullptr;
    void* annotatorContext = nullptr;
};

// Generates assembly for target function and all inner functions
std::string getAssembly(lua_State* L, int idx, AssemblyOptions options = {});

} // namespace CodeGen
} // namespace Luau
