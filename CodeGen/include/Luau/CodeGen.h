// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>

#include <stdint.h>

struct lua_State;

namespace Luau
{
namespace CodeGen
{

enum CodeGenFlags
{
    // Only run native codegen for modules that have been marked with --!native
    CodeGen_OnlyNativeModules = 1 << 0,
};

struct CompilationStats
{
    size_t bytecodeSizeBytes = 0;
    size_t nativeCodeSizeBytes = 0;
    size_t nativeDataSizeBytes = 0;
    size_t nativeMetadataSizeBytes = 0;

    uint32_t functionsCompiled = 0;
};

using AllocationCallback = void(void* context, void* oldPointer, size_t oldSize, void* newPointer, size_t newSize);

bool isSupported();

void create(lua_State* L, AllocationCallback* allocationCallback, void* allocationCallbackContext);
void create(lua_State* L);

// Builds target function and all inner functions
void compile(lua_State* L, int idx, unsigned int flags = 0, CompilationStats* stats = nullptr);

using AnnotatorFn = void (*)(void* context, std::string& result, int fid, int instpos);

struct AssemblyOptions
{
    enum Target
    {
        Host,
        A64,
        A64_NoFeatures,
        X64_Windows,
        X64_SystemV,
    };

    Target target = Host;

    bool outputBinary = false;

    bool includeAssembly = false;
    bool includeIr = false;
    bool includeOutlinedCode = false;

    // Optional annotator function can be provided to describe each instruction, it takes function id and sequential instruction id
    AnnotatorFn annotator = nullptr;
    void* annotatorContext = nullptr;
};

// Generates assembly for target function and all inner functions
std::string getAssembly(lua_State* L, int idx, AssemblyOptions options = {});

using PerfLogFn = void (*)(void* context, uintptr_t addr, unsigned size, const char* symbol);

void setPerfLog(void* context, PerfLogFn logFn);

} // namespace CodeGen
} // namespace Luau
