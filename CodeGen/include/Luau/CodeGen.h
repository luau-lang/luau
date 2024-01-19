// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <algorithm>
#include <string>
#include <vector>

#include <stddef.h>
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
    // Run native codegen for functions that the compiler considers not profitable
    CodeGen_ColdFunctions = 1 << 1,
};

enum class CodeGenCompilationResult
{
    Success,          // Successfully generated code for at least one function
    NothingToCompile, // There were no new functions to compile

    CodeGenNotInitialized, // Native codegen system is not initialized
    CodeGenFailed,         // Native codegen failed due to an internal compiler error
    AllocationFailed,      // Native codegen failed due to an allocation error
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
CodeGenCompilationResult compile(lua_State* L, int idx, unsigned int flags = 0, CompilationStats* stats = nullptr);

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

    unsigned int flags = 0;

    bool outputBinary = false;

    bool includeAssembly = false;
    bool includeIr = false;
    bool includeOutlinedCode = false;

    bool includeIrPrefix = true; // "#" before IR blocks and instructions
    bool includeUseInfo = true;
    bool includeCfgInfo = true;
    bool includeRegFlowInfo = true;

    // Optional annotator function can be provided to describe each instruction, it takes function id and sequential instruction id
    AnnotatorFn annotator = nullptr;
    void* annotatorContext = nullptr;
};

struct BlockLinearizationStats
{
    unsigned int constPropInstructionCount = 0;
    double timeSeconds = 0.0;

    BlockLinearizationStats& operator+=(const BlockLinearizationStats& that)
    {
        this->constPropInstructionCount += that.constPropInstructionCount;
        this->timeSeconds += that.timeSeconds;

        return *this;
    }

    BlockLinearizationStats operator+(const BlockLinearizationStats& other) const
    {
        BlockLinearizationStats result(*this);
        result += other;
        return result;
    }
};

enum FunctionStatsFlags
{
    // Enable stats collection per function
    FunctionStats_Enable = 1 << 0,
    // Compute function bytecode summary
    FunctionStats_BytecodeSummary = 1 << 1,
};

struct FunctionStats
{
    std::string name;
    int line = -1;
    unsigned bcodeCount = 0;
    unsigned irCount = 0;
    unsigned asmCount = 0;
    unsigned asmSize = 0;
    std::vector<std::vector<unsigned>> bytecodeSummary;
};

struct LoweringStats
{
    unsigned totalFunctions = 0;
    unsigned skippedFunctions = 0;
    int spillsToSlot = 0;
    int spillsToRestore = 0;
    unsigned maxSpillSlotsUsed = 0;
    unsigned blocksPreOpt = 0;
    unsigned blocksPostOpt = 0;
    unsigned maxBlockInstructions = 0;

    int regAllocErrors = 0;
    int loweringErrors = 0;

    BlockLinearizationStats blockLinearizationStats;

    unsigned functionStatsFlags = 0;
    std::vector<FunctionStats> functions;

    LoweringStats operator+(const LoweringStats& other) const
    {
        LoweringStats result(*this);
        result += other;
        return result;
    }

    LoweringStats& operator+=(const LoweringStats& that)
    {
        this->totalFunctions += that.totalFunctions;
        this->skippedFunctions += that.skippedFunctions;
        this->spillsToSlot += that.spillsToSlot;
        this->spillsToRestore += that.spillsToRestore;
        this->maxSpillSlotsUsed = std::max(this->maxSpillSlotsUsed, that.maxSpillSlotsUsed);
        this->blocksPreOpt += that.blocksPreOpt;
        this->blocksPostOpt += that.blocksPostOpt;
        this->maxBlockInstructions = std::max(this->maxBlockInstructions, that.maxBlockInstructions);
        this->regAllocErrors += that.regAllocErrors;
        this->loweringErrors += that.loweringErrors;
        this->blockLinearizationStats += that.blockLinearizationStats;
        if (this->functionStatsFlags & FunctionStats_Enable)
            this->functions.insert(this->functions.end(), that.functions.begin(), that.functions.end());
        return *this;
    }
};

// Generates assembly for target function and all inner functions
std::string getAssembly(lua_State* L, int idx, AssemblyOptions options = {}, LoweringStats* stats = nullptr);

using PerfLogFn = void (*)(void* context, uintptr_t addr, unsigned size, const char* symbol);

void setPerfLog(void* context, PerfLogFn logFn);

} // namespace CodeGen
} // namespace Luau
