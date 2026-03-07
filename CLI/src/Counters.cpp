// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Counters.h"
#include "Luau/CodeGenOptions.h"
#include "Luau/DenseHash.h"

#include "lua.h"

#include <algorithm>
#include <string>
#include <vector>

struct LineCounters
{
    uint64_t regularExecuted = 0;
    uint64_t fallbackExecuted = 0;
    uint64_t vmExitTaken = 0;
};

struct FunctionCounters
{
    std::string name;
    Luau::DenseHashMap<int, LineCounters> counters{-1};
};

struct ModuleCounters
{
    std::string name;
    std::vector<FunctionCounters> functions;
};

struct Counters
{
    lua_State* L = nullptr;
    std::vector<int> moduleRefs;

    std::vector<ModuleCounters> moduleCounters;

} gCounters;

void countersInit(lua_State* L)
{
    gCounters.L = lua_mainthread(L);
}

bool countersActive()
{
    return gCounters.L != nullptr;
}

void countersTrack(lua_State* L, int funcindex)
{
    int ref = lua_ref(L, funcindex);
    gCounters.moduleRefs.push_back(ref);
}

static void countersFunctionCallback(void* context, const char* function, int lineDefined)
{
    ModuleCounters& counters = *(ModuleCounters*)context;

    std::string name;

    if (function == nullptr && lineDefined == 1)
        name = "<main>";
    else if (function)
        name = std::string(function) + ":" + std::to_string(lineDefined);
    else
        name = "<anonymous>:" + std::to_string(lineDefined);

    counters.functions.emplace_back();
    counters.functions.back().name = name;
}

static void countersValueCallback(void* context, int kind, int line, uint64_t hits)
{
    ModuleCounters& counters = *(ModuleCounters*)context;
    FunctionCounters& function = counters.functions.back();

    Luau::CodeGen::CodeGenCounter counterKind = Luau::CodeGen::CodeGenCounter(kind);

    if (counterKind == Luau::CodeGen::CodeGenCounter::RegularBlockExecuted)
        function.counters[line].regularExecuted += hits;
    else if (counterKind == Luau::CodeGen::CodeGenCounter::FallbackBlockExecuted)
        function.counters[line].fallbackExecuted += hits;
    else if (counterKind == Luau::CodeGen::CodeGenCounter::VmExitTaken)
        function.counters[line].vmExitTaken += hits;
}

void countersDump(const char* path)
{
    lua_State* L = gCounters.L;

    for (int fref : gCounters.moduleRefs)
    {
        lua_getref(L, fref);

        lua_Debug ar = {};
        lua_getinfo(L, -1, "s", &ar);

        gCounters.moduleCounters.emplace_back();
        ModuleCounters& moduleCounters = gCounters.moduleCounters.back();

        moduleCounters.name = std::string(ar.short_src);
        lua_getcounters(L, -1, &moduleCounters, countersFunctionCallback, countersValueCallback);

        lua_pop(L, 1);
    }

    FILE* f = fopen(path, "wb");
    if (!f)
    {
        fprintf(stderr, "Error opening counters file (callgrind) %s\n", path);
        return;
    }

    fprintf(f, "version: 1\n");
    fprintf(f, "creator: Luau REPL\n");
    fprintf(f, "events: Regular Fallback VmExit\n");

    for (const ModuleCounters& moduleCounter : gCounters.moduleCounters)
    {
        fprintf(f, "fl=%s\n", moduleCounter.name.c_str());

        for (const FunctionCounters& functionCounter : moduleCounter.functions)
        {
            fprintf(f, "fn=%s\n", functionCounter.name.c_str());

            // For presentation and stability, we need line counter data sorted by line
            std::vector<std::pair<int, LineCounters>> sortedCounters;

            for (const auto& [line, counters] : functionCounter.counters)
                sortedCounters.emplace_back(line, counters);

            std::sort(
                sortedCounters.begin(),
                sortedCounters.end(),
                [](auto&& a, auto&& b)
                {
                    return a.first < b.first;
                }
            );

            for (const auto& [line, counters] : sortedCounters)
            {
                if (counters.regularExecuted != 0 || counters.fallbackExecuted != 0 || counters.vmExitTaken != 0)
                    fprintf(f, "%d %lld %lld %lld\n", line, (long long)counters.regularExecuted, (long long)counters.fallbackExecuted, (long long)counters.vmExitTaken);
            }
        }
    }

    fclose(f);

    printf("Counters data written to %s (%d modules)\n", path, int(gCounters.moduleCounters.size()));
}
