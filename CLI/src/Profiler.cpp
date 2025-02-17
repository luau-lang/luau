// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"

#include "Luau/DenseHash.h"

#include <thread>
#include <atomic>
#include <string>

struct Profiler
{
    // static state
    lua_Callbacks* callbacks = nullptr;
    int frequency = 1000;
    std::thread thread;

    // variables for communication between loop and trigger
    std::atomic<bool> exit = false;
    std::atomic<uint64_t> ticks = 0;
    std::atomic<uint64_t> samples = 0;

    // private state for trigger
    uint64_t currentTicks = 0;
    std::string stackScratch;

    // statistics, updated by trigger
    Luau::DenseHashMap<std::string, uint64_t> data{""};
    uint64_t gc[16] = {};
} gProfiler;

static void profilerTrigger(lua_State* L, int gc)
{
    uint64_t currentTicks = gProfiler.ticks.load();
    uint64_t elapsedTicks = currentTicks - gProfiler.currentTicks;

    if (elapsedTicks)
    {
        std::string& stack = gProfiler.stackScratch;

        stack.clear();

        if (gc > 0)
            stack += "GC,GC,";

        lua_Debug ar;
        for (int level = 0; lua_getinfo(L, level, "sn", &ar); ++level)
        {
            if (!stack.empty())
                stack += ';';

            stack += ar.short_src;
            stack += ',';
            if (ar.name)
                stack += ar.name;
            stack += ',';
            if (ar.linedefined > 0)
                stack += std::to_string(ar.linedefined);
        }

        if (!stack.empty())
        {
            gProfiler.data[stack] += elapsedTicks;
        }

        if (gc > 0)
        {
            gProfiler.gc[gc] += elapsedTicks;
        }
    }

    gProfiler.currentTicks = currentTicks;
    gProfiler.callbacks->interrupt = nullptr;
}

static void profilerLoop()
{
    double last = lua_clock();

    while (!gProfiler.exit)
    {
        double now = lua_clock();

        if (now - last >= 1.0 / double(gProfiler.frequency))
        {
            int64_t ticks = int64_t((now - last) * 1e6);

            gProfiler.ticks += ticks;
            gProfiler.samples++;
            gProfiler.callbacks->interrupt = profilerTrigger;

            last += ticks * 1e-6;
        }
        else
        {
            std::this_thread::yield();
        }
    }
}

void profilerStart(lua_State* L, int frequency)
{
    gProfiler.frequency = frequency;
    gProfiler.callbacks = lua_callbacks(L);

    gProfiler.exit = false;
    gProfiler.thread = std::thread(profilerLoop);
}

void profilerStop()
{
    gProfiler.exit = true;
    gProfiler.thread.join();
}

void profilerDump(const char* path)
{
    FILE* f = fopen(path, "wb");
    if (!f)
    {
        fprintf(stderr, "Error opening profile %s\n", path);
        return;
    }

    uint64_t total = 0;

    for (auto& p : gProfiler.data)
    {
        fprintf(f, "%lld %s\n", static_cast<long long>(p.second), p.first.c_str());
        total += p.second;
    }

    fclose(f);

    printf(
        "Profiler dump written to %s (total runtime %.3f seconds, %lld samples, %lld stacks)\n",
        path,
        double(total) / 1e6,
        static_cast<long long>(gProfiler.samples.load()),
        static_cast<long long>(gProfiler.data.size())
    );

    uint64_t totalgc = 0;
    for (uint64_t p : gProfiler.gc)
        totalgc += p;

    if (totalgc)
    {
        printf("GC: %.3f seconds (%.2f%%)", double(totalgc) / 1e6, double(totalgc) / double(total) * 100);

        for (size_t i = 0; i < std::size(gProfiler.gc); ++i)
        {
            extern const char* luaC_statename(int state);

            uint64_t p = gProfiler.gc[i];

            if (p)
                printf(", %s %.2f%%", luaC_statename(int(i)), double(p) / double(totalgc) * 100);
        }

        printf("\n");
    }
}
