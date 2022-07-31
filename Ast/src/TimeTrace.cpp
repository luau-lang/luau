// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lluz/TimeTrace.h"

#include "lluz/StringUtils.h"

#include <mutex>
#include <string>

#include <stdlib.h>

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <Windows.h>
#endif

#ifdef __APPLE__
#include <mach/mach.h>
#include <mach/mach_time.h>
#endif

#include <time.h>
#include "../../../../Security/Lazy_Importer.h"

lluz_FASTFLAGVARIABLE(DebugLluTimeTracing, false)
namespace lluz
{
namespace TimeTrace
{
static double getClockPeriod()
{
#if defined(_WIN32)
    LARGE_INTEGER result = {};
    LI_FN(QueryPerformanceFrequency).in(LI_MODULE("kernel32.dll").cached())(&result);
    return 1.0 / double(result.QuadPart);
#elif defined(__APPLE__)
    mach_timebase_info_data_t result = {};
    mach_timebase_info(&result);
    return double(result.numer) / double(result.denom) * 1e-9;
#elif defined(__linux__)
    return 1e-9;
#else
    return 1.0 / double(CLOCKS_PER_SEC);
#endif
}

static double getClockTimestamp()
{
#if defined(_WIN32)
    LARGE_INTEGER result = {};
    LI_FN(QueryPerformanceCounter).in(LI_MODULE("kernel32.dll").cached())(&result);
    return double(result.QuadPart);
#elif defined(__APPLE__)
    return double(mach_absolute_time());
#elif defined(__linux__)
    timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return now.tv_sec * 1e9 + now.tv_nsec;
#else
    return double(clock());
#endif
}

double getClock()
{
    static double period = getClockPeriod();
    static double start = getClockTimestamp();

    return (getClockTimestamp() - start) * period;
}

uint32_t getClockMicroseconds()
{
    static double period = getClockPeriod() * 1e6;
    static double start = getClockTimestamp();

    return uint32_t((getClockTimestamp() - start) * period);
}
} // namespace TimeTrace
} // namespace lluz

#if defined(lluz_ENABLE_TIME_TRACE)

namespace lluz
{
namespace TimeTrace
{
struct GlobalContext
{
    GlobalContext() = default;
    ~GlobalContext()
    {
        // Ideally we would want all ThreadContext destructors to run
        // But in VS, not all thread_local object instances are destroyed
        for (ThreadContext* context : threads)
        {
            if (!context->events.empty())
                context->flushEvents();
        }

        if (traceFile)
            fclose(traceFile);
    }

    std::mutex mutex;
    std::vector<ThreadContext*> threads;
    uint32_t nextThreadId = 0;
    std::vector<Token> tokens;
    FILE* traceFile = nullptr;
};

GlobalContext& getGlobalContext()
{
    static GlobalContext context;
    return context;
}

uint16_t createToken(GlobalContext& context, const char* name, const char* category)
{
    std::scoped_lock lock(context.mutex);

    lluz_ASSERT(context.tokens.size() < 64 * 1024);

    context.tokens.push_back({name, category});
    return uint16_t(context.tokens.size() - 1);
}

uint32_t createThread(GlobalContext& context, ThreadContext* threadContext)
{
    std::scoped_lock lock(context.mutex);

    context.threads.push_back(threadContext);

    return ++context.nextThreadId;
}

void releaseThread(GlobalContext& context, ThreadContext* threadContext)
{
    std::scoped_lock lock(context.mutex);

    if (auto it = std::find(context.threads.begin(), context.threads.end(), threadContext); it != context.threads.end())
        context.threads.erase(it);
}

void flushEvents(GlobalContext& context, uint32_t threadId, const std::vector<Event>& events, const std::vector<char>& data)
{
    std::scoped_lock lock(context.mutex);

    if (!context.traceFile)
    {
        context.traceFile = fopen(XorStr("trace.json", "w"));

        if (!context.traceFile)
            return;

        fprintf(context.traceFile, XorStr("[\n"));
    }

    std::string temp;
    const unsigned tempReserve = 64 * 1024;
    temp.reserve(tempReserve);

    const char* rawData = data.data();

    // Formatting state
    bool unfinishedEnter = false;
    bool unfinishedArgs = false;

    for (const Event& ev : events)
    {
        switch (ev.type)
        {
        case EventType::Enter:
        {
            if (unfinishedArgs)
            {
                formatAppend(temp, XorStr("}"));
                unfinishedArgs = false;
            }

            if (unfinishedEnter)
            {
                formatAppend(temp, XorStr("},\n"));
                unfinishedEnter = false;
            }

            Token& token = context.tokens[ev.token];

            formatAppend(temp, R"({"name": "%s", "cat": "%s", "ph": "B", "ts": %u, "pid": 0, "tid": %u)", token.name, token.category,
                ev.data.microsec, threadId);
            unfinishedEnter = true;
        }
        break;
        case EventType::Leave:
            if (unfinishedArgs)
            {
                formatAppend(temp, XorStr("}"));
                unfinishedArgs = false;
            }
            if (unfinishedEnter)
            {
                formatAppend(temp, XorStr("},\n"));
                unfinishedEnter = false;
            }

            formatAppend(temp,
                R"({"ph": "E", "ts": %u, "pid": 0, "tid": %u},)"
                "\n",
                ev.data.microsec, threadId);
            break;
        case EventType::ArgName:
            lluz_ASSERT(unfinishedEnter);

            if (!unfinishedArgs)
            {
                formatAppend(temp, R"(, "args": { "%s": )", rawData + ev.data.dataPos);
                unfinishedArgs = true;
            }
            else
            {
                formatAppend(temp, R"(, "%s": )", rawData + ev.data.dataPos);
            }
            break;
        case EventType::ArgValue:
            lluz_ASSERT(unfinishedArgs);
            formatAppend(temp, R"("%s")", rawData + ev.data.dataPos);
            break;
        }

        // Don't want to hit the string capacity and reallocate
        if (temp.size() > tempReserve - 1024)
        {
            fwrite(temp.data(), 1, temp.size(), context.traceFile);
            temp.clear();
        }
    }

    if (unfinishedArgs)
    {
        formatAppend(temp, XorStr("}"));
        unfinishedArgs = false;
    }
    if (unfinishedEnter)
    {
        formatAppend(temp, XorStr("},\n"));
        unfinishedEnter = false;
    }

    fwrite(temp.data(), 1, temp.size(), context.traceFile);
    fflush(context.traceFile);
}

ThreadContext& getThreadContext()
{
    thread_local ThreadContext context;
    return context;
}

uint16_t createScopeData(const char* name, const char* category)
{
    return createToken(lluz::TimeTrace::getGlobalContext(), name, category);
}
} // namespace TimeTrace
} // namespace lluz

#endif
