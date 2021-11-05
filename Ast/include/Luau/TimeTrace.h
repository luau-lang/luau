// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Common.h"

#include <vector>

#include <stdint.h>

LUAU_FASTFLAG(DebugLuauTimeTracing)

#if defined(LUAU_ENABLE_TIME_TRACE)

namespace Luau
{
namespace TimeTrace
{
uint32_t getClockMicroseconds();

struct Token
{
    const char* name;
    const char* category;
};

enum class EventType : uint8_t
{
    Enter,
    Leave,

    ArgName,
    ArgValue,
};

struct Event
{
    EventType type;
    uint16_t token;

    union
    {
        uint32_t microsec; // 1 hour trace limit
        uint32_t dataPos;
    } data;
};

struct GlobalContext;
struct ThreadContext;

GlobalContext& getGlobalContext();

uint16_t createToken(GlobalContext& context, const char* name, const char* category);
uint32_t createThread(GlobalContext& context, ThreadContext* threadContext);
void releaseThread(GlobalContext& context, ThreadContext* threadContext);
void flushEvents(GlobalContext& context, uint32_t threadId, const std::vector<Event>& events, const std::vector<char>& data);

struct ThreadContext
{
    ThreadContext()
        : globalContext(getGlobalContext())
    {
        threadId = createThread(globalContext, this);
    }

    ~ThreadContext()
    {
        if (!events.empty())
            flushEvents();

        releaseThread(globalContext, this);
    }

    void flushEvents()
    {
        static uint16_t flushToken = createToken(globalContext, "flushEvents", "TimeTrace");

        events.push_back({EventType::Enter, flushToken, {getClockMicroseconds()}});

        TimeTrace::flushEvents(globalContext, threadId, events, data);

        events.clear();
        data.clear();

        events.push_back({EventType::Leave, 0, {getClockMicroseconds()}});
    }

    void eventEnter(uint16_t token)
    {
        eventEnter(token, getClockMicroseconds());
    }

    void eventEnter(uint16_t token, uint32_t microsec)
    {
        events.push_back({EventType::Enter, token, {microsec}});
    }

    void eventLeave()
    {
        eventLeave(getClockMicroseconds());
    }

    void eventLeave(uint32_t microsec)
    {
        events.push_back({EventType::Leave, 0, {microsec}});

        if (events.size() > kEventFlushLimit)
            flushEvents();
    }

    void eventArgument(const char* name, const char* value)
    {
        uint32_t pos = uint32_t(data.size());
        data.insert(data.end(), name, name + strlen(name) + 1);
        events.push_back({EventType::ArgName, 0, {pos}});

        pos = uint32_t(data.size());
        data.insert(data.end(), value, value + strlen(value) + 1);
        events.push_back({EventType::ArgValue, 0, {pos}});
    }

    GlobalContext& globalContext;
    uint32_t threadId;
    std::vector<Event> events;
    std::vector<char> data;

    static constexpr size_t kEventFlushLimit = 8192;
};

ThreadContext& getThreadContext();

struct Scope
{
    explicit Scope(ThreadContext& context, uint16_t token)
        : context(context)
    {
        if (!FFlag::DebugLuauTimeTracing)
            return;

        context.eventEnter(token);
    }

    ~Scope()
    {
        if (!FFlag::DebugLuauTimeTracing)
            return;

        context.eventLeave();
    }

    ThreadContext& context;
};

struct OptionalTailScope
{
    explicit OptionalTailScope(ThreadContext& context, uint16_t token, uint32_t threshold)
        : context(context)
        , token(token)
        , threshold(threshold)
    {
        if (!FFlag::DebugLuauTimeTracing)
            return;

        pos = uint32_t(context.events.size());
        microsec = getClockMicroseconds();
    }

    ~OptionalTailScope()
    {
        if (!FFlag::DebugLuauTimeTracing)
            return;

        if (pos == context.events.size())
        {
            uint32_t curr = getClockMicroseconds();

            if (curr - microsec > threshold)
            {
                context.eventEnter(token, microsec);
                context.eventLeave(curr);
            }
        }
    }

    ThreadContext& context;
    uint16_t token;
    uint32_t threshold;
    uint32_t microsec;
    uint32_t pos;
};

LUAU_NOINLINE std::pair<uint16_t, Luau::TimeTrace::ThreadContext&> createScopeData(const char* name, const char* category);

} // namespace TimeTrace
} // namespace Luau

// Regular scope
#define LUAU_TIMETRACE_SCOPE(name, category) \
    static auto lttScopeStatic = Luau::TimeTrace::createScopeData(name, category); \
    Luau::TimeTrace::Scope lttScope(lttScopeStatic.second, lttScopeStatic.first)

// A scope without nested scopes that may be skipped if the time it took is less than the threshold
#define LUAU_TIMETRACE_OPTIONAL_TAIL_SCOPE(name, category, microsec) \
    static auto lttScopeStaticOptTail = Luau::TimeTrace::createScopeData(name, category); \
    Luau::TimeTrace::OptionalTailScope lttScope(lttScopeStaticOptTail.second, lttScopeStaticOptTail.first, microsec)

// Extra key/value data can be added to regular scopes
#define LUAU_TIMETRACE_ARGUMENT(name, value) \
    do \
    { \
        if (FFlag::DebugLuauTimeTracing) \
            lttScopeStatic.second.eventArgument(name, value); \
    } while (false)

#else

#define LUAU_TIMETRACE_SCOPE(name, category)
#define LUAU_TIMETRACE_OPTIONAL_TAIL_SCOPE(name, category, microsec)
#define LUAU_TIMETRACE_ARGUMENT(name, value) \
    do \
    { \
    } while (false)

#endif
