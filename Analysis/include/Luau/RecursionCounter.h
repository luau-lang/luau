// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/Error.h"

#include <stdexcept>
#include <exception>

namespace Luau
{

struct RecursionLimitException : public InternalCompilerError
{
    RecursionLimitException()
        : InternalCompilerError("Internal recursion counter limit exceeded")
    {
        LUAU_ASSERT(FFlag::LuauIceExceptionInheritanceChange);
    }
};

struct RecursionLimitException_DEPRECATED : public std::exception
{
    const char* what() const noexcept
    {
        LUAU_ASSERT(!FFlag::LuauIceExceptionInheritanceChange);
        return "Internal recursion counter limit exceeded";
    }
};

struct RecursionCounter
{
    RecursionCounter(int* count)
        : count(count)
    {
        ++(*count);
    }

    ~RecursionCounter()
    {
        LUAU_ASSERT(*count > 0);
        --(*count);
    }

private:
    int* count;
};

struct RecursionLimiter : RecursionCounter
{
    RecursionLimiter(int* count, int limit)
        : RecursionCounter(count)
    {
        if (limit > 0 && *count > limit)
        {
            if (FFlag::LuauIceExceptionInheritanceChange)
            {
                throw RecursionLimitException();
            }
            else
            {
                throw RecursionLimitException_DEPRECATED();
            }
        }
    }
};

} // namespace Luau
