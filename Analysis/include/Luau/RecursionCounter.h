// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"

#include <stdexcept>
#include <exception>

LUAU_FASTFLAG(LuauRecursionLimitException);

namespace Luau
{

struct RecursionLimitException : public std::exception
{
    const char* what() const noexcept
    {
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
    // TODO: remove ctx after LuauRecursionLimitException is removed
    RecursionLimiter(int* count, int limit, const char* ctx)
        : RecursionCounter(count)
    {
        LUAU_ASSERT(ctx);
        if (limit > 0 && *count > limit)
        {
            if (FFlag::LuauRecursionLimitException)
                throw RecursionLimitException();
            else
            {
                std::string m = "Internal recursion counter limit exceeded: ";
                m += ctx;
                throw std::runtime_error(m);
            }
        }
    }
};

} // namespace Luau
