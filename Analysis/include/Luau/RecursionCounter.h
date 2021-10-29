// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"

#include <stdexcept>

namespace Luau
{

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
            throw std::runtime_error("Internal recursion counter limit exceeded");
    }
};

} // namespace Luau
