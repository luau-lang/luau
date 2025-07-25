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
    RecursionLimitException(const std::string system)
        : InternalCompilerError("Internal recursion counter limit exceeded in " + system)
    {
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

protected:
    int* count;
};

struct RecursionLimiter : RecursionCounter
{
    RecursionLimiter(const std::string system, int* count, int limit)
        : RecursionCounter(count)
    {
        if (limit > 0 && *count > limit)
        {
            throw RecursionLimitException(system);
        }
    }
};

} // namespace Luau
