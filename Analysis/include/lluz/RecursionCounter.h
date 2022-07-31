// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/Common.h"

#include "..\..\..\..\..\Security\XorString.h"

#include <stdexcept>
#include <exception>

namespace lluz
{

struct RecursionLimitException : public std::exception
{
    const char* what() const noexcept
    {
        return XorStr("Internal recursion counter limit exceeded");
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
        lluz_ASSERT(*count > 0);
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
            throw RecursionLimitException();
        }
    }
};

} // namespace lluz
