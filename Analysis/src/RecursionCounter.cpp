// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/RecursionCounter.h"
#include "Luau/Error.h"

namespace Luau
{

RecursionLimitException::RecursionLimitException(const std::string& system)
    : InternalCompilerError("Internal recursion counter limit exceeded in " + system)
{
}

RecursionCounter::RecursionCounter(int* count)
    : count(count)
{
    ++(*count);
}

RecursionCounter::~RecursionCounter()
{
    LUAU_ASSERT(*count > 0);
    --(*count);
}

RecursionLimiter::RecursionLimiter(const std::string& system, int* count, int limit)
    : RecursionCounter(count)
{
    if (!nativeStackGuard.isOk())
    {
        throw InternalCompilerError("Stack overflow in " + system);
    }

    if (limit > 0 && *count > limit)
    {
        throw RecursionLimitException(system);
    }
}

NonExceptionalRecursionLimiter::NonExceptionalRecursionLimiter(int* count)
    : RecursionCounter(count)
{
}

bool NonExceptionalRecursionLimiter::isOk(int limit) const {
    return nativeStackGuard.isOk() && !(limit > 0 && *count > limit);
}

}
