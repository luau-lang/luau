// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/Error.h"
#include "Luau/NativeStackGuard.h"

#include <stdexcept>
#include <exception>

namespace Luau
{

struct RecursionLimitException : public InternalCompilerError
{
    explicit RecursionLimitException(const std::string& system);
};

struct RecursionCounter
{
    explicit RecursionCounter(int* count);
    ~RecursionCounter();

    RecursionCounter(const RecursionCounter&) = delete;
    RecursionCounter& operator=(const RecursionCounter&) = delete;
    RecursionCounter(RecursionCounter&&) = delete;
    RecursionCounter& operator=(RecursionCounter&&) = delete;

protected:
    int* count;
};

struct RecursionLimiter : RecursionCounter
{
    NativeStackGuard nativeStackGuard;

    RecursionLimiter(const std::string& system, int* count, int limit);
};

struct NonExceptionalRecursionLimiter : RecursionCounter
{
    NativeStackGuard nativeStackGuard;

    bool isOk(int limit) const;

    NonExceptionalRecursionLimiter(int* count);

};

} // namespace Luau
