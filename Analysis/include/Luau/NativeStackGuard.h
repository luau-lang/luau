// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <stdint.h>

namespace Luau
{

struct NativeStackGuard
{
    NativeStackGuard();

    // Returns true if we are not dangerously close to overrunning the C stack.
    bool isOk() const;

private:
    uintptr_t high;
    uintptr_t low;
};

}

namespace Luau
{

uintptr_t getStackAddressSpaceSize();

}
