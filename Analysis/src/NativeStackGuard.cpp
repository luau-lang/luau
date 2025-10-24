// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/NativeStackGuard.h"
#include "Luau/Common.h"
#include <iostream>

LUAU_FASTFLAGVARIABLE(LuauUseNativeStackGuard);

// The minimum number of available bytes in the stack's address space.
// If we have less, we want to fail and exit rather than risking an overflow.
LUAU_FASTINTVARIABLE(LuauStackGuardThreshold, 1024);

#if defined(_MSC_VER)

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <intrin.h>

namespace Luau
{

NativeStackGuard::NativeStackGuard()
    : high(0)
    , low(0)
{
    if (!FFlag::LuauUseNativeStackGuard)
        return;

    GetCurrentThreadStackLimits((PULONG_PTR)&low, (PULONG_PTR)&high);
}

bool NativeStackGuard::isOk() const
{
    if (!FFlag::LuauUseNativeStackGuard || FInt::LuauStackGuardThreshold <= 0)
        return true;

    const uintptr_t sp = uintptr_t(_AddressOfReturnAddress());

    const uintptr_t remaining = sp - low;

    return remaining > uintptr_t(FInt::LuauStackGuardThreshold);
}

uintptr_t getStackAddressSpaceSize()
{
    uintptr_t low = 0;
    uintptr_t high = 0;
    GetCurrentThreadStackLimits((PULONG_PTR)&low, (PULONG_PTR)&high);

    return high - low;
}

}

#elif defined(__APPLE__)

#include <pthread.h>

namespace Luau
{

NativeStackGuard::NativeStackGuard()
    : high(0)
    , low(0)
{
    if (!FFlag::LuauUseNativeStackGuard)
        return;

    pthread_t self = pthread_self();
    char* addr = static_cast<char*>(pthread_get_stackaddr_np(self));
    size_t size = pthread_get_stacksize_np(self);

    low = uintptr_t(addr - size);
    high = uintptr_t(addr);
}

bool NativeStackGuard::isOk() const
{
    if (!FFlag::LuauUseNativeStackGuard || FInt::LuauStackGuardThreshold <= 0)
        return true;

    const uintptr_t sp = uintptr_t(__builtin_frame_address(0));

    const uintptr_t remaining = sp - low;

    return remaining > uintptr_t(FInt::LuauStackGuardThreshold);
}

uintptr_t getStackAddressSpaceSize()
{
    pthread_t self = pthread_self();
    return pthread_get_stacksize_np(self);
}

}

#else


namespace Luau
{

NativeStackGuard::NativeStackGuard()
    : high(0)
    , low(0)
{
    (void)low;
    (void)high;
}

bool NativeStackGuard::isOk() const
{
    return true;
}

uintptr_t getStackAddressSpaceSize()
{
    return 0;
}

}

#endif
