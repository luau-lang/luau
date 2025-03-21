// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypedAllocator.h"

#include "Luau/Common.h"

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>

const size_t kPageSize = 4096;
#else
#include <sys/mman.h>
#include <unistd.h>

#if defined(__FreeBSD__) && !(_POSIX_C_SOURCE >= 200112L)
const size_t kPageSize = getpagesize();
#else
const size_t kPageSize = sysconf(_SC_PAGESIZE);
#endif
#endif

#include <stdint.h>
#include <stdlib.h>

LUAU_FASTFLAG(DebugLuauFreezeArena)

namespace Luau
{

static size_t pageAlign(size_t size)
{
    return (size + kPageSize - 1) & ~(kPageSize - 1);
}

void* pagedAllocate(size_t size)
{
    // By default we use operator new/delete instead of malloc/free so that they can be overridden externally
    if (!FFlag::DebugLuauFreezeArena)
    {
        return ::operator new(size, std::nothrow);
    }

    // On Windows, VirtualAlloc results in 64K granularity allocations; we allocate in chunks of ~32K so aligned_malloc is a little more efficient
    // On Linux, we must use mmap because using regular heap results in mprotect() fragmenting the page table and us bumping into 64K mmap limit.
#ifdef _WIN32
    return _aligned_malloc(size, kPageSize);
#elif defined(__FreeBSD__)
    return aligned_alloc(kPageSize, size);
#else
    return mmap(nullptr, pageAlign(size), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
#endif
}

void pagedDeallocate(void* ptr, size_t size)
{
    // By default we use operator new/delete instead of malloc/free so that they can be overridden externally
    if (!FFlag::DebugLuauFreezeArena)
        return ::operator delete(ptr);

#ifdef _WIN32
    _aligned_free(ptr);
#elif defined(__FreeBSD__)
    free(ptr);
#else
    int rc = munmap(ptr, size);
    LUAU_ASSERT(rc == 0);
#endif
}

void pagedFreeze(void* ptr, size_t size)
{
    LUAU_ASSERT(FFlag::DebugLuauFreezeArena);
    LUAU_ASSERT(uintptr_t(ptr) % kPageSize == 0);

#ifdef _WIN32
    DWORD oldProtect;
    BOOL rc = VirtualProtect(ptr, pageAlign(size), PAGE_READONLY, &oldProtect);
    LUAU_ASSERT(rc);
#else
    int rc = mprotect(ptr, pageAlign(size), PROT_READ);
    LUAU_ASSERT(rc == 0);
#endif
}

void pagedUnfreeze(void* ptr, size_t size)
{
    LUAU_ASSERT(FFlag::DebugLuauFreezeArena);
    LUAU_ASSERT(uintptr_t(ptr) % kPageSize == 0);

#ifdef _WIN32
    DWORD oldProtect;
    BOOL rc = VirtualProtect(ptr, pageAlign(size), PAGE_READWRITE, &oldProtect);
    LUAU_ASSERT(rc);
#else
    int rc = mprotect(ptr, pageAlign(size), PROT_READ | PROT_WRITE);
    LUAU_ASSERT(rc == 0);
#endif
}

} // namespace Luau
