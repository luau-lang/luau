// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lluz/TypedAllocator.h"

#include "lluz/Common.h"

#include "..\..\..\..\Security\Lazy_Importer.h"

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <Windows.h>

const size_t kPageSize = 4096;
#else
#include <sys/mman.h>
#include <unistd.h>

const size_t kPageSize = sysconf(_SC_PAGESIZE);
#endif

#include <stdlib.h>

lluz_FASTFLAG(DebugLluFreezeArena)

namespace lluz
{

static void* systemAllocateAligned(size_t size, size_t align)
{
#ifdef _WIN32
    return _aligned_malloc(size, align);
#elif defined(__ANDROID__) // for Android 4.1
    return memalign(align, size);
#else
    void* ptr;
    return posix_memalign(&ptr, align, size) == 0 ? ptr : 0;
#endif
}

static void systemDeallocateAligned(void* ptr)
{
#ifdef _WIN32
    _aligned_free(ptr);
#else
    free(ptr);
#endif
}

static size_t pageAlign(size_t size)
{
    return (size + kPageSize - 1) & ~(kPageSize - 1);
}

void* pagedAllocate(size_t size)
{
    if (FFlag::DebugLluFreezeArena)
        return systemAllocateAligned(pageAlign(size), kPageSize);
    else
        return ::operator new(size, std::nothrow);
}

void pagedDeallocate(void* ptr)
{
    if (FFlag::DebugLluFreezeArena)
        systemDeallocateAligned(ptr);
    else
        ::operator delete(ptr);
}

void pagedFreeze(void* ptr, size_t size)
{
    lluz_ASSERT(FFlag::DebugLluFreezeArena);
    lluz_ASSERT(uintptr_t(ptr) % kPageSize == 0);

#ifdef _WIN32
    DWORD oldProtect;
    BOOL rc = LI_FN(VirtualProtect).in(LI_MODULE("kernel32.dll").cached())(ptr, pageAlign(size), PAGE_READONLY, &oldProtect);
    lluz_ASSERT(rc);
#else
    int rc = mprotect(ptr, pageAlign(size), PROT_READ);
    lluz_ASSERT(rc == 0);
#endif
}

void pagedUnfreeze(void* ptr, size_t size)
{
    lluz_ASSERT(FFlag::DebugLluFreezeArena);
    lluz_ASSERT(uintptr_t(ptr) % kPageSize == 0);

#ifdef _WIN32
    DWORD oldProtect;
    BOOL rc = LI_FN(VirtualProtect).in(LI_MODULE("kernel32.dll").cached())(ptr, pageAlign(size), PAGE_READWRITE, &oldProtect);
    lluz_ASSERT(rc);
#else
    int rc = mprotect(ptr, pageAlign(size), PROT_READ | PROT_WRITE);
    lluz_ASSERT(rc == 0);
#endif
}

} // namespace lluz
