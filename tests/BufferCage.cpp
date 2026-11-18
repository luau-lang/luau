// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "BufferCage.h"

#include <algorithm>

#include <string.h>

#if defined(_WIN32)
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#else
#include <sys/mman.h>
#include <unistd.h>
#endif

// hardcode reserve 16 gb with no reasoning behind it
static constexpr uint64_t kCageSize = 1ull << 34;
// runways will be 4 gb on each side
static constexpr uint64_t kRunwaySize = 1ull << 32;

static uintptr_t alignUp(uintptr_t value, uint64_t alignment)
{
    return uintptr_t((value + alignment - 1) & ~(alignment - 1));
}

struct CageReservation
{
    void* mappingStart;    // os-level mapping
    void* base;            // cage base
    uint64_t reservedSize; // cage + both runways
};

#if defined(_WIN32)
static uint64_t getSystemPageSizeImpl()
{
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    return info.dwPageSize;
}

static CageReservation reserveAlignedCageImpl(uint64_t cageSize, uint64_t runwaySize)
{
    uint64_t targetSize = cageSize + runwaySize * 2;
    uint64_t overSize = cageSize * 2 + runwaySize * 2;

    if (overSize > SIZE_MAX)
        return {nullptr, nullptr, 0};

    // we reserve a region twice the cage size so we guarantee a cage-aligned base addr is inside it
    // windows doesn't allow partial reservation releases, so we:
    // - find the aligned addr
    // - release the entire 2x allocation
    // - reserve the allocation target size at the aligned addr
    // note: V8 does a multiple retry system here while they attempt to reallocate
    void* mapping = VirtualAlloc(nullptr, size_t(overSize), MEM_RESERVE, PAGE_NOACCESS);
    if (!mapping)
        return {nullptr, nullptr, 0};

    uintptr_t aligned = alignUp(reinterpret_cast<uintptr_t>(mapping), cageSize);
    VirtualFree(mapping, 0, MEM_RELEASE);

    void* alignedMapping = VirtualAlloc(reinterpret_cast<void*>(aligned), size_t(targetSize), MEM_RESERVE, PAGE_NOACCESS);
    if (!alignedMapping)
        return {nullptr, nullptr, 0};

    CageReservation r{};
    r.mappingStart = alignedMapping;
    r.base = static_cast<char*>(alignedMapping) + runwaySize;
    r.reservedSize = targetSize;
    return r;
}

static void releaseMappedImpl(void* mappingStart, uint64_t reservedSize)
{
    VirtualFree(mappingStart, 0, MEM_RELEASE);
}

static bool commitImpl(void* base, uint64_t offset, uint64_t size)
{
    return VirtualAlloc(static_cast<char*>(base) + offset, size_t(size), MEM_COMMIT, PAGE_READWRITE) != nullptr;
}

static bool decommitImpl(void* base, uint64_t offset, uint64_t size)
{
    return VirtualFree(static_cast<char*>(base) + offset, size_t(size), MEM_DECOMMIT) != 0;
}
#else
static uint64_t getSystemPageSizeImpl()
{
    long systemPageSize = sysconf(_SC_PAGESIZE);
    return systemPageSize > 0 ? uint64_t(systemPageSize) : 4096;
}

static CageReservation reserveAlignedCageImpl(uint64_t cageSize, uint64_t runwaySize)
{
    // same comments in the windows allocation apply here, except that linux allows us to free partial allocated memory
    // which avoids the alloc -> free -> realloc dance we do in windows
    uint64_t targetSize = cageSize + runwaySize * 2;
    uint64_t mappingSize = cageSize * 2 + runwaySize * 2;

    if (mappingSize > SIZE_MAX)
        return {nullptr, nullptr, 0};

    // |                       |                         |                       |
    // |                       |                         |                       |
    // |      4 GB Runway      |      16 * 2 GB cage     |      4 GB Runway      |
    // |                       |                         |                       |
    // |                       |                         |                       |
    // ^ mapping address

    void* mapping = mmap(nullptr, size_t(mappingSize), PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mapping == MAP_FAILED)
        return {nullptr, nullptr, 0};

    uintptr_t mappingAddress = reinterpret_cast<uintptr_t>(mapping);
    uintptr_t alignedAddress = alignUp(mappingAddress, cageSize);

    uint64_t prefixSize = alignedAddress - mappingAddress;
    uint64_t suffixSize = mappingSize - prefixSize - targetSize;

    if (prefixSize != 0u)
        munmap(mapping, size_t(prefixSize));
    if (suffixSize != 0u)
        munmap(reinterpret_cast<void*>(alignedAddress + targetSize), size_t(suffixSize));

    //                 |                       |                         |                       |
    //                 |                       |                         |                       |
    //    prefixSize   |      4 GB Runway      |        16 GB cage       |      4 GB Runway      |   suffixSize
    //                 |                       |                         |                       |
    //                 |                       |                         |                       |
    //  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    //               PROT_NONE                                                            PROT_NONE
    //                 ^ mapping start         ^ base

    CageReservation r{};
    r.mappingStart = reinterpret_cast<void*>(alignedAddress);
    r.base = reinterpret_cast<void*>(alignedAddress + runwaySize);
    r.reservedSize = targetSize;
    return r;
}

static void releaseMappedImpl(void* mappingStart, uint64_t reservedSize)
{
    munmap(mappingStart, size_t(reservedSize));
}

static bool commitImpl(void* base, uint64_t offset, uint64_t size)
{
    return mprotect(static_cast<char*>(base) + offset, size_t(size), PROT_READ | PROT_WRITE) == 0;
}

static bool decommitImpl(void* base, uint64_t offset, uint64_t size)
{
    void* address = static_cast<char*>(base) + offset;
    if (mprotect(address, size_t(size), PROT_NONE) != 0)
        return false;
    madvise(address, size_t(size), MADV_DONTNEED);
    return true;
}
#endif

BufferCage::BufferCage()
{
    pageSize = getSystemPageSizeImpl();
    reserve();
}

BufferCage::~BufferCage()
{
    if (base)
        release();
}

void* BufferCage::reserve()
{
    if (this->base && this->mappingStart)
    {
        release();
    }

    CageReservation r = reserveAlignedCageImpl(kCageSize, kRunwaySize);
    if (!r.base)
        return nullptr;

    this->base = r.base;
    this->mappingStart = r.mappingStart;
    this->reservedSize = r.reservedSize;
    return this->base;
}

void BufferCage::release()
{
    if (!this->base)
        return;
    releaseMappedImpl(this->mappingStart, this->reservedSize);

    this->base = nullptr;
    this->mappingStart = nullptr;
    this->reservedSize = 0;
    this->nextOffset = 0;
    this->cachedRangeCount = 0;
    this->cachedBytes = 0;
}

bool BufferCage::commit(uint64_t offset, uint64_t size) const
{
    if (!base || offset > kCageSize || size > kCageSize - offset)
        return false;

    return commitImpl(base, offset, size);
}

bool BufferCage::decommit(uint64_t offset, uint64_t size) const
{
    if (!base || offset > kCageSize || size > kCageSize - offset)
        return false;

    return decommitImpl(base, offset, size);
}

void* BufferCage::frealloc(void* cage, void* pointer, size_t oldSize, size_t newSize, int type)
{
    BufferCage* self = static_cast<BufferCage*>(cage);
    self->lastAllocationType = type;

    if (newSize == 0)
    {
        if (pointer && (oldSize != 0u))
        {
            uint64_t committedSize = alignUp(oldSize, self->pageSize);

            if (committedSize <= kMaxCachedRangeSize && self->cachedRangeCount < kMaxCachedRanges &&
                committedSize <= kMaxCachedBytes - self->cachedBytes)
            {
                self->cachedRanges[self->cachedRangeCount++] = {pointer, committedSize};
                self->cachedBytes += committedSize;
            }
            else
            {
                uintptr_t address = reinterpret_cast<uintptr_t>(pointer);
                uintptr_t base = reinterpret_cast<uintptr_t>(self->base);
                uint64_t offset = address - base;
                self->decommit(offset, committedSize);
            }
        }
        return nullptr;
    }

    if (!self->base || newSize > (1ull << 32) - (self->pageSize - 1))
        return nullptr;

    uint64_t committedSize = alignUp(newSize, self->pageSize);
    void* result = nullptr;

    // search from the end because ranges are inserted and normally reused in LIFO order
    // exact-size reuse avoids fragmentation and splitting
    for (size_t i = self->cachedRangeCount; i > 0; --i)
    {
        if (self->cachedRanges[i - 1].size == committedSize)
        {
            result = self->cachedRanges[i - 1].pointer;
            self->cachedBytes -= committedSize;
            self->cachedRanges[i - 1] = self->cachedRanges[--self->cachedRangeCount];
            break;
        }
    }

    if (!result)
    {
        if (self->nextOffset > kCageSize || committedSize > kCageSize - self->nextOffset)
            return nullptr;

        uint64_t offset = self->nextOffset;
        if (!self->commit(offset, committedSize))
            return nullptr;

        result = static_cast<char*>(self->base) + offset;
        self->nextOffset += committedSize;
    }

    if (pointer)
    {
        memcpy(result, pointer, std::min(oldSize, newSize));
        frealloc(cage, pointer, oldSize, 0, 0);
    }

    return result;
}
