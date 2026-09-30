// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"

#include <algorithm>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <cstddef>

// the cage reserves 24 gb of virtual address space (16 gb cage + 4 gb runway on each side),
// which cannot fit into a 32-bit process, so the cage is 64-bit only
#if defined(_WIN64) || defined(__x86_64__) || defined(__aarch64__)
#define LUAU_BUFFER_CAGE_SUPPORTED 1
#endif

#ifdef LUAU_BUFFER_CAGE_SUPPORTED

#if defined(_WIN32)
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <io.h>
#include <fcntl.h>
#include <windows.h>
#else
#include <sys/mman.h>
#include <unistd.h>
#endif

static size_t alignUp(size_t value, size_t alignment)
{
    return (value + alignment - 1) & ~(alignment - 1);
}

struct CageReservation
{
    void* mappingStart;  // os-level mapping
    void* base;          // cage base
    size_t reservedSize; // cage + both runways
};

#if defined(_WIN32)
static size_t getSystemPageSizeImpl()
{
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    return info.dwPageSize;
}

static CageReservation reserveAlignedCageImpl(size_t cageSize, size_t runwaySize)
{
    size_t targetSize = cageSize + runwaySize * 2;
    size_t overSize = cageSize * 2 + runwaySize * 2;

    // we reserve a region twice the cage size so we guarantee a cage-aligned base addr is inside it
    // windows doesn't allow partial reservation releases, so we:
    // - find the aligned addr
    // - release the entire 2x allocation
    // - reserve the allocation target size at the aligned addr
    // note: V8 does a multiple retry system here while they attempt to reallocate
    void* mapping = VirtualAlloc(nullptr, overSize, MEM_RESERVE, PAGE_NOACCESS);
    if (!mapping)
        return {nullptr, nullptr, 0};

    uintptr_t aligned = alignUp(reinterpret_cast<uintptr_t>(mapping), cageSize);
    VirtualFree(mapping, 0, MEM_RELEASE);

    void* alignedMapping = VirtualAlloc(reinterpret_cast<void*>(aligned), targetSize, MEM_RESERVE, PAGE_NOACCESS);
    if (!alignedMapping)
        return {nullptr, nullptr, 0};

    CageReservation r{};
    r.mappingStart = alignedMapping;
    r.base = static_cast<char*>(alignedMapping) + runwaySize;
    r.reservedSize = targetSize;
    return r;
}

static void releaseMappedImpl(void* mappingStart, size_t reservedSize)
{
    VirtualFree(mappingStart, 0, MEM_RELEASE);
}

static bool commitImpl(void* base, size_t offset, size_t size)
{
    return VirtualAlloc(static_cast<char*>(base) + offset, size, MEM_COMMIT, PAGE_READWRITE) != nullptr;
}

static bool decommitImpl(void* base, size_t offset, size_t size)
{
    return VirtualFree(static_cast<char*>(base) + offset, size, MEM_DECOMMIT) != 0;
}
#else
static size_t getSystemPageSizeImpl()
{
    long systemPageSize = sysconf(_SC_PAGESIZE);
    return systemPageSize > 0 ? size_t(systemPageSize) : 4096;
}

static CageReservation reserveAlignedCageImpl(size_t cageSize, size_t runwaySize)
{
    // same comments in the windows allocation apply here, except that linux allows us to free partial allocated memory 
    // which avoids the alloc -> free -> realloc dance we do in windows
    size_t targetSize = cageSize + runwaySize * 2;
    size_t mappingSize = cageSize * 2 + runwaySize * 2;

    // |                       |                         |                       |
    // |                       |                         |                       |
    // |      4 GB Runway      |      16 * 2 GB cage     |      4 GB Runway      |
    // |                       |                         |                       |
    // |                       |                         |                       |
    // ^ mapping address

    void* mapping = mmap(nullptr, mappingSize, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mapping == MAP_FAILED)
        return {nullptr, nullptr, 0};

    uintptr_t mappingAddress = reinterpret_cast<uintptr_t>(mapping);
    uintptr_t alignedAddress = alignUp(mappingAddress, cageSize);

    size_t prefixSize = alignedAddress - mappingAddress;
    size_t suffixSize = mappingSize - prefixSize - targetSize;

    if (prefixSize != 0u)
        munmap(mapping, prefixSize);
    if (suffixSize != 0u)
        munmap(reinterpret_cast<void*>(alignedAddress + targetSize), suffixSize);

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

static void releaseMappedImpl(void* mappingStart, size_t reservedSize)
{
    munmap(mappingStart, reservedSize);
}

static bool commitImpl(void* base, size_t offset, size_t size)
{
    return mprotect(static_cast<char*>(base) + offset, size, PROT_READ | PROT_WRITE) == 0;
}

static bool decommitImpl(void* base, size_t offset, size_t size)
{
    void* address = static_cast<char*>(base) + offset;
    if (mprotect(address, size, PROT_NONE) != 0)
        return false;
    madvise(address, size, MADV_DONTNEED);
    return true;
}
#endif

struct BufferCage
{
    // hardcode reserve 16 gb with no reasoning behind it
    static constexpr uint64_t kCageSize = 1ull << 34;
    // runways will be 4 gb on each side
    static constexpr uint64_t kRunwaySize = 1ull << 32;

    // we keep a bounded set of freed ranges committed so that hot allocation sizes
    // don't pay for mprotect/VirtualAlloc and fresh page faults on every reuse
    static constexpr size_t kMaxCachedRanges = 256;
    static constexpr size_t kMaxCachedRangeSize = 1 << 20;
    static constexpr size_t kMaxCachedBytes = 8 << 20;

    struct CachedRange
    {
        void* pointer;
        size_t size;
    };

    // where the buffercage starts
    void* base = nullptr;
    // where the backing memory starts
    // invariant: base - mappingStart = RunwaySize
    void* mappingStart = nullptr;
    size_t reservedSize = 0;
    size_t pageSize = 0;
    size_t nextOffset = 0;
    CachedRange cachedRanges[kMaxCachedRanges] = {};
    size_t cachedRangeCount = 0;
    size_t cachedBytes = 0;
    int lastAllocationType = LUA_TNONE;
    BufferCage()
    {
        pageSize = getSystemPageSizeImpl();
        reserve();
    }

    ~BufferCage()
    {
        if (base)
            release();
    }

    BufferCage(const BufferCage&) = delete;
    BufferCage& operator=(const BufferCage&) = delete;

    void* reserve()
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

    void release()
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

    bool commit(size_t offset, size_t size) const
    {
        if (!this->base || offset > kCageSize || size > kCageSize - offset)
            return false;

        return commitImpl(this->base, offset, size);
    }

    bool decommit(size_t offset, size_t size) const
    {
        if (!this->base || offset > kCageSize || size > kCageSize - offset)
            return false;

        return decommitImpl(this->base, offset, size);
    }

    static void* frealloc(void* cage, void* pointer, size_t oldSize, size_t newSize, int type)
    {
        BufferCage* self = static_cast<BufferCage*>(cage);
        self->lastAllocationType = type;

        if (newSize == 0)
        {
            if (pointer && (oldSize != 0u))
            {
                size_t committedSize = alignUp(oldSize, self->pageSize);

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
                    size_t offset = address - base;
                    self->decommit(offset, committedSize);
                }
            }
            return nullptr;
        }

        if (!self->base || newSize > (1ull << 32) - (self->pageSize - 1))
            return nullptr;

        size_t committedSize = alignUp(newSize, self->pageSize);
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

            size_t offset = self->nextOffset;
            if (!self->commit(offset, committedSize))
                return nullptr;

            result = static_cast<char*>(self->base) + offset;
            self->nextOffset += committedSize;
        }

        if (pointer)
        {
            memcpy(result, pointer, std::min<size_t>(oldSize, newSize));
            frealloc(cage, pointer, oldSize, 0, 0);
        }

        return result;
    }
};

#endif // LUAU_BUFFER_CAGE_SUPPORTED
