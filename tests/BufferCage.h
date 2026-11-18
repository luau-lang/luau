// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <stdint.h>
#include <stddef.h>

// the cage reserves 24 gb of virtual address space (16 gb cage + 4 gb runway on each side),
// which cannot fit into a 32-bit process, so the cage is 64-bit only
#if defined(_WIN64) || defined(__x86_64__) || defined(__aarch64__)
#define LUAU_BUFFER_CAGE_SUPPORTED 1
#endif

struct BufferCage
{
    // we keep a bounded set of freed ranges committed so that hot allocation sizes
    // don't pay for mprotect/VirtualAlloc and fresh page faults on every reuse
    static constexpr uint64_t kMaxCachedRanges = 256;
    static constexpr uint64_t kMaxCachedRangeSize = 1 << 20;
    static constexpr uint64_t kMaxCachedBytes = 8 << 20;

    // where the cage starts
    void* base = nullptr;
    // where the backing memory starts
    // invariant: base - mappingStart = RunwaySize
    void* mappingStart = nullptr;
    uint64_t reservedSize = 0;
    uint64_t pageSize = 0;
    uint64_t nextOffset = 0;

    struct CachedRange
    {
        void* pointer = nullptr;
        uint64_t size = 0;
    };

    CachedRange cachedRanges[kMaxCachedRanges];
    size_t cachedRangeCount = 0;
    uint64_t cachedBytes = 0;

    int lastAllocationType = -1;

    BufferCage();
    ~BufferCage();

    BufferCage(const BufferCage&) = delete;
    BufferCage& operator=(const BufferCage&) = delete;

    void* reserve();
    void release();

    bool commit(uint64_t offset, uint64_t size) const;
    bool decommit(uint64_t offset, uint64_t size) const;

    static void* frealloc(void* cage, void* pointer, size_t oldSize, size_t newSize, int type);
};
