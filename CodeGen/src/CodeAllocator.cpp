// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/CodeAllocator.h"

#include "Luau/Common.h"

#include <string.h>

#if defined(_WIN32)

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

#if defined(__FreeBSD__) && !(_POSIX_C_SOURCE >= 200112L)
const size_t kPageSize = getpagesize();
#else
const size_t kPageSize = sysconf(_SC_PAGESIZE);
#endif
#endif

static size_t alignToPageSize(size_t size)
{
    return (size + kPageSize - 1) & ~(kPageSize - 1);
}

#if defined(_WIN32)
static uint8_t* allocatePages(size_t size)
{
    return (uint8_t*)VirtualAlloc(nullptr, alignToPageSize(size), MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
}

static void freePages(uint8_t* mem, size_t size)
{
    if (VirtualFree(mem, 0, MEM_RELEASE) == 0)
        LUAU_ASSERT(!"failed to deallocate block memory");
}

static void makePagesExecutable(uint8_t* mem, size_t size)
{
    LUAU_ASSERT((uintptr_t(mem) & (kPageSize - 1)) == 0);
    LUAU_ASSERT(size == alignToPageSize(size));

    DWORD oldProtect;
    if (VirtualProtect(mem, size, PAGE_EXECUTE_READ, &oldProtect) == 0)
        LUAU_ASSERT(!"failed to change page protection");
}

static void flushInstructionCache(uint8_t* mem, size_t size)
{
    if (FlushInstructionCache(GetCurrentProcess(), mem, size) == 0)
        LUAU_ASSERT(!"failed to flush instruction cache");
}
#else
static uint8_t* allocatePages(size_t size)
{
    return (uint8_t*)mmap(nullptr, alignToPageSize(size), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
}

static void freePages(uint8_t* mem, size_t size)
{
    if (munmap(mem, alignToPageSize(size)) != 0)
        LUAU_ASSERT(!"failed to deallocate block memory");
}

static void makePagesExecutable(uint8_t* mem, size_t size)
{
    LUAU_ASSERT((uintptr_t(mem) & (kPageSize - 1)) == 0);
    LUAU_ASSERT(size == alignToPageSize(size));

    if (mprotect(mem, size, PROT_READ | PROT_EXEC) != 0)
        LUAU_ASSERT(!"failed to change page protection");
}

static void flushInstructionCache(uint8_t* mem, size_t size)
{
    __builtin___clear_cache((char*)mem, (char*)mem + size);
}
#endif

namespace Luau
{
namespace CodeGen
{

CodeAllocator::CodeAllocator(size_t blockSize, size_t maxTotalSize)
    : blockSize(blockSize)
    , maxTotalSize(maxTotalSize)
{
    LUAU_ASSERT(blockSize > kMaxReservedDataSize);
    LUAU_ASSERT(maxTotalSize >= blockSize);
}

CodeAllocator::~CodeAllocator()
{
    if (destroyBlockUnwindInfo)
    {
        for (void* unwindInfo : unwindInfos)
            destroyBlockUnwindInfo(context, unwindInfo);
    }

    for (uint8_t* block : blocks)
        freePages(block, blockSize);
}

bool CodeAllocator::allocate(
    uint8_t* data, size_t dataSize, uint8_t* code, size_t codeSize, uint8_t*& result, size_t& resultSize, uint8_t*& resultCodeStart)
{
    // 'Round up' to preserve code alignment
    size_t alignedDataSize = (dataSize + (kCodeAlignment - 1)) & ~(kCodeAlignment - 1);

    size_t totalSize = alignedDataSize + codeSize;

    // Function has to fit into a single block with unwinding information
    if (totalSize > blockSize - kMaxReservedDataSize)
        return false;

    size_t startOffset = 0;

    // We might need a new block
    if (totalSize > size_t(blockEnd - blockPos))
    {
        if (!allocateNewBlock(startOffset))
            return false;

        LUAU_ASSERT(totalSize <= size_t(blockEnd - blockPos));
    }

    LUAU_ASSERT((uintptr_t(blockPos) & (kPageSize - 1)) == 0); // Allocation starts on page boundary

    size_t dataOffset = startOffset + alignedDataSize - dataSize;
    size_t codeOffset = startOffset + alignedDataSize;

    if (dataSize)
        memcpy(blockPos + dataOffset, data, dataSize);
    if (codeSize)
        memcpy(blockPos + codeOffset, code, codeSize);

    size_t pageAlignedSize = alignToPageSize(startOffset + totalSize);

    makePagesExecutable(blockPos, pageAlignedSize);
    flushInstructionCache(blockPos + codeOffset, codeSize);

    result = blockPos + startOffset;
    resultSize = totalSize;
    resultCodeStart = blockPos + codeOffset;

    // Ensure that future allocations from the block start from a page boundary.
    // This is important since we use W^X, and writing to the previous page would require briefly removing
    // executable bit from it, which may result in access violations if that code is being executed concurrently.
    if (pageAlignedSize <= size_t(blockEnd - blockPos))
    {
        blockPos += pageAlignedSize;
        LUAU_ASSERT((uintptr_t(blockPos) & (kPageSize - 1)) == 0);
        LUAU_ASSERT(blockPos <= blockEnd);
    }
    else
    {
        // Future allocations will need to allocate fresh blocks
        blockPos = blockEnd;
    }

    return true;
}

bool CodeAllocator::allocateNewBlock(size_t& unwindInfoSize)
{
    // Stop allocating once we reach a global limit
    if ((blocks.size() + 1) * blockSize > maxTotalSize)
        return false;

    uint8_t* block = allocatePages(blockSize);

    if (!block)
        return false;

    blockPos = block;
    blockEnd = block + blockSize;

    blocks.push_back(block);

    if (createBlockUnwindInfo)
    {
        void* unwindInfo = createBlockUnwindInfo(context, block, blockSize, unwindInfoSize);

        // 'Round up' to preserve alignment of the following data and code
        unwindInfoSize = (unwindInfoSize + (kCodeAlignment - 1)) & ~(kCodeAlignment - 1);

        LUAU_ASSERT(unwindInfoSize <= kMaxReservedDataSize);

        if (!unwindInfo)
            return false;

        unwindInfos.push_back(unwindInfo);
    }

    return true;
}

} // namespace CodeGen
} // namespace Luau
