// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/CodeAllocator.h"

#include "Luau/CodeGenCommon.h"

#include <string.h>

#if defined(_WIN32)

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

#ifdef __APPLE__
extern "C" void sys_icache_invalidate(void* start, size_t len);
#endif

static size_t alignToPageSize(size_t size)
{
    return (size + kPageSize - 1) & ~(kPageSize - 1);
}

#if defined(_WIN32)
static uint8_t* allocatePagesImpl(size_t size)
{
    CODEGEN_ASSERT(size == alignToPageSize(size));

    return (uint8_t*)VirtualAlloc(nullptr, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
}

static void freePagesImpl(uint8_t* mem, size_t size)
{
    CODEGEN_ASSERT(size == alignToPageSize(size));

    if (VirtualFree(mem, 0, MEM_RELEASE) == 0)
        CODEGEN_ASSERT(!"failed to deallocate block memory");
}

[[nodiscard]] static bool makePagesExecutable(uint8_t* mem, size_t size)
{
    CODEGEN_ASSERT((uintptr_t(mem) & (kPageSize - 1)) == 0);
    CODEGEN_ASSERT(size == alignToPageSize(size));

    DWORD oldProtect;
    return VirtualProtect(mem, size, PAGE_EXECUTE_READ, &oldProtect) != 0;
}

static void flushInstructionCache(uint8_t* mem, size_t size)
{
#if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP | WINAPI_PARTITION_SYSTEM)
    if (FlushInstructionCache(GetCurrentProcess(), mem, size) == 0)
        CODEGEN_ASSERT(!"Failed to flush instruction cache");
#endif
}
#else
static uint8_t* allocatePagesImpl(size_t size)
{
    CODEGEN_ASSERT(size == alignToPageSize(size));

#ifdef __APPLE__
    void* result = mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON | MAP_JIT, -1, 0);
#else
    void* result = mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
#endif

    return (result == MAP_FAILED) ? nullptr : static_cast<uint8_t*>(result);
}

static void freePagesImpl(uint8_t* mem, size_t size)
{
    CODEGEN_ASSERT(size == alignToPageSize(size));

    if (munmap(mem, size) != 0)
        CODEGEN_ASSERT(!"Failed to deallocate block memory");
}

[[nodiscard]] static bool makePagesExecutable(uint8_t* mem, size_t size)
{
    CODEGEN_ASSERT((uintptr_t(mem) & (kPageSize - 1)) == 0);
    CODEGEN_ASSERT(size == alignToPageSize(size));

    return mprotect(mem, size, PROT_READ | PROT_EXEC) == 0;
}

static void flushInstructionCache(uint8_t* mem, size_t size)
{
#ifdef __APPLE__
    sys_icache_invalidate(mem, size);
#else
    __builtin___clear_cache((char*)mem, (char*)mem + size);
#endif
}
#endif

namespace Luau
{
namespace CodeGen
{

CodeAllocator::CodeAllocator(size_t blockSize, size_t maxTotalSize)
    : CodeAllocator(blockSize, maxTotalSize, nullptr, nullptr)
{
}

CodeAllocator::CodeAllocator(size_t blockSize, size_t maxTotalSize, AllocationCallback* allocationCallback, void* allocationCallbackContext)
    : blockSize{blockSize}
    , maxTotalSize{maxTotalSize}
    , allocationCallback{allocationCallback}
    , allocationCallbackContext{allocationCallbackContext}
{
    CODEGEN_ASSERT(blockSize > kMaxReservedDataSize);
    CODEGEN_ASSERT(maxTotalSize >= blockSize);
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
    const uint8_t* data,
    size_t dataSize,
    const uint8_t* code,
    size_t codeSize,
    uint8_t*& result,
    size_t& resultSize,
    uint8_t*& resultCodeStart
)
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

        CODEGEN_ASSERT(totalSize <= size_t(blockEnd - blockPos));
    }

    CODEGEN_ASSERT((uintptr_t(blockPos) & (kPageSize - 1)) == 0); // Allocation starts on page boundary

    size_t dataOffset = startOffset + alignedDataSize - dataSize;
    size_t codeOffset = startOffset + alignedDataSize;

    if (dataSize)
        memcpy(blockPos + dataOffset, data, dataSize);
    if (codeSize)
        memcpy(blockPos + codeOffset, code, codeSize);

    size_t pageAlignedSize = alignToPageSize(startOffset + totalSize);

    if (!makePagesExecutable(blockPos, pageAlignedSize))
        return false;

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
        CODEGEN_ASSERT((uintptr_t(blockPos) & (kPageSize - 1)) == 0);
        CODEGEN_ASSERT(blockPos <= blockEnd);
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

        CODEGEN_ASSERT(unwindInfoSize <= kMaxReservedDataSize);

        if (!unwindInfo)
            return false;

        unwindInfos.push_back(unwindInfo);
    }

    return true;
}

uint8_t* CodeAllocator::allocatePages(size_t size) const
{
    const size_t pageAlignedSize = alignToPageSize(size);

    uint8_t* const mem = allocatePagesImpl(pageAlignedSize);
    if (mem == nullptr)
        return nullptr;

    if (allocationCallback)
        allocationCallback(allocationCallbackContext, nullptr, 0, mem, pageAlignedSize);

    return mem;
}

void CodeAllocator::freePages(uint8_t* mem, size_t size) const
{
    const size_t pageAlignedSize = alignToPageSize(size);

    if (allocationCallback)
        allocationCallback(allocationCallbackContext, mem, pageAlignedSize, nullptr, 0);

    freePagesImpl(mem, pageAlignedSize);
}

} // namespace CodeGen
} // namespace Luau
