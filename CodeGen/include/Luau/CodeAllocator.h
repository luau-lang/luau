// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <vector>
#include <stdint.h>
#include <stddef.h>

namespace Luau
{
namespace CodeGen
{

struct CodeAllocator
{
    CodeAllocator(size_t blockSize, size_t maxTotalSize);
    ~CodeAllocator();

    // Places data and code into the executable page area
    // To allow allocation while previously allocated code is already running, allocation has page granularity
    // It's important to group functions together so that page alignment won't result in a lot of wasted space
    bool allocate(uint8_t* data, size_t dataSize, uint8_t* code, size_t codeSize, uint8_t*& result, size_t& resultSize, uint8_t*& resultCodeStart);

    // Provided to callbacks
    void* context = nullptr;

    // Called when new block is created to create and setup the unwinding information for all the code in the block
    // Some platforms require this data to be placed inside the block itself, so we also return 'unwindDataSizeInBlock'
    void* (*createBlockUnwindInfo)(void* context, uint8_t* block, size_t blockSize, size_t& unwindDataSizeInBlock) = nullptr;

    // Called to destroy unwinding information returned by 'createBlockUnwindInfo'
    void (*destroyBlockUnwindInfo)(void* context, void* unwindData) = nullptr;

    static const size_t kMaxUnwindDataSize = 128;

    bool allocateNewBlock(size_t& unwindInfoSize);

    // Current block we use for allocations
    uint8_t* blockPos = nullptr;
    uint8_t* blockEnd = nullptr;

    // All allocated blocks
    std::vector<uint8_t*> blocks;
    std::vector<void*> unwindInfos;

    size_t blockSize = 0;
    size_t maxTotalSize = 0;
};

} // namespace CodeGen
} // namespace Luau
