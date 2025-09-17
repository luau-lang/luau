// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"

#include <vector>
#include <memory>

namespace Luau
{

void* pagedAllocate(size_t size);
void pagedDeallocate(void* ptr, size_t size);
void pagedFreeze(void* ptr, size_t size);
void pagedUnfreeze(void* ptr, size_t size);

template<typename T>
class TypedAllocator
{
public:
    TypedAllocator()
    {
        currentBlockSize = kBlockSize;
    }

    TypedAllocator(const TypedAllocator&) = delete;
    TypedAllocator& operator=(const TypedAllocator&) = delete;

    TypedAllocator(TypedAllocator&&) = default;
    TypedAllocator& operator=(TypedAllocator&&) = default;

    ~TypedAllocator()
    {
        if (frozen)
            unfreeze();
        free();
    }

    template<typename... Args>
    T* allocate(Args&&... args)
    {
        LUAU_ASSERT(!frozen);

        if (currentBlockSize >= kBlockSize)
        {
            LUAU_ASSERT(currentBlockSize == kBlockSize);
            appendBlock();
        }

        T* block = stuff.back();
        T* res = block + currentBlockSize;
        new (res) T(std::forward<Args>(args)...);
        ++currentBlockSize;
        return res;
    }

    bool contains(const T* ptr) const
    {
        for (T* block : stuff)
            if (ptr >= block && ptr < block + kBlockSize)
                return true;

        return false;
    }

    bool empty() const
    {
        return stuff.empty();
    }

    size_t size() const
    {
        return stuff.empty() ? 0 : kBlockSize * (stuff.size() - 1) + currentBlockSize;
    }

    void clear()
    {
        if (frozen)
            unfreeze();
        free();

        currentBlockSize = kBlockSize;
    }

    void freeze()
    {
        for (T* block : stuff)
            pagedFreeze(block, kBlockSizeBytes);
        frozen = true;
    }

    void unfreeze()
    {
        for (T* block : stuff)
            pagedUnfreeze(block, kBlockSizeBytes);
        frozen = false;
    }

    bool isFrozen()
    {
        return frozen;
    }

private:
    void free()
    {
        LUAU_ASSERT(!frozen);

        for (T* block : stuff)
        {
            size_t blockSize = (block == stuff.back()) ? currentBlockSize : kBlockSize;

            for (size_t i = 0; i < blockSize; ++i)
                block[i].~T();

            pagedDeallocate(block, kBlockSizeBytes);
        }

        stuff.clear();
        currentBlockSize = 0;
    }

    void appendBlock()
    {
        void* block = pagedAllocate(kBlockSizeBytes);
        if (!block)
            throw std::bad_alloc();

        stuff.emplace_back(static_cast<T*>(block));
        currentBlockSize = 0;
    }

    bool frozen = false;
    std::vector<T*> stuff;
    size_t currentBlockSize = 0;

    static constexpr size_t kBlockSizeBytes = 32768;
    static constexpr size_t kBlockSize = kBlockSizeBytes / sizeof(T);
};

} // namespace Luau
