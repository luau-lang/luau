// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/CodeBlockUnwind.h"

#include "Luau/CodeAllocator.h"
#include "Luau/UnwindBuilder.h"

#include <string.h>

#if defined(_WIN32) && defined(_M_X64)

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <Windows.h>

#elif !defined(_WIN32)

// Defined in unwind.h which may not be easily discoverable on various platforms
extern "C" void __register_frame(const void*);
extern "C" void __deregister_frame(const void*);

#endif

#if defined(__APPLE__)
// On Mac, each FDE inside eh_frame section has to be handled separately
static void visitFdeEntries(char* pos, void (*cb)(const void*))
{
    for (;;)
    {
        unsigned partLength;
        memcpy(&partLength, pos, sizeof(partLength));

        if (partLength == 0) // Zero-length section signals completion
            break;

        unsigned partId;
        memcpy(&partId, pos + 4, sizeof(partId));

        if (partId != 0) // Skip CIE part
            cb(pos);     // CIE is found using an offset in FDE

        pos += partLength + 4;
    }
}
#endif

namespace Luau
{
namespace CodeGen
{

void* createBlockUnwindInfo(void* context, uint8_t* block, size_t blockSize, size_t& beginOffset)
{
    UnwindBuilder* unwind = (UnwindBuilder*)context;

    // All unwinding related data is placed together at the start of the block
    size_t unwindSize = unwind->getSize();
    unwindSize = (unwindSize + (kCodeAlignment - 1)) & ~(kCodeAlignment - 1); // Match code allocator alignment
    LUAU_ASSERT(blockSize >= unwindSize);

    char* unwindData = (char*)block;
    unwind->finalize(unwindData, unwindSize, block, blockSize);

#if defined(_WIN32) && defined(_M_X64)
    if (!RtlAddFunctionTable((RUNTIME_FUNCTION*)block, uint32_t(unwind->getFunctionCount()), uintptr_t(block)))
    {
        LUAU_ASSERT(!"failed to allocate function table");
        return nullptr;
    }
#elif defined(__APPLE__)
    visitFdeEntries(unwindData, __register_frame);
#elif !defined(_WIN32)
    __register_frame(unwindData);
#endif

    beginOffset = unwindSize + unwind->getBeginOffset();
    return block;
}

void destroyBlockUnwindInfo(void* context, void* unwindData)
{
#if defined(_WIN32) && defined(_M_X64)
    if (!RtlDeleteFunctionTable((RUNTIME_FUNCTION*)unwindData))
        LUAU_ASSERT(!"failed to deallocate function table");
#elif defined(__APPLE__)
    visitFdeEntries((char*)unwindData, __deregister_frame);
#elif !defined(_WIN32)
    __deregister_frame(unwindData);
#endif
}

} // namespace CodeGen
} // namespace Luau
