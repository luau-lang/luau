// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/CodeBlockUnwind.h"

#include "Luau/CodeAllocator.h"
#include "Luau/UnwindBuilder.h"

#include <string.h>
#include <stdlib.h>

#if defined(_WIN32) && defined(_M_X64)

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>

#elif defined(__linux__) || defined(__APPLE__)

// Defined in unwind.h which may not be easily discoverable on various platforms
extern "C" void __register_frame(const void*);
extern "C" void __deregister_frame(const void*);

extern "C" void __unw_add_dynamic_fde() __attribute__((weak));
#endif

#if defined(__APPLE__) && defined(__aarch64__)
#include <sys/sysctl.h>
#include <mach-o/loader.h>
#include <dlfcn.h>

struct unw_dynamic_unwind_sections_t
{
    uintptr_t dso_base;
    uintptr_t dwarf_section;
    size_t dwarf_section_length;
    uintptr_t compact_unwind_section;
    size_t compact_unwind_section_length;
};

typedef int (*unw_add_find_dynamic_unwind_sections_t)(int (*)(uintptr_t addr, unw_dynamic_unwind_sections_t* info));
#endif

namespace Luau
{
namespace CodeGen
{

#if defined(__APPLE__) && defined(__aarch64__)
static int findDynamicUnwindSections(uintptr_t addr, unw_dynamic_unwind_sections_t* info)
{
    // Define a minimal mach header for JIT'd code.
    static const mach_header_64 kFakeMachHeader = {
        MH_MAGIC_64,
        CPU_TYPE_ARM64,
        CPU_SUBTYPE_ARM64_ALL,
        MH_DYLIB,
    };

    info->dso_base = (uintptr_t)&kFakeMachHeader;
    info->dwarf_section = 0;
    info->dwarf_section_length = 0;
    info->compact_unwind_section = 0;
    info->compact_unwind_section_length = 0;
    return 1;
}
#endif

#if defined(__linux__) || defined(__APPLE__)
static void visitFdeEntries(char* pos, void (*cb)(const void*))
{
    // When using glibc++ unwinder, we need to call __register_frame/__deregister_frame on the entire .eh_frame data
    // When using libc++ unwinder (libunwind), each FDE has to be handled separately
    // libc++ unwinder is the macOS unwinder, but on Linux the unwinder depends on the library the executable is linked with
    // __unw_add_dynamic_fde is specific to libc++ unwinder, as such we determine the library based on its existence
    if (__unw_add_dynamic_fde == nullptr)
        return cb(pos);

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
        LUAU_ASSERT(!"Failed to allocate function table");
        return nullptr;
    }
#elif defined(__linux__) || defined(__APPLE__)
    visitFdeEntries(unwindData, __register_frame);
#endif

#if defined(__APPLE__) && defined(__aarch64__)
    // Starting from macOS 14, we need to register unwind section callback to state that our ABI doesn't require pointer authentication
    // This might conflict with other JITs that do the same; unfortunately this is the best we can do for now.
    static unw_add_find_dynamic_unwind_sections_t unw_add_find_dynamic_unwind_sections =
        unw_add_find_dynamic_unwind_sections_t(dlsym(RTLD_DEFAULT, "__unw_add_find_dynamic_unwind_sections"));
    static int regonce = unw_add_find_dynamic_unwind_sections ? unw_add_find_dynamic_unwind_sections(findDynamicUnwindSections) : 0;
    LUAU_ASSERT(regonce == 0);
#endif

    beginOffset = unwindSize + unwind->getBeginOffset();
    return block;
}

void destroyBlockUnwindInfo(void* context, void* unwindData)
{
#if defined(_WIN32) && defined(_M_X64)
    if (!RtlDeleteFunctionTable((RUNTIME_FUNCTION*)unwindData))
        LUAU_ASSERT(!"Failed to deallocate function table");
#elif defined(__linux__) || defined(__APPLE__)
    visitFdeEntries((char*)unwindData, __deregister_frame);
#endif
}

bool isUnwindSupported()
{
#if defined(_WIN32) && defined(_M_X64)
    return true;
#elif defined(__APPLE__) && defined(__aarch64__)
    char ver[256];
    size_t verLength = sizeof(ver);
    // libunwind on macOS 12 and earlier (which maps to osrelease 21) assumes JIT frames use pointer authentication without a way to override that
    return sysctlbyname("kern.osrelease", ver, &verLength, NULL, 0) == 0 && atoi(ver) >= 22;
#elif defined(__linux__) || defined(__APPLE__)
    return true;
#else
    return false;
#endif
}

} // namespace CodeGen
} // namespace Luau
