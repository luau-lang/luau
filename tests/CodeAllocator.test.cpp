// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AssemblyBuilderX64.h"
#include "Luau/CodeAllocator.h"

#include "doctest.h"

#include <string.h>

using namespace Luau::CodeGen;

TEST_SUITE_BEGIN("CodeAllocation");

TEST_CASE("CodeAllocation")
{
    size_t blockSize = 1024 * 1024;
    size_t maxTotalSize = 1024 * 1024;
    CodeAllocator allocator(blockSize, maxTotalSize);

    uint8_t* nativeData = nullptr;
    size_t sizeNativeData = 0;
    uint8_t* nativeEntry = nullptr;

    std::vector<uint8_t> code;
    code.resize(128);

    REQUIRE(allocator.allocate(nullptr, 0, code.data(), code.size(), nativeData, sizeNativeData, nativeEntry));
    CHECK(nativeData != nullptr);
    CHECK(sizeNativeData == 128);
    CHECK(nativeEntry != nullptr);
    CHECK(nativeEntry == nativeData);

    std::vector<uint8_t> data;
    data.resize(8);

    REQUIRE(allocator.allocate(data.data(), data.size(), code.data(), code.size(), nativeData, sizeNativeData, nativeEntry));
    CHECK(nativeData != nullptr);
    CHECK(sizeNativeData == 16 + 128);
    CHECK(nativeEntry != nullptr);
    CHECK(nativeEntry == nativeData + 16);
}

TEST_CASE("CodeAllocationFailure")
{
    size_t blockSize = 4096;
    size_t maxTotalSize = 8192;
    CodeAllocator allocator(blockSize, maxTotalSize);

    uint8_t* nativeData;
    size_t sizeNativeData;
    uint8_t* nativeEntry;

    std::vector<uint8_t> code;
    code.resize(6000);

    REQUIRE(!allocator.allocate(nullptr, 0, code.data(), code.size(), nativeData, sizeNativeData, nativeEntry));

    code.resize(3000);
    REQUIRE(allocator.allocate(nullptr, 0, code.data(), code.size(), nativeData, sizeNativeData, nativeEntry));
    REQUIRE(allocator.allocate(nullptr, 0, code.data(), code.size(), nativeData, sizeNativeData, nativeEntry));
    REQUIRE(!allocator.allocate(nullptr, 0, code.data(), code.size(), nativeData, sizeNativeData, nativeEntry));
}

TEST_CASE("CodeAllocationWithUnwindCallbacks")
{
    struct Info
    {
        std::vector<uint8_t> unwind;
        uint8_t* block = nullptr;
        bool destroyCalled = false;
    };
    Info info;
    info.unwind.resize(8);

    {
        size_t blockSize = 1024 * 1024;
        size_t maxTotalSize = 1024 * 1024;
        CodeAllocator allocator(blockSize, maxTotalSize);

        uint8_t* nativeData = nullptr;
        size_t sizeNativeData = 0;
        uint8_t* nativeEntry = nullptr;

        std::vector<uint8_t> code;
        code.resize(128);

        std::vector<uint8_t> data;
        data.resize(8);

        allocator.context = &info;
        allocator.createBlockUnwindInfo = [](void* context, uint8_t* block, size_t blockSize, size_t& unwindDataSizeInBlock) -> void* {
            Info& info = *(Info*)context;

            CHECK(info.unwind.size() == 8);
            memcpy(block, info.unwind.data(), info.unwind.size());
            unwindDataSizeInBlock = 8;

            info.block = block;

            return new int(7);
        };
        allocator.destroyBlockUnwindInfo = [](void* context, void* unwindData) {
            Info& info = *(Info*)context;

            info.destroyCalled = true;

            CHECK(*(int*)unwindData == 7);
            delete (int*)unwindData;
        };

        REQUIRE(allocator.allocate(data.data(), data.size(), code.data(), code.size(), nativeData, sizeNativeData, nativeEntry));
        CHECK(nativeData != nullptr);
        CHECK(sizeNativeData == 16 + 128);
        CHECK(nativeEntry != nullptr);
        CHECK(nativeEntry == nativeData + 16);
        CHECK(nativeData == info.block + 16);
    }

    CHECK(info.destroyCalled);
}

#if defined(__x86_64__) || defined(_M_X64)
TEST_CASE("GeneratedCodeExecution")
{
#if defined(_WIN32)
    // Windows x64 ABI
    constexpr RegisterX64 rArg1 = rcx;
    constexpr RegisterX64 rArg2 = rdx;
#else
    // System V AMD64 ABI
    constexpr RegisterX64 rArg1 = rdi;
    constexpr RegisterX64 rArg2 = rsi;
#endif

    AssemblyBuilderX64 build(/* logText= */ false);

    build.mov(rax, rArg1);
    build.add(rax, rArg2);
    build.imul(rax, rax, 7);
    build.ret();

    build.finalize();

    size_t blockSize = 1024 * 1024;
    size_t maxTotalSize = 1024 * 1024;
    CodeAllocator allocator(blockSize, maxTotalSize);

    uint8_t* nativeData;
    size_t sizeNativeData;
    uint8_t* nativeEntry;
    REQUIRE(allocator.allocate(build.data.data(), build.data.size(), build.code.data(), build.code.size(), nativeData, sizeNativeData, nativeEntry));
    REQUIRE(nativeEntry);

    using FunctionType = int64_t(int64_t, int64_t);
    FunctionType* f = (FunctionType*)nativeEntry;
    int64_t result = f(10, 20);
    CHECK(result == 210);
}
#endif

TEST_SUITE_END();
