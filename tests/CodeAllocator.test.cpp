// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AssemblyBuilderX64.h"
#include "Luau/AssemblyBuilderA64.h"
#include "Luau/CodeAllocator.h"
#include "Luau/CodeBlockUnwind.h"
#include "Luau/CodeGen.h"
#include "Luau/UnwindBuilder.h"
#include "Luau/UnwindBuilderDwarf2.h"
#include "Luau/UnwindBuilderWin.h"

#include "doctest.h"

#include <memory>
#include <stdexcept>

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
    CHECK(sizeNativeData == kCodeAlignment + 128);
    CHECK(nativeEntry != nullptr);
    CHECK(nativeEntry == nativeData + kCodeAlignment);
}

TEST_CASE("CodeAllocationCallbacks")
{
    struct AllocationData
    {
        size_t bytesAllocated = 0;
        size_t bytesFreed = 0;
    };

    AllocationData allocationData{};

    const auto allocationCallback = [](void* context, void* oldPointer, size_t oldSize, void* newPointer, size_t newSize)
    {
        AllocationData& allocationData = *static_cast<AllocationData*>(context);
        if (oldPointer != nullptr)
        {
            CHECK(oldSize != 0);

            allocationData.bytesFreed += oldSize;
        }

        if (newPointer != nullptr)
        {
            CHECK(newSize != 0);

            allocationData.bytesAllocated += newSize;
        }
    };

    const size_t blockSize = 1024 * 1024;
    const size_t maxTotalSize = 1024 * 1024;

    {
        CodeAllocator allocator(blockSize, maxTotalSize, allocationCallback, &allocationData);

        uint8_t* nativeData = nullptr;
        size_t sizeNativeData = 0;
        uint8_t* nativeEntry = nullptr;

        std::vector<uint8_t> code;
        code.resize(128);

        REQUIRE(allocator.allocate(nullptr, 0, code.data(), code.size(), nativeData, sizeNativeData, nativeEntry));
        CHECK(allocationData.bytesAllocated == blockSize);
        CHECK(allocationData.bytesFreed == 0);
    }

    CHECK(allocationData.bytesAllocated == blockSize);
    CHECK(allocationData.bytesFreed == blockSize);
}

TEST_CASE("CodeAllocationFailure")
{
    size_t blockSize = 3000;
    size_t maxTotalSize = 7000;
    CodeAllocator allocator(blockSize, maxTotalSize);

    uint8_t* nativeData;
    size_t sizeNativeData;
    uint8_t* nativeEntry;

    std::vector<uint8_t> code;
    code.resize(4000);

    // allocation has to fit in a block
    REQUIRE(!allocator.allocate(nullptr, 0, code.data(), code.size(), nativeData, sizeNativeData, nativeEntry));

    // each allocation exhausts a block, so third allocation fails
    code.resize(2000);
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
        allocator.createBlockUnwindInfo = [](void* context, uint8_t* block, size_t blockSize, size_t& beginOffset) -> void*
        {
            Info& info = *(Info*)context;

            CHECK(info.unwind.size() == 8);
            memcpy(block, info.unwind.data(), info.unwind.size());
            beginOffset = 8;

            info.block = block;

            return new int(7);
        };
        allocator.destroyBlockUnwindInfo = [](void* context, void* unwindData)
        {
            Info& info = *(Info*)context;

            info.destroyCalled = true;

            CHECK(*(int*)unwindData == 7);
            delete (int*)unwindData;
        };

        REQUIRE(allocator.allocate(data.data(), data.size(), code.data(), code.size(), nativeData, sizeNativeData, nativeEntry));
        CHECK(nativeData != nullptr);
        CHECK(sizeNativeData == kCodeAlignment + 128);
        CHECK(nativeEntry != nullptr);
        CHECK(nativeEntry == nativeData + kCodeAlignment);
        CHECK(nativeData == info.block + kCodeAlignment);
    }

    CHECK(info.destroyCalled);
}

#if !defined(LUAU_BIG_ENDIAN)
TEST_CASE("WindowsUnwindCodesX64")
{
    using namespace X64;

    UnwindBuilderWin unwind;

    unwind.startInfo(UnwindBuilder::X64);
    unwind.startFunction();
    unwind.prologueX64(/* prologueSize= */ 23, /* stackSize= */ 72, /* setupFrame= */ true, {rdi, rsi, rbx, r12, r13, r14, r15}, {});
    unwind.finishFunction(0x11223344, 0x55443322);
    unwind.finishInfo();

    std::vector<char> data;
    data.resize(unwind.getUnwindInfoSize());
    unwind.finalize(data.data(), 0, nullptr, 0);

    std::vector<uint8_t> expected{0x44, 0x33, 0x22, 0x11, 0x22, 0x33, 0x44, 0x55, 0x0c, 0x00, 0x00, 0x00, 0x01, 0x17, 0x0a, 0x05, 0x17, 0x82,
                                  0x13, 0xf0, 0x11, 0xe0, 0x0f, 0xd0, 0x0d, 0xc0, 0x0b, 0x30, 0x09, 0x60, 0x07, 0x70, 0x05, 0x03, 0x02, 0x50};

    REQUIRE(data.size() == expected.size());
    CHECK(memcmp(data.data(), expected.data(), expected.size()) == 0);
}
#endif

TEST_CASE("Dwarf2UnwindCodesX64")
{
    using namespace X64;

    UnwindBuilderDwarf2 unwind;

    unwind.startInfo(UnwindBuilder::X64);
    unwind.startFunction();
    unwind.prologueX64(/* prologueSize= */ 23, /* stackSize= */ 72, /* setupFrame= */ true, {rdi, rsi, rbx, r12, r13, r14, r15}, {});
    unwind.finishFunction(0, 0);
    unwind.finishInfo();

    std::vector<char> data;
    data.resize(unwind.getUnwindInfoSize());
    unwind.finalize(data.data(), 0, nullptr, 0);

    std::vector<uint8_t> expected{0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x78, 0x10, 0x0c, 0x07, 0x08, 0x90, 0x01,
                                  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4c, 0x00, 0x00, 0x00, 0x1c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x02, 0x0e, 0x10, 0x86, 0x02,
                                  0x02, 0x03, 0x02, 0x02, 0x0e, 0x18, 0x85, 0x03, 0x02, 0x02, 0x0e, 0x20, 0x84, 0x04, 0x02, 0x02, 0x0e, 0x28,
                                  0x83, 0x05, 0x02, 0x02, 0x0e, 0x30, 0x8c, 0x06, 0x02, 0x02, 0x0e, 0x38, 0x8d, 0x07, 0x02, 0x02, 0x0e, 0x40,
                                  0x8e, 0x08, 0x02, 0x02, 0x0e, 0x48, 0x8f, 0x09, 0x02, 0x04, 0x0e, 0x90, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00};

    REQUIRE(data.size() == expected.size());
    CHECK(memcmp(data.data(), expected.data(), expected.size()) == 0);
}

TEST_CASE("Dwarf2UnwindCodesA64")
{
    using namespace A64;

    UnwindBuilderDwarf2 unwind;

    unwind.startInfo(UnwindBuilder::A64);
    unwind.startFunction();
    unwind.prologueA64(/* prologueSize= */ 28, /* stackSize= */ 64, {x29, x30, x19, x20, x21, x22, x23, x24});
    unwind.finishFunction(0, 32);
    unwind.finishInfo();

    std::vector<char> data;
    data.resize(unwind.getUnwindInfoSize());
    unwind.finalize(data.data(), 0, nullptr, 0);

    std::vector<uint8_t> expected{0x0c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x78, 0x1e, 0x0c, 0x1f, 0x00, 0x2c,
                                  0x00, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x00,
                                  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x04, 0x0e, 0x40, 0x02, 0x18, 0x9d, 0x08, 0x9e, 0x07, 0x93,
                                  0x06, 0x94, 0x05, 0x95, 0x04, 0x96, 0x03, 0x97, 0x02, 0x98, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

    REQUIRE(data.size() == expected.size());
    CHECK(memcmp(data.data(), expected.data(), expected.size()) == 0);
}

#if defined(CODEGEN_TARGET_X64)

#if defined(_WIN32)
// Windows x64 ABI
constexpr X64::RegisterX64 rArg1 = X64::rcx;
constexpr X64::RegisterX64 rArg2 = X64::rdx;
constexpr X64::RegisterX64 rArg3 = X64::r8;
#else
// System V AMD64 ABI
constexpr X64::RegisterX64 rArg1 = X64::rdi;
constexpr X64::RegisterX64 rArg2 = X64::rsi;
constexpr X64::RegisterX64 rArg3 = X64::rdx;
#endif

constexpr X64::RegisterX64 rNonVol1 = X64::r12;
constexpr X64::RegisterX64 rNonVol2 = X64::rbx;
constexpr X64::RegisterX64 rNonVol3 = X64::r13;
constexpr X64::RegisterX64 rNonVol4 = X64::r14;

TEST_CASE("GeneratedCodeExecutionX64")
{
    if (!Luau::CodeGen::isSupported())
        return;

    using namespace X64;

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

static void throwing(int64_t arg)
{
    CHECK(arg == 25);

    throw std::runtime_error("testing");
}

static void nonthrowing(int64_t arg)
{
    CHECK(arg == 25);
}

TEST_CASE("GeneratedCodeExecutionWithThrowX64")
{
    if (!Luau::CodeGen::isSupported())
        return;

    using namespace X64;

    AssemblyBuilderX64 build(/* logText= */ false);

#if defined(_WIN32)
    std::unique_ptr<UnwindBuilder> unwind = std::make_unique<UnwindBuilderWin>();
#else
    std::unique_ptr<UnwindBuilder> unwind = std::make_unique<UnwindBuilderDwarf2>();
#endif

    unwind->startInfo(UnwindBuilder::X64);

    Label functionBegin = build.setLabel();
    unwind->startFunction();

    // Prologue
    build.push(rbp);
    build.mov(rbp, rsp);
    build.push(rNonVol1);
    build.push(rNonVol2);

    int stackSize = 32;
    int localsSize = 16;

    build.sub(rsp, stackSize + localsSize);

    uint32_t prologueSize = build.setLabel().location;

    unwind->prologueX64(prologueSize, stackSize + localsSize, /* setupFrame= */ true, {rNonVol1, rNonVol2}, {});

    // Body
    build.mov(rNonVol1, rArg1);
    build.mov(rNonVol2, rArg2);

    build.add(rNonVol1, 15);
    build.mov(rArg1, rNonVol1);
    build.call(rNonVol2);

    // Epilogue
    build.add(rsp, stackSize + localsSize);
    build.pop(rNonVol2);
    build.pop(rNonVol1);
    build.pop(rbp);
    build.ret();

    unwind->finishFunction(build.getLabelOffset(functionBegin), ~0u);

    build.finalize();

    unwind->finishInfo();

    size_t blockSize = 1024 * 1024;
    size_t maxTotalSize = 1024 * 1024;
    CodeAllocator allocator(blockSize, maxTotalSize);

    allocator.context = unwind.get();
    allocator.createBlockUnwindInfo = createBlockUnwindInfo;
    allocator.destroyBlockUnwindInfo = destroyBlockUnwindInfo;

    uint8_t* nativeData;
    size_t sizeNativeData;
    uint8_t* nativeEntry;
    REQUIRE(allocator.allocate(build.data.data(), build.data.size(), build.code.data(), build.code.size(), nativeData, sizeNativeData, nativeEntry));
    REQUIRE(nativeEntry);

    using FunctionType = int64_t(int64_t, void (*)(int64_t));
    FunctionType* f = (FunctionType*)nativeEntry;

    f(10, nonthrowing);

    // To simplify debugging, CHECK_THROWS_WITH_AS is not used here
    try
    {
        f(10, throwing);
    }
    catch (const std::runtime_error& error)
    {
        CHECK(strcmp(error.what(), "testing") == 0);
    }
}

static void obscureThrowCase(int64_t (*f)(int64_t, void (*)(int64_t)))
{
    // To simplify debugging, CHECK_THROWS_WITH_AS is not used here
    try
    {
        f(10, throwing);
    }
    catch (const std::runtime_error& error)
    {
        CHECK(strcmp(error.what(), "testing") == 0);
    }
}

TEST_CASE("GeneratedCodeExecutionWithThrowX64Simd")
{
    // This test requires AVX
    if (!Luau::CodeGen::isSupported())
        return;

    using namespace X64;

    AssemblyBuilderX64 build(/* logText= */ false);

#if defined(_WIN32)
    std::unique_ptr<UnwindBuilder> unwind = std::make_unique<UnwindBuilderWin>();
#else
    std::unique_ptr<UnwindBuilder> unwind = std::make_unique<UnwindBuilderDwarf2>();
#endif

    unwind->startInfo(UnwindBuilder::X64);

    Label functionBegin = build.setLabel();
    unwind->startFunction();

    int stackSize = 32 + 64;
    int localsSize = 16;

    // Prologue
    build.push(rNonVol1);
    build.push(rNonVol2);
    build.push(rbp);
    build.sub(rsp, stackSize + localsSize);

    if (build.abi == ABIX64::Windows)
    {
        build.vmovaps(xmmword[rsp + ((stackSize + localsSize) - 0x40)], xmm6);
        build.vmovaps(xmmword[rsp + ((stackSize + localsSize) - 0x30)], xmm7);
        build.vmovaps(xmmword[rsp + ((stackSize + localsSize) - 0x20)], xmm8);
        build.vmovaps(xmmword[rsp + ((stackSize + localsSize) - 0x10)], xmm9);
    }

    uint32_t prologueSize = build.setLabel().location;

    if (build.abi == ABIX64::Windows)
        unwind->prologueX64(prologueSize, stackSize + localsSize, /* setupFrame= */ false, {rNonVol1, rNonVol2, rbp}, {xmm6, xmm7, xmm8, xmm9});
    else
        unwind->prologueX64(prologueSize, stackSize + localsSize, /* setupFrame= */ false, {rNonVol1, rNonVol2, rbp}, {});

    // Body
    build.vxorpd(xmm0, xmm0, xmm0);
    build.vmovsd(xmm6, xmm0, xmm0);
    build.vmovsd(xmm7, xmm0, xmm0);
    build.vmovsd(xmm8, xmm0, xmm0);
    build.vmovsd(xmm9, xmm0, xmm0);

    build.mov(rNonVol1, rArg1);
    build.mov(rNonVol2, rArg2);

    build.add(rNonVol1, 15);
    build.mov(rArg1, rNonVol1);
    build.call(rNonVol2);

    // Epilogue
    if (build.abi == ABIX64::Windows)
    {
        build.vmovaps(xmm6, xmmword[rsp + ((stackSize + localsSize) - 0x40)]);
        build.vmovaps(xmm7, xmmword[rsp + ((stackSize + localsSize) - 0x30)]);
        build.vmovaps(xmm8, xmmword[rsp + ((stackSize + localsSize) - 0x20)]);
        build.vmovaps(xmm9, xmmword[rsp + ((stackSize + localsSize) - 0x10)]);
    }

    build.add(rsp, stackSize + localsSize);
    build.pop(rbp);
    build.pop(rNonVol2);
    build.pop(rNonVol1);
    build.ret();

    unwind->finishFunction(build.getLabelOffset(functionBegin), ~0u);

    build.finalize();

    unwind->finishInfo();

    size_t blockSize = 1024 * 1024;
    size_t maxTotalSize = 1024 * 1024;
    CodeAllocator allocator(blockSize, maxTotalSize);

    allocator.context = unwind.get();
    allocator.createBlockUnwindInfo = createBlockUnwindInfo;
    allocator.destroyBlockUnwindInfo = destroyBlockUnwindInfo;

    uint8_t* nativeData;
    size_t sizeNativeData;
    uint8_t* nativeEntry;
    REQUIRE(allocator.allocate(build.data.data(), build.data.size(), build.code.data(), build.code.size(), nativeData, sizeNativeData, nativeEntry));
    REQUIRE(nativeEntry);

    using FunctionType = int64_t(int64_t, void (*)(int64_t));
    FunctionType* f = (FunctionType*)nativeEntry;

    f(10, nonthrowing);

    obscureThrowCase(f);
}

TEST_CASE("GeneratedCodeExecutionMultipleFunctionsWithThrowX64")
{
    if (!Luau::CodeGen::isSupported())
        return;

    using namespace X64;

    AssemblyBuilderX64 build(/* logText= */ false);

#if defined(_WIN32)
    std::unique_ptr<UnwindBuilder> unwind = std::make_unique<UnwindBuilderWin>();
#else
    std::unique_ptr<UnwindBuilder> unwind = std::make_unique<UnwindBuilderDwarf2>();
#endif

    unwind->startInfo(UnwindBuilder::X64);

    Label start1;
    Label start2;

    // First function
    {
        build.setLabel(start1);
        unwind->startFunction();

        // Prologue
        build.push(rbp);
        build.mov(rbp, rsp);
        build.push(rNonVol1);
        build.push(rNonVol2);

        int stackSize = 32;
        int localsSize = 16;

        build.sub(rsp, stackSize + localsSize);

        uint32_t prologueSize = build.setLabel().location - start1.location;

        unwind->prologueX64(prologueSize, stackSize + localsSize, /* setupFrame= */ true, {rNonVol1, rNonVol2}, {});

        // Body
        build.mov(rNonVol1, rArg1);
        build.mov(rNonVol2, rArg2);

        build.add(rNonVol1, 15);
        build.mov(rArg1, rNonVol1);
        build.call(rNonVol2);

        // Epilogue
        build.add(rsp, stackSize + localsSize);
        build.pop(rNonVol2);
        build.pop(rNonVol1);
        build.pop(rbp);
        build.ret();

        Label end1 = build.setLabel();
        unwind->finishFunction(build.getLabelOffset(start1), build.getLabelOffset(end1));
    }

    // Second function with different layout and no frame
    {
        build.setLabel(start2);
        unwind->startFunction();

        // Prologue
        build.push(rNonVol1);
        build.push(rNonVol2);
        build.push(rNonVol3);
        build.push(rNonVol4);

        int stackSize = 32;
        int localsSize = 24;

        build.sub(rsp, stackSize + localsSize);

        uint32_t prologueSize = build.setLabel().location - start2.location;

        unwind->prologueX64(prologueSize, stackSize + localsSize, /* setupFrame= */ false, {rNonVol1, rNonVol2, rNonVol3, rNonVol4}, {});

        // Body
        build.mov(rNonVol3, rArg1);
        build.mov(rNonVol4, rArg2);

        build.add(rNonVol3, 15);
        build.mov(rArg1, rNonVol3);
        build.call(rNonVol4);

        // Epilogue
        build.add(rsp, stackSize + localsSize);
        build.pop(rNonVol4);
        build.pop(rNonVol3);
        build.pop(rNonVol2);
        build.pop(rNonVol1);
        build.ret();

        unwind->finishFunction(build.getLabelOffset(start2), ~0u);
    }

    build.finalize();

    unwind->finishInfo();

    size_t blockSize = 1024 * 1024;
    size_t maxTotalSize = 1024 * 1024;
    CodeAllocator allocator(blockSize, maxTotalSize);

    allocator.context = unwind.get();
    allocator.createBlockUnwindInfo = createBlockUnwindInfo;
    allocator.destroyBlockUnwindInfo = destroyBlockUnwindInfo;

    uint8_t* nativeData;
    size_t sizeNativeData;
    uint8_t* nativeEntry;
    REQUIRE(allocator.allocate(build.data.data(), build.data.size(), build.code.data(), build.code.size(), nativeData, sizeNativeData, nativeEntry));
    REQUIRE(nativeEntry);

    using FunctionType = int64_t(int64_t, void (*)(int64_t));
    FunctionType* f1 = (FunctionType*)(nativeEntry + start1.location);
    FunctionType* f2 = (FunctionType*)(nativeEntry + start2.location);

    // To simplify debugging, CHECK_THROWS_WITH_AS is not used here
    try
    {
        f1(10, throwing);
    }
    catch (const std::runtime_error& error)
    {
        CHECK(strcmp(error.what(), "testing") == 0);
    }

    try
    {
        f2(10, throwing);
    }
    catch (const std::runtime_error& error)
    {
        CHECK(strcmp(error.what(), "testing") == 0);
    }
}

TEST_CASE("GeneratedCodeExecutionWithThrowOutsideTheGateX64")
{
    if (!Luau::CodeGen::isSupported())
        return;

    using namespace X64;

    AssemblyBuilderX64 build(/* logText= */ false);

#if defined(_WIN32)
    std::unique_ptr<UnwindBuilder> unwind = std::make_unique<UnwindBuilderWin>();
#else
    std::unique_ptr<UnwindBuilder> unwind = std::make_unique<UnwindBuilderDwarf2>();
#endif

    unwind->startInfo(UnwindBuilder::X64);

    Label functionBegin = build.setLabel();
    unwind->startFunction();

    // Prologue (some of these registers don't have to be saved, but we want to have a big prologue)
    build.push(rbp);
    build.mov(rbp, rsp);
    build.push(r10);
    build.push(r11);
    build.push(r12);
    build.push(r13);
    build.push(r14);
    build.push(r15);

    int stackSize = 64;
    int localsSize = 16;

    build.sub(rsp, stackSize + localsSize);

    uint32_t prologueSize = build.setLabel().location;

    unwind->prologueX64(prologueSize, stackSize + localsSize, /* setupFrame= */ true, {r10, r11, r12, r13, r14, r15}, {});

    // Body
    build.mov(rax, rArg1);
    build.mov(rArg1, 25);
    build.jmp(rax);

    Label returnOffset = build.setLabel();

    // Epilogue
    build.add(rsp, stackSize + localsSize);
    build.pop(r15);
    build.pop(r14);
    build.pop(r13);
    build.pop(r12);
    build.pop(r11);
    build.pop(r10);
    build.pop(rbp);
    build.ret();

    unwind->finishFunction(build.getLabelOffset(functionBegin), ~0u);

    build.finalize();

    unwind->finishInfo();

    size_t blockSize = 4096; // Force allocate to create a new block each time
    size_t maxTotalSize = 1024 * 1024;
    CodeAllocator allocator(blockSize, maxTotalSize);

    allocator.context = unwind.get();
    allocator.createBlockUnwindInfo = createBlockUnwindInfo;
    allocator.destroyBlockUnwindInfo = destroyBlockUnwindInfo;

    uint8_t* nativeData1;
    size_t sizeNativeData1;
    uint8_t* nativeEntry1;
    REQUIRE(
        allocator.allocate(build.data.data(), build.data.size(), build.code.data(), build.code.size(), nativeData1, sizeNativeData1, nativeEntry1)
    );
    REQUIRE(nativeEntry1);

    // Now we set the offset at the begining so that functions in new blocks will not overlay the locations
    // specified by the unwind information of the entry function
    unwind->setBeginOffset(prologueSize);

    using FunctionType = int64_t(void*, void (*)(int64_t), void*);
    FunctionType* f = (FunctionType*)nativeEntry1;

    uint8_t* nativeExit = nativeEntry1 + returnOffset.location;

    AssemblyBuilderX64 build2(/* logText= */ false);

    build2.mov(r12, rArg3);
    build2.call(rArg2);
    build2.jmp(r12);

    build2.finalize();

    uint8_t* nativeData2;
    size_t sizeNativeData2;
    uint8_t* nativeEntry2;
    REQUIRE(
        allocator.allocate(build2.data.data(), build2.data.size(), build2.code.data(), build2.code.size(), nativeData2, sizeNativeData2, nativeEntry2)
    );
    REQUIRE(nativeEntry2);

    // To simplify debugging, CHECK_THROWS_WITH_AS is not used here
    try
    {
        f(nativeEntry2, throwing, nativeExit);
    }
    catch (const std::runtime_error& error)
    {
        CHECK(strcmp(error.what(), "testing") == 0);
    }

    REQUIRE(nativeEntry2);
}

#endif

#if defined(CODEGEN_TARGET_A64)

TEST_CASE("GeneratedCodeExecutionA64")
{
    using namespace A64;

    AssemblyBuilderA64 build(/* logText= */ false);

    Label skip;
    build.cbz(x1, skip);
    build.ldrsw(x1, x1);
    build.cbnz(x1, skip);
    build.mov(x1, 0); // doesn't execute due to cbnz above
    build.setLabel(skip);

    uint8_t one = 1;
    build.adr(x2, &one, 1);
    build.ldrb(w2, x2);
    build.sub(x1, x1, x2);

    build.add(x1, x1, 2);
    build.add(x0, x0, x1, /* LSL */ 1);

    build.ret();

    build.finalize();

    size_t blockSize = 1024 * 1024;
    size_t maxTotalSize = 1024 * 1024;
    CodeAllocator allocator(blockSize, maxTotalSize);

    uint8_t* nativeData;
    size_t sizeNativeData;
    uint8_t* nativeEntry;
    REQUIRE(allocator.allocate(
        build.data.data(),
        build.data.size(),
        reinterpret_cast<uint8_t*>(build.code.data()),
        build.code.size() * 4,
        nativeData,
        sizeNativeData,
        nativeEntry
    ));
    REQUIRE(nativeEntry);

    using FunctionType = int64_t(int64_t, int*);
    FunctionType* f = (FunctionType*)nativeEntry;
    int input = 10;
    int64_t result = f(20, &input);
    CHECK(result == 42);
}

static void throwing(int64_t arg)
{
    CHECK(arg == 25);

    throw std::runtime_error("testing");
}

TEST_CASE("GeneratedCodeExecutionWithThrowA64")
{
    // macOS 12 doesn't support JIT frames without pointer authentication
    if (!isUnwindSupported())
        return;

    using namespace A64;

    AssemblyBuilderA64 build(/* logText= */ false);

    std::unique_ptr<UnwindBuilder> unwind = std::make_unique<UnwindBuilderDwarf2>();

    unwind->startInfo(UnwindBuilder::A64);

    build.sub(sp, sp, 32);
    build.stp(x29, x30, mem(sp));
    build.str(x28, mem(sp, 16));
    build.mov(x29, sp);

    Label prologueEnd = build.setLabel();

    build.add(x0, x0, 15);
    build.blr(x1);

    build.ldr(x28, mem(sp, 16));
    build.ldp(x29, x30, mem(sp));
    build.add(sp, sp, 32);

    build.ret();

    Label functionEnd = build.setLabel();

    unwind->startFunction();
    unwind->prologueA64(build.getLabelOffset(prologueEnd), 32, {x29, x30, x28});
    unwind->finishFunction(0, build.getLabelOffset(functionEnd));

    build.finalize();

    unwind->finishInfo();

    size_t blockSize = 1024 * 1024;
    size_t maxTotalSize = 1024 * 1024;
    CodeAllocator allocator(blockSize, maxTotalSize);

    allocator.context = unwind.get();
    allocator.createBlockUnwindInfo = createBlockUnwindInfo;
    allocator.destroyBlockUnwindInfo = destroyBlockUnwindInfo;

    uint8_t* nativeData;
    size_t sizeNativeData;
    uint8_t* nativeEntry;
    REQUIRE(allocator.allocate(
        build.data.data(),
        build.data.size(),
        reinterpret_cast<uint8_t*>(build.code.data()),
        build.code.size() * 4,
        nativeData,
        sizeNativeData,
        nativeEntry
    ));
    REQUIRE(nativeEntry);

    using FunctionType = int64_t(int64_t, void (*)(int64_t));
    FunctionType* f = (FunctionType*)nativeEntry;

    // To simplify debugging, CHECK_THROWS_WITH_AS is not used here
    try
    {
        f(10, throwing);
    }
    catch (const std::runtime_error& error)
    {
        CHECK(strcmp(error.what(), "testing") == 0);
    }
}

#endif

TEST_SUITE_END();
