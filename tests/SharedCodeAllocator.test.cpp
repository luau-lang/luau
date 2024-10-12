// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/SharedCodeAllocator.h"

#include "Luau/CodeAllocator.h"

#include "luacode.h"
#include "luacodegen.h"
#include "lualib.h"

#include "doctest.h"
#include "ScopedFlags.h"

// We explicitly test correctness of self-assignment for some types
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wself-assign-overloaded"
#endif

using namespace Luau::CodeGen;


constexpr size_t kBlockSize = 1024 * 1024;
constexpr size_t kMaxTotalSize = 1024 * 1024;

static const uint8_t fakeCode[1] = {0x00};

TEST_SUITE_BEGIN("SharedCodeAllocator");

TEST_CASE("NativeModuleRefRefcounting")
{
    if (!luau_codegen_supported())
        return;

    CodeAllocator codeAllocator{kBlockSize, kMaxTotalSize};
    SharedCodeAllocator allocator{&codeAllocator};

    REQUIRE(allocator.tryGetNativeModule(ModuleId{0x0a}).empty());

    NativeModuleRef modRefA = allocator.getOrInsertNativeModule(ModuleId{0x0a}, {}, nullptr, 0, fakeCode, std::size(fakeCode)).first;
    REQUIRE(!modRefA.empty());

    // If we attempt to get the module again, we should get the same module back:
    REQUIRE(allocator.tryGetNativeModule(ModuleId{0x0a}).get() == modRefA.get());

    // If we try to insert another instance of the module, we should get the
    // existing module back:
    REQUIRE(allocator.getOrInsertNativeModule(ModuleId{0x0a}, {}, nullptr, 0, fakeCode, std::size(fakeCode)).first.get() == modRefA.get());

    // If we try to look up a different module, we should not get the existing
    // module back:
    REQUIRE(allocator.tryGetNativeModule(ModuleId{0x0b}).empty());

    // (Insert a second module to help with validation below)
    NativeModuleRef modRefB = allocator.getOrInsertNativeModule(ModuleId{0x0b}, {}, nullptr, 0, fakeCode, std::size(fakeCode)).first;
    REQUIRE(!modRefB.empty());
    REQUIRE(modRefB.get() != modRefA.get());

    // Verify NativeModuleRef refcounting:
    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef non-null copy construction:
    {
        NativeModuleRef modRef1{modRefA};
        REQUIRE(modRef1.get() == modRefA.get());
        REQUIRE(modRefA->getRefcount() == 2);
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef null copy construction:
    {
        NativeModuleRef modRef1{};
        NativeModuleRef modRef2{modRef1};
        REQUIRE(modRef1.empty());
        REQUIRE(modRef2.empty());
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef non-null move construction:
    {
        NativeModuleRef modRef1{modRefA};
        NativeModuleRef modRef2{std::move(modRef1)};
        REQUIRE(modRef1.empty());
        REQUIRE(modRef2.get() == modRefA.get());
        REQUIRE(modRefA->getRefcount() == 2);
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef null move construction:
    {
        NativeModuleRef modRef1{};
        NativeModuleRef modRef2{std::move(modRef1)};
        REQUIRE(modRef1.empty());
        REQUIRE(modRef2.empty());
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef null -> non-null copy assignment:
    {
        NativeModuleRef modRef1{};
        modRef1 = modRefA;
        REQUIRE(modRef1.get() == modRefA.get());
        REQUIRE(modRefA->getRefcount() == 2);
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef null -> null copy assignment:
    {
        NativeModuleRef modRef1{};
        NativeModuleRef modRef2{};
        modRef2 = modRef1;
        REQUIRE(modRef1.empty());
        REQUIRE(modRef2.empty());
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef self copy assignment:
    {
        NativeModuleRef modRef1{modRefA};
        modRef1 = modRef1;
        REQUIRE(modRef1.get() == modRefA.get());
        REQUIRE(modRefA->getRefcount() == 2);
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef non-null -> non-null copy assignment:
    {
        NativeModuleRef modRef1{modRefA};
        NativeModuleRef modRef2{modRefB};
        modRef2 = modRef1;
        REQUIRE(modRef1.get() == modRefA.get());
        REQUIRE(modRef2.get() == modRefA.get());
        REQUIRE(modRefA->getRefcount() == 3);
        REQUIRE(modRefB->getRefcount() == 1);
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef null -> non-null move assignment:
    {
        NativeModuleRef modRef1{modRefA};
        NativeModuleRef modRef2{};
        modRef2 = std::move(modRef1);
        REQUIRE(modRef1.empty());
        REQUIRE(modRef2.get() == modRefA.get());
        REQUIRE(modRefA->getRefcount() == 2);
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef null -> null move assignment:
    {
        NativeModuleRef modRef1{};
        NativeModuleRef modRef2{};
        modRef2 = std::move(modRef1);
        REQUIRE(modRef1.empty());
        REQUIRE(modRef2.empty());
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

#if defined(__linux__) && defined(__GNUC__)
#else
    // NativeModuleRef self move assignment:
    {
        NativeModuleRef modRef1{modRefA};
        modRef1 = std::move(modRef1);
        REQUIRE(modRef1.get() == modRefA.get());
        REQUIRE(modRefA->getRefcount() == 2);
    }

#endif

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef non-null -> non-null move assignment:
    {
        NativeModuleRef modRef1{modRefA};
        NativeModuleRef modRef2{modRefB};
        modRef2 = std::move(modRef1);
        REQUIRE(modRef1.empty());
        REQUIRE(modRef2.get() == modRefA.get());
        REQUIRE(modRefA->getRefcount() == 2);
        REQUIRE(modRefB->getRefcount() == 1);
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef null reset:
    {
        NativeModuleRef modRef1{};
        modRef1.reset();
        REQUIRE(modRef1.empty());
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef non-null reset:
    {
        NativeModuleRef modRef1{modRefA};
        modRef1.reset();
        REQUIRE(modRef1.empty());
        REQUIRE(modRefA->getRefcount() == 1);
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // NativeModuleRef swap:
    {
        NativeModuleRef modRef1{modRefA};
        NativeModuleRef modRef2{modRefB};
        modRef1.swap(modRef2);
        REQUIRE(modRef1.get() == modRefB.get());
        REQUIRE(modRef2.get() == modRefA.get());
        REQUIRE(modRefA->getRefcount() == 2);
        REQUIRE(modRefB->getRefcount() == 2);
    }

    REQUIRE(modRefA->getRefcount() == 1);
    REQUIRE(modRefB->getRefcount() == 1);

    // If we release the last reference to a module, it should destroy the
    // module:
    modRefA.reset();
    REQUIRE(allocator.tryGetNativeModule(ModuleId{0x0a}).empty());
}

TEST_CASE("NativeProtoRefcounting")
{
    if (!luau_codegen_supported())
        return;

    CodeAllocator codeAllocator{kBlockSize, kMaxTotalSize};
    SharedCodeAllocator allocator{&codeAllocator};

    std::vector<NativeProtoExecDataPtr> nativeProtos;
    nativeProtos.reserve(1);
    NativeProtoExecDataPtr nativeProto = createNativeProtoExecData(0);
    getNativeProtoExecDataHeader(nativeProto.get()).bytecodeId = 0x01;
    nativeProtos.push_back(std::move(nativeProto));

    NativeModuleRef modRefA =
        allocator.getOrInsertNativeModule(ModuleId{0x0a}, std::move(nativeProtos), nullptr, 0, fakeCode, std::size(fakeCode)).first;
    REQUIRE(!modRefA.empty());
    REQUIRE(modRefA->getRefcount() == 1);

    // Verify behavior of addRef:
    modRefA->addRef();
    REQUIRE(modRefA->getRefcount() == 2);

    // Verify behavior of addRefs:
    modRefA->addRefs(2);
    REQUIRE(modRefA->getRefcount() == 4);

    // Undo two of our addRef(s):
    modRefA->release();
    REQUIRE(modRefA->getRefcount() == 3);

    modRefA->release();
    REQUIRE(modRefA->getRefcount() == 2);

    // If we release our NativeModuleRef, the module should be kept alive by
    // the owning reference we acquired:
    modRefA.reset();

    modRefA = allocator.tryGetNativeModule(ModuleId{0x0a});
    REQUIRE(!modRefA.empty());
    REQUIRE(modRefA->getRefcount() == 2);

    // If the last "release" comes via releaseOwningPointerToInstructionOffsets,
    // the module should be successfully destroyed:
    const NativeModule* rawModA = modRefA.get();

    modRefA.reset();
    rawModA->release();
    REQUIRE(allocator.tryGetNativeModule(ModuleId{0x0a}).empty());
}

TEST_CASE("NativeProtoState")
{
    if (!luau_codegen_supported())
        return;

    CodeAllocator codeAllocator{kBlockSize, kMaxTotalSize};
    SharedCodeAllocator allocator{&codeAllocator};

    const std::vector<uint8_t> data(16);
    const std::vector<uint8_t> code(16);

    std::vector<NativeProtoExecDataPtr> nativeProtos;
    nativeProtos.reserve(2);

    {
        NativeProtoExecDataPtr nativeProto = createNativeProtoExecData(2);
        getNativeProtoExecDataHeader(nativeProto.get()).bytecodeId = 1;
        getNativeProtoExecDataHeader(nativeProto.get()).entryOffsetOrAddress = reinterpret_cast<const uint8_t*>(0x00);
        nativeProto[0] = 0;
        nativeProto[1] = 4;

        nativeProtos.push_back(std::move(nativeProto));
    }

    {
        NativeProtoExecDataPtr nativeProto = createNativeProtoExecData(2);
        getNativeProtoExecDataHeader(nativeProto.get()).bytecodeId = 3;
        getNativeProtoExecDataHeader(nativeProto.get()).entryOffsetOrAddress = reinterpret_cast<const uint8_t*>(0x08);
        nativeProto[0] = 8;
        nativeProto[1] = 12;

        nativeProtos.push_back(std::move(nativeProto));
    }

    NativeModuleRef modRefA =
        allocator.getOrInsertNativeModule(ModuleId{0x0a}, std::move(nativeProtos), data.data(), data.size(), code.data(), code.size()).first;
    REQUIRE(!modRefA.empty());
    REQUIRE(modRefA->getModuleBaseAddress() != nullptr);

    const uint32_t* proto1 = modRefA->tryGetNativeProto(1);
    REQUIRE(proto1 != nullptr);
    REQUIRE(getNativeProtoExecDataHeader(proto1).bytecodeId == 1);
    REQUIRE(getNativeProtoExecDataHeader(proto1).entryOffsetOrAddress == modRefA->getModuleBaseAddress() + 0x00);
    REQUIRE(proto1[0] == 0);
    REQUIRE(proto1[1] == 4);

    const uint32_t* proto3 = modRefA->tryGetNativeProto(3);
    REQUIRE(proto3 != nullptr);
    REQUIRE(getNativeProtoExecDataHeader(proto3).bytecodeId == 3);
    REQUIRE(getNativeProtoExecDataHeader(proto3).entryOffsetOrAddress == modRefA->getModuleBaseAddress() + 0x08);
    REQUIRE(proto3[0] == 8);
    REQUIRE(proto3[1] == 12);

    // Ensure that non-existent native protos cannot be found:
    REQUIRE(modRefA->tryGetNativeProto(0) == nullptr);
    REQUIRE(modRefA->tryGetNativeProto(2) == nullptr);
    REQUIRE(modRefA->tryGetNativeProto(4) == nullptr);
}

TEST_CASE("AnonymousModuleLifetime")
{
    if (!luau_codegen_supported())
        return;

    CodeAllocator codeAllocator{kBlockSize, kMaxTotalSize};
    SharedCodeAllocator allocator{&codeAllocator};

    const std::vector<uint8_t> data(8);
    const std::vector<uint8_t> code(8);

    std::vector<NativeProtoExecDataPtr> nativeProtos;
    nativeProtos.reserve(1);

    {
        NativeProtoExecDataPtr nativeProto = createNativeProtoExecData(2);
        getNativeProtoExecDataHeader(nativeProto.get()).bytecodeId = 1;
        getNativeProtoExecDataHeader(nativeProto.get()).entryOffsetOrAddress = reinterpret_cast<const uint8_t*>(0x00);
        nativeProto[0] = 0;
        nativeProto[1] = 4;

        nativeProtos.push_back(std::move(nativeProto));
    }

    NativeModuleRef modRef = allocator.insertAnonymousNativeModule(std::move(nativeProtos), data.data(), data.size(), code.data(), code.size());
    REQUIRE(!modRef.empty());
    REQUIRE(modRef->getModuleBaseAddress() != nullptr);
    REQUIRE(modRef->tryGetNativeProto(1) != nullptr);
    REQUIRE(modRef->getRefcount() == 1);

    const NativeModule* mod = modRef.get();

    // Acquire a reference (as if we are binding it to a Luau VM Proto):
    modRef->addRef();
    REQUIRE(mod->getRefcount() == 2);

    // Release our "owning" reference:
    modRef.reset();
    REQUIRE(mod->getRefcount() == 1);

    // Release our added reference (as if the Luau VM Proto is being GC'ed):
    mod->release();

    // When we return and the sharedCodeAllocator is destroyed it will verify
    // that there are no outstanding anonymous NativeModules.
}

TEST_CASE("SharedAllocation")
{
    if (!luau_codegen_supported())
        return;

    UniqueSharedCodeGenContext sharedCodeGenContext = createSharedCodeGenContext();

    std::unique_ptr<lua_State, void (*)(lua_State*)> L1{luaL_newstate(), lua_close};
    std::unique_ptr<lua_State, void (*)(lua_State*)> L2{luaL_newstate(), lua_close};

    create(L1.get(), sharedCodeGenContext.get());
    create(L2.get(), sharedCodeGenContext.get());

    std::string source = R"(
        function add(x, y) return x + y end
        function sub(x, y) return x - y end
    )";

    size_t bytecodeSize = 0;
    std::unique_ptr<char[], void (*)(void*)> bytecode{luau_compile(source.data(), source.size(), nullptr, &bytecodeSize), free};
    const int loadResult1 = luau_load(L1.get(), "=Functions", bytecode.get(), bytecodeSize, 0);
    const int loadResult2 = luau_load(L2.get(), "=Functions", bytecode.get(), bytecodeSize, 0);
    REQUIRE(loadResult1 == 0);
    REQUIRE(loadResult2 == 0);
    bytecode.reset();

    const ModuleId moduleId = {0x01};

    CompilationOptions options;
    options.flags = CodeGen_ColdFunctions;

    CompilationStats nativeStats1 = {};
    CompilationStats nativeStats2 = {};
    const CompilationResult codeGenResult1 = Luau::CodeGen::compile(moduleId, L1.get(), -1, options, &nativeStats1);
    const CompilationResult codeGenResult2 = Luau::CodeGen::compile(moduleId, L2.get(), -1, options, &nativeStats2);
    REQUIRE(codeGenResult1.result == CodeGenCompilationResult::Success);
    REQUIRE(codeGenResult2.result == CodeGenCompilationResult::Success);

    // We should have identified all three functions both times through:
    REQUIRE(nativeStats1.functionsTotal == 3);
    REQUIRE(nativeStats2.functionsTotal == 3);

    // We should have compiled the three functions only the first time:
    REQUIRE(nativeStats1.functionsCompiled == 3);
    REQUIRE(nativeStats2.functionsCompiled == 0);

    // We should have bound all three functions both times through:
    REQUIRE(nativeStats1.functionsBound == 3);
    REQUIRE(nativeStats2.functionsBound == 3);
}
