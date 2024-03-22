// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/SharedCodeAllocator.h"

#include "doctest.h"

// We explicitly test correctness of self-assignment for some types
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wself-assign-overloaded"
#endif

using namespace Luau::CodeGen;

TEST_SUITE_BEGIN("SharedCodeAllocator");

TEST_CASE("NativeModuleRefRefcounting")
{
    SharedCodeAllocator allocator{};

    REQUIRE(allocator.tryGetNativeModule(ModuleId{0x0a}).empty());

    NativeModuleRef modRefA = allocator.getOrInsertNativeModule(ModuleId{0x0a}, {}, {}, {});
    REQUIRE(!modRefA.empty());

    // If we attempt to get the module again, we should get the same module back:
    REQUIRE(allocator.tryGetNativeModule(ModuleId{0x0a}).get() == modRefA.get());

    // If we try to insert another instance of the module, we should get the
    // existing module back:
    REQUIRE(allocator.getOrInsertNativeModule(ModuleId{0x0a}, {}, {}, {}).get() == modRefA.get());

    // If we try to look up a different module, we should not get the existing
    // module back:
    REQUIRE(allocator.tryGetNativeModule(ModuleId{0x0b}).empty());

    // (Insert a second module to help with validation below)
    NativeModuleRef modRefB = allocator.getOrInsertNativeModule(ModuleId{0x0b}, {}, {}, {});
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

    // NativeModuleRef self move assignment:
    {
        NativeModuleRef modRef1{modRefA};
        modRef1 = std::move(modRef1);
        REQUIRE(modRef1.get() == modRefA.get());
        REQUIRE(modRefA->getRefcount() == 2);
    }

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
    SharedCodeAllocator allocator{};

    std::vector<NativeProto> nativeProtos;
    nativeProtos.reserve(1);
    nativeProtos.push_back(NativeProto{0x01, createNativeProtoExecData(0)});

    NativeModuleRef modRefA = allocator.getOrInsertNativeModule(ModuleId{0x0a}, std::move(nativeProtos), {}, {});
    REQUIRE(!modRefA.empty());
    REQUIRE(modRefA->getRefcount());

    const NativeProto* proto1 = modRefA->tryGetNativeProto(0x01);
    REQUIRE(proto1 != nullptr);

    // getNonOwningPointerToInstructionOffsets should not acquire ownership:
    const uint32_t* unownedInstructionOffsets = proto1->getNonOwningPointerToInstructionOffsets();
    REQUIRE(unownedInstructionOffsets != nullptr);
    REQUIRE(modRefA->getRefcount() == 1);

    // getOwningPointerToInstructionOffsets should acquire ownership:
    const uint32_t* ownedInstructionOffsets = proto1->getOwningPointerToInstructionOffsets();
    REQUIRE(ownedInstructionOffsets == unownedInstructionOffsets);
    REQUIRE(modRefA->getRefcount() == 2);

    // We should be able to call it multiple times to get multiple references:
    const uint32_t* ownedInstructionOffsets2 = proto1->getOwningPointerToInstructionOffsets();
    REQUIRE(ownedInstructionOffsets2 == unownedInstructionOffsets);
    REQUIRE(modRefA->getRefcount() == 3);

    // releaseOwningPointerToInstructionOffsets should be callable to release
    // the reference:
    NativeProto::releaseOwningPointerToInstructionOffsets(ownedInstructionOffsets2);
    REQUIRE(modRefA->getRefcount() == 2);

    // If we release our NativeModuleRef, the module should be kept alive by
    // the owning instruction offsets pointer:
    modRefA.reset();

    modRefA = allocator.tryGetNativeModule(ModuleId{0x0a});
    REQUIRE(!modRefA.empty());
    REQUIRE(modRefA->getRefcount() == 2);

    // If the last "release" comes via releaseOwningPointerToInstructionOffsets,
    // the module should be successfully destroyed:
    modRefA.reset();
    NativeProto::releaseOwningPointerToInstructionOffsets(ownedInstructionOffsets);
    REQUIRE(allocator.tryGetNativeModule(ModuleId{0x0a}).empty());
}

TEST_CASE("NativeProtoState")
{
    SharedCodeAllocator allocator{};

    const std::vector<uint8_t> data(16);
    const std::vector<uint8_t> code(16);

    std::vector<NativeProto> nativeProtos;
    nativeProtos.reserve(2);

    {
        NativeProtoExecDataPtr nativeExecData = createNativeProtoExecData(2);
        nativeExecData[0] = 0;
        nativeExecData[1] = 4;

        NativeProto proto{1, std::move(nativeExecData)};
        proto.setEntryOffset(0x00);
        nativeProtos.push_back(std::move(proto));
    }

    {
        NativeProtoExecDataPtr nativeExecData = createNativeProtoExecData(2);
        nativeExecData[0] = 8;
        nativeExecData[1] = 12;

        NativeProto proto{3, std::move(nativeExecData)};
        proto.setEntryOffset(0x08);
        nativeProtos.push_back(std::move(proto));
    }

    NativeModuleRef modRefA = allocator.getOrInsertNativeModule(ModuleId{0x0a}, std::move(nativeProtos), data, code);
    REQUIRE(!modRefA.empty());
    REQUIRE(modRefA->getModuleBaseAddress() != nullptr);

    const NativeProto* proto1 = modRefA->tryGetNativeProto(1);
    REQUIRE(proto1 != nullptr);
    REQUIRE(proto1->getBytecodeId() == 1);
    REQUIRE(proto1->getEntryAddress() == modRefA->getModuleBaseAddress() + 0x00);
    const uint32_t* proto1Offsets = proto1->getNonOwningPointerToInstructionOffsets();
    REQUIRE(proto1Offsets != nullptr);
    REQUIRE(proto1Offsets[0] == 0);
    REQUIRE(proto1Offsets[1] == 4);

    const NativeProto* proto3 = modRefA->tryGetNativeProto(3);
    REQUIRE(proto3 != nullptr);
    REQUIRE(proto3->getBytecodeId() == 3);
    REQUIRE(proto3->getEntryAddress() == modRefA->getModuleBaseAddress() + 0x08);
    const uint32_t* proto3Offsets = proto3->getNonOwningPointerToInstructionOffsets();
    REQUIRE(proto3Offsets != nullptr);
    REQUIRE(proto3Offsets[0] == 8);
    REQUIRE(proto3Offsets[1] == 12);

    // Ensure that non-existent native protos cannot be found:
    REQUIRE(modRefA->tryGetNativeProto(0) == nullptr);
    REQUIRE(modRefA->tryGetNativeProto(2) == nullptr);
    REQUIRE(modRefA->tryGetNativeProto(4) == nullptr);
}
