// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/SharedCodeAllocator.h"

#include <algorithm>
#include <string_view>
#include <utility>

namespace Luau
{
namespace CodeGen
{



NativeProto::NativeProto(uint32_t bytecodeId, NativeProtoExecDataPtr nativeExecData)
    : bytecodeId{bytecodeId}
    , nativeExecData{std::move(nativeExecData)}
{
}

void NativeProto::setEntryOffset(uint32_t entryOffset) noexcept
{
    entryOffsetOrAddress = reinterpret_cast<const uint8_t*>(static_cast<uintptr_t>(entryOffset));
}

void NativeProto::assignToModule(NativeModule* nativeModule) noexcept
{
    getNativeProtoExecDataHeader(nativeExecData.get()).nativeModule = nativeModule;

    entryOffsetOrAddress = nativeModule->getModuleBaseAddress() + reinterpret_cast<uintptr_t>(entryOffsetOrAddress);
}

[[nodiscard]] uint32_t NativeProto::getBytecodeId() const noexcept
{
    return bytecodeId;
}

[[nodiscard]] const uint8_t* NativeProto::getEntryAddress() const noexcept
{
    return entryOffsetOrAddress;
}

[[nodiscard]] const NativeProtoExecDataHeader& NativeProto::getNativeExecDataHeader() const noexcept
{
    return getNativeProtoExecDataHeader(nativeExecData.get());
}

[[nodiscard]] const uint32_t* NativeProto::getNonOwningPointerToInstructionOffsets() const noexcept
{
    return nativeExecData.get();
}

[[nodiscard]] const uint32_t* NativeProto::getOwningPointerToInstructionOffsets() const noexcept
{
    getNativeProtoExecDataHeader(nativeExecData.get()).nativeModule->addRef();
    return nativeExecData.get();
}

void NativeProto::releaseOwningPointerToInstructionOffsets(const uint32_t* ownedInstructionOffsets) noexcept
{
    getNativeProtoExecDataHeader(ownedInstructionOffsets).nativeModule->release();
}


struct NativeProtoBytecodeIdEqual
{
    [[nodiscard]] bool operator()(const NativeProto& left, const NativeProto& right) const noexcept
    {
        return left.getBytecodeId() == right.getBytecodeId();
    }
};

struct NativeProtoBytecodeIdLess
{
    [[nodiscard]] bool operator()(const NativeProto& left, const NativeProto& right) const noexcept
    {
        return left.getBytecodeId() < right.getBytecodeId();
    }

    [[nodiscard]] bool operator()(const NativeProto& left, uint32_t right) const noexcept
    {
        return left.getBytecodeId() < right;
    }

    [[nodiscard]] bool operator()(uint32_t left, const NativeProto& right) const noexcept
    {
        return left < right.getBytecodeId();
    }
};

NativeModule::NativeModule(
    SharedCodeAllocator* allocator, const ModuleId& moduleId, const uint8_t* moduleBaseAddress, std::vector<NativeProto> nativeProtos) noexcept
    : allocator{allocator}
    , moduleId{moduleId}
    , moduleBaseAddress{moduleBaseAddress}
    , nativeProtos{std::move(nativeProtos)}
{
    LUAU_ASSERT(allocator != nullptr);
    LUAU_ASSERT(moduleBaseAddress != nullptr);

    // Bind all of the NativeProtos to this module:
    for (NativeProto& nativeProto : this->nativeProtos)
    {
        nativeProto.assignToModule(this);
    }

    std::sort(this->nativeProtos.begin(), this->nativeProtos.end(), NativeProtoBytecodeIdLess{});

    // We should not have two NativeProtos for the same bytecode id:
    LUAU_ASSERT(std::adjacent_find(this->nativeProtos.begin(), this->nativeProtos.end(), NativeProtoBytecodeIdEqual{}) == this->nativeProtos.end());
}

NativeModule::~NativeModule() noexcept
{
    LUAU_ASSERT(refcount == 0);
}

size_t NativeModule::addRef() const noexcept
{
    return refcount.fetch_add(1) + 1;
}

size_t NativeModule::release() const noexcept
{
    size_t newRefcount = refcount.fetch_sub(1) - 1;
    if (newRefcount != 0)
        return newRefcount;

    allocator->eraseNativeModuleIfUnreferenced(moduleId);

    // NOTE:  *this may have been destroyed by the prior call, and must not be
    // accessed after this point.
    return 0;
}

[[nodiscard]] size_t NativeModule::getRefcount() const noexcept
{
    return refcount;
}

[[nodiscard]] const uint8_t* NativeModule::getModuleBaseAddress() const noexcept
{
    return moduleBaseAddress;
}

[[nodiscard]] const NativeProto* NativeModule::tryGetNativeProto(uint32_t bytecodeId) const noexcept
{
    const auto range = std::equal_range(nativeProtos.begin(), nativeProtos.end(), bytecodeId, NativeProtoBytecodeIdLess{});
    if (range.first == range.second)
        return nullptr;

    LUAU_ASSERT(std::next(range.first) == range.second);

    return &*range.first;
}


NativeModuleRef::NativeModuleRef(NativeModule* nativeModule) noexcept
    : nativeModule{nativeModule}
{
    if (nativeModule != nullptr)
        nativeModule->addRef();
}

NativeModuleRef::NativeModuleRef(const NativeModuleRef& other) noexcept
    : nativeModule{other.nativeModule}
{
    if (nativeModule != nullptr)
        nativeModule->addRef();
}

NativeModuleRef::NativeModuleRef(NativeModuleRef&& other) noexcept
    : nativeModule{std::exchange(other.nativeModule, nullptr)}
{
}

NativeModuleRef& NativeModuleRef::operator=(NativeModuleRef other) noexcept
{
    swap(other);

    return *this;
}

NativeModuleRef::~NativeModuleRef() noexcept
{
    reset();
}

void NativeModuleRef::reset() noexcept
{
    if (nativeModule == nullptr)
        return;

    nativeModule->release();
    nativeModule = nullptr;
}

void NativeModuleRef::swap(NativeModuleRef& other) noexcept
{
    std::swap(nativeModule, other.nativeModule);
}

[[nodiscard]] bool NativeModuleRef::empty() const noexcept
{
    return nativeModule == nullptr;
}

NativeModuleRef::operator bool() const noexcept
{
    return nativeModule != nullptr;
}

[[nodiscard]] const NativeModule* NativeModuleRef::get() const noexcept
{
    return nativeModule;
}

[[nodiscard]] const NativeModule* NativeModuleRef::operator->() const noexcept
{
    return nativeModule;
}

[[nodiscard]] const NativeModule& NativeModuleRef::operator*() const noexcept
{
    return *nativeModule;
}


SharedCodeAllocator::~SharedCodeAllocator() noexcept
{
    // The allocator should not be destroyed until all outstanding references
    // have been released and all allocated modules have been destroyed.
    LUAU_ASSERT(nativeModules.empty());
}

[[nodiscard]] NativeModuleRef SharedCodeAllocator::tryGetNativeModule(const ModuleId& moduleId) const noexcept
{
    std::unique_lock lock{mutex};

    return tryGetNativeModuleWithLockHeld(moduleId);
}

NativeModuleRef SharedCodeAllocator::getOrInsertNativeModule(
    const ModuleId& moduleId, std::vector<NativeProto> nativeProtos, const std::vector<uint8_t>& data, const std::vector<uint8_t>& code)
{
    std::unique_lock lock{mutex};

    if (NativeModuleRef existingModule = tryGetNativeModuleWithLockHeld(moduleId))
        return existingModule;

    // We simulate allocation until the backend allocator is integrated

    std::unique_ptr<NativeModule>& nativeModule = nativeModules[moduleId];
    nativeModule = std::make_unique<NativeModule>(this, moduleId, baseAddress, std::move(nativeProtos));

    baseAddress += data.size() + code.size();

    return NativeModuleRef{nativeModule.get()};
}

void SharedCodeAllocator::eraseNativeModuleIfUnreferenced(const ModuleId& moduleId)
{
    std::unique_lock lock{mutex};

    const auto it = nativeModules.find(moduleId);
    if (it == nativeModules.end())
        return;

    // It is possible that someone acquired a reference to the module between
    // the time that we called this function and the time that we acquired the
    // lock.  If so, that's okay.
    if (it->second->getRefcount() != 0)
        return;

    nativeModules.erase(it);
}

[[nodiscard]] NativeModuleRef SharedCodeAllocator::tryGetNativeModuleWithLockHeld(const ModuleId& moduleId) const noexcept
{
    const auto it = nativeModules.find(moduleId);
    if (it == nativeModules.end())
        return NativeModuleRef{};

    return NativeModuleRef{it->second.get()};
}

[[nodiscard]] size_t SharedCodeAllocator::ModuleIdHash::operator()(const ModuleId& moduleId) const noexcept
{
    return std::hash<std::string_view>{}(std::string_view{reinterpret_cast<const char*>(moduleId.data()), moduleId.size()});
}

} // namespace CodeGen
} // namespace Luau
