// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/SharedCodeAllocator.h"

#include "Luau/CodeAllocator.h"

#include <algorithm>
#include <string_view>
#include <utility>

namespace Luau
{
namespace CodeGen
{


struct NativeProtoBytecodeIdEqual
{
    [[nodiscard]] bool operator()(const NativeProtoExecDataPtr& left, const NativeProtoExecDataPtr& right) const noexcept
    {
        return getNativeProtoExecDataHeader(left.get()).bytecodeId == getNativeProtoExecDataHeader(right.get()).bytecodeId;
    }
};

struct NativeProtoBytecodeIdLess
{
    [[nodiscard]] bool operator()(const NativeProtoExecDataPtr& left, const NativeProtoExecDataPtr& right) const noexcept
    {
        return getNativeProtoExecDataHeader(left.get()).bytecodeId < getNativeProtoExecDataHeader(right.get()).bytecodeId;
    }

    [[nodiscard]] bool operator()(const NativeProtoExecDataPtr& left, uint32_t right) const noexcept
    {
        return getNativeProtoExecDataHeader(left.get()).bytecodeId < right;
    }

    [[nodiscard]] bool operator()(uint32_t left, const NativeProtoExecDataPtr& right) const noexcept
    {
        return left < getNativeProtoExecDataHeader(right.get()).bytecodeId;
    }
};

NativeModule::NativeModule(SharedCodeAllocator* allocator, const ModuleId& moduleId, const uint8_t* moduleBaseAddress,
    std::vector<NativeProtoExecDataPtr> nativeProtos) noexcept
    : allocator{allocator}
    , moduleId{moduleId}
    , moduleBaseAddress{moduleBaseAddress}
    , nativeProtos{std::move(nativeProtos)}
{
    LUAU_ASSERT(allocator != nullptr);
    LUAU_ASSERT(moduleBaseAddress != nullptr);

    // Bind all of the NativeProtos to this module:
    for (const NativeProtoExecDataPtr& nativeProto : this->nativeProtos)
    {
        NativeProtoExecDataHeader& header = getNativeProtoExecDataHeader(nativeProto.get());
        header.nativeModule = this;
        header.entryOffsetOrAddress = moduleBaseAddress + reinterpret_cast<uintptr_t>(header.entryOffsetOrAddress);
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

size_t NativeModule::addRefs(size_t count) const noexcept
{
    return refcount.fetch_add(count) + count;
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

[[nodiscard]] const uint32_t* NativeModule::tryGetNativeProto(uint32_t bytecodeId) const noexcept
{
    const auto range = std::equal_range(nativeProtos.begin(), nativeProtos.end(), bytecodeId, NativeProtoBytecodeIdLess{});
    if (range.first == range.second)
        return nullptr;

    LUAU_ASSERT(std::next(range.first) == range.second);

    return range.first->get();
}

[[nodiscard]] const std::vector<NativeProtoExecDataPtr>& NativeModule::getNativeProtos() const noexcept
{
    return nativeProtos;
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


SharedCodeAllocator::SharedCodeAllocator(CodeAllocator* codeAllocator) noexcept
    : codeAllocator{codeAllocator}
{
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

std::pair<NativeModuleRef, bool> SharedCodeAllocator::getOrInsertNativeModule(const ModuleId& moduleId,
    std::vector<NativeProtoExecDataPtr> nativeProtos, const uint8_t* data, size_t dataSize, const uint8_t* code, size_t codeSize)
{
    std::unique_lock lock{mutex};

    if (NativeModuleRef existingModule = tryGetNativeModuleWithLockHeld(moduleId))
        return {std::move(existingModule), false};

    uint8_t* nativeData = nullptr;
    size_t sizeNativeData = 0;
    uint8_t* codeStart = nullptr;
    if (!codeAllocator->allocate(data, int(dataSize), code, int(codeSize), nativeData, sizeNativeData, codeStart))
    {
        return {};
    }

    std::unique_ptr<NativeModule>& nativeModule = nativeModules[moduleId];
    nativeModule = std::make_unique<NativeModule>(this, moduleId, codeStart, std::move(nativeProtos));

    return {NativeModuleRef{nativeModule.get()}, true};
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
