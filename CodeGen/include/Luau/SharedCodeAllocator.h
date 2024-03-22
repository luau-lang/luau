// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/NativeProtoExecData.h"

#include <array>
#include <atomic>
#include <memory>
#include <mutex>
#include <stdint.h>
#include <unordered_map>
#include <vector>

namespace Luau
{
namespace CodeGen
{

// SharedCodeAllocator is a native executable code allocator that provides
// shared ownership of the native code.  Code is allocated on a per-module
// basis.  Each module is uniquely identifiable via an id, which may be a hash
// or other unique value.  Each module may contain multiple natively compiled
// functions (protos).
//
// The module is the unit of shared ownership (i.e., it is where the reference
// count is maintained).

using ModuleId = std::array<uint8_t, 16>;

class NativeProto;
class NativeModule;
class NativeModuleRef;
class SharedCodeAllocator;

// A NativeProto represents a single natively-compiled function.  A NativeProto
// should be constructed for each function as it is compiled.  When compilation
// of all of the functions in a module is complete, the set of NativeProtos
// representing those functions should be passed to the NativeModule constructor.
class NativeProto
{
public:
    NativeProto(uint32_t bytecodeId, NativeProtoExecDataPtr nativeExecData);

    NativeProto(const NativeProto&) = delete;
    NativeProto(NativeProto&&) noexcept = default;
    NativeProto& operator=(const NativeProto&) = delete;
    NativeProto& operator=(NativeProto&&) noexcept = default;

    // This should be called to initialize the NativeProto state prior to
    // passing the NativeProto to the NativeModule constructor.
    void setEntryOffset(uint32_t entryOffset) noexcept;

    // This will be called by the NativeModule constructor to bind this
    // NativeProto to the NativeModule.
    void assignToModule(NativeModule* nativeModule) noexcept;

    // Gets the bytecode id for the Proto that was compiled into this NativeProto
    [[nodiscard]] uint32_t getBytecodeId() const noexcept;

    // Gets the address of the entry point for this function
    [[nodiscard]] const uint8_t* getEntryAddress() const noexcept;

    // Gets the native exec data for this function
    [[nodiscard]] const NativeProtoExecDataHeader& getNativeExecDataHeader() const noexcept;

    // The NativeProto stores an array that maps bytecode instruction indices to
    // native code offsets relative to the native entry point.  When compilation
    // and code allocation is complete, we store a pointer to this data in the
    // Luau VM Proto object for this function.  When we do this, we must acquire
    // a reference to the NativeModule that owns this NativeProto.  The
    // getOwning-version of this function acquires that reference and gets the
    // instruction offsets pointer.  When the Proto object is destroyed, this
    // pointer must be passed to releaseOwningPointerToInstructionOffsets to
    // release the reference.
    //
    // (This structure is designed to make it much more difficult to "forget"
    // to acquire a reference.)
    [[nodiscard]] const uint32_t* getNonOwningPointerToInstructionOffsets() const noexcept;
    [[nodiscard]] const uint32_t* getOwningPointerToInstructionOffsets() const noexcept;

    static void releaseOwningPointerToInstructionOffsets(const uint32_t* ownedInstructionOffsets) noexcept;

private:
    uint32_t bytecodeId = 0;

    // We store the native code offset until assignToModule() is called, after
    // which point we store the actual address.
    const uint8_t* entryOffsetOrAddress = nullptr;

    NativeProtoExecDataPtr nativeExecData = {};
};

// A NativeModule represents a single natively-compiled module (script).  It is
// the unit of shared ownership and is thus where the reference count is
// maintained.  It owns a set of NativeProtos, with associated native exec data,
// and the allocated native data and code.
class NativeModule
{
public:
    NativeModule(
        SharedCodeAllocator* allocator, const ModuleId& moduleId, const uint8_t* moduleBaseAddress, std::vector<NativeProto> nativeProtos) noexcept;

    NativeModule(const NativeModule&) = delete;
    NativeModule(NativeModule&&) = delete;
    NativeModule& operator=(const NativeModule&) = delete;
    NativeModule& operator=(NativeModule&&) = delete;

    // The NativeModule must not be destroyed if there are any outstanding
    // references.  It should thus only be destroyed by a call to release()
    // that releases the last reference.
    ~NativeModule() noexcept;

    size_t addRef() const noexcept;
    size_t release() const noexcept;
    [[nodiscard]] size_t getRefcount() const noexcept;

    // Gets the base address of the executable native code for the module.
    [[nodiscard]] const uint8_t* getModuleBaseAddress() const noexcept;

    // Attempts to find the NativeProto with the given bytecode id.  If no
    // NativeProto for that bytecode id exists, a null pointer is returned.
    [[nodiscard]] const NativeProto* tryGetNativeProto(uint32_t bytecodeId) const noexcept;

private:
    mutable std::atomic<size_t> refcount = 0;

    SharedCodeAllocator* allocator = nullptr;
    ModuleId moduleId = {};
    const uint8_t* moduleBaseAddress = nullptr;

    std::vector<NativeProto> nativeProtos = {};
};

// A NativeModuleRef is an owning reference to a NativeModule.  (Note:  We do
// not use shared_ptr, to avoid complex state management in the Luau GC Proto
// object.)
class NativeModuleRef
{
public:
    NativeModuleRef() noexcept = default;
    NativeModuleRef(NativeModule* nativeModule) noexcept;

    NativeModuleRef(const NativeModuleRef& other) noexcept;
    NativeModuleRef(NativeModuleRef&& other) noexcept;
    NativeModuleRef& operator=(NativeModuleRef other) noexcept;

    ~NativeModuleRef() noexcept;

    void reset() noexcept;
    void swap(NativeModuleRef& other) noexcept;

    [[nodiscard]] bool empty() const noexcept;
    explicit operator bool() const noexcept;

    [[nodiscard]] const NativeModule* get() const noexcept;
    [[nodiscard]] const NativeModule* operator->() const noexcept;
    [[nodiscard]] const NativeModule& operator*() const noexcept;

private:
    const NativeModule* nativeModule = nullptr;
};

class SharedCodeAllocator
{
public:
    SharedCodeAllocator() = default;

    SharedCodeAllocator(const SharedCodeAllocator&) = delete;
    SharedCodeAllocator(SharedCodeAllocator&&) = delete;
    SharedCodeAllocator& operator=(const SharedCodeAllocator&) = delete;
    SharedCodeAllocator& operator=(SharedCodeAllocator&&) = delete;

    ~SharedCodeAllocator() noexcept;

    // If we have a NativeModule for the given ModuleId, an owning reference to
    // it is returned.  Otherwise, an empty NativeModuleRef is returned.
    [[nodiscard]] NativeModuleRef tryGetNativeModule(const ModuleId& moduleId) const noexcept;

    // If we have a NativeModule for the given ModuleId, an owning reference to
    // it is returned.  Otherwise, a new NativeModule is created for that ModuleId
    // using the provided NativeProtos, data, and code (space is allocated for the
    // data and code such that it can be executed).
    NativeModuleRef getOrInsertNativeModule(
        const ModuleId& moduleId, std::vector<NativeProto> nativeProtos, const std::vector<uint8_t>& data, const std::vector<uint8_t>& code);

    // If a NativeModule exists for the given ModuleId and that NativeModule
    // is no longer referenced, the NativeModule is destroyed.  This should
    // usually only be called by NativeModule::release() when the reference
    // count becomes zero
    void eraseNativeModuleIfUnreferenced(const ModuleId& moduleId);

private:
    struct ModuleIdHash
    {
        [[nodiscard]] size_t operator()(const ModuleId& moduleId) const noexcept;
    };

    [[nodiscard]] NativeModuleRef tryGetNativeModuleWithLockHeld(const ModuleId& moduleId) const noexcept;

    mutable std::mutex mutex;

    // Will be removed when backend allocator is integrated
    const uint8_t* baseAddress = reinterpret_cast<const uint8_t*>(0x0f00'0000);

    std::unordered_map<ModuleId, std::unique_ptr<NativeModule>, ModuleIdHash, std::equal_to<>> nativeModules;
};

} // namespace CodeGen
} // namespace Luau
