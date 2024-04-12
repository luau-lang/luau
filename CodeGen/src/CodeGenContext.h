// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/SharedCodeAllocator.h"

#include "NativeState.h"

#include <memory>
#include <optional>
#include <stdint.h>

namespace Luau
{
namespace CodeGen
{

// The "code-gen context" maintains the native code-gen state.  There are two
// implementations.  The StandaloneCodeGenContext is a VM-specific context type.
// It is the "simple" implementation that can be used when native code-gen is
// used with a single Luau VM.  The SharedCodeGenContext supports use from
// multiple Luau VMs concurrently, and allows for sharing of executable native
// code and related metadata.

struct ModuleBindResult
{
    CodeGenCompilationResult compilationResult = {};

    uint32_t functionsBound = 0;
};

class BaseCodeGenContext
{
public:
    BaseCodeGenContext(size_t blockSize, size_t maxTotalSize, AllocationCallback* allocationCallback, void* allocationCallbackContext);

    [[nodiscard]] bool initHeaderFunctions();

    [[nodiscard]] virtual std::optional<ModuleBindResult> tryBindExistingModule(
        const ModuleId& moduleId, const std::vector<Proto*>& moduleProtos) = 0;

    [[nodiscard]] virtual ModuleBindResult bindModule(const std::optional<ModuleId>& moduleId, const std::vector<Proto*>& moduleProtos,
        std::vector<NativeProtoExecDataPtr> nativeExecDatas, const uint8_t* data, size_t dataSize, const uint8_t* code, size_t codeSize) = 0;

    virtual void onCloseState() noexcept = 0;
    virtual void onDestroyFunction(void* execdata) noexcept = 0;

    CodeAllocator codeAllocator;
    std::unique_ptr<UnwindBuilder> unwindBuilder;

    uint8_t* gateData = nullptr;
    size_t gateDataSize = 0;

    NativeContext context;
};

class StandaloneCodeGenContext final : public BaseCodeGenContext
{
public:
    StandaloneCodeGenContext(size_t blockSize, size_t maxTotalSize, AllocationCallback* allocationCallback, void* allocationCallbackContext);

    [[nodiscard]] virtual std::optional<ModuleBindResult> tryBindExistingModule(
        const ModuleId& moduleId, const std::vector<Proto*>& moduleProtos) override;

    [[nodiscard]] virtual ModuleBindResult bindModule(const std::optional<ModuleId>& moduleId, const std::vector<Proto*>& moduleProtos,
        std::vector<NativeProtoExecDataPtr> nativeExecDatas, const uint8_t* data, size_t dataSize, const uint8_t* code, size_t codeSize) override;

    virtual void onCloseState() noexcept override;
    virtual void onDestroyFunction(void* execdata) noexcept override;

private:
};

class SharedCodeGenContext final : public BaseCodeGenContext
{
public:
    SharedCodeGenContext(size_t blockSize, size_t maxTotalSize, AllocationCallback* allocationCallback, void* allocationCallbackContext);

    [[nodiscard]] virtual std::optional<ModuleBindResult> tryBindExistingModule(
        const ModuleId& moduleId, const std::vector<Proto*>& moduleProtos) override;

    [[nodiscard]] virtual ModuleBindResult bindModule(const std::optional<ModuleId>& moduleId, const std::vector<Proto*>& moduleProtos,
        std::vector<NativeProtoExecDataPtr> nativeExecDatas, const uint8_t* data, size_t dataSize, const uint8_t* code, size_t codeSize) override;

    virtual void onCloseState() noexcept override;
    virtual void onDestroyFunction(void* execdata) noexcept override;

private:
    SharedCodeAllocator sharedAllocator;
};


// The following will become the public interface, and can be moved into
// CodeGen.h after the shared allocator work is complete.  When the old
// implementation is removed, the _NEW suffix can be dropped from these
// functions.

// Initializes native code-gen on the provided Luau VM, using a VM-specific
// code-gen context and either the default allocator parameters or custom
// allocator parameters.
void create_NEW(lua_State* L);
void create_NEW(lua_State* L, AllocationCallback* allocationCallback, void* allocationCallbackContext);
void create_NEW(lua_State* L, size_t blockSize, size_t maxTotalSize, AllocationCallback* allocationCallback, void* allocationCallbackContext);

// Initializes native code-gen on the provided Luau VM, using the provided
// SharedCodeGenContext.  Note that after this function is called, the
// SharedCodeGenContext must not be destroyed until after the Luau VM L is
// destroyed via lua_close.
void create_NEW(lua_State* L, SharedCodeGenContext* codeGenContext);

CompilationResult compile_NEW(lua_State* L, int idx, unsigned int flags, CompilationStats* stats);
CompilationResult compile_NEW(const ModuleId& moduleId, lua_State* L, int idx, unsigned int flags, CompilationStats* stats);

// Returns true if native execution is currently enabled for this VM
[[nodiscard]] bool isNativeExecutionEnabled_NEW(lua_State* L);

// Enables or disables native excution for this VM
void setNativeExecutionEnabled_NEW(lua_State* L, bool enabled);

} // namespace CodeGen
} // namespace Luau
