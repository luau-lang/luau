// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "CodeGenContext.h"

#include "CodeGenA64.h"
#include "CodeGenX64.h"

#include "Luau/CodeBlockUnwind.h"
#include "Luau/UnwindBuilder.h"
#include "Luau/UnwindBuilderDwarf2.h"
#include "Luau/UnwindBuilderWin.h"

LUAU_FASTFLAG(LuauCodegenHeapSizeReport)

LUAU_FASTINT(LuauCodeGenBlockSize)
LUAU_FASTINT(LuauCodeGenMaxTotalSize)

namespace Luau
{
namespace CodeGen
{

// From CodeGen.cpp
extern void* gPerfLogContext;
extern PerfLogFn gPerfLogFn;

BaseCodeGenContext::BaseCodeGenContext(size_t blockSize, size_t maxTotalSize, AllocationCallback* allocationCallback, void* allocationCallbackContext)
    : codeAllocator{blockSize, maxTotalSize, allocationCallback, allocationCallbackContext}
{
    CODEGEN_ASSERT(isSupported());

#if defined(_WIN32)
    unwindBuilder = std::make_unique<UnwindBuilderWin>();
#else
    unwindBuilder = std::make_unique<UnwindBuilderDwarf2>();
#endif

    codeAllocator.context = unwindBuilder.get();
    codeAllocator.createBlockUnwindInfo = createBlockUnwindInfo;
    codeAllocator.destroyBlockUnwindInfo = destroyBlockUnwindInfo;

    initFunctions(context);
}

[[nodiscard]] bool BaseCodeGenContext::initHeaderFunctions()
{
#if defined(__x86_64__) || defined(_M_X64)
    if (!X64::initHeaderFunctions(*this))
        return false;
#elif defined(__aarch64__)
    if (!A64::initHeaderFunctions(*this))
        return false;
#endif

    if (gPerfLogFn)
        gPerfLogFn(gPerfLogContext, uintptr_t(context.gateEntry), 4096, "<luau gate>");

    return true;
}


StandaloneCodeGenContext::StandaloneCodeGenContext(
    size_t blockSize, size_t maxTotalSize, AllocationCallback* allocationCallback, void* allocationCallbackContext)
    : BaseCodeGenContext{blockSize, maxTotalSize, allocationCallback, allocationCallbackContext}
{
}

void StandaloneCodeGenContext::compileOrBindModule(const ModuleId&, lua_State*, int, unsigned int, CompilationStats*) {}

void StandaloneCodeGenContext::onCloseState() noexcept
{
    // The StandaloneCodeGenContext is owned by the one VM that owns it, so when
    // that VM is destroyed, we destroy *this as well:
    delete this;
}

void StandaloneCodeGenContext::onDestroyFunction(void* execdata) noexcept
{
    destroyNativeProtoExecData(static_cast<uint32_t*>(execdata));
}


SharedCodeGenContext::SharedCodeGenContext(
    size_t blockSize, size_t maxTotalSize, AllocationCallback* allocationCallback, void* allocationCallbackContext)
    : BaseCodeGenContext{blockSize, maxTotalSize, allocationCallback, allocationCallbackContext}
{
}

void SharedCodeGenContext::compileOrBindModule(const ModuleId&, lua_State*, int, unsigned int, CompilationStats*) {}

void SharedCodeGenContext::onCloseState() noexcept
{
    // The lifetime of the SharedCodeGenContext is managed separately from the
    // VMs that use it.  When a VM is destroyed, we don't need to do anything
    // here.
}

void SharedCodeGenContext::onDestroyFunction(void* execdata) noexcept
{
    getNativeProtoExecDataHeader(static_cast<const uint32_t*>(execdata)).nativeModule->release();
}


[[nodiscard]] UniqueSharedCodeGenContext createSharedCodeGenContext()
{
    return createSharedCodeGenContext(size_t(FInt::LuauCodeGenBlockSize), size_t(FInt::LuauCodeGenMaxTotalSize), nullptr, nullptr);
}

[[nodiscard]] UniqueSharedCodeGenContext createSharedCodeGenContext(AllocationCallback* allocationCallback, void* allocationCallbackContext)
{
    return createSharedCodeGenContext(
        size_t(FInt::LuauCodeGenBlockSize), size_t(FInt::LuauCodeGenMaxTotalSize), allocationCallback, allocationCallbackContext);
}

[[nodiscard]] UniqueSharedCodeGenContext createSharedCodeGenContext(
    size_t blockSize, size_t maxTotalSize, AllocationCallback* allocationCallback, void* allocationCallbackContext)
{
    UniqueSharedCodeGenContext codeGenContext{new SharedCodeGenContext{blockSize, maxTotalSize, nullptr, nullptr}};

    if (!codeGenContext->initHeaderFunctions())
        return {};

    return codeGenContext;
}

void destroySharedCodeGenContext(const SharedCodeGenContext* codeGenContext) noexcept
{
    delete codeGenContext;
}

void SharedCodeGenContextDeleter::operator()(const SharedCodeGenContext* codeGenContext) const noexcept
{
    destroySharedCodeGenContext(codeGenContext);
}


[[nodiscard]] static BaseCodeGenContext* getCodeGenContext(lua_State* L) noexcept
{
    return static_cast<BaseCodeGenContext*>(L->global->ecb.context);
}

static void onCloseState(lua_State* L) noexcept
{
    getCodeGenContext(L)->onCloseState();
    L->global->ecb = lua_ExecutionCallbacks{};
}

static void onDestroyFunction(lua_State* L, Proto* proto) noexcept
{
    getCodeGenContext(L)->onDestroyFunction(proto->execdata);
    proto->execdata = nullptr;
    proto->exectarget = 0;
    proto->codeentry = proto->code;
}

static int onEnter(lua_State* L, Proto* proto)
{
    BaseCodeGenContext* codeGenContext = getCodeGenContext(L);

    CODEGEN_ASSERT(proto->execdata);
    CODEGEN_ASSERT(L->ci->savedpc >= proto->code && L->ci->savedpc < proto->code + proto->sizecode);

    uintptr_t target = proto->exectarget + static_cast<uint32_t*>(proto->execdata)[L->ci->savedpc - proto->code];

    // Returns 1 to finish the function in the VM
    return GateFn(codeGenContext->context.gateEntry)(L, proto, target, &codeGenContext->context);
}

// Defined in CodeGen.cpp
void onDisable(lua_State* L, Proto* proto);

static size_t getMemorySize(lua_State* L, Proto* proto)
{
    CODEGEN_ASSERT(FFlag::LuauCodegenHeapSizeReport);

    const NativeProtoExecDataHeader& execDataHeader = getNativeProtoExecDataHeader(static_cast<const uint32_t*>(proto->execdata));

    const size_t execDataSize = sizeof(NativeProtoExecDataHeader) + execDataHeader.bytecodeInstructionCount * sizeof(Instruction);

    // While execDataSize is exactly the size of the allocation we made and hold for 'execdata' field, the code size is approximate
    // This is because code+data page is shared and owned by all Proto from a single module and each one can keep the whole region alive
    // So individual Proto being freed by GC will not reflect memory use by native code correctly
    return execDataSize + execDataHeader.nativeCodeSize;
}

static void initializeExecutionCallbacks(lua_State* L, BaseCodeGenContext* codeGenContext) noexcept
{
    lua_ExecutionCallbacks* ecb = &L->global->ecb;

    ecb->context = codeGenContext;
    ecb->close = onCloseState;
    ecb->destroy = onDestroyFunction;
    ecb->enter = onEnter;
    ecb->disable = onDisable;

    if (FFlag::LuauCodegenHeapSizeReport)
        ecb->getmemorysize = getMemorySize;
}

void create_NEW(lua_State* L)
{
    return create_NEW(L, size_t(FInt::LuauCodeGenBlockSize), size_t(FInt::LuauCodeGenMaxTotalSize), nullptr, nullptr);
}

void create_NEW(lua_State* L, AllocationCallback* allocationCallback, void* allocationCallbackContext)
{
    return create_NEW(L, size_t(FInt::LuauCodeGenBlockSize), size_t(FInt::LuauCodeGenMaxTotalSize), allocationCallback, allocationCallbackContext);
}

void create_NEW(lua_State* L, size_t blockSize, size_t maxTotalSize, AllocationCallback* allocationCallback, void* allocationCallbackContext)
{
    std::unique_ptr<StandaloneCodeGenContext> codeGenContext =
        std::make_unique<StandaloneCodeGenContext>(blockSize, maxTotalSize, allocationCallback, allocationCallbackContext);

    if (!codeGenContext->initHeaderFunctions())
        return;

    initializeExecutionCallbacks(L, codeGenContext.release());
}

void create_NEW(lua_State* L, SharedCodeGenContext* codeGenContext)
{
    initializeExecutionCallbacks(L, codeGenContext);
}

} // namespace CodeGen
} // namespace Luau
