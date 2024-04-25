// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/CodeGen.h"

#include "CodeGenLower.h"

#include "Luau/Common.h"
#include "Luau/CodeAllocator.h"
#include "Luau/CodeBlockUnwind.h"
#include "Luau/IrBuilder.h"

#include "Luau/UnwindBuilder.h"
#include "Luau/UnwindBuilderDwarf2.h"
#include "Luau/UnwindBuilderWin.h"

#include "Luau/AssemblyBuilderA64.h"
#include "Luau/AssemblyBuilderX64.h"

#include "CodeGenContext.h"
#include "NativeState.h"

#include "CodeGenA64.h"
#include "CodeGenX64.h"

#include "lapi.h"
#include "lmem.h"

#include <memory>
#include <optional>

#if defined(__x86_64__) || defined(_M_X64)
#ifdef _MSC_VER
#include <intrin.h> // __cpuid
#else
#include <cpuid.h> // __cpuid
#endif
#endif

#if defined(__aarch64__)
#ifdef __APPLE__
#include <sys/sysctl.h>
#endif
#endif

LUAU_FASTFLAGVARIABLE(DebugCodegenNoOpt, false)
LUAU_FASTFLAGVARIABLE(DebugCodegenOptSize, false)
LUAU_FASTFLAGVARIABLE(DebugCodegenSkipNumbering, false)

// Per-module IR instruction count limit
LUAU_FASTINTVARIABLE(CodegenHeuristicsInstructionLimit, 1'048'576) // 1 M

// Per-function IR block limit
// Current value is based on some member variables being limited to 16 bits
// Because block check is made before optimization passes and optimization can generate new blocks, limit is lowered 2x
// The limit will probably be adjusted in the future to avoid performance issues with analysis that's more complex than O(n)
LUAU_FASTINTVARIABLE(CodegenHeuristicsBlockLimit, 32'768) // 32 K

// Per-function IR instruction limit
// Current value is based on some member variables being limited to 16 bits
LUAU_FASTINTVARIABLE(CodegenHeuristicsBlockInstructionLimit, 65'536) // 64 K

LUAU_FASTFLAG(LuauCodegenContext)

namespace Luau
{
namespace CodeGen
{

static const Instruction kCodeEntryInsn = LOP_NATIVECALL;

void* gPerfLogContext = nullptr;
PerfLogFn gPerfLogFn = nullptr;

struct OldNativeProto
{
    Proto* p;
    void* execdata;
    uintptr_t exectarget;
};

// Additional data attached to Proto::execdata
// Guaranteed to be aligned to 16 bytes
struct ExtraExecData
{
    size_t execDataSize;
    size_t codeSize;
};

static int alignTo(int value, int align)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);
    CODEGEN_ASSERT(align > 0 && (align & (align - 1)) == 0);
    return (value + (align - 1)) & ~(align - 1);
}

// Returns the size of execdata required to store all code offsets and ExtraExecData structure at proper alignment
// Always a multiple of 4 bytes
static int calculateExecDataSize(Proto* proto)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);
    int size = proto->sizecode * sizeof(uint32_t);

    size = alignTo(size, 16);
    size += sizeof(ExtraExecData);

    return size;
}

// Returns pointer to the ExtraExecData inside the Proto::execdata
// Even though 'execdata' is a field in Proto, we require it to support cases where it's not attached to Proto during construction
ExtraExecData* getExtraExecData(Proto* proto, void* execdata)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);
    int size = proto->sizecode * sizeof(uint32_t);

    size = alignTo(size, 16);

    return reinterpret_cast<ExtraExecData*>(reinterpret_cast<char*>(execdata) + size);
}

static OldNativeProto createOldNativeProto(Proto* proto, const IrBuilder& ir)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);

    int execDataSize = calculateExecDataSize(proto);
    CODEGEN_ASSERT(execDataSize % 4 == 0);

    uint32_t* execData = new uint32_t[execDataSize / 4];
    uint32_t instTarget = ir.function.entryLocation;

    for (int i = 0; i < proto->sizecode; i++)
    {
        CODEGEN_ASSERT(ir.function.bcMapping[i].asmLocation >= instTarget);

        execData[i] = ir.function.bcMapping[i].asmLocation - instTarget;
    }

    // Set first instruction offset to 0 so that entering this function still executes any generated entry code.
    execData[0] = 0;

    ExtraExecData* extra = getExtraExecData(proto, execData);
    memset(extra, 0, sizeof(ExtraExecData));

    extra->execDataSize = execDataSize;

    // entry target will be relocated when assembly is finalized
    return {proto, execData, instTarget};
}

static void destroyExecData(void* execdata)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);

    delete[] static_cast<uint32_t*>(execdata);
}

static void logPerfFunction(Proto* p, uintptr_t addr, unsigned size)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);
    CODEGEN_ASSERT(p->source);

    const char* source = getstr(p->source);
    source = (source[0] == '=' || source[0] == '@') ? source + 1 : "[string]";

    char name[256];
    snprintf(name, sizeof(name), "<luau> %s:%d %s", source, p->linedefined, p->debugname ? getstr(p->debugname) : "");

    if (gPerfLogFn)
        gPerfLogFn(gPerfLogContext, addr, size, name);
}

template<typename AssemblyBuilder>
static std::optional<OldNativeProto> createNativeFunction(
    AssemblyBuilder& build, ModuleHelpers& helpers, Proto* proto, uint32_t& totalIrInstCount, CodeGenCompilationResult& result)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);

    IrBuilder ir;
    ir.buildFunctionIr(proto);

    unsigned instCount = unsigned(ir.function.instructions.size());

    if (totalIrInstCount + instCount >= unsigned(FInt::CodegenHeuristicsInstructionLimit.value))
    {
        result = CodeGenCompilationResult::CodeGenOverflowInstructionLimit;
        return std::nullopt;
    }
    totalIrInstCount += instCount;

    if (!lowerFunction(ir, build, helpers, proto, {}, /* stats */ nullptr, result))
        return std::nullopt;

    return createOldNativeProto(proto, ir);
}

static NativeState* getNativeState(lua_State* L)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);

    return static_cast<NativeState*>(L->global->ecb.context);
}

static void onCloseState(lua_State* L)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);

    delete getNativeState(L);
    L->global->ecb = lua_ExecutionCallbacks();
}

static void onDestroyFunction(lua_State* L, Proto* proto)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);

    destroyExecData(proto->execdata);
    proto->execdata = nullptr;
    proto->exectarget = 0;
    proto->codeentry = proto->code;
}

static int onEnter(lua_State* L, Proto* proto)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);

    NativeState* data = getNativeState(L);

    CODEGEN_ASSERT(proto->execdata);
    CODEGEN_ASSERT(L->ci->savedpc >= proto->code && L->ci->savedpc < proto->code + proto->sizecode);

    uintptr_t target = proto->exectarget + static_cast<uint32_t*>(proto->execdata)[L->ci->savedpc - proto->code];

    // Returns 1 to finish the function in the VM
    return GateFn(data->context.gateEntry)(L, proto, target, &data->context);
}

// used to disable native execution, unconditionally
static int onEnterDisabled(lua_State* L, Proto* proto)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);

    return 1;
}

void onDisable(lua_State* L, Proto* proto)
{
    // do nothing if proto already uses bytecode
    if (proto->codeentry == proto->code)
        return;

    // ensure that VM does not call native code for this proto
    proto->codeentry = proto->code;

    // prevent native code from entering proto with breakpoints
    proto->exectarget = 0;

    // walk all thread call stacks and clear the LUA_CALLINFO_NATIVE flag from any
    // entries pointing to the current proto that has native code enabled.
    luaM_visitgco(L, proto, [](void* context, lua_Page* page, GCObject* gco) {
        Proto* proto = (Proto*)context;

        if (gco->gch.tt != LUA_TTHREAD)
            return false;

        lua_State* th = gco2th(gco);

        for (CallInfo* ci = th->ci; ci > th->base_ci; ci--)
        {
            if (isLua(ci))
            {
                Proto* p = clvalue(ci->func)->l.p;

                if (p == proto)
                {
                    ci->flags &= ~LUA_CALLINFO_NATIVE;
                }
            }
        }

        return false;
    });
}

static size_t getMemorySize(lua_State* L, Proto* proto)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);
    ExtraExecData* extra = getExtraExecData(proto, proto->execdata);

    // While execDataSize is exactly the size of the allocation we made and hold for 'execdata' field, the code size is approximate
    // This is because code+data page is shared and owned by all Proto from a single module and each one can keep the whole region alive
    // So individual Proto being freed by GC will not reflect memory use by native code correctly
    return extra->execDataSize + extra->codeSize;
}

#if defined(__aarch64__)
unsigned int getCpuFeaturesA64()
{
    unsigned int result = 0;

#ifdef __APPLE__
    int jscvt = 0;
    size_t jscvtLen = sizeof(jscvt);
    if (sysctlbyname("hw.optional.arm.FEAT_JSCVT", &jscvt, &jscvtLen, nullptr, 0) == 0 && jscvt == 1)
        result |= A64::Feature_JSCVT;
#endif

    return result;
}
#endif

bool isSupported()
{
    if (LUA_EXTRA_SIZE != 1)
        return false;

    if (sizeof(TValue) != 16)
        return false;

    if (sizeof(LuaNode) != 32)
        return false;

    // Windows CRT uses stack unwinding in longjmp so we have to use unwind data; on other platforms, it's only necessary for C++ EH.
#if defined(_WIN32)
    if (!isUnwindSupported())
        return false;
#else
    if (!LUA_USE_LONGJMP && !isUnwindSupported())
        return false;
#endif

#if defined(__x86_64__) || defined(_M_X64)
    int cpuinfo[4] = {};
#ifdef _MSC_VER
    __cpuid(cpuinfo, 1);
#else
    __cpuid(1, cpuinfo[0], cpuinfo[1], cpuinfo[2], cpuinfo[3]);
#endif

    // We require AVX1 support for VEX encoded XMM operations
    // We also requre SSE4.1 support for ROUNDSD but the AVX check below covers it
    // https://en.wikipedia.org/wiki/CPUID#EAX=1:_Processor_Info_and_Feature_Bits
    if ((cpuinfo[2] & (1 << 28)) == 0)
        return false;

    return true;
#elif defined(__aarch64__)
    return true;
#else
    return false;
#endif
}

static void create_OLD(lua_State* L, AllocationCallback* allocationCallback, void* allocationCallbackContext)
{
    CODEGEN_ASSERT(!FFlag::LuauCodegenContext);
    CODEGEN_ASSERT(isSupported());

    std::unique_ptr<NativeState> data = std::make_unique<NativeState>(allocationCallback, allocationCallbackContext);

#if defined(_WIN32)
    data->unwindBuilder = std::make_unique<UnwindBuilderWin>();
#else
    data->unwindBuilder = std::make_unique<UnwindBuilderDwarf2>();
#endif

    data->codeAllocator.context = data->unwindBuilder.get();
    data->codeAllocator.createBlockUnwindInfo = createBlockUnwindInfo;
    data->codeAllocator.destroyBlockUnwindInfo = destroyBlockUnwindInfo;

    initFunctions(*data);

#if defined(__x86_64__) || defined(_M_X64)
    if (!X64::initHeaderFunctions(*data))
        return;
#elif defined(__aarch64__)
    if (!A64::initHeaderFunctions(*data))
        return;
#endif

    if (gPerfLogFn)
        gPerfLogFn(gPerfLogContext, uintptr_t(data->context.gateEntry), 4096, "<luau gate>");

    lua_ExecutionCallbacks* ecb = &L->global->ecb;

    ecb->context = data.release();
    ecb->close = onCloseState;
    ecb->destroy = onDestroyFunction;
    ecb->enter = onEnter;
    ecb->disable = onDisable;
    ecb->getmemorysize = getMemorySize;
}

void create(lua_State* L, AllocationCallback* allocationCallback, void* allocationCallbackContext)
{
    if (FFlag::LuauCodegenContext)
    {
        create_NEW(L, allocationCallback, allocationCallbackContext);
    }
    else
    {
        create_OLD(L, allocationCallback, allocationCallbackContext);
    }
}

void create(lua_State* L)
{
    if (FFlag::LuauCodegenContext)
    {
        create_NEW(L);
    }
    else
    {
        create(L, nullptr, nullptr);
    }
}

void create(lua_State* L, SharedCodeGenContext* codeGenContext)
{
    CODEGEN_ASSERT(FFlag::LuauCodegenContext);

    create_NEW(L, codeGenContext);
}

[[nodiscard]] bool isNativeExecutionEnabled(lua_State* L)
{
    if (FFlag::LuauCodegenContext)
    {
        return isNativeExecutionEnabled_NEW(L);
    }
    else
    {
        return getNativeState(L) ? (L->global->ecb.enter == onEnter) : false;
    }
}

void setNativeExecutionEnabled(lua_State* L, bool enabled)
{
    if (FFlag::LuauCodegenContext)
    {
        setNativeExecutionEnabled_NEW(L, enabled);
    }
    else
    {
        if (getNativeState(L))
            L->global->ecb.enter = enabled ? onEnter : onEnterDisabled;
    }
}

static CompilationResult compile_OLD(lua_State* L, int idx, unsigned int flags, CompilationStats* stats)
{
    CompilationResult compilationResult;

    CODEGEN_ASSERT(lua_isLfunction(L, idx));
    const TValue* func = luaA_toobject(L, idx);

    Proto* root = clvalue(func)->l.p;

    if ((flags & CodeGen_OnlyNativeModules) != 0 && (root->flags & LPF_NATIVE_MODULE) == 0)
    {
        compilationResult.result = CodeGenCompilationResult::NotNativeModule;
        return compilationResult;
    }

    // If initialization has failed, do not compile any functions
    NativeState* data = getNativeState(L);
    if (!data)
    {
        compilationResult.result = CodeGenCompilationResult::CodeGenNotInitialized;
        return compilationResult;
    }

    std::vector<Proto*> protos;
    gatherFunctions(protos, root, flags);

    // Skip protos that have been compiled during previous invocations of CodeGen::compile
    protos.erase(std::remove_if(protos.begin(), protos.end(),
                     [](Proto* p) {
                         return p == nullptr || p->execdata != nullptr;
                     }),
        protos.end());

    if (protos.empty())
    {
        compilationResult.result = CodeGenCompilationResult::NothingToCompile;
        return compilationResult;
    }

    if (stats != nullptr)
        stats->functionsTotal = uint32_t(protos.size());

#if defined(__aarch64__)
    static unsigned int cpuFeatures = getCpuFeaturesA64();
    A64::AssemblyBuilderA64 build(/* logText= */ false, cpuFeatures);
#else
    X64::AssemblyBuilderX64 build(/* logText= */ false);
#endif

    ModuleHelpers helpers;
#if defined(__aarch64__)
    A64::assembleHelpers(build, helpers);
#else
    X64::assembleHelpers(build, helpers);
#endif

    std::vector<OldNativeProto> results;
    results.reserve(protos.size());

    uint32_t totalIrInstCount = 0;

    for (Proto* p : protos)
    {
        CodeGenCompilationResult protoResult = CodeGenCompilationResult::Success;

        if (std::optional<OldNativeProto> np = createNativeFunction(build, helpers, p, totalIrInstCount, protoResult))
            results.push_back(*np);
        else
            compilationResult.protoFailures.push_back({protoResult, p->debugname ? getstr(p->debugname) : "", p->linedefined});
    }

    // Very large modules might result in overflowing a jump offset; in this case we currently abandon the entire module
    if (!build.finalize())
    {
        for (OldNativeProto result : results)
            destroyExecData(result.execdata);

        compilationResult.result = CodeGenCompilationResult::CodeGenAssemblerFinalizationFailure;
        return compilationResult;
    }

    // If no functions were assembled, we don't need to allocate/copy executable pages for helpers
    if (results.empty())
        return compilationResult;

    uint8_t* nativeData = nullptr;
    size_t sizeNativeData = 0;
    uint8_t* codeStart = nullptr;
    if (!data->codeAllocator.allocate(build.data.data(), int(build.data.size()), reinterpret_cast<const uint8_t*>(build.code.data()),
            int(build.code.size() * sizeof(build.code[0])), nativeData, sizeNativeData, codeStart))
    {
        for (OldNativeProto result : results)
            destroyExecData(result.execdata);

        compilationResult.result = CodeGenCompilationResult::AllocationFailed;
        return compilationResult;
    }

    if (gPerfLogFn && results.size() > 0)
        gPerfLogFn(gPerfLogContext, uintptr_t(codeStart), uint32_t(results[0].exectarget), "<luau helpers>");

    for (size_t i = 0; i < results.size(); ++i)
    {
        uint32_t begin = uint32_t(results[i].exectarget);
        uint32_t end = i + 1 < results.size() ? uint32_t(results[i + 1].exectarget) : uint32_t(build.code.size() * sizeof(build.code[0]));
        CODEGEN_ASSERT(begin < end);

        if (gPerfLogFn)
            logPerfFunction(results[i].p, uintptr_t(codeStart) + begin, end - begin);

        ExtraExecData* extra = getExtraExecData(results[i].p, results[i].execdata);
        extra->codeSize = end - begin;
    }

    for (const OldNativeProto& result : results)
    {
        // the memory is now managed by VM and will be freed via onDestroyFunction
        result.p->execdata = result.execdata;
        result.p->exectarget = uintptr_t(codeStart) + result.exectarget;
        result.p->codeentry = &kCodeEntryInsn;
    }

    if (stats != nullptr)
    {
        for (const OldNativeProto& result : results)
        {
            stats->bytecodeSizeBytes += result.p->sizecode * sizeof(Instruction);

            // Account for the native -> bytecode instruction offsets mapping:
            stats->nativeMetadataSizeBytes += result.p->sizecode * sizeof(uint32_t);
        }

        stats->functionsCompiled += uint32_t(results.size());
        stats->nativeCodeSizeBytes += build.code.size();
        stats->nativeDataSizeBytes += build.data.size();
    }

    return compilationResult;
}

CompilationResult compile(lua_State* L, int idx, unsigned int flags, CompilationStats* stats)
{
    if (FFlag::LuauCodegenContext)
    {
        return compile_NEW(L, idx, flags, stats);
    }
    else
    {
        return compile_OLD(L, idx, flags, stats);
    }
}

CompilationResult compile(const ModuleId& moduleId, lua_State* L, int idx, unsigned int flags, CompilationStats* stats)
{
    CODEGEN_ASSERT(FFlag::LuauCodegenContext);

    return compile_NEW(moduleId, L, idx, flags, stats);
}

void setPerfLog(void* context, PerfLogFn logFn)
{
    gPerfLogContext = context;
    gPerfLogFn = logFn;
}

} // namespace CodeGen
} // namespace Luau
