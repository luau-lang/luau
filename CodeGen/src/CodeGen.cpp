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

#include "NativeState.h"

#include "CodeGenA64.h"
#include "CodeGenX64.h"

#include "lapi.h"

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
LUAU_FASTINTVARIABLE(CodegenHeuristicsInstructionLimit, 1'048'576)   // 1 M

// Per-function IR block limit
// Current value is based on some member variables being limited to 16 bits
// Because block check is made before optimization passes and optimization can generate new blocks, limit is lowered 2x
// The limit will probably be adjusted in the future to avoid performance issues with analysis that's more complex than O(n)
LUAU_FASTINTVARIABLE(CodegenHeuristicsBlockLimit, 32'768)            // 32 K

// Per-function IR instruction limit
// Current value is based on some member variables being limited to 16 bits
LUAU_FASTINTVARIABLE(CodegenHeuristicsBlockInstructionLimit, 65'536) // 64 K

namespace Luau
{
namespace CodeGen
{

static const Instruction kCodeEntryInsn = LOP_NATIVECALL;

static void* gPerfLogContext = nullptr;
static PerfLogFn gPerfLogFn = nullptr;

struct NativeProto
{
    Proto* p;
    void* execdata;
    uintptr_t exectarget;
};

static NativeProto createNativeProto(Proto* proto, const IrBuilder& ir)
{
    int sizecode = proto->sizecode;

    uint32_t* instOffsets = new uint32_t[sizecode];
    uint32_t instTarget = ir.function.entryLocation;

    for (int i = 0; i < sizecode; i++)
    {
        LUAU_ASSERT(ir.function.bcMapping[i].asmLocation >= instTarget);

        instOffsets[i] = ir.function.bcMapping[i].asmLocation - instTarget;
    }

    // Set first instruction offset to 0 so that entering this function still executes any generated entry code.
    instOffsets[0] = 0;

    // entry target will be relocated when assembly is finalized
    return {proto, instOffsets, instTarget};
}

static void destroyExecData(void* execdata)
{
    delete[] static_cast<uint32_t*>(execdata);
}

static void logPerfFunction(Proto* p, uintptr_t addr, unsigned size)
{
    LUAU_ASSERT(p->source);

    const char* source = getstr(p->source);
    source = (source[0] == '=' || source[0] == '@') ? source + 1 : "[string]";

    char name[256];
    snprintf(name, sizeof(name), "<luau> %s:%d %s", source, p->linedefined, p->debugname ? getstr(p->debugname) : "");

    if (gPerfLogFn)
        gPerfLogFn(gPerfLogContext, addr, size, name);
}

template<typename AssemblyBuilder>
static std::optional<NativeProto> createNativeFunction(AssemblyBuilder& build, ModuleHelpers& helpers, Proto* proto, uint32_t& totalIrInstCount)
{
    IrBuilder ir;
    ir.buildFunctionIr(proto);

    unsigned instCount = unsigned(ir.function.instructions.size());

    if (totalIrInstCount + instCount >= unsigned(FInt::CodegenHeuristicsInstructionLimit.value))
        return std::nullopt;

    totalIrInstCount += instCount;

    if (!lowerFunction(ir, build, helpers, proto, {}, /* stats */ nullptr))
        return std::nullopt;

    return createNativeProto(proto, ir);
}

static NativeState* getNativeState(lua_State* L)
{
    return static_cast<NativeState*>(L->global->ecb.context);
}

static void onCloseState(lua_State* L)
{
    delete getNativeState(L);
    L->global->ecb = lua_ExecutionCallbacks();
}

static void onDestroyFunction(lua_State* L, Proto* proto)
{
    destroyExecData(proto->execdata);
    proto->execdata = nullptr;
    proto->exectarget = 0;
    proto->codeentry = proto->code;
}

static int onEnter(lua_State* L, Proto* proto)
{
    NativeState* data = getNativeState(L);

    LUAU_ASSERT(proto->execdata);
    LUAU_ASSERT(L->ci->savedpc >= proto->code && L->ci->savedpc < proto->code + proto->sizecode);

    uintptr_t target = proto->exectarget + static_cast<uint32_t*>(proto->execdata)[L->ci->savedpc - proto->code];

    // Returns 1 to finish the function in the VM
    return GateFn(data->context.gateEntry)(L, proto, target, &data->context);
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

void create(lua_State* L, AllocationCallback* allocationCallback, void* allocationCallbackContext)
{
    LUAU_ASSERT(isSupported());

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
}

void create(lua_State* L)
{
    create(L, nullptr, nullptr);
}

CodeGenCompilationResult compile(lua_State* L, int idx, unsigned int flags, CompilationStats* stats)
{
    LUAU_ASSERT(lua_isLfunction(L, idx));
    const TValue* func = luaA_toobject(L, idx);

    Proto* root = clvalue(func)->l.p;
    if ((flags & CodeGen_OnlyNativeModules) != 0 && (root->flags & LPF_NATIVE_MODULE) == 0)
        return CodeGenCompilationResult::NothingToCompile;

    // If initialization has failed, do not compile any functions
    NativeState* data = getNativeState(L);
    if (!data)
        return CodeGenCompilationResult::CodeGenNotInitialized;

    std::vector<Proto*> protos;
    gatherFunctions(protos, root, flags);

    // Skip protos that have been compiled during previous invocations of CodeGen::compile
    protos.erase(std::remove_if(protos.begin(), protos.end(),
                     [](Proto* p) {
                         return p == nullptr || p->execdata != nullptr;
                     }),
        protos.end());

    if (protos.empty())
        return CodeGenCompilationResult::NothingToCompile;

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

    std::vector<NativeProto> results;
    results.reserve(protos.size());

    uint32_t totalIrInstCount = 0;

    for (Proto* p : protos)
    {
        if (std::optional<NativeProto> np = createNativeFunction(build, helpers, p, totalIrInstCount))
            results.push_back(*np);
    }

    // Very large modules might result in overflowing a jump offset; in this case we currently abandon the entire module
    if (!build.finalize())
    {
        for (NativeProto result : results)
            destroyExecData(result.execdata);

        return CodeGenCompilationResult::CodeGenFailed;
    }

    // If no functions were assembled, we don't need to allocate/copy executable pages for helpers
    if (results.empty())
        return CodeGenCompilationResult::CodeGenFailed;

    uint8_t* nativeData = nullptr;
    size_t sizeNativeData = 0;
    uint8_t* codeStart = nullptr;
    if (!data->codeAllocator.allocate(build.data.data(), int(build.data.size()), reinterpret_cast<const uint8_t*>(build.code.data()),
            int(build.code.size() * sizeof(build.code[0])), nativeData, sizeNativeData, codeStart))
    {
        for (NativeProto result : results)
            destroyExecData(result.execdata);

        return CodeGenCompilationResult::AllocationFailed;
    }

    if (gPerfLogFn && results.size() > 0)
    {
        gPerfLogFn(gPerfLogContext, uintptr_t(codeStart), uint32_t(results[0].exectarget), "<luau helpers>");

        for (size_t i = 0; i < results.size(); ++i)
        {
            uint32_t begin = uint32_t(results[i].exectarget);
            uint32_t end = i + 1 < results.size() ? uint32_t(results[i + 1].exectarget) : uint32_t(build.code.size() * sizeof(build.code[0]));
            LUAU_ASSERT(begin < end);

            logPerfFunction(results[i].p, uintptr_t(codeStart) + begin, end - begin);
        }
    }

    for (const NativeProto& result : results)
    {
        // the memory is now managed by VM and will be freed via onDestroyFunction
        result.p->execdata = result.execdata;
        result.p->exectarget = uintptr_t(codeStart) + result.exectarget;
        result.p->codeentry = &kCodeEntryInsn;
    }

    if (stats != nullptr)
    {
        for (const NativeProto& result : results)
        {
            stats->bytecodeSizeBytes += result.p->sizecode * sizeof(Instruction);

            // Account for the native -> bytecode instruction offsets mapping:
            stats->nativeMetadataSizeBytes += result.p->sizecode * sizeof(uint32_t);
        }

        stats->functionsCompiled += uint32_t(results.size());
        stats->nativeCodeSizeBytes += build.code.size();
        stats->nativeDataSizeBytes += build.data.size();
    }

    return CodeGenCompilationResult::Success;
}

void setPerfLog(void* context, PerfLogFn logFn)
{
    gPerfLogContext = context;
    gPerfLogFn = logFn;
}

} // namespace CodeGen
} // namespace Luau
