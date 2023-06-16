// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/CodeGen.h"

#include "Luau/Common.h"
#include "Luau/CodeAllocator.h"
#include "Luau/CodeBlockUnwind.h"
#include "Luau/IrAnalysis.h"
#include "Luau/IrBuilder.h"
#include "Luau/IrDump.h"
#include "Luau/IrUtils.h"
#include "Luau/OptimizeConstProp.h"
#include "Luau/OptimizeFinalX64.h"

#include "Luau/UnwindBuilder.h"
#include "Luau/UnwindBuilderDwarf2.h"
#include "Luau/UnwindBuilderWin.h"

#include "Luau/AssemblyBuilderA64.h"
#include "Luau/AssemblyBuilderX64.h"

#include "NativeState.h"

#include "CodeGenA64.h"
#include "EmitCommonA64.h"
#include "IrLoweringA64.h"

#include "CodeGenX64.h"
#include "EmitCommonX64.h"
#include "EmitInstructionX64.h"
#include "IrLoweringX64.h"

#include "lapi.h"

#include <algorithm>
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
    uint32_t instTarget = ir.function.bcMapping[0].asmLocation;

    for (int i = 0; i < sizecode; i++)
    {
        LUAU_ASSERT(ir.function.bcMapping[i].asmLocation >= instTarget);

        instOffsets[i] = ir.function.bcMapping[i].asmLocation - instTarget;
    }

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

template<typename AssemblyBuilder, typename IrLowering>
static bool lowerImpl(AssemblyBuilder& build, IrLowering& lowering, IrFunction& function, int bytecodeid, AssemblyOptions options)
{
    // While we will need a better block ordering in the future, right now we want to mostly preserve build order with fallbacks outlined
    std::vector<uint32_t> sortedBlocks;
    sortedBlocks.reserve(function.blocks.size());
    for (uint32_t i = 0; i < function.blocks.size(); i++)
        sortedBlocks.push_back(i);

    std::sort(sortedBlocks.begin(), sortedBlocks.end(), [&](uint32_t idxA, uint32_t idxB) {
        const IrBlock& a = function.blocks[idxA];
        const IrBlock& b = function.blocks[idxB];

        // Place fallback blocks at the end
        if ((a.kind == IrBlockKind::Fallback) != (b.kind == IrBlockKind::Fallback))
            return (a.kind == IrBlockKind::Fallback) < (b.kind == IrBlockKind::Fallback);

        // Try to order by instruction order
        return a.sortkey < b.sortkey;
    });

    // For each IR instruction that begins a bytecode instruction, which bytecode instruction is it?
    std::vector<uint32_t> bcLocations(function.instructions.size() + 1, ~0u);

    for (size_t i = 0; i < function.bcMapping.size(); ++i)
    {
        uint32_t irLocation = function.bcMapping[i].irLocation;

        if (irLocation != ~0u)
            bcLocations[irLocation] = uint32_t(i);
    }

    bool outputEnabled = options.includeAssembly || options.includeIr;

    IrToStringContext ctx{build.text, function.blocks, function.constants, function.cfg};

    // We use this to skip outlined fallback blocks from IR/asm text output
    size_t textSize = build.text.length();
    uint32_t codeSize = build.getCodeSize();
    bool seenFallback = false;

    IrBlock dummy;
    dummy.start = ~0u;

    for (size_t i = 0; i < sortedBlocks.size(); ++i)
    {
        uint32_t blockIndex = sortedBlocks[i];
        IrBlock& block = function.blocks[blockIndex];

        if (block.kind == IrBlockKind::Dead)
            continue;

        LUAU_ASSERT(block.start != ~0u);
        LUAU_ASSERT(block.finish != ~0u);

        // If we want to skip fallback code IR/asm, we'll record when those blocks start once we see them
        if (block.kind == IrBlockKind::Fallback && !seenFallback)
        {
            textSize = build.text.length();
            codeSize = build.getCodeSize();
            seenFallback = true;
        }

        if (options.includeIr)
        {
            build.logAppend("# ");
            toStringDetailed(ctx, block, blockIndex, /* includeUseInfo */ true);
        }

        // Values can only reference restore operands in the current block
        function.validRestoreOpBlockIdx = blockIndex;

        build.setLabel(block.label);

        for (uint32_t index = block.start; index <= block.finish; index++)
        {
            LUAU_ASSERT(index < function.instructions.size());

            uint32_t bcLocation = bcLocations[index];

            // If IR instruction is the first one for the original bytecode, we can annotate it with source code text
            if (outputEnabled && options.annotator && bcLocation != ~0u)
            {
                options.annotator(options.annotatorContext, build.text, bytecodeid, bcLocation);
            }

            // If bytecode needs the location of this instruction for jumps, record it
            if (bcLocation != ~0u)
            {
                Label label = (index == block.start) ? block.label : build.setLabel();
                function.bcMapping[bcLocation].asmLocation = build.getLabelOffset(label);
            }

            IrInst& inst = function.instructions[index];

            // Skip pseudo instructions, but make sure they are not used at this stage
            // This also prevents them from getting into text output when that's enabled
            if (isPseudo(inst.cmd))
            {
                LUAU_ASSERT(inst.useCount == 0);
                continue;
            }

            // Either instruction result value is not referenced or the use count is not zero
            LUAU_ASSERT(inst.lastUse == 0 || inst.useCount != 0);

            if (options.includeIr)
            {
                build.logAppend("# ");
                toStringDetailed(ctx, block, blockIndex, inst, index, /* includeUseInfo */ true);
            }

            IrBlock& next = i + 1 < sortedBlocks.size() ? function.blocks[sortedBlocks[i + 1]] : dummy;

            lowering.lowerInst(inst, index, next);

            if (lowering.hasError())
            {
                // Place labels for all blocks that we're skipping
                // This is needed to avoid AssemblyBuilder assertions about jumps in earlier blocks with unplaced labels
                for (size_t j = i + 1; j < sortedBlocks.size(); ++j)
                {
                    IrBlock& abandoned = function.blocks[sortedBlocks[j]];

                    build.setLabel(abandoned.label);
                }

                lowering.finishFunction();

                return false;
            }
        }

        lowering.finishBlock();

        if (options.includeIr)
            build.logAppend("#\n");
    }

    if (!seenFallback)
    {
        textSize = build.text.length();
        codeSize = build.getCodeSize();
    }

    lowering.finishFunction();

    if (outputEnabled && !options.includeOutlinedCode && textSize < build.text.size())
    {
        build.text.resize(textSize);

        if (options.includeAssembly)
            build.logAppend("; skipping %u bytes of outlined code\n", unsigned((build.getCodeSize() - codeSize) * sizeof(build.code[0])));
    }

    return true;
}

[[maybe_unused]] static bool lowerIr(
    X64::AssemblyBuilderX64& build, IrBuilder& ir, NativeState& data, ModuleHelpers& helpers, Proto* proto, AssemblyOptions options)
{
    optimizeMemoryOperandsX64(ir.function);

    X64::IrLoweringX64 lowering(build, helpers, data, ir.function);

    return lowerImpl(build, lowering, ir.function, proto->bytecodeid, options);
}

[[maybe_unused]] static bool lowerIr(
    A64::AssemblyBuilderA64& build, IrBuilder& ir, NativeState& data, ModuleHelpers& helpers, Proto* proto, AssemblyOptions options)
{
    A64::IrLoweringA64 lowering(build, helpers, data, ir.function);

    return lowerImpl(build, lowering, ir.function, proto->bytecodeid, options);
}

template<typename AssemblyBuilder>
static std::optional<NativeProto> assembleFunction(AssemblyBuilder& build, NativeState& data, ModuleHelpers& helpers, Proto* proto, AssemblyOptions options)
{
    if (options.includeAssembly || options.includeIr)
    {
        if (proto->debugname)
            build.logAppend("; function %s(", getstr(proto->debugname));
        else
            build.logAppend("; function(");

        for (int i = 0; i < proto->numparams; i++)
        {
            LocVar* var = proto->locvars ? &proto->locvars[proto->sizelocvars - proto->numparams + i] : nullptr;

            if (var && var->varname)
                build.logAppend("%s%s", i == 0 ? "" : ", ", getstr(var->varname));
            else
                build.logAppend("%s$arg%d", i == 0 ? "" : ", ", i);
        }

        if (proto->numparams != 0 && proto->is_vararg)
            build.logAppend(", ...)");
        else
            build.logAppend(")");

        if (proto->linedefined >= 0)
            build.logAppend(" line %d\n", proto->linedefined);
        else
            build.logAppend("\n");
    }

    IrBuilder ir;
    ir.buildFunctionIr(proto);

    computeCfgInfo(ir.function);

    if (!FFlag::DebugCodegenNoOpt)
    {
        bool useValueNumbering = !FFlag::DebugCodegenSkipNumbering;

        constPropInBlockChains(ir, useValueNumbering);

        if (!FFlag::DebugCodegenOptSize)
            createLinearBlocks(ir, useValueNumbering);
    }

    if (!lowerIr(build, ir, data, helpers, proto, options))
    {
        if (build.logText)
            build.logAppend("; skipping (can't lower)\n\n");

        return std::nullopt;
    }

    if (build.logText)
        build.logAppend("\n");

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

static void onSetBreakpoint(lua_State* L, Proto* proto, int instruction)
{
    if (!proto->execdata)
        return;

    LUAU_ASSERT(!"Native breakpoints are not implemented");
}

#if defined(__aarch64__)
static unsigned int getCpuFeaturesA64()
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
    if (!LUA_CUSTOM_EXECUTION)
        return false;

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

void create(lua_State* L)
{
    LUAU_ASSERT(isSupported());

    std::unique_ptr<NativeState> data = std::make_unique<NativeState>();

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
    ecb->setbreakpoint = onSetBreakpoint;
}

static void gatherFunctions(std::vector<Proto*>& results, Proto* proto)
{
    if (results.size() <= size_t(proto->bytecodeid))
        results.resize(proto->bytecodeid + 1);

    // Skip protos that we've already compiled in this run: this happens because at -O2, inlined functions get their protos reused
    if (results[proto->bytecodeid])
        return;

    results[proto->bytecodeid] = proto;

    for (int i = 0; i < proto->sizep; i++)
        gatherFunctions(results, proto->p[i]);
}

void compile(lua_State* L, int idx)
{
    LUAU_ASSERT(lua_isLfunction(L, idx));
    const TValue* func = luaA_toobject(L, idx);

    // If initialization has failed, do not compile any functions
    NativeState* data = getNativeState(L);
    if (!data)
        return;

#if defined(__aarch64__)
    A64::AssemblyBuilderA64 build(/* logText= */ false, getCpuFeaturesA64());
#else
    X64::AssemblyBuilderX64 build(/* logText= */ false);
#endif

    std::vector<Proto*> protos;
    gatherFunctions(protos, clvalue(func)->l.p);

    ModuleHelpers helpers;
#if defined(__aarch64__)
    A64::assembleHelpers(build, helpers);
#else
    X64::assembleHelpers(build, helpers);
#endif

    std::vector<NativeProto> results;
    results.reserve(protos.size());

    // Skip protos that have been compiled during previous invocations of CodeGen::compile
    for (Proto* p : protos)
        if (p && p->execdata == nullptr)
            if (std::optional<NativeProto> np = assembleFunction(build, *data, helpers, p, {}))
                results.push_back(*np);

    // Very large modules might result in overflowing a jump offset; in this case we currently abandon the entire module
    if (!build.finalize())
    {
        for (NativeProto result : results)
            destroyExecData(result.execdata);

        return;
    }

    // If no functions were assembled, we don't need to allocate/copy executable pages for helpers
    if (results.empty())
        return;

    uint8_t* nativeData = nullptr;
    size_t sizeNativeData = 0;
    uint8_t* codeStart = nullptr;
    if (!data->codeAllocator.allocate(build.data.data(), int(build.data.size()), reinterpret_cast<const uint8_t*>(build.code.data()),
            int(build.code.size() * sizeof(build.code[0])), nativeData, sizeNativeData, codeStart))
    {
        for (NativeProto result : results)
            destroyExecData(result.execdata);

        return;
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

    for (NativeProto result : results)
    {
        // the memory is now managed by VM and will be freed via onDestroyFunction
        result.p->execdata = result.execdata;
        result.p->exectarget = uintptr_t(codeStart) + result.exectarget;
        result.p->codeentry = &kCodeEntryInsn;
    }
}

std::string getAssembly(lua_State* L, int idx, AssemblyOptions options)
{
    LUAU_ASSERT(lua_isLfunction(L, idx));
    const TValue* func = luaA_toobject(L, idx);

#if defined(__aarch64__)
    A64::AssemblyBuilderA64 build(/* logText= */ options.includeAssembly, getCpuFeaturesA64());
#else
    X64::AssemblyBuilderX64 build(/* logText= */ options.includeAssembly);
#endif

    NativeState data;
    initFunctions(data);

    std::vector<Proto*> protos;
    gatherFunctions(protos, clvalue(func)->l.p);

    ModuleHelpers helpers;
#if defined(__aarch64__)
    A64::assembleHelpers(build, helpers);
#else
    X64::assembleHelpers(build, helpers);
#endif

    if (!options.includeOutlinedCode && options.includeAssembly)
    {
        build.text.clear();
        build.logAppend("; skipping %u bytes of outlined helpers\n", unsigned(build.getCodeSize() * sizeof(build.code[0])));
    }

    for (Proto* p : protos)
        if (p)
            if (std::optional<NativeProto> np = assembleFunction(build, data, helpers, p, options))
                destroyExecData(np->execdata);

    if (!build.finalize())
        return std::string();

    if (options.outputBinary)
        return std::string(reinterpret_cast<const char*>(build.code.data()), reinterpret_cast<const char*>(build.code.data() + build.code.size())) +
               std::string(build.data.begin(), build.data.end());
    else
        return build.text;
}

void setPerfLog(void* context, PerfLogFn logFn)
{
    gPerfLogContext = context;
    gPerfLogFn = logFn;
}

} // namespace CodeGen
} // namespace Luau
