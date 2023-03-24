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

#include "CustomExecUtils.h"
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

#if defined(__x86_64__) || defined(_M_X64)
#ifdef _MSC_VER
#include <intrin.h> // __cpuid
#else
#include <cpuid.h> // __cpuid
#endif
#endif

LUAU_FASTFLAGVARIABLE(DebugCodegenNoOpt, false)

namespace Luau
{
namespace CodeGen
{

static NativeProto* createNativeProto(Proto* proto, const IrBuilder& ir)
{
    NativeProto* result = new NativeProto();

    result->proto = proto;
    result->instTargets = new uintptr_t[proto->sizecode];

    for (int i = 0; i < proto->sizecode; i++)
    {
        auto [irLocation, asmLocation] = ir.function.bcMapping[i];

        result->instTargets[i] = irLocation == ~0u ? 0 : asmLocation;
    }

    return result;
}

template<typename AssemblyBuilder, typename IrLowering>
static void lowerImpl(AssemblyBuilder& build, IrLowering& lowering, IrFunction& function, int bytecodeid, AssemblyOptions options)
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
        return a.start < b.start;
    });

    DenseHashMap<uint32_t, uint32_t> bcLocations{~0u};

    // Create keys for IR assembly locations that original bytecode instruction are interested in
    for (const auto& [irLocation, asmLocation] : function.bcMapping)
    {
        if (irLocation != ~0u)
            bcLocations[irLocation] = 0;
    }

    DenseHashMap<uint32_t, uint32_t> indexIrToBc{~0u};
    bool outputEnabled = options.includeAssembly || options.includeIr;

    if (outputEnabled && options.annotator)
    {
        // Create reverse mapping from IR location to bytecode location
        for (size_t i = 0; i < function.bcMapping.size(); ++i)
        {
            uint32_t irLocation = function.bcMapping[i].irLocation;

            if (irLocation != ~0u)
                indexIrToBc[irLocation] = uint32_t(i);
        }
    }

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

        build.setLabel(block.label);

        for (uint32_t index = block.start; index <= block.finish; index++)
        {
            LUAU_ASSERT(index < function.instructions.size());

            // If IR instruction is the first one for the original bytecode, we can annotate it with source code text
            if (outputEnabled && options.annotator)
            {
                if (uint32_t* bcIndex = indexIrToBc.find(index))
                    options.annotator(options.annotatorContext, build.text, bytecodeid, *bcIndex);
            }

            // If bytecode needs the location of this instruction for jumps, record it
            if (uint32_t* bcLocation = bcLocations.find(index))
            {
                Label label = (index == block.start) ? block.label : build.setLabel();
                *bcLocation = build.getLabelOffset(label);
            }

            IrInst& inst = function.instructions[index];

            // Skip pseudo instructions, but make sure they are not used at this stage
            // This also prevents them from getting into text output when that's enabled
            if (isPseudo(inst.cmd))
            {
                LUAU_ASSERT(inst.useCount == 0);
                continue;
            }

            if (options.includeIr)
            {
                build.logAppend("# ");
                toStringDetailed(ctx, inst, index, /* includeUseInfo */ true);
            }

            IrBlock& next = i + 1 < sortedBlocks.size() ? function.blocks[sortedBlocks[i + 1]] : dummy;

            lowering.lowerInst(inst, index, next);
        }

        if (options.includeIr)
            build.logAppend("#\n");
    }

    if (outputEnabled && !options.includeOutlinedCode && seenFallback)
    {
        build.text.resize(textSize);

        if (options.includeAssembly)
            build.logAppend("; skipping %u bytes of outlined code\n", unsigned((build.getCodeSize() - codeSize) * sizeof(build.code[0])));
    }

    // Copy assembly locations of IR instructions that are mapped to bytecode instructions
    for (auto& [irLocation, asmLocation] : function.bcMapping)
    {
        if (irLocation != ~0u)
            asmLocation = bcLocations[irLocation];
    }
}

[[maybe_unused]] static void lowerIr(
    X64::AssemblyBuilderX64& build, IrBuilder& ir, NativeState& data, ModuleHelpers& helpers, Proto* proto, AssemblyOptions options)
{
    constexpr uint32_t kFunctionAlignment = 32;

    optimizeMemoryOperandsX64(ir.function);

    build.align(kFunctionAlignment, X64::AlignmentDataX64::Ud2);

    X64::IrLoweringX64 lowering(build, helpers, data, ir.function);

    lowerImpl(build, lowering, ir.function, proto->bytecodeid, options);
}

[[maybe_unused]] static void lowerIr(
    A64::AssemblyBuilderA64& build, IrBuilder& ir, NativeState& data, ModuleHelpers& helpers, Proto* proto, AssemblyOptions options)
{
    if (A64::IrLoweringA64::canLower(ir.function))
    {
        A64::IrLoweringA64 lowering(build, helpers, data, proto, ir.function);

        lowerImpl(build, lowering, ir.function, proto->bytecodeid, options);
    }
    else
    {
        // TODO: This is only needed while we don't support all IR opcodes
        // When we can't translate some parts of the function, we instead encode a dummy assembly sequence that hands off control to VM
        // In the future we could return nullptr from assembleFunction and handle it because there may be other reasons for why we refuse to assemble.
        Label start = build.setLabel();

        build.mov(A64::x0, 1); // finish function in VM
        build.ldr(A64::x1, A64::mem(A64::rNativeContext, offsetof(NativeContext, gateExit)));
        build.br(A64::x1);

        for (int i = 0; i < proto->sizecode; i++)
            ir.function.bcMapping[i].asmLocation = build.getLabelOffset(start);
    }
}

template<typename AssemblyBuilder>
static NativeProto* assembleFunction(AssemblyBuilder& build, NativeState& data, ModuleHelpers& helpers, Proto* proto, AssemblyOptions options)
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
        constPropInBlockChains(ir);
    }

    lowerIr(build, ir, data, helpers, proto, options);

    if (build.logText)
        build.logAppend("\n");

    return createNativeProto(proto, ir);
}

static void destroyNativeProto(NativeProto* nativeProto)
{
    delete[] nativeProto->instTargets;
    delete nativeProto;
}

static void onCloseState(lua_State* L)
{
    destroyNativeState(L);
}

static void onDestroyFunction(lua_State* L, Proto* proto)
{
    NativeProto* nativeProto = getProtoExecData(proto);
    LUAU_ASSERT(nativeProto->proto == proto);

    setProtoExecData(proto, nullptr);
    destroyNativeProto(nativeProto);
}

static int onEnter(lua_State* L, Proto* proto)
{
    if (L->singlestep)
        return 1;

    NativeState* data = getNativeState(L);

    if (!L->ci->savedpc)
        L->ci->savedpc = proto->code;

    // We will jump into native code through a gateway
    bool (*gate)(lua_State*, Proto*, uintptr_t, NativeContext*) = (bool (*)(lua_State*, Proto*, uintptr_t, NativeContext*))data->context.gateEntry;

    NativeProto* nativeProto = getProtoExecData(proto);
    uintptr_t target = nativeProto->instTargets[L->ci->savedpc - proto->code];

    // Returns 1 to finish the function in the VM
    return gate(L, proto, target, &data->context);
}

static void onSetBreakpoint(lua_State* L, Proto* proto, int instruction)
{
    if (!getProtoExecData(proto))
        return;

    LUAU_ASSERT(!"native breakpoints are not implemented");
}

bool isSupported()
{
#if !LUA_CUSTOM_EXECUTION
    return false;
#elif defined(__x86_64__) || defined(_M_X64)
    if (LUA_EXTRA_SIZE != 1)
        return false;

    if (sizeof(TValue) != 16)
        return false;

    if (sizeof(LuaNode) != 32)
        return false;

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
    // TODO: A64 codegen does not generate correct unwind info at the moment so it requires longjmp instead of C++ exceptions
    return bool(LUA_USE_LONGJMP);
#else
    return false;
#endif
}

void create(lua_State* L)
{
    LUAU_ASSERT(isSupported());

    NativeState& data = *createNativeState(L);

#if defined(_WIN32)
    data.unwindBuilder = std::make_unique<UnwindBuilderWin>();
#else
    data.unwindBuilder = std::make_unique<UnwindBuilderDwarf2>();
#endif

    data.codeAllocator.context = data.unwindBuilder.get();
    data.codeAllocator.createBlockUnwindInfo = createBlockUnwindInfo;
    data.codeAllocator.destroyBlockUnwindInfo = destroyBlockUnwindInfo;

    initFallbackTable(data);
    initHelperFunctions(data);

#if defined(__x86_64__) || defined(_M_X64)
    if (!X64::initEntryFunction(data))
    {
        destroyNativeState(L);
        return;
    }
#elif defined(__aarch64__)
    if (!A64::initEntryFunction(data))
    {
        destroyNativeState(L);
        return;
    }
#endif

    lua_ExecutionCallbacks* ecb = getExecutionCallbacks(L);

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
    if (!getNativeState(L))
        return;

#if defined(__aarch64__)
    A64::AssemblyBuilderA64 build(/* logText= */ false);
#else
    X64::AssemblyBuilderX64 build(/* logText= */ false);
#endif

    NativeState* data = getNativeState(L);

    std::vector<Proto*> protos;
    gatherFunctions(protos, clvalue(func)->l.p);

    ModuleHelpers helpers;
#if defined(__aarch64__)
    A64::assembleHelpers(build, helpers);
#else
    X64::assembleHelpers(build, helpers);
#endif

    std::vector<NativeProto*> results;
    results.reserve(protos.size());

    // Skip protos that have been compiled during previous invocations of CodeGen::compile
    for (Proto* p : protos)
        if (p && getProtoExecData(p) == nullptr)
            results.push_back(assembleFunction(build, *data, helpers, p, {}));

    build.finalize();

    uint8_t* nativeData = nullptr;
    size_t sizeNativeData = 0;
    uint8_t* codeStart = nullptr;
    if (!data->codeAllocator.allocate(build.data.data(), int(build.data.size()), reinterpret_cast<const uint8_t*>(build.code.data()),
            int(build.code.size() * sizeof(build.code[0])), nativeData, sizeNativeData, codeStart))
    {
        for (NativeProto* result : results)
            destroyNativeProto(result);

        return;
    }

    // Relocate instruction offsets
    for (NativeProto* result : results)
    {
        for (int i = 0; i < result->proto->sizecode; i++)
            result->instTargets[i] += uintptr_t(codeStart);

        LUAU_ASSERT(result->proto->sizecode);
        result->entryTarget = result->instTargets[0];
    }

    // Link native proto objects to Proto; the memory is now managed by VM and will be freed via onDestroyFunction
    for (NativeProto* result : results)
        setProtoExecData(result->proto, result);
}

std::string getAssembly(lua_State* L, int idx, AssemblyOptions options)
{
    LUAU_ASSERT(lua_isLfunction(L, idx));
    const TValue* func = luaA_toobject(L, idx);

#if defined(__aarch64__)
    A64::AssemblyBuilderA64 build(/* logText= */ options.includeAssembly);
#else
    X64::AssemblyBuilderX64 build(/* logText= */ options.includeAssembly);
#endif

    NativeState data;
    initFallbackTable(data);

    std::vector<Proto*> protos;
    gatherFunctions(protos, clvalue(func)->l.p);

    ModuleHelpers helpers;
#if defined(__aarch64__)
    A64::assembleHelpers(build, helpers);
#else
    X64::assembleHelpers(build, helpers);
#endif

    for (Proto* p : protos)
        if (p)
        {
            NativeProto* nativeProto = assembleFunction(build, data, helpers, p, options);
            destroyNativeProto(nativeProto);
        }

    build.finalize();

    if (options.outputBinary)
        return std::string(reinterpret_cast<const char*>(build.code.data()), reinterpret_cast<const char*>(build.code.data() + build.code.size())) +
               std::string(build.data.begin(), build.data.end());
    else
        return build.text;
}

} // namespace CodeGen
} // namespace Luau
