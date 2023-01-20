// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/CodeGen.h"

#include "Luau/AssemblyBuilderX64.h"
#include "Luau/Common.h"
#include "Luau/CodeAllocator.h"
#include "Luau/CodeBlockUnwind.h"
#include "Luau/UnwindBuilder.h"
#include "Luau/UnwindBuilderDwarf2.h"
#include "Luau/UnwindBuilderWin.h"

#include "CustomExecUtils.h"
#include "CodeGenX64.h"
#include "EmitCommonX64.h"
#include "EmitInstructionX64.h"
#include "NativeState.h"

#include "lapi.h"

#include <memory>

#if defined(__x86_64__) || defined(_M_X64)
#ifdef _MSC_VER
#include <intrin.h> // __cpuid
#else
#include <cpuid.h> // __cpuid
#endif
#endif

namespace Luau
{
namespace CodeGen
{

constexpr uint32_t kFunctionAlignment = 32;

struct InstructionOutline
{
    int pcpos;
    int length;
};

static void assembleHelpers(AssemblyBuilderX64& build, ModuleHelpers& helpers)
{
    if (build.logText)
        build.logAppend("; exitContinueVm\n");
    helpers.exitContinueVm = build.setLabel();
    emitExit(build, /* continueInVm */ true);

    if (build.logText)
        build.logAppend("; exitNoContinueVm\n");
    helpers.exitNoContinueVm = build.setLabel();
    emitExit(build, /* continueInVm */ false);

    if (build.logText)
        build.logAppend("; continueCallInVm\n");
    helpers.continueCallInVm = build.setLabel();
    emitContinueCallInVm(build);
}

static int emitInst(AssemblyBuilderX64& build, NativeState& data, ModuleHelpers& helpers, Proto* proto, LuauOpcode op, const Instruction* pc, int i,
    Label* labelarr, Label& next, Label& fallback)
{
    int skip = 0;

    switch (op)
    {
    case LOP_NOP:
        break;
    case LOP_LOADNIL:
        emitInstLoadNil(build, pc);
        break;
    case LOP_LOADB:
        emitInstLoadB(build, pc, i, labelarr);
        break;
    case LOP_LOADN:
        emitInstLoadN(build, pc);
        break;
    case LOP_LOADK:
        emitInstLoadK(build, pc);
        break;
    case LOP_LOADKX:
        emitInstLoadKX(build, pc);
        break;
    case LOP_MOVE:
        emitInstMove(build, pc);
        break;
    case LOP_GETGLOBAL:
        emitInstGetGlobal(build, pc, i, fallback);
        break;
    case LOP_SETGLOBAL:
        emitInstSetGlobal(build, pc, i, next, fallback);
        break;
    case LOP_NAMECALL:
        emitInstNameCall(build, pc, i, proto->k, next, fallback);
        break;
    case LOP_CALL:
        emitInstCall(build, helpers, pc, i);
        break;
    case LOP_RETURN:
        emitInstReturn(build, helpers, pc, i);
        break;
    case LOP_GETTABLE:
        emitInstGetTable(build, pc, fallback);
        break;
    case LOP_SETTABLE:
        emitInstSetTable(build, pc, next, fallback);
        break;
    case LOP_GETTABLEKS:
        emitInstGetTableKS(build, pc, i, fallback);
        break;
    case LOP_SETTABLEKS:
        emitInstSetTableKS(build, pc, i, next, fallback);
        break;
    case LOP_GETTABLEN:
        emitInstGetTableN(build, pc, fallback);
        break;
    case LOP_SETTABLEN:
        emitInstSetTableN(build, pc, next, fallback);
        break;
    case LOP_JUMP:
        emitInstJump(build, pc, i, labelarr);
        break;
    case LOP_JUMPBACK:
        emitInstJumpBack(build, pc, i, labelarr);
        break;
    case LOP_JUMPIF:
        emitInstJumpIf(build, pc, i, labelarr, /* not_ */ false);
        break;
    case LOP_JUMPIFNOT:
        emitInstJumpIf(build, pc, i, labelarr, /* not_ */ true);
        break;
    case LOP_JUMPIFEQ:
        emitInstJumpIfEq(build, pc, i, labelarr, /* not_ */ false, fallback);
        break;
    case LOP_JUMPIFLE:
        emitInstJumpIfCond(build, pc, i, labelarr, ConditionX64::LessEqual, fallback);
        break;
    case LOP_JUMPIFLT:
        emitInstJumpIfCond(build, pc, i, labelarr, ConditionX64::Less, fallback);
        break;
    case LOP_JUMPIFNOTEQ:
        emitInstJumpIfEq(build, pc, i, labelarr, /* not_ */ true, fallback);
        break;
    case LOP_JUMPIFNOTLE:
        emitInstJumpIfCond(build, pc, i, labelarr, ConditionX64::NotLessEqual, fallback);
        break;
    case LOP_JUMPIFNOTLT:
        emitInstJumpIfCond(build, pc, i, labelarr, ConditionX64::NotLess, fallback);
        break;
    case LOP_JUMPX:
        emitInstJumpX(build, pc, i, labelarr);
        break;
    case LOP_JUMPXEQKNIL:
        emitInstJumpxEqNil(build, pc, i, labelarr);
        break;
    case LOP_JUMPXEQKB:
        emitInstJumpxEqB(build, pc, i, labelarr);
        break;
    case LOP_JUMPXEQKN:
        emitInstJumpxEqN(build, pc, proto->k, i, labelarr);
        break;
    case LOP_JUMPXEQKS:
        emitInstJumpxEqS(build, pc, i, labelarr);
        break;
    case LOP_ADD:
        emitInstBinary(build, pc, TM_ADD, fallback);
        break;
    case LOP_SUB:
        emitInstBinary(build, pc, TM_SUB, fallback);
        break;
    case LOP_MUL:
        emitInstBinary(build, pc, TM_MUL, fallback);
        break;
    case LOP_DIV:
        emitInstBinary(build, pc, TM_DIV, fallback);
        break;
    case LOP_MOD:
        emitInstBinary(build, pc, TM_MOD, fallback);
        break;
    case LOP_POW:
        emitInstBinary(build, pc, TM_POW, fallback);
        break;
    case LOP_ADDK:
        emitInstBinaryK(build, pc, TM_ADD, fallback);
        break;
    case LOP_SUBK:
        emitInstBinaryK(build, pc, TM_SUB, fallback);
        break;
    case LOP_MULK:
        emitInstBinaryK(build, pc, TM_MUL, fallback);
        break;
    case LOP_DIVK:
        emitInstBinaryK(build, pc, TM_DIV, fallback);
        break;
    case LOP_MODK:
        emitInstBinaryK(build, pc, TM_MOD, fallback);
        break;
    case LOP_POWK:
        emitInstPowK(build, pc, proto->k, fallback);
        break;
    case LOP_NOT:
        emitInstNot(build, pc);
        break;
    case LOP_MINUS:
        emitInstMinus(build, pc, fallback);
        break;
    case LOP_LENGTH:
        emitInstLength(build, pc, fallback);
        break;
    case LOP_NEWTABLE:
        emitInstNewTable(build, pc, i, next);
        break;
    case LOP_DUPTABLE:
        emitInstDupTable(build, pc, i, next);
        break;
    case LOP_SETLIST:
        emitInstSetList(build, pc, next);
        break;
    case LOP_GETUPVAL:
        emitInstGetUpval(build, pc);
        break;
    case LOP_SETUPVAL:
        emitInstSetUpval(build, pc, next);
        break;
    case LOP_CLOSEUPVALS:
        emitInstCloseUpvals(build, pc, next);
        break;
    case LOP_FASTCALL:
        // We want to lower next instruction at skip+2, but this instruction is only 1 long, so we need to add 1
        skip = emitInstFastCall(build, pc, i, next) + 1;
        break;
    case LOP_FASTCALL1:
        // We want to lower next instruction at skip+2, but this instruction is only 1 long, so we need to add 1
        skip = emitInstFastCall1(build, pc, i, next) + 1;
        break;
    case LOP_FASTCALL2:
        skip = emitInstFastCall2(build, pc, i, next);
        break;
    case LOP_FASTCALL2K:
        skip = emitInstFastCall2K(build, pc, i, next);
        break;
    case LOP_FORNPREP:
        emitInstForNPrep(build, pc, i, labelarr[i + 1 + LUAU_INSN_D(*pc)]);
        break;
    case LOP_FORNLOOP:
        emitInstForNLoop(build, pc, i, labelarr[i + 1 + LUAU_INSN_D(*pc)], next);
        break;
    case LOP_FORGLOOP:
        emitinstForGLoop(build, pc, i, labelarr[i + 1 + LUAU_INSN_D(*pc)], next, fallback);
        break;
    case LOP_FORGPREP_NEXT:
        emitInstForGPrepNext(build, pc, labelarr[i + 1 + LUAU_INSN_D(*pc)], fallback);
        break;
    case LOP_FORGPREP_INEXT:
        emitInstForGPrepInext(build, pc, labelarr[i + 1 + LUAU_INSN_D(*pc)], fallback);
        break;
    case LOP_AND:
        emitInstAnd(build, pc);
        break;
    case LOP_ANDK:
        emitInstAndK(build, pc);
        break;
    case LOP_OR:
        emitInstOr(build, pc);
        break;
    case LOP_ORK:
        emitInstOrK(build, pc);
        break;
    case LOP_GETIMPORT:
        emitInstGetImport(build, pc, fallback);
        break;
    case LOP_CONCAT:
        emitInstConcat(build, pc, i, next);
        break;
    case LOP_COVERAGE:
        emitInstCoverage(build, i);
        break;
    default:
        emitFallback(build, data, op, i);
        break;
    }

    return skip;
}

static void emitInstFallback(AssemblyBuilderX64& build, NativeState& data, LuauOpcode op, const Instruction* pc, int i, Label* labelarr)
{
    switch (op)
    {
    case LOP_GETIMPORT:
        emitSetSavedPc(build, i + 1);
        emitInstGetImportFallback(build, LUAU_INSN_A(*pc), pc[1]);
        break;
    case LOP_GETTABLE:
        emitInstGetTableFallback(build, pc, i);
        break;
    case LOP_SETTABLE:
        emitInstSetTableFallback(build, pc, i);
        break;
    case LOP_GETTABLEN:
        emitInstGetTableNFallback(build, pc, i);
        break;
    case LOP_SETTABLEN:
        emitInstSetTableNFallback(build, pc, i);
        break;
    case LOP_NAMECALL:
        // TODO: fast-paths that we've handled can be removed from the fallback
        emitFallback(build, data, op, i);
        break;
    case LOP_JUMPIFEQ:
        emitInstJumpIfEqFallback(build, pc, i, labelarr, /* not_ */ false);
        break;
    case LOP_JUMPIFLE:
        emitInstJumpIfCondFallback(build, pc, i, labelarr, ConditionX64::LessEqual);
        break;
    case LOP_JUMPIFLT:
        emitInstJumpIfCondFallback(build, pc, i, labelarr, ConditionX64::Less);
        break;
    case LOP_JUMPIFNOTEQ:
        emitInstJumpIfEqFallback(build, pc, i, labelarr, /* not_ */ true);
        break;
    case LOP_JUMPIFNOTLE:
        emitInstJumpIfCondFallback(build, pc, i, labelarr, ConditionX64::NotLessEqual);
        break;
    case LOP_JUMPIFNOTLT:
        emitInstJumpIfCondFallback(build, pc, i, labelarr, ConditionX64::NotLess);
        break;
    case LOP_ADD:
        emitInstBinaryFallback(build, pc, i, TM_ADD);
        break;
    case LOP_SUB:
        emitInstBinaryFallback(build, pc, i, TM_SUB);
        break;
    case LOP_MUL:
        emitInstBinaryFallback(build, pc, i, TM_MUL);
        break;
    case LOP_DIV:
        emitInstBinaryFallback(build, pc, i, TM_DIV);
        break;
    case LOP_MOD:
        emitInstBinaryFallback(build, pc, i, TM_MOD);
        break;
    case LOP_POW:
        emitInstBinaryFallback(build, pc, i, TM_POW);
        break;
    case LOP_ADDK:
        emitInstBinaryKFallback(build, pc, i, TM_ADD);
        break;
    case LOP_SUBK:
        emitInstBinaryKFallback(build, pc, i, TM_SUB);
        break;
    case LOP_MULK:
        emitInstBinaryKFallback(build, pc, i, TM_MUL);
        break;
    case LOP_DIVK:
        emitInstBinaryKFallback(build, pc, i, TM_DIV);
        break;
    case LOP_MODK:
        emitInstBinaryKFallback(build, pc, i, TM_MOD);
        break;
    case LOP_POWK:
        emitInstBinaryKFallback(build, pc, i, TM_POW);
        break;
    case LOP_MINUS:
        emitInstMinusFallback(build, pc, i);
        break;
    case LOP_LENGTH:
        emitInstLengthFallback(build, pc, i);
        break;
    case LOP_FORGLOOP:
        emitinstForGLoopFallback(build, pc, i, labelarr[i + 1 + LUAU_INSN_D(*pc)]);
        break;
    case LOP_FORGPREP_NEXT:
    case LOP_FORGPREP_INEXT:
        emitInstForGPrepXnextFallback(build, pc, i, labelarr[i + 1 + LUAU_INSN_D(*pc)]);
        break;
    case LOP_GETGLOBAL:
        // TODO: luaV_gettable + cachedslot update instead of full fallback
        emitFallback(build, data, op, i);
        break;
    case LOP_SETGLOBAL:
        // TODO: luaV_settable + cachedslot update instead of full fallback
        emitFallback(build, data, op, i);
        break;
    case LOP_GETTABLEKS:
        // Full fallback required for LOP_GETTABLEKS because 'luaV_gettable' doesn't handle builtin vector field access
        // It is also required to perform cached slot update
        // TODO: extra fast-paths could be lowered before the full fallback
        emitFallback(build, data, op, i);
        break;
    case LOP_SETTABLEKS:
        // TODO: luaV_settable + cachedslot update instead of full fallback
        emitFallback(build, data, op, i);
        break;
    default:
        LUAU_ASSERT(!"Expected fallback for instruction");
    }
}

static NativeProto* assembleFunction(AssemblyBuilderX64& build, NativeState& data, ModuleHelpers& helpers, Proto* proto, AssemblyOptions options)
{
    NativeProto* result = new NativeProto();

    result->proto = proto;

    if (build.logText)
    {
        if (proto->debugname)
            build.logAppend("; function %s()", getstr(proto->debugname));
        else
            build.logAppend("; function()");

        if (proto->linedefined >= 0)
            build.logAppend(" line %d\n", proto->linedefined);
        else
            build.logAppend("\n");
    }

    std::vector<Label> instLabels;
    instLabels.resize(proto->sizecode);

    std::vector<Label> instFallbacks;
    instFallbacks.resize(proto->sizecode);

    std::vector<InstructionOutline> instOutlines;
    instOutlines.reserve(64);

    build.align(kFunctionAlignment, AlignmentDataX64::Ud2);

    Label start = build.setLabel();

    for (int i = 0; i < proto->sizecode;)
    {
        const Instruction* pc = &proto->code[i];
        LuauOpcode op = LuauOpcode(LUAU_INSN_OP(*pc));

        int nexti = i + getOpLength(op);
        LUAU_ASSERT(nexti <= proto->sizecode);

        build.setLabel(instLabels[i]);

        if (options.annotator)
            options.annotator(options.annotatorContext, build.text, proto->bytecodeid, i);

        Label& next = nexti < proto->sizecode ? instLabels[nexti] : start; // Last instruction can't use 'next' label

        int skip = emitInst(build, data, helpers, proto, op, pc, i, instLabels.data(), next, instFallbacks[i]);

        if (skip != 0)
            instOutlines.push_back({nexti, skip});

        i = nexti + skip;
        LUAU_ASSERT(i <= proto->sizecode);
    }

    size_t textSize = build.text.size();
    uint32_t codeSize = build.getCodeSize();

    if (options.annotator && !options.skipOutlinedCode)
        build.logAppend("; outlined instructions\n");

    for (auto [pcpos, length] : instOutlines)
    {
        int i = pcpos;

        while (i < pcpos + length)
        {
            const Instruction* pc = &proto->code[i];
            LuauOpcode op = LuauOpcode(LUAU_INSN_OP(*pc));

            int nexti = i + getOpLength(op);
            LUAU_ASSERT(nexti <= proto->sizecode);

            build.setLabel(instLabels[i]);

            if (options.annotator && !options.skipOutlinedCode)
                options.annotator(options.annotatorContext, build.text, proto->bytecodeid, i);

            Label& next = nexti < proto->sizecode ? instLabels[nexti] : start; // Last instruction can't use 'next' label

            int skip = emitInst(build, data, helpers, proto, op, pc, i, instLabels.data(), next, instFallbacks[i]);
            LUAU_ASSERT(skip == 0);

            i = nexti;
        }

        if (i < proto->sizecode)
            build.jmp(instLabels[i]);
    }

    if (options.annotator && !options.skipOutlinedCode)
        build.logAppend("; outlined code\n");

    for (int i = 0, instid = 0; i < proto->sizecode; ++instid)
    {
        const Instruction* pc = &proto->code[i];
        LuauOpcode op = LuauOpcode(LUAU_INSN_OP(*pc));

        int nexti = i + getOpLength(op);
        LUAU_ASSERT(nexti <= proto->sizecode);

        if (instFallbacks[i].id == 0)
        {
            i = nexti;
            continue;
        }

        if (options.annotator && !options.skipOutlinedCode)
            options.annotator(options.annotatorContext, build.text, proto->bytecodeid, instid);

        build.setLabel(instFallbacks[i]);

        emitInstFallback(build, data, op, pc, i, instLabels.data());

        // Jump back to the next instruction handler
        if (nexti < proto->sizecode)
            build.jmp(instLabels[nexti]);

        i = nexti;
    }

    // Truncate assembly output if we don't care for outlined code part
    if (options.skipOutlinedCode)
    {
        build.text.resize(textSize);

        build.logAppend("; skipping %u bytes of outlined code\n", build.getCodeSize() - codeSize);
    }

    result->instTargets = new uintptr_t[proto->sizecode];

    for (int i = 0; i < proto->sizecode; i++)
        result->instTargets[i] = instLabels[i].location - start.location;

    result->location = start.location;

    if (build.logText)
        build.logAppend("\n");

    return result;
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

    if (!x64::initEntryFunction(data))
    {
        destroyNativeState(L);
        return;
    }

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

    AssemblyBuilderX64 build(/* logText= */ false);
    NativeState* data = getNativeState(L);

    std::vector<Proto*> protos;
    gatherFunctions(protos, clvalue(func)->l.p);

    ModuleHelpers helpers;
    assembleHelpers(build, helpers);

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
    if (!data->codeAllocator.allocate(
            build.data.data(), int(build.data.size()), build.code.data(), int(build.code.size()), nativeData, sizeNativeData, codeStart))
    {
        for (NativeProto* result : results)
            destroyNativeProto(result);

        return;
    }

    // Relocate instruction offsets
    for (NativeProto* result : results)
    {
        for (int i = 0; i < result->proto->sizecode; i++)
            result->instTargets[i] += uintptr_t(codeStart + result->location);

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

    AssemblyBuilderX64 build(/* logText= */ !options.outputBinary);

    NativeState data;
    initFallbackTable(data);

    std::vector<Proto*> protos;
    gatherFunctions(protos, clvalue(func)->l.p);

    ModuleHelpers helpers;
    assembleHelpers(build, helpers);

    for (Proto* p : protos)
        if (p)
        {
            NativeProto* nativeProto = assembleFunction(build, data, helpers, p, options);
            destroyNativeProto(nativeProto);
        }

    build.finalize();

    if (options.outputBinary)
        return std::string(build.code.begin(), build.code.end()) + std::string(build.data.begin(), build.data.end());
    else
        return build.text;
}

} // namespace CodeGen
} // namespace Luau
