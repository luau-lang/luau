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

static NativeProto* assembleFunction(AssemblyBuilderX64& build, NativeState& data, Proto* proto)
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

    Label start = build.setLabel();

    for (int i = 0; i < proto->sizecode;)
    {
        const Instruction* pc = &proto->code[i];
        LuauOpcode op = LuauOpcode(LUAU_INSN_OP(*pc));

        build.setLabel(instLabels[i]);

        if (build.logText)
            build.logAppend("; #%d: %s\n", i, data.names[op]);

        switch (op)
        {
        case LOP_NOP:
            break;
        case LOP_LOADNIL:
            emitInstLoadNil(build, data, pc);
            break;
        case LOP_LOADB:
            emitInstLoadB(build, data, pc, i, instLabels.data());
            break;
        case LOP_LOADN:
            emitInstLoadN(build, data, pc);
            break;
        case LOP_LOADK:
            emitInstLoadK(build, data, pc, proto->k);
            break;
        case LOP_MOVE:
            emitInstMove(build, data, pc);
            break;
        case LOP_GETTABLE:
            emitInstGetTable(build, pc, i);
            break;
        case LOP_SETTABLE:
            emitInstSetTable(build, pc, i);
            break;
        case LOP_GETTABLEN:
            emitInstGetTableN(build, pc, i);
            break;
        case LOP_SETTABLEN:
            emitInstSetTableN(build, pc, i);
            break;
        case LOP_JUMP:
            emitInstJump(build, data, pc, i, instLabels.data());
            break;
        case LOP_JUMPBACK:
            emitInstJumpBack(build, data, pc, i, instLabels.data());
            break;
        case LOP_JUMPIF:
            emitInstJumpIf(build, data, pc, i, instLabels.data(), /* not_ */ false);
            break;
        case LOP_JUMPIFNOT:
            emitInstJumpIf(build, data, pc, i, instLabels.data(), /* not_ */ true);
            break;
        case LOP_JUMPIFEQ:
            emitInstJumpIfEq(build, data, pc, i, instLabels.data(), /* not_ */ false);
            break;
        case LOP_JUMPIFLE:
            emitInstJumpIfCond(build, data, pc, i, instLabels.data(), Condition::LessEqual);
            break;
        case LOP_JUMPIFLT:
            emitInstJumpIfCond(build, data, pc, i, instLabels.data(), Condition::Less);
            break;
        case LOP_JUMPIFNOTEQ:
            emitInstJumpIfEq(build, data, pc, i, instLabels.data(), /* not_ */ true);
            break;
        case LOP_JUMPIFNOTLE:
            emitInstJumpIfCond(build, data, pc, i, instLabels.data(), Condition::NotLessEqual);
            break;
        case LOP_JUMPIFNOTLT:
            emitInstJumpIfCond(build, data, pc, i, instLabels.data(), Condition::NotLess);
            break;
        case LOP_JUMPX:
            emitInstJumpX(build, data, pc, i, instLabels.data());
            break;
        case LOP_JUMPXEQKNIL:
            emitInstJumpxEqNil(build, data, pc, proto->k, i, instLabels.data());
            break;
        case LOP_JUMPXEQKB:
            emitInstJumpxEqB(build, data, pc, proto->k, i, instLabels.data());
            break;
        case LOP_JUMPXEQKN:
            emitInstJumpxEqN(build, data, pc, proto->k, i, instLabels.data());
            break;
        case LOP_JUMPXEQKS:
            emitInstJumpxEqS(build, data, pc, proto->k, i, instLabels.data());
            break;
        case LOP_ADD:
            emitInstAdd(build, pc, i);
            break;
        case LOP_SUB:
            emitInstSub(build, pc, i);
            break;
        case LOP_MUL:
            emitInstMul(build, pc, i);
            break;
        case LOP_DIV:
            emitInstDiv(build, pc, i);
            break;
        case LOP_MOD:
            emitInstMod(build, pc, i);
            break;
        case LOP_POW:
            emitInstPow(build, pc, i);
            break;
        case LOP_ADDK:
            emitInstAddK(build, pc, proto->k, i);
            break;
        case LOP_SUBK:
            emitInstSubK(build, pc, proto->k, i);
            break;
        case LOP_MULK:
            emitInstMulK(build, pc, proto->k, i);
            break;
        case LOP_DIVK:
            emitInstDivK(build, pc, proto->k, i);
            break;
        case LOP_MODK:
            emitInstModK(build, pc, proto->k, i);
            break;
        case LOP_POWK:
            emitInstPowK(build, pc, proto->k, i);
            break;
        case LOP_NOT:
            emitInstNot(build, pc);
            break;
        case LOP_MINUS:
            emitInstMinus(build, pc, i);
            break;
        case LOP_LENGTH:
            emitInstLength(build, pc, i);
            break;
        case LOP_GETUPVAL:
            emitInstGetUpval(build, pc, i);
            break;
        case LOP_FASTCALL:
            emitInstFastCall(build, pc, i, instLabels.data());
            break;
        case LOP_FASTCALL1:
            emitInstFastCall1(build, pc, i, instLabels.data());
            break;
        case LOP_FASTCALL2:
            emitInstFastCall2(build, pc, i, instLabels.data());
            break;
        case LOP_FASTCALL2K:
            emitInstFastCall2K(build, pc, proto->k, i, instLabels.data());
            break;
        case LOP_FORNPREP:
            emitInstForNPrep(build, pc, i, instLabels.data());
            break;
        case LOP_FORNLOOP:
            emitInstForNLoop(build, pc, i, instLabels.data());
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
        default:
            emitFallback(build, data, op, i);
            break;
        }

        i += getOpLength(op);
        LUAU_ASSERT(i <= proto->sizecode);
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

    std::vector<NativeProto*> results;
    results.reserve(protos.size());

    // Skip protos that have been compiled during previous invocations of CodeGen::compile
    for (Proto* p : protos)
        if (p && getProtoExecData(p) == nullptr)
            results.push_back(assembleFunction(build, *data, p));

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
    }

    // Link native proto objects to Proto; the memory is now managed by VM and will be freed via onDestroyFunction
    for (NativeProto* result : results)
        setProtoExecData(result->proto, result);
}

std::string getAssemblyText(lua_State* L, int idx)
{
    LUAU_ASSERT(lua_isLfunction(L, idx));
    const TValue* func = luaA_toobject(L, idx);

    AssemblyBuilderX64 build(/* logText= */ true);
    NativeState data;
    initFallbackTable(data);
    initInstructionNames(data);

    std::vector<Proto*> protos;
    gatherFunctions(protos, clvalue(func)->l.p);

    for (Proto* p : protos)
        if (p)
        {
            NativeProto* nativeProto = assembleFunction(build, data, p);
            destroyNativeProto(nativeProto);
        }

    build.finalize();

    return build.text;
}

} // namespace CodeGen
} // namespace Luau
