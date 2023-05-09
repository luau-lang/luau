// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "CodeGenA64.h"

#include "Luau/AssemblyBuilderA64.h"
#include "Luau/UnwindBuilder.h"

#include "CustomExecUtils.h"
#include "NativeState.h"
#include "EmitCommonA64.h"

#include "lstate.h"

namespace Luau
{
namespace CodeGen
{
namespace A64
{

struct EntryLocations
{
    Label start;
    Label prologueEnd;
    Label epilogueStart;
};

static void emitExit(AssemblyBuilderA64& build, bool continueInVm)
{
    build.mov(x0, continueInVm);
    build.ldr(x1, mem(rNativeContext, offsetof(NativeContext, gateExit)));
    build.br(x1);
}

static void emitInterrupt(AssemblyBuilderA64& build)
{
    // x0 = pc offset
    // x1 = return address in native code

    Label skip;

    // Stash return address in rBase; we need to reload rBase anyway
    build.mov(rBase, x1);

    // Load interrupt handler; it may be nullptr in case the update raced with the check before we got here
    build.ldr(x2, mem(rState, offsetof(lua_State, global)));
    build.ldr(x2, mem(x2, offsetof(global_State, cb.interrupt)));
    build.cbz(x2, skip);

    // Update savedpc; required in case interrupt errors
    build.add(x0, rCode, x0);
    build.ldr(x1, mem(rState, offsetof(lua_State, ci)));
    build.str(x0, mem(x1, offsetof(CallInfo, savedpc)));

    // Call interrupt
    build.mov(x0, rState);
    build.mov(w1, -1);
    build.blr(x2);

    // Check if we need to exit
    build.ldrb(w0, mem(rState, offsetof(lua_State, status)));
    build.cbz(w0, skip);

    // L->ci->savedpc--
    // note: recomputing this avoids having to stash x0
    build.ldr(x1, mem(rState, offsetof(lua_State, ci)));
    build.ldr(x0, mem(x1, offsetof(CallInfo, savedpc)));
    build.sub(x0, x0, sizeof(Instruction));
    build.str(x0, mem(x1, offsetof(CallInfo, savedpc)));

    emitExit(build, /* continueInVm */ false);

    build.setLabel(skip);

    // Return back to caller; rBase has stashed return address
    build.mov(x0, rBase);

    emitUpdateBase(build); // interrupt may have reallocated stack

    build.br(x0);
}

static void emitReentry(AssemblyBuilderA64& build, ModuleHelpers& helpers)
{
    // x0 = closure object to reentry (equal to clvalue(L->ci->func))

    // If the fallback requested an exit, we need to do this right away
    build.cbz(x0, helpers.exitNoContinueVm);

    emitUpdateBase(build);

    // Need to update state of the current function before we jump away
    build.ldr(x1, mem(x0, offsetof(Closure, l.p))); // cl->l.p aka proto

    build.mov(rClosure, x0);
    build.ldr(rConstants, mem(x1, offsetof(Proto, k))); // proto->k
    build.ldr(rCode, mem(x1, offsetof(Proto, code)));   // proto->code

    // Get instruction index from instruction pointer
    // To get instruction index from instruction pointer, we need to divide byte offset by 4
    // But we will actually need to scale instruction index by 4 back to byte offset later so it cancels out
    // Note that we're computing negative offset here (code-savedpc) so that we can add it to NativeProto address, as we use reverse indexing
    build.ldr(x2, mem(rState, offsetof(lua_State, ci))); // L->ci
    build.ldr(x2, mem(x2, offsetof(CallInfo, savedpc))); // L->ci->savedpc
    build.sub(x2, rCode, x2);

    // We need to check if the new function can be executed natively
    // TODO: This can be done earlier in the function flow, to reduce the JIT->VM transition penalty
    build.ldr(x1, mem(x1, offsetofProtoExecData));
    build.cbz(x1, helpers.exitContinueVm);

    // Get new instruction location and jump to it
    LUAU_ASSERT(offsetof(NativeProto, instOffsets) == 0);
    build.ldr(w2, mem(x1, x2));
    build.ldr(x1, mem(x1, offsetof(NativeProto, instBase)));
    build.add(x1, x1, x2);
    build.br(x1);
}

static EntryLocations buildEntryFunction(AssemblyBuilderA64& build, UnwindBuilder& unwind)
{
    EntryLocations locations;

    // Arguments: x0 = lua_State*, x1 = Proto*, x2 = native code pointer to jump to, x3 = NativeContext*

    locations.start = build.setLabel();

    // prologue
    build.sub(sp, sp, kStackSize);
    build.stp(x29, x30, mem(sp)); // fp, lr

    // stash non-volatile registers used for execution environment
    build.stp(x19, x20, mem(sp, 16));
    build.stp(x21, x22, mem(sp, 32));
    build.stp(x23, x24, mem(sp, 48));

    build.mov(x29, sp); // this is only necessary if we maintain frame pointers, which we do in the JIT for now

    locations.prologueEnd = build.setLabel();

    uint32_t prologueSize = build.getLabelOffset(locations.prologueEnd) - build.getLabelOffset(locations.start);

    // Setup native execution environment
    build.mov(rState, x0);
    build.mov(rNativeContext, x3);

    build.ldr(rBase, mem(x0, offsetof(lua_State, base))); // L->base
    build.ldr(rConstants, mem(x1, offsetof(Proto, k)));   // proto->k
    build.ldr(rCode, mem(x1, offsetof(Proto, code)));     // proto->code

    build.ldr(x9, mem(x0, offsetof(lua_State, ci)));          // L->ci
    build.ldr(x9, mem(x9, offsetof(CallInfo, func)));         // L->ci->func
    build.ldr(rClosure, mem(x9, offsetof(TValue, value.gc))); // L->ci->func->value.gc aka cl

    // Jump to the specified instruction; further control flow will be handled with custom ABI with register setup from EmitCommonA64.h
    build.br(x2);

    // Even though we jumped away, we will return here in the end
    locations.epilogueStart = build.setLabel();

    // Cleanup and exit
    build.ldp(x23, x24, mem(sp, 48));
    build.ldp(x21, x22, mem(sp, 32));
    build.ldp(x19, x20, mem(sp, 16));
    build.ldp(x29, x30, mem(sp)); // fp, lr
    build.add(sp, sp, kStackSize);

    build.ret();

    // Our entry function is special, it spans the whole remaining code area
    unwind.startFunction();
    unwind.prologueA64(prologueSize, kStackSize, {x29, x30, x19, x20, x21, x22, x23, x24});
    unwind.finishFunction(build.getLabelOffset(locations.start), kFullBlockFuncton);

    return locations;
}

bool initHeaderFunctions(NativeState& data)
{
    AssemblyBuilderA64 build(/* logText= */ false);
    UnwindBuilder& unwind = *data.unwindBuilder.get();

    unwind.startInfo(UnwindBuilder::A64);

    EntryLocations entryLocations = buildEntryFunction(build, unwind);

    build.finalize();

    unwind.finishInfo();

    LUAU_ASSERT(build.data.empty());

    uint8_t* codeStart = nullptr;
    if (!data.codeAllocator.allocate(build.data.data(), int(build.data.size()), reinterpret_cast<const uint8_t*>(build.code.data()),
            int(build.code.size() * sizeof(build.code[0])), data.gateData, data.gateDataSize, codeStart))
    {
        LUAU_ASSERT(!"failed to create entry function");
        return false;
    }

    // Set the offset at the begining so that functions in new blocks will not overlay the locations
    // specified by the unwind information of the entry function
    unwind.setBeginOffset(build.getLabelOffset(entryLocations.prologueEnd));

    data.context.gateEntry = codeStart + build.getLabelOffset(entryLocations.start);
    data.context.gateExit = codeStart + build.getLabelOffset(entryLocations.epilogueStart);

    return true;
}

void assembleHelpers(AssemblyBuilderA64& build, ModuleHelpers& helpers)
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
        build.logAppend("; reentry\n");
    helpers.reentry = build.setLabel();
    emitReentry(build, helpers);

    if (build.logText)
        build.logAppend("; interrupt\n");
    helpers.interrupt = build.setLabel();
    emitInterrupt(build);
}

} // namespace A64
} // namespace CodeGen
} // namespace Luau
