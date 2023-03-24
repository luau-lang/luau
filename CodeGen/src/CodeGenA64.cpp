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

bool initEntryFunction(NativeState& data)
{
    AssemblyBuilderA64 build(/* logText= */ false);
    UnwindBuilder& unwind = *data.unwindBuilder.get();

    // Arguments: x0 = lua_State*, x1 = Proto*, x2 = native code pointer to jump to, x3 = NativeContext*

    unwind.start();
    unwind.allocStack(8); // TODO: this is just a hack to make UnwindBuilder assertions cooperate

    // prologue
    build.sub(sp, sp, kStackSize);
    build.stp(x29, x30, mem(sp)); // fp, lr

    // stash non-volatile registers used for execution environment
    build.stp(x19, x20, mem(sp, 16));
    build.stp(x21, x22, mem(sp, 32));
    build.stp(x23, x24, mem(sp, 48));

    build.mov(x29, sp); // this is only necessary if we maintain frame pointers, which we do in the JIT for now

    unwind.finish();

    size_t prologueSize = build.setLabel().location;

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
    Label returnOff = build.setLabel();

    // Cleanup and exit
    build.ldp(x23, x24, mem(sp, 48));
    build.ldp(x21, x22, mem(sp, 32));
    build.ldp(x19, x20, mem(sp, 16));
    build.ldp(x29, x30, mem(sp)); // fp, lr
    build.add(sp, sp, kStackSize);

    build.ret();

    build.finalize();

    LUAU_ASSERT(build.data.empty());

    if (!data.codeAllocator.allocate(build.data.data(), int(build.data.size()), reinterpret_cast<const uint8_t*>(build.code.data()),
            int(build.code.size() * sizeof(build.code[0])), data.gateData, data.gateDataSize, data.context.gateEntry))
    {
        LUAU_ASSERT(!"failed to create entry function");
        return false;
    }

    // Set the offset at the begining so that functions in new blocks will not overlay the locations
    // specified by the unwind information of the entry function
    unwind.setBeginOffset(prologueSize);

    data.context.gateExit = data.context.gateEntry + build.getLabelOffset(returnOff);

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
}

} // namespace A64
} // namespace CodeGen
} // namespace Luau
