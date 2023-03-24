// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "EmitCommonA64.h"

#include "NativeState.h"
#include "CustomExecUtils.h"

namespace Luau
{
namespace CodeGen
{
namespace A64
{

void emitExit(AssemblyBuilderA64& build, bool continueInVm)
{
    build.mov(x0, continueInVm);
    build.ldr(x1, mem(rNativeContext, offsetof(NativeContext, gateExit)));
    build.br(x1);
}

void emitUpdateBase(AssemblyBuilderA64& build)
{
    build.ldr(rBase, mem(rState, offsetof(lua_State, base)));
}

void emitSetSavedPc(AssemblyBuilderA64& build, int pcpos)
{
    if (pcpos * sizeof(Instruction) <= AssemblyBuilderA64::kMaxImmediate)
    {
        build.add(x0, rCode, uint16_t(pcpos * sizeof(Instruction)));
    }
    else
    {
        build.mov(x0, pcpos * sizeof(Instruction));
        build.add(x0, rCode, x0);
    }

    build.ldr(x1, mem(rState, offsetof(lua_State, ci)));
    build.str(x0, mem(x1, offsetof(CallInfo, savedpc)));
}

void emitInterrupt(AssemblyBuilderA64& build, int pcpos)
{
    Label skip;

    build.ldr(x2, mem(rState, offsetof(lua_State, global)));
    build.ldr(x2, mem(x2, offsetof(global_State, cb.interrupt)));
    build.cbz(x2, skip);

    emitSetSavedPc(build, pcpos + 1); // uses x0/x1

    // Call interrupt
    // TODO: This code should be outlined so that it can be shared by multiple interruptible instructions
    build.mov(x0, rState);
    build.mov(w1, -1);
    build.blr(x2);

    // Check if we need to exit
    build.ldrb(w0, mem(rState, offsetof(lua_State, status)));
    build.cbz(w0, skip);

    // L->ci->savedpc--
    build.ldr(x0, mem(rState, offsetof(lua_State, ci)));
    build.ldr(x1, mem(x0, offsetof(CallInfo, savedpc)));
    build.sub(x1, x1, sizeof(Instruction));
    build.str(x1, mem(x0, offsetof(CallInfo, savedpc)));

    emitExit(build, /* continueInVm */ false);

    build.setLabel(skip);
}

} // namespace A64
} // namespace CodeGen
} // namespace Luau
