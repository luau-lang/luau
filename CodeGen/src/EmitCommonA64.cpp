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

void emitUpdateBase(AssemblyBuilderA64& build)
{
    build.ldr(rBase, mem(rState, offsetof(lua_State, base)));
}

void emitExit(AssemblyBuilderA64& build, bool continueInVm)
{
    build.mov(x0, continueInVm);
    build.ldr(x1, mem(rNativeContext, offsetof(NativeContext, gateExit)));
    build.br(x1);
}

void emitInterrupt(AssemblyBuilderA64& build)
{
    // x0 = pc offset
    // x1 = return address in native code
    // x2 = interrupt

    // Stash return address in rBase; we need to reload rBase anyway
    build.mov(rBase, x1);

    // Update savedpc; required in case interrupt errors
    build.add(x0, rCode, x0);
    build.ldr(x1, mem(rState, offsetof(lua_State, ci)));
    build.str(x0, mem(x1, offsetof(CallInfo, savedpc)));

    // Call interrupt
    build.mov(x0, rState);
    build.mov(w1, -1);
    build.blr(x2);

    // Check if we need to exit
    Label skip;
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

void emitReentry(AssemblyBuilderA64& build, ModuleHelpers& helpers)
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
    // But we will actually need to scale instruction index by 8 back to byte offset later so it cancels out
    build.ldr(x2, mem(rState, offsetof(lua_State, ci))); // L->ci
    build.ldr(x2, mem(x2, offsetof(CallInfo, savedpc))); // L->ci->savedpc
    build.sub(x2, x2, rCode);
    build.add(x2, x2, x2); // TODO: this would not be necessary if we supported shifted register offsets in loads

    // We need to check if the new function can be executed natively
    // TODO: This can be done earlier in the function flow, to reduce the JIT->VM transition penalty
    build.ldr(x1, mem(x1, offsetofProtoExecData));
    build.cbz(x1, helpers.exitContinueVm);

    // Get new instruction location and jump to it
    build.ldr(x1, mem(x1, offsetof(NativeProto, instTargets)));
    build.ldr(x1, mem(x1, x2));
    build.br(x1);
}

void emitFallback(AssemblyBuilderA64& build, int op, int pcpos)
{
    // fallback(L, instruction, base, k)
    build.mov(x0, rState);

    // TODO: refactor into a common helper
    if (pcpos * sizeof(Instruction) <= AssemblyBuilderA64::kMaxImmediate)
    {
        build.add(x1, rCode, uint16_t(pcpos * sizeof(Instruction)));
    }
    else
    {
        build.mov(x1, pcpos * sizeof(Instruction));
        build.add(x1, rCode, x1);
    }

    build.mov(x2, rBase);
    build.mov(x3, rConstants);
    build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, fallback) + op * sizeof(NativeFallback) + offsetof(NativeFallback, fallback)));
    build.blr(x4);

    emitUpdateBase(build);
}

} // namespace A64
} // namespace CodeGen
} // namespace Luau
