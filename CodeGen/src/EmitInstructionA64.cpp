// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "EmitInstructionA64.h"

#include "Luau/AssemblyBuilderA64.h"

#include "EmitCommonA64.h"
#include "NativeState.h"
#include "CustomExecUtils.h"

namespace Luau
{
namespace CodeGen
{
namespace A64
{

void emitInstReturn(AssemblyBuilderA64& build, ModuleHelpers& helpers, int ra, int n)
{
    // callFallback(L, ra, n)
    build.mov(x0, rState);
    build.add(x1, rBase, uint16_t(ra * sizeof(TValue)));
    build.mov(w2, n);
    build.ldr(x3, mem(rNativeContext, offsetof(NativeContext, returnFallback)));
    build.blr(x3);

    emitUpdateBase(build);

    // If the fallback requested an exit, we need to do this right away
    build.cbz(x0, helpers.exitNoContinueVm);

    // Need to update state of the current function before we jump away
    build.ldr(x1, mem(rState, offsetof(lua_State, ci)));      // L->ci
    build.ldr(x1, mem(x1, offsetof(CallInfo, func)));         // L->ci->func
    build.ldr(rClosure, mem(x1, offsetof(TValue, value.gc))); // L->ci->func->value.gc aka cl

    build.ldr(x1, mem(rClosure, offsetof(Closure, l.p))); // cl->l.p aka proto

    build.ldr(rConstants, mem(x1, offsetof(Proto, k))); // proto->k
    build.ldr(rCode, mem(x1, offsetof(Proto, code)));   // proto->code

    // Get instruction index from instruction pointer
    // To get instruction index from instruction pointer, we need to divide byte offset by 4
    // But we will actually need to scale instruction index by 8 back to byte offset later so it cancels out
    build.sub(x2, x0, rCode);
    build.add(x2, x2, x2); // TODO: this would not be necessary if we supported shifted register offsets in loads

    // We need to check if the new function can be executed natively
    build.ldr(x1, mem(x1, offsetofProtoExecData));
    build.cbz(x1, helpers.exitContinueVm);

    // Get new instruction location and jump to it
    build.ldr(x1, mem(x1, offsetof(NativeProto, instTargets)));
    build.ldr(x1, mem(x1, x2));
    build.br(x1);
}

} // namespace A64
} // namespace CodeGen
} // namespace Luau
