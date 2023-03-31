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

    // reentry with x0=closure (NULL will trigger exit)
    build.b(helpers.reentry);
}

void emitInstCall(AssemblyBuilderA64& build, ModuleHelpers& helpers, int ra, int nparams, int nresults)
{
    // argtop = (nparams == LUA_MULTRET) ? L->top : ra + 1 + nparams;
    if (nparams == LUA_MULTRET)
        build.ldr(x2, mem(rState, offsetof(lua_State, top)));
    else
        build.add(x2, rBase, uint16_t((ra + 1 + nparams) * sizeof(TValue)));

    // callFallback(L, ra, argtop, nresults)
    build.mov(x0, rState);
    build.add(x1, rBase, uint16_t(ra * sizeof(TValue)));
    build.mov(w3, nresults);
    build.ldr(x4, mem(rNativeContext, offsetof(NativeContext, callFallback)));
    build.blr(x4);

    // reentry with x0=closure (NULL will trigger exit)
    build.b(helpers.reentry);
}

void emitInstGetImport(AssemblyBuilderA64& build, int ra, uint32_t aux)
{
    // luaV_getimport(L, cl->env, k, aux, /* propagatenil= */ false)
    build.mov(x0, rState);
    build.ldr(x1, mem(rClosure, offsetof(Closure, env)));
    build.mov(x2, rConstants);
    build.mov(w3, aux);
    build.mov(w4, 0);
    build.ldr(x5, mem(rNativeContext, offsetof(NativeContext, luaV_getimport)));
    build.blr(x5);

    emitUpdateBase(build);

    // setobj2s(L, ra, L->top - 1)
    build.ldr(x0, mem(rState, offsetof(lua_State, top)));
    build.sub(x0, x0, sizeof(TValue));
    build.ldr(q0, x0);
    build.str(q0, mem(rBase, ra * sizeof(TValue)));

    // L->top--
    build.str(x0, mem(rState, offsetof(lua_State, top)));
}

} // namespace A64
} // namespace CodeGen
} // namespace Luau
