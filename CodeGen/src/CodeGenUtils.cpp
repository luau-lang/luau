// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "CodeGenUtils.h"

#include "ldo.h"
#include "ltable.h"

#include "FallbacksProlog.h"

#include <string.h>

namespace Luau
{
namespace CodeGen
{

bool forgLoopNodeIter(lua_State* L, Table* h, int index, TValue* ra)
{
    // then we advance index through the hash portion
    while (unsigned(index - h->sizearray) < unsigned(1 << h->lsizenode))
    {
        LuaNode* n = &h->node[index - h->sizearray];

        if (!ttisnil(gval(n)))
        {
            setpvalue(ra + 2, reinterpret_cast<void*>(uintptr_t(index + 1)));
            getnodekey(L, ra + 3, n);
            setobj(L, ra + 4, gval(n));

            return true;
        }

        index++;
    }

    return false;
}

bool forgLoopNonTableFallback(lua_State* L, int insnA, int aux)
{
    TValue* base = L->base;
    TValue* ra = VM_REG(insnA);

    // note: it's safe to push arguments past top for complicated reasons (see lvmexecute.cpp)
    setobj2s(L, ra + 3 + 2, ra + 2);
    setobj2s(L, ra + 3 + 1, ra + 1);
    setobj2s(L, ra + 3, ra);

    L->top = ra + 3 + 3; // func + 2 args (state and index)
    LUAU_ASSERT(L->top <= L->stack_last);

    luaD_call(L, ra + 3, uint8_t(aux));
    L->top = L->ci->top;

    // recompute ra since stack might have been reallocated
    base = L->base;
    ra = VM_REG(insnA);

    // copy first variable back into the iteration index
    setobj2s(L, ra + 2, ra + 3);

    return !ttisnil(ra + 3);
}

void forgPrepXnextFallback(lua_State* L, TValue* ra, int pc)
{
    if (!ttisfunction(ra))
    {
        Closure* cl = clvalue(L->ci->func);
        L->ci->savedpc = cl->l.p->code + pc;

        luaG_typeerror(L, ra, "iterate over");
    }
}

Closure* callProlog(lua_State* L, TValue* ra, StkId argtop, int nresults)
{
    // slow-path: not a function call
    if (LUAU_UNLIKELY(!ttisfunction(ra)))
    {
        luaV_tryfuncTM(L, ra);
        argtop++; // __call adds an extra self
    }

    Closure* ccl = clvalue(ra);

    CallInfo* ci = incr_ci(L);
    ci->func = ra;
    ci->base = ra + 1;
    ci->top = argtop + ccl->stacksize; // note: technically UB since we haven't reallocated the stack yet
    ci->savedpc = NULL;
    ci->flags = 0;
    ci->nresults = nresults;

    L->base = ci->base;
    L->top = argtop;

    // note: this reallocs stack, but we don't need to VM_PROTECT this
    // this is because we're going to modify base/savedpc manually anyhow
    // crucially, we can't use ra/argtop after this line
    luaD_checkstack(L, ccl->stacksize);

    return ccl;
}

void callEpilogC(lua_State* L, int nresults, int n)
{
    // ci is our callinfo, cip is our parent
    CallInfo* ci = L->ci;
    CallInfo* cip = ci - 1;

    // copy return values into parent stack (but only up to nresults!), fill the rest with nil
    // note: in MULTRET context nresults starts as -1 so i != 0 condition never activates intentionally
    StkId res = ci->func;
    StkId vali = L->top - n;
    StkId valend = L->top;

    int i;
    for (i = nresults; i != 0 && vali < valend; i--)
        setobj2s(L, res++, vali++);
    while (i-- > 0)
        setnilvalue(res++);

    // pop the stack frame
    L->ci = cip;
    L->base = cip->base;
    L->top = (nresults == LUA_MULTRET) ? res : cip->top;
}

} // namespace CodeGen
} // namespace Luau
