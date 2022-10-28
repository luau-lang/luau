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

} // namespace CodeGen
} // namespace Luau
