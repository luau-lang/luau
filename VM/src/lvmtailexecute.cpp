#include "lvm.h"
#include "lvmexecuteshared.h"

#include "lstate.h"
#include "ltable.h"
#include "lfunc.h"
#include "lstring.h"
#include "lgc.h"
#include "lmem.h"
#include "ldebug.h"
#include "ldo.h"
#include "lbuiltins.h"
#include "lnumutils.h"
#include "lbytecode.h"

#define MUSTTAIL [[clang::musttail]]
#define NONE __attribute__((preserve_none))

#define OPFUNCTIONARGS (lua_State * L, Closure * cl, StkId base, TValue * k, const Instruction* pc)
#define OPFUNCTIONSIG void NONE(*) OPFUNCTIONARGS
#define OP_FORWARD_DECLARE(op) void NONE FUNC_##op OPFUNCTIONARGS

using OpFunction = OPFUNCTIONSIG;

#define OP_DECLARE(op) OP_FORWARD_DECLARE(op);
LUAU_OPCODE_LIST(OP_DECLARE)

#define OP_TABLE_ENTRY(op) &FUNC_##op,
OpFunction kDispatchTable[LOP__COUNT] = {LUAU_OPCODE_LIST(OP_TABLE_ENTRY)};

#define VM_TAILFUNC(op) void NONE FUNC_##op OPFUNCTIONARGS
#define VM_NEXT() MUSTTAIL return kDispatchTable[LUAU_INSN_OP(*pc)](L, cl, base, k, pc)

LUAI_FUNC void luau_execute_tail(lua_State* L) {}

VM_TAILFUNC(LOP_NOP)
{
    Instruction insn = *pc++;
    LUAU_ASSERT(insn == 0);
    VM_NEXT();
}

VM_TAILFUNC(LOP_LOADNIL)
{
    Instruction insn = *pc++;
    StkId ra = VM_REG(LUAU_INSN_A(insn));

    setnilvalue(ra);
    VM_NEXT();
}

VM_TAILFUNC(LOP_LOADB)
{
    Instruction insn = *pc++;
    StkId ra = VM_REG(LUAU_INSN_A(insn));

    setbvalue(ra, LUAU_INSN_B(insn));

    pc += LUAU_INSN_C(insn);
    LUAU_ASSERT(unsigned(pc - cl->l.p->code) < unsigned(cl->l.p->sizecode));
    VM_NEXT();
}

VM_TAILFUNC(LOP_LOADN)
{
    Instruction insn = *pc++;
    StkId ra = VM_REG(LUAU_INSN_A(insn));

    setnvalue(ra, LUAU_INSN_D(insn));
    VM_NEXT();
}

VM_TAILFUNC(LOP_LOADK)
{
    Instruction insn = *pc++;
    StkId ra = VM_REG(LUAU_INSN_A(insn));
    TValue* kv = VM_KV(LUAU_INSN_D(insn));

    setobj2s(L, ra, kv);
    VM_NEXT();
}

VM_TAILFUNC(LOP_MOVE)
{
    Instruction insn = *pc++;
    StkId ra = VM_REG(LUAU_INSN_A(insn));
    StkId rb = VM_REG(LUAU_INSN_B(insn));

    setobj2s(L, ra, rb);
    VM_NEXT();
}

VM_TAILFUNC(LOP_GETIMPORT)
{
    Instruction insn = *pc++;
    StkId ra = VM_REG(LUAU_INSN_A(insn));
    TValue* kv = VM_KV(LUAU_INSN_D(insn));

    // fast-path: import resolution was successful and closure environment is "safe" for import
    if (!ttisnil(kv) && cl->env->safeenv)
    {
        setobj2s(L, ra, kv);
        pc++; // skip over AUX
        VM_NEXT();
    }
    else
    {
        uint32_t aux = *pc++;

        VM_PROTECT(luaV_getimport(L, cl->env, k, ra, aux, /* propagatenil= */ false));
        VM_NEXT();
    }
}
