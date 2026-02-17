
// All external function calls that can cause stack realloc or Lua calls have to be wrapped in VM_PROTECT
// This makes sure that we save the pc (in case the Lua call needs to generate a backtrace) before the call,
// and restores the stack pointer after in case stack gets reallocated
// Should only be used on the slow paths.
#define VM_PROTECT(x) \
    { \
        L->ci->savedpc = pc; \
        { \
            x; \
        }; \
        base = L->base; \
    }

// Some external functions can cause an error, but never reallocate the stack; for these, VM_PROTECT_PC() is
// a cheaper version of VM_PROTECT that can be called before the external call.
#define VM_PROTECT_PC() L->ci->savedpc = pc

#define VM_REG(i) (LUAU_ASSERT(unsigned(i) < unsigned(L->top - base)), &base[i])
#define VM_KV(i) (LUAU_ASSERT(unsigned(i) < unsigned(cl->l.p->sizek)), &k[i])
#define VM_UV(i) (LUAU_ASSERT(unsigned(i) < unsigned(cl->nupvalues)), &cl->l.uprefs[i])

#define VM_PATCH_C(pc, slot) *const_cast<Instruction*>(pc) = ((uint8_t(slot) << 24) | (0x00ffffffu & *(pc)))
#define VM_PATCH_E(pc, slot) *const_cast<Instruction*>(pc) = ((uint32_t(slot) << 8) | (0x000000ffu & *(pc)))

// When adding new bytecodes, add them here.
#define LUAU_OPCODE_LIST(X) \
    X(LOP_NOP) \
    X(LOP_BREAK) \
    X(LOP_LOADNIL) \
    X(LOP_LOADB) \
    X(LOP_LOADN) \
    X(LOP_LOADK) \
    X(LOP_MOVE) \
    X(LOP_GETGLOBAL) \
    X(LOP_SETGLOBAL) \
    X(LOP_GETUPVAL) \
    X(LOP_SETUPVAL) \
    X(LOP_CLOSEUPVALS) \
    X(LOP_GETIMPORT) \
    X(LOP_GETTABLE) \
    X(LOP_SETTABLE) \
    X(LOP_GETTABLEKS) \
    X(LOP_SETTABLEKS) \
    X(LOP_GETTABLEN) \
    X(LOP_SETTABLEN) \
    X(LOP_NEWCLOSURE) \
    X(LOP_NAMECALL) \
    X(LOP_CALL) \
    X(LOP_RETURN) \
    X(LOP_JUMP) \
    X(LOP_JUMPBACK) \
    X(LOP_JUMPIF) \
    X(LOP_JUMPIFNOT) \
    X(LOP_JUMPIFEQ) \
    X(LOP_JUMPIFLE) \
    X(LOP_JUMPIFLT) \
    X(LOP_JUMPIFNOTEQ) \
    X(LOP_JUMPIFNOTLE) \
    X(LOP_JUMPIFNOTLT) \
    X(LOP_ADD) \
    X(LOP_SUB) \
    X(LOP_MUL) \
    X(LOP_DIV) \
    X(LOP_MOD) \
    X(LOP_POW) \
    X(LOP_ADDK) \
    X(LOP_SUBK) \
    X(LOP_MULK) \
    X(LOP_DIVK) \
    X(LOP_MODK) \
    X(LOP_POWK) \
    X(LOP_AND) \
    X(LOP_OR) \
    X(LOP_ANDK) \
    X(LOP_ORK) \
    X(LOP_CONCAT) \
    X(LOP_NOT) \
    X(LOP_MINUS) \
    X(LOP_LENGTH) \
    X(LOP_NEWTABLE) \
    X(LOP_DUPTABLE) \
    X(LOP_SETLIST) \
    X(LOP_FORNPREP) \
    X(LOP_FORNLOOP) \
    X(LOP_FORGLOOP) \
    X(LOP_FORGPREP_INEXT) \
    X(LOP_FASTCALL3) \
    X(LOP_FORGPREP_NEXT) \
    X(LOP_NATIVECALL) \
    X(LOP_GETVARARGS) \
    X(LOP_DUPCLOSURE) \
    X(LOP_PREPVARARGS) \
    X(LOP_LOADKX) \
    X(LOP_JUMPX) \
    X(LOP_FASTCALL) \
    X(LOP_COVERAGE) \
    X(LOP_CAPTURE) \
    X(LOP_SUBRK) \
    X(LOP_DIVRK) \
    X(LOP_FASTCALL1) \
    X(LOP_FASTCALL2) \
    X(LOP_FASTCALL2K) \
    X(LOP_FORGPREP) \
    X(LOP_JUMPXEQKNIL) \
    X(LOP_JUMPXEQKB) \
    X(LOP_JUMPXEQKN) \
    X(LOP_JUMPXEQKS) \
    X(LOP_IDIV) \
    X(LOP_IDIVK)
