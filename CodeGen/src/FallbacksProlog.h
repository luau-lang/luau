// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lvm.h"

#include "lbuiltins.h"
#include "lbytecode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lnumutils.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"

#include <string.h>

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

#define VM_INTERRUPT() \
    { \
        void (*interrupt)(lua_State*, int) = L->global->cb.interrupt; \
        if (LUAU_UNLIKELY(!!interrupt)) \
        { /* the interrupt hook is called right before we advance pc */ \
            VM_PROTECT(L->ci->savedpc++; interrupt(L, -1)); \
            if (L->status != 0) \
            { \
                L->ci->savedpc--; \
                return NULL; \
            } \
        } \
    }
