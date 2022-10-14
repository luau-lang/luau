// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "NativeState.h"

#include "Luau/UnwindBuilder.h"

#include "CustomExecUtils.h"
#include "Fallbacks.h"

#include "lbuiltins.h"
#include "lgc.h"
#include "ltable.h"
#include "lvm.h"

#include <math.h>

#define CODEGEN_SET_FALLBACK(op, flags) data.context.fallback[op] = {execute_##op, flags}
#define CODEGEN_SET_NAME(op) data.names[op] = #op

// Similar to a dispatch table in lvmexecute.cpp
#define CODEGEN_SET_NAMES() \
    CODEGEN_SET_NAME(LOP_NOP), CODEGEN_SET_NAME(LOP_BREAK), CODEGEN_SET_NAME(LOP_LOADNIL), CODEGEN_SET_NAME(LOP_LOADB), CODEGEN_SET_NAME(LOP_LOADN), \
        CODEGEN_SET_NAME(LOP_LOADK), CODEGEN_SET_NAME(LOP_MOVE), CODEGEN_SET_NAME(LOP_GETGLOBAL), CODEGEN_SET_NAME(LOP_SETGLOBAL), \
        CODEGEN_SET_NAME(LOP_GETUPVAL), CODEGEN_SET_NAME(LOP_SETUPVAL), CODEGEN_SET_NAME(LOP_CLOSEUPVALS), CODEGEN_SET_NAME(LOP_GETIMPORT), \
        CODEGEN_SET_NAME(LOP_GETTABLE), CODEGEN_SET_NAME(LOP_SETTABLE), CODEGEN_SET_NAME(LOP_GETTABLEKS), CODEGEN_SET_NAME(LOP_SETTABLEKS), \
        CODEGEN_SET_NAME(LOP_GETTABLEN), CODEGEN_SET_NAME(LOP_SETTABLEN), CODEGEN_SET_NAME(LOP_NEWCLOSURE), CODEGEN_SET_NAME(LOP_NAMECALL), \
        CODEGEN_SET_NAME(LOP_CALL), CODEGEN_SET_NAME(LOP_RETURN), CODEGEN_SET_NAME(LOP_JUMP), CODEGEN_SET_NAME(LOP_JUMPBACK), \
        CODEGEN_SET_NAME(LOP_JUMPIF), CODEGEN_SET_NAME(LOP_JUMPIFNOT), CODEGEN_SET_NAME(LOP_JUMPIFEQ), CODEGEN_SET_NAME(LOP_JUMPIFLE), \
        CODEGEN_SET_NAME(LOP_JUMPIFLT), CODEGEN_SET_NAME(LOP_JUMPIFNOTEQ), CODEGEN_SET_NAME(LOP_JUMPIFNOTLE), CODEGEN_SET_NAME(LOP_JUMPIFNOTLT), \
        CODEGEN_SET_NAME(LOP_ADD), CODEGEN_SET_NAME(LOP_SUB), CODEGEN_SET_NAME(LOP_MUL), CODEGEN_SET_NAME(LOP_DIV), CODEGEN_SET_NAME(LOP_MOD), \
        CODEGEN_SET_NAME(LOP_POW), CODEGEN_SET_NAME(LOP_ADDK), CODEGEN_SET_NAME(LOP_SUBK), CODEGEN_SET_NAME(LOP_MULK), CODEGEN_SET_NAME(LOP_DIVK), \
        CODEGEN_SET_NAME(LOP_MODK), CODEGEN_SET_NAME(LOP_POWK), CODEGEN_SET_NAME(LOP_AND), CODEGEN_SET_NAME(LOP_OR), CODEGEN_SET_NAME(LOP_ANDK), \
        CODEGEN_SET_NAME(LOP_ORK), CODEGEN_SET_NAME(LOP_CONCAT), CODEGEN_SET_NAME(LOP_NOT), CODEGEN_SET_NAME(LOP_MINUS), \
        CODEGEN_SET_NAME(LOP_LENGTH), CODEGEN_SET_NAME(LOP_NEWTABLE), CODEGEN_SET_NAME(LOP_DUPTABLE), CODEGEN_SET_NAME(LOP_SETLIST), \
        CODEGEN_SET_NAME(LOP_FORNPREP), CODEGEN_SET_NAME(LOP_FORNLOOP), CODEGEN_SET_NAME(LOP_FORGLOOP), CODEGEN_SET_NAME(LOP_FORGPREP_INEXT), \
        CODEGEN_SET_NAME(LOP_DEP_FORGLOOP_INEXT), CODEGEN_SET_NAME(LOP_FORGPREP_NEXT), CODEGEN_SET_NAME(LOP_DEP_FORGLOOP_NEXT), \
        CODEGEN_SET_NAME(LOP_GETVARARGS), CODEGEN_SET_NAME(LOP_DUPCLOSURE), CODEGEN_SET_NAME(LOP_PREPVARARGS), CODEGEN_SET_NAME(LOP_LOADKX), \
        CODEGEN_SET_NAME(LOP_JUMPX), CODEGEN_SET_NAME(LOP_FASTCALL), CODEGEN_SET_NAME(LOP_COVERAGE), CODEGEN_SET_NAME(LOP_CAPTURE), \
        CODEGEN_SET_NAME(LOP_DEP_JUMPIFEQK), CODEGEN_SET_NAME(LOP_DEP_JUMPIFNOTEQK), CODEGEN_SET_NAME(LOP_FASTCALL1), \
        CODEGEN_SET_NAME(LOP_FASTCALL2), CODEGEN_SET_NAME(LOP_FASTCALL2K), CODEGEN_SET_NAME(LOP_FORGPREP), CODEGEN_SET_NAME(LOP_JUMPXEQKNIL), \
        CODEGEN_SET_NAME(LOP_JUMPXEQKB), CODEGEN_SET_NAME(LOP_JUMPXEQKN), CODEGEN_SET_NAME(LOP_JUMPXEQKS)

static int luauF_missing(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams)
{
    return -1;
}

namespace Luau
{
namespace CodeGen
{

constexpr unsigned kBlockSize = 4 * 1024 * 1024;
constexpr unsigned kMaxTotalSize = 256 * 1024 * 1024;

NativeState::NativeState()
    : codeAllocator(kBlockSize, kMaxTotalSize)
{
}

NativeState::~NativeState() = default;

void initFallbackTable(NativeState& data)
{
    CODEGEN_SET_FALLBACK(LOP_GETGLOBAL, 0);
    CODEGEN_SET_FALLBACK(LOP_SETGLOBAL, 0);
    CODEGEN_SET_FALLBACK(LOP_SETUPVAL, 0);
    CODEGEN_SET_FALLBACK(LOP_CLOSEUPVALS, 0);
    CODEGEN_SET_FALLBACK(LOP_GETIMPORT, 0);
    CODEGEN_SET_FALLBACK(LOP_GETTABLEKS, 0);
    CODEGEN_SET_FALLBACK(LOP_SETTABLEKS, 0);
    CODEGEN_SET_FALLBACK(LOP_NEWCLOSURE, kFallbackUpdatePc);
    CODEGEN_SET_FALLBACK(LOP_NAMECALL, 0);
    CODEGEN_SET_FALLBACK(LOP_CALL, kFallbackUpdateCi | kFallbackCheckInterrupt);
    CODEGEN_SET_FALLBACK(LOP_RETURN, kFallbackUpdateCi | kFallbackCheckInterrupt);
    CODEGEN_SET_FALLBACK(LOP_CONCAT, 0);
    CODEGEN_SET_FALLBACK(LOP_NEWTABLE, 0);
    CODEGEN_SET_FALLBACK(LOP_DUPTABLE, 0);
    CODEGEN_SET_FALLBACK(LOP_SETLIST, kFallbackCheckInterrupt);
    CODEGEN_SET_FALLBACK(LOP_FORGPREP, kFallbackUpdatePc);
    CODEGEN_SET_FALLBACK(LOP_FORGLOOP, kFallbackUpdatePc | kFallbackCheckInterrupt);
    CODEGEN_SET_FALLBACK(LOP_FORGPREP_INEXT, kFallbackUpdatePc);
    CODEGEN_SET_FALLBACK(LOP_FORGPREP_NEXT, kFallbackUpdatePc);
    CODEGEN_SET_FALLBACK(LOP_GETVARARGS, 0);
    CODEGEN_SET_FALLBACK(LOP_DUPCLOSURE, 0);
    CODEGEN_SET_FALLBACK(LOP_PREPVARARGS, 0);
    CODEGEN_SET_FALLBACK(LOP_LOADKX, 0);
    CODEGEN_SET_FALLBACK(LOP_COVERAGE, 0);
    CODEGEN_SET_FALLBACK(LOP_BREAK, 0);
}

void initHelperFunctions(NativeState& data)
{
    static_assert(sizeof(data.context.luauF_table) / sizeof(data.context.luauF_table[0]) == sizeof(luauF_table) / sizeof(luauF_table[0]),
        "fast call tables are not of the same length");

    // Replace missing fast call functions with an empty placeholder that forces LOP_CALL fallback
    for (size_t i = 0; i < sizeof(data.context.luauF_table) / sizeof(data.context.luauF_table[0]); i++)
        data.context.luauF_table[i] = luauF_table[i] ? luauF_table[i] : luauF_missing;

    data.context.luaV_lessthan = luaV_lessthan;
    data.context.luaV_lessequal = luaV_lessequal;
    data.context.luaV_equalval = luaV_equalval;
    data.context.luaV_doarith = luaV_doarith;
    data.context.luaV_dolen = luaV_dolen;
    data.context.luaV_prepareFORN = luaV_prepareFORN;
    data.context.luaV_gettable = luaV_gettable;
    data.context.luaV_settable = luaV_settable;

    data.context.luaH_getn = luaH_getn;

    data.context.luaC_barriertable = luaC_barriertable;

    data.context.libm_pow = pow;
}

void initInstructionNames(NativeState& data)
{
    CODEGEN_SET_NAMES();
}

} // namespace CodeGen
} // namespace Luau
