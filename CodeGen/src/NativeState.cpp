// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "NativeState.h"

#include "Luau/UnwindBuilder.h"

#include "CodeGenUtils.h"
#include "CustomExecUtils.h"
#include "Fallbacks.h"

#include "lbuiltins.h"
#include "lgc.h"
#include "ltable.h"
#include "lfunc.h"
#include "lvm.h"

#include <math.h>
#include <string.h>

#define CODEGEN_SET_FALLBACK(op, flags) data.context.fallback[op] = {execute_##op, flags}

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
    // When fallback is completely removed, remove it from includeInsts list in lvmexecute_split.py
    CODEGEN_SET_FALLBACK(LOP_NEWCLOSURE, 0);
    CODEGEN_SET_FALLBACK(LOP_NAMECALL, 0);
    CODEGEN_SET_FALLBACK(LOP_FORGPREP, kFallbackUpdatePc);
    CODEGEN_SET_FALLBACK(LOP_GETVARARGS, 0);
    CODEGEN_SET_FALLBACK(LOP_DUPCLOSURE, 0);
    CODEGEN_SET_FALLBACK(LOP_PREPVARARGS, 0);
    CODEGEN_SET_FALLBACK(LOP_BREAK, 0);

    // Fallbacks that are called from partial implementation of an instruction
    CODEGEN_SET_FALLBACK(LOP_GETGLOBAL, 0);
    CODEGEN_SET_FALLBACK(LOP_SETGLOBAL, 0);
    CODEGEN_SET_FALLBACK(LOP_GETTABLEKS, 0);
    CODEGEN_SET_FALLBACK(LOP_SETTABLEKS, 0);
}

void initHelperFunctions(NativeState& data)
{
    static_assert(sizeof(data.context.luauF_table) == sizeof(luauF_table), "fastcall tables are not of the same length");
    memcpy(data.context.luauF_table, luauF_table, sizeof(luauF_table));

    data.context.luaV_lessthan = luaV_lessthan;
    data.context.luaV_lessequal = luaV_lessequal;
    data.context.luaV_equalval = luaV_equalval;
    data.context.luaV_doarith = luaV_doarith;
    data.context.luaV_dolen = luaV_dolen;
    data.context.luaV_prepareFORN = luaV_prepareFORN;
    data.context.luaV_gettable = luaV_gettable;
    data.context.luaV_settable = luaV_settable;
    data.context.luaV_getimport = luaV_getimport;
    data.context.luaV_concat = luaV_concat;

    data.context.luaH_getn = luaH_getn;
    data.context.luaH_new = luaH_new;
    data.context.luaH_clone = luaH_clone;
    data.context.luaH_resizearray = luaH_resizearray;

    data.context.luaC_barriertable = luaC_barriertable;
    data.context.luaC_barrierf = luaC_barrierf;
    data.context.luaC_barrierback = luaC_barrierback;
    data.context.luaC_step = luaC_step;

    data.context.luaF_close = luaF_close;

    data.context.luaT_gettm = luaT_gettm;

    data.context.libm_exp = exp;
    data.context.libm_pow = pow;
    data.context.libm_fmod = fmod;
    data.context.libm_log = log;
    data.context.libm_log2 = log2;
    data.context.libm_log10 = log10;

    data.context.libm_asin = asin;
    data.context.libm_sin = sin;
    data.context.libm_sinh = sinh;
    data.context.libm_acos = acos;
    data.context.libm_cos = cos;
    data.context.libm_cosh = cosh;
    data.context.libm_atan = atan;
    data.context.libm_atan2 = atan2;
    data.context.libm_tan = tan;
    data.context.libm_tanh = tanh;

    data.context.forgLoopNodeIter = forgLoopNodeIter;
    data.context.forgLoopNonTableFallback = forgLoopNonTableFallback;
    data.context.forgPrepXnextFallback = forgPrepXnextFallback;
    data.context.callProlog = callProlog;
    data.context.callEpilogC = callEpilogC;
}

} // namespace CodeGen
} // namespace Luau
