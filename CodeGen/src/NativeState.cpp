// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "NativeState.h"

#include "Luau/UnwindBuilder.h"

#include "CodeGenUtils.h"

#include "lbuiltins.h"
#include "lgc.h"
#include "ltable.h"
#include "lfunc.h"
#include "lvm.h"

#include <math.h>
#include <string.h>

LUAU_FASTINTVARIABLE(LuauCodeGenBlockSize, 4 * 1024 * 1024)
LUAU_FASTINTVARIABLE(LuauCodeGenMaxTotalSize, 256 * 1024 * 1024)

namespace Luau
{
namespace CodeGen
{

NativeState::NativeState()
    : NativeState(nullptr, nullptr)
{
}

NativeState::NativeState(AllocationCallback* allocationCallback, void* allocationCallbackContext)
    : codeAllocator{size_t(FInt::LuauCodeGenBlockSize), size_t(FInt::LuauCodeGenMaxTotalSize), allocationCallback, allocationCallbackContext}
{
}

NativeState::~NativeState() = default;

void initFunctions(NativeState& data)
{
    static_assert(sizeof(data.context.luauF_table) == sizeof(luauF_table), "fastcall tables are not of the same length");
    memcpy(data.context.luauF_table, luauF_table, sizeof(luauF_table));

    data.context.luaV_lessthan = luaV_lessthan;
    data.context.luaV_lessequal = luaV_lessequal;
    data.context.luaV_equalval = luaV_equalval;
    data.context.luaV_doarith = luaV_doarith;

    data.context.luaV_doarithadd = luaV_doarithimpl<TM_ADD>;
    data.context.luaV_doarithsub = luaV_doarithimpl<TM_SUB>;
    data.context.luaV_doarithmul = luaV_doarithimpl<TM_MUL>;
    data.context.luaV_doarithdiv = luaV_doarithimpl<TM_DIV>;
    data.context.luaV_doarithidiv = luaV_doarithimpl<TM_IDIV>;
    data.context.luaV_doarithmod = luaV_doarithimpl<TM_MOD>;
    data.context.luaV_doarithpow = luaV_doarithimpl<TM_POW>;
    data.context.luaV_doarithunm = luaV_doarithimpl<TM_UNM>;

    data.context.luaV_dolen = luaV_dolen;
    data.context.luaV_gettable = luaV_gettable;
    data.context.luaV_settable = luaV_settable;
    data.context.luaV_getimport = luaV_getimport;
    data.context.luaV_concat = luaV_concat;

    data.context.luaH_getn = luaH_getn;
    data.context.luaH_new = luaH_new;
    data.context.luaH_clone = luaH_clone;
    data.context.luaH_resizearray = luaH_resizearray;
    data.context.luaH_setnum = luaH_setnum;

    data.context.luaC_barriertable = luaC_barriertable;
    data.context.luaC_barrierf = luaC_barrierf;
    data.context.luaC_barrierback = luaC_barrierback;
    data.context.luaC_step = luaC_step;

    data.context.luaF_close = luaF_close;
    data.context.luaF_findupval = luaF_findupval;
    data.context.luaF_newLclosure = luaF_newLclosure;

    data.context.luaT_gettm = luaT_gettm;
    data.context.luaT_objtypenamestr = luaT_objtypenamestr;

    data.context.libm_exp = exp;
    data.context.libm_pow = pow;
    data.context.libm_fmod = fmod;
    data.context.libm_log = log;
    data.context.libm_log2 = log2;
    data.context.libm_log10 = log10;
    data.context.libm_ldexp = ldexp;
    data.context.libm_round = round;
    data.context.libm_frexp = frexp;
    data.context.libm_modf = modf;

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

    data.context.forgLoopTableIter = forgLoopTableIter;
    data.context.forgLoopNodeIter = forgLoopNodeIter;
    data.context.forgLoopNonTableFallback = forgLoopNonTableFallback;
    data.context.forgPrepXnextFallback = forgPrepXnextFallback;
    data.context.callProlog = callProlog;
    data.context.callEpilogC = callEpilogC;

    data.context.callFallback = callFallback;

    data.context.executeGETGLOBAL = executeGETGLOBAL;
    data.context.executeSETGLOBAL = executeSETGLOBAL;
    data.context.executeGETTABLEKS = executeGETTABLEKS;
    data.context.executeSETTABLEKS = executeSETTABLEKS;

    data.context.executeNAMECALL = executeNAMECALL;
    data.context.executeFORGPREP = executeFORGPREP;
    data.context.executeGETVARARGSMultRet = executeGETVARARGSMultRet;
    data.context.executeGETVARARGSConst = executeGETVARARGSConst;
    data.context.executeDUPCLOSURE = executeDUPCLOSURE;
    data.context.executePREPVARARGS = executePREPVARARGS;
    data.context.executeSETLIST = executeSETLIST;
}

void initFunctions(NativeContext& context)
{
    static_assert(sizeof(context.luauF_table) == sizeof(luauF_table), "fastcall tables are not of the same length");
    memcpy(context.luauF_table, luauF_table, sizeof(luauF_table));

    context.luaV_lessthan = luaV_lessthan;
    context.luaV_lessequal = luaV_lessequal;
    context.luaV_equalval = luaV_equalval;
    context.luaV_doarith = luaV_doarith;

    context.luaV_doarithadd = luaV_doarithimpl<TM_ADD>;
    context.luaV_doarithsub = luaV_doarithimpl<TM_SUB>;
    context.luaV_doarithmul = luaV_doarithimpl<TM_MUL>;
    context.luaV_doarithdiv = luaV_doarithimpl<TM_DIV>;
    context.luaV_doarithidiv = luaV_doarithimpl<TM_IDIV>;
    context.luaV_doarithmod = luaV_doarithimpl<TM_MOD>;
    context.luaV_doarithpow = luaV_doarithimpl<TM_POW>;
    context.luaV_doarithunm = luaV_doarithimpl<TM_UNM>;

    context.luaV_dolen = luaV_dolen;
    context.luaV_gettable = luaV_gettable;
    context.luaV_settable = luaV_settable;
    context.luaV_getimport = luaV_getimport;
    context.luaV_concat = luaV_concat;

    context.luaH_getn = luaH_getn;
    context.luaH_new = luaH_new;
    context.luaH_clone = luaH_clone;
    context.luaH_resizearray = luaH_resizearray;
    context.luaH_setnum = luaH_setnum;

    context.luaC_barriertable = luaC_barriertable;
    context.luaC_barrierf = luaC_barrierf;
    context.luaC_barrierback = luaC_barrierback;
    context.luaC_step = luaC_step;

    context.luaF_close = luaF_close;
    context.luaF_findupval = luaF_findupval;
    context.luaF_newLclosure = luaF_newLclosure;

    context.luaT_gettm = luaT_gettm;
    context.luaT_objtypenamestr = luaT_objtypenamestr;

    context.libm_exp = exp;
    context.libm_pow = pow;
    context.libm_fmod = fmod;
    context.libm_log = log;
    context.libm_log2 = log2;
    context.libm_log10 = log10;
    context.libm_ldexp = ldexp;
    context.libm_round = round;
    context.libm_frexp = frexp;
    context.libm_modf = modf;

    context.libm_asin = asin;
    context.libm_sin = sin;
    context.libm_sinh = sinh;
    context.libm_acos = acos;
    context.libm_cos = cos;
    context.libm_cosh = cosh;
    context.libm_atan = atan;
    context.libm_atan2 = atan2;
    context.libm_tan = tan;
    context.libm_tanh = tanh;

    context.forgLoopTableIter = forgLoopTableIter;
    context.forgLoopNodeIter = forgLoopNodeIter;
    context.forgLoopNonTableFallback = forgLoopNonTableFallback;
    context.forgPrepXnextFallback = forgPrepXnextFallback;
    context.callProlog = callProlog;
    context.callEpilogC = callEpilogC;

    context.callFallback = callFallback;

    context.executeGETGLOBAL = executeGETGLOBAL;
    context.executeSETGLOBAL = executeSETGLOBAL;
    context.executeGETTABLEKS = executeGETTABLEKS;
    context.executeSETTABLEKS = executeSETTABLEKS;

    context.executeNAMECALL = executeNAMECALL;
    context.executeFORGPREP = executeFORGPREP;
    context.executeGETVARARGSMultRet = executeGETVARARGSMultRet;
    context.executeGETVARARGSConst = executeGETVARARGSConst;
    context.executeDUPCLOSURE = executeDUPCLOSURE;
    context.executePREPVARARGS = executePREPVARARGS;
    context.executeSETLIST = executeSETLIST;
}

} // namespace CodeGen
} // namespace Luau
