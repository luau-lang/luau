// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Bytecode.h"
#include "Luau/CodeAllocator.h"
#include "Luau/Label.h"

#include <memory>

#include <stdint.h>

#include "ldebug.h"
#include "lobject.h"
#include "ltm.h"
#include "lstate.h"

typedef int (*luau_FastFunction)(lua_State* L, StkId res, TValue* arg0, int nresults, StkId args, int nparams);

namespace Luau
{
namespace CodeGen
{

class UnwindBuilder;

using FallbackFn = const Instruction*(lua_State* L, const Instruction* pc, StkId base, TValue* k);

constexpr uint8_t kFallbackUpdatePc = 1 << 0;

struct NativeFallback
{
    FallbackFn* fallback;
    uint8_t flags;
};

struct NativeProto
{
    uintptr_t entryTarget = 0;
    uintptr_t* instTargets = nullptr; // TODO: NativeProto should be variable-size with all target embedded

    Proto* proto = nullptr;
    uint32_t location = 0;
};

struct NativeContext
{
    // Gateway (C => native transition) entry & exit, compiled at runtime
    uint8_t* gateEntry = nullptr;
    uint8_t* gateExit = nullptr;

    // Opcode fallbacks, implemented in C
    NativeFallback fallback[LOP__COUNT] = {};

    // Fast call methods, implemented in C
    luau_FastFunction luauF_table[256] = {};

    // Helper functions, implemented in C
    int (*luaV_lessthan)(lua_State* L, const TValue* l, const TValue* r) = nullptr;
    int (*luaV_lessequal)(lua_State* L, const TValue* l, const TValue* r) = nullptr;
    int (*luaV_equalval)(lua_State* L, const TValue* t1, const TValue* t2) = nullptr;
    void (*luaV_doarith)(lua_State* L, StkId ra, const TValue* rb, const TValue* rc, TMS op) = nullptr;
    void (*luaV_dolen)(lua_State* L, StkId ra, const TValue* rb) = nullptr;
    void (*luaV_prepareFORN)(lua_State* L, StkId plimit, StkId pstep, StkId pinit) = nullptr;
    void (*luaV_gettable)(lua_State* L, const TValue* t, TValue* key, StkId val) = nullptr;
    void (*luaV_settable)(lua_State* L, const TValue* t, TValue* key, StkId val) = nullptr;
    void (*luaV_getimport)(lua_State* L, Table* env, TValue* k, uint32_t id, bool propagatenil) = nullptr;
    void (*luaV_concat)(lua_State* L, int total, int last) = nullptr;

    int (*luaH_getn)(Table* t) = nullptr;
    Table* (*luaH_new)(lua_State* L, int narray, int lnhash) = nullptr;
    Table* (*luaH_clone)(lua_State* L, Table* tt) = nullptr;
    void (*luaH_resizearray)(lua_State* L, Table* t, int nasize) = nullptr;

    void (*luaC_barriertable)(lua_State* L, Table* t, GCObject* v) = nullptr;
    void (*luaC_barrierf)(lua_State* L, GCObject* o, GCObject* v) = nullptr;
    void (*luaC_barrierback)(lua_State* L, GCObject* o, GCObject** gclist) = nullptr;
    size_t (*luaC_step)(lua_State* L, bool assist) = nullptr;

    void (*luaF_close)(lua_State* L, StkId level) = nullptr;

    const TValue* (*luaT_gettm)(Table* events, TMS event, TString* ename) = nullptr;

    double (*libm_exp)(double) = nullptr;
    double (*libm_pow)(double, double) = nullptr;
    double (*libm_fmod)(double, double) = nullptr;
    double (*libm_asin)(double) = nullptr;
    double (*libm_sin)(double) = nullptr;
    double (*libm_sinh)(double) = nullptr;
    double (*libm_acos)(double) = nullptr;
    double (*libm_cos)(double) = nullptr;
    double (*libm_cosh)(double) = nullptr;
    double (*libm_atan)(double) = nullptr;
    double (*libm_atan2)(double, double) = nullptr;
    double (*libm_tan)(double) = nullptr;
    double (*libm_tanh)(double) = nullptr;
    double (*libm_log)(double) = nullptr;
    double (*libm_log2)(double) = nullptr;
    double (*libm_log10)(double) = nullptr;

    // Helper functions
    bool (*forgLoopNodeIter)(lua_State* L, Table* h, int index, TValue* ra) = nullptr;
    bool (*forgLoopNonTableFallback)(lua_State* L, int insnA, int aux) = nullptr;
    void (*forgPrepXnextFallback)(lua_State* L, TValue* ra, int pc) = nullptr;
    Closure* (*callProlog)(lua_State* L, TValue* ra, StkId argtop, int nresults) = nullptr;
    void (*callEpilogC)(lua_State* L, int nresults, int n) = nullptr;
};

struct NativeState
{
    NativeState();
    ~NativeState();

    CodeAllocator codeAllocator;
    std::unique_ptr<UnwindBuilder> unwindBuilder;

    uint8_t* gateData = nullptr;
    size_t gateDataSize = 0;

    NativeContext context;
};

void initFallbackTable(NativeState& data);
void initHelperFunctions(NativeState& data);

} // namespace CodeGen
} // namespace Luau
