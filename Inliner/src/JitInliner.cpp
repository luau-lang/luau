// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/JitInliner.h"

#include "Luau/Bytecode.h"
#include "Luau/BytecodeGraph.h"
#include "Luau/BytecodeCallInliner.h"
#include "Luau/BytecodeUtils.h"

#include "BytecodeGraphParser.h"
#include "BytecodeGraphSerializer.h"
#include "RuntimeBytecodeBuilder.h"

#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"

#include <cstdio>
#include <optional>

using namespace Luau::Bytecode;

namespace Luau
{
namespace JitInliner
{

using RuntimeBcFunction = BcFunction<TValue*>;

std::optional<std::pair<RuntimeBcFunction, BcOp>> buildGraphFromProto(Proto* p, std::optional<uint32_t> callPc = {})
{
    RuntimeBcFunction fn;

    fn.maxstacksize = p->maxstacksize;
    fn.numparams = p->numparams;
    fn.nups = p->nups;
    fn.is_vararg = static_cast<bool>(p->is_vararg);
    fn.flags = p->flags;

    uint8_t* typeinfo = p->typeinfo;
    fn.typeInfo = std::string_view(reinterpret_cast<char*>(typeinfo), p->sizetypeinfo);

    fn.constants.resize(p->sizek);
    for (int i = 0; i < p->sizek; i++)
        fn.constants[i] = &p->k[i];

    fn.protos.resize(p->sizep);
    for (int i = 0; i < p->sizep; i++)
        fn.protos[i] = i;

    std::vector<uint32_t> lines(p->sizecode, 0);
    if (p->lineinfo != nullptr && p->abslineinfo != nullptr)
        for (int i = 0; i < p->sizecode; i++)
            lines[i] = p->abslineinfo[i >> p->linegaplog2] + p->lineinfo[i];

    std::vector<uint32_t> insnsPC;
    BytecodeGraphParser<TValue*> graphParser(fn);

    Instruction* code = p->code;
    if (!graphParser.rebuildGraph(code, p->sizecode, lines, insnsPC))
        return {};

    BcOp callOp;
    if (callPc)
    {
        LUAU_ASSERT(*callPc < insnsPC.size());
        callOp = BcOp{BcOpKind::Inst, insnsPC[*callPc]};
    }

    return {{fn, callOp}};
}

constexpr uint32_t kUnassignedPC = ~0;

std::optional<CodeData> emitCode(lua_State* L, RuntimeBcFunction& graph, std::vector<Proto*>& protos)
{
    RuntimeBytecodeBuilder bcb(L, graph.constants, protos);
    bcb.beginFunction(graph.numparams, graph.is_vararg);

    BytecodeGraphSerializer<TValue*> serializer(bcb, graph);
    std::vector<uint32_t> insnsPC = serializer.emitBytecode();

    if (insnsPC.size() == 0)
        return {};

    bcb.foldJumps();

    std::vector<uint32_t> remap = bcb.expandJumps();
    if (remap.size() > 0)
    {
        LUAU_ASSERT(insnsPC.size() <= remap.size());
        for (size_t i = 0; i < insnsPC.size(); i++)
            insnsPC[i] = remap[insnsPC[i]];
    }

    auto res = bcb.finishAndDumpCode(graph.maxstacksize, graph.nups);

    for (uint32_t i = 0; i < graph.instructions.size(); i++)
    {
        BcInst& insn = graph.instructions[i];
        if (insn.op == LOP_CALLFB)
        {
            BcCallFB<TValue*> callFB = graph.template as<BcCallFB<TValue*>>(BcOp{BcOpKind::Inst, i});
            if (callFB.FbSlot() >= 0)
            {
                uint32_t fbSlot = static_cast<uint32_t>(callFB.FbSlot());
                if (fbSlot >= res.fbSlotPCs.size())
                    res.fbSlotPCs.resize(fbSlot + 1, kUnassignedPC);
                res.fbSlotPCs[fbSlot] = insnsPC[i];
            }
        }
    }

    return {res};
}

Proto* createInlinedProto(lua_State* L, Proto* caller, Proto* target, RuntimeBcFunction& graph, CodeData& codeData)
{
    Proto* p = luaF_newproto(L);

    p->debugname = caller->debugname;
    p->maxstacksize = graph.maxstacksize;
    p->numparams = graph.numparams;
    p->nups = graph.nups;
    p->is_vararg = graph.is_vararg;
    p->flags = graph.flags;
    p->gclist = nullptr;
    p->funid = caller->funid;
    p->userdata = caller->userdata;
    p->source = caller->source;
    p->linedefined = caller->linedefined;
    
    p->k = luaM_newarray(L, graph.constants.size(), TValue, L->activememcat);
    p->sizek = graph.constants.size();
    for (int i = 0; i < p->sizek; i++)
        p->k[i] = *graph.constants[i];

    p->p = luaM_newarray(L, graph.protos.size(), Proto*, L->activememcat);
    p->sizep = graph.protos.size();
    LUAU_ASSERT(p->sizep == (caller->sizep + target->sizep));
    memcpy(p->p, caller->p, caller->sizep * sizeof(Proto*));
    memcpy(p->p + caller->sizep, target->p, target->sizep * sizeof(Proto*));

    p->code = luaM_newarray(L, codeData.code.size(), Instruction, L->activememcat);
    p->sizecode = codeData.code.size();
    memcpy(p->code, codeData.code.data(), p->sizecode * sizeof(Instruction));
    // Lineinfo data is preallocated by emitCode
    p->linegaplog2 = codeData.linegaplog2;
    p->abslineinfo = codeData.abslineinfo;
    p->lineinfo = codeData.lineinfo;
    p->sizelineinfo = codeData.sizelineinfo;
    p->codeentry = p->code;
    p->bytecodeid = caller->bytecodeid;

    uint32_t feedbackvecsize = caller->feedbackvecsize + target->feedbackvecsize;
    p->feedbackvec = luaM_newarray(L, feedbackvecsize, FeedbackVectorSlot, L->activememcat);
    p->feedbackvecsize = feedbackvecsize;
    memcpy(p->feedbackvec, caller->feedbackvec, caller->feedbackvecsize * sizeof(FeedbackVectorSlot));
    memcpy(p->feedbackvec + caller->feedbackvecsize, target->feedbackvec, target->feedbackvecsize * sizeof(FeedbackVectorSlot));

    for (uint32_t i = 0; i < std::min<uint32_t>(p->feedbackvecsize, codeData.fbSlotPCs.size()); i++)
        if (codeData.fbSlotPCs[i] != kUnassignedPC)
        {
            LUAU_ASSERT(p->feedbackvec[i].kind == FeedbackVectorSlotKind::CALL_TARGET);
            p->feedbackvec[i].call_target.pc = codeData.fbSlotPCs[i];
        }

    p->deoptimized = caller;
    caller->optimized = p;
    luaC_objbarrier(L, caller, p);

    return p;
}

void sealAllSlots(Instruction* code, uint32_t codesize)
{
    Instruction* codeend = code + codesize;
    for (Instruction* pc = code; pc < codeend;)
    {
        if (LUAU_INSN_OP(*pc) == LOP_CALLFB)
            *(pc + 1) = 0xFFFFFFFF;
        pc += Luau::getOpLength(static_cast<LuauOpcode>(LUAU_INSN_OP(*pc)));
    }
}

constexpr int kMaxFunctionBytecodeSize = 0xFFFF;

Proto* onInlineFunction(lua_State* L, Closure* caller, Closure* target, uint32_t pc)
{
    LUAU_ASSERT(!caller->isC && !target->isC);

    Proto* callerProto = caller->l.p;
    Proto* targetProto = target->l.p;

    // Checking if a caller was optimized already.
    if (callerProto->optimized != nullptr)
        return nullptr;

    if ((targetProto->flags & LPF_INLINABLE) == 0)
        return nullptr;

    LUAU_ASSERT(target->nupvalues == 0);

    // recursive inlining is not supported yet.
    if (targetProto->funid == callerProto->funid)
        return nullptr;

    // pick the latest optimized version for inlining
    while (targetProto->optimized != nullptr)
        targetProto = targetProto->optimized;

    if (callerProto->sizecode >= kMaxFunctionBytecodeSize || targetProto->sizecode >= kMaxFunctionBytecodeSize)
        return nullptr;

    auto callerGraph = buildGraphFromProto(callerProto, pc);
    auto targetGraph = buildGraphFromProto(targetProto);

    if (!callerGraph || !targetGraph)
        return nullptr;

    if (!inlineCall(callerGraph->first, targetGraph->first, callerGraph->second, targetProto->funid, callerProto->feedbackvecsize))
        return nullptr;

    std::vector<Proto*> protos;
    protos.resize(callerProto->sizep + targetProto->sizep);
    memcpy(protos.data(), callerProto->p, callerProto->sizep * sizeof(Proto*));
    memcpy(protos.data() + callerProto->sizep, targetProto->p, targetProto->sizep * sizeof(Proto*));

    std::optional<CodeData> codeData = emitCode(L, callerGraph->first, protos);
    if (!codeData)
    {
        sealAllSlots(callerProto->code, callerProto->sizecode);
        return nullptr;
    }

    createInlinedProto(L, callerProto, targetProto, callerGraph->first, *codeData);

    return nullptr;
}

void setup(lua_State* L)
{
    L->global->ecb.inlinefunction = onInlineFunction;
}

void disable(lua_State* L)
{
    L->global->ecb.inlinefunction = nullptr;
}

} // namespace JitInliner
} // namespace Luau