// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/BytecodeGraph.h"

typedef struct lua_TValue TValue;

namespace Luau
{
namespace Bytecode
{

LUAU_PRINTF_ATTR(2, 3)
void append(std::string& result, const char* fmt, ...);

void padToDetailColumn(std::string& result, size_t lineStart);
bool isPrintableStringConstant(const char* str, size_t len);
int decomposeImportId(uint32_t ids, int32_t& id0, int32_t& id1, int32_t& id2);

const char* getBlockEdgeKindName(BcBlockEdgeKind kind);
const char* getLuauOpcodeName(LuauOpcode cmd);

struct ToStringContext
{
    std::string& result;
    bool includeDetailColumn = true;
};

void toString(ToStringContext& ctx, const BcImm& imm);
void toString(ToStringContext& ctx, BcFunction<BcVmConst>& function, const BcVmConst& data, bool detailed);
void toString(ToStringContext& ctx, BcFunction<TValue*>& function, TValue* data, bool detailed);

template<typename VmConst>
void toString(ToStringContext& ctx, BcFunction<VmConst>& function, BcOp op);

template<typename VmConst>
void toStringUses(ToStringContext& ctx, BcFunction<VmConst>& function, const std::vector<BcOp>& uses)
{
    bool comma = false;

    for (const BcOp& use : uses)
    {
        if (comma)
            append(ctx.result, ", ");
        comma = true;

        toString(ctx, function, use);
    }
}

template<typename VmConst>
void toStringBlockEdges(ToStringContext& ctx, BcFunction<VmConst>& function, const BcEdges& edges)
{
    bool comma = false;

    for (const BcBlockEdge& edge : edges)
    {
        if (comma)
            append(ctx.result, ", ");
        comma = true;

        toString(ctx, function, edge.target);
        append(ctx.result, " [%s]", getBlockEdgeKindName(edge.kind));
    }
}

template<typename VmConst>
void toStringFunctionHeader(ToStringContext& ctx, BcFunction<VmConst>& function)
{
    if (!function.debugname.empty())
        append(ctx.result, "; function %s(", function.debugname.c_str());
    else
        append(ctx.result, "; function(");

    for (int i = 0; i < function.numparams; i++)
    {
        if (i < int(function.locals.size()))
        {
            const DebugLocal& local = function.locals[i];
            append(ctx.result, "%s%.*s", i == 0 ? "" : ", ", int(local.varname.length()), local.varname.data());
        }
        else
        {
            append(ctx.result, "%s$arg%d", i == 0 ? "" : ", ", i);
        }
    }

    if (function.numparams != 0 && function.is_vararg)
        append(ctx.result, ", ...)");
    else if (function.is_vararg)
        append(ctx.result, "...)");
    else
        append(ctx.result, ")");

    if (function.linedefined != ~0u)
        append(ctx.result, " line %u", function.linedefined);

    append(ctx.result, " maxstacksize: %d upvalues: %d flags: %d", function.maxstacksize, function.nups, function.flags);
}
template<typename VmConst>
void toString(ToStringContext& ctx, BcFunction<VmConst>& function, const BcInst& inst, uint32_t index)
{
    size_t start = ctx.result.size();

    append(ctx.result, "  %%%u = %s ", index, getLuauOpcodeName(inst.op));

    bool comma = false;

    for (const BcOp& op : inst.ops)
    {
        if (comma)
            append(ctx.result, ", ");
        comma = true;

        toString(ctx, function, op);
    }

    if (ctx.includeDetailColumn && !inst.uses.empty())
    {
        padToDetailColumn(ctx.result, start);

        append(ctx.result, " ; uses: ");
        toStringUses(ctx, function, inst.uses);
    }
}

template<typename VmConst>
void toString(ToStringContext& ctx, BcFunction<VmConst>& function, const BcProj& proj)
{
    toString(ctx, function, proj.op);
    append(ctx.result, "[%d]", proj.index);
}

template<typename VmConst>
void toString(ToStringContext& ctx, BcFunction<VmConst>& function, const BcPhi& phi, uint32_t index)
{
    size_t start = ctx.result.size();

    append(ctx.result, "  phi.%u = ", index);

    bool comma = false;

    for (const BcOp& op : phi.ops)
    {
        if (comma)
            append(ctx.result, ", ");
        comma = true;

        toString(ctx, function, op);

        if (op.kind == BcOpKind::Inst)
        {
            append(ctx.result, " from ");

            toString(ctx, function, function.instOp(op).block);
        }
    }

    if (ctx.includeDetailColumn && !phi.uses.empty())
    {
        padToDetailColumn(ctx.result, start);

        append(ctx.result, " ; uses: ");
        toStringUses(ctx, function, phi.uses);
    }
}

template<typename VmConst>
void toString(ToStringContext& ctx, BcFunction<VmConst>& function, BcOp op)
{
    switch (op.kind)
    {
    case BcOpKind::None:
        break;
    case BcOpKind::Imm:
        toString(ctx, *function.imm(op));
        break;
    case BcOpKind::Inst:
        append(ctx.result, "%%%u", op.index);
        break;
    case BcOpKind::Block:
        append(ctx.result, "bb_%u", op.index);
        break;
    case BcOpKind::Phi:
        append(ctx.result, "phi.%u", op.index);
        break;
    case BcOpKind::Proj:
        toString(ctx, function, function.projOp(op));
        break;
    case BcOpKind::VmReg:
        append(ctx.result, "R%d", op.index);
        break;
    case BcOpKind::VmConst:
        append(ctx.result, "K%d", op.index);

        append(ctx.result, " (");
        toString(ctx, function, function.constOp(op), false);
        append(ctx.result, ")");
        break;
    case BcOpKind::VmUpvalue:
        append(ctx.result, "U%d", op.index);
        break;
    case BcOpKind::VmProto:
        append(ctx.result, "P%d", op.index);
        break;
    }
}

template<typename VmConst>
void toString(ToStringContext& ctx, BcFunction<VmConst>& function, const BcBlock& block, uint32_t blockIdx)
{
    size_t start = ctx.result.size();

    append(ctx.result, "bb_%d", blockIdx);

    if (function.entryBlock.index == blockIdx)
        append(ctx.result, " (entry)");
    else if (function.exitBlock.index == blockIdx)
        append(ctx.result, " (exit)");

    append(ctx.result, ":");

    if (ctx.includeDetailColumn && block.startpc != kBlockNoStartPc)
    {
        padToDetailColumn(ctx.result, start);
        append(ctx.result, " ; startpc: %u", block.startpc);
    }

    append(ctx.result, "\n");

    if (!block.predecessors.empty())
    {
        append(ctx.result, "; predecessors: ");
        toStringBlockEdges(ctx, function, block.predecessors);
        append(ctx.result, "\n");
    }

    if (!block.successors.empty())
    {
        append(ctx.result, "; successors: ");
        toStringBlockEdges(ctx, function, block.successors);
        append(ctx.result, "\n");
    }

    for (const BcOp& phi : block.phis)
    {
        toString(ctx, function, function.phiOp(phi), phi.index);
        append(ctx.result, "\n");
    }

    for (const BcOp& op : block.ops)
    {
        toString(ctx, function, *function.inst(op), op.index);
        append(ctx.result, "\n");
    }
}

template<typename VmConst>
std::string toString(BcFunction<VmConst>& function, bool includeDetailColumn)
{
    std::string result;
    ToStringContext ctx{result, includeDetailColumn};

    toStringFunctionHeader(ctx, function);
    append(ctx.result, "\n");

    std::vector<BcOp> sortedBlocks;
    sortedBlocks.reserve(function.blocks.size());
    for (uint32_t i = 0; i < function.blocks.size(); i++)
    {
        if ((function.blocks[i].flags & BcBlockFlag::Dead) == 0)
            sortedBlocks.push_back(BcOp{BcOpKind::Block, i});
    }

    std::sort(
        sortedBlocks.begin(),
        sortedBlocks.end(),
        [&](BcOp opA, BcOp opB)
        {
            const BcBlock& a = function.blockOp(opA);
            const BcBlock& b = function.blockOp(opB);

            if (a.sortkey == b.sortkey)
                return a.chainkey < b.chainkey;

            return a.sortkey < b.sortkey;
        }
    );

    for (BcOp op : sortedBlocks)
    {
        const BcBlock& block = function.blocks[op.index];

        toString(ctx, function, block, uint32_t(op.index));

        if (op == function.exitBlock)
            continue;

        if (block.ops.empty())
            append(ctx.result, "  *empty*\n");

        append(ctx.result, "\n");
    }

    return result;
}

std::string dump(BcFunction<BcVmConst>& function);
std::string dump(BcFunction<TValue*>& function);

} // namespace Bytecode
} // namespace Luau
