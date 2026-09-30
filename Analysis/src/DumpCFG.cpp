// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/DumpCFG.h"
#include "Luau/Common.h"
#include "Luau/ControlFlowGraph.h"

#include <cstdio>
#include <sstream>

LUAU_FASTFLAGVARIABLE(DebugLuauLogCFG)
LUAU_FASTFLAGVARIABLE(DebugLuauDumpCFGJson)
using namespace Luau::CFG;

namespace Luau
{

static std::string getLocalName(AstLocal* local)
{
    if (local->name.value)
        return local->name.value;
    return "?";
}

static std::string dumpDef(Definition* def)
{
    return def->versionedName();
}

// Walks an expression tree, printing locals as their resolved definition versions.
struct ExprPrinter : AstVisitor
{
    NotNull<const ControlFlowGraph> cfg;
    std::string result;

    explicit ExprPrinter(NotNull<const ControlFlowGraph> cfg)
        : cfg(cfg)
    {
    }

    bool visit(AstExprLocal* node) override
    {
        if (Definition* def = cfg->getUseDef(node))
            result += dumpDef(def);
        else
            result += getLocalName(node->local) + "?";
        return false;
    }

    bool visit(AstExprConstantNumber* node) override
    {
        if (node->parseResult == ConstantNumberParseResult::Ok && node->value == static_cast<int64_t>(node->value))
            result += std::to_string(static_cast<int64_t>(node->value));
        else
            result += std::to_string(node->value);
        return false;
    }

    bool visit(AstExprConstantString* node) override
    {
        result += "\"" + std::string(node->value.data, node->value.size) + "\"";
        return false;
    }

    bool visit(AstExprConstantBool* node) override
    {
        result += node->value ? "true" : "false";
        return false;
    }

    bool visit(AstExprConstantNil*) override
    {
        result += "nil";
        return false;
    }

    bool visit(AstExprBinary* node) override
    {
        node->left->visit(this);
        result += " " + toString(node->op) + " ";
        node->right->visit(this);
        return false;
    }

    bool visit(AstExprUnary* node) override
    {
        result += toString(node->op);
        node->expr->visit(this);
        return false;
    }

    bool visit(AstExpr* node) override
    {
        result += "<expr>";
        return false;
    }
};

static std::string dumpExpr(AstExpr* expr, NotNull<const ControlFlowGraph> cfg)
{
    ExprPrinter printer(cfg);
    expr->visit(&printer);
    return printer.result;
}

static AstExpr* findRhsExpr(Symbol sym, AstStatLocal* source)
{
    if (!sym.local)
        return nullptr;
    for (size_t i = 0; i < source->vars.size; i++)
    {
        if (source->vars.data[i] == sym.local && i < source->values.size)
            return source->values.data[i];
    }
    return nullptr;
}

static AstExpr* findRhsExpr(Symbol sym, AstStatAssign* source)
{
    for (size_t i = 0; i < source->vars.size; i++)
    {
        if (i >= source->values.size)
            continue;
        AstExpr* var = source->vars.data[i];
        if (sym.local)
        {
            if (auto* exprLocal = var->as<AstExprLocal>())
            {
                if (exprLocal->local == sym.local)
                    return source->values.data[i];
            }
        }
        else if (sym.global.value)
        {
            if (auto* exprGlobal = var->as<AstExprGlobal>())
            {
                if (exprGlobal->name == sym.global)
                    return source->values.data[i];
            }
        }
    }
    return nullptr;
}

static std::string dumpInstruction(const Instruction* inst, NotNull<const ControlFlowGraph> cfg)
{
    return Luau::visit(
        overloaded{
            [&](const Declare& decl) -> std::string
            {
                std::string result = "local " + dumpDef(decl.def);
                if (AstExpr* rhs = findRhsExpr(decl.def->sym, decl.source))
                    result += " = " + dumpExpr(rhs, cfg);
                return result;
            },
            [&](const Assign& assign) -> std::string
            {
                std::string result = dumpDef(assign.def);
                if (AstExpr* rhs = findRhsExpr(assign.def->sym, assign.source))
                    result += " = " + dumpExpr(rhs, cfg);
                return result;
            },
            [](const Join& join) -> std::string
            {
                std::string result = dumpDef(join.definition) + " = join(";
                for (size_t i = 0; i < join.operands.size(); i++)
                {
                    if (i > 0)
                        result += ", ";
                    result += dumpDef(join.operands[i]);
                }
                result += ")";
                return result;
            },
            [](const Refine& flow) -> std::string
            {
                std::string rhs;
                if (flow.type)
                {
                    const char* guard = flow.isTypeof ? "typeof" : "type";
                    const char* cmp = flow.sense ? "==" : "~=";
                    rhs = std::string(guard) + "(" + dumpDef(flow.toRefine) + ") " + cmp + " \"" + *flow.type + "\"";
                }
                else
                {
                    rhs = dumpDef(flow.toRefine) + " " + (flow.sense ? "truthy" : "falsy");
                }
                return dumpDef(flow.definition) + " = refine(" + rhs + ")";
            },
            [](const Dead&) -> std::string
            {
                return "<dead>";
            },
        },
        *inst
    );
}

static std::string dumpBlock(const Block& block, NotNull<const ControlFlowGraph> cfg)
{
    std::string result;
    for (const Instruction* inst : block.getInstructions())
    {
        if (inst->get_if<Dead>())
            continue;
        result += "  " + dumpInstruction(inst, cfg) + "\n";
    }
    return result;
}

static const char* blockKindName(BlockKind kind)
{
    switch (kind)
    {
    case BlockKind::Entry:
        return "entry";
    case BlockKind::Linear:
        return "linear";
    case BlockKind::Condition:
        return "condition";
    }
    LUAU_ASSERT(!"Unknown BlockKind - you may need to add a case to the stringifier here");
    return "?";
}

std::string dumpCFG(const ControlFlowGraph& cfg)
{
    std::stringstream result;
    for (size_t i = 0; i < cfg.blocks.size(); i++)
    {
        const Block* block = cfg.blocks[i];
        result << "Block " << i << " (" << blockKindName(block->kind);
        if (!block->debugName.empty())
            result << " \"" << block->debugName << "\"";
        result << ")";

        const std::vector<BlockId>& successors = block->getSuccessors();
        if (!successors.empty())
        {
            result << " -> [";
            for (size_t j = 0; j < successors.size(); j++)
            {
                if (j > 0)
                    result << ", ";
                for (size_t k = 0; k < cfg.blocks.size(); k++)
                {
                    if (cfg.blocks[k] == successors[j])
                    {
                        result << "B" << k;
                        break;
                    }
                }
            }
            result << "]";
        }

        result << ":\n";
        result << dumpBlock(*block, NotNull{&cfg});
    }
    return result.str();
}

static std::string jsonEscape(const std::string& s)
{
    std::string out;
    out.reserve(s.size() + 2);
    for (char c : s)
    {
        switch (c)
        {
        case '"':
            out += "\\\"";
            break;
        case '\\':
            out += "\\\\";
            break;
        case '\b':
            out += "\\b";
            break;
        case '\f':
            out += "\\f";
            break;
        case '\n':
            out += "\\n";
            break;
        case '\r':
            out += "\\r";
            break;
        case '\t':
            out += "\\t";
            break;
        default:
            if (static_cast<unsigned char>(c) < 0x20)
            {
                char buf[8];
                snprintf(buf, sizeof(buf), "\\u%04x", static_cast<unsigned char>(c));
                out += buf;
            }
            else
            {
                out += c;
            }
        }
    }
    return out;
}

static size_t indexOfBlock(const ControlFlowGraph& cfg, BlockId block)
{
    for (size_t i = 0; i < cfg.blocks.size(); i++)
    {
        if (cfg.blocks[i] == block)
            return i;
    }
    return 0;
}

std::string dumpCFGJson(const ControlFlowGraph& cfg)
{
    // iongraph requires every "loopheader" block to have exactly one predecessor
    // marked "backedge" (asserted in Graph.ts). A loop header is any block whose
    // predecessor has a higher index than itself (the back-edge comes from within
    // the loop body). Pre-compute both flags plus per-block loopDepth in a single
    // pass; loopDepth is the number of loops enclosing the block, and iongraph
    // uses it to resolve each block to its innermost loop header.
    std::vector<bool> isLoopHeader(cfg.blocks.size(), false);
    std::vector<bool> isBackedge(cfg.blocks.size(), false);
    std::vector<std::pair<size_t, size_t>> backedges; // (loop header index, back-edge source index)
    for (size_t i = 0; i < cfg.blocks.size(); i++)
    {
        for (const BlockId& pred : cfg.blocks[i]->getPredecessors())
        {
            size_t predIdx = indexOfBlock(cfg, pred);
            if (predIdx > i)
            {
                isLoopHeader[i] = true;
                isBackedge[predIdx] = true;
                backedges.emplace_back(i, predIdx);
            }
        }
    }
    std::vector<int> loopDepth(cfg.blocks.size(), 0);
    for (size_t i = 0; i < cfg.blocks.size(); i++)
    {
        for (const auto& be : backedges)
        {
            if (be.first <= i && i <= be.second)
                loopDepth[i]++;
        }
    }

    std::string out = "{\"functions\":[{\"name\":\"cfg\",\"passes\":[{\"name\":\"CFG\",\"mir\":{\"blocks\":[";

    int nextInstrId = 1;
    for (size_t i = 0; i < cfg.blocks.size(); i++)
    {
        const Block* block = cfg.blocks[i];
        if (i > 0)
            out += ',';

        out += "{\"number\":" + std::to_string(i);
        out += ",\"loopDepth\":" + std::to_string(loopDepth[i]);

        out += ",\"attributes\":[";
        bool firstAttr = true;
        if (isLoopHeader[i])
        {
            out += "\"loopheader\"";
            firstAttr = false;
        }
        if (isBackedge[i])
        {
            if (!firstAttr)
                out += ',';
            out += "\"backedge\"";
        }
        out += "]";

        out += ",\"predecessors\":[";
        const std::vector<BlockId>& preds = block->getPredecessors();
        for (size_t j = 0; j < preds.size(); j++)
        {
            if (j > 0)
                out += ',';
            out += std::to_string(indexOfBlock(cfg, preds[j]));
        }
        out += "]";

        out += ",\"successors\":[";
        const std::vector<BlockId>& succs = block->getSuccessors();
        for (size_t j = 0; j < succs.size(); j++)
        {
            if (j > 0)
                out += ',';
            out += std::to_string(indexOfBlock(cfg, succs[j]));
        }
        out += "]";

        out += ",\"instructions\":[";
        const std::vector<InstrId>& instructions = block->getInstructions();
        for (size_t j = 0; j < instructions.size(); j++)
        {
            if (j > 0)
                out += ',';
            out += "{\"id\":" + std::to_string(nextInstrId++);
            out += ",\"opcode\":\"" + jsonEscape(dumpInstruction(instructions[j], NotNull{&cfg})) + "\"";
            out += ",\"attributes\":[],\"inputs\":[],\"uses\":[],\"memInputs\":[],\"type\":\"\"}";
        }
        out += "]}";
    }

    // Close blocks array and mir object, then add an empty lir (iongraph touches p.lir.blocks unconditionally).
    out += "]},\"lir\":{\"blocks\":[]}";
    // Close: pass object, passes array, function object, functions array, root object.
    out += "}]}]}";
    return out;
}

} // namespace Luau
