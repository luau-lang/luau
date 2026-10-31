// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/DumpCFG.h"
#include "Luau/Common.h"
#include "Luau/ControlFlowGraph.h"

#include <cstdio>
#include <sstream>

LUAU_FASTFLAGVARIABLE(DebugLuauLogCFG)
LUAU_FASTFLAGVARIABLE(DebugLuauDumpCFGJson)

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

[[maybe_unused]] static std::string dumpExpr(AstExpr* expr, NotNull<const ControlFlowGraph> cfg)
{
    ExprPrinter printer(cfg);
    expr->visit(&printer);
    return printer.result;
}

static std::string dumpSingletonValue(const SingletonValue& value)
{
    return Luau::visit(
        overloaded{
            [](const StringSingletonValue& stringValue) -> std::string
            {
                return "\"" + stringValue.value + "\"";
            },
            [](const BooleanSingletonValue& booleanValue) -> std::string
            {
                return booleanValue.value ? "true" : "false";
            },
            [](const NilSingletonValue& nilValue) -> std::string
            {
                return "nil";
            },
        },
        value
    );
}

static std::string dumpLeafConstraint(const std::string& target, const LeafConstraint& constraint)
{
    return Luau::visit(
        overloaded{
            [&](const Truthy&) -> std::string
            {
                return target + " is truthy";
            },
            [&](const Falsy&) -> std::string
            {
                return target + " is falsy";
            },
            [&](const TypeEq& eq) -> std::string
            {
                std::string fn = eq.isTypeOf ? "typeof" : "type";
                return fn + "(" + target + ") == \"" + eq.type + "\"";
            },
            [&](const TypeNeq& neq) -> std::string
            {
                std::string fn = neq.isTypeOf ? "typeof" : "type";
                return fn + "(" + target + ") ~= \"" + neq.type + "\"";
            },
            [&](const SingletonEq& eq) -> std::string
            {
                return target + " == " + dumpSingletonValue(eq.v);
            },
            [&](const SingletonNeq& neq) -> std::string
            {
                return target + " ~= " + dumpSingletonValue(neq.v);
            },
        },
        constraint
    );
}

static std::string dumpInstruction(const Instruction* inst, NotNull<const ControlFlowGraph> cfg)
{
    return Luau::visit(
        overloaded{
            [&](const Declare& decl) -> std::string
            {
                return "local " + dumpDef(decl.def) + " = <expr>";
            },
            [&](const LocalFunction& fn) -> std::string
            {
                return "local function " + dumpDef(fn.def);
            },
            [&](const GlobalFunction& fn) -> std::string
            {
                return "global function " + dumpDef(fn.def);
            },
            [&](const Arg& arg) -> std::string
            {
                return "param " + dumpDef(arg.def);
            },
            [&](const Upvalue& uv) -> std::string
            {
                return dumpDef(uv.def) + " = upvalue";
            },
            [&](const Assign& assign) -> std::string
            {
                return dumpDef(assign.def) + " = <expr>";
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
            [](const Refine& refine) -> std::string
            {
                std::string target = dumpDef(refine.target);
                std::string constraint = Luau::visit(
                    overloaded{
                        [&](const Truthy&) -> std::string
                        {
                            return target + " is truthy";
                        },
                        [&](const Falsy&) -> std::string
                        {
                            return target + " is falsy";
                        },
                        [&](const TypeEq& eq) -> std::string
                        {
                            std::string fn = eq.isTypeOf ? "typeof" : "type";
                            return fn + "(" + target + ") == \"" + eq.type + "\"";
                        },
                        [&](const TypeNeq& neq) -> std::string
                        {
                            std::string fn = neq.isTypeOf ? "typeof" : "type";
                            return fn + "(" + target + ") ~= \"" + neq.type + "\"";
                        },
                        [&](const SingletonEq& eq) -> std::string
                        {
                            return target + " == " + dumpSingletonValue(eq.v);
                        },
                        [&](const SingletonNeq& neq) -> std::string
                        {
                            return target + " ~= " + dumpSingletonValue(neq.v);
                        },
                        [&](const HasShape& hs) -> std::string
                        {
                            std::string path = target;
                            for (const auto& p : hs.path)
                                path += "." + p;

                            return dumpLeafConstraint(path, hs.constraint);
                        },
                    },
                    refine.constraint
                );
                return dumpDef(refine.definition) + " = refine(" + constraint + ")";
            },
            [](const Dead&) -> std::string
            {
                return "<dead>";
            }
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

    int nextInstructionId = 1;
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
        const std::vector<InstructionId>& instructions = block->getInstructions();
        for (size_t j = 0; j < instructions.size(); j++)
        {
            if (j > 0)
                out += ',';
            out += "{\"id\":" + std::to_string(nextInstructionId++);
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

std::string dump(const Definition* def)
{
    if (!def)
        return "(null)";
    char buf[128];
    snprintf(buf, sizeof(buf), "%p %s", static_cast<const void*>(def), def->versionedName().c_str());
    printf("%s\n", buf);
    return buf;
}

std::string dump(const Instruction* inst)
{
    if (!inst)
    {
        printf("(null)\n");
        return "(null)";
    }
    std::string result = Luau::visit(
        overloaded{
            [&](const Declare& decl) -> std::string
            {
                return "local " + dumpDef(decl.def);
            },
            [&](const LocalFunction& fn) -> std::string
            {
                return "local function " + dumpDef(fn.def);
            },
            [&](const GlobalFunction& fn) -> std::string
            {
                return "global function " + dumpDef(fn.def);
            },
            [&](const Arg& arg) -> std::string
            {
                return "param " + dumpDef(arg.def);
            },
            [&](const Upvalue& uv) -> std::string
            {
                return dumpDef(uv.def) + " = upvalue";
            },
            [&](const Assign& assign) -> std::string
            {
                return dumpDef(assign.def) + " = <expr>";
            },
            [](const Join& join) -> std::string
            {
                std::string r = dumpDef(join.definition) + " = join(";
                for (size_t i = 0; i < join.operands.size(); i++)
                {
                    if (i > 0)
                        r += ", ";
                    r += dumpDef(join.operands[i]);
                }
                return r + ")";
            },
            [](const Refine& refine) -> std::string
            {
                return dumpDef(refine.definition) + " = refine(" + dumpDef(refine.target) + ")";
            },
            [](const Dead&) -> std::string
            {
                return "<dead>";
            },
        },
        *inst
    );
    printf("%s\n", result.c_str());
    return result;
}

void dump(const Block* block)
{
    if (!block)
    {
        printf("(null)\n");
        return;
    }
    printf(
        "%p %s \"%s\" [%zu instrs]\n",
        static_cast<const void*>(block),
        blockKindName(block->kind),
        block->debugName.c_str(),
        block->getInstructions().size()
    );
    for (const Instruction* inst : block->getInstructions())
    {
        if (inst->get_if<Dead>())
            continue;
        printf("  ");
        dump(inst);
    }
}

std::string dump(SSAId def)
{
    return dump(def.get());
}
std::string dump(InstructionId inst)
{
    return dump(inst.get());
}
void dump(BlockId block)
{
    return dump(block.get());
}

} // namespace Luau
