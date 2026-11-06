// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ControlFlowGraph.h"
#include "Luau/Ast.h"
#include "Luau/AstUtils.h"
#include "Luau/Common.h"
#include "Luau/Variant.h"

#include <algorithm>
#include <memory>
#include <optional>

LUAU_FASTFLAG(DebugLuauFreezeArena)

namespace Luau
{

static LeafConstraint invert(const LeafConstraint& c)
{
    return visit(
        overloaded{
            [](const Truthy&) -> LeafConstraint
            {
                return Falsy{};
            },
            [](const Falsy&) -> LeafConstraint
            {
                return Truthy{};
            },
            [](const TypeEq& eq) -> LeafConstraint
            {
                return TypeNeq{eq.type, eq.isTypeOf};
            },
            [](const TypeNeq& neq) -> LeafConstraint
            {
                return TypeEq{neq.type, neq.isTypeOf};
            },
            [](const SingletonEq& eq) -> LeafConstraint
            {
                return SingletonNeq{eq.v};
            },
            [](const SingletonNeq& neq) -> LeafConstraint
            {
                return SingletonEq{neq.v};
            },
        },
        c
    );
}

static NarrowingConstraint invert(const NarrowingConstraint& c)
{
    return visit(
        overloaded{
            [](const Truthy&) -> NarrowingConstraint
            {
                return Falsy{};
            },
            [](const Falsy&) -> NarrowingConstraint
            {
                return Truthy{};
            },
            [](const TypeEq& eq) -> NarrowingConstraint
            {
                return TypeNeq{eq.type, eq.isTypeOf};
            },
            [](const TypeNeq& neq) -> NarrowingConstraint
            {
                return TypeEq{neq.type, neq.isTypeOf};
            },
            [](const SingletonEq& eq) -> NarrowingConstraint
            {
                return SingletonNeq{eq.v};
            },
            [](const SingletonNeq& neq) -> NarrowingConstraint
            {
                return SingletonEq{neq.v};
            },
            [](const HasShape& hs) -> NarrowingConstraint
            {
                return HasShape{hs.path, invert(hs.constraint)};
            },
        },
        c
    );
}

std::optional<Index> Index::fromExpr(AstExpr* expr)
{
    std::vector<std::string> path;
    AstExpr* curr = expr;

    while (true)
    {
        if (auto indexName = curr->as<AstExprIndexName>())
        {
            path.emplace_back(indexName->index.value);
            curr = indexName->expr;
        }
        else if (auto indexExpr = curr->as<AstExprIndexExpr>())
        {
            if (auto str = indexExpr->index->as<AstExprConstantString>())
            {
                path.emplace_back(str->value.data, str->value.size);
                curr = indexExpr->expr;
            }
            else
            {
                return std::nullopt;
            }
        }
        else
        {
            break;
        }
    }

    std::optional<Symbol> rootSym;
    if (auto local = curr->as<AstExprLocal>())
        rootSym = Symbol(local->local);
    else if (auto global = curr->as<AstExprGlobal>())
        rootSym = Symbol(global->name);

    if (!rootSym)
        return std::nullopt;

    std::reverse(path.begin(), path.end());
    return Index{*rootSym, curr, std::move(path)};
}

SSAId Refine::getTarget() const
{
    return target;
}

Block::Block(BlockKind kind, std::string debugName)
    : kind(kind)
    , debugName(debugName)
{
}

void Block::addSuccessor(Block* target)
{
    successors.emplace_back(target);
    target->predecessors.emplace_back(this);
}

bool Block::containsDefinition(Symbol sym) const
{
    return reachingDefinitions.contains(sym);
}

Definition* Block::getReachingDefinition(Symbol sym) const
{
    if (auto* v = reachingDefinitions.find(sym))
        return *v;
    return nullptr;
}

void Block::setReachingDefinition(Symbol sym, SSAId def)
{
    reachingDefinitions[sym] = def;
}

const std::vector<InstructionId>& Block::getInstructions() const
{
    return instructions;
}

const std::vector<BlockId>& Block::getPredecessors() const
{
    return predecessors;
}

const std::vector<BlockId>& Block::getSuccessors() const
{
    return successors;
}

Block* CFGAllocator::newBlock(BlockKind kind, std::string debugName)
{
    return block.allocate(kind, debugName);
}

SSAId CFGAllocator::newDefinition(Symbol sym)
{
    return NotNull{defs.allocate(SymDef{sym, nextVersionIndex(sym)})};
}

void CFGAllocator::freeze()
{
    block.freeze();
    defs.freeze();
    frozen = true;
}

BlockId ControlFlowGraph::newBlock(BlockKind kind, std::string debugName)
{
    Block* b = allocator->newBlock(kind, debugName);
    return blocks.emplace_back(b);
}

std::vector<BlockId> ControlFlowGraph::postOrderAtRoot(BlockId root, DenseHashSet<BlockId>& visited)
{
    std::vector<BlockId> stack{root};
    std::vector<BlockId> ordering;

    while (!stack.empty())
    {
        auto& curr = stack.back();
        visited.insert(curr);
        bool added = false;
        for (auto succ : curr->getSuccessors())
        {
            if (!visited.contains(succ))
            {
                stack.emplace_back(succ);
                added = true;
            }
        }
        if (added)
            continue;

        ordering.emplace_back(curr);
        stack.pop_back();
    }

    return ordering;
}

void ControlFlowGraph::computeRPO()
{
    // TODO: Should this live on the CFGBuilder
    std::vector<BlockId> roots;

    DenseHashSet<BlockId> visited;
    roots.emplace_back(blocks[0]);
    roots.insert(roots.end(), functionBlocks.begin(), functionBlocks.end());

    // Concatenate each subgraph's postorder, then reverse once to get reverse
    // postorder. RPO guarantees a block's definitions are visited before their
    // uses (except across loop back-edges), which the TypeStateMap relies on.
    for (auto& root : roots)
    {
        auto ordering = postOrderAtRoot(root, visited);
        rpoOrder.insert(rpoOrder.end(), ordering.begin(), ordering.end());
    }

    std::reverse(rpoOrder.begin(), rpoOrder.end());
}

Definition* ControlFlowGraph::getUseDef(AstExpr* expr) const
{
    if (auto* def = useDefs.find(expr))
        return *def;
    return nullptr;
}

Definition* ControlFlowGraph::getLhsDef(const LHS& lv) const
{
    if (auto* def = lhsDefs.find(lv))
        return *def;
    return nullptr;
}


CFGBuilder::CFGBuilder(NotNull<CFGAllocator> allocator)
    : cfg(std::make_unique<ControlFlowGraph>(allocator))
    , allocator(allocator)
    , currentBlock(newSealedBlock(BlockKind::Entry, "Entry Block"))
{
}

void CFGBuilder::finalize(AstStatBlock* root)
{
    // This pass tidies up the CFG by
    // a) removing dead instructions
    // b) materializing each definition to allow for the CFG to be in a valid ssa state.
    // The reason to do this is to guarantee that the final CFG does not have any unused instructions
    // or unused definitions left behind.

    auto materializeDef = [](SSAId def)
    {
        return NotNull{def->materialize()};
    };
    for (auto& blk : cfg->blocks)
    {
        std::vector<InstructionId> undead;
        for (auto& instrId : blk->instructions)
        {
            if (instrId->get_if<Dead>())
                continue;

            visit(
                overloaded{
                    [&](Dead&) {},
                    [&](Declare& d)
                    {
                        d.def = materializeDef(d.def);
                    },
                    [&](LocalFunction& fn)
                    {
                        fn.def = materializeDef(fn.def);
                    },
                    [&](GlobalFunction& fn)
                    {
                        fn.def = materializeDef(fn.def);
                    },
                    [&](Arg& a)
                    {
                        a.def = materializeDef(a.def);
                    },
                    [&](Upvalue& u)
                    {
                        u.def = materializeDef(u.def);
                    },
                    [&](Assign& d)
                    {
                        d.def = materializeDef(d.def);
                    },
                    [&](Join& j)
                    {
                        j.definition = materializeDef(j.definition);
                        for (auto& op : j.operands)
                            op = materializeDef(op);
                    },
                    [&](Refine& r)
                    {
                        r.definition = materializeDef(r.definition);
                        r.target = materializeDef(r.target);
                    },
                },
                *instrId
            );

            undead.emplace_back(instrId);
        }

        std::swap(blk->instructions, undead);
    }

    for (auto& [expr, def] : cfg->useDefs)
        def = materializeDef(NotNull{def});
    for (auto& [lv, def] : cfg->lhsDefs)
        def = materializeDef(NotNull{def});

#ifdef LUAU_ASSERTENABLED
    validate(root);
#endif
}

void CFGBuilder::validate(AstStatBlock* root)
{
    // Properties we want to assert about the CFG
    // 1) No Dead Instructions
    // 2) No trivial phis
    // 3) Every variable reference in the AST has a definition
    for (const auto& blocks : cfg->blocks)
    {
        for (const auto& instruction : blocks->instructions)
        {
            if (instruction->get_if<Dead>())
                LUAU_ASSERT(!"Unfinalized CFG: No Dead instructions allowed");

            if (const Join* join = instruction->get_if<Join>())
            {
                auto joinArgs = join->operands.size();
                if (joinArgs == 0)
                    LUAU_ASSERT(
                        !"Malformed CFG: Trivial Phi Node with no arguments - this definitions should have been replaced with a Dead instruction"
                    );
                else if (joinArgs == 1)
                    LUAU_ASSERT(!"Malformed CFG: Trivial Phi Node with one argument - this definition should have been short circuited");
                DenseHashSet<SSAId> args;
                for (const auto& ssaId : join->operands)
                {
                    if (args.contains(ssaId))
                        LUAU_ASSERT(!"Malformed CFG: Phi node with a duplicate operand - we emitted something like v-1 = Join(v-0, v-0)");
                    else
                        args.insert(ssaId);
                }
                if (args.contains(join->definition))
                {
                    LUAU_ASSERT(!"Malformed CFG: self referential phi-node - we emitted something like v-1 := Join(v-1, v-2)");
                }
            }
        }
    }
}

std::unique_ptr<ControlFlowGraph> CFGBuilder::makeCFG(NotNull<CFGAllocator> allocator, AstStatBlock* block)
{
    CFGBuilder builder(allocator);
    builder.lower(block);
    builder.finalize(block);
    auto cfg = std::move(builder.cfg);
    cfg->computeRPO();

    if (FFlag::DebugLuauFreezeArena)
        allocator->freeze();
    return cfg;
}

bool CFGBuilder::isSealed(BlockId b)
{
    return sealedBlocks.contains(b);
}

// The CFGBuilder walks a AstStatBlock and labels definitions in SSA form.
// When we finish walking a block and labelling each def, the block is 'filled'
// Filled blocks are allowed to have successors
// a block is sealed, if no predecessors need to be added
// Because only filled blocks can have successors, predecessors are always filled.
// You should seal a block when you are sure that no more predecessors will be added to it.
// E.g. When lowering a while loop, the condition block might have a back edge from the body of the while loop
// You can only seal this when the body has finished lowering, since that is the last
// predecessor that must be added
void CFGBuilder::seal(BlockId b)
{
    LUAU_ASSERT(!isSealed(b));
    auto joinsToFill = incompleteJoins.find(b);
    if (joinsToFill != nullptr)
    {
        for (auto inst : *joinsToFill)
        {
            // If you're sealing a block and filling the arguments to joins,
            // it's possible for other instructions to get recursively re-written, including other joins.
            // In this case, we should check if the incomplete instruction is still a join
            if (auto join = inst->get_if<Join>())
            {
                fillJoinOperands(b, NotNull{inst}, join);
            }
        }
        incompleteJoins[b] = DenseHashSet<InstructionId>{};
    }
    sealedBlocks.insert(b);
}

ControlFlow CFGBuilder::lower(AstStat* statement)
{
    if (auto blk = statement->as<AstStatBlock>())
        return lower(blk);
    else if (auto local = statement->as<AstStatLocal>())
        return lower(local);
    else if (auto assn = statement->as<AstStatAssign>())
        return lower(assn);
    else if (auto statIf = statement->as<AstStatIf>())
        return lower(statIf);
    else if (auto statWhile = statement->as<AstStatWhile>())
        return lower(statWhile);
    else if (auto statRepeat = statement->as<AstStatRepeat>())
        return lower(statRepeat);
    else if (auto statBreak = statement->as<AstStatBreak>())
        return lower(statBreak);
    else if (auto statContinue = statement->as<AstStatContinue>())
        return lower(statContinue);
    else if (auto statReturn = statement->as<AstStatReturn>())
        return lower(statReturn);
    else if (auto expr = statement->as<AstStatExpr>())
        return lower(expr);
    else if (auto statFor = statement->as<AstStatFor>())
        return lower(statFor);
    else if (auto statForIn = statement->as<AstStatForIn>())
        return lower(statForIn);
    else if (auto compAssn = statement->as<AstStatCompoundAssign>())
        return lower(compAssn);
    else if (auto statFn = statement->as<AstStatFunction>())
        return lower(statFn);
    else if (auto localFn = statement->as<AstStatLocalFunction>())
        return lower(localFn);
    else if (auto typeAlias = statement->as<AstStatTypeAlias>())
        return lower(typeAlias);
    else if (auto typeFn = statement->as<AstStatTypeFunction>())
        return lower(typeFn);
    else if (auto declGlobal = statement->as<AstStatDeclareGlobal>())
        return lower(declGlobal);
    else if (auto declFn = statement->as<AstStatDeclareFunction>())
        return lower(declFn);
    else if (auto declClass = statement->as<AstStatClass>())
        return lower(declClass);
    else if (auto declExternType = statement->as<AstStatDeclareExternType>())
        return lower(declExternType);
    else if (auto err = statement->as<AstStatError>())
        return lower(err);
    else
        LUAU_ASSERT(!"Unhandled statement");
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatExpr* stat)
{
    LoweringExprContext ctx;
    lowerExpr(ctx, stat->expr);
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatRepeat* statRepeat)
{
    Block* entryToCurrent = currentBlock.get();

    // current -> body -> condition -> exit
    //                       \------> body (back-edge)
    //
    // `continue` in a repeat loop jumps to the condition, not the body.
    BlockId bodyBlock = newUnsealedBlock(BlockKind::Linear, "repeat-loop body", entryToCurrent);
    BlockId loopCondition = newUnsealedBlock(BlockKind::Condition, "repeat-loop condition");
    BlockId loopExit = newUnsealedBlock(BlockKind::Linear, "repeat-loop exit");
    PushLoopContext plc(loopStack, loopCondition, loopExit);

    Block* bodyExit;
    ControlFlow bodyFlow;
    {
        BlockScope scope{*this, bodyBlock};
        bodyFlow = lower(statRepeat->body);
        bodyExit = currentBlock.get();
    }

    if (bodyFlow == ControlFlow::None)
        bodyExit->addSuccessor(loopCondition);


    seal(loopCondition);
    if (!loopCondition->getPredecessors().empty())
    {
        BlockId conditionTruthy = newUnsealedBlock(BlockKind::Linear, "repeat-loop condition truthy");
        BlockId conditionFalsy = newUnsealedBlock(BlockKind::Linear, "repeat-loop condition falsy");

        LoweringExprContext ctx;
        ctx.inConditionalContext = true;
        ctx.sense = true;
        ctx.thenBlock = conditionTruthy;
        ctx.elseBlock = conditionFalsy;

        std::optional<LoweringResult> condResult;
        {
            BlockScope scope{*this, loopCondition};
            condResult = lowerExpr(ctx, statRepeat->condition);

            if (condResult && condResult->constraint)
                emitConditionalRefine(*condResult, conditionTruthy, conditionFalsy);
            else if (currentBlock->getSuccessors().empty())
            {
                currentBlock->addSuccessor(conditionTruthy);
                currentBlock->addSuccessor(conditionFalsy);
            }
        }

        conditionTruthy->addSuccessor(loopExit);
        conditionFalsy->addSuccessor(bodyBlock);
        seal(conditionTruthy);
        seal(conditionFalsy);
    }

    seal(bodyBlock);
    seal(loopExit);

    currentBlock = NotNull{loopExit};
    // CLI-233788 - Control flow for 'throws' is incorrectly modelled in CFG
    return matches(bodyFlow, ControlFlow::Breaks | ControlFlow::Continues) ? ControlFlow::None : bodyFlow;
}

ControlFlow CFGBuilder::lower(AstStatBreak*)
{
    if (LoopContext* loop = currentLoop())
        currentBlock->addSuccessor(loop->loopExit);
    return ControlFlow::Breaks;
}

ControlFlow CFGBuilder::lower(AstStatContinue*)
{
    if (LoopContext* loop = currentLoop())
        currentBlock->addSuccessor(loop->loopHeader);
    return ControlFlow::Continues;
}

ControlFlow CFGBuilder::lower(AstStatReturn* statReturn)
{
    for (auto& returnItem : statReturn->list)
    {
        LoweringExprContext ctx{nullptr, nullptr, false, true};
        lowerExpr(ctx, returnItem);
    }

    // TODO: unimplemented
    return ControlFlow::Returns;
}

ControlFlow CFGBuilder::lower(AstStatFor* statFor)
{
    Block* entryToCurrent = currentBlock.get();
    LoweringExprContext ctx;
    // for var = from, to, step do body end
    // Lower the bound expressions in the current block before entering the loop.
    lowerExpr(ctx, statFor->from);
    lowerExpr(ctx, statFor->to);
    if (statFor->step)
        lowerExpr(ctx, statFor->step);

    // Structure mirrors while: header -> body -> header (back-edge)
    //                          header -> exit
    BlockId loopHeader = newUnsealedBlock(BlockKind::Condition, "for-loop condition", entryToCurrent);
    BlockId loopExit = newUnsealedBlock(BlockKind::Linear, "for-loop exit", loopHeader);
    PushLoopContext plc(loopStack, loopHeader, loopExit);

    // Declare the loop variable in the body (scoped to the loop).
    BlockId bodyBlock = newSealedBlock(BlockKind::Linear, "for-loop body", loopHeader);
    {
        Symbol sym{statFor->var};
        emitDefHelper<Declare>(sym, bodyBlock);
    }

    Block* bodyExit;
    ControlFlow bodyFlow;
    {
        BlockScope scope{*this, bodyBlock};
        bodyFlow = lower(statFor->body);
        bodyExit = currentBlock.get();
    }

    if (bodyFlow == ControlFlow::None)
        bodyExit->addSuccessor(loopHeader);
    seal(loopHeader);
    seal(loopExit);

    currentBlock = NotNull{loopExit};
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatForIn* statForIn)
{
    Block* entryToCurrent = currentBlock.get();
    LoweringExprContext ctx;
    // for vars in values do body end
    // Lower the iterator expressions in the current block.
    for (size_t i = 0; i < statForIn->values.size; i++)
        lowerExpr(ctx, statForIn->values.data[i]);

    BlockId loopHeader = newUnsealedBlock(BlockKind::Condition, "for-in-loop condition", entryToCurrent);
    BlockId loopExit = newUnsealedBlock(BlockKind::Linear, "for-in-loop exit", loopHeader);
    PushLoopContext plc(loopStack, loopHeader, loopExit);

    // Declare loop variables in the body (scoped to the loop).
    BlockId bodyBlock = newSealedBlock(BlockKind::Linear, "for-in-loop body", loopHeader);
    for (size_t i = 0; i < statForIn->vars.size; i++)
    {
        AstLocal* loc = statForIn->vars.data[i];
        Symbol sym{loc};
        emitDefHelper<Declare>(sym, bodyBlock);
    }

    Block* bodyExit;
    ControlFlow bodyFlow;
    {
        BlockScope scope{*this, bodyBlock};
        bodyFlow = lower(statForIn->body);
        bodyExit = currentBlock.get();
    }

    if (bodyFlow == ControlFlow::None)
        bodyExit->addSuccessor(loopHeader);
    seal(loopHeader);
    seal(loopExit);

    currentBlock = NotNull{loopExit};
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatCompoundAssign* compAssn)
{
    // TODO: Unimplemented
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatFunction* fn)
{
    //  function f() ... end
    // turns into GlobalFunction F w/ a child block for the function body

    AstName n;
    if (auto glob = fn->name->as<AstExprGlobal>())
    {
        n = glob->name;
    }
    else
    {
        LUAU_ASSERT(!"AstStatFunction with a non-global name Unimplemented");
    }


    SSAId functionNameDef = emitDefHelper<GlobalFunction>(Symbol{n}, currentBlock);
    // TODO: lower the definition into

    std::string fnName = std::string(n.value ? n.value : "empty global");
    BlockId fnBlock = newSealedBlock(BlockKind::Linear, "function " + fnName);
    // Arguments lower into the function
    {
        BlockScope scope{*this, fnBlock};
        currentBlock->setReachingDefinition(n, functionNameDef);
        for (const auto& arg : fn->func->args)
        {
            Symbol argSym{arg};
            SSAId argDef = emitDefHelper<Arg>(argSym, currentBlock);
            // Record the parameter's initial def so the ConstraintGenerator can
            // bind it to the signature's argument type.
            cfg->lhsDefs[LHS{argSym}] = argDef.get();
        }

        lower(fn->func->body);
    }

    cfg->functionBlocks.emplace_back(fnBlock);
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatLocalFunction* localFn)
{
    // local function f() ... end
    // turns into LocalFunction F
    // there is a child block
    SSAId functionNameDef = emitDefHelper<LocalFunction>(localFn->name, currentBlock);

    std::string fnName = std::string(localFn->name->name.value);
    BlockId fnBlock = newSealedBlock(BlockKind::Linear, "function " + fnName);
    // Arguments lower into the function
    {
        BlockScope scope{*this, fnBlock};
        currentBlock->setReachingDefinition(localFn->name, functionNameDef);
        for (const auto& arg : localFn->func->args)
        {
            Symbol argSym{arg};
            SSAId argDef = emitDefHelper<Arg>(argSym, currentBlock);
            // Record the parameter's initial def so the ConstraintGenerator can
            // bind it to the signature's argument type.
            cfg->lhsDefs[LHS{argSym}] = argDef.get();
        }

        lower(localFn->func->body);
    }

    cfg->functionBlocks.emplace_back(fnBlock);
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatTypeAlias* alias)
{
    // TODO: Unimplemented
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatTypeFunction*)
{
    // TODO: Unimplemented
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatDeclareGlobal*)
{
    // TODO: Unimplemented
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatDeclareFunction*)
{
    // TODO: Unimplemented
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatClass*)
{
    // TODO: Unimplemented
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatDeclareExternType*)
{
    // TODO: Unimplemented
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatError* err)
{
    for (const auto& stat : err->statements)
        lower(stat);

    LoweringExprContext ctx;
    for (const auto& expr : err->expressions)
        lowerExpr(ctx, expr);

    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatBlock* block)
{
    for (auto st : block->body)
    {
        ControlFlow cf = lower(st);
        if (cf != ControlFlow::None)
            return cf;
    }
    return ControlFlow::None;
}

ControlFlow CFGBuilder::lower(AstStatLocal* local)
{

    for (size_t i = 0; i < local->vars.size; i++)
    {
        AstLocal* loc = local->vars.data[i];
        AstExpr* expr = i < local->values.size ? local->values.data[i] : nullptr;
        LoweringExprContext ctx;
        if (expr)
            lowerExpr(ctx, expr);

        Symbol sym{loc};
        SSAId def = emitDefHelper<Declare>(sym, currentBlock);
        cfg->lhsDefs[LHS{sym}] = def.get();
    }
    return ControlFlow::None;
}

void CFGBuilder::lowerLValue(LoweringExprContext& ctx, AstExpr* target)
{
    if (auto local = target->as<AstExprLocal>())
    {
        Symbol sym{local->local};
        SSAId def = emitDefHelper<Assign>(sym, currentBlock);
        cfg->lhsDefs[LHS{target}] = def.get();
    }
    else if (auto global = target->as<AstExprGlobal>())
    {
        Symbol sym{global->name};
        SSAId def = emitDefHelper<Assign>(sym, currentBlock);
        cfg->lhsDefs[LHS{target}] = def.get();
    }
}

ControlFlow CFGBuilder::lower(AstStatAssign* assn)
{
    LoweringExprContext ctx;
    for (size_t i = 0; i < assn->values.size; i++)
        lowerExpr(ctx, assn->values.data[i]);

    for (size_t i = 0; i < assn->vars.size; i++)
        lowerLValue(ctx, assn->vars.data[i]);

    return ControlFlow::None;
}

BlockId CFGBuilder::newSealedBlock(BlockKind kind, std::string debugName, Block* pred)
{
    BlockId b = newUnsealedBlock(kind, debugName, pred);
    seal(b);
    return b;
}

BlockId CFGBuilder::newUnsealedBlock(BlockKind kind, std::string debugName, Block* pred)
{
    BlockId b = cfg->newBlock(kind, debugName);
    if (pred)
        pred->addSuccessor(b);
    return b;
}

SSAId CFGBuilder::newDefinition(Symbol sym)
{
    return allocator->newDefinition(sym);
}

std::pair<InstructionId, NotNull<Join>> CFGBuilder::emitJoin(BlockId block, Symbol sym)
{
    SSAId def = newDefinition(sym);
    InstructionId jInstr = emit<Join>(block, def);
    Join* j = jInstr->get_if<Join>();
    LUAU_ASSERT(j);

    block->setReachingDefinition(sym, def);
    incompleteJoins[block].insert(jInstr);
    return {jInstr, NotNull{j}};
}

ControlFlow CFGBuilder::lower(AstStatIf* statIf)
{
    BlockId thenBlock = newUnsealedBlock(BlockKind::Linear, "then block");
    BlockId elseBlock = newUnsealedBlock(BlockKind::Linear, "else block");

    LoweringExprContext ctx;
    ctx.inConditionalContext = true;
    ctx.thenBlock = thenBlock;
    ctx.elseBlock = elseBlock;
    ctx.sense = true;
    auto result = lowerExpr(ctx, statIf->condition);

    if (result && result->constraint)
        emitConditionalRefine(*result, thenBlock, elseBlock);
    else if (statIf->conditionLocal)
    {
        SSAId binding = readVariable(currentBlock, statIf->conditionLocal);
        cfg->lhsDefs[LHS{Symbol(statIf->conditionLocal)}] = binding;
        LoweringResult ifLocal{binding, Truthy{}};
        emitConditionalRefine(ifLocal, thenBlock, elseBlock);
    }
    else if (currentBlock->getSuccessors().empty())
    {
        currentBlock->addSuccessor(thenBlock);
        currentBlock->addSuccessor(elseBlock);
    }

    seal(thenBlock);
    seal(elseBlock);

    Block* thenExit;
    ControlFlow thenFlow;
    {
        BlockScope scope{*this, thenBlock};
        thenFlow = lower(statIf->thenbody);
        thenExit = currentBlock.get();
    }

    Block* elseExit;
    ControlFlow elseFlow = ControlFlow::None;
    {
        BlockScope scope(*this, elseBlock);
        if (statIf->elsebody)
            elseFlow = lower(statIf->elsebody);
        elseExit = currentBlock.get();
    }

    BlockId mergeBlock = newUnsealedBlock(BlockKind::Linear, "merge");
    if (thenFlow == ControlFlow::None)
        thenExit->addSuccessor(mergeBlock);
    if (elseFlow == ControlFlow::None)
        elseExit->addSuccessor(mergeBlock);
    seal(mergeBlock);
    currentBlock = NotNull{mergeBlock};
    if (thenFlow == elseFlow)
        return thenFlow;
    else if (matches(thenFlow, ControlFlow::Returns | ControlFlow::Throws) && matches(elseFlow, ControlFlow::Returns | ControlFlow::Throws))
        return ControlFlow::Returns;
    else
        return ControlFlow::None;
}


ControlFlow CFGBuilder::lower(AstStatWhile* statWhile)
{
    Block* entryToCurrent = currentBlock.get();

    // current -> loopHeader -> body -> loopHeader (back-edge)
    //                       -> loopExit
    BlockId loopHeader = newUnsealedBlock(BlockKind::Condition, "while-loop condition", entryToCurrent);
    BlockId bodyBlock = newUnsealedBlock(BlockKind::Linear, "while-loop body");
    BlockId loopExit = newUnsealedBlock(BlockKind::Linear, "while-loop exit");
    PushLoopContext plc(loopStack, loopHeader, loopExit);

    // Lower the condition in the header with conditional context
    LoweringExprContext ctx;
    ctx.inConditionalContext = true;
    ctx.sense = true;
    ctx.thenBlock = bodyBlock;
    ctx.elseBlock = loopExit;
    std::optional<LoweringResult> condResult;
    {
        BlockScope scope{*this, loopHeader};
        condResult = lowerExpr(ctx, statWhile->condition);

        if (condResult && condResult->constraint)
            emitConditionalRefine(*condResult, bodyBlock, loopExit);
        else if (currentBlock->getSuccessors().empty())
        {
            currentBlock->addSuccessor(bodyBlock);
            currentBlock->addSuccessor(loopExit);
        }
    }

    seal(bodyBlock);

    Block* bodyExit;
    ControlFlow bodyFlow;
    {
        BlockScope scope{*this, bodyBlock};
        bodyFlow = lower(statWhile->body);
        bodyExit = currentBlock.get();
    }

    if (bodyFlow == ControlFlow::None)
        bodyExit->addSuccessor(loopHeader);
    seal(loopHeader);
    seal(loopExit);

    currentBlock = NotNull{loopExit};
    return ControlFlow::None;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExpr* expr)
{
    if (auto group = expr->as<AstExprGroup>())
        return lowerExpr(ctx, group);
    else if (auto nil = expr->as<AstExprConstantNil>())
        return std::nullopt;
    else if (auto boolExpr = expr->as<AstExprConstantBool>())
        return std::nullopt;
    else if (auto num = expr->as<AstExprConstantNumber>())
        return std::nullopt;
    else if (auto intExpr = expr->as<AstExprConstantInteger>())
        return std::nullopt;
    else if (auto str = expr->as<AstExprConstantString>())
        return std::nullopt;
    else if (auto local = expr->as<AstExprLocal>())
        return lowerExpr(ctx, local);
    else if (auto global = expr->as<AstExprGlobal>())
        return lowerExpr(ctx, global);
    else if (auto varargs = expr->as<AstExprVarargs>())
        return lowerExpr(ctx, varargs);
    else if (auto call = expr->as<AstExprCall>())
        return lowerExpr(ctx, call);
    else if (auto indexName = expr->as<AstExprIndexName>())
        return lowerExpr(ctx, indexName);
    else if (auto indexExpr = expr->as<AstExprIndexExpr>())
        return lowerExpr(ctx, indexExpr);
    else if (auto func = expr->as<AstExprFunction>())
        return lowerExpr(ctx, func);
    else if (auto table = expr->as<AstExprTable>())
        return lowerExpr(ctx, table);
    else if (auto unary = expr->as<AstExprUnary>())
        return lowerExpr(ctx, unary);
    else if (auto binop = expr->as<AstExprBinary>())
        return lowerExpr(ctx, binop);
    else if (auto typeAssertion = expr->as<AstExprTypeAssertion>())
        return lowerExpr(ctx, typeAssertion);
    else if (auto ifElse = expr->as<AstExprIfElse>())
        return lowerExpr(ctx, ifElse);
    else if (auto interpStr = expr->as<AstExprInterpString>())
        return lowerExpr(ctx, interpStr);
    else if (auto instantiate = expr->as<AstExprInstantiate>())
        return lowerExpr(ctx, instantiate);
    else if (auto err = expr->as<AstExprError>())
        return lowerExpr(ctx, err);
    else
        LUAU_ASSERT(!"Unhandled expression type");

    return std::nullopt;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprGroup* group)
{
    return lowerExpr(ctx, group->expr);
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprLocal* local)
{
    if (local->upvalue)
    {
        SSAId def = emitDefHelper<Upvalue>(local->local, currentBlock);
        return LoweringResult{def};
    }
    SSAId def = readVariable(currentBlock, Symbol(local->local));
    cfg->useDefs[local] = def;
    return LoweringResult{def, Truthy{}};
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprGlobal* global)
{
    SSAId def = readVariable(currentBlock, Symbol(global->name));
    cfg->useDefs[global] = def;
    return LoweringResult{def, Truthy{}};
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprVarargs* args)
{
    return std::nullopt;
}


std::optional<LoweringResult> CFGBuilder::handleAssert(LoweringExprContext& ctx, const AstExprCall& call)
{
    if (call.args.size == 0)
        return std::nullopt;

    auto global = call.func->as<AstExprGlobal>();
    if (!global || global->name != "assert")
        return std::nullopt;

    BlockId resumeBlock = newUnsealedBlock(BlockKind::Condition, "assertion-finish");
    BlockId elseBlock = newUnsealedBlock(BlockKind::Condition, "assertion-err-else");

    LoweringExprContext c;
    c.inConditionalContext = true;
    c.sense = true;
    c.thenBlock = resumeBlock;
    c.elseBlock = elseBlock;
    // TODO: Handle additional arguments to assert, such as the error message.
    // Also traverse through the function associated with assert.
    auto result = lowerExpr(c, call.args.data[0]);

    if (result && result->constraint)
        emitConditionalRefine(*result, resumeBlock, elseBlock);
    else if (currentBlock->getSuccessors().empty())
    {
        currentBlock->addSuccessor(resumeBlock);
        currentBlock->addSuccessor(elseBlock);
    }

    seal(resumeBlock);
    seal(elseBlock);
    currentBlock = resumeBlock;
    return result;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprCall* call)
{
    if (auto loweringResult = handleAssert(ctx, *call))
        return loweringResult;
    lowerExpr(ctx, call->func);
    for (size_t i = 0; i < call->args.size; i++)
        lowerExpr(ctx, call->args.data[i]);
    return std::nullopt;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprIndexName* indexName)
{
    auto idx = Index::fromExpr(indexName);
    if (!idx || idx->path.empty())
    {
        LoweringExprContext subCtx;
        lowerExpr(subCtx, indexName->expr);
        return std::nullopt;
    }

    SSAId def = readVariable(currentBlock, idx->source);
    cfg->useDefs[idx->sourceExpr] = def;

    return LoweringResult{def, HasShape{std::move(idx->path), Truthy{}}};
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprIndexExpr* indexExpr)
{
    // TODO: Unimplemented
    return std::nullopt;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprFunction*)
{
    // TODO: Unimplemented
    return std::nullopt;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprTable* table)
{

    // TODO: Unimplemented
    return std::nullopt;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprUnary* unary)
{
    if (unary->op == AstExprUnary::Op::Not)
    {
        ctx.sense = !ctx.sense;
        auto result = lowerExpr(ctx, unary->expr);
        if (result && result->constraint)
            return LoweringResult{result->def, invert(*result->constraint)};
        return result;
    }

    LoweringExprContext subCtx;
    lowerExpr(subCtx, unary->expr);
    return std::nullopt;
}

// Wires currentBlock to truthyDest and falsyDest, emitting Refine instructions
// based on the LowerResult's constraint.
void CFGBuilder::emitConditionalRefine(LoweringResult& result, Block* truthyDest, Block* falsyDest)
{
    currentBlock->addSuccessor(truthyDest);
    currentBlock->addSuccessor(falsyDest);

    Symbol sym = result.def->getSym();

    emitDefHelper<Refine>(sym, truthyDest, result.def, NarrowingConstraint{*result.constraint});
    emitDefHelper<Refine>(sym, falsyDest, result.def, invert(*result.constraint));
}

static std::optional<LeafConstraint> lowerSingletonExpr(AstExpr* expr, bool sense)
{
    if (auto grp = expr->as<AstExprGroup>())
        return lowerSingletonExpr(grp->expr, sense);
    else if (auto str = expr->as<AstExprConstantString>())
    {
        auto stringleton = StringSingletonValue{std::string(str->value.data, str->value.size)};
        if (sense)
            return SingletonEq{std::move(stringleton)};
        else
            return SingletonNeq{std::move(stringleton)};
    }
    else if (auto b = expr->as<AstExprConstantBool>())
    {
        auto boolsingleton = BooleanSingletonValue{b->value};
        if (sense)
            return SingletonEq{std::move(boolsingleton)};
        else
            return SingletonNeq{std::move(boolsingleton)};
    }
    else if (auto n = expr->as<AstExprConstantNil>())
    {
        if (sense)
            return SingletonEq{NilSingletonValue{}};
        else
            return SingletonNeq{NilSingletonValue{}};
    }

    return std::nullopt;
}

struct SingletonGuard
{
    SingletonGuard(AstExpr* toLower, LeafConstraint c)
        : toLower(toLower)
        , c(std::move(c))
    {
    }
    AstExpr* toLower;
    LeafConstraint c;
};

static std::optional<SingletonGuard> matchSingletonGuard(AstExprBinary* expr)
{
    if (expr->op != AstExprBinary::CompareEq && expr->op != AstExprBinary::CompareNe)
        return std::nullopt;

    bool sense = expr->op == AstExprBinary::CompareEq;
    if (auto lhs = lowerSingletonExpr(expr->left, sense))
        return SingletonGuard{expr->right, *lhs};

    if (auto rhs = lowerSingletonExpr(expr->right, sense))
        return SingletonGuard{expr->left, *rhs};

    return std::nullopt;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprBinary* binop)
{
    if (auto tg = matchTypeGuard(binop->op, binop->left, binop->right))
    {
        AstExpr* target = tg->target;

        if (auto idx = Index::fromExpr(target))
        {
            SSAId def = readVariable(currentBlock, idx->source);
            cfg->useDefs[idx->sourceExpr] = def;

            if (idx->path.empty())
            {
                if (binop->op == AstExprBinary::CompareEq)
                    return LoweringResult{def, TypeEq{tg->type, tg->isTypeof}};
                else
                    return LoweringResult{def, TypeNeq{tg->type, tg->isTypeof}};
            }
            else
            {
                LeafConstraint leaf = (binop->op == AstExprBinary::CompareEq) ? LeafConstraint{TypeEq{tg->type, tg->isTypeof}}
                                                                              : LeafConstraint{TypeNeq{tg->type, tg->isTypeof}};
                return LoweringResult{def, HasShape{idx->path, std::move(leaf)}};
            }
        }

        LoweringExprContext subCtx;
        lowerExpr(subCtx, target);
        return std::nullopt;
    }

    if (auto sg = matchSingletonGuard(binop))
    {
        if (auto idx = Index::fromExpr(sg->toLower))
        {
            SSAId def = readVariable(currentBlock, idx->source);
            cfg->useDefs[idx->sourceExpr] = def;

            if (idx->path.empty())
            {
                if (auto eq = sg->c.get_if<SingletonEq>())
                    return LoweringResult{def, SingletonEq{eq->v}};
                else if (auto neq = sg->c.get_if<SingletonNeq>())
                    return LoweringResult{def, SingletonNeq{neq->v}};

                LUAU_ASSERT(!"singleton guard should only produce singleton constraints");
                return std::nullopt;
            }

            return LoweringResult{def, HasShape{idx->path, std::move(sg->c)}};
        }

        LoweringExprContext subCtx;
        lowerExpr(subCtx, sg->toLower);
        return std::nullopt;
    }

    if (binop->op == AstExprBinary::And)
    {
        if (ctx.inConditionalContext)
        {
            // and short-circuits on falsy
            Block* shortCircuitDest = ctx.sense ? ctx.elseBlock : ctx.thenBlock;
            Block* truthyDest = ctx.sense ? ctx.thenBlock : ctx.elseBlock;
            BlockId continueBlock = newUnsealedBlock(BlockKind::Linear, "and-rhs");

            // Intermediate block for lhs falsy refinement lives here, flows to shortCircuitDest
            BlockId lhsFalsyBlock = newUnsealedBlock(BlockKind::Linear, "and-lhs-falsy");

            // Lower lhs — compound operands need conditional context to wire internally
            LoweringExprContext lhsCtx;
            lhsCtx.inConditionalContext = true;
            lhsCtx.sense = true;
            lhsCtx.thenBlock = continueBlock;
            lhsCtx.elseBlock = lhsFalsyBlock;
            auto lhsResult = lowerExpr(lhsCtx, binop->left);

            // curr -> continue(and lhs truthy) block
            //.     -> continue(and lhs falsy) block -> dest
            //
            if (lhsResult && lhsResult->constraint)
                emitConditionalRefine(*lhsResult, continueBlock, lhsFalsyBlock);
            else if (currentBlock->getSuccessors().empty())
            {
                currentBlock->addSuccessor(continueBlock);
                currentBlock->addSuccessor(lhsFalsyBlock);
            }

            lhsFalsyBlock->addSuccessor(shortCircuitDest);
            seal(lhsFalsyBlock);
            seal(continueBlock);

            {
                BlockScope scope{*this, continueBlock};
                auto rhsResult = lowerExpr(ctx, binop->right);

                if (rhsResult && rhsResult->constraint)
                {
                    // Intermediate block for rhs falsy refinement lives here, flows to shortCircuitDest
                    BlockId rhsFalsyBlock = newSealedBlock(BlockKind::Linear, "and-rhs-falsy", currentBlock.get());
                    {
                        Symbol sym = rhsResult->def->getSym();
                        emitDefHelper<Refine>(sym, rhsFalsyBlock, rhsResult->def, invert(*rhsResult->constraint));
                    }
                    rhsFalsyBlock->addSuccessor(shortCircuitDest);

                    // Intermediate block for rhs truthy refinement lives here, flows to truthyDest
                    BlockId rhsTruthyBlock = newSealedBlock(BlockKind::Linear, "and-rhs-truthy", currentBlock.get());
                    {
                        Symbol sym = rhsResult->def->getSym();
                        emitDefHelper<Refine>(sym, rhsTruthyBlock, rhsResult->def, NarrowingConstraint{*rhsResult->constraint});
                    }
                    rhsTruthyBlock->addSuccessor(truthyDest);
                }
                else if (currentBlock->getSuccessors().empty())
                {
                    currentBlock->addSuccessor(truthyDest);
                    currentBlock->addSuccessor(shortCircuitDest);
                }
            }

            return std::nullopt;
        }

        // Non-conditional: value-producing `and`
        Block* currBlock = currentBlock.get();
        LoweringExprContext subCtx;
        auto lhs = lowerExpr(subCtx, binop->left);
        if (!lhs || !lhs->constraint)
        {
            lowerExpr(subCtx, binop->right);
            return std::nullopt;
        }

        BlockId rhsBlock = newSealedBlock(BlockKind::Linear, "and-rhs", currBlock);
        BlockId shortCircuitBlock = newSealedBlock(BlockKind::Linear, "and-short-circuit", currBlock);

        std::optional<LoweringResult> rhsResult;
        {
            BlockScope scope{*this, rhsBlock};
            Symbol sym = lhs->def->getSym();
            emitDefHelper<Refine>(sym, rhsBlock, lhs->def, NarrowingConstraint{*lhs->constraint});
            rhsResult = lowerExpr(subCtx, binop->right);
        }

        {
            BlockScope scope{*this, shortCircuitBlock};
            Symbol sym = lhs->def->getSym();
            emitDefHelper<Refine>(sym, shortCircuitBlock, lhs->def, invert(*lhs->constraint));
        }

        BlockId mergeBlock = newUnsealedBlock(BlockKind::Linear, "and-merge");
        rhsBlock.get()->addSuccessor(mergeBlock);
        shortCircuitBlock.get()->addSuccessor(mergeBlock);
        seal(mergeBlock);
        currentBlock = mergeBlock;

        return rhsResult;
    }

    if (binop->op == AstExprBinary::Or)
    {
        if (ctx.inConditionalContext)
        {
            // or short-circuits on truthy
            Block* shortCircuitDest = ctx.sense ? ctx.thenBlock : ctx.elseBlock;
            Block* falsyDest = ctx.sense ? ctx.elseBlock : ctx.thenBlock;
            BlockId continueBlock = newUnsealedBlock(BlockKind::Linear, "or-rhs");

            // Intermediate block for lhs truthy — refinement lives here, flows to shortCircuitDest
            BlockId lhsTruthyBlock = newUnsealedBlock(BlockKind::Linear, "or-lhs-truthy");

            // Lower lhs — for `or`, truthy → short-circuit, falsy → continue
            LoweringExprContext lhsCtx;
            lhsCtx.inConditionalContext = true;
            lhsCtx.sense = true;
            lhsCtx.thenBlock = lhsTruthyBlock;
            lhsCtx.elseBlock = continueBlock;
            auto lhsResult = lowerExpr(lhsCtx, binop->left);

            if (lhsResult && lhsResult->constraint)
                emitConditionalRefine(*lhsResult, lhsTruthyBlock, continueBlock);
            else if (currentBlock->getSuccessors().empty())
            {
                currentBlock->addSuccessor(lhsTruthyBlock);
                currentBlock->addSuccessor(continueBlock);
            }

            lhsTruthyBlock->addSuccessor(shortCircuitDest);
            seal(lhsTruthyBlock);
            seal(continueBlock);

            {
                BlockScope scope{*this, continueBlock};
                auto rhsResult = lowerExpr(ctx, binop->right);

                if (rhsResult && rhsResult->constraint)
                {
                    // Intermediate block for rhs truthy — refinement lives here, flows to shortCircuitDest
                    BlockId rhsTruthyBlock = newSealedBlock(BlockKind::Linear, "or-rhs-truthy", currentBlock.get());
                    {
                        Symbol sym = rhsResult->def->getSym();
                        emitDefHelper<Refine>(sym, rhsTruthyBlock, rhsResult->def, NarrowingConstraint{*rhsResult->constraint});
                    }
                    rhsTruthyBlock->addSuccessor(shortCircuitDest);

                    // Intermediate block for rhs falsy — refinement lives here, flows to falsyDest
                    BlockId rhsFalsyBlock = newSealedBlock(BlockKind::Linear, "or-rhs-falsy", currentBlock.get());
                    {
                        Symbol sym = rhsResult->def->getSym();
                        emitDefHelper<Refine>(sym, rhsFalsyBlock, rhsResult->def, invert(*rhsResult->constraint));
                    }
                    rhsFalsyBlock->addSuccessor(falsyDest);
                }
                else if (currentBlock->getSuccessors().empty())
                {
                    currentBlock->addSuccessor(shortCircuitDest);
                    currentBlock->addSuccessor(falsyDest);
                }
            }

            return std::nullopt;
        }

        // Non-conditional: value-producing `or`
        Block* currBlock = currentBlock.get();
        LoweringExprContext subCtx;
        auto lhs = lowerExpr(subCtx, binop->left);
        if (!lhs || !lhs->constraint)
        {
            lowerExpr(subCtx, binop->right);
            return std::nullopt;
        }

        BlockId shortCircuitBlock = newSealedBlock(BlockKind::Linear, "or-short-circuit", currBlock);
        BlockId rhsBlock = newSealedBlock(BlockKind::Linear, "or-rhs", currBlock);

        {
            BlockScope scope{*this, shortCircuitBlock};
            Symbol sym = lhs->def->getSym();
            emitDefHelper<Refine>(sym, shortCircuitBlock, lhs->def, NarrowingConstraint{*lhs->constraint});
        }

        std::optional<LoweringResult> rhsResult;
        {
            BlockScope scope{*this, rhsBlock};
            Symbol sym = lhs->def->getSym();
            emitDefHelper<Refine>(sym, rhsBlock, lhs->def, invert(*lhs->constraint));
            rhsResult = lowerExpr(subCtx, binop->right);
        }

        BlockId mergeBlock = newUnsealedBlock(BlockKind::Linear, "or-merge");
        shortCircuitBlock.get()->addSuccessor(mergeBlock);
        rhsBlock.get()->addSuccessor(mergeBlock);
        seal(mergeBlock);
        currentBlock = mergeBlock;

        return rhsResult;
    }

    LoweringExprContext subCtx;
    lowerExpr(subCtx, binop->left);
    lowerExpr(subCtx, binop->right);
    return std::nullopt;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprTypeAssertion* typeAssertion)
{
    LoweringExprContext subCtx;
    lowerExpr(subCtx, typeAssertion->expr);
    return std::nullopt;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprIfElse* ifElse)
{
    BlockId thenBlock = newUnsealedBlock(BlockKind::Linear, "if-else expr then");
    BlockId elseBlock = newUnsealedBlock(BlockKind::Linear, "if-else expr else");

    LoweringExprContext condCtx;
    condCtx.inConditionalContext = true;
    condCtx.thenBlock = thenBlock;
    condCtx.elseBlock = elseBlock;
    condCtx.sense = true;
    auto condResult = lowerExpr(condCtx, ifElse->condition);

    if (condResult && condResult->constraint)
        emitConditionalRefine(*condResult, thenBlock, elseBlock);
    else if (ifElse->conditionLocal)
    {
        SSAId binding = readVariable(currentBlock, ifElse->conditionLocal);
        cfg->lhsDefs[LHS{Symbol(ifElse->conditionLocal)}] = binding;
        LoweringResult ifLocal{binding, Truthy{}};
        emitConditionalRefine(ifLocal, thenBlock, elseBlock);
    }
    else if (currentBlock->getSuccessors().empty())
    {
        currentBlock->addSuccessor(thenBlock);
        currentBlock->addSuccessor(elseBlock);
    }

    seal(thenBlock);
    seal(elseBlock);

    {
        BlockScope scope{*this, thenBlock};
        LoweringExprContext subCtx;
        lowerExpr(subCtx, ifElse->trueExpr);
    }

    {
        BlockScope scope{*this, elseBlock};
        LoweringExprContext subCtx;
        lowerExpr(subCtx, ifElse->falseExpr);
    }

    BlockId mergeBlock = newUnsealedBlock(BlockKind::Linear, "if-else expr merge");
    thenBlock.get()->addSuccessor(mergeBlock);
    elseBlock.get()->addSuccessor(mergeBlock);
    seal(mergeBlock);
    currentBlock = mergeBlock;

    return std::nullopt;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprInterpString* interpStr)
{
    for (size_t i = 0; i < interpStr->expressions.size; i++)
        lowerExpr(ctx, interpStr->expressions.data[i]);
    return std::nullopt;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprInstantiate* instantiate)
{
    lowerExpr(ctx, instantiate->expr);
    return std::nullopt;
}

std::optional<LoweringResult> CFGBuilder::lowerExpr(LoweringExprContext& ctx, AstExprError* err)
{
    for (size_t i = 0; i < err->expressions.size; i++)
        lowerExpr(ctx, err->expressions.data[i]);

    return std::nullopt;
}

SSAId CFGBuilder::readVariable(BlockId block, Symbol sym)
{
    if (auto v = block->getReachingDefinition(sym); v != nullptr)
        return NotNull{v};

    if (!isSealed(block))
    {
        auto p = emitJoin(block, sym);
        return p.second->definition;
    }
    else if (block->getPredecessors().size() == 1)
    {
        auto def = readVariable(block->getPredecessors().front(), sym);
        block->setReachingDefinition(sym, def);
        return def;
    }
    else
    {
        auto [inst, join] = emitJoin(block, sym);
        block->setReachingDefinition(sym, join->definition);
        auto d = fillJoinOperands(block, inst, join);
        block->setReachingDefinition(d->getSym(), d);
        return d;
    }
}

SSAId CFGBuilder::fillJoinOperands(Block* block, InstructionId instr, Join* j)
{
    for (BlockId pred : block->getPredecessors())
    {
        auto def = readVariable(pred, j->definition->getSym());
        j->operands.emplace_back(def);
    }

    return trimTrivialJoin(instr, j);
}


SSAId CFGBuilder::trimTrivialJoin(InstructionId inst, Join* j)
{
    LUAU_ASSERT(j);
    SSAId curr = j->definition;

    // Collect unique operands, excluding self-references
    InsertionOrderedMap<SSAId, bool> unique;
    for (auto& op : j->operands)
    {
        if (op != curr)
            unique.insert(op, true);
    }

    // All operands are self-references (unreachable back-edge only)
    if (unique.size() == 0)
        return curr;

    // Exactly one distinct non-self operand — join is trivial, redirect to it
    if (unique.size() == 1)
    {
        Definition* same = nullptr;
        for (auto& op : unique)
            same = op.first;

        inst->emplace<Dead>();
        curr->redirect(same);
        return NotNull{same};
    }

    // Multiple distinct operands — keep the join with deduplicated operands
    std::vector<SSAId> newOps;
    for (auto& op : unique)
        newOps.emplace_back(op.first);
    j->operands = std::move(newOps);
    return curr;
}

} // namespace Luau
