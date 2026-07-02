// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ControlFlowGraph.h"
#include "Luau/Ast.h"
#include "Luau/AstUtils.h"
#include "Luau/Common.h"

#include <algorithm>
#include <memory>
#include <optional>

LUAU_FASTFLAG(DebugLuauFreezeArena)

namespace Luau::CFG
{

namespace CFGRefinement
{

RefinementId RefinementArena::proposition(DefId def, bool sense)
{
    return NotNull{allocator.allocate(Proposition{def, std::nullopt, /*isTypeof*/ false, sense})};
}

RefinementId RefinementArena::typeProposition(DefId def, std::optional<std::string> type, bool isTypeof, bool sense)
{
    return NotNull{allocator.allocate(Proposition{def, std::move(type), isTypeof, sense})};
}

RefinementId RefinementArena::conjunction(RefinementId lhs, RefinementId rhs)
{
    return NotNull{allocator.allocate(Conjunction{lhs, rhs})};
}

RefinementId RefinementArena::disjunction(RefinementId lhs, RefinementId rhs)
{
    return NotNull{allocator.allocate(Disjunction{lhs, rhs})};
}

RefinementId RefinementArena::negation(RefinementId r)
{
    if (auto* conj = get<Conjunction>(r))
        return disjunction(negation(conj->lhs), negation(conj->rhs));
    if (auto* disj = get<Disjunction>(r))
        return conjunction(negation(disj->lhs), negation(disj->rhs));
    if (auto* neg = get<Negation>(r))
        return neg->refinement;

    LUAU_ASSERT(get<Proposition>(r));
    return NotNull{allocator.allocate(Negation{r})};
}

void RefinementArena::freeze()
{
    allocator.freeze();
}

} // namespace CFGRefinement

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

void Block::setReachingDefinition(Symbol sym, DefId def)
{
    reachingDefinitions[sym] = def;
}

const std::vector<InstrId>& Block::getInstructions() const
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

DefId CFGAllocator::newDefinition(Symbol sym, size_t version)
{
    return NotNull{defs.allocate(SymDef{sym, version})};
}

void CFGAllocator::freeze()
{
    block.freeze();
    defs.freeze();
    refinementArena.freeze();
    frozen = true;
}

BlockId ControlFlowGraph::newBlock(BlockKind kind, std::string debugName)
{
    Block* b = allocator->newBlock(kind, debugName);
    return blocks.emplace_back(b);
}

void ControlFlowGraph::computeRPO()
{
    std::vector<Block*> stack;
    DenseHashSet<Block*> visited{nullptr};

    stack.push_back(blocks[0]);
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

        rpoOrder.emplace_back(curr);
        stack.pop_back();
    }

    std::reverse(rpoOrder.begin(), rpoOrder.end());
}

Definition* ControlFlowGraph::resolve(Definition* def) const
{
    // It's possible that we could do path compression here as an easy optimization.
    while (auto* fwd = forwards.find(def))
        def = *fwd;
    return def;
}

Definition* ControlFlowGraph::getUseDef(AstExpr* expr) const
{
    if (auto* def = useDefs.find(expr))
        return resolve(*def);
    return nullptr;
}

Definition* ControlFlowGraph::getLhsDef(const LValue& lv) const
{
    if (auto* def = lhsDefs.find(lv))
        return resolve(*def);
    return nullptr;
}


CFGBuilder::CFGBuilder(NotNull<CFGAllocator> allocator)
    : cfg(std::make_unique<ControlFlowGraph>(allocator))
    , allocator(allocator)
    , currentBlock(cfg->newBlock(BlockKind::Entry, "Entry Block"))
{
    seal(currentBlock);
}

std::unique_ptr<ControlFlowGraph> CFGBuilder::makeCFG(NotNull<CFGAllocator> allocator, AstStatBlock* block)
{
    CFGBuilder builder(allocator);
    builder.lower(block);
    auto cfg = std::move(builder.cfg);
    cfg->computeRPO();
    if (FFlag::DebugLuauFreezeArena)
        allocator->freeze();
    return cfg;
}

bool CFGBuilder::isSealed(Block* b)
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
void CFGBuilder::seal(Block* b)
{
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
                DefId resolved = fillJoinOperands(b, NotNull{inst}, join);
                b->setReachingDefinition(resolved->sym, resolved);
            }
        }
        incompleteJoins[b] = DenseHashSet<Instruction*>{};
    }
    sealedBlocks.insert(b);
}

void CFGBuilder::lower(AstStat* statement)
{
    if (auto blk = statement->as<AstStatBlock>())
        lower(blk);
    else if (auto local = statement->as<AstStatLocal>())
        lower(local);
    else if (auto assn = statement->as<AstStatAssign>())
        lower(assn);
    else if (auto statIf = statement->as<AstStatIf>())
        lower(statIf);
    else if (auto statWhile = statement->as<AstStatWhile>())
        lower(statWhile);
    else if (auto expr = statement->as<AstStatExpr>())
        lower(expr);
    else
    {
        LUAU_ASSERT(!"Unhandled statement");
    }
}

void CFGBuilder::lower(AstStatExpr* stat)
{
    lowerExpr(stat->expr);
}

bool CFGBuilder::tryLowerAssertion(AstExprCall* call)
{
    if (call->args.size == 0)
        return false;

    auto global = call->func->as<AstExprGlobal>();
    if (!global || global->name != "assert")
        return false;

    AstExpr* cond = call->args.data[0];
    for (size_t i = 0; i < call->args.size; i++)
    {
        lowerExpr(call->args.data[i]);
        if (i == 0)
        {
            if (auto ref = resolveCondition(cond))
                emitRefineInstruction(currentBlock, *ref);
        }
    }

    return true;
}

void CFGBuilder::lower(AstStatBlock* statement)
{
    for (auto st : statement->body)
        lower(st);
}

void CFGBuilder::lower(AstStatLocal* local)
{
    for (size_t i = 0; i < local->vars.size; i++)
    {
        AstLocal* loc = local->vars.data[i];
        AstExpr* expr = i < local->values.size ? local->values.data[i] : nullptr;

        if (expr)
            lowerExpr(expr);

        Symbol sym(loc);
        DefId def = newDefinition(sym);
        emit<Declare>(currentBlock, def, local);
        currentBlock->setReachingDefinition(sym, def);
        cfg->lhsDefs[LValue{sym}] = def.get();
    }
}

std::optional<Symbol> extractLValueSymbol(AstExpr* target)
{
    if (auto local = target->as<AstExprLocal>())
        return Symbol(local->local);
    else if (auto global = target->as<AstExprGlobal>())
        return Symbol(global->name);

    return std::nullopt;
}

void CFGBuilder::lower(AstStatAssign* assn)
{
    for (size_t i = 0; i < assn->values.size; i++)
        lowerExpr(assn->values.data[i]);

    for (size_t i = 0; i < assn->vars.size; i++)
    {
        AstExpr* target = assn->vars.data[i];

        if (auto sym = extractLValueSymbol(target))
        {
            DefId def = newDefinition(*sym);
            emit<Assign>(currentBlock, def, assn);
            currentBlock->setReachingDefinition(*sym, def);
            cfg->lhsDefs[LValue{target}] = def.get();
        }
        else
        {
            LUAU_ASSERT(!"Unhandled lvalue type");
        }
    }
}

Block* CFGBuilder::newBlock(BlockKind kind, std::string debugName, Block* pred)
{
    Block* b = cfg->newBlock(kind, debugName);
    if (pred)
        pred->addSuccessor(b);
    return b;
}

DefId CFGBuilder::newDefinition(Symbol sym)
{
    return allocator->newDefinition(sym, nextVersionIndex(sym));
}

std::pair<InstrId, NotNull<Join>> CFGBuilder::emitJoin(Block* block, Symbol sym)
{
    DefId def = newDefinition(sym);
    InstrId jInstr = emit<Join>(block, def);
    Join* j = jInstr->get_if<Join>();
    LUAU_ASSERT(j);

    block->setReachingDefinition(sym, def);
    incompleteJoins[block].insert(jInstr);
    return {jInstr, NotNull{j}};
}

void CFGBuilder::lower(AstStatIf* statIf)
{
    Block* currBlock = currentBlock.get();

    Block* thenBlock = newBlock(BlockKind::Linear, "then branch", currBlock);
    auto ref = resolveCondition(statIf->condition);
    if (ref)
        emitRefineInstruction(thenBlock, *ref);

    // Then only has one predecessor
    seal(thenBlock);
    Block* thenExit;
    {
        BlockScope scope(*this, thenBlock);
        lower(statIf->thenbody);
        thenExit = currentBlock.get();
    }

    // Else branch (may be nullptr, another AstStatIf for elseif, or a block)
    Block* elseBlock = newBlock(BlockKind::Linear, "else branch", currBlock);
    Block* elseExit = elseBlock; // If there is an else body, overwrite this
    seal(elseBlock);
    if (ref)
        emitRefineInstruction(elseBlock, allocator->refinementArena.negation(*ref));


    if (statIf->elsebody)
    {
        BlockScope scope(*this, elseBlock);
        lower(statIf->elsebody);
        elseExit = currentBlock.get();
    }

    // Merge block — all paths converge here
    // Predecessors are wired up below once we know which branches reach here.
    Block* mergeBlock = newBlock(BlockKind::Linear, "merge");
    thenExit->addSuccessor(mergeBlock);
    elseExit->addSuccessor(mergeBlock);
    seal(mergeBlock);
    currentBlock = NotNull{mergeBlock};
}


void CFGBuilder::lower(AstStatWhile* statWhile)
{
    Block* preLoop = currentBlock.get();

    // Loop header — receives the back-edge so we don't seal it yet. Resolve the
    // condition inside the header's scope so reads of variables mutated in the
    // body hit this unsealed block, emit an incomplete Join, and get their
    // operands filled in when the header is sealed after the back-edge.
    Block* loopHeader = newBlock(BlockKind::Condition, "while-loop condition", preLoop);
    std::optional<CFGRefinement::RefinementId> ref;
    {
        BlockScope scope(*this, loopHeader);
        ref = resolveCondition(statWhile->condition);
    }

    Block* bodyBlock = newBlock(BlockKind::Linear, "while-loop body", loopHeader);
    if (ref)
        emitRefineInstruction(bodyBlock, *ref);
    seal(bodyBlock);
    Block* bodyExit;
    {
        BlockScope scope(*this, bodyBlock);
        lower(statWhile->body);
        bodyExit = currentBlock.get();
    }

    // You can seal the loop header now because no predecessors will be added to it.
    bodyExit->addSuccessor(loopHeader);
    seal(loopHeader);

    Block* exitBlock = newBlock(BlockKind::Linear, "while-loop exit", loopHeader);
    if (ref)
        emitRefineInstruction(exitBlock, allocator->refinementArena.negation(*ref));
    seal(exitBlock);
    currentBlock = NotNull{exitBlock};
}

void CFGBuilder::lowerExpr(AstExpr* expr)
{
    if (auto local = expr->as<AstExprLocal>())
    {
        lowerExpr(local);
    }
    else if (auto binop = expr->as<AstExprBinary>())
    {
        lowerExpr(binop->left);
        lowerExpr(binop->right);
    }
    else if (auto call = expr->as<AstExprCall>())
    {
        lowerExpr(call);
    }
    else if (auto group = expr->as<AstExprGroup>())
    {
        lowerExpr(group->expr);
    }
}

void CFGBuilder::lowerExpr(AstExprLocal* local)
{
    DefId def = readVariable(currentBlock, Symbol(local->local));
    cfg->useDefs[local] = def;
}

void CFGBuilder::lowerExpr(AstExprCall* call)
{
    if (tryLowerAssertion(call))
        return;
    lowerExpr(call->func);
    for (size_t i = 0; i < call->args.size; i++)
        lowerExpr(call->args.data[i]);
}

std::optional<CFGRefinement::RefinementId> CFGBuilder::resolveCondition(AstExpr* condition)
{
    auto& arena = allocator->refinementArena;

    if (auto group = condition->as<AstExprGroup>())
    {
        return resolveCondition(group->expr);
    }
    else if (auto loc = condition->as<AstExprLocal>())
    {
        DefId def = readVariable(currentBlock, Symbol(loc->local));
        cfg->useDefs[loc] = def;
        return arena.proposition(def, /* sense */ true);
    }
    else if (auto binop = condition->as<AstExprBinary>())
    {
        if (auto tg = matchTypeGuard(binop->op, binop->left, binop->right))
        {
            if (auto tgtLocal = tg->target->as<AstExprLocal>())
            {
                auto def = readVariable(currentBlock, Symbol(tgtLocal->local));
                cfg->useDefs[tgtLocal] = def;
                bool sense = binop->op == AstExprBinary::CompareEq;
                return arena.typeProposition(def, tg->type, tg->isTypeof, sense);
            }
            return std::nullopt;
        }

        auto lRef = resolveCondition(binop->left);
        auto rRef = resolveCondition(binop->right);
        if (binop->op == AstExprBinary::And)
        {
            // (A and B) truthy => both truthy; a missing side still preserves the other.
            if (lRef && rRef)
                return arena.conjunction(*lRef, *rRef);
            return lRef ? lRef : rRef;
        }
        else if (binop->op == AstExprBinary::Or)
        {
            // (A or B) truthy => at least one truthy; an unrefined side means we can't narrow.
            if (lRef && rRef)
                return arena.disjunction(*lRef, *rRef);
        }
    }
    else if (auto unop = condition->as<AstExprUnary>())
    {
        if (unop->op == AstExprUnary::Op::Not)
        {
            if (auto inner = resolveCondition(unop->expr))
                return arena.negation(*inner);
        }
    }

    return std::nullopt;
}

void CFGBuilder::emitRefineInstruction(Block* block, CFGRefinement::RefinementId refinement)
{
    // This function walks the refinement tree and, at every
    // Proposition, emits a refinement into the block with a fresh definition.
    // This differs only slightly from SSI form, which introduces a virtual `sigma`
    // definition as a terminator, which then gets written in subsequent blocks.
    // I've chosen to elide this terminator in favor of just emitting the fresh def + refinement
    // explicitly into the block. A consequence of this is that this representation will mint a
    // empty block with only refinement information, but this just makes it easier to handle phi emission.
    visit(
        overloaded{
            [&](const CFGRefinement::Proposition& prop)
            {
                DefId refined = newDefinition(prop.ptr->sym);
                emit<Refine>(block, refined, prop);
                block->setReachingDefinition(prop.ptr->sym, refined);
            },
            [&](const CFGRefinement::Conjunction& conj)
            {
                emitRefineInstruction(block, conj.lhs);
                emitRefineInstruction(block, conj.rhs);
            },
            [&](const CFGRefinement::Negation& neg)
            {
                // RefinementArena::negation pushes through And/Or via DeMorgan and cancels
                // double negation, so the only shape that reaches here is Negation(Proposition).
                auto prop = neg.refinement->get_if<CFGRefinement::Proposition>();
                LUAU_ASSERT(prop != nullptr);
                emitRefineInstruction(block, allocator->refinementArena.typeProposition(prop->ptr, prop->type, prop->isTypeof, !prop->sense));
            },
            [&](const CFGRefinement::Disjunction&)
            {
                // CLI-205330 tracks the work needed to handle Disjunctions (e.g. we need to calculate sets of propositions on individual defs
                // For the most part, this would just re-implement the existing refinement calculation logic in constraint generation.
            },
        },
        *refinement
    );
}

DefId CFGBuilder::readVariable(BlockId block, Symbol sym)
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
        block->setReachingDefinition(d->sym, d);
        return d;
    }
}

DefId CFGBuilder::fillJoinOperands(Block* block, InstrId instr, Join* j)
{
    for (BlockId pred : block->getPredecessors())
    {
        auto def = readVariable(pred, j->definition->sym);
        j->operands.emplace_back(def);
    }

    recordUses(instr);
    return trimTrivialJoin(instr, j);
}


DefId CFGBuilder::trimTrivialJoin(InstrId inst, Join* j)
{
    LUAU_ASSERT(j);
    auto curr = j->definition;
    // Tracks the duplicated or unreachable phi nodes
    Definition* same = nullptr;
    // Phis have two operands (by construction)
    for (auto& op : j->operands)
    {
        if (op == same || op == curr)
            continue;
        if (same != nullptr)
            return curr;
        same = op;
    }

    Set<Instruction*> tmp_;
    Set<Instruction*>& usingInsts = tmp_;
    if (Set<Instruction*>* uses = usingInstructions.find(curr))
        usingInsts = *uses;
    usingInsts.erase(inst);

    if (same == nullptr)
        return curr;

    inst->emplace<Dead>();
    // The current join is trivial, so we can replace it with the single op that isn't itself
    cfg->forwards[curr.get()] = same;

    for (auto& usingInst : usingInsts)
    {
        if (Join* j = usingInst->get_if<Join>())
            trimTrivialJoin(NotNull{usingInst}, j);
    }

    return NotNull{same};
}

void CFGBuilder::recordUses(InstrId inst)
{
    visit(
        overloaded{
            [&](const Declare&) {},
            [&](const Assign&) {},
            [&](const Dead&) {},
            [&](const Join& join)
            {
                for (auto& op : join.operands)
                    usingInstructions[op].insert(inst);
            },
            [&](const Refine& refine)
            {
                usingInstructions[refine.toRefine.get()].insert(inst);
            },
        },
        *inst.get()
    );
}

size_t CFGBuilder::nextVersionIndex(Symbol sym)
{
    if (!versionCounter.contains(sym))
    {
        versionCounter[sym] = 0;
        return 0;
    }

    auto ref = versionCounter.find(sym);
    *ref += 1;
    return *ref;
}

} // namespace Luau::CFG
