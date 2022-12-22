// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/DataFlowGraph.h"

#include "Luau/Error.h"

LUAU_FASTFLAG(DebugLuauFreezeArena)
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

namespace Luau
{

std::optional<DefId> DataFlowGraph::getDef(const AstExpr* expr) const
{
    // We need to skip through AstExprGroup because DFG doesn't try its best to transitively
    while (auto group = expr->as<AstExprGroup>())
        expr = group->expr;
    if (auto def = astDefs.find(expr))
        return NotNull{*def};
    return std::nullopt;
}

std::optional<DefId> DataFlowGraph::getDef(const AstLocal* local) const
{
    if (auto def = localDefs.find(local))
        return NotNull{*def};
    return std::nullopt;
}

std::optional<DefId> DataFlowGraph::getDef(const Symbol& symbol) const
{
    if (symbol.local)
        return getDef(symbol.local);
    else
        return std::nullopt;
}

DataFlowGraph DataFlowGraphBuilder::build(AstStatBlock* block, NotNull<InternalErrorReporter> handle)
{
    LUAU_ASSERT(FFlag::DebugLuauDeferredConstraintResolution);

    DataFlowGraphBuilder builder;
    builder.handle = handle;
    builder.visit(nullptr, block); // nullptr is the root DFG scope.
    if (FFlag::DebugLuauFreezeArena)
        builder.arena->allocator.freeze();
    return std::move(builder.graph);
}

DfgScope* DataFlowGraphBuilder::childScope(DfgScope* scope)
{
    return scopes.emplace_back(new DfgScope{scope}).get();
}

std::optional<DefId> DataFlowGraphBuilder::use(DfgScope* scope, Symbol symbol, AstExpr* e)
{
    for (DfgScope* current = scope; current; current = current->parent)
    {
        if (auto def = current->bindings.find(symbol))
        {
            graph.astDefs[e] = *def;
            return NotNull{*def};
        }
    }

    return std::nullopt;
}

DefId DataFlowGraphBuilder::use(DefId def, AstExprIndexName* e)
{
    auto& propertyDef = props[def][e->index.value];
    if (!propertyDef)
        propertyDef = arena->freshCell(def, e->index.value);
    graph.astDefs[e] = propertyDef;
    return NotNull{propertyDef};
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatBlock* b)
{
    DfgScope* child = childScope(scope);
    return visitBlockWithoutChildScope(child, b);
}

void DataFlowGraphBuilder::visitBlockWithoutChildScope(DfgScope* scope, AstStatBlock* b)
{
    for (AstStat* s : b->body)
        visit(scope, s);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStat* s)
{
    if (auto b = s->as<AstStatBlock>())
        return visit(scope, b);
    else if (auto i = s->as<AstStatIf>())
        return visit(scope, i);
    else if (auto w = s->as<AstStatWhile>())
        return visit(scope, w);
    else if (auto r = s->as<AstStatRepeat>())
        return visit(scope, r);
    else if (auto b = s->as<AstStatBreak>())
        return visit(scope, b);
    else if (auto c = s->as<AstStatContinue>())
        return visit(scope, c);
    else if (auto r = s->as<AstStatReturn>())
        return visit(scope, r);
    else if (auto e = s->as<AstStatExpr>())
        return visit(scope, e);
    else if (auto l = s->as<AstStatLocal>())
        return visit(scope, l);
    else if (auto f = s->as<AstStatFor>())
        return visit(scope, f);
    else if (auto f = s->as<AstStatForIn>())
        return visit(scope, f);
    else if (auto a = s->as<AstStatAssign>())
        return visit(scope, a);
    else if (auto c = s->as<AstStatCompoundAssign>())
        return visit(scope, c);
    else if (auto f = s->as<AstStatFunction>())
        return visit(scope, f);
    else if (auto l = s->as<AstStatLocalFunction>())
        return visit(scope, l);
    else if (auto t = s->as<AstStatTypeAlias>())
        return; // ok
    else if (auto d = s->as<AstStatDeclareFunction>())
        return; // ok
    else if (auto d = s->as<AstStatDeclareGlobal>())
        return; // ok
    else if (auto d = s->as<AstStatDeclareFunction>())
        return; // ok
    else if (auto d = s->as<AstStatDeclareClass>())
        return; // ok
    else if (auto _ = s->as<AstStatError>())
        return; // ok
    else
        handle->ice("Unknown AstStat in DataFlowGraphBuilder");
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatIf* i)
{
    DfgScope* condScope = childScope(scope);
    visitExpr(condScope, i->condition);
    visit(condScope, i->thenbody);

    if (i->elsebody)
        visit(scope, i->elsebody);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatWhile* w)
{
    // TODO(controlflow): entry point has a back edge from exit point
    DfgScope* whileScope = childScope(scope);
    visitExpr(whileScope, w->condition);
    visit(whileScope, w->body);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatRepeat* r)
{
    // TODO(controlflow): entry point has a back edge from exit point
    DfgScope* repeatScope = childScope(scope); // TODO: loop scope.
    visitBlockWithoutChildScope(repeatScope, r->body);
    visitExpr(repeatScope, r->condition);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatBreak* b)
{
    // TODO: Control flow analysis
    return; // ok
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatContinue* c)
{
    // TODO: Control flow analysis
    return; // ok
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatReturn* r)
{
    // TODO: Control flow analysis
    for (AstExpr* e : r->list)
        visitExpr(scope, e);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatExpr* e)
{
    visitExpr(scope, e->expr);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatLocal* l)
{
    // TODO: alias tracking
    for (AstExpr* e : l->values)
        visitExpr(scope, e);

    for (AstLocal* local : l->vars)
    {
        DefId def = arena->freshCell();
        graph.localDefs[local] = def;
        scope->bindings[local] = def;
    }
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatFor* f)
{
    DfgScope* forScope = childScope(scope); // TODO: loop scope.
    DefId def = arena->freshCell();
    graph.localDefs[f->var] = def;
    scope->bindings[f->var] = def;

    // TODO(controlflow): entry point has a back edge from exit point
    visit(forScope, f->body);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatForIn* f)
{
    DfgScope* forScope = childScope(scope); // TODO: loop scope.

    for (AstLocal* local : f->vars)
    {
        DefId def = arena->freshCell();
        graph.localDefs[local] = def;
        forScope->bindings[local] = def;
    }

    // TODO(controlflow): entry point has a back edge from exit point
    for (AstExpr* e : f->values)
        visitExpr(forScope, e);

    visit(forScope, f->body);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatAssign* a)
{
    for (AstExpr* r : a->values)
        visitExpr(scope, r);

    for (AstExpr* l : a->vars)
    {
        AstExpr* root = l;

        bool isUpdatable = true;
        while (true)
        {
            if (root->is<AstExprLocal>() || root->is<AstExprGlobal>())
                break;

            AstExprIndexName* indexName = root->as<AstExprIndexName>();
            if (!indexName)
            {
                isUpdatable = false;
                break;
            }

            root = indexName->expr;
        }

        if (isUpdatable)
        {
            // TODO global?
            if (auto exprLocal = root->as<AstExprLocal>())
            {
                DefId def = arena->freshCell();
                graph.astDefs[exprLocal] = def;

                // Update the def in the scope that introduced the local.  Not
                // the current scope.
                AstLocal* local = exprLocal->local;
                DfgScope* s = scope;
                while (s && !s->bindings.find(local))
                    s = s->parent;
                LUAU_ASSERT(s && s->bindings.find(local));
                s->bindings[local] = def;
            }
        }

        visitExpr(scope, l); // TODO: they point to a new def!!
    }
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatCompoundAssign* c)
{
    // TODO(typestates): The lhs is being read and written to. This might or might not be annoying.
    visitExpr(scope, c->value);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatFunction* f)
{
    visitExpr(scope, f->name);
    visitExpr(scope, f->func);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatLocalFunction* l)
{
    DefId def = arena->freshCell();
    graph.localDefs[l->name] = def;
    scope->bindings[l->name] = def;

    visitExpr(scope, l->func);
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExpr* e)
{
    if (auto g = e->as<AstExprGroup>())
        return visitExpr(scope, g->expr);
    else if (auto c = e->as<AstExprConstantNil>())
        return {}; // ok
    else if (auto c = e->as<AstExprConstantBool>())
        return {}; // ok
    else if (auto c = e->as<AstExprConstantNumber>())
        return {}; // ok
    else if (auto c = e->as<AstExprConstantString>())
        return {}; // ok
    else if (auto l = e->as<AstExprLocal>())
        return visitExpr(scope, l);
    else if (auto g = e->as<AstExprGlobal>())
        return visitExpr(scope, g);
    else if (auto v = e->as<AstExprVarargs>())
        return {}; // ok
    else if (auto c = e->as<AstExprCall>())
        return visitExpr(scope, c);
    else if (auto i = e->as<AstExprIndexName>())
        return visitExpr(scope, i);
    else if (auto i = e->as<AstExprIndexExpr>())
        return visitExpr(scope, i);
    else if (auto f = e->as<AstExprFunction>())
        return visitExpr(scope, f);
    else if (auto t = e->as<AstExprTable>())
        return visitExpr(scope, t);
    else if (auto u = e->as<AstExprUnary>())
        return visitExpr(scope, u);
    else if (auto b = e->as<AstExprBinary>())
        return visitExpr(scope, b);
    else if (auto t = e->as<AstExprTypeAssertion>())
        return visitExpr(scope, t);
    else if (auto i = e->as<AstExprIfElse>())
        return visitExpr(scope, i);
    else if (auto i = e->as<AstExprInterpString>())
        return visitExpr(scope, i);
    else if (auto _ = e->as<AstExprError>())
        return {}; // ok
    else
        handle->ice("Unknown AstExpr in DataFlowGraphBuilder");
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprLocal* l)
{
    return {use(scope, l->local, l)};
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprGlobal* g)
{
    return {use(scope, g->name, g)};
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprCall* c)
{
    visitExpr(scope, c->func);

    for (AstExpr* arg : c->args)
        visitExpr(scope, arg);

    return {};
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprIndexName* i)
{
    std::optional<DefId> def = visitExpr(scope, i->expr).def;
    if (!def)
        return {};

    return {use(*def, i)};
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprIndexExpr* i)
{
    visitExpr(scope, i->expr);
    visitExpr(scope, i->expr);

    if (i->index->as<AstExprConstantString>())
    {
        // TODO: properties for the def
    }

    return {};
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprFunction* f)
{
    if (AstLocal* self = f->self)
    {
        DefId def = arena->freshCell();
        graph.localDefs[self] = def;
        scope->bindings[self] = def;
    }

    for (AstLocal* param : f->args)
    {
        DefId def = arena->freshCell();
        graph.localDefs[param] = def;
        scope->bindings[param] = def;
    }

    visit(scope, f->body);

    return {};
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprTable* t)
{
    return {};
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprUnary* u)
{
    visitExpr(scope, u->expr);

    return {};
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprBinary* b)
{
    visitExpr(scope, b->left);
    visitExpr(scope, b->right);

    return {};
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprTypeAssertion* t)
{
    ExpressionFlowGraph result = visitExpr(scope, t->expr);
    // TODO: visit type
    return result;
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprIfElse* i)
{
    DfgScope* condScope = childScope(scope);
    visitExpr(condScope, i->condition);
    visitExpr(condScope, i->trueExpr);

    visitExpr(scope, i->falseExpr);

    return {};
}

ExpressionFlowGraph DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprInterpString* i)
{
    for (AstExpr* e : i->expressions)
        visitExpr(scope, e);
    return {};
}

} // namespace Luau
