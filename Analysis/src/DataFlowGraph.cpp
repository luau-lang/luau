// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/DataFlowGraph.h"

#include "Luau/Ast.h"
#include "Luau/Def.h"
#include "Luau/Common.h"
#include "Luau/Error.h"

#include <algorithm>
#include <optional>

LUAU_FASTFLAG(DebugLuauFreezeArena)
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

namespace Luau
{

const RefinementKey* RefinementKeyArena::leaf(DefId def)
{
    return allocator.allocate(RefinementKey{nullptr, def, std::nullopt});
}

const RefinementKey* RefinementKeyArena::node(const RefinementKey* parent, DefId def, const std::string& propName)
{
    return allocator.allocate(RefinementKey{parent, def, propName});
}

DefId DataFlowGraph::getDef(const AstExpr* expr) const
{
    auto def = astDefs.find(expr);
    LUAU_ASSERT(def);
    return NotNull{*def};
}

std::optional<DefId> DataFlowGraph::getRValueDefForCompoundAssign(const AstExpr* expr) const
{
    auto def = compoundAssignBreadcrumbs.find(expr);
    return def ? std::optional<DefId>(*def) : std::nullopt;
}

DefId DataFlowGraph::getDef(const AstLocal* local) const
{
    auto def = localDefs.find(local);
    LUAU_ASSERT(def);
    return NotNull{*def};
}

DefId DataFlowGraph::getDef(const AstStatDeclareGlobal* global) const
{
    auto def = declaredDefs.find(global);
    LUAU_ASSERT(def);
    return NotNull{*def};
}

DefId DataFlowGraph::getDef(const AstStatDeclareFunction* func) const
{
    auto def = declaredDefs.find(func);
    LUAU_ASSERT(def);
    return NotNull{*def};
}

const RefinementKey* DataFlowGraph::getRefinementKey(const AstExpr* expr) const
{
    if (auto key = astRefinementKeys.find(expr))
        return *key;

    return nullptr;
}

std::optional<DefId> DfgScope::lookup(Symbol symbol) const
{
    for (const DfgScope* current = this; current; current = current->parent)
    {
        if (auto def = current->bindings.find(symbol))
            return NotNull{*def};
    }

    return std::nullopt;
}

std::optional<DefId> DfgScope::lookup(DefId def, const std::string& key) const
{
    for (const DfgScope* current = this; current; current = current->parent)
    {
        if (auto map = props.find(def))
        {
            if (auto it = map->find(key); it != map->end())
                return NotNull{it->second};
        }
    }

    return std::nullopt;
}

DataFlowGraph DataFlowGraphBuilder::build(AstStatBlock* block, NotNull<InternalErrorReporter> handle)
{
    LUAU_ASSERT(FFlag::DebugLuauDeferredConstraintResolution);

    DataFlowGraphBuilder builder;
    builder.handle = handle;
    builder.moduleScope = builder.childScope(nullptr); // nullptr is the root DFG scope.
    builder.visitBlockWithoutChildScope(builder.moduleScope, block);

    if (FFlag::DebugLuauFreezeArena)
    {
        builder.defArena->allocator.freeze();
        builder.keyArena->allocator.freeze();
    }

    return std::move(builder.graph);
}

DfgScope* DataFlowGraphBuilder::childScope(DfgScope* scope)
{
    return scopes.emplace_back(new DfgScope{scope}).get();
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
        return visit(scope, t);
    else if (auto d = s->as<AstStatDeclareGlobal>())
        return visit(scope, d);
    else if (auto d = s->as<AstStatDeclareFunction>())
        return visit(scope, d);
    else if (auto d = s->as<AstStatDeclareClass>())
        return visit(scope, d);
    else if (auto error = s->as<AstStatError>())
        return visit(scope, error);
    else
        handle->ice("Unknown AstStat in DataFlowGraphBuilder::visit");
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatIf* i)
{
    // TODO: type states and control flow analysis
    visitExpr(scope, i->condition);
    visit(scope, i->thenbody);
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
    // We're gonna need a `visitExprList` and `visitVariadicExpr` (function calls and `...`)
    std::vector<DefId> defs;
    defs.reserve(l->values.size);
    for (AstExpr* e : l->values)
        defs.push_back(visitExpr(scope, e).def);

    for (size_t i = 0; i < l->vars.size; ++i)
    {
        AstLocal* local = l->vars.data[i];
        if (local->annotation)
            visitType(scope, local->annotation);

        // We need to create a new def to intentionally avoid alias tracking, but we'd like to
        // make sure that the non-aliased defs are also marked as a subscript for refinements.
        bool subscripted = i < defs.size() && containsSubscriptedDefinition(defs[i]);
        DefId def = defArena->freshCell(subscripted);
        graph.localDefs[local] = def;
        scope->bindings[local] = def;
    }
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatFor* f)
{
    DfgScope* forScope = childScope(scope); // TODO: loop scope.

    visitExpr(scope, f->from);
    visitExpr(scope, f->to);
    if (f->step)
        visitExpr(scope, f->step);

    if (f->var->annotation)
        visitType(forScope, f->var->annotation);

    DefId def = defArena->freshCell();
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
        if (local->annotation)
            visitType(forScope, local->annotation);

        DefId def = defArena->freshCell();
        graph.localDefs[local] = def;
        forScope->bindings[local] = def;
    }

    // TODO(controlflow): entry point has a back edge from exit point
    // We're gonna need a `visitExprList` and `visitVariadicExpr` (function calls and `...`)
    for (AstExpr* e : f->values)
        visitExpr(forScope, e);

    visit(forScope, f->body);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatAssign* a)
{
    std::vector<DefId> defs;
    defs.reserve(a->values.size);
    for (AstExpr* e : a->values)
        defs.push_back(visitExpr(scope, e).def);

    for (size_t i = 0; i < a->vars.size; ++i)
    {
        AstExpr* v = a->vars.data[i];
        visitLValue(scope, v, i < defs.size() ? defs[i] : defArena->freshCell());
    }
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatCompoundAssign* c)
{
    // TODO: This needs revisiting because this is incorrect. The `c->var` part is both being read and written to,
    // but the `c->var` only has one pointer address, so we need to come up with a way to store both.
    // For now, it's not important because we don't have type states, but it is going to be important, e.g.
    //
    // local a = 5 -- a-1
    // a += 5      -- a-2 = a-1 + 5
    // We can't just visit `c->var` as a rvalue and then separately traverse `c->var` as an lvalue, since that's O(n^2).
    DefId def = visitExpr(scope, c->value).def;
    visitLValue(scope, c->var, def, /* isCompoundAssignment */ true);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatFunction* f)
{
    // In the old solver, we assumed that the name of the function is always a function in the body
    // but this isn't true, e.g. the following example will print `5`, not a function address.
    //
    // local function f() print(f) end
    // local g = f
    // f = 5
    // g() --> 5
    //
    // which is evidence that references to variables must be a phi node of all possible definitions,
    // but for bug compatibility, we'll assume the same thing here.
    DefId prototype = defArena->freshCell();
    visitLValue(scope, f->name, prototype);
    visitExpr(scope, f->func);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatLocalFunction* l)
{
    DefId def = defArena->freshCell();
    graph.localDefs[l->name] = def;
    scope->bindings[l->name] = def;
    visitExpr(scope, l->func);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatTypeAlias* t)
{
    DfgScope* unreachable = childScope(scope);
    visitGenerics(unreachable, t->generics);
    visitGenericPacks(unreachable, t->genericPacks);
    visitType(unreachable, t->type);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatDeclareGlobal* d)
{
    DefId def = defArena->freshCell();
    graph.declaredDefs[d] = def;
    scope->bindings[d->name] = def;

    visitType(scope, d->type);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatDeclareFunction* d)
{
    DefId def = defArena->freshCell();
    graph.declaredDefs[d] = def;
    scope->bindings[d->name] = def;

    DfgScope* unreachable = childScope(scope);
    visitGenerics(unreachable, d->generics);
    visitGenericPacks(unreachable, d->genericPacks);
    visitTypeList(unreachable, d->params);
    visitTypeList(unreachable, d->retTypes);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatDeclareClass* d)
{
    // This declaration does not "introduce" any bindings in value namespace,
    // so there's no symbolic value to begin with. We'll traverse the properties
    // because their type annotations may depend on something in the value namespace.
    DfgScope* unreachable = childScope(scope);
    for (AstDeclaredClassProp prop : d->props)
        visitType(unreachable, prop.ty);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatError* error)
{
    DfgScope* unreachable = childScope(scope);
    for (AstStat* s : error->statements)
        visit(unreachable, s);
    for (AstExpr* e : error->expressions)
        visitExpr(unreachable, e);
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExpr* e)
{
    auto go = [&]() -> DataFlowResult {
        if (auto g = e->as<AstExprGroup>())
            return visitExpr(scope, g);
        else if (auto c = e->as<AstExprConstantNil>())
            return {defArena->freshCell(), nullptr}; // ok
        else if (auto c = e->as<AstExprConstantBool>())
            return {defArena->freshCell(), nullptr}; // ok
        else if (auto c = e->as<AstExprConstantNumber>())
            return {defArena->freshCell(), nullptr}; // ok
        else if (auto c = e->as<AstExprConstantString>())
            return {defArena->freshCell(), nullptr}; // ok
        else if (auto l = e->as<AstExprLocal>())
            return visitExpr(scope, l);
        else if (auto g = e->as<AstExprGlobal>())
            return visitExpr(scope, g);
        else if (auto v = e->as<AstExprVarargs>())
            return {defArena->freshCell(), nullptr}; // ok
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
        else if (auto error = e->as<AstExprError>())
            return visitExpr(scope, error);
        else
            handle->ice("Unknown AstExpr in DataFlowGraphBuilder::visitExpr");
    };

    auto [def, key] = go();
    graph.astDefs[e] = def;
    if (key)
        graph.astRefinementKeys[e] = key;
    return {def, key};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprGroup* group)
{
    return visitExpr(scope, group->expr);
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprLocal* l)
{
    if (auto def = scope->lookup(l->local))
    {
        const RefinementKey* key = keyArena->leaf(*def);
        return {*def, key};
    }

    handle->ice("DFG: AstExprLocal came before its declaration?");
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprGlobal* g)
{
    if (auto def = scope->lookup(g->name))
        return {*def, keyArena->leaf(*def)};

    DefId def = defArena->freshCell();
    moduleScope->bindings[g->name] = def;
    return {def, keyArena->leaf(def)};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprCall* c)
{
    visitExpr(scope, c->func);

    for (AstExpr* arg : c->args)
        visitExpr(scope, arg);

    return {defArena->freshCell(), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprIndexName* i)
{
    auto [parentDef, parentKey] = visitExpr(scope, i->expr);

    std::string index = i->index.value;
    auto& propDef = moduleScope->props[parentDef][index];
    if (!propDef)
        propDef = defArena->freshCell();

    return {NotNull{propDef}, keyArena->node(parentKey, NotNull{propDef}, index)};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprIndexExpr* i)
{
    auto [parentDef, parentKey] = visitExpr(scope, i->expr);
    visitExpr(scope, i->index);

    if (auto string = i->index->as<AstExprConstantString>())
    {
        std::string index{string->value.data, string->value.size};
        auto& propDef = moduleScope->props[parentDef][index];
        if (!propDef)
            propDef = defArena->freshCell();

        return {NotNull{propDef}, keyArena->node(parentKey, NotNull{propDef}, index)};
    }

    return {defArena->freshCell(/* subscripted= */true), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprFunction* f)
{
    DfgScope* signatureScope = childScope(scope);

    if (AstLocal* self = f->self)
    {
        // There's no syntax for `self` to have an annotation if using `function t:m()`
        LUAU_ASSERT(!self->annotation);

        DefId def = defArena->freshCell();
        graph.localDefs[self] = def;
        signatureScope->bindings[self] = def;
    }

    for (AstLocal* param : f->args)
    {
        if (param->annotation)
            visitType(signatureScope, param->annotation);

        DefId def = defArena->freshCell();
        graph.localDefs[param] = def;
        signatureScope->bindings[param] = def;
    }

    if (f->varargAnnotation)
        visitTypePack(scope, f->varargAnnotation);

    if (f->returnAnnotation)
        visitTypeList(signatureScope, *f->returnAnnotation);

    // TODO: function body can be re-entrant, as in mutations that occurs at the end of the function can also be
    // visible to the beginning of the function, so statically speaking, the body of the function has an exit point
    // that points back to itself, e.g.
    //
    // local function f() print(f) f = 5 end
    // local g = f
    // g() --> function: address
    // g() --> 5
    visit(signatureScope, f->body);

    return {defArena->freshCell(), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprTable* t)
{
    for (AstExprTable::Item item : t->items)
    {
        if (item.key)
            visitExpr(scope, item.key);
        visitExpr(scope, item.value);
    }

    return {defArena->freshCell(), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprUnary* u)
{
    visitExpr(scope, u->expr);

    return {defArena->freshCell(), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprBinary* b)
{
    visitExpr(scope, b->left);
    visitExpr(scope, b->right);

    return {defArena->freshCell(), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprTypeAssertion* t)
{
    auto [def, key] = visitExpr(scope, t->expr);
    visitType(scope, t->annotation);

    return {def, key};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprIfElse* i)
{
    visitExpr(scope, i->condition);
    visitExpr(scope, i->trueExpr);
    visitExpr(scope, i->falseExpr);

    return {defArena->freshCell(), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprInterpString* i)
{
    for (AstExpr* e : i->expressions)
        visitExpr(scope, e);

    return {defArena->freshCell(), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprError* error)
{
    DfgScope* unreachable = childScope(scope);
    for (AstExpr* e : error->expressions)
        visitExpr(unreachable, e);

    return {defArena->freshCell(), nullptr};
}

void DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExpr* e, DefId incomingDef, bool isCompoundAssignment)
{
    if (auto l = e->as<AstExprLocal>())
        return visitLValue(scope, l, incomingDef, isCompoundAssignment);
    else if (auto g = e->as<AstExprGlobal>())
        return visitLValue(scope, g, incomingDef, isCompoundAssignment);
    else if (auto i = e->as<AstExprIndexName>())
        return visitLValue(scope, i, incomingDef);
    else if (auto i = e->as<AstExprIndexExpr>())
        return visitLValue(scope, i, incomingDef);
    else if (auto error = e->as<AstExprError>())
        return visitLValue(scope, error, incomingDef);
    else
        handle->ice("Unknown AstExpr in DataFlowGraphBuilder::visitLValue");
}

void DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprLocal* l, DefId incomingDef, bool isCompoundAssignment)
{
    // We need to keep the previous breadcrumb around for a compound assignment.
    if (isCompoundAssignment)
    {
        if (auto def = scope->lookup(l->local))
            graph.compoundAssignBreadcrumbs[l] = *def;
    }

    // In order to avoid alias tracking, we need to clip the reference to the parent def.
    DefId updated = defArena->freshCell(containsSubscriptedDefinition(incomingDef));
    graph.astDefs[l] = updated;
    scope->bindings[l->local] = updated;
}

void DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprGlobal* g, DefId incomingDef, bool isCompoundAssignment)
{
    // We need to keep the previous breadcrumb around for a compound assignment.
    if (isCompoundAssignment)
    {
        if (auto def = scope->lookup(g->name))
            graph.compoundAssignBreadcrumbs[g] = *def;
    }

    // In order to avoid alias tracking, we need to clip the reference to the parent def.
    DefId updated = defArena->freshCell(containsSubscriptedDefinition(incomingDef));
    graph.astDefs[g] = updated;
    scope->bindings[g->name] = updated;
}

void DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprIndexName* i, DefId incomingDef)
{
    DefId parentDef = visitExpr(scope, i->expr).def;

    DefId updated = defArena->freshCell(containsSubscriptedDefinition(incomingDef));
    graph.astDefs[i] = updated;
    scope->props[parentDef][i->index.value] = updated;
}

void DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprIndexExpr* i, DefId incomingDef)
{
    DefId parentDef = visitExpr(scope, i->expr).def;
    visitExpr(scope, i->index);

    if (auto string = i->index->as<AstExprConstantString>())
    {
        DefId updated = defArena->freshCell(containsSubscriptedDefinition(incomingDef));
        graph.astDefs[i] = updated;
        scope->props[parentDef][string->value.data] = updated;
    }

    graph.astDefs[i] = defArena->freshCell();
}

void DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprError* error, DefId incomingDef)
{
    DefId def = visitExpr(scope, error).def;
    graph.astDefs[error] = def;
}

void DataFlowGraphBuilder::visitType(DfgScope* scope, AstType* t)
{
    if (auto r = t->as<AstTypeReference>())
        return visitType(scope, r);
    else if (auto table = t->as<AstTypeTable>())
        return visitType(scope, table);
    else if (auto f = t->as<AstTypeFunction>())
        return visitType(scope, f);
    else if (auto tyof = t->as<AstTypeTypeof>())
        return visitType(scope, tyof);
    else if (auto u = t->as<AstTypeUnion>())
        return visitType(scope, u);
    else if (auto i = t->as<AstTypeIntersection>())
        return visitType(scope, i);
    else if (auto e = t->as<AstTypeError>())
        return visitType(scope, e);
    else if (auto s = t->as<AstTypeSingletonBool>())
        return; // ok
    else if (auto s = t->as<AstTypeSingletonString>())
        return; // ok
    else
        handle->ice("Unknown AstType in DataFlowGraphBuilder::visitType");
}

void DataFlowGraphBuilder::visitType(DfgScope* scope, AstTypeReference* r)
{
    for (AstTypeOrPack param : r->parameters)
    {
        if (param.type)
            visitType(scope, param.type);
        else
            visitTypePack(scope, param.typePack);
    }
}

void DataFlowGraphBuilder::visitType(DfgScope* scope, AstTypeTable* t)
{
    for (AstTableProp p : t->props)
        visitType(scope, p.type);

    if (t->indexer)
    {
        visitType(scope, t->indexer->indexType);
        visitType(scope, t->indexer->resultType);
    }
}

void DataFlowGraphBuilder::visitType(DfgScope* scope, AstTypeFunction* f)
{
    visitGenerics(scope, f->generics);
    visitGenericPacks(scope, f->genericPacks);
    visitTypeList(scope, f->argTypes);
    visitTypeList(scope, f->returnTypes);
}

void DataFlowGraphBuilder::visitType(DfgScope* scope, AstTypeTypeof* t)
{
    visitExpr(scope, t->expr);
}

void DataFlowGraphBuilder::visitType(DfgScope* scope, AstTypeUnion* u)
{
    for (AstType* t : u->types)
        visitType(scope, t);
}

void DataFlowGraphBuilder::visitType(DfgScope* scope, AstTypeIntersection* i)
{
    for (AstType* t : i->types)
        visitType(scope, t);
}

void DataFlowGraphBuilder::visitType(DfgScope* scope, AstTypeError* error)
{
    for (AstType* t : error->types)
        visitType(scope, t);
}

void DataFlowGraphBuilder::visitTypePack(DfgScope* scope, AstTypePack* p)
{
    if (auto e = p->as<AstTypePackExplicit>())
        return visitTypePack(scope, e);
    else if (auto v = p->as<AstTypePackVariadic>())
        return visitTypePack(scope, v);
    else if (auto g = p->as<AstTypePackGeneric>())
        return; // ok
    else
        handle->ice("Unknown AstTypePack in DataFlowGraphBuilder::visitTypePack");
}

void DataFlowGraphBuilder::visitTypePack(DfgScope* scope, AstTypePackExplicit* e)
{
    visitTypeList(scope, e->typeList);
}

void DataFlowGraphBuilder::visitTypePack(DfgScope* scope, AstTypePackVariadic* v)
{
    visitType(scope, v->variadicType);
}

void DataFlowGraphBuilder::visitTypeList(DfgScope* scope, AstTypeList l)
{
    for (AstType* t : l.types)
        visitType(scope, t);

    if (l.tailType)
        visitTypePack(scope, l.tailType);
}

void DataFlowGraphBuilder::visitGenerics(DfgScope* scope, AstArray<AstGenericType> g)
{
    for (AstGenericType generic : g)
    {
        if (generic.defaultValue)
            visitType(scope, generic.defaultValue);
    }
}

void DataFlowGraphBuilder::visitGenericPacks(DfgScope* scope, AstArray<AstGenericTypePack> g)
{
    for (AstGenericTypePack generic : g)
    {
        if (generic.defaultValue)
            visitTypePack(scope, generic.defaultValue);
    }
}

} // namespace Luau
