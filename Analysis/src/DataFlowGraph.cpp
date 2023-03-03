// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/DataFlowGraph.h"

#include "Luau/Breadcrumb.h"
#include "Luau/Error.h"
#include "Luau/Refinement.h"

LUAU_FASTFLAG(DebugLuauFreezeArena)
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

namespace Luau
{

NullableBreadcrumbId DataFlowGraph::getBreadcrumb(const AstExpr* expr) const
{
    // We need to skip through AstExprGroup because DFG doesn't try its best to transitively
    while (auto group = expr->as<AstExprGroup>())
        expr = group->expr;
    if (auto bc = astBreadcrumbs.find(expr))
        return *bc;
    return nullptr;
}

BreadcrumbId DataFlowGraph::getBreadcrumb(const AstLocal* local) const
{
    auto bc = localBreadcrumbs.find(local);
    LUAU_ASSERT(bc);
    return NotNull{*bc};
}

BreadcrumbId DataFlowGraph::getBreadcrumb(const AstExprLocal* local) const
{
    auto bc = astBreadcrumbs.find(local);
    LUAU_ASSERT(bc);
    return NotNull{*bc};
}

BreadcrumbId DataFlowGraph::getBreadcrumb(const AstExprGlobal* global) const
{
    auto bc = astBreadcrumbs.find(global);
    LUAU_ASSERT(bc);
    return NotNull{*bc};
}

BreadcrumbId DataFlowGraph::getBreadcrumb(const AstStatDeclareGlobal* global) const
{
    auto bc = declaredBreadcrumbs.find(global);
    LUAU_ASSERT(bc);
    return NotNull{*bc};
}

BreadcrumbId DataFlowGraph::getBreadcrumb(const AstStatDeclareFunction* func) const
{
    auto bc = declaredBreadcrumbs.find(func);
    LUAU_ASSERT(bc);
    return NotNull{*bc};
}

NullableBreadcrumbId DfgScope::lookup(Symbol symbol) const
{
    for (const DfgScope* current = this; current; current = current->parent)
    {
        if (auto breadcrumb = current->bindings.find(symbol))
            return *breadcrumb;
    }

    return nullptr;
}

NullableBreadcrumbId DfgScope::lookup(DefId def, const std::string& key) const
{
    for (const DfgScope* current = this; current; current = current->parent)
    {
        if (auto map = props.find(def))
        {
            if (auto it = map->find(key); it != map->end())
                return it->second;
        }
    }

    return nullptr;
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
        builder.defs->allocator.freeze();
        builder.breadcrumbs->allocator.freeze();
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
    std::vector<BreadcrumbId> bcs;
    bcs.reserve(l->values.size);
    for (AstExpr* e : l->values)
        bcs.push_back(visitExpr(scope, e));

    for (size_t i = 0; i < l->vars.size; ++i)
    {
        AstLocal* local = l->vars.data[i];
        if (local->annotation)
            visitType(scope, local->annotation);

        // We need to create a new breadcrumb with new defs to intentionally avoid alias tracking.
        BreadcrumbId bc = breadcrumbs->add(nullptr, defs->freshCell(), i < bcs.size() ? bcs[i]->metadata : std::nullopt);
        graph.localBreadcrumbs[local] = bc;
        scope->bindings[local] = bc;
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

    // TODO: RangeMetadata.
    BreadcrumbId bc = breadcrumbs->add(nullptr, defs->freshCell());
    graph.localBreadcrumbs[f->var] = bc;
    scope->bindings[f->var] = bc;

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

        // TODO: IterMetadata (different from RangeMetadata)
        BreadcrumbId bc = breadcrumbs->add(nullptr, defs->freshCell());
        graph.localBreadcrumbs[local] = bc;
        forScope->bindings[local] = bc;
    }

    // TODO(controlflow): entry point has a back edge from exit point
    // We're gonna need a `visitExprList` and `visitVariadicExpr` (function calls and `...`)
    for (AstExpr* e : f->values)
        visitExpr(forScope, e);

    visit(forScope, f->body);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatAssign* a)
{
    for (AstExpr* r : a->values)
        visitExpr(scope, r);

    for (AstExpr* l : a->vars)
        visitLValue(scope, l);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatCompoundAssign* c)
{
    // TODO: This needs revisiting because this is incorrect. The `c->var` part is both being read and written to,
    // but the `c->var` only has one pointer address, so we need to come up with a way to store both.
    // For now, it's not important because we don't have type states, but it is going to be important, e.g.
    //
    // local a = 5 -- a[1]
    // a += 5      -- a[2] = a[1] + 5
    //
    // We can't just visit `c->var` as a rvalue and then separately traverse `c->var` as an lvalue, since that's O(n^2).
    visitLValue(scope, c->var);
    visitExpr(scope, c->value);
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
    visitLValue(scope, f->name);
    visitExpr(scope, f->func);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatLocalFunction* l)
{
    BreadcrumbId bc = breadcrumbs->add(nullptr, defs->freshCell());
    graph.localBreadcrumbs[l->name] = bc;
    scope->bindings[l->name] = bc;

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
    // TODO: AmbientDeclarationMetadata.
    BreadcrumbId bc = breadcrumbs->add(nullptr, defs->freshCell());
    graph.declaredBreadcrumbs[d] = bc;
    scope->bindings[d->name] = bc;

    visitType(scope, d->type);
}

void DataFlowGraphBuilder::visit(DfgScope* scope, AstStatDeclareFunction* d)
{
    // TODO: AmbientDeclarationMetadata.
    BreadcrumbId bc = breadcrumbs->add(nullptr, defs->freshCell());
    graph.declaredBreadcrumbs[d] = bc;
    scope->bindings[d->name] = bc;

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

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExpr* e)
{
    if (auto g = e->as<AstExprGroup>())
        return visitExpr(scope, g->expr);
    else if (auto c = e->as<AstExprConstantNil>())
        return breadcrumbs->add(nullptr, defs->freshCell()); // ok
    else if (auto c = e->as<AstExprConstantBool>())
        return breadcrumbs->add(nullptr, defs->freshCell()); // ok
    else if (auto c = e->as<AstExprConstantNumber>())
        return breadcrumbs->add(nullptr, defs->freshCell()); // ok
    else if (auto c = e->as<AstExprConstantString>())
        return breadcrumbs->add(nullptr, defs->freshCell()); // ok
    else if (auto l = e->as<AstExprLocal>())
        return visitExpr(scope, l);
    else if (auto g = e->as<AstExprGlobal>())
        return visitExpr(scope, g);
    else if (auto v = e->as<AstExprVarargs>())
        return breadcrumbs->add(nullptr, defs->freshCell()); // ok
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
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprLocal* l)
{
    NullableBreadcrumbId breadcrumb = scope->lookup(l->local);
    if (!breadcrumb)
        handle->ice("AstExprLocal came before its declaration?");

    graph.astBreadcrumbs[l] = breadcrumb;
    return NotNull{breadcrumb};
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprGlobal* g)
{
    NullableBreadcrumbId bc = scope->lookup(g->name);
    if (!bc)
    {
        bc = breadcrumbs->add(nullptr, defs->freshCell());
        moduleScope->bindings[g->name] = bc;
    }

    graph.astBreadcrumbs[g] = bc;
    return NotNull{bc};
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprCall* c)
{
    visitExpr(scope, c->func);

    for (AstExpr* arg : c->args)
        visitExpr(scope, arg);

    return breadcrumbs->add(nullptr, defs->freshCell());
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprIndexName* i)
{
    BreadcrumbId parentBreadcrumb = visitExpr(scope, i->expr);

    std::string key = i->index.value;
    NullableBreadcrumbId& propBreadcrumb = moduleScope->props[parentBreadcrumb->def][key];
    if (!propBreadcrumb)
        propBreadcrumb = breadcrumbs->emplace<FieldMetadata>(parentBreadcrumb, defs->freshCell(), key);

    graph.astBreadcrumbs[i] = propBreadcrumb;
    return NotNull{propBreadcrumb};
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprIndexExpr* i)
{
    BreadcrumbId parentBreadcrumb = visitExpr(scope, i->expr);
    BreadcrumbId key = visitExpr(scope, i->index);

    if (auto string = i->index->as<AstExprConstantString>())
    {
        std::string key{string->value.data, string->value.size};
        NullableBreadcrumbId& propBreadcrumb = moduleScope->props[parentBreadcrumb->def][key];
        if (!propBreadcrumb)
            propBreadcrumb = breadcrumbs->emplace<FieldMetadata>(parentBreadcrumb, defs->freshCell(), key);

        graph.astBreadcrumbs[i] = NotNull{propBreadcrumb};
        return NotNull{propBreadcrumb};
    }

    return breadcrumbs->emplace<SubscriptMetadata>(nullptr, defs->freshCell(), key);
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprFunction* f)
{
    DfgScope* signatureScope = childScope(scope);

    if (AstLocal* self = f->self)
    {
        // There's no syntax for `self` to have an annotation if using `function t:m()`
        LUAU_ASSERT(!self->annotation);

        // TODO: ParameterMetadata.
        BreadcrumbId bc = breadcrumbs->add(nullptr, defs->freshCell());
        graph.localBreadcrumbs[self] = bc;
        signatureScope->bindings[self] = bc;
    }

    for (AstLocal* param : f->args)
    {
        if (param->annotation)
            visitType(signatureScope, param->annotation);

        // TODO: ParameterMetadata.
        BreadcrumbId bc = breadcrumbs->add(nullptr, defs->freshCell());
        graph.localBreadcrumbs[param] = bc;
        signatureScope->bindings[param] = bc;
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

    return breadcrumbs->add(nullptr, defs->freshCell());
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprTable* t)
{
    for (AstExprTable::Item item : t->items)
    {
        if (item.key)
            visitExpr(scope, item.key);
        visitExpr(scope, item.value);
    }

    return breadcrumbs->add(nullptr, defs->freshCell());
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprUnary* u)
{
    visitExpr(scope, u->expr);

    return breadcrumbs->add(nullptr, defs->freshCell());
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprBinary* b)
{
    visitExpr(scope, b->left);
    visitExpr(scope, b->right);

    return breadcrumbs->add(nullptr, defs->freshCell());
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprTypeAssertion* t)
{
    // TODO: TypeAssertionMetadata?
    BreadcrumbId bc = visitExpr(scope, t->expr);
    visitType(scope, t->annotation);

    return bc;
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprIfElse* i)
{
    visitExpr(scope, i->condition);
    visitExpr(scope, i->trueExpr);
    visitExpr(scope, i->falseExpr);

    return breadcrumbs->add(nullptr, defs->freshCell());
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprInterpString* i)
{
    for (AstExpr* e : i->expressions)
        visitExpr(scope, e);

    return breadcrumbs->add(nullptr, defs->freshCell());
}

BreadcrumbId DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprError* error)
{
    DfgScope* unreachable = childScope(scope);
    for (AstExpr* e : error->expressions)
        visitExpr(unreachable, e);

    return breadcrumbs->add(nullptr, defs->freshCell());
}

void DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExpr* e)
{
    if (auto l = e->as<AstExprLocal>())
        return visitLValue(scope, l);
    else if (auto g = e->as<AstExprGlobal>())
        return visitLValue(scope, g);
    else if (auto i = e->as<AstExprIndexName>())
        return visitLValue(scope, i);
    else if (auto i = e->as<AstExprIndexExpr>())
        return visitLValue(scope, i);
    else if (auto error = e->as<AstExprError>())
    {
        visitExpr(scope, error); // TODO: is this right?
        return;
    }
    else
        handle->ice("Unknown AstExpr in DataFlowGraphBuilder::visitLValue");
}

void DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprLocal* l)
{
    // Bug compatibility: we don't support type states yet, so we need to do this.
    NullableBreadcrumbId bc = scope->lookup(l->local);
    LUAU_ASSERT(bc);

    graph.astBreadcrumbs[l] = bc;
    scope->bindings[l->local] = bc;
}

void DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprGlobal* g)
{
    // Bug compatibility: we don't support type states yet, so we need to do this.
    NullableBreadcrumbId bc = scope->lookup(g->name);
    if (!bc)
        bc = breadcrumbs->add(nullptr, defs->freshCell());

    graph.astBreadcrumbs[g] = bc;
    scope->bindings[g->name] = bc;
}

void DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprIndexName* i)
{
    // Bug compatibility: we don't support type states yet, so we need to do this.
    BreadcrumbId parentBreadcrumb = visitExpr(scope, i->expr);

    std::string key = i->index.value;
    NullableBreadcrumbId propBreadcrumb = scope->lookup(parentBreadcrumb->def, key);
    if (!propBreadcrumb)
    {
        propBreadcrumb = breadcrumbs->emplace<FieldMetadata>(parentBreadcrumb, defs->freshCell(), key);
        moduleScope->props[parentBreadcrumb->def][key] = propBreadcrumb;
    }

    graph.astBreadcrumbs[i] = propBreadcrumb;
}

void DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprIndexExpr* i)
{
    BreadcrumbId parentBreadcrumb = visitExpr(scope, i->expr);
    visitExpr(scope, i->index);

    if (auto string = i->index->as<AstExprConstantString>())
    {
        std::string key{string->value.data, string->value.size};
        NullableBreadcrumbId propBreadcrumb = scope->lookup(parentBreadcrumb->def, key);
        if (!propBreadcrumb)
        {
            propBreadcrumb = breadcrumbs->add(parentBreadcrumb, parentBreadcrumb->def);
            moduleScope->props[parentBreadcrumb->def][key] = propBreadcrumb;
        }

        graph.astBreadcrumbs[i] = propBreadcrumb;
    }
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
