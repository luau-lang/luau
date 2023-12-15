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
LUAU_FASTFLAG(LuauLoopControlFlowAnalysis)

namespace Luau
{

bool doesCallError(const AstExprCall* call); // TypeInfer.cpp

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
    auto def = compoundAssignDefs.find(expr);
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
        if (auto props = current->props.find(def))
        {
            if (auto it = props->find(key); it != props->end())
                return NotNull{it->second};
        }
    }

    return std::nullopt;
}

void DfgScope::inherit(const DfgScope* childScope)
{
    for (const auto& [k, a] : childScope->bindings)
    {
        if (lookup(k))
            bindings[k] = a;
    }

    for (const auto& [k1, a1] : childScope->props)
    {
        for (const auto& [k2, a2] : a1)
            props[k1][k2] = a2;
    }
}

bool DfgScope::canUpdateDefinition(Symbol symbol) const
{
    for (const DfgScope* current = this; current; current = current->parent)
    {
        if (current->bindings.find(symbol))
            return true;
        else if (current->scopeType == DfgScope::Loop)
            return false;
    }

    return true;
}

bool DfgScope::canUpdateDefinition(DefId def, const std::string& key) const
{
    for (const DfgScope* current = this; current; current = current->parent)
    {
        if (auto props = current->props.find(def))
            return true;
        else if (current->scopeType == DfgScope::Loop)
            return false;
    }

    return true;
}

DataFlowGraph DataFlowGraphBuilder::build(AstStatBlock* block, NotNull<InternalErrorReporter> handle)
{
    LUAU_ASSERT(FFlag::DebugLuauDeferredConstraintResolution);

    DataFlowGraphBuilder builder;
    builder.handle = handle;
    builder.moduleScope = builder.childScope(nullptr); // nullptr is the root DFG scope.
    builder.visitBlockWithoutChildScope(builder.moduleScope, block);
    builder.resolveCaptures();

    if (FFlag::DebugLuauFreezeArena)
    {
        builder.defArena->allocator.freeze();
        builder.keyArena->allocator.freeze();
    }

    return std::move(builder.graph);
}

void DataFlowGraphBuilder::resolveCaptures()
{
    for (const auto& [_, capture] : captures)
    {
        std::vector<DefId> operands;
        for (size_t i = capture.versionOffset; i < capture.allVersions.size(); ++i)
            collectOperands(capture.allVersions[i], &operands);

        for (DefId captureDef : capture.captureDefs)
        {
            Phi* phi = const_cast<Phi*>(get<Phi>(captureDef));
            LUAU_ASSERT(phi);
            LUAU_ASSERT(phi->operands.empty());
            phi->operands = operands;
        }
    }
}

DfgScope* DataFlowGraphBuilder::childScope(DfgScope* scope, DfgScope::ScopeType scopeType)
{
    return scopes.emplace_back(new DfgScope{scope, scopeType}).get();
}

void DataFlowGraphBuilder::join(DfgScope* p, DfgScope* a, DfgScope* b)
{
    joinBindings(p, *a, *b);
    joinProps(p, *a, *b);
}

void DataFlowGraphBuilder::joinBindings(DfgScope* p, const DfgScope& a, const DfgScope& b)
{
    for (const auto& [sym, def1] : a.bindings)
    {
        if (auto def2 = b.bindings.find(sym))
            p->bindings[sym] = defArena->phi(NotNull{def1}, NotNull{*def2});
        else if (auto def2 = p->lookup(sym))
            p->bindings[sym] = defArena->phi(NotNull{def1}, NotNull{*def2});
    }

    for (const auto& [sym, def1] : b.bindings)
    {
        if (auto def2 = p->lookup(sym))
            p->bindings[sym] = defArena->phi(NotNull{def1}, NotNull{*def2});
    }
}

void DataFlowGraphBuilder::joinProps(DfgScope* result, const DfgScope& a, const DfgScope& b)
{
    auto phinodify = [this](DfgScope* scope, const auto& a, const auto& b, DefId parent) mutable {
        auto& p = scope->props[parent];
        for (const auto& [k, defA] : a)
        {
            if (auto it = b.find(k); it != b.end())
                p[k] = defArena->phi(NotNull{it->second}, NotNull{defA});
            else if (auto it = p.find(k); it != p.end())
                p[k] = defArena->phi(NotNull{it->second}, NotNull{defA});
            else if (auto def2 = scope->lookup(parent, k))
                p[k] = defArena->phi(*def2, NotNull{defA});
            else
                p[k] = defA;
        }

        for (const auto& [k, defB] : b)
        {
            if (auto it = a.find(k); it != a.end())
                continue;
            else if (auto it = p.find(k); it != p.end())
                p[k] = defArena->phi(NotNull{it->second}, NotNull{defB});
            else if (auto def2 = scope->lookup(parent, k))
                p[k] = defArena->phi(*def2, NotNull{defB});
            else
                p[k] = defB;
        }
    };

    for (const auto& [def, a1] : a.props)
    {
        result->props.try_insert(def, {});
        if (auto a2 = b.props.find(def))
            phinodify(result, a1, *a2, NotNull{def});
        else if (auto a2 = result->props.find(def))
            phinodify(result, a1, *a2, NotNull{def});
    }

    for (const auto& [def, a1] : b.props)
    {
        result->props.try_insert(def, {});
        if (a.props.find(def))
            continue;
        else if (auto a2 = result->props.find(def))
            phinodify(result, a1, *a2, NotNull{def});
    }
}

DefId DataFlowGraphBuilder::lookup(DfgScope* scope, Symbol symbol)
{
    for (DfgScope* current = scope; current; current = current->parent)
    {
        if (auto found = current->bindings.find(symbol))
            return NotNull{*found};
        else if (current->scopeType == DfgScope::Function)
        {
            FunctionCapture& capture = captures[symbol];
            DefId captureDef = defArena->phi({});
            capture.captureDefs.push_back(captureDef);
            scope->bindings[symbol] = captureDef;
            return NotNull{captureDef};
        }
    }

    DefId result = defArena->freshCell();
    scope->bindings[symbol] = result;
    captures[symbol].allVersions.push_back(result);
    return result;
}

DefId DataFlowGraphBuilder::lookup(DfgScope* scope, DefId def, const std::string& key)
{
    for (DfgScope* current = scope; current; current = current->parent)
    {
        if (auto props = current->props.find(def))
        {
            if (auto it = props->find(key); it != props->end())
                return NotNull{it->second};
        }
        else if (auto phi = get<Phi>(def); phi && phi->operands.empty()) // Unresolved phi nodes
        {
            DefId result = defArena->freshCell();
            scope->props[def][key] = result;
            return result;
        }
    }

    if (auto phi = get<Phi>(def))
    {
        std::vector<DefId> defs;
        for (DefId operand : phi->operands)
            defs.push_back(lookup(scope, operand, key));

        DefId result = defArena->phi(defs);
        scope->props[def][key] = result;
        return result;
    }
    else if (get<Cell>(def))
    {
        DefId result = defArena->freshCell();
        scope->props[def][key] = result;
        return result;
    }
    else
        handle->ice("Inexhaustive lookup cases in DataFlowGraphBuilder::lookup");
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatBlock* b)
{
    DfgScope* child = childScope(scope);
    ControlFlow cf = visitBlockWithoutChildScope(child, b);
    scope->inherit(child);
    return cf;
}

ControlFlow DataFlowGraphBuilder::visitBlockWithoutChildScope(DfgScope* scope, AstStatBlock* b)
{
    std::optional<ControlFlow> firstControlFlow;
    for (AstStat* stat : b->body)
    {
        ControlFlow cf = visit(scope, stat);
        if (cf != ControlFlow::None && !firstControlFlow)
            firstControlFlow = cf;
    }

    return firstControlFlow.value_or(ControlFlow::None);
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStat* s)
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

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatIf* i)
{
    visitExpr(scope, i->condition);

    DfgScope* thenScope = childScope(scope);
    DfgScope* elseScope = childScope(scope);

    ControlFlow thencf = visit(thenScope, i->thenbody);
    ControlFlow elsecf = ControlFlow::None;
    if (i->elsebody)
        elsecf = visit(elseScope, i->elsebody);

    if (thencf != ControlFlow::None && elsecf == ControlFlow::None)
        join(scope, scope, elseScope);
    else if (thencf == ControlFlow::None && elsecf != ControlFlow::None)
        join(scope, thenScope, scope);
    else if ((thencf | elsecf) == ControlFlow::None)
        join(scope, thenScope, elseScope);

    if (FFlag::LuauLoopControlFlowAnalysis && thencf == elsecf)
        return thencf;
    else if (matches(thencf, ControlFlow::Returns | ControlFlow::Throws) && matches(elsecf, ControlFlow::Returns | ControlFlow::Throws))
        return ControlFlow::Returns;
    else
        return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatWhile* w)
{
    // TODO(controlflow): entry point has a back edge from exit point
    DfgScope* whileScope = childScope(scope, DfgScope::Loop);
    visitExpr(whileScope, w->condition);
    visit(whileScope, w->body);

    scope->inherit(whileScope);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatRepeat* r)
{
    // TODO(controlflow): entry point has a back edge from exit point
    DfgScope* repeatScope = childScope(scope, DfgScope::Loop);
    visitBlockWithoutChildScope(repeatScope, r->body);
    visitExpr(repeatScope, r->condition);

    scope->inherit(repeatScope);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatBreak* b)
{
    return ControlFlow::Breaks;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatContinue* c)
{
    return ControlFlow::Continues;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatReturn* r)
{
    for (AstExpr* e : r->list)
        visitExpr(scope, e);

    return ControlFlow::Returns;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatExpr* e)
{
    visitExpr(scope, e->expr);
    if (auto call = e->expr->as<AstExprCall>(); call && doesCallError(call))
        return ControlFlow::Throws;
    else
        return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatLocal* l)
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
        if (i < l->values.size)
        {
            AstExpr* e = l->values.data[i];
            if (const AstExprTable* tbl = e->as<AstExprTable>())
            {
                def = defs[i];
            }
        }
        graph.localDefs[local] = def;
        scope->bindings[local] = def;
        captures[local].allVersions.push_back(def);
    }

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatFor* f)
{
    DfgScope* forScope = childScope(scope, DfgScope::Loop);

    visitExpr(scope, f->from);
    visitExpr(scope, f->to);
    if (f->step)
        visitExpr(scope, f->step);

    if (f->var->annotation)
        visitType(forScope, f->var->annotation);

    DefId def = defArena->freshCell();
    graph.localDefs[f->var] = def;
    scope->bindings[f->var] = def;
    captures[f->var].allVersions.push_back(def);

    // TODO(controlflow): entry point has a back edge from exit point
    visit(forScope, f->body);

    scope->inherit(forScope);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatForIn* f)
{
    DfgScope* forScope = childScope(scope, DfgScope::Loop);

    for (AstLocal* local : f->vars)
    {
        if (local->annotation)
            visitType(forScope, local->annotation);

        DefId def = defArena->freshCell();
        graph.localDefs[local] = def;
        forScope->bindings[local] = def;
        captures[local].allVersions.push_back(def);
    }

    // TODO(controlflow): entry point has a back edge from exit point
    // We're gonna need a `visitExprList` and `visitVariadicExpr` (function calls and `...`)
    for (AstExpr* e : f->values)
        visitExpr(forScope, e);

    visit(forScope, f->body);

    scope->inherit(forScope);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatAssign* a)
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

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatCompoundAssign* c)
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

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatFunction* f)
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
    visitLValue(scope, f->name, defArena->freshCell());
    visitExpr(scope, f->func);

    if (auto local = f->name->as<AstExprLocal>())
    {
        // local f
        // function f()
        //   if cond() then
        //     f() -- should reference only the function version and other future version, and nothing prior
        //   end
        // end
        FunctionCapture& capture = captures[local->local];
        capture.versionOffset = capture.allVersions.size() - 1;
    }

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatLocalFunction* l)
{
    DefId def = defArena->freshCell();
    graph.localDefs[l->name] = def;
    scope->bindings[l->name] = def;
    captures[l->name].allVersions.push_back(def);
    visitExpr(scope, l->func);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatTypeAlias* t)
{
    DfgScope* unreachable = childScope(scope);
    visitGenerics(unreachable, t->generics);
    visitGenericPacks(unreachable, t->genericPacks);
    visitType(unreachable, t->type);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatDeclareGlobal* d)
{
    DefId def = defArena->freshCell();
    graph.declaredDefs[d] = def;
    scope->bindings[d->name] = def;
    captures[d->name].allVersions.push_back(def);

    visitType(scope, d->type);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatDeclareFunction* d)
{
    DefId def = defArena->freshCell();
    graph.declaredDefs[d] = def;
    scope->bindings[d->name] = def;
    captures[d->name].allVersions.push_back(def);

    DfgScope* unreachable = childScope(scope);
    visitGenerics(unreachable, d->generics);
    visitGenericPacks(unreachable, d->genericPacks);
    visitTypeList(unreachable, d->params);
    visitTypeList(unreachable, d->retTypes);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatDeclareClass* d)
{
    // This declaration does not "introduce" any bindings in value namespace,
    // so there's no symbolic value to begin with. We'll traverse the properties
    // because their type annotations may depend on something in the value namespace.
    DfgScope* unreachable = childScope(scope);
    for (AstDeclaredClassProp prop : d->props)
        visitType(unreachable, prop.ty);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(DfgScope* scope, AstStatError* error)
{
    DfgScope* unreachable = childScope(scope);
    for (AstStat* s : error->statements)
        visit(unreachable, s);
    for (AstExpr* e : error->expressions)
        visitExpr(unreachable, e);

    return ControlFlow::None;
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExpr* e)
{
    // Some subexpressions could be visited two times. If we've already seen it, just extract it.
    if (auto def = graph.astDefs.find(e))
    {
        auto key = graph.astRefinementKeys.find(e);
        return {NotNull{*def}, key ? *key : nullptr};
    }

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
    DefId def = lookup(scope, l->local);
    const RefinementKey* key = keyArena->leaf(def);
    return {def, key};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprGlobal* g)
{
    DefId def = lookup(scope, g->name);
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

    DefId def = lookup(scope, parentDef, index);
    return {def, keyArena->node(parentKey, def, index)};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprIndexExpr* i)
{
    auto [parentDef, parentKey] = visitExpr(scope, i->expr);
    visitExpr(scope, i->index);

    if (auto string = i->index->as<AstExprConstantString>())
    {
        std::string index{string->value.data, string->value.size};

        DefId def = lookup(scope, parentDef, index);
        return {def, keyArena->node(parentKey, def, index)};
    }

    return {defArena->freshCell(/* subscripted= */ true), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(DfgScope* scope, AstExprFunction* f)
{
    DfgScope* signatureScope = childScope(scope, DfgScope::Function);

    if (AstLocal* self = f->self)
    {
        // There's no syntax for `self` to have an annotation if using `function t:m()`
        LUAU_ASSERT(!self->annotation);

        DefId def = defArena->freshCell();
        graph.localDefs[self] = def;
        signatureScope->bindings[self] = def;
        captures[self].allVersions.push_back(def);
    }

    for (AstLocal* param : f->args)
    {
        if (param->annotation)
            visitType(signatureScope, param->annotation);

        DefId def = defArena->freshCell();
        graph.localDefs[param] = def;
        signatureScope->bindings[param] = def;
        captures[param].allVersions.push_back(def);
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
    DefId tableCell = defArena->freshCell();
    scope->props[tableCell] = {};
    for (AstExprTable::Item item : t->items)
    {
        DataFlowResult result = visitExpr(scope, item.value);
        if (item.key)
        {
            visitExpr(scope, item.key);
            if (auto string = item.key->as<AstExprConstantString>())
                scope->props[tableCell][string->value.data] = result.def;
        }
    }

    return {tableCell, nullptr};
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
    auto go = [&]() {
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
    };

    graph.astDefs[e] = go();
}

DefId DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprLocal* l, DefId incomingDef, bool isCompoundAssignment)
{
    // We need to keep the previous def around for a compound assignment.
    if (isCompoundAssignment)
    {
        DefId def = lookup(scope, l->local);
        graph.compoundAssignDefs[l] = def;
    }

    // In order to avoid alias tracking, we need to clip the reference to the parent def.
    if (scope->canUpdateDefinition(l->local))
    {
        DefId updated = defArena->freshCell(containsSubscriptedDefinition(incomingDef));
        scope->bindings[l->local] = updated;
        captures[l->local].allVersions.push_back(updated);
        return updated;
    }
    else
        return visitExpr(scope, static_cast<AstExpr*>(l)).def;
}

DefId DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprGlobal* g, DefId incomingDef, bool isCompoundAssignment)
{
    // We need to keep the previous def around for a compound assignment.
    if (isCompoundAssignment)
    {
        DefId def = lookup(scope, g->name);
        graph.compoundAssignDefs[g] = def;
    }

    // In order to avoid alias tracking, we need to clip the reference to the parent def.
    if (scope->canUpdateDefinition(g->name))
    {
        DefId updated = defArena->freshCell(containsSubscriptedDefinition(incomingDef));
        scope->bindings[g->name] = updated;
        captures[g->name].allVersions.push_back(updated);
        return updated;
    }
    else
        return visitExpr(scope, static_cast<AstExpr*>(g)).def;
}

DefId DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprIndexName* i, DefId incomingDef)
{
    DefId parentDef = visitExpr(scope, i->expr).def;

    if (scope->canUpdateDefinition(parentDef, i->index.value))
    {
        DefId updated = defArena->freshCell(containsSubscriptedDefinition(incomingDef));
        scope->props[parentDef][i->index.value] = updated;
        return updated;
    }
    else
        return visitExpr(scope, static_cast<AstExpr*>(i)).def;
}

DefId DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprIndexExpr* i, DefId incomingDef)
{
    DefId parentDef = visitExpr(scope, i->expr).def;
    visitExpr(scope, i->index);

    if (auto string = i->index->as<AstExprConstantString>())
    {
        if (scope->canUpdateDefinition(parentDef, string->value.data))
        {
            DefId updated = defArena->freshCell(containsSubscriptedDefinition(incomingDef));
            scope->props[parentDef][string->value.data] = updated;
            return updated;
        }
        else
            return visitExpr(scope, static_cast<AstExpr*>(i)).def;
    }
    else
        return defArena->freshCell(/*subscripted=*/true);
}

DefId DataFlowGraphBuilder::visitLValue(DfgScope* scope, AstExprError* error, DefId incomingDef)
{
    return visitExpr(scope, error).def;
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
