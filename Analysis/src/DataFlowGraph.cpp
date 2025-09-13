// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/DataFlowGraph.h"

#include "Luau/Ast.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Def.h"
#include "Luau/Common.h"
#include "Luau/Error.h"
#include "Luau/TimeTrace.h"

#include <memory>
#include <optional>

LUAU_FASTFLAG(DebugLuauFreezeArena)
LUAU_FASTFLAG(LuauSolverV2)

namespace Luau
{

bool doesCallError(const AstExprCall* call); // TypeInfer.cpp

struct PushScope
{
    ScopeStack& stack;
    size_t previousSize;

    PushScope(ScopeStack& stack, DfgScope* scope)
        : stack(stack)
        , previousSize(stack.size())
    {
        // `scope` should never be `nullptr` here.
        LUAU_ASSERT(scope);
        stack.push_back(scope);
    }

    ~PushScope()
    {
        // If somehow this stack has _shrunk_ to be smaller than we expect,
        // something very strange has happened.
        LUAU_ASSERT(stack.size() > previousSize);
        while (stack.size() > previousSize)
            stack.pop_back();
    }
};

const RefinementKey* RefinementKeyArena::leaf(DefId def)
{
    return allocator.allocate(RefinementKey{nullptr, def, std::nullopt});
}

const RefinementKey* RefinementKeyArena::node(const RefinementKey* parent, DefId def, const std::string& propName)
{
    return allocator.allocate(RefinementKey{parent, def, propName});
}

DataFlowGraph::DataFlowGraph(NotNull<DefArena> defArena, NotNull<RefinementKeyArena> keyArena)
    : defArena{defArena}
    , keyArena{keyArena}
{
}

DefId DataFlowGraph::getDef(const AstExpr* expr) const
{
    auto def = astDefs.find(expr);
    LUAU_ASSERT(def);
    return NotNull{*def};
}

std::optional<DefId> DataFlowGraph::getDefOptional(const AstExpr* expr) const
{
    auto def = astDefs.find(expr);
    if (!def)
        return std::nullopt;
    return NotNull{*def};
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

std::optional<Symbol> DataFlowGraph::getSymbolFromDef(const Def* def) const
{
    if (auto ref = defToSymbol.find(def))
        return *ref;

    return std::nullopt;
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

DataFlowGraphBuilder::DataFlowGraphBuilder(NotNull<DefArena> defArena, NotNull<RefinementKeyArena> keyArena)
    : graph{defArena, keyArena}
    , defArena{defArena}
    , keyArena{keyArena}
{
}

DataFlowGraph DataFlowGraphBuilder::build(
    AstStatBlock* block,
    NotNull<DefArena> defArena,
    NotNull<RefinementKeyArena> keyArena,
    NotNull<struct InternalErrorReporter> handle
)
{
    LUAU_TIMETRACE_SCOPE("DataFlowGraphBuilder::build", "Typechecking");

    DataFlowGraphBuilder builder(defArena, keyArena);
    builder.handle = handle;

    DfgScope* moduleScope = builder.scopes.emplace_back(new DfgScope{nullptr, DfgScope::ScopeType::Linear}).get();
    PushScope ps{builder.scopeStack, moduleScope};
    builder.visitBlockWithoutChildScope(block);
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

NotNull<DfgScope> DataFlowGraphBuilder::currentScope()
{
    LUAU_ASSERT(!scopeStack.empty());
    return NotNull{scopeStack.back()};
}

DfgScope* DataFlowGraphBuilder::makeChildScope(DfgScope::ScopeType scopeType)
{
    return scopes.emplace_back(new DfgScope{currentScope(), scopeType}).get();
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
    auto phinodify = [this](DfgScope* scope, const auto& a, const auto& b, DefId parent) mutable
    {
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

DefId DataFlowGraphBuilder::lookup(Symbol symbol, Location location)
{
    DfgScope* scope = currentScope();

    // true if any of the considered scopes are a loop.
    bool outsideLoopScope = false;
    for (DfgScope* current = scope; current; current = current->parent)
    {
        outsideLoopScope = outsideLoopScope || current->scopeType == DfgScope::Loop;

        if (auto found = current->bindings.find(symbol))
            return NotNull{*found};
        else if (current->scopeType == DfgScope::Function)
        {
            FunctionCapture& capture = captures[symbol];
            DefId captureDef = defArena->phi({});
            capture.captureDefs.push_back(captureDef);

            // If we are outside of a loop scope, then we don't want to actually bind
            // uses of `symbol` to this new phi node since it will not get populated.
            if (!outsideLoopScope)
                scope->bindings[symbol] = captureDef;

            return NotNull{captureDef};
        }
    }

    DefId result = defArena->freshCell(symbol, location);
    scope->bindings[symbol] = result;
    captures[symbol].allVersions.push_back(result);
    return result;
}

DefId DataFlowGraphBuilder::lookup(DefId def, const std::string& key, Location location)
{
    DfgScope* scope = currentScope();
    for (DfgScope* current = scope; current; current = current->parent)
    {
        if (auto props = current->props.find(def))
        {
            if (auto it = props->find(key); it != props->end())
                return NotNull{it->second};
        }
        else if (auto phi = get<Phi>(def); phi && phi->operands.empty()) // Unresolved phi nodes
        {
            DefId result = defArena->freshCell(def->name, location);
            scope->props[def][key] = result;
            return result;
        }
    }

    if (auto phi = get<Phi>(def))
    {
        std::vector<DefId> defs;
        for (DefId operand : phi->operands)
            defs.push_back(lookup(operand, key, location));

        DefId result = defArena->phi(defs);
        scope->props[def][key] = result;
        return result;
    }
    else if (get<Cell>(def))
    {
        DefId result = defArena->freshCell(def->name, location);
        scope->props[def][key] = result;
        return result;
    }
    else
        handle->ice("Inexhaustive lookup cases in DataFlowGraphBuilder::lookup");
}

ControlFlow DataFlowGraphBuilder::visit(AstStatBlock* b)
{
    DfgScope* child = makeChildScope();

    ControlFlow cf;
    {
        PushScope ps{scopeStack, child};
        cf = visitBlockWithoutChildScope(b);
    }

    currentScope()->inherit(child);
    return cf;
}

ControlFlow DataFlowGraphBuilder::visitBlockWithoutChildScope(AstStatBlock* b)
{
    std::optional<ControlFlow> firstControlFlow;
    for (AstStat* stat : b->body)
    {
        ControlFlow cf = visit(stat);
        if (cf != ControlFlow::None && !firstControlFlow)
            firstControlFlow = cf;
    }

    return firstControlFlow.value_or(ControlFlow::None);
}

ControlFlow DataFlowGraphBuilder::visit(AstStat* s)
{
    if (auto b = s->as<AstStatBlock>())
        return visit(b);
    else if (auto i = s->as<AstStatIf>())
        return visit(i);
    else if (auto w = s->as<AstStatWhile>())
        return visit(w);
    else if (auto r = s->as<AstStatRepeat>())
        return visit(r);
    else if (auto b = s->as<AstStatBreak>())
        return visit(b);
    else if (auto c = s->as<AstStatContinue>())
        return visit(c);
    else if (auto r = s->as<AstStatReturn>())
        return visit(r);
    else if (auto e = s->as<AstStatExpr>())
        return visit(e);
    else if (auto l = s->as<AstStatLocal>())
        return visit(l);
    else if (auto f = s->as<AstStatFor>())
        return visit(f);
    else if (auto f = s->as<AstStatForIn>())
        return visit(f);
    else if (auto a = s->as<AstStatAssign>())
        return visit(a);
    else if (auto c = s->as<AstStatCompoundAssign>())
        return visit(c);
    else if (auto f = s->as<AstStatFunction>())
        return visit(f);
    else if (auto l = s->as<AstStatLocalFunction>())
        return visit(l);
    else if (auto t = s->as<AstStatTypeAlias>())
        return visit(t);
    else if (auto f = s->as<AstStatTypeFunction>())
        return visit(f);
    else if (auto d = s->as<AstStatDeclareGlobal>())
        return visit(d);
    else if (auto d = s->as<AstStatDeclareFunction>())
        return visit(d);
    else if (auto d = s->as<AstStatDeclareExternType>())
        return visit(d);
    else if (auto error = s->as<AstStatError>())
        return visit(error);
    else
        handle->ice("Unknown AstStat in DataFlowGraphBuilder::visit");
}

ControlFlow DataFlowGraphBuilder::visit(AstStatIf* i)
{
    visitExpr(i->condition);

    DfgScope* thenScope = makeChildScope();
    DfgScope* elseScope = makeChildScope();

    ControlFlow thencf;
    {
        PushScope ps{scopeStack, thenScope};
        thencf = visit(i->thenbody);
    }

    ControlFlow elsecf = ControlFlow::None;
    if (i->elsebody)
    {
        PushScope ps{scopeStack, elseScope};
        elsecf = visit(i->elsebody);
    }

    DfgScope* scope = currentScope();
    // If the control flow from the `if` or `else` block is non-linear,
    // then we should assume that the _other_ branch is the one taken.
    if (thencf != ControlFlow::None && elsecf == ControlFlow::None)
        scope->inherit(elseScope);
    else if (thencf == ControlFlow::None && elsecf != ControlFlow::None)
        scope->inherit(thenScope);
    else if ((thencf | elsecf) == ControlFlow::None)
        join(scope, thenScope, elseScope);

    if (thencf == elsecf)
        return thencf;
    else if (matches(thencf, ControlFlow::Returns | ControlFlow::Throws) && matches(elsecf, ControlFlow::Returns | ControlFlow::Throws))
        return ControlFlow::Returns;
    else
        return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatWhile* w)
{
    // FIXME: This is unsound, as it does not consider the _second_ loop
    // iteration. Consider something like:
    //
    //   local function f(_: number) end
    //   local x = 42
    //   while math.random () > 0.5 do
    //       f(x)
    //       x = ""
    //   end
    //
    // While the first iteration is fine, the second iteration would
    // allow a string to flow into a position that expects.
    DfgScope* whileScope = makeChildScope(DfgScope::Loop);

    ControlFlow cf;
    {
        PushScope ps{scopeStack, whileScope};
        visitExpr(w->condition);
        cf = visit(w->body);
    }

    auto scope = currentScope();
    // If the inner loop unconditioanlly returns or throws we shouldn't
    // consume any type state from the loop body.
    if (!matches(cf, ControlFlow::Returns | ControlFlow::Throws))
        join(scope, scope, whileScope);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatRepeat* r)
{
    // See comment in visit(AstStatWhile*): this is unsound as it
    // does not consider the _second_ loop iteration.
    DfgScope* repeatScope = makeChildScope(DfgScope::Loop);

    ControlFlow cf;

    {
        PushScope ps{scopeStack, repeatScope};
        cf = visitBlockWithoutChildScope(r->body);
        visitExpr(r->condition);
    }

    // Ultimately: the options for a repeat-until loop are more
    // straightforward.
    currentScope()->inherit(repeatScope);

    // `repeat` loops will unconditionally fire: if the internal control
    // flow is unconditionally a break or continue, then we have linear
    // control flow, but if it's throws or returns, then we need to
    // return _that_ to the parent.
    return matches(cf, ControlFlow::Breaks | ControlFlow::Continues) ? ControlFlow::None : cf;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatBreak* b)
{
    return ControlFlow::Breaks;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatContinue* c)
{
    return ControlFlow::Continues;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatReturn* r)
{
    for (AstExpr* e : r->list)
        visitExpr(e);

    return ControlFlow::Returns;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatExpr* e)
{
    visitExpr(e->expr);
    if (auto call = e->expr->as<AstExprCall>(); call && doesCallError(call))
        return ControlFlow::Throws;
    else
        return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatLocal* l)
{
    // We're gonna need a `visitExprList` and `visitVariadicExpr` (function calls and `...`)
    std::vector<DefId> defs;
    defs.reserve(l->values.size);
    for (AstExpr* e : l->values)
        defs.push_back(visitExpr(e).def);

    for (size_t i = 0; i < l->vars.size; ++i)
    {
        AstLocal* local = l->vars.data[i];
        if (local->annotation)
            visitType(local->annotation);

        // We need to create a new def to intentionally avoid alias tracking, but we'd like to
        // make sure that the non-aliased defs are also marked as a subscript for refinements.
        bool subscripted = i < defs.size() && containsSubscriptedDefinition(defs[i]);
        DefId def = defArena->freshCell(local, local->location, subscripted);
        if (i < l->values.size)
        {
            AstExpr* e = l->values.data[i];
            if (const AstExprTable* tbl = e->as<AstExprTable>())
            {
                def = defs[i];
            }
        }
        graph.localDefs[local] = def;
        currentScope()->bindings[local] = def;
        captures[local].allVersions.push_back(def);
    }

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatFor* f)
{
    // See comment in visit(AstStatWhile*): this is unsound as it
    // does not consider the _second_ loop iteration.
    DfgScope* forScope = makeChildScope(DfgScope::Loop);

    visitExpr(f->from);
    visitExpr(f->to);
    if (f->step)
        visitExpr(f->step);

    ControlFlow cf;
    {
        PushScope ps{scopeStack, forScope};

        if (f->var->annotation)
            visitType(f->var->annotation);

        DefId def = defArena->freshCell(f->var, f->var->location);
        graph.localDefs[f->var] = def;
        currentScope()->bindings[f->var] = def;
        captures[f->var].allVersions.push_back(def);

        cf = visit(f->body);
    }

    auto scope = currentScope();
    // If the inner loop unconditioanlly returns or throws we shouldn't
    // consume any type state from the loop body.
    if (!matches(cf, ControlFlow::Returns | ControlFlow::Throws))
        join(scope, scope, forScope);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatForIn* f)
{
    DfgScope* forScope = makeChildScope(DfgScope::Loop);

    ControlFlow cf;
    {
        PushScope ps{scopeStack, forScope};

        for (AstLocal* local : f->vars)
        {
            if (local->annotation)
                visitType(local->annotation);

            DefId def = defArena->freshCell(local, local->location);
            graph.localDefs[local] = def;
            currentScope()->bindings[local] = def;
            captures[local].allVersions.push_back(def);
        }

        // TODO(controlflow): entry point has a back edge from exit point
        // We're gonna need a `visitExprList` and `visitVariadicExpr` (function calls and `...`)
        for (AstExpr* e : f->values)
            visitExpr(e);

        cf = visit(f->body);
    }

    auto scope = currentScope();
    // If the inner loop unconditioanlly returns or throws we shouldn't
    // consume any type state from the loop body.
    if (!matches(cf, ControlFlow::Returns | ControlFlow::Throws))
        join(scope, scope, forScope);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatAssign* a)
{
    std::vector<DefId> defs;
    defs.reserve(a->values.size);
    for (AstExpr* e : a->values)
        defs.push_back(visitExpr(e).def);

    for (size_t i = 0; i < a->vars.size; ++i)
    {
        AstExpr* v = a->vars.data[i];
        visitLValue(v, i < defs.size() ? defs[i] : defArena->freshCell(Symbol{}, v->location));
    }

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatCompoundAssign* c)
{
    (void)visitExpr(c->value);
    (void)visitExpr(c->var);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatFunction* f)
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
    visitLValue(f->name, defArena->freshCell(Symbol{}, f->name->location));
    visitExpr(f->func);

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

ControlFlow DataFlowGraphBuilder::visit(AstStatLocalFunction* l)
{
    DefId def = defArena->freshCell(l->name, l->location);
    graph.localDefs[l->name] = def;
    currentScope()->bindings[l->name] = def;
    captures[l->name].allVersions.push_back(def);
    visitExpr(l->func);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatTypeAlias* t)
{
    DfgScope* unreachable = makeChildScope();
    PushScope ps{scopeStack, unreachable};

    visitGenerics(t->generics);
    visitGenericPacks(t->genericPacks);
    visitType(t->type);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatTypeFunction* f)
{
    DfgScope* unreachable = makeChildScope();
    PushScope ps{scopeStack, unreachable};

    visitExpr(f->body);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatDeclareGlobal* d)
{
    DefId def = defArena->freshCell(d->name, d->nameLocation);
    graph.declaredDefs[d] = def;
    currentScope()->bindings[d->name] = def;
    captures[d->name].allVersions.push_back(def);

    visitType(d->type);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatDeclareFunction* d)
{
    DefId def = defArena->freshCell(d->name, d->nameLocation);
    graph.declaredDefs[d] = def;
    currentScope()->bindings[d->name] = def;
    captures[d->name].allVersions.push_back(def);

    DfgScope* unreachable = makeChildScope();
    PushScope ps{scopeStack, unreachable};

    visitGenerics(d->generics);
    visitGenericPacks(d->genericPacks);
    visitTypeList(d->params);
    visitTypePack(d->retTypes);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatDeclareExternType* d)
{
    // This declaration does not "introduce" any bindings in value namespace,
    // so there's no symbolic value to begin with. We'll traverse the properties
    // because their type annotations may depend on something in the value namespace.
    DfgScope* unreachable = makeChildScope();
    PushScope ps{scopeStack, unreachable};

    for (AstDeclaredExternTypeProperty prop : d->props)
        visitType(prop.ty);

    return ControlFlow::None;
}

ControlFlow DataFlowGraphBuilder::visit(AstStatError* error)
{
    DfgScope* unreachable = makeChildScope();
    PushScope ps{scopeStack, unreachable};

    for (AstStat* s : error->statements)
        visit(s);
    for (AstExpr* e : error->expressions)
        visitExpr(e);

    return ControlFlow::None;
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExpr* e)
{
    // Some subexpressions could be visited two times. If we've already seen it, just extract it.
    if (auto def = graph.astDefs.find(e))
    {
        auto key = graph.astRefinementKeys.find(e);
        return {NotNull{*def}, key ? *key : nullptr};
    }

    auto go = [&]() -> DataFlowResult
    {
        if (auto g = e->as<AstExprGroup>())
            return visitExpr(g);
        else if (auto c = e->as<AstExprConstantNil>())
            return {defArena->freshCell(Symbol{}, c->location), nullptr}; // ok
        else if (auto c = e->as<AstExprConstantBool>())
            return {defArena->freshCell(Symbol{}, c->location), nullptr}; // ok
        else if (auto c = e->as<AstExprConstantNumber>())
            return {defArena->freshCell(Symbol{}, c->location), nullptr}; // ok
        else if (auto c = e->as<AstExprConstantString>())
            return {defArena->freshCell(Symbol{}, c->location), nullptr}; // ok
        else if (auto l = e->as<AstExprLocal>())
            return visitExpr(l);
        else if (auto g = e->as<AstExprGlobal>())
            return visitExpr(g);
        else if (auto v = e->as<AstExprVarargs>())
            return {defArena->freshCell(Symbol{}, v->location), nullptr}; // ok
        else if (auto c = e->as<AstExprCall>())
            return visitExpr(c);
        else if (auto i = e->as<AstExprIndexName>())
            return visitExpr(i);
        else if (auto i = e->as<AstExprIndexExpr>())
            return visitExpr(i);
        else if (auto f = e->as<AstExprFunction>())
            return visitExpr(f);
        else if (auto t = e->as<AstExprTable>())
            return visitExpr(t);
        else if (auto u = e->as<AstExprUnary>())
            return visitExpr(u);
        else if (auto b = e->as<AstExprBinary>())
            return visitExpr(b);
        else if (auto t = e->as<AstExprTypeAssertion>())
            return visitExpr(t);
        else if (auto i = e->as<AstExprIfElse>())
            return visitExpr(i);
        else if (auto i = e->as<AstExprInterpString>())
            return visitExpr(i);
        else if (auto error = e->as<AstExprError>())
            return visitExpr(error);
        else
            handle->ice("Unknown AstExpr in DataFlowGraphBuilder::visitExpr");
    };

    auto [def, key] = go();
    graph.astDefs[e] = def;
    if (key)
        graph.astRefinementKeys[e] = key;
    return {def, key};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprGroup* group)
{
    return visitExpr(group->expr);
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprLocal* l)
{
    DefId def = lookup(l->local, l->local->location);
    const RefinementKey* key = keyArena->leaf(def);
    graph.defToSymbol[def] = l->local;
    return {def, key};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprGlobal* g)
{
    DefId def = lookup(g->name, g->location);
    graph.defToSymbol[def] = g->name;
    return {def, keyArena->leaf(def)};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprCall* c)
{
    visitExpr(c->func);

    for (AstExpr* arg : c->args)
        visitExpr(arg);

    if (shouldTypestateForFirstArgument(*c) && c->args.size > 1 && isLValue(*c->args.begin()))
    {
        AstExpr* firstArg = *c->args.begin();

        // this logic has to handle the name-like subset of expressions.
        std::optional<DataFlowResult> result;
        if (auto l = firstArg->as<AstExprLocal>())
            result = visitExpr(l);
        else if (auto g = firstArg->as<AstExprGlobal>())
            result = visitExpr(g);
        else if (auto i = firstArg->as<AstExprIndexName>())
            result = visitExpr(i);
        else if (auto i = firstArg->as<AstExprIndexExpr>())
            result = visitExpr(i);
        else
            LUAU_UNREACHABLE(); // This is unreachable because the whole thing is guarded by `isLValue`.

        LUAU_ASSERT(result);

        DfgScope* child = makeChildScope();
        scopeStack.push_back(child);

        auto [def, key] = *result;
        graph.astDefs[firstArg] = def;
        if (key)
            graph.astRefinementKeys[firstArg] = key;

        visitLValue(firstArg, def);
    }

    // We treat function calls as "subscripted" as they could potentially
    // return a subscripted value, consider:
    //
    //  local function foo(tbl: {[string]: woof)
    //      return tbl["foobarbaz"]
    //  end
    //
    //  local v = foo({})
    //
    // We want to consider `v` to be subscripted here.
    return {defArena->freshCell(Symbol{}, c->location, /*subscripted=*/true)};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprIndexName* i)
{
    auto [parentDef, parentKey] = visitExpr(i->expr);
    std::string index = i->index.value;

    DefId def = lookup(parentDef, index, i->location);
    return {def, keyArena->node(parentKey, def, index)};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprIndexExpr* i)
{
    auto [parentDef, parentKey] = visitExpr(i->expr);
    visitExpr(i->index);

    if (auto string = i->index->as<AstExprConstantString>())
    {
        std::string index{string->value.data, string->value.size};

        DefId def = lookup(parentDef, index, i->location);
        return {def, keyArena->node(parentKey, def, index)};
    }

    return {defArena->freshCell(Symbol{}, i->location, /* subscripted= */ true), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprFunction* f)
{
    DfgScope* signatureScope = makeChildScope(DfgScope::Function);
    PushScope ps{scopeStack, signatureScope};

    if (AstLocal* self = f->self)
    {
        // There's no syntax for `self` to have an annotation if using `function t:m()`
        LUAU_ASSERT(!self->annotation);

        DefId def = defArena->freshCell(f->debugname, f->location);
        graph.localDefs[self] = def;
        signatureScope->bindings[self] = def;
        captures[self].allVersions.push_back(def);
    }

    for (AstLocal* param : f->args)
    {
        if (param->annotation)
            visitType(param->annotation);

        DefId def = defArena->freshCell(param, param->location);
        graph.localDefs[param] = def;
        signatureScope->bindings[param] = def;
        captures[param].allVersions.push_back(def);
    }

    if (f->varargAnnotation)
        visitTypePack(f->varargAnnotation);

    if (f->returnAnnotation)
        visitTypePack(f->returnAnnotation);

    // TODO: function body can be re-entrant, as in mutations that occurs at the end of the function can also be
    // visible to the beginning of the function, so statically speaking, the body of the function has an exit point
    // that points back to itself, e.g.
    //
    // local function f() print(f) f = 5 end
    // local g = f
    // g() --> function: address
    // g() --> 5
    visit(f->body);

    return {defArena->freshCell(f->debugname, f->location), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprTable* t)
{
    DefId tableCell = defArena->freshCell(Symbol{}, t->location);
    currentScope()->props[tableCell] = {};
    for (AstExprTable::Item item : t->items)
    {
        DataFlowResult result = visitExpr(item.value);
        if (item.key)
        {
            visitExpr(item.key);
            if (auto string = item.key->as<AstExprConstantString>())
            {
                currentScope()->props[tableCell][string->value.data] = result.def;
            }
        }
    }

    return {tableCell, nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprUnary* u)
{
    visitExpr(u->expr);

    return {defArena->freshCell(Symbol{}, u->location), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprBinary* b)
{
    auto left = visitExpr(b->left);
    auto right = visitExpr(b->right);
    // I think there's some subtlety here. There are probably cases where
    // X or Y / X and Y can _never_ "be subscripted."
    auto subscripted = (b->op == AstExprBinary::And || b->op == AstExprBinary::Or) &&
                       (containsSubscriptedDefinition(left.def) || containsSubscriptedDefinition(right.def));
    return {defArena->freshCell(Symbol{}, b->location, subscripted), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprTypeAssertion* t)
{
    auto [def, key] = visitExpr(t->expr);
    visitType(t->annotation);

    return {def, key};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprIfElse* i)
{
    visitExpr(i->condition);
    visitExpr(i->trueExpr);
    visitExpr(i->falseExpr);

    return {defArena->freshCell(Symbol{}, i->location), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprInterpString* i)
{
    for (AstExpr* e : i->expressions)
        visitExpr(e);

    return {defArena->freshCell(Symbol{}, i->location), nullptr};
}

DataFlowResult DataFlowGraphBuilder::visitExpr(AstExprError* error)
{
    DfgScope* unreachable = makeChildScope();
    PushScope ps{scopeStack, unreachable};

    for (AstExpr* e : error->expressions)
        visitExpr(e);

    return {defArena->freshCell(Symbol{}, error->location), nullptr};
}

void DataFlowGraphBuilder::visitLValue(AstExpr* e, DefId incomingDef)
{
    auto go = [&]()
    {
        if (auto l = e->as<AstExprLocal>())
            return visitLValue(l, incomingDef);
        else if (auto g = e->as<AstExprGlobal>())
            return visitLValue(g, incomingDef);
        else if (auto i = e->as<AstExprIndexName>())
            return visitLValue(i, incomingDef);
        else if (auto i = e->as<AstExprIndexExpr>())
            return visitLValue(i, incomingDef);
        else if (auto error = e->as<AstExprError>())
            return visitLValue(error, incomingDef);
        else
            handle->ice("Unknown AstExpr in DataFlowGraphBuilder::visitLValue");
    };

    graph.astDefs[e] = go();
}

DefId DataFlowGraphBuilder::visitLValue(AstExprLocal* l, DefId incomingDef)
{
    DfgScope* scope = currentScope();

    // In order to avoid alias tracking, we need to clip the reference to the parent def.
    if (!l->upvalue)
    {
        DefId updated = defArena->freshCell(l->local, l->location, containsSubscriptedDefinition(incomingDef));
        scope->bindings[l->local] = updated;
        captures[l->local].allVersions.push_back(updated);
        return updated;
    }
    else
        return visitExpr(static_cast<AstExpr*>(l)).def;
}

DefId DataFlowGraphBuilder::visitLValue(AstExprGlobal* g, DefId incomingDef)
{
    DfgScope* scope = currentScope();

    DefId updated = defArena->freshCell(g->name, g->location, containsSubscriptedDefinition(incomingDef));
    scope->bindings[g->name] = updated;
    captures[g->name].allVersions.push_back(updated);
    return updated;
}

DefId DataFlowGraphBuilder::visitLValue(AstExprIndexName* i, DefId incomingDef)
{
    DefId parentDef = visitExpr(i->expr).def;

    DfgScope* scope = currentScope();
    DefId updated = defArena->freshCell(i->index, i->location, containsSubscriptedDefinition(incomingDef));
    scope->props[parentDef][i->index.value] = updated;
    return updated;
}

DefId DataFlowGraphBuilder::visitLValue(AstExprIndexExpr* i, DefId incomingDef)
{
    DefId parentDef = visitExpr(i->expr).def;
    visitExpr(i->index);

    DfgScope* scope = currentScope();
    if (auto string = i->index->as<AstExprConstantString>())
    {
        DefId updated = defArena->freshCell(Symbol{}, i->location, containsSubscriptedDefinition(incomingDef));
        scope->props[parentDef][string->value.data] = updated;
        return updated;
    }
    else
        return defArena->freshCell(Symbol{}, i->location, /*subscripted=*/true);
}

DefId DataFlowGraphBuilder::visitLValue(AstExprError* error, DefId incomingDef)
{
    return visitExpr(error).def;
}

void DataFlowGraphBuilder::visitType(AstType* t)
{
    if (auto r = t->as<AstTypeReference>())
        return visitType(r);
    else if (auto table = t->as<AstTypeTable>())
        return visitType(table);
    else if (auto f = t->as<AstTypeFunction>())
        return visitType(f);
    else if (auto tyof = t->as<AstTypeTypeof>())
        return visitType(tyof);
    else if (auto o = t->as<AstTypeOptional>())
        return;
    else if (auto u = t->as<AstTypeUnion>())
        return visitType(u);
    else if (auto i = t->as<AstTypeIntersection>())
        return visitType(i);
    else if (auto e = t->as<AstTypeError>())
        return visitType(e);
    else if (auto s = t->as<AstTypeSingletonBool>())
        return; // ok
    else if (auto s = t->as<AstTypeSingletonString>())
        return; // ok
    else if (auto g = t->as<AstTypeGroup>())
        return visitType(g->type);
    else
        handle->ice("Unknown AstType in DataFlowGraphBuilder::visitType");
}

void DataFlowGraphBuilder::visitType(AstTypeReference* r)
{
    for (AstTypeOrPack param : r->parameters)
    {
        if (param.type)
            visitType(param.type);
        else
            visitTypePack(param.typePack);
    }
}

void DataFlowGraphBuilder::visitType(AstTypeTable* t)
{
    for (AstTableProp p : t->props)
        visitType(p.type);

    if (t->indexer)
    {
        visitType(t->indexer->indexType);
        visitType(t->indexer->resultType);
    }
}

void DataFlowGraphBuilder::visitType(AstTypeFunction* f)
{
    visitGenerics(f->generics);
    visitGenericPacks(f->genericPacks);
    visitTypeList(f->argTypes);
    visitTypePack(f->returnTypes);
}

void DataFlowGraphBuilder::visitType(AstTypeTypeof* t)
{
    visitExpr(t->expr);
}

void DataFlowGraphBuilder::visitType(AstTypeUnion* u)
{
    for (AstType* t : u->types)
        visitType(t);
}

void DataFlowGraphBuilder::visitType(AstTypeIntersection* i)
{
    for (AstType* t : i->types)
        visitType(t);
}

void DataFlowGraphBuilder::visitType(AstTypeError* error)
{
    for (AstType* t : error->types)
        visitType(t);
}

void DataFlowGraphBuilder::visitTypePack(AstTypePack* p)
{
    if (auto e = p->as<AstTypePackExplicit>())
        return visitTypePack(e);
    else if (auto v = p->as<AstTypePackVariadic>())
        return visitTypePack(v);
    else if (auto g = p->as<AstTypePackGeneric>())
        return; // ok
    else
        handle->ice("Unknown AstTypePack in DataFlowGraphBuilder::visitTypePack");
}

void DataFlowGraphBuilder::visitTypePack(AstTypePackExplicit* e)
{
    visitTypeList(e->typeList);
}

void DataFlowGraphBuilder::visitTypePack(AstTypePackVariadic* v)
{
    visitType(v->variadicType);
}

void DataFlowGraphBuilder::visitTypeList(AstTypeList l)
{
    for (AstType* t : l.types)
        visitType(t);

    if (l.tailType)
        visitTypePack(l.tailType);
}

void DataFlowGraphBuilder::visitGenerics(AstArray<AstGenericType*> g)
{
    for (AstGenericType* generic : g)
    {
        if (generic->defaultValue)
            visitType(generic->defaultValue);
    }
}

void DataFlowGraphBuilder::visitGenericPacks(AstArray<AstGenericTypePack*> g)
{
    for (AstGenericTypePack* generic : g)
    {
        if (generic->defaultValue)
            visitTypePack(generic->defaultValue);
    }
}

} // namespace Luau
