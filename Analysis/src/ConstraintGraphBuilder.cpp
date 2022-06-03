// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/ConstraintGraphBuilder.h"

namespace Luau
{

Constraint::Constraint(ConstraintV&& c)
    : c(std::move(c))
{
}

Constraint::Constraint(ConstraintV&& c, std::vector<Constraint*> dependencies)
    : c(std::move(c))
    , dependencies(dependencies)
{
}

std::optional<TypeId> Scope2::lookup(Symbol sym)
{
    Scope2* s = this;

    while (true)
    {
        auto it = s->bindings.find(sym);
        if (it != s->bindings.end())
            return it->second;

        if (s->parent)
            s = s->parent;
        else
            return std::nullopt;
    }
}

ConstraintGraphBuilder::ConstraintGraphBuilder(TypeArena* arena)
    : singletonTypes(getSingletonTypes())
    , arena(arena)
    , rootScope(nullptr)
{
    LUAU_ASSERT(arena);
}

TypeId ConstraintGraphBuilder::freshType(Scope2* scope)
{
    LUAU_ASSERT(scope);
    return arena->addType(FreeTypeVar{scope});
}

TypePackId ConstraintGraphBuilder::freshTypePack(Scope2* scope)
{
    LUAU_ASSERT(scope);
    FreeTypePack f{scope};
    return arena->addTypePack(TypePackVar{std::move(f)});
}

Scope2* ConstraintGraphBuilder::childScope(Location location, Scope2* parent)
{
    LUAU_ASSERT(parent);
    auto scope = std::make_unique<Scope2>();
    Scope2* borrow = scope.get();
    scopes.emplace_back(location, std::move(scope));

    borrow->parent = parent;
    borrow->returnType = parent->returnType;
    parent->children.push_back(borrow);

    return borrow;
}

void ConstraintGraphBuilder::addConstraint(Scope2* scope, ConstraintV cv)
{
    LUAU_ASSERT(scope);
    scope->constraints.emplace_back(new Constraint{std::move(cv)});
}

void ConstraintGraphBuilder::addConstraint(Scope2* scope, std::unique_ptr<Constraint> c)
{
    LUAU_ASSERT(scope);
    scope->constraints.emplace_back(std::move(c));
}

void ConstraintGraphBuilder::visit(AstStatBlock* block)
{
    LUAU_ASSERT(scopes.empty());
    LUAU_ASSERT(rootScope == nullptr);
    scopes.emplace_back(block->location, std::make_unique<Scope2>());
    rootScope = scopes.back().second.get();
    rootScope->returnType = freshTypePack(rootScope);

    visit(rootScope, block);
}

void ConstraintGraphBuilder::visit(Scope2* scope, AstStat* stat)
{
    LUAU_ASSERT(scope);

    if (auto s = stat->as<AstStatBlock>())
        visit(scope, s);
    else if (auto s = stat->as<AstStatLocal>())
        visit(scope, s);
    else if (auto f = stat->as<AstStatLocalFunction>())
        visit(scope, f);
    else if (auto r = stat->as<AstStatReturn>())
        visit(scope, r);
    else
        LUAU_ASSERT(0);
}

void ConstraintGraphBuilder::visit(Scope2* scope, AstStatLocal* local)
{
    LUAU_ASSERT(scope);

    std::vector<TypeId> varTypes;

    for (AstLocal* local : local->vars)
    {
        // TODO annotations
        TypeId ty = freshType(scope);
        varTypes.push_back(ty);
        scope->bindings[local] = ty;
    }

    for (size_t i = 0; i < local->vars.size; ++i)
    {
        if (i < local->values.size)
        {
            TypeId exprType = check(scope, local->values.data[i]);
            addConstraint(scope, SubtypeConstraint{varTypes[i], exprType});
        }
    }
}

void addConstraints(Constraint* constraint, Scope2* scope)
{
    LUAU_ASSERT(scope);

    scope->constraints.reserve(scope->constraints.size() + scope->constraints.size());

    for (const auto& c : scope->constraints)
        constraint->dependencies.push_back(c.get());

    for (Scope2* childScope : scope->children)
        addConstraints(constraint, childScope);
}

void ConstraintGraphBuilder::visit(Scope2* scope, AstStatLocalFunction* function)
{
    LUAU_ASSERT(scope);

    // Local
    // Global
    // Dotted path
    // Self?

    TypeId functionType = nullptr;
    auto ty = scope->lookup(function->name);
    LUAU_ASSERT(!ty.has_value()); // The parser ensures that every local function has a distinct Symbol for its name.

    functionType = freshType(scope);
    scope->bindings[function->name] = functionType;

    Scope2* innerScope = childScope(function->func->body->location, scope);
    TypePackId returnType = freshTypePack(scope);
    innerScope->returnType = returnType;

    std::vector<TypeId> argTypes;

    for (AstLocal* local : function->func->args)
    {
        TypeId t = freshType(innerScope);
        argTypes.push_back(t);
        innerScope->bindings[local] = t; // TODO annotations
    }

    for (AstStat* stat : function->func->body->body)
        visit(innerScope, stat);

    FunctionTypeVar actualFunction{arena->addTypePack(argTypes), returnType};
    TypeId actualFunctionType = arena->addType(std::move(actualFunction));

    std::unique_ptr<Constraint> c{new Constraint{GeneralizationConstraint{functionType, actualFunctionType, innerScope}}};
    addConstraints(c.get(), innerScope);

    addConstraint(scope, std::move(c));
}

void ConstraintGraphBuilder::visit(Scope2* scope, AstStatReturn* ret)
{
    LUAU_ASSERT(scope);

    TypePackId exprTypes = checkPack(scope, ret->list);
    addConstraint(scope, PackSubtypeConstraint{exprTypes, scope->returnType});
}

void ConstraintGraphBuilder::visit(Scope2* scope, AstStatBlock* block)
{
    LUAU_ASSERT(scope);

    for (AstStat* stat : block->body)
        visit(scope, stat);
}

TypePackId ConstraintGraphBuilder::checkPack(Scope2* scope, AstArray<AstExpr*> exprs)
{
    LUAU_ASSERT(scope);

    if (exprs.size == 0)
        return arena->addTypePack({});

    std::vector<TypeId> types;
    TypePackId last = nullptr;

    for (size_t i = 0; i < exprs.size; ++i)
    {
        if (i < exprs.size - 1)
            types.push_back(check(scope, exprs.data[i]));
        else
            last = checkPack(scope, exprs.data[i]);
    }

    LUAU_ASSERT(last != nullptr);

    return arena->addTypePack(TypePack{std::move(types), last});
}

TypePackId ConstraintGraphBuilder::checkPack(Scope2* scope, AstExpr* expr)
{
    LUAU_ASSERT(scope);

    // TEMP TEMP TEMP HACK HACK HACK FIXME FIXME
    TypeId t = check(scope, expr);
    return arena->addTypePack({t});
}

TypeId ConstraintGraphBuilder::check(Scope2* scope, AstExpr* expr)
{
    LUAU_ASSERT(scope);

    if (auto a = expr->as<AstExprConstantString>())
        return singletonTypes.stringType;
    else if (auto a = expr->as<AstExprConstantNumber>())
        return singletonTypes.numberType;
    else if (auto a = expr->as<AstExprConstantBool>())
        return singletonTypes.booleanType;
    else if (auto a = expr->as<AstExprConstantNil>())
        return singletonTypes.nilType;
    else if (auto a = expr->as<AstExprLocal>())
    {
        std::optional<TypeId> ty = scope->lookup(a->local);
        if (ty)
            return *ty;
        else
            return singletonTypes.errorRecoveryType(singletonTypes.anyType); // FIXME?  Record an error at this point?
    }
    else if (auto a = expr->as<AstExprCall>())
    {
        std::vector<TypeId> args;

        for (AstExpr* arg : a->args)
        {
            args.push_back(check(scope, arg));
        }

        TypeId fnType = check(scope, a->func);
        TypeId instantiatedType = freshType(scope);
        addConstraint(scope, InstantiationConstraint{instantiatedType, fnType});

        TypeId firstRet = freshType(scope);
        TypePackId rets = arena->addTypePack(TypePack{{firstRet}, arena->addTypePack(TypePackVar{FreeTypePack{TypeLevel{}}})});
        FunctionTypeVar ftv(arena->addTypePack(TypePack{args, {}}), rets);
        TypeId inferredFnType = arena->addType(ftv);

        addConstraint(scope, SubtypeConstraint{inferredFnType, instantiatedType});
        return firstRet;
    }
    else
    {
        LUAU_ASSERT(0);
        return freshType(scope);
    }
}

static void collectConstraints(std::vector<const Constraint*>& result, Scope2* scope)
{
    for (const auto& c : scope->constraints)
        result.push_back(c.get());

    for (Scope2* child : scope->children)
        collectConstraints(result, child);
}

std::vector<const Constraint*> collectConstraints(Scope2* rootScope)
{
    std::vector<const Constraint*> result;
    collectConstraints(result, rootScope);
    return result;
}

} // namespace Luau
