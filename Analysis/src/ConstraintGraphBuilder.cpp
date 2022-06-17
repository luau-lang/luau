// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/ConstraintGraphBuilder.h"

namespace Luau
{

const AstStat* getFallthrough(const AstStat* node); // TypeInfer.cpp

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

void ConstraintGraphBuilder::addConstraint(Scope2* scope, ConstraintV cv, Location location)
{
    LUAU_ASSERT(scope);
    scope->constraints.emplace_back(new Constraint{std::move(cv), location});
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
    else if (auto f = stat->as<AstStatFunction>())
        visit(scope, f);
    else if (auto f = stat->as<AstStatLocalFunction>())
        visit(scope, f);
    else if (auto r = stat->as<AstStatReturn>())
        visit(scope, r);
    else if (auto a = stat->as<AstStatAssign>())
        visit(scope, a);
    else if (auto e = stat->as<AstStatExpr>())
        checkPack(scope, e->expr);
    else if (auto i = stat->as<AstStatIf>())
        visit(scope, i);
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

    for (size_t i = 0; i < local->values.size; ++i)
    {
        if (local->values.data[i]->is<AstExprConstantNil>())
        {
            // HACK: we leave nil-initialized things floating under the assumption that they will later be populated.
            // See the test TypeInfer/infer_locals_with_nil_value.
            // Better flow awareness should make this obsolete.
        }
        else if (i == local->values.size - 1)
        {
            TypePackId exprPack = checkPack(scope, local->values.data[i]);

            if (i < local->vars.size)
            {
                std::vector<TypeId> tailValues{varTypes.begin() + i, varTypes.end()};
                TypePackId tailPack = arena->addTypePack(std::move(tailValues));
                addConstraint(scope, PackSubtypeConstraint{exprPack, tailPack}, local->location);
            }
        }
        else
        {
            TypeId exprType = check(scope, local->values.data[i]);
            if (i < varTypes.size())
                addConstraint(scope, SubtypeConstraint{varTypes[i], exprType}, local->vars.data[i]->location);
        }
    }
}

void addConstraints(Constraint* constraint, Scope2* scope)
{
    LUAU_ASSERT(scope);

    scope->constraints.reserve(scope->constraints.size() + scope->constraints.size());

    for (const auto& c : scope->constraints)
        constraint->dependencies.push_back(NotNull{c.get()});

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
    if (ty.has_value())
    {
        // TODO: This is duplicate definition of a local function.  Is this allowed?
        functionType = *ty;
    }
    else
    {
        functionType = arena->addType(BlockedTypeVar{});
        scope->bindings[function->name] = functionType;
    }

    auto [actualFunctionType, innerScope] = checkFunctionSignature(scope, function->func);
    innerScope->bindings[function->name] = actualFunctionType;

    checkFunctionBody(innerScope, function->func);

    std::unique_ptr<Constraint> c{new Constraint{GeneralizationConstraint{functionType, actualFunctionType, innerScope}, function->location}};
    addConstraints(c.get(), innerScope);

    addConstraint(scope, std::move(c));
}

void ConstraintGraphBuilder::visit(Scope2* scope, AstStatFunction* function)
{
    // Name could be AstStatLocal, AstStatGlobal, AstStatIndexName.
    // With or without self

    TypeId functionType = nullptr;

    auto [actualFunctionType, innerScope] = checkFunctionSignature(scope, function->func);

    if (AstExprLocal* localName = function->name->as<AstExprLocal>())
    {
        std::optional<TypeId> existingFunctionTy = scope->lookup(localName->local);
        if (existingFunctionTy)
        {
            // Duplicate definition
            functionType = *existingFunctionTy;
        }
        else
        {
            functionType = arena->addType(BlockedTypeVar{});
            scope->bindings[localName->local] = functionType;
        }
        innerScope->bindings[localName->local] = actualFunctionType;
    }
    else if (AstExprGlobal* globalName = function->name->as<AstExprGlobal>())
    {
        std::optional<TypeId> existingFunctionTy = scope->lookup(globalName->name);
        if (existingFunctionTy)
        {
            // Duplicate definition
            functionType = *existingFunctionTy;
        }
        else
        {
            functionType = arena->addType(BlockedTypeVar{});
            rootScope->bindings[globalName->name] = functionType;
        }
        innerScope->bindings[globalName->name] = actualFunctionType;
    }
    else if (AstExprIndexName* indexName = function->name->as<AstExprIndexName>())
    {
        LUAU_ASSERT(0); // not yet implemented
    }

    checkFunctionBody(innerScope, function->func);

    std::unique_ptr<Constraint> c{new Constraint{GeneralizationConstraint{functionType, actualFunctionType, innerScope}, function->location}};
    addConstraints(c.get(), innerScope);

    addConstraint(scope, std::move(c));
}

void ConstraintGraphBuilder::visit(Scope2* scope, AstStatReturn* ret)
{
    LUAU_ASSERT(scope);

    TypePackId exprTypes = checkPack(scope, ret->list);
    addConstraint(scope, PackSubtypeConstraint{exprTypes, scope->returnType}, ret->location);
}

void ConstraintGraphBuilder::visit(Scope2* scope, AstStatBlock* block)
{
    LUAU_ASSERT(scope);

    for (AstStat* stat : block->body)
        visit(scope, stat);
}

void ConstraintGraphBuilder::visit(Scope2* scope, AstStatAssign* assign)
{
    TypePackId varPackId = checkExprList(scope, assign->vars);
    TypePackId valuePack = checkPack(scope, assign->values);

    addConstraint(scope, PackSubtypeConstraint{valuePack, varPackId}, assign->location);
}

void ConstraintGraphBuilder::visit(Scope2* scope, AstStatIf* ifStatement)
{
    check(scope, ifStatement->condition);

    Scope2* thenScope = childScope(ifStatement->thenbody->location, scope);
    visit(thenScope, ifStatement->thenbody);

    if (ifStatement->elsebody)
    {
        Scope2* elseScope = childScope(ifStatement->elsebody->location, scope);
        visit(elseScope, ifStatement->elsebody);
    }
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

TypePackId ConstraintGraphBuilder::checkExprList(Scope2* scope, const AstArray<AstExpr*>& exprs)
{
    TypePackId result = arena->addTypePack({});
    TypePack* resultPack = getMutable<TypePack>(result);
    LUAU_ASSERT(resultPack);

    for (size_t i = 0; i < exprs.size; ++i)
    {
        AstExpr* expr = exprs.data[i];
        if (i < exprs.size - 1)
            resultPack->head.push_back(check(scope, expr));
        else
            resultPack->tail = checkPack(scope, expr);
    }

    if (resultPack->head.empty() && resultPack->tail)
        return *resultPack->tail;
    else
        return result;
}

TypePackId ConstraintGraphBuilder::checkPack(Scope2* scope, AstExpr* expr)
{
    LUAU_ASSERT(scope);

    TypePackId result = nullptr;

    if (AstExprCall* call = expr->as<AstExprCall>())
    {
        std::vector<TypeId> args;

        for (AstExpr* arg : call->args)
        {
            args.push_back(check(scope, arg));
        }

        // TODO self

        TypeId fnType = check(scope, call->func);

        astOriginalCallTypes[call->func] = fnType;

        TypeId instantiatedType = freshType(scope);
        addConstraint(scope, InstantiationConstraint{instantiatedType, fnType}, expr->location);

        TypePackId rets = freshTypePack(scope);
        FunctionTypeVar ftv(arena->addTypePack(TypePack{args, {}}), rets);
        TypeId inferredFnType = arena->addType(ftv);

        addConstraint(scope, SubtypeConstraint{inferredFnType, instantiatedType}, expr->location);
        result = rets;
    }
    else
    {
        TypeId t = check(scope, expr);
        result = arena->addTypePack({t});
    }

    LUAU_ASSERT(result);
    astTypePacks[expr] = result;
    return result;
}

TypeId ConstraintGraphBuilder::check(Scope2* scope, AstExpr* expr)
{
    LUAU_ASSERT(scope);

    TypeId result = nullptr;

    if (auto group = expr->as<AstExprGroup>())
        result = check(scope, group->expr);
    else if (expr->is<AstExprConstantString>())
        result = singletonTypes.stringType;
    else if (expr->is<AstExprConstantNumber>())
        result = singletonTypes.numberType;
    else if (expr->is<AstExprConstantBool>())
        result = singletonTypes.booleanType;
    else if (expr->is<AstExprConstantNil>())
        result = singletonTypes.nilType;
    else if (auto a = expr->as<AstExprLocal>())
    {
        std::optional<TypeId> ty = scope->lookup(a->local);
        if (ty)
            result = *ty;
        else
            result = singletonTypes.errorRecoveryType(); // FIXME?  Record an error at this point?
    }
    else if (auto g = expr->as<AstExprGlobal>())
    {
        std::optional<TypeId> ty = scope->lookup(g->name);
        if (ty)
            result = *ty;
        else
            result = singletonTypes.errorRecoveryType(); // FIXME?  Record an error at this point?
    }
    else if (auto a = expr->as<AstExprCall>())
    {
        TypePackId packResult = checkPack(scope, expr);
        if (auto f = first(packResult))
            return *f;
        else if (get<FreeTypePack>(packResult))
        {
            TypeId typeResult = freshType(scope);
            TypePack onePack{{typeResult}, freshTypePack(scope)};
            TypePackId oneTypePack = arena->addTypePack(std::move(onePack));

            addConstraint(scope, PackSubtypeConstraint{packResult, oneTypePack}, expr->location);

            return typeResult;
        }
    }
    else if (auto a = expr->as<AstExprFunction>())
    {
        auto [fnType, functionScope] = checkFunctionSignature(scope, a);
        checkFunctionBody(functionScope, a);
        return fnType;
    }
    else if (auto indexName = expr->as<AstExprIndexName>())
    {
        result = check(scope, indexName);
    }
    else if (auto table = expr->as<AstExprTable>())
    {
        result = checkExprTable(scope, table);
    }
    else
    {
        LUAU_ASSERT(0);
        result = freshType(scope);
    }

    LUAU_ASSERT(result);
    astTypes[expr] = result;
    return result;
}

TypeId ConstraintGraphBuilder::check(Scope2* scope, AstExprIndexName* indexName)
{
    TypeId obj = check(scope, indexName->expr);
    TypeId result = freshType(scope);

    TableTypeVar::Props props{{indexName->index.value, Property{result}}};
    const std::optional<TableIndexer> indexer;
    TableTypeVar ttv{std::move(props), indexer, TypeLevel{}, TableState::Free};

    TypeId expectedTableType = arena->addType(std::move(ttv));

    addConstraint(scope, SubtypeConstraint{obj, expectedTableType}, indexName->location);

    return result;
}

TypeId ConstraintGraphBuilder::checkExprTable(Scope2* scope, AstExprTable* expr)
{
    TypeId ty = arena->addType(TableTypeVar{});
    TableTypeVar* ttv = getMutable<TableTypeVar>(ty);
    LUAU_ASSERT(ttv);

    auto createIndexer = [this, scope, ttv](
                             TypeId currentIndexType, TypeId currentResultType, Location itemLocation, std::optional<Location> keyLocation) {
        if (!ttv->indexer)
        {
            TypeId indexType = this->freshType(scope);
            TypeId resultType = this->freshType(scope);
            ttv->indexer = TableIndexer{indexType, resultType};
        }

        addConstraint(scope, SubtypeConstraint{ttv->indexer->indexType, currentIndexType}, keyLocation ? *keyLocation : itemLocation);
        addConstraint(scope, SubtypeConstraint{ttv->indexer->indexResultType, currentResultType}, itemLocation);
    };

    for (const AstExprTable::Item& item : expr->items)
    {
        TypeId itemTy = check(scope, item.value);

        if (item.key)
        {
            // Even though we don't need to use the type of the item's key if
            // it's a string constant, we still want to check it to populate
            // astTypes.
            TypeId keyTy = check(scope, item.key);

            if (AstExprConstantString* key = item.key->as<AstExprConstantString>())
            {
                ttv->props[key->value.begin()] = {itemTy};
            }
            else
            {
                createIndexer(keyTy, itemTy, item.value->location, item.key->location);
            }
        }
        else
        {
            TypeId numberType = singletonTypes.numberType;
            createIndexer(numberType, itemTy, item.value->location, std::nullopt);
        }
    }

    return ty;
}

std::pair<TypeId, Scope2*> ConstraintGraphBuilder::checkFunctionSignature(Scope2* parent, AstExprFunction* fn)
{
    Scope2* innerScope = childScope(fn->body->location, parent);
    TypePackId returnType = freshTypePack(innerScope);
    innerScope->returnType = returnType;

    std::vector<TypeId> argTypes;

    for (AstLocal* local : fn->args)
    {
        TypeId t = freshType(innerScope);
        argTypes.push_back(t);
        innerScope->bindings[local] = t; // TODO annotations
    }

    FunctionTypeVar actualFunction{arena->addTypePack(argTypes), returnType};
    TypeId actualFunctionType = arena->addType(std::move(actualFunction));
    LUAU_ASSERT(actualFunctionType);
    astTypes[fn] = actualFunctionType;

    return {actualFunctionType, innerScope};
}

void ConstraintGraphBuilder::checkFunctionBody(Scope2* scope, AstExprFunction* fn)
{
    for (AstStat* stat : fn->body->body)
        visit(scope, stat);

    // If it is possible for execution to reach the end of the function, the return type must be compatible with ()

    if (nullptr != getFallthrough(fn->body))
    {
        TypePackId empty = arena->addTypePack({}); // TODO we could have CSG retain one of these forever
        addConstraint(scope, PackSubtypeConstraint{scope->returnType, empty}, fn->body->location);
    }
}

void collectConstraints(std::vector<NotNull<Constraint>>& result, Scope2* scope)
{
    for (const auto& c : scope->constraints)
        result.push_back(NotNull{c.get()});

    for (Scope2* child : scope->children)
        collectConstraints(result, child);
}

std::vector<NotNull<Constraint>> collectConstraints(Scope2* rootScope)
{
    std::vector<NotNull<Constraint>> result;
    collectConstraints(result, rootScope);
    return result;
}

} // namespace Luau
