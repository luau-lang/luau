// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/ConstraintGraphBuilder.h"

#include "Luau/Scope.h"

namespace Luau
{

const AstStat* getFallthrough(const AstStat* node); // TypeInfer.cpp

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

    // TODO: We should share the global scope.
    rootScope->typeBindings["nil"] = singletonTypes.nilType;
    rootScope->typeBindings["number"] = singletonTypes.numberType;
    rootScope->typeBindings["string"] = singletonTypes.stringType;
    rootScope->typeBindings["boolean"] = singletonTypes.booleanType;
    rootScope->typeBindings["thread"] = singletonTypes.threadType;

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
    else if (auto a = stat->as<AstStatTypeAlias>())
        visit(scope, a);
    else
        LUAU_ASSERT(0);
}

void ConstraintGraphBuilder::visit(Scope2* scope, AstStatLocal* local)
{
    LUAU_ASSERT(scope);

    std::vector<TypeId> varTypes;

    for (AstLocal* local : local->vars)
    {
        TypeId ty = freshType(scope);

        if (local->annotation)
        {
            TypeId annotation = resolveType(scope, local->annotation);
            addConstraint(scope, SubtypeConstraint{ty, annotation});
        }

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
                addConstraint(scope, PackSubtypeConstraint{exprPack, tailPack});
            }
        }
        else
        {
            TypeId exprType = check(scope, local->values.data[i]);
            if (i < varTypes.size())
                addConstraint(scope, SubtypeConstraint{varTypes[i], exprType});
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

    std::unique_ptr<Constraint> c{new Constraint{GeneralizationConstraint{functionType, actualFunctionType, innerScope}}};
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

    // In order to enable mutually-recursive type aliases, we need to
    // populate the type bindings before we actually check any of the
    // alias statements. Since we're not ready to actually resolve
    // any of the annotations, we just use a fresh type for now.
    for (AstStat* stat : block->body)
    {
        if (auto alias = stat->as<AstStatTypeAlias>())
        {
            TypeId initialType = freshType(scope);
            scope->typeBindings[alias->name.value] = initialType;
        }
    }

    for (AstStat* stat : block->body)
        visit(scope, stat);
}

void ConstraintGraphBuilder::visit(Scope2* scope, AstStatAssign* assign)
{
    TypePackId varPackId = checkExprList(scope, assign->vars);
    TypePackId valuePack = checkPack(scope, assign->values);

    addConstraint(scope, PackSubtypeConstraint{valuePack, varPackId});
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

void ConstraintGraphBuilder::visit(Scope2* scope, AstStatTypeAlias* alias)
{
    // TODO: Exported type aliases
    // TODO: Generic type aliases

    auto it = scope->typeBindings.find(alias->name.value);
    // This should always be here since we do a separate pass over the
    // AST to set up typeBindings. If it's not, we've somehow skipped
    // this alias in that first pass.
    LUAU_ASSERT(it != scope->typeBindings.end());

    TypeId ty = resolveType(scope, alias->type);

    // Rather than using a subtype constraint, we instead directly bind
    // the free type we generated in the first pass to the resolved type.
    // This prevents a case where you could cause another constraint to
    // bind the free alias type to an unrelated type, causing havoc.
    asMutable(it->second)->ty.emplace<BoundTypeVar>(ty);

    addConstraint(scope, NameConstraint{ty, alias->name.value});
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
        addConstraint(scope, InstantiationConstraint{instantiatedType, fnType});

        TypePackId rets = freshTypePack(scope);
        FunctionTypeVar ftv(arena->addTypePack(TypePack{args, {}}), rets);
        TypeId inferredFnType = arena->addType(ftv);

        addConstraint(scope, SubtypeConstraint{inferredFnType, instantiatedType});
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

            addConstraint(scope, PackSubtypeConstraint{packResult, oneTypePack});

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

    addConstraint(scope, SubtypeConstraint{obj, expectedTableType});

    return result;
}

TypeId ConstraintGraphBuilder::checkExprTable(Scope2* scope, AstExprTable* expr)
{
    TypeId ty = arena->addType(TableTypeVar{});
    TableTypeVar* ttv = getMutable<TableTypeVar>(ty);
    LUAU_ASSERT(ttv);

    auto createIndexer = [this, scope, ttv](TypeId currentIndexType, TypeId currentResultType) {
        if (!ttv->indexer)
        {
            TypeId indexType = this->freshType(scope);
            TypeId resultType = this->freshType(scope);
            ttv->indexer = TableIndexer{indexType, resultType};
        }

        addConstraint(scope, SubtypeConstraint{ttv->indexer->indexType, currentIndexType});
        addConstraint(scope, SubtypeConstraint{ttv->indexer->indexResultType, currentResultType});
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
                createIndexer(keyTy, itemTy);
            }
        }
        else
        {
            TypeId numberType = singletonTypes.numberType;
            createIndexer(numberType, itemTy);
        }
    }

    return ty;
}

std::pair<TypeId, Scope2*> ConstraintGraphBuilder::checkFunctionSignature(Scope2* parent, AstExprFunction* fn)
{
    Scope2* innerScope = childScope(fn->body->location, parent);
    TypePackId returnType = freshTypePack(innerScope);
    innerScope->returnType = returnType;

    if (fn->returnAnnotation)
    {
        TypePackId annotatedRetType = resolveTypePack(innerScope, *fn->returnAnnotation);
        addConstraint(innerScope, PackSubtypeConstraint{returnType, annotatedRetType});
    }

    std::vector<TypeId> argTypes;

    for (AstLocal* local : fn->args)
    {
        TypeId t = freshType(innerScope);
        argTypes.push_back(t);
        innerScope->bindings[local] = t;

        if (local->annotation)
        {
            TypeId argAnnotation = resolveType(innerScope, local->annotation);
            addConstraint(innerScope, SubtypeConstraint{t, argAnnotation});
        }
    }

    // TODO: Vararg annotation.

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
        addConstraint(scope, PackSubtypeConstraint{scope->returnType, empty});
    }
}

TypeId ConstraintGraphBuilder::resolveType(Scope2* scope, AstType* ty)
{
    TypeId result = nullptr;

    if (auto ref = ty->as<AstTypeReference>())
    {
        // TODO: Support imported types w/ require tracing.
        // TODO: Support generic type references.
        LUAU_ASSERT(!ref->prefix);
        LUAU_ASSERT(!ref->hasParameterList);

        // TODO: If it doesn't exist, should we introduce a free binding?
        // This is probably important for handling type aliases.
        result = scope->lookupTypeBinding(ref->name.value).value_or(singletonTypes.errorRecoveryType());
    }
    else if (auto tab = ty->as<AstTypeTable>())
    {
        TableTypeVar::Props props;
        std::optional<TableIndexer> indexer;

        for (const AstTableProp& prop : tab->props)
        {
            std::string name = prop.name.value;
            // TODO: Recursion limit.
            TypeId propTy = resolveType(scope, prop.type);
            // TODO: Fill in location.
            props[name] = {propTy};
        }

        if (tab->indexer)
        {
            // TODO: Recursion limit.
            indexer = TableIndexer{
                resolveType(scope, tab->indexer->indexType),
                resolveType(scope, tab->indexer->resultType),
            };
        }

        // TODO: Remove TypeLevel{} here, we don't need it.
        result = arena->addType(TableTypeVar{props, indexer, TypeLevel{}, TableState::Sealed});
    }
    else if (auto fn = ty->as<AstTypeFunction>())
    {
        // TODO: Generic functions.
        // TODO: Scope (though it may not be needed).
        // TODO: Recursion limit.
        TypePackId argTypes = resolveTypePack(scope, fn->argTypes);
        TypePackId returnTypes = resolveTypePack(scope, fn->returnTypes);

        // TODO: Is this the right constructor to use?
        result = arena->addType(FunctionTypeVar{argTypes, returnTypes});

        FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(result);
        ftv->argNames.reserve(fn->argNames.size);
        for (const auto& el : fn->argNames)
        {
            if (el)
            {
                const auto& [name, location] = *el;
                ftv->argNames.push_back(FunctionArgument{name.value, location});
            }
            else
            {
                ftv->argNames.push_back(std::nullopt);
            }
        }
    }
    else if (auto tof = ty->as<AstTypeTypeof>())
    {
        // TODO: Recursion limit.
        TypeId exprType = check(scope, tof->expr);
        result = exprType;
    }
    else if (auto unionAnnotation = ty->as<AstTypeUnion>())
    {
        std::vector<TypeId> parts;
        for (AstType* part : unionAnnotation->types)
        {
            // TODO: Recursion limit.
            parts.push_back(resolveType(scope, part));
        }

        result = arena->addType(UnionTypeVar{parts});
    }
    else if (auto intersectionAnnotation = ty->as<AstTypeIntersection>())
    {
        std::vector<TypeId> parts;
        for (AstType* part : intersectionAnnotation->types)
        {
            // TODO: Recursion limit.
            parts.push_back(resolveType(scope, part));
        }

        result = arena->addType(IntersectionTypeVar{parts});
    }
    else if (auto boolAnnotation = ty->as<AstTypeSingletonBool>())
    {
        result = arena->addType(SingletonTypeVar(BooleanSingleton{boolAnnotation->value}));
    }
    else if (auto stringAnnotation = ty->as<AstTypeSingletonString>())
    {
        result = arena->addType(SingletonTypeVar(StringSingleton{std::string(stringAnnotation->value.data, stringAnnotation->value.size)}));
    }
    else if (ty->is<AstTypeError>())
    {
        result = singletonTypes.errorRecoveryType();
    }
    else
    {
        LUAU_ASSERT(0);
        result = singletonTypes.errorRecoveryType();
    }

    astResolvedTypes[ty] = result;
    return result;
}

TypePackId ConstraintGraphBuilder::resolveTypePack(Scope2* scope, AstTypePack* tp)
{
    TypePackId result;
    if (auto expl = tp->as<AstTypePackExplicit>())
    {
        result = resolveTypePack(scope, expl->typeList);
    }
    else if (auto var = tp->as<AstTypePackVariadic>())
    {
        TypeId ty = resolveType(scope, var->variadicType);
        result = arena->addTypePack(TypePackVar{VariadicTypePack{ty}});
    }
    else if (auto gen = tp->as<AstTypePackGeneric>())
    {
        result = arena->addTypePack(TypePackVar{GenericTypePack{scope, gen->genericName.value}});
    }
    else
    {
        LUAU_ASSERT(0);
        result = singletonTypes.errorRecoveryTypePack();
    }

    astResolvedTypePacks[tp] = result;
    return result;
}

TypePackId ConstraintGraphBuilder::resolveTypePack(Scope2* scope, const AstTypeList& list)
{
    std::vector<TypeId> head;

    for (AstType* headTy : list.types)
    {
        head.push_back(resolveType(scope, headTy));
    }

    std::optional<TypePackId> tail = std::nullopt;
    if (list.tailType)
    {
        tail = resolveTypePack(scope, list.tailType);
    }

    return arena->addTypePack(TypePack{head, tail});
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
