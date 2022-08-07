// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/ConstraintGraphBuilder.h"
#include "Luau/RecursionCounter.h"
#include "Luau/ToString.h"

LUAU_FASTINT(LuauCheckRecursionLimit);

#include "Luau/Scope.h"

namespace Luau
{

const AstStat* getFallthrough(const AstStat* node); // TypeInfer.cpp

ConstraintGraphBuilder::ConstraintGraphBuilder(
    const ModuleName& moduleName, TypeArena* arena, NotNull<InternalErrorReporter> ice, NotNull<Scope> globalScope)
    : moduleName(moduleName)
    , singletonTypes(getSingletonTypes())
    , arena(arena)
    , rootScope(nullptr)
    , ice(ice)
    , globalScope(globalScope)
{
    LUAU_ASSERT(arena);
}

TypeId ConstraintGraphBuilder::freshType(const ScopePtr& scope)
{
    return arena->addType(FreeTypeVar{scope.get()});
}

TypePackId ConstraintGraphBuilder::freshTypePack(const ScopePtr& scope)
{
    FreeTypePack f{scope.get()};
    return arena->addTypePack(TypePackVar{std::move(f)});
}

ScopePtr ConstraintGraphBuilder::childScope(Location location, const ScopePtr& parent)
{
    auto scope = std::make_shared<Scope>(parent);
    scopes.emplace_back(location, scope);

    scope->returnType = parent->returnType;
    parent->children.push_back(NotNull(scope.get()));

    return scope;
}

void ConstraintGraphBuilder::addConstraint(const ScopePtr& scope, ConstraintV cv)
{
    scope->constraints.emplace_back(new Constraint{std::move(cv)});
}

void ConstraintGraphBuilder::addConstraint(const ScopePtr& scope, std::unique_ptr<Constraint> c)
{
    scope->constraints.emplace_back(std::move(c));
}

void ConstraintGraphBuilder::visit(AstStatBlock* block)
{
    LUAU_ASSERT(scopes.empty());
    LUAU_ASSERT(rootScope == nullptr);
    ScopePtr scope = std::make_shared<Scope>(singletonTypes.anyTypePack);
    rootScope = scope.get();
    scopes.emplace_back(block->location, scope);

    rootScope->returnType = freshTypePack(scope);

    prepopulateGlobalScope(scope, block);

    // TODO: We should share the global scope.
    rootScope->typeBindings["nil"] = TypeFun{singletonTypes.nilType};
    rootScope->typeBindings["number"] = TypeFun{singletonTypes.numberType};
    rootScope->typeBindings["string"] = TypeFun{singletonTypes.stringType};
    rootScope->typeBindings["boolean"] = TypeFun{singletonTypes.booleanType};
    rootScope->typeBindings["thread"] = TypeFun{singletonTypes.threadType};

    visitBlockWithoutChildScope(scope, block);
}

void ConstraintGraphBuilder::visitBlockWithoutChildScope(const ScopePtr& scope, AstStatBlock* block)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportCodeTooComplex(block->location);
        return;
    }

    std::unordered_map<Name, Location> aliasDefinitionLocations;

    // In order to enable mutually-recursive type aliases, we need to
    // populate the type bindings before we actually check any of the
    // alias statements. Since we're not ready to actually resolve
    // any of the annotations, we just use a fresh type for now.
    for (AstStat* stat : block->body)
    {
        if (auto alias = stat->as<AstStatTypeAlias>())
        {
            if (scope->typeBindings.count(alias->name.value) != 0)
            {
                auto it = aliasDefinitionLocations.find(alias->name.value);
                LUAU_ASSERT(it != aliasDefinitionLocations.end());
                reportError(alias->location, DuplicateTypeDefinition{alias->name.value, it->second});
                continue;
            }

            bool hasGenerics = alias->generics.size > 0 || alias->genericPacks.size > 0;

            ScopePtr defnScope = scope;
            if (hasGenerics)
            {
                defnScope = childScope(alias->location, scope);
            }

            TypeId initialType = freshType(scope);
            TypeFun initialFun = TypeFun{initialType};

            for (const auto& [name, gen] : createGenerics(defnScope, alias->generics))
            {
                initialFun.typeParams.push_back(gen);
                defnScope->typeBindings[name] = TypeFun{gen.ty};
            }

            for (const auto& [name, genPack] : createGenericPacks(defnScope, alias->genericPacks))
            {
                initialFun.typePackParams.push_back(genPack);
                defnScope->typePackBindings[name] = genPack.tp;
            }

            scope->typeBindings[alias->name.value] = std::move(initialFun);
            astTypeAliasDefiningScopes[alias] = defnScope;
            aliasDefinitionLocations[alias->name.value] = alias->location;
        }
    }

    for (AstStat* stat : block->body)
        visit(scope, stat);
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStat* stat)
{
    RecursionLimiter limiter{&recursionCount, FInt::LuauCheckRecursionLimit};

    if (auto s = stat->as<AstStatBlock>())
        visit(scope, s);
    else if (auto s = stat->as<AstStatLocal>())
        visit(scope, s);
    else if (auto s = stat->as<AstStatFor>())
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
    else if (auto s = stat->as<AstStatDeclareGlobal>())
        visit(scope, s);
    else if (auto s = stat->as<AstStatDeclareClass>())
        visit(scope, s);
    else if (auto s = stat->as<AstStatDeclareFunction>())
        visit(scope, s);
    else
        LUAU_ASSERT(0);
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatLocal* local)
{
    std::vector<TypeId> varTypes;

    for (AstLocal* local : local->vars)
    {
        TypeId ty = freshType(scope);
        Location location = local->location;

        if (local->annotation)
        {
            location = local->annotation->location;
            TypeId annotation = resolveType(scope, local->annotation, /* topLevel */ true);
            addConstraint(scope, SubtypeConstraint{ty, annotation});
        }

        varTypes.push_back(ty);
        scope->bindings[local] = Binding{ty, location};
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

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatFor* for_)
{
    auto checkNumber = [&](AstExpr* expr) {
        if (!expr)
            return;

        TypeId t = check(scope, expr);
        addConstraint(scope, SubtypeConstraint{t, singletonTypes.numberType});
    };

    checkNumber(for_->from);
    checkNumber(for_->to);
    checkNumber(for_->step);

    ScopePtr forScope = childScope(for_->location, scope);
    forScope->bindings[for_->var] = Binding{singletonTypes.numberType, for_->var->location};

    visit(forScope, for_->body);
}

void addConstraints(Constraint* constraint, NotNull<Scope> scope)
{
    scope->constraints.reserve(scope->constraints.size() + scope->constraints.size());

    for (const auto& c : scope->constraints)
        constraint->dependencies.push_back(NotNull{c.get()});

    for (NotNull<Scope> childScope : scope->children)
        addConstraints(constraint, childScope);
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatLocalFunction* function)
{
    // Local
    // Global
    // Dotted path
    // Self?

    TypeId functionType = nullptr;
    auto ty = scope->lookup(function->name);
    LUAU_ASSERT(!ty.has_value()); // The parser ensures that every local function has a distinct Symbol for its name.

    functionType = arena->addType(BlockedTypeVar{});
    scope->bindings[function->name] = Binding{functionType, function->name->location};

    FunctionSignature sig = checkFunctionSignature(scope, function->func);
    sig.bodyScope->bindings[function->name] = Binding{sig.signature, function->func->location};

    checkFunctionBody(sig.bodyScope, function->func);

    std::unique_ptr<Constraint> c{
        new Constraint{GeneralizationConstraint{functionType, sig.signature, sig.signatureScope ? sig.signatureScope.get() : sig.bodyScope.get()}}};
    addConstraints(c.get(), NotNull(sig.bodyScope.get()));

    addConstraint(scope, std::move(c));
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatFunction* function)
{
    // Name could be AstStatLocal, AstStatGlobal, AstStatIndexName.
    // With or without self

    TypeId functionType = nullptr;

    FunctionSignature sig = checkFunctionSignature(scope, function->func);

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
            scope->bindings[localName->local] = Binding{functionType, localName->location};
        }
        sig.bodyScope->bindings[localName->local] = Binding{sig.signature, localName->location};
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
            rootScope->bindings[globalName->name] = Binding{functionType, globalName->location};
        }
        sig.bodyScope->bindings[globalName->name] = Binding{sig.signature, globalName->location};
    }
    else if (AstExprIndexName* indexName = function->name->as<AstExprIndexName>())
    {
        TypeId containingTableType = check(scope, indexName->expr);

        functionType = arena->addType(BlockedTypeVar{});
        TypeId prospectiveTableType =
            arena->addType(TableTypeVar{}); // TODO look into stack utilization.  This is probably ok because it scales with AST depth.
        NotNull<TableTypeVar> prospectiveTable{getMutable<TableTypeVar>(prospectiveTableType)};

        Property& prop = prospectiveTable->props[indexName->index.value];
        prop.type = functionType;
        prop.location = function->name->location;

        addConstraint(scope, SubtypeConstraint{containingTableType, prospectiveTableType});
    }
    else if (AstExprError* err = function->name->as<AstExprError>())
    {
        functionType = singletonTypes.errorRecoveryType();
    }

    LUAU_ASSERT(functionType != nullptr);

    checkFunctionBody(sig.bodyScope, function->func);

    std::unique_ptr<Constraint> c{
        new Constraint{GeneralizationConstraint{functionType, sig.signature, sig.signatureScope ? sig.signatureScope.get() : sig.bodyScope.get()}}};
    addConstraints(c.get(), NotNull(sig.bodyScope.get()));

    addConstraint(scope, std::move(c));
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatReturn* ret)
{
    TypePackId exprTypes = checkPack(scope, ret->list);
    addConstraint(scope, PackSubtypeConstraint{exprTypes, scope->returnType});
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatBlock* block)
{
    ScopePtr innerScope = childScope(block->location, scope);

    visitBlockWithoutChildScope(innerScope, block);
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatAssign* assign)
{
    TypePackId varPackId = checkExprList(scope, assign->vars);
    TypePackId valuePack = checkPack(scope, assign->values);

    addConstraint(scope, PackSubtypeConstraint{valuePack, varPackId});
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatIf* ifStatement)
{
    check(scope, ifStatement->condition);

    ScopePtr thenScope = childScope(ifStatement->thenbody->location, scope);
    visit(thenScope, ifStatement->thenbody);

    if (ifStatement->elsebody)
    {
        ScopePtr elseScope = childScope(ifStatement->elsebody->location, scope);
        visit(elseScope, ifStatement->elsebody);
    }
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatTypeAlias* alias)
{
    // TODO: Exported type aliases

    auto bindingIt = scope->typeBindings.find(alias->name.value);
    ScopePtr* defnIt = astTypeAliasDefiningScopes.find(alias);
    // These will be undefined if the alias was a duplicate definition, in which
    // case we just skip over it.
    if (bindingIt == scope->typeBindings.end() || defnIt == nullptr)
    {
        return;
    }

    ScopePtr resolvingScope = *defnIt;
    TypeId ty = resolveType(resolvingScope, alias->type, /* topLevel */ true);

    LUAU_ASSERT(get<FreeTypeVar>(bindingIt->second.type));

    // Rather than using a subtype constraint, we instead directly bind
    // the free type we generated in the first pass to the resolved type.
    // This prevents a case where you could cause another constraint to
    // bind the free alias type to an unrelated type, causing havoc.
    asMutable(bindingIt->second.type)->ty.emplace<BoundTypeVar>(ty);

    addConstraint(scope, NameConstraint{ty, alias->name.value});
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatDeclareGlobal* global)
{
    LUAU_ASSERT(global->type);

    TypeId globalTy = resolveType(scope, global->type);
    scope->bindings[global->name] = Binding{globalTy, global->location};
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatDeclareClass* global)
{
    LUAU_ASSERT(false); // TODO: implement
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatDeclareFunction* global)
{
    LUAU_ASSERT(false); // TODO: implement
}

TypePackId ConstraintGraphBuilder::checkPack(const ScopePtr& scope, AstArray<AstExpr*> exprs)
{
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

TypePackId ConstraintGraphBuilder::checkExprList(const ScopePtr& scope, const AstArray<AstExpr*>& exprs)
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

TypePackId ConstraintGraphBuilder::checkPack(const ScopePtr& scope, AstExpr* expr)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportCodeTooComplex(expr->location);
        return singletonTypes.errorRecoveryTypePack();
    }

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

        TypeId instantiatedType = arena->addType(BlockedTypeVar{});
        addConstraint(scope, InstantiationConstraint{instantiatedType, fnType});

        TypePackId rets = freshTypePack(scope);
        FunctionTypeVar ftv(arena->addTypePack(TypePack{args, {}}), rets);
        TypeId inferredFnType = arena->addType(ftv);

        addConstraint(scope, SubtypeConstraint{inferredFnType, instantiatedType});
        result = rets;
    }
    else if (AstExprVarargs* varargs = expr->as<AstExprVarargs>())
    {
        if (scope->varargPack)
            result = *scope->varargPack;
        else
            result = singletonTypes.errorRecoveryTypePack();
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

TypeId ConstraintGraphBuilder::check(const ScopePtr& scope, AstExpr* expr)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportCodeTooComplex(expr->location);
        return singletonTypes.errorRecoveryType();
    }

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
        {
            /* prepopulateGlobalScope() has already added all global functions to the environment by this point, so any
             * global that is not already in-scope is definitely an unknown symbol.
             */
            reportError(g->location, UnknownSymbol{g->name.value});
            result = singletonTypes.errorRecoveryType(); // FIXME?  Record an error at this point?
        }
    }
    else if (expr->is<AstExprVarargs>())
        result = flattenPack(scope, expr->location, checkPack(scope, expr));
    else if (expr->is<AstExprCall>())
        result = flattenPack(scope, expr->location, checkPack(scope, expr));
    else if (auto a = expr->as<AstExprFunction>())
    {
        FunctionSignature sig = checkFunctionSignature(scope, a);
        checkFunctionBody(sig.bodyScope, a);
        return sig.signature;
    }
    else if (auto indexName = expr->as<AstExprIndexName>())
        result = check(scope, indexName);
    else if (auto indexExpr = expr->as<AstExprIndexExpr>())
        result = check(scope, indexExpr);
    else if (auto table = expr->as<AstExprTable>())
        result = checkExprTable(scope, table);
    else if (auto unary = expr->as<AstExprUnary>())
        result = check(scope, unary);
    else if (auto binary = expr->as<AstExprBinary>())
        result = check(scope, binary);
    else if (auto err = expr->as<AstExprError>())
    {
        // Open question: Should we traverse into this?
        result = singletonTypes.errorRecoveryType();
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

TypeId ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprIndexName* indexName)
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

TypeId ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprIndexExpr* indexExpr)
{
    TypeId obj = check(scope, indexExpr->expr);
    TypeId indexType = check(scope, indexExpr->index);

    TypeId result = freshType(scope);

    TableIndexer indexer{indexType, result};
    TypeId tableType = arena->addType(TableTypeVar{TableTypeVar::Props{}, TableIndexer{indexType, result}, TypeLevel{}, TableState::Free});

    addConstraint(scope, SubtypeConstraint{obj, tableType});

    return result;
}

TypeId ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprUnary* unary)
{
    TypeId operandType = check(scope, unary->expr);

    switch (unary->op)
    {
    case AstExprUnary::Minus:
    {
        TypeId resultType = arena->addType(BlockedTypeVar{});
        addConstraint(scope, UnaryConstraint{AstExprUnary::Minus, operandType, resultType});
        return resultType;
    }
    default:
        LUAU_ASSERT(0);
    }

    LUAU_UNREACHABLE();
    return singletonTypes.errorRecoveryType();
}

TypeId ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprBinary* binary)
{
    TypeId leftType = check(scope, binary->left);
    TypeId rightType = check(scope, binary->right);
    switch (binary->op)
    {
    case AstExprBinary::Or:
    {
        addConstraint(scope, SubtypeConstraint{leftType, rightType});
        return leftType;
    }
    case AstExprBinary::Sub:
    {
        TypeId resultType = arena->addType(BlockedTypeVar{});
        addConstraint(scope, BinaryConstraint{AstExprBinary::Sub, leftType, rightType, resultType});
        return resultType;
    }
    default:
        LUAU_ASSERT(0);
    }

    LUAU_ASSERT(0);
    return nullptr;
}

TypeId ConstraintGraphBuilder::checkExprTable(const ScopePtr& scope, AstExprTable* expr)
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
        if (get<ErrorTypeVar>(follow(itemTy)))
            return ty;

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

ConstraintGraphBuilder::FunctionSignature ConstraintGraphBuilder::checkFunctionSignature(const ScopePtr& parent, AstExprFunction* fn)
{
    ScopePtr signatureScope = nullptr;
    ScopePtr bodyScope = nullptr;
    TypePackId returnType = nullptr;

    std::vector<TypeId> genericTypes;
    std::vector<TypePackId> genericTypePacks;

    bool hasGenerics = fn->generics.size > 0 || fn->genericPacks.size > 0;

    // If we don't have any generics, we can save some memory and compute by not
    // creating the signatureScope, which is only used to scope the declared
    // generics properly.
    if (hasGenerics)
    {
        signatureScope = childScope(fn->location, parent);

        // We need to assign returnType before creating bodyScope so that the
        // return type gets propogated to bodyScope.
        returnType = freshTypePack(signatureScope);
        signatureScope->returnType = returnType;

        bodyScope = childScope(fn->body->location, signatureScope);

        std::vector<std::pair<Name, GenericTypeDefinition>> genericDefinitions = createGenerics(signatureScope, fn->generics);
        std::vector<std::pair<Name, GenericTypePackDefinition>> genericPackDefinitions = createGenericPacks(signatureScope, fn->genericPacks);

        // We do not support default values on function generics, so we only
        // care about the types involved.
        for (const auto& [name, g] : genericDefinitions)
        {
            genericTypes.push_back(g.ty);
            signatureScope->typeBindings[name] = TypeFun{g.ty};
        }

        for (const auto& [name, g] : genericPackDefinitions)
        {
            genericTypePacks.push_back(g.tp);
            signatureScope->typePackBindings[name] = g.tp;
        }
    }
    else
    {
        bodyScope = childScope(fn->body->location, parent);

        returnType = freshTypePack(bodyScope);
        bodyScope->returnType = returnType;

        // To eliminate the need to branch on hasGenerics below, we say that the
        // signature scope is the body scope when there is no real signature
        // scope.
        signatureScope = bodyScope;
    }

    if (fn->returnAnnotation)
    {
        TypePackId annotatedRetType = resolveTypePack(signatureScope, *fn->returnAnnotation);
        addConstraint(signatureScope, PackSubtypeConstraint{returnType, annotatedRetType});
    }

    std::vector<TypeId> argTypes;

    for (AstLocal* local : fn->args)
    {
        TypeId t = freshType(signatureScope);
        argTypes.push_back(t);
        signatureScope->bindings[local] = Binding{t, local->location};

        if (local->annotation)
        {
            TypeId argAnnotation = resolveType(signatureScope, local->annotation, /* topLevel */ true);
            addConstraint(signatureScope, SubtypeConstraint{t, argAnnotation});
        }
    }

    // TODO: Vararg annotation.
    // TODO: Preserve argument names in the function's type.

    FunctionTypeVar actualFunction{arena->addTypePack(argTypes), returnType};
    actualFunction.hasNoGenerics = !hasGenerics;
    actualFunction.generics = std::move(genericTypes);
    actualFunction.genericPacks = std::move(genericTypePacks);

    TypeId actualFunctionType = arena->addType(std::move(actualFunction));
    LUAU_ASSERT(actualFunctionType);
    astTypes[fn] = actualFunctionType;

    return {
        /* signature */ actualFunctionType,
        // Undo the workaround we made above: if there's no signature scope,
        // don't report it.
        /* signatureScope */ hasGenerics ? signatureScope : nullptr,
        /* bodyScope */ bodyScope,
    };
}

void ConstraintGraphBuilder::checkFunctionBody(const ScopePtr& scope, AstExprFunction* fn)
{
    visitBlockWithoutChildScope(scope, fn->body);

    // If it is possible for execution to reach the end of the function, the return type must be compatible with ()

    if (nullptr != getFallthrough(fn->body))
    {
        TypePackId empty = arena->addTypePack({}); // TODO we could have CSG retain one of these forever
        addConstraint(scope, PackSubtypeConstraint{scope->returnType, empty});
    }
}

TypeId ConstraintGraphBuilder::resolveType(const ScopePtr& scope, AstType* ty, bool topLevel)
{
    TypeId result = nullptr;

    if (auto ref = ty->as<AstTypeReference>())
    {
        // TODO: Support imported types w/ require tracing.
        LUAU_ASSERT(!ref->prefix);

        std::optional<TypeFun> alias = scope->lookupTypeBinding(ref->name.value);

        if (alias.has_value())
        {
            // If the alias is not generic, we don't need to set up a blocked
            // type and an instantiation constraint.
            if (alias->typeParams.empty() && alias->typePackParams.empty())
            {
                result = alias->type;
            }
            else
            {
                std::vector<TypeId> parameters;
                std::vector<TypePackId> packParameters;

                for (const AstTypeOrPack& p : ref->parameters)
                {
                    // We do not enforce the ordering of types vs. type packs here;
                    // that is done in the parser.
                    if (p.type)
                    {
                        parameters.push_back(resolveType(scope, p.type));
                    }
                    else if (p.typePack)
                    {
                        packParameters.push_back(resolveTypePack(scope, p.typePack));
                    }
                    else
                    {
                        // This indicates a parser bug: one of these two pointers
                        // should be set.
                        LUAU_ASSERT(false);
                    }
                }

                result = arena->addType(PendingExpansionTypeVar{*alias, parameters, packParameters});

                if (topLevel)
                {
                    addConstraint(scope, TypeAliasExpansionConstraint{
                                             /* target */ result,
                                         });
                }
            }
        }
        else
        {
            reportError(ty->location, UnknownSymbol{ref->name.value, UnknownSymbol::Context::Type});
            result = singletonTypes.errorRecoveryType();
        }
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
        // TODO: Recursion limit.
        bool hasGenerics = fn->generics.size > 0 || fn->genericPacks.size > 0;
        ScopePtr signatureScope = nullptr;

        std::vector<TypeId> genericTypes;
        std::vector<TypePackId> genericTypePacks;

        // If we don't have generics, we do not need to generate a child scope
        // for the generic bindings to live on.
        if (hasGenerics)
        {
            signatureScope = childScope(fn->location, scope);

            std::vector<std::pair<Name, GenericTypeDefinition>> genericDefinitions = createGenerics(signatureScope, fn->generics);
            std::vector<std::pair<Name, GenericTypePackDefinition>> genericPackDefinitions = createGenericPacks(signatureScope, fn->genericPacks);

            for (const auto& [name, g] : genericDefinitions)
            {
                genericTypes.push_back(g.ty);
                signatureScope->typeBindings[name] = TypeFun{g.ty};
            }

            for (const auto& [name, g] : genericPackDefinitions)
            {
                genericTypePacks.push_back(g.tp);
                signatureScope->typePackBindings[name] = g.tp;
            }
        }
        else
        {
            // To eliminate the need to branch on hasGenerics below, we say that
            // the signature scope is the parent scope if we don't have
            // generics.
            signatureScope = scope;
        }

        TypePackId argTypes = resolveTypePack(signatureScope, fn->argTypes);
        TypePackId returnTypes = resolveTypePack(signatureScope, fn->returnTypes);

        // TODO: FunctionTypeVar needs a pointer to the scope so that we know
        // how to quantify/instantiate it.
        FunctionTypeVar ftv{argTypes, returnTypes};

        // This replicates the behavior of the appropriate FunctionTypeVar
        // constructors.
        ftv.hasNoGenerics = !hasGenerics;
        ftv.generics = std::move(genericTypes);
        ftv.genericPacks = std::move(genericTypePacks);

        ftv.argNames.reserve(fn->argNames.size);
        for (const auto& el : fn->argNames)
        {
            if (el)
            {
                const auto& [name, location] = *el;
                ftv.argNames.push_back(FunctionArgument{name.value, location});
            }
            else
            {
                ftv.argNames.push_back(std::nullopt);
            }
        }

        result = arena->addType(std::move(ftv));
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

TypePackId ConstraintGraphBuilder::resolveTypePack(const ScopePtr& scope, AstTypePack* tp)
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
        if (std::optional<TypePackId> lookup = scope->lookupTypePackBinding(gen->genericName.value))
        {
            result = *lookup;
        }
        else
        {
            reportError(tp->location, UnknownSymbol{gen->genericName.value, UnknownSymbol::Context::Type});
            result = singletonTypes.errorRecoveryTypePack();
        }
    }
    else
    {
        LUAU_ASSERT(0);
        result = singletonTypes.errorRecoveryTypePack();
    }

    astResolvedTypePacks[tp] = result;
    return result;
}

TypePackId ConstraintGraphBuilder::resolveTypePack(const ScopePtr& scope, const AstTypeList& list)
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

std::vector<std::pair<Name, GenericTypeDefinition>> ConstraintGraphBuilder::createGenerics(const ScopePtr& scope, AstArray<AstGenericType> generics)
{
    std::vector<std::pair<Name, GenericTypeDefinition>> result;
    for (const auto& generic : generics)
    {
        TypeId genericTy = arena->addType(GenericTypeVar{scope.get(), generic.name.value});
        std::optional<TypeId> defaultTy = std::nullopt;

        if (generic.defaultValue)
            defaultTy = resolveType(scope, generic.defaultValue);

        result.push_back({generic.name.value, GenericTypeDefinition{
                                                  genericTy,
                                                  defaultTy,
                                              }});
    }

    return result;
}

std::vector<std::pair<Name, GenericTypePackDefinition>> ConstraintGraphBuilder::createGenericPacks(
    const ScopePtr& scope, AstArray<AstGenericTypePack> generics)
{
    std::vector<std::pair<Name, GenericTypePackDefinition>> result;
    for (const auto& generic : generics)
    {
        TypePackId genericTy = arena->addTypePack(TypePackVar{GenericTypePack{scope.get(), generic.name.value}});
        std::optional<TypePackId> defaultTy = std::nullopt;

        if (generic.defaultValue)
            defaultTy = resolveTypePack(scope, generic.defaultValue);

        result.push_back({generic.name.value, GenericTypePackDefinition{
                                                  genericTy,
                                                  defaultTy,
                                              }});
    }

    return result;
}

TypeId ConstraintGraphBuilder::flattenPack(const ScopePtr& scope, Location location, TypePackId tp)
{
    if (auto f = first(tp))
        return *f;

    TypeId typeResult = freshType(scope);
    TypePack onePack{{typeResult}, freshTypePack(scope)};
    TypePackId oneTypePack = arena->addTypePack(std::move(onePack));

    addConstraint(scope, PackSubtypeConstraint{tp, oneTypePack});

    return typeResult;
}

void ConstraintGraphBuilder::reportError(Location location, TypeErrorData err)
{
    errors.push_back(TypeError{location, moduleName, std::move(err)});
}

void ConstraintGraphBuilder::reportCodeTooComplex(Location location)
{
    errors.push_back(TypeError{location, moduleName, CodeTooComplex{}});
}

struct GlobalPrepopulator : AstVisitor
{
    const NotNull<Scope> globalScope;
    const NotNull<TypeArena> arena;

    GlobalPrepopulator(NotNull<Scope> globalScope, NotNull<TypeArena> arena)
        : globalScope(globalScope)
        , arena(arena)
    {
    }

    bool visit(AstStatFunction* function) override
    {
        if (AstExprGlobal* g = function->name->as<AstExprGlobal>())
            globalScope->bindings[g->name] = Binding{arena->addType(BlockedTypeVar{})};

        return true;
    }
};

void ConstraintGraphBuilder::prepopulateGlobalScope(const ScopePtr& globalScope, AstStatBlock* program)
{
    GlobalPrepopulator gp{NotNull{globalScope.get()}, arena};

    program->visit(&gp);
}

void collectConstraints(std::vector<NotNull<Constraint>>& result, NotNull<Scope> scope)
{
    for (const auto& c : scope->constraints)
        result.push_back(NotNull{c.get()});

    for (NotNull<Scope> child : scope->children)
        collectConstraints(result, child);
}

std::vector<NotNull<Constraint>> collectConstraints(NotNull<Scope> rootScope)
{
    std::vector<NotNull<Constraint>> result;
    collectConstraints(result, rootScope);
    return result;
}

} // namespace Luau
