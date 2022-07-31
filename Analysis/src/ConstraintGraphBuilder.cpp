// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details

#include "lluz/ConstraintGraphBuilder.h"
#include "lluz/RecursionCounter.h"
#include "lluz/ToString.h"

lluz_FASTINT(LluCheckRecursionLimit);

#include "lluz/Scope.h"

namespace lluz
{

const AstStat* getFallthrough(const AstStat* node); // TypeInfer.cpp

ConstraintGraphBuilder::ConstraintGraphBuilder(
    const ModuleName& moduleName, TypeArena* arena, NotNull<InternalErrorReporter> ice, NotNull<Scope2> globalScope)
    : moduleName(moduleName)
    , singletonTypes(getSingletonTypes())
    , arena(arena)
    , rootScope(nullptr)
    , ice(ice)
    , globalScope(globalScope)
{
    lluz_ASSERT(arena);
}

TypeId ConstraintGraphBuilder::freshType(NotNull<Scope2> scope)
{
    return arena->addType(FreeTypeVar{scope});
}

TypePackId ConstraintGraphBuilder::freshTypePack(NotNull<Scope2> scope)
{
    FreeTypePack f{scope};
    return arena->addTypePack(TypePackVar{std::move(f)});
}

NotNull<Scope2> ConstraintGraphBuilder::childScope(Location location, NotNull<Scope2> parent)
{
    auto scope = std::make_unique<Scope2>();
    NotNull<Scope2> borrow = NotNull(scope.get());
    scopes.emplace_back(location, std::move(scope));

    borrow->parent = parent;
    borrow->returnType = parent->returnType;
    parent->children.push_back(borrow);

    return borrow;
}

void ConstraintGraphBuilder::addConstraint(NotNull<Scope2> scope, ConstraintV cv)
{
    scope->constraints.emplace_back(new Constraint{std::move(cv)});
}

void ConstraintGraphBuilder::addConstraint(NotNull<Scope2> scope, std::unique_ptr<Constraint> c)
{
    scope->constraints.emplace_back(std::move(c));
}

void ConstraintGraphBuilder::visit(AstStatBlock* block)
{
    lluz_ASSERT(scopes.empty());
    lluz_ASSERT(rootScope == nullptr);
    scopes.emplace_back(block->location, std::make_unique<Scope2>());
    rootScope = scopes.back().second.get();
    NotNull<Scope2> borrow = NotNull(rootScope);

    rootScope->returnType = freshTypePack(borrow);

    prepopulateGlobalScope(borrow, block);

    // TODO: We should share the global scope.
    rootScope->typeBindings["nil"] = singletonTypes.nilType;
    rootScope->typeBindings["number"] = singletonTypes.numberType;
    rootScope->typeBindings["string"] = singletonTypes.stringType;
    rootScope->typeBindings["boolean"] = singletonTypes.booleanType;
    rootScope->typeBindings["thread"] = singletonTypes.threadType;

    visitBlockWithoutChildScope(borrow, block);
}

void ConstraintGraphBuilder::visitBlockWithoutChildScope(NotNull<Scope2> scope, AstStatBlock* block)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LluCheckRecursionLimit)
    {
        reportCodeTooComplex(block->location);
        return;
    }

    for (AstStat* stat : block->body)
        visit(scope, stat);
}

void ConstraintGraphBuilder::visit(NotNull<Scope2> scope, AstStat* stat)
{
    RecursionLimiter limiter{&recursionCount, FInt::LluCheckRecursionLimit};

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
        lluz_ASSERT(0);
}

void ConstraintGraphBuilder::visit(NotNull<Scope2> scope, AstStatLocal* local)
{
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

void addConstraints(Constraint* constraint, NotNull<Scope2> scope)
{
    scope->constraints.reserve(scope->constraints.size() + scope->constraints.size());

    for (const auto& c : scope->constraints)
        constraint->dependencies.push_back(NotNull{c.get()});

    for (NotNull<Scope2> childScope : scope->children)
        addConstraints(constraint, childScope);
}

void ConstraintGraphBuilder::visit(NotNull<Scope2> scope, AstStatLocalFunction* function)
{
    // Local
    // Global
    // Dotted path
    // Self?

    TypeId functionType = nullptr;
    auto ty = scope->lookup(function->name);
    lluz_ASSERT(!ty.has_value()); // The parser ensures that every local function has a distinct Symbol for its name.

    functionType = arena->addType(BlockedTypeVar{});
    scope->bindings[function->name] = functionType;

    FunctionSignature sig = checkFunctionSignature(scope, function->func);
    sig.bodyScope->bindings[function->name] = sig.signature;

    checkFunctionBody(sig.bodyScope, function->func);

    std::unique_ptr<Constraint> c{
        new Constraint{GeneralizationConstraint{functionType, sig.signature, sig.signatureScope ? sig.signatureScope : sig.bodyScope}}};
    addConstraints(c.get(), sig.bodyScope);

    addConstraint(scope, std::move(c));
}

void ConstraintGraphBuilder::visit(NotNull<Scope2> scope, AstStatFunction* function)
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
            scope->bindings[localName->local] = functionType;
        }
        sig.bodyScope->bindings[localName->local] = sig.signature;
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
        sig.bodyScope->bindings[globalName->name] = sig.signature;
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

    lluz_ASSERT(functionType != nullptr);

    checkFunctionBody(sig.bodyScope, function->func);

    std::unique_ptr<Constraint> c{
        new Constraint{GeneralizationConstraint{functionType, sig.signature, sig.signatureScope ? sig.signatureScope : sig.bodyScope}}};
    addConstraints(c.get(), sig.bodyScope);

    addConstraint(scope, std::move(c));
}

void ConstraintGraphBuilder::visit(NotNull<Scope2> scope, AstStatReturn* ret)
{
    TypePackId exprTypes = checkPack(scope, ret->list);
    addConstraint(scope, PackSubtypeConstraint{exprTypes, scope->returnType});
}

void ConstraintGraphBuilder::visit(NotNull<Scope2> scope, AstStatBlock* block)
{
    NotNull<Scope2> innerScope = childScope(block->location, scope);

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

    visitBlockWithoutChildScope(innerScope, block);
}

void ConstraintGraphBuilder::visit(NotNull<Scope2> scope, AstStatAssign* assign)
{
    TypePackId varPackId = checkExprList(scope, assign->vars);
    TypePackId valuePack = checkPack(scope, assign->values);

    addConstraint(scope, PackSubtypeConstraint{valuePack, varPackId});
}

void ConstraintGraphBuilder::visit(NotNull<Scope2> scope, AstStatIf* ifStatement)
{
    check(scope, ifStatement->condition);

    NotNull<Scope2> thenScope = childScope(ifStatement->thenbody->location, scope);
    visit(thenScope, ifStatement->thenbody);

    if (ifStatement->elsebody)
    {
        NotNull<Scope2> elseScope = childScope(ifStatement->elsebody->location, scope);
        visit(elseScope, ifStatement->elsebody);
    }
}

void ConstraintGraphBuilder::visit(NotNull<Scope2> scope, AstStatTypeAlias* alias)
{
    // TODO: Exported type aliases
    // TODO: Generic type aliases

    auto it = scope->typeBindings.find(alias->name.value);
    // This should always be here since we do a separate pass over the
    // AST to set up typeBindings. If it's not, we've somehow skipped
    // this alias in that first pass.
    lluz_ASSERT(it != scope->typeBindings.end());
    if (it == scope->typeBindings.end())
    {
        ice->ice("Type alias does not have a pre-populated binding", alias->location);
    }

    TypeId ty = resolveType(scope, alias->type);

    // Rather than using a subtype constraint, we instead directly bind
    // the free type we generated in the first pass to the resolved type.
    // This prevents a case where you could cause another constraint to
    // bind the free alias type to an unrelated type, causing havoc.
    asMutable(it->second)->ty.emplace<BoundTypeVar>(ty);

    addConstraint(scope, NameConstraint{ty, alias->name.value});
}

TypePackId ConstraintGraphBuilder::checkPack(NotNull<Scope2> scope, AstArray<AstExpr*> exprs)
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

    lluz_ASSERT(last != nullptr);

    return arena->addTypePack(TypePack{std::move(types), last});
}

TypePackId ConstraintGraphBuilder::checkExprList(NotNull<Scope2> scope, const AstArray<AstExpr*>& exprs)
{
    TypePackId result = arena->addTypePack({});
    TypePack* resultPack = getMutable<TypePack>(result);
    lluz_ASSERT(resultPack);

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

TypePackId ConstraintGraphBuilder::checkPack(NotNull<Scope2> scope, AstExpr* expr)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LluCheckRecursionLimit)
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

    lluz_ASSERT(result);
    astTypePacks[expr] = result;
    return result;
}

TypeId ConstraintGraphBuilder::check(NotNull<Scope2> scope, AstExpr* expr)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LluCheckRecursionLimit)
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
        lluz_ASSERT(0);
        result = freshType(scope);
    }

    lluz_ASSERT(result);
    astTypes[expr] = result;
    return result;
}

TypeId ConstraintGraphBuilder::check(NotNull<Scope2> scope, AstExprIndexName* indexName)
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

TypeId ConstraintGraphBuilder::check(NotNull<Scope2> scope, AstExprIndexExpr* indexExpr)
{
    TypeId obj = check(scope, indexExpr->expr);
    TypeId indexType = check(scope, indexExpr->index);

    TypeId result = freshType(scope);

    TableIndexer indexer{indexType, result};
    TypeId tableType = arena->addType(TableTypeVar{TableTypeVar::Props{}, TableIndexer{indexType, result}, TypeLevel{}, TableState::Free});

    addConstraint(scope, SubtypeConstraint{obj, tableType});

    return result;
}

TypeId ConstraintGraphBuilder::check(NotNull<Scope2> scope, AstExprUnary* unary)
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
        lluz_ASSERT(0);
    }

    lluz_UNREACHABLE();
    return singletonTypes.errorRecoveryType();
}

TypeId ConstraintGraphBuilder::check(NotNull<Scope2> scope, AstExprBinary* binary)
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
        lluz_ASSERT(0);
    }

    lluz_ASSERT(0);
    return nullptr;
}

TypeId ConstraintGraphBuilder::checkExprTable(NotNull<Scope2> scope, AstExprTable* expr)
{
    TypeId ty = arena->addType(TableTypeVar{});
    TableTypeVar* ttv = getMutable<TableTypeVar>(ty);
    lluz_ASSERT(ttv);

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

ConstraintGraphBuilder::FunctionSignature ConstraintGraphBuilder::checkFunctionSignature(NotNull<Scope2> parent, AstExprFunction* fn)
{
    Scope2* signatureScope = nullptr;
    Scope2* bodyScope = nullptr;
    TypePackId returnType = nullptr;

    std::vector<TypeId> genericTypes;
    std::vector<TypePackId> genericTypePacks;

    bool hasGenerics = fn->generics.size > 0 || fn->genericPacks.size > 0;

    // If we don't have any generics, we can save some memory and compute by not
    // creating the signatureScope, which is only used to scope the declared
    // generics properly.
    if (hasGenerics)
    {
        NotNull signatureBorrow = childScope(fn->location, parent);
        signatureScope = signatureBorrow.get();

        // We need to assign returnType before creating bodyScope so that the
        // return type gets propogated to bodyScope.
        returnType = freshTypePack(signatureBorrow);
        signatureScope->returnType = returnType;

        bodyScope = childScope(fn->body->location, signatureBorrow).get();

        std::vector<std::pair<Name, GenericTypeDefinition>> genericDefinitions = createGenerics(signatureBorrow, fn->generics);
        std::vector<std::pair<Name, GenericTypePackDefinition>> genericPackDefinitions = createGenericPacks(signatureBorrow, fn->genericPacks);

        // We do not support default values on function generics, so we only
        // care about the types involved.
        for (const auto& [name, g] : genericDefinitions)
        {
            genericTypes.push_back(g.ty);
            signatureScope->typeBindings[name] = g.ty;
        }

        for (const auto& [name, g] : genericPackDefinitions)
        {
            genericTypePacks.push_back(g.tp);
            signatureScope->typePackBindings[name] = g.tp;
        }
    }
    else
    {
        NotNull bodyBorrow = childScope(fn->body->location, parent);
        bodyScope = bodyBorrow.get();

        returnType = freshTypePack(bodyBorrow);
        bodyBorrow->returnType = returnType;

        // To eliminate the need to branch on hasGenerics below, we say that the
        // signature scope is the body scope when there is no real signature
        // scope.
        signatureScope = bodyScope;
    }

    NotNull bodyBorrow = NotNull(bodyScope);
    NotNull signatureBorrow = NotNull(signatureScope);

    if (fn->returnAnnotation)
    {
        TypePackId annotatedRetType = resolveTypePack(signatureBorrow, *fn->returnAnnotation);
        addConstraint(signatureBorrow, PackSubtypeConstraint{returnType, annotatedRetType});
    }

    std::vector<TypeId> argTypes;

    for (AstLocal* local : fn->args)
    {
        TypeId t = freshType(signatureBorrow);
        argTypes.push_back(t);
        signatureScope->bindings[local] = t;

        if (local->annotation)
        {
            TypeId argAnnotation = resolveType(signatureBorrow, local->annotation);
            addConstraint(signatureBorrow, SubtypeConstraint{t, argAnnotation});
        }
    }

    // TODO: Vararg annotation.
    // TODO: Preserve argument names in the function's type.

    FunctionTypeVar actualFunction{arena->addTypePack(argTypes), returnType};
    actualFunction.hasNoGenerics = !hasGenerics;
    actualFunction.generics = std::move(genericTypes);
    actualFunction.genericPacks = std::move(genericTypePacks);

    TypeId actualFunctionType = arena->addType(std::move(actualFunction));
    lluz_ASSERT(actualFunctionType);
    astTypes[fn] = actualFunctionType;

    return {
        /* signature */ actualFunctionType,
        // Undo the workaround we made above: if there's no signature scope,
        // don't report it.
        /* signatureScope */ hasGenerics ? signatureScope : nullptr,
        /* bodyScope */ bodyBorrow,
    };
}

void ConstraintGraphBuilder::checkFunctionBody(NotNull<Scope2> scope, AstExprFunction* fn)
{
    visitBlockWithoutChildScope(scope, fn->body);

    // If it is possible for execution to reach the end of the function, the return type must be compatible with ()

    if (nullptr != getFallthrough(fn->body))
    {
        TypePackId empty = arena->addTypePack({}); // TODO we could have CSG retain one of these forever
        addConstraint(scope, PackSubtypeConstraint{scope->returnType, empty});
    }
}

TypeId ConstraintGraphBuilder::resolveType(NotNull<Scope2> scope, AstType* ty)
{
    TypeId result = nullptr;

    if (auto ref = ty->as<AstTypeReference>())
    {
        // TODO: Support imported types w/ require tracing.
        // TODO: Support generic type references.
        lluz_ASSERT(!ref->prefix);
        lluz_ASSERT(!ref->hasParameterList);

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
        // TODO: Recursion limit.
        bool hasGenerics = fn->generics.size > 0 || fn->genericPacks.size > 0;
        Scope2* signatureScope = nullptr;

        std::vector<TypeId> genericTypes;
        std::vector<TypePackId> genericTypePacks;

        // If we don't have generics, we do not need to generate a child scope
        // for the generic bindings to live on.
        if (hasGenerics)
        {
            NotNull<Scope2> signatureBorrow = childScope(fn->location, scope);
            signatureScope = signatureBorrow.get();

            std::vector<std::pair<Name, GenericTypeDefinition>> genericDefinitions = createGenerics(signatureBorrow, fn->generics);
            std::vector<std::pair<Name, GenericTypePackDefinition>> genericPackDefinitions = createGenericPacks(signatureBorrow, fn->genericPacks);

            for (const auto& [name, g] : genericDefinitions)
            {
                genericTypes.push_back(g.ty);
                signatureBorrow->typeBindings[name] = g.ty;
            }

            for (const auto& [name, g] : genericPackDefinitions)
            {
                genericTypePacks.push_back(g.tp);
                signatureBorrow->typePackBindings[name] = g.tp;
            }
        }
        else
        {
            // To eliminate the need to branch on hasGenerics below, we say that
            // the signature scope is the parent scope if we don't have
            // generics.
            signatureScope = scope.get();
        }

        NotNull<Scope2> signatureBorrow(signatureScope);

        TypePackId argTypes = resolveTypePack(signatureBorrow, fn->argTypes);
        TypePackId returnTypes = resolveTypePack(signatureBorrow, fn->returnTypes);

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
        lluz_ASSERT(0);
        result = singletonTypes.errorRecoveryType();
    }

    astResolvedTypes[ty] = result;
    return result;
}

TypePackId ConstraintGraphBuilder::resolveTypePack(NotNull<Scope2> scope, AstTypePack* tp)
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
        lluz_ASSERT(0);
        result = singletonTypes.errorRecoveryTypePack();
    }

    astResolvedTypePacks[tp] = result;
    return result;
}

TypePackId ConstraintGraphBuilder::resolveTypePack(NotNull<Scope2> scope, const AstTypeList& list)
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

std::vector<std::pair<Name, GenericTypeDefinition>> ConstraintGraphBuilder::createGenerics(NotNull<Scope2> scope, AstArray<AstGenericType> generics)
{
    std::vector<std::pair<Name, GenericTypeDefinition>> result;
    for (const auto& generic : generics)
    {
        TypeId genericTy = arena->addType(GenericTypeVar{scope, generic.name.value});
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
    NotNull<Scope2> scope, AstArray<AstGenericTypePack> generics)
{
    std::vector<std::pair<Name, GenericTypePackDefinition>> result;
    for (const auto& generic : generics)
    {
        TypePackId genericTy = arena->addTypePack(TypePackVar{GenericTypePack{scope, generic.name.value}});
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

TypeId ConstraintGraphBuilder::flattenPack(NotNull<Scope2> scope, Location location, TypePackId tp)
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
    const NotNull<Scope2> globalScope;
    const NotNull<TypeArena> arena;

    GlobalPrepopulator(NotNull<Scope2> globalScope, NotNull<TypeArena> arena)
        : globalScope(globalScope)
        , arena(arena)
    {
    }

    bool visit(AstStatFunction* function) override
    {
        if (AstExprGlobal* g = function->name->as<AstExprGlobal>())
            globalScope->bindings[g->name] = arena->addType(BlockedTypeVar{});

        return true;
    }
};

void ConstraintGraphBuilder::prepopulateGlobalScope(NotNull<Scope2> globalScope, AstStatBlock* program)
{
    GlobalPrepopulator gp{NotNull{globalScope}, arena};

    program->visit(&gp);
}

void collectConstraints(std::vector<NotNull<Constraint>>& result, NotNull<Scope2> scope)
{
    for (const auto& c : scope->constraints)
        result.push_back(NotNull{c.get()});

    for (NotNull<Scope2> child : scope->children)
        collectConstraints(result, child);
}

std::vector<NotNull<Constraint>> collectConstraints(NotNull<Scope2> rootScope)
{
    std::vector<NotNull<Constraint>> result;
    collectConstraints(result, rootScope);
    return result;
}

} // namespace lluz
