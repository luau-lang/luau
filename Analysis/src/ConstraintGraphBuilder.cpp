// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/ConstraintGraphBuilder.h"
#include "Luau/Ast.h"
#include "Luau/Common.h"
#include "Luau/Constraint.h"
#include "Luau/ModuleResolver.h"
#include "Luau/RecursionCounter.h"
#include "Luau/ToString.h"
#include "Luau/DcrLogger.h"

LUAU_FASTINT(LuauCheckRecursionLimit);
LUAU_FASTFLAG(DebugLuauLogSolverToJson);

#include "Luau/Scope.h"

namespace Luau
{

const AstStat* getFallthrough(const AstStat* node); // TypeInfer.cpp

static std::optional<AstExpr*> matchRequire(const AstExprCall& call)
{
    const char* require = "require";

    if (call.args.size != 1)
        return std::nullopt;

    const AstExprGlobal* funcAsGlobal = call.func->as<AstExprGlobal>();
    if (!funcAsGlobal || funcAsGlobal->name != require)
        return std::nullopt;

    if (call.args.size != 1)
        return std::nullopt;

    return call.args.data[0];
}

static bool matchSetmetatable(const AstExprCall& call)
{
    const char* smt = "setmetatable";

    if (call.args.size != 2)
        return false;
    
    const AstExprGlobal* funcAsGlobal = call.func->as<AstExprGlobal>();
    if (!funcAsGlobal || funcAsGlobal->name != smt)
        return false;

    return true;
}

ConstraintGraphBuilder::ConstraintGraphBuilder(const ModuleName& moduleName, ModulePtr module, TypeArena* arena,
    NotNull<ModuleResolver> moduleResolver, NotNull<SingletonTypes> singletonTypes, NotNull<InternalErrorReporter> ice, const ScopePtr& globalScope, DcrLogger* logger)
    : moduleName(moduleName)
    , module(module)
    , singletonTypes(singletonTypes)
    , arena(arena)
    , rootScope(nullptr)
    , moduleResolver(moduleResolver)
    , ice(ice)
    , globalScope(globalScope)
    , logger(logger)
{
    if (FFlag::DebugLuauLogSolverToJson)
        LUAU_ASSERT(logger);

    LUAU_ASSERT(module);
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

ScopePtr ConstraintGraphBuilder::childScope(AstNode* node, const ScopePtr& parent)
{
    auto scope = std::make_shared<Scope>(parent);
    scopes.emplace_back(node->location, scope);

    scope->returnType = parent->returnType;
    scope->varargPack = parent->varargPack;

    parent->children.push_back(NotNull{scope.get()});
    module->astScopes[node] = scope.get();

    return scope;
}

void ConstraintGraphBuilder::addConstraint(const ScopePtr& scope, const Location& location, ConstraintV cv)
{
    scope->constraints.emplace_back(new Constraint{NotNull{scope.get()}, location, std::move(cv)});
}

void ConstraintGraphBuilder::addConstraint(const ScopePtr& scope, std::unique_ptr<Constraint> c)
{
    scope->constraints.emplace_back(std::move(c));
}

void ConstraintGraphBuilder::visit(AstStatBlock* block)
{
    LUAU_ASSERT(scopes.empty());
    LUAU_ASSERT(rootScope == nullptr);
    ScopePtr scope = std::make_shared<Scope>(globalScope);
    rootScope = scope.get();
    scopes.emplace_back(block->location, scope);
    module->astScopes[block] = NotNull{scope.get()};

    rootScope->returnType = freshTypePack(scope);

    prepopulateGlobalScope(scope, block);

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
            if (scope->privateTypeBindings.count(alias->name.value) != 0)
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
                defnScope = childScope(alias, scope);
            }

            TypeId initialType = freshType(scope);
            TypeFun initialFun = TypeFun{initialType};

            for (const auto& [name, gen] : createGenerics(defnScope, alias->generics))
            {
                initialFun.typeParams.push_back(gen);
                defnScope->privateTypeBindings[name] = TypeFun{gen.ty};
            }

            for (const auto& [name, genPack] : createGenericPacks(defnScope, alias->genericPacks))
            {
                initialFun.typePackParams.push_back(genPack);
                defnScope->privateTypePackBindings[name] = genPack.tp;
            }

            scope->privateTypeBindings[alias->name.value] = std::move(initialFun);
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
    else if (auto s = stat->as<AstStatForIn>())
        visit(scope, s);
    else if (auto s = stat->as<AstStatWhile>())
        visit(scope, s);
    else if (auto s = stat->as<AstStatRepeat>())
        visit(scope, s);
    else if (auto f = stat->as<AstStatFunction>())
        visit(scope, f);
    else if (auto f = stat->as<AstStatLocalFunction>())
        visit(scope, f);
    else if (auto r = stat->as<AstStatReturn>())
        visit(scope, r);
    else if (auto a = stat->as<AstStatAssign>())
        visit(scope, a);
    else if (auto a = stat->as<AstStatCompoundAssign>())
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
        TypeId ty = nullptr;
        Location location = local->location;

        if (local->annotation)
        {
            location = local->annotation->location;
            ty = resolveType(scope, local->annotation, /* topLevel */ true);
        }
        else
            ty = freshType(scope);

        varTypes.push_back(ty);
        scope->bindings[local] = Binding{ty, location};
    }

    for (size_t i = 0; i < local->values.size; ++i)
    {
        AstExpr* value = local->values.data[i];
        const bool hasAnnotation = i < local->vars.size && nullptr != local->vars.data[i]->annotation;

        if (value->is<AstExprConstantNil>())
        {
            // HACK: we leave nil-initialized things floating under the assumption that they will later be populated.
            // See the test TypeInfer/infer_locals_with_nil_value.
            // Better flow awareness should make this obsolete.
        }
        else if (i == local->values.size - 1)
        {
            std::vector<TypeId> expectedTypes;
            if (hasAnnotation)
                expectedTypes.insert(begin(expectedTypes), begin(varTypes) + i, end(varTypes));

            TypePackId exprPack = checkPack(scope, value, expectedTypes);

            if (i < local->vars.size)
            {
                std::vector<TypeId> tailValues{varTypes.begin() + i, varTypes.end()};
                TypePackId tailPack = arena->addTypePack(std::move(tailValues));
                addConstraint(scope, local->location, PackSubtypeConstraint{exprPack, tailPack});
            }
        }
        else
        {
            std::optional<TypeId> expectedType;
            if (hasAnnotation)
                expectedType = varTypes.at(i);

            TypeId exprType = check(scope, value, expectedType);
            if (i < varTypes.size())
                addConstraint(scope, local->location, SubtypeConstraint{varTypes[i], exprType});
        }
    }

    if (local->values.size > 0)
    {
        // To correctly handle 'require', we need to import the exported type bindings into the variable 'namespace'.
        for (size_t i = 0; i < local->values.size && i < local->vars.size; ++i)
        {
            const AstExprCall* call = local->values.data[i]->as<AstExprCall>();
            if (!call)
                continue;

            if (auto maybeRequire = matchRequire(*call))
            {
                AstExpr* require = *maybeRequire;

                if (auto moduleInfo = moduleResolver->resolveModuleInfo(moduleName, *require))
                {
                    const Name name{local->vars.data[i]->name.value};

                    if (ModulePtr module = moduleResolver->getModule(moduleInfo->name))
                        scope->importedTypeBindings[name] = module->getModuleScope()->exportedTypeBindings;
                }
            }
        }
    }
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatFor* for_)
{
    auto checkNumber = [&](AstExpr* expr) {
        if (!expr)
            return;

        TypeId t = check(scope, expr);
        addConstraint(scope, expr->location, SubtypeConstraint{t, singletonTypes->numberType});
    };

    checkNumber(for_->from);
    checkNumber(for_->to);
    checkNumber(for_->step);

    ScopePtr forScope = childScope(for_, scope);
    forScope->bindings[for_->var] = Binding{singletonTypes->numberType, for_->var->location};

    visit(forScope, for_->body);
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatForIn* forIn)
{
    ScopePtr loopScope = childScope(forIn, scope);

    TypePackId iterator = checkPack(scope, forIn->values);

    std::vector<TypeId> variableTypes;
    variableTypes.reserve(forIn->vars.size);
    for (AstLocal* var : forIn->vars)
    {
        TypeId ty = freshType(loopScope);
        loopScope->bindings[var] = Binding{ty, var->location};
        variableTypes.push_back(ty);
    }

    // It is always ok to provide too few variables, so we give this pack a free tail.
    TypePackId variablePack = arena->addTypePack(std::move(variableTypes), arena->addTypePack(FreeTypePack{loopScope.get()}));

    addConstraint(loopScope, getLocation(forIn->values), IterableConstraint{iterator, variablePack});

    visit(loopScope, forIn->body);
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatWhile* while_)
{
    check(scope, while_->condition);

    ScopePtr whileScope = childScope(while_, scope);

    visit(whileScope, while_->body);
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatRepeat* repeat)
{
    ScopePtr repeatScope = childScope(repeat, scope);

    visit(repeatScope, repeat->body);

    // The condition does indeed have access to bindings from within the body of
    // the loop.
    check(repeatScope, repeat->condition);
}

void addConstraints(Constraint* constraint, NotNull<Scope> scope)
{
    scope->constraints.reserve(scope->constraints.size() + scope->constraints.size());

    for (const auto& c : scope->constraints)
        constraint->dependencies.push_back(NotNull{c.get()});

    for (const auto& c : scope->unqueuedConstraints)
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

    NotNull<Scope> constraintScope{sig.signatureScope ? sig.signatureScope.get() : sig.bodyScope.get()};
    std::unique_ptr<Constraint> c =
        std::make_unique<Constraint>(constraintScope, function->name->location, GeneralizationConstraint{functionType, sig.signature});
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

        addConstraint(scope, indexName->location, SubtypeConstraint{containingTableType, prospectiveTableType});
    }
    else if (AstExprError* err = function->name->as<AstExprError>())
    {
        functionType = singletonTypes->errorRecoveryType();
    }

    LUAU_ASSERT(functionType != nullptr);

    checkFunctionBody(sig.bodyScope, function->func);

    NotNull<Scope> constraintScope{sig.signatureScope ? sig.signatureScope.get() : sig.bodyScope.get()};
    std::unique_ptr<Constraint> c =
        std::make_unique<Constraint>(constraintScope, function->name->location, GeneralizationConstraint{functionType, sig.signature});
    addConstraints(c.get(), NotNull(sig.bodyScope.get()));

    addConstraint(scope, std::move(c));
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatReturn* ret)
{
    // At this point, the only way scope->returnType should have anything
    // interesting in it is if the function has an explicit return annotation.
    // If this is the case, then we can expect that the return expression
    // conforms to that.
    std::vector<TypeId> expectedTypes;
    for (TypeId ty : scope->returnType)
        expectedTypes.push_back(ty);

    TypePackId exprTypes = checkPack(scope, ret->list, expectedTypes);
    addConstraint(scope, ret->location, PackSubtypeConstraint{exprTypes, scope->returnType});
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatBlock* block)
{
    ScopePtr innerScope = childScope(block, scope);

    visitBlockWithoutChildScope(innerScope, block);
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatAssign* assign)
{
    TypePackId varPackId = checkPack(scope, assign->vars);
    TypePackId valuePack = checkPack(scope, assign->values);

    addConstraint(scope, assign->location, PackSubtypeConstraint{valuePack, varPackId});
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatCompoundAssign* assign)
{
    // Synthesize A = A op B from A op= B and then build constraints for that instead.

    AstExprBinary exprBinary{assign->location, assign->op, assign->var, assign->value};
    AstExpr* exprBinaryPtr = &exprBinary;

    AstArray<AstExpr*> vars{&assign->var, 1};
    AstArray<AstExpr*> values{&exprBinaryPtr, 1};
    AstStatAssign syntheticAssign{assign->location, vars, values};

    visit(scope, &syntheticAssign);
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatIf* ifStatement)
{
    check(scope, ifStatement->condition);

    ScopePtr thenScope = childScope(ifStatement->thenbody, scope);
    visit(thenScope, ifStatement->thenbody);

    if (ifStatement->elsebody)
    {
        ScopePtr elseScope = childScope(ifStatement->elsebody, scope);
        visit(elseScope, ifStatement->elsebody);
    }
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatTypeAlias* alias)
{
    auto bindingIt = scope->privateTypeBindings.find(alias->name.value);
    ScopePtr* defnIt = astTypeAliasDefiningScopes.find(alias);
    // These will be undefined if the alias was a duplicate definition, in which
    // case we just skip over it.
    if (bindingIt == scope->privateTypeBindings.end() || defnIt == nullptr)
    {
        return;
    }

    ScopePtr resolvingScope = *defnIt;
    TypeId ty = resolveType(resolvingScope, alias->type, /* topLevel */ true);

    if (alias->exported)
    {
        Name typeName(alias->name.value);
        scope->exportedTypeBindings[typeName] = TypeFun{ty};
    }

    LUAU_ASSERT(get<FreeTypeVar>(bindingIt->second.type));

    // Rather than using a subtype constraint, we instead directly bind
    // the free type we generated in the first pass to the resolved type.
    // This prevents a case where you could cause another constraint to
    // bind the free alias type to an unrelated type, causing havoc.
    asMutable(bindingIt->second.type)->ty.emplace<BoundTypeVar>(ty);

    addConstraint(scope, alias->location, NameConstraint{ty, alias->name.value});
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatDeclareGlobal* global)
{
    LUAU_ASSERT(global->type);

    TypeId globalTy = resolveType(scope, global->type);
    Name globalName(global->name.value);

    module->declaredGlobals[globalName] = globalTy;
    scope->bindings[global->name] = Binding{globalTy, global->location};
}

static bool isMetamethod(const Name& name)
{
    return name == "__index" || name == "__newindex" || name == "__call" || name == "__concat" || name == "__unm" || name == "__add" ||
           name == "__sub" || name == "__mul" || name == "__div" || name == "__mod" || name == "__pow" || name == "__tostring" ||
           name == "__metatable" || name == "__eq" || name == "__lt" || name == "__le" || name == "__mode" || name == "__iter" || name == "__len";
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatDeclareClass* declaredClass)
{
    std::optional<TypeId> superTy = std::nullopt;
    if (declaredClass->superName)
    {
        Name superName = Name(declaredClass->superName->value);
        std::optional<TypeFun> lookupType = scope->lookupType(superName);

        if (!lookupType)
        {
            reportError(declaredClass->location, UnknownSymbol{superName, UnknownSymbol::Type});
            return;
        }

        // We don't have generic classes, so this assertion _should_ never be hit.
        LUAU_ASSERT(lookupType->typeParams.size() == 0 && lookupType->typePackParams.size() == 0);
        superTy = lookupType->type;

        if (!get<ClassTypeVar>(follow(*superTy)))
        {
            reportError(declaredClass->location,
                GenericError{format("Cannot use non-class type '%s' as a superclass of class '%s'", superName.c_str(), declaredClass->name.value)});

            return;
        }
    }

    Name className(declaredClass->name.value);

    TypeId classTy = arena->addType(ClassTypeVar(className, {}, superTy, std::nullopt, {}, {}, moduleName));
    ClassTypeVar* ctv = getMutable<ClassTypeVar>(classTy);

    TypeId metaTy = arena->addType(TableTypeVar{TableState::Sealed, scope->level});
    TableTypeVar* metatable = getMutable<TableTypeVar>(metaTy);

    ctv->metatable = metaTy;

    scope->exportedTypeBindings[className] = TypeFun{{}, classTy};

    for (const AstDeclaredClassProp& prop : declaredClass->props)
    {
        Name propName(prop.name.value);
        TypeId propTy = resolveType(scope, prop.ty);

        bool assignToMetatable = isMetamethod(propName);

        // Function types always take 'self', but this isn't reflected in the
        // parsed annotation. Add it here.
        if (prop.isMethod)
        {
            if (FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(propTy))
            {
                ftv->argNames.insert(ftv->argNames.begin(), FunctionArgument{"self", {}});
                ftv->argTypes = arena->addTypePack(TypePack{{classTy}, ftv->argTypes});

                ftv->hasSelf = true;
            }
        }

        if (ctv->props.count(propName) == 0)
        {
            if (assignToMetatable)
                metatable->props[propName] = {propTy};
            else
                ctv->props[propName] = {propTy};
        }
        else
        {
            TypeId currentTy = assignToMetatable ? metatable->props[propName].type : ctv->props[propName].type;

            // We special-case this logic to keep the intersection flat; otherwise we
            // would create a ton of nested intersection types.
            if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(currentTy))
            {
                std::vector<TypeId> options = itv->parts;
                options.push_back(propTy);
                TypeId newItv = arena->addType(IntersectionTypeVar{std::move(options)});

                if (assignToMetatable)
                    metatable->props[propName] = {newItv};
                else
                    ctv->props[propName] = {newItv};
            }
            else if (get<FunctionTypeVar>(currentTy))
            {
                TypeId intersection = arena->addType(IntersectionTypeVar{{currentTy, propTy}});

                if (assignToMetatable)
                    metatable->props[propName] = {intersection};
                else
                    ctv->props[propName] = {intersection};
            }
            else
            {
                reportError(declaredClass->location, GenericError{format("Cannot overload non-function class member '%s'", propName.c_str())});
            }
        }
    }
}

void ConstraintGraphBuilder::visit(const ScopePtr& scope, AstStatDeclareFunction* global)
{

    std::vector<std::pair<Name, GenericTypeDefinition>> generics = createGenerics(scope, global->generics);
    std::vector<std::pair<Name, GenericTypePackDefinition>> genericPacks = createGenericPacks(scope, global->genericPacks);

    std::vector<TypeId> genericTys;
    genericTys.reserve(generics.size());
    for (auto& [name, generic] : generics)
    {
        genericTys.push_back(generic.ty);
        scope->privateTypeBindings[name] = TypeFun{generic.ty};
    }

    std::vector<TypePackId> genericTps;
    genericTps.reserve(genericPacks.size());
    for (auto& [name, generic] : genericPacks)
    {
        genericTps.push_back(generic.tp);
        scope->privateTypePackBindings[name] = generic.tp;
    }

    ScopePtr funScope = scope;
    if (!generics.empty() || !genericPacks.empty())
        funScope = childScope(global, scope);

    TypePackId paramPack = resolveTypePack(funScope, global->params);
    TypePackId retPack = resolveTypePack(funScope, global->retTypes);
    TypeId fnType = arena->addType(FunctionTypeVar{funScope->level, std::move(genericTys), std::move(genericTps), paramPack, retPack});
    FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(fnType);

    ftv->argNames.reserve(global->paramNames.size);
    for (const auto& el : global->paramNames)
        ftv->argNames.push_back(FunctionArgument{el.first.value, el.second});

    Name fnName(global->name.value);

    module->declaredGlobals[fnName] = fnType;
    scope->bindings[global->name] = Binding{fnType, global->location};
}

TypePackId ConstraintGraphBuilder::checkPack(const ScopePtr& scope, AstArray<AstExpr*> exprs, const std::vector<TypeId>& expectedTypes)
{
    std::vector<TypeId> head;
    std::optional<TypePackId> tail;

    for (size_t i = 0; i < exprs.size; ++i)
    {
        AstExpr* expr = exprs.data[i];
        if (i < exprs.size - 1)
        {
            std::optional<TypeId> expectedType;
            if (i < expectedTypes.size())
                expectedType = expectedTypes[i];
            head.push_back(check(scope, expr));
        }
        else
        {
            std::vector<TypeId> expectedTailTypes{begin(expectedTypes) + i, end(expectedTypes)};
            tail = checkPack(scope, expr, expectedTailTypes);
        }
    }

    if (head.empty() && tail)
        return *tail;
    else
        return arena->addTypePack(TypePack{std::move(head), tail});
}

TypePackId ConstraintGraphBuilder::checkPack(const ScopePtr& scope, AstExpr* expr, const std::vector<TypeId>& expectedTypes)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportCodeTooComplex(expr->location);
        return singletonTypes->errorRecoveryTypePack();
    }

    TypePackId result = nullptr;

    if (AstExprCall* call = expr->as<AstExprCall>())
    {
        TypeId fnType = check(scope, call->func);
        const size_t constraintIndex = scope->constraints.size();
        const size_t scopeIndex = scopes.size();

        std::vector<TypeId> args;

        for (AstExpr* arg : call->args)
        {
            args.push_back(check(scope, arg));
        }

        // TODO self

        if (matchSetmetatable(*call))
        {
            LUAU_ASSERT(args.size() == 2);
            TypeId target = args[0];
            TypeId mt = args[1];

            MetatableTypeVar mtv{target, mt};
            TypeId resultTy = arena->addType(mtv);
            result = arena->addTypePack({resultTy});
        }
        else
        {
            const size_t constraintEndIndex = scope->constraints.size();
            const size_t scopeEndIndex = scopes.size();

            astOriginalCallTypes[call->func] = fnType;

            TypeId instantiatedType = arena->addType(BlockedTypeVar{});
            // TODO: How do expectedTypes play into this?  Do they?
            TypePackId rets = arena->addTypePack(BlockedTypePack{});
            FunctionTypeVar ftv(arena->addTypePack(TypePack{args, {}}), rets);
            TypeId inferredFnType = arena->addType(ftv);

            scope->unqueuedConstraints.push_back(
                std::make_unique<Constraint>(NotNull{scope.get()}, call->func->location, InstantiationConstraint{instantiatedType, fnType}));
            NotNull<const Constraint> ic(scope->unqueuedConstraints.back().get());

            scope->unqueuedConstraints.push_back(
                std::make_unique<Constraint>(NotNull{scope.get()}, call->func->location, SubtypeConstraint{inferredFnType, instantiatedType}));
            NotNull<Constraint> sc(scope->unqueuedConstraints.back().get());

            // We force constraints produced by checking function arguments to wait
            // until after we have resolved the constraint on the function itself.
            // This ensures, for instance, that we start inferring the contents of
            // lambdas under the assumption that their arguments and return types
            // will be compatible with the enclosing function call.
            for (size_t ci = constraintIndex; ci < constraintEndIndex; ++ci)
                scope->constraints[ci]->dependencies.push_back(sc);

            for (size_t si = scopeIndex; si < scopeEndIndex; ++si)
            {
                for (auto& c : scopes[si].second->constraints)
                {
                    c->dependencies.push_back(sc);
                }
            }

            addConstraint(scope, call->func->location,
                FunctionCallConstraint{
                    {ic, sc},
                    fnType,
                    rets,
                    call,
                });

            result = rets;
        }
    }
    else if (AstExprVarargs* varargs = expr->as<AstExprVarargs>())
    {
        if (scope->varargPack)
            result = *scope->varargPack;
        else
            result = singletonTypes->errorRecoveryTypePack();
    }
    else
    {
        std::optional<TypeId> expectedType;
        if (!expectedTypes.empty())
            expectedType = expectedTypes[0];
        TypeId t = check(scope, expr, expectedType);
        result = arena->addTypePack({t});
    }

    LUAU_ASSERT(result);
    astTypePacks[expr] = result;
    return result;
}

TypeId ConstraintGraphBuilder::check(const ScopePtr& scope, AstExpr* expr, std::optional<TypeId> expectedType)
{
    RecursionCounter counter{&recursionCount};

    if (recursionCount >= FInt::LuauCheckRecursionLimit)
    {
        reportCodeTooComplex(expr->location);
        return singletonTypes->errorRecoveryType();
    }

    TypeId result = nullptr;

    if (auto group = expr->as<AstExprGroup>())
        result = check(scope, group->expr);
    else if (auto stringExpr = expr->as<AstExprConstantString>())
    {
        if (expectedType)
        {
            const TypeId expectedTy = follow(*expectedType);
            if (get<BlockedTypeVar>(expectedTy) || get<PendingExpansionTypeVar>(expectedTy))
            {
                result = arena->addType(BlockedTypeVar{});
                TypeId singletonType = arena->addType(SingletonTypeVar(StringSingleton{std::string(stringExpr->value.data, stringExpr->value.size)}));
                addConstraint(scope, expr->location, PrimitiveTypeConstraint{result, expectedTy, singletonType, singletonTypes->stringType});
            }
            else if (maybeSingleton(expectedTy))
                result = arena->addType(SingletonTypeVar{StringSingleton{std::string{stringExpr->value.data, stringExpr->value.size}}});
            else
                result = singletonTypes->stringType;
        }
        else
            result = singletonTypes->stringType;
    }
    else if (expr->is<AstExprConstantNumber>())
        result = singletonTypes->numberType;
    else if (auto boolExpr = expr->as<AstExprConstantBool>())
    {
        if (expectedType)
        {
            const TypeId expectedTy = follow(*expectedType);
            const TypeId singletonType = boolExpr->value ? singletonTypes->trueType : singletonTypes->falseType;

            if (get<BlockedTypeVar>(expectedTy) || get<PendingExpansionTypeVar>(expectedTy))
            {
                result = arena->addType(BlockedTypeVar{});
                addConstraint(scope, expr->location, PrimitiveTypeConstraint{result, expectedTy, singletonType, singletonTypes->booleanType});
            }
            else if (maybeSingleton(expectedTy))
                result = singletonType;
            else
                result = singletonTypes->booleanType;
        }
        else
            result = singletonTypes->booleanType;
    }
    else if (expr->is<AstExprConstantNil>())
        result = singletonTypes->nilType;
    else if (auto a = expr->as<AstExprLocal>())
    {
        std::optional<TypeId> ty = scope->lookup(a->local);
        if (ty)
            result = *ty;
        else
            result = singletonTypes->errorRecoveryType(); // FIXME?  Record an error at this point?
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
            result = singletonTypes->errorRecoveryType(); // FIXME?  Record an error at this point?
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
        result = check(scope, table, expectedType);
    else if (auto unary = expr->as<AstExprUnary>())
        result = check(scope, unary);
    else if (auto binary = expr->as<AstExprBinary>())
        result = check(scope, binary);
    else if (auto ifElse = expr->as<AstExprIfElse>())
        result = check(scope, ifElse, expectedType);
    else if (auto typeAssert = expr->as<AstExprTypeAssertion>())
        result = check(scope, typeAssert);
    else if (auto err = expr->as<AstExprError>())
    {
        // Open question: Should we traverse into this?
        result = singletonTypes->errorRecoveryType();
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

    addConstraint(scope, indexName->expr->location, SubtypeConstraint{obj, expectedTableType});

    return result;
}

TypeId ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprIndexExpr* indexExpr)
{
    TypeId obj = check(scope, indexExpr->expr);
    TypeId indexType = check(scope, indexExpr->index);

    TypeId result = freshType(scope);

    TableIndexer indexer{indexType, result};
    TypeId tableType = arena->addType(TableTypeVar{TableTypeVar::Props{}, TableIndexer{indexType, result}, TypeLevel{}, TableState::Free});

    addConstraint(scope, indexExpr->expr->location, SubtypeConstraint{obj, tableType});

    return result;
}

TypeId ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprUnary* unary)
{
    TypeId operandType = check(scope, unary->expr);

    TypeId resultType = arena->addType(BlockedTypeVar{});
    addConstraint(scope, unary->location, UnaryConstraint{unary->op, operandType, resultType});
    return resultType;
}

TypeId ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprBinary* binary)
{
    TypeId leftType = check(scope, binary->left);
    TypeId rightType = check(scope, binary->right);
    switch (binary->op)
    {
    case AstExprBinary::And:
    case AstExprBinary::Or:
    {
        addConstraint(scope, binary->location, SubtypeConstraint{leftType, rightType});
        return leftType;
    }
    case AstExprBinary::Add:
    case AstExprBinary::Sub:
    case AstExprBinary::Mul:
    case AstExprBinary::Div:
    case AstExprBinary::Mod:
    case AstExprBinary::Pow:
    case AstExprBinary::CompareNe:
    case AstExprBinary::CompareEq:
    case AstExprBinary::CompareLt:
    case AstExprBinary::CompareLe:
    case AstExprBinary::CompareGt:
    case AstExprBinary::CompareGe:
    {
        TypeId resultType = arena->addType(BlockedTypeVar{});
        addConstraint(scope, binary->location, BinaryConstraint{binary->op, leftType, rightType, resultType});
        return resultType;
    }
    case AstExprBinary::Concat:
    {
        addConstraint(scope, binary->left->location, SubtypeConstraint{leftType, singletonTypes->stringType});
        addConstraint(scope, binary->right->location, SubtypeConstraint{rightType, singletonTypes->stringType});
        return singletonTypes->stringType;
    }
    default:
        LUAU_ASSERT(0);
    }

    LUAU_ASSERT(0);
    return nullptr;
}

TypeId ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprIfElse* ifElse, std::optional<TypeId> expectedType)
{
    check(scope, ifElse->condition);

    TypeId thenType = check(scope, ifElse->trueExpr, expectedType);
    TypeId elseType = check(scope, ifElse->falseExpr, expectedType);

    if (ifElse->hasElse)
    {
        TypeId resultType = expectedType ? *expectedType : freshType(scope);
        addConstraint(scope, ifElse->trueExpr->location, SubtypeConstraint{thenType, resultType});
        addConstraint(scope, ifElse->falseExpr->location, SubtypeConstraint{elseType, resultType});
        return resultType;
    }

    return thenType;
}

TypeId ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprTypeAssertion* typeAssert)
{
    check(scope, typeAssert->expr);
    return resolveType(scope, typeAssert->annotation);
}

TypeId ConstraintGraphBuilder::check(const ScopePtr& scope, AstExprTable* expr, std::optional<TypeId> expectedType)
{
    TypeId ty = arena->addType(TableTypeVar{});
    TableTypeVar* ttv = getMutable<TableTypeVar>(ty);
    LUAU_ASSERT(ttv);

    auto createIndexer = [this, scope, ttv](const Location& location, TypeId currentIndexType, TypeId currentResultType) {
        if (!ttv->indexer)
        {
            TypeId indexType = this->freshType(scope);
            TypeId resultType = this->freshType(scope);
            ttv->indexer = TableIndexer{indexType, resultType};
        }

        addConstraint(scope, location, SubtypeConstraint{ttv->indexer->indexType, currentIndexType});
        addConstraint(scope, location, SubtypeConstraint{ttv->indexer->indexResultType, currentResultType});
    };

    for (const AstExprTable::Item& item : expr->items)
    {
        std::optional<TypeId> expectedValueType;

        if (item.key && expectedType)
        {
            if (auto stringKey = item.key->as<AstExprConstantString>())
            {
                expectedValueType = arena->addType(BlockedTypeVar{});
                addConstraint(scope, item.value->location, HasPropConstraint{*expectedValueType, *expectedType, stringKey->value.data});
            }
        }

        TypeId itemTy = check(scope, item.value, expectedValueType);
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
                createIndexer(item.key->location, keyTy, itemTy);
            }
        }
        else
        {
            TypeId numberType = singletonTypes->numberType;
            // FIXME?  The location isn't quite right here.  Not sure what is
            // right.
            createIndexer(item.value->location, numberType, itemTy);
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
        signatureScope = childScope(fn, parent);

        // We need to assign returnType before creating bodyScope so that the
        // return type gets propogated to bodyScope.
        returnType = freshTypePack(signatureScope);
        signatureScope->returnType = returnType;

        bodyScope = childScope(fn->body, signatureScope);

        std::vector<std::pair<Name, GenericTypeDefinition>> genericDefinitions = createGenerics(signatureScope, fn->generics);
        std::vector<std::pair<Name, GenericTypePackDefinition>> genericPackDefinitions = createGenericPacks(signatureScope, fn->genericPacks);

        // We do not support default values on function generics, so we only
        // care about the types involved.
        for (const auto& [name, g] : genericDefinitions)
        {
            genericTypes.push_back(g.ty);
            signatureScope->privateTypeBindings[name] = TypeFun{g.ty};
        }

        for (const auto& [name, g] : genericPackDefinitions)
        {
            genericTypePacks.push_back(g.tp);
            signatureScope->privateTypePackBindings[name] = g.tp;
        }
    }
    else
    {
        bodyScope = childScope(fn->body, parent);

        returnType = freshTypePack(bodyScope);
        bodyScope->returnType = returnType;

        // To eliminate the need to branch on hasGenerics below, we say that the
        // signature scope is the body scope when there is no real signature
        // scope.
        signatureScope = bodyScope;
    }

    TypePackId varargPack = nullptr;

    if (fn->vararg)
    {
        if (fn->varargAnnotation)
        {
            TypePackId annotationType = resolveTypePack(signatureScope, fn->varargAnnotation);
            varargPack = annotationType;
        }
        else
        {
            varargPack = arena->freshTypePack(signatureScope.get());
        }

        signatureScope->varargPack = varargPack;
    }
    else
    {
        varargPack = arena->addTypePack(VariadicTypePack{singletonTypes->anyType, /*hidden*/ true});
        // We do not add to signatureScope->varargPack because ... is not valid
        // in functions without an explicit ellipsis.
    }

    LUAU_ASSERT(nullptr != varargPack);

    if (fn->returnAnnotation)
    {
        TypePackId annotatedRetType = resolveTypePack(signatureScope, *fn->returnAnnotation);

        // We bind the annotated type directly here so that, when we need to
        // generate constraints for return types, we have a guarantee that we
        // know the annotated return type already, if one was provided.
        LUAU_ASSERT(get<FreeTypePack>(returnType));
        asMutable(returnType)->ty.emplace<BoundTypePack>(annotatedRetType);
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
            addConstraint(signatureScope, local->annotation->location, SubtypeConstraint{t, argAnnotation});
        }
    }

    // TODO: Vararg annotation.
    // TODO: Preserve argument names in the function's type.

    FunctionTypeVar actualFunction{arena->addTypePack(argTypes, varargPack), returnType};
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
        addConstraint(scope, fn->location, PackSubtypeConstraint{scope->returnType, empty});
    }
}

TypeId ConstraintGraphBuilder::resolveType(const ScopePtr& scope, AstType* ty, bool topLevel)
{
    TypeId result = nullptr;

    if (auto ref = ty->as<AstTypeReference>())
    {
        std::optional<TypeFun> alias = scope->lookupType(ref->name.value);

        if (alias.has_value() || ref->prefix.has_value())
        {
            // If the alias is not generic, we don't need to set up a blocked
            // type and an instantiation constraint.
            if (alias.has_value() && alias->typeParams.empty() && alias->typePackParams.empty())
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

                result = arena->addType(PendingExpansionTypeVar{ref->prefix, ref->name, parameters, packParameters});

                if (topLevel)
                {
                    addConstraint(scope, ty->location, TypeAliasExpansionConstraint{/* target */ result});
                }
            }
        }
        else
        {
            reportError(ty->location, UnknownSymbol{ref->name.value, UnknownSymbol::Context::Type});
            result = singletonTypes->errorRecoveryType();
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

        result = arena->addType(TableTypeVar{props, indexer, scope->level, TableState::Sealed});
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
            signatureScope = childScope(fn, scope);

            std::vector<std::pair<Name, GenericTypeDefinition>> genericDefinitions = createGenerics(signatureScope, fn->generics);
            std::vector<std::pair<Name, GenericTypePackDefinition>> genericPackDefinitions = createGenericPacks(signatureScope, fn->genericPacks);

            for (const auto& [name, g] : genericDefinitions)
            {
                genericTypes.push_back(g.ty);
                signatureScope->privateTypeBindings[name] = TypeFun{g.ty};
            }

            for (const auto& [name, g] : genericPackDefinitions)
            {
                genericTypePacks.push_back(g.tp);
                signatureScope->privateTypePackBindings[name] = g.tp;
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
        result = singletonTypes->errorRecoveryType();
    }
    else
    {
        LUAU_ASSERT(0);
        result = singletonTypes->errorRecoveryType();
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
        if (std::optional<TypePackId> lookup = scope->lookupPack(gen->genericName.value))
        {
            result = *lookup;
        }
        else
        {
            reportError(tp->location, UnknownSymbol{gen->genericName.value, UnknownSymbol::Context::Type});
            result = singletonTypes->errorRecoveryTypePack();
        }
    }
    else
    {
        LUAU_ASSERT(0);
        result = singletonTypes->errorRecoveryTypePack();
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

    addConstraint(scope, location, PackSubtypeConstraint{tp, oneTypePack});

    return typeResult;
}

void ConstraintGraphBuilder::reportError(Location location, TypeErrorData err)
{
    errors.push_back(TypeError{location, moduleName, std::move(err)});

    if (FFlag::DebugLuauLogSolverToJson)
        logger->captureGenerationError(errors.back());
}

void ConstraintGraphBuilder::reportCodeTooComplex(Location location)
{
    errors.push_back(TypeError{location, moduleName, CodeTooComplex{}});

    if (FFlag::DebugLuauLogSolverToJson)
        logger->captureGenerationError(errors.back());
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
