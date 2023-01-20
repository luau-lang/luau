// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"

#include "Luau/Ast.h"
#include "Luau/Frontend.h"
#include "Luau/Symbol.h"
#include "Luau/Common.h"
#include "Luau/ToString.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/ConstraintGraphBuilder.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/Type.h"
#include "Luau/TypeUtils.h"

#include <algorithm>

LUAU_FASTFLAG(LuauUnknownAndNeverType)
LUAU_FASTFLAGVARIABLE(LuauBuiltInMetatableNoBadSynthetic, false)
LUAU_FASTFLAG(LuauReportShadowedTypeAlias)

/** FIXME: Many of these type definitions are not quite completely accurate.
 *
 * Some of them require richer generics than we have.  For instance, we do not yet have a way to talk
 * about a function that takes any number of values, but where each value must have some specific type.
 */

namespace Luau
{

static std::optional<WithPredicate<TypePackId>> magicFunctionSelect(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, WithPredicate<TypePackId> withPredicate);
static std::optional<WithPredicate<TypePackId>> magicFunctionSetMetaTable(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, WithPredicate<TypePackId> withPredicate);
static std::optional<WithPredicate<TypePackId>> magicFunctionAssert(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, WithPredicate<TypePackId> withPredicate);
static std::optional<WithPredicate<TypePackId>> magicFunctionPack(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, WithPredicate<TypePackId> withPredicate);
static std::optional<WithPredicate<TypePackId>> magicFunctionRequire(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, WithPredicate<TypePackId> withPredicate);


static bool dcrMagicFunctionSelect(MagicFunctionCallContext context);
static bool dcrMagicFunctionRequire(MagicFunctionCallContext context);
static bool dcrMagicFunctionPack(MagicFunctionCallContext context);

static std::vector<ConnectiveId> dcrMagicRefinementAssert(const MagicRefinementContext& context);

TypeId makeUnion(TypeArena& arena, std::vector<TypeId>&& types)
{
    return arena.addType(UnionType{std::move(types)});
}

TypeId makeIntersection(TypeArena& arena, std::vector<TypeId>&& types)
{
    return arena.addType(IntersectionType{std::move(types)});
}

TypeId makeOption(Frontend& frontend, TypeArena& arena, TypeId t)
{
    return makeUnion(arena, {frontend.typeChecker.nilType, t});
}

TypeId makeOption(TypeChecker& typeChecker, TypeArena& arena, TypeId t)
{
    return makeUnion(arena, {typeChecker.nilType, t});
}

TypeId makeFunction(
    TypeArena& arena, std::optional<TypeId> selfType, std::initializer_list<TypeId> paramTypes, std::initializer_list<TypeId> retTypes)
{
    return makeFunction(arena, selfType, {}, {}, paramTypes, {}, retTypes);
}

TypeId makeFunction(TypeArena& arena, std::optional<TypeId> selfType, std::initializer_list<TypeId> generics,
    std::initializer_list<TypePackId> genericPacks, std::initializer_list<TypeId> paramTypes, std::initializer_list<TypeId> retTypes)
{
    return makeFunction(arena, selfType, generics, genericPacks, paramTypes, {}, retTypes);
}

TypeId makeFunction(TypeArena& arena, std::optional<TypeId> selfType, std::initializer_list<TypeId> paramTypes,
    std::initializer_list<std::string> paramNames, std::initializer_list<TypeId> retTypes)
{
    return makeFunction(arena, selfType, {}, {}, paramTypes, paramNames, retTypes);
}

TypeId makeFunction(TypeArena& arena, std::optional<TypeId> selfType, std::initializer_list<TypeId> generics,
    std::initializer_list<TypePackId> genericPacks, std::initializer_list<TypeId> paramTypes, std::initializer_list<std::string> paramNames,
    std::initializer_list<TypeId> retTypes)
{
    std::vector<TypeId> params;
    if (selfType)
        params.push_back(*selfType);
    for (auto&& p : paramTypes)
        params.push_back(p);

    TypePackId paramPack = arena.addTypePack(std::move(params));
    TypePackId retPack = arena.addTypePack(std::vector<TypeId>(retTypes));
    FunctionType ftv{generics, genericPacks, paramPack, retPack, {}, selfType.has_value()};

    if (selfType)
        ftv.argNames.push_back(Luau::FunctionArgument{"self", {}});

    if (paramNames.size() != 0)
    {
        for (auto&& p : paramNames)
            ftv.argNames.push_back(Luau::FunctionArgument{std::move(p), {}});
    }
    else if (selfType)
    {
        // If argument names were not provided, but we have already added a name for 'self' argument, we have to fill remaining slots as well
        for (size_t i = 0; i < paramTypes.size(); i++)
            ftv.argNames.push_back(std::nullopt);
    }

    return arena.addType(std::move(ftv));
}

void attachMagicFunction(TypeId ty, MagicFunction fn)
{
    if (auto ftv = getMutable<FunctionType>(ty))
        ftv->magicFunction = fn;
    else
        LUAU_ASSERT(!"Got a non functional type");
}

void attachDcrMagicFunction(TypeId ty, DcrMagicFunction fn)
{
    if (auto ftv = getMutable<FunctionType>(ty))
        ftv->dcrMagicFunction = fn;
    else
        LUAU_ASSERT(!"Got a non functional type");
}

void attachDcrMagicRefinement(TypeId ty, DcrMagicRefinement fn)
{
    if (auto ftv = getMutable<FunctionType>(ty))
        ftv->dcrMagicRefinement = fn;
    else
        LUAU_ASSERT(!"Got a non functional type");
}

Property makeProperty(TypeId ty, std::optional<std::string> documentationSymbol)
{
    return {
        /* type */ ty,
        /* deprecated */ false,
        /* deprecatedSuggestion */ {},
        /* location */ std::nullopt,
        /* tags */ {},
        documentationSymbol,
    };
}

void addGlobalBinding(Frontend& frontend, const std::string& name, TypeId ty, const std::string& packageName)
{
    addGlobalBinding(frontend, frontend.getGlobalScope(), name, ty, packageName);
}

void addGlobalBinding(TypeChecker& typeChecker, const ScopePtr& scope, const std::string& name, TypeId ty, const std::string& packageName);

void addGlobalBinding(TypeChecker& typeChecker, const std::string& name, TypeId ty, const std::string& packageName)
{
    addGlobalBinding(typeChecker, typeChecker.globalScope, name, ty, packageName);
}

void addGlobalBinding(Frontend& frontend, const std::string& name, Binding binding)
{
    addGlobalBinding(frontend, frontend.getGlobalScope(), name, binding);
}

void addGlobalBinding(TypeChecker& typeChecker, const std::string& name, Binding binding)
{
    addGlobalBinding(typeChecker, typeChecker.globalScope, name, binding);
}

void addGlobalBinding(Frontend& frontend, const ScopePtr& scope, const std::string& name, TypeId ty, const std::string& packageName)
{
    std::string documentationSymbol = packageName + "/global/" + name;
    addGlobalBinding(frontend, scope, name, Binding{ty, Location{}, {}, {}, documentationSymbol});
}

void addGlobalBinding(TypeChecker& typeChecker, const ScopePtr& scope, const std::string& name, TypeId ty, const std::string& packageName)
{
    std::string documentationSymbol = packageName + "/global/" + name;
    addGlobalBinding(typeChecker, scope, name, Binding{ty, Location{}, {}, {}, documentationSymbol});
}

void addGlobalBinding(Frontend& frontend, const ScopePtr& scope, const std::string& name, Binding binding)
{
    addGlobalBinding(frontend.typeChecker, scope, name, binding);
}

void addGlobalBinding(TypeChecker& typeChecker, const ScopePtr& scope, const std::string& name, Binding binding)
{
    scope->bindings[typeChecker.globalNames.names->getOrAdd(name.c_str())] = binding;
}

std::optional<Binding> tryGetGlobalBinding(TypeChecker& typeChecker, const std::string& name)
{
    AstName astName = typeChecker.globalNames.names->getOrAdd(name.c_str());
    auto it = typeChecker.globalScope->bindings.find(astName);
    if (it != typeChecker.globalScope->bindings.end())
        return it->second;

    return std::nullopt;
}

TypeId getGlobalBinding(TypeChecker& typeChecker, const std::string& name)
{
    auto t = tryGetGlobalBinding(typeChecker, name);
    LUAU_ASSERT(t.has_value());
    return t->typeId;
}

TypeId getGlobalBinding(Frontend& frontend, const std::string& name)
{
    return getGlobalBinding(frontend.typeChecker, name);
}

std::optional<Binding> tryGetGlobalBinding(Frontend& frontend, const std::string& name)
{
    return tryGetGlobalBinding(frontend.typeChecker, name);
}

Binding* tryGetGlobalBindingRef(TypeChecker& typeChecker, const std::string& name)
{
    AstName astName = typeChecker.globalNames.names->get(name.c_str());
    if (astName == AstName())
        return nullptr;

    auto it = typeChecker.globalScope->bindings.find(astName);
    if (it != typeChecker.globalScope->bindings.end())
        return &it->second;

    return nullptr;
}

void assignPropDocumentationSymbols(TableType::Props& props, const std::string& baseName)
{
    for (auto& [name, prop] : props)
    {
        prop.documentationSymbol = baseName + "." + name;
    }
}

void registerBuiltinTypes(Frontend& frontend)
{
    frontend.getGlobalScope()->addBuiltinTypeBinding("any", TypeFun{{}, frontend.builtinTypes->anyType});
    frontend.getGlobalScope()->addBuiltinTypeBinding("nil", TypeFun{{}, frontend.builtinTypes->nilType});
    frontend.getGlobalScope()->addBuiltinTypeBinding("number", TypeFun{{}, frontend.builtinTypes->numberType});
    frontend.getGlobalScope()->addBuiltinTypeBinding("string", TypeFun{{}, frontend.builtinTypes->stringType});
    frontend.getGlobalScope()->addBuiltinTypeBinding("boolean", TypeFun{{}, frontend.builtinTypes->booleanType});
    frontend.getGlobalScope()->addBuiltinTypeBinding("thread", TypeFun{{}, frontend.builtinTypes->threadType});
    if (FFlag::LuauUnknownAndNeverType)
    {
        frontend.getGlobalScope()->addBuiltinTypeBinding("unknown", TypeFun{{}, frontend.builtinTypes->unknownType});
        frontend.getGlobalScope()->addBuiltinTypeBinding("never", TypeFun{{}, frontend.builtinTypes->neverType});
    }
}

void registerBuiltinGlobals(TypeChecker& typeChecker)
{
    LUAU_ASSERT(!typeChecker.globalTypes.types.isFrozen());
    LUAU_ASSERT(!typeChecker.globalTypes.typePacks.isFrozen());

    TypeId nilType = typeChecker.nilType;

    TypeArena& arena = typeChecker.globalTypes;
    NotNull<BuiltinTypes> builtinTypes = typeChecker.builtinTypes;

    LoadDefinitionFileResult loadResult = Luau::loadDefinitionFile(typeChecker, typeChecker.globalScope, getBuiltinDefinitionSource(), "@luau");
    LUAU_ASSERT(loadResult.success);

    TypeId genericK = arena.addType(GenericType{"K"});
    TypeId genericV = arena.addType(GenericType{"V"});
    TypeId mapOfKtoV = arena.addType(TableType{{}, TableIndexer(genericK, genericV), typeChecker.globalScope->level, TableState::Generic});

    std::optional<TypeId> stringMetatableTy = getMetatable(builtinTypes->stringType, builtinTypes);
    LUAU_ASSERT(stringMetatableTy);
    const TableType* stringMetatableTable = get<TableType>(follow(*stringMetatableTy));
    LUAU_ASSERT(stringMetatableTable);

    auto it = stringMetatableTable->props.find("__index");
    LUAU_ASSERT(it != stringMetatableTable->props.end());

    addGlobalBinding(typeChecker, "string", it->second.type, "@luau");

    // next<K, V>(t: Table<K, V>, i: K?) -> (K?, V)
    TypePackId nextArgsTypePack = arena.addTypePack(TypePack{{mapOfKtoV, makeOption(typeChecker, arena, genericK)}});
    TypePackId nextRetsTypePack = arena.addTypePack(TypePack{{makeOption(typeChecker, arena, genericK), genericV}});
    addGlobalBinding(typeChecker, "next", arena.addType(FunctionType{{genericK, genericV}, {}, nextArgsTypePack, nextRetsTypePack}), "@luau");

    TypePackId pairsArgsTypePack = arena.addTypePack({mapOfKtoV});

    TypeId pairsNext = arena.addType(FunctionType{nextArgsTypePack, nextRetsTypePack});
    TypePackId pairsReturnTypePack = arena.addTypePack(TypePack{{pairsNext, mapOfKtoV, nilType}});

    // pairs<K, V>(t: Table<K, V>) -> ((Table<K, V>, K?) -> (K, V), Table<K, V>, nil)
    addGlobalBinding(typeChecker, "pairs", arena.addType(FunctionType{{genericK, genericV}, {}, pairsArgsTypePack, pairsReturnTypePack}), "@luau");

    TypeId genericMT = arena.addType(GenericType{"MT"});

    TableType tab{TableState::Generic, typeChecker.globalScope->level};
    TypeId tabTy = arena.addType(tab);

    TypeId tableMetaMT = arena.addType(MetatableType{tabTy, genericMT});

    addGlobalBinding(typeChecker, "getmetatable", makeFunction(arena, std::nullopt, {genericMT}, {}, {tableMetaMT}, {genericMT}), "@luau");

    // clang-format off
    // setmetatable<T: {}, MT>(T, MT) -> { @metatable MT, T }
    addGlobalBinding(typeChecker, "setmetatable",
        arena.addType(
            FunctionType{
                {genericMT},
                {},
                arena.addTypePack(TypePack{{FFlag::LuauUnknownAndNeverType ? tabTy : tableMetaMT, genericMT}}),
                arena.addTypePack(TypePack{{tableMetaMT}})
            }
        ), "@luau"
    );
    // clang-format on

    for (const auto& pair : typeChecker.globalScope->bindings)
    {
        persist(pair.second.typeId);

        if (TableType* ttv = getMutable<TableType>(pair.second.typeId))
        {
            if (!ttv->name)
                ttv->name = "typeof(" + toString(pair.first) + ")";
        }
    }

    attachMagicFunction(getGlobalBinding(typeChecker, "assert"), magicFunctionAssert);
    attachMagicFunction(getGlobalBinding(typeChecker, "setmetatable"), magicFunctionSetMetaTable);
    attachMagicFunction(getGlobalBinding(typeChecker, "select"), magicFunctionSelect);
    attachDcrMagicFunction(getGlobalBinding(typeChecker, "select"), dcrMagicFunctionSelect);

    if (TableType* ttv = getMutable<TableType>(getGlobalBinding(typeChecker, "table")))
    {
        // tabTy is a generic table type which we can't express via declaration syntax yet
        ttv->props["freeze"] = makeProperty(makeFunction(arena, std::nullopt, {tabTy}, {tabTy}), "@luau/global/table.freeze");
        ttv->props["clone"] = makeProperty(makeFunction(arena, std::nullopt, {tabTy}, {tabTy}), "@luau/global/table.clone");

        attachMagicFunction(ttv->props["pack"].type, magicFunctionPack);
        attachDcrMagicFunction(ttv->props["pack"].type, dcrMagicFunctionPack);
    }

    attachMagicFunction(getGlobalBinding(typeChecker, "require"), magicFunctionRequire);
    attachDcrMagicFunction(getGlobalBinding(typeChecker, "require"), dcrMagicFunctionRequire);
}

void registerBuiltinGlobals(Frontend& frontend)
{
    LUAU_ASSERT(!frontend.globalTypes.types.isFrozen());
    LUAU_ASSERT(!frontend.globalTypes.typePacks.isFrozen());

    if (FFlag::LuauReportShadowedTypeAlias)
        registerBuiltinTypes(frontend);

    TypeArena& arena = frontend.globalTypes;
    NotNull<BuiltinTypes> builtinTypes = frontend.builtinTypes;

    LoadDefinitionFileResult loadResult = frontend.loadDefinitionFile(getBuiltinDefinitionSource(), "@luau");
    LUAU_ASSERT(loadResult.success);

    TypeId genericK = arena.addType(GenericType{"K"});
    TypeId genericV = arena.addType(GenericType{"V"});
    TypeId mapOfKtoV = arena.addType(TableType{{}, TableIndexer(genericK, genericV), frontend.getGlobalScope()->level, TableState::Generic});

    std::optional<TypeId> stringMetatableTy = getMetatable(builtinTypes->stringType, builtinTypes);
    LUAU_ASSERT(stringMetatableTy);
    const TableType* stringMetatableTable = get<TableType>(follow(*stringMetatableTy));
    LUAU_ASSERT(stringMetatableTable);

    auto it = stringMetatableTable->props.find("__index");
    LUAU_ASSERT(it != stringMetatableTable->props.end());

    addGlobalBinding(frontend, "string", it->second.type, "@luau");

    // next<K, V>(t: Table<K, V>, i: K?) -> (K?, V)
    TypePackId nextArgsTypePack = arena.addTypePack(TypePack{{mapOfKtoV, makeOption(frontend, arena, genericK)}});
    TypePackId nextRetsTypePack = arena.addTypePack(TypePack{{makeOption(frontend, arena, genericK), genericV}});
    addGlobalBinding(frontend, "next", arena.addType(FunctionType{{genericK, genericV}, {}, nextArgsTypePack, nextRetsTypePack}), "@luau");

    TypePackId pairsArgsTypePack = arena.addTypePack({mapOfKtoV});

    TypeId pairsNext = arena.addType(FunctionType{nextArgsTypePack, nextRetsTypePack});
    TypePackId pairsReturnTypePack = arena.addTypePack(TypePack{{pairsNext, mapOfKtoV, frontend.builtinTypes->nilType}});

    // pairs<K, V>(t: Table<K, V>) -> ((Table<K, V>, K?) -> (K?, V), Table<K, V>, nil)
    addGlobalBinding(frontend, "pairs", arena.addType(FunctionType{{genericK, genericV}, {}, pairsArgsTypePack, pairsReturnTypePack}), "@luau");

    TypeId genericMT = arena.addType(GenericType{"MT"});

    TableType tab{TableState::Generic, frontend.getGlobalScope()->level};
    TypeId tabTy = arena.addType(tab);

    TypeId tableMetaMT = arena.addType(MetatableType{tabTy, genericMT});

    addGlobalBinding(frontend, "getmetatable", makeFunction(arena, std::nullopt, {genericMT}, {}, {tableMetaMT}, {genericMT}), "@luau");

    // clang-format off
    // setmetatable<T: {}, MT>(T, MT) -> { @metatable MT, T }
    addGlobalBinding(frontend, "setmetatable",
        arena.addType(
            FunctionType{
                {genericMT},
                {},
                arena.addTypePack(TypePack{{FFlag::LuauUnknownAndNeverType ? tabTy : tableMetaMT, genericMT}}),
                arena.addTypePack(TypePack{{tableMetaMT}})
            }
        ), "@luau"
    );
    // clang-format on

    for (const auto& pair : frontend.getGlobalScope()->bindings)
    {
        persist(pair.second.typeId);

        if (TableType* ttv = getMutable<TableType>(pair.second.typeId))
        {
            if (!ttv->name)
                ttv->name = "typeof(" + toString(pair.first) + ")";
        }
    }

    attachMagicFunction(getGlobalBinding(frontend, "assert"), magicFunctionAssert);
    attachDcrMagicRefinement(getGlobalBinding(frontend, "assert"), dcrMagicRefinementAssert);
    attachMagicFunction(getGlobalBinding(frontend, "setmetatable"), magicFunctionSetMetaTable);
    attachMagicFunction(getGlobalBinding(frontend, "select"), magicFunctionSelect);
    attachDcrMagicFunction(getGlobalBinding(frontend, "select"), dcrMagicFunctionSelect);

    if (TableType* ttv = getMutable<TableType>(getGlobalBinding(frontend, "table")))
    {
        // tabTy is a generic table type which we can't express via declaration syntax yet
        ttv->props["freeze"] = makeProperty(makeFunction(arena, std::nullopt, {tabTy}, {tabTy}), "@luau/global/table.freeze");
        ttv->props["clone"] = makeProperty(makeFunction(arena, std::nullopt, {tabTy}, {tabTy}), "@luau/global/table.clone");

        attachMagicFunction(ttv->props["pack"].type, magicFunctionPack);
    }

    attachMagicFunction(getGlobalBinding(frontend, "require"), magicFunctionRequire);
    attachDcrMagicFunction(getGlobalBinding(frontend, "require"), dcrMagicFunctionRequire);
}

static std::optional<WithPredicate<TypePackId>> magicFunctionSelect(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, WithPredicate<TypePackId> withPredicate)
{
    auto [paramPack, _predicates] = withPredicate;

    (void)scope;

    if (expr.args.size <= 0)
    {
        typechecker.reportError(TypeError{expr.location, GenericError{"select should take 1 or more arguments"}});
        return std::nullopt;
    }

    AstExpr* arg1 = expr.args.data[0];
    if (AstExprConstantNumber* num = arg1->as<AstExprConstantNumber>())
    {
        const auto& [v, tail] = flatten(paramPack);

        int offset = int(num->value);
        if (offset > 0)
        {
            if (size_t(offset) < v.size())
            {
                std::vector<TypeId> result(v.begin() + offset, v.end());
                return WithPredicate<TypePackId>{typechecker.currentModule->internalTypes.addTypePack(TypePack{std::move(result), tail})};
            }
            else if (tail)
                return WithPredicate<TypePackId>{*tail};
        }

        typechecker.reportError(TypeError{arg1->location, GenericError{"bad argument #1 to select (index out of range)"}});
    }
    else if (AstExprConstantString* str = arg1->as<AstExprConstantString>())
    {
        if (str->value.size == 1 && str->value.data[0] == '#')
            return WithPredicate<TypePackId>{typechecker.currentModule->internalTypes.addTypePack({typechecker.numberType})};
    }

    return std::nullopt;
}

static bool dcrMagicFunctionSelect(MagicFunctionCallContext context)
{
    if (context.callSite->args.size <= 0)
    {
        context.solver->reportError(TypeError{context.callSite->location, GenericError{"select should take 1 or more arguments"}});
        return false;
    }

    AstExpr* arg1 = context.callSite->args.data[0];

    if (AstExprConstantNumber* num = arg1->as<AstExprConstantNumber>())
    {
        const auto& [v, tail] = flatten(context.arguments);

        int offset = int(num->value);
        if (offset > 0)
        {
            if (size_t(offset) < v.size())
            {
                std::vector<TypeId> res(v.begin() + offset, v.end());
                TypePackId resTypePack = context.solver->arena->addTypePack({std::move(res), tail});
                asMutable(context.result)->ty.emplace<BoundTypePack>(resTypePack);
            }
            else if (tail)
                asMutable(context.result)->ty.emplace<BoundTypePack>(*tail);

            return true;
        }

        return false;
    }

    if (AstExprConstantString* str = arg1->as<AstExprConstantString>())
    {
        if (str->value.size == 1 && str->value.data[0] == '#')
        {
            TypePackId numberTypePack = context.solver->arena->addTypePack({context.solver->builtinTypes->numberType});
            asMutable(context.result)->ty.emplace<BoundTypePack>(numberTypePack);
            return true;
        }
    }

    return false;
}

static std::optional<WithPredicate<TypePackId>> magicFunctionSetMetaTable(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, WithPredicate<TypePackId> withPredicate)
{
    auto [paramPack, _predicates] = withPredicate;

    if (FFlag::LuauUnknownAndNeverType)
    {
        if (size(paramPack) < 2 && finite(paramPack))
            return std::nullopt;
    }

    TypeArena& arena = typechecker.currentModule->internalTypes;

    std::vector<TypeId> expectedArgs = typechecker.unTypePack(scope, paramPack, 2, expr.location);

    TypeId target = follow(expectedArgs[0]);
    TypeId mt = follow(expectedArgs[1]);

    if (FFlag::LuauUnknownAndNeverType)
    {
        typechecker.tablify(target);
        typechecker.tablify(mt);
    }

    if (const auto& tab = get<TableType>(target))
    {
        if (target->persistent)
        {
            typechecker.reportError(TypeError{expr.location, CannotExtendTable{target, CannotExtendTable::Metatable}});
        }
        else
        {
            if (!FFlag::LuauUnknownAndNeverType)
                typechecker.tablify(mt);

            const TableType* mtTtv = get<TableType>(mt);
            MetatableType mtv{target, mt};
            if ((tab->name || tab->syntheticName) && (mtTtv && (mtTtv->name || mtTtv->syntheticName)))
            {
                std::string tableName = tab->name ? *tab->name : *tab->syntheticName;
                std::string metatableName = mtTtv->name ? *mtTtv->name : *mtTtv->syntheticName;

                if (tableName == metatableName)
                    mtv.syntheticName = tableName;
                else if (!FFlag::LuauBuiltInMetatableNoBadSynthetic)
                    mtv.syntheticName = "{ @metatable: " + metatableName + ", " + tableName + " }";
            }

            TypeId mtTy = arena.addType(mtv);

            if (expr.args.size < 1)
            {
                if (FFlag::LuauUnknownAndNeverType)
                    return std::nullopt;
                else
                    return WithPredicate<TypePackId>{};
            }

            if (!expr.self)
            {
                AstExpr* targetExpr = expr.args.data[0];
                if (AstExprLocal* targetLocal = targetExpr->as<AstExprLocal>())
                {
                    const Name targetName(targetLocal->local->name.value);
                    scope->bindings[targetLocal->local] = Binding{mtTy, expr.location};
                }
            }

            return WithPredicate<TypePackId>{arena.addTypePack({mtTy})};
        }
    }
    else if (get<AnyType>(target) || get<ErrorType>(target) || isTableIntersection(target))
    {
    }
    else
    {
        typechecker.reportError(TypeError{expr.location, GenericError{"setmetatable should take a table"}});
    }

    return WithPredicate<TypePackId>{arena.addTypePack({target})};
}

static std::optional<WithPredicate<TypePackId>> magicFunctionAssert(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, WithPredicate<TypePackId> withPredicate)
{
    auto [paramPack, predicates] = withPredicate;

    TypeArena& arena = typechecker.currentModule->internalTypes;

    auto [head, tail] = flatten(paramPack);
    if (head.empty() && tail)
    {
        std::optional<TypeId> fst = first(*tail);
        if (!fst)
            return WithPredicate<TypePackId>{paramPack};
        head.push_back(*fst);
    }

    typechecker.resolve(predicates, scope, true);

    if (head.size() > 0)
    {
        auto [ty, ok] = typechecker.pickTypesFromSense(head[0], true, typechecker.builtinTypes->nilType);
        if (FFlag::LuauUnknownAndNeverType)
        {
            if (get<NeverType>(*ty))
                head = {*ty};
            else
                head[0] = *ty;
        }
        else
        {
            if (!ty)
                head = {typechecker.nilType};
            else
                head[0] = *ty;
        }
    }

    return WithPredicate<TypePackId>{arena.addTypePack(TypePack{std::move(head), tail})};
}

static std::vector<ConnectiveId> dcrMagicRefinementAssert(const MagicRefinementContext& ctx)
{
    if (ctx.argumentConnectives.empty())
        return {};

    ctx.cgb->applyRefinements(ctx.scope, ctx.callSite->location, ctx.argumentConnectives[0]);
    return {};
}

static std::optional<WithPredicate<TypePackId>> magicFunctionPack(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, WithPredicate<TypePackId> withPredicate)
{
    auto [paramPack, _predicates] = withPredicate;

    TypeArena& arena = typechecker.currentModule->internalTypes;

    const auto& [paramTypes, paramTail] = flatten(paramPack);

    std::vector<TypeId> options;
    options.reserve(paramTypes.size());
    for (auto type : paramTypes)
        options.push_back(type);

    if (paramTail)
    {
        if (const VariadicTypePack* vtp = get<VariadicTypePack>(*paramTail))
            options.push_back(vtp->ty);
    }

    options = reduceUnion(options);

    // table.pack()         -> {| n: number, [number]: nil |}
    // table.pack(1)        -> {| n: number, [number]: number |}
    // table.pack(1, "foo") -> {| n: number, [number]: number | string |}
    TypeId result = nullptr;
    if (options.empty())
        result = typechecker.nilType;
    else if (options.size() == 1)
        result = options[0];
    else
        result = arena.addType(UnionType{std::move(options)});

    TypeId packedTable =
        arena.addType(TableType{{{"n", {typechecker.numberType}}}, TableIndexer(typechecker.numberType, result), scope->level, TableState::Sealed});

    return WithPredicate<TypePackId>{arena.addTypePack({packedTable})};
}

static bool dcrMagicFunctionPack(MagicFunctionCallContext context)
{

    TypeArena* arena = context.solver->arena;

    const auto& [paramTypes, paramTail] = flatten(context.arguments);

    std::vector<TypeId> options;
    options.reserve(paramTypes.size());
    for (auto type : paramTypes)
        options.push_back(type);

    if (paramTail)
    {
        if (const VariadicTypePack* vtp = get<VariadicTypePack>(*paramTail))
            options.push_back(vtp->ty);
    }

    options = reduceUnion(options);

    // table.pack()         -> {| n: number, [number]: nil |}
    // table.pack(1)        -> {| n: number, [number]: number |}
    // table.pack(1, "foo") -> {| n: number, [number]: number | string |}
    TypeId result = nullptr;
    if (options.empty())
        result = context.solver->builtinTypes->nilType;
    else if (options.size() == 1)
        result = options[0];
    else
        result = arena->addType(UnionType{std::move(options)});

    TypeId numberType = context.solver->builtinTypes->numberType;
    TypeId packedTable = arena->addType(TableType{{{"n", {numberType}}}, TableIndexer(numberType, result), {}, TableState::Sealed});

    TypePackId tableTypePack = arena->addTypePack({packedTable});
    asMutable(context.result)->ty.emplace<BoundTypePack>(tableTypePack);

    return true;
}

static bool checkRequirePath(TypeChecker& typechecker, AstExpr* expr)
{
    // require(foo.parent.bar) will technically work, but it depends on legacy goop that
    // Luau does not and could not support without a bunch of work.  It's deprecated anyway, so
    // we'll warn here if we see it.
    bool good = true;
    AstExprIndexName* indexExpr = expr->as<AstExprIndexName>();

    while (indexExpr)
    {
        if (indexExpr->index == "parent")
        {
            typechecker.reportError(indexExpr->indexLocation, DeprecatedApiUsed{"parent", "Parent"});
            good = false;
        }

        indexExpr = indexExpr->expr->as<AstExprIndexName>();
    }

    return good;
}

static std::optional<WithPredicate<TypePackId>> magicFunctionRequire(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, WithPredicate<TypePackId> withPredicate)
{
    TypeArena& arena = typechecker.currentModule->internalTypes;

    if (expr.args.size != 1)
    {
        typechecker.reportError(TypeError{expr.location, GenericError{"require takes 1 argument"}});
        return std::nullopt;
    }

    if (!checkRequirePath(typechecker, expr.args.data[0]))
        return std::nullopt;

    if (auto moduleInfo = typechecker.resolver->resolveModuleInfo(typechecker.currentModuleName, expr))
        return WithPredicate<TypePackId>{arena.addTypePack({typechecker.checkRequire(scope, *moduleInfo, expr.location)})};

    return std::nullopt;
}

static bool checkRequirePathDcr(NotNull<ConstraintSolver> solver, AstExpr* expr)
{
    // require(foo.parent.bar) will technically work, but it depends on legacy goop that
    // Luau does not and could not support without a bunch of work.  It's deprecated anyway, so
    // we'll warn here if we see it.
    bool good = true;
    AstExprIndexName* indexExpr = expr->as<AstExprIndexName>();

    while (indexExpr)
    {
        if (indexExpr->index == "parent")
        {
            solver->reportError(DeprecatedApiUsed{"parent", "Parent"}, indexExpr->indexLocation);
            good = false;
        }

        indexExpr = indexExpr->expr->as<AstExprIndexName>();
    }

    return good;
}

static bool dcrMagicFunctionRequire(MagicFunctionCallContext context)
{
    if (context.callSite->args.size != 1)
    {
        context.solver->reportError(GenericError{"require takes 1 argument"}, context.callSite->location);
        return false;
    }

    if (!checkRequirePathDcr(context.solver, context.callSite->args.data[0]))
        return false;

    if (auto moduleInfo = context.solver->moduleResolver->resolveModuleInfo(context.solver->currentModuleName, *context.callSite))
    {
        TypeId moduleType = context.solver->resolveModule(*moduleInfo, context.callSite->location);
        TypePackId moduleResult = context.solver->arena->addTypePack({moduleType});
        asMutable(context.result)->ty.emplace<BoundTypePack>(moduleResult);

        return true;
    }

    return false;
}

} // namespace Luau
