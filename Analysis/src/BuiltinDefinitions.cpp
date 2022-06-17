// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"

#include "Luau/Frontend.h"
#include "Luau/Symbol.h"
#include "Luau/Common.h"
#include "Luau/ToString.h"

#include <algorithm>

LUAU_FASTFLAGVARIABLE(LuauSetMetaTableArgsCheck, false)

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

TypeId makeUnion(TypeArena& arena, std::vector<TypeId>&& types)
{
    return arena.addType(UnionTypeVar{std::move(types)});
}

TypeId makeIntersection(TypeArena& arena, std::vector<TypeId>&& types)
{
    return arena.addType(IntersectionTypeVar{std::move(types)});
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
    FunctionTypeVar ftv{generics, genericPacks, paramPack, retPack, {}, selfType.has_value()};

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
    if (auto ftv = getMutable<FunctionTypeVar>(ty))
        ftv->magicFunction = fn;
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

void addGlobalBinding(TypeChecker& typeChecker, const std::string& name, TypeId ty, const std::string& packageName)
{
    addGlobalBinding(typeChecker, typeChecker.globalScope, name, ty, packageName);
}

void addGlobalBinding(TypeChecker& typeChecker, const std::string& name, Binding binding)
{
    addGlobalBinding(typeChecker, typeChecker.globalScope, name, binding);
}

void addGlobalBinding(TypeChecker& typeChecker, const ScopePtr& scope, const std::string& name, TypeId ty, const std::string& packageName)
{
    std::string documentationSymbol = packageName + "/global/" + name;
    addGlobalBinding(typeChecker, scope, name, Binding{ty, Location{}, {}, {}, documentationSymbol});
}

void addGlobalBinding(TypeChecker& typeChecker, const ScopePtr& scope, const std::string& name, Binding binding)
{
    scope->bindings[typeChecker.globalNames.names->getOrAdd(name.c_str())] = binding;
}

TypeId getGlobalBinding(TypeChecker& typeChecker, const std::string& name)
{
    auto t = tryGetGlobalBinding(typeChecker, name);
    LUAU_ASSERT(t.has_value());
    return t->typeId;
}

std::optional<Binding> tryGetGlobalBinding(TypeChecker& typeChecker, const std::string& name)
{
    AstName astName = typeChecker.globalNames.names->getOrAdd(name.c_str());
    auto it = typeChecker.globalScope->bindings.find(astName);
    if (it != typeChecker.globalScope->bindings.end())
        return it->second;

    return std::nullopt;
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

void assignPropDocumentationSymbols(TableTypeVar::Props& props, const std::string& baseName)
{
    for (auto& [name, prop] : props)
    {
        prop.documentationSymbol = baseName + "." + name;
    }
}

void registerBuiltinTypes(TypeChecker& typeChecker)
{
    LUAU_ASSERT(!typeChecker.globalTypes.typeVars.isFrozen());
    LUAU_ASSERT(!typeChecker.globalTypes.typePacks.isFrozen());

    TypeId nilType = typeChecker.nilType;

    TypeArena& arena = typeChecker.globalTypes;

    LoadDefinitionFileResult loadResult = Luau::loadDefinitionFile(typeChecker, typeChecker.globalScope, getBuiltinDefinitionSource(), "@luau");
    LUAU_ASSERT(loadResult.success);

    TypeId genericK = arena.addType(GenericTypeVar{"K"});
    TypeId genericV = arena.addType(GenericTypeVar{"V"});
    TypeId mapOfKtoV = arena.addType(TableTypeVar{{}, TableIndexer(genericK, genericV), typeChecker.globalScope->level, TableState::Generic});

    std::optional<TypeId> stringMetatableTy = getMetatable(getSingletonTypes().stringType);
    LUAU_ASSERT(stringMetatableTy);
    const TableTypeVar* stringMetatableTable = get<TableTypeVar>(follow(*stringMetatableTy));
    LUAU_ASSERT(stringMetatableTable);

    auto it = stringMetatableTable->props.find("__index");
    LUAU_ASSERT(it != stringMetatableTable->props.end());

    addGlobalBinding(typeChecker, "string", it->second.type, "@luau");

    // next<K, V>(t: Table<K, V>, i: K?) -> (K, V)
    TypePackId nextArgsTypePack = arena.addTypePack(TypePack{{mapOfKtoV, makeOption(typeChecker, arena, genericK)}});
    addGlobalBinding(typeChecker, "next",
        arena.addType(FunctionTypeVar{{genericK, genericV}, {}, nextArgsTypePack, arena.addTypePack(TypePack{{genericK, genericV}})}), "@luau");

    TypePackId pairsArgsTypePack = arena.addTypePack({mapOfKtoV});

    TypeId pairsNext = arena.addType(FunctionTypeVar{nextArgsTypePack, arena.addTypePack(TypePack{{genericK, genericV}})});
    TypePackId pairsReturnTypePack = arena.addTypePack(TypePack{{pairsNext, mapOfKtoV, nilType}});

    // pairs<K, V>(t: Table<K, V>) -> ((Table<K, V>, K?) -> (K, V), Table<K, V>, nil)
    addGlobalBinding(typeChecker, "pairs", arena.addType(FunctionTypeVar{{genericK, genericV}, {}, pairsArgsTypePack, pairsReturnTypePack}), "@luau");

    TypeId genericMT = arena.addType(GenericTypeVar{"MT"});

    TableTypeVar tab{TableState::Generic, typeChecker.globalScope->level};
    TypeId tabTy = arena.addType(tab);

    TypeId tableMetaMT = arena.addType(MetatableTypeVar{tabTy, genericMT});

    addGlobalBinding(typeChecker, "getmetatable", makeFunction(arena, std::nullopt, {genericMT}, {}, {tableMetaMT}, {genericMT}), "@luau");

    // setmetatable<MT>({ @metatable MT }, MT) -> { @metatable MT }
    // clang-format off
    addGlobalBinding(typeChecker, "setmetatable",
        arena.addType(
            FunctionTypeVar{
                {genericMT},
                {},
                arena.addTypePack(TypePack{{tableMetaMT, genericMT}}),
                arena.addTypePack(TypePack{{tableMetaMT}})
            }
        ), "@luau"
    );
    // clang-format on

    for (const auto& pair : typeChecker.globalScope->bindings)
    {
        persist(pair.second.typeId);

        if (TableTypeVar* ttv = getMutable<TableTypeVar>(pair.second.typeId))
        {
            if (!ttv->name)
                ttv->name = toString(pair.first);
        }
    }

    attachMagicFunction(getGlobalBinding(typeChecker, "assert"), magicFunctionAssert);
    attachMagicFunction(getGlobalBinding(typeChecker, "setmetatable"), magicFunctionSetMetaTable);
    attachMagicFunction(getGlobalBinding(typeChecker, "select"), magicFunctionSelect);

    if (TableTypeVar* ttv = getMutable<TableTypeVar>(getGlobalBinding(typeChecker, "table")))
    {
        // tabTy is a generic table type which we can't express via declaration syntax yet
        ttv->props["freeze"] = makeProperty(makeFunction(arena, std::nullopt, {tabTy}, {tabTy}), "@luau/global/table.freeze");
        ttv->props["clone"] = makeProperty(makeFunction(arena, std::nullopt, {tabTy}, {tabTy}), "@luau/global/table.clone");

        attachMagicFunction(ttv->props["pack"].type, magicFunctionPack);
    }

    attachMagicFunction(getGlobalBinding(typeChecker, "require"), magicFunctionRequire);
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

static std::optional<WithPredicate<TypePackId>> magicFunctionSetMetaTable(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, WithPredicate<TypePackId> withPredicate)
{
    auto [paramPack, _predicates] = withPredicate;

    TypeArena& arena = typechecker.currentModule->internalTypes;

    std::vector<TypeId> expectedArgs = typechecker.unTypePack(scope, paramPack, 2, expr.location);

    TypeId target = follow(expectedArgs[0]);
    TypeId mt = follow(expectedArgs[1]);

    if (const auto& tab = get<TableTypeVar>(target))
    {
        if (target->persistent)
        {
            typechecker.reportError(TypeError{expr.location, CannotExtendTable{target, CannotExtendTable::Metatable}});
        }
        else
        {
            typechecker.tablify(mt);

            const TableTypeVar* mtTtv = get<TableTypeVar>(mt);
            MetatableTypeVar mtv{target, mt};
            if ((tab->name || tab->syntheticName) && (mtTtv && (mtTtv->name || mtTtv->syntheticName)))
            {
                std::string tableName = tab->name ? *tab->name : *tab->syntheticName;
                std::string metatableName = mtTtv->name ? *mtTtv->name : *mtTtv->syntheticName;

                if (tableName == metatableName)
                    mtv.syntheticName = tableName;
                else
                    mtv.syntheticName = "{ @metatable: " + metatableName + ", " + tableName + " }";
            }

            TypeId mtTy = arena.addType(mtv);

            if (FFlag::LuauSetMetaTableArgsCheck && expr.args.size < 1)
            {
                return WithPredicate<TypePackId>{};
            }

            if (!FFlag::LuauSetMetaTableArgsCheck || !expr.self)
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
    else if (get<AnyTypeVar>(target) || get<ErrorTypeVar>(target) || isTableIntersection(target))
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
        std::optional<TypeId> newhead = typechecker.pickTypesFromSense(head[0], true);
        if (!newhead)
            head = {typechecker.nilType};
        else
            head[0] = *newhead;
    }

    return WithPredicate<TypePackId>{arena.addTypePack(TypePack{std::move(head), tail})};
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

    options = typechecker.reduceUnion(options);

    // table.pack()         -> {| n: number, [number]: nil |}
    // table.pack(1)        -> {| n: number, [number]: number |}
    // table.pack(1, "foo") -> {| n: number, [number]: number | string |}
    TypeId result = nullptr;
    if (options.empty())
        result = typechecker.nilType;
    else if (options.size() == 1)
        result = options[0];
    else
        result = arena.addType(UnionTypeVar{std::move(options)});

    TypeId packedTable = arena.addType(
        TableTypeVar{{{"n", {typechecker.numberType}}}, TableIndexer(typechecker.numberType, result), scope->level, TableState::Sealed});

    return WithPredicate<TypePackId>{arena.addTypePack({packedTable})};
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

} // namespace Luau
