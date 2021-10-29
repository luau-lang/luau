// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"

#include "Luau/Frontend.h"
#include "Luau/Symbol.h"
#include "Luau/Common.h"
#include "Luau/ToString.h"

#include <algorithm>

LUAU_FASTFLAG(LuauParseGenericFunctions)
LUAU_FASTFLAG(LuauGenericFunctions)
LUAU_FASTFLAG(LuauRankNTypes)
LUAU_FASTFLAG(LuauStringMetatable)

/** FIXME: Many of these type definitions are not quite completely accurate.
 *
 * Some of them require richer generics than we have.  For instance, we do not yet have a way to talk
 * about a function that takes any number of values, but where each value must have some specific type.
 */

namespace Luau
{

static std::optional<ExprResult<TypePackId>> magicFunctionSelect(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult);
static std::optional<ExprResult<TypePackId>> magicFunctionSetMetaTable(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult);
static std::optional<ExprResult<TypePackId>> magicFunctionAssert(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult);
static std::optional<ExprResult<TypePackId>> magicFunctionPack(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult);
static std::optional<ExprResult<TypePackId>> magicFunctionRequire(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult);

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

void attachFunctionTag(TypeId ty, std::string tag)
{
    if (auto ftv = getMutable<FunctionTypeVar>(ty))
    {
        ftv->tags.emplace_back(std::move(tag));
    }
    else
    {
        LUAU_ASSERT(!"Got a non functional type");
    }
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

    TypeId numberType = typeChecker.numberType;
    TypeId booleanType = typeChecker.booleanType;
    TypeId nilType = typeChecker.nilType;
    TypeId stringType = typeChecker.stringType;
    TypeId threadType = typeChecker.threadType;
    TypeId anyType = typeChecker.anyType;

    TypeArena& arena = typeChecker.globalTypes;

    TypeId optionalNumber = makeOption(typeChecker, arena, numberType);
    TypeId optionalString = makeOption(typeChecker, arena, stringType);
    TypeId optionalBoolean = makeOption(typeChecker, arena, booleanType);

    TypeId stringOrNumber = makeUnion(arena, {stringType, numberType});

    TypePackId emptyPack = arena.addTypePack({});
    TypePackId oneNumberPack = arena.addTypePack({numberType});
    TypePackId oneStringPack = arena.addTypePack({stringType});
    TypePackId oneBooleanPack = arena.addTypePack({booleanType});
    TypePackId oneAnyPack = arena.addTypePack({anyType});

    TypePackId anyTypePack = typeChecker.anyTypePack;

    TypePackId numberVariadicList = arena.addTypePack(TypePackVar{VariadicTypePack{numberType}});
    TypePackId stringVariadicList = arena.addTypePack(TypePackVar{VariadicTypePack{stringType}});
    TypePackId listOfAtLeastOneNumber = arena.addTypePack(TypePack{{numberType}, numberVariadicList});

    TypeId listOfAtLeastOneNumberToNumberType = arena.addType(FunctionTypeVar{
        listOfAtLeastOneNumber,
        oneNumberPack,
    });

    TypeId listOfAtLeastZeroNumbersToNumberType = arena.addType(FunctionTypeVar{numberVariadicList, oneNumberPack});

    TypeId stringToAnyMap = arena.addType(TableTypeVar{{}, TableIndexer(stringType, anyType), typeChecker.globalScope->level});

    LoadDefinitionFileResult loadResult = Luau::loadDefinitionFile(typeChecker, typeChecker.globalScope, getBuiltinDefinitionSource(), "@luau");
    LUAU_ASSERT(loadResult.success);

    TypeId mathLibType = getGlobalBinding(typeChecker, "math");
    if (TableTypeVar* ttv = getMutable<TableTypeVar>(mathLibType))
    {
        ttv->props["min"] = makeProperty(listOfAtLeastOneNumberToNumberType, "@luau/global/math.min");
        ttv->props["max"] = makeProperty(listOfAtLeastOneNumberToNumberType, "@luau/global/math.max");
    }

    TypeId bit32LibType = getGlobalBinding(typeChecker, "bit32");
    if (TableTypeVar* ttv = getMutable<TableTypeVar>(bit32LibType))
    {
        ttv->props["band"] = makeProperty(listOfAtLeastZeroNumbersToNumberType, "@luau/global/bit32.band");
        ttv->props["bor"] = makeProperty(listOfAtLeastZeroNumbersToNumberType, "@luau/global/bit32.bor");
        ttv->props["bxor"] = makeProperty(listOfAtLeastZeroNumbersToNumberType, "@luau/global/bit32.bxor");
        ttv->props["btest"] = makeProperty(arena.addType(FunctionTypeVar{listOfAtLeastOneNumber, oneBooleanPack}), "@luau/global/bit32.btest");
    }

    TypeId anyFunction = arena.addType(FunctionTypeVar{anyTypePack, anyTypePack});

    TypeId genericK = arena.addType(GenericTypeVar{"K"});
    TypeId genericV = arena.addType(GenericTypeVar{"V"});
    TypeId mapOfKtoV = arena.addType(TableTypeVar{{}, TableIndexer(genericK, genericV), typeChecker.globalScope->level});

    if (FFlag::LuauStringMetatable)
    {
        std::optional<TypeId> stringMetatableTy = getMetatable(singletonTypes.stringType);
        LUAU_ASSERT(stringMetatableTy);
        const TableTypeVar* stringMetatableTable = get<TableTypeVar>(follow(*stringMetatableTy));
        LUAU_ASSERT(stringMetatableTable);

        auto it = stringMetatableTable->props.find("__index");
        LUAU_ASSERT(it != stringMetatableTable->props.end());

        TypeId stringLib = it->second.type;
        addGlobalBinding(typeChecker, "string", stringLib, "@luau");
    }

    if (FFlag::LuauParseGenericFunctions && FFlag::LuauGenericFunctions)
    {
        if (!FFlag::LuauStringMetatable)
        {
            TypeId stringLibTy = getGlobalBinding(typeChecker, "string");
            TableTypeVar* stringLib = getMutable<TableTypeVar>(stringLibTy);
            TypeId replArgType = makeUnion(
                arena, {stringType,
                           arena.addType(TableTypeVar({}, TableIndexer(stringType, stringType), typeChecker.globalScope->level, TableState::Generic)),
                           makeFunction(arena, std::nullopt, {stringType}, {stringType})});
            TypeId gsubFunc = makeFunction(arena, stringType, {stringType, replArgType, optionalNumber}, {stringType, numberType});

            stringLib->props["gsub"] = makeProperty(gsubFunc, "@luau/global/string.gsub");
        }
    }
    else
    {
        if (!FFlag::LuauStringMetatable)
        {
            TypeId stringToStringType = makeFunction(arena, std::nullopt, {stringType}, {stringType});

            TypeId gmatchFunc = makeFunction(arena, stringType, {stringType}, {arena.addType(FunctionTypeVar{emptyPack, stringVariadicList})});

            TypeId replArgType = makeUnion(
                arena, {stringType,
                           arena.addType(TableTypeVar({}, TableIndexer(stringType, stringType), typeChecker.globalScope->level, TableState::Generic)),
                           makeFunction(arena, std::nullopt, {stringType}, {stringType})});
            TypeId gsubFunc = makeFunction(arena, stringType, {stringType, replArgType, optionalNumber}, {stringType, numberType});

            TypeId formatFn = arena.addType(FunctionTypeVar{arena.addTypePack(TypePack{{stringType}, anyTypePack}), oneStringPack});

            TableTypeVar::Props stringLib = {
                // FIXME string.byte "can" return a pack of numbers, but only if 2nd or 3rd arguments were supplied
                {"byte", {makeFunction(arena, stringType, {optionalNumber, optionalNumber}, {optionalNumber})}},
                // FIXME char takes a variadic pack of numbers
                {"char", {makeFunction(arena, std::nullopt, {numberType, optionalNumber, optionalNumber, optionalNumber}, {stringType})}},
                {"find", {makeFunction(arena, stringType, {stringType, optionalNumber, optionalBoolean}, {optionalNumber, optionalNumber})}},
                {"format", {formatFn}}, // FIXME
                {"gmatch", {gmatchFunc}},
                {"gsub", {gsubFunc}},
                {"len", {makeFunction(arena, stringType, {}, {numberType})}},
                {"lower", {stringToStringType}},
                {"match", {makeFunction(arena, stringType, {stringType, optionalNumber}, {optionalString})}},
                {"rep", {makeFunction(arena, stringType, {numberType}, {stringType})}},
                {"reverse", {stringToStringType}},
                {"sub", {makeFunction(arena, stringType, {numberType, optionalNumber}, {stringType})}},
                {"upper", {stringToStringType}},
                {"split", {makeFunction(arena, stringType, {stringType, optionalString},
                              {arena.addType(TableTypeVar{{}, TableIndexer{numberType, stringType}, typeChecker.globalScope->level})})}},
                {"pack", {arena.addType(FunctionTypeVar{
                             arena.addTypePack(TypePack{{stringType}, anyTypePack}),
                             oneStringPack,
                         })}},
                {"packsize", {makeFunction(arena, stringType, {}, {numberType})}},
                {"unpack", {arena.addType(FunctionTypeVar{
                               arena.addTypePack(TypePack{{stringType, stringType, optionalNumber}}),
                               anyTypePack,
                           })}},
            };

            assignPropDocumentationSymbols(stringLib, "@luau/global/string");
            addGlobalBinding(typeChecker, "string",
                arena.addType(TableTypeVar{stringLib, std::nullopt, typeChecker.globalScope->level, TableState::Sealed}), "@luau");
        }

        TableTypeVar::Props debugLib{
            {"info", {makeIntersection(arena,
                         {
                             arena.addType(FunctionTypeVar{arena.addTypePack({typeChecker.threadType, numberType, stringType}), anyTypePack}),
                             arena.addType(FunctionTypeVar{arena.addTypePack({numberType, stringType}), anyTypePack}),
                             arena.addType(FunctionTypeVar{arena.addTypePack({anyFunction, stringType}), anyTypePack}),
                         })}},
            {"traceback", {makeIntersection(arena,
                              {
                                  makeFunction(arena, std::nullopt, {optionalString, optionalNumber}, {stringType}),
                                  makeFunction(arena, std::nullopt, {typeChecker.threadType, optionalString, optionalNumber}, {stringType}),
                              })}},
        };

        assignPropDocumentationSymbols(debugLib, "@luau/global/debug");
        addGlobalBinding(typeChecker, "debug",
            arena.addType(TableTypeVar{debugLib, std::nullopt, typeChecker.globalScope->level, Luau::TableState::Sealed}), "@luau");

        TableTypeVar::Props utf8Lib = {
            {"char", {arena.addType(FunctionTypeVar{listOfAtLeastOneNumber, oneStringPack})}}, // FIXME
            {"charpattern", {stringType}},
            {"codes", {makeFunction(arena, std::nullopt, {stringType},
                          {makeFunction(arena, std::nullopt, {stringType, numberType}, {numberType, numberType}), stringType, numberType})}},
            {"codepoint",
                {arena.addType(FunctionTypeVar{arena.addTypePack({stringType, optionalNumber, optionalNumber}), listOfAtLeastOneNumber})}}, // FIXME
            {"len", {makeFunction(arena, std::nullopt, {stringType, optionalNumber, optionalNumber}, {optionalNumber, numberType})}},
            {"offset", {makeFunction(arena, std::nullopt, {stringType, optionalNumber, optionalNumber}, {numberType})}},
            {"nfdnormalize", {makeFunction(arena, std::nullopt, {stringType}, {stringType})}},
            {"graphemes", {makeFunction(arena, std::nullopt, {stringType, optionalNumber, optionalNumber},
                              {makeFunction(arena, std::nullopt, {}, {numberType, numberType})})}},
            {"nfcnormalize", {makeFunction(arena, std::nullopt, {stringType}, {stringType})}},
        };

        assignPropDocumentationSymbols(utf8Lib, "@luau/global/utf8");
        addGlobalBinding(
            typeChecker, "utf8", arena.addType(TableTypeVar{utf8Lib, std::nullopt, typeChecker.globalScope->level, TableState::Sealed}), "@luau");

        TypeId optionalV = makeOption(typeChecker, arena, genericV);

        TypeId arrayOfV = arena.addType(TableTypeVar{{}, TableIndexer(numberType, genericV), typeChecker.globalScope->level});

        TypePackId unpackArgsPack = arena.addTypePack(TypePack{{arrayOfV, optionalNumber, optionalNumber}});
        TypePackId unpackReturnPack = arena.addTypePack(TypePack{{}, anyTypePack});
        TypeId unpackFunc = arena.addType(FunctionTypeVar{{genericV}, {}, unpackArgsPack, unpackReturnPack});

        TypeId packResult = arena.addType(TableTypeVar{
            TableTypeVar::Props{{"n", {numberType}}}, TableIndexer{numberType, numberType}, typeChecker.globalScope->level, TableState::Sealed});
        TypePackId packArgsPack = arena.addTypePack(TypePack{{}, anyTypePack});
        TypePackId packReturnPack = arena.addTypePack(TypePack{{packResult}});

        TypeId comparator = makeFunction(arena, std::nullopt, {genericV, genericV}, {booleanType});
        TypeId optionalComparator = makeOption(typeChecker, arena, comparator);

        TypeId packFn = arena.addType(FunctionTypeVar(packArgsPack, packReturnPack));

        TableTypeVar::Props tableLib = {
            {"concat", {makeFunction(arena, std::nullopt, {genericV}, {}, {arrayOfV, optionalString, optionalNumber, optionalNumber}, {stringType})}},
            {"insert", {makeIntersection(arena, {makeFunction(arena, std::nullopt, {genericV}, {}, {arrayOfV, genericV}, {}),
                                                    makeFunction(arena, std::nullopt, {genericV}, {}, {arrayOfV, numberType, genericV}, {})})}},
            {"maxn", {makeFunction(arena, std::nullopt, {genericV}, {}, {arrayOfV}, {numberType})}},
            {"remove", {makeFunction(arena, std::nullopt, {genericV}, {}, {arrayOfV, optionalNumber}, {optionalV})}},
            {"sort", {makeFunction(arena, std::nullopt, {genericV}, {}, {arrayOfV, optionalComparator}, {})}},
            {"create", {makeFunction(arena, std::nullopt, {genericV}, {}, {numberType, optionalV}, {arrayOfV})}},
            {"find", {makeFunction(arena, std::nullopt, {genericV}, {}, {arrayOfV, genericV, optionalNumber}, {optionalNumber})}},

            {"unpack", {unpackFunc}}, // FIXME
            {"pack", {packFn}},

            // Lua 5.0 compat
            {"getn", {makeFunction(arena, std::nullopt, {genericV}, {}, {arrayOfV}, {numberType})}},
            {"foreach", {makeFunction(arena, std::nullopt, {genericK, genericV}, {},
                            {mapOfKtoV, makeFunction(arena, std::nullopt, {genericK, genericV}, {})}, {})}},
            {"foreachi", {makeFunction(arena, std::nullopt, {genericV}, {}, {arrayOfV, makeFunction(arena, std::nullopt, {genericV}, {})}, {})}},

            // backported from Lua 5.3
            {"move", {makeFunction(arena, std::nullopt, {genericV}, {}, {arrayOfV, numberType, numberType, numberType, arrayOfV}, {})}},

            // added in Luau (borrowed from LuaJIT)
            {"clear", {makeFunction(arena, std::nullopt, {genericK, genericV}, {}, {mapOfKtoV}, {})}},

            {"freeze", {makeFunction(arena, std::nullopt, {genericK, genericV}, {}, {mapOfKtoV}, {mapOfKtoV})}},
            {"isfrozen", {makeFunction(arena, std::nullopt, {genericK, genericV}, {}, {mapOfKtoV}, {booleanType})}},
        };

        assignPropDocumentationSymbols(tableLib, "@luau/global/table");
        addGlobalBinding(
            typeChecker, "table", arena.addType(TableTypeVar{tableLib, std::nullopt, typeChecker.globalScope->level, TableState::Sealed}), "@luau");

        TableTypeVar::Props coroutineLib = {
            {"create", {makeFunction(arena, std::nullopt, {anyFunction}, {threadType})}},
            {"resume", {arena.addType(FunctionTypeVar{arena.addTypePack(TypePack{{threadType}, anyTypePack}), anyTypePack})}},
            {"running", {makeFunction(arena, std::nullopt, {}, {threadType})}},
            {"status", {makeFunction(arena, std::nullopt, {threadType}, {stringType})}},
            {"wrap", {makeFunction(
                         arena, std::nullopt, {anyFunction}, {anyType})}}, // FIXME this technically returns a function, but we can't represent this
                                                                           // atm since it can be called with different arg types at different times
            {"yield", {arena.addType(FunctionTypeVar{anyTypePack, anyTypePack})}},
            {"isyieldable", {makeFunction(arena, std::nullopt, {}, {booleanType})}},
        };

        assignPropDocumentationSymbols(coroutineLib, "@luau/global/coroutine");
        addGlobalBinding(typeChecker, "coroutine",
            arena.addType(TableTypeVar{coroutineLib, std::nullopt, typeChecker.globalScope->level, TableState::Sealed}), "@luau");

        TypeId genericT = arena.addType(GenericTypeVar{"T"});
        TypeId genericR = arena.addType(GenericTypeVar{"R"});

        // assert returns all arguments
        TypePackId assertArgs = arena.addTypePack({genericT, optionalString});
        TypePackId assertRets = arena.addTypePack({genericT});
        addGlobalBinding(typeChecker, "assert", arena.addType(FunctionTypeVar{assertArgs, assertRets}), "@luau");

        addGlobalBinding(typeChecker, "print", arena.addType(FunctionTypeVar{anyTypePack, emptyPack}), "@luau");

        addGlobalBinding(typeChecker, "type", makeFunction(arena, std::nullopt, {genericT}, {}, {genericT}, {stringType}), "@luau");
        addGlobalBinding(typeChecker, "typeof", makeFunction(arena, std::nullopt, {genericT}, {}, {genericT}, {stringType}), "@luau");

        addGlobalBinding(typeChecker, "error", makeFunction(arena, std::nullopt, {genericT}, {}, {genericT, optionalNumber}, {}), "@luau");

        addGlobalBinding(typeChecker, "tostring", makeFunction(arena, std::nullopt, {genericT}, {}, {genericT}, {stringType}), "@luau");
        addGlobalBinding(
            typeChecker, "tonumber", makeFunction(arena, std::nullopt, {genericT}, {}, {genericT, optionalNumber}, {numberType}), "@luau");

        addGlobalBinding(
            typeChecker, "rawequal", makeFunction(arena, std::nullopt, {genericT, genericR}, {}, {genericT, genericR}, {booleanType}), "@luau");
        addGlobalBinding(
            typeChecker, "rawget", makeFunction(arena, std::nullopt, {genericK, genericV}, {}, {mapOfKtoV, genericK}, {genericV}), "@luau");
        addGlobalBinding(typeChecker, "rawset",
            makeFunction(arena, std::nullopt, {genericK, genericV}, {}, {mapOfKtoV, genericK, genericV}, {mapOfKtoV}), "@luau");

        TypePackId genericTPack = arena.addTypePack({genericT});
        TypePackId genericRPack = arena.addTypePack({genericR});
        TypeId genericArgsToReturnFunction = arena.addType(
            FunctionTypeVar{{genericT, genericR}, {}, arena.addTypePack(TypePack{{}, genericTPack}), arena.addTypePack(TypePack{{}, genericRPack})});

        TypeId setfenvArgType = makeUnion(arena, {numberType, genericArgsToReturnFunction});
        TypeId setfenvReturnType = makeOption(typeChecker, arena, genericArgsToReturnFunction);
        addGlobalBinding(typeChecker, "setfenv", makeFunction(arena, std::nullopt, {setfenvArgType, stringToAnyMap}, {setfenvReturnType}), "@luau");

        TypePackId ipairsArgsTypePack = arena.addTypePack({arrayOfV});

        TypeId ipairsNextFunctionType = arena.addType(
            FunctionTypeVar{{genericK, genericV}, {}, arena.addTypePack({arrayOfV, numberType}), arena.addTypePack({numberType, genericV})});

        // ipairs returns 'next, Array<V>, 0' so we would need type-level primitives and change to
        // again, we have a direct reference to 'next' because ipairs returns it
        // ipairs<V>(t: Array<V>) -> ((Array<V>) -> (number, V), Array<V>, 0)
        TypePackId ipairsReturnTypePack = arena.addTypePack(TypePack{{ipairsNextFunctionType, arrayOfV, numberType}});

        // ipairs<V>(t: Array<V>) -> ((Array<V>) -> (number, V), Array<V>, number)
        addGlobalBinding(typeChecker, "ipairs", arena.addType(FunctionTypeVar{{genericV}, {}, ipairsArgsTypePack, ipairsReturnTypePack}), "@luau");

        TypePackId pcallArg0FnArgs = arena.addTypePack(TypePackVar{GenericTypeVar{"A"}});
        TypePackId pcallArg0FnRet = arena.addTypePack(TypePackVar{GenericTypeVar{"R"}});
        TypeId pcallArg0 = arena.addType(FunctionTypeVar{pcallArg0FnArgs, pcallArg0FnRet});
        TypePackId pcallArgsTypePack = arena.addTypePack(TypePack{{pcallArg0}, pcallArg0FnArgs});

        TypePackId pcallReturnTypePack = arena.addTypePack(TypePack{{booleanType}, pcallArg0FnRet});

        // pcall<A..., R...>(f: (A...) -> R..., args: A...) -> boolean, R...
        addGlobalBinding(typeChecker, "pcall",
            arena.addType(FunctionTypeVar{{}, {pcallArg0FnArgs, pcallArg0FnRet}, pcallArgsTypePack, pcallReturnTypePack}), "@luau");

        // errors thrown by the function 'f' are propagated onto the function 'err' that accepts it.
        // and either 'f' or 'err' are valid results of this xpcall
        // if 'err' did throw an error, then it returns: false, "error in error handling"
        // TODO: the above is not represented (nor representable) in the type annotation below.
        //
        // The real type of xpcall is as such: <E, A..., R1..., R2...>(f: (A...) -> R1..., err: (E) -> R2..., A...) -> (true, R1...) | (false,
        // R2...)
        TypePackId genericAPack = arena.addTypePack(TypePackVar{GenericTypeVar{"A"}});
        TypePackId genericR1Pack = arena.addTypePack(TypePackVar{GenericTypeVar{"R1"}});
        TypePackId genericR2Pack = arena.addTypePack(TypePackVar{GenericTypeVar{"R2"}});

        TypeId genericE = arena.addType(GenericTypeVar{"E"});

        TypeId xpcallFArg = arena.addType(FunctionTypeVar{genericAPack, genericR1Pack});
        TypeId xpcallErrArg = arena.addType(FunctionTypeVar{arena.addTypePack({genericE}), genericR2Pack});

        TypePackId xpcallArgsPack = arena.addTypePack({{xpcallFArg, xpcallErrArg}, genericAPack});
        TypePackId xpcallRetPack = arena.addTypePack({{booleanType}, genericR1Pack}); // FIXME

        addGlobalBinding(typeChecker, "xpcall",
            arena.addType(FunctionTypeVar{{genericE}, {genericAPack, genericR1Pack, genericR2Pack}, xpcallArgsPack, xpcallRetPack}), "@luau");

        addGlobalBinding(typeChecker, "unpack", unpackFunc, "@luau");

        TypePackId selectArgsTypePack = arena.addTypePack(TypePack{
            {stringOrNumber},
            anyTypePack // FIXME?  select() is tricky.
        });

        addGlobalBinding(typeChecker, "select", arena.addType(FunctionTypeVar{selectArgsTypePack, anyTypePack}), "@luau");

        // TODO: not completely correct. loadstring's return type should be a function or (nil, string)
        TypeId loadstringFunc = arena.addType(FunctionTypeVar{anyTypePack, oneAnyPack});

        addGlobalBinding(typeChecker, "loadstring",
            makeFunction(arena, std::nullopt, {stringType, optionalString},
                {
                    makeOption(typeChecker, arena, loadstringFunc),
                    makeOption(typeChecker, arena, stringType),
                }),
            "@luau");

        // a userdata object is "roughly" the same as a sealed empty table
        // except `type(newproxy(false))` evaluates to "userdata" so we may need another special type here too.
        // another important thing to note: the value passed in conditionally creates an empty metatable, and you have to use getmetatable, NOT
        // setmetatable.
        // TODO: change this to something Luau can understand how to reject `setmetatable(newproxy(false or true), {})`.
        TypeId sealedTable = arena.addType(TableTypeVar(TableState::Sealed, typeChecker.globalScope->level));
        addGlobalBinding(typeChecker, "newproxy", makeFunction(arena, std::nullopt, {optionalBoolean}, {sealedTable}), "@luau");
    }

    // next<K, V>(t: Table<K, V>, i: K | nil) -> (K, V)
    TypePackId nextArgsTypePack = arena.addTypePack(TypePack{{mapOfKtoV, makeOption(typeChecker, arena, genericK)}});
    addGlobalBinding(typeChecker, "next",
        arena.addType(FunctionTypeVar{{genericK, genericV}, {}, nextArgsTypePack, arena.addTypePack(TypePack{{genericK, genericV}})}), "@luau");

    TypePackId pairsArgsTypePack = arena.addTypePack({mapOfKtoV});

    TypeId pairsNext = (FFlag::LuauRankNTypes ? arena.addType(FunctionTypeVar{nextArgsTypePack, arena.addTypePack(TypePack{{genericK, genericV}})})
                                              : getGlobalBinding(typeChecker, "next"));
    TypePackId pairsReturnTypePack = arena.addTypePack(TypePack{{pairsNext, mapOfKtoV, nilType}});

    // NOTE we are missing 'i: K | nil' argument in the first return types' argument.
    // pairs<K, V>(t: Table<K, V>) -> ((Table<K, V>) -> (K, V), Table<K, V>, nil)
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
            ttv->name = toString(pair.first);
    }

    attachMagicFunction(getGlobalBinding(typeChecker, "assert"), magicFunctionAssert);
    attachMagicFunction(getGlobalBinding(typeChecker, "setmetatable"), magicFunctionSetMetaTable);
    attachMagicFunction(getGlobalBinding(typeChecker, "select"), magicFunctionSelect);

    auto tableLib = getMutable<TableTypeVar>(getGlobalBinding(typeChecker, "table"));
    attachMagicFunction(tableLib->props["pack"].type, magicFunctionPack);

    auto stringLib = getMutable<TableTypeVar>(getGlobalBinding(typeChecker, "string"));
    attachMagicFunction(stringLib->props["format"].type, magicFunctionFormat);

    attachMagicFunction(getGlobalBinding(typeChecker, "require"), magicFunctionRequire);
}

static std::optional<ExprResult<TypePackId>> magicFunctionSelect(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult)
{
    auto [paramPack, _predicates] = exprResult;

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
                return ExprResult<TypePackId>{typechecker.currentModule->internalTypes.addTypePack(TypePack{std::move(result), tail})};
            }
            else if (tail)
                return ExprResult<TypePackId>{*tail};
        }

        typechecker.reportError(TypeError{arg1->location, GenericError{"bad argument #1 to select (index out of range)"}});
    }
    else if (AstExprConstantString* str = arg1->as<AstExprConstantString>())
    {
        if (str->value.size == 1 && str->value.data[0] == '#')
            return ExprResult<TypePackId>{typechecker.currentModule->internalTypes.addTypePack({typechecker.numberType})};
    }

    return std::nullopt;
}

static std::optional<ExprResult<TypePackId>> magicFunctionSetMetaTable(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult)
{
    auto [paramPack, _predicates] = exprResult;

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

            AstExpr* targetExpr = expr.args.data[0];
            if (AstExprLocal* targetLocal = targetExpr->as<AstExprLocal>())
            {
                const Name targetName(targetLocal->local->name.value);
                scope->bindings[targetLocal->local] = Binding{mtTy, expr.location};
            }

            return ExprResult<TypePackId>{arena.addTypePack({mtTy})};
        }
    }
    else if (get<AnyTypeVar>(target) || get<ErrorTypeVar>(target) || isTableIntersection(target))
    {
    }
    else
    {
        typechecker.reportError(TypeError{expr.location, GenericError{"setmetatable should take a table"}});
    }

    return ExprResult<TypePackId>{arena.addTypePack({target})};
}

static std::optional<ExprResult<TypePackId>> magicFunctionAssert(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult)
{
    auto [paramPack, predicates] = exprResult;

    if (expr.args.size < 1)
        return ExprResult<TypePackId>{paramPack};

    typechecker.reportErrors(typechecker.resolve(predicates, scope, true));

    return ExprResult<TypePackId>{paramPack};
}

static std::optional<ExprResult<TypePackId>> magicFunctionPack(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult)
{
    auto [paramPack, _predicates] = exprResult;

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

    return ExprResult<TypePackId>{arena.addTypePack({packedTable})};
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

static std::optional<ExprResult<TypePackId>> magicFunctionRequire(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult)
{
    TypeArena& arena = typechecker.currentModule->internalTypes;

    if (expr.args.size != 1)
    {
        typechecker.reportError(TypeError{expr.location, GenericError{"require takes 1 argument"}});
        return std::nullopt;
    }

    AstExpr* require = expr.args.data[0];

    if (!checkRequirePath(typechecker, require))
        return std::nullopt;

    if (auto moduleInfo = typechecker.resolver->resolveModuleInfo(typechecker.currentModuleName, *require))
        return ExprResult<TypePackId>{arena.addTypePack({typechecker.checkRequire(scope, *moduleInfo, expr.location)})};

    return std::nullopt;
}

} // namespace Luau
