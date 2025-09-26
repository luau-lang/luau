// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"

#include "Luau/Ast.h"
#include "Luau/BuiltinTypeFunctions.h"
#include "Luau/Clone.h"
#include "Luau/Common.h"
#include "Luau/ConstraintGenerator.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/DenseHash.h"
#include "Luau/Error.h"
#include "Luau/Frontend.h"
#include "Luau/InferPolarity.h"
#include "Luau/Module.h"
#include "Luau/NotNull.h"
#include "Luau/Subtyping.h"
#include "Luau/Symbol.h"
#include "Luau/Type.h"
#include "Luau/TypeChecker2.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"

#include <algorithm>
#include <string_view>

/** FIXME: Many of these type definitions are not quite completely accurate.
 *
 * Some of them require richer generics than we have.  For instance, we do not yet have a way to talk
 * about a function that takes any number of values, but where each value must have some specific type.
 */

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAGVARIABLE(LuauTableCloneClonesType3)
LUAU_FASTFLAG(LuauUseWorkspacePropToChooseSolver)
LUAU_FASTFLAG(LuauEmplaceNotPushBack)
LUAU_FASTFLAG(LuauBuiltinTypeFunctionsArentGlobal)

namespace Luau
{

struct MagicSelect final : MagicFunction
{
    std::optional<WithPredicate<TypePackId>> handleOldSolver(
        struct TypeChecker&,
        const std::shared_ptr<struct Scope>&,
        const class AstExprCall&,
        WithPredicate<TypePackId>
    ) override;
    bool infer(const MagicFunctionCallContext& ctx) override;
};

struct MagicSetMetatable final : MagicFunction
{
    std::optional<WithPredicate<TypePackId>> handleOldSolver(
        struct TypeChecker&,
        const std::shared_ptr<struct Scope>&,
        const class AstExprCall&,
        WithPredicate<TypePackId>
    ) override;
    bool infer(const MagicFunctionCallContext& ctx) override;
};

struct MagicAssert final : MagicFunction
{
    std::optional<WithPredicate<TypePackId>> handleOldSolver(
        struct TypeChecker&,
        const std::shared_ptr<struct Scope>&,
        const class AstExprCall&,
        WithPredicate<TypePackId>
    ) override;
    bool infer(const MagicFunctionCallContext& ctx) override;
};

struct MagicPack final : MagicFunction
{
    std::optional<WithPredicate<TypePackId>> handleOldSolver(
        struct TypeChecker&,
        const std::shared_ptr<struct Scope>&,
        const class AstExprCall&,
        WithPredicate<TypePackId>
    ) override;
    bool infer(const MagicFunctionCallContext& ctx) override;
};

struct MagicRequire final : MagicFunction
{
    std::optional<WithPredicate<TypePackId>> handleOldSolver(
        struct TypeChecker&,
        const std::shared_ptr<struct Scope>&,
        const class AstExprCall&,
        WithPredicate<TypePackId>
    ) override;
    bool infer(const MagicFunctionCallContext& ctx) override;
};

struct MagicClone final : MagicFunction
{
    std::optional<WithPredicate<TypePackId>> handleOldSolver(
        struct TypeChecker&,
        const std::shared_ptr<struct Scope>&,
        const class AstExprCall&,
        WithPredicate<TypePackId>
    ) override;
    bool infer(const MagicFunctionCallContext& ctx) override;
};

struct MagicFreeze final : MagicFunction
{
    std::optional<WithPredicate<TypePackId>> handleOldSolver(
        struct TypeChecker&,
        const std::shared_ptr<struct Scope>&,
        const class AstExprCall&,
        WithPredicate<TypePackId>
    ) override;
    bool infer(const MagicFunctionCallContext& ctx) override;
};

struct MagicFormat final : MagicFunction
{
    std::optional<WithPredicate<TypePackId>> handleOldSolver(
        struct TypeChecker&,
        const std::shared_ptr<struct Scope>&,
        const class AstExprCall&,
        WithPredicate<TypePackId>
    ) override;
    bool infer(const MagicFunctionCallContext& ctx) override;
    bool typeCheck(const MagicFunctionTypeCheckContext& ctx) override;
};

struct MagicMatch final : MagicFunction
{
    std::optional<WithPredicate<TypePackId>> handleOldSolver(
        struct TypeChecker&,
        const std::shared_ptr<struct Scope>&,
        const class AstExprCall&,
        WithPredicate<TypePackId>
    ) override;
    bool infer(const MagicFunctionCallContext& ctx) override;
};

struct MagicGmatch final : MagicFunction
{
    std::optional<WithPredicate<TypePackId>> handleOldSolver(
        struct TypeChecker&,
        const std::shared_ptr<struct Scope>&,
        const class AstExprCall&,
        WithPredicate<TypePackId>
    ) override;
    bool infer(const MagicFunctionCallContext& ctx) override;
};

struct MagicFind final : MagicFunction
{
    std::optional<WithPredicate<TypePackId>> handleOldSolver(
        struct TypeChecker&,
        const std::shared_ptr<struct Scope>&,
        const class AstExprCall&,
        WithPredicate<TypePackId>
    ) override;
    bool infer(const MagicFunctionCallContext& ctx) override;
};

TypeId makeUnion(TypeArena& arena, std::vector<TypeId>&& types)
{
    return arena.addType(UnionType{std::move(types)});
}

TypeId makeIntersection(TypeArena& arena, std::vector<TypeId>&& types)
{
    return arena.addType(IntersectionType{std::move(types)});
}

TypeId makeOption(NotNull<BuiltinTypes> builtinTypes, TypeArena& arena, TypeId t)
{
    LUAU_ASSERT(t);
    return makeUnion(arena, {builtinTypes->nilType, t});
}

TypeId makeFunction(
    TypeArena& arena,
    std::optional<TypeId> selfType,
    std::initializer_list<TypeId> paramTypes,
    std::initializer_list<TypeId> retTypes,
    bool checked
)
{
    return makeFunction(arena, selfType, {}, {}, paramTypes, {}, retTypes, checked);
}

TypeId makeFunction(
    TypeArena& arena,
    std::optional<TypeId> selfType,
    std::initializer_list<TypeId> generics,
    std::initializer_list<TypePackId> genericPacks,
    std::initializer_list<TypeId> paramTypes,
    std::initializer_list<TypeId> retTypes,
    bool checked
)
{
    return makeFunction(arena, selfType, generics, genericPacks, paramTypes, {}, retTypes, checked);
}

TypeId makeFunction(
    TypeArena& arena,
    std::optional<TypeId> selfType,
    std::initializer_list<TypeId> paramTypes,
    std::initializer_list<std::string> paramNames,
    std::initializer_list<TypeId> retTypes,
    bool checked
)
{
    return makeFunction(arena, selfType, {}, {}, paramTypes, paramNames, retTypes, checked);
}

TypeId makeFunction(
    TypeArena& arena,
    std::optional<TypeId> selfType,
    std::initializer_list<TypeId> generics,
    std::initializer_list<TypePackId> genericPacks,
    std::initializer_list<TypeId> paramTypes,
    std::initializer_list<std::string> paramNames,
    std::initializer_list<TypeId> retTypes,
    bool checked
)
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
    {
        if (FFlag::LuauEmplaceNotPushBack)
            ftv.argNames.emplace_back(Luau::FunctionArgument{"self", {}});
        else
            ftv.argNames.push_back(Luau::FunctionArgument{"self", {}});
    }

    if (paramNames.size() != 0)
    {
        for (auto&& p : paramNames)
            if (FFlag::LuauEmplaceNotPushBack)
                ftv.argNames.emplace_back(Luau::FunctionArgument{p, Location{}});
            else
                ftv.argNames.push_back(Luau::FunctionArgument{std::move(p), {}});
    }
    else if (selfType)
    {
        // If argument names were not provided, but we have already added a name for 'self' argument, we have to fill remaining slots as well
        for (size_t i = 0; i < paramTypes.size(); i++)
            if (FFlag::LuauEmplaceNotPushBack)
                ftv.argNames.emplace_back(std::nullopt);
            else
                ftv.argNames.push_back(std::nullopt);
    }

    ftv.isCheckedFunction = checked;

    return arena.addType(std::move(ftv));
}

void attachMagicFunction(TypeId ty, std::shared_ptr<MagicFunction> magic)
{
    if (auto ftv = getMutable<FunctionType>(ty))
        ftv->magic = std::move(magic);
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

void addGlobalBinding(GlobalTypes& globals, const std::string& name, TypeId ty, const std::string& packageName)
{
    addGlobalBinding(globals, globals.globalScope, name, ty, packageName);
}

void addGlobalBinding(GlobalTypes& globals, const std::string& name, Binding binding)
{
    addGlobalBinding(globals, globals.globalScope, name, std::move(binding));
}

void addGlobalBinding(GlobalTypes& globals, const ScopePtr& scope, const std::string& name, TypeId ty, const std::string& packageName)
{
    std::string documentationSymbol = packageName + "/global/" + name;
    addGlobalBinding(globals, scope, name, Binding{ty, Location{}, {}, {}, documentationSymbol});
}

void addGlobalBinding(GlobalTypes& globals, const ScopePtr& scope, const std::string& name, Binding binding)
{
    inferGenericPolarities(NotNull{&globals.globalTypes}, NotNull{scope.get()}, binding.typeId);
    scope->bindings[globals.globalNames.names->getOrAdd(name.c_str())] = binding;
}

std::optional<Binding> tryGetGlobalBinding(GlobalTypes& globals, const std::string& name)
{
    AstName astName = globals.globalNames.names->getOrAdd(name.c_str());
    auto it = globals.globalScope->bindings.find(astName);
    if (it != globals.globalScope->bindings.end())
        return it->second;

    return std::nullopt;
}

TypeId getGlobalBinding(GlobalTypes& globals, const std::string& name)
{
    auto t = tryGetGlobalBinding(globals, name);
    LUAU_ASSERT(t.has_value());
    return t->typeId;
}

Binding* tryGetGlobalBindingRef(GlobalTypes& globals, const std::string& name)
{
    AstName astName = globals.globalNames.names->get(name.c_str());
    if (astName == AstName())
        return nullptr;

    auto it = globals.globalScope->bindings.find(astName);
    if (it != globals.globalScope->bindings.end())
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

static void finalizeGlobalBindings(ScopePtr scope)
{
    for (const auto& pair : scope->bindings)
    {
        persist(pair.second.typeId);

        if (TableType* ttv = getMutable<TableType>(pair.second.typeId))
        {
            if (!ttv->name)
                ttv->name = "typeof(" + toString(pair.first) + ")";
        }
    }
}

void registerBuiltinGlobals(Frontend& frontend, GlobalTypes& globals, bool typeCheckForAutocomplete)
{
    LUAU_ASSERT(!globals.globalTypes.types.isFrozen());
    LUAU_ASSERT(!globals.globalTypes.typePacks.isFrozen());

    TypeArena& arena = globals.globalTypes;
    NotNull<BuiltinTypes> builtinTypes = globals.builtinTypes;
    NotNull<Scope> globalScope{globals.globalScope.get()};

    if (FFlag::LuauBuiltinTypeFunctionsArentGlobal)
    {
        if (frontend.getLuauSolverMode() == SolverMode::New)
            builtinTypes->typeFunctions->addToScope(NotNull{&arena}, NotNull{globals.globalScope.get()});
    }
    else
    {
        if (frontend.getLuauSolverMode() == SolverMode::New)
            builtinTypeFunctions_DEPRECATED().addToScope(NotNull{&arena}, NotNull{globals.globalScope.get()});
    }


    LoadDefinitionFileResult loadResult = frontend.loadDefinitionFile(
        globals, globals.globalScope, getBuiltinDefinitionSource(), "@luau", /* captureComments */ false, typeCheckForAutocomplete
    );
    LUAU_ASSERT(loadResult.success);

    TypeId genericK = arena.addType(GenericType{globalScope, "K"});
    TypeId genericV = arena.addType(GenericType{globalScope, "V"});
    TypeId mapOfKtoV = arena.addType(TableType{{}, TableIndexer(genericK, genericV), globals.globalScope->level, TableState::Generic});

    std::optional<TypeId> stringMetatableTy = getMetatable(builtinTypes->stringType, builtinTypes);
    LUAU_ASSERT(stringMetatableTy);
    const TableType* stringMetatableTable = get<TableType>(follow(*stringMetatableTy));
    LUAU_ASSERT(stringMetatableTable);

    auto it = stringMetatableTable->props.find("__index");
    LUAU_ASSERT(it != stringMetatableTable->props.end());

    addGlobalBinding(globals, "string", *it->second.readTy, "@luau");
    addGlobalBinding(globals, "string", *it->second.writeTy, "@luau");

    // Setup 'vector' metatable
    if (auto it = globals.globalScope->exportedTypeBindings.find("vector"); it != globals.globalScope->exportedTypeBindings.end())
    {
        TypeId vectorTy = it->second.type;
        ExternType* vectorCls = getMutable<ExternType>(vectorTy);

        vectorCls->metatable = arena.addType(TableType{{}, std::nullopt, TypeLevel{}, TableState::Sealed});
        TableType* metatableTy = Luau::getMutable<TableType>(vectorCls->metatable);

        metatableTy->props["__add"] = {makeFunction(arena, vectorTy, {vectorTy}, {vectorTy})};
        metatableTy->props["__sub"] = {makeFunction(arena, vectorTy, {vectorTy}, {vectorTy})};
        metatableTy->props["__unm"] = {makeFunction(arena, vectorTy, {}, {vectorTy})};

        std::initializer_list<TypeId> mulOverloads{
            makeFunction(arena, vectorTy, {vectorTy}, {vectorTy}),
            makeFunction(arena, vectorTy, {builtinTypes->numberType}, {vectorTy}),
        };
        metatableTy->props["__mul"] = {makeIntersection(arena, mulOverloads)};
        metatableTy->props["__div"] = {makeIntersection(arena, mulOverloads)};
        metatableTy->props["__idiv"] = {makeIntersection(arena, mulOverloads)};
    }

    // next<K, V>(t: Table<K, V>, i: K?) -> (K?, V)
    TypePackId nextArgsTypePack = arena.addTypePack(TypePack{{mapOfKtoV, makeOption(builtinTypes, arena, genericK)}});
    TypePackId nextRetsTypePack = arena.addTypePack(TypePack{{makeOption(builtinTypes, arena, genericK), genericV}});
    addGlobalBinding(globals, "next", arena.addType(FunctionType{{genericK, genericV}, {}, nextArgsTypePack, nextRetsTypePack}), "@luau");

    TypePackId pairsArgsTypePack = arena.addTypePack({mapOfKtoV});

    TypeId pairsNext = arena.addType(FunctionType{nextArgsTypePack, nextRetsTypePack});
    TypePackId pairsReturnTypePack = arena.addTypePack(TypePack{{pairsNext, mapOfKtoV, builtinTypes->nilType}});

    // pairs<K, V>(t: Table<K, V>) -> ((Table<K, V>, K?) -> (K, V), Table<K, V>, nil)
    addGlobalBinding(globals, "pairs", arena.addType(FunctionType{{genericK, genericV}, {}, pairsArgsTypePack, pairsReturnTypePack}), "@luau");

    TypeId genericMT = arena.addType(GenericType{globalScope, "MT"});

    TableType tab{TableState::Generic, globals.globalScope->level};
    TypeId tabTy = arena.addType(std::move(tab));

    TypeId tableMetaMT = arena.addType(MetatableType{tabTy, genericMT});

    TypeId genericT = arena.addType(GenericType{globalScope, "T"});

    if (frontend.getLuauSolverMode() == SolverMode::New)
    {
        // getmetatable : <T>(T) -> getmetatable<T>
        TypeId getmtReturn = arena.addType(
            TypeFunctionInstanceType{
                FFlag::LuauBuiltinTypeFunctionsArentGlobal ? builtinTypes->typeFunctions->getmetatableFunc
                                                           : builtinTypeFunctions_DEPRECATED().getmetatableFunc,
                {genericT}
            }
        );
        addGlobalBinding(globals, "getmetatable", makeFunction(arena, std::nullopt, {genericT}, {}, {genericT}, {getmtReturn}), "@luau");
    }
    else
    {
        // getmetatable : <MT>({ @metatable MT, {+ +} }) -> MT
        addGlobalBinding(globals, "getmetatable", makeFunction(arena, std::nullopt, {genericMT}, {}, {tableMetaMT}, {genericMT}), "@luau");
    }

    if (frontend.getLuauSolverMode() == SolverMode::New)
    {
        // setmetatable<T: {}, MT>(T, MT) -> setmetatable<T, MT>
        TypeId setmtReturn = arena.addType(
            TypeFunctionInstanceType{
                FFlag::LuauBuiltinTypeFunctionsArentGlobal ? builtinTypes->typeFunctions->setmetatableFunc
                                                           : builtinTypeFunctions_DEPRECATED().setmetatableFunc,
                {genericT, genericMT}
            }
        );
        addGlobalBinding(
            globals, "setmetatable", makeFunction(arena, std::nullopt, {genericT, genericMT}, {}, {genericT, genericMT}, {setmtReturn}), "@luau"
        );
    }
    else
    {
        // clang-format off
        // setmetatable<T: {}, MT>(T, MT) -> { @metatable MT, T }
        addGlobalBinding(globals, "setmetatable",
            arena.addType(
                FunctionType{
                    {genericMT},
                    {},
                    arena.addTypePack(TypePack{{tabTy, genericMT}}),
                    arena.addTypePack(TypePack{{tableMetaMT}})
                }
            ), "@luau"
        );
        // clang-format on
    }

    finalizeGlobalBindings(globals.globalScope);

    attachMagicFunction(getGlobalBinding(globals, "assert"), std::make_shared<MagicAssert>());

    if (frontend.getLuauSolverMode() == SolverMode::New)
    {
        // declare function assert<T>(value: T, errorMessage: string?): intersect<T, ~(false?)>
        TypeId genericT = arena.addType(GenericType{globalScope, "T"});
        TypeId refinedTy = arena.addType(
            TypeFunctionInstanceType{
                NotNull{
                    FFlag::LuauBuiltinTypeFunctionsArentGlobal ? &builtinTypes->typeFunctions->intersectFunc
                                                               : &builtinTypeFunctions_DEPRECATED().intersectFunc
                },
                {genericT, arena.addType(NegationType{builtinTypes->falsyType})},
                {}
            }
        );

        TypeId assertTy = arena.addType(
            FunctionType{
                {genericT}, {}, arena.addTypePack(TypePack{{genericT, builtinTypes->optionalStringType}}), arena.addTypePack(TypePack{{refinedTy}})
            }
        );
        addGlobalBinding(globals, "assert", assertTy, "@luau");
    }

    attachMagicFunction(getGlobalBinding(globals, "setmetatable"), std::make_shared<MagicSetMetatable>());
    attachMagicFunction(getGlobalBinding(globals, "select"), std::make_shared<MagicSelect>());

    if (TableType* ttv = getMutable<TableType>(getGlobalBinding(globals, "table")))
    {
        if (frontend.getLuauSolverMode() == SolverMode::New)
        {
            // CLI-114044 - The new solver does not yet support generic tables,
            // which act, in an odd way, like generics that are constrained to
            // the top table type.  We do the best we can by modelling these
            // functions using unconstrained generics.  It's not quite right,
            // but it'll be ok for now.
            TypeId genericTy = arena.addType(GenericType{globalScope, "T"});
            TypePackId thePack = arena.addTypePack({genericTy});
            TypeId idTyWithMagic = arena.addType(FunctionType{{genericTy}, {}, thePack, thePack});
            ttv->props["freeze"] = makeProperty(idTyWithMagic, "@luau/global/table.freeze");
            inferGenericPolarities(NotNull{&globals.globalTypes}, NotNull{globalScope}, idTyWithMagic);

            TypeId idTy = arena.addType(FunctionType{{genericTy}, {}, thePack, thePack});
            inferGenericPolarities(NotNull{&globals.globalTypes}, NotNull{globalScope}, idTy);
            ttv->props["clone"] = makeProperty(idTy, "@luau/global/table.clone");
        }
        else
        {
            // tabTy is a generic table type which we can't express via declaration syntax yet
            ttv->props["freeze"] = makeProperty(makeFunction(arena, std::nullopt, {tabTy}, {tabTy}), "@luau/global/table.freeze");
            ttv->props["clone"] = makeProperty(makeFunction(arena, std::nullopt, {tabTy}, {tabTy}), "@luau/global/table.clone");
        }

        ttv->props["getn"].deprecated = true;
        ttv->props["getn"].deprecatedSuggestion = "#";
        ttv->props["foreach"].deprecated = true;
        ttv->props["foreachi"].deprecated = true;

        attachMagicFunction(*ttv->props["pack"].readTy, std::make_shared<MagicPack>());
        if (FFlag::LuauTableCloneClonesType3)
            attachMagicFunction(*ttv->props["clone"].readTy, std::make_shared<MagicClone>());
        attachMagicFunction(*ttv->props["freeze"].readTy, std::make_shared<MagicFreeze>());
    }

    TypeId requireTy = getGlobalBinding(globals, "require");
    attachTag(requireTy, kRequireTagName);
    attachMagicFunction(requireTy, std::make_shared<MagicRequire>());

    // Global scope cannot be the parent of the type checking environment because it can be changed by the embedder
    globals.globalTypeFunctionScope->exportedTypeBindings = globals.globalScope->exportedTypeBindings;
    globals.globalTypeFunctionScope->builtinTypeNames = globals.globalScope->builtinTypeNames;

    // Type function runtime also removes a few standard libraries and globals, so we will take only the ones that are defined
    static constexpr const char* typeFunctionRuntimeBindings[] = {
        // Libraries
        "math",
        "table",
        "string",
        "bit32",
        "utf8",
        "buffer",

        // Globals
        "assert",
        "error",
        "print",
        "next",
        "ipairs",
        "pairs",
        "select",
        "unpack",
        "getmetatable",
        "setmetatable",
        "rawget",
        "rawset",
        "rawlen",
        "rawequal",
        "tonumber",
        "tostring",
        "type",
        "typeof",
    };

    for (auto& name : typeFunctionRuntimeBindings)
    {
        AstName astName = globals.globalNames.names->get(name);
        LUAU_ASSERT(astName.value);

        globals.globalTypeFunctionScope->bindings[astName] = globals.globalScope->bindings[astName];
    }

    LoadDefinitionFileResult typeFunctionLoadResult = frontend.loadDefinitionFile(
        globals, globals.globalTypeFunctionScope, getTypeFunctionDefinitionSource(), "@luau", /* captureComments */ false, false
    );
    LUAU_ASSERT(typeFunctionLoadResult.success);

    finalizeGlobalBindings(globals.globalTypeFunctionScope);
}

static std::vector<TypeId> parseFormatString(NotNull<BuiltinTypes> builtinTypes, const char* data, size_t size)
{
    const char* options = "cdiouxXeEfgGqs*";

    std::vector<TypeId> result;

    for (size_t i = 0; i < size; ++i)
    {
        if (data[i] == '%')
        {
            i++;

            if (i < size && data[i] == '%')
                continue;

            // we just ignore all characters (including flags/precision) up until first alphabetic character
            while (i < size && !(data[i] > 0 && (isalpha(data[i]) || data[i] == '*')))
                i++;

            if (i == size)
                break;

            if (data[i] == 'q' || data[i] == 's')
                result.push_back(builtinTypes->stringType);
            else if (data[i] == '*')
                result.push_back(builtinTypes->unknownType);
            else if (strchr(options, data[i]))
                result.push_back(builtinTypes->numberType);
            else
                result.push_back(builtinTypes->errorRecoveryType(builtinTypes->anyType));
        }
    }

    return result;
}

std::optional<WithPredicate<TypePackId>> MagicFormat::handleOldSolver(
    TypeChecker& typechecker,
    const ScopePtr& scope,
    const AstExprCall& expr,
    WithPredicate<TypePackId> withPredicate
)
{
    auto [paramPack, _predicates] = std::move(withPredicate);

    TypeArena& arena = typechecker.currentModule->internalTypes;

    AstExprConstantString* fmt = nullptr;
    if (auto index = expr.func->as<AstExprIndexName>(); index && expr.self)
    {
        if (auto group = index->expr->as<AstExprGroup>())
            fmt = group->expr->as<AstExprConstantString>();
        else
            fmt = index->expr->as<AstExprConstantString>();
    }

    if (!expr.self && expr.args.size > 0)
        fmt = expr.args.data[0]->as<AstExprConstantString>();

    if (!fmt)
        return std::nullopt;

    std::vector<TypeId> expected = parseFormatString(typechecker.builtinTypes, fmt->value.data, fmt->value.size);
    const auto& [params, tail] = flatten(paramPack);

    size_t paramOffset = 1;
    size_t dataOffset = expr.self ? 0 : 1;

    // unify the prefix one argument at a time
    for (size_t i = 0; i < expected.size() && i + paramOffset < params.size(); ++i)
    {
        Location location = expr.args.data[std::min(i + dataOffset, expr.args.size - 1)]->location;

        typechecker.unify(params[i + paramOffset], expected[i], scope, location);
    }

    // if we know the argument count or if we have too many arguments for sure, we can issue an error
    size_t numActualParams = params.size();
    size_t numExpectedParams = expected.size() + 1; // + 1 for the format string

    if (numExpectedParams != numActualParams && (!tail || numExpectedParams < numActualParams))
        typechecker.reportError(TypeError{expr.location, CountMismatch{numExpectedParams, std::nullopt, numActualParams}});

    return WithPredicate<TypePackId>{arena.addTypePack({typechecker.stringType})};
}

bool MagicFormat::infer(const MagicFunctionCallContext& context)
{
    TypeArena* arena = context.solver->arena;

    auto iter = begin(context.arguments);

    // we'll suppress any errors for `string.format` if the format string is error suppressing.
    if (iter == end(context.arguments) || shouldSuppressErrors(context.solver->normalizer, follow(*iter)) == ErrorSuppression::Suppress)
    {
        TypePackId resultPack = arena->addTypePack({context.solver->builtinTypes->stringType});
        asMutable(context.result)->ty.emplace<BoundTypePack>(resultPack);
        return true;
    }

    AstExprConstantString* fmt = nullptr;
    if (auto index = context.callSite->func->as<AstExprIndexName>(); index && context.callSite->self)
        fmt = unwrapGroup(index->expr)->as<AstExprConstantString>();

    if (!context.callSite->self && context.callSite->args.size > 0)
        fmt = context.callSite->args.data[0]->as<AstExprConstantString>();

    std::optional<std::string_view> formatString;
    if (fmt)
        formatString = {fmt->value.data, fmt->value.size};
    else if (auto singleton = get<SingletonType>(follow(*iter)))
    {
        if (auto stringSingleton = get<StringSingleton>(singleton))
            formatString = {stringSingleton->value};
    }

    if (!formatString)
        return false;

    std::vector<TypeId> expected = parseFormatString(context.solver->builtinTypes, formatString->data(), formatString->size());
    const auto& [params, tail] = flatten(context.arguments);

    size_t paramOffset = 1;

    // unify the prefix one argument at a time - needed if any of the involved types are free
    for (size_t i = 0; i < expected.size() && i + paramOffset < params.size(); ++i)
    {
        context.solver->unify(context.constraint, params[i + paramOffset], expected[i]);
    }

    // if we know the argument count or if we have too many arguments for sure, we can issue an error
    size_t numActualParams = params.size();
    size_t numExpectedParams = expected.size() + 1; // + 1 for the format string

    if (numExpectedParams != numActualParams && (!tail || numExpectedParams < numActualParams))
        context.solver->reportError(TypeError{context.callSite->location, CountMismatch{numExpectedParams, std::nullopt, numActualParams}});

    // This is invoked at solve time, so we just need to provide a type for the result of :/.format
    TypePackId resultPack = arena->addTypePack({context.solver->builtinTypes->stringType});
    asMutable(context.result)->ty.emplace<BoundTypePack>(resultPack);

    return true;
}

bool MagicFormat::typeCheck(const MagicFunctionTypeCheckContext& context)
{
    auto iter = begin(context.arguments);

    if (iter == end(context.arguments))
    {
        context.typechecker->reportError(CountMismatch{1, std::nullopt, 0, CountMismatch::Arg, true, "string.format"}, context.callSite->location);
        return true;
    }

    // we'll suppress any errors for `string.format` if the format string is error suppressing.
    if (shouldSuppressErrors(NotNull{&context.typechecker->normalizer}, follow(*iter)) == ErrorSuppression::Suppress)
    {
        return true;
    }

    AstExprConstantString* fmt = nullptr;
    if (auto index = context.callSite->func->as<AstExprIndexName>(); index && context.callSite->self)
        fmt = unwrapGroup(index->expr)->as<AstExprConstantString>();

    if (!context.callSite->self && context.callSite->args.size > 0)
        fmt = context.callSite->args.data[0]->as<AstExprConstantString>();

    std::optional<std::string_view> formatString;
    if (fmt)
        formatString = {fmt->value.data, fmt->value.size};
    else if (auto singleton = get<SingletonType>(follow(*iter)))
    {
        if (auto stringSingleton = get<StringSingleton>(singleton))
            formatString = {stringSingleton->value};
    }

    if (!formatString)
    {
        context.typechecker->reportError(CannotCheckDynamicStringFormatCalls{}, context.callSite->location);
        return true;
    }

    // CLI-150726: The block below effectively constructs a type pack and then type checks it by going parameter-by-parameter.
    // This does _not_ handle cases like:
    //
    //  local foo : () -> (...string) = (nil :: any)
    //  print(string.format("%s %d %s", foo()))
    //
    // ... which should be disallowed.

    std::vector<TypeId> expected = parseFormatString(context.builtinTypes, formatString->data(), formatString->size());

    const auto& [params, tail] = flatten(context.arguments);

    size_t paramOffset = 1;
    // Compare the expressions passed with the types the function expects to determine whether this function was called with : or .
    bool calledWithSelf = expected.size() == context.callSite->args.size;
    // unify the prefix one argument at a time
    for (size_t i = 0; i < expected.size() && i + paramOffset < params.size(); ++i)
    {
        TypeId actualTy = params[i + paramOffset];
        TypeId expectedTy = expected[i];
        Location location = context.callSite->args.data[std::min(context.callSite->args.size - 1, i + (calledWithSelf ? 0 : paramOffset))]->location;
        // use subtyping instead here
        SubtypingResult result = context.typechecker->subtyping->isSubtype(actualTy, expectedTy, context.checkScope);

        if (!result.isSubtype)
        {
            switch (shouldSuppressErrors(NotNull{&context.typechecker->normalizer}, actualTy))
            {
            case ErrorSuppression::Suppress:
                break;
            case ErrorSuppression::NormalizationFailed:
                break;
            case ErrorSuppression::DoNotSuppress:
                Reasonings reasonings = context.typechecker->explainReasonings(actualTy, expectedTy, location, result);

                if (!reasonings.suppressed)
                    context.typechecker->reportError(TypeMismatch{expectedTy, actualTy, reasonings.toString()}, location);
            }
        }
    }

    return true;
}

static std::vector<TypeId> parsePatternString(NotNull<BuiltinTypes> builtinTypes, const char* data, size_t size)
{
    std::vector<TypeId> result;
    int depth = 0;
    bool parsingSet = false;

    for (size_t i = 0; i < size; ++i)
    {
        if (data[i] == '%')
        {
            ++i;
            if (!parsingSet && i < size && data[i] == 'b')
                i += 2;
        }
        else if (!parsingSet && data[i] == '[')
        {
            parsingSet = true;
            if (i + 1 < size && data[i + 1] == ']')
                i += 1;
        }
        else if (parsingSet && data[i] == ']')
        {
            parsingSet = false;
        }
        else if (data[i] == '(')
        {
            if (parsingSet)
                continue;

            if (i + 1 < size && data[i + 1] == ')')
            {
                i++;
                result.push_back(builtinTypes->optionalNumberType);
                continue;
            }

            ++depth;
            result.push_back(builtinTypes->optionalStringType);
        }
        else if (data[i] == ')')
        {
            if (parsingSet)
                continue;

            --depth;

            if (depth < 0)
                break;
        }
    }

    if (depth != 0 || parsingSet)
        return std::vector<TypeId>();

    if (result.empty())
        result.push_back(builtinTypes->optionalStringType);

    return result;
}

std::optional<WithPredicate<TypePackId>> MagicGmatch::handleOldSolver(
    TypeChecker& typechecker,
    const ScopePtr& scope,
    const AstExprCall& expr,
    WithPredicate<TypePackId> withPredicate
)
{
    auto [paramPack, _predicates] = std::move(withPredicate);
    const auto& [params, tail] = flatten(paramPack);

    if (params.size() != 2)
        return std::nullopt;

    TypeArena& arena = typechecker.currentModule->internalTypes;

    AstExprConstantString* pattern = nullptr;
    size_t index = expr.self ? 0 : 1;
    if (expr.args.size > index)
        pattern = expr.args.data[index]->as<AstExprConstantString>();

    if (!pattern)
        return std::nullopt;

    std::vector<TypeId> returnTypes = parsePatternString(typechecker.builtinTypes, pattern->value.data, pattern->value.size);

    if (returnTypes.empty())
        return std::nullopt;

    typechecker.unify(params[0], typechecker.stringType, scope, expr.args.data[0]->location);

    const TypePackId emptyPack = arena.addTypePack({});
    const TypePackId returnList = arena.addTypePack(std::move(returnTypes));
    const TypeId iteratorType = arena.addType(FunctionType{emptyPack, returnList});
    return WithPredicate<TypePackId>{arena.addTypePack({iteratorType})};
}

bool MagicGmatch::infer(const MagicFunctionCallContext& context)
{
    const auto& [params, tail] = flatten(context.arguments);

    if (params.size() != 2)
        return false;

    TypeArena* arena = context.solver->arena;

    AstExprConstantString* pattern = nullptr;
    size_t index = context.callSite->self ? 0 : 1;
    if (context.callSite->args.size > index)
        pattern = context.callSite->args.data[index]->as<AstExprConstantString>();

    if (!pattern)
        return false;

    std::vector<TypeId> returnTypes = parsePatternString(context.solver->builtinTypes, pattern->value.data, pattern->value.size);

    if (returnTypes.empty())
        return false;

    context.solver->unify(context.constraint, params[0], context.solver->builtinTypes->stringType);

    const TypePackId emptyPack = arena->addTypePack({});
    const TypePackId returnList = arena->addTypePack(std::move(returnTypes));
    const TypeId iteratorType = arena->addType(FunctionType{emptyPack, returnList});
    const TypePackId resTypePack = arena->addTypePack({iteratorType});
    asMutable(context.result)->ty.emplace<BoundTypePack>(resTypePack);

    return true;
}

std::optional<WithPredicate<TypePackId>> MagicMatch::handleOldSolver(
    TypeChecker& typechecker,
    const ScopePtr& scope,
    const AstExprCall& expr,
    WithPredicate<TypePackId> withPredicate
)
{
    auto [paramPack, _predicates] = std::move(withPredicate);
    const auto& [params, tail] = flatten(paramPack);

    if (params.size() < 2 || params.size() > 3)
        return std::nullopt;

    TypeArena& arena = typechecker.currentModule->internalTypes;

    AstExprConstantString* pattern = nullptr;
    size_t patternIndex = expr.self ? 0 : 1;
    if (expr.args.size > patternIndex)
        pattern = expr.args.data[patternIndex]->as<AstExprConstantString>();

    if (!pattern)
        return std::nullopt;

    std::vector<TypeId> returnTypes = parsePatternString(typechecker.builtinTypes, pattern->value.data, pattern->value.size);

    if (returnTypes.empty())
        return std::nullopt;

    typechecker.unify(params[0], typechecker.stringType, scope, expr.args.data[0]->location);

    const TypeId optionalNumber = arena.addType(UnionType{{typechecker.nilType, typechecker.numberType}});

    size_t initIndex = expr.self ? 1 : 2;
    if (params.size() == 3 && expr.args.size > initIndex)
        typechecker.unify(params[2], optionalNumber, scope, expr.args.data[initIndex]->location);

    const TypePackId returnList = arena.addTypePack(std::move(returnTypes));
    return WithPredicate<TypePackId>{returnList};
}

bool MagicMatch::infer(const MagicFunctionCallContext& context)
{
    const auto& [params, tail] = flatten(context.arguments);

    if (params.size() < 2 || params.size() > 3)
        return false;

    TypeArena* arena = context.solver->arena;

    AstExprConstantString* pattern = nullptr;
    size_t patternIndex = context.callSite->self ? 0 : 1;
    if (context.callSite->args.size > patternIndex)
        pattern = context.callSite->args.data[patternIndex]->as<AstExprConstantString>();

    if (!pattern)
        return false;

    std::vector<TypeId> returnTypes = parsePatternString(context.solver->builtinTypes, pattern->value.data, pattern->value.size);

    if (returnTypes.empty())
        return false;

    context.solver->unify(context.constraint, params[0], context.solver->builtinTypes->stringType);

    const TypeId optionalNumber = arena->addType(UnionType{{context.solver->builtinTypes->nilType, context.solver->builtinTypes->numberType}});

    size_t initIndex = context.callSite->self ? 1 : 2;
    if (params.size() == 3 && context.callSite->args.size > initIndex)
        context.solver->unify(context.constraint, params[2], optionalNumber);

    const TypePackId returnList = arena->addTypePack(std::move(returnTypes));
    asMutable(context.result)->ty.emplace<BoundTypePack>(returnList);

    return true;
}

std::optional<WithPredicate<TypePackId>> MagicFind::handleOldSolver(
    TypeChecker& typechecker,
    const ScopePtr& scope,
    const AstExprCall& expr,
    WithPredicate<TypePackId> withPredicate
)
{
    auto [paramPack, _predicates] = std::move(withPredicate);
    const auto& [params, tail] = flatten(paramPack);

    if (params.size() < 2 || params.size() > 4)
        return std::nullopt;

    TypeArena& arena = typechecker.currentModule->internalTypes;

    AstExprConstantString* pattern = nullptr;
    size_t patternIndex = expr.self ? 0 : 1;
    if (expr.args.size > patternIndex)
        pattern = expr.args.data[patternIndex]->as<AstExprConstantString>();

    if (!pattern)
        return std::nullopt;

    bool plain = false;
    size_t plainIndex = expr.self ? 2 : 3;
    if (expr.args.size > plainIndex)
    {
        AstExprConstantBool* p = expr.args.data[plainIndex]->as<AstExprConstantBool>();
        plain = p && p->value;
    }

    std::vector<TypeId> returnTypes;
    if (!plain)
    {
        returnTypes = parsePatternString(typechecker.builtinTypes, pattern->value.data, pattern->value.size);

        if (returnTypes.empty())
            return std::nullopt;
    }

    typechecker.unify(params[0], typechecker.stringType, scope, expr.args.data[0]->location);

    const TypeId optionalNumber = arena.addType(UnionType{{typechecker.nilType, typechecker.numberType}});
    const TypeId optionalBoolean = arena.addType(UnionType{{typechecker.nilType, typechecker.booleanType}});

    size_t initIndex = expr.self ? 1 : 2;
    if (params.size() >= 3 && expr.args.size > initIndex)
        typechecker.unify(params[2], optionalNumber, scope, expr.args.data[initIndex]->location);

    if (params.size() == 4 && expr.args.size > plainIndex)
        typechecker.unify(params[3], optionalBoolean, scope, expr.args.data[plainIndex]->location);

    returnTypes.insert(returnTypes.begin(), {optionalNumber, optionalNumber});

    const TypePackId returnList = arena.addTypePack(std::move(returnTypes));
    return WithPredicate<TypePackId>{returnList};
}

bool MagicFind::infer(const MagicFunctionCallContext& context)
{
    const auto& [params, tail] = flatten(context.arguments);

    if (params.size() < 2 || params.size() > 4)
        return false;

    TypeArena* arena = context.solver->arena;
    NotNull<BuiltinTypes> builtinTypes = context.solver->builtinTypes;

    AstExprConstantString* pattern = nullptr;
    size_t patternIndex = context.callSite->self ? 0 : 1;
    if (context.callSite->args.size > patternIndex)
        pattern = context.callSite->args.data[patternIndex]->as<AstExprConstantString>();

    if (!pattern)
        return false;

    bool plain = false;
    size_t plainIndex = context.callSite->self ? 2 : 3;
    if (context.callSite->args.size > plainIndex)
    {
        AstExprConstantBool* p = context.callSite->args.data[plainIndex]->as<AstExprConstantBool>();
        plain = p && p->value;
    }

    std::vector<TypeId> returnTypes;
    if (!plain)
    {
        returnTypes = parsePatternString(builtinTypes, pattern->value.data, pattern->value.size);

        if (returnTypes.empty())
            return false;
    }

    context.solver->unify(context.constraint, params[0], builtinTypes->stringType);

    const TypeId optionalNumber = arena->addType(UnionType{{builtinTypes->nilType, builtinTypes->numberType}});
    const TypeId optionalBoolean = arena->addType(UnionType{{builtinTypes->nilType, builtinTypes->booleanType}});

    size_t initIndex = context.callSite->self ? 1 : 2;
    if (params.size() >= 3 && context.callSite->args.size > initIndex)
        context.solver->unify(context.constraint, params[2], optionalNumber);

    if (params.size() == 4 && context.callSite->args.size > plainIndex)
        context.solver->unify(context.constraint, params[3], optionalBoolean);

    returnTypes.insert(returnTypes.begin(), {optionalNumber, optionalNumber});

    const TypePackId returnList = arena->addTypePack(std::move(returnTypes));
    asMutable(context.result)->ty.emplace<BoundTypePack>(returnList);
    return true;
}

TypeId makeStringMetatable(NotNull<BuiltinTypes> builtinTypes, SolverMode mode)
{
    NotNull<TypeArena> arena{builtinTypes->arena.get()};

    const TypeId nilType = builtinTypes->nilType;
    const TypeId numberType = builtinTypes->numberType;
    const TypeId booleanType = builtinTypes->booleanType;
    const TypeId stringType = builtinTypes->stringType;

    const TypeId optionalNumber = arena->addType(UnionType{{nilType, numberType}});
    const TypeId optionalString = arena->addType(UnionType{{nilType, stringType}});
    const TypeId optionalBoolean = arena->addType(UnionType{{nilType, booleanType}});

    const TypePackId oneStringPack = arena->addTypePack({stringType});
    const TypePackId anyTypePack = builtinTypes->anyTypePack;

    const TypePackId variadicTailPack = (FFlag::LuauUseWorkspacePropToChooseSolver && mode == SolverMode::New) ? builtinTypes->unknownTypePack
                                        : FFlag::LuauSolverV2                                                  ? builtinTypes->unknownTypePack
                                                                                                               : anyTypePack;
    const TypePackId emptyPack = arena->addTypePack({});
    const TypePackId stringVariadicList = arena->addTypePack(TypePackVar{VariadicTypePack{stringType}});
    const TypePackId numberVariadicList = arena->addTypePack(TypePackVar{VariadicTypePack{numberType}});


    FunctionType formatFTV{arena->addTypePack(TypePack{{stringType}, variadicTailPack}), oneStringPack};
    formatFTV.isCheckedFunction = true;
    const TypeId formatFn = arena->addType(std::move(formatFTV));
    attachMagicFunction(formatFn, std::make_shared<MagicFormat>());


    const TypeId stringToStringType = makeFunction(*arena, std::nullopt, {}, {}, {stringType}, {}, {stringType}, /* checked */ true);

    const TypeId replArgType = arena->addType(
        UnionType{
            {stringType,
             arena->addType(TableType({}, TableIndexer(stringType, stringType), TypeLevel{}, TableState::Generic)),
             makeFunction(*arena, std::nullopt, {}, {}, {stringType}, {}, {stringType}, /* checked */ false)}
        }
    );
    const TypeId gsubFunc =
        makeFunction(*arena, stringType, {}, {}, {stringType, replArgType, optionalNumber}, {}, {stringType, numberType}, /* checked */ false);
    const TypeId gmatchFunc =
        makeFunction(*arena, stringType, {}, {}, {stringType}, {}, {arena->addType(FunctionType{emptyPack, stringVariadicList})}, /* checked */ true);
    attachMagicFunction(gmatchFunc, std::make_shared<MagicGmatch>());

    FunctionType matchFuncTy{
        arena->addTypePack({stringType, stringType, optionalNumber}), arena->addTypePack(TypePackVar{VariadicTypePack{stringType}})
    };
    matchFuncTy.isCheckedFunction = true;
    const TypeId matchFunc = arena->addType(std::move(matchFuncTy));
    attachMagicFunction(matchFunc, std::make_shared<MagicMatch>());

    FunctionType findFuncTy{
        arena->addTypePack({stringType, stringType, optionalNumber, optionalBoolean}),
        arena->addTypePack(TypePack{{optionalNumber, optionalNumber}, stringVariadicList})
    };
    findFuncTy.isCheckedFunction = true;
    const TypeId findFunc = arena->addType(std::move(findFuncTy));
    attachMagicFunction(findFunc, std::make_shared<MagicFind>());

    // string.byte : string -> number? -> number? -> ...number
    FunctionType stringDotByte{arena->addTypePack({stringType, optionalNumber, optionalNumber}), numberVariadicList};
    stringDotByte.isCheckedFunction = true;

    // string.char : .... number -> string
    FunctionType stringDotChar{numberVariadicList, arena->addTypePack({stringType})};
    stringDotChar.isCheckedFunction = true;

    // string.unpack : string -> string -> number? -> ...any
    FunctionType stringDotUnpack{
        arena->addTypePack(TypePack{{stringType, stringType, optionalNumber}}),
        variadicTailPack,
    };
    stringDotUnpack.isCheckedFunction = true;

    TableType::Props stringLib = {
        {"byte", {arena->addType(std::move(stringDotByte))}},
        {"char", {arena->addType(std::move(stringDotChar))}},
        {"find", {findFunc}},
        {"format", {formatFn}}, // FIXME
        {"gmatch", {gmatchFunc}},
        {"gsub", {gsubFunc}},
        {"len", {makeFunction(*arena, stringType, {}, {}, {}, {}, {numberType}, /* checked */ true)}},
        {"lower", {stringToStringType}},
        {"match", {matchFunc}},
        {"rep", {makeFunction(*arena, stringType, {}, {}, {numberType}, {}, {stringType}, /* checked */ true)}},
        {"reverse", {stringToStringType}},
        {"sub", {makeFunction(*arena, stringType, {}, {}, {numberType, optionalNumber}, {}, {stringType}, /* checked */ true)}},
        {"upper", {stringToStringType}},
        {"split",
         {makeFunction(
             *arena,
             stringType,
             {},
             {},
             {optionalString},
             {},
             {arena->addType(TableType{{}, TableIndexer{numberType, stringType}, TypeLevel{}, TableState::Sealed})},
             /* checked */ true
         )}},
        {"pack",
         {arena->addType(
             FunctionType{
                 arena->addTypePack(TypePack{{stringType}, variadicTailPack}),
                 oneStringPack,
             }
         )}},
        {"packsize", {makeFunction(*arena, stringType, {}, {}, {}, {}, {numberType}, /* checked */ true)}},
        {"unpack", {arena->addType(std::move(stringDotUnpack))}},
    };

    assignPropDocumentationSymbols(stringLib, "@luau/global/string");

    TypeId tableType = arena->addType(TableType{std::move(stringLib), std::nullopt, TypeLevel{}, TableState::Sealed});

    if (TableType* ttv = getMutable<TableType>(tableType))
        ttv->name = "typeof(string)";

    return arena->addType(TableType{{{{"__index", {tableType}}}}, std::nullopt, TypeLevel{}, TableState::Sealed});
}

std::optional<WithPredicate<TypePackId>> MagicSelect::handleOldSolver(
    TypeChecker& typechecker,
    const ScopePtr& scope,
    const AstExprCall& expr,
    WithPredicate<TypePackId> withPredicate
)
{
    auto [paramPack, _predicates] = std::move(withPredicate);

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

bool MagicSelect::infer(const MagicFunctionCallContext& context)
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

std::optional<WithPredicate<TypePackId>> MagicSetMetatable::handleOldSolver(
    TypeChecker& typechecker,
    const ScopePtr& scope,
    const AstExprCall& expr,
    WithPredicate<TypePackId> withPredicate
)
{
    auto [paramPack, _predicates] = std::move(withPredicate);

    if (size(paramPack) < 2 && finite(paramPack))
        return std::nullopt;

    TypeArena& arena = typechecker.currentModule->internalTypes;

    std::vector<TypeId> expectedArgs = typechecker.unTypePack(scope, paramPack, 2, expr.location);

    TypeId target = follow(expectedArgs[0]);
    TypeId mt = follow(expectedArgs[1]);

    typechecker.tablify(target);
    typechecker.tablify(mt);

    if (const auto& tab = get<TableType>(target))
    {
        if (target->persistent)
        {
            typechecker.reportError(TypeError{expr.location, CannotExtendTable{target, CannotExtendTable::Metatable}});
        }
        else
        {
            const TableType* mtTtv = get<TableType>(mt);
            MetatableType mtv{target, mt};
            if ((tab->name || tab->syntheticName) && (mtTtv && (mtTtv->name || mtTtv->syntheticName)))
            {
                std::string tableName = tab->name ? *tab->name : *tab->syntheticName;
                std::string metatableName = mtTtv->name ? *mtTtv->name : *mtTtv->syntheticName;

                if (tableName == metatableName)
                    mtv.syntheticName = tableName;
            }

            TypeId mtTy = arena.addType(std::move(mtv));

            if (expr.args.size < 1)
                return std::nullopt;

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
    else if (isTableUnion(target))
    {
        const UnionType* ut = get<UnionType>(target);
        LUAU_ASSERT(ut);

        std::vector<TypeId> resultParts;

        for (TypeId ty : ut)
            resultParts.push_back(arena.addType(MetatableType{ty, mt}));

        return WithPredicate<TypePackId>{arena.addTypePack({arena.addType(UnionType{std::move(resultParts)})})};
    }
    else
    {
        typechecker.reportError(TypeError{expr.location, GenericError{"setmetatable should take a table"}});
    }

    return WithPredicate<TypePackId>{arena.addTypePack({target})};
}

bool MagicSetMetatable::infer(const MagicFunctionCallContext&)
{
    return false;
}

std::optional<WithPredicate<TypePackId>> MagicAssert::handleOldSolver(
    TypeChecker& typechecker,
    const ScopePtr& scope,
    const AstExprCall& expr,
    WithPredicate<TypePackId> withPredicate
)
{
    auto [paramPack, predicates] = std::move(withPredicate);

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
        if (get<NeverType>(*ty))
            head = {*ty};
        else
            head[0] = *ty;
    }

    return WithPredicate<TypePackId>{arena.addTypePack(TypePack{std::move(head), tail})};
}

bool MagicAssert::infer(const MagicFunctionCallContext&)
{
    return false;
}

std::optional<WithPredicate<TypePackId>> MagicPack::handleOldSolver(
    TypeChecker& typechecker,
    const ScopePtr& scope,
    const AstExprCall& expr,
    WithPredicate<TypePackId> withPredicate
)
{
    auto [paramPack, _predicates] = std::move(withPredicate);

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

bool MagicPack::infer(const MagicFunctionCallContext& context)
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

std::optional<WithPredicate<TypePackId>> MagicClone::handleOldSolver(
    TypeChecker& typechecker,
    const ScopePtr& scope,
    const AstExprCall& expr,
    WithPredicate<TypePackId> withPredicate
)
{
    LUAU_ASSERT(FFlag::LuauTableCloneClonesType3);

    auto [paramPack, _predicates] = std::move(withPredicate);

    TypeArena& arena = typechecker.currentModule->internalTypes;

    const auto& [paramTypes, paramTail] = flatten(paramPack);
    if (paramTypes.empty() || expr.args.size == 0)
    {
        typechecker.reportError(expr.argLocation, CountMismatch{1, std::nullopt, 0});
        return std::nullopt;
    }

    TypeId inputType = follow(paramTypes[0]);

    if (!get<TableType>(inputType))
        return std::nullopt;

    CloneState cloneState{typechecker.builtinTypes};
    TypeId resultType = shallowClone(inputType, arena, cloneState, /* clonePersistentTypes */ false);

    TypePackId clonedTypePack = arena.addTypePack({resultType});
    return WithPredicate<TypePackId>{clonedTypePack};
}

bool MagicClone::infer(const MagicFunctionCallContext& context)
{
    LUAU_ASSERT(FFlag::LuauTableCloneClonesType3);

    TypeArena* arena = context.solver->arena;

    const auto& [paramTypes, paramTail] = flatten(context.arguments);
    if (paramTypes.empty() || context.callSite->args.size == 0)
    {
        context.solver->reportError(CountMismatch{1, std::nullopt, 0}, context.callSite->argLocation);
        return false;
    }

    TypeId inputType = follow(paramTypes[0]);

    if (!get<TableType>(inputType))
        return false;

    CloneState cloneState{context.solver->builtinTypes};
    TypeId resultType = shallowClone(inputType, *arena, cloneState, /* ignorePersistent */ true);

    if (auto tableType = getMutable<TableType>(resultType))
    {
        tableType->scope = context.constraint->scope.get();
    }

    trackInteriorFreeType(context.constraint->scope.get(), resultType);

    TypePackId clonedTypePack = arena->addTypePack({resultType});
    asMutable(context.result)->ty.emplace<BoundTypePack>(clonedTypePack);

    return true;
}

static std::optional<TypeId> freezeTable(TypeId inputType, const MagicFunctionCallContext& context)
{
    TypeArena* arena = context.solver->arena;
    inputType = follow(inputType);
    if (auto mt = get<MetatableType>(inputType))
    {
        std::optional<TypeId> frozenTable = freezeTable(mt->table, context);

        if (!frozenTable)
            return std::nullopt;

        TypeId resultType = arena->addType(MetatableType{*frozenTable, mt->metatable, mt->syntheticName});

        return resultType;
    }

    if (get<TableType>(inputType))
    {
        // Clone the input type, this will become our final result type after we mutate it.
        CloneState cloneState{context.solver->builtinTypes};
        TypeId resultType = shallowClone(inputType, *arena, cloneState, /* ignorePersistent */ true);
        auto tableTy = getMutable<TableType>(resultType);
        // `clone` should not break this.
        LUAU_ASSERT(tableTy);
        tableTy->state = TableState::Sealed;

        // We'll mutate the table to make every property type read-only.
        for (auto iter = tableTy->props.begin(); iter != tableTy->props.end();)
        {
            if (iter->second.isWriteOnly())
                iter = tableTy->props.erase(iter);
            else
            {
                iter->second.writeTy = std::nullopt;
                iter++;
            }
        }

        return resultType;
    }

    context.solver->reportError(TypeMismatch{context.solver->builtinTypes->tableType, inputType}, context.callSite->argLocation);
    return std::nullopt;
}

std::optional<WithPredicate<TypePackId>> MagicFreeze::handleOldSolver(
    struct TypeChecker&,
    const std::shared_ptr<struct Scope>&,
    const class AstExprCall&,
    WithPredicate<TypePackId>
)
{
    return std::nullopt;
}

bool MagicFreeze::infer(const MagicFunctionCallContext& context)
{
    TypeArena* arena = context.solver->arena;
    const DataFlowGraph* dfg = context.solver->dfg.get();
    Scope* scope = context.constraint->scope.get();

    const auto& [paramTypes, paramTail] = extendTypePack(*arena, context.solver->builtinTypes, context.arguments, 1);
    if (paramTypes.empty() || context.callSite->args.size == 0)
    {
        context.solver->reportError(CountMismatch{1, std::nullopt, 0}, context.callSite->argLocation);
        return false;
    }

    TypeId inputType = follow(paramTypes[0]);

    AstExpr* targetExpr = context.callSite->args.data[0];
    std::optional<DefId> resultDef = dfg->getDefOptional(targetExpr);
    std::optional<TypeId> resultTy = resultDef ? scope->lookup(*resultDef) : std::nullopt;

    if (resultTy && !get<BlockedType>(follow(resultTy)))
    {
        // If there's an existing result type, but it's _not_ blocked, then
        // we aren't type stating this builtin and should fall back to
        // regular inference.
        return false;
    }

    std::optional<TypeId> frozenType = freezeTable(inputType, context);

    // At this point: we know for sure that if `resultTy` exists, it is a
    // blocked type, and can safely emplace it.
    if (!frozenType)
    {
        if (resultTy)
            asMutable(*resultTy)->ty.emplace<BoundType>(context.solver->builtinTypes->errorType);
        asMutable(context.result)->ty.emplace<BoundTypePack>(context.solver->builtinTypes->errorTypePack);

        return true;
    }

    if (resultTy)
        asMutable(*resultTy)->ty.emplace<BoundType>(*frozenType);
    asMutable(context.result)->ty.emplace<BoundTypePack>(arena->addTypePack({*frozenType}));

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

std::optional<WithPredicate<TypePackId>> MagicRequire::handleOldSolver(
    TypeChecker& typechecker,
    const ScopePtr& scope,
    const AstExprCall& expr,
    WithPredicate<TypePackId> withPredicate
)
{
    TypeArena& arena = typechecker.currentModule->internalTypes;

    if (expr.args.size != 1)
    {
        typechecker.reportError(TypeError{expr.location, GenericError{"require takes 1 argument"}});
        return std::nullopt;
    }

    if (!checkRequirePath(typechecker, expr.args.data[0]))
        return std::nullopt;

    if (auto moduleInfo = typechecker.resolver->resolveModuleInfo(typechecker.currentModule->name, expr))
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

bool MagicRequire::infer(const MagicFunctionCallContext& context)
{
    if (context.callSite->args.size != 1)
    {
        context.solver->reportError(GenericError{"require takes 1 argument"}, context.callSite->location);
        return false;
    }

    if (!checkRequirePathDcr(context.solver, context.callSite->args.data[0]))
        return false;

    if (auto moduleInfo = context.solver->moduleResolver->resolveModuleInfo(context.solver->module->name, *context.callSite))
    {
        TypeId moduleType = context.solver->resolveModule(*moduleInfo, context.callSite->location);
        TypePackId moduleResult = context.solver->arena->addTypePack({moduleType});
        asMutable(context.result)->ty.emplace<BoundTypePack>(moduleResult);

        return true;
    }

    return false;
}

bool matchSetMetatable(const AstExprCall& call)
{
    const char* smt = "setmetatable";

    if (call.args.size != 2)
        return false;

    const AstExprGlobal* funcAsGlobal = call.func->as<AstExprGlobal>();
    if (!funcAsGlobal || funcAsGlobal->name != smt)
        return false;

    return true;
}

bool matchTableFreeze(const AstExprCall& call)
{
    if (call.args.size < 1)
        return false;

    const AstExprIndexName* index = call.func->as<AstExprIndexName>();
    if (!index || index->index != "freeze")
        return false;

    const AstExprGlobal* global = index->expr->as<AstExprGlobal>();
    if (!global || global->name != "table")
        return false;

    return true;
}

bool matchAssert(const AstExprCall& call)
{
    if (call.args.size < 1)
        return false;

    const AstExprGlobal* funcAsGlobal = call.func->as<AstExprGlobal>();
    if (!funcAsGlobal || funcAsGlobal->name != "assert")
        return false;

    return true;
}

bool shouldTypestateForFirstArgument(const AstExprCall& call)
{
    // TODO: magic function for setmetatable and assert and then add them
    return matchTableFreeze(call);
}

} // namespace Luau
