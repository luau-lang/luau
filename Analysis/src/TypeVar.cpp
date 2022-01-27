// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeVar.h"

#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/DenseHash.h"
#include "Luau/Error.h"
#include "Luau/RecursionCounter.h"
#include "Luau/StringUtils.h"
#include "Luau/ToString.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/VisitTypeVar.h"

#include <algorithm>
#include <optional>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>

LUAU_FASTFLAG(DebugLuauFreezeArena)

LUAU_FASTINTVARIABLE(LuauTypeMaximumStringifierLength, 500)
LUAU_FASTINTVARIABLE(LuauTableTypeMaximumStringifierLength, 0)
LUAU_FASTINT(LuauTypeInferRecursionLimit)
LUAU_FASTFLAG(LuauLengthOnCompositeType)
LUAU_FASTFLAGVARIABLE(LuauMetatableAreEqualRecursion, false)
LUAU_FASTFLAGVARIABLE(LuauRefactorTypeVarQuestions, false)
LUAU_FASTFLAG(LuauErrorRecoveryType)

namespace Luau
{

std::optional<ExprResult<TypePackId>> magicFunctionFormat(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult);

TypeId follow(TypeId t)
{
    return follow(t, [](TypeId t) {
        return t;
    });
}

TypeId follow(TypeId t, std::function<TypeId(TypeId)> mapper)
{
    auto advance = [&mapper](TypeId ty) -> std::optional<TypeId> {
        if (auto btv = get<Unifiable::Bound<TypeId>>(mapper(ty)))
            return btv->boundTo;
        else if (auto ttv = get<TableTypeVar>(mapper(ty)))
            return ttv->boundTo;
        else
            return std::nullopt;
    };

    auto force = [&mapper](TypeId ty) {
        if (auto ltv = get_if<LazyTypeVar>(&mapper(ty)->ty))
        {
            TypeId res = ltv->thunk();
            if (get<LazyTypeVar>(res))
                throw std::runtime_error("Lazy TypeVar cannot resolve to another Lazy TypeVar");

            *asMutable(ty) = BoundTypeVar(res);
        }
    };

    force(t);

    TypeId cycleTester = t; // Null once we've determined that there is no cycle
    if (auto a = advance(cycleTester))
        cycleTester = *a;
    else
        return t;

    while (true)
    {
        force(t);
        auto a1 = advance(t);
        if (a1)
            t = *a1;
        else
            return t;

        if (nullptr != cycleTester)
        {
            auto a2 = advance(cycleTester);
            if (a2)
            {
                auto a3 = advance(*a2);
                if (a3)
                    cycleTester = *a3;
                else
                    cycleTester = nullptr;
            }
            else
                cycleTester = nullptr;

            if (t == cycleTester)
                throw std::runtime_error("Luau::follow detected a TypeVar cycle!!");
        }
    }
}

std::vector<TypeId> flattenIntersection(TypeId ty)
{
    if (!get<IntersectionTypeVar>(follow(ty)))
        return {ty};

    std::unordered_set<TypeId> seen;
    std::deque<TypeId> queue{ty};

    std::vector<TypeId> result;

    while (!queue.empty())
    {
        TypeId current = follow(queue.front());
        queue.pop_front();

        if (seen.find(current) != seen.end())
            continue;

        seen.insert(current);

        if (auto itv = get<IntersectionTypeVar>(current))
        {
            for (TypeId ty : itv->parts)
                queue.push_back(ty);
        }
        else
            result.push_back(current);
    }

    return result;
}

bool isPrim(TypeId ty, PrimitiveTypeVar::Type primType)
{
    auto p = get<PrimitiveTypeVar>(follow(ty));
    return p && p->type == primType;
}

bool isNil(TypeId ty)
{
    return isPrim(ty, PrimitiveTypeVar::NilType);
}

bool isBoolean(TypeId ty)
{
    if (FFlag::LuauRefactorTypeVarQuestions)
    {
        if (isPrim(ty, PrimitiveTypeVar::Boolean) || get<BooleanSingleton>(get<SingletonTypeVar>(follow(ty))))
            return true;

        if (auto utv = get<UnionTypeVar>(follow(ty)))
            return std::all_of(begin(utv), end(utv), isBoolean);

        return false;
    }
    else
    {
        return isPrim(ty, PrimitiveTypeVar::Boolean);
    }
}

bool isNumber(TypeId ty)
{
    return isPrim(ty, PrimitiveTypeVar::Number);
}

bool isString(TypeId ty)
{
    if (FFlag::LuauRefactorTypeVarQuestions)
    {
        if (isPrim(ty, PrimitiveTypeVar::String) || get<StringSingleton>(get<SingletonTypeVar>(follow(ty))))
            return true;

        if (auto utv = get<UnionTypeVar>(follow(ty)))
            return std::all_of(begin(utv), end(utv), isString);

        return false;
    }
    else
    {
        return isPrim(ty, PrimitiveTypeVar::String);
    }
}

bool isThread(TypeId ty)
{
    return isPrim(ty, PrimitiveTypeVar::Thread);
}

bool isOptional(TypeId ty)
{
    if (isNil(ty))
        return true;

    if (FFlag::LuauRefactorTypeVarQuestions)
    {
        auto utv = get<UnionTypeVar>(follow(ty));
        if (!utv)
            return false;

        return std::any_of(begin(utv), end(utv), isNil);
    }
    else
    {
        std::unordered_set<TypeId> seen;
        std::deque<TypeId> queue{ty};
        while (!queue.empty())
        {
            TypeId current = follow(queue.front());
            queue.pop_front();

            if (seen.count(current))
                continue;

            seen.insert(current);

            if (isNil(current))
                return true;

            if (auto u = get<UnionTypeVar>(current))
            {
                for (TypeId option : u->options)
                {
                    if (isNil(option))
                        return true;

                    queue.push_back(option);
                }
            }
        }

        return false;
    }
}

bool isTableIntersection(TypeId ty)
{
    if (!get<IntersectionTypeVar>(follow(ty)))
        return false;

    std::vector<TypeId> parts = flattenIntersection(ty);
    return std::all_of(parts.begin(), parts.end(), getTableType);
}

bool isOverloadedFunction(TypeId ty)
{
    if (!get<IntersectionTypeVar>(follow(ty)))
        return false;

    auto isFunction = [](TypeId part) -> bool {
        return get<FunctionTypeVar>(part);
    };

    std::vector<TypeId> parts = flattenIntersection(ty);
    return std::all_of(parts.begin(), parts.end(), isFunction);
}

std::optional<TypeId> getMetatable(TypeId type)
{
    if (const MetatableTypeVar* mtType = get<MetatableTypeVar>(type))
        return mtType->metatable;
    else if (const ClassTypeVar* classType = get<ClassTypeVar>(type))
        return classType->metatable;
    else if (FFlag::LuauRefactorTypeVarQuestions)
    {
        if (isString(type))
        {
            auto ptv = get<PrimitiveTypeVar>(getSingletonTypes().stringType);
            LUAU_ASSERT(ptv && ptv->metatable);
            return ptv->metatable;
        }
        else
            return std::nullopt;
    }
    else
    {
        if (const PrimitiveTypeVar* primitiveType = get<PrimitiveTypeVar>(type); primitiveType && primitiveType->metatable)
        {
            LUAU_ASSERT(primitiveType->type == PrimitiveTypeVar::String);
            return primitiveType->metatable;
        }
        else
            return std::nullopt;
    }
}

const TableTypeVar* getTableType(TypeId type)
{
    if (const TableTypeVar* ttv = get<TableTypeVar>(type))
        return ttv;
    else if (const MetatableTypeVar* mtv = get<MetatableTypeVar>(type))
        return get<TableTypeVar>(mtv->table);
    else
        return nullptr;
}

TableTypeVar* getMutableTableType(TypeId type)
{
    return const_cast<TableTypeVar*>(getTableType(type));
}

const std::string* getName(TypeId type)
{
    type = follow(type);
    if (auto mtv = get<MetatableTypeVar>(type))
    {
        if (mtv->syntheticName)
            return &*mtv->syntheticName;
        type = mtv->table;
    }

    if (auto ttv = get<TableTypeVar>(type))
    {
        if (ttv->name)
            return &*ttv->name;
        if (ttv->syntheticName)
            return &*ttv->syntheticName;
    }

    return nullptr;
}

bool isSubset(const UnionTypeVar& super, const UnionTypeVar& sub)
{
    std::unordered_set<TypeId> superTypes;

    for (TypeId id : super.options)
        superTypes.insert(id);

    for (TypeId id : sub.options)
    {
        if (superTypes.find(id) == superTypes.end())
            return false;
    }

    return true;
}

// When typechecking an assignment `x = e`, we typecheck `x:T` and `e:U`,
// then instantiate U if `isGeneric(U)` is true, and `maybeGeneric(T)` is false.
bool isGeneric(TypeId ty)
{
    ty = follow(ty);
    if (auto ftv = get<FunctionTypeVar>(ty))
        return ftv->generics.size() > 0 || ftv->genericPacks.size() > 0;
    else
        // TODO: recurse on type synonyms CLI-39914
        // TODO: recurse on table types CLI-39914
        return false;
}

bool maybeGeneric(TypeId ty)
{
    ty = follow(ty);
    if (get<FreeTypeVar>(ty))
        return true;
    else if (auto ttv = get<TableTypeVar>(ty))
    {
        // TODO: recurse on table types CLI-39914
        (void)ttv;
        return true;
    }
    else
        return isGeneric(ty);
}

bool maybeSingleton(TypeId ty)
{
    ty = follow(ty);
    if (get<SingletonTypeVar>(ty))
        return true;
    if (const UnionTypeVar* utv = get<UnionTypeVar>(ty))
        for (TypeId option : utv)
            if (get<SingletonTypeVar>(follow(option)))
                return true;
    return false;
}

bool hasLength(TypeId ty, DenseHashSet<TypeId>& seen, int* recursionCount)
{
    LUAU_ASSERT(FFlag::LuauLengthOnCompositeType);

    RecursionLimiter _rl(recursionCount, FInt::LuauTypeInferRecursionLimit);

    ty = follow(ty);

    if (seen.contains(ty))
        return true;

    if (isPrim(ty, PrimitiveTypeVar::String) || get<AnyTypeVar>(ty) || get<TableTypeVar>(ty) || get<MetatableTypeVar>(ty))
        return true;

    if (auto uty = get<UnionTypeVar>(ty))
    {
        seen.insert(ty);

        for (TypeId part : uty->options)
        {
            if (!hasLength(part, seen, recursionCount))
                return false;
        }

        return true;
    }

    if (auto ity = get<IntersectionTypeVar>(ty))
    {
        seen.insert(ty);

        for (TypeId part : ity->parts)
        {
            if (hasLength(part, seen, recursionCount))
                return true;
        }

        return false;
    }

    return false;
}

FunctionTypeVar::FunctionTypeVar(TypePackId argTypes, TypePackId retType, std::optional<FunctionDefinition> defn, bool hasSelf)
    : argTypes(argTypes)
    , retType(retType)
    , definition(std::move(defn))
    , hasSelf(hasSelf)
{
}

FunctionTypeVar::FunctionTypeVar(TypeLevel level, TypePackId argTypes, TypePackId retType, std::optional<FunctionDefinition> defn, bool hasSelf)
    : level(level)
    , argTypes(argTypes)
    , retType(retType)
    , definition(std::move(defn))
    , hasSelf(hasSelf)
{
}

FunctionTypeVar::FunctionTypeVar(std::vector<TypeId> generics, std::vector<TypePackId> genericPacks, TypePackId argTypes, TypePackId retType,
    std::optional<FunctionDefinition> defn, bool hasSelf)
    : generics(generics)
    , genericPacks(genericPacks)
    , argTypes(argTypes)
    , retType(retType)
    , definition(std::move(defn))
    , hasSelf(hasSelf)
{
}

FunctionTypeVar::FunctionTypeVar(TypeLevel level, std::vector<TypeId> generics, std::vector<TypePackId> genericPacks, TypePackId argTypes,
    TypePackId retType, std::optional<FunctionDefinition> defn, bool hasSelf)
    : level(level)
    , generics(generics)
    , genericPacks(genericPacks)
    , argTypes(argTypes)
    , retType(retType)
    , definition(std::move(defn))
    , hasSelf(hasSelf)
{
}

TableTypeVar::TableTypeVar(TableState state, TypeLevel level)
    : state(state)
    , level(level)
{
}

TableTypeVar::TableTypeVar(const Props& props, const std::optional<TableIndexer>& indexer, TypeLevel level, TableState state)
    : props(props)
    , indexer(indexer)
    , state(state)
    , level(level)
{
}

// Test TypeVars for equivalence
// More complex than we'd like because TypeVars can self-reference.

bool areSeen(SeenSet& seen, const void* lhs, const void* rhs)
{
    if (lhs == rhs)
        return true;

    auto p = std::make_pair(const_cast<void*>(lhs), const_cast<void*>(rhs));
    if (seen.find(p) != seen.end())
        return true;

    seen.insert(p);
    return false;
}

bool areEqual(SeenSet& seen, const FunctionTypeVar& lhs, const FunctionTypeVar& rhs)
{
    if (areSeen(seen, &lhs, &rhs))
        return true;

    // TODO: check generics CLI-39915

    if (!areEqual(seen, *lhs.argTypes, *rhs.argTypes))
        return false;

    if (!areEqual(seen, *lhs.retType, *rhs.retType))
        return false;

    return true;
}

bool areEqual(SeenSet& seen, const TableTypeVar& lhs, const TableTypeVar& rhs)
{
    if (areSeen(seen, &lhs, &rhs))
        return true;

    if (lhs.state != rhs.state)
        return false;

    if (lhs.props.size() != rhs.props.size())
        return false;

    if (bool(lhs.indexer) != bool(rhs.indexer))
        return false;

    if (lhs.indexer && rhs.indexer)
    {
        if (!areEqual(seen, *lhs.indexer->indexType, *rhs.indexer->indexType))
            return false;

        if (!areEqual(seen, *lhs.indexer->indexResultType, *rhs.indexer->indexResultType))
            return false;
    }

    auto l = lhs.props.begin();
    auto r = rhs.props.begin();

    while (l != lhs.props.end())
    {
        if (l->first != r->first)
            return false;

        if (!areEqual(seen, *l->second.type, *r->second.type))
            return false;
        ++l;
        ++r;
    }

    return true;
}

static bool areEqual(SeenSet& seen, const MetatableTypeVar& lhs, const MetatableTypeVar& rhs)
{
    if (FFlag::LuauMetatableAreEqualRecursion && areSeen(seen, &lhs, &rhs))
        return true;

    return areEqual(seen, *lhs.table, *rhs.table) && areEqual(seen, *lhs.metatable, *rhs.metatable);
}

bool areEqual(SeenSet& seen, const TypeVar& lhs, const TypeVar& rhs)
{
    if (auto bound = get_if<BoundTypeVar>(&lhs.ty))
        return areEqual(seen, *bound->boundTo, rhs);

    if (auto bound = get_if<BoundTypeVar>(&rhs.ty))
        return areEqual(seen, lhs, *bound->boundTo);

    if (lhs.ty.index() != rhs.ty.index())
        return false;

    {
        const FreeTypeVar* lf = get_if<FreeTypeVar>(&lhs.ty);
        const FreeTypeVar* rf = get_if<FreeTypeVar>(&rhs.ty);
        if (lf && rf)
            return lf->index == rf->index;
    }

    {
        const GenericTypeVar* lg = get_if<GenericTypeVar>(&lhs.ty);
        const GenericTypeVar* rg = get_if<GenericTypeVar>(&rhs.ty);
        if (lg && rg)
            return lg->index == rg->index;
    }

    {
        const PrimitiveTypeVar* lp = get_if<PrimitiveTypeVar>(&lhs.ty);
        const PrimitiveTypeVar* rp = get_if<PrimitiveTypeVar>(&rhs.ty);
        if (lp && rp)
            return lp->type == rp->type;
    }

    {
        const GenericTypeVar* lg = get_if<GenericTypeVar>(&lhs.ty);
        const GenericTypeVar* rg = get_if<GenericTypeVar>(&rhs.ty);
        if (lg && rg)
            return lg->index == rg->index;
    }

    {
        const ErrorTypeVar* le = get_if<ErrorTypeVar>(&lhs.ty);
        const ErrorTypeVar* re = get_if<ErrorTypeVar>(&rhs.ty);
        if (le && re)
            return le->index == re->index;
    }

    {
        const FunctionTypeVar* lf = get_if<FunctionTypeVar>(&lhs.ty);
        const FunctionTypeVar* rf = get_if<FunctionTypeVar>(&rhs.ty);
        if (lf && rf)
            return areEqual(seen, *lf, *rf);
    }

    {
        const TableTypeVar* lt = get_if<TableTypeVar>(&lhs.ty);
        const TableTypeVar* rt = get_if<TableTypeVar>(&rhs.ty);
        if (lt && rt)
            return areEqual(seen, *lt, *rt);
    }

    {
        const MetatableTypeVar* lmt = get_if<MetatableTypeVar>(&lhs.ty);
        const MetatableTypeVar* rmt = get_if<MetatableTypeVar>(&rhs.ty);

        if (lmt && rmt)
            return areEqual(seen, *lmt, *rmt);
    }

    if (get_if<AnyTypeVar>(&lhs.ty) && get_if<AnyTypeVar>(&rhs.ty))
        return true;

    return false;
}

TypeVar* asMutable(TypeId ty)
{
    return const_cast<TypeVar*>(ty);
}

bool TypeVar::operator==(const TypeVar& rhs) const
{
    SeenSet seen;
    return areEqual(seen, *this, rhs);
}

bool TypeVar::operator!=(const TypeVar& rhs) const
{
    SeenSet seen;
    return !areEqual(seen, *this, rhs);
}

TypeVar& TypeVar::operator=(const TypeVariant& rhs)
{
    ty = rhs;
    return *this;
}

TypeVar& TypeVar::operator=(TypeVariant&& rhs)
{
    ty = std::move(rhs);
    return *this;
}

TypeId makeFunction(TypeArena& arena, std::optional<TypeId> selfType, std::initializer_list<TypeId> generics,
    std::initializer_list<TypePackId> genericPacks, std::initializer_list<TypeId> paramTypes, std::initializer_list<std::string> paramNames,
    std::initializer_list<TypeId> retTypes);

static TypeVar nilType_{PrimitiveTypeVar{PrimitiveTypeVar::NilType}, /*persistent*/ true};
static TypeVar numberType_{PrimitiveTypeVar{PrimitiveTypeVar::Number}, /*persistent*/ true};
static TypeVar stringType_{PrimitiveTypeVar{PrimitiveTypeVar::String}, /*persistent*/ true};
static TypeVar booleanType_{PrimitiveTypeVar{PrimitiveTypeVar::Boolean}, /*persistent*/ true};
static TypeVar threadType_{PrimitiveTypeVar{PrimitiveTypeVar::Thread}, /*persistent*/ true};
static TypeVar anyType_{AnyTypeVar{}};
static TypeVar errorType_{ErrorTypeVar{}};
static TypeVar optionalNumberType_{UnionTypeVar{{&numberType_, &nilType_}}};

static TypePackVar anyTypePack_{VariadicTypePack{&anyType_}, true};
static TypePackVar errorTypePack_{Unifiable::Error{}};

SingletonTypes::SingletonTypes()
    : nilType(&nilType_)
    , numberType(&numberType_)
    , stringType(&stringType_)
    , booleanType(&booleanType_)
    , threadType(&threadType_)
    , anyType(&anyType_)
    , optionalNumberType(&optionalNumberType_)
    , anyTypePack(&anyTypePack_)
    , arena(new TypeArena)
{
    TypeId stringMetatable = makeStringMetatable();
    stringType_.ty = PrimitiveTypeVar{PrimitiveTypeVar::String, stringMetatable};
    persist(stringMetatable);

    debugFreezeArena = FFlag::DebugLuauFreezeArena;
    freeze(*arena);
}

SingletonTypes::~SingletonTypes()
{
    // Destroy the arena with the same memory management flags it was created with
    bool prevFlag = FFlag::DebugLuauFreezeArena;
    FFlag::DebugLuauFreezeArena.value = debugFreezeArena;

    unfreeze(*arena);
    arena.reset(nullptr);

    FFlag::DebugLuauFreezeArena.value = prevFlag;
}

TypeId SingletonTypes::makeStringMetatable()
{
    const TypeId optionalNumber = arena->addType(UnionTypeVar{{nilType, numberType}});
    const TypeId optionalString = arena->addType(UnionTypeVar{{nilType, stringType}});
    const TypeId optionalBoolean = arena->addType(UnionTypeVar{{nilType, &booleanType_}});

    const TypePackId oneStringPack = arena->addTypePack({stringType});
    const TypePackId anyTypePack = arena->addTypePack(TypePackVar{VariadicTypePack{anyType}, true});

    FunctionTypeVar formatFTV{arena->addTypePack(TypePack{{stringType}, anyTypePack}), oneStringPack};
    formatFTV.magicFunction = &magicFunctionFormat;
    const TypeId formatFn = arena->addType(formatFTV);

    const TypePackId emptyPack = arena->addTypePack({});
    const TypePackId stringVariadicList = arena->addTypePack(TypePackVar{VariadicTypePack{stringType}});
    const TypePackId numberVariadicList = arena->addTypePack(TypePackVar{VariadicTypePack{numberType}});

    const TypeId stringToStringType = makeFunction(*arena, std::nullopt, {}, {}, {stringType}, {}, {stringType});

    const TypeId replArgType = arena->addType(
        UnionTypeVar{{stringType, arena->addType(TableTypeVar({}, TableIndexer(stringType, stringType), TypeLevel{}, TableState::Generic)),
            makeFunction(*arena, std::nullopt, {}, {}, {stringType}, {}, {stringType})}});
    const TypeId gsubFunc = makeFunction(*arena, stringType, {}, {}, {stringType, replArgType, optionalNumber}, {}, {stringType, numberType});
    const TypeId gmatchFunc =
        makeFunction(*arena, stringType, {}, {}, {stringType}, {}, {arena->addType(FunctionTypeVar{emptyPack, stringVariadicList})});

    TableTypeVar::Props stringLib = {
        {"byte", {arena->addType(FunctionTypeVar{arena->addTypePack({stringType, optionalNumber, optionalNumber}), numberVariadicList})}},
        {"char", {arena->addType(FunctionTypeVar{arena->addTypePack(TypePack{{numberType}, numberVariadicList}), arena->addTypePack({stringType})})}},
        {"find", {makeFunction(*arena, stringType, {}, {}, {stringType, optionalNumber, optionalBoolean}, {}, {optionalNumber, optionalNumber})}},
        {"format", {formatFn}}, // FIXME
        {"gmatch", {gmatchFunc}},
        {"gsub", {gsubFunc}},
        {"len", {makeFunction(*arena, stringType, {}, {}, {}, {}, {numberType})}},
        {"lower", {stringToStringType}},
        {"match", {makeFunction(*arena, stringType, {}, {}, {stringType, optionalNumber}, {}, {optionalString})}},
        {"rep", {makeFunction(*arena, stringType, {}, {}, {numberType}, {}, {stringType})}},
        {"reverse", {stringToStringType}},
        {"sub", {makeFunction(*arena, stringType, {}, {}, {numberType, optionalNumber}, {}, {stringType})}},
        {"upper", {stringToStringType}},
        {"split", {makeFunction(*arena, stringType, {}, {}, {optionalString}, {},
                      {arena->addType(TableTypeVar{{}, TableIndexer{numberType, stringType}, TypeLevel{}})})}},
        {"pack", {arena->addType(FunctionTypeVar{
                     arena->addTypePack(TypePack{{stringType}, anyTypePack}),
                     oneStringPack,
                 })}},
        {"packsize", {makeFunction(*arena, stringType, {}, {}, {}, {}, {numberType})}},
        {"unpack", {arena->addType(FunctionTypeVar{
                       arena->addTypePack(TypePack{{stringType, stringType, optionalNumber}}),
                       anyTypePack,
                   })}},
    };

    assignPropDocumentationSymbols(stringLib, "@luau/global/string");

    TypeId tableType = arena->addType(TableTypeVar{std::move(stringLib), std::nullopt, TypeLevel{}, TableState::Sealed});

    if (TableTypeVar* ttv = getMutable<TableTypeVar>(tableType))
        ttv->name = "string";

    return arena->addType(TableTypeVar{{{{"__index", {tableType}}}}, std::nullopt, TypeLevel{}, TableState::Sealed});
}

TypeId SingletonTypes::errorRecoveryType()
{
    return &errorType_;
}

TypePackId SingletonTypes::errorRecoveryTypePack()
{
    return &errorTypePack_;
}

TypeId SingletonTypes::errorRecoveryType(TypeId guess)
{
    if (FFlag::LuauErrorRecoveryType)
        return guess;
    else
        return &errorType_;
}

TypePackId SingletonTypes::errorRecoveryTypePack(TypePackId guess)
{
    if (FFlag::LuauErrorRecoveryType)
        return guess;
    else
        return &errorTypePack_;
}

SingletonTypes& getSingletonTypes()
{
    static SingletonTypes singletonTypes;
    return singletonTypes;
}

void persist(TypeId ty)
{
    std::deque<TypeId> queue{ty};

    while (!queue.empty())
    {
        TypeId t = queue.front();
        queue.pop_front();

        if (t->persistent)
            continue;

        asMutable(t)->persistent = true;

        if (auto btv = get<BoundTypeVar>(t))
            queue.push_back(btv->boundTo);
        else if (auto ftv = get<FunctionTypeVar>(t))
        {
            persist(ftv->argTypes);
            persist(ftv->retType);
        }
        else if (auto ttv = get<TableTypeVar>(t))
        {
            for (const auto& [_name, prop] : ttv->props)
                queue.push_back(prop.type);

            if (ttv->indexer)
            {
                queue.push_back(ttv->indexer->indexType);
                queue.push_back(ttv->indexer->indexResultType);
            }
        }
        else if (auto ctv = get<ClassTypeVar>(t))
        {
            for (const auto& [_name, prop] : ctv->props)
                queue.push_back(prop.type);
        }
        else if (auto utv = get<UnionTypeVar>(t))
        {
            for (TypeId opt : utv->options)
                queue.push_back(opt);
        }
        else if (auto itv = get<IntersectionTypeVar>(t))
        {
            for (TypeId opt : itv->parts)
                queue.push_back(opt);
        }
        else if (auto mtv = get<MetatableTypeVar>(t))
        {
            queue.push_back(mtv->table);
            queue.push_back(mtv->metatable);
        }
        else if (get<GenericTypeVar>(t) || get<AnyTypeVar>(t) || get<FreeTypeVar>(t) || get<SingletonTypeVar>(t) || get<PrimitiveTypeVar>(t))
        {
        }
        else
        {
            LUAU_ASSERT(!"TypeId is not supported in a persist call");
        }
    }
}

void persist(TypePackId tp)
{
    if (tp->persistent)
        return;

    asMutable(tp)->persistent = true;

    if (auto p = get<TypePack>(tp))
    {
        for (TypeId ty : p->head)
            persist(ty);
        if (p->tail)
            persist(*p->tail);
    }
    else if (auto vtp = get<VariadicTypePack>(tp))
    {
        persist(vtp->ty);
    }
    else if (get<GenericTypePack>(tp))
    {
    }
    else
    {
        LUAU_ASSERT(!"TypePackId is not supported in a persist call");
    }
}

const TypeLevel* getLevel(TypeId ty)
{
    ty = follow(ty);

    if (auto ftv = get<Unifiable::Free>(ty))
        return &ftv->level;
    else if (auto ttv = get<TableTypeVar>(ty))
        return &ttv->level;
    else if (auto ftv = get<FunctionTypeVar>(ty))
        return &ftv->level;
    else
        return nullptr;
}

TypeLevel* getMutableLevel(TypeId ty)
{
    return const_cast<TypeLevel*>(getLevel(ty));
}

const Property* lookupClassProp(const ClassTypeVar* cls, const Name& name)
{
    while (cls)
    {
        auto it = cls->props.find(name);
        if (it != cls->props.end())
            return &it->second;

        if (cls->parent)
            cls = get<ClassTypeVar>(*cls->parent);
        else
            return nullptr;

        LUAU_ASSERT(cls);
    }

    return nullptr;
}

bool isSubclass(const ClassTypeVar* cls, const ClassTypeVar* parent)
{
    while (cls)
    {
        if (cls == parent)
            return true;
        else if (!cls->parent)
            return false;

        cls = get<ClassTypeVar>(*cls->parent);
        LUAU_ASSERT(cls);
    }

    return false;
}

UnionTypeVarIterator::UnionTypeVarIterator(const UnionTypeVar* utv)
{
    LUAU_ASSERT(utv);

    if (!utv->options.empty())
        stack.push_front({utv, 0});

    seen.insert(utv);
}

UnionTypeVarIterator& UnionTypeVarIterator::operator++()
{
    advance();
    descend();
    return *this;
}

UnionTypeVarIterator UnionTypeVarIterator::operator++(int)
{
    UnionTypeVarIterator copy = *this;
    ++copy;
    return copy;
}

bool UnionTypeVarIterator::operator!=(const UnionTypeVarIterator& rhs)
{
    return !(*this == rhs);
}

bool UnionTypeVarIterator::operator==(const UnionTypeVarIterator& rhs)
{
    if (!stack.empty() && !rhs.stack.empty())
        return stack.front() == rhs.stack.front();

    return stack.empty() && rhs.stack.empty();
}

const TypeId& UnionTypeVarIterator::operator*()
{
    LUAU_ASSERT(!stack.empty());

    descend();

    auto [utv, currentIndex] = stack.front();
    LUAU_ASSERT(utv);
    LUAU_ASSERT(currentIndex < utv->options.size());

    const TypeId& ty = utv->options[currentIndex];
    LUAU_ASSERT(!get<UnionTypeVar>(follow(ty)));
    return ty;
}

void UnionTypeVarIterator::advance()
{
    while (!stack.empty())
    {
        auto& [utv, currentIndex] = stack.front();
        ++currentIndex;

        if (currentIndex >= utv->options.size())
            stack.pop_front();
        else
            break;
    }
}

void UnionTypeVarIterator::descend()
{
    while (!stack.empty())
    {
        auto [utv, currentIndex] = stack.front();
        if (auto innerUnion = get<UnionTypeVar>(follow(utv->options[currentIndex])))
        {
            // If we're about to descend into a cyclic UnionTypeVar, we should skip over this.
            // Ideally this should never happen, but alas it does from time to time. :(
            if (seen.find(innerUnion) != seen.end())
                advance();
            else
            {
                seen.insert(innerUnion);
                stack.push_front({innerUnion, 0});
            }

            continue;
        }

        break;
    }
}

UnionTypeVarIterator begin(const UnionTypeVar* utv)
{
    return UnionTypeVarIterator{utv};
}

UnionTypeVarIterator end(const UnionTypeVar* utv)
{
    return UnionTypeVarIterator{};
}

static std::vector<TypeId> parseFormatString(TypeChecker& typechecker, const char* data, size_t size)
{
    const char* options = "cdiouxXeEfgGqs";

    std::vector<TypeId> result;

    for (size_t i = 0; i < size; ++i)
    {
        if (data[i] == '%')
        {
            i++;

            if (i < size && data[i] == '%')
                continue;

            // we just ignore all characters (including flags/precision) up until first alphabetic character
            while (i < size && !(data[i] > 0 && isalpha(data[i])))
                i++;

            if (i == size)
                break;

            if (data[i] == 'q' || data[i] == 's')
                result.push_back(typechecker.stringType);
            else if (strchr(options, data[i]))
                result.push_back(typechecker.numberType);
            else
                result.push_back(typechecker.errorRecoveryType(typechecker.anyType));
        }
    }

    return result;
}

std::optional<ExprResult<TypePackId>> magicFunctionFormat(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult)
{
    auto [paramPack, _predicates] = exprResult;

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

    std::vector<TypeId> expected = parseFormatString(typechecker, fmt->value.data, fmt->value.size);
    const auto& [params, tail] = flatten(paramPack);

    size_t paramOffset = 1;
    size_t dataOffset = expr.self ? 0 : 1;

    // unify the prefix one argument at a time
    for (size_t i = 0; i < expected.size() && i + paramOffset < params.size(); ++i)
    {
        Location location = expr.args.data[std::min(i + dataOffset, expr.args.size - 1)]->location;

        typechecker.unify(params[i + paramOffset], expected[i], location);
    }

    // if we know the argument count or if we have too many arguments for sure, we can issue an error
    size_t actualParamSize = params.size() - paramOffset;

    if (expected.size() != actualParamSize && (!tail || expected.size() < actualParamSize))
        typechecker.reportError(TypeError{expr.location, CountMismatch{expected.size(), actualParamSize}});

    return ExprResult<TypePackId>{arena.addTypePack({typechecker.stringType})};
}

std::vector<TypeId> filterMap(TypeId type, TypeIdPredicate predicate)
{
    type = follow(type);

    if (auto utv = get<UnionTypeVar>(type))
    {
        std::set<TypeId> options;
        for (TypeId option : utv)
            if (auto out = predicate(follow(option)))
                options.insert(*out);

        return std::vector<TypeId>(options.begin(), options.end());
    }
    else if (auto out = predicate(type))
        return {*out};

    return {};
}

static Tags* getTags(TypeId ty)
{
    ty = follow(ty);

    if (auto ftv = getMutable<FunctionTypeVar>(ty))
        return &ftv->tags;
    else if (auto ttv = getMutable<TableTypeVar>(ty))
        return &ttv->tags;
    else if (auto ctv = getMutable<ClassTypeVar>(ty))
        return &ctv->tags;

    return nullptr;
}

void attachTag(TypeId ty, const std::string& tagName)
{
    if (auto tags = getTags(ty))
        tags->push_back(tagName);
    else
        LUAU_ASSERT(!"This TypeId does not support tags");
}

void attachTag(Property& prop, const std::string& tagName)
{
    prop.tags.push_back(tagName);
}

// We would ideally not expose this because it could cause a footgun.
// If the Base class has a tag and you ask if Derived has that tag, it would return false.
// Unfortunately, there's already use cases that's hard to disentangle. For now, we expose it.
bool hasTag(const Tags& tags, const std::string& tagName)
{
    return std::find(tags.begin(), tags.end(), tagName) != tags.end();
}

bool hasTag(TypeId ty, const std::string& tagName)
{
    ty = follow(ty);

    // We special case classes because getTags only returns a pointer to one vector of tags.
    // But classes has multiple vector of tags, represented throughout the hierarchy.
    if (auto ctv = get<ClassTypeVar>(ty))
    {
        while (ctv)
        {
            if (hasTag(ctv->tags, tagName))
                return true;
            else if (!ctv->parent)
                return false;

            ctv = get<ClassTypeVar>(*ctv->parent);
            LUAU_ASSERT(ctv);
        }
    }
    else if (auto tags = getTags(ty))
        return hasTag(*tags, tagName);

    return false;
}

bool hasTag(const Property& prop, const std::string& tagName)
{
    return hasTag(prop.tags, tagName);
}

} // namespace Luau
