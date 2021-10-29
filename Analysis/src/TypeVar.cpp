// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeVar.h"

#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/DenseHash.h"
#include "Luau/Error.h"
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

LUAU_FASTINTVARIABLE(LuauTypeMaximumStringifierLength, 500)
LUAU_FASTINTVARIABLE(LuauTableTypeMaximumStringifierLength, 0)
LUAU_FASTFLAG(LuauImprovedTypeGuardPredicate2)
LUAU_FASTFLAGVARIABLE(LuauToStringFollowsBoundTo, false)
LUAU_FASTFLAG(LuauRankNTypes)
LUAU_FASTFLAGVARIABLE(LuauStringMetatable, false)
LUAU_FASTFLAG(LuauTypeGuardPeelsAwaySubclasses)

namespace Luau
{

std::optional<ExprResult<TypePackId>> magicFunctionFormat(
    TypeChecker& typechecker, const ScopePtr& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult);

TypeId follow(TypeId t)
{
    auto advance = [](TypeId ty) -> std::optional<TypeId> {
        if (auto btv = get<Unifiable::Bound<TypeId>>(ty))
            return btv->boundTo;
        else if (auto ttv = get<TableTypeVar>(ty))
            return ttv->boundTo;
        else
            return std::nullopt;
    };

    auto force = [](TypeId ty) {
        if (auto ltv = FFlag::LuauAddMissingFollow ? get_if<LazyTypeVar>(&ty->ty) : get<LazyTypeVar>(ty))
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
    return isPrim(ty, PrimitiveTypeVar::Boolean);
}

bool isNumber(TypeId ty)
{
    return isPrim(ty, PrimitiveTypeVar::Number);
}

bool isString(TypeId ty)
{
    return isPrim(ty, PrimitiveTypeVar::String);
}

bool isThread(TypeId ty)
{
    return isPrim(ty, PrimitiveTypeVar::Thread);
}

bool isOptional(TypeId ty)
{
    if (isNil(ty))
        return true;

    if (!get<UnionTypeVar>(follow(ty)))
        return false;

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

bool isTableIntersection(TypeId ty)
{
    if (FFlag::LuauImprovedTypeGuardPredicate2)
    {
        if (!get<IntersectionTypeVar>(follow(ty)))
            return false;

        std::vector<TypeId> parts = flattenIntersection(ty);
        return std::all_of(parts.begin(), parts.end(), getTableType);
    }
    else
    {
        if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(ty))
        {
            for (TypeId part : itv->parts)
            {
                if (getTableType(follow(part)))
                    return true;
            }
        }

        return false;
    }
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
    else if (const PrimitiveTypeVar* primitiveType = get<PrimitiveTypeVar>(type);
             FFlag::LuauStringMetatable && primitiveType && primitiveType->metatable)
    {
        LUAU_ASSERT(primitiveType->type == PrimitiveTypeVar::String);
        return primitiveType->metatable;
    }
    else
        return std::nullopt;
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
    if (auto ftv = get<FreeTypeVar>(ty))
        return FFlag::LuauRankNTypes || ftv->DEPRECATED_canBeGeneric;
    else if (auto ttv = get<TableTypeVar>(ty))
    {
        // TODO: recurse on table types CLI-39914
        (void)ttv;
        return true;
    }
    else
        return isGeneric(ty);
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

SingletonTypes::SingletonTypes()
    : arena(new TypeArena)
    , nilType_{PrimitiveTypeVar{PrimitiveTypeVar::NilType}, /*persistent*/ true}
    , numberType_{PrimitiveTypeVar{PrimitiveTypeVar::Number}, /*persistent*/ true}
    , stringType_{PrimitiveTypeVar{PrimitiveTypeVar::String}, /*persistent*/ true}
    , booleanType_{PrimitiveTypeVar{PrimitiveTypeVar::Boolean}, /*persistent*/ true}
    , threadType_{PrimitiveTypeVar{PrimitiveTypeVar::Thread}, /*persistent*/ true}
    , anyType_{AnyTypeVar{}}
    , errorType_{ErrorTypeVar{}}
{
    TypeId stringMetatable = makeStringMetatable();
    stringType_.ty = PrimitiveTypeVar{PrimitiveTypeVar::String, makeStringMetatable()};
    persist(stringMetatable);
    freeze(*arena);
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
        {"split", {makeFunction(*arena, stringType, {}, {}, {stringType, optionalString}, {},
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

    return arena->addType(TableTypeVar{{{{"__index", {tableType}}}}, std::nullopt, TypeLevel{}, TableState::Sealed});
}

SingletonTypes singletonTypes;

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
}

namespace
{

struct StateDot
{
    StateDot(ToDotOptions opts)
        : opts(opts)
    {
    }

    ToDotOptions opts;

    std::unordered_set<TypeId> seenTy;
    std::unordered_set<TypePackId> seenTp;
    std::unordered_map<TypeId, int> tyToIndex;
    std::unordered_map<TypePackId, int> tpToIndex;
    int nextIndex = 1;
    std::string result;

    bool canDuplicatePrimitive(TypeId ty);

    void visitChildren(TypeId ty, int index);
    void visitChildren(TypePackId ty, int index);

    void visitChild(TypeId ty, int parentIndex, const char* linkName = nullptr);
    void visitChild(TypePackId tp, int parentIndex, const char* linkName = nullptr);

    void startNode(int index);
    void finishNode();

    void startNodeLabel();
    void finishNodeLabel(TypeId ty);
    void finishNodeLabel(TypePackId tp);
};

bool StateDot::canDuplicatePrimitive(TypeId ty)
{
    if (get<BoundTypeVar>(ty))
        return false;

    return get<PrimitiveTypeVar>(ty) || get<AnyTypeVar>(ty);
}

void StateDot::visitChild(TypeId ty, int parentIndex, const char* linkName)
{
    if (!tyToIndex.count(ty) || (opts.duplicatePrimitives && canDuplicatePrimitive(ty)))
        tyToIndex[ty] = nextIndex++;

    int index = tyToIndex[ty];

    if (parentIndex != 0)
    {
        if (linkName)
            formatAppend(result, "n%d -> n%d [label=\"%s\"];\n", parentIndex, index, linkName);
        else
            formatAppend(result, "n%d -> n%d;\n", parentIndex, index);
    }

    if (opts.duplicatePrimitives && canDuplicatePrimitive(ty))
    {
        if (const PrimitiveTypeVar* ptv = get<PrimitiveTypeVar>(ty))
            formatAppend(result, "n%d [label=\"%s\"];\n", index, toStringDetailed(ty, {}).name.c_str());
        else if (const AnyTypeVar* atv = get<AnyTypeVar>(ty))
            formatAppend(result, "n%d [label=\"any\"];\n", index);
    }
    else
    {
        visitChildren(ty, index);
    }
}

void StateDot::visitChild(TypePackId tp, int parentIndex, const char* linkName)
{
    if (!tpToIndex.count(tp))
        tpToIndex[tp] = nextIndex++;

    if (linkName)
        formatAppend(result, "n%d -> n%d [label=\"%s\"];\n", parentIndex, tpToIndex[tp], linkName);
    else
        formatAppend(result, "n%d -> n%d;\n", parentIndex, tpToIndex[tp]);

    visitChildren(tp, tpToIndex[tp]);
}

void StateDot::startNode(int index)
{
    formatAppend(result, "n%d [", index);
}

void StateDot::finishNode()
{
    formatAppend(result, "];\n");
}

void StateDot::startNodeLabel()
{
    formatAppend(result, "label=\"");
}

void StateDot::finishNodeLabel(TypeId ty)
{
    if (opts.showPointers)
        formatAppend(result, "\n0x%p", ty);
    // additional common attributes can be added here as well
    result += "\"";
}

void StateDot::finishNodeLabel(TypePackId tp)
{
    if (opts.showPointers)
        formatAppend(result, "\n0x%p", tp);
    // additional common attributes can be added here as well
    result += "\"";
}

void StateDot::visitChildren(TypeId ty, int index)
{
    if (seenTy.count(ty))
        return;
    seenTy.insert(ty);

    startNode(index);
    startNodeLabel();

    if (const BoundTypeVar* btv = get<BoundTypeVar>(ty))
    {
        formatAppend(result, "BoundTypeVar %d", index);
        finishNodeLabel(ty);
        finishNode();

        visitChild(btv->boundTo, index);
    }
    else if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty))
    {
        formatAppend(result, "FunctionTypeVar %d", index);
        finishNodeLabel(ty);
        finishNode();

        visitChild(ftv->argTypes, index, "arg");
        visitChild(ftv->retType, index, "ret");
    }
    else if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
    {
        if (ttv->name)
            formatAppend(result, "TableTypeVar %s", ttv->name->c_str());
        else if (ttv->syntheticName)
            formatAppend(result, "TableTypeVar %s", ttv->syntheticName->c_str());
        else
            formatAppend(result, "TableTypeVar %d", index);
        finishNodeLabel(ty);
        finishNode();

        if (ttv->boundTo)
            return visitChild(*ttv->boundTo, index, "boundTo");

        for (const auto& [name, prop] : ttv->props)
            visitChild(prop.type, index, name.c_str());
        if (ttv->indexer)
        {
            visitChild(ttv->indexer->indexType, index, "[index]");
            visitChild(ttv->indexer->indexResultType, index, "[value]");
        }
        for (TypeId itp : ttv->instantiatedTypeParams)
            visitChild(itp, index, "typeParam");
    }
    else if (const MetatableTypeVar* mtv = get<MetatableTypeVar>(ty))
    {
        formatAppend(result, "MetatableTypeVar %d", index);
        finishNodeLabel(ty);
        finishNode();

        visitChild(mtv->table, index, "table");
        visitChild(mtv->metatable, index, "metatable");
    }
    else if (const UnionTypeVar* utv = get<UnionTypeVar>(ty))
    {
        formatAppend(result, "UnionTypeVar %d", index);
        finishNodeLabel(ty);
        finishNode();

        for (TypeId opt : utv->options)
            visitChild(opt, index);
    }
    else if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(ty))
    {
        formatAppend(result, "IntersectionTypeVar %d", index);
        finishNodeLabel(ty);
        finishNode();

        for (TypeId part : itv->parts)
            visitChild(part, index);
    }
    else if (const GenericTypeVar* gtv = get<GenericTypeVar>(ty))
    {
        if (gtv->explicitName)
            formatAppend(result, "GenericTypeVar %s", gtv->name.c_str());
        else
            formatAppend(result, "GenericTypeVar %d", index);
        finishNodeLabel(ty);
        finishNode();
    }
    else if (const FreeTypeVar* ftv = get<FreeTypeVar>(ty))
    {
        formatAppend(result, "FreeTypeVar %d", ftv->index);
        finishNodeLabel(ty);
        finishNode();
    }
    else if (const AnyTypeVar* atv = get<AnyTypeVar>(ty))
    {
        formatAppend(result, "AnyTypeVar %d", index);
        finishNodeLabel(ty);
        finishNode();
    }
    else if (const PrimitiveTypeVar* ptv = get<PrimitiveTypeVar>(ty))
    {
        formatAppend(result, "PrimitiveTypeVar %s", toStringDetailed(ty, {}).name.c_str());
        finishNodeLabel(ty);
        finishNode();
    }
    else if (const ErrorTypeVar* etv = get<ErrorTypeVar>(ty))
    {
        formatAppend(result, "ErrorTypeVar %d", index);
        finishNodeLabel(ty);
        finishNode();
    }
    else if (const ClassTypeVar* ctv = get<ClassTypeVar>(ty))
    {
        formatAppend(result, "ClassTypeVar %s", ctv->name.c_str());
        finishNodeLabel(ty);
        finishNode();

        for (const auto& [name, prop] : ctv->props)
            visitChild(prop.type, index, name.c_str());

        if (ctv->parent)
            visitChild(*ctv->parent, index, "[parent]");

        if (ctv->metatable)
            visitChild(*ctv->metatable, index, "[metatable]");
    }
    else
    {
        LUAU_ASSERT(!"unknown type kind");
        finishNodeLabel(ty);
        finishNode();
    }
}

void StateDot::visitChildren(TypePackId tp, int index)
{
    if (seenTp.count(tp))
        return;
    seenTp.insert(tp);

    startNode(index);
    startNodeLabel();

    if (const BoundTypePack* btp = get<BoundTypePack>(tp))
    {
        formatAppend(result, "BoundTypePack %d", index);
        finishNodeLabel(tp);
        finishNode();

        visitChild(btp->boundTo, index);
    }
    else if (const TypePack* tpp = get<TypePack>(tp))
    {
        formatAppend(result, "TypePack %d", index);
        finishNodeLabel(tp);
        finishNode();

        for (TypeId tv : tpp->head)
            visitChild(tv, index);
        if (tpp->tail)
            visitChild(*tpp->tail, index, "tail");
    }
    else if (const VariadicTypePack* vtp = get<VariadicTypePack>(tp))
    {
        formatAppend(result, "VariadicTypePack %d", index);
        finishNodeLabel(tp);
        finishNode();

        visitChild(vtp->ty, index);
    }
    else if (const FreeTypePack* ftp = get<FreeTypePack>(tp))
    {
        formatAppend(result, "FreeTypePack %d", ftp->index);
        finishNodeLabel(tp);
        finishNode();
    }
    else if (const GenericTypePack* gtp = get<GenericTypePack>(tp))
    {
        if (gtp->explicitName)
            formatAppend(result, "GenericTypePack %s", gtp->name.c_str());
        else
            formatAppend(result, "GenericTypePack %d", gtp->index);
        finishNodeLabel(tp);
        finishNode();
    }
    else if (const Unifiable::Error* etp = get<Unifiable::Error>(tp))
    {
        formatAppend(result, "ErrorTypePack %d", index);
        finishNodeLabel(tp);
        finishNode();
    }
    else
    {
        LUAU_ASSERT(!"unknown type pack kind");
        finishNodeLabel(tp);
        finishNode();
    }
}

} // namespace

std::string toDot(TypeId ty, const ToDotOptions& opts)
{
    StateDot state{opts};

    state.result = "digraph graphname {\n";
    state.visitChild(ty, 0);
    state.result += "}";

    return state.result;
}

std::string toDot(TypePackId tp, const ToDotOptions& opts)
{
    StateDot state{opts};

    state.result = "digraph graphname {\n";
    state.visitChild(tp, 0);
    state.result += "}";

    return state.result;
}

std::string toDot(TypeId ty)
{
    return toDot(ty, {});
}

std::string toDot(TypePackId tp)
{
    return toDot(tp, {});
}

void dumpDot(TypeId ty)
{
    printf("%s\n", toDot(ty).c_str());
}

void dumpDot(TypePackId tp)
{
    printf("%s\n", toDot(tp).c_str());
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

struct QVarFinder
{
    mutable DenseHashSet<const void*> seen;

    QVarFinder()
        : seen(nullptr)
    {
    }

    bool hasSeen(const void* tv) const
    {
        if (seen.contains(tv))
            return true;

        seen.insert(tv);
        return false;
    }

    bool hasGeneric(TypeId tid) const
    {
        if (hasSeen(&tid->ty))
            return false;

        return Luau::visit(*this, tid->ty);
    }

    bool hasGeneric(TypePackId tp) const
    {
        if (hasSeen(&tp->ty))
            return false;

        return Luau::visit(*this, tp->ty);
    }

    bool operator()(const Unifiable::Free&) const
    {
        return false;
    }

    bool operator()(const Unifiable::Bound<TypeId>& bound) const
    {
        return hasGeneric(bound.boundTo);
    }

    bool operator()(const Unifiable::Generic&) const
    {
        return true;
    }
    bool operator()(const Unifiable::Error&) const
    {
        return false;
    }
    bool operator()(const PrimitiveTypeVar&) const
    {
        return false;
    }

    bool operator()(const FunctionTypeVar& ftv) const
    {
        if (hasGeneric(ftv.argTypes))
            return true;
        return hasGeneric(ftv.retType);
    }

    bool operator()(const TableTypeVar& ttv) const
    {
        if (ttv.state == TableState::Generic)
            return true;

        if (ttv.indexer)
        {
            if (hasGeneric(ttv.indexer->indexType))
                return true;
            if (hasGeneric(ttv.indexer->indexResultType))
                return true;
        }

        for (const auto& [_name, prop] : ttv.props)
        {
            if (hasGeneric(prop.type))
                return true;
        }

        return false;
    }

    bool operator()(const MetatableTypeVar& mtv) const
    {
        return hasGeneric(mtv.table) || hasGeneric(mtv.metatable);
    }

    bool operator()(const ClassTypeVar& ctv) const
    {
        for (const auto& [name, prop] : ctv.props)
        {
            if (hasGeneric(prop.type))
                return true;
        }

        if (ctv.parent)
            return hasGeneric(*ctv.parent);

        return false;
    }

    bool operator()(const AnyTypeVar&) const
    {
        return false;
    }

    bool operator()(const UnionTypeVar& utv) const
    {
        for (TypeId tid : utv.options)
            if (hasGeneric(tid))
                return true;

        return false;
    }

    bool operator()(const IntersectionTypeVar& utv) const
    {
        for (TypeId tid : utv.parts)
            if (hasGeneric(tid))
                return true;

        return false;
    }

    bool operator()(const LazyTypeVar&) const
    {
        return false;
    }

    bool operator()(const Unifiable::Bound<TypePackId>& bound) const
    {
        return hasGeneric(bound.boundTo);
    }

    bool operator()(const TypePack& pack) const
    {
        for (TypeId ty : pack.head)
            if (hasGeneric(ty))
                return true;

        if (pack.tail)
            return hasGeneric(*pack.tail);

        return false;
    }

    bool operator()(const VariadicTypePack& pack) const
    {
        return hasGeneric(pack.ty);
    }
};

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

bool hasGeneric(TypeId ty)
{
    return Luau::visit(QVarFinder{}, ty->ty);
}

bool hasGeneric(TypePackId tp)
{
    return Luau::visit(QVarFinder{}, tp->ty);
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

static std::vector<TypeId> DEPRECATED_filterMap(TypeId type, TypeIdPredicate predicate)
{
    std::vector<TypeId> result;

    if (auto utv = get<UnionTypeVar>(follow(type)))
    {
        for (TypeId option : utv)
        {
            if (auto out = predicate(follow(option)))
                result.push_back(*out);
        }
    }
    else if (auto out = predicate(follow(type)))
        return {*out};

    return result;
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
                result.push_back(typechecker.errorType);
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

    const size_t dataOffset = 1;

    // unify the prefix one argument at a time
    for (size_t i = 0; i < expected.size() && i + dataOffset < params.size(); ++i)
    {
        Location location = expr.args.data[std::min(i, expr.args.size - 1)]->location;

        typechecker.unify(expected[i], params[i + dataOffset], location);
    }

    // if we know the argument count or if we have too many arguments for sure, we can issue an error
    const size_t actualParamSize = params.size() - dataOffset;

    if (expected.size() != actualParamSize && (!tail || expected.size() < actualParamSize))
        typechecker.reportError(TypeError{expr.location, CountMismatch{expected.size(), actualParamSize}});

    return ExprResult<TypePackId>{arena.addTypePack({typechecker.stringType})};
}

std::vector<TypeId> filterMap(TypeId type, TypeIdPredicate predicate)
{
    if (!FFlag::LuauTypeGuardPeelsAwaySubclasses)
        return DEPRECATED_filterMap(type, predicate);

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

} // namespace Luau
