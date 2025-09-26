// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Type.h"

#include "Luau/BuiltinDefinitions.h"
#include "Luau/BuiltinTypeFunctions.h"
#include "Luau/Common.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/DenseHash.h"
#include "Luau/Error.h"
#include "Luau/RecursionCounter.h"
#include "Luau/StringUtils.h"
#include "Luau/ToString.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/VecDeque.h"
#include "Luau/VisitType.h"

#include <algorithm>
#include <memory>
#include <optional>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>

LUAU_FASTFLAG(DebugLuauFreezeArena)

LUAU_FASTFLAG(LuauSolverV2)

LUAU_FASTINTVARIABLE(LuauTypeMaximumStringifierLength, 500)
LUAU_FASTINTVARIABLE(LuauTableTypeMaximumStringifierLength, 0)
LUAU_FASTINT(LuauTypeInferRecursionLimit)
LUAU_FASTFLAG(LuauInstantiateInSubtyping)
LUAU_FASTFLAG(LuauUseWorkspacePropToChooseSolver)

namespace Luau
{

// LUAU_NOINLINE prevents unwrapLazy from being inlined into advance below; advance is important to keep inlineable
static LUAU_NOINLINE TypeId unwrapLazy(LazyType* ltv)
{
    TypeId unwrapped = ltv->unwrapped.load();

    if (unwrapped)
        return unwrapped;

    ltv->unwrap(*ltv);
    unwrapped = ltv->unwrapped.load();

    if (!unwrapped)
        throw InternalCompilerError("Lazy Type didn't fill in unwrapped type field");

    if (get<LazyType>(unwrapped))
        throw InternalCompilerError("Lazy Type cannot resolve to another Lazy Type");

    return unwrapped;
}

TypeId follow(TypeId t)
{
    return follow(t, FollowOption::Normal);
}

TypeId follow(TypeId t, FollowOption followOption)
{
    return follow(
        t,
        followOption,
        nullptr,
        [](const void*, TypeId t) -> TypeId
        {
            return t;
        }
    );
}

TypeId follow(TypeId t, const void* context, TypeId (*mapper)(const void*, TypeId))
{
    return follow(t, FollowOption::Normal, context, mapper);
}

TypeId follow(TypeId t, FollowOption followOption, const void* context, TypeId (*mapper)(const void*, TypeId))
{
    auto advance = [followOption, context, mapper](TypeId ty) -> std::optional<TypeId>
    {
        TypeId mapped = mapper(context, ty);

        if (auto btv = get<Unifiable::Bound<TypeId>>(mapped))
            return btv->boundTo;

        if (auto ttv = get<TableType>(mapped))
            return ttv->boundTo;

        if (auto ltv = getMutable<LazyType>(mapped); ltv && followOption != FollowOption::DisableLazyTypeThunks)
            return unwrapLazy(ltv);

        return std::nullopt;
    };

    TypeId cycleTester = t; // Null once we've determined that there is no cycle
    if (auto a = advance(cycleTester))
        cycleTester = *a;
    else
        return t;

    if (!advance(cycleTester)) // Short circuit traversal for the rather common case when advance(advance(t)) == null
        return cycleTester;

    while (true)
    {
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
                throw InternalCompilerError("Luau::follow detected a Type cycle!!");
        }
    }
}

std::vector<TypeId> flattenIntersection(TypeId ty)
{
    if (!get<IntersectionType>(follow(ty)))
        return {ty};

    std::unordered_set<TypeId> seen;
    VecDeque<TypeId> queue{ty};

    std::vector<TypeId> result;

    while (!queue.empty())
    {
        TypeId current = follow(queue.front());
        queue.pop_front();

        if (seen.find(current) != seen.end())
            continue;

        seen.insert(current);

        if (auto itv = get<IntersectionType>(current))
        {
            for (TypeId ty : itv->parts)
                queue.push_back(ty);
        }
        else
            result.push_back(current);
    }

    return result;
}

bool isPrim(TypeId ty, PrimitiveType::Type primType)
{
    auto p = get<PrimitiveType>(follow(ty));
    return p && p->type == primType;
}

bool isNil(TypeId ty)
{
    return isPrim(ty, PrimitiveType::NilType);
}

bool isBoolean(TypeId ty)
{
    if (isPrim(ty, PrimitiveType::Boolean) || get<BooleanSingleton>(get<SingletonType>(follow(ty))))
        return true;

    if (auto utv = get<UnionType>(follow(ty)))
        return std::all_of(begin(utv), end(utv), isBoolean);

    return false;
}

bool isNumber(TypeId ty)
{
    return isPrim(ty, PrimitiveType::Number);
}

// Returns true when ty is a subtype of string
bool isString(TypeId ty)
{
    ty = follow(ty);

    if (isPrim(ty, PrimitiveType::String) || get<StringSingleton>(get<SingletonType>(ty)))
        return true;

    if (auto utv = get<UnionType>(ty))
        return std::all_of(begin(utv), end(utv), isString);

    return false;
}

// Returns true when ty is a supertype of string
bool maybeString(TypeId ty)
{
    ty = follow(ty);

    if (isPrim(ty, PrimitiveType::String) || get<AnyType>(ty))
        return true;

    if (auto utv = get<UnionType>(ty))
        return std::any_of(begin(utv), end(utv), maybeString);

    return false;
}

bool isThread(TypeId ty)
{
    return isPrim(ty, PrimitiveType::Thread);
}

bool isBuffer(TypeId ty)
{
    return isPrim(ty, PrimitiveType::Buffer);
}

bool isOptional(TypeId ty)
{
    if (isNil(ty))
        return true;

    ty = follow(ty);

    if (get<AnyType>(ty) || get<UnknownType>(ty))
        return true;

    auto utv = get<UnionType>(ty);
    if (!utv)
        return false;

    return std::any_of(begin(utv), end(utv), isOptional);
}

bool isTableIntersection(TypeId ty)
{
    if (!get<IntersectionType>(follow(ty)))
        return false;

    std::vector<TypeId> parts = flattenIntersection(ty);
    return std::all_of(parts.begin(), parts.end(), getTableType);
}

bool isTableUnion(TypeId ty)
{
    const UnionType* ut = get<UnionType>(follow(ty));
    if (!ut)
        return false;

    return std::all_of(begin(ut), end(ut), getTableType);
}

bool isOverloadedFunction(TypeId ty)
{
    if (!get<IntersectionType>(follow(ty)))
        return false;

    auto isFunction = [](TypeId part) -> bool
    {
        return get<FunctionType>(part);
    };

    std::vector<TypeId> parts = flattenIntersection(ty);
    return std::all_of(parts.begin(), parts.end(), isFunction);
}

std::optional<TypeId> getMetatable(TypeId type, NotNull<BuiltinTypes> builtinTypes)
{
    type = follow(type);

    if (const MetatableType* mtType = get<MetatableType>(type))
        return mtType->metatable;
    else if (const ExternType* externType = get<ExternType>(type))
        return externType->metatable;
    else if (isString(type))
    {
        auto ptv = get<PrimitiveType>(builtinTypes->stringType);
        LUAU_ASSERT(ptv && ptv->metatable);
        return ptv->metatable;
    }

    return std::nullopt;
}

const TableType* getTableType(TypeId type)
{
    type = follow(type);

    if (const TableType* ttv = get<TableType>(type))
        return ttv;
    else if (const MetatableType* mtv = get<MetatableType>(type))
        return get<TableType>(follow(mtv->table));
    else
        return nullptr;
}

TableType* getMutableTableType(TypeId type)
{
    return const_cast<TableType*>(getTableType(type));
}

const std::string* getName(TypeId type)
{
    type = follow(type);
    if (auto mtv = get<MetatableType>(type))
    {
        if (mtv->syntheticName)
            return &*mtv->syntheticName;
        type = follow(mtv->table);
    }

    if (auto ttv = get<TableType>(type))
    {
        if (ttv->name)
            return &*ttv->name;
        if (ttv->syntheticName)
            return &*ttv->syntheticName;
    }

    return nullptr;
}

std::optional<ModuleName> getDefinitionModuleName(TypeId type)
{
    type = follow(type);

    if (auto ttv = get<TableType>(type))
    {
        if (!ttv->definitionModuleName.empty())
            return ttv->definitionModuleName;
    }
    else if (auto ftv = get<FunctionType>(type))
    {
        if (ftv->definition)
            return ftv->definition->definitionModuleName;
    }
    else if (auto etv = get<ExternType>(type))
    {
        if (!etv->definitionModuleName.empty())
            return etv->definitionModuleName;
    }

    return std::nullopt;
}

bool isSubset(const UnionType& super, const UnionType& sub)
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
bool hasPrimitiveTypeInIntersection(TypeId ty, PrimitiveType::Type primTy)
{
    TypeId tf = follow(ty);
    if (isPrim(tf, primTy))
        return true;

    for (auto t : flattenIntersection(tf))
        return isPrim(follow(t), primTy);
    return false;
}
// When typechecking an assignment `x = e`, we typecheck `x:T` and `e:U`,
// then instantiate U if `isGeneric(U)` is true, and `maybeGeneric(T)` is false.
bool isGeneric(TypeId ty)
{
    LUAU_ASSERT(!FFlag::LuauInstantiateInSubtyping);

    ty = follow(ty);
    if (auto ftv = get<FunctionType>(ty))
        return ftv->generics.size() > 0 || ftv->genericPacks.size() > 0;
    else
        // TODO: recurse on type synonyms CLI-39914
        // TODO: recurse on table types CLI-39914
        return false;
}

bool maybeGeneric(TypeId ty)
{
    LUAU_ASSERT(!FFlag::LuauInstantiateInSubtyping);

    ty = follow(ty);

    if (get<FreeType>(ty))
        return true;

    if (auto ttv = get<TableType>(ty))
    {
        // TODO: recurse on table types CLI-39914
        (void)ttv;
        return true;
    }

    if (auto itv = get<IntersectionType>(ty))
    {
        return std::any_of(begin(itv), end(itv), maybeGeneric);
    }

    return isGeneric(ty);
}

bool maybeSingleton(TypeId ty)
{
    ty = follow(ty);
    if (get<SingletonType>(ty))
        return true;
    if (const UnionType* utv = get<UnionType>(ty))
        for (TypeId option : utv)
            if (get<SingletonType>(follow(option)))
                return true;
    if (const IntersectionType* itv = get<IntersectionType>(ty))
        for (TypeId part : itv)
            if (maybeSingleton(part)) // will i regret this?
                return true;
    if (const TypeFunctionInstanceType* tfit = get<TypeFunctionInstanceType>(ty))
        if (tfit->function->name == "keyof" || tfit->function->name == "rawkeyof")
            return true;
    return false;
}

bool hasLength(TypeId ty, DenseHashSet<TypeId>& seen, int* recursionCount)
{
    RecursionLimiter _rl("Type::hasLength", recursionCount, FInt::LuauTypeInferRecursionLimit);

    ty = follow(ty);

    if (seen.contains(ty))
        return true;

    if (isString(ty) || isPrim(ty, PrimitiveType::Table) || get<AnyType>(ty) || get<TableType>(ty) || get<MetatableType>(ty))
        return true;

    if (auto uty = get<UnionType>(ty))
    {
        seen.insert(ty);

        for (TypeId part : uty->options)
        {
            if (!hasLength(part, seen, recursionCount))
                return false;
        }

        return true;
    }

    if (auto ity = get<IntersectionType>(ty))
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

// New constructors
FreeType::FreeType(TypeLevel level, TypeId lowerBound, TypeId upperBound)
    : index(Unifiable::freshIndex())
    , level(level)
    , lowerBound(lowerBound)
    , upperBound(upperBound)
{
}

FreeType::FreeType(Scope* scope, TypeId lowerBound, TypeId upperBound, Polarity polarity)
    : index(Unifiable::freshIndex())
    , scope(scope)
    , lowerBound(lowerBound)
    , upperBound(upperBound)
    , polarity(polarity)
{
}

FreeType::FreeType(Scope* scope, TypeLevel level, TypeId lowerBound, TypeId upperBound)
    : index(Unifiable::freshIndex())
    , level(level)
    , scope(scope)
    , lowerBound(lowerBound)
    , upperBound(upperBound)
{
}

GenericType::GenericType()
    : index(Unifiable::freshIndex())
    , name("g" + std::to_string(index))
{
}

GenericType::GenericType(TypeLevel level)
    : index(Unifiable::freshIndex())
    , level(level)
    , name("g" + std::to_string(index))
{
}

GenericType::GenericType(const Name& name, Polarity polarity)
    : index(Unifiable::freshIndex())
    , name(name)
    , explicitName(true)
    , polarity(polarity)
{
}

GenericType::GenericType(Scope* scope, Polarity polarity)
    : index(Unifiable::freshIndex())
    , scope(scope)
    , polarity(polarity)
{
}

GenericType::GenericType(TypeLevel level, const Name& name)
    : index(Unifiable::freshIndex())
    , level(level)
    , name(name)
    , explicitName(true)
{
}

GenericType::GenericType(Scope* scope, const Name& name)
    : index(Unifiable::freshIndex())
    , scope(scope)
    , name(name)
    , explicitName(true)
{
}

BlockedType::BlockedType()
    : index(Unifiable::freshIndex())
{
}

const Constraint* BlockedType::getOwner() const
{
    return owner;
}

void BlockedType::setOwner(const Constraint* newOwner)
{
    LUAU_ASSERT(owner == nullptr);

    if (owner != nullptr)
        return;

    owner = newOwner;
}

void BlockedType::replaceOwner(const Constraint* newOwner)
{
    owner = newOwner;
}

PendingExpansionType::PendingExpansionType(
    std::optional<AstName> prefix,
    AstName name,
    std::vector<TypeId> typeArguments,
    std::vector<TypePackId> packArguments
)
    : prefix(prefix)
    , name(name)
    , typeArguments(std::move(typeArguments))
    , packArguments(std::move(packArguments))
    , index(++nextIndex)
{
}

size_t PendingExpansionType::nextIndex = 0;

FunctionType::FunctionType(TypePackId argTypes, TypePackId retTypes, std::optional<FunctionDefinition> defn, bool hasSelf)
    : definition(std::move(defn))
    , argTypes(argTypes)
    , retTypes(retTypes)
    , hasSelf(hasSelf)
{
}

FunctionType::FunctionType(TypeLevel level, TypePackId argTypes, TypePackId retTypes, std::optional<FunctionDefinition> defn, bool hasSelf)
    : definition(std::move(defn))
    , level(level)
    , argTypes(argTypes)
    , retTypes(retTypes)
    , hasSelf(hasSelf)
{
}

FunctionType::FunctionType(
    std::vector<TypeId> generics,
    std::vector<TypePackId> genericPacks,
    TypePackId argTypes,
    TypePackId retTypes,
    std::optional<FunctionDefinition> defn,
    bool hasSelf
)
    : definition(std::move(defn))
    , generics(std::move(generics))
    , genericPacks(std::move(genericPacks))
    , argTypes(argTypes)
    , retTypes(retTypes)
    , hasSelf(hasSelf)
{
}

FunctionType::FunctionType(
    TypeLevel level,
    std::vector<TypeId> generics,
    std::vector<TypePackId> genericPacks,
    TypePackId argTypes,
    TypePackId retTypes,
    std::optional<FunctionDefinition> defn,
    bool hasSelf
)
    : definition(std::move(defn))
    , generics(std::move(generics))
    , genericPacks(std::move(genericPacks))
    , level(level)
    , argTypes(argTypes)
    , retTypes(retTypes)
    , hasSelf(hasSelf)
{
}

Property::Property() {}

Property::Property(
    TypeId readTy,
    bool deprecated,
    const std::string& deprecatedSuggestion,
    std::optional<Location> location,
    const Tags& tags,
    const std::optional<std::string>& documentationSymbol,
    std::optional<Location> typeLocation
)
    : deprecated(deprecated)
    , deprecatedSuggestion(deprecatedSuggestion)
    , location(location)
    , typeLocation(typeLocation)
    , tags(tags)
    , documentationSymbol(documentationSymbol)
    , readTy(readTy)
    , writeTy(readTy)
{
}

Property Property::readonly(TypeId ty)
{
    Property p;
    p.readTy = ty;
    return p;
}

Property Property::writeonly(TypeId ty)
{
    Property p;
    p.writeTy = ty;
    return p;
}

Property Property::rw(TypeId ty)
{
    return Property::rw(ty, ty);
}

Property Property::rw(TypeId read, TypeId write)
{
    Property p;
    p.readTy = read;
    p.writeTy = write;
    return p;
}

Property Property::create(std::optional<TypeId> read, std::optional<TypeId> write)
{
    if (read && !write)
        return Property::readonly(*read);
    else if (!read && write)
        return Property::writeonly(*write);
    else
    {
        LUAU_ASSERT(read && write);
        return Property::rw(*read, *write);
    }
}

TypeId Property::type_DEPRECATED() const
{
    LUAU_ASSERT(readTy);
    return *readTy;
}

void Property::setType(TypeId ty)
{
    readTy = ty;
    writeTy = ty;
}

void Property::makeShared()
{
    if (writeTy)
        writeTy = readTy;
}

bool Property::isShared() const
{
    return readTy && writeTy && readTy == writeTy;
}

bool Property::isReadOnly() const
{
    return readTy && !writeTy;
}

bool Property::isWriteOnly() const
{
    return !readTy && writeTy;
}

bool Property::isReadWrite() const
{
    return readTy && writeTy;
}

TableType::TableType(TableState state, TypeLevel level, Scope* scope)
    : state(state)
    , level(level)
    , scope(scope)
{
}

TableType::TableType(const Props& props, const std::optional<TableIndexer>& indexer, TypeLevel level, TableState state)
    : props(props)
    , indexer(indexer)
    , state(state)
    , level(level)
{
}

TableType::TableType(const Props& props, const std::optional<TableIndexer>& indexer, TypeLevel level, Scope* scope, TableState state)
    : props(props)
    , indexer(indexer)
    , state(state)
    , level(level)
    , scope(scope)
{
}

// Test Types for equivalence
// More complex than we'd like because Types can self-reference.

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

bool areEqual(SeenSet& seen, const FunctionType& lhs, const FunctionType& rhs)
{
    if (areSeen(seen, &lhs, &rhs))
        return true;

    // TODO: check generics CLI-39915

    if (!areEqual(seen, *lhs.argTypes, *rhs.argTypes))
        return false;

    if (!areEqual(seen, *lhs.retTypes, *rhs.retTypes))
        return false;

    return true;
}

bool areEqual(SeenSet& seen, const TableType& lhs, const TableType& rhs)
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

        if (FFlag::LuauSolverV2)
        {
            if (l->second.readTy && r->second.readTy)
            {
                if (!areEqual(seen, **l->second.readTy, **r->second.readTy))
                    return false;
            }
            else if (l->second.readTy || r->second.readTy)
                return false;

            if (l->second.writeTy && r->second.writeTy)
            {
                if (!areEqual(seen, **l->second.writeTy, **r->second.writeTy))
                    return false;
            }
            else if (l->second.writeTy || r->second.writeTy)
                return false;
        }
        else if (!areEqual(seen, *l->second.type_DEPRECATED(), *r->second.type_DEPRECATED()))
            return false;
        ++l;
        ++r;
    }

    return true;
}

static bool areEqual(SeenSet& seen, const MetatableType& lhs, const MetatableType& rhs)
{
    if (areSeen(seen, &lhs, &rhs))
        return true;

    return areEqual(seen, *lhs.table, *rhs.table) && areEqual(seen, *lhs.metatable, *rhs.metatable);
}

bool areEqual(SeenSet& seen, const Type& lhs, const Type& rhs)
{
    if (auto bound = get_if<BoundType>(&lhs.ty))
        return areEqual(seen, *bound->boundTo, rhs);

    if (auto bound = get_if<BoundType>(&rhs.ty))
        return areEqual(seen, lhs, *bound->boundTo);

    if (lhs.ty.index() != rhs.ty.index())
        return false;

    {
        const FreeType* lf = get_if<FreeType>(&lhs.ty);
        const FreeType* rf = get_if<FreeType>(&rhs.ty);
        if (lf && rf)
            return lf->index == rf->index;
    }

    {
        const GenericType* lg = get_if<GenericType>(&lhs.ty);
        const GenericType* rg = get_if<GenericType>(&rhs.ty);
        if (lg && rg)
            return lg->index == rg->index;
    }

    {
        const PrimitiveType* lp = get_if<PrimitiveType>(&lhs.ty);
        const PrimitiveType* rp = get_if<PrimitiveType>(&rhs.ty);
        if (lp && rp)
            return lp->type == rp->type;
    }

    {
        const GenericType* lg = get_if<GenericType>(&lhs.ty);
        const GenericType* rg = get_if<GenericType>(&rhs.ty);
        if (lg && rg)
            return lg->index == rg->index;
    }

    {
        const ErrorType* le = get_if<ErrorType>(&lhs.ty);
        const ErrorType* re = get_if<ErrorType>(&rhs.ty);
        if (le && re)
            return le->index == re->index;
    }

    {
        const FunctionType* lf = get_if<FunctionType>(&lhs.ty);
        const FunctionType* rf = get_if<FunctionType>(&rhs.ty);
        if (lf && rf)
            return areEqual(seen, *lf, *rf);
    }

    {
        const TableType* lt = get_if<TableType>(&lhs.ty);
        const TableType* rt = get_if<TableType>(&rhs.ty);
        if (lt && rt)
            return areEqual(seen, *lt, *rt);
    }

    {
        const MetatableType* lmt = get_if<MetatableType>(&lhs.ty);
        const MetatableType* rmt = get_if<MetatableType>(&rhs.ty);

        if (lmt && rmt)
            return areEqual(seen, *lmt, *rmt);
    }

    if (get_if<AnyType>(&lhs.ty) && get_if<AnyType>(&rhs.ty))
        return true;

    return false;
}

Type* asMutable(TypeId ty)
{
    return const_cast<Type*>(ty);
}

bool Type::operator==(const Type& rhs) const
{
    SeenSet seen;
    return areEqual(seen, *this, rhs);
}

bool Type::operator!=(const Type& rhs) const
{
    SeenSet seen;
    return !areEqual(seen, *this, rhs);
}

Type& Type::operator=(const TypeVariant& rhs)
{
    ty = rhs;
    return *this;
}

Type& Type::operator=(TypeVariant&& rhs)
{
    ty = std::move(rhs);
    return *this;
}

Type& Type::operator=(const Type& rhs)
{
    LUAU_ASSERT(owningArena == rhs.owningArena);
    LUAU_ASSERT(!rhs.persistent);

    reassign(rhs);

    return *this;
}

Type Type::clone() const
{
    return *this;
}

TypeId makeFunction(
    TypeArena& arena,
    std::optional<TypeId> selfType,
    std::initializer_list<TypeId> generics,
    std::initializer_list<TypePackId> genericPacks,
    std::initializer_list<TypeId> paramTypes,
    std::initializer_list<std::string> paramNames,
    std::initializer_list<TypeId> retTypes
);

TypeId makeStringMetatable(NotNull<BuiltinTypes> builtinTypes, SolverMode mode); // BuiltinDefinitions.cpp

BuiltinTypes::BuiltinTypes()
    : arena(new TypeArena)
    , debugFreezeArena(FFlag::DebugLuauFreezeArena)
    , typeFunctions(std::make_unique<BuiltinTypeFunctions>())
    , nilType(arena->addType(Type{PrimitiveType{PrimitiveType::NilType}, /*persistent*/ true}))
    , numberType(arena->addType(Type{PrimitiveType{PrimitiveType::Number}, /*persistent*/ true}))
    , stringType(arena->addType(Type{PrimitiveType{PrimitiveType::String}, /*persistent*/ true}))
    , booleanType(arena->addType(Type{PrimitiveType{PrimitiveType::Boolean}, /*persistent*/ true}))
    , threadType(arena->addType(Type{PrimitiveType{PrimitiveType::Thread}, /*persistent*/ true}))
    , bufferType(arena->addType(Type{PrimitiveType{PrimitiveType::Buffer}, /*persistent*/ true}))
    , functionType(arena->addType(Type{PrimitiveType{PrimitiveType::Function}, /*persistent*/ true}))
    , externType(arena->addType(Type{ExternType{"class", {}, std::nullopt, std::nullopt, {}, {}, {}, {}}, /*persistent*/ true}))
    , tableType(arena->addType(Type{PrimitiveType{PrimitiveType::Table}, /*persistent*/ true}))
    , emptyTableType(arena->addType(Type{TableType{TableState::Sealed, TypeLevel{}, nullptr}, /*persistent*/ true}))
    , trueType(arena->addType(Type{SingletonType{BooleanSingleton{true}}, /*persistent*/ true}))
    , falseType(arena->addType(Type{SingletonType{BooleanSingleton{false}}, /*persistent*/ true}))
    , anyType(arena->addType(Type{AnyType{}, /*persistent*/ true}))
    , unknownType(arena->addType(Type{UnknownType{}, /*persistent*/ true}))
    , neverType(arena->addType(Type{NeverType{}, /*persistent*/ true}))
    , errorType(arena->addType(Type{ErrorType{}, /*persistent*/ true}))
    , noRefineType(arena->addType(Type{NoRefineType{}, /*persistent*/ true}))
    , falsyType(arena->addType(Type{UnionType{{falseType, nilType}}, /*persistent*/ true}))
    , truthyType(arena->addType(Type{NegationType{falsyType}, /*persistent*/ true}))
    , notNilType(arena->addType(Type{NegationType{nilType}, /*persistent*/ true}))
    , optionalNumberType(arena->addType(Type{UnionType{{numberType, nilType}}, /*persistent*/ true}))
    , optionalStringType(arena->addType(Type{UnionType{{stringType, nilType}}, /*persistent*/ true}))
    , emptyTypePack(arena->addTypePack(TypePackVar{TypePack{{}}, /*persistent*/ true}))
    , anyTypePack(arena->addTypePack(TypePackVar{VariadicTypePack{anyType}, /*persistent*/ true}))
    , unknownTypePack(arena->addTypePack(TypePackVar{VariadicTypePack{unknownType}, /*persistent*/ true}))
    , neverTypePack(arena->addTypePack(TypePackVar{VariadicTypePack{neverType}, /*persistent*/ true}))
    , uninhabitableTypePack(arena->addTypePack(TypePackVar{TypePack{{neverType}, neverTypePack}, /*persistent*/ true}))
    , errorTypePack(arena->addTypePack(TypePackVar{ErrorTypePack{}, /*persistent*/ true}))
{
    freeze(*arena);
}

BuiltinTypes::~BuiltinTypes()
{
    // Destroy the arena with the same memory management flags it was created with
    bool prevFlag = FFlag::DebugLuauFreezeArena;
    FFlag::DebugLuauFreezeArena.value = debugFreezeArena;

    unfreeze(*arena);
    arena.reset(nullptr);

    FFlag::DebugLuauFreezeArena.value = prevFlag;
}

TypeId BuiltinTypes::errorRecoveryType(TypeId guess) const
{
    return guess;
}

TypePackId BuiltinTypes::errorRecoveryTypePack(TypePackId guess) const
{
    return guess;
}

void persist(TypeId ty)
{
    VecDeque<TypeId> queue{ty};

    while (!queue.empty())
    {
        TypeId t = queue.front();
        queue.pop_front();

        if (t->persistent)
            continue;

        asMutable(t)->persistent = true;

        if (auto btv = get<BoundType>(t))
            queue.push_back(btv->boundTo);
        else if (auto ftv = get<FunctionType>(t))
        {
            persist(ftv->argTypes);
            persist(ftv->retTypes);
        }
        else if (auto ttv = get<TableType>(t))
        {
            LUAU_ASSERT(ttv->state != TableState::Free && ttv->state != TableState::Unsealed);

            for (const auto& [_name, prop] : ttv->props)
            {
                if (FFlag::LuauSolverV2)
                {
                    if (prop.readTy)
                        queue.push_back(*prop.readTy);
                    if (prop.writeTy)
                        queue.push_back(*prop.writeTy);
                }
                else
                    queue.push_back(prop.type_DEPRECATED());
            }


            if (ttv->indexer)
            {
                queue.push_back(ttv->indexer->indexType);
                queue.push_back(ttv->indexer->indexResultType);
            }
        }
        else if (auto etv = get<ExternType>(t))
        {
            for (const auto& [_name, prop] : etv->props)
            {
                if (FFlag::LuauSolverV2)
                {
                    if (prop.readTy)
                        queue.push_back(*prop.readTy);
                    if (prop.writeTy)
                        queue.push_back(*prop.writeTy);
                }
                else
                    queue.push_back(prop.type_DEPRECATED());
            }
        }
        else if (auto utv = get<UnionType>(t))
        {
            for (TypeId opt : utv->options)
                queue.push_back(opt);
        }
        else if (auto itv = get<IntersectionType>(t))
        {
            for (TypeId opt : itv->parts)
                queue.push_back(opt);
        }
        else if (auto mtv = get<MetatableType>(t))
        {
            queue.push_back(mtv->table);
            queue.push_back(mtv->metatable);
        }
        else if (get<GenericType>(t) || get<AnyType>(t) || get<FreeType>(t) || get<SingletonType>(t) || get<PrimitiveType>(t) || get<NegationType>(t))
        {
        }
        else if (auto tfit = get<TypeFunctionInstanceType>(t))
        {
            for (auto ty : tfit->typeArguments)
                queue.push_back(ty);

            for (auto tp : tfit->packArguments)
                persist(tp);
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
    else if (auto tfitp = get<TypeFunctionInstanceTypePack>(tp))
    {
        for (auto ty : tfitp->typeArguments)
            persist(ty);

        for (auto tp : tfitp->packArguments)
            persist(tp);
    }
    else
    {
        LUAU_ASSERT(!"TypePackId is not supported in a persist call");
    }
}

const TypeLevel* getLevel(TypeId ty)
{
    ty = follow(ty);

    if (auto ftv = get<FreeType>(ty))
        return &ftv->level;
    else if (auto ttv = get<TableType>(ty))
        return &ttv->level;
    else if (auto ftv = get<FunctionType>(ty))
        return &ftv->level;
    else
        return nullptr;
}

TypeLevel* getMutableLevel(TypeId ty)
{
    return const_cast<TypeLevel*>(getLevel(ty));
}

std::optional<TypeLevel> getLevel(TypePackId tp)
{
    tp = follow(tp);

    if (auto ftv = get<FreeTypePack>(tp))
        return ftv->level;
    else
        return std::nullopt;
}

const Property* lookupExternTypeProp(const ExternType* cls, const Name& name)
{
    while (cls)
    {
        auto it = cls->props.find(name);
        if (it != cls->props.end())
            return &it->second;

        if (cls->parent)
            cls = get<ExternType>(*cls->parent);
        else
            return nullptr;

        LUAU_ASSERT(cls);
    }

    return nullptr;
}

bool isSubclass(const ExternType* cls, const ExternType* parent)
{
    while (cls)
    {
        if (cls == parent)
            return true;
        else if (!cls->parent)
            return false;

        cls = get<ExternType>(*cls->parent);
        LUAU_ASSERT(cls);
    }

    return false;
}

const std::vector<TypeId>& getTypes(const UnionType* utv)
{
    return utv->options;
}

const std::vector<TypeId>& getTypes(const IntersectionType* itv)
{
    return itv->parts;
}

UnionTypeIterator begin(const UnionType* utv)
{
    return UnionTypeIterator{utv};
}

UnionTypeIterator end(const UnionType* utv)
{
    return UnionTypeIterator{};
}

IntersectionTypeIterator begin(const IntersectionType* itv)
{
    return IntersectionTypeIterator{itv};
}

IntersectionTypeIterator end(const IntersectionType* itv)
{
    return IntersectionTypeIterator{};
}

TypeId freshType(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, Scope* scope, Polarity polarity)
{
    return arena->addType(FreeType{scope, builtinTypes->neverType, builtinTypes->unknownType, polarity});
}

std::vector<TypeId> filterMap(TypeId type, TypeIdPredicate predicate)
{
    type = follow(type);

    if (auto utv = get<UnionType>(type))
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

    if (auto ftv = getMutable<FunctionType>(ty))
        return &ftv->tags;
    else if (auto ttv = getMutable<TableType>(ty))
        return &ttv->tags;
    else if (auto etv = getMutable<ExternType>(ty))
        return &etv->tags;

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

    // We special case extern types because getTags only returns a pointer to one vector of tags.
    // But extern types has multiple vector of tags, represented throughout the hierarchy.
    if (auto etv = get<ExternType>(ty))
    {
        while (etv)
        {
            if (hasTag(etv->tags, tagName))
                return true;
            else if (!etv->parent)
                return false;

            etv = get<ExternType>(*etv->parent);
            LUAU_ASSERT(etv);
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

bool TypeFun::operator==(const TypeFun& rhs) const
{
    return type == rhs.type && typeParams == rhs.typeParams && typePackParams == rhs.typePackParams;
}

bool GenericTypeDefinition::operator==(const GenericTypeDefinition& rhs) const
{
    return ty == rhs.ty && defaultValue == rhs.defaultValue;
}

bool GenericTypePackDefinition::operator==(const GenericTypePackDefinition& rhs) const
{
    return tp == rhs.tp && defaultValue == rhs.defaultValue;
}

template<>
LUAU_NOINLINE Unifiable::Bound<TypeId>* emplaceType<BoundType>(Type* ty, TypeId& tyArg)
{
    LUAU_ASSERT(ty != follow(tyArg));
    return &ty->ty.emplace<BoundType>(tyArg);
}

} // namespace Luau
