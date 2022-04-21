// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Clone.h"
#include "Luau/Module.h"
#include "Luau/RecursionCounter.h"
#include "Luau/TypePack.h"
#include "Luau/Unifiable.h"

LUAU_FASTFLAG(DebugLuauCopyBeforeNormalizing)

LUAU_FASTINTVARIABLE(LuauTypeCloneRecursionLimit, 300)
LUAU_FASTFLAG(LuauTypecheckOptPass)
LUAU_FASTFLAGVARIABLE(LuauLosslessClone, false)

namespace Luau
{

namespace
{

struct TypePackCloner;

/*
 * Both TypeCloner and TypePackCloner work by depositing the requested type variable into the appropriate 'seen' set.
 * They do not return anything because their sole consumer (the deepClone function) already has a pointer into this storage.
 */

struct TypeCloner
{
    TypeCloner(TypeArena& dest, TypeId typeId, CloneState& cloneState)
        : dest(dest)
        , typeId(typeId)
        , seenTypes(cloneState.seenTypes)
        , seenTypePacks(cloneState.seenTypePacks)
        , cloneState(cloneState)
    {
    }

    TypeArena& dest;
    TypeId typeId;
    SeenTypes& seenTypes;
    SeenTypePacks& seenTypePacks;
    CloneState& cloneState;

    template<typename T>
    void defaultClone(const T& t);

    void operator()(const Unifiable::Free& t);
    void operator()(const Unifiable::Generic& t);
    void operator()(const Unifiable::Bound<TypeId>& t);
    void operator()(const Unifiable::Error& t);
    void operator()(const PrimitiveTypeVar& t);
    void operator()(const ConstrainedTypeVar& t);
    void operator()(const SingletonTypeVar& t);
    void operator()(const FunctionTypeVar& t);
    void operator()(const TableTypeVar& t);
    void operator()(const MetatableTypeVar& t);
    void operator()(const ClassTypeVar& t);
    void operator()(const AnyTypeVar& t);
    void operator()(const UnionTypeVar& t);
    void operator()(const IntersectionTypeVar& t);
    void operator()(const LazyTypeVar& t);
};

struct TypePackCloner
{
    TypeArena& dest;
    TypePackId typePackId;
    SeenTypes& seenTypes;
    SeenTypePacks& seenTypePacks;
    CloneState& cloneState;

    TypePackCloner(TypeArena& dest, TypePackId typePackId, CloneState& cloneState)
        : dest(dest)
        , typePackId(typePackId)
        , seenTypes(cloneState.seenTypes)
        , seenTypePacks(cloneState.seenTypePacks)
        , cloneState(cloneState)
    {
    }

    template<typename T>
    void defaultClone(const T& t)
    {
        TypePackId cloned = dest.addTypePack(TypePackVar{t});
        seenTypePacks[typePackId] = cloned;
    }

    void operator()(const Unifiable::Free& t)
    {
        if (FFlag::LuauLosslessClone)
        {
            defaultClone(t);
        }
        else
        {
            cloneState.encounteredFreeType = true;

            TypePackId err = getSingletonTypes().errorRecoveryTypePack(getSingletonTypes().anyTypePack);
            TypePackId cloned = dest.addTypePack(*err);
            seenTypePacks[typePackId] = cloned;
        }
    }

    void operator()(const Unifiable::Generic& t)
    {
        defaultClone(t);
    }
    void operator()(const Unifiable::Error& t)
    {
        defaultClone(t);
    }

    // While we are a-cloning, we can flatten out bound TypeVars and make things a bit tighter.
    // We just need to be sure that we rewrite pointers both to the binder and the bindee to the same pointer.
    void operator()(const Unifiable::Bound<TypePackId>& t)
    {
        TypePackId cloned = clone(t.boundTo, dest, cloneState);
        if (FFlag::DebugLuauCopyBeforeNormalizing)
            cloned = dest.addTypePack(TypePackVar{BoundTypePack{cloned}});
        seenTypePacks[typePackId] = cloned;
    }

    void operator()(const VariadicTypePack& t)
    {
        TypePackId cloned = dest.addTypePack(TypePackVar{VariadicTypePack{clone(t.ty, dest, cloneState), /*hidden*/ t.hidden}});
        seenTypePacks[typePackId] = cloned;
    }

    void operator()(const TypePack& t)
    {
        TypePackId cloned = dest.addTypePack(TypePack{});
        TypePack* destTp = getMutable<TypePack>(cloned);
        LUAU_ASSERT(destTp != nullptr);
        seenTypePacks[typePackId] = cloned;

        for (TypeId ty : t.head)
            destTp->head.push_back(clone(ty, dest, cloneState));

        if (t.tail)
            destTp->tail = clone(*t.tail, dest, cloneState);
    }
};

template<typename T>
void TypeCloner::defaultClone(const T& t)
{
    TypeId cloned = dest.addType(t);
    seenTypes[typeId] = cloned;
}

void TypeCloner::operator()(const Unifiable::Free& t)
{
    if (FFlag::LuauLosslessClone)
    {
        defaultClone(t);
    }
    else
    {
        cloneState.encounteredFreeType = true;

        TypeId err = getSingletonTypes().errorRecoveryType(getSingletonTypes().anyType);
        TypeId cloned = dest.addType(*err);
        seenTypes[typeId] = cloned;
    }
}

void TypeCloner::operator()(const Unifiable::Generic& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const Unifiable::Bound<TypeId>& t)
{
    TypeId boundTo = clone(t.boundTo, dest, cloneState);
    if (FFlag::DebugLuauCopyBeforeNormalizing)
        boundTo = dest.addType(BoundTypeVar{boundTo});
    seenTypes[typeId] = boundTo;
}

void TypeCloner::operator()(const Unifiable::Error& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const PrimitiveTypeVar& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const ConstrainedTypeVar& t)
{
    if (!FFlag::LuauLosslessClone)
        cloneState.encounteredFreeType = true;

    TypeId res = dest.addType(ConstrainedTypeVar{t.level});
    ConstrainedTypeVar* ctv = getMutable<ConstrainedTypeVar>(res);
    LUAU_ASSERT(ctv);

    seenTypes[typeId] = res;

    std::vector<TypeId> parts;
    for (TypeId part : t.parts)
        parts.push_back(clone(part, dest, cloneState));

    ctv->parts = std::move(parts);
}

void TypeCloner::operator()(const SingletonTypeVar& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const FunctionTypeVar& t)
{
    TypeId result = dest.addType(FunctionTypeVar{TypeLevel{0, 0}, {}, {}, nullptr, nullptr, t.definition, t.hasSelf});
    FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(result);
    LUAU_ASSERT(ftv != nullptr);

    seenTypes[typeId] = result;

    for (TypeId generic : t.generics)
        ftv->generics.push_back(clone(generic, dest, cloneState));

    for (TypePackId genericPack : t.genericPacks)
        ftv->genericPacks.push_back(clone(genericPack, dest, cloneState));

    ftv->tags = t.tags;
    ftv->argTypes = clone(t.argTypes, dest, cloneState);
    ftv->argNames = t.argNames;
    ftv->retType = clone(t.retType, dest, cloneState);

    if (FFlag::LuauTypecheckOptPass)
        ftv->hasNoGenerics = t.hasNoGenerics;
}

void TypeCloner::operator()(const TableTypeVar& t)
{
    // If table is now bound to another one, we ignore the content of the original
    if (!FFlag::DebugLuauCopyBeforeNormalizing && t.boundTo)
    {
        TypeId boundTo = clone(*t.boundTo, dest, cloneState);
        seenTypes[typeId] = boundTo;
        return;
    }

    TypeId result = dest.addType(TableTypeVar{});
    TableTypeVar* ttv = getMutable<TableTypeVar>(result);
    LUAU_ASSERT(ttv != nullptr);

    *ttv = t;

    seenTypes[typeId] = result;

    ttv->level = TypeLevel{0, 0};

    if (FFlag::DebugLuauCopyBeforeNormalizing && t.boundTo)
        ttv->boundTo = clone(*t.boundTo, dest, cloneState);

    for (const auto& [name, prop] : t.props)
        ttv->props[name] = {clone(prop.type, dest, cloneState), prop.deprecated, {}, prop.location, prop.tags};

    if (t.indexer)
        ttv->indexer = TableIndexer{clone(t.indexer->indexType, dest, cloneState), clone(t.indexer->indexResultType, dest, cloneState)};

    for (TypeId& arg : ttv->instantiatedTypeParams)
        arg = clone(arg, dest, cloneState);

    for (TypePackId& arg : ttv->instantiatedTypePackParams)
        arg = clone(arg, dest, cloneState);

    if (!FFlag::LuauLosslessClone && ttv->state == TableState::Free)
    {
        cloneState.encounteredFreeType = true;

        ttv->state = TableState::Sealed;
    }

    ttv->definitionModuleName = t.definitionModuleName;
    ttv->methodDefinitionLocations = t.methodDefinitionLocations;
    ttv->tags = t.tags;
}

void TypeCloner::operator()(const MetatableTypeVar& t)
{
    TypeId result = dest.addType(MetatableTypeVar{});
    MetatableTypeVar* mtv = getMutable<MetatableTypeVar>(result);
    seenTypes[typeId] = result;

    mtv->table = clone(t.table, dest, cloneState);
    mtv->metatable = clone(t.metatable, dest, cloneState);
}

void TypeCloner::operator()(const ClassTypeVar& t)
{
    TypeId result = dest.addType(ClassTypeVar{t.name, {}, std::nullopt, std::nullopt, t.tags, t.userData, t.definitionModuleName});
    ClassTypeVar* ctv = getMutable<ClassTypeVar>(result);

    seenTypes[typeId] = result;

    for (const auto& [name, prop] : t.props)
        ctv->props[name] = {clone(prop.type, dest, cloneState), prop.deprecated, {}, prop.location, prop.tags};

    if (t.parent)
        ctv->parent = clone(*t.parent, dest, cloneState);

    if (t.metatable)
        ctv->metatable = clone(*t.metatable, dest, cloneState);
}

void TypeCloner::operator()(const AnyTypeVar& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const UnionTypeVar& t)
{
    std::vector<TypeId> options;
    options.reserve(t.options.size());

    for (TypeId ty : t.options)
        options.push_back(clone(ty, dest, cloneState));

    TypeId result = dest.addType(UnionTypeVar{std::move(options)});
    seenTypes[typeId] = result;
}

void TypeCloner::operator()(const IntersectionTypeVar& t)
{
    TypeId result = dest.addType(IntersectionTypeVar{});
    seenTypes[typeId] = result;

    IntersectionTypeVar* option = getMutable<IntersectionTypeVar>(result);
    LUAU_ASSERT(option != nullptr);

    for (TypeId ty : t.parts)
        option->parts.push_back(clone(ty, dest, cloneState));
}

void TypeCloner::operator()(const LazyTypeVar& t)
{
    defaultClone(t);
}

} // anonymous namespace

TypePackId clone(TypePackId tp, TypeArena& dest, CloneState& cloneState)
{
    if (tp->persistent)
        return tp;

    RecursionLimiter _ra(&cloneState.recursionCount, FInt::LuauTypeCloneRecursionLimit, "cloning TypePackId");

    TypePackId& res = cloneState.seenTypePacks[tp];

    if (res == nullptr)
    {
        TypePackCloner cloner{dest, tp, cloneState};
        Luau::visit(cloner, tp->ty); // Mutates the storage that 'res' points into.
    }

    return res;
}

TypeId clone(TypeId typeId, TypeArena& dest, CloneState& cloneState)
{
    if (typeId->persistent)
        return typeId;

    RecursionLimiter _ra(&cloneState.recursionCount, FInt::LuauTypeCloneRecursionLimit, "cloning TypeId");

    TypeId& res = cloneState.seenTypes[typeId];

    if (res == nullptr)
    {
        TypeCloner cloner{dest, typeId, cloneState};
        Luau::visit(cloner, typeId->ty); // Mutates the storage that 'res' points into.

        // Persistent types are not being cloned and we get the original type back which might be read-only
        if (!res->persistent)
        {
            asMutable(res)->documentationSymbol = typeId->documentationSymbol;
            asMutable(res)->normal = typeId->normal;
        }
    }

    return res;
}

TypeFun clone(const TypeFun& typeFun, TypeArena& dest, CloneState& cloneState)
{
    TypeFun result;

    for (auto param : typeFun.typeParams)
    {
        TypeId ty = clone(param.ty, dest, cloneState);
        std::optional<TypeId> defaultValue;

        if (param.defaultValue)
            defaultValue = clone(*param.defaultValue, dest, cloneState);

        result.typeParams.push_back({ty, defaultValue});
    }

    for (auto param : typeFun.typePackParams)
    {
        TypePackId tp = clone(param.tp, dest, cloneState);
        std::optional<TypePackId> defaultValue;

        if (param.defaultValue)
            defaultValue = clone(*param.defaultValue, dest, cloneState);

        result.typePackParams.push_back({tp, defaultValue});
    }

    result.type = clone(typeFun.type, dest, cloneState);

    return result;
}

} // namespace Luau
