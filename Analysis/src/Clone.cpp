// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Clone.h"
#include "Luau/Module.h"
#include "Luau/RecursionCounter.h"
#include "Luau/TypePack.h"
#include "Luau/Unifiable.h"

LUAU_FASTINTVARIABLE(LuauTypeCloneRecursionLimit, 300)

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
    TypeCloner(TypeArena& dest, TypeId typeId, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, CloneState& cloneState)
        : dest(dest)
        , typeId(typeId)
        , seenTypes(seenTypes)
        , seenTypePacks(seenTypePacks)
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

    TypePackCloner(TypeArena& dest, TypePackId typePackId, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, CloneState& cloneState)
        : dest(dest)
        , typePackId(typePackId)
        , seenTypes(seenTypes)
        , seenTypePacks(seenTypePacks)
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
        cloneState.encounteredFreeType = true;

        TypePackId err = getSingletonTypes().errorRecoveryTypePack(getSingletonTypes().anyTypePack);
        TypePackId cloned = dest.addTypePack(*err);
        seenTypePacks[typePackId] = cloned;
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
        TypePackId cloned = clone(t.boundTo, dest, seenTypes, seenTypePacks, cloneState);
        seenTypePacks[typePackId] = cloned;
    }

    void operator()(const VariadicTypePack& t)
    {
        TypePackId cloned = dest.addTypePack(TypePackVar{VariadicTypePack{clone(t.ty, dest, seenTypes, seenTypePacks, cloneState)}});
        seenTypePacks[typePackId] = cloned;
    }

    void operator()(const TypePack& t)
    {
        TypePackId cloned = dest.addTypePack(TypePack{});
        TypePack* destTp = getMutable<TypePack>(cloned);
        LUAU_ASSERT(destTp != nullptr);
        seenTypePacks[typePackId] = cloned;

        for (TypeId ty : t.head)
            destTp->head.push_back(clone(ty, dest, seenTypes, seenTypePacks, cloneState));

        if (t.tail)
            destTp->tail = clone(*t.tail, dest, seenTypes, seenTypePacks, cloneState);
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
    cloneState.encounteredFreeType = true;
    TypeId err = getSingletonTypes().errorRecoveryType(getSingletonTypes().anyType);
    TypeId cloned = dest.addType(*err);
    seenTypes[typeId] = cloned;
}

void TypeCloner::operator()(const Unifiable::Generic& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const Unifiable::Bound<TypeId>& t)
{
    TypeId boundTo = clone(t.boundTo, dest, seenTypes, seenTypePacks, cloneState);
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
        ftv->generics.push_back(clone(generic, dest, seenTypes, seenTypePacks, cloneState));

    for (TypePackId genericPack : t.genericPacks)
        ftv->genericPacks.push_back(clone(genericPack, dest, seenTypes, seenTypePacks, cloneState));

    ftv->tags = t.tags;
    ftv->argTypes = clone(t.argTypes, dest, seenTypes, seenTypePacks, cloneState);
    ftv->argNames = t.argNames;
    ftv->retType = clone(t.retType, dest, seenTypes, seenTypePacks, cloneState);
}

void TypeCloner::operator()(const TableTypeVar& t)
{
    // If table is now bound to another one, we ignore the content of the original
    if (t.boundTo)
    {
        TypeId boundTo = clone(*t.boundTo, dest, seenTypes, seenTypePacks, cloneState);
        seenTypes[typeId] = boundTo;
        return;
    }

    TypeId result = dest.addType(TableTypeVar{});
    TableTypeVar* ttv = getMutable<TableTypeVar>(result);
    LUAU_ASSERT(ttv != nullptr);

    *ttv = t;

    seenTypes[typeId] = result;

    ttv->level = TypeLevel{0, 0};

    for (const auto& [name, prop] : t.props)
        ttv->props[name] = {clone(prop.type, dest, seenTypes, seenTypePacks, cloneState), prop.deprecated, {}, prop.location, prop.tags};

    if (t.indexer)
        ttv->indexer = TableIndexer{clone(t.indexer->indexType, dest, seenTypes, seenTypePacks, cloneState),
            clone(t.indexer->indexResultType, dest, seenTypes, seenTypePacks, cloneState)};

    for (TypeId& arg : ttv->instantiatedTypeParams)
        arg = clone(arg, dest, seenTypes, seenTypePacks, cloneState);

    for (TypePackId& arg : ttv->instantiatedTypePackParams)
        arg = clone(arg, dest, seenTypes, seenTypePacks, cloneState);

    if (ttv->state == TableState::Free)
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

    mtv->table = clone(t.table, dest, seenTypes, seenTypePacks, cloneState);
    mtv->metatable = clone(t.metatable, dest, seenTypes, seenTypePacks, cloneState);
}

void TypeCloner::operator()(const ClassTypeVar& t)
{
    TypeId result = dest.addType(ClassTypeVar{t.name, {}, std::nullopt, std::nullopt, t.tags, t.userData});
    ClassTypeVar* ctv = getMutable<ClassTypeVar>(result);

    seenTypes[typeId] = result;

    for (const auto& [name, prop] : t.props)
        ctv->props[name] = {clone(prop.type, dest, seenTypes, seenTypePacks, cloneState), prop.deprecated, {}, prop.location, prop.tags};

    if (t.parent)
        ctv->parent = clone(*t.parent, dest, seenTypes, seenTypePacks, cloneState);

    if (t.metatable)
        ctv->metatable = clone(*t.metatable, dest, seenTypes, seenTypePacks, cloneState);
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
        options.push_back(clone(ty, dest, seenTypes, seenTypePacks, cloneState));

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
        option->parts.push_back(clone(ty, dest, seenTypes, seenTypePacks, cloneState));
}

void TypeCloner::operator()(const LazyTypeVar& t)
{
    defaultClone(t);
}

} // anonymous namespace

TypePackId clone(TypePackId tp, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, CloneState& cloneState)
{
    if (tp->persistent)
        return tp;

    RecursionLimiter _ra(&cloneState.recursionCount, FInt::LuauTypeCloneRecursionLimit);

    TypePackId& res = seenTypePacks[tp];

    if (res == nullptr)
    {
        TypePackCloner cloner{dest, tp, seenTypes, seenTypePacks, cloneState};
        Luau::visit(cloner, tp->ty); // Mutates the storage that 'res' points into.
    }

    return res;
}

TypeId clone(TypeId typeId, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, CloneState& cloneState)
{
    if (typeId->persistent)
        return typeId;

    RecursionLimiter _ra(&cloneState.recursionCount, FInt::LuauTypeCloneRecursionLimit);

    TypeId& res = seenTypes[typeId];

    if (res == nullptr)
    {
        TypeCloner cloner{dest, typeId, seenTypes, seenTypePacks, cloneState};
        Luau::visit(cloner, typeId->ty); // Mutates the storage that 'res' points into.

        // Persistent types are not being cloned and we get the original type back which might be read-only
        if (!res->persistent)
            asMutable(res)->documentationSymbol = typeId->documentationSymbol;
    }

    return res;
}

TypeFun clone(const TypeFun& typeFun, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, CloneState& cloneState)
{
    TypeFun result;

    for (auto param : typeFun.typeParams)
    {
        TypeId ty = clone(param.ty, dest, seenTypes, seenTypePacks, cloneState);
        std::optional<TypeId> defaultValue;

        if (param.defaultValue)
            defaultValue = clone(*param.defaultValue, dest, seenTypes, seenTypePacks, cloneState);

        result.typeParams.push_back({ty, defaultValue});
    }

    for (auto param : typeFun.typePackParams)
    {
        TypePackId tp = clone(param.tp, dest, seenTypes, seenTypePacks, cloneState);
        std::optional<TypePackId> defaultValue;

        if (param.defaultValue)
            defaultValue = clone(*param.defaultValue, dest, seenTypes, seenTypePacks, cloneState);

        result.typePackParams.push_back({tp, defaultValue});
    }

    result.type = clone(typeFun.type, dest, seenTypes, seenTypePacks, cloneState);

    return result;
}

} // namespace Luau
