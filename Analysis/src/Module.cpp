// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Module.h"

#include "Luau/TypeInfer.h"
#include "Luau/TypePack.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"
#include "Luau/Common.h"

#include <algorithm>

LUAU_FASTFLAGVARIABLE(DebugLuauFreezeArena, false)
LUAU_FASTFLAGVARIABLE(DebugLuauTrackOwningArena, false)
LUAU_FASTFLAG(LuauSecondTypecheckKnowsTheDataModel)
LUAU_FASTFLAG(LuauCaptureBrokenCommentSpans)

namespace Luau
{

static bool contains(Position pos, Comment comment)
{
    if (comment.location.contains(pos))
        return true;
    else if (FFlag::LuauCaptureBrokenCommentSpans && comment.type == Lexeme::BrokenComment &&
             comment.location.begin <= pos) // Broken comments are broken specifically because they don't have an end
        return true;
    else if (comment.type == Lexeme::Comment && comment.location.end == pos)
        return true;
    else
        return false;
}

bool isWithinComment(const SourceModule& sourceModule, Position pos)
{
    auto iter = std::lower_bound(sourceModule.commentLocations.begin(), sourceModule.commentLocations.end(),
        Comment{Lexeme::Comment, Location{pos, pos}}, [](const Comment& a, const Comment& b) {
            return a.location.end < b.location.end;
        });

    if (iter == sourceModule.commentLocations.end())
        return false;

    if (contains(pos, *iter))
        return true;

    // Due to the nature of std::lower_bound, it is possible that iter points at a comment that ends
    // at pos.  We'll try the next comment, if it exists.
    ++iter;
    if (iter == sourceModule.commentLocations.end())
        return false;

    return contains(pos, *iter);
}

void TypeArena::clear()
{
    typeVars.clear();
    typePacks.clear();
}

TypeId TypeArena::addTV(TypeVar&& tv)
{
    TypeId allocated = typeVars.allocate(std::move(tv));

    if (FFlag::DebugLuauTrackOwningArena)
        asMutable(allocated)->owningArena = this;

    return allocated;
}

TypeId TypeArena::freshType(TypeLevel level)
{
    TypeId allocated = typeVars.allocate(FreeTypeVar{level});

    if (FFlag::DebugLuauTrackOwningArena)
        asMutable(allocated)->owningArena = this;

    return allocated;
}

TypePackId TypeArena::addTypePack(std::initializer_list<TypeId> types)
{
    TypePackId allocated = typePacks.allocate(TypePack{std::move(types)});

    if (FFlag::DebugLuauTrackOwningArena)
        asMutable(allocated)->owningArena = this;

    return allocated;
}

TypePackId TypeArena::addTypePack(std::vector<TypeId> types)
{
    TypePackId allocated = typePacks.allocate(TypePack{std::move(types)});

    if (FFlag::DebugLuauTrackOwningArena)
        asMutable(allocated)->owningArena = this;

    return allocated;
}

TypePackId TypeArena::addTypePack(TypePack tp)
{
    TypePackId allocated = typePacks.allocate(std::move(tp));

    if (FFlag::DebugLuauTrackOwningArena)
        asMutable(allocated)->owningArena = this;

    return allocated;
}

TypePackId TypeArena::addTypePack(TypePackVar tp)
{
    TypePackId allocated = typePacks.allocate(std::move(tp));

    if (FFlag::DebugLuauTrackOwningArena)
        asMutable(allocated)->owningArena = this;

    return allocated;
}

using SeenTypes = std::unordered_map<TypeId, TypeId>;
using SeenTypePacks = std::unordered_map<TypePackId, TypePackId>;

TypePackId clone(TypePackId tp, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, bool* encounteredFreeType);
TypeId clone(TypeId tp, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, bool* encounteredFreeType);

namespace
{

struct TypePackCloner;

/*
 * Both TypeCloner and TypePackCloner work by depositing the requested type variable into the appropriate 'seen' set.
 * They do not return anything because their sole consumer (the deepClone function) already has a pointer into this storage.
 */

struct TypeCloner
{
    TypeCloner(TypeArena& dest, TypeId typeId, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks)
        : dest(dest)
        , typeId(typeId)
        , seenTypes(seenTypes)
        , seenTypePacks(seenTypePacks)
    {
    }

    TypeArena& dest;
    TypeId typeId;
    SeenTypes& seenTypes;
    SeenTypePacks& seenTypePacks;

    bool* encounteredFreeType = nullptr;

    template<typename T>
    void defaultClone(const T& t);

    void operator()(const Unifiable::Free& t);
    void operator()(const Unifiable::Generic& t);
    void operator()(const Unifiable::Bound<TypeId>& t);
    void operator()(const Unifiable::Error& t);
    void operator()(const PrimitiveTypeVar& t);
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
    bool* encounteredFreeType = nullptr;

    TypePackCloner(TypeArena& dest, TypePackId typePackId, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks)
        : dest(dest)
        , typePackId(typePackId)
        , seenTypes(seenTypes)
        , seenTypePacks(seenTypePacks)
    {
    }

    template<typename T>
    void defaultClone(const T& t)
    {
        TypePackId cloned = dest.typePacks.allocate(t);
        seenTypePacks[typePackId] = cloned;
    }

    void operator()(const Unifiable::Free& t)
    {
        if (encounteredFreeType)
            *encounteredFreeType = true;

        seenTypePacks[typePackId] = dest.typePacks.allocate(TypePackVar{Unifiable::Error{}});
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
        TypePackId cloned = clone(t.boundTo, dest, seenTypes, seenTypePacks, encounteredFreeType);
        seenTypePacks[typePackId] = cloned;
    }

    void operator()(const VariadicTypePack& t)
    {
        TypePackId cloned = dest.typePacks.allocate(VariadicTypePack{clone(t.ty, dest, seenTypes, seenTypePacks, encounteredFreeType)});
        seenTypePacks[typePackId] = cloned;
    }

    void operator()(const TypePack& t)
    {
        TypePackId cloned = dest.typePacks.allocate(TypePack{});
        TypePack* destTp = getMutable<TypePack>(cloned);
        LUAU_ASSERT(destTp != nullptr);
        seenTypePacks[typePackId] = cloned;

        for (TypeId ty : t.head)
            destTp->head.push_back(clone(ty, dest, seenTypes, seenTypePacks, encounteredFreeType));

        if (t.tail)
            destTp->tail = clone(*t.tail, dest, seenTypes, seenTypePacks, encounteredFreeType);
    }
};

template<typename T>
void TypeCloner::defaultClone(const T& t)
{
    TypeId cloned = dest.typeVars.allocate(t);
    seenTypes[typeId] = cloned;
}

void TypeCloner::operator()(const Unifiable::Free& t)
{
    if (encounteredFreeType)
        *encounteredFreeType = true;

    seenTypes[typeId] = dest.typeVars.allocate(ErrorTypeVar{});
}

void TypeCloner::operator()(const Unifiable::Generic& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const Unifiable::Bound<TypeId>& t)
{
    TypeId boundTo = clone(t.boundTo, dest, seenTypes, seenTypePacks, encounteredFreeType);
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

void TypeCloner::operator()(const FunctionTypeVar& t)
{
    TypeId result = dest.typeVars.allocate(FunctionTypeVar{TypeLevel{0, 0}, {}, {}, nullptr, nullptr, t.definition, t.hasSelf});
    FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(result);
    LUAU_ASSERT(ftv != nullptr);

    seenTypes[typeId] = result;

    for (TypeId generic : t.generics)
        ftv->generics.push_back(clone(generic, dest, seenTypes, seenTypePacks, encounteredFreeType));

    for (TypePackId genericPack : t.genericPacks)
        ftv->genericPacks.push_back(clone(genericPack, dest, seenTypes, seenTypePacks, encounteredFreeType));

    if (FFlag::LuauSecondTypecheckKnowsTheDataModel)
        ftv->tags = t.tags;

    ftv->argTypes = clone(t.argTypes, dest, seenTypes, seenTypePacks, encounteredFreeType);
    ftv->argNames = t.argNames;
    ftv->retType = clone(t.retType, dest, seenTypes, seenTypePacks, encounteredFreeType);
}

void TypeCloner::operator()(const TableTypeVar& t)
{
    TypeId result = dest.typeVars.allocate(TableTypeVar{});
    TableTypeVar* ttv = getMutable<TableTypeVar>(result);
    LUAU_ASSERT(ttv != nullptr);

    *ttv = t;

    seenTypes[typeId] = result;

    ttv->level = TypeLevel{0, 0};

    for (const auto& [name, prop] : t.props)
    {
        if (FFlag::LuauSecondTypecheckKnowsTheDataModel)
            ttv->props[name] = {clone(prop.type, dest, seenTypes, seenTypePacks, encounteredFreeType), prop.deprecated, {}, prop.location, prop.tags};
        else
            ttv->props[name] = {clone(prop.type, dest, seenTypes, seenTypePacks, encounteredFreeType), prop.deprecated, {}, prop.location};
    }

    if (t.indexer)
        ttv->indexer = TableIndexer{clone(t.indexer->indexType, dest, seenTypes, seenTypePacks, encounteredFreeType),
            clone(t.indexer->indexResultType, dest, seenTypes, seenTypePacks, encounteredFreeType)};

    if (t.boundTo)
        ttv->boundTo = clone(*t.boundTo, dest, seenTypes, seenTypePacks, encounteredFreeType);

    for (TypeId& arg : ttv->instantiatedTypeParams)
        arg = (clone(arg, dest, seenTypes, seenTypePacks, encounteredFreeType));

    if (ttv->state == TableState::Free)
    {
        if (!t.boundTo)
        {
            if (encounteredFreeType)
                *encounteredFreeType = true;
        }

        ttv->state = TableState::Sealed;
    }

    ttv->definitionModuleName = t.definitionModuleName;
    ttv->methodDefinitionLocations = t.methodDefinitionLocations;
    ttv->tags = t.tags;
}

void TypeCloner::operator()(const MetatableTypeVar& t)
{
    TypeId result = dest.typeVars.allocate(MetatableTypeVar{});
    MetatableTypeVar* mtv = getMutable<MetatableTypeVar>(result);
    seenTypes[typeId] = result;

    mtv->table = clone(t.table, dest, seenTypes, seenTypePacks, encounteredFreeType);
    mtv->metatable = clone(t.metatable, dest, seenTypes, seenTypePacks, encounteredFreeType);
}

void TypeCloner::operator()(const ClassTypeVar& t)
{
    TypeId result = dest.typeVars.allocate(ClassTypeVar{t.name, {}, std::nullopt, std::nullopt, t.tags, t.userData});
    ClassTypeVar* ctv = getMutable<ClassTypeVar>(result);

    seenTypes[typeId] = result;

    for (const auto& [name, prop] : t.props)
        if (FFlag::LuauSecondTypecheckKnowsTheDataModel)
            ctv->props[name] = {clone(prop.type, dest, seenTypes, seenTypePacks, encounteredFreeType), prop.deprecated, {}, prop.location, prop.tags};
        else
            ctv->props[name] = {clone(prop.type, dest, seenTypes, seenTypePacks, encounteredFreeType), prop.deprecated, {}, prop.location};

    if (t.parent)
        ctv->parent = clone(*t.parent, dest, seenTypes, seenTypePacks, encounteredFreeType);

    if (t.metatable)
        ctv->metatable = clone(*t.metatable, dest, seenTypes, seenTypePacks, encounteredFreeType);
}

void TypeCloner::operator()(const AnyTypeVar& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const UnionTypeVar& t)
{
    TypeId result = dest.typeVars.allocate(UnionTypeVar{});
    seenTypes[typeId] = result;

    UnionTypeVar* option = getMutable<UnionTypeVar>(result);
    LUAU_ASSERT(option != nullptr);

    for (TypeId ty : t.options)
        option->options.push_back(clone(ty, dest, seenTypes, seenTypePacks, encounteredFreeType));
}

void TypeCloner::operator()(const IntersectionTypeVar& t)
{
    TypeId result = dest.typeVars.allocate(IntersectionTypeVar{});
    seenTypes[typeId] = result;

    IntersectionTypeVar* option = getMutable<IntersectionTypeVar>(result);
    LUAU_ASSERT(option != nullptr);

    for (TypeId ty : t.parts)
        option->parts.push_back(clone(ty, dest, seenTypes, seenTypePacks, encounteredFreeType));
}

void TypeCloner::operator()(const LazyTypeVar& t)
{
    defaultClone(t);
}

} // anonymous namespace

TypePackId clone(TypePackId tp, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, bool* encounteredFreeType)
{
    if (tp->persistent)
        return tp;

    TypePackId& res = seenTypePacks[tp];

    if (res == nullptr)
    {
        TypePackCloner cloner{dest, tp, seenTypes, seenTypePacks};
        cloner.encounteredFreeType = encounteredFreeType;
        Luau::visit(cloner, tp->ty); // Mutates the storage that 'res' points into.
    }

    if (FFlag::DebugLuauTrackOwningArena)
        asMutable(res)->owningArena = &dest;

    return res;
}

TypeId clone(TypeId typeId, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, bool* encounteredFreeType)
{
    if (typeId->persistent)
        return typeId;

    TypeId& res = seenTypes[typeId];

    if (res == nullptr)
    {
        TypeCloner cloner{dest, typeId, seenTypes, seenTypePacks};
        cloner.encounteredFreeType = encounteredFreeType;
        Luau::visit(cloner, typeId->ty); // Mutates the storage that 'res' points into.
        asMutable(res)->documentationSymbol = typeId->documentationSymbol;
    }

    if (FFlag::DebugLuauTrackOwningArena)
        asMutable(res)->owningArena = &dest;

    return res;
}

TypeFun clone(const TypeFun& typeFun, TypeArena& dest, SeenTypes& seenTypes, SeenTypePacks& seenTypePacks, bool* encounteredFreeType)
{
    TypeFun result;
    for (TypeId param : typeFun.typeParams)
        result.typeParams.push_back(clone(param, dest, seenTypes, seenTypePacks, encounteredFreeType));

    result.type = clone(typeFun.type, dest, seenTypes, seenTypePacks, encounteredFreeType);

    return result;
}

ScopePtr Module::getModuleScope() const
{
    LUAU_ASSERT(!scopes.empty());
    return scopes.front().second;
}

void freeze(TypeArena& arena)
{
    if (!FFlag::DebugLuauFreezeArena)
        return;

    arena.typeVars.freeze();
    arena.typePacks.freeze();
}

void unfreeze(TypeArena& arena)
{
    if (!FFlag::DebugLuauFreezeArena)
        return;

    arena.typeVars.unfreeze();
    arena.typePacks.unfreeze();
}

Module::~Module()
{
    unfreeze(interfaceTypes);
    unfreeze(internalTypes);
}

bool Module::clonePublicInterface()
{
    LUAU_ASSERT(interfaceTypes.typeVars.empty());
    LUAU_ASSERT(interfaceTypes.typePacks.empty());

    bool encounteredFreeType = false;

    SeenTypePacks seenTypePacks;
    SeenTypes seenTypes;

    ScopePtr moduleScope = getModuleScope();

    moduleScope->returnType = clone(moduleScope->returnType, interfaceTypes, seenTypes, seenTypePacks, &encounteredFreeType);
    if (moduleScope->varargPack)
        moduleScope->varargPack = clone(*moduleScope->varargPack, interfaceTypes, seenTypes, seenTypePacks, &encounteredFreeType);

    for (auto& pair : moduleScope->exportedTypeBindings)
        pair.second = clone(pair.second, interfaceTypes, seenTypes, seenTypePacks, &encounteredFreeType);

    for (TypeId ty : moduleScope->returnType)
        if (get<GenericTypeVar>(follow(ty)))
            *asMutable(ty) = AnyTypeVar{};

    freeze(internalTypes);
    freeze(interfaceTypes);

    return encounteredFreeType;
}

} // namespace Luau
