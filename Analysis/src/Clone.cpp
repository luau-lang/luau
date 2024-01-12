// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Clone.h"

#include "Luau/NotNull.h"
#include "Luau/RecursionCounter.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/Unifiable.h"

LUAU_FASTFLAG(DebugLuauReadWriteProperties)

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)
LUAU_FASTINTVARIABLE(LuauTypeCloneRecursionLimit, 300)

LUAU_FASTFLAGVARIABLE(LuauStacklessTypeClone3, false)
LUAU_FASTINTVARIABLE(LuauTypeCloneIterationLimit, 100'000)

namespace Luau
{

namespace
{

using Kind = Variant<TypeId, TypePackId>;

template<typename T>
const T* get(const Kind& kind)
{
    return get_if<T>(&kind);
}

class TypeCloner2
{
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;

    // A queue of kinds where we cloned it, but whose interior types hasn't
    // been updated to point to new clones. Once all of its interior types
    // has been updated, it gets removed from the queue.
    std::vector<Kind> queue;

    NotNull<SeenTypes> types;
    NotNull<SeenTypePacks> packs;

    int steps = 0;

public:
    TypeCloner2(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<SeenTypes> types, NotNull<SeenTypePacks> packs)
        : arena(arena)
        , builtinTypes(builtinTypes)
        , types(types)
        , packs(packs)
    {
    }

    TypeId clone(TypeId ty)
    {
        shallowClone(ty);
        run();

        if (hasExceededIterationLimit())
        {
            TypeId error = builtinTypes->errorRecoveryType();
            (*types)[ty] = error;
            return error;
        }

        return find(ty).value_or(builtinTypes->errorRecoveryType());
    }

    TypePackId clone(TypePackId tp)
    {
        shallowClone(tp);
        run();

        if (hasExceededIterationLimit())
        {
            TypePackId error = builtinTypes->errorRecoveryTypePack();
            (*packs)[tp] = error;
            return error;
        }

        return find(tp).value_or(builtinTypes->errorRecoveryTypePack());
    }

private:
    bool hasExceededIterationLimit() const
    {
        if (FInt::LuauTypeCloneIterationLimit == 0)
            return false;

        return steps + queue.size() >= size_t(FInt::LuauTypeCloneIterationLimit);
    }

    void run()
    {
        while (!queue.empty())
        {
            ++steps;

            if (hasExceededIterationLimit())
                break;

            Kind kind = queue.back();
            queue.pop_back();

            if (find(kind))
                continue;

            cloneChildren(kind);
        }
    }

    std::optional<TypeId> find(TypeId ty) const
    {
        ty = follow(ty, FollowOption::DisableLazyTypeThunks);
        if (auto it = types->find(ty); it != types->end())
            return it->second;
        else if (ty->persistent)
            return ty;
        return std::nullopt;
    }

    std::optional<TypePackId> find(TypePackId tp) const
    {
        tp = follow(tp);
        if (auto it = packs->find(tp); it != packs->end())
            return it->second;
        else if (tp->persistent)
            return tp;
        return std::nullopt;
    }

    std::optional<Kind> find(Kind kind) const
    {
        if (auto ty = get<TypeId>(kind))
            return find(*ty);
        else if (auto tp = get<TypePackId>(kind))
            return find(*tp);
        else
        {
            LUAU_ASSERT(!"Unknown kind?");
            return std::nullopt;
        }
    }

private:
    TypeId shallowClone(TypeId ty)
    {
        // We want to [`Luau::follow`] but without forcing the expansion of [`LazyType`]s.
        ty = follow(ty, FollowOption::DisableLazyTypeThunks);

        if (auto clone = find(ty))
            return *clone;
        else if (ty->persistent)
            return ty;

        TypeId target = arena->addType(ty->ty);
        asMutable(target)->documentationSymbol = ty->documentationSymbol;

        if (auto generic = getMutable<GenericType>(target))
            generic->scope = nullptr;
        else if (auto free = getMutable<FreeType>(target))
            free->scope = nullptr;
        else if (auto fn = getMutable<FunctionType>(target))
            fn->scope = nullptr;
        else if (auto table = getMutable<TableType>(target))
            table->scope = nullptr;

        (*types)[ty] = target;
        queue.push_back(target);
        return target;
    }

    TypePackId shallowClone(TypePackId tp)
    {
        tp = follow(tp);

        if (auto clone = find(tp))
            return *clone;
        else if (tp->persistent)
            return tp;

        TypePackId target = arena->addTypePack(tp->ty);

        if (auto generic = getMutable<GenericTypePack>(target))
            generic->scope = nullptr;
        else if (auto free = getMutable<FreeTypePack>(target))
            free->scope = nullptr;

        (*packs)[tp] = target;
        queue.push_back(target);
        return target;
    }

    Property shallowClone(const Property& p)
    {
        if (FFlag::DebugLuauReadWriteProperties)
        {
            std::optional<TypeId> cloneReadTy;
            if (auto ty = p.readType())
                cloneReadTy = shallowClone(*ty);

            std::optional<TypeId> cloneWriteTy;
            if (auto ty = p.writeType())
                cloneWriteTy = shallowClone(*ty);

            std::optional<Property> cloned = Property::create(cloneReadTy, cloneWriteTy);
            LUAU_ASSERT(cloned);
            cloned->deprecated = p.deprecated;
            cloned->deprecatedSuggestion = p.deprecatedSuggestion;
            cloned->location = p.location;
            cloned->tags = p.tags;
            cloned->documentationSymbol = p.documentationSymbol;
            cloned->typeLocation = p.typeLocation;
            return *cloned;
        }
        else
        {
            return Property{
                shallowClone(p.type()),
                p.deprecated,
                p.deprecatedSuggestion,
                p.location,
                p.tags,
                p.documentationSymbol,
                p.typeLocation,
            };
        }
    }

    void cloneChildren(TypeId ty)
    {
        return visit(
            [&](auto&& t) {
                return cloneChildren(&t);
            },
            asMutable(ty)->ty);
    }

    void cloneChildren(TypePackId tp)
    {
        return visit(
            [&](auto&& t) {
                return cloneChildren(&t);
            },
            asMutable(tp)->ty);
    }

    void cloneChildren(Kind kind)
    {
        if (auto ty = get<TypeId>(kind))
            return cloneChildren(*ty);
        else if (auto tp = get<TypePackId>(kind))
            return cloneChildren(*tp);
        else
            LUAU_ASSERT(!"Item holds neither TypeId nor TypePackId when enqueuing its children?");
    }

    // ErrorType and ErrorTypePack is an alias to this type.
    void cloneChildren(Unifiable::Error* t)
    {
        // noop.
    }

    void cloneChildren(BoundType* t)
    {
        t->boundTo = shallowClone(t->boundTo);
    }

    void cloneChildren(FreeType* t)
    {
        if (t->lowerBound)
            t->lowerBound = shallowClone(t->lowerBound);
        if (t->upperBound)
            t->upperBound = shallowClone(t->upperBound);
    }

    void cloneChildren(LocalType* t)
    {
        t->domain = shallowClone(t->domain);
    }

    void cloneChildren(GenericType* t)
    {
        // TOOD: clone upper bounds.
    }

    void cloneChildren(PrimitiveType* t)
    {
        // noop.
    }

    void cloneChildren(BlockedType* t)
    {
        // TODO: In the new solver, we should ice.
    }

    void cloneChildren(PendingExpansionType* t)
    {
        // TODO: In the new solver, we should ice.
    }

    void cloneChildren(SingletonType* t)
    {
        // noop.
    }

    void cloneChildren(FunctionType* t)
    {
        for (TypeId& g : t->generics)
            g = shallowClone(g);

        for (TypePackId& gp : t->genericPacks)
            gp = shallowClone(gp);

        t->argTypes = shallowClone(t->argTypes);
        t->retTypes = shallowClone(t->retTypes);
    }

    void cloneChildren(TableType* t)
    {
        if (t->indexer)
        {
            t->indexer->indexType = shallowClone(t->indexer->indexType);
            t->indexer->indexResultType = shallowClone(t->indexer->indexResultType);
        }

        for (auto& [_, p] : t->props)
            p = shallowClone(p);

        for (TypeId& ty : t->instantiatedTypeParams)
            ty = shallowClone(ty);

        for (TypePackId& tp : t->instantiatedTypePackParams)
            tp = shallowClone(tp);
    }

    void cloneChildren(MetatableType* t)
    {
        t->table = shallowClone(t->table);
        t->metatable = shallowClone(t->metatable);
    }

    void cloneChildren(ClassType* t)
    {
        for (auto& [_, p] : t->props)
            p = shallowClone(p);

        if (t->parent)
            t->parent = shallowClone(*t->parent);

        if (t->metatable)
            t->metatable = shallowClone(*t->metatable);

        if (t->indexer)
        {
            t->indexer->indexType = shallowClone(t->indexer->indexType);
            t->indexer->indexResultType = shallowClone(t->indexer->indexResultType);
        }
    }

    void cloneChildren(AnyType* t)
    {
        // noop.
    }

    void cloneChildren(UnionType* t)
    {
        for (TypeId& ty : t->options)
            ty = shallowClone(ty);
    }

    void cloneChildren(IntersectionType* t)
    {
        for (TypeId& ty : t->parts)
            ty = shallowClone(ty);
    }

    void cloneChildren(LazyType* t)
    {
        if (auto unwrapped = t->unwrapped.load())
            t->unwrapped.store(shallowClone(unwrapped));
    }

    void cloneChildren(UnknownType* t)
    {
        // noop.
    }

    void cloneChildren(NeverType* t)
    {
        // noop.
    }

    void cloneChildren(NegationType* t)
    {
        t->ty = shallowClone(t->ty);
    }

    void cloneChildren(TypeFamilyInstanceType* t)
    {
        for (TypeId& ty : t->typeArguments)
            ty = shallowClone(ty);

        for (TypePackId& tp : t->packArguments)
            tp = shallowClone(tp);
    }

    void cloneChildren(FreeTypePack* t)
    {
        // TODO: clone lower and upper bounds.
        // TODO: In the new solver, we should ice.
    }

    void cloneChildren(GenericTypePack* t)
    {
        // TOOD: clone upper bounds.
    }

    void cloneChildren(BlockedTypePack* t)
    {
        // TODO: In the new solver, we should ice.
    }

    void cloneChildren(BoundTypePack* t)
    {
        t->boundTo = shallowClone(t->boundTo);
    }

    void cloneChildren(VariadicTypePack* t)
    {
        t->ty = shallowClone(t->ty);
    }

    void cloneChildren(TypePack* t)
    {
        for (TypeId& ty : t->head)
            ty = shallowClone(ty);

        if (t->tail)
            t->tail = shallowClone(*t->tail);
    }

    void cloneChildren(TypeFamilyInstanceTypePack* t)
    {
        for (TypeId& ty : t->typeArguments)
            ty = shallowClone(ty);

        for (TypePackId& tp : t->packArguments)
            tp = shallowClone(tp);
    }
};

} // namespace

namespace
{

Property clone(const Property& prop, TypeArena& dest, CloneState& cloneState)
{
    if (FFlag::DebugLuauReadWriteProperties)
    {
        std::optional<TypeId> cloneReadTy;
        if (auto ty = prop.readType())
            cloneReadTy = clone(*ty, dest, cloneState);

        std::optional<TypeId> cloneWriteTy;
        if (auto ty = prop.writeType())
            cloneWriteTy = clone(*ty, dest, cloneState);

        std::optional<Property> cloned = Property::create(cloneReadTy, cloneWriteTy);
        LUAU_ASSERT(cloned);
        cloned->deprecated = prop.deprecated;
        cloned->deprecatedSuggestion = prop.deprecatedSuggestion;
        cloned->location = prop.location;
        cloned->tags = prop.tags;
        cloned->documentationSymbol = prop.documentationSymbol;
        cloned->typeLocation = prop.typeLocation;
        return *cloned;
    }
    else
    {
        return Property{
            clone(prop.type(), dest, cloneState),
            prop.deprecated,
            prop.deprecatedSuggestion,
            prop.location,
            prop.tags,
            prop.documentationSymbol,
            prop.typeLocation,
        };
    }
}

static TableIndexer clone(const TableIndexer& indexer, TypeArena& dest, CloneState& cloneState)
{
    return TableIndexer{clone(indexer.indexType, dest, cloneState), clone(indexer.indexResultType, dest, cloneState)};
}

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

    void operator()(const FreeType& t);
    void operator()(const LocalType& t);
    void operator()(const GenericType& t);
    void operator()(const BoundType& t);
    void operator()(const ErrorType& t);
    void operator()(const BlockedType& t);
    void operator()(const PendingExpansionType& t);
    void operator()(const PrimitiveType& t);
    void operator()(const SingletonType& t);
    void operator()(const FunctionType& t);
    void operator()(const TableType& t);
    void operator()(const MetatableType& t);
    void operator()(const ClassType& t);
    void operator()(const AnyType& t);
    void operator()(const UnionType& t);
    void operator()(const IntersectionType& t);
    void operator()(const LazyType& t);
    void operator()(const UnknownType& t);
    void operator()(const NeverType& t);
    void operator()(const NegationType& t);
    void operator()(const TypeFamilyInstanceType& t);
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

    void operator()(const FreeTypePack& t)
    {
        defaultClone(t);
    }

    void operator()(const GenericTypePack& t)
    {
        defaultClone(t);
    }

    void operator()(const ErrorTypePack& t)
    {
        defaultClone(t);
    }

    void operator()(const BlockedTypePack& t)
    {
        defaultClone(t);
    }

    // While we are a-cloning, we can flatten out bound Types and make things a bit tighter.
    // We just need to be sure that we rewrite pointers both to the binder and the bindee to the same pointer.
    void operator()(const Unifiable::Bound<TypePackId>& t)
    {
        TypePackId cloned = clone(t.boundTo, dest, cloneState);
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

    void operator()(const TypeFamilyInstanceTypePack& t)
    {
        TypePackId cloned = dest.addTypePack(TypeFamilyInstanceTypePack{t.family, {}, {}});
        TypeFamilyInstanceTypePack* destTp = getMutable<TypeFamilyInstanceTypePack>(cloned);
        LUAU_ASSERT(destTp);
        seenTypePacks[typePackId] = cloned;

        destTp->typeArguments.reserve(t.typeArguments.size());
        for (TypeId ty : t.typeArguments)
            destTp->typeArguments.push_back(clone(ty, dest, cloneState));

        destTp->packArguments.reserve(t.packArguments.size());
        for (TypePackId tp : t.packArguments)
            destTp->packArguments.push_back(clone(tp, dest, cloneState));
    }
};

template<typename T>
void TypeCloner::defaultClone(const T& t)
{
    TypeId cloned = dest.addType(t);
    seenTypes[typeId] = cloned;
}

void TypeCloner::operator()(const FreeType& t)
{
    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        FreeType ft{nullptr, clone(t.lowerBound, dest, cloneState), clone(t.upperBound, dest, cloneState)};
        TypeId res = dest.addType(ft);
        seenTypes[typeId] = res;
    }
    else
        defaultClone(t);
}

void TypeCloner::operator()(const LocalType& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const GenericType& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const Unifiable::Bound<TypeId>& t)
{
    TypeId boundTo = clone(t.boundTo, dest, cloneState);
    seenTypes[typeId] = boundTo;
}

void TypeCloner::operator()(const Unifiable::Error& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const BlockedType& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const PendingExpansionType& t)
{
    TypeId res = dest.addType(PendingExpansionType{t.prefix, t.name, t.typeArguments, t.packArguments});
    PendingExpansionType* petv = getMutable<PendingExpansionType>(res);
    LUAU_ASSERT(petv);

    seenTypes[typeId] = res;

    std::vector<TypeId> typeArguments;
    for (TypeId arg : t.typeArguments)
        typeArguments.push_back(clone(arg, dest, cloneState));

    std::vector<TypePackId> packArguments;
    for (TypePackId arg : t.packArguments)
        packArguments.push_back(clone(arg, dest, cloneState));

    petv->typeArguments = std::move(typeArguments);
    petv->packArguments = std::move(packArguments);
}

void TypeCloner::operator()(const PrimitiveType& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const SingletonType& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const FunctionType& t)
{
    // FISHY: We always erase the scope when we clone things.  clone() was
    // originally written so that we could copy a module's type surface into an
    // export arena.  This probably dates to that.
    TypeId result = dest.addType(FunctionType{TypeLevel{0, 0}, {}, {}, nullptr, nullptr, t.definition, t.hasSelf});
    FunctionType* ftv = getMutable<FunctionType>(result);
    LUAU_ASSERT(ftv != nullptr);

    seenTypes[typeId] = result;

    for (TypeId generic : t.generics)
        ftv->generics.push_back(clone(generic, dest, cloneState));

    for (TypePackId genericPack : t.genericPacks)
        ftv->genericPacks.push_back(clone(genericPack, dest, cloneState));

    ftv->tags = t.tags;
    ftv->argTypes = clone(t.argTypes, dest, cloneState);
    ftv->argNames = t.argNames;
    ftv->retTypes = clone(t.retTypes, dest, cloneState);
    ftv->hasNoFreeOrGenericTypes = t.hasNoFreeOrGenericTypes;
    ftv->isCheckedFunction = t.isCheckedFunction;
}

void TypeCloner::operator()(const TableType& t)
{
    // If table is now bound to another one, we ignore the content of the original
    if (t.boundTo)
    {
        TypeId boundTo = clone(*t.boundTo, dest, cloneState);
        seenTypes[typeId] = boundTo;
        return;
    }

    TypeId result = dest.addType(TableType{});
    TableType* ttv = getMutable<TableType>(result);
    LUAU_ASSERT(ttv != nullptr);

    *ttv = t;

    seenTypes[typeId] = result;

    ttv->level = TypeLevel{0, 0};

    for (const auto& [name, prop] : t.props)
        ttv->props[name] = clone(prop, dest, cloneState);

    if (t.indexer)
        ttv->indexer = clone(*t.indexer, dest, cloneState);

    for (TypeId& arg : ttv->instantiatedTypeParams)
        arg = clone(arg, dest, cloneState);

    for (TypePackId& arg : ttv->instantiatedTypePackParams)
        arg = clone(arg, dest, cloneState);

    ttv->definitionModuleName = t.definitionModuleName;
    ttv->definitionLocation = t.definitionLocation;
    ttv->tags = t.tags;
}

void TypeCloner::operator()(const MetatableType& t)
{
    TypeId result = dest.addType(MetatableType{});
    MetatableType* mtv = getMutable<MetatableType>(result);
    seenTypes[typeId] = result;

    mtv->table = clone(t.table, dest, cloneState);
    mtv->metatable = clone(t.metatable, dest, cloneState);
}

void TypeCloner::operator()(const ClassType& t)
{
    TypeId result = dest.addType(ClassType{t.name, {}, std::nullopt, std::nullopt, t.tags, t.userData, t.definitionModuleName});
    ClassType* ctv = getMutable<ClassType>(result);

    seenTypes[typeId] = result;

    for (const auto& [name, prop] : t.props)
        ctv->props[name] = clone(prop, dest, cloneState);

    if (t.parent)
        ctv->parent = clone(*t.parent, dest, cloneState);

    if (t.metatable)
        ctv->metatable = clone(*t.metatable, dest, cloneState);

    if (t.indexer)
        ctv->indexer = clone(*t.indexer, dest, cloneState);
}

void TypeCloner::operator()(const AnyType& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const UnionType& t)
{
    // We're just using this FreeType as a placeholder until we've finished
    // cloning the parts of this union so it is okay that its bounds are
    // nullptr.  We'll never indirect them.
    TypeId result = dest.addType(FreeType{nullptr, /*lowerBound*/ nullptr, /*upperBound*/ nullptr});
    seenTypes[typeId] = result;

    std::vector<TypeId> options;
    options.reserve(t.options.size());

    for (TypeId ty : t.options)
        options.push_back(clone(ty, dest, cloneState));

    asMutable(result)->ty.emplace<UnionType>(std::move(options));
}

void TypeCloner::operator()(const IntersectionType& t)
{
    TypeId result = dest.addType(IntersectionType{});
    seenTypes[typeId] = result;

    IntersectionType* option = getMutable<IntersectionType>(result);
    LUAU_ASSERT(option != nullptr);

    for (TypeId ty : t.parts)
        option->parts.push_back(clone(ty, dest, cloneState));
}

void TypeCloner::operator()(const LazyType& t)
{
    if (TypeId unwrapped = t.unwrapped.load())
    {
        seenTypes[typeId] = clone(unwrapped, dest, cloneState);
    }
    else
    {
        defaultClone(t);
    }
}

void TypeCloner::operator()(const UnknownType& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const NeverType& t)
{
    defaultClone(t);
}

void TypeCloner::operator()(const NegationType& t)
{
    TypeId result = dest.addType(AnyType{});
    seenTypes[typeId] = result;

    TypeId ty = clone(t.ty, dest, cloneState);
    asMutable(result)->ty = NegationType{ty};
}

void TypeCloner::operator()(const TypeFamilyInstanceType& t)
{
    TypeId result = dest.addType(TypeFamilyInstanceType{
        t.family,
        {},
        {},
    });

    seenTypes[typeId] = result;

    TypeFamilyInstanceType* tfit = getMutable<TypeFamilyInstanceType>(result);
    LUAU_ASSERT(tfit != nullptr);

    tfit->typeArguments.reserve(t.typeArguments.size());
    for (TypeId p : t.typeArguments)
        tfit->typeArguments.push_back(clone(p, dest, cloneState));

    tfit->packArguments.reserve(t.packArguments.size());
    for (TypePackId p : t.packArguments)
        tfit->packArguments.push_back(clone(p, dest, cloneState));
}

} // anonymous namespace

TypePackId clone(TypePackId tp, TypeArena& dest, CloneState& cloneState)
{
    if (tp->persistent)
        return tp;

    if (FFlag::LuauStacklessTypeClone3)
    {
        TypeCloner2 cloner{NotNull{&dest}, cloneState.builtinTypes, NotNull{&cloneState.seenTypes}, NotNull{&cloneState.seenTypePacks}};
        return cloner.clone(tp);
    }
    else
    {
        RecursionLimiter _ra(&cloneState.recursionCount, FInt::LuauTypeCloneRecursionLimit);

        TypePackId& res = cloneState.seenTypePacks[tp];

        if (res == nullptr)
        {
            TypePackCloner cloner{dest, tp, cloneState};
            Luau::visit(cloner, tp->ty); // Mutates the storage that 'res' points into.
        }

        return res;
    }
}

TypeId clone(TypeId typeId, TypeArena& dest, CloneState& cloneState)
{
    if (typeId->persistent)
        return typeId;

    if (FFlag::LuauStacklessTypeClone3)
    {
        TypeCloner2 cloner{NotNull{&dest}, cloneState.builtinTypes, NotNull{&cloneState.seenTypes}, NotNull{&cloneState.seenTypePacks}};
        return cloner.clone(typeId);
    }
    else
    {
        RecursionLimiter _ra(&cloneState.recursionCount, FInt::LuauTypeCloneRecursionLimit);

        TypeId& res = cloneState.seenTypes[typeId];

        if (res == nullptr)
        {
            TypeCloner cloner{dest, typeId, cloneState};
            Luau::visit(cloner, typeId->ty); // Mutates the storage that 'res' points into.

            // Persistent types are not being cloned and we get the original type back which might be read-only
            if (!res->persistent)
            {
                asMutable(res)->documentationSymbol = typeId->documentationSymbol;
            }
        }

        return res;
    }
}

TypeFun clone(const TypeFun& typeFun, TypeArena& dest, CloneState& cloneState)
{
    if (FFlag::LuauStacklessTypeClone3)
    {
        TypeCloner2 cloner{NotNull{&dest}, cloneState.builtinTypes, NotNull{&cloneState.seenTypes}, NotNull{&cloneState.seenTypePacks}};

        TypeFun copy = typeFun;

        for (auto& param : copy.typeParams)
        {
            param.ty = cloner.clone(param.ty);

            if (param.defaultValue)
                param.defaultValue = cloner.clone(*param.defaultValue);
        }

        for (auto& param : copy.typePackParams)
        {
            param.tp = cloner.clone(param.tp);

            if (param.defaultValue)
                param.defaultValue = cloner.clone(*param.defaultValue);
        }

        copy.type = cloner.clone(copy.type);

        return copy;
    }
    else
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
}

} // namespace Luau
