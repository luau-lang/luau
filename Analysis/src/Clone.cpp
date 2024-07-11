// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Clone.h"

#include "Luau/NotNull.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/Unifiable.h"

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

// For each `Luau::clone` call, we will clone only up to N amount of types _and_ packs, as controlled by this limit.
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

class TypeCloner
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
    TypeCloner(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<SeenTypes> types, NotNull<SeenTypePacks> packs)
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
        if (FFlag::DebugLuauDeferredConstraintResolution)
        {
            std::optional<TypeId> cloneReadTy;
            if (auto ty = p.readTy)
                cloneReadTy = shallowClone(*ty);

            std::optional<TypeId> cloneWriteTy;
            if (auto ty = p.writeTy)
                cloneWriteTy = shallowClone(*ty);

            Property cloned = Property::create(cloneReadTy, cloneWriteTy);
            cloned.deprecated = p.deprecated;
            cloned.deprecatedSuggestion = p.deprecatedSuggestion;
            cloned.location = p.location;
            cloned.tags = p.tags;
            cloned.documentationSymbol = p.documentationSymbol;
            cloned.typeLocation = p.typeLocation;
            return cloned;
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

    void cloneChildren(TypeFunctionInstanceType* t)
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

    void cloneChildren(TypeFunctionInstanceTypePack* t)
    {
        for (TypeId& ty : t->typeArguments)
            ty = shallowClone(ty);

        for (TypePackId& tp : t->packArguments)
            tp = shallowClone(tp);
    }
};

} // namespace

TypePackId clone(TypePackId tp, TypeArena& dest, CloneState& cloneState)
{
    if (tp->persistent)
        return tp;

    TypeCloner cloner{NotNull{&dest}, cloneState.builtinTypes, NotNull{&cloneState.seenTypes}, NotNull{&cloneState.seenTypePacks}};
    return cloner.clone(tp);
}

TypeId clone(TypeId typeId, TypeArena& dest, CloneState& cloneState)
{
    if (typeId->persistent)
        return typeId;

    TypeCloner cloner{NotNull{&dest}, cloneState.builtinTypes, NotNull{&cloneState.seenTypes}, NotNull{&cloneState.seenTypePacks}};
    return cloner.clone(typeId);
}

TypeFun clone(const TypeFun& typeFun, TypeArena& dest, CloneState& cloneState)
{
    TypeCloner cloner{NotNull{&dest}, cloneState.builtinTypes, NotNull{&cloneState.seenTypes}, NotNull{&cloneState.seenTypePacks}};

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

} // namespace Luau
