// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Clone.h"

#include "Luau/Common.h"
#include "Luau/NotNull.h"
#include "Luau/Type.h"
#include "Luau/TypeOrPack.h"
#include "Luau/TypePack.h"
#include "Luau/Unifiable.h"
#include "Luau/VisitType.h"

LUAU_FASTFLAG(LuauSolverV2)

// For each `Luau::clone` call, we will clone only up to N amount of types _and_ packs, as controlled by this limit.
LUAU_FASTINTVARIABLE(LuauTypeCloneIterationLimit, 100'000)

namespace Luau
{

namespace
{

class TypeCloner
{

protected:
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;

    // A queue of kinds where we cloned it, but whose interior types hasn't
    // been updated to point to new clones. Once all of its interior types
    // has been updated, it gets removed from the queue.
    std::vector<TypeOrPack> queue;

    NotNull<SeenTypes> types;
    NotNull<SeenTypePacks> packs;

    TypeId forceTy = nullptr;
    TypePackId forceTp = nullptr;

    int steps = 0;

public:
    TypeCloner(
        NotNull<TypeArena> arena,
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<SeenTypes> types,
        NotNull<SeenTypePacks> packs,
        TypeId forceTy,
        TypePackId forceTp
    )
        : arena(arena)
        , builtinTypes(builtinTypes)
        , types(types)
        , packs(packs)
        , forceTy(forceTy)
        , forceTp(forceTp)
    {
    }

    virtual ~TypeCloner() = default;

    TypeId clone(TypeId ty)
    {
        shallowClone(ty);
        run();

        if (hasExceededIterationLimit())
        {
            TypeId error = builtinTypes->errorType;
            (*types)[ty] = error;
            return error;
        }

        return find(ty).value_or(builtinTypes->errorType);
    }

    TypePackId clone(TypePackId tp)
    {
        shallowClone(tp);
        run();

        if (hasExceededIterationLimit())
        {
            TypePackId error = builtinTypes->errorTypePack;
            (*packs)[tp] = error;
            return error;
        }

        return find(tp).value_or(builtinTypes->errorTypePack);
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

            TypeOrPack kind = queue.back();
            queue.pop_back();

            if (find(kind))
                continue;

            cloneChildren(kind);
        }
    }

protected:
    std::optional<TypeId> find(TypeId ty) const
    {
        ty = follow(ty, FollowOption::DisableLazyTypeThunks);
        if (auto it = types->find(ty); it != types->end())
            return it->second;
        else if (ty->persistent && ty != forceTy)
            return ty;
        return std::nullopt;
    }

    std::optional<TypePackId> find(TypePackId tp) const
    {
        tp = follow(tp);
        if (auto it = packs->find(tp); it != packs->end())
            return it->second;
        else if (tp->persistent && tp != forceTp)
            return tp;
        return std::nullopt;
    }

    std::optional<TypeOrPack> find(TypeOrPack kind) const
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

public:
    virtual TypeId shallowClone(TypeId ty)
    {
        // We want to [`Luau::follow`] but without forcing the expansion of [`LazyType`]s.
        ty = follow(ty, FollowOption::DisableLazyTypeThunks);

        if (auto clone = find(ty))
            return *clone;
        else if (ty->persistent && ty != forceTy)
            return ty;

        TypeId target = arena->addType(ty->ty);
        asMutable(target)->documentationSymbol = ty->documentationSymbol;

        if (auto generic = getMutable<GenericType>(target))
            generic->scope = nullptr;
        else if (auto free = getMutable<FreeType>(target))
            free->scope = nullptr;
        else if (auto table = getMutable<TableType>(target))
            table->scope = nullptr;

        (*types)[ty] = target;
        queue.push_back(target);
        return target;
    }

    virtual TypePackId shallowClone(TypePackId tp)
    {
        tp = follow(tp);

        if (auto clone = find(tp))
            return *clone;
        else if (tp->persistent && tp != forceTp)
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

private:
    Property shallowClone(const Property& p)
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

    void cloneChildren(TypeId ty)
    {
        return visit(
            [&](auto&& t)
            {
                return cloneChildren(&t);
            },
            asMutable(ty)->ty
        );
    }

    void cloneChildren(TypePackId tp)
    {
        return visit(
            [&](auto&& t)
            {
                return cloneChildren(&t);
            },
            asMutable(tp)->ty
        );
    }

    void cloneChildren(TypeOrPack kind)
    {
        if (auto ty = get<TypeId>(kind))
            return cloneChildren(*ty);
        else if (auto tp = get<TypePackId>(kind))
            return cloneChildren(*tp);
        else
            LUAU_ASSERT(!"Item holds neither TypeId nor TypePackId when enqueuing its children?");
    }

    void cloneChildren(ErrorType* t)
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

    void cloneChildren(ExternType* t)
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

    void cloneChildren(NoRefineType* t)
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

    virtual void cloneChildren(LazyType* t)
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

    void cloneChildren(ErrorTypePack* t)
    {
        // noop.
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

class FragmentAutocompleteTypeCloner final : public TypeCloner
{
    Scope* replacementForNullScope = nullptr;

public:
    FragmentAutocompleteTypeCloner(
        NotNull<TypeArena> arena,
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<SeenTypes> types,
        NotNull<SeenTypePacks> packs,
        TypeId forceTy,
        TypePackId forceTp,
        Scope* replacementForNullScope
    )
        : TypeCloner(arena, builtinTypes, types, packs, forceTy, forceTp)
        , replacementForNullScope(replacementForNullScope)
    {
        LUAU_ASSERT(replacementForNullScope);
    }

    TypeId shallowClone(TypeId ty) override
    {
        // We want to [`Luau::follow`] but without forcing the expansion of [`LazyType`]s.
        ty = follow(ty, FollowOption::DisableLazyTypeThunks);

        if (auto clone = find(ty))
            return *clone;
        else if (ty->persistent && ty != forceTy)
            return ty;

        TypeId target = arena->addType(ty->ty);
        asMutable(target)->documentationSymbol = ty->documentationSymbol;

        if (auto generic = getMutable<GenericType>(target))
            generic->scope = nullptr;
        else if (auto free = getMutable<FreeType>(target))
        {
            free->scope = replacementForNullScope;
        }
        else if (auto tt = getMutable<TableType>(target))
            tt->scope = replacementForNullScope;

        (*types)[ty] = target;
        queue.emplace_back(target);
        return target;
    }

    TypePackId shallowClone(TypePackId tp) override
    {
        tp = follow(tp);

        if (auto clone = find(tp))
            return *clone;
        else if (tp->persistent && tp != forceTp)
            return tp;

        TypePackId target = arena->addTypePack(tp->ty);

        if (auto generic = getMutable<GenericTypePack>(target))
            generic->scope = nullptr;
        else if (auto free = getMutable<FreeTypePack>(target))
            free->scope = replacementForNullScope;

        (*packs)[tp] = target;
        queue.emplace_back(target);
        return target;
    }

    void cloneChildren(LazyType* t) override
    {
        // Do not clone lazy types
    }
};


} // namespace

TypePackId shallowClone(TypePackId tp, TypeArena& dest, CloneState& cloneState, bool clonePersistentTypes)
{
    if (tp->persistent && !clonePersistentTypes)
        return tp;

    TypeCloner cloner{
        NotNull{&dest},
        cloneState.builtinTypes,
        NotNull{&cloneState.seenTypes},
        NotNull{&cloneState.seenTypePacks},
        nullptr,
        clonePersistentTypes ? tp : nullptr
    };

    return cloner.shallowClone(tp);
}

TypeId shallowClone(TypeId typeId, TypeArena& dest, CloneState& cloneState, bool clonePersistentTypes)
{
    if (typeId->persistent && !clonePersistentTypes)
        return typeId;

    TypeCloner cloner{
        NotNull{&dest},
        cloneState.builtinTypes,
        NotNull{&cloneState.seenTypes},
        NotNull{&cloneState.seenTypePacks},
        clonePersistentTypes ? typeId : nullptr,
        nullptr
    };

    return cloner.shallowClone(typeId);
}

TypePackId clone(TypePackId tp, TypeArena& dest, CloneState& cloneState)
{
    if (tp->persistent)
        return tp;

    TypeCloner cloner{NotNull{&dest}, cloneState.builtinTypes, NotNull{&cloneState.seenTypes}, NotNull{&cloneState.seenTypePacks}, nullptr, nullptr};
    return cloner.clone(tp);
}

TypeId clone(TypeId typeId, TypeArena& dest, CloneState& cloneState)
{
    if (typeId->persistent)
        return typeId;

    TypeCloner cloner{NotNull{&dest}, cloneState.builtinTypes, NotNull{&cloneState.seenTypes}, NotNull{&cloneState.seenTypePacks}, nullptr, nullptr};
    return cloner.clone(typeId);
}

TypeFun clone(const TypeFun& typeFun, TypeArena& dest, CloneState& cloneState)
{
    TypeCloner cloner{NotNull{&dest}, cloneState.builtinTypes, NotNull{&cloneState.seenTypes}, NotNull{&cloneState.seenTypePacks}, nullptr, nullptr};

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

Binding clone(const Binding& binding, TypeArena& dest, CloneState& cloneState)
{
    TypeCloner cloner{NotNull{&dest}, cloneState.builtinTypes, NotNull{&cloneState.seenTypes}, NotNull{&cloneState.seenTypePacks}, nullptr, nullptr};

    Binding b;
    b.deprecated = binding.deprecated;
    b.deprecatedSuggestion = binding.deprecatedSuggestion;
    b.documentationSymbol = binding.documentationSymbol;
    b.location = binding.location;
    b.typeId = cloner.clone(binding.typeId);

    return b;
}

TypePackId cloneIncremental(TypePackId tp, TypeArena& dest, CloneState& cloneState, Scope* freshScopeForFreeTypes)
{
    if (tp->persistent)
        return tp;

    FragmentAutocompleteTypeCloner cloner{
        NotNull{&dest},
        cloneState.builtinTypes,
        NotNull{&cloneState.seenTypes},
        NotNull{&cloneState.seenTypePacks},
        nullptr,
        nullptr,
        freshScopeForFreeTypes
    };
    return cloner.clone(tp);
}

TypeId cloneIncremental(TypeId typeId, TypeArena& dest, CloneState& cloneState, Scope* freshScopeForFreeTypes)
{
    if (typeId->persistent)
        return typeId;

    FragmentAutocompleteTypeCloner cloner{
        NotNull{&dest},
        cloneState.builtinTypes,
        NotNull{&cloneState.seenTypes},
        NotNull{&cloneState.seenTypePacks},
        nullptr,
        nullptr,
        freshScopeForFreeTypes
    };
    return cloner.clone(typeId);
}

TypeFun cloneIncremental(const TypeFun& typeFun, TypeArena& dest, CloneState& cloneState, Scope* freshScopeForFreeTypes)
{
    FragmentAutocompleteTypeCloner cloner{
        NotNull{&dest},
        cloneState.builtinTypes,
        NotNull{&cloneState.seenTypes},
        NotNull{&cloneState.seenTypePacks},
        nullptr,
        nullptr,
        freshScopeForFreeTypes
    };

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

Binding cloneIncremental(const Binding& binding, TypeArena& dest, CloneState& cloneState, Scope* freshScopeForFreeTypes)
{
    FragmentAutocompleteTypeCloner cloner{
        NotNull{&dest},
        cloneState.builtinTypes,
        NotNull{&cloneState.seenTypes},
        NotNull{&cloneState.seenTypePacks},
        nullptr,
        nullptr,
        freshScopeForFreeTypes
    };

    Binding b;
    b.deprecated = binding.deprecated;
    b.deprecatedSuggestion = binding.deprecatedSuggestion;
    b.documentationSymbol = binding.documentationSymbol;
    b.location = binding.location;
    b.typeId = binding.typeId->persistent ? binding.typeId : cloner.clone(binding.typeId);

    return b;
}


} // namespace Luau
