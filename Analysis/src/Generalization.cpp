// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Generalization.h"

#include "Luau/Scope.h"
#include "Luau/Type.h"
#include "Luau/ToString.h"
#include "Luau/TypeArena.h"
#include "Luau/TypePack.h"
#include "Luau/VisitType.h"

LUAU_FASTFLAG(LuauAutocompleteRefactorsForIncrementalAutocomplete)

namespace Luau
{

struct MutatingGeneralizer : TypeOnceVisitor
{
    NotNull<BuiltinTypes> builtinTypes;

    NotNull<Scope> scope;
    NotNull<DenseHashSet<TypeId>> cachedTypes;
    DenseHashMap<const void*, size_t> positiveTypes;
    DenseHashMap<const void*, size_t> negativeTypes;
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;

    bool isWithinFunction = false;
    bool avoidSealingTables = false;

    MutatingGeneralizer(
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<Scope> scope,
        NotNull<DenseHashSet<TypeId>> cachedTypes,
        DenseHashMap<const void*, size_t> positiveTypes,
        DenseHashMap<const void*, size_t> negativeTypes,
        bool avoidSealingTables
    )
        : TypeOnceVisitor(/* skipBoundTypes */ true)
        , builtinTypes(builtinTypes)
        , scope(scope)
        , cachedTypes(cachedTypes)
        , positiveTypes(std::move(positiveTypes))
        , negativeTypes(std::move(negativeTypes))
        , avoidSealingTables(avoidSealingTables)
    {
    }

    static void replace(DenseHashSet<TypeId>& seen, TypeId haystack, TypeId needle, TypeId replacement)
    {
        haystack = follow(haystack);

        if (seen.find(haystack))
            return;
        seen.insert(haystack);

        if (UnionType* ut = getMutable<UnionType>(haystack))
        {
            for (auto iter = ut->options.begin(); iter != ut->options.end();)
            {
                // FIXME: I bet this function has reentrancy problems
                TypeId option = follow(*iter);

                if (option == needle && get<NeverType>(replacement))
                {
                    iter = ut->options.erase(iter);
                    continue;
                }

                if (option == needle)
                {
                    *iter = replacement;
                    iter++;
                    continue;
                }

                // advance the iterator, nothing after this can use it.
                iter++;

                if (seen.find(option))
                    continue;
                seen.insert(option);

                if (get<UnionType>(option))
                    replace(seen, option, needle, haystack);
                else if (get<IntersectionType>(option))
                    replace(seen, option, needle, haystack);
            }

            if (ut->options.size() == 1)
            {
                TypeId onlyType = ut->options[0];
                LUAU_ASSERT(onlyType != haystack);
                emplaceType<BoundType>(asMutable(haystack), onlyType);
            }

            return;
        }

        if (IntersectionType* it = getMutable<IntersectionType>(needle))
        {
            for (auto iter = it->parts.begin(); iter != it->parts.end();)
            {
                // FIXME: I bet this function has reentrancy problems
                TypeId part = follow(*iter);

                if (part == needle && get<UnknownType>(replacement))
                {
                    iter = it->parts.erase(iter);
                    continue;
                }

                if (part == needle)
                {
                    *iter = replacement;
                    iter++;
                    continue;
                }

                // advance the iterator, nothing after this can use it.
                iter++;

                if (seen.find(part))
                    continue;
                seen.insert(part);

                if (get<UnionType>(part))
                    replace(seen, part, needle, haystack);
                else if (get<IntersectionType>(part))
                    replace(seen, part, needle, haystack);
            }

            if (it->parts.size() == 1)
            {
                TypeId onlyType = it->parts[0];
                LUAU_ASSERT(onlyType != needle);
                emplaceType<BoundType>(asMutable(needle), onlyType);
            }

            return;
        }
    }

    bool visit(TypeId ty, const FunctionType& ft) override
    {
        if (cachedTypes->contains(ty))
            return false;

        const bool oldValue = isWithinFunction;

        isWithinFunction = true;

        traverse(ft.argTypes);
        traverse(ft.retTypes);

        isWithinFunction = oldValue;

        return false;
    }

    bool visit(TypeId ty, const FreeType&) override
    {
        LUAU_ASSERT(!cachedTypes->contains(ty));

        const FreeType* ft = get<FreeType>(ty);
        LUAU_ASSERT(ft);

        traverse(ft->lowerBound);
        traverse(ft->upperBound);

        // It is possible for the above traverse() calls to cause ty to be
        // transmuted.  We must reacquire ft if this happens.
        ty = follow(ty);
        ft = get<FreeType>(ty);
        if (!ft)
            return false;

        const size_t positiveCount = getCount(positiveTypes, ty);
        const size_t negativeCount = getCount(negativeTypes, ty);

        if (!positiveCount && !negativeCount)
            return false;

        const bool hasLowerBound = !get<NeverType>(follow(ft->lowerBound));
        const bool hasUpperBound = !get<UnknownType>(follow(ft->upperBound));

        DenseHashSet<TypeId> seen{nullptr};
        seen.insert(ty);

        if (!hasLowerBound && !hasUpperBound)
        {
            if (!isWithinFunction || (positiveCount + negativeCount == 1))
                emplaceType<BoundType>(asMutable(ty), builtinTypes->unknownType);
            else
            {
                emplaceType<GenericType>(asMutable(ty), scope);
                generics.push_back(ty);
            }
        }

        // It is possible that this free type has other free types in its upper
        // or lower bounds.  If this is the case, we must replace those
        // references with never (for the lower bound) or unknown (for the upper
        // bound).
        //
        // If we do not do this, we get tautological bounds like a <: a <: unknown.
        else if (positiveCount && !hasUpperBound)
        {
            TypeId lb = follow(ft->lowerBound);
            if (FreeType* lowerFree = getMutable<FreeType>(lb); lowerFree && lowerFree->upperBound == ty)
                lowerFree->upperBound = builtinTypes->unknownType;
            else
            {
                DenseHashSet<TypeId> replaceSeen{nullptr};
                replace(replaceSeen, lb, ty, builtinTypes->unknownType);
            }

            if (lb != ty)
                emplaceType<BoundType>(asMutable(ty), lb);
            else if (!isWithinFunction || (positiveCount + negativeCount == 1))
                emplaceType<BoundType>(asMutable(ty), builtinTypes->unknownType);
            else
            {
                // if the lower bound is the type in question, we don't actually have a lower bound.
                emplaceType<GenericType>(asMutable(ty), scope);
                generics.push_back(ty);
            }
        }
        else
        {
            TypeId ub = follow(ft->upperBound);
            if (FreeType* upperFree = getMutable<FreeType>(ub); upperFree && upperFree->lowerBound == ty)
                upperFree->lowerBound = builtinTypes->neverType;
            else
            {
                DenseHashSet<TypeId> replaceSeen{nullptr};
                replace(replaceSeen, ub, ty, builtinTypes->neverType);
            }

            if (ub != ty)
                emplaceType<BoundType>(asMutable(ty), ub);
            else if (!isWithinFunction || (positiveCount + negativeCount == 1))
                emplaceType<BoundType>(asMutable(ty), builtinTypes->unknownType);
            else
            {
                // if the upper bound is the type in question, we don't actually have an upper bound.
                emplaceType<GenericType>(asMutable(ty), scope);
                generics.push_back(ty);
            }
        }

        return false;
    }

    size_t getCount(const DenseHashMap<const void*, size_t>& map, const void* ty)
    {
        if (const size_t* count = map.find(ty))
            return *count;
        else
            return 0;
    }

    bool visit(TypeId ty, const TableType&) override
    {
        if (cachedTypes->contains(ty))
            return false;

        const size_t positiveCount = getCount(positiveTypes, ty);
        const size_t negativeCount = getCount(negativeTypes, ty);

        // FIXME: Free tables should probably just be replaced by upper bounds on free types.
        //
        // eg never <: 'a <: {x: number} & {z: boolean}

        if (!positiveCount && !negativeCount)
            return true;

        TableType* tt = getMutable<TableType>(ty);
        LUAU_ASSERT(tt);

        if (!avoidSealingTables)
            tt->state = TableState::Sealed;

        return true;
    }

    bool visit(TypePackId tp, const FreeTypePack& ftp) override
    {
        if (!subsumes(scope, ftp.scope))
            return true;

        tp = follow(tp);

        const size_t positiveCount = getCount(positiveTypes, tp);
        const size_t negativeCount = getCount(negativeTypes, tp);

        if (1 == positiveCount + negativeCount)
            emplaceTypePack<BoundTypePack>(asMutable(tp), builtinTypes->unknownTypePack);
        else
        {
            emplaceTypePack<GenericTypePack>(asMutable(tp), scope);
            genericPacks.push_back(tp);
        }

        return true;
    }
};

struct FreeTypeSearcher : TypeVisitor
{
    NotNull<Scope> scope;
    NotNull<DenseHashSet<TypeId>> cachedTypes;

    explicit FreeTypeSearcher(NotNull<Scope> scope, NotNull<DenseHashSet<TypeId>> cachedTypes)
        : TypeVisitor(/*skipBoundTypes*/ true)
        , scope(scope)
        , cachedTypes(cachedTypes)
    {
    }

    enum Polarity
    {
        Positive,
        Negative,
        Both,
    };

    Polarity polarity = Positive;

    void flip()
    {
        switch (polarity)
        {
        case Positive:
            polarity = Negative;
            break;
        case Negative:
            polarity = Positive;
            break;
        case Both:
            break;
        }
    }

    DenseHashSet<const void*> seenPositive{nullptr};
    DenseHashSet<const void*> seenNegative{nullptr};

    bool seenWithPolarity(const void* ty)
    {
        switch (polarity)
        {
        case Positive:
        {
            if (seenPositive.contains(ty))
                return true;

            seenPositive.insert(ty);
            return false;
        }
        case Negative:
        {
            if (seenNegative.contains(ty))
                return true;

            seenNegative.insert(ty);
            return false;
        }
        case Both:
        {
            if (seenPositive.contains(ty) && seenNegative.contains(ty))
                return true;

            seenPositive.insert(ty);
            seenNegative.insert(ty);
            return false;
        }
        }

        return false;
    }

    // The keys in these maps are either TypeIds or TypePackIds. It's safe to
    // mix them because we only use these pointers as unique keys.  We never
    // indirect them.
    DenseHashMap<const void*, size_t> negativeTypes{0};
    DenseHashMap<const void*, size_t> positiveTypes{0};

    bool visit(TypeId ty) override
    {
        if (cachedTypes->contains(ty) || seenWithPolarity(ty))
            return false;

        LUAU_ASSERT(ty);
        return true;
    }

    bool visit(TypeId ty, const FreeType& ft) override
    {
        if (cachedTypes->contains(ty) || seenWithPolarity(ty))
            return false;

        if (!subsumes(scope, ft.scope))
            return true;

        switch (polarity)
        {
        case Positive:
            positiveTypes[ty]++;
            break;
        case Negative:
            negativeTypes[ty]++;
            break;
        case Both:
            positiveTypes[ty]++;
            negativeTypes[ty]++;
            break;
        }

        return true;
    }

    bool visit(TypeId ty, const TableType& tt) override
    {
        if (cachedTypes->contains(ty) || seenWithPolarity(ty))
            return false;

        if ((tt.state == TableState::Free || tt.state == TableState::Unsealed) && subsumes(scope, tt.scope))
        {
            switch (polarity)
            {
            case Positive:
                positiveTypes[ty]++;
                break;
            case Negative:
                negativeTypes[ty]++;
                break;
            case Both:
                positiveTypes[ty]++;
                negativeTypes[ty]++;
                break;
            }
        }

        for (const auto& [_name, prop] : tt.props)
        {
            if (prop.isReadOnly())
                traverse(*prop.readTy);
            else
            {
                LUAU_ASSERT(prop.isShared() || FFlag::LuauAutocompleteRefactorsForIncrementalAutocomplete);

                Polarity p = polarity;
                polarity = Both;
                traverse(prop.type());
                polarity = p;
            }
        }

        if (tt.indexer)
        {
            traverse(tt.indexer->indexType);
            traverse(tt.indexer->indexResultType);
        }

        return false;
    }

    bool visit(TypeId ty, const FunctionType& ft) override
    {
        if (cachedTypes->contains(ty) || seenWithPolarity(ty))
            return false;

        flip();
        traverse(ft.argTypes);
        flip();

        traverse(ft.retTypes);

        return false;
    }

    bool visit(TypeId, const ClassType&) override
    {
        return false;
    }

    bool visit(TypePackId tp, const FreeTypePack& ftp) override
    {
        if (seenWithPolarity(tp))
            return false;

        if (!subsumes(scope, ftp.scope))
            return true;

        switch (polarity)
        {
        case Positive:
            positiveTypes[tp]++;
            break;
        case Negative:
            negativeTypes[tp]++;
            break;
        case Both:
            positiveTypes[tp]++;
            negativeTypes[tp]++;
            break;
        }

        return true;
    }
};

// We keep a running set of types that will not change under generalization and
// only have outgoing references to types that are the same.  We use this to
// short circuit generalization.  It improves performance quite a lot.
//
// We do this by tracing through the type and searching for types that are
// uncacheable. If a type has a reference to an uncacheable type, it is itself
// uncacheable.
//
// If a type has no outbound references to uncacheable types, we add it to the
// cache.
struct TypeCacher : TypeOnceVisitor
{
    NotNull<DenseHashSet<TypeId>> cachedTypes;

    DenseHashSet<TypeId> uncacheable{nullptr};
    DenseHashSet<TypePackId> uncacheablePacks{nullptr};

    explicit TypeCacher(NotNull<DenseHashSet<TypeId>> cachedTypes)
        : TypeOnceVisitor(/* skipBoundTypes */ false)
        , cachedTypes(cachedTypes)
    {
    }

    void cache(TypeId ty)
    {
        cachedTypes->insert(ty);
    }

    bool isCached(TypeId ty) const
    {
        return cachedTypes->contains(ty);
    }

    void markUncacheable(TypeId ty)
    {
        uncacheable.insert(ty);
    }

    void markUncacheable(TypePackId tp)
    {
        uncacheablePacks.insert(tp);
    }

    bool isUncacheable(TypeId ty) const
    {
        return uncacheable.contains(ty);
    }

    bool isUncacheable(TypePackId tp) const
    {
        return uncacheablePacks.contains(tp);
    }

    bool visit(TypeId ty) override
    {
        // NOTE: `TypeCacher` should explicitly visit _all_ types and type packs,
        // otherwise it's prone to marking types that cannot be cached as
        // cacheable.
        LUAU_ASSERT(false);
        LUAU_UNREACHABLE();
    }

    bool visit(TypeId ty, const BoundType& btv) override
    {
        traverse(btv.boundTo);
        if (isUncacheable(btv.boundTo))
            markUncacheable(ty);
        return false;
    }

    bool visit(TypeId ty, const FreeType& ft) override
    {
        // Free types are never cacheable.
        LUAU_ASSERT(!isCached(ty));

        if (!isUncacheable(ty))
        {
            traverse(ft.lowerBound);
            traverse(ft.upperBound);

            markUncacheable(ty);
        }

        return false;
    }

    bool visit(TypeId ty, const GenericType&) override
    {
        cache(ty);
        return false;
    }

    bool visit(TypeId ty, const ErrorType&) override
    {
        cache(ty);
        return false;
    }

    bool visit(TypeId ty, const PrimitiveType&) override
    {
        cache(ty);
        return false;
    }

    bool visit(TypeId ty, const SingletonType&) override
    {
        cache(ty);
        return false;
    }

    bool visit(TypeId ty, const BlockedType&) override
    {
        markUncacheable(ty);
        return false;
    }

    bool visit(TypeId ty, const PendingExpansionType&) override
    {
        markUncacheable(ty);
        return false;
    }

    bool visit(TypeId ty, const FunctionType& ft) override
    {
        if (isCached(ty) || isUncacheable(ty))
            return false;

        traverse(ft.argTypes);
        traverse(ft.retTypes);
        for (TypeId gen : ft.generics)
            traverse(gen);

        bool uncacheable = false;

        if (isUncacheable(ft.argTypes))
            uncacheable = true;

        else if (isUncacheable(ft.retTypes))
            uncacheable = true;

        for (TypeId argTy : ft.argTypes)
        {
            if (isUncacheable(argTy))
            {
                uncacheable = true;
                break;
            }
        }

        for (TypeId retTy : ft.retTypes)
        {
            if (isUncacheable(retTy))
            {
                uncacheable = true;
                break;
            }
        }

        for (TypeId g : ft.generics)
        {
            if (isUncacheable(g))
            {
                uncacheable = true;
                break;
            }
        }

        if (uncacheable)
            markUncacheable(ty);
        else
            cache(ty);

        return false;
    }

    bool visit(TypeId ty, const TableType& tt) override
    {
        if (isCached(ty) || isUncacheable(ty))
            return false;

        if (tt.boundTo)
        {
            traverse(*tt.boundTo);
            if (isUncacheable(*tt.boundTo))
            {
                markUncacheable(ty);
                return false;
            }
        }

        bool uncacheable = false;

        // This logic runs immediately after generalization, so any remaining
        // unsealed tables are assuredly not cacheable.  They may yet have
        // properties added to them.
        if (tt.state == TableState::Free || tt.state == TableState::Unsealed)
            uncacheable = true;

        for (const auto& [_name, prop] : tt.props)
        {
            if (prop.readTy)
            {
                traverse(*prop.readTy);

                if (isUncacheable(*prop.readTy))
                    uncacheable = true;
            }
            if (prop.writeTy && prop.writeTy != prop.readTy)
            {
                traverse(*prop.writeTy);

                if (isUncacheable(*prop.writeTy))
                    uncacheable = true;
            }
        }

        if (tt.indexer)
        {
            traverse(tt.indexer->indexType);
            if (isUncacheable(tt.indexer->indexType))
                uncacheable = true;

            traverse(tt.indexer->indexResultType);
            if (isUncacheable(tt.indexer->indexResultType))
                uncacheable = true;
        }

        if (uncacheable)
            markUncacheable(ty);
        else
            cache(ty);

        return false;
    }

    bool visit(TypeId ty, const MetatableType& mtv) override
    {
        traverse(mtv.table);
        traverse(mtv.metatable);
        if (isUncacheable(mtv.table) || isUncacheable(mtv.metatable))
            markUncacheable(ty);
        else
            cache(ty);
        return false;
    }

    bool visit(TypeId ty, const ClassType&) override
    {
        cache(ty);
        return false;
    }

    bool visit(TypeId ty, const AnyType&) override
    {
        cache(ty);
        return false;
    }

    bool visit(TypeId ty, const NoRefineType&) override
    {
        cache(ty);
        return false;
    }

    bool visit(TypeId ty, const UnionType& ut) override
    {
        if (isUncacheable(ty) || isCached(ty))
            return false;

        bool uncacheable = false;

        for (TypeId partTy : ut.options)
        {
            traverse(partTy);

            uncacheable |= isUncacheable(partTy);
        }

        if (uncacheable)
            markUncacheable(ty);
        else
            cache(ty);

        return false;
    }

    bool visit(TypeId ty, const IntersectionType& it) override
    {
        if (isUncacheable(ty) || isCached(ty))
            return false;

        bool uncacheable = false;

        for (TypeId partTy : it.parts)
        {
            traverse(partTy);

            uncacheable |= isUncacheable(partTy);
        }

        if (uncacheable)
            markUncacheable(ty);
        else
            cache(ty);

        return false;
    }

    bool visit(TypeId ty, const UnknownType&) override
    {
        cache(ty);
        return false;
    }

    bool visit(TypeId ty, const NeverType&) override
    {
        cache(ty);
        return false;
    }

    bool visit(TypeId ty, const NegationType& nt) override
    {
        if (!isCached(ty) && !isUncacheable(ty))
        {
            traverse(nt.ty);

            if (isUncacheable(nt.ty))
                markUncacheable(ty);
            else
                cache(ty);
        }

        return false;
    }

    bool visit(TypeId ty, const TypeFunctionInstanceType& tfit) override
    {
        if (isCached(ty) || isUncacheable(ty))
            return false;

        bool uncacheable = false;

        for (TypeId argTy : tfit.typeArguments)
        {
            traverse(argTy);

            if (isUncacheable(argTy))
                uncacheable = true;
        }

        for (TypePackId argPack : tfit.packArguments)
        {
            traverse(argPack);

            if (isUncacheable(argPack))
                uncacheable = true;
        }

        if (uncacheable)
            markUncacheable(ty);
        else
            cache(ty);

        return false;
    }

    bool visit(TypePackId tp) override
    {
        // NOTE: `TypeCacher` should explicitly visit _all_ types and type packs,
        // otherwise it's prone to marking types that cannot be cached as
        // cacheable, which will segfault down the line.
        LUAU_ASSERT(false);
        LUAU_UNREACHABLE();
    }

    bool visit(TypePackId tp, const FreeTypePack&) override
    {
        markUncacheable(tp);
        return false;
    }

    bool visit(TypePackId tp, const GenericTypePack& gtp) override
    {
        return true;
    }

    bool visit(TypePackId tp, const ErrorTypePack& etp) override
    {
        return true;
    }

    bool visit(TypePackId tp, const VariadicTypePack& vtp) override
    {
        if (isUncacheable(tp))
            return false;

        traverse(vtp.ty);

        if (isUncacheable(vtp.ty))
            markUncacheable(tp);

        return false;
    }

    bool visit(TypePackId tp, const BlockedTypePack&) override
    {
        markUncacheable(tp);
        return false;
    }

    bool visit(TypePackId tp, const TypeFunctionInstanceTypePack&) override
    {
        markUncacheable(tp);
        return false;
    }

    bool visit(TypePackId tp, const BoundTypePack& btp) override {
        traverse(btp.boundTo);
        if (isUncacheable(btp.boundTo))
            markUncacheable(tp);
        return false;
    }

    bool visit(TypePackId tp, const TypePack& typ) override
    {
        bool uncacheable = false;
        for (TypeId ty : typ.head)
        {
            traverse(ty);
            uncacheable |= isUncacheable(ty);
        }
        if (typ.tail)
        {
            traverse(*typ.tail);
            uncacheable |= isUncacheable(*typ.tail);
        }
        if (uncacheable)
            markUncacheable(tp);
        return false;
    }
};

std::optional<TypeId> generalize(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    NotNull<DenseHashSet<TypeId>> cachedTypes,
    TypeId ty,
    bool avoidSealingTables
)
{
    ty = follow(ty);

    if (ty->owningArena != arena || ty->persistent)
        return ty;

    FreeTypeSearcher fts{scope, cachedTypes};
    fts.traverse(ty);

    MutatingGeneralizer gen{builtinTypes, scope, cachedTypes, std::move(fts.positiveTypes), std::move(fts.negativeTypes), avoidSealingTables};

    gen.traverse(ty);

    /* MutatingGeneralizer mutates types in place, so it is possible that ty has
     * been transmuted to a BoundType. We must follow it again and verify that
     * we are allowed to mutate it before we attach generics to it.
     */
    ty = follow(ty);

    if (ty->owningArena != arena || ty->persistent)
        return ty;

    TypeCacher cacher{cachedTypes};
    cacher.traverse(ty);

    FunctionType* ftv = getMutable<FunctionType>(ty);
    if (ftv)
    {
        // If we're generalizing a function type, add any of the newly inferred
        // generics to the list of existing generic types.
        for (const auto g : std::move(gen.generics))
        {
            ftv->generics.push_back(g);
        }
        // Ditto for generic packs.
        for (const auto gp : std::move(gen.genericPacks))
        {
            ftv->genericPacks.push_back(gp);
        }
    }

    return ty;
}

} // namespace Luau
