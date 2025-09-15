// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Generalization.h"

#include "Luau/Common.h"
#include "Luau/DenseHash.h"
#include "Luau/InsertionOrderedMap.h"
#include "Luau/OrderedSet.h"
#include "Luau/Polarity.h"
#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeIds.h"
#include "Luau/TypePack.h"
#include "Luau/VisitType.h"

LUAU_FASTFLAGVARIABLE(LuauReduceSetTypeStackPressure)
LUAU_FASTINTVARIABLE(LuauGenericCounterMaxDepth, 15)
LUAU_FASTFLAG(LuauExplicitSkipBoundTypes)

namespace Luau
{

struct FreeTypeSearcher : TypeVisitor
{
    NotNull<Scope> scope;
    NotNull<DenseHashSet<TypeId>> cachedTypes;

    explicit FreeTypeSearcher(NotNull<Scope> scope, NotNull<DenseHashSet<TypeId>> cachedTypes)
        : TypeVisitor("FreeTypeSearcher", /* skipBoundTypes */ true)
        , scope(scope)
        , cachedTypes(cachedTypes)
    {
    }

    bool isWithinFunction = false;
    Polarity polarity = Polarity::Positive;

    void flip()
    {
        polarity = invert(polarity);
    }

    DenseHashSet<const void*> seenPositive{nullptr};
    DenseHashSet<const void*> seenNegative{nullptr};

    bool seenWithCurrentPolarity(const void* ty)
    {
        switch (polarity)
        {
        case Polarity::Positive:
        {
            if (seenPositive.contains(ty))
                return true;

            seenPositive.insert(ty);
            return false;
        }
        case Polarity::Negative:
        {
            if (seenNegative.contains(ty))
                return true;

            seenNegative.insert(ty);
            return false;
        }
        case Polarity::Mixed:
        {
            if (seenPositive.contains(ty) && seenNegative.contains(ty))
                return true;

            seenPositive.insert(ty);
            seenNegative.insert(ty);
            return false;
        }
        default:
            LUAU_ASSERT(!"Unreachable");
        }

        return false;
    }

    DenseHashMap<const void*, size_t> negativeTypes{0};
    DenseHashMap<const void*, size_t> positiveTypes{0};

    InsertionOrderedMap<TypeId, GeneralizationParams<TypeId>> types;
    InsertionOrderedMap<TypePackId, GeneralizationParams<TypePackId>> typePacks;

    OrderedSet<TypeId> unsealedTables;

    bool visit(TypeId ty) override
    {
        if (cachedTypes->contains(ty) || seenWithCurrentPolarity(ty))
            return false;

        LUAU_ASSERT(ty);
        return true;
    }

    bool visit(TypeId ty, const FreeType& ft) override
    {
        if (!subsumes(scope, ft.scope))
            return true;

        GeneralizationParams<TypeId>& params = types[ty];
        ++params.useCount;

        if (cachedTypes->contains(ty) || seenWithCurrentPolarity(ty))
            return false;

        if (!isWithinFunction)
            params.foundOutsideFunctions = true;

        params.polarity |= polarity;

        return true;
    }

    bool visit(TypeId ty, const TableType& tt) override
    {
        if (cachedTypes->contains(ty) || seenWithCurrentPolarity(ty))
            return false;

        if ((tt.state == TableState::Free || tt.state == TableState::Unsealed) && subsumes(scope, tt.scope))
            unsealedTables.insert(ty);

        for (const auto& [_name, prop] : tt.props)
        {
            if (prop.isReadOnly())
            {
                traverse(*prop.readTy);
            }
            else if (prop.isWriteOnly())
            {
                Polarity p = polarity;
                polarity = Polarity::Negative;
                traverse(*prop.writeTy);
                polarity = p;
            }
            else if (prop.isShared())
            {
                Polarity p = polarity;
                polarity = Polarity::Mixed;
                traverse(*prop.readTy);
                polarity = p;
            }
            else
            {
                LUAU_ASSERT(prop.isReadWrite() && !prop.isShared());

                traverse(*prop.readTy);
                Polarity p = polarity;
                polarity = Polarity::Negative;
                traverse(*prop.writeTy);
                polarity = p;
            }
        }

        if (tt.indexer)
        {
            // {[K]: V} is equivalent to three functions: get, set, and iterate
            //
            // (K) -> V
            // (K, V) -> ()
            // () -> {K}
            //
            // K and V therefore both have mixed polarity.

            const Polarity p = polarity;
            polarity = Polarity::Mixed;
            traverse(tt.indexer->indexType);
            traverse(tt.indexer->indexResultType);
            polarity = p;
        }

        return false;
    }

    bool visit(TypeId ty, const FunctionType& ft) override
    {
        if (cachedTypes->contains(ty) || seenWithCurrentPolarity(ty))
            return false;

        const bool oldValue = isWithinFunction;
        isWithinFunction = true;

        flip();
        traverse(ft.argTypes);
        flip();

        traverse(ft.retTypes);

        isWithinFunction = oldValue;

        return false;
    }

    bool visit(TypeId, const ExternType&) override
    {
        return false;
    }

    bool visit(TypePackId tp, const FreeTypePack& ftp) override
    {
        if (seenWithCurrentPolarity(tp))
            return false;

        if (!subsumes(scope, ftp.scope))
            return true;

        GeneralizationParams<TypePackId>& params = typePacks[tp];
        ++params.useCount;

        if (!isWithinFunction)
            params.foundOutsideFunctions = true;

        params.polarity |= polarity;

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
        : TypeOnceVisitor("TypeCacher", /* skipBoundTypes */ FFlag::LuauReduceSetTypeStackPressure)
        , cachedTypes(cachedTypes)
    {
    }

    void cache(TypeId ty) const
    {
        if (FFlag::LuauReduceSetTypeStackPressure)
            cachedTypes->insert(follow(ty));
        else
            cachedTypes->insert(ty);
    }

    bool isCached(TypeId ty) const
    {
        if (FFlag::LuauReduceSetTypeStackPressure)
            return cachedTypes->contains(follow(ty));

        return cachedTypes->contains(ty);
    }

    void markUncacheable(TypeId ty)
    {
        if (FFlag::LuauReduceSetTypeStackPressure)
            uncacheable.insert(follow(ty));
        else
            uncacheable.insert(ty);
    }

    void markUncacheable(TypePackId tp)
    {
        if (FFlag::LuauReduceSetTypeStackPressure)
            uncacheablePacks.insert(follow(tp));
        else
            uncacheablePacks.insert(tp);
    }

    bool isUncacheable(TypeId ty) const
    {
        if (FFlag::LuauReduceSetTypeStackPressure)
            return uncacheable.contains(follow(ty));

        return uncacheable.contains(ty);
    }

    bool isUncacheable(TypePackId tp) const
    {
        if (FFlag::LuauReduceSetTypeStackPressure)
            return uncacheablePacks.contains(follow(tp));

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
        LUAU_ASSERT(!FFlag::LuauReduceSetTypeStackPressure);
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

    bool visit(TypeId ty, const ExternType&) override
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

    bool visit(TypePackId tp, const BoundTypePack& btp) override
    {
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

namespace
{

struct TypeRemover
{
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<TypeArena> arena;

    TypeId needle;
    DenseHashSet<TypeId> seen{nullptr};

    void process(TypeId item)
    {
        item = follow(item);

        // If we've already visited this item, or it's outside our arena, then
        // do not try to mutate it.
        if (seen.contains(item) || item->owningArena != arena || item->persistent)
            return;
        seen.insert(item);

        if (auto ut = getMutable<UnionType>(item))
        {
            TypeIds newOptions;
            for (TypeId option : ut->options)
            {
                process(option);
                option = follow(option);
                if (option != needle && !is<NeverType>(option) && option != item)
                    newOptions.insert(option);
            }
            if (ut->options.size() != newOptions.size())
            {
                if (newOptions.empty())
                    emplaceType<BoundType>(asMutable(item), builtinTypes->neverType);
                else if (newOptions.size() == 1)
                    emplaceType<BoundType>(asMutable(item), *newOptions.begin());
                else
                    emplaceType<BoundType>(asMutable(item), arena->addType(UnionType{newOptions.take()}));
            }
        }
        else if (auto it = getMutable<IntersectionType>(item))
        {
            TypeIds newParts;
            for (TypeId part : it->parts)
            {
                process(part);
                part = follow(part);
                if (part != needle && !is<UnknownType>(part) && part != item)
                    newParts.insert(part);
            }
            if (it->parts.size() != newParts.size())
            {
                if (newParts.empty())
                    emplaceType<BoundType>(asMutable(item), builtinTypes->unknownType);
                else if (newParts.size() == 1)
                    emplaceType<BoundType>(asMutable(item), *newParts.begin());
                else
                    emplaceType<BoundType>(asMutable(item), arena->addType(IntersectionType{newParts.take()}));
            }
        }
    }
};

void removeType(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, TypeId haystack, TypeId needle)
{
    TypeRemover tr{builtinTypes, arena, needle};
    tr.process(haystack);
}

} // namespace

GeneralizationResult<TypeId> generalizeType(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    TypeId freeTy,
    const GeneralizationParams<TypeId>& params
)
{
    freeTy = follow(freeTy);

    FreeType* ft = getMutable<FreeType>(freeTy);
    LUAU_ASSERT(ft);

    LUAU_ASSERT(isKnown(params.polarity));

    const bool hasLowerBound = !get<NeverType>(follow(ft->lowerBound));
    const bool hasUpperBound = !get<UnknownType>(follow(ft->upperBound));

    const bool isWithinFunction = !params.foundOutsideFunctions;

    if (!hasLowerBound && !hasUpperBound)
    {
        if (!isWithinFunction)
            emplaceType<BoundType>(asMutable(freeTy), builtinTypes->unknownType);
        else
        {
            emplaceType<GenericType>(asMutable(freeTy), scope, params.polarity);
            return {freeTy, /*wasReplacedByGeneric*/ true};
        }
    }
    // It is possible that this free type has other free types in its upper
    // or lower bounds.  If this is the case, we must replace those
    // references with never (for the lower bound) or unknown (for the upper
    // bound).
    //
    // If we do not do this, we get tautological bounds like a <: a <: unknown.
    else if (isPositive(params.polarity) && !hasUpperBound)
    {
        TypeId lb = follow(ft->lowerBound);
        if (FreeType* lowerFree = getMutable<FreeType>(lb); lowerFree && lowerFree->upperBound == freeTy)
            lowerFree->upperBound = builtinTypes->unknownType;
        else
            removeType(arena, builtinTypes, lb, freeTy);

        if (follow(lb) != freeTy)
            emplaceType<BoundType>(asMutable(freeTy), lb);
        else if (!isWithinFunction)
            emplaceType<BoundType>(asMutable(freeTy), builtinTypes->unknownType);
        else
        {
            // if the lower bound is the type in question (eg 'a <: 'a), we don't actually have a lower bound.
            emplaceType<GenericType>(asMutable(freeTy), scope, params.polarity);
            return {freeTy, /*wasReplacedByGeneric*/ true};
        }
    }
    else
    {
        TypeId ub = follow(ft->upperBound);
        if (FreeType* upperFree = getMutable<FreeType>(ub); upperFree && upperFree->lowerBound == freeTy)
            upperFree->lowerBound = builtinTypes->neverType;
        else
            removeType(arena, builtinTypes, ub, freeTy);

        if (follow(ub) != freeTy)
            emplaceType<BoundType>(asMutable(freeTy), ub);
        else if (!isWithinFunction || params.useCount == 1)
        {
            // If we have some free type:
            //
            //  A <: 'b < C
            //
            // We can approximately generalize this to the intersection of its
            // bounds, taking care to avoid constructing a degenerate
            // union or intersection by clipping the free type from the upper
            // and lower bounds, then also cleaning the resulting intersection.
            removeType(arena, builtinTypes, ft->lowerBound, freeTy);
            TypeId cleanedTy = arena->addType(IntersectionType{{ft->lowerBound, ub}});
            removeType(arena, builtinTypes, cleanedTy, freeTy);
            emplaceType<BoundType>(asMutable(freeTy), cleanedTy);
        }
        else
        {
            // if the upper bound is the type in question, we don't actually have an upper bound.
            emplaceType<GenericType>(asMutable(freeTy), scope, params.polarity);
            return {freeTy, /*wasReplacedByGeneric*/ true};
        }
    }

    return {freeTy, /*wasReplacedByGeneric*/ false};
}

GeneralizationResult<TypePackId> generalizeTypePack(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    TypePackId tp,
    const GeneralizationParams<TypePackId>& params
)
{
    tp = follow(tp);

    if (tp->owningArena != arena)
        return {tp, /*wasReplacedByGeneric*/ false};

    const FreeTypePack* ftp = get<FreeTypePack>(tp);
    if (!ftp)
        return {tp, /*wasReplacedByGeneric*/ false};

    if (!subsumes(scope, ftp->scope))
        return {tp, /*wasReplacedByGeneric*/ false};

    if (1 == params.useCount)
        emplaceTypePack<BoundTypePack>(asMutable(tp), builtinTypes->unknownTypePack);
    else
    {
        emplaceTypePack<GenericTypePack>(asMutable(tp), scope, params.polarity);
        return {tp, /*wasReplacedByGeneric*/ true};
    }

    return {tp, /*wasReplacedByGeneric*/ false};
}

void sealTable(NotNull<Scope> scope, TypeId ty)
{
    TableType* tableTy = getMutable<TableType>(follow(ty));
    if (!tableTy)
        return;

    if (!subsumes(scope, tableTy->scope))
        return;

    if (tableTy->state == TableState::Unsealed || tableTy->state == TableState::Free)
        tableTy->state = TableState::Sealed;
}

std::optional<TypeId> generalize(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    NotNull<DenseHashSet<TypeId>> cachedTypes,
    TypeId ty,
    std::optional<TypeId> generalizationTarget
)
{
    ty = follow(ty);

    if (ty->owningArena != arena || ty->persistent)
        return ty;

    FreeTypeSearcher fts{scope, cachedTypes};
    fts.traverse(ty);

    FunctionType* functionTy = getMutable<FunctionType>(ty);
    auto pushGeneric = [&](TypeId t)
    {
        if (functionTy)
            functionTy->generics.push_back(t);
    };

    auto pushGenericPack = [&](TypePackId tp)
    {
        if (functionTy)
            functionTy->genericPacks.push_back(tp);
    };

    for (const auto& [freeTy, params] : fts.types)
    {
        if (!generalizationTarget || freeTy == *generalizationTarget)
        {
            GeneralizationResult<TypeId> res = generalizeType(arena, builtinTypes, scope, freeTy, params);
            if (res.resourceLimitsExceeded)
                return std::nullopt;

            if (res && res.wasReplacedByGeneric)
                pushGeneric(*res.result);
        }
    }

    for (TypeId unsealedTableTy : fts.unsealedTables)
    {
        if (!generalizationTarget || unsealedTableTy == *generalizationTarget)
            sealTable(scope, unsealedTableTy);
    }

    for (const auto& [freePackId, params] : fts.typePacks)
    {
        TypePackId freePack = follow(freePackId);
        if (!generalizationTarget)
        {
            GeneralizationResult<TypePackId> generalizedTp = generalizeTypePack(arena, builtinTypes, scope, freePack, params);

            if (generalizedTp.resourceLimitsExceeded)
                return std::nullopt;

            if (generalizedTp && generalizedTp.wasReplacedByGeneric)
                pushGenericPack(freePack);
        }
    }

    TypeCacher cacher{cachedTypes};
    cacher.traverse(ty);

    return ty;
}

struct GenericCounter : TypeVisitor
{
    struct CounterState
    {
        size_t count = 0;
        Polarity polarity = Polarity::None;
    };

    // This traversal does need to walk into types multiple times because we
    // care about generics that are only refererd to once. If a type is present
    // more than once, however, we don't care exactly how many times, so we also
    // track counts in our "seen set."
    DenseHashMap<TypeId, size_t> seenCounts{nullptr};

    NotNull<DenseHashSet<TypeId>> cachedTypes;
    DenseHashMap<TypeId, CounterState> generics{nullptr};
    DenseHashMap<TypePackId, CounterState> genericPacks{nullptr};

    Polarity polarity = Polarity::Positive;

    int depth = 0;
    bool hitLimits = false;

    explicit GenericCounter(NotNull<DenseHashSet<TypeId>> cachedTypes)
        : TypeVisitor("GenericCounter", FFlag::LuauExplicitSkipBoundTypes)
        , cachedTypes(cachedTypes)
    {
    }

    void checkLimits()
    {
        if (FFlag::LuauReduceSetTypeStackPressure && depth > FInt::LuauGenericCounterMaxDepth)
            hitLimits = true;
    }

    bool visit(TypeId ty) override
    {
        checkLimits();
        return !FFlag::LuauReduceSetTypeStackPressure || !hitLimits;
    }


    bool visit(TypeId ty, const FunctionType& ft) override
    {
        std::optional<RecursionCounter> rc{std::nullopt};
        if (FFlag::LuauReduceSetTypeStackPressure)
        {
            rc.emplace(&depth);
            checkLimits();
        }

        if (ty->persistent)
            return false;

        size_t& seenCount = seenCounts[ty];
        if (seenCount > 1)
            return false;

        ++seenCount;

        polarity = invert(polarity);
        traverse(ft.argTypes);
        polarity = invert(polarity);
        traverse(ft.retTypes);

        return false;
    }

    bool visit(TypeId ty, const TableType& tt) override
    {
        std::optional<RecursionCounter> rc{std::nullopt};
        if (FFlag::LuauReduceSetTypeStackPressure)
        {
            rc.emplace(&depth);
            checkLimits();
        }

        if (ty->persistent)
            return false;

        size_t& seenCount = seenCounts[ty];
        if (seenCount > 1)
            return false;
        ++seenCount;

        const Polarity previous = polarity;

        for (const auto& [_name, prop] : tt.props)
        {
            if (prop.isReadOnly())
            {
                traverse(*prop.readTy);
            }
            else if (prop.isWriteOnly())
            {
                Polarity p = polarity;
                polarity = Polarity::Negative;
                traverse(*prop.writeTy);
                polarity = p;
            }
            else if (prop.isShared())
            {
                Polarity p = polarity;
                polarity = Polarity::Mixed;
                traverse(*prop.readTy);
                polarity = p;
            }
            else
            {
                LUAU_ASSERT(prop.isReadWrite() && !prop.isShared());

                traverse(*prop.readTy);
                Polarity p = polarity;
                polarity = Polarity::Negative;
                traverse(*prop.writeTy);
                polarity = p;
            }
        }

        if (tt.indexer)
        {
            polarity = Polarity::Mixed;
            traverse(tt.indexer->indexType);
            traverse(tt.indexer->indexResultType);
            polarity = previous;
        }

        return false;
    }

    bool visit(TypeId ty, const ExternType&) override
    {
        return false;
    }

    bool visit(TypeId ty, const GenericType&) override
    {
        auto state = generics.find(ty);
        if (state)
        {
            ++state->count;
            state->polarity |= polarity;
        }

        return false;
    }

    bool visit(TypePackId tp, const GenericTypePack&) override
    {
        auto state = genericPacks.find(tp);
        if (state)
        {
            ++state->count;
            state->polarity |= polarity;
        }

        return false;
    }
};

void pruneUnnecessaryGenerics(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    NotNull<DenseHashSet<TypeId>> cachedTypes,
    TypeId ty
)
{
    ty = follow(ty);

    if (ty->owningArena != arena || ty->persistent)
        return;

    FunctionType* functionTy = getMutable<FunctionType>(ty);

    if (!functionTy)
        return;

    // If a generic has no explicit name and is only referred to in one place in
    // the function's signature, it can be replaced with unknown.

    GenericCounter counter{cachedTypes};
    for (TypeId generic : functionTy->generics)
    {
        generic = follow(generic);
        auto g = get<GenericType>(generic);
        if (g && !g->explicitName)
            counter.generics[generic] = {};
    }

    // It is sometimes the case that a pack in the generic list will become a
    // pack that (transitively) has a generic tail.  If it does, we need to add
    // that generic tail to the generic pack list.
    for (size_t i = 0; i < functionTy->genericPacks.size(); ++i)
    {
        TypePackId genericPack = follow(functionTy->genericPacks[i]);

        TypePackId tail = getTail(genericPack);

        if (tail != genericPack)
            functionTy->genericPacks.push_back(tail);

        if (auto g = get<GenericTypePack>(tail); g && !g->explicitName)
            counter.genericPacks[genericPack] = {};
    }

    counter.traverse(ty);

    if (!FFlag::LuauReduceSetTypeStackPressure || !counter.hitLimits)
    {
        for (const auto& [generic, state] : counter.generics)
        {
            if (state.count == 1 && state.polarity != Polarity::Mixed)
            {
                if (arena.get() != generic->owningArena)
                    continue;
                emplaceType<BoundType>(asMutable(generic), builtinTypes->unknownType);
            }
        }
    }

    // Remove duplicates and types that aren't actually generics.
    DenseHashSet<TypeId> seen{nullptr};
    auto it = std::remove_if(
        functionTy->generics.begin(),
        functionTy->generics.end(),
        [&](TypeId ty)
        {
            ty = follow(ty);
            if (seen.contains(ty))
                return true;
            seen.insert(ty);

            if (!FFlag::LuauReduceSetTypeStackPressure || !counter.hitLimits)
            {
                auto state = counter.generics.find(ty);
                if (state && state->count == 0)
                    return true;
            }

            return !get<GenericType>(ty);
        }
    );

    functionTy->generics.erase(it, functionTy->generics.end());


    if (!FFlag::LuauReduceSetTypeStackPressure || !counter.hitLimits)
    {
        for (const auto& [genericPack, state] : counter.genericPacks)
        {
            if (state.count == 1)
                emplaceTypePack<BoundTypePack>(asMutable(genericPack), builtinTypes->unknownTypePack);
        }
    }


    DenseHashSet<TypePackId> seen2{nullptr};
    auto it2 = std::remove_if(
        functionTy->genericPacks.begin(),
        functionTy->genericPacks.end(),
        [&](TypePackId tp)
        {
            tp = follow(tp);
            if (seen2.contains(tp))
                return true;
            seen2.insert(tp);

            if (!FFlag::LuauReduceSetTypeStackPressure || !counter.hitLimits)
            {
                auto state = counter.genericPacks.find(tp);
                if (state && state->count == 0)
                    return true;
            }

            return !get<GenericTypePack>(tp);
        }
    );

    functionTy->genericPacks.erase(it2, functionTy->genericPacks.end());
}

} // namespace Luau
