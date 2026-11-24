// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Generalization.h"

#include "Luau/Common.h"
#include "Luau/DenseHash.h"
#include "Luau/InsertionOrderedMap.h"
#include "Luau/IterativeTypeVisitor.h"
#include "Luau/OrderedSet.h"
#include "Luau/Polarity.h"
#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeIds.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"

namespace Luau
{

namespace
{

struct TypeRemover
{
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<TypeArena> arena;

    TypeId needle;
    DenseHashSet<TypeId> seen;

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

struct FreeTypeFinder : IterativeTypeVisitor
{
    NotNull<TypeArena> arena;
    TypeIds freeTys;

    explicit FreeTypeFinder(NotNull<TypeArena> arena)
        : IterativeTypeVisitor("FreeTypeFinder", /* visitOnce */ true, /*skipBoundTypes*/ true)
        , arena(arena)
    {
    }

    bool visit(TypeId ty, const FreeType&) override
    {
        if (ty->owningArena != arena)
            return false;

        freeTys.insert(ty);
        return true;
    }

    bool visit(TypeId ty, const TableType&) override
    {
        return false;
    }

    bool visit(TypeId ty, const MetatableType&) override
    {
        return false;
    }

    bool visit(TypeId ty, const FunctionType&) override
    {
        return false;
    }

    bool visit(TypeId ty, const ExternType&) override
    {
        return false;
    }
};

TypeId getDirectFreeNeighbor(TypeId ty)
{
    ty = follow(ty);
    if (get<FreeType>(ty))
        return ty;
    return nullptr;
}

void collapseInvariantFreeType(NotNull<TypeArena> arena, TypeId ty)
{
    FreeTypeFinder ftf{arena};
    ftf.run(ty);

    for (TypeId t : ftf.freeTys)
    {
        const FreeType* ft = get<FreeType>(t);
        LUAU_ASSERT(ft);

        auto ub = follow(ft->upperBound);
        auto lb = follow(ft->lowerBound);
        if (ub == lb && ub != t)
            emplaceType<BoundType>(asMutable(t), ub);
    }
}


// Walk direct free-type bounds starting from `startTy`.  If a cycle is
// reachable, collapse every member into one representative free type whose
// bounds are the union of every member's external lower bound and the
// intersection of every member's external upper bound.  Cycle self-references
// (whether direct or nested in unions/intersections) are stripped from those
// bounds.
//
// A "direct bound" means A.lowerBound or A.upperBound IS another free type
// (not nested inside a union/intersection/table/function).  Cycles formed by
// direct bounds (A->B->...->A) are the only cycles this helper detects;
// non-cycling chains are left alone.
//
// Returns true if any types were collapsed.
bool collapseDirectBoundCycleAt(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, TypeId startTy)
{
    collapseInvariantFreeType(arena, startTy);

    startTy = follow(startTy);

    if (!get<FreeType>(startTy))
        return false;

    TypeIds path;
    TypeId cur = startTy;

    while (cur)
    {
        if (path.contains(cur))
        {
            // Collect cycle members: everything on the path from `cur` onward.
            TypeIds cycleMembers;
            bool inCycle = false;
            for (TypeId member : path)
            {
                if (member == cur)
                    inCycle = true;
                if (inCycle)
                    cycleMembers.insert(member);
            }
            LUAU_ASSERT(!cycleMembers.empty());

            // Merge external bounds from every cycle member into the
            // representative.  Lower bounds union; upper bounds intersect.
            // Skip bounds that are entirely cycle self-refs.
            UnionBuilder mergedLowers{arena, builtinTypes};
            IntersectionBuilder mergedUppers{arena, builtinTypes};

            for (TypeId m : cycleMembers)
            {
                FreeType* ft = getMutable<FreeType>(m);
                if (!ft)
                    continue;

                TypeId lb = follow(ft->lowerBound);
                if (!cycleMembers.contains(lb))
                {
                    for (TypeId other : cycleMembers)
                        removeType(arena, builtinTypes, lb, other);
                    lb = follow(lb);
                    if (!get<NeverType>(lb) && !cycleMembers.contains(lb))
                        mergedLowers.add(lb);
                }

                TypeId ub = follow(ft->upperBound);
                if (!cycleMembers.contains(ub))
                {
                    for (TypeId other : cycleMembers)
                        removeType(arena, builtinTypes, ub, other);
                    ub = follow(ub);
                    if (!get<UnknownType>(ub) && !cycleMembers.contains(ub))
                        mergedUppers.add(ub);
                }
            }

            // Pick the back-edge target (cycleMembers[0] = `cur`) as the
            // representative, give it the merged bounds, and bind the rest.
            TypeId rep = cycleMembers.front();
            FreeType* repFree = getMutable<FreeType>(rep);
            LUAU_ASSERT(repFree);

            repFree->lowerBound = mergedLowers.build();
            repFree->upperBound = mergedUppers.build();

            auto it = cycleMembers.begin() + 1;
            while (it != cycleMembers.end())
            {
                emplaceType<BoundType>(asMutable(*it), rep);
                ++it;
            }

            return true;
        }

        path.insert(cur);

        FreeType* ft = getMutable<FreeType>(cur);
        if (!ft)
            break;

        // Try to follow a direct free-type bound (prefer upper, then lower).
        TypeId next = getDirectFreeNeighbor(ft->upperBound);
        if (!next || next == cur)
            next = getDirectFreeNeighbor(ft->lowerBound);
        if (next == cur)
            next = nullptr;

        cur = next;
    }

    return false;
}

// Batch pre-pass: collapse direct-bound cycles among the free types in the
// generalization frontier.  Iteration order does not matter -- once a cycle
// has been collapsed, subsequent walks from any member terminate immediately
// because the type is no longer free (it has been bound to the rep) or
// because the rep's bounds no longer reference cycle members.
void collapseFreeTypeCycles(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    const InsertionOrderedMap<TypeId, GeneralizationParams<TypeId>>& freeTypes
)
{
    for (const auto& [startTy, _] : freeTypes)
        collapseDirectBoundCycleAt(arena, builtinTypes, startTy);
}

/**
 * Iterate over all reachable types from a root to determine the polarity of
 * any visited free types or generic types.
 *
 * - This implements a BFS that visits each node at most once: from a negative
 *   polarity and from a positive polarity. Mixed polarity is optimized a bit
 *   by emitting a single traversal.
 * - To limit search scope, we stop at any arena boundaries: intuitively, we
 *   should not depend on types outside of our arena to determine free types or
 *   generic polarity, at least not during generalization.
 */
struct FindTypesWithPolarity
{
    NotNull<Scope> scope;
    NotNull<TypeArena> targetArena;

    explicit FindTypesWithPolarity(NotNull<TypeArena> targetArena, NotNull<Scope> scope)
        : scope(scope)
        , targetArena(targetArena)
    {
    }

    DenseHashSet<const void*> seenPositive;
    DenseHashSet<const void*> seenNegative;

    InsertionOrderedMap<TypeId, GeneralizationParams<TypeId>> types;
    InsertionOrderedMap<TypePackId, GeneralizationParams<TypePackId>> typePacks;

    OrderedSet<TypeId> unsealedTables;

    struct WorkItem
    {
        Luau::Variant<TypeId, TypePackId> value;
        Polarity polarity;
        bool isWithinFunction;
    };

    std::vector<WorkItem> work;

    bool seenWithPolarity(const void* ty, Polarity polarity) const
    {
        switch (polarity)
        {
        case Polarity::Positive:
            return seenPositive.contains(ty);
        case Polarity::Negative:
            return seenNegative.contains(ty);
        case Polarity::Mixed:
            return seenPositive.contains(ty) && seenNegative.contains(ty);
        case Polarity::None:
        case Polarity::Unknown:
        default:
            LUAU_ASSERT(!"Unreachable");
        }
        // This should only occur if we hit the "unreachable" branch above.
        return true;
    }

    void observe(const void* ty, Polarity polarity)
    {
        if (isPositive(polarity))
            seenPositive.insert(ty);

        if (isNegative(polarity))
            seenNegative.insert(ty);
    }

    void push(TypePackId tp, Polarity polarity, bool isWithinFunction)
    {
        tp = follow(tp);
        // We should not be traversing across types that we do not own to determine polarity.
        if (!seenWithPolarity(tp, polarity) && tp->owningArena == targetArena)
        {
            observe(tp, polarity);
            work.push_back({tp, polarity, isWithinFunction});
        }
    }

    void push(TypeId ty, Polarity polarity, bool isWithinFunction)
    {
        ty = follow(ty);
        // We should not be traversing across types that we do not own to determine polarity.
        if (!seenWithPolarity(ty, polarity) && ty->owningArena == targetArena)
        {
            observe(ty, polarity);
            work.push_back({ty, polarity, isWithinFunction});
        }
    }

    void stepType(TypeId ty, Polarity polarity, bool isWithinFunction)
    {
        LUAU_ASSERT(!is<BoundType>(ty));

        if (auto ft = get<FreeType>(ty))
        {
            if (subsumes(scope, ft->scope))
            {
                GeneralizationParams<TypeId>& params = types[ty];
                params.useCount++;
                if (!isWithinFunction)
                    params.foundOutsideFunctions = true;

                params.polarity |= polarity;
            }

            push(ft->lowerBound, polarity, isWithinFunction);
            push(ft->upperBound, polarity, isWithinFunction);
        }
        else if (get<GenericType>(ty))
        {
            GeneralizationParams<TypeId>& params = types[ty];
            params.useCount++;
            params.polarity |= polarity;
        }
        else if (auto funTy = get<FunctionType>(ty))
        {
            push(funTy->argTypes, invert(polarity), true);
            push(funTy->retTypes, polarity, true);
        }
        else if (auto tt = get<TableType>(ty))
        {
            if ((tt->state == TableState::Free || tt->state == TableState::Unsealed) && subsumes(scope, tt->scope))
                unsealedTables.insert(ty);

            if (tt->indexer)
            {
                Polarity p = tt->indexer->isReadOnly ? polarity : Polarity::Mixed;
                push(tt->indexer->indexResultType, p, isWithinFunction);
                push(tt->indexer->indexType, p, isWithinFunction);
            }

            for (const auto& [_, prop] : tt->props)
            {
                if (prop.isShared())
                    push(*prop.readTy, Polarity::Mixed, isWithinFunction);
                else
                {
                    if (prop.readTy)
                        push(*prop.readTy, polarity, isWithinFunction);
                    if (prop.writeTy)
                        push(*prop.writeTy, invert(polarity), isWithinFunction);
                }
            }
        }
        else if (auto lt = get<LazyType>(ty))
        {
            if (TypeId unwrapped = lt->unwrapped)
                push(unwrapped, polarity, isWithinFunction);
        }
        else if (auto mt = get<MetatableType>(ty))
        {
            push(mt->metatable, polarity, isWithinFunction);
            push(mt->table, polarity, isWithinFunction);
        }
        else if (auto utv = get<UnionType>(ty))
        {
            for (auto option : utv)
                push(option, polarity, isWithinFunction);
        }
        else if (auto itv = get<IntersectionType>(ty))
        {
            for (auto part : itv)
                push(part, polarity, isWithinFunction);
        }
        else if (auto nt = get<NegationType>(ty))
            push(nt->ty, polarity, isWithinFunction);
        else if (auto tfit = get<TypeFunctionInstanceType>(ty))
        {
            for (auto typeArg : tfit->typeArguments)
                push(typeArg, polarity, isWithinFunction);

            for (auto packArg : tfit->packArguments)
                push(packArg, polarity, isWithinFunction);
        }
        else
        {
            LUAU_ASSERT(
                (is<PendingExpansionType,
                    BlockedType,
                    ErrorType,
                    PrimitiveType,
                    SingletonType,
                    AnyType,
                    NoRefineType,
                    UnknownType,
                    NeverType,
                    ExternType>(ty))
            );
        }
    }

    void stepTypePack(TypePackId tp, Polarity polarity, bool isWithinFunction)
    {
        LUAU_ASSERT(!is<BoundTypePack>(tp));

        if (auto ftp = get<FreeTypePack>(tp))
        {
            if (subsumes(scope, ftp->scope))
            {
                GeneralizationParams<TypePackId>& params = typePacks[tp];
                params.useCount++;

                if (!isWithinFunction)
                    params.foundOutsideFunctions = true;

                params.polarity |= polarity;
            }
        }
        else if (auto pack = get<TypePack>(tp))
        {
            for (auto hd : pack->head)
                push(hd, polarity, isWithinFunction);

            if (pack->tail)
                push(*pack->tail, polarity, isWithinFunction);
        }
        else if (auto vtp = get<VariadicTypePack>(tp))
        {
            push(vtp->ty, polarity, isWithinFunction);
        }
        else if (auto tfit = get<TypeFunctionInstanceTypePack>(tp))
        {
            for (auto typeArg : tfit->typeArguments)
                push(typeArg, polarity, isWithinFunction);

            for (auto packArg : tfit->packArguments)
                push(packArg, polarity, isWithinFunction);
        }
        else if (get<GenericTypePack>(tp))
        {
            GeneralizationParams<TypePackId>& params = typePacks[tp];
            params.useCount++;
            params.polarity |= polarity;
        }
        else
        {
            LUAU_ASSERT((is<BlockedTypePack, ErrorTypePack>(tp)));
        }
    }

    void run(TypeId ty)
    {
        work.reserve(32);
        work.clear();
        seenNegative.clear();
        seenPositive.clear();
        types.clear();
        typePacks.clear();
        push(ty, Polarity::Positive, false);

        while (!work.empty())
        {
            WorkItem item = work.back();
            work.pop_back();
            visit(
                overloaded{
                    [&](TypeId ty)
                    {
                        stepType(ty, item.polarity, item.isWithinFunction);
                    },
                    [&](TypePackId tp)
                    {
                        stepTypePack(tp, item.polarity, item.isWithinFunction);
                    }
                },
                item.value
            );
        }
    }
};

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

    // Collapse any direct-bound cycle this free type participates in before we
    // commit to a generalization decision.  This handles the per-call
    // invocations from ConstraintSolver -- which bypass the batch pre-pass in
    // generalize() -- and is a no-op when the cycle has already been collapsed
    // by that pre-pass.  When this fires, freeTy may be re-bound to the
    // representative of the cycle, so we re-follow it.
    // Run this and unconditionally re-follow.
    collapseDirectBoundCycleAt(arena, builtinTypes, freeTy);
    freeTy = follow(freeTy);

    if (!get<FreeType>(freeTy))
        return {freeTy, /*wasReplacedByGeneric*/ false};

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
        // The pre-pass collapseDirectBoundCycleAt has already collapsed any
        // 2-cycle here, so there is no neighbor bound to forward -- just
        // strip the free type from the upper bound.
        removeType(arena, builtinTypes, ub, freeTy);

        if (follow(ub) != freeTy)
            emplaceType<BoundType>(asMutable(freeTy), ub);
        else if (!isWithinFunction || params.useCount == 1)
        {
            // If we have some free type:
            //
            //  A <: 'b <: C
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

    FindTypesWithPolarity ftwp{arena, scope};
    ftwp.run(ty);

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

    if (!generalizationTarget)
        collapseFreeTypeCycles(arena, builtinTypes, ftwp.types);

    auto generalizeJustOne = [&](TypeId freeTy, const auto& params)
    {
        if (!get<FreeType>(follow(freeTy)))
            return GeneralizationResult<TypeId>{};

        GeneralizationResult<TypeId> res = generalizeType(arena, builtinTypes, scope, freeTy, params);

        if (res.resourceLimitsExceeded)
            return res;

        if (res && res.wasReplacedByGeneric)
            pushGeneric(*res.result);

        return res;
    };

    if (generalizationTarget)
    {
        auto it = ftwp.types.find(*generalizationTarget);
        if (it != ftwp.types.end())
        {
            const auto [freeTy, params] = *it;
            auto res = generalizeJustOne(freeTy, params);
            if (res.resourceLimitsExceeded)
                return std::nullopt;
        }
    }
    else
    {
        for (const auto& [freeTy, params] : ftwp.types)
        {
            auto res = generalizeJustOne(freeTy, params);
            if (res.resourceLimitsExceeded)
                return std::nullopt;
        }
    }

    for (TypeId unsealedTableTy : ftwp.unsealedTables)
    {
        if (generalizationTarget && unsealedTableTy != *generalizationTarget)
            continue;

        sealTable(scope, unsealedTableTy);
    }

    for (const auto& [freePackId, params] : ftwp.typePacks)
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

    return ty;
}

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

    FindTypesWithPolarity ftwp{arena, scope};
    ftwp.run(ty);

    DenseHashSet<TypeId> seenTypes;
    DenseHashSet<TypePackId> seenPacks;

    // It is sometimes the case that a pack in the generic list will become a
    // pack that (transitively) has a generic tail.  If it does, we need to add
    // that generic tail to the generic pack list.
    for (size_t i = 0; i < functionTy->genericPacks.size(); ++i)
    {
        TypePackId genericPack = follow(functionTy->genericPacks[i]);

        TypePackId tail = getTail(genericPack);

        if (tail != genericPack)
            functionTy->genericPacks.push_back(tail);
    }

    for (auto it = functionTy->generics.begin(); it != functionTy->generics.end();)
    {
        TypeId generic = follow(*it);
        auto genericTy = getMutable<GenericType>(generic);

        if (!genericTy)
        {
            // This type is no longer generic, clip it.
            it = functionTy->generics.erase(it);
            continue;
        }

        if (seenTypes.contains(generic))
        {
            it = functionTy->generics.erase(it);
            continue;
        }

        if (arena.get() != generic->owningArena)
        {
            // This was a band-aid fix that we never actually followed up on.
            // We should never see generics from other arenas flowing
            // into the generics of a function we're trying to generalize.
            it++;
            continue;
        }

        auto entry = ftwp.types.find(generic);

        if (genericTy->explicitName)
        {
            // Well, the user said this should be generic, so it's generic.
            seenTypes.insert(generic);
            if (entry != ftwp.types.end())
                genericTy->polarity = entry->second.polarity;
            it++;
            continue;
        }

        if (entry == ftwp.types.end())
        {
            // We didn't see this generic ... at all ... so clip it.
            it = functionTy->generics.erase(it);
            continue;
        }

        switch (entry->second.polarity)
        {
        case Polarity::Positive:
            // If we've only seen this generic in a positive position, then we can clip it to `never`.
            emplaceType<BoundType>(asMutable(generic), builtinTypes->neverType);
            it = functionTy->generics.erase(it);
            break;

        case Polarity::Negative:
            // If we've only seen this generic in a negative position, then we can clip it to `unknown`.
            emplaceType<BoundType>(asMutable(generic), builtinTypes->unknownType);
            it = functionTy->generics.erase(it);
            break;

        case Polarity::Mixed:
            genericTy->polarity = Polarity::Mixed;
            seenTypes.insert(generic);
            it++;
            break;

        case Polarity::None:
        case Polarity::Unknown:
            // If we have an entry in `ftwp.types`, then we should have
            // recorded *a* polarity.
            LUAU_ASSERT(!"Unexpected polarity.");
            it++;
            break;
        }
    }

    for (auto it = functionTy->genericPacks.begin(); it != functionTy->genericPacks.end();)
    {
        TypePackId generic = follow(*it);

        auto genericTy = getMutable<GenericTypePack>(generic);

        if (!genericTy)
        {
            // This type is no longer generic, clip it.
            it = functionTy->genericPacks.erase(it);
            continue;
        }

        if (seenPacks.contains(generic))
        {
            it = functionTy->genericPacks.erase(it);
            continue;
        }

        if (arena.get() != generic->owningArena)
        {
            // This was a band-aid fix that we never actually followed up on.
            // We should never see generics from other arenas flowing
            // into the generics of a function we're trying to generalize.
            it++;
            continue;
        }

        auto entry = ftwp.typePacks.find(generic);

        if (genericTy->explicitName)
        {
            // Well, the user said this should be generic, so it's generic.
            seenPacks.insert(generic);
            if (entry != ftwp.typePacks.end())
                genericTy->polarity = entry->second.polarity;
            it++;
            continue;
        }

        if (entry == ftwp.typePacks.end())
        {
            // We didn't see this generic ... at all ... so clip it.
            it = functionTy->genericPacks.erase(it);
            continue;
        }

        switch (entry->second.polarity)
        {
        case Polarity::Positive:
        case Polarity::Negative:
            // We *could* use `never...` as the pack here, not much of a point.
            emplaceTypePack<BoundTypePack>(asMutable(generic), builtinTypes->unknownTypePack);
            it = functionTy->genericPacks.erase(it);
            break;

        case Polarity::Mixed:
            genericTy->polarity = Polarity::Mixed;
            seenPacks.insert(generic);
            it++;
            break;

        case Polarity::None:
        case Polarity::Unknown:
            LUAU_ASSERT(!"Unimplemented");
            it++;
            break;
        }
    }
}

} // namespace Luau
