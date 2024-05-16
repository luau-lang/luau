// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Generalization.h"

#include "Luau/Scope.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypePack.h"
#include "Luau/VisitType.h"

namespace Luau
{

struct MutatingGeneralizer : TypeOnceVisitor
{
    NotNull<BuiltinTypes> builtinTypes;

    NotNull<Scope> scope;
    DenseHashMap<const void*, size_t> positiveTypes;
    DenseHashMap<const void*, size_t> negativeTypes;
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;

    bool isWithinFunction = false;

    MutatingGeneralizer(NotNull<BuiltinTypes> builtinTypes, NotNull<Scope> scope, DenseHashMap<const void*, size_t> positiveTypes,
                        DenseHashMap<const void*, size_t> negativeTypes)
        : TypeOnceVisitor(/* skipBoundTypes */ true)
        , builtinTypes(builtinTypes)
        , scope(scope)
        , positiveTypes(std::move(positiveTypes))
        , negativeTypes(std::move(negativeTypes))
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
        const bool oldValue = isWithinFunction;

        isWithinFunction = true;

        traverse(ft.argTypes);
        traverse(ft.retTypes);

        isWithinFunction = oldValue;

        return false;
    }

    bool visit(TypeId ty, const FreeType&) override
    {
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
        const size_t positiveCount = getCount(positiveTypes, ty);
        const size_t negativeCount = getCount(negativeTypes, ty);

        // FIXME: Free tables should probably just be replaced by upper bounds on free types.
        //
        // eg never <: 'a <: {x: number} & {z: boolean}

        if (!positiveCount && !negativeCount)
            return true;

        TableType* tt = getMutable<TableType>(ty);
        LUAU_ASSERT(tt);

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

    explicit FreeTypeSearcher(NotNull<Scope> scope)
        : TypeVisitor(/*skipBoundTypes*/ true)
        , scope(scope)
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
        if (seenWithPolarity(ty))
            return false;

        LUAU_ASSERT(ty);
        return true;
    }

    bool visit(TypeId ty, const FreeType& ft) override
    {
        if (seenWithPolarity(ty))
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
        if (seenWithPolarity(ty))
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
                LUAU_ASSERT(prop.isShared());

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
        if (seenWithPolarity(ty))
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


std::optional<TypeId> generalize(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<Scope> scope, TypeId ty)
{
    ty = follow(ty);

    if (ty->owningArena != arena || ty->persistent)
        return ty;

    if (const FunctionType* ft = get<FunctionType>(ty); ft && (!ft->generics.empty() || !ft->genericPacks.empty()))
        return ty;

    FreeTypeSearcher fts{scope};
    fts.traverse(ty);

    MutatingGeneralizer gen{builtinTypes, scope, std::move(fts.positiveTypes), std::move(fts.negativeTypes)};

    gen.traverse(ty);

    /* MutatingGeneralizer mutates types in place, so it is possible that ty has
        * been transmuted to a BoundType. We must follow it again and verify that
        * we are allowed to mutate it before we attach generics to it.
        */
    ty = follow(ty);

    if (ty->owningArena != arena || ty->persistent)
        return ty;

    FunctionType* ftv = getMutable<FunctionType>(ty);
    if (ftv)
    {
        ftv->generics = std::move(gen.generics);
        ftv->genericPacks = std::move(gen.genericPacks);
    }

    return ty;
}

} // namespace Luau
