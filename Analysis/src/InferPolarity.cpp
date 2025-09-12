// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/DenseHash.h"
#include "Luau/Polarity.h"
#include "Luau/Scope.h"
#include "Luau/VisitType.h"

LUAU_FASTFLAG(LuauExplicitSkipBoundTypes)

namespace Luau
{

struct InferPolarity : TypeVisitor
{
    NotNull<TypeArena> arena;
    NotNull<Scope> scope;

    DenseHashMap<TypeId, Polarity> types{nullptr};
    DenseHashMap<TypePackId, Polarity> packs{nullptr};

    Polarity polarity = Polarity::Positive;

    explicit InferPolarity(NotNull<TypeArena> arena, NotNull<Scope> scope)
        : TypeVisitor("InferPolarity", FFlag::LuauExplicitSkipBoundTypes)
        , arena(arena)
        , scope(scope)
    {
    }

    void flip()
    {
        polarity = invert(polarity);
    }

    bool visit(TypeId ty, const GenericType& gt) override
    {
        if (ty->owningArena != arena)
            return false;

        if (subsumes(scope, gt.scope))
            types[ty] |= polarity;

        return false;
    }

    bool visit(TypeId ty, const TableType& tt) override
    {
        if (ty->owningArena != arena)
            return false;

        const Polarity p = polarity;
        for (const auto& [name, prop] : tt.props)
        {
            if (prop.isShared())
            {
                polarity = Polarity::Mixed;
                traverse(*prop.readTy);
                continue;
            }

            if (prop.readTy)
            {
                polarity = p;
                traverse(*prop.readTy);
            }

            if (prop.writeTy)
            {
                polarity = invert(p);
                traverse(*prop.writeTy);
            }
        }

        if (tt.indexer)
        {
            polarity = Polarity::Mixed;
            traverse(tt.indexer->indexType);
            traverse(tt.indexer->indexResultType);
        }

        polarity = p;

        return false;
    }

    bool visit(TypeId ty, const FunctionType& ft) override
    {
        if (ty->owningArena != arena)
            return false;

        const Polarity p = polarity;

        polarity = Polarity::Positive;

        // If these types actually occur within the function signature, their
        // polarity will be overwritten. If not, we infer that they are phantom
        // types.
        for (TypeId generic : ft.generics)
        {
            generic = follow(generic);
            const auto gen = get<GenericType>(generic);
            if (gen && subsumes(scope, gen->scope))
                types[generic] = Polarity::None;
        }
        for (TypePackId genericPack : ft.genericPacks)
        {
            genericPack = follow(genericPack);
            const auto gen = get<GenericTypePack>(genericPack);
            if (gen && subsumes(scope, gen->scope))
                packs[genericPack] = Polarity::None;
        }

        flip();
        traverse(ft.argTypes);
        flip();
        traverse(ft.retTypes);

        polarity = p;

        return false;
    }

    bool visit(TypeId, const ExternType&) override
    {
        return false;
    }

    bool visit(TypePackId tp, const GenericTypePack& gtp) override
    {
        packs[tp] |= polarity;
        return false;
    }
};

template<typename TID>
static void inferGenericPolarities_(NotNull<TypeArena> arena, NotNull<Scope> scope, TID ty)
{
    InferPolarity infer{arena, scope};
    infer.traverse(ty);

    for (const auto& [ty, polarity] : infer.types)
    {
        auto gt = getMutable<GenericType>(ty);
        LUAU_ASSERT(gt);
        gt->polarity = polarity;
    }

    for (const auto& [tp, polarity] : infer.packs)
    {
        if (tp->owningArena != arena)
            continue;
        auto gp = getMutable<GenericTypePack>(tp);
        LUAU_ASSERT(gp);
        gp->polarity = polarity;
    }
}

void inferGenericPolarities(NotNull<TypeArena> arena, NotNull<Scope> scope, TypeId ty)
{
    inferGenericPolarities_(arena, scope, ty);
}

void inferGenericPolarities(NotNull<TypeArena> arena, NotNull<Scope> scope, TypePackId tp)
{
    inferGenericPolarities_(arena, scope, tp);
}

} // namespace Luau
