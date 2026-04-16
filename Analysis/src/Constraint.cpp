// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Constraint.h"
#include "Luau/TypeFunction.h"
#include "Luau/VisitType.h"

LUAU_FASTFLAG(LuauUseConstraintSetsToTrackFreeTypes)

namespace Luau
{

Constraint::Constraint(NotNull<Scope> scope, const Location& location, ConstraintV&& c)
    : scope(scope)
    , location(location)
    , c(std::move(c))
{
}

struct ReferenceCountInitializer : TypeOnceVisitor
{
    NotNull<TypeIds> mutatedTypes;
    TypePackIds* mutatedTypePacks;
    bool traverseIntoTypeFunctions = true;

    explicit ReferenceCountInitializer(NotNull<TypeIds> mutatedTypes)
        : TypeOnceVisitor("ReferenceCountInitializer", /* skipBoundTypes */ true)
        , mutatedTypes(mutatedTypes)
        , mutatedTypePacks(nullptr)
    {
        LUAU_ASSERT(!FFlag::LuauUseConstraintSetsToTrackFreeTypes);
    }

    explicit ReferenceCountInitializer(
        NotNull<TypeIds> mutatedTypes,
        NotNull<TypePackIds> mutatedTypePacks
    )
        : TypeOnceVisitor("ReferenceCountInitializer", /* skipBoundTypes */ true)
        , mutatedTypes(mutatedTypes)
        , mutatedTypePacks(mutatedTypePacks.get())
    {
        LUAU_ASSERT(FFlag::LuauUseConstraintSetsToTrackFreeTypes);
    }

    bool visit(TypeId ty, const FreeType&) override
    {
        mutatedTypes->insert(ty);
        return false;
    }

    bool visit(TypeId ty, const BlockedType&) override
    {
        mutatedTypes->insert(ty);
        return false;
    }

    bool visit(TypeId ty, const PendingExpansionType&) override
    {
        mutatedTypes->insert(ty);
        return false;
    }

    bool visit(TypeId ty, const TableType& tt) override
    {
        if (tt.state == TableState::Unsealed || tt.state == TableState::Free)
            mutatedTypes->insert(ty);

        return true;
    }

    bool visit(TypeId ty, const ExternType&) override
    {
        // ExternTypes never contain free types.
        return false;
    }

    bool visit(TypeId, const TypeFunctionInstanceType& tfit) override
    {
        return tfit.function->canReduceGenerics;
    }
};

bool isReferenceCountedType(const TypeId typ)
{
    if (auto tt = get<TableType>(typ))
        return tt->state == TableState::Free || tt->state == TableState::Unsealed;

    // n.b. this should match whatever `ReferenceCountInitializer` includes.
    return get<FreeType>(typ) || get<BlockedType>(typ) || get<PendingExpansionType>(typ);
}

TypeIds Constraint::DEPRECATED_getMaybeMutatedFreeTypes() const
{
    LUAU_ASSERT(!FFlag::LuauUseConstraintSetsToTrackFreeTypes);
    // For the purpose of this function and reference counting in general, we are only considering
    // mutations that affect the _bounds_ of the free type, and not something that may bind the free
    // type itself to a new type. As such, `ReduceConstraint` and `GeneralizationConstraint` have no
    // contribution to the output set here.

    TypeIds types;
    ReferenceCountInitializer rci{NotNull{&types}};

    if (auto ec = get<EqualityConstraint>(*this))
    {
        rci.traverse(ec->resultType);
        rci.traverse(ec->assignmentType);
    }
    else if (auto sc = get<SubtypeConstraint>(*this))
    {
        rci.traverse(sc->subType);
        rci.traverse(sc->superType);
    }
    else if (auto psc = get<PackSubtypeConstraint>(*this))
    {
        rci.traverse(psc->subPack);
        rci.traverse(psc->superPack);
    }
    else if (auto itc = get<IterableConstraint>(*this))
    {
        for (TypeId ty : itc->variables)
            rci.traverse(ty);
        // `IterableConstraints` should not mutate `iterator`.
    }
    else if (auto nc = get<NameConstraint>(*this))
    {
        rci.traverse(nc->namedType);
    }
    else if (auto taec = get<TypeAliasExpansionConstraint>(*this))
    {
        rci.traverse(taec->target);
    }
    else if (auto fchc = get<FunctionCheckConstraint>(*this))
    {
        rci.traverse(fchc->argsPack);
    }
    else if (auto fcc = get<FunctionCallConstraint>(*this))
    {
        rci.traverseIntoTypeFunctions = false;
        rci.traverse(fcc->fn);
        rci.traverse(fcc->argsPack);
        rci.traverseIntoTypeFunctions = true;
    }
    else if (auto ptc = get<PrimitiveTypeConstraint>(*this))
    {
        rci.traverse(ptc->freeType);
    }
    else if (auto hpc = get<HasPropConstraint>(*this))
    {
        rci.traverse(hpc->resultType);
        rci.traverse(hpc->subjectType);
    }
    else if (auto hic = get<HasIndexerConstraint>(*this))
    {
        rci.traverse(hic->subjectType);
        rci.traverse(hic->resultType);
        // `HasIndexerConstraint` should not mutate `indexType`.
    }
    else if (auto apc = get<AssignPropConstraint>(*this))
    {
        rci.traverse(apc->lhsType);
        rci.traverse(apc->rhsType);
    }
    else if (auto aic = get<AssignIndexConstraint>(*this))
    {
        rci.traverse(aic->lhsType);
        rci.traverse(aic->indexType);
        rci.traverse(aic->rhsType);
    }
    else if (auto uc = get<UnpackConstraint>(*this))
    {
        for (TypeId ty : uc->resultPack)
            rci.traverse(ty);
        // `UnpackConstraint` should not mutate `sourcePack`.
    }
    else if (auto rpc = get<ReducePackConstraint>(*this))
    {
        rci.traverse(rpc->tp);
    }
    else if (auto pftc = get<PushFunctionTypeConstraint>(*this))
    {
        rci.traverse(pftc->functionType);
    }
    else if (auto ptc = get<PushTypeConstraint>(*this))
    {
        rci.traverse(ptc->targetType);
    }

    return types;
}

std::pair<TypeIds, TypePackIds> Constraint::getMaybeMutatedTypes() const
{
    LUAU_ASSERT(FFlag::LuauUseConstraintSetsToTrackFreeTypes);

    // For the purpose of this function and reference counting in general, we are only considering
    // mutations that affect the _bounds_ of the free type, and not something that may bind the free
    // type itself to a new type. As such, `ReduceConstraint` and `GeneralizationConstraint` have no
    // contribution to the output set here.

    TypeIds types;

    // NOTE: In the future we'd like to track references to type packs, so we're
    // adding this local, but we do not modify it.
    TypePackIds typePacks;

    ReferenceCountInitializer rci{NotNull{&types}, NotNull{&typePacks}};

    if (auto ec = get<EqualityConstraint>(*this))
    {
        rci.traverse(ec->resultType);
        rci.traverse(ec->assignmentType);
    }
    else if (auto sc = get<SubtypeConstraint>(*this))
    {
        rci.traverse(sc->subType);
        rci.traverse(sc->superType);
    }
    else if (auto psc = get<PackSubtypeConstraint>(*this))
    {
        rci.traverse(psc->subPack);
        rci.traverse(psc->superPack);
    }
    else if (auto itc = get<IterableConstraint>(*this))
    {
        for (TypeId ty : itc->variables)
            rci.traverse(ty);
        // `IterableConstraints` should not mutate `iterator`.
    }
    else if (auto nc = get<NameConstraint>(*this))
    {
        rci.traverse(nc->namedType);
    }
    else if (auto taec = get<TypeAliasExpansionConstraint>(*this))
    {
        rci.traverse(taec->target);
    }
    else if (auto fchc = get<FunctionCheckConstraint>(*this))
    {
        rci.traverse(fchc->argsPack);
    }
    else if (auto fcc = get<FunctionCallConstraint>(*this))
    {
        rci.traverseIntoTypeFunctions = false;
        rci.traverse(fcc->fn);
        rci.traverse(fcc->argsPack);
        rci.traverseIntoTypeFunctions = true;
    }
    else if (auto ptc = get<PrimitiveTypeConstraint>(*this))
    {
        rci.traverse(ptc->freeType);
    }
    else if (auto hpc = get<HasPropConstraint>(*this))
    {
        rci.traverse(hpc->resultType);
        rci.traverse(hpc->subjectType);
    }
    else if (auto hic = get<HasIndexerConstraint>(*this))
    {
        rci.traverse(hic->subjectType);
        rci.traverse(hic->resultType);
        // `HasIndexerConstraint` should not mutate `indexType`.
    }
    else if (auto apc = get<AssignPropConstraint>(*this))
    {
        rci.traverse(apc->lhsType);
        rci.traverse(apc->rhsType);
    }
    else if (auto aic = get<AssignIndexConstraint>(*this))
    {
        rci.traverse(aic->lhsType);
        rci.traverse(aic->indexType);
        rci.traverse(aic->rhsType);
    }
    else if (auto uc = get<UnpackConstraint>(*this))
    {
        for (TypeId ty : uc->resultPack)
            rci.traverse(ty);
        // Consider:
        //
        //  function set(dictionary, key, value)
        //      local new = table.clone(dictionary)
        //      new[key] = value
        //      return new
        //  end
        //
        // In this case, we would expect `dictionary` to be inferred as
        // something like `{ [T]: K }` for some generic `T` and `K`.
        // However, in order to avoid eagerly generalizing dictionary,
        // we need to track that it may be mutated by the line:
        //
        //  new[key] = value
        //
        // ... this implies that `UnpackConstraint` can mutate both
        // it's LHS and RHS operands. LHS directly, and RHS by proxy.
        rci.traverse(uc->sourcePack);
    }
    else if (auto rpc = get<ReducePackConstraint>(*this))
    {
        rci.traverse(rpc->tp);
    }
    else if (auto pftc = get<PushFunctionTypeConstraint>(*this))
    {
        rci.traverse(pftc->functionType);
    }
    else if (auto ptc = get<PushTypeConstraint>(*this))
    {
        rci.traverse(ptc->targetType);
    }

    return { std::move(types), std::move(typePacks) };
}

} // namespace Luau
