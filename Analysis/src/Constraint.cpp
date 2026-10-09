// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Constraint.h"
#include "Luau/TypeFunction.h"
#include "Luau/VisitType.h"

LUAU_FASTFLAGVARIABLE(LuauIterableConstraintMutatesIterator)
LUAU_FASTFLAGVARIABLE(LuauReferenceCountInitializerIsIterative)

namespace Luau
{

// Clip with LuauCyclicRequireTypeInference
Constraint::Constraint(NotNull<Scope> scope, const Location& location, ConstraintV&& c)
    : scope(scope)
    , location(location)
    , c(std::move(c))
{
}

Constraint::Constraint(NotNull<Scope> scope, const Location& location, ConstraintV&& c, std::shared_ptr<ModuleName> moduleName)
    : scope(scope)
    , location(location)
    , c(std::move(c))
    , moduleName(std::move(moduleName))
{
}

ReferenceCountInitializer_DEPRECATED::ReferenceCountInitializer_DEPRECATED(NotNull<TypeIds> mutatedTypes, NotNull<TypePackIds> mutatedTypePacks)
    : TypeOnceVisitor("ReferenceCountInitializer", /* skipBoundTypes */ true)
    , mutatedTypes(mutatedTypes)
    , mutatedTypePacks(mutatedTypePacks.get())
{
}

bool ReferenceCountInitializer_DEPRECATED::visit(TypeId ty, const FreeType&)
{
    mutatedTypes->insert(ty);
    return false;
}

bool ReferenceCountInitializer_DEPRECATED::visit(TypeId ty, const BlockedType&)
{
    mutatedTypes->insert(ty);
    return false;
}

bool ReferenceCountInitializer_DEPRECATED::visit(TypeId ty, const PendingExpansionType&)
{
    mutatedTypes->insert(ty);
    return false;
}

bool ReferenceCountInitializer_DEPRECATED::visit(TypeId ty, const TableType& tt)
{
    if (tt.state == TableState::Unsealed || tt.state == TableState::Free)
        mutatedTypes->insert(ty);

    return true;
}

bool ReferenceCountInitializer_DEPRECATED::visit(TypeId ty, const ExternType&)
{
    // ExternTypes never contain free types.
    return false;
}

bool ReferenceCountInitializer_DEPRECATED::visit(TypeId, const TypeFunctionInstanceType& tfit)
{
    return tfit.function->canReduceGenerics;
}


bool ReferenceCountInitializer_DEPRECATED::visit(TypePackId tp, const BlockedTypePack&)
{
    LUAU_ASSERT(mutatedTypePacks);
    mutatedTypePacks->insert(tp);
    return true;
}

bool ReferenceCountInitializer_DEPRECATED::visit(TypePackId tp, const FreeTypePack&)
{
    LUAU_ASSERT(mutatedTypePacks);
    mutatedTypePacks->insert(tp);
    return true;
}

ReferenceCountInitializer::ReferenceCountInitializer(
    NotNull<TypeArena> currentArena,
    NotNull<TypeIds> mutatedTypes,
    NotNull<TypePackIds> mutatedTypePacks
)
    : IterativeTypeVisitor("ReferenceCountInitializer", /* skipBoundTypes */ true)
    , currentArena(currentArena)
    , mutatedTypes(mutatedTypes)
    , mutatedTypePacks(mutatedTypePacks)
{
}

bool ReferenceCountInitializer::visit(TypeId ty)
{
    return ty->owningArena == currentArena;
}

bool ReferenceCountInitializer::visit(TypeId ty, const FreeType&)
{
    mutatedTypes->insert(ty);
    return false;
}

bool ReferenceCountInitializer::visit(TypeId ty, const BlockedType&)
{
    mutatedTypes->insert(ty);
    return false;
}

bool ReferenceCountInitializer::visit(TypeId ty, const PendingExpansionType&)
{
    mutatedTypes->insert(ty);
    return false;
}

bool ReferenceCountInitializer::visit(TypeId ty, const TableType& tt)
{
    if (tt.state == TableState::Unsealed || tt.state == TableState::Free)
        mutatedTypes->insert(ty);

    return true;
}

bool ReferenceCountInitializer::visit(TypeId ty, const ExternType&)
{
    // ExternTypes never contain free types.
    return false;
}

bool ReferenceCountInitializer::visit(TypeId, const TypeFunctionInstanceType& tfit)
{
    return tfit.function->canReduceGenerics;
}


bool ReferenceCountInitializer::visit(TypePackId tp, const BlockedTypePack&)
{
    mutatedTypePacks->insert(tp);
    return true;
}

bool ReferenceCountInitializer::visit(TypePackId tp, const FreeTypePack&)
{
    mutatedTypePacks->insert(tp);
    return true;
}

bool isReferenceCountedType(const TypeId typ)
{
    if (auto tt = get<TableType>(typ))
        return tt->state == TableState::Free || tt->state == TableState::Unsealed;

    // n.b. this should match whatever `ReferenceCountInitializer` includes.
    return get<FreeType>(typ) || get<BlockedType>(typ) || get<PendingExpansionType>(typ);
}

std::pair<TypeIds, TypePackIds> Constraint::getMaybeMutatedTypesIn(NotNull<TypeArena> currentArena) const
{
    LUAU_ASSERT(FFlag::LuauReferenceCountInitializerIsIterative);
    // For the purpose of this function and reference counting in general, we are only considering
    // mutations that affect the _bounds_ of the free type, and not something that may bind the free
    // type itself to a new type. As such, `ReduceConstraint` and `GeneralizationConstraint` have no
    // contribution to the output set here.

    TypeIds types;
    TypePackIds typePacks;

    ReferenceCountInitializer rci{currentArena, NotNull{&types}, NotNull{&typePacks}};

    if (auto ec = get<EqualityConstraint>(*this))
    {
        rci.run(ec->resultType);
        rci.run(ec->assignmentType);
    }
    else if (auto sc = get<SubtypeConstraint>(*this))
    {
        rci.run(sc->subType);
        rci.run(sc->superType);
    }
    else if (auto psc = get<PackSubtypeConstraint>(*this))
    {
        rci.run(psc->subPack);
        rci.run(psc->superPack);
    }
    else if (auto itc = get<IterableConstraint>(*this))
    {
        for (TypeId ty : itc->variables)
            rci.run(ty);

        if (FFlag::LuauIterableConstraintMutatesIterator)
            rci.run(itc->iterator);
    }
    else if (auto nc = get<NameConstraint>(*this))
    {
        rci.run(nc->namedType);
    }
    else if (auto taec = get<TypeAliasExpansionConstraint>(*this))
    {
        rci.run(taec->target);
    }
    else if (auto fchc = get<FunctionCheckConstraint>(*this))
    {
        rci.run(fchc->argsPack);
    }
    else if (auto fcc = get<FunctionCallConstraint>(*this))
    {
        rci.run(fcc->fn);
        rci.run(fcc->argsPack);
    }
    else if (auto hpc = get<HasPropConstraint>(*this))
    {
        rci.run(hpc->resultType);
        rci.run(hpc->subjectType);
    }
    else if (auto hic = get<HasIndexerConstraint>(*this))
    {
        rci.run(hic->subjectType);
        rci.run(hic->resultType);
        // `HasIndexerConstraint` should not mutate `indexType`.
    }
    else if (auto apc = get<AssignPropConstraint>(*this))
    {
        rci.run(apc->lhsType);
        rci.run(apc->rhsType);
    }
    else if (auto aic = get<AssignIndexConstraint>(*this))
    {
        rci.run(aic->lhsType);
        rci.run(aic->indexType);
        rci.run(aic->rhsType);
    }
    else if (auto uc = get<UnpackConstraint>(*this))
    {
        for (TypeId ty : uc->resultPack)
            rci.run(ty);

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
        rci.run(uc->sourcePack);
    }
    else if (auto rpc = get<ReducePackConstraint>(*this))
    {
        rci.run(rpc->tp);
    }
    else if (auto pftc = get<PushFunctionTypeConstraint>(*this))
    {
        rci.run(pftc->functionType);
    }
    else if (auto ptc = get<PushTypeConstraint>(*this))
    {
        rci.run(ptc->targetType);
    }

    return {std::move(types), std::move(typePacks)};
}

std::pair<TypeIds, TypePackIds> Constraint::getMaybeMutatedTypes_DEPRECATED() const
{
    LUAU_ASSERT(!FFlag::LuauReferenceCountInitializerIsIterative);
    // For the purpose of this function and reference counting in general, we are only considering
    // mutations that affect the _bounds_ of the free type, and not something that may bind the free
    // type itself to a new type. As such, `ReduceConstraint` and `GeneralizationConstraint` have no
    // contribution to the output set here.

    TypeIds types;

    // NOTE: In the future we'd like to track references to type packs, so we're
    // adding this local, but we do not modify it.
    TypePackIds typePacks;

    ReferenceCountInitializer_DEPRECATED rci{NotNull{&types}, NotNull{&typePacks}};

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
        if (FFlag::LuauIterableConstraintMutatesIterator)
        {
            rci.traverse(itc->iterator);
        }
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

    return {std::move(types), std::move(typePacks)};
}

} // namespace Luau
