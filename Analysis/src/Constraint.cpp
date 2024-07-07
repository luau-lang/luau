// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Constraint.h"
#include "Luau/VisitType.h"

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

    DenseHashSet<TypeId>* result;

    ReferenceCountInitializer(DenseHashSet<TypeId>* result)
        : result(result)
    {
    }

    bool visit(TypeId ty, const FreeType&) override
    {
        result->insert(ty);
        return false;
    }

    bool visit(TypeId ty, const BlockedType&) override
    {
        result->insert(ty);
        return false;
    }

    bool visit(TypeId ty, const PendingExpansionType&) override
    {
        result->insert(ty);
        return false;
    }

    bool visit(TypeId ty, const ClassType&) override
    {
        // ClassTypes never contain free types.
        return false;
    }
};

bool isReferenceCountedType(const TypeId typ)
{
    // n.b. this should match whatever `ReferenceCountInitializer` includes.
    return get<FreeType>(typ) || get<BlockedType>(typ) || get<PendingExpansionType>(typ);
}

DenseHashSet<TypeId> Constraint::getMaybeMutatedFreeTypes() const
{
    // For the purpose of this function and reference counting in general, we are only considering
    // mutations that affect the _bounds_ of the free type, and not something that may bind the free
    // type itself to a new type. As such, `ReduceConstraint` and `GeneralizationConstraint` have no
    // contribution to the output set here.

    DenseHashSet<TypeId> types{{}};
    ReferenceCountInitializer rci{&types};

    if (auto ec = get<EqualityConstraint>(*this))
    {
        rci.traverse(ec->resultType);
        // `EqualityConstraints` should not mutate `assignmentType`.
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
    else if (auto ptc = get<PrimitiveTypeConstraint>(*this))
    {
        rci.traverse(ptc->freeType);
    }
    else if (auto hpc = get<HasPropConstraint>(*this))
    {
        rci.traverse(hpc->resultType);
        // `HasPropConstraints` should not mutate `subjectType`.
    }
    else if (auto hic = get<HasIndexerConstraint>(*this))
    {
        rci.traverse(hic->resultType);
        // `HasIndexerConstraint` should not mutate `subjectType` or `indexType`.
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

    return types;
}

} // namespace Luau
