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
    else if (auto gc = get<GeneralizationConstraint>(*this))
    {
        rci.traverse(gc->generalizedType);
        // `GeneralizationConstraints` should not mutate `sourceType` or `interiorTypes`.
    }
    else if (auto itc = get<IterableConstraint>(*this))
    {
        rci.traverse(itc->variables);
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
    else if (auto ptc = get<PrimitiveTypeConstraint>(*this))
    {
        rci.traverse(ptc->freeType);
    }
    else if (auto hpc = get<HasPropConstraint>(*this))
    {
        rci.traverse(hpc->resultType);
        // `HasPropConstraints` should not mutate `subjectType`.
    }
    else if (auto spc = get<SetPropConstraint>(*this))
    {
        rci.traverse(spc->resultType);
        // `SetPropConstraints` should not mutate `subjectType` or `propType`.
        // TODO: is this true? it "unifies" with `propType`, so maybe mutates that one too?
    }
    else if (auto hic = get<HasIndexerConstraint>(*this))
    {
        rci.traverse(hic->resultType);
        // `HasIndexerConstraint` should not mutate `subjectType` or `indexType`.
    }
    else if (auto sic = get<SetIndexerConstraint>(*this))
    {
        rci.traverse(sic->propType);
        // `SetIndexerConstraints` should not mutate `subjectType` or `indexType`.
    }
    else if (auto uc = get<UnpackConstraint>(*this))
    {
        rci.traverse(uc->resultPack);
        // `UnpackConstraint` should not mutate `sourcePack`.
    }
    else if (auto u1c = get<Unpack1Constraint>(*this))
    {
        rci.traverse(u1c->resultType);
        // `Unpack1Constraint` should not mutate `sourceType`.
    }
    else if (auto rc = get<ReduceConstraint>(*this))
    {
        rci.traverse(rc->ty);
    }
    else if (auto rpc = get<ReducePackConstraint>(*this))
    {
        rci.traverse(rpc->tp);
    }

    return types;
}

} // namespace Luau
