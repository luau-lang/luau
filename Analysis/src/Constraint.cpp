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

struct FreeTypeCollector : TypeOnceVisitor
{

    DenseHashSet<TypeId>* result;

    FreeTypeCollector(DenseHashSet<TypeId>* result)
        : result(result)
    {
    }

    bool visit(TypeId ty, const FreeType&) override
    {
        result->insert(ty);
        return false;
    }
};

DenseHashSet<TypeId> Constraint::getFreeTypes() const
{
    DenseHashSet<TypeId> types{{}};
    FreeTypeCollector ftc{&types};

    if (auto sc = get<SubtypeConstraint>(*this))
    {
        ftc.traverse(sc->subType);
        ftc.traverse(sc->superType);
    }
    else if (auto psc = get<PackSubtypeConstraint>(*this))
    {
        ftc.traverse(psc->subPack);
        ftc.traverse(psc->superPack);
    }

    return types;
}

} // namespace Luau
