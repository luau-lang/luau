// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Refinement.h"

namespace Luau
{

RefinementId RefinementArena::variadic(const std::vector<RefinementId>& refis)
{
    return NotNull{allocator.allocate(Variadic{refis})};
}

RefinementId RefinementArena::negation(RefinementId refinement)
{
    return NotNull{allocator.allocate(Negation{refinement})};
}

RefinementId RefinementArena::conjunction(RefinementId lhs, RefinementId rhs)
{
    return NotNull{allocator.allocate(Conjunction{lhs, rhs})};
}

RefinementId RefinementArena::disjunction(RefinementId lhs, RefinementId rhs)
{
    return NotNull{allocator.allocate(Disjunction{lhs, rhs})};
}

RefinementId RefinementArena::equivalence(RefinementId lhs, RefinementId rhs)
{
    return NotNull{allocator.allocate(Equivalence{lhs, rhs})};
}

RefinementId RefinementArena::proposition(BreadcrumbId breadcrumb, TypeId discriminantTy)
{
    return NotNull{allocator.allocate(Proposition{breadcrumb, discriminantTy})};
}

} // namespace Luau
