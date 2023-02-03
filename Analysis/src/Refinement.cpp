// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Refinement.h"

namespace Luau
{

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

RefinementId RefinementArena::proposition(DefId def, TypeId discriminantTy)
{
    return NotNull{allocator.allocate(Proposition{def, discriminantTy})};
}

} // namespace Luau
