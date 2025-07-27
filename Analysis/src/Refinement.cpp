// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Refinement.h"
#include <algorithm>

namespace Luau
{

RefinementId RefinementArena::variadic(const std::vector<RefinementId>& refis)
{
    bool hasRefinements = false;
    for (RefinementId r : refis)
        hasRefinements |= bool(r);

    if (!hasRefinements)
        return nullptr;

    return NotNull{allocator.allocate(Variadic{refis})};
}

RefinementId RefinementArena::negation(RefinementId refinement)
{
    if (!refinement)
        return nullptr;

    return NotNull{allocator.allocate(Negation{refinement})};
}

RefinementId RefinementArena::conjunction(RefinementId lhs, RefinementId rhs)
{
    if (!lhs && !rhs)
        return nullptr;

    return NotNull{allocator.allocate(Conjunction{lhs, rhs})};
}

RefinementId RefinementArena::disjunction(RefinementId lhs, RefinementId rhs)
{
    if (!lhs && !rhs)
        return nullptr;

    return NotNull{allocator.allocate(Disjunction{lhs, rhs})};
}

RefinementId RefinementArena::equivalence(RefinementId lhs, RefinementId rhs)
{
    if (!lhs && !rhs)
        return nullptr;

    return NotNull{allocator.allocate(Equivalence{lhs, rhs})};
}

RefinementId RefinementArena::proposition(const RefinementKey* key, TypeId discriminantTy)
{
    if (!key)
        return nullptr;

    return NotNull{allocator.allocate(Proposition{key, discriminantTy, false})};
}

RefinementId RefinementArena::implicitProposition(const RefinementKey* key, TypeId discriminantTy)
{
    if (!key)
        return nullptr;

    return NotNull{allocator.allocate(Proposition{key, discriminantTy, true})};
}

} // namespace Luau
