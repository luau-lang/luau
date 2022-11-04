// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Connective.h"

namespace Luau
{

ConnectiveId ConnectiveArena::negation(ConnectiveId connective)
{
    return NotNull{allocator.allocate(Negation{connective})};
}

ConnectiveId ConnectiveArena::conjunction(ConnectiveId lhs, ConnectiveId rhs)
{
    return NotNull{allocator.allocate(Conjunction{lhs, rhs})};
}

ConnectiveId ConnectiveArena::disjunction(ConnectiveId lhs, ConnectiveId rhs)
{
    return NotNull{allocator.allocate(Disjunction{lhs, rhs})};
}

ConnectiveId ConnectiveArena::equivalence(ConnectiveId lhs, ConnectiveId rhs)
{
    return NotNull{allocator.allocate(Equivalence{lhs, rhs})};
}

ConnectiveId ConnectiveArena::proposition(DefId def, TypeId discriminantTy)
{
    return NotNull{allocator.allocate(Proposition{def, discriminantTy})};
}

} // namespace Luau
