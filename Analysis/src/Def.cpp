// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Def.h"
#include "Luau/Common.h"

namespace Luau
{

bool containsSubscriptedDefinition(DefId def)
{
    if (auto cell = get<Cell>(def))
        return cell->subscripted;

    LUAU_ASSERT(!"Phi nodes not implemented yet");
    return false;
}

DefId DefArena::freshCell(bool subscripted)
{
    return NotNull{allocator.allocate(Def{Cell{subscripted}})};
}

} // namespace Luau
