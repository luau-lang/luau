// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Def.h"

namespace Luau
{

DefId DefArena::freshCell()
{
    return NotNull{allocator.allocate(Def{Cell{}})};
}

} // namespace Luau
