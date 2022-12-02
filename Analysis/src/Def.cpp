// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Def.h"

namespace Luau
{

DefId DefArena::freshCell()
{
    return NotNull{allocator.allocate<Def>(Def{Cell{std::nullopt}})};
}

DefId DefArena::freshCell(DefId parent, const std::string& prop)
{
    return NotNull{allocator.allocate<Def>(Def{Cell{FieldMetadata{parent, prop}}})};
}

} // namespace Luau
