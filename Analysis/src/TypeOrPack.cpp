// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeOrPack.h"
#include "Luau/Common.h"

namespace Luau
{

const void* ptr(TypeOrPack tyOrTp)
{
    if (auto ty = get<TypeId>(tyOrTp))
        return static_cast<const void*>(*ty);
    else if (auto tp = get<TypePackId>(tyOrTp))
        return static_cast<const void*>(*tp);
    else
        LUAU_UNREACHABLE();
}

TypeOrPack follow(TypeOrPack tyOrTp)
{
    if (auto ty = get<TypeId>(tyOrTp))
        return follow(*ty);
    else if (auto tp = get<TypePackId>(tyOrTp))
        return follow(*tp);
    else
        LUAU_UNREACHABLE();
}

} // namespace Luau
