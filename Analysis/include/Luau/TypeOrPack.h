// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/Variant.h"

#include <type_traits>

namespace Luau
{

const void* ptr(TypeOrPack ty);

template<typename T, typename std::enable_if_t<TypeOrPack::is_part_of_v<T>, bool> = true>
const T* get(const TypeOrPack& tyOrTp)
{
    return tyOrTp.get_if<T>();
}

template<typename T, typename std::enable_if_t<TypeVariant::is_part_of_v<T>, bool> = true>
const T* get(const TypeOrPack& tyOrTp)
{
    if (const TypeId* ty = get<TypeId>(tyOrTp))
        return get<T>(*ty);
    else
        return nullptr;
}

template<typename T, typename std::enable_if_t<TypePackVariant::is_part_of_v<T>, bool> = true>
const T* get(const TypeOrPack& tyOrTp)
{
    if (const TypePackId* tp = get<TypePackId>(tyOrTp))
        return get<T>(*tp);
    else
        return nullptr;
}

TypeOrPack follow(TypeOrPack ty);

} // namespace Luau
