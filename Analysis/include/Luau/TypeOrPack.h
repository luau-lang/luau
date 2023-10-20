// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/Variant.h"

#include <type_traits>

namespace Luau
{

const void* ptr(TypeOrPack ty);

template<typename T>
const T* get(TypeOrPack ty)
{
    if constexpr (std::is_same_v<T, TypeId>)
        return ty.get_if<TypeId>();
    else if constexpr (std::is_same_v<T, TypePackId>)
        return ty.get_if<TypePackId>();
    else if constexpr (TypeVariant::is_part_of_v<T>)
    {
        if (auto innerTy = ty.get_if<TypeId>())
            return get<T>(*innerTy);
        else
            return nullptr;
    }
    else if constexpr (TypePackVariant::is_part_of_v<T>)
    {
        if (auto innerTp = ty.get_if<TypePackId>())
            return get<T>(*innerTp);
        else
            return nullptr;
    }
    else
    {
        static_assert(always_false_v<T>, "invalid T to get from TypeOrPack");
        LUAU_UNREACHABLE();
    }
}

TypeOrPack follow(TypeOrPack ty);

} // namespace Luau
