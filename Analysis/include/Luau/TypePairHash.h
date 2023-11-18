// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeFwd.h"

#include <stdint.h>
#include <utility>

namespace Luau
{

struct TypePairHash
{
    size_t hashOne(TypeId key) const
    {
        return (uintptr_t(key) >> 4) ^ (uintptr_t(key) >> 9);
    }

    size_t hashOne(TypePackId key) const
    {
        return (uintptr_t(key) >> 4) ^ (uintptr_t(key) >> 9);
    }

    size_t operator()(const std::pair<TypeId, TypeId>& x) const
    {
        return hashOne(x.first) ^ (hashOne(x.second) << 1);
    }

    size_t operator()(const std::pair<TypePackId, TypePackId>& x) const
    {
        return hashOne(x.first) ^ (hashOne(x.second) << 1);
    }
};

} // namespace Luau
