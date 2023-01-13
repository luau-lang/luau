// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypePack.h"
#include "Luau/Variant.h"

namespace Luau
{

/// If it's desirable to allocate into a different arena than the TypeReduction instance you have, you will need
/// to create a temporary TypeReduction in that case. This is because TypeReduction caches the reduced type.
struct TypeReduction
{
    explicit TypeReduction(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<InternalErrorReporter> handle);

    std::optional<TypeId> reduce(TypeId ty);
    std::optional<TypePackId> reduce(TypePackId tp);
    std::optional<TypeFun> reduce(const TypeFun& fun);

private:
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<struct InternalErrorReporter> handle;

    DenseHashMap<TypeId, TypeId> cachedTypes{nullptr};
    DenseHashMap<TypePackId, TypePackId> cachedTypePacks{nullptr};

    std::pair<std::optional<TypeId>, bool> reduceImpl(TypeId ty);
    std::pair<std::optional<TypePackId>, bool> reduceImpl(TypePackId tp);

    // Computes an *estimated length* of the cartesian product of the given type.
    size_t cartesianProductSize(TypeId ty) const;

    bool hasExceededCartesianProductLimit(TypeId ty) const;
    bool hasExceededCartesianProductLimit(TypePackId tp) const;
};

} // namespace Luau
