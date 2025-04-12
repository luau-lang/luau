// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Scope.h"
#include "Luau/NotNull.h"
#include "Luau/TypeFwd.h"

namespace Luau
{

template<typename TID>
struct GeneralizationParams
{
    bool foundOutsideFunctions = false;
    size_t useCount = 0;
    Polarity polarity = Polarity::None;
};

// Replace a single free type by its bounds according to the polarity provided.
std::optional<TypeId> generalizeType(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    TypeId freeTy,
    const GeneralizationParams<TypeId>& params
);

// Generalize one type pack
std::optional<TypePackId> generalizeTypePack(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    TypePackId tp,
    const GeneralizationParams<TypePackId>& params
);

void sealTable(NotNull<Scope> scope, TypeId ty);

std::optional<TypeId> generalize(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    NotNull<DenseHashSet<TypeId>> cachedTypes,
    TypeId ty
);
}
