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

template<typename TID>
struct GeneralizationResult
{
    std::optional<TID> result;

    // True if the provided type was replaced with a generic.
    bool wasReplacedByGeneric = false;

    bool resourceLimitsExceeded = false;

    explicit operator bool() const
    {
        return bool(result);
    }
};

// Replace a single free type by its bounds according to the polarity provided.
GeneralizationResult<TypeId> generalizeType(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    TypeId freeTy,
    const GeneralizationParams<TypeId>& params
);

// Generalize one type pack
GeneralizationResult<TypePackId> generalizeTypePack(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    TypePackId tp,
    const GeneralizationParams<TypePackId>& params
);

void sealTable(NotNull<Scope> scope, TypeId ty);

/** Attempt to generalize a type.
 *
 * If generalizationTarget is set, then only that type will be replaced by its
 * bounds.  The way this is intended to be used is that ty is some function that
 * is not fully generalized, and generalizationTarget is a type within its
 * signature.  There should be no further constraints that could affect the
 * bounds of generalizationTarget.
 *
 * Returns nullopt if generalization failed due to resources limits.
 */
std::optional<TypeId> generalize(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    NotNull<DenseHashSet<TypeId>> cachedTypes,
    TypeId ty,
    std::optional<TypeId> generalizationTarget = {}
);

void pruneUnnecessaryGenerics(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Scope> scope,
    NotNull<DenseHashSet<TypeId>> cachedTypes,
    TypeId ty
);

} // namespace Luau
