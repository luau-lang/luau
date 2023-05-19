// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Error.h"
#include "Luau/NotNull.h"
#include "Luau/Variant.h"

#include <functional>
#include <string>
#include <optional>

namespace Luau
{

struct Type;
using TypeId = const Type*;

struct TypePackVar;
using TypePackId = const TypePackVar*;

struct TypeArena;
struct BuiltinTypes;
struct TxnLog;
class Normalizer;

/// Represents a reduction result, which may have successfully reduced the type,
/// may have concretely failed to reduce the type, or may simply be stuck
/// without more information.
template<typename Ty>
struct TypeFamilyReductionResult
{
    /// The result of the reduction, if any. If this is nullopt, the family
    /// could not be reduced.
    std::optional<Ty> result;
    /// Whether the result is uninhabited: whether we know, unambiguously and
    /// permanently, whether this type family reduction results in an
    /// uninhabitable type. This will trigger an error to be reported.
    bool uninhabited;
    /// Any types that need to be progressed or mutated before the reduction may
    /// proceed.
    std::vector<TypeId> blockedTypes;
    /// Any type packs that need to be progressed or mutated before the
    /// reduction may proceed.
    std::vector<TypePackId> blockedPacks;
};

/// Represents a type function that may be applied to map a series of types and
/// type packs to a single output type.
struct TypeFamily
{
    /// The human-readable name of the type family. Used to stringify instance
    /// types.
    std::string name;

    /// The reducer function for the type family.
    std::function<TypeFamilyReductionResult<TypeId>(std::vector<TypeId>, std::vector<TypePackId>, NotNull<TypeArena>, NotNull<BuiltinTypes>,
        NotNull<TxnLog>, NotNull<Scope>, NotNull<Normalizer>)>
        reducer;
};

/// Represents a type function that may be applied to map a series of types and
/// type packs to a single output type pack.
struct TypePackFamily
{
    /// The human-readable name of the type pack family. Used to stringify
    /// instance packs.
    std::string name;

    /// The reducer function for the type pack family.
    std::function<TypeFamilyReductionResult<TypePackId>(std::vector<TypeId>, std::vector<TypePackId>, NotNull<TypeArena>, NotNull<BuiltinTypes>,
        NotNull<TxnLog>, NotNull<Scope>, NotNull<Normalizer>)>
        reducer;
};

struct FamilyGraphReductionResult
{
    ErrorVec errors;
    DenseHashSet<TypeId> blockedTypes{nullptr};
    DenseHashSet<TypePackId> blockedPacks{nullptr};
    DenseHashSet<TypeId> reducedTypes{nullptr};
    DenseHashSet<TypePackId> reducedPacks{nullptr};
};

/**
 * Attempt to reduce all instances of any type or type pack family in the type
 * graph provided.
 *
 * @param entrypoint the entry point to the type graph.
 * @param location the location the reduction is occurring at; used to populate
 * type errors.
 * @param arena an arena to allocate types into.
 * @param builtins the built-in types.
 * @param log a TxnLog to use. If one is provided, substitution will take place
 * against the TxnLog, otherwise substitutions will directly mutate the type
 * graph. Do not provide the empty TxnLog, as a result.
 */
FamilyGraphReductionResult reduceFamilies(TypeId entrypoint, Location location, NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins,
    NotNull<Scope> scope, NotNull<Normalizer> normalizer, TxnLog* log = nullptr, bool force = false);

/**
 * Attempt to reduce all instances of any type or type pack family in the type
 * graph provided.
 *
 * @param entrypoint the entry point to the type graph.
 * @param location the location the reduction is occurring at; used to populate
 * type errors.
 * @param arena an arena to allocate types into.
 * @param builtins the built-in types.
 * @param log a TxnLog to use. If one is provided, substitution will take place
 * against the TxnLog, otherwise substitutions will directly mutate the type
 * graph. Do not provide the empty TxnLog, as a result.
 */
FamilyGraphReductionResult reduceFamilies(TypePackId entrypoint, Location location, NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins,
    NotNull<Scope> scope, NotNull<Normalizer> normalizer, TxnLog* log = nullptr, bool force = false);

struct BuiltinTypeFamilies
{
    BuiltinTypeFamilies();

    TypeFamily addFamily;
};

const BuiltinTypeFamilies kBuiltinTypeFamilies{};

} // namespace Luau
