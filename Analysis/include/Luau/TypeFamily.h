// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/ConstraintSolver.h"
#include "Luau/Error.h"
#include "Luau/NotNull.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeFwd.h"
#include "Luau/Variant.h"

#include <functional>
#include <string>
#include <optional>

namespace Luau
{

struct TypeArena;
struct TxnLog;
class Normalizer;

struct TypeFamilyContext
{
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtins;
    NotNull<Scope> scope;
    NotNull<Normalizer> normalizer;
    NotNull<InternalErrorReporter> ice;
    NotNull<TypeCheckLimits> limits;

    // nullptr if the type family is being reduced outside of the constraint solver.
    ConstraintSolver* solver;

    TypeFamilyContext(NotNull<ConstraintSolver> cs, NotNull<Scope> scope)
        : arena(cs->arena)
        , builtins(cs->builtinTypes)
        , scope(scope)
        , normalizer(cs->normalizer)
        , ice(NotNull{&cs->iceReporter})
        , limits(NotNull{&cs->limits})
        , solver(cs.get())
    {
    }

    TypeFamilyContext(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins, NotNull<Scope> scope, NotNull<Normalizer> normalizer,
        NotNull<InternalErrorReporter> ice, NotNull<TypeCheckLimits> limits)
        : arena(arena)
        , builtins(builtins)
        , scope(scope)
        , normalizer(normalizer)
        , ice(ice)
        , limits(limits)
        , solver(nullptr)
    {
    }
};

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
    std::function<TypeFamilyReductionResult<TypeId>(const std::vector<TypeId>&, const std::vector<TypePackId>&, NotNull<TypeFamilyContext>)> reducer;
};

/// Represents a type function that may be applied to map a series of types and
/// type packs to a single output type pack.
struct TypePackFamily
{
    /// The human-readable name of the type pack family. Used to stringify
    /// instance packs.
    std::string name;

    /// The reducer function for the type pack family.
    std::function<TypeFamilyReductionResult<TypePackId>(const std::vector<TypeId>&, const std::vector<TypePackId>&, NotNull<TypeFamilyContext>)> reducer;
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
 * @param normalizer the normalizer to use when normalizing types
 * @param ice the internal error reporter to use for ICEs
 */
FamilyGraphReductionResult reduceFamilies(TypeId entrypoint, Location location, TypeFamilyContext, bool force = false);

/**
 * Attempt to reduce all instances of any type or type pack family in the type
 * graph provided.
 *
 * @param entrypoint the entry point to the type graph.
 * @param location the location the reduction is occurring at; used to populate
 * type errors.
 * @param arena an arena to allocate types into.
 * @param builtins the built-in types.
 * @param normalizer the normalizer to use when normalizing types
 * @param ice the internal error reporter to use for ICEs
 */
FamilyGraphReductionResult reduceFamilies(TypePackId entrypoint, Location location, TypeFamilyContext, bool force = false);

struct BuiltinTypeFamilies
{
    BuiltinTypeFamilies();

    TypeFamily notFamily;
    TypeFamily lenFamily;
    TypeFamily unmFamily;

    TypeFamily addFamily;
    TypeFamily subFamily;
    TypeFamily mulFamily;
    TypeFamily divFamily;
    TypeFamily idivFamily;
    TypeFamily powFamily;
    TypeFamily modFamily;

    TypeFamily concatFamily;

    TypeFamily andFamily;
    TypeFamily orFamily;

    TypeFamily ltFamily;
    TypeFamily leFamily;
    TypeFamily eqFamily;

    TypeFamily refineFamily;
    TypeFamily keyofFamily;
    TypeFamily rawkeyofFamily;

    void addToScope(NotNull<TypeArena> arena, NotNull<Scope> scope) const;
};



const BuiltinTypeFamilies kBuiltinTypeFamilies{};

} // namespace Luau
