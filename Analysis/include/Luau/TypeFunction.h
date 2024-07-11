// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/ConstraintSolver.h"
#include "Luau/Error.h"
#include "Luau/NotNull.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeFwd.h"

#include <functional>
#include <string>
#include <optional>

namespace Luau
{

struct TypeArena;
struct TxnLog;
class Normalizer;

struct TypeFunctionContext
{
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtins;
    NotNull<Scope> scope;
    NotNull<Normalizer> normalizer;
    NotNull<InternalErrorReporter> ice;
    NotNull<TypeCheckLimits> limits;

    // nullptr if the type function is being reduced outside of the constraint solver.
    ConstraintSolver* solver;
    // The constraint being reduced in this run of the reduction
    const Constraint* constraint;

    TypeFunctionContext(NotNull<ConstraintSolver> cs, NotNull<Scope> scope, NotNull<const Constraint> constraint)
        : arena(cs->arena)
        , builtins(cs->builtinTypes)
        , scope(scope)
        , normalizer(cs->normalizer)
        , ice(NotNull{&cs->iceReporter})
        , limits(NotNull{&cs->limits})
        , solver(cs.get())
        , constraint(constraint.get())
    {
    }

    TypeFunctionContext(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins, NotNull<Scope> scope, NotNull<Normalizer> normalizer,
        NotNull<InternalErrorReporter> ice, NotNull<TypeCheckLimits> limits)
        : arena(arena)
        , builtins(builtins)
        , scope(scope)
        , normalizer(normalizer)
        , ice(ice)
        , limits(limits)
        , solver(nullptr)
        , constraint(nullptr)
    {
    }

    NotNull<Constraint> pushConstraint(ConstraintV&& c);
};

/// Represents a reduction result, which may have successfully reduced the type,
/// may have concretely failed to reduce the type, or may simply be stuck
/// without more information.
template<typename Ty>
struct TypeFunctionReductionResult
{
    /// The result of the reduction, if any. If this is nullopt, the type function
    /// could not be reduced.
    std::optional<Ty> result;
    /// Whether the result is uninhabited: whether we know, unambiguously and
    /// permanently, whether this type function reduction results in an
    /// uninhabitable type. This will trigger an error to be reported.
    bool uninhabited;
    /// Any types that need to be progressed or mutated before the reduction may
    /// proceed.
    std::vector<TypeId> blockedTypes;
    /// Any type packs that need to be progressed or mutated before the
    /// reduction may proceed.
    std::vector<TypePackId> blockedPacks;
};

template<typename T>
using ReducerFunction =
    std::function<TypeFunctionReductionResult<T>(T, const std::vector<TypeId>&, const std::vector<TypePackId>&, NotNull<TypeFunctionContext>)>;

/// Represents a type function that may be applied to map a series of types and
/// type packs to a single output type.
struct TypeFunction
{
    /// The human-readable name of the type function. Used to stringify instance
    /// types.
    std::string name;

    /// The reducer function for the type function.
    ReducerFunction<TypeId> reducer;
};

/// Represents a type function that may be applied to map a series of types and
/// type packs to a single output type pack.
struct TypePackFunction
{
    /// The human-readable name of the type pack function. Used to stringify
    /// instance packs.
    std::string name;

    /// The reducer function for the type pack function.
    ReducerFunction<TypePackId> reducer;
};

struct FunctionGraphReductionResult
{
    ErrorVec errors;
    DenseHashSet<TypeId> blockedTypes{nullptr};
    DenseHashSet<TypePackId> blockedPacks{nullptr};
    DenseHashSet<TypeId> reducedTypes{nullptr};
    DenseHashSet<TypePackId> reducedPacks{nullptr};
};

/**
 * Attempt to reduce all instances of any type or type pack functions in the type
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
FunctionGraphReductionResult reduceTypeFunctions(TypeId entrypoint, Location location, TypeFunctionContext, bool force = false);

/**
 * Attempt to reduce all instances of any type or type pack functions in the type
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
FunctionGraphReductionResult reduceTypeFunctions(TypePackId entrypoint, Location location, TypeFunctionContext, bool force = false);

struct BuiltinTypeFunctions
{
    BuiltinTypeFunctions();

    TypeFunction notFunc;
    TypeFunction lenFunc;
    TypeFunction unmFunc;

    TypeFunction addFunc;
    TypeFunction subFunc;
    TypeFunction mulFunc;
    TypeFunction divFunc;
    TypeFunction idivFunc;
    TypeFunction powFunc;
    TypeFunction modFunc;

    TypeFunction concatFunc;

    TypeFunction andFunc;
    TypeFunction orFunc;

    TypeFunction ltFunc;
    TypeFunction leFunc;
    TypeFunction eqFunc;

    TypeFunction refineFunc;
    TypeFunction singletonFunc;
    TypeFunction unionFunc;
    TypeFunction intersectFunc;

    TypeFunction keyofFunc;
    TypeFunction rawkeyofFunc;
    TypeFunction indexFunc;
    TypeFunction rawgetFunc;

    void addToScope(NotNull<TypeArena> arena, NotNull<Scope> scope) const;
};

const BuiltinTypeFunctions& builtinTypeFunctions();

} // namespace Luau
