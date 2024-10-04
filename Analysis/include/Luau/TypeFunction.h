// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Constraint.h"
#include "Luau/Error.h"
#include "Luau/NotNull.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeFunctionRuntime.h"
#include "Luau/TypeFwd.h"

#include <functional>
#include <string>
#include <optional>

struct lua_State;

namespace Luau
{

struct TypeArena;
struct TxnLog;
struct ConstraintSolver;
class Normalizer;

using StateRef = std::unique_ptr<lua_State, void (*)(lua_State*)>;

struct TypeFunctionRuntime
{
    TypeFunctionRuntime(NotNull<InternalErrorReporter> ice, NotNull<TypeCheckLimits> limits);
    ~TypeFunctionRuntime();

    // Return value is an error message if registration failed
    std::optional<std::string> registerFunction(AstStatTypeFunction* function);

    // For user-defined type functions, we store all generated types and packs for the duration of the typecheck
    TypedAllocator<TypeFunctionType> typeArena;
    TypedAllocator<TypeFunctionTypePackVar> typePackArena;

    NotNull<InternalErrorReporter> ice;
    NotNull<TypeCheckLimits> limits;

    StateRef state;

    // Evaluation of type functions should only be performed in the absence of parse errors in the source module
    bool allowEvaluation = true;

private:
    void prepareState();
};

struct TypeFunctionContext
{
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtins;
    NotNull<Scope> scope;
    NotNull<Normalizer> normalizer;
    NotNull<TypeFunctionRuntime> typeFunctionRuntime;
    NotNull<InternalErrorReporter> ice;
    NotNull<TypeCheckLimits> limits;

    // nullptr if the type function is being reduced outside of the constraint solver.
    ConstraintSolver* solver;
    // The constraint being reduced in this run of the reduction
    const Constraint* constraint;

    std::optional<AstName> userFuncName;          // Name of the user-defined type function; only available for UDTFs

    TypeFunctionContext(NotNull<ConstraintSolver> cs, NotNull<Scope> scope, NotNull<const Constraint> constraint);

    TypeFunctionContext(
        NotNull<TypeArena> arena,
        NotNull<BuiltinTypes> builtins,
        NotNull<Scope> scope,
        NotNull<Normalizer> normalizer,
        NotNull<TypeFunctionRuntime> typeFunctionRuntime,
        NotNull<InternalErrorReporter> ice,
        NotNull<TypeCheckLimits> limits
    )
        : arena(arena)
        , builtins(builtins)
        , scope(scope)
        , normalizer(normalizer)
        , typeFunctionRuntime(typeFunctionRuntime)
        , ice(ice)
        , limits(limits)
        , solver(nullptr)
        , constraint(nullptr)
    {
    }

    NotNull<Constraint> pushConstraint(ConstraintV&& c) const;
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
    /// A runtime error message from user-defined type functions
    std::optional<std::string> error;
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

    TypeFunction userFunc;

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
