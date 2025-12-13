// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Error.h"
#include "Luau/InsertionOrderedMap.h"
#include "Luau/Location.h"
#include "Luau/NotNull.h"
#include "Luau/Subtyping.h"
#include "Luau/TypeFwd.h"

namespace Luau
{

struct BuiltinTypes;
struct TypeArena;
struct Scope;
struct InternalErrorReporter;
struct TypeCheckLimits;
struct Subtyping;

class Normalizer;

// There are essentially two reasons why we might consider an overload
// to be unsuitable:
//
// First, subtyping might fail.  This also includes cases where we
// cannot find a generic substitution scheme that results in a
// callable function.
//
// Second, subtyping might succeed, but it might result in the
// presence of an unreducible type function.  (eg add<string, string>)
//
// TODO: Subtyping should probably also be the thing that checks for
// unreducible type functions.  This would be nice because it would
// mean that overload resolution would not have to talk about type
// functions at all.
using IncompatibilityReason = Variant<SubtypingReasonings, ErrorVec>;

/**
 * Struct representing "selecting" an overload from `OverloadResolution::resolveOverload`.
 */
struct SelectedOverload
{
   /**
     * An unambiguous overload, if one can be selected. This is _not_ necessarily
     * an overload that is valid for the argument pack provided. For example:
     *
     *  local f: ((string) -> "one") & ((string, string) -> "two")
     *
     *  -- We will select `(string) -> "one"` here based on arity. Type checking
     *  -- will later inform us that `42` is not a string, but that's ok.
     *  f(42)
     */
    std::optional<TypeId> overload;

    /**
     * Any associated constraints with selecting this overload. For example, in
     * the code block:
     *
     *  local f: ((string, number) -> string) & ((number, boolean) -> number)
     *  local function g(x)
     *      -- When selecting an overload at this point, we'll reject the
     *      -- second overload, and claim that this is the only possible
     *      -- overload with a constraint of `x <: string`.
     *      f(x, 42)
     *   end
     */
    std::vector<ConstraintV> assumedConstraints;

    /**
     * Whether we should potentially defer selecting an overload, such as in:
     *
     *  local f: ((string) -> string) & ((number) -> number)
     *  local function g(x)
     *      -- There *may* be another constraint on `x` later that allows us to
     *      -- unambiguously select an overload.
     *      f(x)
     *  end
     */
    bool shouldRetry;
};

struct OverloadResolution
{
    // Overloads that will work
    std::vector<TypeId> ok;

    // "Overloads" that aren't callable.
    std::vector<TypeId> nonFunctions;

    // Overloads that could match, but require that other constraints also be satisfied.
    std::vector<std::pair<TypeId, std::vector<ConstraintV>>> potentialOverloads;

    // Overloads that have the correct arity, but do not work.
    std::vector<std::pair<TypeId, IncompatibilityReason>> incompatibleOverloads;

    // Overloads that will never work specifically because of an arity mismatch.
    std::vector<TypeId> arityMismatches;

    // If a particular overload is a __call metamethod, then type inference
    // needs to know so that it can prepend the self argument to the argument
    // list when it infers.
    DenseHashSet<TypeId> metamethods{nullptr};

    /**
     * Try to determine an unambiguous overload. See `SelectedOverload` for
     * documentation.
     */
    SelectedOverload getUnambiguousOverload() const;

};

struct OverloadResolver
{
    enum Analysis
    {
        Ok,
        TypeIsNotAFunction,
        ArityMismatch,
        OverloadIsNonviable, // Arguments were incompatible with the overloads parameters but were otherwise compatible by arity
    };

    OverloadResolver(
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<TypeArena> arena,
        NotNull<Normalizer> normalizer,
        NotNull<TypeFunctionRuntime> typeFunctionRuntime,
        NotNull<Scope> scope,
        NotNull<InternalErrorReporter> reporter,
        NotNull<TypeCheckLimits> limits,
        Location callLocation
    );

    NotNull<BuiltinTypes> builtinTypes;
    NotNull<TypeArena> arena;
    NotNull<Normalizer> normalizer;
    NotNull<TypeFunctionRuntime> typeFunctionRuntime;
    NotNull<Scope> scope;
    NotNull<InternalErrorReporter> ice;
    NotNull<TypeCheckLimits> limits;
    Subtyping subtyping;
    Location callLoc;

    // Resolver results
    std::vector<TypeId> ok;
    std::vector<TypeId> nonFunctions;
    std::vector<std::pair<TypeId, ErrorVec>> arityMismatches;
    std::vector<std::pair<TypeId, ErrorVec>> nonviableOverloads;
    InsertionOrderedMap<TypeId, std::pair<OverloadResolver::Analysis, size_t>> resolution;

    // Given a (potentially overloaded) function and a set of arguments, test each overload.
    OverloadResolution resolveOverload(
        TypeId fnTy,
        TypePackId args,
        Location fnLocation,
        NotNull<DenseHashSet<TypeId>> uniqueTypes,
        bool useFreeTypeBounds
    );

    void reportErrors(
        ErrorVec& errors,
        TypeId fnTy,
        Location fnLocation,
        const ModuleName& moduleName,
        TypePackId argPack,
        const std::vector<AstExpr*>& argExprs,
        const SubtypingReasoning& reason
    ) const;

private:
    void testFunctionOrUnion(
        OverloadResolution& result,
        TypeId fnTy,
        TypePackId argsPack,
        Location fnLocation,
        NotNull<DenseHashSet<TypeId>> uniqueTypes
    );

    void testFunction(
        OverloadResolution& result,
        TypeId fnTy,
        TypePackId argsPack,
        Location fnLocation,
        NotNull<DenseHashSet<TypeId>> uniqueTypes
    );

    void testFunctionOrCallMetamethod(
        OverloadResolution& result,
        TypeId fnTy,
        TypePackId argsPack,
        Location fnLocation,
        NotNull<DenseHashSet<TypeId>> uniqueTypes
    );

public:
    // We want to clip this with LuauNewOverloadResolver2, but there are other
    // call sites such as `solveFunctionCall`.
    std::pair<Analysis, TypeId> selectOverload_DEPRECATED(
        TypeId ty,
        TypePackId args,
        NotNull<DenseHashSet<TypeId>> uniqueTypes,
        bool useFreeTypeBounds
    );

    // Clip with LuauNewOverloadResolver2
    void resolve_DEPRECATED(
        TypeId fnTy,
        const TypePack* args,
        AstExpr* selfExpr,
        const std::vector<AstExpr*>* argExprs,
        NotNull<DenseHashSet<TypeId>> uniqueTypes
    );

private:
    std::pair<Analysis, ErrorVec> checkOverload(
        TypeId fnTy,
        const TypePack* args,
        AstExpr* fnLoc,
        const std::vector<AstExpr*>* argExprs,
        NotNull<DenseHashSet<TypeId>> uniqueTypes,
        bool callMetamethodOk = true
    );
    static bool isLiteral(AstExpr* expr);
    LUAU_NOINLINE
    std::pair<Analysis, ErrorVec> checkOverload_(
        TypeId fnTy,
        const FunctionType* fn,
        const TypePack* args,
        AstExpr* fnExpr,
        const std::vector<AstExpr*>* argExprs,
        NotNull<DenseHashSet<TypeId>> uniqueTypes
    );
    size_t indexof(Analysis analysis);
    void add(Analysis analysis, TypeId ty, ErrorVec&& errors);
    void maybeEmplaceError(
        ErrorVec* errors,
        Location argLocation,
        const SubtypingReasoning* reason,
        std::optional<TypeId> wantedTy,
        std::optional<TypeId> givenTy
    ) const;

    void maybeEmplaceError(
        ErrorVec* errors,
        Location argLocation,
        const ModuleName& moduleName,
        const SubtypingReasoning* reason,
        std::optional<TypeId> wantedTy,
        std::optional<TypeId> givenTy
    ) const;

    void maybeEmplaceError(
        ErrorVec* errors,
        Location argLocation,
        const ModuleName& moduleName,
        const SubtypingReasoning* reason,
        std::optional<TypePackId> wantedTp,
        std::optional<TypePackId> givenTp
    ) const;

    void maybeEmplaceError(
        ErrorVec* errors,
        Location argLocation,
        const ModuleName& moduleName,
        const SubtypingReasoning* reason,
        std::optional<TypeOrPack> wantedType,
        std::optional<TypeOrPack> givenType
    ) const;

    // Checks if the candidate args are arity-compatible with the desired parameters.
    // Used during overload selection to do arity-based filtering of overloads.
    // We do not accept nil in place of a generic unless that generic is explicitly optional.
    bool isArityCompatible(TypePackId candidate, TypePackId desired, NotNull<BuiltinTypes> builtinTypes) const;

    bool testFunctionTypeForOverloadSelection(
        const FunctionType* ftv,
        NotNull<DenseHashSet<TypeId>> uniqueTypes,
        TypePackId argsPack,
        bool useFreeTypeBounds
    );
};

struct SolveResult
{
    enum OverloadCallResult
    {
        Ok,
        CodeTooComplex,
        OccursCheckFailed,
        NoMatchingOverload,
    };

    OverloadCallResult result;
    std::optional<TypePackId> typePackId; // nullopt if result != Ok

    TypeId overloadToUse = nullptr;
    TypeId inferredTy = nullptr;
    DenseHashMap<TypeId, std::vector<TypeId>> expandedFreeTypes{nullptr};
};

// Helper utility, presently used for binary operator type functions.
//
// Given a function and a set of arguments, select a suitable overload.
// Clip with FFlag::LuauBuiltinTypeFunctionsUseNewOverloadResolution
SolveResult solveFunctionCall_DEPRECATED(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Normalizer> normalizer,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    NotNull<InternalErrorReporter> iceReporter,
    NotNull<TypeCheckLimits> limits,
    NotNull<Scope> scope,
    const Location& location,
    TypeId fn,
    TypePackId argsPack
);

} // namespace Luau
