// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Error.h"
#include "Luau/Location.h"
#include "Luau/ParseOptions.h"
#include "Luau/Scope.h"
#include "Luau/Substitution.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeArena.h"
#include "Luau/UnifierSharedState.h"
#include "Normalize.h"

#include <unordered_set>

namespace Luau
{

enum Variance
{
    Covariant,
    Invariant
};

// A substitution which replaces singleton types by their wider types
struct Widen : Substitution
{
    Widen(TypeArena* arena, NotNull<BuiltinTypes> builtinTypes)
        : Substitution(TxnLog::empty(), arena)
        , builtinTypes(builtinTypes)
    {
    }

    NotNull<BuiltinTypes> builtinTypes;

    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId ty) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId ty) override;
    bool ignoreChildren(TypeId ty) override;

    TypeId operator()(TypeId ty);
    TypePackId operator()(TypePackId ty);
};

// TODO: Use this more widely.
struct UnifierOptions
{
    bool isFunctionCall = false;
};

struct Unifier
{
    TypeArena* const types;
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<Normalizer> normalizer;
    Mode mode;

    NotNull<Scope> scope; // const Scope maybe
    TxnLog log;
    ErrorVec errors;
    Location location;
    Variance variance = Covariant;
    bool normalize;         // Normalize unions and intersections if necessary
    bool useScopes = false; // If true, we use the scope hierarchy rather than TypeLevels
    CountMismatch::Context ctx = CountMismatch::Arg;

    UnifierSharedState& sharedState;

    Unifier(
        NotNull<Normalizer> normalizer, Mode mode, NotNull<Scope> scope, const Location& location, Variance variance, TxnLog* parentLog = nullptr);

    // Test whether the two type vars unify.  Never commits the result.
    ErrorVec canUnify(TypeId subTy, TypeId superTy);
    ErrorVec canUnify(TypePackId subTy, TypePackId superTy, bool isFunctionCall = false);

    /** Attempt to unify.
     * Populate the vector errors with any type errors that may arise.
     * Populate the transaction log with the set of TypeIds that need to be reset to undo the unification attempt.
     */
    void tryUnify(TypeId subTy, TypeId superTy, bool isFunctionCall = false, bool isIntersection = false);

private:
    void tryUnify_(TypeId subTy, TypeId superTy, bool isFunctionCall = false, bool isIntersection = false);
    void tryUnifyUnionWithType(TypeId subTy, const UnionType* uv, TypeId superTy);
    void tryUnifyTypeWithUnion(TypeId subTy, TypeId superTy, const UnionType* uv, bool cacheEnabled, bool isFunctionCall);
    void tryUnifyTypeWithIntersection(TypeId subTy, TypeId superTy, const IntersectionType* uv);
    void tryUnifyIntersectionWithType(TypeId subTy, const IntersectionType* uv, TypeId superTy, bool cacheEnabled, bool isFunctionCall);
    void tryUnifyNormalizedTypes(TypeId subTy, TypeId superTy, const NormalizedType& subNorm, const NormalizedType& superNorm, std::string reason,
        std::optional<TypeError> error = std::nullopt);
    void tryUnifyPrimitives(TypeId subTy, TypeId superTy);
    void tryUnifySingletons(TypeId subTy, TypeId superTy);
    void tryUnifyFunctions(TypeId subTy, TypeId superTy, bool isFunctionCall = false);
    void tryUnifyTables(TypeId subTy, TypeId superTy, bool isIntersection = false);
    void tryUnifyScalarShape(TypeId subTy, TypeId superTy, bool reversed);
    void tryUnifyWithMetatable(TypeId subTy, TypeId superTy, bool reversed);
    void tryUnifyWithClass(TypeId subTy, TypeId superTy, bool reversed);
    void tryUnifyTypeWithNegation(TypeId subTy, TypeId superTy);
    void tryUnifyNegationWithType(TypeId subTy, TypeId superTy);

    TypePackId tryApplyOverloadedFunction(TypeId function, const NormalizedFunctionType& overloads, TypePackId args);

    TypeId widen(TypeId ty);
    TypePackId widen(TypePackId tp);

    TypeId deeplyOptional(TypeId ty, std::unordered_map<TypeId, TypeId> seen = {});

    bool canCacheResult(TypeId subTy, TypeId superTy);
    void cacheResult(TypeId subTy, TypeId superTy, size_t prevErrorCount);

public:
    void tryUnify(TypePackId subTy, TypePackId superTy, bool isFunctionCall = false);

private:
    void tryUnify_(TypePackId subTy, TypePackId superTy, bool isFunctionCall = false);
    void tryUnifyVariadics(TypePackId subTy, TypePackId superTy, bool reversed, int subOffset = 0);

    void tryUnifyWithAny(TypeId subTy, TypeId anyTy);
    void tryUnifyWithAny(TypePackId subTy, TypePackId anyTp);

    std::optional<TypeId> findTablePropertyRespectingMeta(TypeId lhsType, Name name);

    TxnLog combineLogsIntoIntersection(std::vector<TxnLog> logs);
    TxnLog combineLogsIntoUnion(std::vector<TxnLog> logs);

public:
    // Returns true if the type "needle" already occurs within "haystack" and reports an "infinite type error"
    bool occursCheck(TypeId needle, TypeId haystack);
    bool occursCheck(DenseHashSet<TypeId>& seen, TypeId needle, TypeId haystack);
    bool occursCheck(TypePackId needle, TypePackId haystack);
    bool occursCheck(DenseHashSet<TypePackId>& seen, TypePackId needle, TypePackId haystack);

    Unifier makeChildUnifier();

    void reportError(TypeError err);
    LUAU_NOINLINE void reportError(Location location, TypeErrorData data);

private:
    bool isNonstrictMode() const;
    TypeMismatch::Context mismatchContext();

    void checkChildUnifierTypeMismatch(const ErrorVec& innerErrors, TypeId wantedType, TypeId givenType);
    void checkChildUnifierTypeMismatch(const ErrorVec& innerErrors, const std::string& prop, TypeId wantedType, TypeId givenType);

    [[noreturn]] void ice(const std::string& message, const Location& location);
    [[noreturn]] void ice(const std::string& message);

    // Available after regular type pack unification errors
    std::optional<int> firstPackErrorPos;
};

void promoteTypeLevels(TxnLog& log, const TypeArena* arena, TypeLevel minLevel, Scope* outerScope, bool useScope, TypePackId tp);

} // namespace Luau
