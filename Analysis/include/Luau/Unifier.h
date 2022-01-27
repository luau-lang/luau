// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Error.h"
#include "Luau/Location.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeInfer.h"
#include "Luau/Module.h" // FIXME: For TypeArena.  It merits breaking out into its own header.
#include "Luau/UnifierSharedState.h"

#include <unordered_set>

namespace Luau
{

enum Variance
{
    Covariant,
    Invariant
};

struct Unifier
{
    TypeArena* const types;
    Mode mode;
    ScopePtr globalScope; // sigh.  Needed solely to get at string's metatable.

    DEPRECATED_TxnLog DEPRECATED_log;
    TxnLog log;
    ErrorVec errors;
    Location location;
    Variance variance = Covariant;
    CountMismatch::Context ctx = CountMismatch::Arg;

    UnifierSharedState& sharedState;

    Unifier(TypeArena* types, Mode mode, ScopePtr globalScope, const Location& location, Variance variance, UnifierSharedState& sharedState,
        TxnLog* parentLog = nullptr);
    Unifier(TypeArena* types, Mode mode, ScopePtr globalScope, std::vector<std::pair<TypeId, TypeId>>* sharedSeen, const Location& location,
        Variance variance, UnifierSharedState& sharedState, TxnLog* parentLog = nullptr);

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
    void tryUnifyPrimitives(TypeId subTy, TypeId superTy);
    void tryUnifySingletons(TypeId subTy, TypeId superTy);
    void tryUnifyFunctions(TypeId subTy, TypeId superTy, bool isFunctionCall = false);
    void tryUnifyTables(TypeId subTy, TypeId superTy, bool isIntersection = false);
    void DEPRECATED_tryUnifyTables(TypeId subTy, TypeId superTy, bool isIntersection = false);
    void tryUnifyFreeTable(TypeId subTy, TypeId superTy);
    void tryUnifySealedTables(TypeId subTy, TypeId superTy, bool isIntersection);
    void tryUnifyWithMetatable(TypeId subTy, TypeId superTy, bool reversed);
    void tryUnifyWithClass(TypeId subTy, TypeId superTy, bool reversed);
    void tryUnifyIndexer(const TableIndexer& subIndexer, const TableIndexer& superIndexer);
    TypeId deeplyOptional(TypeId ty, std::unordered_map<TypeId, TypeId> seen = {});
    void cacheResult(TypeId subTy, TypeId superTy);

public:
    void tryUnify(TypePackId subTy, TypePackId superTy, bool isFunctionCall = false);

private:
    void tryUnify_(TypePackId subTy, TypePackId superTy, bool isFunctionCall = false);
    void tryUnifyVariadics(TypePackId subTy, TypePackId superTy, bool reversed, int subOffset = 0);

    void tryUnifyWithAny(TypeId subTy, TypeId anyTy);
    void tryUnifyWithAny(TypePackId subTy, TypePackId anyTp);

    std::optional<TypeId> findTablePropertyRespectingMeta(TypeId lhsType, Name name);

public:
    // Report an "infinite type error" if the type "needle" already occurs within "haystack"
    void occursCheck(TypeId needle, TypeId haystack);
    void occursCheck(DenseHashSet<TypeId>& seen, TypeId needle, TypeId haystack);
    void occursCheck(TypePackId needle, TypePackId haystack);
    void occursCheck(DenseHashSet<TypePackId>& seen, TypePackId needle, TypePackId haystack);

    Unifier makeChildUnifier();

    // A utility function that appends the given error to the unifier's error log.
    // This allows setting a breakpoint wherever the unifier reports an error.
    void reportError(TypeError error)
    {
        errors.push_back(error);
    }

private:
    bool isNonstrictMode() const;

    void checkChildUnifierTypeMismatch(const ErrorVec& innerErrors, TypeId wantedType, TypeId givenType);
    void checkChildUnifierTypeMismatch(const ErrorVec& innerErrors, const std::string& prop, TypeId wantedType, TypeId givenType);

    [[noreturn]] void ice(const std::string& message, const Location& location);
    [[noreturn]] void ice(const std::string& message);

    // Available after regular type pack unification errors
    std::optional<int> firstPackErrorPos;
};

} // namespace Luau
