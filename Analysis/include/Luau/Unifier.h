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

struct UnifierCounters
{
    int recursionCount = 0;
    int iterationCount = 0;
};

struct Unifier
{
    TypeArena* const types;
    Mode mode;
    ScopePtr globalScope; // sigh.  Needed solely to get at string's metatable.

    TxnLog log;
    ErrorVec errors;
    Location location;
    Variance variance = Covariant;
    CountMismatch::Context ctx = CountMismatch::Arg;

    UnifierCounters* counters;
    UnifierCounters countersData;

    std::shared_ptr<UnifierCounters> counters_DEPRECATED;

    UnifierSharedState& sharedState;

    Unifier(TypeArena* types, Mode mode, ScopePtr globalScope, const Location& location, Variance variance, UnifierSharedState& sharedState);
    Unifier(TypeArena* types, Mode mode, ScopePtr globalScope, const std::vector<std::pair<TypeId, TypeId>>& ownedSeen, const Location& location,
        Variance variance, UnifierSharedState& sharedState, const std::shared_ptr<UnifierCounters>& counters_DEPRECATED = nullptr,
        UnifierCounters* counters = nullptr);
    Unifier(TypeArena* types, Mode mode, ScopePtr globalScope, std::vector<std::pair<TypeId, TypeId>>* sharedSeen, const Location& location,
        Variance variance, UnifierSharedState& sharedState, const std::shared_ptr<UnifierCounters>& counters_DEPRECATED = nullptr,
        UnifierCounters* counters = nullptr);

    // Test whether the two type vars unify.  Never commits the result.
    ErrorVec canUnify(TypeId superTy, TypeId subTy);
    ErrorVec canUnify(TypePackId superTy, TypePackId subTy, bool isFunctionCall = false);

    /** Attempt to unify left with right.
     * Populate the vector errors with any type errors that may arise.
     * Populate the transaction log with the set of TypeIds that need to be reset to undo the unification attempt.
     */
    void tryUnify(TypeId superTy, TypeId subTy, bool isFunctionCall = false, bool isIntersection = false);

private:
    void tryUnify_(TypeId superTy, TypeId subTy, bool isFunctionCall = false, bool isIntersection = false);
    void tryUnifyPrimitives(TypeId superTy, TypeId subTy);
    void tryUnifySingletons(TypeId superTy, TypeId subTy);
    void tryUnifyFunctions(TypeId superTy, TypeId subTy, bool isFunctionCall = false);
    void tryUnifyTables(TypeId left, TypeId right, bool isIntersection = false);
    void DEPRECATED_tryUnifyTables(TypeId left, TypeId right, bool isIntersection = false);
    void tryUnifyFreeTable(TypeId free, TypeId other);
    void tryUnifySealedTables(TypeId left, TypeId right, bool isIntersection);
    void tryUnifyWithMetatable(TypeId metatable, TypeId other, bool reversed);
    void tryUnifyWithClass(TypeId superTy, TypeId subTy, bool reversed);
    void tryUnify(const TableIndexer& superIndexer, const TableIndexer& subIndexer);
    TypeId deeplyOptional(TypeId ty, std::unordered_map<TypeId, TypeId> seen = {});
    void cacheResult(TypeId superTy, TypeId subTy);

public:
    void tryUnify(TypePackId superTy, TypePackId subTy, bool isFunctionCall = false);

private:
    void tryUnify_(TypePackId superTy, TypePackId subTy, bool isFunctionCall = false);
    void tryUnifyVariadics(TypePackId superTy, TypePackId subTy, bool reversed, int subOffset = 0);

    void tryUnifyWithAny(TypeId any, TypeId ty);
    void tryUnifyWithAny(TypePackId any, TypePackId ty);

    std::optional<TypeId> findTablePropertyRespectingMeta(TypeId lhsType, Name name);

public:
    // Report an "infinite type error" if the type "needle" already occurs within "haystack"
    void occursCheck(TypeId needle, TypeId haystack);
    void occursCheck(std::unordered_set<TypeId>& seen_DEPRECATED, DenseHashSet<TypeId>& seen, TypeId needle, TypeId haystack);
    void occursCheck(TypePackId needle, TypePackId haystack);
    void occursCheck(std::unordered_set<TypePackId>& seen_DEPRECATED, DenseHashSet<TypePackId>& seen, TypePackId needle, TypePackId haystack);

    Unifier makeChildUnifier();

private:
    bool isNonstrictMode() const;

    void checkChildUnifierTypeMismatch(const ErrorVec& innerErrors, TypeId wantedType, TypeId givenType);
    void checkChildUnifierTypeMismatch(const ErrorVec& innerErrors, const std::string& prop, TypeId wantedType, TypeId givenType);

    [[noreturn]] void ice(const std::string& message, const Location& location);
    [[noreturn]] void ice(const std::string& message);

    // Remove with FFlagLuauCacheUnifyTableResults
    DenseHashSet<TypeId> tempSeenTy_DEPRECATED{nullptr};
    DenseHashSet<TypePackId> tempSeenTp_DEPRECATED{nullptr};
};

} // namespace Luau
