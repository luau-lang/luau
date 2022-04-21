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

// A substitution which replaces singleton types by their wider types
struct Widen : Substitution
{
    Widen(TypeArena* arena)
        : Substitution(TxnLog::empty(), arena)
    {
    }

    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId ty) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId ty) override;
    bool ignoreChildren(TypeId ty) override;
};

// TODO: Use this more widely.
struct UnifierOptions
{
    bool isFunctionCall = false;
};

struct Unifier
{
    TypeArena* const types;
    Mode mode;

    TxnLog log;
    ErrorVec errors;
    Location location;
    Variance variance = Covariant;
    bool anyIsTop = false; // If true, we consider any to be a top type.  If false, it is a familiar but weird mix of top and bottom all at once.
    CountMismatch::Context ctx = CountMismatch::Arg;

    UnifierSharedState& sharedState;

    Unifier(TypeArena* types, Mode mode, const Location& location, Variance variance, UnifierSharedState& sharedState, TxnLog* parentLog = nullptr);
    Unifier(TypeArena* types, Mode mode, std::vector<std::pair<TypeOrPackId, TypeOrPackId>>* sharedSeen, const Location& location, Variance variance,
        UnifierSharedState& sharedState, TxnLog* parentLog = nullptr);

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
    void tryUnifyUnionWithType(TypeId subTy, const UnionTypeVar* uv, TypeId superTy);
    void tryUnifyTypeWithUnion(TypeId subTy, TypeId superTy, const UnionTypeVar* uv, bool cacheEnabled, bool isFunctionCall);
    void tryUnifyTypeWithIntersection(TypeId subTy, TypeId superTy, const IntersectionTypeVar* uv);
    void tryUnifyIntersectionWithType(TypeId subTy, const IntersectionTypeVar* uv, TypeId superTy, bool cacheEnabled, bool isFunctionCall);
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

    void tryUnifyWithConstrainedSubTypeVar(TypeId subTy, TypeId superTy);
    void tryUnifyWithConstrainedSuperTypeVar(TypeId subTy, TypeId superTy);

public:
    void unifyLowerBound(TypePackId subTy, TypePackId superTy);

    // Report an "infinite type error" if the type "needle" already occurs within "haystack"
    void occursCheck(TypeId needle, TypeId haystack);
    void occursCheck(DenseHashSet<TypeId>& seen, TypeId needle, TypeId haystack);
    void occursCheck(TypePackId needle, TypePackId haystack);
    void occursCheck(DenseHashSet<TypePackId>& seen, TypePackId needle, TypePackId haystack);

    Unifier makeChildUnifier();

    void reportError(TypeError err);

private:
    bool isNonstrictMode() const;

    void checkChildUnifierTypeMismatch(const ErrorVec& innerErrors, TypeId wantedType, TypeId givenType);
    void checkChildUnifierTypeMismatch(const ErrorVec& innerErrors, const std::string& prop, TypeId wantedType, TypeId givenType);

    [[noreturn]] void ice(const std::string& message, const Location& location);
    [[noreturn]] void ice(const std::string& message);

    // Available after regular type pack unification errors
    std::optional<int> firstPackErrorPos;
};

void promoteTypeLevels(TxnLog& log, const TypeArena* arena, TypeLevel minLevel, TypePackId tp);

} // namespace Luau
