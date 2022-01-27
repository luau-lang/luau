// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Unifier.h"

#include "Luau/Common.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/TimeTrace.h"
#include "Luau/VisitTypeVar.h"

#include <algorithm>

LUAU_FASTINT(LuauTypeInferRecursionLimit);
LUAU_FASTINT(LuauTypeInferTypePackLoopLimit);
LUAU_FASTFLAGVARIABLE(LuauCommittingTxnLogFreeTpPromote, false)
LUAU_FASTFLAG(LuauUseCommittingTxnLog)
LUAU_FASTINTVARIABLE(LuauTypeInferIterationLimit, 2000);
LUAU_FASTFLAGVARIABLE(LuauTableSubtypingVariance2, false);
LUAU_FASTFLAGVARIABLE(LuauTableUnificationEarlyTest, false)
LUAU_FASTFLAG(LuauSingletonTypes)
LUAU_FASTFLAG(LuauErrorRecoveryType);
LUAU_FASTFLAG(LuauProperTypeLevels);
LUAU_FASTFLAGVARIABLE(LuauUnifyPackTails, false)

namespace Luau
{

struct PromoteTypeLevels
{
    DEPRECATED_TxnLog& DEPRECATED_log;
    TxnLog& log;
    TypeLevel minLevel;

    explicit PromoteTypeLevels(DEPRECATED_TxnLog& DEPRECATED_log, TxnLog& log, TypeLevel minLevel)
        : DEPRECATED_log(DEPRECATED_log)
        , log(log)
        , minLevel(minLevel)
    {
    }

    template<typename TID, typename T>
    void promote(TID ty, T* t)
    {
        LUAU_ASSERT(t);
        if (minLevel.subsumesStrict(t->level))
        {
            if (FFlag::LuauUseCommittingTxnLog)
            {
                log.changeLevel(ty, minLevel);
            }
            else
            {
                DEPRECATED_log(ty);
                t->level = minLevel;
            }
        }
    }

    template<typename TID>
    void cycle(TID)
    {
    }

    template<typename TID, typename T>
    bool operator()(TID, const T&)
    {
        return true;
    }

    bool operator()(TypeId ty, const FreeTypeVar&)
    {
        // Surprise, it's actually a BoundTypeVar that hasn't been committed yet.
        // Calling getMutable on this will trigger an assertion.
        if (FFlag::LuauUseCommittingTxnLog && !log.is<FreeTypeVar>(ty))
            return true;

        promote(ty, FFlag::LuauUseCommittingTxnLog ? log.getMutable<FreeTypeVar>(ty) : getMutable<FreeTypeVar>(ty));
        return true;
    }

    bool operator()(TypeId ty, const FunctionTypeVar&)
    {
        promote(ty, FFlag::LuauUseCommittingTxnLog ? log.getMutable<FunctionTypeVar>(ty) : getMutable<FunctionTypeVar>(ty));
        return true;
    }

    bool operator()(TypeId ty, const TableTypeVar& ttv)
    {
        if (ttv.state != TableState::Free && ttv.state != TableState::Generic)
            return true;

        promote(ty, FFlag::LuauUseCommittingTxnLog ? log.getMutable<TableTypeVar>(ty) : getMutable<TableTypeVar>(ty));
        return true;
    }

    bool operator()(TypePackId tp, const FreeTypePack&)
    {
        // Surprise, it's actually a BoundTypePack that hasn't been committed yet.
        // Calling getMutable on this will trigger an assertion.
        if (FFlag::LuauCommittingTxnLogFreeTpPromote && FFlag::LuauUseCommittingTxnLog && !log.is<FreeTypePack>(tp))
            return true;

        promote(tp, FFlag::LuauUseCommittingTxnLog ? log.getMutable<FreeTypePack>(tp) : getMutable<FreeTypePack>(tp));
        return true;
    }
};

void promoteTypeLevels(DEPRECATED_TxnLog& DEPRECATED_log, TxnLog& log, TypeLevel minLevel, TypeId ty)
{
    PromoteTypeLevels ptl{DEPRECATED_log, log, minLevel};
    DenseHashSet<void*> seen{nullptr};
    visitTypeVarOnce(ty, ptl, seen);
}

void promoteTypeLevels(DEPRECATED_TxnLog& DEPRECATED_log, TxnLog& log, TypeLevel minLevel, TypePackId tp)
{
    PromoteTypeLevels ptl{DEPRECATED_log, log, minLevel};
    DenseHashSet<void*> seen{nullptr};
    visitTypeVarOnce(tp, ptl, seen);
}

struct SkipCacheForType
{
    SkipCacheForType(const DenseHashMap<TypeId, bool>& skipCacheForType)
        : skipCacheForType(skipCacheForType)
    {
    }

    void cycle(TypeId) {}
    void cycle(TypePackId) {}

    bool operator()(TypeId ty, const FreeTypeVar& ftv)
    {
        result = true;
        return false;
    }

    bool operator()(TypeId ty, const BoundTypeVar& btv)
    {
        result = true;
        return false;
    }

    bool operator()(TypeId ty, const GenericTypeVar& btv)
    {
        result = true;
        return false;
    }

    bool operator()(TypeId ty, const TableTypeVar&)
    {
        TableTypeVar& ttv = *getMutable<TableTypeVar>(ty);

        if (ttv.boundTo)
        {
            result = true;
            return false;
        }

        if (ttv.state != TableState::Sealed)
        {
            result = true;
            return false;
        }

        return true;
    }

    template<typename T>
    bool operator()(TypeId ty, const T& t)
    {
        const bool* prev = skipCacheForType.find(ty);

        if (prev && *prev)
        {
            result = true;
            return false;
        }

        return true;
    }

    template<typename T>
    bool operator()(TypePackId, const T&)
    {
        return true;
    }

    bool operator()(TypePackId tp, const FreeTypePack& ftp)
    {
        result = true;
        return false;
    }

    bool operator()(TypePackId tp, const BoundTypePack& ftp)
    {
        result = true;
        return false;
    }

    bool operator()(TypePackId tp, const GenericTypePack& ftp)
    {
        result = true;
        return false;
    }

    const DenseHashMap<TypeId, bool>& skipCacheForType;
    bool result = false;
};

static std::optional<TypeError> hasUnificationTooComplex(const ErrorVec& errors)
{
    auto isUnificationTooComplex = [](const TypeError& te) {
        return nullptr != get<UnificationTooComplex>(te);
    };

    auto it = std::find_if(errors.begin(), errors.end(), isUnificationTooComplex);
    if (it == errors.end())
        return std::nullopt;
    else
        return *it;
}

// Used for tagged union matching heuristic, returns first singleton type field
static std::optional<std::pair<Luau::Name, const SingletonTypeVar*>> getTableMatchTag(TypeId type)
{
    type = follow(type);

    if (auto ttv = get<TableTypeVar>(type))
    {
        for (auto&& [name, prop] : ttv->props)
        {
            if (auto sing = get<SingletonTypeVar>(follow(prop.type)))
                return {{name, sing}};
        }
    }
    else if (auto mttv = get<MetatableTypeVar>(type))
    {
        return getTableMatchTag(mttv->table);
    }

    return std::nullopt;
}

Unifier::Unifier(TypeArena* types, Mode mode, ScopePtr globalScope, const Location& location, Variance variance, UnifierSharedState& sharedState,
    TxnLog* parentLog)
    : types(types)
    , mode(mode)
    , globalScope(std::move(globalScope))
    , log(parentLog)
    , location(location)
    , variance(variance)
    , sharedState(sharedState)
{
    LUAU_ASSERT(sharedState.iceHandler);
}

Unifier::Unifier(TypeArena* types, Mode mode, ScopePtr globalScope, std::vector<std::pair<TypeId, TypeId>>* sharedSeen, const Location& location,
    Variance variance, UnifierSharedState& sharedState, TxnLog* parentLog)
    : types(types)
    , mode(mode)
    , globalScope(std::move(globalScope))
    , DEPRECATED_log(sharedSeen)
    , log(parentLog, sharedSeen)
    , location(location)
    , variance(variance)
    , sharedState(sharedState)
{
    LUAU_ASSERT(sharedState.iceHandler);
}

void Unifier::tryUnify(TypeId subTy, TypeId superTy, bool isFunctionCall, bool isIntersection)
{
    sharedState.counters.iterationCount = 0;

    tryUnify_(subTy, superTy, isFunctionCall, isIntersection);
}

void Unifier::tryUnify_(TypeId subTy, TypeId superTy, bool isFunctionCall, bool isIntersection)
{
    RecursionLimiter _ra(&sharedState.counters.recursionCount, FInt::LuauTypeInferRecursionLimit);

    ++sharedState.counters.iterationCount;

    if (FInt::LuauTypeInferIterationLimit > 0 && FInt::LuauTypeInferIterationLimit < sharedState.counters.iterationCount)
    {
        reportError(TypeError{location, UnificationTooComplex{}});
        return;
    }

    if (FFlag::LuauUseCommittingTxnLog)
    {
        superTy = log.follow(superTy);
        subTy = log.follow(subTy);
    }
    else
    {
        superTy = follow(superTy);
        subTy = follow(subTy);
    }

    if (superTy == subTy)
        return;

    auto superFree = getMutable<FreeTypeVar>(superTy);
    auto subFree = getMutable<FreeTypeVar>(subTy);

    if (FFlag::LuauUseCommittingTxnLog)
    {
        superFree = log.getMutable<FreeTypeVar>(superTy);
        subFree = log.getMutable<FreeTypeVar>(subTy);
    }

    if (superFree && subFree && superFree->level.subsumes(subFree->level))
    {
        occursCheck(subTy, superTy);

        // The occurrence check might have caused superTy no longer to be a free type
        bool occursFailed = false;
        if (FFlag::LuauUseCommittingTxnLog)
            occursFailed = bool(log.getMutable<ErrorTypeVar>(subTy));
        else
            occursFailed = bool(get<ErrorTypeVar>(subTy));

        if (!occursFailed)
        {
            if (FFlag::LuauUseCommittingTxnLog)
            {
                log.replace(subTy, BoundTypeVar(superTy));
            }
            else
            {
                DEPRECATED_log(subTy);
                *asMutable(subTy) = BoundTypeVar(superTy);
            }
        }

        return;
    }
    else if (superFree && subFree)
    {
        if (!FFlag::LuauErrorRecoveryType && !FFlag::LuauUseCommittingTxnLog)
        {
            DEPRECATED_log(superTy);
            subFree->level = min(subFree->level, superFree->level);
        }

        occursCheck(superTy, subTy);

        bool occursFailed = false;
        if (FFlag::LuauUseCommittingTxnLog)
            occursFailed = bool(log.getMutable<ErrorTypeVar>(superTy));
        else
            occursFailed = bool(get<ErrorTypeVar>(superTy));

        if (!FFlag::LuauErrorRecoveryType && !FFlag::LuauUseCommittingTxnLog)
        {
            *asMutable(superTy) = BoundTypeVar(subTy);
            return;
        }

        if (!occursFailed)
        {
            if (FFlag::LuauUseCommittingTxnLog)
            {
                if (superFree->level.subsumes(subFree->level))
                {
                    log.changeLevel(subTy, superFree->level);
                }

                log.replace(superTy, BoundTypeVar(subTy));
            }
            else
            {
                DEPRECATED_log(superTy);
                *asMutable(superTy) = BoundTypeVar(subTy);
                subFree->level = min(subFree->level, superFree->level);
            }
        }

        return;
    }
    else if (superFree)
    {
        occursCheck(superTy, subTy);
        bool occursFailed = false;
        if (FFlag::LuauUseCommittingTxnLog)
            occursFailed = bool(log.getMutable<ErrorTypeVar>(superTy));
        else
            occursFailed = bool(get<ErrorTypeVar>(superTy));

        TypeLevel superLevel = superFree->level;

        // Unification can't change the level of a generic.
        auto subGeneric = FFlag::LuauUseCommittingTxnLog ? log.getMutable<GenericTypeVar>(subTy) : get<GenericTypeVar>(subTy);
        if (subGeneric && !subGeneric->level.subsumes(superLevel))
        {
            // TODO: a more informative error message? CLI-39912
            reportError(TypeError{location, GenericError{"Generic subtype escaping scope"}});
            return;
        }

        // The occurrence check might have caused superTy no longer to be a free type
        if (!occursFailed)
        {
            if (FFlag::LuauUseCommittingTxnLog)
            {
                promoteTypeLevels(DEPRECATED_log, log, superLevel, subTy);
                log.replace(superTy, BoundTypeVar(subTy));
            }
            else
            {
                if (FFlag::LuauProperTypeLevels)
                    promoteTypeLevels(DEPRECATED_log, log, superLevel, subTy);
                else if (auto subLevel = getMutableLevel(subTy))
                {
                    if (!subLevel->subsumes(superFree->level))
                        *subLevel = superFree->level;
                }

                DEPRECATED_log(superTy);
                *asMutable(superTy) = BoundTypeVar(subTy);
            }
        }

        return;
    }
    else if (subFree)
    {
        TypeLevel subLevel = subFree->level;

        occursCheck(subTy, superTy);
        bool occursFailed = false;
        if (FFlag::LuauUseCommittingTxnLog)
            occursFailed = bool(log.getMutable<ErrorTypeVar>(subTy));
        else
            occursFailed = bool(get<ErrorTypeVar>(subTy));

        // Unification can't change the level of a generic.
        auto superGeneric = FFlag::LuauUseCommittingTxnLog ? log.getMutable<GenericTypeVar>(superTy) : get<GenericTypeVar>(superTy);
        if (superGeneric && !superGeneric->level.subsumes(subFree->level))
        {
            // TODO: a more informative error message? CLI-39912
            reportError(TypeError{location, GenericError{"Generic supertype escaping scope"}});
            return;
        }

        if (!occursFailed)
        {
            if (FFlag::LuauUseCommittingTxnLog)
            {
                promoteTypeLevels(DEPRECATED_log, log, subLevel, superTy);
                log.replace(subTy, BoundTypeVar(superTy));
            }
            else
            {
                if (FFlag::LuauProperTypeLevels)
                    promoteTypeLevels(DEPRECATED_log, log, subLevel, superTy);
                else if (auto superLevel = getMutableLevel(superTy))
                {
                    if (!superLevel->subsumes(subFree->level))
                    {
                        DEPRECATED_log(superTy);
                        *superLevel = subFree->level;
                    }
                }

                DEPRECATED_log(subTy);
                *asMutable(subTy) = BoundTypeVar(superTy);
            }
        }

        return;
    }

    if (get<ErrorTypeVar>(superTy) || get<AnyTypeVar>(superTy))
        return tryUnifyWithAny(subTy, superTy);

    if (get<ErrorTypeVar>(subTy) || get<AnyTypeVar>(subTy))
        return tryUnifyWithAny(superTy, subTy);

    bool cacheEnabled = !isFunctionCall && !isIntersection;
    auto& cache = sharedState.cachedUnify;

    // What if the types are immutable and we proved their relation before
    if (cacheEnabled && cache.contains({superTy, subTy}) && (variance == Covariant || cache.contains({subTy, superTy})))
        return;

    // If we have seen this pair of types before, we are currently recursing into cyclic types.
    // Here, we assume that the types unify.  If they do not, we will find out as we roll back
    // the stack.

    if (FFlag::LuauUseCommittingTxnLog)
    {
        if (log.haveSeen(superTy, subTy))
            return;

        log.pushSeen(superTy, subTy);
    }
    else
    {
        if (DEPRECATED_log.haveSeen(superTy, subTy))
            return;

        DEPRECATED_log.pushSeen(superTy, subTy);
    }

    if (const UnionTypeVar* uv = FFlag::LuauUseCommittingTxnLog ? log.getMutable<UnionTypeVar>(subTy) : get<UnionTypeVar>(subTy))
    {
        // A | B <: T if A <: T and B <: T
        bool failed = false;
        std::optional<TypeError> unificationTooComplex;
        std::optional<TypeError> firstFailedOption;

        size_t count = uv->options.size();
        size_t i = 0;

        for (TypeId type : uv->options)
        {
            Unifier innerState = makeChildUnifier();
            innerState.tryUnify_(type, superTy);

            if (auto e = hasUnificationTooComplex(innerState.errors))
                unificationTooComplex = e;
            else if (!innerState.errors.empty())
            {
                // 'nil' option is skipped from extended report because we present the type in a special way - 'T?'
                if (!firstFailedOption && !isNil(type))
                    firstFailedOption = {innerState.errors.front()};

                failed = true;
            }

            if (FFlag::LuauUseCommittingTxnLog)
            {
                if (i == count - 1)
                {
                    log.concat(std::move(innerState.log));
                }
            }
            else
            {
                if (i != count - 1)
                {
                    innerState.DEPRECATED_log.rollback();
                }
                else
                {
                    DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));
                }
            }

            ++i;
        }

        if (unificationTooComplex)
            reportError(*unificationTooComplex);
        else if (failed)
        {
            if (firstFailedOption)
                reportError(TypeError{location, TypeMismatch{superTy, subTy, "Not all union options are compatible.", *firstFailedOption}});
            else
                reportError(TypeError{location, TypeMismatch{superTy, subTy}});
        }
    }
    else if (const UnionTypeVar* uv = FFlag::LuauUseCommittingTxnLog ? log.getMutable<UnionTypeVar>(superTy) : get<UnionTypeVar>(superTy))
    {
        // T <: A | B if T <: A or T <: B
        bool found = false;
        std::optional<TypeError> unificationTooComplex;

        size_t failedOptionCount = 0;
        std::optional<TypeError> failedOption;

        bool foundHeuristic = false;
        size_t startIndex = 0;

        if (const std::string* subName = getName(subTy))
        {
            for (size_t i = 0; i < uv->options.size(); ++i)
            {
                const std::string* optionName = getName(uv->options[i]);
                if (optionName && *optionName == *subName)
                {
                    foundHeuristic = true;
                    startIndex = i;
                    break;
                }
            }
        }

        if (auto subMatchTag = getTableMatchTag(subTy))
        {
            for (size_t i = 0; i < uv->options.size(); ++i)
            {
                auto optionMatchTag = getTableMatchTag(uv->options[i]);
                if (optionMatchTag && optionMatchTag->first == subMatchTag->first && *optionMatchTag->second == *subMatchTag->second)
                {
                    foundHeuristic = true;
                    startIndex = i;
                    break;
                }
            }
        }

        if (!foundHeuristic && cacheEnabled)
        {
            for (size_t i = 0; i < uv->options.size(); ++i)
            {
                TypeId type = uv->options[i];

                if (cache.contains({type, subTy}) && (variance == Covariant || cache.contains({subTy, type})))
                {
                    startIndex = i;
                    break;
                }
            }
        }

        for (size_t i = 0; i < uv->options.size(); ++i)
        {
            TypeId type = uv->options[(i + startIndex) % uv->options.size()];
            Unifier innerState = makeChildUnifier();
            innerState.tryUnify_(subTy, type, isFunctionCall);

            if (innerState.errors.empty())
            {
                found = true;
                if (FFlag::LuauUseCommittingTxnLog)
                    log.concat(std::move(innerState.log));
                else
                    DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));

                break;
            }
            else if (auto e = hasUnificationTooComplex(innerState.errors))
            {
                unificationTooComplex = e;
            }
            else if (!isNil(type))
            {
                failedOptionCount++;

                if (!failedOption)
                    failedOption = {innerState.errors.front()};
            }

            if (!FFlag::LuauUseCommittingTxnLog)
                innerState.DEPRECATED_log.rollback();
        }

        if (unificationTooComplex)
        {
            reportError(*unificationTooComplex);
        }
        else if (!found)
        {
            if ((failedOptionCount == 1 || foundHeuristic) && failedOption)
                reportError(
                    TypeError{location, TypeMismatch{superTy, subTy, "None of the union options are compatible. For example:", *failedOption}});
            else
                reportError(TypeError{location, TypeMismatch{superTy, subTy, "none of the union options are compatible"}});
        }
    }
    else if (const IntersectionTypeVar* uv =
                 FFlag::LuauUseCommittingTxnLog ? log.getMutable<IntersectionTypeVar>(superTy) : get<IntersectionTypeVar>(superTy))
    {
        std::optional<TypeError> unificationTooComplex;
        std::optional<TypeError> firstFailedOption;

        // T <: A & B if A <: T and B <: T
        for (TypeId type : uv->parts)
        {
            Unifier innerState = makeChildUnifier();
            innerState.tryUnify_(subTy, type, /*isFunctionCall*/ false, /*isIntersection*/ true);

            if (auto e = hasUnificationTooComplex(innerState.errors))
                unificationTooComplex = e;
            else if (!innerState.errors.empty())
            {
                if (!firstFailedOption)
                    firstFailedOption = {innerState.errors.front()};
            }

            if (FFlag::LuauUseCommittingTxnLog)
                log.concat(std::move(innerState.log));
            else
                DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));
        }

        if (unificationTooComplex)
            reportError(*unificationTooComplex);
        else if (firstFailedOption)
            reportError(TypeError{location, TypeMismatch{superTy, subTy, "Not all intersection parts are compatible.", *firstFailedOption}});
    }
    else if (const IntersectionTypeVar* uv =
                 FFlag::LuauUseCommittingTxnLog ? log.getMutable<IntersectionTypeVar>(subTy) : get<IntersectionTypeVar>(subTy))
    {
        // A & B <: T if T <: A or T <: B
        bool found = false;
        std::optional<TypeError> unificationTooComplex;

        size_t startIndex = 0;

        if (cacheEnabled)
        {
            for (size_t i = 0; i < uv->parts.size(); ++i)
            {
                TypeId type = uv->parts[i];

                if (cache.contains({superTy, type}) && (variance == Covariant || cache.contains({type, superTy})))
                {
                    startIndex = i;
                    break;
                }
            }
        }

        for (size_t i = 0; i < uv->parts.size(); ++i)
        {
            TypeId type = uv->parts[(i + startIndex) % uv->parts.size()];
            Unifier innerState = makeChildUnifier();
            innerState.tryUnify_(type, superTy, isFunctionCall);

            if (innerState.errors.empty())
            {
                found = true;
                if (FFlag::LuauUseCommittingTxnLog)
                    log.concat(std::move(innerState.log));
                else
                    DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));
                break;
            }
            else if (auto e = hasUnificationTooComplex(innerState.errors))
            {
                unificationTooComplex = e;
            }

            if (!FFlag::LuauUseCommittingTxnLog)
                innerState.DEPRECATED_log.rollback();
        }

        if (unificationTooComplex)
            reportError(*unificationTooComplex);
        else if (!found)
        {
            reportError(TypeError{location, TypeMismatch{superTy, subTy, "none of the intersection parts are compatible"}});
        }
    }
    else if ((FFlag::LuauUseCommittingTxnLog && log.getMutable<PrimitiveTypeVar>(superTy) && log.getMutable<PrimitiveTypeVar>(subTy)) ||
             (!FFlag::LuauUseCommittingTxnLog && get<PrimitiveTypeVar>(superTy) && get<PrimitiveTypeVar>(subTy)))
        tryUnifyPrimitives(subTy, superTy);

    else if (FFlag::LuauSingletonTypes &&
             ((FFlag::LuauUseCommittingTxnLog ? log.getMutable<PrimitiveTypeVar>(superTy) : get<PrimitiveTypeVar>(superTy)) ||
                 (FFlag::LuauUseCommittingTxnLog ? log.getMutable<SingletonTypeVar>(superTy) : get<SingletonTypeVar>(superTy))) &&
             (FFlag::LuauUseCommittingTxnLog ? log.getMutable<SingletonTypeVar>(subTy) : get<SingletonTypeVar>(subTy)))
        tryUnifySingletons(subTy, superTy);

    else if ((FFlag::LuauUseCommittingTxnLog && log.getMutable<FunctionTypeVar>(superTy) && log.getMutable<FunctionTypeVar>(subTy)) ||
             (!FFlag::LuauUseCommittingTxnLog && get<FunctionTypeVar>(superTy) && get<FunctionTypeVar>(subTy)))
        tryUnifyFunctions(subTy, superTy, isFunctionCall);

    else if ((FFlag::LuauUseCommittingTxnLog && log.getMutable<TableTypeVar>(superTy) && log.getMutable<TableTypeVar>(subTy)) ||
             (!FFlag::LuauUseCommittingTxnLog && get<TableTypeVar>(superTy) && get<TableTypeVar>(subTy)))
    {
        tryUnifyTables(subTy, superTy, isIntersection);

        if (cacheEnabled && errors.empty())
            cacheResult(subTy, superTy);
    }

    // tryUnifyWithMetatable assumes its first argument is a MetatableTypeVar. The check is otherwise symmetrical.
    else if ((FFlag::LuauUseCommittingTxnLog && log.getMutable<MetatableTypeVar>(superTy)) ||
             (!FFlag::LuauUseCommittingTxnLog && get<MetatableTypeVar>(superTy)))
        tryUnifyWithMetatable(subTy, superTy, /*reversed*/ false);
    else if ((FFlag::LuauUseCommittingTxnLog && log.getMutable<MetatableTypeVar>(subTy)) ||
             (!FFlag::LuauUseCommittingTxnLog && get<MetatableTypeVar>(subTy)))
        tryUnifyWithMetatable(superTy, subTy, /*reversed*/ true);

    else if ((FFlag::LuauUseCommittingTxnLog && log.getMutable<ClassTypeVar>(superTy)) ||
             (!FFlag::LuauUseCommittingTxnLog && get<ClassTypeVar>(superTy)))
        tryUnifyWithClass(subTy, superTy, /*reversed*/ false);

    // Unification of nonclasses with classes is almost, but not quite symmetrical.
    // The order in which we perform this test is significant in the case that both types are classes.
    else if ((FFlag::LuauUseCommittingTxnLog && log.getMutable<ClassTypeVar>(subTy)) || (!FFlag::LuauUseCommittingTxnLog && get<ClassTypeVar>(subTy)))
        tryUnifyWithClass(subTy, superTy, /*reversed*/ true);

    else
        reportError(TypeError{location, TypeMismatch{superTy, subTy}});

    if (FFlag::LuauUseCommittingTxnLog)
        log.popSeen(superTy, subTy);
    else
        DEPRECATED_log.popSeen(superTy, subTy);
}

void Unifier::cacheResult(TypeId subTy, TypeId superTy)
{
    bool* superTyInfo = sharedState.skipCacheForType.find(superTy);

    if (superTyInfo && *superTyInfo)
        return;

    bool* subTyInfo = sharedState.skipCacheForType.find(subTy);

    if (subTyInfo && *subTyInfo)
        return;

    auto skipCacheFor = [this](TypeId ty) {
        SkipCacheForType visitor{sharedState.skipCacheForType};
        visitTypeVarOnce(ty, visitor, sharedState.seenAny);

        sharedState.skipCacheForType[ty] = visitor.result;

        return visitor.result;
    };

    if (!superTyInfo && skipCacheFor(superTy))
        return;

    if (!subTyInfo && skipCacheFor(subTy))
        return;

    sharedState.cachedUnify.insert({superTy, subTy});

    if (variance == Invariant)
        sharedState.cachedUnify.insert({subTy, superTy});
}

struct DEPRECATED_WeirdIter
{
    TypePackId packId;
    const TypePack* pack;
    size_t index;
    bool growing;
    TypeLevel level;

    DEPRECATED_WeirdIter(TypePackId packId)
        : packId(packId)
        , pack(get<TypePack>(packId))
        , index(0)
        , growing(false)
    {
        while (pack && pack->head.empty() && pack->tail)
        {
            packId = *pack->tail;
            pack = get<TypePack>(packId);
        }
    }

    DEPRECATED_WeirdIter(const DEPRECATED_WeirdIter&) = default;

    const TypeId& operator*()
    {
        LUAU_ASSERT(good());
        return pack->head[index];
    }

    bool good() const
    {
        return pack != nullptr && index < pack->head.size();
    }

    bool advance()
    {
        if (!pack)
            return good();

        if (index < pack->head.size())
            ++index;

        if (growing || index < pack->head.size())
            return good();

        if (pack->tail)
        {
            packId = follow(*pack->tail);
            pack = get<TypePack>(packId);
            index = 0;
        }

        return good();
    }

    bool canGrow() const
    {
        return nullptr != get<Unifiable::Free>(packId);
    }

    void grow(TypePackId newTail)
    {
        LUAU_ASSERT(canGrow());
        level = get<Unifiable::Free>(packId)->level;
        *asMutable(packId) = Unifiable::Bound<TypePackId>(newTail);
        packId = newTail;
        pack = get<TypePack>(newTail);
        index = 0;
        growing = true;
    }
};

struct WeirdIter
{
    TypePackId packId;
    TxnLog& log;
    TypePack* pack;
    size_t index;
    bool growing;
    TypeLevel level;

    WeirdIter(TypePackId packId, TxnLog& log)
        : packId(packId)
        , log(log)
        , pack(log.getMutable<TypePack>(packId))
        , index(0)
        , growing(false)
    {
        while (pack && pack->head.empty() && pack->tail)
        {
            packId = *pack->tail;
            pack = log.getMutable<TypePack>(packId);
        }
    }

    WeirdIter(const WeirdIter&) = default;

    TypeId& operator*()
    {
        LUAU_ASSERT(good());
        return pack->head[index];
    }

    bool good() const
    {
        return pack != nullptr && index < pack->head.size();
    }

    bool advance()
    {
        if (!pack)
            return good();

        if (index < pack->head.size())
            ++index;

        if (growing || index < pack->head.size())
            return good();

        if (pack->tail)
        {
            packId = log.follow(*pack->tail);
            pack = log.getMutable<TypePack>(packId);
            index = 0;
        }

        return good();
    }

    bool canGrow() const
    {
        return nullptr != log.getMutable<Unifiable::Free>(packId);
    }

    void grow(TypePackId newTail)
    {
        LUAU_ASSERT(canGrow());
        LUAU_ASSERT(log.getMutable<TypePack>(newTail));

        level = log.getMutable<Unifiable::Free>(packId)->level;
        log.replace(packId, Unifiable::Bound<TypePackId>(newTail));
        packId = newTail;
        pack = log.getMutable<TypePack>(newTail);
        index = 0;
        growing = true;
    }

    void pushType(TypeId ty)
    {
        LUAU_ASSERT(pack);
        PendingTypePack* pendingPack = log.queue(packId);
        if (TypePack* pending = getMutable<TypePack>(pendingPack))
        {
            pending->head.push_back(ty);
            // We've potentially just replaced the TypePack* that we need to look
            // in. We need to replace pack.
            pack = pending;
        }
        else
        {
            LUAU_ASSERT(!"Pending state for this pack was not a TypePack");
        }
    }
};

ErrorVec Unifier::canUnify(TypeId subTy, TypeId superTy)
{
    Unifier s = makeChildUnifier();
    s.tryUnify_(subTy, superTy);

    if (!FFlag::LuauUseCommittingTxnLog)
        s.DEPRECATED_log.rollback();

    return s.errors;
}

ErrorVec Unifier::canUnify(TypePackId subTy, TypePackId superTy, bool isFunctionCall)
{
    Unifier s = makeChildUnifier();
    s.tryUnify_(subTy, superTy, isFunctionCall);

    if (!FFlag::LuauUseCommittingTxnLog)
        s.DEPRECATED_log.rollback();

    return s.errors;
}

void Unifier::tryUnify(TypePackId subTp, TypePackId superTp, bool isFunctionCall)
{
    sharedState.counters.iterationCount = 0;

    tryUnify_(subTp, superTp, isFunctionCall);
}

static std::pair<std::vector<TypeId>, std::optional<TypePackId>> logAwareFlatten(TypePackId tp, const TxnLog& log)
{
    tp = log.follow(tp);

    std::vector<TypeId> flattened;
    std::optional<TypePackId> tail = std::nullopt;

    TypePackIterator it(tp, &log);

    for (; it != end(tp); ++it)
    {
        flattened.push_back(*it);
    }

    tail = it.tail();

    return {flattened, tail};
}

/*
 * This is quite tricky: we are walking two rope-like structures and unifying corresponding elements.
 * If one is longer than the other, but the short end is free, we grow it to the required length.
 */
void Unifier::tryUnify_(TypePackId subTp, TypePackId superTp, bool isFunctionCall)
{
    RecursionLimiter _ra(&sharedState.counters.recursionCount, FInt::LuauTypeInferRecursionLimit);

    ++sharedState.counters.iterationCount;

    if (FInt::LuauTypeInferIterationLimit > 0 && FInt::LuauTypeInferIterationLimit < sharedState.counters.iterationCount)
    {
        reportError(TypeError{location, UnificationTooComplex{}});
        return;
    }

    if (FFlag::LuauUseCommittingTxnLog)
    {
        superTp = log.follow(superTp);
        subTp = log.follow(subTp);

        while (auto tp = log.getMutable<TypePack>(subTp))
        {
            if (tp->head.empty() && tp->tail)
                subTp = log.follow(*tp->tail);
            else
                break;
        }

        while (auto tp = log.getMutable<TypePack>(superTp))
        {
            if (tp->head.empty() && tp->tail)
                superTp = log.follow(*tp->tail);
            else
                break;
        }

        if (superTp == subTp)
            return;

        if (log.getMutable<Unifiable::Free>(superTp))
        {
            occursCheck(superTp, subTp);

            if (!log.getMutable<ErrorTypeVar>(superTp))
            {
                log.replace(superTp, Unifiable::Bound<TypePackId>(subTp));
            }
        }
        else if (log.getMutable<Unifiable::Free>(subTp))
        {
            occursCheck(subTp, superTp);

            if (!log.getMutable<ErrorTypeVar>(subTp))
            {
                log.replace(subTp, Unifiable::Bound<TypePackId>(superTp));
            }
        }
        else if (log.getMutable<Unifiable::Error>(superTp))
            tryUnifyWithAny(subTp, superTp);
        else if (log.getMutable<Unifiable::Error>(subTp))
            tryUnifyWithAny(superTp, subTp);
        else if (log.getMutable<VariadicTypePack>(superTp))
            tryUnifyVariadics(subTp, superTp, false);
        else if (log.getMutable<VariadicTypePack>(subTp))
            tryUnifyVariadics(superTp, subTp, true);
        else if (log.getMutable<TypePack>(superTp) && log.getMutable<TypePack>(subTp))
        {
            auto superTpv = log.getMutable<TypePack>(superTp);
            auto subTpv = log.getMutable<TypePack>(subTp);

            // If the size of two heads does not match, but both packs have free tail
            // We set the sentinel variable to say so to avoid growing it forever.
            auto [superTypes, superTail] = logAwareFlatten(superTp, log);
            auto [subTypes, subTail] = logAwareFlatten(subTp, log);

            bool noInfiniteGrowth =
                (superTypes.size() != subTypes.size()) && (superTail && get<FreeTypePack>(*superTail)) && (subTail && get<FreeTypePack>(*subTail));

            auto superIter = WeirdIter(superTp, log);
            auto subIter = WeirdIter(subTp, log);

            auto mkFreshType = [this](TypeLevel level) {
                return types->freshType(level);
            };

            const TypePackId emptyTp = types->addTypePack(TypePack{{}, std::nullopt});

            int loopCount = 0;

            do
            {
                if (FInt::LuauTypeInferTypePackLoopLimit > 0 && loopCount >= FInt::LuauTypeInferTypePackLoopLimit)
                    ice("Detected possibly infinite TypePack growth");

                ++loopCount;

                if (superIter.good() && subIter.growing)
                {
                    subIter.pushType(mkFreshType(subIter.level));
                }

                if (subIter.good() && superIter.growing)
                {
                    superIter.pushType(mkFreshType(superIter.level));
                }

                if (superIter.good() && subIter.good())
                {
                    tryUnify_(*subIter, *superIter);

                    if (!errors.empty() && !firstPackErrorPos)
                        firstPackErrorPos = loopCount;

                    superIter.advance();
                    subIter.advance();
                    continue;
                }

                // If both are at the end, we're done
                if (!superIter.good() && !subIter.good())
                {
                    if (FFlag::LuauUnifyPackTails && subTpv->tail && superTpv->tail)
                    {
                        tryUnify_(*subTpv->tail, *superTpv->tail);
                        break;
                    }

                    const bool lFreeTail = superTpv->tail && log.getMutable<FreeTypePack>(log.follow(*superTpv->tail)) != nullptr;
                    const bool rFreeTail = subTpv->tail && log.getMutable<FreeTypePack>(log.follow(*subTpv->tail)) != nullptr;
                    if (!FFlag::LuauUnifyPackTails && lFreeTail && rFreeTail)
                        tryUnify_(*subTpv->tail, *superTpv->tail);
                    else if (lFreeTail)
                        tryUnify_(emptyTp, *superTpv->tail);
                    else if (rFreeTail)
                        tryUnify_(emptyTp, *subTpv->tail);

                    break;
                }

                // If both tails are free, bind one to the other and call it a day
                if (superIter.canGrow() && subIter.canGrow())
                    return tryUnify_(*subIter.pack->tail, *superIter.pack->tail);

                // If just one side is free on its tail, grow it to fit the other side.
                // FIXME: The tail-most tail of the growing pack should be the same as the tail-most tail of the non-growing pack.
                if (superIter.canGrow())
                    superIter.grow(types->addTypePack(TypePackVar(TypePack{})));
                else if (subIter.canGrow())
                    subIter.grow(types->addTypePack(TypePackVar(TypePack{})));
                else
                {
                    // A union type including nil marks an optional argument
                    if (superIter.good() && isOptional(*superIter))
                    {
                        superIter.advance();
                        continue;
                    }
                    else if (subIter.good() && isOptional(*subIter))
                    {
                        subIter.advance();
                        continue;
                    }

                    // In nonstrict mode, any also marks an optional argument.
                    else if (superIter.good() && isNonstrictMode() && log.getMutable<AnyTypeVar>(log.follow(*superIter)))
                    {
                        superIter.advance();
                        continue;
                    }

                    if (log.getMutable<VariadicTypePack>(superIter.packId))
                    {
                        tryUnifyVariadics(subIter.packId, superIter.packId, false, int(subIter.index));
                        return;
                    }

                    if (log.getMutable<VariadicTypePack>(subIter.packId))
                    {
                        tryUnifyVariadics(superIter.packId, subIter.packId, true, int(superIter.index));
                        return;
                    }

                    if (!isFunctionCall && subIter.good())
                    {
                        // Sometimes it is ok to pass too many arguments
                        return;
                    }

                    // This is a bit weird because we don't actually know expected vs actual.  We just know
                    // subtype vs supertype.  If we are checking the values returned by a function, we swap
                    // these to produce the expected error message.
                    size_t expectedSize = size(superTp);
                    size_t actualSize = size(subTp);
                    if (ctx == CountMismatch::Result)
                        std::swap(expectedSize, actualSize);
                    reportError(TypeError{location, CountMismatch{expectedSize, actualSize, ctx}});

                    while (superIter.good())
                    {
                        tryUnify_(*superIter, getSingletonTypes().errorRecoveryType());
                        superIter.advance();
                    }

                    while (subIter.good())
                    {
                        tryUnify_(*subIter, getSingletonTypes().errorRecoveryType());
                        subIter.advance();
                    }

                    return;
                }

            } while (!noInfiniteGrowth);
        }
        else
        {
            reportError(TypeError{location, GenericError{"Failed to unify type packs"}});
        }
    }
    else
    {
        superTp = follow(superTp);
        subTp = follow(subTp);

        while (auto tp = get<TypePack>(subTp))
        {
            if (tp->head.empty() && tp->tail)
                subTp = follow(*tp->tail);
            else
                break;
        }

        while (auto tp = get<TypePack>(superTp))
        {
            if (tp->head.empty() && tp->tail)
                superTp = follow(*tp->tail);
            else
                break;
        }

        if (superTp == subTp)
            return;

        if (get<Unifiable::Free>(superTp))
        {
            occursCheck(superTp, subTp);

            if (!get<ErrorTypeVar>(superTp))
            {
                DEPRECATED_log(superTp);
                *asMutable(superTp) = Unifiable::Bound<TypePackId>(subTp);
            }
        }
        else if (get<Unifiable::Free>(subTp))
        {
            occursCheck(subTp, superTp);

            if (!get<ErrorTypeVar>(subTp))
            {
                DEPRECATED_log(subTp);
                *asMutable(subTp) = Unifiable::Bound<TypePackId>(superTp);
            }
        }

        else if (get<Unifiable::Error>(superTp))
            tryUnifyWithAny(subTp, superTp);

        else if (get<Unifiable::Error>(subTp))
            tryUnifyWithAny(superTp, subTp);

        else if (get<VariadicTypePack>(superTp))
            tryUnifyVariadics(subTp, superTp, false);
        else if (get<VariadicTypePack>(subTp))
            tryUnifyVariadics(superTp, subTp, true);

        else if (get<TypePack>(superTp) && get<TypePack>(subTp))
        {
            auto superTpv = get<TypePack>(superTp);
            auto subTpv = get<TypePack>(subTp);

            // If the size of two heads does not match, but both packs have free tail
            // We set the sentinel variable to say so to avoid growing it forever.
            auto [superTypes, superTail] = flatten(superTp);
            auto [subTypes, subTail] = flatten(subTp);

            bool noInfiniteGrowth =
                (superTypes.size() != subTypes.size()) && (superTail && get<FreeTypePack>(*superTail)) && (subTail && get<FreeTypePack>(*subTail));

            auto superIter = DEPRECATED_WeirdIter{superTp};
            auto subIter = DEPRECATED_WeirdIter{subTp};

            auto mkFreshType = [this](TypeLevel level) {
                return types->freshType(level);
            };

            const TypePackId emptyTp = types->addTypePack(TypePack{{}, std::nullopt});

            int loopCount = 0;

            do
            {
                if (FInt::LuauTypeInferTypePackLoopLimit > 0 && loopCount >= FInt::LuauTypeInferTypePackLoopLimit)
                    ice("Detected possibly infinite TypePack growth");

                ++loopCount;

                if (superIter.good() && subIter.growing)
                    asMutable(subIter.pack)->head.push_back(mkFreshType(subIter.level));

                if (subIter.good() && superIter.growing)
                    asMutable(superIter.pack)->head.push_back(mkFreshType(superIter.level));

                if (superIter.good() && subIter.good())
                {
                    tryUnify_(*subIter, *superIter);

                    if (!errors.empty() && !firstPackErrorPos)
                        firstPackErrorPos = loopCount;

                    superIter.advance();
                    subIter.advance();
                    continue;
                }

                // If both are at the end, we're done
                if (!superIter.good() && !subIter.good())
                {
                    if (FFlag::LuauUnifyPackTails && subTpv->tail && superTpv->tail)
                    {
                        tryUnify_(*subTpv->tail, *superTpv->tail);
                        break;
                    }

                    const bool lFreeTail = superTpv->tail && get<FreeTypePack>(follow(*superTpv->tail)) != nullptr;
                    const bool rFreeTail = subTpv->tail && get<FreeTypePack>(follow(*subTpv->tail)) != nullptr;
                    if (!FFlag::LuauUnifyPackTails && lFreeTail && rFreeTail)
                        tryUnify_(*subTpv->tail, *superTpv->tail);
                    else if (lFreeTail)
                        tryUnify_(emptyTp, *superTpv->tail);
                    else if (rFreeTail)
                        tryUnify_(emptyTp, *subTpv->tail);

                    break;
                }

                // If both tails are free, bind one to the other and call it a day
                if (superIter.canGrow() && subIter.canGrow())
                    return tryUnify_(*subIter.pack->tail, *superIter.pack->tail);

                // If just one side is free on its tail, grow it to fit the other side.
                // FIXME: The tail-most tail of the growing pack should be the same as the tail-most tail of the non-growing pack.
                if (superIter.canGrow())
                    superIter.grow(types->addTypePack(TypePackVar(TypePack{})));

                else if (subIter.canGrow())
                    subIter.grow(types->addTypePack(TypePackVar(TypePack{})));

                else
                {
                    // A union type including nil marks an optional argument
                    if (superIter.good() && isOptional(*superIter))
                    {
                        superIter.advance();
                        continue;
                    }
                    else if (subIter.good() && isOptional(*subIter))
                    {
                        subIter.advance();
                        continue;
                    }

                    // In nonstrict mode, any also marks an optional argument.
                    else if (superIter.good() && isNonstrictMode() && get<AnyTypeVar>(follow(*superIter)))
                    {
                        superIter.advance();
                        continue;
                    }

                    if (get<VariadicTypePack>(superIter.packId))
                    {
                        tryUnifyVariadics(subIter.packId, superIter.packId, false, int(subIter.index));
                        return;
                    }

                    if (get<VariadicTypePack>(subIter.packId))
                    {
                        tryUnifyVariadics(superIter.packId, subIter.packId, true, int(superIter.index));
                        return;
                    }

                    if (!isFunctionCall && subIter.good())
                    {
                        // Sometimes it is ok to pass too many arguments
                        return;
                    }

                    // This is a bit weird because we don't actually know expected vs actual.  We just know
                    // subtype vs supertype.  If we are checking the values returned by a function, we swap
                    // these to produce the expected error message.
                    size_t expectedSize = size(superTp);
                    size_t actualSize = size(subTp);
                    if (ctx == CountMismatch::Result)
                        std::swap(expectedSize, actualSize);
                    reportError(TypeError{location, CountMismatch{expectedSize, actualSize, ctx}});

                    while (superIter.good())
                    {
                        tryUnify_(*superIter, getSingletonTypes().errorRecoveryType());
                        superIter.advance();
                    }

                    while (subIter.good())
                    {
                        tryUnify_(*subIter, getSingletonTypes().errorRecoveryType());
                        subIter.advance();
                    }

                    return;
                }

            } while (!noInfiniteGrowth);
        }
        else
        {
            reportError(TypeError{location, GenericError{"Failed to unify type packs"}});
        }
    }
}

void Unifier::tryUnifyPrimitives(TypeId subTy, TypeId superTy)
{
    const PrimitiveTypeVar* superPrim = get<PrimitiveTypeVar>(superTy);
    const PrimitiveTypeVar* subPrim = get<PrimitiveTypeVar>(subTy);
    if (!superPrim || !subPrim)
        ice("passed non primitive types to unifyPrimitives");

    if (superPrim->type != subPrim->type)
        reportError(TypeError{location, TypeMismatch{superTy, subTy}});
}

void Unifier::tryUnifySingletons(TypeId subTy, TypeId superTy)
{
    const PrimitiveTypeVar* superPrim = get<PrimitiveTypeVar>(superTy);
    const SingletonTypeVar* superSingleton = get<SingletonTypeVar>(superTy);
    const SingletonTypeVar* subSingleton = get<SingletonTypeVar>(subTy);

    if ((!superPrim && !superSingleton) || !subSingleton)
        ice("passed non singleton/primitive types to unifySingletons");

    if (superSingleton && *superSingleton == *subSingleton)
        return;

    if (superPrim && superPrim->type == PrimitiveTypeVar::Boolean && get<BooleanSingleton>(subSingleton) && variance == Covariant)
        return;

    if (superPrim && superPrim->type == PrimitiveTypeVar::String && get<StringSingleton>(subSingleton) && variance == Covariant)
        return;

    reportError(TypeError{location, TypeMismatch{superTy, subTy}});
}

void Unifier::tryUnifyFunctions(TypeId subTy, TypeId superTy, bool isFunctionCall)
{
    FunctionTypeVar* superFunction = getMutable<FunctionTypeVar>(superTy);
    FunctionTypeVar* subFunction = getMutable<FunctionTypeVar>(subTy);

    if (FFlag::LuauUseCommittingTxnLog)
    {
        superFunction = log.getMutable<FunctionTypeVar>(superTy);
        subFunction = log.getMutable<FunctionTypeVar>(subTy);
    }

    if (!superFunction || !subFunction)
        ice("passed non-function types to unifyFunction");

    size_t numGenerics = superFunction->generics.size();
    if (numGenerics != subFunction->generics.size())
    {
        numGenerics = std::min(superFunction->generics.size(), subFunction->generics.size());

        reportError(TypeError{location, TypeMismatch{superTy, subTy, "different number of generic type parameters"}});
    }

    size_t numGenericPacks = superFunction->genericPacks.size();
    if (numGenericPacks != subFunction->genericPacks.size())
    {
        numGenericPacks = std::min(superFunction->genericPacks.size(), subFunction->genericPacks.size());

        reportError(TypeError{location, TypeMismatch{superTy, subTy, "different number of generic type pack parameters"}});
    }

    for (size_t i = 0; i < numGenerics; i++)
    {
        if (FFlag::LuauUseCommittingTxnLog)
            log.pushSeen(superFunction->generics[i], subFunction->generics[i]);
        else
            DEPRECATED_log.pushSeen(superFunction->generics[i], subFunction->generics[i]);
    }

    CountMismatch::Context context = ctx;

    if (!isFunctionCall)
    {
        Unifier innerState = makeChildUnifier();

        innerState.ctx = CountMismatch::Arg;
        innerState.tryUnify_(superFunction->argTypes, subFunction->argTypes, isFunctionCall);

        bool reported = !innerState.errors.empty();

        if (auto e = hasUnificationTooComplex(innerState.errors))
            reportError(*e);
        else if (!innerState.errors.empty() && innerState.firstPackErrorPos)
            reportError(
                TypeError{location, TypeMismatch{superTy, subTy, format("Argument #%d type is not compatible.", *innerState.firstPackErrorPos),
                                        innerState.errors.front()}});
        else if (!innerState.errors.empty())
            reportError(TypeError{location, TypeMismatch{superTy, subTy, "", innerState.errors.front()}});

        innerState.ctx = CountMismatch::Result;
        innerState.tryUnify_(subFunction->retType, superFunction->retType);

        if (!reported)
        {
            if (auto e = hasUnificationTooComplex(innerState.errors))
                reportError(*e);
            else if (!innerState.errors.empty() && size(superFunction->retType) == 1 && finite(superFunction->retType))
                reportError(TypeError{location, TypeMismatch{superTy, subTy, "Return type is not compatible.", innerState.errors.front()}});
            else if (!innerState.errors.empty() && innerState.firstPackErrorPos)
                reportError(
                    TypeError{location, TypeMismatch{superTy, subTy, format("Return #%d type is not compatible.", *innerState.firstPackErrorPos),
                                            innerState.errors.front()}});
            else if (!innerState.errors.empty())
                reportError(TypeError{location, TypeMismatch{superTy, subTy, "", innerState.errors.front()}});
        }

        if (FFlag::LuauUseCommittingTxnLog)
        {
            log.concat(std::move(innerState.log));
        }
        else
        {
            DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));
        }
    }
    else
    {
        ctx = CountMismatch::Arg;
        tryUnify_(superFunction->argTypes, subFunction->argTypes, isFunctionCall);

        ctx = CountMismatch::Result;
        tryUnify_(subFunction->retType, superFunction->retType);
    }

    if (FFlag::LuauUseCommittingTxnLog)
    {
        if (superFunction->definition && !subFunction->definition && !subTy->persistent)
        {
            PendingType* newSubTy = log.queue(subTy);
            FunctionTypeVar* newSubFtv = getMutable<FunctionTypeVar>(newSubTy);
            LUAU_ASSERT(newSubFtv);
            newSubFtv->definition = superFunction->definition;
        }
        else if (!superFunction->definition && subFunction->definition && !superTy->persistent)
        {
            PendingType* newSuperTy = log.queue(superTy);
            FunctionTypeVar* newSuperFtv = getMutable<FunctionTypeVar>(newSuperTy);
            LUAU_ASSERT(newSuperFtv);
            newSuperFtv->definition = subFunction->definition;
        }
    }
    else
    {
        if (superFunction->definition && !subFunction->definition && !subTy->persistent)
        {
            subFunction->definition = superFunction->definition;
        }
        else if (!superFunction->definition && subFunction->definition && !superTy->persistent)
        {
            superFunction->definition = subFunction->definition;
        }
    }

    ctx = context;

    for (int i = int(numGenerics) - 1; 0 <= i; i--)
    {
        if (FFlag::LuauUseCommittingTxnLog)
            log.popSeen(superFunction->generics[i], subFunction->generics[i]);
        else
            DEPRECATED_log.popSeen(superFunction->generics[i], subFunction->generics[i]);
    }
}

namespace
{

struct Resetter
{
    explicit Resetter(Variance* variance)
        : oldValue(*variance)
        , variance(variance)
    {
    }

    Variance oldValue;
    Variance* variance;

    ~Resetter()
    {
        *variance = oldValue;
    }
};

} // namespace

void Unifier::tryUnifyTables(TypeId subTy, TypeId superTy, bool isIntersection)
{
    if (!FFlag::LuauTableSubtypingVariance2)
        return DEPRECATED_tryUnifyTables(subTy, superTy, isIntersection);

    TableTypeVar* superTable = getMutable<TableTypeVar>(superTy);
    TableTypeVar* subTable = getMutable<TableTypeVar>(subTy);
    if (!superTable || !subTable)
        ice("passed non-table types to unifyTables");

    std::vector<std::string> missingProperties;
    std::vector<std::string> extraProperties;

    // Optimization: First test that the property sets are compatible without doing any recursive unification
    if (FFlag::LuauTableUnificationEarlyTest && !subTable->indexer && subTable->state != TableState::Free)
    {
        for (const auto& [propName, superProp] : superTable->props)
        {
            auto subIter = subTable->props.find(propName);
            if (subIter == subTable->props.end() && !isOptional(superProp.type) && !get<AnyTypeVar>(follow(superProp.type)))
                missingProperties.push_back(propName);
        }

        if (!missingProperties.empty())
        {
            reportError(TypeError{location, MissingProperties{superTy, subTy, std::move(missingProperties)}});
            return;
        }
    }

    // And vice versa if we're invariant
    if (FFlag::LuauTableUnificationEarlyTest && variance == Invariant && !superTable->indexer && superTable->state != TableState::Unsealed &&
        superTable->state != TableState::Free)
    {
        for (const auto& [propName, subProp] : subTable->props)
        {
            auto superIter = superTable->props.find(propName);
            if (superIter == superTable->props.end() && !isOptional(subProp.type) && !get<AnyTypeVar>(follow(subProp.type)))
                extraProperties.push_back(propName);
        }

        if (!extraProperties.empty())
        {
            reportError(TypeError{location, MissingProperties{superTy, subTy, std::move(extraProperties), MissingProperties::Extra}});
            return;
        }
    }

    // Width subtyping: any property in the supertype must be in the subtype,
    // and the types must agree.
    for (const auto& [name, prop] : superTable->props)
    {
        const auto& r = subTable->props.find(name);
        if (r != subTable->props.end())
        {
            // TODO: read-only properties don't need invariance
            Resetter resetter{&variance};
            variance = Invariant;

            Unifier innerState = makeChildUnifier();
            innerState.tryUnify_(r->second.type, prop.type);

            checkChildUnifierTypeMismatch(innerState.errors, name, superTy, subTy);

            if (FFlag::LuauUseCommittingTxnLog)
            {
                if (innerState.errors.empty())
                    log.concat(std::move(innerState.log));
            }
            else
            {
                if (innerState.errors.empty())
                    DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));
                else
                    innerState.DEPRECATED_log.rollback();
            }
        }
        else if (subTable->indexer && isString(subTable->indexer->indexType))
        {
            // TODO: read-only indexers don't need invariance
            // TODO: really we should only allow this if prop.type is optional.
            Resetter resetter{&variance};
            variance = Invariant;

            Unifier innerState = makeChildUnifier();
            innerState.tryUnify_(subTable->indexer->indexResultType, prop.type);

            checkChildUnifierTypeMismatch(innerState.errors, name, superTy, subTy);

            if (FFlag::LuauUseCommittingTxnLog)
            {
                if (innerState.errors.empty())
                    log.concat(std::move(innerState.log));
            }
            else
            {
                if (innerState.errors.empty())
                    DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));
                else
                    innerState.DEPRECATED_log.rollback();
            }
        }
        else if (isOptional(prop.type) || get<AnyTypeVar>(follow(prop.type)))
        // TODO: this case is unsound, but without it our test suite fails. CLI-46031
        // TODO: should isOptional(anyType) be true?
        {
        }
        else if (subTable->state == TableState::Free)
        {
            if (FFlag::LuauUseCommittingTxnLog)
            {
                PendingType* pendingSub = log.queue(subTy);
                TableTypeVar* ttv = getMutable<TableTypeVar>(pendingSub);
                LUAU_ASSERT(ttv);
                ttv->props[name] = prop;
            }
            else
            {
                DEPRECATED_log(subTy);
                subTable->props[name] = prop;
            }
        }
        else
            missingProperties.push_back(name);
    }

    for (const auto& [name, prop] : subTable->props)
    {
        if (superTable->props.count(name))
        {
            // If both lt and rt contain the property, then
            // we're done since we already unified them above
        }
        else if (superTable->indexer && isString(superTable->indexer->indexType))
        {
            // TODO: read-only indexers don't need invariance
            // TODO: really we should only allow this if prop.type is optional.
            Resetter resetter{&variance};
            variance = Invariant;

            Unifier innerState = makeChildUnifier();
            innerState.tryUnify_(superTable->indexer->indexResultType, prop.type);

            checkChildUnifierTypeMismatch(innerState.errors, name, superTy, subTy);

            if (FFlag::LuauUseCommittingTxnLog)
            {
                if (innerState.errors.empty())
                    log.concat(std::move(innerState.log));
            }
            else
            {
                if (innerState.errors.empty())
                    DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));
                else
                    innerState.DEPRECATED_log.rollback();
            }
        }
        else if (superTable->state == TableState::Unsealed)
        {
            // TODO: this case is unsound when variance is Invariant, but without it lua-apps fails to typecheck.
            // TODO: file a JIRA
            // TODO: hopefully readonly/writeonly properties will fix this.
            Property clone = prop;
            clone.type = deeplyOptional(clone.type);

            if (FFlag::LuauUseCommittingTxnLog)
            {
                PendingType* pendingSuper = log.queue(superTy);
                TableTypeVar* pendingSuperTtv = getMutable<TableTypeVar>(pendingSuper);
                pendingSuperTtv->props[name] = clone;
            }
            else
            {
                DEPRECATED_log(superTy);
                superTable->props[name] = clone;
            }
        }
        else if (variance == Covariant)
        {
        }
        else if (isOptional(prop.type) || get<AnyTypeVar>(follow(prop.type)))
        // TODO: this case is unsound, but without it our test suite fails. CLI-46031
        // TODO: should isOptional(anyType) be true?
        {
        }
        else if (superTable->state == TableState::Free)
        {
            if (FFlag::LuauUseCommittingTxnLog)
            {
                PendingType* pendingSuper = log.queue(superTy);
                TableTypeVar* pendingSuperTtv = getMutable<TableTypeVar>(pendingSuper);
                pendingSuperTtv->props[name] = prop;
            }
            else
            {
                DEPRECATED_log(superTy);
                superTable->props[name] = prop;
            }
        }
        else
            extraProperties.push_back(name);
    }

    // Unify indexers
    if (superTable->indexer && subTable->indexer)
    {
        // TODO: read-only indexers don't need invariance
        Resetter resetter{&variance};
        variance = Invariant;

        Unifier innerState = makeChildUnifier();
        innerState.tryUnifyIndexer(*subTable->indexer, *superTable->indexer);
        checkChildUnifierTypeMismatch(innerState.errors, superTy, subTy);

        if (FFlag::LuauUseCommittingTxnLog)
        {
            if (innerState.errors.empty())
                log.concat(std::move(innerState.log));
        }
        else
        {
            if (innerState.errors.empty())
                DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));
            else
                innerState.DEPRECATED_log.rollback();
        }
    }
    else if (superTable->indexer)
    {
        if (subTable->state == TableState::Unsealed || subTable->state == TableState::Free)
        {
            // passing/assigning a table without an indexer to something that has one
            // e.g. table.insert(t, 1) where t is a non-sealed table and doesn't have an indexer.
            // TODO: we only need to do this if the supertype's indexer is read/write
            // since that can add indexed elements.
            if (FFlag::LuauUseCommittingTxnLog)
            {
                log.changeIndexer(subTy, superTable->indexer);
            }
            else
            {
                DEPRECATED_log(subTy);
                subTable->indexer = superTable->indexer;
            }
        }
    }
    else if (subTable->indexer && variance == Invariant)
    {
        // Symmetric if we are invariant
        if (superTable->state == TableState::Unsealed || superTable->state == TableState::Free)
        {
            if (FFlag::LuauUseCommittingTxnLog)
            {
                log.changeIndexer(superTy, subTable->indexer);
            }
            else
            {
                DEPRECATED_log(superTy);
                superTable->indexer = subTable->indexer;
            }
        }
    }

    if (!missingProperties.empty())
    {
        reportError(TypeError{location, MissingProperties{superTy, subTy, std::move(missingProperties)}});
        return;
    }

    if (!extraProperties.empty())
    {
        reportError(TypeError{location, MissingProperties{superTy, subTy, std::move(extraProperties), MissingProperties::Extra}});
        return;
    }

    /*
     * TypeVars are commonly cyclic, so it is entirely possible
     * for unifying a property of a table to change the table itself!
     * We need to check for this and start over if we notice this occurring.
     *
     * I believe this is guaranteed to terminate eventually because this will
     * only happen when a free table is bound to another table.
     */
    if (superTable->boundTo || subTable->boundTo)
        return tryUnify_(subTy, superTy);

    if (superTable->state == TableState::Free)
    {
        if (FFlag::LuauUseCommittingTxnLog)
        {
            log.bindTable(superTy, subTy);
        }
        else
        {
            DEPRECATED_log(superTable);
            superTable->boundTo = subTy;
        }
    }
    else if (subTable->state == TableState::Free)
    {
        if (FFlag::LuauUseCommittingTxnLog)
        {
            log.bindTable(subTy, superTy);
        }
        else
        {
            DEPRECATED_log(subTy);
            subTable->boundTo = superTy;
        }
    }
}

TypeId Unifier::deeplyOptional(TypeId ty, std::unordered_map<TypeId, TypeId> seen)
{
    ty = follow(ty);
    if (get<AnyTypeVar>(ty))
        return ty;
    else if (isOptional(ty))
        return ty;
    else if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
    {
        TypeId& result = seen[ty];
        if (result)
            return result;
        result = types->addType(*ttv);
        TableTypeVar* resultTtv = getMutable<TableTypeVar>(result);
        for (auto& [name, prop] : resultTtv->props)
            prop.type = deeplyOptional(prop.type, seen);
        return types->addType(UnionTypeVar{{getSingletonTypes().nilType, result}});
    }
    else
        return types->addType(UnionTypeVar{{getSingletonTypes().nilType, ty}});
}

void Unifier::DEPRECATED_tryUnifyTables(TypeId subTy, TypeId superTy, bool isIntersection)
{
    LUAU_ASSERT(!FFlag::LuauTableSubtypingVariance2);
    Resetter resetter{&variance};
    variance = Invariant;

    TableTypeVar* superTable = getMutable<TableTypeVar>(superTy);
    TableTypeVar* subTable = getMutable<TableTypeVar>(subTy);

    if (FFlag::LuauUseCommittingTxnLog)
    {
        superTable = log.getMutable<TableTypeVar>(superTy);
        subTable = log.getMutable<TableTypeVar>(subTy);
    }

    if (!superTable || !subTable)
        ice("passed non-table types to unifyTables");

    if (superTable->state == TableState::Sealed && subTable->state == TableState::Sealed)
        return tryUnifySealedTables(subTy, superTy, isIntersection);
    else if ((superTable->state == TableState::Sealed && subTable->state == TableState::Unsealed) ||
             (superTable->state == TableState::Unsealed && subTable->state == TableState::Sealed))
        return tryUnifySealedTables(subTy, superTy, isIntersection);
    else if ((superTable->state == TableState::Sealed && subTable->state == TableState::Generic) ||
             (superTable->state == TableState::Generic && subTable->state == TableState::Sealed))
        reportError(TypeError{location, TypeMismatch{superTy, subTy}});
    else if ((superTable->state == TableState::Free) != (subTable->state == TableState::Free)) // one table is free and the other is not
    {
        TypeId freeTypeId = subTable->state == TableState::Free ? subTy : superTy;
        TypeId otherTypeId = subTable->state == TableState::Free ? superTy : subTy;

        return tryUnifyFreeTable(otherTypeId, freeTypeId);
    }
    else if (superTable->state == TableState::Free && subTable->state == TableState::Free)
    {
        tryUnifyFreeTable(subTy, superTy);

        // avoid creating a cycle when the types are already pointing at each other
        if (follow(superTy) != follow(subTy))
        {
            if (FFlag::LuauUseCommittingTxnLog)
            {
                log.bindTable(superTy, subTy);
            }
            else
            {
                DEPRECATED_log(superTable);
                superTable->boundTo = subTy;
            }
        }
        return;
    }
    else if (superTable->state != TableState::Sealed && subTable->state != TableState::Sealed)
    {
        // All free tables are checked in one of the branches above
        LUAU_ASSERT(superTable->state != TableState::Free);
        LUAU_ASSERT(subTable->state != TableState::Free);

        // Tables must have exactly the same props and their types must all unify
        // I honestly have no idea if this is remotely close to reasonable.
        for (const auto& [name, prop] : superTable->props)
        {
            const auto& r = subTable->props.find(name);
            if (r == subTable->props.end())
                reportError(TypeError{location, UnknownProperty{subTy, name}});
            else
                tryUnify_(r->second.type, prop.type);
        }

        if (superTable->indexer && subTable->indexer)
            tryUnifyIndexer(*subTable->indexer, *superTable->indexer);
        else if (superTable->indexer)
        {
            // passing/assigning a table without an indexer to something that has one
            // e.g. table.insert(t, 1) where t is a non-sealed table and doesn't have an indexer.
            if (subTable->state == TableState::Unsealed)
            {
                if (FFlag::LuauUseCommittingTxnLog)
                {
                    log.changeIndexer(subTy, superTable->indexer);
                }
                else
                {
                    subTable->indexer = superTable->indexer;
                }
            }
            else
                reportError(TypeError{location, CannotExtendTable{subTy, CannotExtendTable::Indexer}});
        }
    }
    else if (superTable->state == TableState::Sealed)
    {
        // lt is sealed and so it must be possible for rt to have precisely the same shape
        // Verify that this is the case, then bind rt to lt.
        ice("unsealed tables are not working yet", location);
    }
    else if (subTable->state == TableState::Sealed)
        return tryUnifyTables(superTy, subTy, isIntersection);
    else
        ice("tryUnifyTables");
}

void Unifier::tryUnifyFreeTable(TypeId subTy, TypeId superTy)
{
    TableTypeVar* freeTable = getMutable<TableTypeVar>(superTy);
    TableTypeVar* subTable = getMutable<TableTypeVar>(subTy);

    if (FFlag::LuauUseCommittingTxnLog)
    {
        freeTable = log.getMutable<TableTypeVar>(superTy);
        subTable = log.getMutable<TableTypeVar>(subTy);
    }

    if (!freeTable || !subTable)
        ice("passed non-table types to tryUnifyFreeTable");

    // Any properties in freeTable must unify with those in otherTable.
    // Then bind freeTable to otherTable.
    for (const auto& [freeName, freeProp] : freeTable->props)
    {
        if (auto subProp = findTablePropertyRespectingMeta(subTy, freeName))
        {
            tryUnify_(freeProp.type, *subProp);

            /*
             * TypeVars are commonly cyclic, so it is entirely possible
             * for unifying a property of a table to change the table itself!
             * We need to check for this and start over if we notice this occurring.
             *
             * I believe this is guaranteed to terminate eventually because this will
             * only happen when a free table is bound to another table.
             */
            if (FFlag::LuauUseCommittingTxnLog)
            {
                if (!log.getMutable<TableTypeVar>(superTy) || !log.getMutable<TableTypeVar>(subTy))
                    return tryUnify_(subTy, superTy);

                if (TableTypeVar* pendingFreeTtv = log.getMutable<TableTypeVar>(superTy); pendingFreeTtv && pendingFreeTtv->boundTo)
                    return tryUnify_(subTy, superTy);
            }
            else
            {
                if (!get<TableTypeVar>(superTy) || !get<TableTypeVar>(subTy))
                    return tryUnify_(subTy, superTy);

                if (freeTable->boundTo)
                    return tryUnify_(subTy, superTy);
            }
        }
        else
        {
            // If the other table is also free, then we are learning that it has more
            // properties than we previously thought.  Else, it is an error.
            if (subTable->state == TableState::Free)
            {
                if (FFlag::LuauUseCommittingTxnLog)
                {
                    PendingType* pendingSub = log.queue(subTy);
                    TableTypeVar* pendingSubTtv = getMutable<TableTypeVar>(pendingSub);
                    LUAU_ASSERT(pendingSubTtv);
                    pendingSubTtv->props.insert({freeName, freeProp});
                }
                else
                {
                    subTable->props.insert({freeName, freeProp});
                }
            }
            else
                reportError(TypeError{location, UnknownProperty{subTy, freeName}});
        }
    }

    if (freeTable->indexer && subTable->indexer)
    {
        Unifier innerState = makeChildUnifier();
        innerState.tryUnifyIndexer(*subTable->indexer, *freeTable->indexer);

        checkChildUnifierTypeMismatch(innerState.errors, superTy, subTy);

        if (FFlag::LuauUseCommittingTxnLog)
            log.concat(std::move(innerState.log));
        else
            DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));
    }
    else if (subTable->state == TableState::Free && freeTable->indexer)
    {
        if (FFlag::LuauUseCommittingTxnLog)
        {
            log.changeIndexer(superTy, subTable->indexer);
        }
        else
        {
            freeTable->indexer = subTable->indexer;
        }
    }

    if (!freeTable->boundTo && subTable->state != TableState::Free)
    {
        if (FFlag::LuauUseCommittingTxnLog)
        {
            log.bindTable(superTy, subTy);
        }
        else
        {
            DEPRECATED_log(freeTable);
            freeTable->boundTo = subTy;
        }
    }
}

void Unifier::tryUnifySealedTables(TypeId subTy, TypeId superTy, bool isIntersection)
{
    TableTypeVar* superTable = getMutable<TableTypeVar>(superTy);
    TableTypeVar* subTable = getMutable<TableTypeVar>(subTy);

    if (FFlag::LuauUseCommittingTxnLog)
    {
        superTable = log.getMutable<TableTypeVar>(superTy);
        subTable = log.getMutable<TableTypeVar>(subTy);
    }

    if (!superTable || !subTable)
        ice("passed non-table types to unifySealedTables");

    Unifier innerState = makeChildUnifier();

    std::vector<std::string> missingPropertiesInSuper;
    bool isUnnamedTable = subTable->name == std::nullopt && subTable->syntheticName == std::nullopt;
    bool errorReported = false;

    // Optimization: First test that the property sets are compatible without doing any recursive unification
    if (FFlag::LuauTableUnificationEarlyTest && !subTable->indexer)
    {
        for (const auto& [propName, superProp] : superTable->props)
        {
            auto subIter = subTable->props.find(propName);
            if (subIter == subTable->props.end() && !isOptional(superProp.type))
                missingPropertiesInSuper.push_back(propName);
        }

        if (!missingPropertiesInSuper.empty())
        {
            reportError(TypeError{location, MissingProperties{superTy, subTy, std::move(missingPropertiesInSuper)}});
            return;
        }
    }

    // Tables must have exactly the same props and their types must all unify
    for (const auto& it : superTable->props)
    {
        const auto& r = subTable->props.find(it.first);
        if (r == subTable->props.end())
        {
            if (isOptional(it.second.type))
                continue;

            missingPropertiesInSuper.push_back(it.first);

            innerState.reportError(TypeError{location, TypeMismatch{superTy, subTy}});
        }
        else
        {
            if (isUnnamedTable && r->second.location)
            {
                size_t oldErrorSize = innerState.errors.size();
                Location old = innerState.location;
                innerState.location = *r->second.location;
                innerState.tryUnify_(r->second.type, it.second.type);
                innerState.location = old;

                if (oldErrorSize != innerState.errors.size() && !errorReported)
                {
                    errorReported = true;
                    reportError(innerState.errors.back());
                }
            }
            else
            {
                innerState.tryUnify_(r->second.type, it.second.type);
            }
        }
    }

    if (superTable->indexer || subTable->indexer)
    {
        if (FFlag::LuauUseCommittingTxnLog)
        {
            if (superTable->indexer && subTable->indexer)
                innerState.tryUnifyIndexer(*subTable->indexer, *superTable->indexer);
            else if (subTable->state == TableState::Unsealed)
            {
                if (superTable->indexer && !subTable->indexer)
                {
                    log.changeIndexer(subTy, superTable->indexer);
                }
            }
            else if (superTable->state == TableState::Unsealed)
            {
                if (subTable->indexer && !superTable->indexer)
                {
                    log.changeIndexer(superTy, subTable->indexer);
                }
            }
            else if (superTable->indexer)
            {
                innerState.tryUnify_(getSingletonTypes().stringType, superTable->indexer->indexType);
                for (const auto& [name, type] : subTable->props)
                {
                    const auto& it = superTable->props.find(name);
                    if (it == superTable->props.end())
                        innerState.tryUnify_(type.type, superTable->indexer->indexResultType);
                }
            }
            else
                innerState.reportError(TypeError{location, TypeMismatch{superTy, subTy}});
        }
        else
        {
            if (superTable->indexer && subTable->indexer)
                innerState.tryUnifyIndexer(*subTable->indexer, *superTable->indexer);
            else if (subTable->state == TableState::Unsealed)
            {
                if (superTable->indexer && !subTable->indexer)
                    subTable->indexer = superTable->indexer;
            }
            else if (superTable->state == TableState::Unsealed)
            {
                if (subTable->indexer && !superTable->indexer)
                    superTable->indexer = subTable->indexer;
            }
            else if (superTable->indexer)
            {
                innerState.tryUnify_(getSingletonTypes().stringType, superTable->indexer->indexType);
                // We already try to unify properties in both tables.
                // Skip those and just look for the ones remaining and see if they fit into the indexer.
                for (const auto& [name, type] : subTable->props)
                {
                    const auto& it = superTable->props.find(name);
                    if (it == superTable->props.end())
                        innerState.tryUnify_(type.type, superTable->indexer->indexResultType);
                }
            }
            else
                innerState.reportError(TypeError{location, TypeMismatch{superTy, subTy}});
        }
    }

    if (FFlag::LuauUseCommittingTxnLog)
    {
        if (!errorReported)
            log.concat(std::move(innerState.log));
    }
    else
        DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));

    if (errorReported)
        return;

    if (!missingPropertiesInSuper.empty())
    {
        reportError(TypeError{location, MissingProperties{superTy, subTy, std::move(missingPropertiesInSuper)}});
        return;
    }

    // If the superTy is an immediate part of an intersection type, do not do extra-property check.
    // Otherwise, we would falsely generate an extra-property-error for 's' in this code:
    // local a: {n: number} & {s: string} = {n=1, s=""}
    // When checking against the table '{n: number}'.
    if (!isIntersection && superTable->state != TableState::Unsealed && !superTable->indexer)
    {
        // Check for extra properties in the subTy
        std::vector<std::string> extraPropertiesInSub;

        for (const auto& [subKey, subProp] : subTable->props)
        {
            const auto& superIt = superTable->props.find(subKey);
            if (superIt == superTable->props.end())
            {
                if (isOptional(subProp.type))
                    continue;

                extraPropertiesInSub.push_back(subKey);
            }
        }

        if (!extraPropertiesInSub.empty())
        {
            reportError(TypeError{location, MissingProperties{superTy, subTy, std::move(extraPropertiesInSub), MissingProperties::Extra}});
            return;
        }
    }

    checkChildUnifierTypeMismatch(innerState.errors, superTy, subTy);
}

void Unifier::tryUnifyWithMetatable(TypeId subTy, TypeId superTy, bool reversed)
{
    const MetatableTypeVar* superMetatable = get<MetatableTypeVar>(superTy);
    if (!superMetatable)
        ice("tryUnifyMetatable invoked with non-metatable TypeVar");

    TypeError mismatchError = TypeError{location, TypeMismatch{reversed ? subTy : superTy, reversed ? superTy : subTy}};

    if (const MetatableTypeVar* subMetatable =
            FFlag::LuauUseCommittingTxnLog ? log.getMutable<MetatableTypeVar>(subTy) : get<MetatableTypeVar>(subTy))
    {
        Unifier innerState = makeChildUnifier();
        innerState.tryUnify_(subMetatable->table, superMetatable->table);
        innerState.tryUnify_(subMetatable->metatable, superMetatable->metatable);

        if (auto e = hasUnificationTooComplex(innerState.errors))
            reportError(*e);
        else if (!innerState.errors.empty())
            reportError(
                TypeError{location, TypeMismatch{reversed ? subTy : superTy, reversed ? superTy : subTy, "", innerState.errors.front()}});

        if (FFlag::LuauUseCommittingTxnLog)
            log.concat(std::move(innerState.log));
        else
            DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));
    }
    else if (TableTypeVar* subTable = FFlag::LuauUseCommittingTxnLog ? log.getMutable<TableTypeVar>(subTy) : getMutable<TableTypeVar>(subTy))
    {
        switch (subTable->state)
        {
        case TableState::Free:
        {
            tryUnify_(subTy, superMetatable->table);

            if (FFlag::LuauUseCommittingTxnLog)
            {
                log.bindTable(subTy, superTy);
            }
            else
            {
                subTable->boundTo = superTy;
            }

            break;
        }
        // We know the shape of sealed, unsealed, and generic tables; you can't add a metatable on to any of these.
        case TableState::Sealed:
        case TableState::Unsealed:
        case TableState::Generic:
            reportError(mismatchError);
        }
    }
    else if (FFlag::LuauUseCommittingTxnLog ? (log.getMutable<AnyTypeVar>(subTy) || log.getMutable<ErrorTypeVar>(subTy))
                                            : (get<AnyTypeVar>(subTy) || get<ErrorTypeVar>(subTy)))
    {
    }
    else
    {
        reportError(mismatchError);
    }
}

// Class unification is almost, but not quite symmetrical.  We use the 'reversed' boolean to indicate which scenario we are evaluating.
void Unifier::tryUnifyWithClass(TypeId subTy, TypeId superTy, bool reversed)
{
    if (reversed)
        std::swap(superTy, subTy);

    auto fail = [&]() {
        if (!reversed)
            reportError(TypeError{location, TypeMismatch{superTy, subTy}});
        else
            reportError(TypeError{location, TypeMismatch{subTy, superTy}});
    };

    const ClassTypeVar* superClass = get<ClassTypeVar>(superTy);
    if (!superClass)
        ice("tryUnifyClass invoked with non-class TypeVar");

    if (const ClassTypeVar* subClass = get<ClassTypeVar>(subTy))
    {
        switch (variance)
        {
        case Covariant:
            if (!isSubclass(subClass, superClass))
                return fail();
            return;
        case Invariant:
            if (subClass != superClass)
                return fail();
            return;
        }
        ice("Illegal variance setting!");
    }
    else if (TableTypeVar* subTable = getMutable<TableTypeVar>(subTy))
    {
        /**
         * A free table is something whose shape we do not exactly know yet.
         * Thus, it is entirely reasonable that we might discover that it is being used as some class type.
         * In this case, the free table must indeed be that exact class.
         * For this to hold, the table must not have any properties that the class does not.
         * Further, all properties of the table should unify cleanly with the matching class properties.
         * TODO: What does it mean for the table to have an indexer? (probably failure?)
         *
         * Tables that are not free are known to be actual tables.
         */
        if (subTable->state != TableState::Free)
            return fail();

        bool ok = true;

        for (const auto& [propName, prop] : subTable->props)
        {
            const Property* classProp = lookupClassProp(superClass, propName);
            if (!classProp)
            {
                ok = false;
                reportError(TypeError{location, UnknownProperty{superTy, propName}});
            }
            else
            {
                Unifier innerState = makeChildUnifier();
                innerState.tryUnify_(classProp->type, prop.type);

                checkChildUnifierTypeMismatch(innerState.errors, propName, reversed ? subTy : superTy, reversed ? superTy : subTy);

                if (FFlag::LuauUseCommittingTxnLog)
                {
                    if (innerState.errors.empty())
                    {
                        log.concat(std::move(innerState.log));
                    }
                    else
                    {
                        ok = false;
                    }
                }
                else
                {
                    if (innerState.errors.empty())
                    {
                        DEPRECATED_log.concat(std::move(innerState.DEPRECATED_log));
                    }
                    else
                    {
                        ok = false;
                        innerState.DEPRECATED_log.rollback();
                    }
                }
            }
        }

        if (subTable->indexer)
        {
            ok = false;
            std::string msg = "Class " + superClass->name + " does not have an indexer";
            reportError(TypeError{location, GenericError{msg}});
        }

        if (!ok)
            return;

        if (FFlag::LuauUseCommittingTxnLog)
        {
            log.bindTable(subTy, superTy);
        }
        else
        {
            DEPRECATED_log(subTable);
            subTable->boundTo = superTy;
        }
    }
    else
        return fail();
}

void Unifier::tryUnifyIndexer(const TableIndexer& subIndexer, const TableIndexer& superIndexer)
{
    tryUnify_(subIndexer.indexType, superIndexer.indexType);
    tryUnify_(subIndexer.indexResultType, superIndexer.indexResultType);
}

static void queueTypePack(std::vector<TypeId>& queue, DenseHashSet<TypePackId>& seenTypePacks, Unifier& state, TypePackId a, TypePackId anyTypePack)
{
    while (true)
    {
        a = follow(a);

        if (seenTypePacks.find(a))
            break;
        seenTypePacks.insert(a);

        if (FFlag::LuauUseCommittingTxnLog)
        {
            if (state.log.getMutable<Unifiable::Free>(a))
            {
                state.log.replace(a, Unifiable::Bound{anyTypePack});
            }
            else if (auto tp = state.log.getMutable<TypePack>(a))
            {
                queue.insert(queue.end(), tp->head.begin(), tp->head.end());
                if (tp->tail)
                    a = *tp->tail;
                else
                    break;
            }
        }
        else
        {
            if (get<Unifiable::Free>(a))
            {
                state.DEPRECATED_log(a);
                *asMutable(a) = Unifiable::Bound{anyTypePack};
            }
            else if (auto tp = get<TypePack>(a))
            {
                queue.insert(queue.end(), tp->head.begin(), tp->head.end());
                if (tp->tail)
                    a = *tp->tail;
                else
                    break;
            }
        }
    }
}

void Unifier::tryUnifyVariadics(TypePackId subTp, TypePackId superTp, bool reversed, int subOffset)
{
    const VariadicTypePack* superVariadic = get<VariadicTypePack>(superTp);

    if (FFlag::LuauUseCommittingTxnLog)
    {
        superVariadic = log.getMutable<VariadicTypePack>(superTp);
    }

    if (!superVariadic)
        ice("passed non-variadic pack to tryUnifyVariadics");

    if (const VariadicTypePack* subVariadic = get<VariadicTypePack>(subTp))
        tryUnify_(reversed ? superVariadic->ty : subVariadic->ty, reversed ? subVariadic->ty : superVariadic->ty);
    else if (get<TypePack>(subTp))
    {
        TypePackIterator subIter = begin(subTp, &log);
        TypePackIterator subEnd = end(subTp);

        std::advance(subIter, subOffset);

        while (subIter != subEnd)
        {
            tryUnify_(reversed ? superVariadic->ty : *subIter, reversed ? *subIter : superVariadic->ty);
            ++subIter;
        }

        if (std::optional<TypePackId> maybeTail = subIter.tail())
        {
            TypePackId tail = follow(*maybeTail);
            if (get<FreeTypePack>(tail))
            {
                if (FFlag::LuauUseCommittingTxnLog)
                {
                    log.replace(tail, BoundTypePack(superTp));
                }
                else
                {
                    DEPRECATED_log(tail);
                    *asMutable(tail) = BoundTypePack{superTp};
                }
            }
            else if (const VariadicTypePack* vtp = get<VariadicTypePack>(tail))
            {
                tryUnify_(vtp->ty, superVariadic->ty);
            }
            else if (get<Unifiable::Generic>(tail))
            {
                reportError(TypeError{location, GenericError{"Cannot unify variadic and generic packs"}});
            }
            else if (get<Unifiable::Error>(tail))
            {
                // Nothing to do here.
            }
            else
            {
                ice("Unknown TypePack kind");
            }
        }
    }
    else
    {
        reportError(TypeError{location, GenericError{"Failed to unify variadic packs"}});
    }
}

static void tryUnifyWithAny(std::vector<TypeId>& queue, Unifier& state, DenseHashSet<TypeId>& seen, DenseHashSet<TypePackId>& seenTypePacks,
    TypeId anyType, TypePackId anyTypePack)
{
    while (!queue.empty())
    {
        if (FFlag::LuauUseCommittingTxnLog)
        {
            TypeId ty = state.log.follow(queue.back());
            queue.pop_back();
            if (seen.find(ty))
                continue;
            seen.insert(ty);

            if (state.log.getMutable<FreeTypeVar>(ty))
            {
                state.log.replace(ty, BoundTypeVar{anyType});
            }
            else if (auto fun = state.log.getMutable<FunctionTypeVar>(ty))
            {
                queueTypePack(queue, seenTypePacks, state, fun->argTypes, anyTypePack);
                queueTypePack(queue, seenTypePacks, state, fun->retType, anyTypePack);
            }
            else if (auto table = state.log.getMutable<TableTypeVar>(ty))
            {
                for (const auto& [_name, prop] : table->props)
                    queue.push_back(prop.type);

                if (table->indexer)
                {
                    queue.push_back(table->indexer->indexType);
                    queue.push_back(table->indexer->indexResultType);
                }
            }
            else if (auto mt = state.log.getMutable<MetatableTypeVar>(ty))
            {
                queue.push_back(mt->table);
                queue.push_back(mt->metatable);
            }
            else if (state.log.getMutable<ClassTypeVar>(ty))
            {
                // ClassTypeVars never contain free typevars.
            }
            else if (auto union_ = state.log.getMutable<UnionTypeVar>(ty))
                queue.insert(queue.end(), union_->options.begin(), union_->options.end());
            else if (auto intersection = state.log.getMutable<IntersectionTypeVar>(ty))
                queue.insert(queue.end(), intersection->parts.begin(), intersection->parts.end());
            else
            {
            } // Primitives, any, errors, and generics are left untouched.
        }
        else
        {
            TypeId ty = follow(queue.back());
            queue.pop_back();
            if (seen.find(ty))
                continue;
            seen.insert(ty);

            if (get<FreeTypeVar>(ty))
            {
                state.DEPRECATED_log(ty);
                *asMutable(ty) = BoundTypeVar{anyType};
            }
            else if (auto fun = get<FunctionTypeVar>(ty))
            {
                queueTypePack(queue, seenTypePacks, state, fun->argTypes, anyTypePack);
                queueTypePack(queue, seenTypePacks, state, fun->retType, anyTypePack);
            }
            else if (auto table = get<TableTypeVar>(ty))
            {
                for (const auto& [_name, prop] : table->props)
                    queue.push_back(prop.type);

                if (table->indexer)
                {
                    queue.push_back(table->indexer->indexType);
                    queue.push_back(table->indexer->indexResultType);
                }
            }
            else if (auto mt = get<MetatableTypeVar>(ty))
            {
                queue.push_back(mt->table);
                queue.push_back(mt->metatable);
            }
            else if (get<ClassTypeVar>(ty))
            {
                // ClassTypeVars never contain free typevars.
            }
            else if (auto union_ = get<UnionTypeVar>(ty))
                queue.insert(queue.end(), union_->options.begin(), union_->options.end());
            else if (auto intersection = get<IntersectionTypeVar>(ty))
                queue.insert(queue.end(), intersection->parts.begin(), intersection->parts.end());
            else
            {
            } // Primitives, any, errors, and generics are left untouched.
        }
    }
}

void Unifier::tryUnifyWithAny(TypeId subTy, TypeId anyTy)
{
    LUAU_ASSERT(get<AnyTypeVar>(anyTy) || get<ErrorTypeVar>(anyTy));

    // These types are not visited in general loop below
    if (get<PrimitiveTypeVar>(subTy) || get<AnyTypeVar>(subTy) || get<ClassTypeVar>(subTy))
        return;

    const TypePackId anyTypePack = types->addTypePack(TypePackVar{VariadicTypePack{getSingletonTypes().anyType}});

    const TypePackId anyTP = get<AnyTypeVar>(anyTy) ? anyTypePack : types->addTypePack(TypePackVar{Unifiable::Error{}});

    std::vector<TypeId> queue = {subTy};

    sharedState.tempSeenTy.clear();
    sharedState.tempSeenTp.clear();

    Luau::tryUnifyWithAny(queue, *this, sharedState.tempSeenTy, sharedState.tempSeenTp, getSingletonTypes().anyType, anyTP);
}

void Unifier::tryUnifyWithAny(TypePackId subTy, TypePackId anyTp)
{
    LUAU_ASSERT(get<Unifiable::Error>(anyTp));

    const TypeId anyTy = getSingletonTypes().errorRecoveryType();

    std::vector<TypeId> queue;

    sharedState.tempSeenTy.clear();
    sharedState.tempSeenTp.clear();

    queueTypePack(queue, sharedState.tempSeenTp, *this, subTy, anyTp);

    Luau::tryUnifyWithAny(queue, *this, sharedState.tempSeenTy, sharedState.tempSeenTp, anyTy, anyTp);
}

std::optional<TypeId> Unifier::findTablePropertyRespectingMeta(TypeId lhsType, Name name)
{
    return Luau::findTablePropertyRespectingMeta(errors, globalScope, lhsType, name, location);
}

void Unifier::occursCheck(TypeId needle, TypeId haystack)
{
    sharedState.tempSeenTy.clear();

    return occursCheck(sharedState.tempSeenTy, needle, haystack);
}

void Unifier::occursCheck(DenseHashSet<TypeId>& seen, TypeId needle, TypeId haystack)
{
    RecursionLimiter _ra(&sharedState.counters.recursionCount, FInt::LuauTypeInferRecursionLimit);

    auto check = [&](TypeId tv) {
        occursCheck(seen, needle, tv);
    };

    if (FFlag::LuauUseCommittingTxnLog)
    {
        needle = log.follow(needle);
        haystack = log.follow(haystack);

        if (seen.find(haystack))
            return;

        seen.insert(haystack);

        if (log.getMutable<Unifiable::Error>(needle))
            return;

        if (!log.getMutable<Unifiable::Free>(needle))
            ice("Expected needle to be free");

        if (needle == haystack)
        {
            reportError(TypeError{location, OccursCheckFailed{}});
            log.replace(needle, *getSingletonTypes().errorRecoveryType());

            return;
        }

        if (log.getMutable<FreeTypeVar>(haystack))
            return;
        else if (auto a = log.getMutable<UnionTypeVar>(haystack))
        {
            for (TypeId ty : a->options)
                check(ty);
        }
        else if (auto a = log.getMutable<IntersectionTypeVar>(haystack))
        {
            for (TypeId ty : a->parts)
                check(ty);
        }
    }
    else
    {
        needle = follow(needle);
        haystack = follow(haystack);

        if (seen.find(haystack))
            return;

        seen.insert(haystack);

        if (get<Unifiable::Error>(needle))
            return;

        if (!get<Unifiable::Free>(needle))
            ice("Expected needle to be free");

        if (needle == haystack)
        {
            reportError(TypeError{location, OccursCheckFailed{}});
            DEPRECATED_log(needle);
            *asMutable(needle) = *getSingletonTypes().errorRecoveryType();
            return;
        }

        if (get<FreeTypeVar>(haystack))
            return;
        else if (auto a = get<UnionTypeVar>(haystack))
        {
            for (TypeId ty : a->options)
                check(ty);
        }
        else if (auto a = get<IntersectionTypeVar>(haystack))
        {
            for (TypeId ty : a->parts)
                check(ty);
        }
    }
}

void Unifier::occursCheck(TypePackId needle, TypePackId haystack)
{
    sharedState.tempSeenTp.clear();

    return occursCheck(sharedState.tempSeenTp, needle, haystack);
}

void Unifier::occursCheck(DenseHashSet<TypePackId>& seen, TypePackId needle, TypePackId haystack)
{
    if (FFlag::LuauUseCommittingTxnLog)
    {
        needle = log.follow(needle);
        haystack = log.follow(haystack);

        if (seen.find(haystack))
            return;

        seen.insert(haystack);

        if (log.getMutable<Unifiable::Error>(needle))
            return;

        if (!log.getMutable<Unifiable::Free>(needle))
            ice("Expected needle pack to be free");

        RecursionLimiter _ra(&sharedState.counters.recursionCount, FInt::LuauTypeInferRecursionLimit);

        while (!log.getMutable<ErrorTypeVar>(haystack))
        {
            if (needle == haystack)
            {
                reportError(TypeError{location, OccursCheckFailed{}});
                log.replace(needle, *getSingletonTypes().errorRecoveryTypePack());

                return;
            }

            if (auto a = get<TypePack>(haystack); a && a->tail)
            {
                haystack = log.follow(*a->tail);
                continue;
            }

            break;
        }
    }
    else
    {
        needle = follow(needle);
        haystack = follow(haystack);

        if (seen.find(haystack))
            return;

        seen.insert(haystack);

        if (get<Unifiable::Error>(needle))
            return;

        if (!get<Unifiable::Free>(needle))
            ice("Expected needle pack to be free");

        RecursionLimiter _ra(&sharedState.counters.recursionCount, FInt::LuauTypeInferRecursionLimit);

        while (!get<ErrorTypeVar>(haystack))
        {
            if (needle == haystack)
            {
                reportError(TypeError{location, OccursCheckFailed{}});
                DEPRECATED_log(needle);
                *asMutable(needle) = *getSingletonTypes().errorRecoveryTypePack();
            }

            if (auto a = get<TypePack>(haystack); a && a->tail)
            {
                haystack = follow(*a->tail);
                continue;
            }

            break;
        }
    }
}

Unifier Unifier::makeChildUnifier()
{
    if (FFlag::LuauUseCommittingTxnLog)
        return Unifier{types, mode, globalScope, log.sharedSeen, location, variance, sharedState, &log};
    else
        return Unifier{types, mode, globalScope, DEPRECATED_log.sharedSeen, location, variance, sharedState, &log};
}

bool Unifier::isNonstrictMode() const
{
    return (mode == Mode::Nonstrict) || (mode == Mode::NoCheck);
}

void Unifier::checkChildUnifierTypeMismatch(const ErrorVec& innerErrors, TypeId wantedType, TypeId givenType)
{
    if (auto e = hasUnificationTooComplex(innerErrors))
        reportError(*e);
    else if (!innerErrors.empty())
        reportError(TypeError{location, TypeMismatch{wantedType, givenType}});
}

void Unifier::checkChildUnifierTypeMismatch(const ErrorVec& innerErrors, const std::string& prop, TypeId wantedType, TypeId givenType)
{
    if (auto e = hasUnificationTooComplex(innerErrors))
        reportError(*e);
    else if (!innerErrors.empty())
        reportError(
            TypeError{location, TypeMismatch{wantedType, givenType, format("Property '%s' is not compatible.", prop.c_str()), innerErrors.front()}});
}

void Unifier::ice(const std::string& message, const Location& location)
{
    sharedState.iceHandler->ice(message, location);
}

void Unifier::ice(const std::string& message)
{
    sharedState.iceHandler->ice(message);
}

} // namespace Luau
