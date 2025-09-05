// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Unifier.h"

#include "Luau/Common.h"
#include "Luau/Instantiation.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/StringUtils.h"
#include "Luau/TimeTrace.h"
#include "Luau/ToString.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/Type.h"
#include "Luau/VisitType.h"

#include <algorithm>

LUAU_FASTINT(LuauTypeInferTypePackLoopLimit)
LUAU_FASTFLAG(LuauErrorRecoveryType)
LUAU_FASTFLAGVARIABLE(LuauInstantiateInSubtyping)
LUAU_FASTFLAGVARIABLE(LuauTransitiveSubtyping)
LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAGVARIABLE(LuauFixIndexerSubtypingOrdering)
LUAU_FASTFLAGVARIABLE(LuauUnifierRecursionOnRestart)

namespace Luau
{

struct PromoteTypeLevels final : TypeOnceVisitor
{
    TxnLog& log;
    const TypeArena* typeArena = nullptr;
    TypeLevel minLevel;

    PromoteTypeLevels(TxnLog& log, const TypeArena* typeArena, TypeLevel minLevel)
        : TypeOnceVisitor("PromoteTypeLevels", /* skipBoundTypes */ false)
        , log(log)
        , typeArena(typeArena)
        , minLevel(minLevel)
    {
    }

    template<typename TID, typename T>
    void promote(TID ty, T* t)
    {
        LUAU_ASSERT(t);

        if (minLevel.subsumesStrict(t->level))
            log.changeLevel(ty, minLevel);
    }

    bool visit(TypeId ty) override
    {
        // Type levels of types from other modules are already global, so we don't need to promote anything inside
        if (ty->owningArena != typeArena)
            return false;

        return true;
    }

    bool visit(TypePackId tp) override
    {
        // Type levels of types from other modules are already global, so we don't need to promote anything inside
        if (tp->owningArena != typeArena)
            return false;

        return true;
    }

    bool visit(TypeId ty, const FreeType&) override
    {
        // Surprise, it's actually a BoundType that hasn't been committed yet.
        // Calling getMutable on this will trigger an assertion.
        if (!log.is<FreeType>(ty))
            return true;

        promote(ty, log.getMutable<FreeType>(ty));
        return true;
    }

    bool visit(TypeId ty, const FunctionType&) override
    {
        // Type levels of types from other modules are already global, so we don't need to promote anything inside
        if (ty->owningArena != typeArena)
            return false;

        // Surprise, it's actually a BoundTypePack that hasn't been committed yet.
        // Calling getMutable on this will trigger an assertion.
        if (!log.is<FunctionType>(ty))
            return true;

        promote(ty, log.getMutable<FunctionType>(ty));
        return true;
    }

    bool visit(TypeId ty, const TableType& ttv) override
    {
        // Type levels of types from other modules are already global, so we don't need to promote anything inside
        if (ty->owningArena != typeArena)
            return false;

        if (ttv.state != TableState::Free && ttv.state != TableState::Generic)
            return true;

        // Surprise, it's actually a BoundTypePack that hasn't been committed yet.
        // Calling getMutable on this will trigger an assertion.
        if (!log.is<TableType>(ty))
            return true;

        promote(ty, log.getMutable<TableType>(ty));
        return true;
    }

    bool visit(TypePackId tp, const FreeTypePack&) override
    {
        // Surprise, it's actually a BoundTypePack that hasn't been committed yet.
        // Calling getMutable on this will trigger an assertion.
        if (!log.is<FreeTypePack>(tp))
            return true;

        promote(tp, log.getMutable<FreeTypePack>(tp));
        return true;
    }
};

static void promoteTypeLevels(TxnLog& log, const TypeArena* typeArena, TypeLevel minLevel, TypeId ty)
{
    // Type levels of types from other modules are already global, so we don't need to promote anything inside
    if (ty->owningArena != typeArena)
        return;

    PromoteTypeLevels ptl{log, typeArena, minLevel};
    ptl.traverse(ty);
}

void promoteTypeLevels(TxnLog& log, const TypeArena* typeArena, TypeLevel minLevel, TypePackId tp)
{
    // Type levels of types from other modules are already global, so we don't need to promote anything inside
    if (tp->owningArena != typeArena)
        return;

    PromoteTypeLevels ptl{log, typeArena, minLevel};
    ptl.traverse(tp);
}

struct SkipCacheForType final : TypeOnceVisitor
{
    SkipCacheForType(const DenseHashMap<TypeId, bool>& skipCacheForType, const TypeArena* typeArena)
        : TypeOnceVisitor("SkipCacheForType", /* skipBoundTypes */ false)
        , skipCacheForType(skipCacheForType)
        , typeArena(typeArena)
    {
    }

    bool visit(TypeId, const FreeType&) override
    {
        result = true;
        return false;
    }

    bool visit(TypeId, const BoundType&) override
    {
        result = true;
        return false;
    }

    bool visit(TypeId, const GenericType&) override
    {
        result = true;
        return false;
    }

    bool visit(TypeId, const BlockedType&) override
    {
        result = true;
        return false;
    }

    bool visit(TypeId, const PendingExpansionType&) override
    {
        result = true;
        return false;
    }

    bool visit(TypeId ty, const TableType&) override
    {
        // Types from other modules don't contain mutable elements and are ok to cache
        if (ty->owningArena != typeArena)
            return false;

        TableType& ttv = *getMutable<TableType>(ty);

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

    bool visit(TypeId ty) override
    {
        // Types from other modules don't contain mutable elements and are ok to cache
        if (ty->owningArena != typeArena)
            return false;

        const bool* prev = skipCacheForType.find(ty);

        if (prev && *prev)
        {
            result = true;
            return false;
        }

        return true;
    }

    bool visit(TypePackId tp) override
    {
        // Types from other modules don't contain mutable elements and are ok to cache
        if (tp->owningArena != typeArena)
            return false;

        return true;
    }

    bool visit(TypePackId tp, const FreeTypePack&) override
    {
        result = true;
        return false;
    }

    bool visit(TypePackId tp, const BoundTypePack&) override
    {
        result = true;
        return false;
    }

    bool visit(TypePackId tp, const GenericTypePack&) override
    {
        result = true;
        return false;
    }

    bool visit(TypePackId tp, const BlockedTypePack&) override
    {
        result = true;
        return false;
    }

    const DenseHashMap<TypeId, bool>& skipCacheForType;
    const TypeArena* typeArena = nullptr;
    bool result = false;
};

bool Widen::isDirty(TypeId ty)
{
    return log->is<SingletonType>(ty);
}

bool Widen::isDirty(TypePackId)
{
    return false;
}

TypeId Widen::clean(TypeId ty)
{
    LUAU_ASSERT(isDirty(ty));
    auto stv = log->getMutable<SingletonType>(ty);
    LUAU_ASSERT(stv);

    if (get<StringSingleton>(stv))
        return builtinTypes->stringType;
    else
    {
        // If this assert trips, it's likely we now have number singletons.
        LUAU_ASSERT(get<BooleanSingleton>(stv));
        return builtinTypes->booleanType;
    }
}

TypePackId Widen::clean(TypePackId)
{
    throw InternalCompilerError("Widen attempted to clean a dirty type pack?");
}

bool Widen::ignoreChildren(TypeId ty)
{
    if (get<ExternType>(ty))
        return true;

    return !log->is<UnionType>(ty);
}

TypeId Widen::operator()(TypeId ty)
{
    return substitute(ty).value_or(ty);
}

TypePackId Widen::operator()(TypePackId tp)
{
    return substitute(tp).value_or(tp);
}

std::optional<TypeError> hasUnificationTooComplex(const ErrorVec& errors)
{
    auto isUnificationTooComplex = [](const TypeError& te)
    {
        return nullptr != get<UnificationTooComplex>(te);
    };

    auto it = std::find_if(errors.begin(), errors.end(), isUnificationTooComplex);
    if (it == errors.end())
        return std::nullopt;
    else
        return *it;
}

std::optional<TypeError> hasCountMismatch(const ErrorVec& errors)
{
    auto isCountMismatch = [](const TypeError& te)
    {
        return nullptr != get<CountMismatch>(te);
    };

    auto it = std::find_if(errors.begin(), errors.end(), isCountMismatch);
    if (it == errors.end())
        return std::nullopt;
    else
        return *it;
}

// Used for tagged union matching heuristic, returns first singleton type field
static std::optional<std::pair<Luau::Name, const SingletonType*>> getTableMatchTag(TypeId type)
{
    if (auto ttv = getTableType(type))
    {
        for (auto&& [name, prop] : ttv->props)
        {
            if (auto sing = get<SingletonType>(follow(prop.type_DEPRECATED())))
                return {{name, sing}};
        }
    }

    return std::nullopt;
}

template<typename TY_A, typename TY_B>
static bool subsumes(TY_A* left, TY_B* right)
{
    return left->level.subsumes(right->level);
}

TypeMismatch::Context Unifier::mismatchContext()
{
    switch (variance)
    {
    case Covariant:
        return TypeMismatch::CovariantContext;
    case Invariant:
        return TypeMismatch::InvariantContext;
    default:
        LUAU_ASSERT(false); // This codepath should be unreachable.
        return TypeMismatch::CovariantContext;
    }
}

Unifier::Unifier(NotNull<Normalizer> normalizer, NotNull<Scope> scope, const Location& location, Variance variance, TxnLog* parentLog)
    : types(normalizer->arena)
    , builtinTypes(normalizer->builtinTypes)
    , normalizer(normalizer)
    , scope(scope)
    , log(parentLog)
    , location(location)
    , variance(variance)
    , sharedState(*normalizer->sharedState)
{
    LUAU_ASSERT(sharedState.iceHandler);
}

void Unifier::tryUnify(TypeId subTy, TypeId superTy, bool isFunctionCall, bool isIntersection, const LiteralProperties* literalProperties)
{
    sharedState.counters.iterationCount = 0;

    tryUnify_(subTy, superTy, isFunctionCall, isIntersection, literalProperties);
}

static bool isBlocked(const TxnLog& log, TypeId ty)
{
    ty = log.follow(ty);
    return get<BlockedType>(ty) || get<PendingExpansionType>(ty);
}

static bool isBlocked(const TxnLog& log, TypePackId tp)
{
    tp = log.follow(tp);
    return get<BlockedTypePack>(tp);
}

void Unifier::tryUnify_(TypeId subTy, TypeId superTy, bool isFunctionCall, bool isIntersection, const LiteralProperties* literalProperties)
{
    RecursionLimiter _ra("Unifier::tryUnify_", &sharedState.counters.recursionCount, sharedState.counters.recursionLimit);

    ++sharedState.counters.iterationCount;

    if (sharedState.counters.iterationLimit > 0 && sharedState.counters.iterationLimit < sharedState.counters.iterationCount)
    {
        reportError(location, UnificationTooComplex{});
        return;
    }

    superTy = log.follow(superTy);
    subTy = log.follow(subTy);

    if (superTy == subTy)
        return;

    if (isBlocked(log, subTy) && isBlocked(log, superTy))
    {
        blockedTypes.push_back(subTy);
        blockedTypes.push_back(superTy);
    }
    else if (isBlocked(log, subTy))
        blockedTypes.push_back(subTy);
    else if (isBlocked(log, superTy))
        blockedTypes.push_back(superTy);

    if (log.get<TypeFunctionInstanceType>(superTy))
        ice("Unexpected TypeFunctionInstanceType superTy");

    if (log.get<TypeFunctionInstanceType>(subTy))
        ice("Unexpected TypeFunctionInstanceType subTy");

    auto superFree = log.getMutable<FreeType>(superTy);
    auto subFree = log.getMutable<FreeType>(subTy);

    if (superFree && subFree && subsumes(superFree, subFree))
    {
        if (!occursCheck(subTy, superTy, /* reversed = */ false))
            log.replace(subTy, BoundType(superTy));

        return;
    }
    else if (superFree && subFree)
    {
        if (!occursCheck(superTy, subTy, /* reversed = */ true))
        {
            if (subsumes(superFree, subFree))
            {
                log.changeLevel(subTy, superFree->level);
            }

            log.replace(superTy, BoundType(subTy));
        }

        return;
    }
    else if (superFree)
    {
        // Unification can't change the level of a generic.
        auto subGeneric = log.getMutable<GenericType>(subTy);
        if (subGeneric && !subsumes(subGeneric, superFree))
        {
            // TODO: a more informative error message? CLI-39912
            reportError(location, GenericError{"Generic subtype escaping scope"});
            return;
        }

        if (!occursCheck(superTy, subTy, /* reversed = */ true))
        {
            promoteTypeLevels(log, types, superFree->level, subTy);

            Widen widen{types, builtinTypes};
            log.replace(superTy, BoundType(widen(subTy)));
        }

        return;
    }
    else if (subFree)
    {
        // Normally, if the subtype is free, it should not be bound to any, unknown, or error types.
        // But for bug compatibility, we'll only apply this rule to unknown. Doing this will silence cascading type errors.
        if (log.get<UnknownType>(superTy))
            return;

        // Unification can't change the level of a generic.
        auto superGeneric = log.getMutable<GenericType>(superTy);
        if (superGeneric && !subsumes(superGeneric, subFree))
        {
            // TODO: a more informative error message? CLI-39912
            reportError(location, GenericError{"Generic supertype escaping scope"});
            return;
        }

        if (!occursCheck(subTy, superTy, /* reversed = */ false))
        {
            promoteTypeLevels(log, types, subFree->level, superTy);
            log.replace(subTy, BoundType(superTy));
        }

        return;
    }

    if (log.get<AnyType>(superTy))
        return tryUnifyWithAny(subTy, builtinTypes->anyType);

    if (log.get<AnyType>(subTy))
    {
        if (normalize)
        {
            // TODO: there are probably cheaper ways to check if any <: T.
            std::shared_ptr<const NormalizedType> superNorm = normalizer->normalize(superTy);

            if (!superNorm)
                return reportError(location, NormalizationTooComplex{});

            if (!log.get<AnyType>(superNorm->tops))
                failure = true;
        }
        else
            failure = true;
        return tryUnifyWithAny(superTy, builtinTypes->anyType);
    }

    if (log.get<NeverType>(subTy))
        return tryUnifyWithAny(superTy, builtinTypes->neverType);

    auto& cache = sharedState.cachedUnify;

    // What if the types are immutable and we proved their relation before
    bool cacheEnabled = !isFunctionCall && !isIntersection && variance == Invariant;

    if (cacheEnabled)
    {
        if (cache.contains({subTy, superTy}))
            return;

        if (auto error = sharedState.cachedUnifyError.find({subTy, superTy}))
        {
            reportError(location, *error);
            return;
        }
    }

    // If we have seen this pair of types before, we are currently recursing into cyclic types.
    // Here, we assume that the types unify.  If they do not, we will find out as we roll back
    // the stack.

    if (log.haveSeen(superTy, subTy))
        return;

    log.pushSeen(superTy, subTy);

    size_t errorCount = errors.size();

    if (const UnionType* subUnion = log.getMutable<UnionType>(subTy))
    {
        tryUnifyUnionWithType(subTy, subUnion, superTy);
    }
    else if (const IntersectionType* uv = log.getMutable<IntersectionType>(superTy))
    {
        tryUnifyTypeWithIntersection(subTy, superTy, uv);
    }
    else if (const UnionType* uv = log.getMutable<UnionType>(superTy))
    {
        tryUnifyTypeWithUnion(subTy, superTy, uv, cacheEnabled, isFunctionCall);
    }
    else if (const IntersectionType* uv = log.getMutable<IntersectionType>(subTy))
    {
        tryUnifyIntersectionWithType(subTy, uv, superTy, cacheEnabled, isFunctionCall);
    }
    else if (log.get<AnyType>(subTy))
    {
        tryUnifyWithAny(superTy, builtinTypes->unknownType);
        failure = true;
    }
    else if (log.get<ErrorType>(subTy) && log.get<ErrorType>(superTy))
    {
        // error <: error
    }
    else if (log.get<ErrorType>(superTy))
    {
        tryUnifyWithAny(subTy, builtinTypes->errorType);
        failure = true;
    }
    else if (log.get<ErrorType>(subTy))
    {
        tryUnifyWithAny(superTy, builtinTypes->errorType);
        failure = true;
    }
    else if (log.get<UnknownType>(superTy))
    {
        // At this point, all the supertypes of `error` have been handled,
        // and if `error </: T` then `T <: unknown`.
        tryUnifyWithAny(subTy, builtinTypes->unknownType);
    }
    else if (log.get<UnknownType>(superTy))
    {
        tryUnifyWithAny(subTy, builtinTypes->unknownType);
    }
    else if (log.getMutable<PrimitiveType>(superTy) && log.getMutable<PrimitiveType>(subTy))
        tryUnifyPrimitives(subTy, superTy);

    else if ((log.getMutable<PrimitiveType>(superTy) || log.getMutable<SingletonType>(superTy)) && log.getMutable<SingletonType>(subTy))
        tryUnifySingletons(subTy, superTy);

    else if (auto ptv = get<PrimitiveType>(superTy); ptv && ptv->type == PrimitiveType::Function && get<FunctionType>(subTy))
    {
        // Ok.  Do nothing.  forall functions F, F <: function
    }

    else if (isPrim(superTy, PrimitiveType::Table) && (get<TableType>(subTy) || get<MetatableType>(subTy)))
    {
        // Ok, do nothing: forall tables T, T <: table
    }

    else if (log.getMutable<FunctionType>(superTy) && log.getMutable<FunctionType>(subTy))
        tryUnifyFunctions(subTy, superTy, isFunctionCall);

    else if (auto table = log.get<PrimitiveType>(superTy); table && table->type == PrimitiveType::Table)
        tryUnify(subTy, builtinTypes->emptyTableType, isFunctionCall, isIntersection);
    else if (auto table = log.get<PrimitiveType>(subTy); table && table->type == PrimitiveType::Table)
        tryUnify(builtinTypes->emptyTableType, superTy, isFunctionCall, isIntersection);

    else if (log.getMutable<TableType>(superTy) && log.getMutable<TableType>(subTy))
    {
        tryUnifyTables(subTy, superTy, isIntersection, literalProperties);
    }
    else if (log.get<TableType>(superTy) && (log.get<PrimitiveType>(subTy) || log.get<SingletonType>(subTy)))
    {
        tryUnifyScalarShape(subTy, superTy, /*reversed*/ false);
    }
    else if (log.get<TableType>(subTy) && (log.get<PrimitiveType>(superTy) || log.get<SingletonType>(superTy)))
    {
        tryUnifyScalarShape(subTy, superTy, /*reversed*/ true);
    }

    // tryUnifyWithMetatable assumes its first argument is a MetatableType. The check is otherwise symmetrical.
    else if (log.getMutable<MetatableType>(superTy))
        tryUnifyWithMetatable(subTy, superTy, /*reversed*/ false);
    else if (log.getMutable<MetatableType>(subTy))
        tryUnifyWithMetatable(superTy, subTy, /*reversed*/ true);

    else if (log.getMutable<ExternType>(superTy))
        tryUnifyWithExternType(subTy, superTy, /*reversed*/ false);

    // Unification of Luau types with extern types is almost, but not quite symmetrical.
    // The order in which we perform this test is significant in the case that both types are extern types.
    else if (log.getMutable<ExternType>(subTy))
        tryUnifyWithExternType(subTy, superTy, /*reversed*/ true);

    else if (log.get<NegationType>(superTy) || log.get<NegationType>(subTy))
        tryUnifyNegations(subTy, superTy);

    // If the normalizer hits resource limits, we can't show it's uninhabited, so, we should error.
    else if (checkInhabited && normalizer->isInhabited(subTy) == NormalizationResult::False)
    {
    }
    else
        reportError(location, TypeMismatch{superTy, subTy, mismatchContext()});

    if (cacheEnabled)
        cacheResult(subTy, superTy, errorCount);

    log.popSeen(superTy, subTy);
}

void Unifier::tryUnifyUnionWithType(TypeId subTy, const UnionType* subUnion, TypeId superTy)
{
    // A | B <: T if and only if A <: T and B <: T
    bool failed = false;
    bool errorsSuppressed = true;
    std::optional<TypeError> unificationTooComplex;
    std::optional<TypeError> firstFailedOption;

    std::vector<TxnLog> logs;

    for (TypeId type : subUnion->options)
    {
        std::unique_ptr<Unifier> innerState = makeChildUnifier();
        innerState->tryUnify_(type, superTy);

        if (auto e = hasUnificationTooComplex(innerState->errors))
            unificationTooComplex = e;
        else if (innerState->failure)
        {
            // If errors were suppressed, we store the log up, so we can commit it if no other option succeeds.
            if (innerState->errors.empty())
                logs.push_back(std::move(innerState->log));
            // 'nil' option is skipped from extended report because we present the type in a special way - 'T?'
            else if (!firstFailedOption && !isNil(type))
                firstFailedOption = {innerState->errors.front()};

            failed = true;
            errorsSuppressed &= innerState->errors.empty();
        }
    }

    log.concatAsUnion(combineLogsIntoUnion(std::move(logs)), NotNull{types});

    if (unificationTooComplex)
        reportError(*unificationTooComplex);
    else if (failed)
    {
        if (firstFailedOption)
            reportError(location, TypeMismatch{superTy, subTy, "Not all union options are compatible.", *firstFailedOption, mismatchContext()});
        else if (!errorsSuppressed)
            reportError(location, TypeMismatch{superTy, subTy, mismatchContext()});
        failure = true;
    }
}

void Unifier::tryUnifyTypeWithUnion(TypeId subTy, TypeId superTy, const UnionType* uv, bool cacheEnabled, bool isFunctionCall)
{
    // T <: A | B if T <: A or T <: B
    bool found = false;
    bool errorsSuppressed = false;
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

    if (!foundHeuristic)
    {
        for (size_t i = 0; i < uv->options.size(); ++i)
        {
            TypeId type = uv->options[i];

            if (subTy == type)
            {
                foundHeuristic = true;
                startIndex = i;
                break;
            }
        }
    }

    if (!foundHeuristic && cacheEnabled)
    {
        auto& cache = sharedState.cachedUnify;

        for (size_t i = 0; i < uv->options.size(); ++i)
        {
            TypeId type = uv->options[i];

            if (cache.contains({subTy, type}))
            {
                startIndex = i;
                break;
            }
        }
    }

    std::vector<TxnLog> logs;

    for (size_t i = 0; i < uv->options.size(); ++i)
    {
        TypeId type = uv->options[(i + startIndex) % uv->options.size()];
        std::unique_ptr<Unifier> innerState = makeChildUnifier();
        innerState->normalize = false;
        innerState->tryUnify_(subTy, type, isFunctionCall);

        if (!innerState->failure)
        {
            found = true;
            log.concat(std::move(innerState->log));
            break;
        }
        else if (innerState->errors.empty())
        {
            errorsSuppressed = true;
        }
        else if (auto e = hasUnificationTooComplex(innerState->errors))
        {
            unificationTooComplex = e;
        }
        else if (!isNil(type))
        {
            failedOptionCount++;

            if (!failedOption)
                failedOption = {innerState->errors.front()};
        }
    }

    if (unificationTooComplex)
    {
        reportError(*unificationTooComplex);
    }
    else if (!found && normalize)
    {
        // It is possible that T <: A | B even though T </: A and T </:B
        // for example boolean <: true | false.
        // We deal with this by type normalization.
        std::unique_ptr<Unifier> innerState = makeChildUnifier();

        std::shared_ptr<const NormalizedType> subNorm = normalizer->normalize(subTy);
        std::shared_ptr<const NormalizedType> superNorm = normalizer->normalize(superTy);
        if (!subNorm || !superNorm)
            return reportError(location, NormalizationTooComplex{});
        else if ((failedOptionCount == 1 || foundHeuristic) && failedOption)
            innerState->tryUnifyNormalizedTypes(
                subTy, superTy, *subNorm, *superNorm, "None of the union options are compatible. For example:", *failedOption
            );
        else
            innerState->tryUnifyNormalizedTypes(subTy, superTy, *subNorm, *superNorm, "none of the union options are compatible");

        if (!innerState->failure)
            log.concat(std::move(innerState->log));
        else if (errorsSuppressed || innerState->errors.empty())
            failure = true;
        else
            reportError(std::move(innerState->errors.front()));
    }
    else if (!found && normalize)
    {
        // It is possible that T <: A | B even though T </: A and T </:B
        // for example boolean <: true | false.
        // We deal with this by type normalization.
        std::shared_ptr<const NormalizedType> subNorm = normalizer->normalize(subTy);
        std::shared_ptr<const NormalizedType> superNorm = normalizer->normalize(superTy);
        if (!subNorm || !superNorm)
            reportError(location, NormalizationTooComplex{});
        else if ((failedOptionCount == 1 || foundHeuristic) && failedOption)
            tryUnifyNormalizedTypes(subTy, superTy, *subNorm, *superNorm, "None of the union options are compatible. For example:", *failedOption);
        else
            tryUnifyNormalizedTypes(subTy, superTy, *subNorm, *superNorm, "none of the union options are compatible");
    }
    else if (!found)
    {
        if (errorsSuppressed)
            failure = true;
        else if ((failedOptionCount == 1 || foundHeuristic) && failedOption)
            reportError(
                location, TypeMismatch{superTy, subTy, "None of the union options are compatible. For example:", *failedOption, mismatchContext()}
            );
        else
            reportError(location, TypeMismatch{superTy, subTy, "none of the union options are compatible", mismatchContext()});
    }
}

void Unifier::tryUnifyTypeWithIntersection(TypeId subTy, TypeId superTy, const IntersectionType* uv)
{
    std::optional<TypeError> unificationTooComplex;
    std::optional<TypeError> firstFailedOption;

    std::vector<TxnLog> logs;

    // T <: A & B if and only if  T <: A and T <: B
    for (TypeId type : uv->parts)
    {
        std::unique_ptr<Unifier> innerState = makeChildUnifier();
        innerState->tryUnify_(subTy, type, /*isFunctionCall*/ false, /*isIntersection*/ true);

        if (auto e = hasUnificationTooComplex(innerState->errors))
            unificationTooComplex = e;
        else if (!innerState->errors.empty())
        {
            if (!firstFailedOption)
                firstFailedOption = {innerState->errors.front()};
        }

        log.concat(std::move(innerState->log));
        failure |= innerState->failure;
    }

    if (unificationTooComplex)
        reportError(*unificationTooComplex);
    else if (firstFailedOption)
        reportError(location, TypeMismatch{superTy, subTy, "Not all intersection parts are compatible.", *firstFailedOption, mismatchContext()});
}

struct NegationTypeFinder : TypeOnceVisitor
{
    bool found = false;

    bool visit(TypeId ty) override
    {
        return !found;
    }

    bool visit(TypeId ty, const NegationType&) override
    {
        found = true;
        return !found;
    }
};

void Unifier::tryUnifyIntersectionWithType(TypeId subTy, const IntersectionType* uv, TypeId superTy, bool cacheEnabled, bool isFunctionCall)
{
    // A & B <: T if A <: T or B <: T
    bool found = false;
    bool errorsSuppressed = false;
    std::optional<TypeError> unificationTooComplex;

    size_t startIndex = 0;

    if (cacheEnabled)
    {
        auto& cache = sharedState.cachedUnify;

        for (size_t i = 0; i < uv->parts.size(); ++i)
        {
            TypeId type = uv->parts[i];

            if (cache.contains({type, superTy}))
            {
                startIndex = i;
                break;
            }
        }
    }

    std::vector<TxnLog> logs;

    for (size_t i = 0; i < uv->parts.size(); ++i)
    {
        TypeId type = uv->parts[(i + startIndex) % uv->parts.size()];
        std::unique_ptr<Unifier> innerState = makeChildUnifier();
        innerState->normalize = false;
        innerState->tryUnify_(type, superTy, isFunctionCall);

        // TODO: This sets errorSuppressed to true if any of the parts is error-suppressing,
        // in paricular any & T is error-suppressing. Really, errorSuppressed should be true if
        // all of the parts are error-suppressing, but that fails to typecheck lua-apps.
        if (innerState->errors.empty())
        {
            found = true;
            errorsSuppressed = innerState->failure;
            if (innerState->failure)
                logs.push_back(std::move(innerState->log));
            else
            {
                errorsSuppressed = false;
                log.concat(std::move(innerState->log));
                break;
            }
        }
        else if (auto e = hasUnificationTooComplex(innerState->errors))
        {
            unificationTooComplex = e;
        }
    }

    if (errorsSuppressed)
        log.concat(std::move(logs.front()));

    if (unificationTooComplex)
        reportError(*unificationTooComplex);
    else if (!found && normalize)
    {
        // It is possible that A & B <: T even though A </: T and B </: T
        // for example string? & number? <: nil.
        // We deal with this by type normalization.

        std::shared_ptr<const NormalizedType> subNorm = normalizer->normalize(subTy);
        std::shared_ptr<const NormalizedType> superNorm = normalizer->normalize(superTy);
        if (subNorm && superNorm)
            tryUnifyNormalizedTypes(subTy, superTy, *subNorm, *superNorm, "none of the intersection parts are compatible");
        else
            reportError(location, NormalizationTooComplex{});
    }
    else if (!found)
    {
        reportError(location, TypeMismatch{superTy, subTy, "none of the intersection parts are compatible", mismatchContext()});
    }
    else if (errorsSuppressed)
        failure = true;
}

void Unifier::tryUnifyNormalizedTypes(
    TypeId subTy,
    TypeId superTy,
    const NormalizedType& subNorm,
    const NormalizedType& superNorm,
    std::string reason,
    std::optional<TypeError> error
)
{
    if (get<AnyType>(superNorm.tops))
        return;
    else if (get<AnyType>(subNorm.tops))
    {
        failure = true;
        return;
    }

    if (get<ErrorType>(subNorm.errors))
        if (!get<ErrorType>(superNorm.errors))
        {
            failure = true;
            return;
        }

    if (get<UnknownType>(superNorm.tops))
        return;

    if (get<UnknownType>(subNorm.tops))
        return reportError(location, TypeMismatch{superTy, subTy, std::move(reason), std::move(error), mismatchContext()});

    if (get<PrimitiveType>(subNorm.booleans))
    {
        if (!get<PrimitiveType>(superNorm.booleans))
            return reportError(location, TypeMismatch{superTy, subTy, std::move(reason), std::move(error), mismatchContext()});
    }
    else if (const SingletonType* stv = get<SingletonType>(subNorm.booleans))
    {
        if (!get<PrimitiveType>(superNorm.booleans) && stv != get<SingletonType>(superNorm.booleans))
            return reportError(location, TypeMismatch{superTy, subTy, std::move(reason), std::move(error), mismatchContext()});
    }

    if (get<PrimitiveType>(subNorm.nils))
        if (!get<PrimitiveType>(superNorm.nils))
            return reportError(location, TypeMismatch{superTy, subTy, std::move(reason), std::move(error), mismatchContext()});

    if (get<PrimitiveType>(subNorm.numbers))
        if (!get<PrimitiveType>(superNorm.numbers))
            return reportError(location, TypeMismatch{superTy, subTy, std::move(reason), std::move(error), mismatchContext()});

    if (!isSubtype(subNorm.strings, superNorm.strings))
        return reportError(location, TypeMismatch{superTy, subTy, std::move(reason), std::move(error), mismatchContext()});

    if (get<PrimitiveType>(subNorm.threads))
        if (!get<PrimitiveType>(superNorm.errors))
            return reportError(location, TypeMismatch{superTy, subTy, std::move(reason), std::move(error), mismatchContext()});

    for (const auto& [subExternType, _] : subNorm.externTypes.externTypes)
    {
        bool found = false;
        const ExternType* subCtv = get<ExternType>(subExternType);
        LUAU_ASSERT(subCtv);

        for (const auto& [superExternType, superNegations] : superNorm.externTypes.externTypes)
        {
            const ExternType* superCtv = get<ExternType>(superExternType);
            LUAU_ASSERT(superCtv);

            if (isSubclass(subCtv, superCtv))
            {
                found = true;

                for (TypeId negation : superNegations)
                {
                    const ExternType* negationCtv = get<ExternType>(negation);
                    LUAU_ASSERT(negationCtv);

                    if (isSubclass(subCtv, negationCtv))
                    {
                        found = false;
                        break;
                    }
                }

                if (found)
                    break;
            }
        }

        if (!found)
        {
            return reportError(location, TypeMismatch{superTy, subTy, std::move(reason), std::move(error), mismatchContext()});
        }
    }

    for (TypeId subTable : subNorm.tables)
    {
        bool found = false;
        for (TypeId superTable : superNorm.tables)
        {
            if (isPrim(superTable, PrimitiveType::Table))
            {
                found = true;
                break;
            }

            std::unique_ptr<Unifier> innerState = makeChildUnifier();

            innerState->tryUnify(subTable, superTable);

            if (innerState->errors.empty())
            {
                found = true;
                log.concat(std::move(innerState->log));
                break;
            }
            else if (auto e = hasUnificationTooComplex(innerState->errors))
                return reportError(*e);
        }
        if (!found)
            return reportError(location, TypeMismatch{superTy, subTy, std::move(reason), std::move(error), mismatchContext()});
    }

    if (!subNorm.functions.isNever())
    {
        if (superNorm.functions.isNever())
            return reportError(location, TypeMismatch{superTy, subTy, std::move(reason), std::move(error), mismatchContext()});
        for (TypeId superFun : superNorm.functions.parts)
        {
            std::unique_ptr<Unifier> innerState = makeChildUnifier();
            const FunctionType* superFtv = get<FunctionType>(superFun);
            if (!superFtv)
                return reportError(location, TypeMismatch{superTy, subTy, std::move(reason), std::move(error), mismatchContext()});
            TypePackId tgt = innerState->tryApplyOverloadedFunction(subTy, subNorm.functions, superFtv->argTypes);
            innerState->tryUnify_(tgt, superFtv->retTypes);
            if (innerState->errors.empty())
                log.concat(std::move(innerState->log));
            else if (auto e = hasUnificationTooComplex(innerState->errors))
                return reportError(*e);
            else
                return reportError(location, TypeMismatch{superTy, subTy, std::move(reason), std::move(error), mismatchContext()});
        }
    }

    for (auto& [tyvar, subIntersect] : subNorm.tyvars)
    {
        auto found = superNorm.tyvars.find(tyvar);
        if (found == superNorm.tyvars.end())
            tryUnifyNormalizedTypes(subTy, superTy, *subIntersect, superNorm, reason, error);
        else
            tryUnifyNormalizedTypes(subTy, superTy, *subIntersect, *found->second, reason, error);
        if (!errors.empty())
            return;
    }
}

TypePackId Unifier::tryApplyOverloadedFunction(TypeId function, const NormalizedFunctionType& overloads, TypePackId args)
{
    if (overloads.isNever())
    {
        reportError(location, CannotCallNonFunction{function});
        return builtinTypes->errorTypePack;
    }

    std::optional<TypePackId> result;
    const FunctionType* firstFun = nullptr;
    for (TypeId overload : overloads.parts)
    {
        if (const FunctionType* ftv = get<FunctionType>(overload))
        {
            // TODO: instantiate generics?
            if (ftv->generics.empty() && ftv->genericPacks.empty())
            {
                if (!firstFun)
                    firstFun = ftv;
                std::unique_ptr<Unifier> innerState = makeChildUnifier();
                innerState->tryUnify_(args, ftv->argTypes);
                if (innerState->errors.empty())
                {
                    log.concat(std::move(innerState->log));
                    if (result)
                    {
                        innerState->log.clear();
                        innerState->tryUnify_(*result, ftv->retTypes);
                        if (innerState->errors.empty())
                            log.concat(std::move(innerState->log));
                        // Annoyingly, since we don't support intersection of generic type packs,
                        // the intersection may fail. We rather arbitrarily use the first matching overload
                        // in that case.
                        else if (std::optional<TypePackId> intersect = normalizer->intersectionOfTypePacks(*result, ftv->retTypes))
                            result = intersect;
                    }
                    else
                        result = ftv->retTypes;
                }
                else if (auto e = hasUnificationTooComplex(innerState->errors))
                {
                    reportError(*e);
                    return builtinTypes->errorRecoveryTypePack(args);
                }
            }
        }
    }

    if (result)
        return *result;
    else if (firstFun)
    {
        // TODO: better error reporting?
        // The logic for error reporting overload resolution
        // is currently over in TypeInfer.cpp, should we move it?
        reportError(location, GenericError{"No matching overload."});
        return builtinTypes->errorRecoveryTypePack(firstFun->retTypes);
    }
    else
    {
        reportError(location, CannotCallNonFunction{function});
        return builtinTypes->errorTypePack;
    }
}

bool Unifier::canCacheResult(TypeId subTy, TypeId superTy)
{
    bool* superTyInfo = sharedState.skipCacheForType.find(superTy);

    if (superTyInfo && *superTyInfo)
        return false;

    bool* subTyInfo = sharedState.skipCacheForType.find(subTy);

    if (subTyInfo && *subTyInfo)
        return false;

    auto skipCacheFor = [this](TypeId ty)
    {
        SkipCacheForType visitor{sharedState.skipCacheForType, types};
        visitor.traverse(ty);

        sharedState.skipCacheForType[ty] = visitor.result;

        return visitor.result;
    };

    if (!superTyInfo && skipCacheFor(superTy))
        return false;

    if (!subTyInfo && skipCacheFor(subTy))
        return false;

    return true;
}

void Unifier::cacheResult(TypeId subTy, TypeId superTy, size_t prevErrorCount)
{
    if (errors.size() == prevErrorCount)
    {
        if (canCacheResult(subTy, superTy))
            sharedState.cachedUnify.insert({subTy, superTy});
    }
    else if (errors.size() == prevErrorCount + 1)
    {
        if (canCacheResult(subTy, superTy))
            sharedState.cachedUnifyError[{subTy, superTy}] = errors.back().data;
    }
}

struct WeirdIter
{
    TypePackId packId;
    TxnLog& log;
    TypePack* pack;
    size_t index;
    bool growing;
    TypeLevel level;
    Scope* scope = nullptr;

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

    std::optional<TypePackId> tail() const
    {
        if (!pack)
            return packId;

        LUAU_ASSERT(index == pack->head.size());
        return pack->tail;
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
        return nullptr != log.getMutable<FreeTypePack>(packId);
    }

    void grow(TypePackId newTail)
    {
        LUAU_ASSERT(canGrow());
        LUAU_ASSERT(log.getMutable<TypePack>(newTail));

        auto freePack = log.getMutable<FreeTypePack>(packId);

        level = freePack->level;
        if (freePack->scope != nullptr)
            scope = freePack->scope;
        log.replace(packId, BoundTypePack(newTail));
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
    std::unique_ptr<Unifier> s = makeChildUnifier();
    s->tryUnify_(subTy, superTy);

    return s->errors;
}

ErrorVec Unifier::canUnify(TypePackId subTy, TypePackId superTy, bool isFunctionCall)
{
    std::unique_ptr<Unifier> s = makeChildUnifier();
    s->tryUnify_(subTy, superTy, isFunctionCall);

    return s->errors;
}

void Unifier::tryUnify(TypePackId subTp, TypePackId superTp, bool isFunctionCall)
{
    sharedState.counters.iterationCount = 0;

    tryUnify_(subTp, superTp, isFunctionCall);
}

/*
 * This is quite tricky: we are walking two rope-like structures and unifying corresponding elements.
 * If one is longer than the other, but the short end is free, we grow it to the required length.
 */
void Unifier::tryUnify_(TypePackId subTp, TypePackId superTp, bool isFunctionCall)
{
    RecursionLimiter _ra("Unifier::tryUnify_", &sharedState.counters.recursionCount, sharedState.counters.recursionLimit);

    ++sharedState.counters.iterationCount;

    if (sharedState.counters.iterationLimit > 0 && sharedState.counters.iterationLimit < sharedState.counters.iterationCount)
    {
        reportError(location, UnificationTooComplex{});
        return;
    }

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

    if (log.haveSeen(superTp, subTp))
        return;

    if (isBlocked(log, subTp) && isBlocked(log, superTp))
    {
        blockedTypePacks.push_back(subTp);
        blockedTypePacks.push_back(superTp);
    }
    else if (isBlocked(log, subTp))
        blockedTypePacks.push_back(subTp);
    else if (isBlocked(log, superTp))
        blockedTypePacks.push_back(superTp);

    if (auto superFree = log.getMutable<FreeTypePack>(superTp))
    {
        if (!occursCheck(superTp, subTp, /* reversed = */ true))
        {
            Widen widen{types, builtinTypes};
            log.replace(superTp, Unifiable::Bound<TypePackId>(widen(subTp)));
        }
    }
    else if (auto subFree = log.getMutable<FreeTypePack>(subTp))
    {
        if (!occursCheck(subTp, superTp, /* reversed = */ false))
        {
            log.replace(subTp, Unifiable::Bound<TypePackId>(superTp));
        }
    }
    else if (log.getMutable<ErrorTypePack>(superTp))
        tryUnifyWithAny(subTp, superTp);
    else if (log.getMutable<ErrorTypePack>(subTp))
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
        auto [superTypes, superTail] = flatten(superTp, log);
        auto [subTypes, subTail] = flatten(subTp, log);

        bool noInfiniteGrowth = (superTypes.size() != subTypes.size()) && (superTail && log.getMutable<FreeTypePack>(*superTail)) &&
                                (subTail && log.getMutable<FreeTypePack>(*subTail));

        auto superIter = WeirdIter(superTp, log);
        auto subIter = WeirdIter(subTp, log);

        superIter.scope = scope.get();
        subIter.scope = scope.get();

        auto mkFreshType = [this](Scope* scope, TypeLevel level)
        {
            if (FFlag::LuauSolverV2)
                return freshType(NotNull{types}, builtinTypes, scope);
            else
                return types->freshType(builtinTypes, scope, level);
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
                subIter.pushType(mkFreshType(subIter.scope, subIter.level));
            }

            if (subIter.good() && superIter.growing)
            {
                superIter.pushType(mkFreshType(superIter.scope, superIter.level));
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
                const bool lFreeTail = superTpv->tail && log.getMutable<FreeTypePack>(log.follow(*superTpv->tail)) != nullptr;
                const bool rFreeTail = subTpv->tail && log.getMutable<FreeTypePack>(log.follow(*subTpv->tail)) != nullptr;
                if (lFreeTail && rFreeTail)
                {
                    tryUnify_(*subTpv->tail, *superTpv->tail);
                }
                else if (lFreeTail)
                {
                    tryUnify_(emptyTp, *superTpv->tail);
                }
                else if (rFreeTail)
                {
                    tryUnify_(emptyTp, *subTpv->tail);
                }
                else if (subTpv->tail && superTpv->tail)
                {
                    if (log.getMutable<VariadicTypePack>(superIter.packId))
                        tryUnifyVariadics(subIter.packId, superIter.packId, false, int(subIter.index));
                    else if (log.getMutable<VariadicTypePack>(subIter.packId))
                        tryUnifyVariadics(superIter.packId, subIter.packId, true, int(superIter.index));
                    else
                        tryUnify_(*subTpv->tail, *superTpv->tail);
                }

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
                if (ctx == CountMismatch::FunctionResult || ctx == CountMismatch::ExprListResult)
                    std::swap(expectedSize, actualSize);
                reportError(location, CountMismatch{expectedSize, std::nullopt, actualSize, ctx});

                while (superIter.good())
                {
                    tryUnify_(*superIter, builtinTypes->errorType);
                    superIter.advance();
                }

                while (subIter.good())
                {
                    tryUnify_(*subIter, builtinTypes->errorType);
                    subIter.advance();
                }

                return;
            }

        } while (!noInfiniteGrowth);
    }
    else
    {
        reportError(location, TypePackMismatch{subTp, superTp});
    }
}

void Unifier::tryUnifyPrimitives(TypeId subTy, TypeId superTy)
{
    const PrimitiveType* superPrim = get<PrimitiveType>(superTy);
    const PrimitiveType* subPrim = get<PrimitiveType>(subTy);
    if (!superPrim || !subPrim)
        ice("passed non primitive types to unifyPrimitives");

    if (superPrim->type != subPrim->type)
        reportError(location, TypeMismatch{superTy, subTy, mismatchContext()});
}

void Unifier::tryUnifySingletons(TypeId subTy, TypeId superTy)
{
    const PrimitiveType* superPrim = get<PrimitiveType>(superTy);
    const SingletonType* superSingleton = get<SingletonType>(superTy);
    const SingletonType* subSingleton = get<SingletonType>(subTy);

    if ((!superPrim && !superSingleton) || !subSingleton)
        ice("passed non singleton/primitive types to unifySingletons");

    if (superSingleton && *superSingleton == *subSingleton)
        return;

    if (superPrim && superPrim->type == PrimitiveType::Boolean && get<BooleanSingleton>(subSingleton) && variance == Covariant)
        return;

    if (superPrim && superPrim->type == PrimitiveType::String && get<StringSingleton>(subSingleton) && variance == Covariant)
        return;

    reportError(location, TypeMismatch{superTy, subTy, mismatchContext()});
}

void Unifier::tryUnifyFunctions(TypeId subTy, TypeId superTy, bool isFunctionCall)
{
    FunctionType* superFunction = log.getMutable<FunctionType>(superTy);
    FunctionType* subFunction = log.getMutable<FunctionType>(subTy);

    if (!superFunction || !subFunction)
        ice("passed non-function types to unifyFunction");

    size_t numGenerics = superFunction->generics.size();
    size_t numGenericPacks = superFunction->genericPacks.size();

    bool shouldInstantiate = (numGenerics == 0 && subFunction->generics.size() > 0) || (numGenericPacks == 0 && subFunction->genericPacks.size() > 0);

    // TODO: This is unsound when the context is invariant, but the annotation burden without allowing it and without
    // read-only properties is too high for lua-apps. Read-only properties _should_ resolve their issue by allowing
    // generic methods in tables to be marked read-only.
    if (FFlag::LuauInstantiateInSubtyping && shouldInstantiate)
    {
        std::unique_ptr<Instantiation> instantiation = std::make_unique<Instantiation>(&log, types, builtinTypes, scope->level, scope);

        std::optional<TypeId> instantiated = instantiation->substitute(subTy);
        if (instantiated.has_value())
        {
            subFunction = log.getMutable<FunctionType>(*instantiated);

            if (!subFunction)
                ice("instantiation made a function type into a non-function type in unifyFunction");

            numGenerics = std::min(superFunction->generics.size(), subFunction->generics.size());
            numGenericPacks = std::min(superFunction->genericPacks.size(), subFunction->genericPacks.size());
        }
        else
        {
            reportError(location, UnificationTooComplex{});
        }
    }
    else if (numGenerics != subFunction->generics.size())
    {
        numGenerics = std::min(superFunction->generics.size(), subFunction->generics.size());

        reportError(location, TypeMismatch{superTy, subTy, "different number of generic type parameters", mismatchContext()});
    }

    if (numGenericPacks != subFunction->genericPacks.size())
    {
        numGenericPacks = std::min(superFunction->genericPacks.size(), subFunction->genericPacks.size());

        reportError(location, TypeMismatch{superTy, subTy, "different number of generic type pack parameters", mismatchContext()});
    }

    for (size_t i = 0; i < numGenerics; i++)
    {
        log.pushSeen(superFunction->generics[i], subFunction->generics[i]);
    }

    for (size_t i = 0; i < numGenericPacks; i++)
    {
        log.pushSeen(superFunction->genericPacks[i], subFunction->genericPacks[i]);
    }

    CountMismatch::Context context = ctx;

    if (!isFunctionCall)
    {
        std::unique_ptr<Unifier> innerState = makeChildUnifier();

        innerState->ctx = CountMismatch::Arg;
        innerState->tryUnify_(superFunction->argTypes, subFunction->argTypes, isFunctionCall);

        bool reported = !innerState->errors.empty();

        if (auto e = hasUnificationTooComplex(innerState->errors))
            reportError(*e);
        else if (!innerState->errors.empty() && innerState->firstPackErrorPos)
            reportError(
                location,
                TypeMismatch{
                    superTy,
                    subTy,
                    format("Argument #%d type is not compatible.", *innerState->firstPackErrorPos),
                    innerState->errors.front(),
                    mismatchContext()
                }
            );
        else if (!innerState->errors.empty())
            reportError(location, TypeMismatch{superTy, subTy, "", innerState->errors.front(), mismatchContext()});

        innerState->ctx = CountMismatch::FunctionResult;
        innerState->tryUnify_(subFunction->retTypes, superFunction->retTypes);

        if (!reported)
        {
            if (auto e = hasUnificationTooComplex(innerState->errors))
                reportError(*e);
            else if (!innerState->errors.empty() && size(superFunction->retTypes) == 1 && finite(superFunction->retTypes))
                reportError(location, TypeMismatch{superTy, subTy, "Return type is not compatible.", innerState->errors.front(), mismatchContext()});
            else if (!innerState->errors.empty() && innerState->firstPackErrorPos)
                reportError(
                    location,
                    TypeMismatch{
                        superTy,
                        subTy,
                        format("Return #%d type is not compatible.", *innerState->firstPackErrorPos),
                        innerState->errors.front(),
                        mismatchContext()
                    }
                );
            else if (!innerState->errors.empty())
                reportError(location, TypeMismatch{superTy, subTy, "", innerState->errors.front(), mismatchContext()});
        }

        log.concat(std::move(innerState->log));
    }
    else
    {
        ctx = CountMismatch::Arg;
        tryUnify_(superFunction->argTypes, subFunction->argTypes, isFunctionCall);

        ctx = CountMismatch::FunctionResult;
        tryUnify_(subFunction->retTypes, superFunction->retTypes);
    }

    // Updating the log may have invalidated the function pointers
    superFunction = log.getMutable<FunctionType>(superTy);
    subFunction = log.getMutable<FunctionType>(subTy);

    ctx = context;

    for (int i = int(numGenericPacks) - 1; 0 <= i; i--)
    {
        log.popSeen(superFunction->genericPacks[i], subFunction->genericPacks[i]);
    }

    for (int i = int(numGenerics) - 1; 0 <= i; i--)
    {
        log.popSeen(superFunction->generics[i], subFunction->generics[i]);
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

void Unifier::tryUnifyTables(TypeId subTy, TypeId superTy, bool isIntersection, const LiteralProperties* literalProperties)
{
    if (isPrim(log.follow(subTy), PrimitiveType::Table))
        subTy = builtinTypes->emptyTableType;

    if (isPrim(log.follow(superTy), PrimitiveType::Table))
        superTy = builtinTypes->emptyTableType;

    TypeId activeSubTy = subTy;
    TableType* superTable = log.getMutable<TableType>(superTy);
    TableType* subTable = log.getMutable<TableType>(subTy);

    if (!superTable || !subTable)
        ice("passed non-table types to unifyTables");

    std::vector<std::string> missingProperties;
    std::vector<std::string> extraProperties;

    if (FFlag::LuauInstantiateInSubtyping)
    {
        if (variance == Covariant && subTable->state == TableState::Generic && superTable->state != TableState::Generic)
        {
            Instantiation instantiation{&log, types, builtinTypes, subTable->level, scope};

            std::optional<TypeId> instantiated = instantiation.substitute(subTy);
            if (instantiated.has_value())
            {
                activeSubTy = *instantiated;
                subTable = log.getMutable<TableType>(activeSubTy);

                if (!subTable)
                    ice("instantiation made a table type into a non-table type in tryUnifyTables");
            }
            else
            {
                reportError(location, UnificationTooComplex{});
            }
        }
    }

    // Optimization: First test that the property sets are compatible without doing any recursive unification
    if (!subTable->indexer && subTable->state != TableState::Free)
    {
        for (const auto& [propName, superProp] : superTable->props)
        {
            auto subIter = subTable->props.find(propName);

            if (subIter == subTable->props.end() && subTable->state == TableState::Unsealed && !isOptional(superProp.type_DEPRECATED()))
                missingProperties.push_back(propName);
        }

        if (!missingProperties.empty())
        {
            reportError(location, MissingProperties{superTy, subTy, std::move(missingProperties)});
            return;
        }
    }

    // And vice versa if we're invariant
    if (variance == Invariant && !superTable->indexer && superTable->state != TableState::Unsealed && superTable->state != TableState::Free)
    {
        for (const auto& [propName, subProp] : subTable->props)
        {
            auto superIter = superTable->props.find(propName);

            if (superIter == superTable->props.end())
                extraProperties.push_back(propName);
        }

        if (!extraProperties.empty())
        {
            reportError(location, MissingProperties{superTy, subTy, std::move(extraProperties), MissingProperties::Extra});
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
            if (!literalProperties || !literalProperties->contains(name))
                variance = Invariant;

            std::unique_ptr<Unifier> innerState = makeChildUnifier();
            innerState->tryUnify_(r->second.type_DEPRECATED(), prop.type_DEPRECATED());

            checkChildUnifierTypeMismatch(innerState->errors, name, superTy, subTy);

            if (innerState->errors.empty())
                log.concat(std::move(innerState->log));
            failure |= innerState->failure;
        }
        else if (subTable->indexer && maybeString(subTable->indexer->indexType))
        {
            // TODO: read-only indexers don't need invariance
            // TODO: really we should only allow this if prop.type is optional.
            Resetter resetter{&variance};
            if (!literalProperties || !literalProperties->contains(name))
                variance = Invariant;

            std::unique_ptr<Unifier> innerState = makeChildUnifier();
            innerState->tryUnify_(subTable->indexer->indexResultType, prop.type_DEPRECATED());

            checkChildUnifierTypeMismatch(innerState->errors, name, superTy, subTy);

            if (innerState->errors.empty())
                log.concat(std::move(innerState->log));
            failure |= innerState->failure;
        }
        else if (subTable->state == TableState::Unsealed && isOptional(prop.type_DEPRECATED()))
        // This is sound because unsealed table types are precise, so `{ p : T } <: { p : T, q : U? }`
        // since if `t : { p : T }` then we are guaranteed that `t.q` is `nil`.
        // TODO: if the supertype is written to, the subtype may no longer be precise (alias analysis?)
        {
        }
        else if (subTable->state == TableState::Free)
        {
            PendingType* pendingSub = log.queue(activeSubTy);
            TableType* ttv = getMutable<TableType>(pendingSub);
            LUAU_ASSERT(ttv);
            ttv->props[name] = prop;
            subTable = ttv;
        }
        else
            missingProperties.push_back(name);

        // Recursive unification can change the txn log, and invalidate the old
        // table. If we detect that this has happened, we start over, with the updated
        // txn log.
        TypeId superTyNew = log.follow(superTy);
        TypeId subTyNew = log.follow(activeSubTy);

        // If one of the types stopped being a table altogether, we need to restart from the top
        if ((superTy != superTyNew || activeSubTy != subTyNew) && errors.empty())
        {
            if (FFlag::LuauUnifierRecursionOnRestart)
            {
                RecursionLimiter _ra("Unifier::tryUnifyTables", &sharedState.counters.recursionCount, sharedState.counters.recursionLimit);
                tryUnify(subTy, superTy, false, isIntersection);
                return;
            }
            else
            {
                return tryUnify(subTy, superTy, false, isIntersection);
            }
        }

        // Otherwise, restart only the table unification
        TableType* newSuperTable = log.getMutable<TableType>(superTyNew);
        TableType* newSubTable = log.getMutable<TableType>(subTyNew);

        if (superTable != newSuperTable || subTable != newSubTable)
        {
            if (errors.empty())
            {
                RecursionLimiter _ra("Unifier::tryUnifyTables", &sharedState.counters.recursionCount, sharedState.counters.recursionLimit);
                tryUnifyTables(subTy, superTy, isIntersection);
            }

            return;
        }
    }

    for (const auto& [name, prop] : subTable->props)
    {
        if (superTable->props.count(name))
        {
            // If both lt and rt contain the property, then
            // we're done since we already unified them above
        }
        else if (superTable->indexer && maybeString(superTable->indexer->indexType))
        {
            // TODO: read-only indexers don't need invariance
            // TODO: really we should only allow this if prop.type is optional.
            Resetter resetter{&variance};
            if (!literalProperties || !literalProperties->contains(name))
                variance = Invariant;

            std::unique_ptr<Unifier> innerState = makeChildUnifier();
            if (FFlag::LuauFixIndexerSubtypingOrdering)
                innerState->tryUnify_(prop.type_DEPRECATED(), superTable->indexer->indexResultType);
            else
            {
                // Incredibly, the old solver depends on this bug somehow.
                innerState->tryUnify_(superTable->indexer->indexResultType, prop.type_DEPRECATED());
            }

            checkChildUnifierTypeMismatch(innerState->errors, name, superTy, subTy);

            if (innerState->errors.empty())
                log.concat(std::move(innerState->log));
            failure |= innerState->failure;
        }
        else if (superTable->state == TableState::Unsealed)
        {
            // TODO: this case is unsound when variance is Invariant, but without it lua-apps fails to typecheck.
            // TODO: file a JIRA
            // TODO: hopefully readonly/writeonly properties will fix this.
            Property clone = prop;
            clone.setType(deeplyOptional(clone.type_DEPRECATED()));

            PendingType* pendingSuper = log.queue(superTy);
            TableType* pendingSuperTtv = getMutable<TableType>(pendingSuper);
            pendingSuperTtv->props[name] = clone;
            superTable = pendingSuperTtv;
        }
        else if (variance == Covariant)
        {
        }
        else if (superTable->state == TableState::Free)
        {
            PendingType* pendingSuper = log.queue(superTy);
            TableType* pendingSuperTtv = getMutable<TableType>(pendingSuper);
            pendingSuperTtv->props[name] = prop;
            superTable = pendingSuperTtv;
        }
        else
            extraProperties.push_back(name);

        TypeId superTyNew = log.follow(superTy);
        TypeId subTyNew = log.follow(activeSubTy);

        // If one of the types stopped being a table altogether, we need to restart from the top
        if ((superTy != superTyNew || activeSubTy != subTyNew) && errors.empty())
        {
            if (FFlag::LuauUnifierRecursionOnRestart)
            {
                RecursionLimiter _ra("Unifier::tryUnifyTables", &sharedState.counters.recursionCount, sharedState.counters.recursionLimit);
                tryUnify(subTy, superTy, false, isIntersection);
                return;
            }
            else
            {
                return tryUnify(subTy, superTy, false, isIntersection);
            }
        }

        // Recursive unification can change the txn log, and invalidate the old
        // table. If we detect that this has happened, we start over, with the updated
        // txn log.
        TableType* newSuperTable = log.getMutable<TableType>(superTyNew);
        TableType* newSubTable = log.getMutable<TableType>(subTyNew);

        if (superTable != newSuperTable || subTable != newSubTable)
        {
            if (errors.empty())
            {
                RecursionLimiter _ra("Unifier::tryUnifyTables", &sharedState.counters.recursionCount, sharedState.counters.recursionLimit);
                tryUnifyTables(subTy, superTy, isIntersection);
            }

            return;
        }
    }

    // Unify indexers
    if (superTable->indexer && subTable->indexer)
    {
        // TODO: read-only indexers don't need invariance
        Resetter resetter{&variance};
        variance = Invariant;

        std::unique_ptr<Unifier> innerState = makeChildUnifier();

        innerState->tryUnify_(subTable->indexer->indexType, superTable->indexer->indexType);

        bool reported = !innerState->errors.empty();

        checkChildUnifierTypeMismatch(innerState->errors, "[indexer key]", superTy, subTy);

        innerState->tryUnify_(subTable->indexer->indexResultType, superTable->indexer->indexResultType);

        if (!reported)
            checkChildUnifierTypeMismatch(innerState->errors, "[indexer value]", superTy, subTy);

        if (innerState->errors.empty())
            log.concat(std::move(innerState->log));
        failure |= innerState->failure;
    }
    else if (superTable->indexer)
    {
        if (subTable->state == TableState::Unsealed || subTable->state == TableState::Free)
        {
            // passing/assigning a table without an indexer to something that has one
            // e.g. table.insert(t, 1) where t is a non-sealed table and doesn't have an indexer.
            // TODO: we only need to do this if the supertype's indexer is read/write
            // since that can add indexed elements.
            log.changeIndexer(subTy, superTable->indexer);
        }
    }
    else if (subTable->indexer && variance == Invariant)
    {
        // Symmetric if we are invariant
        if (superTable->state == TableState::Unsealed || superTable->state == TableState::Free)
        {
            log.changeIndexer(superTy, subTable->indexer);
        }
    }

    // Changing the indexer can invalidate the table pointers.
    superTable = log.getMutable<TableType>(log.follow(superTy));
    subTable = log.getMutable<TableType>(log.follow(activeSubTy));

    if (!superTable || !subTable)
        return;

    if (!missingProperties.empty())
    {
        reportError(location, MissingProperties{superTy, subTy, std::move(missingProperties)});
        return;
    }

    if (!extraProperties.empty())
    {
        reportError(location, MissingProperties{superTy, subTy, std::move(extraProperties), MissingProperties::Extra});
        return;
    }

    /*
     * Types are commonly cyclic, so it is entirely possible
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
        log.bindTable(superTy, subTy);
    }
    else if (subTable->state == TableState::Free)
    {
        log.bindTable(subTy, superTy);
    }
}

void Unifier::tryUnifyScalarShape(TypeId subTy, TypeId superTy, bool reversed)
{
    TypeId osubTy = subTy;
    TypeId osuperTy = superTy;

    // If the normalizer hits resource limits, we can't show it's uninhabited, so, we should continue.
    if (checkInhabited && normalizer->isInhabited(subTy) == NormalizationResult::False)
        return;

    if (reversed)
        std::swap(subTy, superTy);

    TableType* superTable = log.getMutable<TableType>(superTy);

    if (!superTable || superTable->state != TableState::Free)
        return reportError(location, TypeMismatch{osuperTy, osubTy, mismatchContext()});

    auto fail = [&](std::optional<TypeError> e)
    {
        std::string reason = "The former's metatable does not satisfy the requirements.";
        if (e)
            reportError(location, TypeMismatch{osuperTy, osubTy, std::move(reason), std::move(e), mismatchContext()});
        else
            reportError(location, TypeMismatch{osuperTy, osubTy, std::move(reason), mismatchContext()});
    };

    // Given t1 where t1 = { lower: (t1) -> (a, b...) }
    // It should be the case that `string <: t1` iff `(subtype's metatable).__index <: t1`
    if (auto metatable = getMetatable(subTy, builtinTypes))
    {
        auto mttv = log.get<TableType>(*metatable);
        if (!mttv)
            fail(std::nullopt);

        if (auto it = mttv->props.find("__index"); it != mttv->props.end())
        {
            TypeId ty = it->second.type_DEPRECATED();
            std::unique_ptr<Unifier> child = makeChildUnifier();
            child->tryUnify_(ty, superTy);

            // To perform subtype <: free table unification, we have tried to unify (subtype's metatable) <: free table
            // There is a chance that it was unified with the origial subtype, but then, (subtype's metatable) <: subtype could've failed
            // Here we check if we have a new supertype instead of the original free table and try original subtype <: new supertype check
            TypeId newSuperTy = child->log.follow(superTy);

            if (superTy != newSuperTy && canUnify(subTy, newSuperTy).empty())
            {
                log.replace(superTy, BoundType{subTy});
                return;
            }

            if (auto e = hasUnificationTooComplex(child->errors))
                reportError(*e);
            else if (!child->errors.empty())
                fail(child->errors.front());

            log.concat(std::move(child->log));

            // To perform subtype <: free table unification, we have tried to unify (subtype's metatable) <: free table
            // We return success because subtype <: free table which means that correct unification is to replace free table with the subtype
            if (child->errors.empty())
                log.replace(superTy, BoundType{subTy});

            return;
        }
        else
        {
            return fail(std::nullopt);
        }
    }

    reportError(location, TypeMismatch{osuperTy, osubTy, mismatchContext()});
    return;
}

TypeId Unifier::deeplyOptional(TypeId ty, std::unordered_map<TypeId, TypeId> seen)
{
    ty = follow(ty);
    if (isOptional(ty))
        return ty;
    else if (const TableType* ttv = get<TableType>(ty))
    {
        TypeId& result = seen[ty];
        if (result)
            return result;
        result = types->addType(*ttv);
        TableType* resultTtv = getMutable<TableType>(result);
        for (auto& [name, prop] : resultTtv->props)
            prop.setType(deeplyOptional(prop.type_DEPRECATED(), seen));
        return types->addType(UnionType{{builtinTypes->nilType, result}});
    }
    else
        return types->addType(UnionType{{builtinTypes->nilType, ty}});
}

void Unifier::tryUnifyWithMetatable(TypeId subTy, TypeId superTy, bool reversed)
{
    const MetatableType* superMetatable = get<MetatableType>(superTy);
    if (!superMetatable)
        ice("tryUnifyMetatable invoked with non-metatable Type");

    TypeError mismatchError = TypeError{location, TypeMismatch{reversed ? subTy : superTy, reversed ? superTy : subTy, mismatchContext()}};

    if (const MetatableType* subMetatable = log.getMutable<MetatableType>(subTy))
    {
        std::unique_ptr<Unifier> innerState = makeChildUnifier();
        innerState->tryUnify_(subMetatable->table, superMetatable->table);
        innerState->tryUnify_(subMetatable->metatable, superMetatable->metatable);

        if (auto e = hasUnificationTooComplex(innerState->errors))
            reportError(*e);
        else if (!innerState->errors.empty())
            reportError(
                location, TypeMismatch{reversed ? subTy : superTy, reversed ? superTy : subTy, "", innerState->errors.front(), mismatchContext()}
            );

        log.concat(std::move(innerState->log));
        failure |= innerState->failure;
    }
    else if (TableType* subTable = log.getMutable<TableType>(subTy))
    {
        switch (subTable->state)
        {
        case TableState::Free:
        {
            tryUnify_(subTy, superMetatable->table);
            log.bindTable(subTy, superTy);

            break;
        }
        // We know the shape of sealed, unsealed, and generic tables; you can't add a metatable on to any of these.
        case TableState::Sealed:
        case TableState::Unsealed:
        case TableState::Generic:
            reportError(std::move(mismatchError));
        }
    }
    else if (log.getMutable<AnyType>(subTy) || log.getMutable<ErrorType>(subTy))
    {
    }
    else
    {
        reportError(std::move(mismatchError));
    }
}

// Extern type unification is almost, but not quite symmetrical.  We use the 'reversed' boolean to indicate which scenario we are evaluating.
void Unifier::tryUnifyWithExternType(TypeId subTy, TypeId superTy, bool reversed)
{
    if (reversed)
        std::swap(superTy, subTy);

    auto fail = [&]()
    {
        if (!reversed)
            reportError(location, TypeMismatch{superTy, subTy, mismatchContext()});
        else
            reportError(location, TypeMismatch{subTy, superTy, mismatchContext()});
    };

    const ExternType* superExternType = get<ExternType>(superTy);
    if (!superExternType)
        ice("tryUnifyExternType invoked with non-class Type");

    if (const ExternType* subExternType = get<ExternType>(subTy))
    {
        switch (variance)
        {
        case Covariant:
            if (!isSubclass(subExternType, superExternType))
                return fail();
            return;
        case Invariant:
            if (subExternType != superExternType)
                return fail();
            return;
        }
        ice("Illegal variance setting!");
    }
    else if (TableType* subTable = getMutable<TableType>(subTy))
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
            const Property* classProp = lookupExternTypeProp(superExternType, propName);
            if (!classProp)
            {
                ok = false;
                reportError(location, UnknownProperty{superTy, propName});
            }
            else
            {
                std::unique_ptr<Unifier> innerState = makeChildUnifier();
                innerState->tryUnify_(classProp->type_DEPRECATED(), prop.type_DEPRECATED());

                checkChildUnifierTypeMismatch(innerState->errors, propName, reversed ? subTy : superTy, reversed ? superTy : subTy);

                if (innerState->errors.empty())
                {
                    log.concat(std::move(innerState->log));
                    failure |= innerState->failure;
                }
                else
                {
                    ok = false;
                }
            }
        }

        if (subTable->indexer)
        {
            ok = false;
            std::string msg = "Extern type " + superExternType->name + " does not have an indexer";
            reportError(location, GenericError{std::move(msg)});
        }

        if (!ok)
            return;

        log.bindTable(subTy, superTy);
    }
    else
        return fail();
}

void Unifier::tryUnifyNegations(TypeId subTy, TypeId superTy)
{
    if (!log.get<NegationType>(subTy) && !log.get<NegationType>(superTy))
        ice("tryUnifyNegations superTy or subTy must be a negation type");

    std::shared_ptr<const NormalizedType> subNorm = normalizer->normalize(subTy);
    std::shared_ptr<const NormalizedType> superNorm = normalizer->normalize(superTy);
    if (!subNorm || !superNorm)
        return reportError(location, NormalizationTooComplex{});

    // T </: ~U iff T <: U
    std::unique_ptr<Unifier> state = makeChildUnifier();
    state->tryUnifyNormalizedTypes(subTy, superTy, *subNorm, *superNorm, "");
    if (state->errors.empty())
        reportError(location, TypeMismatch{superTy, subTy, mismatchContext()});
}

static void queueTypePack(std::vector<TypeId>& queue, DenseHashSet<TypePackId>& seenTypePacks, Unifier& state, TypePackId a, TypePackId anyTypePack)
{
    while (true)
    {
        a = state.log.follow(a);

        if (seenTypePacks.find(a))
            break;
        seenTypePacks.insert(a);

        if (state.log.getMutable<FreeTypePack>(a))
        {
            state.log.replace(a, BoundTypePack{anyTypePack});
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
}

void Unifier::tryUnifyVariadics(TypePackId subTp, TypePackId superTp, bool reversed, int subOffset)
{
    const VariadicTypePack* superVariadic = log.getMutable<VariadicTypePack>(superTp);
    const TypeId variadicTy = follow(superVariadic->ty);

    if (!superVariadic)
        ice("passed non-variadic pack to tryUnifyVariadics");

    if (const VariadicTypePack* subVariadic = log.get<VariadicTypePack>(subTp))
    {
        tryUnify_(reversed ? variadicTy : subVariadic->ty, reversed ? subVariadic->ty : variadicTy);
    }
    else if (log.get<TypePack>(subTp))
    {
        TypePackIterator subIter = begin(subTp, &log);
        TypePackIterator subEnd = end(subTp);

        std::advance(subIter, subOffset);

        while (subIter != subEnd)
        {
            tryUnify_(reversed ? variadicTy : *subIter, reversed ? *subIter : variadicTy);
            ++subIter;
        }

        if (std::optional<TypePackId> maybeTail = subIter.tail())
        {
            TypePackId tail = follow(*maybeTail);

            if (isBlocked(log, tail))
            {
                blockedTypePacks.push_back(tail);
            }
            else if (get<FreeTypePack>(tail))
            {
                log.replace(tail, BoundTypePack(superTp));
            }
            else if (const VariadicTypePack* vtp = get<VariadicTypePack>(tail))
            {
                tryUnify_(vtp->ty, variadicTy);
            }
            else if (get<GenericTypePack>(tail))
                reportError(location, GenericError{"Cannot unify variadic and generic packs"});
            else if (get<ErrorTypePack>(tail))
            {
                // Nothing to do here.
            }
            else
            {
                ice("Unknown TypePack kind");
            }
        }
    }
    else if (get<AnyType>(variadicTy) && log.get<GenericTypePack>(subTp))
    {
        // Nothing to do.  This is ok.
    }
    else
    {
        reportError(location, GenericError{"Failed to unify variadic packs"});
    }
}

static void tryUnifyWithAny(
    std::vector<TypeId>& queue,
    Unifier& state,
    DenseHashSet<TypeId>& seen,
    DenseHashSet<TypePackId>& seenTypePacks,
    const TypeArena* typeArena,
    TypeId anyType,
    TypePackId anyTypePack
)
{
    while (!queue.empty())
    {
        TypeId ty = state.log.follow(queue.back());
        queue.pop_back();

        // Types from other modules don't have free types
        if (ty->owningArena != typeArena)
            continue;

        if (seen.find(ty))
            continue;

        seen.insert(ty);

        if (state.log.getMutable<FreeType>(ty))
        {
            // TODO: Only bind if the anyType isn't any, unknown, or error (?)
            state.log.replace(ty, BoundType{anyType});
        }
        else if (auto fun = state.log.getMutable<FunctionType>(ty))
        {
            queueTypePack(queue, seenTypePacks, state, fun->argTypes, anyTypePack);
            queueTypePack(queue, seenTypePacks, state, fun->retTypes, anyTypePack);
        }
        else if (auto table = state.log.getMutable<TableType>(ty))
        {
            for (const auto& [_name, prop] : table->props)
                queue.push_back(prop.type_DEPRECATED());

            if (table->indexer)
            {
                queue.push_back(table->indexer->indexType);
                queue.push_back(table->indexer->indexResultType);
            }
        }
        else if (auto mt = state.log.getMutable<MetatableType>(ty))
        {
            queue.push_back(mt->table);
            queue.push_back(mt->metatable);
        }
        else if (state.log.getMutable<ExternType>(ty))
        {
            // ExternTypes never contain free types.
        }
        else if (auto union_ = state.log.getMutable<UnionType>(ty))
            queue.insert(queue.end(), union_->options.begin(), union_->options.end());
        else if (auto intersection = state.log.getMutable<IntersectionType>(ty))
            queue.insert(queue.end(), intersection->parts.begin(), intersection->parts.end());
        else
        {
        } // Primitives, any, errors, and generics are left untouched.
    }
}

void Unifier::tryUnifyWithAny(TypeId subTy, TypeId anyTy)
{
    LUAU_ASSERT(get<AnyType>(anyTy) || get<ErrorType>(anyTy) || get<UnknownType>(anyTy) || get<NeverType>(anyTy));

    // These types are not visited in general loop below
    if (log.get<PrimitiveType>(subTy) || log.get<AnyType>(subTy) || log.get<ExternType>(subTy))
        return;

    TypePackId anyTp = types->addTypePack(TypePackVar{VariadicTypePack{anyTy}});

    std::vector<TypeId> queue = {subTy};

    sharedState.tempSeenTy.clear();
    sharedState.tempSeenTp.clear();

    Luau::tryUnifyWithAny(queue, *this, sharedState.tempSeenTy, sharedState.tempSeenTp, types, anyTy, anyTp);
}

void Unifier::tryUnifyWithAny(TypePackId subTy, TypePackId anyTp)
{
    LUAU_ASSERT(get<ErrorTypePack>(anyTp));

    const TypeId anyTy = builtinTypes->errorType;

    std::vector<TypeId> queue;

    sharedState.tempSeenTy.clear();
    sharedState.tempSeenTp.clear();

    queueTypePack(queue, sharedState.tempSeenTp, *this, subTy, anyTp);

    Luau::tryUnifyWithAny(queue, *this, sharedState.tempSeenTy, sharedState.tempSeenTp, types, anyTy, anyTp);
}

std::optional<TypeId> Unifier::findTablePropertyRespectingMeta(TypeId lhsType, Name name)
{
    return Luau::findTablePropertyRespectingMeta(builtinTypes, errors, lhsType, name, location);
}

TxnLog Unifier::combineLogsIntoUnion(std::vector<TxnLog> logs)
{
    TxnLog result;
    for (TxnLog& log : logs)
        result.concatAsUnion(std::move(log), NotNull{types});
    return result;
}

bool Unifier::occursCheck(TypeId needle, TypeId haystack, bool reversed)
{
    sharedState.tempSeenTy.clear();

    bool occurs = occursCheck(sharedState.tempSeenTy, needle, haystack);

    if (occurs)
    {
        std::unique_ptr<Unifier> innerState = makeChildUnifier();
        if (const UnionType* ut = get<UnionType>(haystack))
        {
            if (reversed)
                innerState->tryUnifyUnionWithType(haystack, ut, needle);
            else
                innerState->tryUnifyTypeWithUnion(needle, haystack, ut, /* cacheEnabled = */ false, /* isFunction = */ false);
        }
        else if (const IntersectionType* it = get<IntersectionType>(haystack))
        {
            if (reversed)
                innerState->tryUnifyIntersectionWithType(haystack, it, needle, /* cacheEnabled = */ false, /* isFunction = */ false);
            else
                innerState->tryUnifyTypeWithIntersection(needle, haystack, it);
        }
        else
        {
            innerState->failure = true;
        }

        if (innerState->failure)
        {
            reportError(location, OccursCheckFailed{});
            log.replace(needle, BoundType{builtinTypes->errorType});
        }
    }

    return occurs;
}

bool Unifier::occursCheck(DenseHashSet<TypeId>& seen, TypeId needle, TypeId haystack)
{
    RecursionLimiter _ra("Unifier::occursCheck", &sharedState.counters.recursionCount, sharedState.counters.recursionLimit);

    bool occurrence = false;

    auto check = [&](TypeId tv)
    {
        if (occursCheck(seen, needle, tv))
            occurrence = true;
    };

    needle = log.follow(needle);
    haystack = log.follow(haystack);

    if (seen.find(haystack))
        return false;

    seen.insert(haystack);

    if (log.getMutable<ErrorType>(needle))
        return false;

    if (!log.getMutable<FreeType>(needle))
        ice("Expected needle to be free");

    if (needle == haystack)
        return true;

    if (log.getMutable<FreeType>(haystack))
        return false;
    else if (auto a = log.getMutable<UnionType>(haystack))
    {
        for (TypeId ty : a->options)
            check(ty);
    }
    else if (auto a = log.getMutable<IntersectionType>(haystack))
    {
        for (TypeId ty : a->parts)
            check(ty);
    }

    return occurrence;
}

bool Unifier::occursCheck(TypePackId needle, TypePackId haystack, bool reversed)
{
    sharedState.tempSeenTp.clear();

    bool occurs = occursCheck(sharedState.tempSeenTp, needle, haystack);

    if (occurs)
    {
        reportError(location, OccursCheckFailed{});
        log.replace(needle, BoundTypePack{builtinTypes->errorTypePack});
    }

    return occurs;
}

bool Unifier::occursCheck(DenseHashSet<TypePackId>& seen, TypePackId needle, TypePackId haystack)
{
    needle = log.follow(needle);
    haystack = log.follow(haystack);

    if (seen.find(haystack))
        return false;

    seen.insert(haystack);

    if (log.getMutable<ErrorTypePack>(needle))
        return false;

    if (!log.getMutable<FreeTypePack>(needle))
        ice("Expected needle pack to be free");

    RecursionLimiter _ra("Unifier::occursCheck", &sharedState.counters.recursionCount, sharedState.counters.recursionLimit);

    while (!log.getMutable<ErrorTypePack>(haystack))
    {
        if (needle == haystack)
            return true;

        if (auto a = get<TypePack>(haystack); a && a->tail)
        {
            haystack = log.follow(*a->tail);
            continue;
        }

        break;
    }

    return false;
}

std::unique_ptr<Unifier> Unifier::makeChildUnifier()
{
    std::unique_ptr<Unifier> u = std::make_unique<Unifier>(normalizer, scope, location, variance, &log);
    u->normalize = normalize;
    u->checkInhabited = checkInhabited;

    return u;
}

// A utility function that appends the given error to the unifier's error log.
// This allows setting a breakpoint wherever the unifier reports an error.
//
// Note: report error accepts its arguments by value intentionally to reduce the stack usage of functions which call `reportError`.
void Unifier::reportError(Location location, TypeErrorData data)
{
    errors.emplace_back(std::move(location), std::move(data));
    failure = true;
}

// A utility function that appends the given error to the unifier's error log.
// This allows setting a breakpoint wherever the unifier reports an error.
//
// Note: to conserve stack space in calling functions it is generally preferred to call `Unifier::reportError(Location location, TypeErrorData data)`
// instead of this method.
void Unifier::reportError(TypeError err)
{
    errors.push_back(std::move(err));
    failure = true;
}

void Unifier::checkChildUnifierTypeMismatch(const ErrorVec& innerErrors, TypeId wantedType, TypeId givenType)
{
    if (auto e = hasUnificationTooComplex(innerErrors))
        reportError(*e);
    else if (!innerErrors.empty())
        reportError(location, TypeMismatch{wantedType, givenType, mismatchContext()});
}

void Unifier::checkChildUnifierTypeMismatch(const ErrorVec& innerErrors, const std::string& prop, TypeId wantedType, TypeId givenType)
{
    if (auto e = hasUnificationTooComplex(innerErrors))
        reportError(*e);
    else if (!innerErrors.empty())
        reportError(
            TypeError{
                location,
                TypeMismatch{wantedType, givenType, format("Property '%s' is not compatible.", prop.c_str()), innerErrors.front(), mismatchContext()}
            }
        );
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
