// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Unifier.h"

#include "Luau/Common.h"
#include "Luau/Instantiation.h"
#include "Luau/RecursionCounter.h"
#include "Luau/Scope.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/TimeTrace.h"
#include "Luau/VisitTypeVar.h"
#include "Luau/ToString.h"

#include <algorithm>

LUAU_FASTINT(LuauTypeInferRecursionLimit);
LUAU_FASTINT(LuauTypeInferTypePackLoopLimit);
LUAU_FASTINT(LuauTypeInferIterationLimit);
LUAU_FASTFLAG(LuauAutocompleteDynamicLimits)
LUAU_FASTINTVARIABLE(LuauTypeInferLowerBoundsIterationLimit, 2000);
LUAU_FASTFLAG(LuauErrorRecoveryType);
LUAU_FASTFLAG(LuauUnknownAndNeverType)
LUAU_FASTFLAGVARIABLE(LuauSubtypeNormalizer, false);
LUAU_FASTFLAGVARIABLE(LuauScalarShapeSubtyping, false)
LUAU_FASTFLAGVARIABLE(LuauInstantiateInSubtyping, false)
LUAU_FASTFLAG(LuauClassTypeVarsInSubstitution)
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

namespace Luau
{

struct PromoteTypeLevels final : TypeVarOnceVisitor
{
    TxnLog& log;
    const TypeArena* typeArena = nullptr;
    TypeLevel minLevel;

    Scope* outerScope = nullptr;
    bool useScopes;

    PromoteTypeLevels(TxnLog& log, const TypeArena* typeArena, TypeLevel minLevel, Scope* outerScope, bool useScopes)
        : log(log)
        , typeArena(typeArena)
        , minLevel(minLevel)
        , outerScope(outerScope)
        , useScopes(useScopes)
    {
    }

    template<typename TID, typename T>
    void promote(TID ty, T* t)
    {
        LUAU_ASSERT(t);

        if (useScopes)
        {
            if (subsumesStrict(outerScope, t->scope))
                log.changeScope(ty, NotNull{outerScope});
        }
        else
        {
            if (minLevel.subsumesStrict(t->level))
            {
                log.changeLevel(ty, minLevel);
            }
        }
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

    bool visit(TypeId ty, const FreeTypeVar&) override
    {
        // Surprise, it's actually a BoundTypeVar that hasn't been committed yet.
        // Calling getMutable on this will trigger an assertion.
        if (!log.is<FreeTypeVar>(ty))
            return true;

        promote(ty, log.getMutable<FreeTypeVar>(ty));
        return true;
    }

    bool visit(TypeId ty, const ConstrainedTypeVar&) override
    {
        if (!FFlag::LuauUnknownAndNeverType)
            return visit(ty);

        promote(ty, log.getMutable<ConstrainedTypeVar>(ty));
        return true;
    }

    bool visit(TypeId ty, const FunctionTypeVar&) override
    {
        // Type levels of types from other modules are already global, so we don't need to promote anything inside
        if (ty->owningArena != typeArena)
            return false;

        promote(ty, log.getMutable<FunctionTypeVar>(ty));
        return true;
    }

    bool visit(TypeId ty, const TableTypeVar& ttv) override
    {
        // Type levels of types from other modules are already global, so we don't need to promote anything inside
        if (ty->owningArena != typeArena)
            return false;

        if (ttv.state != TableState::Free && ttv.state != TableState::Generic)
            return true;

        promote(ty, log.getMutable<TableTypeVar>(ty));
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

static void promoteTypeLevels(TxnLog& log, const TypeArena* typeArena, TypeLevel minLevel, Scope* outerScope, bool useScopes, TypeId ty)
{
    // Type levels of types from other modules are already global, so we don't need to promote anything inside
    if (ty->owningArena != typeArena)
        return;

    PromoteTypeLevels ptl{log, typeArena, minLevel, outerScope, useScopes};
    ptl.traverse(ty);
}

void promoteTypeLevels(TxnLog& log, const TypeArena* typeArena, TypeLevel minLevel, Scope* outerScope, bool useScopes, TypePackId tp)
{
    // Type levels of types from other modules are already global, so we don't need to promote anything inside
    if (tp->owningArena != typeArena)
        return;

    PromoteTypeLevels ptl{log, typeArena, minLevel, outerScope, useScopes};
    ptl.traverse(tp);
}

struct SkipCacheForType final : TypeVarOnceVisitor
{
    SkipCacheForType(const DenseHashMap<TypeId, bool>& skipCacheForType, const TypeArena* typeArena)
        : skipCacheForType(skipCacheForType)
        , typeArena(typeArena)
    {
    }

    bool visit(TypeId, const FreeTypeVar&) override
    {
        result = true;
        return false;
    }

    bool visit(TypeId, const BoundTypeVar&) override
    {
        result = true;
        return false;
    }

    bool visit(TypeId, const GenericTypeVar&) override
    {
        result = true;
        return false;
    }

    bool visit(TypeId ty, const TableTypeVar&) override
    {
        // Types from other modules don't contain mutable elements and are ok to cache
        if (ty->owningArena != typeArena)
            return false;

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

    const DenseHashMap<TypeId, bool>& skipCacheForType;
    const TypeArena* typeArena = nullptr;
    bool result = false;
};

bool Widen::isDirty(TypeId ty)
{
    return log->is<SingletonTypeVar>(ty);
}

bool Widen::isDirty(TypePackId)
{
    return false;
}

TypeId Widen::clean(TypeId ty)
{
    LUAU_ASSERT(isDirty(ty));
    auto stv = log->getMutable<SingletonTypeVar>(ty);
    LUAU_ASSERT(stv);

    if (get<StringSingleton>(stv))
        return singletonTypes->stringType;
    else
    {
        // If this assert trips, it's likely we now have number singletons.
        LUAU_ASSERT(get<BooleanSingleton>(stv));
        return singletonTypes->booleanType;
    }
}

TypePackId Widen::clean(TypePackId)
{
    throw std::runtime_error("Widen attempted to clean a dirty type pack?");
}

bool Widen::ignoreChildren(TypeId ty)
{
    if (FFlag::LuauClassTypeVarsInSubstitution && get<ClassTypeVar>(ty))
        return true;

    return !log->is<UnionTypeVar>(ty);
}

TypeId Widen::operator()(TypeId ty)
{
    return substitute(ty).value_or(ty);
}

TypePackId Widen::operator()(TypePackId tp)
{
    return substitute(tp).value_or(tp);
}

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
    if (auto ttv = getTableType(type))
    {
        for (auto&& [name, prop] : ttv->props)
        {
            if (auto sing = get<SingletonTypeVar>(follow(prop.type)))
                return {{name, sing}};
        }
    }

    return std::nullopt;
}

// TODO: Inline and clip with FFlag::DebugLuauDeferredConstraintResolution
template<typename TY_A, typename TY_B>
static bool subsumes(bool useScopes, TY_A* left, TY_B* right)
{
    if (useScopes)
        return subsumes(left->scope, right->scope);
    else
        return left->level.subsumes(right->level);
}

Unifier::Unifier(NotNull<Normalizer> normalizer, Mode mode, NotNull<Scope> scope, const Location& location, Variance variance, TxnLog* parentLog)
    : types(normalizer->arena)
    , singletonTypes(normalizer->singletonTypes)
    , normalizer(normalizer)
    , mode(mode)
    , scope(scope)
    , log(parentLog)
    , location(location)
    , variance(variance)
    , sharedState(*normalizer->sharedState)
{
    normalize = FFlag::LuauSubtypeNormalizer;
    LUAU_ASSERT(sharedState.iceHandler);
}

void Unifier::tryUnify(TypeId subTy, TypeId superTy, bool isFunctionCall, bool isIntersection)
{
    sharedState.counters.iterationCount = 0;

    tryUnify_(subTy, superTy, isFunctionCall, isIntersection);
}

void Unifier::tryUnify_(TypeId subTy, TypeId superTy, bool isFunctionCall, bool isIntersection)
{
    RecursionLimiter _ra(&sharedState.counters.recursionCount,
        FFlag::LuauAutocompleteDynamicLimits ? sharedState.counters.recursionLimit : FInt::LuauTypeInferRecursionLimit);

    ++sharedState.counters.iterationCount;

    if (FFlag::LuauAutocompleteDynamicLimits)
    {
        if (sharedState.counters.iterationLimit > 0 && sharedState.counters.iterationLimit < sharedState.counters.iterationCount)
        {
            reportError(TypeError{location, UnificationTooComplex{}});
            return;
        }
    }
    else
    {
        if (FInt::LuauTypeInferIterationLimit > 0 && FInt::LuauTypeInferIterationLimit < sharedState.counters.iterationCount)
        {
            reportError(TypeError{location, UnificationTooComplex{}});
            return;
        }
    }

    superTy = log.follow(superTy);
    subTy = log.follow(subTy);

    if (superTy == subTy)
        return;

    if (log.get<ConstrainedTypeVar>(superTy))
        return tryUnifyWithConstrainedSuperTypeVar(subTy, superTy);

    auto superFree = log.getMutable<FreeTypeVar>(superTy);
    auto subFree = log.getMutable<FreeTypeVar>(subTy);

    if (superFree && subFree && subsumes(useScopes, superFree, subFree))
    {
        if (!occursCheck(subTy, superTy))
            log.replace(subTy, BoundTypeVar(superTy));

        return;
    }
    else if (superFree && subFree)
    {
        if (!occursCheck(superTy, subTy))
        {
            if (subsumes(useScopes, superFree, subFree))
            {
                log.changeLevel(subTy, superFree->level);
            }

            log.replace(superTy, BoundTypeVar(subTy));
        }

        return;
    }
    else if (superFree)
    {
        // Unification can't change the level of a generic.
        auto subGeneric = log.getMutable<GenericTypeVar>(subTy);
        if (subGeneric && !subsumes(useScopes, subGeneric, superFree))
        {
            // TODO: a more informative error message? CLI-39912
            reportError(TypeError{location, GenericError{"Generic subtype escaping scope"}});
            return;
        }

        if (!occursCheck(superTy, subTy))
        {
            promoteTypeLevels(log, types, superFree->level, superFree->scope, useScopes, subTy);

            Widen widen{types, singletonTypes};
            log.replace(superTy, BoundTypeVar(widen(subTy)));
        }

        return;
    }
    else if (subFree)
    {
        if (FFlag::LuauUnknownAndNeverType)
        {
            // Normally, if the subtype is free, it should not be bound to any, unknown, or error types.
            // But for bug compatibility, we'll only apply this rule to unknown. Doing this will silence cascading type errors.
            if (log.get<UnknownTypeVar>(superTy))
                return;
        }

        // Unification can't change the level of a generic.
        auto superGeneric = log.getMutable<GenericTypeVar>(superTy);
        if (superGeneric && !subsumes(useScopes, superGeneric, subFree))
        {
            // TODO: a more informative error message? CLI-39912
            reportError(TypeError{location, GenericError{"Generic supertype escaping scope"}});
            return;
        }

        if (!occursCheck(subTy, superTy))
        {
            promoteTypeLevels(log, types, subFree->level, subFree->scope, useScopes, superTy);
            log.replace(subTy, BoundTypeVar(superTy));
        }

        return;
    }

    if (get<ErrorTypeVar>(superTy) || get<AnyTypeVar>(superTy) || get<UnknownTypeVar>(superTy))
        return tryUnifyWithAny(subTy, superTy);

    if (get<AnyTypeVar>(subTy))
    {
        if (anyIsTop)
        {
            reportError(TypeError{location, TypeMismatch{superTy, subTy}});
            return;
        }
        else
            return tryUnifyWithAny(superTy, subTy);
    }

    if (log.get<ErrorTypeVar>(subTy))
        return tryUnifyWithAny(superTy, subTy);

    if (log.get<NeverTypeVar>(subTy))
        return tryUnifyWithAny(superTy, subTy);

    auto& cache = sharedState.cachedUnify;

    // What if the types are immutable and we proved their relation before
    bool cacheEnabled = !isFunctionCall && !isIntersection && variance == Invariant;

    if (cacheEnabled)
    {
        if (cache.contains({subTy, superTy}))
            return;

        if (auto error = sharedState.cachedUnifyError.find({subTy, superTy}))
        {
            reportError(TypeError{location, *error});
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

    if (log.get<ConstrainedTypeVar>(subTy))
        tryUnifyWithConstrainedSubTypeVar(subTy, superTy);
    else if (const UnionTypeVar* subUnion = log.getMutable<UnionTypeVar>(subTy))
    {
        tryUnifyUnionWithType(subTy, subUnion, superTy);
    }
    else if (const UnionTypeVar* uv = (FFlag::LuauSubtypeNormalizer ? nullptr : log.getMutable<UnionTypeVar>(superTy)))
    {
        tryUnifyTypeWithUnion(subTy, superTy, uv, cacheEnabled, isFunctionCall);
    }
    else if (const IntersectionTypeVar* uv = log.getMutable<IntersectionTypeVar>(superTy))
    {
        tryUnifyTypeWithIntersection(subTy, superTy, uv);
    }
    else if (const UnionTypeVar* uv = log.getMutable<UnionTypeVar>(superTy))
    {
        tryUnifyTypeWithUnion(subTy, superTy, uv, cacheEnabled, isFunctionCall);
    }
    else if (const IntersectionTypeVar* uv = log.getMutable<IntersectionTypeVar>(subTy))
    {
        tryUnifyIntersectionWithType(subTy, uv, superTy, cacheEnabled, isFunctionCall);
    }
    else if (log.getMutable<PrimitiveTypeVar>(superTy) && log.getMutable<PrimitiveTypeVar>(subTy))
        tryUnifyPrimitives(subTy, superTy);

    else if ((log.getMutable<PrimitiveTypeVar>(superTy) || log.getMutable<SingletonTypeVar>(superTy)) && log.getMutable<SingletonTypeVar>(subTy))
        tryUnifySingletons(subTy, superTy);

    else if (log.getMutable<FunctionTypeVar>(superTy) && log.getMutable<FunctionTypeVar>(subTy))
        tryUnifyFunctions(subTy, superTy, isFunctionCall);

    else if (log.getMutable<TableTypeVar>(superTy) && log.getMutable<TableTypeVar>(subTy))
    {
        tryUnifyTables(subTy, superTy, isIntersection);
    }
    else if (FFlag::LuauScalarShapeSubtyping && log.get<TableTypeVar>(superTy) &&
             (log.get<PrimitiveTypeVar>(subTy) || log.get<SingletonTypeVar>(subTy)))
    {
        tryUnifyScalarShape(subTy, superTy, /*reversed*/ false);
    }
    else if (FFlag::LuauScalarShapeSubtyping && log.get<TableTypeVar>(subTy) &&
             (log.get<PrimitiveTypeVar>(superTy) || log.get<SingletonTypeVar>(superTy)))
    {
        tryUnifyScalarShape(subTy, superTy, /*reversed*/ true);
    }

    // tryUnifyWithMetatable assumes its first argument is a MetatableTypeVar. The check is otherwise symmetrical.
    else if (log.getMutable<MetatableTypeVar>(superTy))
        tryUnifyWithMetatable(subTy, superTy, /*reversed*/ false);
    else if (log.getMutable<MetatableTypeVar>(subTy))
        tryUnifyWithMetatable(superTy, subTy, /*reversed*/ true);

    else if (log.getMutable<ClassTypeVar>(superTy))
        tryUnifyWithClass(subTy, superTy, /*reversed*/ false);

    // Unification of nonclasses with classes is almost, but not quite symmetrical.
    // The order in which we perform this test is significant in the case that both types are classes.
    else if (log.getMutable<ClassTypeVar>(subTy))
        tryUnifyWithClass(subTy, superTy, /*reversed*/ true);

    else
        reportError(TypeError{location, TypeMismatch{superTy, subTy}});

    if (cacheEnabled)
        cacheResult(subTy, superTy, errorCount);

    log.popSeen(superTy, subTy);
}

void Unifier::tryUnifyUnionWithType(TypeId subTy, const UnionTypeVar* subUnion, TypeId superTy)
{
    // A | B <: T if and only if A <: T and B <: T
    bool failed = false;
    std::optional<TypeError> unificationTooComplex;
    std::optional<TypeError> firstFailedOption;

    for (TypeId type : subUnion->options)
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
    }

    // even if A | B <: T fails, we want to bind some options of T with A | B iff A | B was a subtype of that option.
    auto tryBind = [this, subTy](TypeId superOption) {
        superOption = log.follow(superOption);

        // just skip if the superOption is not free-ish.
        auto ttv = log.getMutable<TableTypeVar>(superOption);
        if (!log.is<FreeTypeVar>(superOption) && (!ttv || ttv->state != TableState::Free))
            return;

        // If superOption is already present in subTy, do nothing.  Nothing new has been learned, but the subtype
        // test is successful.
        if (auto subUnion = get<UnionTypeVar>(subTy))
        {
            if (end(subUnion) != std::find(begin(subUnion), end(subUnion), superOption))
                return;
        }

        // Since we have already checked if S <: T, checking it again will not queue up the type for replacement.
        // So we'll have to do it ourselves. We assume they unified cleanly if they are still in the seen set.
        if (log.haveSeen(subTy, superOption))
        {
            // TODO: would it be nice for TxnLog::replace to do this?
            if (log.is<TableTypeVar>(superOption))
                log.bindTable(superOption, subTy);
            else
                log.replace(superOption, *subTy);
        }
    };

    if (auto superUnion = log.getMutable<UnionTypeVar>(superTy))
    {
        for (TypeId ty : superUnion)
            tryBind(ty);
    }
    else
        tryBind(superTy);

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

void Unifier::tryUnifyTypeWithUnion(TypeId subTy, TypeId superTy, const UnionTypeVar* uv, bool cacheEnabled, bool isFunctionCall)
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

    for (size_t i = 0; i < uv->options.size(); ++i)
    {
        TypeId type = uv->options[(i + startIndex) % uv->options.size()];
        Unifier innerState = makeChildUnifier();
        innerState.normalize = false;
        innerState.tryUnify_(subTy, type, isFunctionCall);

        if (innerState.errors.empty())
        {
            found = true;
            log.concat(std::move(innerState.log));

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
        const NormalizedType* subNorm = normalizer->normalize(subTy);
        const NormalizedType* superNorm = normalizer->normalize(superTy);
        if (!subNorm || !superNorm)
            reportError(TypeError{location, UnificationTooComplex{}});
        else if ((failedOptionCount == 1 || foundHeuristic) && failedOption)
            tryUnifyNormalizedTypes(subTy, superTy, *subNorm, *superNorm, "None of the union options are compatible. For example:", *failedOption);
        else
            tryUnifyNormalizedTypes(subTy, superTy, *subNorm, *superNorm, "none of the union options are compatible");
    }
    else if (!found)
    {
        if ((failedOptionCount == 1 || foundHeuristic) && failedOption)
            reportError(TypeError{location, TypeMismatch{superTy, subTy, "None of the union options are compatible. For example:", *failedOption}});
        else
            reportError(TypeError{location, TypeMismatch{superTy, subTy, "none of the union options are compatible"}});
    }
}

void Unifier::tryUnifyTypeWithIntersection(TypeId subTy, TypeId superTy, const IntersectionTypeVar* uv)
{
    std::optional<TypeError> unificationTooComplex;
    std::optional<TypeError> firstFailedOption;

    // T <: A & B if and only if  T <: A and T <: B
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

        log.concat(std::move(innerState.log));
    }

    if (unificationTooComplex)
        reportError(*unificationTooComplex);
    else if (firstFailedOption)
        reportError(TypeError{location, TypeMismatch{superTy, subTy, "Not all intersection parts are compatible.", *firstFailedOption}});
}

void Unifier::tryUnifyIntersectionWithType(TypeId subTy, const IntersectionTypeVar* uv, TypeId superTy, bool cacheEnabled, bool isFunctionCall)
{
    // A & B <: T if A <: T or B <: T
    bool found = false;
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

    for (size_t i = 0; i < uv->parts.size(); ++i)
    {
        TypeId type = uv->parts[(i + startIndex) % uv->parts.size()];
        Unifier innerState = makeChildUnifier();
        innerState.normalize = false;
        innerState.tryUnify_(type, superTy, isFunctionCall);

        if (innerState.errors.empty())
        {
            found = true;
            log.concat(std::move(innerState.log));
            break;
        }
        else if (auto e = hasUnificationTooComplex(innerState.errors))
        {
            unificationTooComplex = e;
        }
    }

    if (unificationTooComplex)
        reportError(*unificationTooComplex);
    else if (!found && normalize)
    {
        // It is possible that A & B <: T even though A </: T and B </: T
        // for example string? & number? <: nil.
        // We deal with this by type normalization.
        const NormalizedType* subNorm = normalizer->normalize(subTy);
        const NormalizedType* superNorm = normalizer->normalize(superTy);
        if (subNorm && superNorm)
            tryUnifyNormalizedTypes(subTy, superTy, *subNorm, *superNorm, "none of the intersection parts are compatible");
        else
            reportError(TypeError{location, UnificationTooComplex{}});
    }
    else if (!found)
    {
        reportError(TypeError{location, TypeMismatch{superTy, subTy, "none of the intersection parts are compatible"}});
    }
}

void Unifier::tryUnifyNormalizedTypes(
    TypeId subTy, TypeId superTy, const NormalizedType& subNorm, const NormalizedType& superNorm, std::string reason, std::optional<TypeError> error)
{
    LUAU_ASSERT(FFlag::LuauSubtypeNormalizer);

    if (get<UnknownTypeVar>(superNorm.tops) || get<AnyTypeVar>(superNorm.tops) || get<AnyTypeVar>(subNorm.tops))
        return;
    else if (get<UnknownTypeVar>(subNorm.tops))
        return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});

    if (get<ErrorTypeVar>(subNorm.errors))
        if (!get<ErrorTypeVar>(superNorm.errors))
            return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});

    if (get<PrimitiveTypeVar>(subNorm.booleans))
    {
        if (!get<PrimitiveTypeVar>(superNorm.booleans))
            return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});
    }
    else if (const SingletonTypeVar* stv = get<SingletonTypeVar>(subNorm.booleans))
    {
        if (!get<PrimitiveTypeVar>(superNorm.booleans) && stv != get<SingletonTypeVar>(superNorm.booleans))
            return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});
    }

    if (get<PrimitiveTypeVar>(subNorm.nils))
        if (!get<PrimitiveTypeVar>(superNorm.nils))
            return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});

    if (get<PrimitiveTypeVar>(subNorm.numbers))
        if (!get<PrimitiveTypeVar>(superNorm.numbers))
            return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});

    if (subNorm.strings && superNorm.strings)
    {
        for (auto [name, ty] : *subNorm.strings)
            if (!superNorm.strings->count(name))
                return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});
    }
    else if (!subNorm.strings && superNorm.strings)
        return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});

    if (get<PrimitiveTypeVar>(subNorm.threads))
        if (!get<PrimitiveTypeVar>(superNorm.errors))
            return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});

    for (TypeId subClass : subNorm.classes)
    {
        bool found = false;
        const ClassTypeVar* subCtv = get<ClassTypeVar>(subClass);
        for (TypeId superClass : superNorm.classes)
        {
            const ClassTypeVar* superCtv = get<ClassTypeVar>(superClass);
            if (isSubclass(subCtv, superCtv))
            {
                found = true;
                break;
            }
        }
        if (!found)
            return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});
    }

    for (TypeId subTable : subNorm.tables)
    {
        bool found = false;
        for (TypeId superTable : superNorm.tables)
        {
            Unifier innerState = makeChildUnifier();
            if (get<MetatableTypeVar>(superTable))
                innerState.tryUnifyWithMetatable(subTable, superTable, /* reversed */ false);
            else if (get<MetatableTypeVar>(subTable))
                innerState.tryUnifyWithMetatable(superTable, subTable, /* reversed */ true);
            else
                innerState.tryUnifyTables(subTable, superTable);
            if (innerState.errors.empty())
            {
                found = true;
                log.concat(std::move(innerState.log));
                break;
            }
            else if (auto e = hasUnificationTooComplex(innerState.errors))
                return reportError(*e);
        }
        if (!found)
            return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});
    }

    if (subNorm.functions)
    {
        if (!superNorm.functions)
            return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});
        if (superNorm.functions->empty())
            return;
        for (TypeId superFun : *superNorm.functions)
        {
            Unifier innerState = makeChildUnifier();
            const FunctionTypeVar* superFtv = get<FunctionTypeVar>(superFun);
            if (!superFtv)
                return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});
            TypePackId tgt = innerState.tryApplyOverloadedFunction(subTy, subNorm.functions, superFtv->argTypes);
            innerState.tryUnify_(tgt, superFtv->retTypes);
            if (innerState.errors.empty())
                log.concat(std::move(innerState.log));
            else if (auto e = hasUnificationTooComplex(innerState.errors))
                return reportError(*e);
            else
                return reportError(TypeError{location, TypeMismatch{superTy, subTy, reason, error}});
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
    if (!overloads || overloads->empty())
    {
        reportError(TypeError{location, CannotCallNonFunction{function}});
        return singletonTypes->errorRecoveryTypePack();
    }

    std::optional<TypePackId> result;
    const FunctionTypeVar* firstFun = nullptr;
    for (TypeId overload : *overloads)
    {
        if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(overload))
        {
            // TODO: instantiate generics?
            if (ftv->generics.empty() && ftv->genericPacks.empty())
            {
                if (!firstFun)
                    firstFun = ftv;
                Unifier innerState = makeChildUnifier();
                innerState.tryUnify_(args, ftv->argTypes);
                if (innerState.errors.empty())
                {
                    log.concat(std::move(innerState.log));
                    if (result)
                    {
                        // Annoyingly, since we don't support intersection of generic type packs,
                        // the intersection may fail. We rather arbitrarily use the first matching overload
                        // in that case.
                        if (std::optional<TypePackId> intersect = normalizer->intersectionOfTypePacks(*result, ftv->retTypes))
                            result = intersect;
                    }
                    else
                        result = ftv->retTypes;
                }
                else if (auto e = hasUnificationTooComplex(innerState.errors))
                {
                    reportError(*e);
                    return singletonTypes->errorRecoveryTypePack(args);
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
        reportError(TypeError{location, GenericError{"No matching overload."}});
        return singletonTypes->errorRecoveryTypePack(firstFun->retTypes);
    }
    else
    {
        reportError(TypeError{location, CannotCallNonFunction{function}});
        return singletonTypes->errorRecoveryTypePack();
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

    auto skipCacheFor = [this](TypeId ty) {
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
        scope = log.getMutable<Unifiable::Free>(packId)->scope;
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
    Unifier s = makeChildUnifier();
    s.tryUnify_(subTy, superTy);

    return s.errors;
}

ErrorVec Unifier::canUnify(TypePackId subTy, TypePackId superTy, bool isFunctionCall)
{
    Unifier s = makeChildUnifier();
    s.tryUnify_(subTy, superTy, isFunctionCall);

    return s.errors;
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
    RecursionLimiter _ra(&sharedState.counters.recursionCount,
        FFlag::LuauAutocompleteDynamicLimits ? sharedState.counters.recursionLimit : FInt::LuauTypeInferRecursionLimit);

    ++sharedState.counters.iterationCount;

    if (FFlag::LuauAutocompleteDynamicLimits)
    {
        if (sharedState.counters.iterationLimit > 0 && sharedState.counters.iterationLimit < sharedState.counters.iterationCount)
        {
            reportError(TypeError{location, UnificationTooComplex{}});
            return;
        }
    }
    else
    {
        if (FInt::LuauTypeInferIterationLimit > 0 && FInt::LuauTypeInferIterationLimit < sharedState.counters.iterationCount)
        {
            reportError(TypeError{location, UnificationTooComplex{}});
            return;
        }
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

    if (log.getMutable<Unifiable::Free>(superTp))
    {
        if (!occursCheck(superTp, subTp))
        {
            Widen widen{types, singletonTypes};
            log.replace(superTp, Unifiable::Bound<TypePackId>(widen(subTp)));
        }
    }
    else if (log.getMutable<Unifiable::Free>(subTp))
    {
        if (!occursCheck(subTp, superTp))
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
        auto [superTypes, superTail] = flatten(superTp, log);
        auto [subTypes, subTail] = flatten(subTp, log);

        bool noInfiniteGrowth = (superTypes.size() != subTypes.size()) && (superTail && log.getMutable<FreeTypePack>(*superTail)) &&
                                (subTail && log.getMutable<FreeTypePack>(*subTail));

        auto superIter = WeirdIter(superTp, log);
        auto subIter = WeirdIter(subTp, log);

        auto mkFreshType = [this](Scope* scope, TypeLevel level) {
            return types->freshType(scope, level);
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
                if (subTpv->tail && superTpv->tail)
                {
                    tryUnify_(*subTpv->tail, *superTpv->tail);
                    break;
                }

                const bool lFreeTail = superTpv->tail && log.getMutable<FreeTypePack>(log.follow(*superTpv->tail)) != nullptr;
                const bool rFreeTail = subTpv->tail && log.getMutable<FreeTypePack>(log.follow(*subTpv->tail)) != nullptr;
                if (lFreeTail)
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
                reportError(TypeError{location, CountMismatch{expectedSize, std::nullopt, actualSize, ctx}});

                while (superIter.good())
                {
                    tryUnify_(*superIter, singletonTypes->errorRecoveryType());
                    superIter.advance();
                }

                while (subIter.good())
                {
                    tryUnify_(*subIter, singletonTypes->errorRecoveryType());
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
    FunctionTypeVar* superFunction = log.getMutable<FunctionTypeVar>(superTy);
    FunctionTypeVar* subFunction = log.getMutable<FunctionTypeVar>(subTy);

    if (!superFunction || !subFunction)
        ice("passed non-function types to unifyFunction");

    size_t numGenerics = superFunction->generics.size();
    size_t numGenericPacks = superFunction->genericPacks.size();

    bool shouldInstantiate = (numGenerics == 0 && subFunction->generics.size() > 0) || (numGenericPacks == 0 && subFunction->genericPacks.size() > 0);

    if (FFlag::LuauInstantiateInSubtyping && variance == Covariant && shouldInstantiate)
    {
        Instantiation instantiation{&log, types, scope->level, scope};

        std::optional<TypeId> instantiated = instantiation.substitute(subTy);
        if (instantiated.has_value())
        {
            subFunction = log.getMutable<FunctionTypeVar>(*instantiated);

            if (!subFunction)
                ice("instantiation made a function type into a non-function type in unifyFunction");

            numGenerics = std::min(superFunction->generics.size(), subFunction->generics.size());
            numGenericPacks = std::min(superFunction->genericPacks.size(), subFunction->genericPacks.size());
        }
        else
        {
            reportError(TypeError{location, UnificationTooComplex{}});
        }
    }
    else if (numGenerics != subFunction->generics.size())
    {
        numGenerics = std::min(superFunction->generics.size(), subFunction->generics.size());

        reportError(TypeError{location, TypeMismatch{superTy, subTy, "different number of generic type parameters"}});
    }

    if (numGenericPacks != subFunction->genericPacks.size())
    {
        numGenericPacks = std::min(superFunction->genericPacks.size(), subFunction->genericPacks.size());

        reportError(TypeError{location, TypeMismatch{superTy, subTy, "different number of generic type pack parameters"}});
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

        innerState.ctx = CountMismatch::FunctionResult;
        innerState.tryUnify_(subFunction->retTypes, superFunction->retTypes);

        if (!reported)
        {
            if (auto e = hasUnificationTooComplex(innerState.errors))
                reportError(*e);
            else if (!innerState.errors.empty() && size(superFunction->retTypes) == 1 && finite(superFunction->retTypes))
                reportError(TypeError{location, TypeMismatch{superTy, subTy, "Return type is not compatible.", innerState.errors.front()}});
            else if (!innerState.errors.empty() && innerState.firstPackErrorPos)
                reportError(
                    TypeError{location, TypeMismatch{superTy, subTy, format("Return #%d type is not compatible.", *innerState.firstPackErrorPos),
                                            innerState.errors.front()}});
            else if (!innerState.errors.empty())
                reportError(TypeError{location, TypeMismatch{superTy, subTy, "", innerState.errors.front()}});
        }

        log.concat(std::move(innerState.log));
    }
    else
    {
        ctx = CountMismatch::Arg;
        tryUnify_(superFunction->argTypes, subFunction->argTypes, isFunctionCall);

        ctx = CountMismatch::FunctionResult;
        tryUnify_(subFunction->retTypes, superFunction->retTypes);
    }

    // Updating the log may have invalidated the function pointers
    superFunction = log.getMutable<FunctionTypeVar>(superTy);
    subFunction = log.getMutable<FunctionTypeVar>(subTy);

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

void Unifier::tryUnifyTables(TypeId subTy, TypeId superTy, bool isIntersection)
{
    TableTypeVar* superTable = log.getMutable<TableTypeVar>(superTy);
    TableTypeVar* subTable = log.getMutable<TableTypeVar>(subTy);

    if (!superTable || !subTable)
        ice("passed non-table types to unifyTables");

    std::vector<std::string> missingProperties;
    std::vector<std::string> extraProperties;

    if (FFlag::LuauInstantiateInSubtyping)
    {
        if (variance == Covariant && subTable->state == TableState::Generic && superTable->state != TableState::Generic)
        {
            Instantiation instantiation{&log, types, subTable->level, scope};

            std::optional<TypeId> instantiated = instantiation.substitute(subTy);
            if (instantiated.has_value())
            {
                subTable = log.getMutable<TableTypeVar>(*instantiated);

                if (!subTable)
                    ice("instantiation made a table type into a non-table type in tryUnifyTables");
            }
            else
            {
                reportError(TypeError{location, UnificationTooComplex{}});
            }
        }
    }

    // Optimization: First test that the property sets are compatible without doing any recursive unification
    if (!subTable->indexer && subTable->state != TableState::Free)
    {
        for (const auto& [propName, superProp] : superTable->props)
        {
            auto subIter = subTable->props.find(propName);

            if (subIter == subTable->props.end() && subTable->state == TableState::Unsealed && !isOptional(superProp.type))
                missingProperties.push_back(propName);
        }

        if (!missingProperties.empty())
        {
            reportError(TypeError{location, MissingProperties{superTy, subTy, std::move(missingProperties)}});
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

            if (innerState.errors.empty())
                log.concat(std::move(innerState.log));
        }
        else if (subTable->indexer && maybeString(subTable->indexer->indexType))
        {
            // TODO: read-only indexers don't need invariance
            // TODO: really we should only allow this if prop.type is optional.
            Resetter resetter{&variance};
            variance = Invariant;

            Unifier innerState = makeChildUnifier();
            innerState.tryUnify_(subTable->indexer->indexResultType, prop.type);

            checkChildUnifierTypeMismatch(innerState.errors, name, superTy, subTy);

            if (innerState.errors.empty())
                log.concat(std::move(innerState.log));
        }
        else if (subTable->state == TableState::Unsealed && isOptional(prop.type))
        // This is sound because unsealed table types are precise, so `{ p : T } <: { p : T, q : U? }`
        // since if `t : { p : T }` then we are guaranteed that `t.q` is `nil`.
        // TODO: if the supertype is written to, the subtype may no longer be precise (alias analysis?)
        {
        }
        else if (subTable->state == TableState::Free)
        {
            PendingType* pendingSub = log.queue(subTy);
            TableTypeVar* ttv = getMutable<TableTypeVar>(pendingSub);
            LUAU_ASSERT(ttv);
            ttv->props[name] = prop;
            subTable = ttv;
        }
        else
            missingProperties.push_back(name);

        // Recursive unification can change the txn log, and invalidate the old
        // table. If we detect that this has happened, we start over, with the updated
        // txn log.
        TableTypeVar* newSuperTable = log.getMutable<TableTypeVar>(superTy);
        TableTypeVar* newSubTable = log.getMutable<TableTypeVar>(subTy);
        if (superTable != newSuperTable || subTable != newSubTable)
        {
            if (errors.empty())
                return tryUnifyTables(subTy, superTy, isIntersection);
            else
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
            variance = Invariant;

            Unifier innerState = makeChildUnifier();
            innerState.tryUnify_(superTable->indexer->indexResultType, prop.type);

            checkChildUnifierTypeMismatch(innerState.errors, name, superTy, subTy);

            if (innerState.errors.empty())
                log.concat(std::move(innerState.log));
        }
        else if (superTable->state == TableState::Unsealed)
        {
            // TODO: this case is unsound when variance is Invariant, but without it lua-apps fails to typecheck.
            // TODO: file a JIRA
            // TODO: hopefully readonly/writeonly properties will fix this.
            Property clone = prop;
            clone.type = deeplyOptional(clone.type);

            PendingType* pendingSuper = log.queue(superTy);
            TableTypeVar* pendingSuperTtv = getMutable<TableTypeVar>(pendingSuper);
            pendingSuperTtv->props[name] = clone;
            superTable = pendingSuperTtv;
        }
        else if (variance == Covariant)
        {
        }
        else if (superTable->state == TableState::Free)
        {
            PendingType* pendingSuper = log.queue(superTy);
            TableTypeVar* pendingSuperTtv = getMutable<TableTypeVar>(pendingSuper);
            pendingSuperTtv->props[name] = prop;
            superTable = pendingSuperTtv;
        }
        else
            extraProperties.push_back(name);

        // Recursive unification can change the txn log, and invalidate the old
        // table. If we detect that this has happened, we start over, with the updated
        // txn log.
        TableTypeVar* newSuperTable = log.getMutable<TableTypeVar>(superTy);
        TableTypeVar* newSubTable = log.getMutable<TableTypeVar>(subTy);
        if (superTable != newSuperTable || subTable != newSubTable)
        {
            if (errors.empty())
                return tryUnifyTables(subTy, superTy, isIntersection);
            else
                return;
        }
    }

    // Unify indexers
    if (superTable->indexer && subTable->indexer)
    {
        // TODO: read-only indexers don't need invariance
        Resetter resetter{&variance};
        variance = Invariant;

        Unifier innerState = makeChildUnifier();

        innerState.tryUnify_(subTable->indexer->indexType, superTable->indexer->indexType);

        bool reported = !innerState.errors.empty();

        checkChildUnifierTypeMismatch(innerState.errors, "[indexer key]", superTy, subTy);

        innerState.tryUnify_(subTable->indexer->indexResultType, superTable->indexer->indexResultType);

        if (!reported)
            checkChildUnifierTypeMismatch(innerState.errors, "[indexer value]", superTy, subTy);

        if (innerState.errors.empty())
            log.concat(std::move(innerState.log));
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
    superTable = log.getMutable<TableTypeVar>(superTy);
    subTable = log.getMutable<TableTypeVar>(subTy);

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
        log.bindTable(superTy, subTy);
    }
    else if (subTable->state == TableState::Free)
    {
        log.bindTable(subTy, superTy);
    }
}

void Unifier::tryUnifyScalarShape(TypeId subTy, TypeId superTy, bool reversed)
{
    LUAU_ASSERT(FFlag::LuauScalarShapeSubtyping);

    TypeId osubTy = subTy;
    TypeId osuperTy = superTy;

    if (reversed)
        std::swap(subTy, superTy);

    if (auto ttv = log.get<TableTypeVar>(superTy); !ttv || ttv->state != TableState::Free)
        return reportError(TypeError{location, TypeMismatch{osuperTy, osubTy}});

    auto fail = [&](std::optional<TypeError> e) {
        std::string reason = "The former's metatable does not satisfy the requirements.";
        if (e)
            reportError(TypeError{location, TypeMismatch{osuperTy, osubTy, reason, *e}});
        else
            reportError(TypeError{location, TypeMismatch{osuperTy, osubTy, reason}});
    };

    // Given t1 where t1 = { lower: (t1) -> (a, b...) }
    // It should be the case that `string <: t1` iff `(subtype's metatable).__index <: t1`
    if (auto metatable = getMetatable(subTy, singletonTypes))
    {
        auto mttv = log.get<TableTypeVar>(*metatable);
        if (!mttv)
            fail(std::nullopt);

        if (auto it = mttv->props.find("__index"); it != mttv->props.end())
        {
            TypeId ty = it->second.type;
            Unifier child = makeChildUnifier();
            child.tryUnify_(ty, superTy);

            if (auto e = hasUnificationTooComplex(child.errors))
                reportError(*e);
            else if (!child.errors.empty())
                fail(child.errors.front());

            log.concat(std::move(child.log));

            return;
        }
        else
        {
            return fail(std::nullopt);
        }
    }

    reportError(TypeError{location, TypeMismatch{osuperTy, osubTy}});
    return;
}

TypeId Unifier::deeplyOptional(TypeId ty, std::unordered_map<TypeId, TypeId> seen)
{
    ty = follow(ty);
    if (isOptional(ty))
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
        return types->addType(UnionTypeVar{{singletonTypes->nilType, result}});
    }
    else
        return types->addType(UnionTypeVar{{singletonTypes->nilType, ty}});
}

void Unifier::tryUnifyWithMetatable(TypeId subTy, TypeId superTy, bool reversed)
{
    const MetatableTypeVar* superMetatable = get<MetatableTypeVar>(superTy);
    if (!superMetatable)
        ice("tryUnifyMetatable invoked with non-metatable TypeVar");

    TypeError mismatchError = TypeError{location, TypeMismatch{reversed ? subTy : superTy, reversed ? superTy : subTy}};

    if (const MetatableTypeVar* subMetatable = log.getMutable<MetatableTypeVar>(subTy))
    {
        Unifier innerState = makeChildUnifier();
        innerState.tryUnify_(subMetatable->table, superMetatable->table);
        innerState.tryUnify_(subMetatable->metatable, superMetatable->metatable);

        if (auto e = hasUnificationTooComplex(innerState.errors))
            reportError(*e);
        else if (!innerState.errors.empty())
            reportError(TypeError{location, TypeMismatch{reversed ? subTy : superTy, reversed ? superTy : subTy, "", innerState.errors.front()}});

        log.concat(std::move(innerState.log));
    }
    else if (TableTypeVar* subTable = log.getMutable<TableTypeVar>(subTy))
    {
        switch (subTable->state)
        {
        case TableState::Free:
        {
            if (FFlag::DebugLuauDeferredConstraintResolution)
            {
                Unifier innerState = makeChildUnifier();
                bool missingProperty = false;

                for (const auto& [propName, prop] : subTable->props)
                {
                    if (std::optional<TypeId> mtPropTy = findTablePropertyRespectingMeta(superTy, propName))
                    {
                        innerState.tryUnify(prop.type, *mtPropTy);
                    }
                    else
                    {
                        reportError(mismatchError);
                        missingProperty = true;
                        break;
                    }
                }

                if (const TableTypeVar* superTable = log.get<TableTypeVar>(log.follow(superMetatable->table)))
                {
                    // TODO: Unify indexers.
                }

                if (auto e = hasUnificationTooComplex(innerState.errors))
                    reportError(*e);
                else if (!innerState.errors.empty())
                    reportError(
                        TypeError{location, TypeMismatch{reversed ? subTy : superTy, reversed ? superTy : subTy, "", innerState.errors.front()}});
                else if (!missingProperty)
                {
                    log.concat(std::move(innerState.log));
                    log.bindTable(subTy, superTy);
                }
            }
            else
            {
                tryUnify_(subTy, superMetatable->table);
                log.bindTable(subTy, superTy);
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
    else if (log.getMutable<AnyTypeVar>(subTy) || log.getMutable<ErrorTypeVar>(subTy))
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

                if (innerState.errors.empty())
                {
                    log.concat(std::move(innerState.log));
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
            std::string msg = "Class " + superClass->name + " does not have an indexer";
            reportError(TypeError{location, GenericError{msg}});
        }

        if (!ok)
            return;

        log.bindTable(subTy, superTy);
    }
    else
        return fail();
}

static void queueTypePack(std::vector<TypeId>& queue, DenseHashSet<TypePackId>& seenTypePacks, Unifier& state, TypePackId a, TypePackId anyTypePack)
{
    while (true)
    {
        a = state.log.follow(a);

        if (seenTypePacks.find(a))
            break;
        seenTypePacks.insert(a);

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
}

void Unifier::tryUnifyVariadics(TypePackId subTp, TypePackId superTp, bool reversed, int subOffset)
{
    const VariadicTypePack* superVariadic = log.getMutable<VariadicTypePack>(superTp);

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
                log.replace(tail, BoundTypePack(superTp));
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
    const TypeArena* typeArena, TypeId anyType, TypePackId anyTypePack)
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

        if (state.log.getMutable<FreeTypeVar>(ty))
        {
            // TODO: Only bind if the anyType isn't any, unknown, or error (?)
            state.log.replace(ty, BoundTypeVar{anyType});
        }
        else if (auto fun = state.log.getMutable<FunctionTypeVar>(ty))
        {
            queueTypePack(queue, seenTypePacks, state, fun->argTypes, anyTypePack);
            queueTypePack(queue, seenTypePacks, state, fun->retTypes, anyTypePack);
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
}

void Unifier::tryUnifyWithAny(TypeId subTy, TypeId anyTy)
{
    LUAU_ASSERT(get<AnyTypeVar>(anyTy) || get<ErrorTypeVar>(anyTy) || get<UnknownTypeVar>(anyTy) || get<NeverTypeVar>(anyTy));

    // These types are not visited in general loop below
    if (get<PrimitiveTypeVar>(subTy) || get<AnyTypeVar>(subTy) || get<ClassTypeVar>(subTy))
        return;

    TypePackId anyTp;
    if (FFlag::LuauUnknownAndNeverType)
        anyTp = types->addTypePack(TypePackVar{VariadicTypePack{anyTy}});
    else
    {
        const TypePackId anyTypePack = types->addTypePack(TypePackVar{VariadicTypePack{singletonTypes->anyType}});
        anyTp = get<AnyTypeVar>(anyTy) ? anyTypePack : types->addTypePack(TypePackVar{Unifiable::Error{}});
    }

    std::vector<TypeId> queue = {subTy};

    sharedState.tempSeenTy.clear();
    sharedState.tempSeenTp.clear();

    Luau::tryUnifyWithAny(
        queue, *this, sharedState.tempSeenTy, sharedState.tempSeenTp, types, FFlag::LuauUnknownAndNeverType ? anyTy : singletonTypes->anyType, anyTp);
}

void Unifier::tryUnifyWithAny(TypePackId subTy, TypePackId anyTp)
{
    LUAU_ASSERT(get<Unifiable::Error>(anyTp));

    const TypeId anyTy = singletonTypes->errorRecoveryType();

    std::vector<TypeId> queue;

    sharedState.tempSeenTy.clear();
    sharedState.tempSeenTp.clear();

    queueTypePack(queue, sharedState.tempSeenTp, *this, subTy, anyTp);

    Luau::tryUnifyWithAny(queue, *this, sharedState.tempSeenTy, sharedState.tempSeenTp, types, anyTy, anyTp);
}

std::optional<TypeId> Unifier::findTablePropertyRespectingMeta(TypeId lhsType, Name name)
{
    return Luau::findTablePropertyRespectingMeta(singletonTypes, errors, lhsType, name, location);
}

void Unifier::tryUnifyWithConstrainedSubTypeVar(TypeId subTy, TypeId superTy)
{
    const ConstrainedTypeVar* subConstrained = get<ConstrainedTypeVar>(subTy);
    if (!subConstrained)
        ice("tryUnifyWithConstrainedSubTypeVar received non-ConstrainedTypeVar subTy!");

    const std::vector<TypeId>& subTyParts = subConstrained->parts;

    // A | B <: T if A <: T and B <: T
    bool failed = false;
    std::optional<TypeError> unificationTooComplex;

    const size_t count = subTyParts.size();

    for (size_t i = 0; i < count; ++i)
    {
        TypeId type = subTyParts[i];
        Unifier innerState = makeChildUnifier();
        innerState.tryUnify_(type, superTy);

        if (i == count - 1)
            log.concat(std::move(innerState.log));

        ++i;

        if (auto e = hasUnificationTooComplex(innerState.errors))
            unificationTooComplex = e;

        if (!innerState.errors.empty())
        {
            failed = true;
            break;
        }
    }

    if (unificationTooComplex)
        reportError(*unificationTooComplex);
    else if (failed)
        reportError(TypeError{location, TypeMismatch{superTy, subTy}});
    else
        log.replace(subTy, BoundTypeVar{superTy});
}

void Unifier::tryUnifyWithConstrainedSuperTypeVar(TypeId subTy, TypeId superTy)
{
    ConstrainedTypeVar* superC = log.getMutable<ConstrainedTypeVar>(superTy);
    if (!superC)
        ice("tryUnifyWithConstrainedSuperTypeVar received non-ConstrainedTypeVar superTy!");

    // subTy could be a
    //  table
    //  metatable
    //  class
    //  function
    //  primitive
    //  free
    //  generic
    //  intersection
    //  union
    // Do we really just tack it on?  I think we might!
    // We can certainly do some deduplication.
    // Is there any point to deducing Player|Instance when we could just reduce to Instance?
    // Is it actually ok to have multiple free types in a single intersection?  What if they are later unified into the same type?
    // Maybe we do a simplification step during quantification.

    auto it = std::find(superC->parts.begin(), superC->parts.end(), subTy);
    if (it != superC->parts.end())
        return;

    superC->parts.push_back(subTy);
}

void Unifier::unifyLowerBound(TypePackId subTy, TypePackId superTy, TypeLevel demotedLevel)
{
    // The duplication between this and regular typepack unification is tragic.

    auto superIter = begin(superTy, &log);
    auto superEndIter = end(superTy);

    auto subIter = begin(subTy, &log);
    auto subEndIter = end(subTy);

    int count = FInt::LuauTypeInferLowerBoundsIterationLimit;

    for (; subIter != subEndIter; ++subIter)
    {
        if (0 >= --count)
            ice("Internal recursion counter limit exceeded in Unifier::unifyLowerBound");

        if (superIter != superEndIter)
        {
            tryUnify_(*subIter, *superIter);
            ++superIter;
            continue;
        }

        if (auto t = superIter.tail())
        {
            TypePackId tailPack = follow(*t);

            if (log.get<FreeTypePack>(tailPack) && occursCheck(tailPack, subTy))
                return;

            FreeTypePack* freeTailPack = log.getMutable<FreeTypePack>(tailPack);
            if (!freeTailPack)
                return;

            TypePack* tp = getMutable<TypePack>(log.replace(tailPack, TypePack{}));

            for (; subIter != subEndIter; ++subIter)
            {
                tp->head.push_back(types->addType(ConstrainedTypeVar{demotedLevel, {follow(*subIter)}}));
            }

            tp->tail = subIter.tail();
        }

        return;
    }

    if (superIter != superEndIter)
    {
        if (auto subTail = subIter.tail())
        {
            TypePackId subTailPack = follow(*subTail);
            if (get<FreeTypePack>(subTailPack))
            {
                TypePack* tp = getMutable<TypePack>(log.replace(subTailPack, TypePack{}));

                for (; superIter != superEndIter; ++superIter)
                    tp->head.push_back(*superIter);
            }
            else if (const VariadicTypePack* subVariadic = log.getMutable<VariadicTypePack>(subTailPack))
            {
                while (superIter != superEndIter)
                {
                    tryUnify_(subVariadic->ty, *superIter);
                    ++superIter;
                }
            }
        }
        else
        {
            while (superIter != superEndIter)
            {
                if (!isOptional(*superIter))
                {
                    errors.push_back(TypeError{location, CountMismatch{size(superTy), std::nullopt, size(subTy), CountMismatch::Return}});
                    return;
                }
                ++superIter;
            }
        }

        return;
    }

    // Both iters are at their respective tails
    auto subTail = subIter.tail();
    auto superTail = superIter.tail();
    if (subTail && superTail)
        tryUnify(*subTail, *superTail);
    else if (subTail)
    {
        const FreeTypePack* freeSubTail = log.getMutable<FreeTypePack>(*subTail);
        if (freeSubTail)
        {
            log.replace(*subTail, TypePack{});
        }
    }
    else if (superTail)
    {
        const FreeTypePack* freeSuperTail = log.getMutable<FreeTypePack>(*superTail);
        if (freeSuperTail)
        {
            log.replace(*superTail, TypePack{});
        }
    }
}

bool Unifier::occursCheck(TypeId needle, TypeId haystack)
{
    sharedState.tempSeenTy.clear();

    return occursCheck(sharedState.tempSeenTy, needle, haystack);
}

bool Unifier::occursCheck(DenseHashSet<TypeId>& seen, TypeId needle, TypeId haystack)
{
    RecursionLimiter _ra(&sharedState.counters.recursionCount,
        FFlag::LuauAutocompleteDynamicLimits ? sharedState.counters.recursionLimit : FInt::LuauTypeInferRecursionLimit);

    bool occurrence = false;

    auto check = [&](TypeId tv) {
        if (occursCheck(seen, needle, tv))
            occurrence = true;
    };

    needle = log.follow(needle);
    haystack = log.follow(haystack);

    if (seen.find(haystack))
        return false;

    seen.insert(haystack);

    if (log.getMutable<Unifiable::Error>(needle))
        return false;

    if (!log.getMutable<Unifiable::Free>(needle))
        ice("Expected needle to be free");

    if (needle == haystack)
    {
        reportError(TypeError{location, OccursCheckFailed{}});
        log.replace(needle, *singletonTypes->errorRecoveryType());

        return true;
    }

    if (log.getMutable<FreeTypeVar>(haystack))
        return false;
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
    else if (auto a = log.getMutable<ConstrainedTypeVar>(haystack))
    {
        for (TypeId ty : a->parts)
            check(ty);
    }

    return occurrence;
}

bool Unifier::occursCheck(TypePackId needle, TypePackId haystack)
{
    sharedState.tempSeenTp.clear();

    return occursCheck(sharedState.tempSeenTp, needle, haystack);
}

bool Unifier::occursCheck(DenseHashSet<TypePackId>& seen, TypePackId needle, TypePackId haystack)
{
    needle = log.follow(needle);
    haystack = log.follow(haystack);

    if (seen.find(haystack))
        return false;

    seen.insert(haystack);

    if (log.getMutable<Unifiable::Error>(needle))
        return false;

    if (!log.getMutable<Unifiable::Free>(needle))
        ice("Expected needle pack to be free");

    RecursionLimiter _ra(&sharedState.counters.recursionCount,
        FFlag::LuauAutocompleteDynamicLimits ? sharedState.counters.recursionLimit : FInt::LuauTypeInferRecursionLimit);

    while (!log.getMutable<ErrorTypeVar>(haystack))
    {
        if (needle == haystack)
        {
            reportError(TypeError{location, OccursCheckFailed{}});
            log.replace(needle, *singletonTypes->errorRecoveryTypePack());

            return true;
        }

        if (auto a = get<TypePack>(haystack); a && a->tail)
        {
            haystack = log.follow(*a->tail);
            continue;
        }

        break;
    }

    return false;
}

Unifier Unifier::makeChildUnifier()
{
    Unifier u = Unifier{normalizer, mode, scope, location, variance, &log};
    u.anyIsTop = anyIsTop;
    u.normalize = normalize;
    return u;
}

// A utility function that appends the given error to the unifier's error log.
// This allows setting a breakpoint wherever the unifier reports an error.
void Unifier::reportError(TypeError err)
{
    errors.push_back(std::move(err));
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
