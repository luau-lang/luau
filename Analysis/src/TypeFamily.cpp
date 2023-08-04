// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeFamily.h"

#include "Luau/DenseHash.h"
#include "Luau/Instantiation.h"
#include "Luau/Normalize.h"
#include "Luau/Substitution.h"
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier.h"
#include "Luau/VisitType.h"

LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypeFamilyGraphReductionMaximumSteps, 1'000'000);

namespace Luau
{

struct InstanceCollector : TypeOnceVisitor
{
    std::deque<TypeId> tys;
    std::deque<TypePackId> tps;

    bool visit(TypeId ty, const TypeFamilyInstanceType&) override
    {
        // TypeOnceVisitor performs a depth-first traversal in the absence of
        // cycles. This means that by pushing to the front of the queue, we will
        // try to reduce deeper instances first if we start with the first thing
        // in the queue. Consider Add<Add<Add<number, number>, number>, number>:
        // we want to reduce the innermost Add<number, number> instantiation
        // first.
        tys.push_front(ty);
        return true;
    }

    bool visit(TypeId ty, const ClassType&) override
    {
        return false;
    }

    bool visit(TypePackId tp, const TypeFamilyInstanceTypePack&) override
    {
        // TypeOnceVisitor performs a depth-first traversal in the absence of
        // cycles. This means that by pushing to the front of the queue, we will
        // try to reduce deeper instances first if we start with the first thing
        // in the queue. Consider Add<Add<Add<number, number>, number>, number>:
        // we want to reduce the innermost Add<number, number> instantiation
        // first.
        tps.push_front(tp);
        return true;
    }
};

struct FamilyReducer
{
    std::deque<TypeId> queuedTys;
    std::deque<TypePackId> queuedTps;
    DenseHashSet<const void*> irreducible{nullptr};
    FamilyGraphReductionResult result;
    Location location;
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtins;
    TxnLog* parentLog = nullptr;
    TxnLog log;
    bool force = false;
    NotNull<Scope> scope;
    NotNull<Normalizer> normalizer;

    FamilyReducer(std::deque<TypeId> queuedTys, std::deque<TypePackId> queuedTps, Location location, NotNull<TypeArena> arena,
        NotNull<BuiltinTypes> builtins, NotNull<Scope> scope, NotNull<Normalizer> normalizer, TxnLog* parentLog = nullptr, bool force = false)
        : queuedTys(std::move(queuedTys))
        , queuedTps(std::move(queuedTps))
        , location(location)
        , arena(arena)
        , builtins(builtins)
        , parentLog(parentLog)
        , log(parentLog)
        , force(force)
        , scope(scope)
        , normalizer(normalizer)
    {
    }

    enum class SkipTestResult
    {
        Irreducible,
        Defer,
        Okay,
    };

    SkipTestResult testForSkippability(TypeId ty)
    {
        ty = log.follow(ty);

        if (log.is<TypeFamilyInstanceType>(ty))
        {
            if (!irreducible.contains(ty))
                return SkipTestResult::Defer;
            else
                return SkipTestResult::Irreducible;
        }
        else if (log.is<GenericType>(ty))
        {
            return SkipTestResult::Irreducible;
        }

        return SkipTestResult::Okay;
    }

    SkipTestResult testForSkippability(TypePackId ty)
    {
        ty = log.follow(ty);

        if (log.is<TypeFamilyInstanceTypePack>(ty))
        {
            if (!irreducible.contains(ty))
                return SkipTestResult::Defer;
            else
                return SkipTestResult::Irreducible;
        }
        else if (log.is<GenericTypePack>(ty))
        {
            return SkipTestResult::Irreducible;
        }

        return SkipTestResult::Okay;
    }

    template<typename T>
    void replace(T subject, T replacement)
    {
        if (parentLog)
            parentLog->replace(subject, Unifiable::Bound{replacement});
        else
            asMutable(subject)->ty.template emplace<Unifiable::Bound<T>>(replacement);

        if constexpr (std::is_same_v<T, TypeId>)
            result.reducedTypes.insert(subject);
        else if constexpr (std::is_same_v<T, TypePackId>)
            result.reducedPacks.insert(subject);
    }

    template<typename T>
    void handleFamilyReduction(T subject, TypeFamilyReductionResult<T> reduction)
    {
        if (reduction.result)
            replace(subject, *reduction.result);
        else
        {
            irreducible.insert(subject);

            if (reduction.uninhabited || force)
            {
                if constexpr (std::is_same_v<T, TypeId>)
                    result.errors.push_back(TypeError{location, UninhabitedTypeFamily{subject}});
                else if constexpr (std::is_same_v<T, TypePackId>)
                    result.errors.push_back(TypeError{location, UninhabitedTypePackFamily{subject}});
            }
            else if (!reduction.uninhabited && !force)
            {
                for (TypeId b : reduction.blockedTypes)
                    result.blockedTypes.insert(b);

                for (TypePackId b : reduction.blockedPacks)
                    result.blockedPacks.insert(b);
            }
        }
    }

    bool done()
    {
        return queuedTys.empty() && queuedTps.empty();
    }

    template<typename T, typename I>
    bool testParameters(T subject, const I* tfit)
    {
        for (TypeId p : tfit->typeArguments)
        {
            SkipTestResult skip = testForSkippability(p);

            if (skip == SkipTestResult::Irreducible)
            {
                irreducible.insert(subject);
                return false;
            }
            else if (skip == SkipTestResult::Defer)
            {
                if constexpr (std::is_same_v<T, TypeId>)
                    queuedTys.push_back(subject);
                else if constexpr (std::is_same_v<T, TypePackId>)
                    queuedTps.push_back(subject);

                return false;
            }
        }

        for (TypePackId p : tfit->packArguments)
        {
            SkipTestResult skip = testForSkippability(p);

            if (skip == SkipTestResult::Irreducible)
            {
                irreducible.insert(subject);
                return false;
            }
            else if (skip == SkipTestResult::Defer)
            {
                if constexpr (std::is_same_v<T, TypeId>)
                    queuedTys.push_back(subject);
                else if constexpr (std::is_same_v<T, TypePackId>)
                    queuedTps.push_back(subject);

                return false;
            }
        }

        return true;
    }

    void stepType()
    {
        TypeId subject = log.follow(queuedTys.front());
        queuedTys.pop_front();

        if (irreducible.contains(subject))
            return;

        if (const TypeFamilyInstanceType* tfit = log.get<TypeFamilyInstanceType>(subject))
        {
            if (!testParameters(subject, tfit))
                return;

            TypeFamilyReductionResult<TypeId> result =
                tfit->family->reducer(tfit->typeArguments, tfit->packArguments, arena, builtins, NotNull{&log}, scope, normalizer);
            handleFamilyReduction(subject, result);
        }
    }

    void stepPack()
    {
        TypePackId subject = log.follow(queuedTps.front());
        queuedTps.pop_front();

        if (irreducible.contains(subject))
            return;

        if (const TypeFamilyInstanceTypePack* tfit = log.get<TypeFamilyInstanceTypePack>(subject))
        {
            if (!testParameters(subject, tfit))
                return;

            TypeFamilyReductionResult<TypePackId> result =
                tfit->family->reducer(tfit->typeArguments, tfit->packArguments, arena, builtins, NotNull{&log}, scope, normalizer);
            handleFamilyReduction(subject, result);
        }
    }

    void step()
    {
        if (!queuedTys.empty())
            stepType();
        else if (!queuedTps.empty())
            stepPack();
    }
};

static FamilyGraphReductionResult reduceFamiliesInternal(std::deque<TypeId> queuedTys, std::deque<TypePackId> queuedTps, Location location,
    NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins, NotNull<Scope> scope, NotNull<Normalizer> normalizer, TxnLog* log, bool force)
{
    FamilyReducer reducer{std::move(queuedTys), std::move(queuedTps), location, arena, builtins, scope, normalizer, log, force};
    int iterationCount = 0;

    while (!reducer.done())
    {
        reducer.step();

        ++iterationCount;
        if (iterationCount > DFInt::LuauTypeFamilyGraphReductionMaximumSteps)
        {
            reducer.result.errors.push_back(TypeError{location, CodeTooComplex{}});
            break;
        }
    }

    return std::move(reducer.result);
}

FamilyGraphReductionResult reduceFamilies(TypeId entrypoint, Location location, NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins,
    NotNull<Scope> scope, NotNull<Normalizer> normalizer, TxnLog* log, bool force)
{
    InstanceCollector collector;

    try
    {
        collector.traverse(entrypoint);
    }
    catch (RecursionLimitException&)
    {
        return FamilyGraphReductionResult{};
    }

    if (collector.tys.empty() && collector.tps.empty())
        return {};

    return reduceFamiliesInternal(std::move(collector.tys), std::move(collector.tps), location, arena, builtins, scope, normalizer, log, force);
}

FamilyGraphReductionResult reduceFamilies(TypePackId entrypoint, Location location, NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins,
    NotNull<Scope> scope, NotNull<Normalizer> normalizer, TxnLog* log, bool force)
{
    InstanceCollector collector;

    try
    {
        collector.traverse(entrypoint);
    }
    catch (RecursionLimitException&)
    {
        return FamilyGraphReductionResult{};
    }

    if (collector.tys.empty() && collector.tps.empty())
        return {};

    return reduceFamiliesInternal(std::move(collector.tys), std::move(collector.tps), location, arena, builtins, scope, normalizer, log, force);
}

bool isPending(TypeId ty, NotNull<TxnLog> log)
{
    return log->is<FreeType>(ty) || log->is<BlockedType>(ty) || log->is<PendingExpansionType>(ty) || log->is<TypeFamilyInstanceType>(ty);
}

TypeFamilyReductionResult<TypeId> addFamilyFn(std::vector<TypeId> typeParams, std::vector<TypePackId> packParams, NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtins, NotNull<TxnLog> log, NotNull<Scope> scope, NotNull<Normalizer> normalizer)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        // TODO: ICE?
        LUAU_ASSERT(false);
        return {std::nullopt, true, {}, {}};
    }

    TypeId lhsTy = log->follow(typeParams.at(0));
    TypeId rhsTy = log->follow(typeParams.at(1));
    const NormalizedType* normLhsTy = normalizer->normalize(lhsTy);
    const NormalizedType* normRhsTy = normalizer->normalize(rhsTy);
    if (!normLhsTy || !normRhsTy)
    {
        return {std::nullopt, false, {}, {}};
    }
    else if (log->is<AnyType>(normLhsTy->tops) || log->is<AnyType>(normRhsTy->tops))
    {
        return {builtins->anyType, false, {}, {}};
    }
    else if ((normLhsTy->hasNumbers() || normLhsTy->hasTops()) && (normRhsTy->hasNumbers() || normRhsTy->hasTops()))
    {
        return {builtins->numberType, false, {}, {}};
    }
    else if (log->is<ErrorType>(lhsTy) || log->is<ErrorType>(rhsTy))
    {
        return {builtins->errorRecoveryType(), false, {}, {}};
    }
    else if (log->is<NeverType>(lhsTy) || log->is<NeverType>(rhsTy))
    {
        return {builtins->neverType, false, {}, {}};
    }
    else if (isPending(lhsTy, log))
    {
        return {std::nullopt, false, {lhsTy}, {}};
    }
    else if (isPending(rhsTy, log))
    {
        return {std::nullopt, false, {rhsTy}, {}};
    }

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> addMm = findMetatableEntry(builtins, dummy, lhsTy, "__add", Location{});
    bool reversed = false;
    if (!addMm)
    {
        addMm = findMetatableEntry(builtins, dummy, rhsTy, "__add", Location{});
        reversed = true;
    }

    if (!addMm)
        return {std::nullopt, true, {}, {}};

    if (isPending(log->follow(*addMm), log))
        return {std::nullopt, false, {log->follow(*addMm)}, {}};

    const FunctionType* mmFtv = log->get<FunctionType>(log->follow(*addMm));
    if (!mmFtv)
        return {std::nullopt, true, {}, {}};

    TypeCheckLimits limits; // TODO: We need to thread TypeCheckLimits in from Frontend to here.
    if (std::optional<TypeId> instantiatedAddMm = instantiate(builtins, arena, NotNull{&limits}, scope, log->follow(*addMm)))
    {
        if (const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedAddMm))
        {
            std::vector<TypeId> inferredArgs;
            if (!reversed)
                inferredArgs = {lhsTy, rhsTy};
            else
                inferredArgs = {rhsTy, lhsTy};

            TypePackId inferredArgPack = arena->addTypePack(std::move(inferredArgs));
            Unifier u{normalizer, scope, Location{}, Variance::Covariant, log.get()};
            u.tryUnify(inferredArgPack, instantiatedMmFtv->argTypes);

            if (std::optional<TypeId> ret = first(instantiatedMmFtv->retTypes); ret && u.errors.empty())
            {
                return {u.log.follow(*ret), false, {}, {}};
            }
            else
            {
                return {std::nullopt, true, {}, {}};
            }
        }
        else
        {
            return {builtins->errorRecoveryType(), false, {}, {}};
        }
    }
    else
    {
        // TODO: Not the nicest logic here.
        return {std::nullopt, true, {}, {}};
    }
}

BuiltinTypeFamilies::BuiltinTypeFamilies()
    : addFamily{"Add", addFamilyFn}
{
}

} // namespace Luau
