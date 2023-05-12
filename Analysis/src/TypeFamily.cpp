// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeFamily.h"

#include "Luau/DenseHash.h"
#include "Luau/VisitType.h"
#include "Luau/TxnLog.h"
#include "Luau/Substitution.h"
#include "Luau/ToString.h"

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
    TxnLog* log = nullptr;
    NotNull<const TxnLog> reducerLog;
    bool force = false;

    FamilyReducer(std::deque<TypeId> queuedTys, std::deque<TypePackId> queuedTps, Location location, NotNull<TypeArena> arena,
        NotNull<BuiltinTypes> builtins, TxnLog* log = nullptr, bool force = false)
        : queuedTys(std::move(queuedTys))
        , queuedTps(std::move(queuedTps))
        , location(location)
        , arena(arena)
        , builtins(builtins)
        , log(log)
        , reducerLog(NotNull{log ? log : TxnLog::empty()})
        , force(force)
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
        ty = reducerLog->follow(ty);

        if (reducerLog->is<TypeFamilyInstanceType>(ty))
        {
            if (!irreducible.contains(ty))
                return SkipTestResult::Defer;
            else
                return SkipTestResult::Irreducible;
        }
        else if (reducerLog->is<GenericType>(ty))
        {
            return SkipTestResult::Irreducible;
        }

        return SkipTestResult::Okay;
    }

    SkipTestResult testForSkippability(TypePackId ty)
    {
        ty = reducerLog->follow(ty);

        if (reducerLog->is<TypeFamilyInstanceTypePack>(ty))
        {
            if (!irreducible.contains(ty))
                return SkipTestResult::Defer;
            else
                return SkipTestResult::Irreducible;
        }
        else if (reducerLog->is<GenericTypePack>(ty))
        {
            return SkipTestResult::Irreducible;
        }

        return SkipTestResult::Okay;
    }

    template<typename T>
    void replace(T subject, T replacement)
    {
        if (log)
            log->replace(subject, Unifiable::Bound{replacement});
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
        TypeId subject = reducerLog->follow(queuedTys.front());
        queuedTys.pop_front();

        if (irreducible.contains(subject))
            return;

        if (const TypeFamilyInstanceType* tfit = reducerLog->get<TypeFamilyInstanceType>(subject))
        {
            if (!testParameters(subject, tfit))
                return;

            TypeFamilyReductionResult<TypeId> result = tfit->family->reducer(tfit->typeArguments, tfit->packArguments, arena, builtins, reducerLog);
            handleFamilyReduction(subject, result);
        }
    }

    void stepPack()
    {
        TypePackId subject = reducerLog->follow(queuedTps.front());
        queuedTps.pop_front();

        if (irreducible.contains(subject))
            return;

        if (const TypeFamilyInstanceTypePack* tfit = reducerLog->get<TypeFamilyInstanceTypePack>(subject))
        {
            if (!testParameters(subject, tfit))
                return;

            TypeFamilyReductionResult<TypePackId> result =
                tfit->family->reducer(tfit->typeArguments, tfit->packArguments, arena, builtins, reducerLog);
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
    NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins, TxnLog* log, bool force)
{
    FamilyReducer reducer{std::move(queuedTys), std::move(queuedTps), location, arena, builtins, log, force};
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

FamilyGraphReductionResult reduceFamilies(
    TypeId entrypoint, Location location, NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins, TxnLog* log, bool force)
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

    return reduceFamiliesInternal(std::move(collector.tys), std::move(collector.tps), location, arena, builtins, log, force);
}

FamilyGraphReductionResult reduceFamilies(
    TypePackId entrypoint, Location location, NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins, TxnLog* log, bool force)
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

    return reduceFamiliesInternal(std::move(collector.tys), std::move(collector.tps), location, arena, builtins, log, force);
}

} // namespace Luau
