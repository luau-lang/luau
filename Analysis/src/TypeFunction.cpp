// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeFunction.h"

#include "Luau/Common.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/DenseHash.h"
#include "Luau/Normalize.h"
#include "Luau/NotNull.h"
#include "Luau/OverloadResolution.h"
#include "Luau/Subtyping.h"
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/TypeChecker2.h"
#include "Luau/TypeFunctionReductionGuesser.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"
#include "Luau/VecDeque.h"
#include "Luau/VisitType.h"

// used to control emitting CodeTooComplex warnings on type function reduction
LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypeFamilyGraphReductionMaximumSteps, 1'000'000);

// used to control the limits of type function application over union type arguments
// e.g. `mul<a | b, c | d>` blows up into `mul<a, c> | mul<a, d> | mul<b, c> | mul<b, d>`
LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypeFamilyApplicationCartesianProductLimit, 5'000);

// used to control falling back to a more conservative reduction based on guessing
// when this value is set to a negative value, guessing will be totally disabled.
LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypeFamilyUseGuesserDepth, -1);

LUAU_FASTFLAG(DebugLuauEqSatSimplification)

LUAU_FASTFLAGVARIABLE(DebugLuauLogTypeFamilies)
LUAU_FASTFLAG(LuauExplicitSkipBoundTypes)

namespace Luau
{

using TypeOrTypePackIdSet = DenseHashSet<const void*>;

struct InstanceCollector : TypeOnceVisitor
{
    DenseHashSet<TypeId> recordedTys{nullptr};
    VecDeque<TypeId> tys;
    DenseHashSet<TypePackId> recordedTps{nullptr};
    VecDeque<TypePackId> tps;
    TypeOrTypePackIdSet shouldGuess{nullptr};
    std::vector<const void*> typeFunctionInstanceStack;
    std::vector<TypeId> cyclicInstance;


    InstanceCollector()
        : TypeOnceVisitor("InstanceCollector", FFlag::LuauExplicitSkipBoundTypes)
    {
    }

    bool visit(TypeId ty, const TypeFunctionInstanceType& tfit) override
    {
        // TypeVisitor performs a depth-first traversal in the absence of
        // cycles. This means that by pushing to the front of the queue, we will
        // try to reduce deeper instances first if we start with the first thing
        // in the queue. Consider Add<Add<Add<number, number>, number>, number>:
        // we want to reduce the innermost Add<number, number> instantiation
        // first.

        typeFunctionInstanceStack.push_back(ty);

        if (DFInt::LuauTypeFamilyUseGuesserDepth >= 0 && int(typeFunctionInstanceStack.size()) > DFInt::LuauTypeFamilyUseGuesserDepth)
            shouldGuess.insert(ty);

        if (!recordedTys.contains(ty))
        {
            recordedTys.insert(ty);
            tys.push_front(ty);
        }

        for (TypeId p : tfit.typeArguments)
            traverse(p);

        for (TypePackId p : tfit.packArguments)
            traverse(p);

        typeFunctionInstanceStack.pop_back();

        return false;
    }

    void cycle(TypeId ty) override
    {
        TypeId t = follow(ty);

        if (get<TypeFunctionInstanceType>(t))
        {
            // If we see a type a second time and it's in the type function stack, it's a real cycle
            if (std::find(typeFunctionInstanceStack.begin(), typeFunctionInstanceStack.end(), t) != typeFunctionInstanceStack.end())
                cyclicInstance.push_back(t);
        }
    }

    bool visit(TypeId ty, const ExternType&) override
    {
        return false;
    }

    bool visit(TypePackId tp, const TypeFunctionInstanceTypePack& tfitp) override
    {
        // TypeVisitor performs a depth-first traversal in the absence of
        // cycles. This means that by pushing to the front of the queue, we will
        // try to reduce deeper instances first if we start with the first thing
        // in the queue. Consider Add<Add<Add<number, number>, number>, number>:
        // we want to reduce the innermost Add<number, number> instantiation
        // first.

        typeFunctionInstanceStack.push_back(tp);

        if (DFInt::LuauTypeFamilyUseGuesserDepth >= 0 && int(typeFunctionInstanceStack.size()) > DFInt::LuauTypeFamilyUseGuesserDepth)
            shouldGuess.insert(tp);

        if (!recordedTps.contains(tp))
        {
            recordedTps.insert(tp);
            tps.push_front(tp);
        }

        for (TypeId p : tfitp.typeArguments)
            traverse(p);

        for (TypePackId p : tfitp.packArguments)
            traverse(p);

        typeFunctionInstanceStack.pop_back();

        return false;
    }
};

struct UnscopedGenericFinder : TypeOnceVisitor
{
    std::vector<TypeId> scopeGenTys;
    std::vector<TypePackId> scopeGenTps;
    bool foundUnscoped = false;

    UnscopedGenericFinder()
        : TypeOnceVisitor("UnscopedGenericFinder", FFlag::LuauExplicitSkipBoundTypes)
    {
    }

    bool visit(TypeId ty) override
    {
        // Once we have found an unscoped generic, we will stop the traversal
        return !foundUnscoped;
    }

    bool visit(TypePackId tp) override
    {
        // Once we have found an unscoped generic, we will stop the traversal
        return !foundUnscoped;
    }

    bool visit(TypeId ty, const GenericType&) override
    {
        if (std::find(scopeGenTys.begin(), scopeGenTys.end(), ty) == scopeGenTys.end())
            foundUnscoped = true;

        return false;
    }

    bool visit(TypePackId tp, const GenericTypePack&) override
    {
        if (std::find(scopeGenTps.begin(), scopeGenTps.end(), tp) == scopeGenTps.end())
            foundUnscoped = true;

        return false;
    }

    bool visit(TypeId ty, const FunctionType& ftv) override
    {
        size_t startTyCount = scopeGenTys.size();
        size_t startTpCount = scopeGenTps.size();

        scopeGenTys.insert(scopeGenTys.end(), ftv.generics.begin(), ftv.generics.end());
        scopeGenTps.insert(scopeGenTps.end(), ftv.genericPacks.begin(), ftv.genericPacks.end());

        traverse(ftv.argTypes);
        traverse(ftv.retTypes);

        scopeGenTys.resize(startTyCount);
        scopeGenTps.resize(startTpCount);

        return false;
    }

    bool visit(TypeId ty, const ExternType&) override
    {
        return false;
    }
};

struct TypeFunctionReducer
{
    NotNull<TypeFunctionContext> ctx;

    VecDeque<TypeId> queuedTys;
    VecDeque<TypePackId> queuedTps;
    TypeOrTypePackIdSet shouldGuess;
    std::vector<TypeId> cyclicTypeFunctions;
    TypeOrTypePackIdSet irreducible{nullptr};
    FunctionGraphReductionResult result;
    bool force = false;

    // Local to the constraint being reduced.
    Location location;

    TypeFunctionReducer(
        VecDeque<TypeId> queuedTys,
        VecDeque<TypePackId> queuedTps,
        TypeOrTypePackIdSet shouldGuess,
        std::vector<TypeId> cyclicTypes,
        Location location,
        NotNull<TypeFunctionContext> ctx,
        bool force = false
    )
        : ctx(ctx)
        , queuedTys(std::move(queuedTys))
        , queuedTps(std::move(queuedTps))
        , shouldGuess(std::move(shouldGuess))
        , cyclicTypeFunctions(std::move(cyclicTypes))
        , force(force)
        , location(location)
    {
    }

    enum class SkipTestResult
    {
        /// If a type function is cyclic, it cannot be reduced, but maybe we can
        /// make a guess and offer a suggested annotation to the user.
        CyclicTypeFunction,

        /// Indicase that we will not be able to reduce this type function this
        /// time. Constraint resolution may cause this type function to become
        /// reducible later.
        Irreducible,

        /// A type function that cannot be reduced any further because it has no valid reduction.
        /// eg add<number, string>
        Stuck,

        /// Some type functions can operate on generic parameters
        Generic,

        /// We might be able to reduce this type function, but not yet.
        Defer,

        /// We can attempt to reduce this type function right now.
        Okay,
    };

    SkipTestResult testForSkippability(TypeId ty)
    {
        VecDeque<TypeId> queue;
        DenseHashSet<TypeId> seen{nullptr};

        queue.push_back(follow(ty));

        while (!queue.empty())
        {
            TypeId t = queue.front();
            queue.pop_front();

            if (seen.contains(t))
                continue;

            if (auto tfit = get<TypeFunctionInstanceType>(t))
            {
                if (tfit->state == TypeFunctionInstanceState::Stuck)
                    return SkipTestResult::Stuck;
                else if (tfit->state == TypeFunctionInstanceState::Solved)
                    return SkipTestResult::Generic;

                for (auto cyclicTy : cyclicTypeFunctions)
                {
                    if (t == cyclicTy)
                        return SkipTestResult::CyclicTypeFunction;
                }

                if (!irreducible.contains(t))
                    return SkipTestResult::Defer;

                return SkipTestResult::Irreducible;
            }
            else if (is<GenericType>(t))
                return SkipTestResult::Generic;
            else if (auto it = get<IntersectionType>(t))
            {
                for (TypeId part : it->parts)
                    queue.push_back(follow(part));
            }

            seen.insert(t);
        }

        return SkipTestResult::Okay;
    }

    SkipTestResult testForSkippability(TypePackId ty) const
    {
        ty = follow(ty);

        if (is<TypeFunctionInstanceTypePack>(ty))
        {
            if (!irreducible.contains(ty))
                return SkipTestResult::Defer;
            else
                return SkipTestResult::Irreducible;
        }
        else if (is<GenericTypePack>(ty))
        {
            return SkipTestResult::Generic;
        }

        return SkipTestResult::Okay;
    }

    template<typename T>
    void replace(T subject, T replacement)
    {
        if (subject->owningArena != ctx->arena.get())
        {
            result.errors.emplace_back(location, InternalError{"Attempting to modify a type function instance from another arena"});
            return;
        }

        if (FFlag::DebugLuauLogTypeFamilies)
            printf("%s => %s\n", toString(subject, {true}).c_str(), toString(replacement, {true}).c_str());

        asMutable(subject)->ty.template emplace<Unifiable::Bound<T>>(replacement);

        if constexpr (std::is_same_v<T, TypeId>)
            result.reducedTypes.insert(subject);
        else if constexpr (std::is_same_v<T, TypePackId>)
            result.reducedPacks.insert(subject);
    }

    TypeFunctionInstanceState getState(TypeId ty) const
    {
        auto tfit = get<TypeFunctionInstanceType>(ty);
        LUAU_ASSERT(tfit);
        return tfit->state;
    }

    void setState(TypeId ty, TypeFunctionInstanceState state) const
    {
        if (ty->owningArena != ctx->arena)
            return;

        TypeFunctionInstanceType* tfit = getMutable<TypeFunctionInstanceType>(ty);
        LUAU_ASSERT(tfit);
        tfit->state = state;
    }

    TypeFunctionInstanceState getState(TypePackId tp) const
    {
        return TypeFunctionInstanceState::Unsolved;
    }

    void setState(TypePackId tp, TypeFunctionInstanceState state) const
    {
        // We do not presently have any type pack functions at all.
        (void)tp;
        (void)state;
    }

    template<typename T>
    void handleTypeFunctionReduction(T subject, TypeFunctionReductionResult<T> reduction)
    {
        for (auto& message : reduction.messages)
            result.messages.emplace_back(location, UserDefinedTypeFunctionError{std::move(message)});

        if (reduction.result)
            replace(subject, *reduction.result);
        else
        {
            irreducible.insert(subject);

            if (reduction.error.has_value())
                result.errors.emplace_back(location, UserDefinedTypeFunctionError{*reduction.error});

            if (reduction.reductionStatus != Reduction::MaybeOk || force)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("%s is uninhabited\n", toString(subject, {true}).c_str());

                if (getState(subject) == TypeFunctionInstanceState::Unsolved)
                {
                    if (reduction.reductionStatus == Reduction::Erroneous)
                        setState(subject, TypeFunctionInstanceState::Stuck);
                    else if (reduction.reductionStatus == Reduction::Irreducible)
                        setState(subject, TypeFunctionInstanceState::Solved);
                    else if (reduction.reductionStatus == Reduction::MaybeOk)
                    {
                        // We cannot make progress because something is unsolved, but we're also forcing.
                        setState(subject, TypeFunctionInstanceState::Stuck);
                    }
                    else
                        ctx->ice->ice("Unexpected TypeFunctionInstanceState");
                }

                if constexpr (std::is_same_v<T, TypeId>)
                    result.errors.emplace_back(location, UninhabitedTypeFunction{subject});
                else if constexpr (std::is_same_v<T, TypePackId>)
                    result.errors.emplace_back(location, UninhabitedTypePackFunction{subject});
            }
            else if (reduction.reductionStatus == Reduction::MaybeOk && !force)
            {
                // We're not forcing and the reduction couldn't proceed, but it isn't obviously busted.
                // Report that this type blocks further reduction.

                if (FFlag::DebugLuauLogTypeFamilies)
                    printf(
                        "%s is irreducible; blocked on %zu types, %zu packs\n",
                        toString(subject, {true}).c_str(),
                        reduction.blockedTypes.size(),
                        reduction.blockedPacks.size()
                    );

                for (TypeId b : reduction.blockedTypes)
                    result.blockedTypes.insert(b);

                for (TypePackId b : reduction.blockedPacks)
                    result.blockedPacks.insert(b);
            }
            else
                LUAU_ASSERT(!"Unreachable");
        }
    }

    bool done() const
    {
        return queuedTys.empty() && queuedTps.empty();
    }

    template<typename T, typename I>
    bool testParameters(T subject, const I* tfit)
    {
        for (TypeId p : tfit->typeArguments)
        {
            SkipTestResult skip = testForSkippability(p);

            if (skip == SkipTestResult::Stuck)
            {
                // SkipTestResult::Stuck cannot happen when this flag is unset.
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("%s is stuck!\n", toString(subject, {true}).c_str());

                irreducible.insert(subject);
                setState(subject, TypeFunctionInstanceState::Stuck);

                return false;
            }
            if (skip == SkipTestResult::Irreducible || (skip == SkipTestResult::Generic && !tfit->function->canReduceGenerics))
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                {
                    if (skip == SkipTestResult::Generic)
                        printf("%s is solved due to a dependency on %s\n", toString(subject, {true}).c_str(), toString(p, {true}).c_str());
                    else
                        printf("%s is irreducible due to a dependency on %s\n", toString(subject, {true}).c_str(), toString(p, {true}).c_str());
                }

                irreducible.insert(subject);

                if (skip == SkipTestResult::Generic)
                    setState(subject, TypeFunctionInstanceState::Solved);

                return false;
            }
            else if (skip == SkipTestResult::Defer)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("Deferring %s until %s is solved\n", toString(subject, {true}).c_str(), toString(p, {true}).c_str());

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

            if (skip == SkipTestResult::Irreducible || (skip == SkipTestResult::Generic && !tfit->function->canReduceGenerics))
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("%s is irreducible due to a dependency on %s\n", toString(subject, {true}).c_str(), toString(p, {true}).c_str());

                irreducible.insert(subject);
                return false;
            }
            else if (skip == SkipTestResult::Defer)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("Deferring %s until %s is solved\n", toString(subject, {true}).c_str(), toString(p, {true}).c_str());

                if constexpr (std::is_same_v<T, TypeId>)
                    queuedTys.push_back(subject);
                else if constexpr (std::is_same_v<T, TypePackId>)
                    queuedTps.push_back(subject);

                return false;
            }
        }

        return true;
    }

    template<typename TID>
    inline bool tryGuessing(TID subject)
    {
        if (shouldGuess.contains(subject))
        {
            if (FFlag::DebugLuauLogTypeFamilies)
                printf("Flagged %s for reduction with guesser.\n", toString(subject, {true}).c_str());

            TypeFunctionReductionGuesser guesser{ctx->arena, ctx->builtins, ctx->normalizer};
            auto guessed = guesser.guess(subject);

            if (guessed)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("Selected %s as the guessed result type.\n", toString(*guessed, {true}).c_str());

                replace(subject, *guessed);
                return true;
            }

            if (FFlag::DebugLuauLogTypeFamilies)
                printf("Failed to produce a guess for the result of %s.\n", toString(subject, {true}).c_str());
        }

        return false;
    }

    void stepType()
    {
        TypeId subject = follow(queuedTys.front());
        queuedTys.pop_front();

        if (irreducible.contains(subject))
            return;

        if (FFlag::DebugLuauLogTypeFamilies)
            printf("Trying to %sreduce %s\n", force ? "force " : "", toString(subject, {true}).c_str());

        if (const TypeFunctionInstanceType* tfit = get<TypeFunctionInstanceType>(subject))
        {
            if (tfit->function->name == "user")
            {
                UnscopedGenericFinder finder;
                finder.traverse(subject);

                if (finder.foundUnscoped)
                {
                    // Do not step into this type again
                    irreducible.insert(subject);

                    // Let the caller know this type will not become reducible
                    result.irreducibleTypes.insert(subject);

                    if (FFlag::DebugLuauLogTypeFamilies)
                        printf("Irreducible due to an unscoped generic type\n");

                    return;
                }
            }

            SkipTestResult testCyclic = testForSkippability(subject);

            if (!testParameters(subject, tfit) && testCyclic != SkipTestResult::CyclicTypeFunction)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("Irreducible due to irreducible/pending and a non-cyclic function\n");

                if (tfit->state == TypeFunctionInstanceState::Stuck || tfit->state == TypeFunctionInstanceState::Solved)
                    tryGuessing(subject);

                return;
            }

            if (tryGuessing(subject))
                return;

            ctx->userFuncName = tfit->userFuncName;

            TypeFunctionReductionResult<TypeId> result = tfit->function->reducer(subject, tfit->typeArguments, tfit->packArguments, ctx);
            handleTypeFunctionReduction(subject, std::move(result));
        }
    }

    void stepPack()
    {
        TypePackId subject = follow(queuedTps.front());
        queuedTps.pop_front();

        if (irreducible.contains(subject))
            return;

        if (FFlag::DebugLuauLogTypeFamilies)
            printf("Trying to reduce %s\n", toString(subject, {true}).c_str());

        if (const TypeFunctionInstanceTypePack* tfit = get<TypeFunctionInstanceTypePack>(subject))
        {
            if (!testParameters(subject, tfit))
                return;

            if (tryGuessing(subject))
                return;

            TypeFunctionReductionResult<TypePackId> result = tfit->function->reducer(subject, tfit->typeArguments, tfit->packArguments, ctx);
            handleTypeFunctionReduction(subject, std::move(result));
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

static FunctionGraphReductionResult reduceFunctionsInternal(
    VecDeque<TypeId> queuedTys,
    VecDeque<TypePackId> queuedTps,
    TypeOrTypePackIdSet shouldGuess,
    std::vector<TypeId> cyclics,
    Location location,
    NotNull<TypeFunctionContext> ctx,
    bool force
)
{
    TypeFunctionReducer reducer{std::move(queuedTys), std::move(queuedTps), std::move(shouldGuess), std::move(cyclics), location, ctx, force};
    int iterationCount = 0;

    // If we are reducing a type function while reducing a type function,
    // we're probably doing something clowny. One known place this can
    // occur is type function reduction => overload selection => subtyping
    // => back to type function reduction. At worst, if there's a reduction
    // that _doesn't_ loop forever and _needs_ reentrancy, we'll fail to
    // handle that and potentially emit an error when we didn't need to.
    if (ctx->normalizer->sharedState->reentrantTypeReduction)
        return {};

    TypeReductionRentrancyGuard _{ctx->normalizer->sharedState};
    while (!reducer.done())
    {
        reducer.step();

        ++iterationCount;
        if (iterationCount > DFInt::LuauTypeFamilyGraphReductionMaximumSteps)
        {
            reducer.result.errors.emplace_back(location, CodeTooComplex{});
            break;
        }
    }

    return std::move(reducer.result);
}

FunctionGraphReductionResult reduceTypeFunctions(TypeId entrypoint, Location location, NotNull<TypeFunctionContext> ctx, bool force)
{
    InstanceCollector collector;

    try
    {
        collector.traverse(entrypoint);
    }
    catch (RecursionLimitException&)
    {
        return FunctionGraphReductionResult{};
    }

    if (collector.tys.empty() && collector.tps.empty())
        return {};

    return reduceFunctionsInternal(
        std::move(collector.tys),
        std::move(collector.tps),
        std::move(collector.shouldGuess),
        std::move(collector.cyclicInstance),
        location,
        ctx,
        force
    );
}

FunctionGraphReductionResult reduceTypeFunctions(TypePackId entrypoint, Location location, NotNull<TypeFunctionContext> ctx, bool force)
{
    InstanceCollector collector;

    try
    {
        collector.traverse(entrypoint);
    }
    catch (RecursionLimitException&)
    {
        return FunctionGraphReductionResult{};
    }

    if (collector.tys.empty() && collector.tps.empty())
        return {};

    return reduceFunctionsInternal(
        std::move(collector.tys),
        std::move(collector.tps),
        std::move(collector.shouldGuess),
        std::move(collector.cyclicInstance),
        location,
        ctx,
        force
    );
}

bool isPending(TypeId ty, ConstraintSolver* solver)
{
    if (auto tfit = get<TypeFunctionInstanceType>(ty); tfit && tfit->state == TypeFunctionInstanceState::Unsolved)
        return true;
    return is<BlockedType, PendingExpansionType>(ty) || (solver && solver->hasUnresolvedConstraints(ty));
}

} // namespace Luau
