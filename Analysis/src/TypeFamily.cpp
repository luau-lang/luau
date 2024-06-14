// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeFamily.h"

#include "Luau/Common.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/DenseHash.h"
#include "Luau/Instantiation.h"
#include "Luau/Normalize.h"
#include "Luau/NotNull.h"
#include "Luau/OverloadResolution.h"
#include "Luau/Set.h"
#include "Luau/Simplify.h"
#include "Luau/Subtyping.h"
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/TypeFamilyReductionGuesser.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"
#include "Luau/VecDeque.h"
#include "Luau/VisitType.h"

#include <iterator>

// used to control emitting CodeTooComplex warnings on type family reduction
LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypeFamilyGraphReductionMaximumSteps, 1'000'000);

// used to control the limits of type family application over union type arguments
// e.g. `mul<a | b, c | d>` blows up into `mul<a, c> | mul<a, d> | mul<b, c> | mul<b, d>`
LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypeFamilyApplicationCartesianProductLimit, 5'000);

// used to control falling back to a more conservative reduction based on guessing
// when this value is set to a negative value, guessing will be totally disabled.
LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypeFamilyUseGuesserDepth, -1);

LUAU_FASTFLAGVARIABLE(DebugLuauLogTypeFamilies, false);

namespace Luau
{

using TypeOrTypePackIdSet = DenseHashSet<const void*>;

struct InstanceCollector : TypeOnceVisitor
{
    VecDeque<TypeId> tys;
    VecDeque<TypePackId> tps;
    TypeOrTypePackIdSet shouldGuess{nullptr};
    std::vector<TypeId> cyclicInstance;

    bool visit(TypeId ty, const TypeFamilyInstanceType&) override
    {
        // TypeOnceVisitor performs a depth-first traversal in the absence of
        // cycles. This means that by pushing to the front of the queue, we will
        // try to reduce deeper instances first if we start with the first thing
        // in the queue. Consider Add<Add<Add<number, number>, number>, number>:
        // we want to reduce the innermost Add<number, number> instantiation
        // first.

        if (DFInt::LuauTypeFamilyUseGuesserDepth >= 0 && typeFamilyDepth > DFInt::LuauTypeFamilyUseGuesserDepth)
            shouldGuess.insert(ty);

        tys.push_front(ty);

        return true;
    }

    void cycle(TypeId ty) override
    {
        /// Detected cyclic type pack
        TypeId t = follow(ty);
        if (get<TypeFamilyInstanceType>(t))
            cyclicInstance.push_back(t);
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

        if (DFInt::LuauTypeFamilyUseGuesserDepth >= 0 && typeFamilyDepth > DFInt::LuauTypeFamilyUseGuesserDepth)
            shouldGuess.insert(tp);

        tps.push_front(tp);

        return true;
    }
};

struct FamilyReducer
{
    TypeFamilyContext ctx;

    VecDeque<TypeId> queuedTys;
    VecDeque<TypePackId> queuedTps;
    TypeOrTypePackIdSet shouldGuess;
    std::vector<TypeId> cyclicTypeFamilies;
    TypeOrTypePackIdSet irreducible{nullptr};
    FamilyGraphReductionResult result;
    bool force = false;

    // Local to the constraint being reduced.
    Location location;

    FamilyReducer(VecDeque<TypeId> queuedTys, VecDeque<TypePackId> queuedTps, TypeOrTypePackIdSet shouldGuess, std::vector<TypeId> cyclicTypes,
        Location location, TypeFamilyContext ctx, bool force = false)
        : ctx(ctx)
        , queuedTys(std::move(queuedTys))
        , queuedTps(std::move(queuedTps))
        , shouldGuess(std::move(shouldGuess))
        , cyclicTypeFamilies(std::move(cyclicTypes))
        , force(force)
        , location(location)
    {
    }

    enum class SkipTestResult
    {
        CyclicTypeFamily,
        Irreducible,
        Defer,
        Okay,
    };

    SkipTestResult testForSkippability(TypeId ty)
    {
        ty = follow(ty);

        if (is<TypeFamilyInstanceType>(ty))
        {
            for (auto t : cyclicTypeFamilies)
            {
                if (ty == t)
                    return SkipTestResult::CyclicTypeFamily;
            }

            if (!irreducible.contains(ty))
                return SkipTestResult::Defer;

            return SkipTestResult::Irreducible;
        }
        else if (is<GenericType>(ty))
        {
            return SkipTestResult::Irreducible;
        }

        return SkipTestResult::Okay;
    }

    SkipTestResult testForSkippability(TypePackId ty)
    {
        ty = follow(ty);

        if (is<TypeFamilyInstanceTypePack>(ty))
        {
            if (!irreducible.contains(ty))
                return SkipTestResult::Defer;
            else
                return SkipTestResult::Irreducible;
        }
        else if (is<GenericTypePack>(ty))
        {
            return SkipTestResult::Irreducible;
        }

        return SkipTestResult::Okay;
    }

    template<typename T>
    void replace(T subject, T replacement)
    {
        if (subject->owningArena != ctx.arena.get())
            ctx.ice->ice("Attempting to modify a type family instance from another arena", location);

        if (FFlag::DebugLuauLogTypeFamilies)
            printf("%s -> %s\n", toString(subject, {true}).c_str(), toString(replacement, {true}).c_str());

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
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("%s is uninhabited\n", toString(subject, {true}).c_str());

                if constexpr (std::is_same_v<T, TypeId>)
                    result.errors.push_back(TypeError{location, UninhabitedTypeFamily{subject}});
                else if constexpr (std::is_same_v<T, TypePackId>)
                    result.errors.push_back(TypeError{location, UninhabitedTypePackFamily{subject}});
            }
            else if (!reduction.uninhabited && !force)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("%s is irreducible; blocked on %zu types, %zu packs\n", toString(subject, {true}).c_str(), reduction.blockedTypes.size(),
                        reduction.blockedPacks.size());

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

        for (TypePackId p : tfit->packArguments)
        {
            SkipTestResult skip = testForSkippability(p);

            if (skip == SkipTestResult::Irreducible)
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

            TypeFamilyReductionGuesser guesser{ctx.arena, ctx.builtins, ctx.normalizer};
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
            printf("Trying to reduce %s\n", toString(subject, {true}).c_str());

        if (const TypeFamilyInstanceType* tfit = get<TypeFamilyInstanceType>(subject))
        {
            SkipTestResult testCyclic = testForSkippability(subject);

            if (!testParameters(subject, tfit) && testCyclic != SkipTestResult::CyclicTypeFamily)
            {
                if (FFlag::DebugLuauLogTypeFamilies)
                    printf("Irreducible due to irreducible/pending and a non-cyclic family\n");

                return;
            }

            if (tryGuessing(subject))
                return;

            TypeFamilyReductionResult<TypeId> result =
                tfit->family->reducer(subject, tfit->typeArguments, tfit->packArguments, NotNull{&ctx});
            handleFamilyReduction(subject, result);
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

        if (const TypeFamilyInstanceTypePack* tfit = get<TypeFamilyInstanceTypePack>(subject))
        {
            if (!testParameters(subject, tfit))
                return;

            if (tryGuessing(subject))
                return;

            TypeFamilyReductionResult<TypePackId> result =
                tfit->family->reducer(subject, tfit->typeArguments, tfit->packArguments, NotNull{&ctx});
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

static FamilyGraphReductionResult reduceFamiliesInternal(VecDeque<TypeId> queuedTys, VecDeque<TypePackId> queuedTps, TypeOrTypePackIdSet shouldGuess,
    std::vector<TypeId> cyclics, Location location, TypeFamilyContext ctx, bool force)
{
    FamilyReducer reducer{std::move(queuedTys), std::move(queuedTps), std::move(shouldGuess), std::move(cyclics), location, ctx, force};
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

FamilyGraphReductionResult reduceFamilies(TypeId entrypoint, Location location, TypeFamilyContext ctx, bool force)
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

    return reduceFamiliesInternal(std::move(collector.tys), std::move(collector.tps), std::move(collector.shouldGuess),
        std::move(collector.cyclicInstance), location, ctx, force);
}

FamilyGraphReductionResult reduceFamilies(TypePackId entrypoint, Location location, TypeFamilyContext ctx, bool force)
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

    return reduceFamiliesInternal(std::move(collector.tys), std::move(collector.tps), std::move(collector.shouldGuess),
        std::move(collector.cyclicInstance), location, ctx, force);
}

bool isPending(TypeId ty, ConstraintSolver* solver)
{
    return is<BlockedType, PendingExpansionType, TypeFamilyInstanceType>(ty) || (solver && solver->hasUnresolvedConstraints(ty));
}

template<typename F, typename... Args>
static std::optional<TypeFamilyReductionResult<TypeId>> tryDistributeTypeFamilyApp(F f, TypeId instance,
    const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx, Args&& ...args)
{
    // op (a | b) (c | d) ~ (op a (c | d)) | (op b (c | d)) ~ (op a c) | (op a d) | (op b c) | (op b d)
    bool uninhabited = false;
    std::vector<TypeId> blockedTypes;
    std::vector<TypeId> results;
    size_t cartesianProductSize = 1;

    const UnionType* firstUnion = nullptr;
    size_t unionIndex;

    std::vector<TypeId> arguments = typeParams;
    for (size_t i = 0; i < arguments.size(); ++i)
    {
        const UnionType* ut = get<UnionType>(follow(arguments[i]));
        if (!ut)
            continue;

        // We want to find the first union type in the set of arguments to distribute that one and only that one union.
        // The function `f` we have is recursive, so `arguments[unionIndex]` will be updated in-place for each option in
        // the union we've found in this context, so that index will no longer be a union type. Any other arguments at
        // index + 1 or after will instead be distributed, if those are a union, which will be subjected to the same rules.
        if (!firstUnion && ut)
        {
            firstUnion = ut;
            unionIndex = i;
        }

        cartesianProductSize *= std::distance(begin(ut), end(ut));

        // TODO: We'd like to report that the type family application is too complex here.
        if (size_t(DFInt::LuauTypeFamilyApplicationCartesianProductLimit) <= cartesianProductSize)
            return {{std::nullopt, true, {}, {}}};
    }

    if (!firstUnion)
    {
        // If we couldn't find any union type argument, we're not distributing.
        return std::nullopt;
    }

    for (TypeId option : firstUnion)
    {
        arguments[unionIndex] = option;

        TypeFamilyReductionResult<TypeId> result = f(instance, arguments, packParams, ctx, args...);
        blockedTypes.insert(blockedTypes.end(), result.blockedTypes.begin(), result.blockedTypes.end());
        uninhabited |= result.uninhabited;

        if (result.uninhabited || !result.result)
            break;
        else
            results.push_back(*result.result);
    }

    if (uninhabited || !blockedTypes.empty())
        return {{std::nullopt, uninhabited, blockedTypes, {}}};

    if (!results.empty())
    {
        if (results.size() == 1)
            return {{results[0], false, {}, {}}};

        TypeId resultTy = ctx->arena->addType(TypeFamilyInstanceType{
            NotNull{&kBuiltinTypeFamilies.unionFamily},
            std::move(results),
            {},
        });

        return {{resultTy, false, {}, {}}};
    }

    return std::nullopt;
}

TypeFamilyReductionResult<TypeId> notFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("not type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId ty = follow(typeParams.at(0));

    if (ty == instance)
        return {ctx->builtins->neverType, false, {}, {}};

    if (isPending(ty, ctx->solver))
        return {std::nullopt, false, {ty}, {}};

    if (auto result = tryDistributeTypeFamilyApp(notFamilyFn, instance, typeParams, packParams, ctx))
        return *result;

    // `not` operates on anything and returns a `boolean` always.
    return {ctx->builtins->booleanType, false, {}, {}};
}

TypeFamilyReductionResult<TypeId> lenFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("len type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId operandTy = follow(typeParams.at(0));

    if (operandTy == instance)
        return {ctx->builtins->neverType, false, {}, {}};

    // check to see if the operand type is resolved enough, and wait to reduce if not
    // the use of `typeFromNormal` later necessitates blocking on local types.
    if (isPending(operandTy, ctx->solver))
        return {std::nullopt, false, {operandTy}, {}};

    // if the type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> maybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, operandTy);
        if (!maybeGeneralized)
            return {std::nullopt, false, {operandTy}, {}};
        operandTy = *maybeGeneralized;
    }

    std::shared_ptr<const NormalizedType> normTy = ctx->normalizer->normalize(operandTy);
    NormalizationResult inhabited = ctx->normalizer->isInhabited(normTy.get());

    // if the type failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normTy || inhabited == NormalizationResult::HitLimits)
        return {std::nullopt, false, {}, {}};

    // if the operand type is error suppressing, we can immediately reduce to `number`.
    if (normTy->shouldSuppressErrors())
        return {ctx->builtins->numberType, false, {}, {}};

    // if we have an uninhabited type (like `never`), we can never observe that the operator didn't work.
    if (inhabited == NormalizationResult::False)
        return {ctx->builtins->neverType, false, {}, {}};

    // if we're checking the length of a string, that works!
    if (normTy->isSubtypeOfString())
        return {ctx->builtins->numberType, false, {}, {}};

    // we use the normalized operand here in case there was an intersection or union.
    TypeId normalizedOperand = ctx->normalizer->typeFromNormal(*normTy);
    if (normTy->hasTopTable() || get<TableType>(normalizedOperand))
        return {ctx->builtins->numberType, false, {}, {}};

    if (auto result = tryDistributeTypeFamilyApp(notFamilyFn, instance, typeParams, packParams, ctx))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, operandTy, "__len", Location{});
    if (!mmType)
        return {std::nullopt, true, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, false, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, true, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, true, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorRecoveryType(), false, {}, {}};

    TypePackId inferredArgPack = ctx->arena->addTypePack({operandTy});
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (!u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, true, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->normalizer, ctx->ice, ctx->scope};
    if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, true, {}, {}};

    // `len` must return a `number`.
    return {ctx->builtins->numberType, false, {}, {}};
}

TypeFamilyReductionResult<TypeId> unmFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("unm type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId operandTy = follow(typeParams.at(0));

    if (operandTy == instance)
        return {ctx->builtins->neverType, false, {}, {}};

    // check to see if the operand type is resolved enough, and wait to reduce if not
    if (isPending(operandTy, ctx->solver))
        return {std::nullopt, false, {operandTy}, {}};

    // if the type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> maybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, operandTy);
        if (!maybeGeneralized)
            return {std::nullopt, false, {operandTy}, {}};
        operandTy = *maybeGeneralized;
    }

    std::shared_ptr<const NormalizedType> normTy = ctx->normalizer->normalize(operandTy);

    // if the operand failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normTy)
        return {std::nullopt, false, {}, {}};

    // if the operand is error suppressing, we can just go ahead and reduce.
    if (normTy->shouldSuppressErrors())
        return {operandTy, false, {}, {}};

    // if we have a `never`, we can never observe that the operation didn't work.
    if (is<NeverType>(operandTy))
        return {ctx->builtins->neverType, false, {}, {}};

    // If the type is exactly `number`, we can reduce now.
    if (normTy->isExactlyNumber())
        return {ctx->builtins->numberType, false, {}, {}};

    if (auto result = tryDistributeTypeFamilyApp(notFamilyFn, instance, typeParams, packParams, ctx))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, operandTy, "__unm", Location{});
    if (!mmType)
        return {std::nullopt, true, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, false, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, true, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, true, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorRecoveryType(), false, {}, {}};

    TypePackId inferredArgPack = ctx->arena->addTypePack({operandTy});
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (!u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, true, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->normalizer, ctx->ice, ctx->scope};
    if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, true, {}, {}};

    if (std::optional<TypeId> ret = first(instantiatedMmFtv->retTypes))
        return {*ret, false, {}, {}};
    else
        return {std::nullopt, true, {}, {}};
}

NotNull<Constraint> TypeFamilyContext::pushConstraint(ConstraintV&& c)
{
    LUAU_ASSERT(solver);
    NotNull<Constraint> newConstraint = solver->pushConstraint(scope, constraint ? constraint->location : Location{}, std::move(c));

    // Every constraint that is blocked on the current constraint must also be
    // blocked on this new one.
    if (constraint)
        solver->inheritBlocks(NotNull{constraint}, newConstraint);

    return newConstraint;
}

TypeFamilyReductionResult<TypeId> numericBinopFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx, const std::string metamethod)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // isPending of `lhsTy` or `rhsTy` would return true, even if it cycles. We want a different answer for that.
    if (lhsTy == instance || rhsTy == instance)
        return {ctx->builtins->neverType, false, {}, {}};

    // if we have a `never`, we can never observe that the math operator is unreachable.
    if (is<NeverType>(lhsTy) || is<NeverType>(rhsTy))
        return {ctx->builtins->neverType, false, {}, {}};

    const Location location = ctx->constraint ? ctx->constraint->location : Location{};

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> lhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, lhsTy);
        std::optional<TypeId> rhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, rhsTy);

        if (!lhsMaybeGeneralized)
            return {std::nullopt, false, {lhsTy}, {}};
        else if (!rhsMaybeGeneralized)
            return {std::nullopt, false, {rhsTy}, {}};

        lhsTy = *lhsMaybeGeneralized;
        rhsTy = *rhsMaybeGeneralized;
    }

    // TODO: Normalization needs to remove cyclic type families from a `NormalizedType`.
    std::shared_ptr<const NormalizedType> normLhsTy = ctx->normalizer->normalize(lhsTy);
    std::shared_ptr<const NormalizedType> normRhsTy = ctx->normalizer->normalize(rhsTy);

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy)
        return {std::nullopt, false, {}, {}};

    // if one of the types is error suppressing, we can reduce to `any` since we should suppress errors in the result of the usage.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->anyType, false, {}, {}};

    // if we're adding two `number` types, the result is `number`.
    if (normLhsTy->isExactlyNumber() && normRhsTy->isExactlyNumber())
        return {ctx->builtins->numberType, false, {}, {}};

    if (auto result = tryDistributeTypeFamilyApp(numericBinopFamilyFn, instance, typeParams, packParams, ctx, metamethod))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, lhsTy, metamethod, location);
    bool reversed = false;
    if (!mmType)
    {
        mmType = findMetatableEntry(ctx->builtins, dummy, rhsTy, metamethod, location);
        reversed = true;
    }

    if (!mmType)
        return {std::nullopt, true, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, false, {*mmType}, {}};

    TypePackId argPack = ctx->arena->addTypePack({lhsTy, rhsTy});
    SolveResult solveResult;

    if (!reversed)
        solveResult = solveFunctionCall(ctx->arena, ctx->builtins, ctx->normalizer, ctx->ice, ctx->limits, ctx->scope, location, *mmType, argPack);
    else
    {
        TypePack* p = getMutable<TypePack>(argPack);
        std::swap(p->head.front(), p->head.back());
        solveResult = solveFunctionCall(ctx->arena, ctx->builtins, ctx->normalizer, ctx->ice, ctx->limits, ctx->scope, location, *mmType, argPack);
    }

    if (!solveResult.typePackId.has_value())
        return {std::nullopt, true, {}, {}};

    TypePack extracted = extendTypePack(*ctx->arena, ctx->builtins, *solveResult.typePackId, 1);
    if (extracted.head.empty())
        return {std::nullopt, true, {}, {}};

    return {extracted.head.front(), false, {}, {}};
}

TypeFamilyReductionResult<TypeId> addFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("add type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(instance, typeParams, packParams, ctx, "__add");
}

TypeFamilyReductionResult<TypeId> subFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("sub type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(instance, typeParams, packParams, ctx, "__sub");
}

TypeFamilyReductionResult<TypeId> mulFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("mul type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(instance, typeParams, packParams, ctx, "__mul");
}

TypeFamilyReductionResult<TypeId> divFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("div type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(instance, typeParams, packParams, ctx, "__div");
}

TypeFamilyReductionResult<TypeId> idivFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("integer div type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(instance, typeParams, packParams, ctx, "__idiv");
}

TypeFamilyReductionResult<TypeId> powFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("pow type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(instance, typeParams, packParams, ctx, "__pow");
}

TypeFamilyReductionResult<TypeId> modFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("modulo type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(instance, typeParams, packParams, ctx, "__mod");
}

TypeFamilyReductionResult<TypeId> concatFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("concat type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // isPending of `lhsTy` or `rhsTy` would return true, even if it cycles. We want a different answer for that.
    if (lhsTy == instance || rhsTy == instance)
        return {ctx->builtins->neverType, false, {}, {}};

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> lhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, lhsTy);
        std::optional<TypeId> rhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, rhsTy);

        if (!lhsMaybeGeneralized)
            return {std::nullopt, false, {lhsTy}, {}};
        else if (!rhsMaybeGeneralized)
            return {std::nullopt, false, {rhsTy}, {}};

        lhsTy = *lhsMaybeGeneralized;
        rhsTy = *rhsMaybeGeneralized;
    }

    std::shared_ptr<const NormalizedType> normLhsTy = ctx->normalizer->normalize(lhsTy);
    std::shared_ptr<const NormalizedType> normRhsTy = ctx->normalizer->normalize(rhsTy);

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy)
        return {std::nullopt, false, {}, {}};

    // if one of the types is error suppressing, we can reduce to `any` since we should suppress errors in the result of the usage.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->anyType, false, {}, {}};

    // if we have a `never`, we can never observe that the numeric operator didn't work.
    if (is<NeverType>(lhsTy) || is<NeverType>(rhsTy))
        return {ctx->builtins->neverType, false, {}, {}};

    // if we're concatenating two elements that are either strings or numbers, the result is `string`.
    if ((normLhsTy->isSubtypeOfString() || normLhsTy->isExactlyNumber()) && (normRhsTy->isSubtypeOfString() || normRhsTy->isExactlyNumber()))
        return {ctx->builtins->stringType, false, {}, {}};

    if (auto result = tryDistributeTypeFamilyApp(concatFamilyFn, instance, typeParams, packParams, ctx))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, lhsTy, "__concat", Location{});
    bool reversed = false;
    if (!mmType)
    {
        mmType = findMetatableEntry(ctx->builtins, dummy, rhsTy, "__concat", Location{});
        reversed = true;
    }

    if (!mmType)
        return {std::nullopt, true, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, false, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, true, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, true, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorRecoveryType(), false, {}, {}};

    std::vector<TypeId> inferredArgs;
    if (!reversed)
        inferredArgs = {lhsTy, rhsTy};
    else
        inferredArgs = {rhsTy, lhsTy};

    TypePackId inferredArgPack = ctx->arena->addTypePack(std::move(inferredArgs));
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (!u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, true, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->normalizer, ctx->ice, ctx->scope};
    if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, true, {}, {}};

    return {ctx->builtins->stringType, false, {}, {}};
}

TypeFamilyReductionResult<TypeId> andFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("and type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // t1 = and<lhs, t1> ~> lhs
    if (follow(rhsTy) == instance && lhsTy != rhsTy)
        return {lhsTy, false, {}, {}};
    // t1 = and<t1, rhs> ~> rhs
    if (follow(lhsTy) == instance && lhsTy != rhsTy)
        return {rhsTy, false, {}, {}};

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> lhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, lhsTy);
        std::optional<TypeId> rhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, rhsTy);

        if (!lhsMaybeGeneralized)
            return {std::nullopt, false, {lhsTy}, {}};
        else if (!rhsMaybeGeneralized)
            return {std::nullopt, false, {rhsTy}, {}};

        lhsTy = *lhsMaybeGeneralized;
        rhsTy = *rhsMaybeGeneralized;
    }

    // And evalutes to a boolean if the LHS is falsey, and the RHS type if LHS is truthy.
    SimplifyResult filteredLhs = simplifyIntersection(ctx->builtins, ctx->arena, lhsTy, ctx->builtins->falsyType);
    SimplifyResult overallResult = simplifyUnion(ctx->builtins, ctx->arena, rhsTy, filteredLhs.result);
    std::vector<TypeId> blockedTypes{};
    for (auto ty : filteredLhs.blockedTypes)
        blockedTypes.push_back(ty);
    for (auto ty : overallResult.blockedTypes)
        blockedTypes.push_back(ty);
    return {overallResult.result, false, std::move(blockedTypes), {}};
}

TypeFamilyReductionResult<TypeId> orFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("or type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // t1 = or<lhs, t1> ~> lhs
    if (follow(rhsTy) == instance && lhsTy != rhsTy)
        return {lhsTy, false, {}, {}};
    // t1 = or<t1, rhs> ~> rhs
    if (follow(lhsTy) == instance && lhsTy != rhsTy)
        return {rhsTy, false, {}, {}};

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> lhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, lhsTy);
        std::optional<TypeId> rhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, rhsTy);

        if (!lhsMaybeGeneralized)
            return {std::nullopt, false, {lhsTy}, {}};
        else if (!rhsMaybeGeneralized)
            return {std::nullopt, false, {rhsTy}, {}};

        lhsTy = *lhsMaybeGeneralized;
        rhsTy = *rhsMaybeGeneralized;
    }

    // Or evalutes to the LHS type if the LHS is truthy, and the RHS type if LHS is falsy.
    SimplifyResult filteredLhs = simplifyIntersection(ctx->builtins, ctx->arena, lhsTy, ctx->builtins->truthyType);
    SimplifyResult overallResult = simplifyUnion(ctx->builtins, ctx->arena, rhsTy, filteredLhs.result);
    std::vector<TypeId> blockedTypes{};
    for (auto ty : filteredLhs.blockedTypes)
        blockedTypes.push_back(ty);
    for (auto ty : overallResult.blockedTypes)
        blockedTypes.push_back(ty);
    return {overallResult.result, false, std::move(blockedTypes), {}};
}

static TypeFamilyReductionResult<TypeId> comparisonFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx, const std::string metamethod)
{

    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    if (lhsTy == instance || rhsTy == instance)
        return {ctx->builtins->neverType, false, {}, {}};

    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    // Algebra Reduction Rules for comparison family functions
    // Note that comparing to never tells you nothing about the other operand
    // lt< 'a , never> -> continue
    // lt< never, 'a>  -> continue
    // lt< 'a, t>      -> 'a is t - we'll solve the constraint, return and solve lt<t, t> -> bool
    // lt< t, 'a>      -> same as above
    bool canSubmitConstraint = ctx->solver && ctx->constraint;
    bool lhsFree = get<FreeType>(lhsTy) != nullptr;
    bool rhsFree = get<FreeType>(rhsTy) != nullptr;
    if (canSubmitConstraint)
    {
        // Implement injective type families for comparison type families
        // lt <number, t> implies t is number
        // lt <t, number> implies t is number
        if (lhsFree && isNumber(rhsTy))
            emplaceType<BoundType>(asMutable(lhsTy), ctx->builtins->numberType);
        else if (rhsFree && isNumber(lhsTy))
            emplaceType<BoundType>(asMutable(rhsTy), ctx->builtins->numberType);
        else if (lhsFree && ctx->normalizer->isInhabited(rhsTy) != NormalizationResult::False)
        {
            auto c1 = ctx->pushConstraint(EqualityConstraint{lhsTy, rhsTy});
            const_cast<Constraint*>(ctx->constraint)->dependencies.emplace_back(c1);
        }
        else if (rhsFree && ctx->normalizer->isInhabited(lhsTy) != NormalizationResult::False)
        {
            auto c1 = ctx->pushConstraint(EqualityConstraint{rhsTy, lhsTy});
            const_cast<Constraint*>(ctx->constraint)->dependencies.emplace_back(c1);
        }
    }

    // The above might have caused the operand types to be rebound, we need to follow them again
    lhsTy = follow(lhsTy);
    rhsTy = follow(rhsTy);

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> lhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, lhsTy);
        std::optional<TypeId> rhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, rhsTy);

        if (!lhsMaybeGeneralized)
            return {std::nullopt, false, {lhsTy}, {}};
        else if (!rhsMaybeGeneralized)
            return {std::nullopt, false, {rhsTy}, {}};

        lhsTy = *lhsMaybeGeneralized;
        rhsTy = *rhsMaybeGeneralized;
    }

    // check to see if both operand types are resolved enough, and wait to reduce if not

    std::shared_ptr<const NormalizedType> normLhsTy = ctx->normalizer->normalize(lhsTy);
    std::shared_ptr<const NormalizedType> normRhsTy = ctx->normalizer->normalize(rhsTy);
    NormalizationResult lhsInhabited = ctx->normalizer->isInhabited(normLhsTy.get());
    NormalizationResult rhsInhabited = ctx->normalizer->isInhabited(normRhsTy.get());

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy || lhsInhabited == NormalizationResult::HitLimits || rhsInhabited == NormalizationResult::HitLimits)
        return {std::nullopt, false, {}, {}};

    // if one of the types is error suppressing, we can just go ahead and reduce.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->booleanType, false, {}, {}};

    // if we have an uninhabited type (e.g. `never`), we can never observe that the comparison didn't work.
    if (lhsInhabited == NormalizationResult::False || rhsInhabited == NormalizationResult::False)
        return {ctx->builtins->booleanType, false, {}, {}};

    // If both types are some strict subset of `string`, we can reduce now.
    if (normLhsTy->isSubtypeOfString() && normRhsTy->isSubtypeOfString())
        return {ctx->builtins->booleanType, false, {}, {}};

    // If both types are exactly `number`, we can reduce now.
    if (normLhsTy->isExactlyNumber() && normRhsTy->isExactlyNumber())
        return {ctx->builtins->booleanType, false, {}, {}};

    if (auto result = tryDistributeTypeFamilyApp(comparisonFamilyFn, instance, typeParams, packParams, ctx, metamethod))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, lhsTy, metamethod, Location{});
    if (!mmType)
        mmType = findMetatableEntry(ctx->builtins, dummy, rhsTy, metamethod, Location{});

    if (!mmType)
        return {std::nullopt, true, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, false, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, true, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, true, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorRecoveryType(), false, {}, {}};

    TypePackId inferredArgPack = ctx->arena->addTypePack({lhsTy, rhsTy});
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (!u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, true, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->normalizer, ctx->ice, ctx->scope};
    if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, true, {}, {}};

    return {ctx->builtins->booleanType, false, {}, {}};
}

TypeFamilyReductionResult<TypeId> ltFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("lt type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return comparisonFamilyFn(instance, typeParams, packParams, ctx, "__lt");
}

TypeFamilyReductionResult<TypeId> leFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("le type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return comparisonFamilyFn(instance, typeParams, packParams, ctx, "__le");
}

TypeFamilyReductionResult<TypeId> eqFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("eq type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> lhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, lhsTy);
        std::optional<TypeId> rhsMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, rhsTy);

        if (!lhsMaybeGeneralized)
            return {std::nullopt, false, {lhsTy}, {}};
        else if (!rhsMaybeGeneralized)
            return {std::nullopt, false, {rhsTy}, {}};

        lhsTy = *lhsMaybeGeneralized;
        rhsTy = *rhsMaybeGeneralized;
    }

    std::shared_ptr<const NormalizedType> normLhsTy = ctx->normalizer->normalize(lhsTy);
    std::shared_ptr<const NormalizedType> normRhsTy = ctx->normalizer->normalize(rhsTy);
    NormalizationResult lhsInhabited = ctx->normalizer->isInhabited(normLhsTy.get());
    NormalizationResult rhsInhabited = ctx->normalizer->isInhabited(normRhsTy.get());

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy || lhsInhabited == NormalizationResult::HitLimits || rhsInhabited == NormalizationResult::HitLimits)
        return {std::nullopt, false, {}, {}};

    // if one of the types is error suppressing, we can just go ahead and reduce.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->booleanType, false, {}, {}};

    // if we have a `never`, we can never observe that the comparison didn't work.
    if (lhsInhabited == NormalizationResult::False || rhsInhabited == NormalizationResult::False)
        return {ctx->builtins->booleanType, false, {}, {}};

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, lhsTy, "__eq", Location{});
    if (!mmType)
        mmType = findMetatableEntry(ctx->builtins, dummy, rhsTy, "__eq", Location{});

    // if neither type has a metatable entry for `__eq`, then we'll check for inhabitance of the intersection!
    NormalizationResult intersectInhabited = ctx->normalizer->isIntersectionInhabited(lhsTy, rhsTy);
    if (!mmType)
    {
        if (intersectInhabited == NormalizationResult::True)
            return {ctx->builtins->booleanType, false, {}, {}}; // if it's inhabited, everything is okay!

        // we might be in a case where we still want to accept the comparison...
        if (intersectInhabited == NormalizationResult::False)
        {
            // if they're both subtypes of `string` but have no common intersection, the comparison is allowed but always `false`.
            if (normLhsTy->isSubtypeOfString() && normRhsTy->isSubtypeOfString())
                return {ctx->builtins->falseType, false, {}, {}};

            // if they're both subtypes of `boolean` but have no common intersection, the comparison is allowed but always `false`.
            if (normLhsTy->isSubtypeOfBooleans() && normRhsTy->isSubtypeOfBooleans())
                return {ctx->builtins->falseType, false, {}, {}};
        }

        return {std::nullopt, true, {}, {}}; // if it's not, then this family is irreducible!
    }

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, false, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, true, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, true, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorRecoveryType(), false, {}, {}};

    TypePackId inferredArgPack = ctx->arena->addTypePack({lhsTy, rhsTy});
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (!u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, true, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->normalizer, ctx->ice, ctx->scope};
    if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, true, {}, {}};

    return {ctx->builtins->booleanType, false, {}, {}};
}

// Collect types that prevent us from reducing a particular refinement.
struct FindRefinementBlockers : TypeOnceVisitor
{
    DenseHashSet<TypeId> found{nullptr};
    bool visit(TypeId ty, const BlockedType&) override
    {
        found.insert(ty);
        return false;
    }

    bool visit(TypeId ty, const PendingExpansionType&) override
    {
        found.insert(ty);
        return false;
    }

    bool visit(TypeId ty, const ClassType&) override
    {
        return false;
    }
};


TypeFamilyReductionResult<TypeId> refineFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("refine type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId targetTy = follow(typeParams.at(0));
    TypeId discriminantTy = follow(typeParams.at(1));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(targetTy, ctx->solver))
        return {std::nullopt, false, {targetTy}, {}};
    else if (isPending(discriminantTy, ctx->solver))
        return {std::nullopt, false, {discriminantTy}, {}};

    // if either type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> targetMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, targetTy);
        std::optional<TypeId> discriminantMaybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, discriminantTy);

        if (!targetMaybeGeneralized)
            return {std::nullopt, false, {targetTy}, {}};
        else if (!discriminantMaybeGeneralized)
            return {std::nullopt, false, {discriminantTy}, {}};

        targetTy = *targetMaybeGeneralized;
        discriminantTy = *discriminantMaybeGeneralized;
    }

    // we need a more complex check for blocking on the discriminant in particular
    FindRefinementBlockers frb;
    frb.traverse(discriminantTy);

    if (!frb.found.empty())
        return {std::nullopt, false, {frb.found.begin(), frb.found.end()}, {}};

    /* HACK: Refinements sometimes produce a type T & ~any under the assumption
     * that ~any is the same as any.  This is so so weird, but refinements needs
     * some way to say "I may refine this, but I'm not sure."
     *
     * It does this by refining on a blocked type and deferring the decision
     * until it is unblocked.
     *
     * Refinements also get negated, so we wind up with types like T & ~*blocked*
     *
     * We need to treat T & ~any as T in this case.
     */

    if (auto nt = get<NegationType>(discriminantTy))
        if (get<AnyType>(follow(nt->ty)))
            return {targetTy, false, {}, {}};

    // If the target type is a table, then simplification already implements the logic to deal with refinements properly since the
    // type of the discriminant is guaranteed to only ever be an (arbitrarily-nested) table of a single property type.
    if (get<TableType>(targetTy))
    {
        SimplifyResult result = simplifyIntersection(ctx->builtins, ctx->arena, targetTy, discriminantTy);
        if (!result.blockedTypes.empty())
            return {std::nullopt, false, {result.blockedTypes.begin(), result.blockedTypes.end()}, {}};

        return {result.result, false, {}, {}};
    }

    // In the general case, we'll still use normalization though.
    TypeId intersection = ctx->arena->addType(IntersectionType{{targetTy, discriminantTy}});
    std::shared_ptr<const NormalizedType> normIntersection = ctx->normalizer->normalize(intersection);
    std::shared_ptr<const NormalizedType> normType = ctx->normalizer->normalize(targetTy);

    // if the intersection failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normIntersection || !normType)
        return {std::nullopt, false, {}, {}};

    TypeId resultTy = ctx->normalizer->typeFromNormal(*normIntersection);

    // include the error type if the target type is error-suppressing and the intersection we computed is not
    if (normType->shouldSuppressErrors() && !normIntersection->shouldSuppressErrors())
        resultTy = ctx->arena->addType(UnionType{{resultTy, ctx->builtins->errorType}});

    return {resultTy, false, {}, {}};
}

TypeFamilyReductionResult<TypeId> singletonFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("singleton type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId type = follow(typeParams.at(0));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(type, ctx->solver))
        return {std::nullopt, false, {type}, {}};

    // if the type is free but has only one remaining reference, we can generalize it to its upper bound here.
    if (ctx->solver)
    {
        std::optional<TypeId> maybeGeneralized = ctx->solver->generalizeFreeType(ctx->scope, type);
        if (!maybeGeneralized)
            return {std::nullopt, false, {type}, {}};
        type = *maybeGeneralized;
    }

    TypeId followed = type;
    // we want to follow through a negation here as well.
    if (auto negation = get<NegationType>(followed))
        followed = follow(negation->ty);

    // if we have a singleton type or `nil`, which is its own singleton type...
    if (get<SingletonType>(followed) || isNil(followed))
        return {type, false, {}, {}};

    // otherwise, we'll return the top type, `unknown`.
    return {ctx->builtins->unknownType, false, {}, {}};
}

TypeFamilyReductionResult<TypeId> unionFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (!packParams.empty())
    {
        ctx->ice->ice("union type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    // if we only have one parameter, there's nothing to do.
    if (typeParams.size() == 1)
        return {follow(typeParams[0]), false, {}, {}};

    // we need to follow all of the type parameters.
    std::vector<TypeId> types;
    types.reserve(typeParams.size());
    for (auto ty : typeParams)
        types.emplace_back(follow(ty));

    // unfortunately, we need this short-circuit: if all but one type is `never`, we will return that one type.
    // this also will early return if _everything_ is `never`, since we already have to check that.
    std::optional<TypeId> lastType = std::nullopt;
    for (auto ty : types)
    {
        // if we have a previous type and it's not `never` and the current type isn't `never`...
        if (lastType && !get<NeverType>(lastType) && !get<NeverType>(ty))
        {
            // we know we are not taking the short-circuited path.
            lastType = std::nullopt;
            break;
        }

        if (get<NeverType>(ty))
            continue;
        lastType = ty;
    }

    // if we still have a `lastType` at the end, we're taking the short-circuit and reducing early.
    if (lastType)
        return {lastType, false, {}, {}};

    // check to see if the operand types are resolved enough, and wait to reduce if not
    for (auto ty : types)
        if (isPending(ty, ctx->solver))
            return {std::nullopt, false, {ty}, {}};

    // fold over the types with `simplifyUnion`
    TypeId resultTy = ctx->builtins->neverType;
    for (auto ty : types)
    {
        SimplifyResult result = simplifyUnion(ctx->builtins, ctx->arena, resultTy, ty);
        if (!result.blockedTypes.empty())
            return {std::nullopt, false, {result.blockedTypes.begin(), result.blockedTypes.end()}, {}};

        resultTy = result.result;
    }

    return {resultTy, false, {}, {}};
}


TypeFamilyReductionResult<TypeId> intersectFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (!packParams.empty())
    {
        ctx->ice->ice("intersect type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    // if we only have one parameter, there's nothing to do.
    if (typeParams.size() == 1)
        return {follow(typeParams[0]), false, {}, {}};

    // we need to follow all of the type parameters.
    std::vector<TypeId> types;
    types.reserve(typeParams.size());
    for (auto ty : typeParams)
        types.emplace_back(follow(ty));

    // check to see if the operand types are resolved enough, and wait to reduce if not
    // if any of them are `never`, the intersection will always be `never`, so we can reduce directly.
    for (auto ty : types)
    {
        if (isPending(ty, ctx->solver))
            return {std::nullopt, false, {ty}, {}};
        else if (get<NeverType>(ty))
            return {ctx->builtins->neverType, false, {}, {}};
    }

    // fold over the types with `simplifyIntersection`
    TypeId resultTy = ctx->builtins->unknownType;
    for (auto ty : types)
    {
        SimplifyResult result = simplifyIntersection(ctx->builtins, ctx->arena, resultTy, ty);
        if (!result.blockedTypes.empty())
            return {std::nullopt, false, {result.blockedTypes.begin(), result.blockedTypes.end()}, {}};

        resultTy = result.result;
    }

    // if the intersection simplifies to `never`, this gives us bad autocomplete.
    // we'll just produce the intersection plainly instead, but this might be revisitable
    // if we ever give `never` some kind of "explanation" trail.
    if (get<NeverType>(resultTy))
    {
        TypeId intersection = ctx->arena->addType(IntersectionType{typeParams});
        return {intersection, false, {}, {}};
    }

    return {resultTy, false, {}, {}};
}

// computes the keys of `ty` into `result`
// `isRaw` parameter indicates whether or not we should follow __index metamethods
// returns `false` if `result` should be ignored because the answer is "all strings"
bool computeKeysOf(TypeId ty, Set<std::string>& result, DenseHashSet<TypeId>& seen, bool isRaw, NotNull<TypeFamilyContext> ctx)
{
    // if the type is the top table type, the answer is just "all strings"
    if (get<PrimitiveType>(ty))
        return false;

    // if we've already seen this type, we can do nothing
    if (seen.contains(ty))
        return true;
    seen.insert(ty);

    // if we have a particular table type, we can insert the keys
    if (auto tableTy = get<TableType>(ty))
    {
        if (tableTy->indexer)
        {
            // if we have a string indexer, the answer is, again, "all strings"
            if (isString(tableTy->indexer->indexType))
                return false;
        }

        for (auto [key, _] : tableTy->props)
            result.insert(key);
        return true;
    }

    // otherwise, we have a metatable to deal with
    if (auto metatableTy = get<MetatableType>(ty))
    {
        bool res = true;

        if (!isRaw)
        {
            // findMetatableEntry demands the ability to emit errors, so we must give it
            // the necessary state to do that, even if we intend to just eat the errors.
            ErrorVec dummy;

            std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, ty, "__index", Location{});
            if (mmType)
                res = res && computeKeysOf(*mmType, result, seen, isRaw, ctx);
        }

        res = res && computeKeysOf(metatableTy->table, result, seen, isRaw, ctx);

        return res;
    }

    // this should not be reachable since the type should be a valid tables part from normalization.
    LUAU_ASSERT(false);
    return false;
}

TypeFamilyReductionResult<TypeId> keyofFamilyImpl(
    const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx, bool isRaw)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("keyof type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId operandTy = follow(typeParams.at(0));

    std::shared_ptr<const NormalizedType> normTy = ctx->normalizer->normalize(operandTy);

    // if the operand failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normTy)
        return {std::nullopt, false, {}, {}};

    // if we don't have either just tables or just classes, we've got nothing to get keys of (at least until a future version perhaps adds classes
    // as well)
    if (normTy->hasTables() == normTy->hasClasses())
        return {std::nullopt, true, {}, {}};

    // this is sort of atrocious, but we're trying to reject any type that has not normalized to a table or a union of tables.
    if (normTy->hasTops() || normTy->hasBooleans() || normTy->hasErrors() || normTy->hasNils() || normTy->hasNumbers() || normTy->hasStrings() ||
        normTy->hasThreads() || normTy->hasBuffers() || normTy->hasFunctions() || normTy->hasTyvars())
        return {std::nullopt, true, {}, {}};

    // we're going to collect the keys in here
    Set<std::string> keys{{}};

    // computing the keys for classes
    if (normTy->hasClasses())
    {
        LUAU_ASSERT(!normTy->hasTables());

        auto classesIter = normTy->classes.ordering.begin();
        auto classesIterEnd = normTy->classes.ordering.end();
        LUAU_ASSERT(classesIter != classesIterEnd); // should be guaranteed by the `hasClasses` check

        auto classTy = get<ClassType>(*classesIter);
        if (!classTy)
        {
            LUAU_ASSERT(false); // this should not be possible according to normalization's spec
            return {std::nullopt, true, {}, {}};
        }

        for (auto [key, _] : classTy->props)
            keys.insert(key);

        // we need to look at each class to remove any keys that are not common amongst them all
        while (++classesIter != classesIterEnd)
        {
            auto classTy = get<ClassType>(*classesIter);
            if (!classTy)
            {
                LUAU_ASSERT(false); // this should not be possible according to normalization's spec
                return {std::nullopt, true, {}, {}};
            }

            for (auto key : keys)
            {
                // remove any keys that are not present in each class
                if (classTy->props.find(key) == classTy->props.end())
                    keys.erase(key);
            }
        }
    }

    // computing the keys for tables
    if (normTy->hasTables())
    {
        LUAU_ASSERT(!normTy->hasClasses());

        // seen set for key computation for tables
        DenseHashSet<TypeId> seen{{}};

        auto tablesIter = normTy->tables.begin();
        LUAU_ASSERT(tablesIter != normTy->tables.end()); // should be guaranteed by the `hasTables` check earlier

        // collect all the properties from the first table type
        if (!computeKeysOf(*tablesIter, keys, seen, isRaw, ctx))
            return {ctx->builtins->stringType, false, {}, {}}; // if it failed, we have the top table type!

        // we need to look at each tables to remove any keys that are not common amongst them all
        while (++tablesIter != normTy->tables.end())
        {
            seen.clear(); // we'll reuse the same seen set

            Set<std::string> localKeys{{}};

            // we can skip to the next table if this one is the top table type
            if (!computeKeysOf(*tablesIter, localKeys, seen, isRaw, ctx))
                continue;

            for (auto key : keys)
            {
                // remove any keys that are not present in each table
                if (!localKeys.contains(key))
                    keys.erase(key);
            }
        }
    }

    // if the set of keys is empty, `keyof<T>` is `never`
    if (keys.empty())
        return {ctx->builtins->neverType, false, {}, {}};

    // everything is validated, we need only construct our big union of singletons now!
    std::vector<TypeId> singletons;
    singletons.reserve(keys.size());

    for (std::string key : keys)
        singletons.push_back(ctx->arena->addType(SingletonType{StringSingleton{key}}));

    return {ctx->arena->addType(UnionType{singletons}), false, {}, {}};
}

TypeFamilyReductionResult<TypeId> keyofFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("keyof type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return keyofFamilyImpl(typeParams, packParams, ctx, /* isRaw */ false);
}

TypeFamilyReductionResult<TypeId> rawkeyofFamilyFn(TypeId instance, const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("rawkeyof type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return keyofFamilyImpl(typeParams, packParams, ctx, /* isRaw */ true);
}

/* Searches through table's or class's props/indexer to find the property of `ty`
   If found, appends that property to `result` and returns true
   Else, returns false */
bool searchPropsAndIndexer(
    TypeId ty, TableType::Props tblProps, std::optional<TableIndexer> tblIndexer, DenseHashSet<TypeId>& result, NotNull<TypeFamilyContext> ctx)
{
    ty = follow(ty);

    // index into tbl's properties
    if (auto stringSingleton = get<StringSingleton>(get<SingletonType>(ty)))
    {
        if (tblProps.find(stringSingleton->value) != tblProps.end())
        {
            TypeId propTy = follow(tblProps.at(stringSingleton->value).type());

            // property is a union type -> we need to extend our reduction type
            if (auto propUnionTy = get<UnionType>(propTy))
            {
                for (TypeId option : propUnionTy->options)
                    result.insert(option);
            }
            else // property is a singular type or intersection type -> we can simply append
                result.insert(propTy);

            return true;
        }
    }

    // index into tbl's indexer
    if (tblIndexer)
    {
        if (isSubtype(ty, tblIndexer->indexType, ctx->scope, ctx->builtins, *ctx->ice))
        {
            TypeId idxResultTy = follow(tblIndexer->indexResultType);

            // indexResultType is a union type -> we need to extend our reduction type
            if (auto idxResUnionTy = get<UnionType>(idxResultTy))
            {
                for (TypeId option : idxResUnionTy->options)
                    result.insert(option);
            }
            else // indexResultType is a singular type or intersection type -> we can simply append
                result.insert(idxResultTy);

            return true;
        }
    }

    return false;
}

/* Handles recursion / metamethods of tables/classes
   `isRaw` parameter indicates whether or not we should follow __index metamethods
   returns false if property of `ty` could not be found */
bool tblIndexInto(TypeId indexer, TypeId indexee, DenseHashSet<TypeId>& result, NotNull<TypeFamilyContext> ctx, bool isRaw)
{
    indexer = follow(indexer);
    indexee = follow(indexee);

    // we have a table type to try indexing
    if (auto tableTy = get<TableType>(indexee))
    {
        return searchPropsAndIndexer(indexer, tableTy->props, tableTy->indexer, result, ctx);
    }

    // we have a metatable type to try indexing
    if (auto metatableTy = get<MetatableType>(indexee))
    {
        if (auto tableTy = get<TableType>(metatableTy->table))
        {

            // try finding all properties within the current scope of the table
            if (searchPropsAndIndexer(indexer, tableTy->props, tableTy->indexer, result, ctx))
                return true;
        }

        // if the code reached here, it means we weren't able to find all properties -> look into __index metamethod
        if (!isRaw)
        {
            // findMetatableEntry demands the ability to emit errors, so we must give it
            // the necessary state to do that, even if we intend to just eat the errors.
            ErrorVec dummy;
            std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, indexee, "__index", Location{});
            if (mmType)
                return tblIndexInto(indexer, *mmType, result, ctx, isRaw);
        }
    }

    return false;
}

/* Vocabulary note: indexee refers to the type that contains the properties,
                    indexer refers to the type that is used to access indexee
   Example:         index<Person, "name"> => `Person` is the indexee and `"name"` is the indexer */
TypeFamilyReductionResult<TypeId> indexFamilyFn(
    TypeId instance, const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("index type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId indexeeTy = follow(typeParams.at(0));
    std::shared_ptr<const NormalizedType> indexeeNormTy = ctx->normalizer->normalize(indexeeTy);

    // if the indexee failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!indexeeNormTy)
        return {std::nullopt, false, {}, {}};

    // if we don't have either just tables or just classes, we've got nothing to index into
    if (indexeeNormTy->hasTables() == indexeeNormTy->hasClasses())
        return {std::nullopt, true, {}, {}};

    // we're trying to reject any type that has not normalized to a table/class or a union of tables/classes.
    if (indexeeNormTy->hasTops() || indexeeNormTy->hasBooleans() || indexeeNormTy->hasErrors() || indexeeNormTy->hasNils() ||
        indexeeNormTy->hasNumbers() || indexeeNormTy->hasStrings() || indexeeNormTy->hasThreads() || indexeeNormTy->hasBuffers() ||
        indexeeNormTy->hasFunctions() || indexeeNormTy->hasTyvars())
        return {std::nullopt, true, {}, {}};

    TypeId indexerTy = follow(typeParams.at(1));
    std::shared_ptr<const NormalizedType> indexerNormTy = ctx->normalizer->normalize(indexerTy);

    // if the indexer failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!indexerNormTy)
        return {std::nullopt, false, {}, {}};

    // we're trying to reject any type that is not a string singleton or primitive (string, number, boolean, thread, nil, function, table, or buffer)
    if (indexerNormTy->hasTops() || indexerNormTy->hasErrors())
        return {std::nullopt, true, {}, {}};

    // indexer can be a union —> break them down into a vector
    const std::vector<TypeId>* typesToFind;
    const std::vector<TypeId> singleType{indexerTy};
    if (auto unionTy = get<UnionType>(indexerTy))
        typesToFind = &unionTy->options;
    else
        typesToFind = &singleType;

    DenseHashSet<TypeId> properties{{}}; // vector of types that will be returned
    bool isRaw = false;

    if (indexeeNormTy->hasClasses())
    {
        LUAU_ASSERT(!indexeeNormTy->hasTables());

        // at least one class is guaranteed to be in the iterator by .hasClasses()
        for (auto classesIter = indexeeNormTy->classes.ordering.begin(); classesIter != indexeeNormTy->classes.ordering.end(); ++classesIter)
        {
            auto classTy = get<ClassType>(*classesIter);
            if (!classTy)
            {
                LUAU_ASSERT(false); // this should not be possible according to normalization's spec
                return {std::nullopt, true, {}, {}};
            }

            for (TypeId ty : *typesToFind)
            {
                // Search for all instances of indexer in class->props and class->indexer using `indexInto`
                if (searchPropsAndIndexer(ty, classTy->props, classTy->indexer, properties, ctx))
                    continue; // Indexer was found in this class, so we can move on to the next

                // If code reaches here,that means the property not found -> check in the metatable's __index

                // findMetatableEntry demands the ability to emit errors, so we must give it
                // the necessary state to do that, even if we intend to just eat the errors.
                ErrorVec dummy;
                std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, *classesIter, "__index", Location{});
                if (!mmType) // if a metatable does not exist, there is no where else to look
                    return {std::nullopt, true, {}, {}};

                if (!tblIndexInto(ty, *mmType, properties, ctx, isRaw)) // if indexer is not in the metatable, we fail to reduce
                    return {std::nullopt, true, {}, {}};
            }
        }
    }

    if (indexeeNormTy->hasTables())
    {
        LUAU_ASSERT(!indexeeNormTy->hasClasses());

        // at least one table is guaranteed to be in the iterator by .hasTables()
        for (auto tablesIter = indexeeNormTy->tables.begin(); tablesIter != indexeeNormTy->tables.end(); ++tablesIter)
        {
            for (TypeId ty : *typesToFind)
                if (!tblIndexInto(ty, *tablesIter, properties, ctx, isRaw))
                    return {std::nullopt, true, {}, {}};
        }
    }

    // Call `follow()` on each element to resolve all Bound types before returning
    std::transform(properties.begin(), properties.end(), properties.begin(), [](TypeId ty) {
        return follow(ty);
    });

    // If the type being reduced to is a single type, no need to union
    if (properties.size() == 1)
        return {*properties.begin(), false, {}, {}};

    return {ctx->arena->addType(UnionType{std::vector<TypeId>(properties.begin(), properties.end())}), false, {}, {}};
}

BuiltinTypeFamilies::BuiltinTypeFamilies()
    : notFamily{"not", notFamilyFn}
    , lenFamily{"len", lenFamilyFn}
    , unmFamily{"unm", unmFamilyFn}
    , addFamily{"add", addFamilyFn}
    , subFamily{"sub", subFamilyFn}
    , mulFamily{"mul", mulFamilyFn}
    , divFamily{"div", divFamilyFn}
    , idivFamily{"idiv", idivFamilyFn}
    , powFamily{"pow", powFamilyFn}
    , modFamily{"mod", modFamilyFn}
    , concatFamily{"concat", concatFamilyFn}
    , andFamily{"and", andFamilyFn}
    , orFamily{"or", orFamilyFn}
    , ltFamily{"lt", ltFamilyFn}
    , leFamily{"le", leFamilyFn}
    , eqFamily{"eq", eqFamilyFn}
    , refineFamily{"refine", refineFamilyFn}
    , singletonFamily{"singleton", singletonFamilyFn}
    , unionFamily{"union", unionFamilyFn}
    , intersectFamily{"intersect", intersectFamilyFn}
    , keyofFamily{"keyof", keyofFamilyFn}
    , rawkeyofFamily{"rawkeyof", rawkeyofFamilyFn}
    , indexFamily{"index", indexFamilyFn}
{
}

void BuiltinTypeFamilies::addToScope(NotNull<TypeArena> arena, NotNull<Scope> scope) const
{
    // make a type function for a one-argument type family
    auto mkUnaryTypeFamily = [&](const TypeFamily* family) {
        TypeId t = arena->addType(GenericType{"T"});
        GenericTypeDefinition genericT{t};

        return TypeFun{{genericT}, arena->addType(TypeFamilyInstanceType{NotNull{family}, {t}, {}})};
    };

    // make a type function for a two-argument type family
    auto mkBinaryTypeFamily = [&](const TypeFamily* family) {
        TypeId t = arena->addType(GenericType{"T"});
        TypeId u = arena->addType(GenericType{"U"});
        GenericTypeDefinition genericT{t};
        GenericTypeDefinition genericU{u, {t}};

        return TypeFun{{genericT, genericU}, arena->addType(TypeFamilyInstanceType{NotNull{family}, {t, u}, {}})};
    };

    scope->exportedTypeBindings[lenFamily.name] = mkUnaryTypeFamily(&lenFamily);
    scope->exportedTypeBindings[unmFamily.name] = mkUnaryTypeFamily(&unmFamily);

    scope->exportedTypeBindings[addFamily.name] = mkBinaryTypeFamily(&addFamily);
    scope->exportedTypeBindings[subFamily.name] = mkBinaryTypeFamily(&subFamily);
    scope->exportedTypeBindings[mulFamily.name] = mkBinaryTypeFamily(&mulFamily);
    scope->exportedTypeBindings[divFamily.name] = mkBinaryTypeFamily(&divFamily);
    scope->exportedTypeBindings[idivFamily.name] = mkBinaryTypeFamily(&idivFamily);
    scope->exportedTypeBindings[powFamily.name] = mkBinaryTypeFamily(&powFamily);
    scope->exportedTypeBindings[modFamily.name] = mkBinaryTypeFamily(&modFamily);
    scope->exportedTypeBindings[concatFamily.name] = mkBinaryTypeFamily(&concatFamily);

    scope->exportedTypeBindings[ltFamily.name] = mkBinaryTypeFamily(&ltFamily);
    scope->exportedTypeBindings[leFamily.name] = mkBinaryTypeFamily(&leFamily);
    scope->exportedTypeBindings[eqFamily.name] = mkBinaryTypeFamily(&eqFamily);

    scope->exportedTypeBindings[keyofFamily.name] = mkUnaryTypeFamily(&keyofFamily);
    scope->exportedTypeBindings[rawkeyofFamily.name] = mkUnaryTypeFamily(&rawkeyofFamily);

    scope->exportedTypeBindings[indexFamily.name] = mkBinaryTypeFamily(&indexFamily);
}

} // namespace Luau
