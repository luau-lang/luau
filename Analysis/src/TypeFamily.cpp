// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeFamily.h"

#include "Luau/ConstraintSolver.h"
#include "Luau/DenseHash.h"
#include "Luau/Instantiation.h"
#include "Luau/Normalize.h"
#include "Luau/Simplify.h"
#include "Luau/Substitution.h"
#include "Luau/Subtyping.h"
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"
#include "Luau/VecDeque.h"
#include "Luau/VisitType.h"

LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypeFamilyGraphReductionMaximumSteps, 1'000'000);

namespace Luau
{

struct InstanceCollector : TypeOnceVisitor
{
    VecDeque<TypeId> tys;
    VecDeque<TypePackId> tps;

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
    TypeFamilyContext ctx;

    VecDeque<TypeId> queuedTys;
    VecDeque<TypePackId> queuedTps;
    DenseHashSet<const void*> irreducible{nullptr};
    FamilyGraphReductionResult result;
    bool force = false;

    // Local to the constraint being reduced.
    Location location;

    FamilyReducer(VecDeque<TypeId> queuedTys, VecDeque<TypePackId> queuedTps, Location location, TypeFamilyContext ctx, bool force = false)
        : ctx(ctx)
        , queuedTys(std::move(queuedTys))
        , queuedTps(std::move(queuedTps))
        , force(force)
        , location(location)
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
        ty = follow(ty);

        if (is<TypeFamilyInstanceType>(ty))
        {
            if (!irreducible.contains(ty))
                return SkipTestResult::Defer;
            else
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
        TypeId subject = follow(queuedTys.front());
        queuedTys.pop_front();

        if (irreducible.contains(subject))
            return;

        if (const TypeFamilyInstanceType* tfit = get<TypeFamilyInstanceType>(subject))
        {
            if (!testParameters(subject, tfit))
                return;

            TypeFamilyReductionResult<TypeId> result = tfit->family->reducer(tfit->typeArguments, tfit->packArguments, NotNull{&ctx});
            handleFamilyReduction(subject, result);
        }
    }

    void stepPack()
    {
        TypePackId subject = follow(queuedTps.front());
        queuedTps.pop_front();

        if (irreducible.contains(subject))
            return;

        if (const TypeFamilyInstanceTypePack* tfit = get<TypeFamilyInstanceTypePack>(subject))
        {
            if (!testParameters(subject, tfit))
                return;

            TypeFamilyReductionResult<TypePackId> result = tfit->family->reducer(tfit->typeArguments, tfit->packArguments, NotNull{&ctx});
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

static FamilyGraphReductionResult reduceFamiliesInternal(
    VecDeque<TypeId> queuedTys, VecDeque<TypePackId> queuedTps, Location location, TypeFamilyContext ctx, bool force)
{
    FamilyReducer reducer{std::move(queuedTys), std::move(queuedTps), location, ctx, force};
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

    return reduceFamiliesInternal(std::move(collector.tys), std::move(collector.tps), location, ctx, force);
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

    return reduceFamiliesInternal(std::move(collector.tys), std::move(collector.tps), location, ctx, force);
}

bool isPending(TypeId ty, ConstraintSolver* solver)
{
    return is<BlockedType>(ty) || is<PendingExpansionType>(ty) || is<TypeFamilyInstanceType>(ty) || (solver && solver->hasUnresolvedConstraints(ty));
}

TypeFamilyReductionResult<TypeId> notFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("not type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId ty = follow(typeParams.at(0));

    if (isPending(ty, ctx->solver))
        return {std::nullopt, false, {ty}, {}};

    // `not` operates on anything and returns a `boolean` always.
    return {ctx->builtins->booleanType, false, {}, {}};
}

TypeFamilyReductionResult<TypeId> lenFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("len type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId operandTy = follow(typeParams.at(0));

    // check to see if the operand type is resolved enough, and wait to reduce if not
    // the use of `typeFromNormal` later necessitates blocking on local types.
    if (isPending(operandTy, ctx->solver) || get<LocalType>(operandTy))
        return {std::nullopt, false, {operandTy}, {}};

    const NormalizedType* normTy = ctx->normalizer->normalize(operandTy);

    // if the type failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normTy)
        return {std::nullopt, false, {}, {}};

    // if the operand type is error suppressing, we can immediately reduce to `number`.
    if (normTy->shouldSuppressErrors())
        return {ctx->builtins->numberType, false, {}, {}};

    // if we have a `never`, we can never observe that the operator didn't work.
    if (is<NeverType>(operandTy))
        return {ctx->builtins->neverType, false, {}, {}};

    // if we're checking the length of a string, that works!
    if (normTy->isSubtypeOfString())
        return {ctx->builtins->numberType, false, {}, {}};

    // we use the normalized operand here in case there was an intersection or union.
    TypeId normalizedOperand = ctx->normalizer->typeFromNormal(*normTy);
    if (normTy->hasTopTable() || get<TableType>(normalizedOperand))
        return {ctx->builtins->numberType, false, {}, {}};

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

TypeFamilyReductionResult<TypeId> unmFamilyFn(
    const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("unm type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId operandTy = follow(typeParams.at(0));

    // check to see if the operand type is resolved enough, and wait to reduce if not
    if (isPending(operandTy, ctx->solver))
        return {std::nullopt, false, {operandTy}, {}};

    const NormalizedType* normTy = ctx->normalizer->normalize(operandTy);

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

TypeFamilyReductionResult<TypeId> numericBinopFamilyFn(
    const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx, const std::string metamethod)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    const NormalizedType* normLhsTy = ctx->normalizer->normalize(lhsTy);
    const NormalizedType* normRhsTy = ctx->normalizer->normalize(rhsTy);

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy)
        return {std::nullopt, false, {}, {}};

    // if one of the types is error suppressing, we can reduce to `any` since we should suppress errors in the result of the usage.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->anyType, false, {}, {}};

    // if we have a `never`, we can never observe that the numeric operator didn't work.
    if (is<NeverType>(lhsTy) || is<NeverType>(rhsTy))
        return {ctx->builtins->neverType, false, {}, {}};

    // if we're adding two `number` types, the result is `number`.
    if (normLhsTy->isExactlyNumber() && normRhsTy->isExactlyNumber())
        return {ctx->builtins->numberType, false, {}, {}};

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, lhsTy, metamethod, Location{});
    bool reversed = false;
    if (!mmType)
    {
        mmType = findMetatableEntry(ctx->builtins, dummy, rhsTy, metamethod, Location{});
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

    if (std::optional<TypeId> ret = first(instantiatedMmFtv->retTypes))
        return {*ret, false, {}, {}};
    else
        return {std::nullopt, true, {}, {}};
}

TypeFamilyReductionResult<TypeId> addFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("add type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__add");
}

TypeFamilyReductionResult<TypeId> subFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("sub type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__sub");
}

TypeFamilyReductionResult<TypeId> mulFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("mul type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__mul");
}

TypeFamilyReductionResult<TypeId> divFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("div type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__div");
}

TypeFamilyReductionResult<TypeId> idivFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("integer div type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__idiv");
}

TypeFamilyReductionResult<TypeId> powFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("pow type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__pow");
}

TypeFamilyReductionResult<TypeId> modFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("modulo type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__mod");
}

TypeFamilyReductionResult<TypeId> concatFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("concat type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    const NormalizedType* normLhsTy = ctx->normalizer->normalize(lhsTy);
    const NormalizedType* normRhsTy = ctx->normalizer->normalize(rhsTy);

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

TypeFamilyReductionResult<TypeId> andFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("and type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

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

TypeFamilyReductionResult<TypeId> orFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("or type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

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

static TypeFamilyReductionResult<TypeId> comparisonFamilyFn(
    const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx, const std::string metamethod)
{

    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, false, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, false, {rhsTy}, {}};

    const NormalizedType* normLhsTy = ctx->normalizer->normalize(lhsTy);
    const NormalizedType* normRhsTy = ctx->normalizer->normalize(rhsTy);

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy)
        return {std::nullopt, false, {}, {}};

    // if one of the types is error suppressing, we can just go ahead and reduce.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->booleanType, false, {}, {}};

    // if we have a `never`, we can never observe that the comparison didn't work.
    if (is<NeverType>(lhsTy) || is<NeverType>(rhsTy))
        return {ctx->builtins->booleanType, false, {}, {}};

    // If both types are some strict subset of `string`, we can reduce now.
    if (normLhsTy->isSubtypeOfString() && normRhsTy->isSubtypeOfString())
        return {ctx->builtins->booleanType, false, {}, {}};

    // If both types are exactly `number`, we can reduce now.
    if (normLhsTy->isExactlyNumber() && normRhsTy->isExactlyNumber())
        return {ctx->builtins->booleanType, false, {}, {}};

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

TypeFamilyReductionResult<TypeId> ltFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("lt type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return comparisonFamilyFn(typeParams, packParams, ctx, "__lt");
}

TypeFamilyReductionResult<TypeId> leFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("le type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return comparisonFamilyFn(typeParams, packParams, ctx, "__le");
}

TypeFamilyReductionResult<TypeId> eqFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
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

    const NormalizedType* normLhsTy = ctx->normalizer->normalize(lhsTy);
    const NormalizedType* normRhsTy = ctx->normalizer->normalize(rhsTy);

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy)
        return {std::nullopt, false, {}, {}};

    // if one of the types is error suppressing, we can just go ahead and reduce.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->booleanType, false, {}, {}};

    // if we have a `never`, we can never observe that the comparison didn't work.
    if (is<NeverType>(lhsTy) || is<NeverType>(rhsTy))
        return {ctx->builtins->booleanType, false, {}, {}};

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, lhsTy, "__eq", Location{});
    if (!mmType)
        mmType = findMetatableEntry(ctx->builtins, dummy, rhsTy, "__eq", Location{});

    // if neither type has a metatable entry for `__eq`, then we'll check for inhabitance of the intersection!
    if (!mmType && ctx->normalizer->isIntersectionInhabited(lhsTy, rhsTy))
        return {ctx->builtins->booleanType, false, {}, {}}; // if it's inhabited, everything is okay!
    else if (!mmType)
        return {std::nullopt, true, {}, {}}; // if it's not, then this family is irreducible!

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

    bool visit(TypeId ty, const LocalType&) override
    {
        found.insert(ty);
        return false;
    }

    bool visit(TypeId ty, const ClassType&) override
    {
        return false;
    }
};


TypeFamilyReductionResult<TypeId> refineFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
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

    TypeId intersection = ctx->arena->addType(IntersectionType{{targetTy, discriminantTy}});
    const NormalizedType* normIntersection = ctx->normalizer->normalize(intersection);
    const NormalizedType* normType = ctx->normalizer->normalize(targetTy);

    // if the intersection failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normIntersection || !normType)
        return {std::nullopt, false, {}, {}};

    TypeId resultTy = ctx->normalizer->typeFromNormal(*normIntersection);

    // include the error type if the target type is error-suppressing and the intersection we computed is not
    if (normType->shouldSuppressErrors() && !normIntersection->shouldSuppressErrors())
        resultTy = ctx->arena->addType(UnionType{{resultTy, ctx->builtins->errorType}});

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

TypeFamilyReductionResult<TypeId> keyofFamilyImpl(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx, bool isRaw)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("keyof type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId operandTy = follow(typeParams.at(0));

    const NormalizedType* normTy = ctx->normalizer->normalize(operandTy);

    // if the operand failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normTy)
        return {std::nullopt, false, {}, {}};

    // if we don't have either just tables or just classes, we've got nothing to get keys of (at least until a future version perhaps adds classes as well)
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

TypeFamilyReductionResult<TypeId> keyofFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("keyof type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return keyofFamilyImpl(typeParams, packParams, ctx, /* isRaw */ false);
}

TypeFamilyReductionResult<TypeId> rawkeyofFamilyFn(const std::vector<TypeId>& typeParams, const std::vector<TypePackId>& packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("rawkeyof type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return keyofFamilyImpl(typeParams, packParams, ctx, /* isRaw */ true);
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
    , keyofFamily{"keyof", keyofFamilyFn}
    , rawkeyofFamily{"rawkeyof", rawkeyofFamilyFn}
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
        GenericTypeDefinition genericU{u};

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
}

} // namespace Luau
