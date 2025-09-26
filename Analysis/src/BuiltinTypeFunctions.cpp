// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/BuiltinTypeFunctions.h"

#include "Luau/Common.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/Instantiation.h"
#include "Luau/OverloadResolution.h"
#include "Luau/Scope.h"
#include "Luau/Simplify.h"
#include "Luau/Subtyping.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeFunctionRuntimeBuilder.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"
#include "Luau/UserDefinedTypeFunction.h"
#include "Luau/VisitType.h"

LUAU_FASTFLAG(DebugLuauEqSatSimplification)
LUAU_DYNAMIC_FASTINT(LuauTypeFamilyApplicationCartesianProductLimit)
LUAU_DYNAMIC_FASTINTVARIABLE(LuauStepRefineRecursionLimit, 64)
LUAU_FASTFLAGVARIABLE(LuauRefineOccursCheckDirectRecursion)
LUAU_FASTFLAG(LuauReduceSetTypeStackPressure)

LUAU_FASTFLAGVARIABLE(LuauRefineNoRefineAlways)
LUAU_FASTFLAGVARIABLE(LuauRefineDistributesOverUnions)
LUAU_FASTFLAG(LuauEGFixGenericsList)
LUAU_FASTFLAG(LuauExplicitSkipBoundTypes)
LUAU_FASTFLAG(LuauRawGetHandlesNil)
LUAU_FASTFLAG(LuauNoMoreComparisonTypeFunctions)
LUAU_FASTFLAGVARIABLE(LuauBuiltinTypeFunctionsArentGlobal)
LUAU_FASTFLAG(LuauPassBindableGenericsByReference)

namespace Luau
{

namespace
{

template<typename F, typename... Args>
std::optional<TypeFunctionReductionResult<TypeId>> tryDistributeTypeFunctionApp(
    F f,
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx,
    Args&&... args
)
{
    // op (a | b) (c | d) ~ (op a (c | d)) | (op b (c | d)) ~ (op a c) | (op a d) | (op b c) | (op b d)
    Reduction reductionStatus = Reduction::MaybeOk;
    std::vector<TypeId> blockedTypes;
    std::vector<TypeId> results;
    size_t cartesianProductSize = 1;

    const UnionType* firstUnion = nullptr;
    size_t unionIndex = 0;

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

        // TODO: We'd like to report that the type function application is too complex here.
        if (size_t(DFInt::LuauTypeFamilyApplicationCartesianProductLimit) <= cartesianProductSize)
            return {{std::nullopt, Reduction::Erroneous, {}, {}}};
    }

    if (!firstUnion)
    {
        // If we couldn't find any union type argument, we're not distributing.
        return std::nullopt;
    }

    for (TypeId option : firstUnion)
    {
        arguments[unionIndex] = option;

        TypeFunctionReductionResult<TypeId> result = f(instance, arguments, packParams, ctx, args...); // NOLINT
        blockedTypes.insert(blockedTypes.end(), result.blockedTypes.begin(), result.blockedTypes.end());
        if (result.reductionStatus != Reduction::MaybeOk)
            reductionStatus = result.reductionStatus;

        if (reductionStatus != Reduction::MaybeOk || !result.result)
            break;
        else
            results.push_back(*result.result);
    }

    if (reductionStatus != Reduction::MaybeOk || !blockedTypes.empty())
        return {{std::nullopt, reductionStatus, std::move(blockedTypes), {}}};

    if (!results.empty())
    {
        if (results.size() == 1)
            return {{results[0], Reduction::MaybeOk, {}, {}}};

        TypeId resultTy = ctx->arena->addType(
            TypeFunctionInstanceType{
                NotNull{
                    FFlag::LuauBuiltinTypeFunctionsArentGlobal ? &ctx->builtins->typeFunctions->unionFunc
                                                               : &builtinTypeFunctions_DEPRECATED().unionFunc
                },
                std::move(results),
                {},
            }
        );

        if (ctx->solver)
            ctx->pushConstraint(ReduceConstraint{resultTy});

        return {{resultTy, Reduction::MaybeOk, {}, {}}};
    }

    return std::nullopt;
}

} // namespace

TypeFunctionReductionResult<TypeId> notTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("not type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId ty = follow(typeParams.at(0));

    if (ty == instance)
        return {ctx->builtins->neverType, Reduction::MaybeOk, {}, {}};

    if (isPending(ty, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {ty}, {}};

    if (auto result = tryDistributeTypeFunctionApp(notTypeFunction, instance, typeParams, packParams, ctx))
        return *result;

    // `not` operates on anything and returns a `boolean` always.
    return {ctx->builtins->booleanType, Reduction::MaybeOk, {}, {}};
}

TypeFunctionReductionResult<TypeId> lenTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("len type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId operandTy = follow(typeParams.at(0));

    if (operandTy == instance)
        return {ctx->builtins->neverType, Reduction::MaybeOk, {}, {}};

    // check to see if the operand type is resolved enough, and wait to reduce if not
    // the use of `typeFromNormal` later necessitates blocking on local types.
    if (isPending(operandTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {operandTy}, {}};

    std::shared_ptr<const NormalizedType> normTy = ctx->normalizer->normalize(operandTy);
    NormalizationResult inhabited = ctx->normalizer->isInhabited(normTy.get());

    // if the type failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normTy || inhabited == NormalizationResult::HitLimits)
        return {std::nullopt, Reduction::MaybeOk, {}, {}};

    // if the operand type is error suppressing, we can immediately reduce to `number`.
    if (normTy->shouldSuppressErrors())
        return {ctx->builtins->numberType, Reduction::MaybeOk, {}, {}};

    // # always returns a number, even if its operand is never.
    // if we're checking the length of a string, that works!
    if (inhabited == NormalizationResult::False || normTy->isSubtypeOfString())
        return {ctx->builtins->numberType, Reduction::MaybeOk, {}, {}};

    // we use the normalized operand here in case there was an intersection or union.
    TypeId normalizedOperand = follow(ctx->normalizer->typeFromNormal(*normTy));
    if (normTy->hasTopTable() || get<TableType>(normalizedOperand))
        return {ctx->builtins->numberType, Reduction::MaybeOk, {}, {}};

    if (auto result = tryDistributeTypeFunctionApp(lenTypeFunction, instance, typeParams, packParams, ctx))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, operandTy, "__len", Location{});
    if (!mmType)
    {
        // If we have a metatable type with no __len, this means we still have a table with default length function
        if (get<MetatableType>(normalizedOperand))
            return {ctx->builtins->numberType, Reduction::MaybeOk, {}, {}};

        return {std::nullopt, Reduction::Erroneous, {}, {}};
    }

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorType, Reduction::MaybeOk, {}, {}};

    TypePackId inferredArgPack = ctx->arena->addTypePack({operandTy});
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (UnifyResult::Ok != u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, Reduction::Erroneous, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->simplifier, ctx->normalizer, ctx->typeFunctionRuntime, ctx->ice};
    if (FFlag::LuauPassBindableGenericsByReference)
    {
        if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope, {}).isSubtype)
            return {std::nullopt, Reduction::Erroneous, {}, {}};
    }
    else if (!subtyping.isSubtype_DEPRECATED(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    // `len` must return a `number`.
    return {ctx->builtins->numberType, Reduction::MaybeOk, {}, {}};
}

TypeFunctionReductionResult<TypeId> unmTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("unm type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId operandTy = follow(typeParams.at(0));

    if (operandTy == instance)
        return {ctx->builtins->neverType, Reduction::MaybeOk, {}, {}};

    // check to see if the operand type is resolved enough, and wait to reduce if not
    if (isPending(operandTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {operandTy}, {}};

    operandTy = follow(operandTy);

    std::shared_ptr<const NormalizedType> normTy = ctx->normalizer->normalize(operandTy);

    // if the operand failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normTy)
        return {std::nullopt, Reduction::MaybeOk, {}, {}};

    // if the operand is error suppressing, we can just go ahead and reduce.
    if (normTy->shouldSuppressErrors())
        return {operandTy, Reduction::MaybeOk, {}, {}};

    // if we have a `never`, we can never observe that the operation didn't work.
    if (is<NeverType>(operandTy))
        return {ctx->builtins->neverType, Reduction::MaybeOk, {}, {}};

    // If the type is exactly `number`, we can reduce now.
    if (normTy->isExactlyNumber())
        return {ctx->builtins->numberType, Reduction::MaybeOk, {}, {}};

    if (auto result = tryDistributeTypeFunctionApp(unmTypeFunction, instance, typeParams, packParams, ctx))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, operandTy, "__unm", Location{});
    if (!mmType)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorType, Reduction::MaybeOk, {}, {}};

    TypePackId inferredArgPack = ctx->arena->addTypePack({operandTy});
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (UnifyResult::Ok != u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, Reduction::Erroneous, {}, {}}; // occurs check failed

    if (!FFlag::LuauEGFixGenericsList)
    {
        Subtyping subtyping{ctx->builtins, ctx->arena, ctx->simplifier, ctx->normalizer, ctx->typeFunctionRuntime, ctx->ice};
        if (FFlag::LuauPassBindableGenericsByReference)
        {
            if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope, {}).isSubtype)
                return {std::nullopt, Reduction::Erroneous, {}, {}};
        }
        else if (!subtyping.isSubtype_DEPRECATED(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope)
                      .isSubtype) // TODO: is this the right variance?
            return {std::nullopt, Reduction::Erroneous, {}, {}};
    }

    if (std::optional<TypeId> ret = first(instantiatedMmFtv->retTypes))
        return {ret, Reduction::MaybeOk, {}, {}};
    else
        return {std::nullopt, Reduction::Erroneous, {}, {}};
}

TypeFunctionContext::TypeFunctionContext(NotNull<ConstraintSolver> cs, NotNull<Scope> scope, NotNull<const Constraint> constraint)
    : arena(cs->arena)
    , builtins(cs->builtinTypes)
    , scope(scope)
    , simplifier(cs->simplifier)
    , normalizer(cs->normalizer)
    , typeFunctionRuntime(cs->typeFunctionRuntime)
    , ice(NotNull{&cs->iceReporter})
    , limits(NotNull{&cs->limits})
    , solver(cs.get())
    , constraint(constraint.get())
{
}

NotNull<Constraint> TypeFunctionContext::pushConstraint(ConstraintV&& c) const
{
    LUAU_ASSERT(solver);
    NotNull<Constraint> newConstraint = solver->pushConstraint(scope, constraint ? constraint->location : Location{}, std::move(c));

    // Every constraint that is blocked on the current constraint must also be
    // blocked on this new one.
    if (constraint)
        solver->inheritBlocks(NotNull{constraint}, newConstraint);

    return newConstraint;
}

TypeFunctionReductionResult<TypeId> numericBinopTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx,
    const std::string metamethod
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // isPending of `lhsTy` or `rhsTy` would return true, even if it cycles. We want a different answer for that.
    if (lhsTy == instance || rhsTy == instance)
        return {ctx->builtins->neverType, Reduction::MaybeOk, {}, {}};

    // if we have a `never`, we can never observe that the math operator is unreachable.
    if (is<NeverType>(lhsTy) || is<NeverType>(rhsTy))
        return {ctx->builtins->neverType, Reduction::MaybeOk, {}, {}};

    const Location location = ctx->constraint ? ctx->constraint->location : Location{};

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {rhsTy}, {}};

    // TODO: Normalization needs to remove cyclic type functions from a `NormalizedType`.
    std::shared_ptr<const NormalizedType> normLhsTy = ctx->normalizer->normalize(lhsTy);
    std::shared_ptr<const NormalizedType> normRhsTy = ctx->normalizer->normalize(rhsTy);

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy)
        return {std::nullopt, Reduction::MaybeOk, {}, {}};

    // if one of the types is error suppressing, we can reduce to `any` since we should suppress errors in the result of the usage.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->anyType, Reduction::MaybeOk, {}, {}};

    // if we're adding two `number` types, the result is `number`.
    if (normLhsTy->isExactlyNumber() && normRhsTy->isExactlyNumber())
        return {ctx->builtins->numberType, Reduction::MaybeOk, {}, {}};

    if (auto result = tryDistributeTypeFunctionApp(numericBinopTypeFunction, instance, typeParams, packParams, ctx, metamethod))
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
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {*mmType}, {}};

    TypePackId argPack = ctx->arena->addTypePack({lhsTy, rhsTy});
    SolveResult solveResult;

    if (!reversed)
        solveResult = solveFunctionCall(
            ctx->arena,
            ctx->builtins,
            ctx->simplifier,
            ctx->normalizer,
            ctx->typeFunctionRuntime,
            ctx->ice,
            ctx->limits,
            ctx->scope,
            location,
            *mmType,
            argPack
        );
    else
    {
        TypePack* p = getMutable<TypePack>(argPack);
        std::swap(p->head.front(), p->head.back());
        solveResult = solveFunctionCall(
            ctx->arena,
            ctx->builtins,
            ctx->simplifier,
            ctx->normalizer,
            ctx->typeFunctionRuntime,
            ctx->ice,
            ctx->limits,
            ctx->scope,
            location,
            *mmType,
            argPack
        );
    }

    if (!solveResult.typePackId.has_value())
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    TypePack extracted = extendTypePack(*ctx->arena, ctx->builtins, *solveResult.typePackId, 1);
    if (extracted.head.empty())
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    return {extracted.head.front(), Reduction::MaybeOk, {}, {}};
}

TypeFunctionReductionResult<TypeId> addTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("add type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__add");
}

TypeFunctionReductionResult<TypeId> subTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("sub type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__sub");
}

TypeFunctionReductionResult<TypeId> mulTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("mul type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__mul");
}

TypeFunctionReductionResult<TypeId> divTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("div type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__div");
}

TypeFunctionReductionResult<TypeId> idivTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("integer div type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__idiv");
}

TypeFunctionReductionResult<TypeId> powTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("pow type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__pow");
}

TypeFunctionReductionResult<TypeId> modTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("modulo type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopTypeFunction(instance, typeParams, packParams, ctx, "__mod");
}

TypeFunctionReductionResult<TypeId> concatTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("concat type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // isPending of `lhsTy` or `rhsTy` would return true, even if it cycles. We want a different answer for that.
    if (lhsTy == instance || rhsTy == instance)
        return {ctx->builtins->neverType, Reduction::MaybeOk, {}, {}};

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {rhsTy}, {}};

    std::shared_ptr<const NormalizedType> normLhsTy = ctx->normalizer->normalize(lhsTy);
    std::shared_ptr<const NormalizedType> normRhsTy = ctx->normalizer->normalize(rhsTy);

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy)
        return {std::nullopt, Reduction::MaybeOk, {}, {}};

    // if one of the types is error suppressing, we can reduce to `any` since we should suppress errors in the result of the usage.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->anyType, Reduction::MaybeOk, {}, {}};

    // if we have a `never`, we can never observe that the operator didn't work.
    if (is<NeverType>(lhsTy) || is<NeverType>(rhsTy))
        return {ctx->builtins->neverType, Reduction::MaybeOk, {}, {}};

    // if we're concatenating two elements that are either strings or numbers, the result is `string`.
    if ((normLhsTy->isSubtypeOfString() || normLhsTy->isExactlyNumber()) && (normRhsTy->isSubtypeOfString() || normRhsTy->isExactlyNumber()))
        return {ctx->builtins->stringType, Reduction::MaybeOk, {}, {}};

    if (auto result = tryDistributeTypeFunctionApp(concatTypeFunction, instance, typeParams, packParams, ctx))
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
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorType, Reduction::MaybeOk, {}, {}};

    std::vector<TypeId> inferredArgs;
    if (!reversed)
        inferredArgs = {lhsTy, rhsTy};
    else
        inferredArgs = {rhsTy, lhsTy};

    TypePackId inferredArgPack = ctx->arena->addTypePack(std::move(inferredArgs));
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (UnifyResult::Ok != u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, Reduction::Erroneous, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->simplifier, ctx->normalizer, ctx->typeFunctionRuntime, ctx->ice};
    if (FFlag::LuauPassBindableGenericsByReference)
    {
        if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope, {}).isSubtype)
            return {std::nullopt, Reduction::Erroneous, {}, {}};
    }
    else if (!subtyping.isSubtype_DEPRECATED(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    return {ctx->builtins->stringType, Reduction::MaybeOk, {}, {}};
}

namespace
{
bool isBlockedOrUnsolvedType(TypeId ty)
{
    if (auto tfit = get<TypeFunctionInstanceType>(ty); tfit && tfit->state == TypeFunctionInstanceState::Unsolved)
        return true;
    return is<BlockedType, PendingExpansionType>(ty);
}
} // namespace

TypeFunctionReductionResult<TypeId> andTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("and type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // t1 = and<lhs, t1> ~> lhs
    if (follow(rhsTy) == instance && lhsTy != rhsTy)
        return {lhsTy, Reduction::MaybeOk, {}, {}};
    // t1 = and<t1, rhs> ~> rhs
    if (follow(lhsTy) == instance && lhsTy != rhsTy)
        return {rhsTy, Reduction::MaybeOk, {}, {}};

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {rhsTy}, {}};

    // And evalutes to a boolean if the LHS is falsey, and the RHS type if LHS is truthy.
    SimplifyResult filteredLhs = simplifyIntersection(ctx->builtins, ctx->arena, lhsTy, ctx->builtins->falsyType);
    SimplifyResult overallResult = simplifyUnion(ctx->builtins, ctx->arena, rhsTy, filteredLhs.result);
    std::vector<TypeId> blockedTypes{};
    for (auto ty : filteredLhs.blockedTypes)
        blockedTypes.push_back(ty);
    for (auto ty : overallResult.blockedTypes)
        blockedTypes.push_back(ty);
    return {overallResult.result, Reduction::MaybeOk, std::move(blockedTypes), {}};
}

TypeFunctionReductionResult<TypeId> orTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("or type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // t1 = or<lhs, t1> ~> lhs
    if (follow(rhsTy) == instance && lhsTy != rhsTy)
        return {lhsTy, Reduction::MaybeOk, {}, {}};
    // t1 = or<t1, rhs> ~> rhs
    if (follow(lhsTy) == instance && lhsTy != rhsTy)
        return {rhsTy, Reduction::MaybeOk, {}, {}};

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isBlockedOrUnsolvedType(lhsTy))
        return {std::nullopt, Reduction::MaybeOk, {lhsTy}, {}};
    else if (isBlockedOrUnsolvedType(rhsTy))
        return {std::nullopt, Reduction::MaybeOk, {rhsTy}, {}};

    // Or evalutes to the LHS type if the LHS is truthy, and the RHS type if LHS is falsy.
    SimplifyResult filteredLhs = simplifyIntersection(ctx->builtins, ctx->arena, lhsTy, ctx->builtins->truthyType);
    SimplifyResult overallResult = simplifyUnion(ctx->builtins, ctx->arena, rhsTy, filteredLhs.result);
    std::vector<TypeId> blockedTypes{};
    for (auto ty : filteredLhs.blockedTypes)
        blockedTypes.push_back(ty);
    for (auto ty : overallResult.blockedTypes)
        blockedTypes.push_back(ty);
    return {overallResult.result, Reduction::MaybeOk, std::move(blockedTypes), {}};
}

static TypeFunctionReductionResult<TypeId> comparisonTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx,
    const std::string metamethod
)
{

    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    if (lhsTy == instance || rhsTy == instance)
        return {ctx->builtins->neverType, Reduction::MaybeOk, {}, {}};

    if (isBlockedOrUnsolvedType(lhsTy))
        return {std::nullopt, Reduction::MaybeOk, {lhsTy}, {}};
    else if (isBlockedOrUnsolvedType(rhsTy))
        return {std::nullopt, Reduction::MaybeOk, {rhsTy}, {}};

    // Algebra Reduction Rules for comparison type functions
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
        // Implement injective type functions for comparison type functions
        // lt <number, t> implies t is number
        // lt <t, number> implies t is number
        if (lhsFree && isNumber(rhsTy))
            emplaceType<BoundType>(asMutable(lhsTy), ctx->builtins->numberType);
        else if (rhsFree && isNumber(lhsTy))
            emplaceType<BoundType>(asMutable(rhsTy), ctx->builtins->numberType);
    }

    // The above might have caused the operand types to be rebound, we need to follow them again
    lhsTy = follow(lhsTy);
    rhsTy = follow(rhsTy);

    // check to see if both operand types are resolved enough, and wait to reduce if not

    std::shared_ptr<const NormalizedType> normLhsTy = ctx->normalizer->normalize(lhsTy);
    std::shared_ptr<const NormalizedType> normRhsTy = ctx->normalizer->normalize(rhsTy);
    NormalizationResult lhsInhabited = ctx->normalizer->isInhabited(normLhsTy.get());
    NormalizationResult rhsInhabited = ctx->normalizer->isInhabited(normRhsTy.get());

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy || lhsInhabited == NormalizationResult::HitLimits || rhsInhabited == NormalizationResult::HitLimits)
        return {std::nullopt, Reduction::MaybeOk, {}, {}};

    // if one of the types is error suppressing, we can just go ahead and reduce.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->booleanType, Reduction::MaybeOk, {}, {}};

    // if we have an uninhabited type (e.g. `never`), we can never observe that the comparison didn't work.
    if (lhsInhabited == NormalizationResult::False || rhsInhabited == NormalizationResult::False)
        return {ctx->builtins->booleanType, Reduction::MaybeOk, {}, {}};

    // If both types are some strict subset of `string`, we can reduce now.
    if (normLhsTy->isSubtypeOfString() && normRhsTy->isSubtypeOfString())
        return {ctx->builtins->booleanType, Reduction::MaybeOk, {}, {}};

    // If both types are exactly `number`, we can reduce now.
    if (normLhsTy->isExactlyNumber() && normRhsTy->isExactlyNumber())
        return {ctx->builtins->booleanType, Reduction::MaybeOk, {}, {}};

    if (auto result = tryDistributeTypeFunctionApp(comparisonTypeFunction, instance, typeParams, packParams, ctx, metamethod))
        return *result;

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, lhsTy, metamethod, Location{});
    if (!mmType)
        mmType = findMetatableEntry(ctx->builtins, dummy, rhsTy, metamethod, Location{});

    if (!mmType)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorType, Reduction::MaybeOk, {}, {}};

    TypePackId inferredArgPack = ctx->arena->addTypePack({lhsTy, rhsTy});
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (UnifyResult::Ok != u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, Reduction::Erroneous, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->simplifier, ctx->normalizer, ctx->typeFunctionRuntime, ctx->ice};
    if (FFlag::LuauPassBindableGenericsByReference)
    {
        if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope, {}).isSubtype)
            return {std::nullopt, Reduction::Erroneous, {}, {}};
    }
    else if (!subtyping.isSubtype_DEPRECATED(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    return {ctx->builtins->booleanType, Reduction::MaybeOk, {}, {}};
}

TypeFunctionReductionResult<TypeId> ltTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("lt type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return comparisonTypeFunction(instance, typeParams, packParams, ctx, "__lt");
}

TypeFunctionReductionResult<TypeId> leTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("le type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return comparisonTypeFunction(instance, typeParams, packParams, ctx, "__le");
}

TypeFunctionReductionResult<TypeId> eqTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("eq type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(lhsTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {lhsTy}, {}};
    else if (isPending(rhsTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {rhsTy}, {}};

    std::shared_ptr<const NormalizedType> normLhsTy = ctx->normalizer->normalize(lhsTy);
    std::shared_ptr<const NormalizedType> normRhsTy = ctx->normalizer->normalize(rhsTy);
    NormalizationResult lhsInhabited = ctx->normalizer->isInhabited(normLhsTy.get());
    NormalizationResult rhsInhabited = ctx->normalizer->isInhabited(normRhsTy.get());

    // if either failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normLhsTy || !normRhsTy || lhsInhabited == NormalizationResult::HitLimits || rhsInhabited == NormalizationResult::HitLimits)
        return {std::nullopt, Reduction::MaybeOk, {}, {}};

    // if one of the types is error suppressing, we can just go ahead and reduce.
    if (normLhsTy->shouldSuppressErrors() || normRhsTy->shouldSuppressErrors())
        return {ctx->builtins->booleanType, Reduction::MaybeOk, {}, {}};

    // if we have a `never`, we can never observe that the comparison didn't work.
    if (lhsInhabited == NormalizationResult::False || rhsInhabited == NormalizationResult::False)
        return {ctx->builtins->booleanType, Reduction::MaybeOk, {}, {}};

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
            return {ctx->builtins->booleanType, Reduction::MaybeOk, {}, {}}; // if it's inhabited, everything is okay!

        // we might be in a case where we still want to accept the comparison...
        if (intersectInhabited == NormalizationResult::False)
        {
            // if they're both subtypes of `string` but have no common intersection, the comparison is allowed but always `false`.
            if (normLhsTy->isSubtypeOfString() && normRhsTy->isSubtypeOfString())
                return {ctx->builtins->falseType, Reduction::MaybeOk, {}, {}};

            // if they're both subtypes of `boolean` but have no common intersection, the comparison is allowed but always `false`.
            if (normLhsTy->isSubtypeOfBooleans() && normRhsTy->isSubtypeOfBooleans())
                return {ctx->builtins->falseType, Reduction::MaybeOk, {}, {}};
        }

        return {std::nullopt, Reduction::Erroneous, {}, {}}; // if it's not, then this type function is irreducible!
    }

    mmType = follow(*mmType);
    if (isPending(*mmType, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {*mmType}, {}};

    const FunctionType* mmFtv = get<FunctionType>(*mmType);
    if (!mmFtv)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType);
    if (!instantiatedMmType)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType);
    if (!instantiatedMmFtv)
        return {ctx->builtins->errorType, Reduction::MaybeOk, {}, {}};

    TypePackId inferredArgPack = ctx->arena->addTypePack({lhsTy, rhsTy});
    Unifier2 u2{ctx->arena, ctx->builtins, ctx->scope, ctx->ice};
    if (UnifyResult::Ok != u2.unify(inferredArgPack, instantiatedMmFtv->argTypes))
        return {std::nullopt, Reduction::Erroneous, {}, {}}; // occurs check failed

    Subtyping subtyping{ctx->builtins, ctx->arena, ctx->simplifier, ctx->normalizer, ctx->typeFunctionRuntime, ctx->ice};
    if (FFlag::LuauPassBindableGenericsByReference)
    {
        if (!subtyping.isSubtype(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope, {}).isSubtype)
            return {std::nullopt, Reduction::Erroneous, {}, {}};
    }
    else if (!subtyping.isSubtype_DEPRECATED(inferredArgPack, instantiatedMmFtv->argTypes, ctx->scope).isSubtype) // TODO: is this the right variance?
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    return {ctx->builtins->booleanType, Reduction::MaybeOk, {}, {}};
}

// Collect types that prevent us from reducing a particular refinement.
struct FindRefinementBlockers : TypeOnceVisitor
{
    DenseHashSet<TypeId> found{nullptr};

    FindRefinementBlockers()
        : TypeOnceVisitor("FindRefinementBlockers", FFlag::LuauExplicitSkipBoundTypes)
    {
    }

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

    bool visit(TypeId ty, const ExternType&) override
    {
        return false;
    }
};

struct ContainsRefinableType : TypeOnceVisitor
{
    bool found = false;
    ContainsRefinableType()
        : TypeOnceVisitor("ContainsRefinableType", /* skipBoundTypes */ true)
    {
    }

    bool visit(TypeId ty) override
    {
        // Default case: if we find *some* type that's worth refining against,
        // then we can claim that this type contains a refineable type.
        found = true;
        return false;
    }

    bool visit(TypeId Ty, const NoRefineType&) override
    {
        // No refine types aren't interesting
        return false;
    }

    bool visit(TypeId ty, const TableType&) override
    {
        return !found;
    }
    bool visit(TypeId ty, const MetatableType&) override
    {
        return !found;
    }
    bool visit(TypeId ty, const FunctionType&) override
    {
        return !found;
    }
    bool visit(TypeId ty, const UnionType&) override
    {
        return !found;
    }
    bool visit(TypeId ty, const IntersectionType&) override
    {
        return !found;
    }
    bool visit(TypeId ty, const NegationType&) override
    {
        return !found;
    }
};

namespace
{

bool isTruthyOrFalsyType(TypeId ty)
{
    ty = follow(ty);
    return isApproximatelyTruthyType(ty) || isApproximatelyFalsyType(ty);
}

struct RefineTypeScrubber : public Substitution
{
    NotNull<TypeFunctionContext> ctx;
    TypeId needle;

    explicit RefineTypeScrubber(NotNull<TypeFunctionContext> ctx, TypeId needle)
        : Substitution(ctx->arena)
        , ctx{ctx}
        , needle{needle}
    {
    }

    bool isDirty(TypePackId tp) override
    {
        return false;
    }

    bool ignoreChildren(TypePackId tp) override
    {
        return false;
    }

    TypePackId clean(TypePackId tp) override
    {
        return tp;
    }

    bool isDirty(TypeId ty) override
    {
        if (auto ut = get<UnionType>(ty))
        {
            for (auto option : ut)
            {
                if (option == needle)
                    return true;
            }
        }
        else if (auto it = get<IntersectionType>(ty))
        {
            for (auto part : it)
            {
                if (part == needle)
                    return true;
            }
        }
        return FFlag::LuauRefineOccursCheckDirectRecursion ? ty == needle : false;
    }

    bool ignoreChildren(TypeId ty) override
    {
        return !is<UnionType, IntersectionType>(ty);
    }

    TypeId clean(TypeId ty) override
    {
        // NOTE: this feels pretty similar to other places where we try to
        // filter over a set type, may be worth combining those in the future.
        if (auto ut = get<UnionType>(ty))
        {
            TypeIds newOptions;
            for (auto option : ut)
            {
                if (option != needle && !is<NeverType>(option))
                    newOptions.insert(option);
            }
            if (newOptions.empty())
                return ctx->builtins->neverType;
            else if (newOptions.size() == 1)
                return *newOptions.begin();
            else
                return ctx->arena->addType(UnionType{newOptions.take()});
        }
        else if (auto it = get<IntersectionType>(ty))
        {
            TypeIds newParts;
            for (auto part : it)
            {
                if (part != needle && !is<UnknownType>(part))
                    newParts.insert(part);
            }
            if (newParts.empty())
                return ctx->builtins->unknownType;
            else if (newParts.size() == 1)
                return *newParts.begin();
            else
                return ctx->arena->addType(IntersectionType{newParts.take()});
        }
        else if (FFlag::LuauRefineOccursCheckDirectRecursion && ty == needle)
            return ctx->builtins->unknownType;
        else
            return ty;
    }
};

bool occurs(TypeId haystack, TypeId needle, DenseHashSet<TypeId>& seen)
{
    if (needle == haystack)
        return true;

    if (seen.contains(haystack))
        return false;

    seen.insert(haystack);

    if (auto ut = get<UnionType>(haystack))
    {
        for (auto option : ut)
            if (occurs(option, needle, seen))
                return true;
    }

    if (auto it = get<UnionType>(haystack))
    {
        for (auto part : it)
            if (occurs(part, needle, seen))
                return true;
    }

    return false;
}

bool occurs(TypeId haystack, TypeId needle)
{
    DenseHashSet<TypeId> seen{nullptr};
    return occurs(haystack, needle, seen);
}

} // namespace

TypeFunctionReductionResult<TypeId> refineTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() < 2 || !packParams.empty())
    {
        ctx->ice->ice("refine type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId targetTy = follow(typeParams.at(0));

    // If we end up minting a refine type like:
    //
    //  t1 where t1 = refine<T | t1, Y>
    //
    // This can create a degenerate set type such as:
    //
    //  t1 where t1 = (T | t1) & Y
    //
    // Instead, we can clip the recursive part:
    //
    //  t1 where t1 = refine<T | t1, Y> => refine<T, Y>
    if (occurs(targetTy, instance))
    {
        RefineTypeScrubber rts{ctx, instance};
        if (auto result = rts.substitute(targetTy))
            targetTy = *result;
    }

    std::vector<TypeId> discriminantTypes;
    for (size_t i = 1; i < typeParams.size(); i++)
        discriminantTypes.push_back(follow(typeParams.at(i)));

    if (FFlag::LuauRefineNoRefineAlways)
    {
        bool hasAnyRealRefinements = false;
        for (auto discriminant : discriminantTypes)
        {
            // If the discriminant type is only:
            // - The `*no-refine*` type or,
            // - tables, metatables, unions, intersections, functions, or negations _containing_ `*no-refine*`.
            // There's no point in refining against it.
            ContainsRefinableType crt;
            crt.traverse(discriminant);

            hasAnyRealRefinements = hasAnyRealRefinements || crt.found;
        }

        // if we don't have any real refinements, i.e. they're all `*no-refine*`, then we can reduce immediately.
        if (!hasAnyRealRefinements)
            return {targetTy, {}};
    }

    const bool targetIsPending = isBlockedOrUnsolvedType(targetTy);

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (targetIsPending)
        return {std::nullopt, Reduction::MaybeOk, {targetTy}, {}};
    else
    {
        for (auto t : discriminantTypes)
        {
            if (isPending(t, ctx->solver))
                return {std::nullopt, Reduction::MaybeOk, {t}, {}};
        }
    }

    // If we have a blocked type in the target, we *could* potentially
    // refine it, but more likely we end up with some type explosion in
    // normalization.
    FindRefinementBlockers frb;
    frb.traverse(targetTy);
    if (!frb.found.empty())
        return {std::nullopt, Reduction::MaybeOk, {frb.found.begin(), frb.found.end()}, {}};

    int stepRefineCount = 0;

    // Refine a target type and a discriminant one at a time.
    // Returns result : TypeId, toBlockOn : vector<TypeId>
    auto stepRefine = [&stepRefineCount, &ctx](TypeId target, TypeId discriminant) -> std::pair<TypeId, std::vector<TypeId>>
    {
        std::optional<RecursionLimiter> rl;
        if (FFlag::LuauRefineDistributesOverUnions)
            rl.emplace("BuiltInTypeFunctions::stepRefine", &stepRefineCount, DFInt::LuauStepRefineRecursionLimit);

        std::vector<TypeId> toBlock;
        // we need a more complex check for blocking on the discriminant in particular
        FindRefinementBlockers frb;
        frb.traverse(discriminant);

        if (!frb.found.empty())
            return {nullptr, {frb.found.begin(), frb.found.end()}};

        if (FFlag::DebugLuauEqSatSimplification)
        {
            auto simplifyResult = eqSatSimplify(ctx->simplifier, ctx->arena->addType(IntersectionType{{target, discriminant}}));
            if (simplifyResult)
            {
                if (ctx->solver)
                {
                    for (TypeId newTf : simplifyResult->newTypeFunctions)
                        ctx->pushConstraint(ReduceConstraint{newTf});
                }

                return {simplifyResult->result, {}};
            }
            else
                return {nullptr, {}};
        }
        else
        {
            // FFlag::LuauRefineNoRefineAlways moves this check upwards so that it runs even if the thing being refined is pending.
            if (!FFlag::LuauRefineNoRefineAlways)
            {
                // If the discriminant type is only:
                // - The `*no-refine*` type or,
                // - tables, metatables, unions, intersections, functions, or negations _containing_ `*no-refine*`.
                // There's no point in refining against it.
                ContainsRefinableType crt;
                crt.traverse(discriminant);
                if (!crt.found)
                    return {target, {}};
            }

            if (auto ty = intersectWithSimpleDiscriminant(ctx->builtins, ctx->arena, target, discriminant))
                return {*ty, {}};

            // NOTE: This block causes us to refine too early in some cases.
            if (auto negation = get<NegationType>(discriminant))
            {
                if (auto primitive = get<PrimitiveType>(follow(negation->ty)); primitive && primitive->type == PrimitiveType::NilType)
                {
                    SimplifyResult result = simplifyIntersection(ctx->builtins, ctx->arena, target, discriminant);
                    return {result.result, {}};
                }
            }

            // If the target type is a table, then simplification already implements the logic to deal with refinements properly since the
            // type of the discriminant is guaranteed to only ever be an (arbitrarily-nested) table of a single property type.
            // We also fire for simple discriminants such as false? and ~(false?): the falsy and truthy types respectively.
            if (is<TableType>(target) || isTruthyOrFalsyType(discriminant))
            {
                SimplifyResult result = simplifyIntersection(ctx->builtins, ctx->arena, target, discriminant);
                // Simplification considers free and generic types to be
                // 'blocking', but that's not suitable for refine<>.
                //
                // If we are only blocked on those types, we consider
                // the simplification a success and reduce.
                if (std::all_of(
                        begin(result.blockedTypes),
                        end(result.blockedTypes),
                        [](TypeId v)
                        {
                            return is<FreeType, GenericType>(follow(v));
                        }
                    ))
                {
                    return {result.result, {}};
                }
                else
                    return {nullptr, {result.blockedTypes.begin(), result.blockedTypes.end()}};

                return {result.result, {}};
            }


            // In the general case, we'll still use normalization though.
            TypeId intersection = ctx->arena->addType(IntersectionType{{target, discriminant}});
            std::shared_ptr<const NormalizedType> normIntersection = ctx->normalizer->normalize(intersection);
            std::shared_ptr<const NormalizedType> normType = ctx->normalizer->normalize(target);

            // if the intersection failed to normalize, we can't reduce, but know nothing about inhabitance.
            if (!normIntersection || !normType)
                return {nullptr, {}};

            TypeId resultTy = ctx->normalizer->typeFromNormal(*normIntersection);
            // include the error type if the target type is error-suppressing and the intersection we computed is not
            if (normType->shouldSuppressErrors() && !normIntersection->shouldSuppressErrors())
            {
                if (FFlag::LuauReduceSetTypeStackPressure)
                    resultTy = addUnion(ctx->arena, ctx->builtins, {resultTy, ctx->builtins->errorType});
                else
                    resultTy = ctx->arena->addType(UnionType{{resultTy, ctx->builtins->errorType}});
            }

            return {resultTy, {}};
        }
    };

    // refine target with each discriminant type in sequence (reverse of insertion order)
    // If we cannot proceed, block. If all discriminant types refine successfully, return
    // the result
    TypeId target = targetTy;
    while (!discriminantTypes.empty())
    {
        TypeId discriminant = discriminantTypes.back();

        if (FFlag::LuauRefineDistributesOverUnions)
        {
            discriminant = follow(discriminant);

            // first, we'll see if simplifying the discriminant alone will solve our problem...
            if (auto ut = get<UnionType>(discriminant))
            {
                TypeId workingType = ctx->builtins->neverType;

                for (auto optionAsDiscriminant : ut->options)
                {
                    SimplifyResult simplified = simplifyUnion(ctx->builtins, ctx->arena, workingType, optionAsDiscriminant);

                    if (!simplified.blockedTypes.empty())
                        return {std::nullopt, Reduction::MaybeOk, {simplified.blockedTypes.begin(), simplified.blockedTypes.end()}, {}};

                    workingType = simplified.result;
                }

                discriminant = workingType;
            }

            // if not, we try distributivity: a & (b | c) <=> (a & b) | (a & c)
            if (auto ut = get<UnionType>(discriminant))
            {
                TypeId finalRefined = ctx->builtins->neverType;

                for (auto optionAsDiscriminant : ut->options)
                {
                    auto [refined, blocked] = stepRefine(target, follow(optionAsDiscriminant));

                    if (blocked.empty() && refined == nullptr)
                        return {std::nullopt, Reduction::MaybeOk, {}, {}};

                    if (!blocked.empty())
                        return {std::nullopt, Reduction::MaybeOk, blocked, {}};

                    SimplifyResult simplified = simplifyUnion(ctx->builtins, ctx->arena, finalRefined, refined);

                    if (!simplified.blockedTypes.empty())
                        return {std::nullopt, Reduction::MaybeOk, {simplified.blockedTypes.begin(), simplified.blockedTypes.end()}, {}};

                    finalRefined = simplified.result;
                }

                target = finalRefined;
                discriminantTypes.pop_back();

                continue;
            }
        }

        auto [refined, blocked] = stepRefine(target, discriminant);

        if (blocked.empty() && refined == nullptr)
            return {std::nullopt, Reduction::MaybeOk, {}, {}};

        if (!blocked.empty())
            return {std::nullopt, Reduction::MaybeOk, blocked, {}};

        target = refined;
        discriminantTypes.pop_back();
    }
    return {target, Reduction::MaybeOk, {}, {}};
}

TypeFunctionReductionResult<TypeId> singletonTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("singleton type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId type = follow(typeParams.at(0));

    // check to see if both operand types are resolved enough, and wait to reduce if not
    if (isPending(type, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {type}, {}};

    TypeId followed = type;
    // we want to follow through a negation here as well.
    if (auto negation = get<NegationType>(followed))
        followed = follow(negation->ty);

    // if we have a singleton type or `nil`, which is its own singleton type...
    if (get<SingletonType>(followed) || isNil(followed))
        return {type, Reduction::MaybeOk, {}, {}};

    // otherwise, we'll return the top type, `unknown`.
    return {ctx->builtins->unknownType, Reduction::MaybeOk, {}, {}};
}

struct CollectUnionTypeOptions : TypeOnceVisitor
{
    NotNull<TypeFunctionContext> ctx;
    DenseHashSet<TypeId> options{nullptr};
    DenseHashSet<TypeId> blockingTypes{nullptr};

    explicit CollectUnionTypeOptions(NotNull<TypeFunctionContext> ctx)
        : TypeOnceVisitor("CollectUnionTypeOptions", /* skipBoundTypes */ true)
        , ctx(ctx)
    {
    }

    bool visit(TypeId ty) override
    {
        options.insert(ty);
        if (isPending(ty, ctx->solver))
            blockingTypes.insert(ty);
        return false;
    }

    bool visit(TypePackId tp) override
    {
        return false;
    }

    bool visit(TypeId ty, const UnionType& ut) override
    {
        // If we have something like:
        //
        //  union<A | B, C | D>
        //
        // We probably just want to consider this to be the same as
        //
        //   union<A, B, C, D>
        return true;
    }

    bool visit(TypeId ty, const TypeFunctionInstanceType& tfit) override
    {
        if (FFlag::LuauBuiltinTypeFunctionsArentGlobal)
        {
            if (tfit.function->name != ctx->builtins->typeFunctions->unionFunc.name)
            {
                options.insert(ty);
                blockingTypes.insert(ty);
                return false;
            }
        }
        else
        {
            if (tfit.function->name != builtinTypeFunctions_DEPRECATED().unionFunc.name)
            {
                options.insert(ty);
                blockingTypes.insert(ty);
                return false;
            }
        }
        return true;
    }
};

TypeFunctionReductionResult<TypeId> unionTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (!packParams.empty())
    {
        ctx->ice->ice("union type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    // if we only have one parameter, there's nothing to do.
    if (typeParams.size() == 1)
        return {follow(typeParams[0]), Reduction::MaybeOk, {}, {}};


    CollectUnionTypeOptions collector{ctx};
    collector.traverse(instance);

    if (!collector.blockingTypes.empty())
    {
        std::vector<TypeId> blockingTypes{collector.blockingTypes.begin(), collector.blockingTypes.end()};
        return {std::nullopt, Reduction::MaybeOk, std::move(blockingTypes), {}};
    }

    TypeId resultTy = ctx->builtins->neverType;
    for (auto ty : collector.options)
    {
        SimplifyResult result = simplifyUnion(ctx->builtins, ctx->arena, resultTy, ty);
        // This condition might fire if one of the arguments to this type
        // function is a free type somewhere deep in a nested union or
        // intersection type, even though we ran a pass above to capture
        // some blocked types.
        if (!result.blockedTypes.empty())
            return {std::nullopt, Reduction::MaybeOk, {result.blockedTypes.begin(), result.blockedTypes.end()}, {}};

        resultTy = result.result;
    }

    return {resultTy, Reduction::MaybeOk, {}, {}};
}


TypeFunctionReductionResult<TypeId> intersectTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (!packParams.empty())
    {
        ctx->ice->ice("intersect type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    // if we only have one parameter, there's nothing to do.
    if (typeParams.size() == 1)
        return {follow(typeParams[0]), Reduction::MaybeOk, {}, {}};

    // we need to follow all of the type parameters.
    std::vector<TypeId> types;
    types.reserve(typeParams.size());
    for (auto ty : typeParams)
        types.emplace_back(follow(ty));

    // if we only have two parameters and one is `*no-refine*`, we're all done.
    if (types.size() == 2 && get<NoRefineType>(types[1]))
        return {types[0], Reduction::MaybeOk, {}, {}};
    else if (types.size() == 2 && get<NoRefineType>(types[0]))
        return {types[1], Reduction::MaybeOk, {}, {}};

    // check to see if the operand types are resolved enough, and wait to reduce if not
    // if any of them are `never`, the intersection will always be `never`, so we can reduce directly.
    for (auto ty : types)
    {
        if (isPending(ty, ctx->solver))
            return {std::nullopt, Reduction::MaybeOk, {ty}, {}};
        else if (get<NeverType>(ty))
            return {ctx->builtins->neverType, Reduction::MaybeOk, {}, {}};
    }

    // fold over the types with `simplifyIntersection`
    TypeId resultTy = ctx->builtins->unknownType;
    // collect types which caused intersection to return never
    DenseHashSet<TypeId> unintersectableTypes{nullptr};
    for (auto ty : types)
    {
        // skip any `*no-refine*` types.
        if (get<NoRefineType>(ty))
            continue;

        if (auto simpleResult = intersectWithSimpleDiscriminant(ctx->builtins, ctx->arena, resultTy, ty))
        {
            if (get<NeverType>(*simpleResult))
                unintersectableTypes.insert(follow(ty));
            else
                resultTy = *simpleResult;
            continue;
        }

        SimplifyResult result = simplifyIntersection(ctx->builtins, ctx->arena, resultTy, ty);

        // If simplifying the intersection returned never, note the type we tried to intersect it with, and continue trying to intersect with the
        // rest
        if (get<NeverType>(result.result))
        {
            unintersectableTypes.insert(follow(ty));
            continue;
        }
        for (TypeId blockedType : result.blockedTypes)
        {
            if (!get<GenericType>(blockedType))
                return {std::nullopt, Reduction::MaybeOk, {result.blockedTypes.begin(), result.blockedTypes.end()}, {}};
        }

        resultTy = result.result;
    }

    if (!unintersectableTypes.empty())
    {
        unintersectableTypes.insert(resultTy);
        if (unintersectableTypes.size() > 1)
        {
            TypeId intersection =
                ctx->arena->addType(IntersectionType{std::vector<TypeId>(unintersectableTypes.begin(), unintersectableTypes.end())});
            return {intersection, Reduction::MaybeOk, {}, {}};
        }
        else
        {
            return {*unintersectableTypes.begin(), Reduction::MaybeOk, {}, {}};
        }
    }
    // if the intersection simplifies to `never`, this gives us bad autocomplete.
    // we'll just produce the intersection plainly instead, but this might be revisitable
    // if we ever give `never` some kind of "explanation" trail.
    if (get<NeverType>(resultTy))
    {
        TypeId intersection = ctx->arena->addType(IntersectionType{typeParams});
        return {intersection, Reduction::MaybeOk, {}, {}};
    }

    return {resultTy, Reduction::MaybeOk, {}, {}};
}

namespace
{

/**
 * Computes the keys of `ty` into `result`
 * `isRaw` parameter indicates whether or not we should follow __index metamethods
 * returns `false` if `result` should be ignored because the answer is "all strings"
 */
bool computeKeysOf(TypeId ty, Set<std::optional<std::string>>& result, DenseHashSet<TypeId>& seen, bool isRaw, NotNull<TypeFunctionContext> ctx)
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

        for (const auto& [key, _] : tableTy->props)
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

    if (auto classTy = get<ExternType>(ty))
    {
        for (const auto& [key, _] : classTy->props)
            result.insert(key);

        bool res = true;
        if (classTy->metatable && !isRaw)
        {
            // findMetatableEntry demands the ability to emit errors, so we must give it
            // the necessary state to do that, even if we intend to just eat the errors.
            ErrorVec dummy;

            std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, ty, "__index", Location{});
            if (mmType)
                res = res && computeKeysOf(*mmType, result, seen, isRaw, ctx);
        }

        if (classTy->parent)
            res = res && computeKeysOf(follow(*classTy->parent), result, seen, isRaw, ctx);

        return res;
    }

    // this should not be reachable since the type should be a valid tables or extern types part from normalization.
    LUAU_ASSERT(false);
    return false;
}

} // namespace

TypeFunctionReductionResult<TypeId> keyofFunctionImpl(
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx,
    bool isRaw
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("keyof type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId operandTy = follow(typeParams.at(0));

    std::shared_ptr<const NormalizedType> normTy = ctx->normalizer->normalize(operandTy);

    // if the operand failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!normTy)
        return {std::nullopt, Reduction::MaybeOk, {}, {}};

    // if we don't have either just tables or just extern types, we've got nothing to get keys of (at least until a future version perhaps adds extern
    // types as well)
    if (normTy->hasTables() == normTy->hasExternTypes())
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    // this is sort of atrocious, but we're trying to reject any type that has not normalized to a table or a union of tables.
    if (normTy->hasTops() || normTy->hasBooleans() || normTy->hasErrors() || normTy->hasNils() || normTy->hasNumbers() || normTy->hasStrings() ||
        normTy->hasThreads() || normTy->hasBuffers() || normTy->hasFunctions() || normTy->hasTyvars())
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    // We're going to collect the keys in here, and we use optional strings
    // so that we can differentiate between the empty string and _no_ string.
    Set<std::optional<std::string>> keys{std::nullopt};

    // computing the keys for extern types
    if (normTy->hasExternTypes())
    {
        LUAU_ASSERT(!normTy->hasTables());

        // seen set for key computation for extern types
        DenseHashSet<TypeId> seen{{}};

        auto externTypeIter = normTy->externTypes.ordering.begin();
        auto externTypeIterEnd = normTy->externTypes.ordering.end();
        LUAU_ASSERT(externTypeIter != externTypeIterEnd); // should be guaranteed by the `hasExternTypes` check earlier

        // collect all the properties from the first class type
        if (!computeKeysOf(*externTypeIter, keys, seen, isRaw, ctx))
            return {ctx->builtins->stringType, Reduction::MaybeOk, {}, {}}; // if it failed, we have a top type!

        // we need to look at each class to remove any keys that are not common amongst them all
        while (++externTypeIter != externTypeIterEnd)
        {
            seen.clear(); // we'll reuse the same seen set

            Set<std::optional<std::string>> localKeys{std::nullopt};

            // we can skip to the next class if this one is a top type
            if (!computeKeysOf(*externTypeIter, localKeys, seen, isRaw, ctx))
                continue;

            for (auto& key : keys)
            {
                // remove any keys that are not present in each class
                if (!localKeys.contains(key))
                    keys.erase(key);
            }
        }
    }

    // computing the keys for tables
    if (normTy->hasTables())
    {
        LUAU_ASSERT(!normTy->hasExternTypes());

        // seen set for key computation for tables
        DenseHashSet<TypeId> seen{{}};

        auto tablesIter = normTy->tables.begin();
        LUAU_ASSERT(tablesIter != normTy->tables.end()); // should be guaranteed by the `hasTables` check earlier

        // collect all the properties from the first table type
        if (!computeKeysOf(*tablesIter, keys, seen, isRaw, ctx))
            return {ctx->builtins->stringType, Reduction::MaybeOk, {}, {}}; // if it failed, we have the top table type!

        // we need to look at each tables to remove any keys that are not common amongst them all
        while (++tablesIter != normTy->tables.end())
        {
            seen.clear(); // we'll reuse the same seen set

            Set<std::optional<std::string>> localKeys{std::nullopt};

            // we can skip to the next table if this one is the top table type
            if (!computeKeysOf(*tablesIter, localKeys, seen, isRaw, ctx))
                continue;

            for (auto& key : keys)
            {
                // remove any keys that are not present in each table
                if (!localKeys.contains(key))
                    keys.erase(key);
            }
        }
    }

    // if the set of keys is empty, `keyof<T>` is `never`
    if (keys.empty())
        return {ctx->builtins->neverType, Reduction::MaybeOk, {}, {}};

    // everything is validated, we need only construct our big union of singletons now!
    std::vector<TypeId> singletons;
    singletons.reserve(keys.size());

    for (const auto& key : keys)
    {
        if (key)
            singletons.push_back(ctx->arena->addType(SingletonType{StringSingleton{*key}}));
    }

    // If there's only one entry, we don't need a UnionType.
    // We can take straight take it from the first entry
    // because it was added into the type arena already.
    if (singletons.size() == 1)
        return {singletons.front(), Reduction::MaybeOk, {}, {}};

    return {ctx->arena->addType(UnionType{std::move(singletons)}), Reduction::MaybeOk, {}, {}};
}

TypeFunctionReductionResult<TypeId> keyofTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("keyof type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return keyofFunctionImpl(typeParams, packParams, ctx, /* isRaw */ false);
}

TypeFunctionReductionResult<TypeId> rawkeyofTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("rawkeyof type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return keyofFunctionImpl(typeParams, packParams, ctx, /* isRaw */ true);
}

/* Searches through table's or class's props/indexer to find the property of `ty`
   If found, appends that property to `result` and returns true
   Else, returns false */
bool searchPropsAndIndexer(
    TypeId ty,
    TableType::Props tblProps,
    std::optional<TableIndexer> tblIndexer,
    DenseHashSet<TypeId>& result,
    NotNull<TypeFunctionContext> ctx
)
{
    ty = follow(ty);

    // index into tbl's properties
    if (auto stringSingleton = get<StringSingleton>(get<SingletonType>(ty)))
    {
        if (tblProps.find(stringSingleton->value) != tblProps.end())
        {

            Property& prop = tblProps.at(stringSingleton->value);

            TypeId propTy;
            if (prop.readTy)
                propTy = follow(*prop.readTy);
            else if (prop.writeTy)
                propTy = follow(*prop.writeTy);
            else // found the property, but there was no type associated with it
                return false;

            // property is a union type -> we need to extend our reduction type
            if (auto propUnionTy = get<UnionType>(propTy))
            {
                for (TypeId option : propUnionTy->options)
                {
                    result.insert(follow(option));
                }
            }
            else // property is a singular type or intersection type -> we can simply append
                result.insert(propTy);

            return true;
        }
    }

    // index into tbl's indexer
    if (tblIndexer)
    {
        TypeId indexType = follow(tblIndexer->indexType);

        if (auto tfit = get<TypeFunctionInstanceType>(indexType))
        {
            // if we have an index function here, it means we're in a cycle, so let's see if it's well-founded if we tie the knot
            if (FFlag::LuauBuiltinTypeFunctionsArentGlobal)
            {
                if (tfit->function.get() == &ctx->builtins->typeFunctions->indexFunc)
                    indexType = follow(tblIndexer->indexResultType);
            }
            else
            {
                if (tfit->function.get() == &builtinTypeFunctions_DEPRECATED().indexFunc)
                    indexType = follow(tblIndexer->indexResultType);
            }
        }

        if (isSubtype(ty, indexType, ctx->scope, ctx->builtins, ctx->simplifier, *ctx->ice, SolverMode::New))
        {
            TypeId idxResultTy = follow(tblIndexer->indexResultType);

            // indexResultType is a union type -> we need to extend our reduction type
            if (auto idxResUnionTy = get<UnionType>(idxResultTy))
            {
                for (TypeId option : idxResUnionTy->options)
                {
                    result.insert(follow(option));
                }
            }
            else // indexResultType is a singular type or intersection type -> we can simply append
                result.insert(idxResultTy);

            return true;
        }
    }

    return false;
}

bool tblIndexInto(
    TypeId indexer,
    TypeId indexee,
    DenseHashSet<TypeId>& result,
    DenseHashSet<TypeId>& seenSet,
    NotNull<TypeFunctionContext> ctx,
    bool isRaw
)
{
    indexer = follow(indexer);
    indexee = follow(indexee);

    if (seenSet.contains(indexee))
        return false;
    seenSet.insert(indexee);

    if (auto unionTy = get<UnionType>(indexee))
    {
        bool res = true;
        for (auto component : unionTy)
        {
            // if the component is in the seen set and isn't the indexee itself,
            // we can skip it cause it means we encountered it in an earlier component in the union.
            if (seenSet.contains(component) && component != indexee)
                continue;

            res = res && tblIndexInto(indexer, component, result, seenSet, ctx, isRaw);
        }
        return res;
    }

    if (get<FunctionType>(indexee))
    {
        TypePackId argPack = ctx->arena->addTypePack({indexer});
        SolveResult solveResult = solveFunctionCall(
            ctx->arena,
            ctx->builtins,
            ctx->simplifier,
            ctx->normalizer,
            ctx->typeFunctionRuntime,
            ctx->ice,
            ctx->limits,
            ctx->scope,
            ctx->scope->location,
            indexee,
            argPack
        );

        if (!solveResult.typePackId.has_value())
            return false;

        TypePack extracted = extendTypePack(*ctx->arena, ctx->builtins, *solveResult.typePackId, 1);
        if (extracted.head.empty())
            return false;

        result.insert(follow(extracted.head.front()));
        return true;
    }

    // we have a table type to try indexing
    if (auto tableTy = get<TableType>(indexee))
    {
        return searchPropsAndIndexer(indexer, tableTy->props, tableTy->indexer, result, ctx);
    }

    // we have a metatable type to try indexing
    if (auto metatableTy = get<MetatableType>(indexee))
    {
        if (auto tableTy = get<TableType>(follow(metatableTy->table)))
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
                return tblIndexInto(indexer, *mmType, result, seenSet, ctx, isRaw);
        }
    }

    return false;
}

bool tblIndexInto(TypeId indexer, TypeId indexee, DenseHashSet<TypeId>& result, NotNull<TypeFunctionContext> ctx, bool isRaw)
{
    DenseHashSet<TypeId> seenSet{{}};
    return tblIndexInto(indexer, indexee, result, seenSet, ctx, isRaw);
}

/* Vocabulary note: indexee refers to the type that contains the properties,
                    indexer refers to the type that is used to access indexee
   Example:         index<Person, "name"> => `Person` is the indexee and `"name"` is the indexer */
TypeFunctionReductionResult<TypeId> indexFunctionImpl(
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx,
    bool isRaw
)
{
    TypeId indexeeTy = follow(typeParams.at(0));

    if (isPending(indexeeTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {indexeeTy}, {}};

    std::shared_ptr<const NormalizedType> indexeeNormTy = ctx->normalizer->normalize(indexeeTy);

    // if the indexee failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!indexeeNormTy)
        return {std::nullopt, Reduction::MaybeOk, {}, {}};

    // if the indexee is `any`, then indexing also gives us `any`.
    if (indexeeNormTy->shouldSuppressErrors())
        return {ctx->builtins->anyType, Reduction::MaybeOk, {}, {}};

    // if we don't have either just tables or just extern types, we've got nothing to index into
    if (indexeeNormTy->hasTables() == indexeeNormTy->hasExternTypes())
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    // we're trying to reject any type that has not normalized to a table or extern type or a union of tables or extern types.
    if (indexeeNormTy->hasTops() || indexeeNormTy->hasBooleans() || indexeeNormTy->hasErrors() || indexeeNormTy->hasNils() ||
        indexeeNormTy->hasNumbers() || indexeeNormTy->hasStrings() || indexeeNormTy->hasThreads() || indexeeNormTy->hasBuffers() ||
        indexeeNormTy->hasFunctions() || indexeeNormTy->hasTyvars())
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    TypeId indexerTy = follow(typeParams.at(1));

    if (isPending(indexerTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {indexerTy}, {}};

    std::shared_ptr<const NormalizedType> indexerNormTy = ctx->normalizer->normalize(indexerTy);

    // if the indexer failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!indexerNormTy)
        return {std::nullopt, Reduction::MaybeOk, {}, {}};

    // we're trying to reject any type that is not a string singleton or primitive (string, number, boolean, thread, nil, function, table, or buffer)
    if (indexerNormTy->hasTops() || indexerNormTy->hasErrors())
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    // indexer can be a union —> break them down into a vector
    const std::vector<TypeId>* typesToFind = nullptr;
    const std::vector<TypeId> singleType{indexerTy};
    if (auto unionTy = get<UnionType>(indexerTy))
        typesToFind = &unionTy->options;
    else
        typesToFind = &singleType;

    DenseHashSet<TypeId> properties{{}}; // vector of types that will be returned

    if (indexeeNormTy->hasExternTypes())
    {
        LUAU_ASSERT(!indexeeNormTy->hasTables());

        if (isRaw) // rawget should never reduce for extern types (to match the behavior of the rawget global function)
            return {std::nullopt, Reduction::Erroneous, {}, {}};

        // at least one class is guaranteed to be in the iterator by .hasExternTypes()
        for (auto externTypeIter = indexeeNormTy->externTypes.ordering.begin(); externTypeIter != indexeeNormTy->externTypes.ordering.end();
             ++externTypeIter)
        {
            auto externTy = get<ExternType>(*externTypeIter);
            if (!externTy)
            {
                LUAU_ASSERT(false); // this should not be possible according to normalization's spec
                return {std::nullopt, Reduction::Erroneous, {}, {}};
            }

            for (TypeId ty : *typesToFind)
            {
                // Search for all instances of indexer in class->props and class->indexer
                if (searchPropsAndIndexer(ty, externTy->props, externTy->indexer, properties, ctx))
                    continue; // Indexer was found in this class, so we can move on to the next

                auto parent = externTy->parent;
                bool foundInParent = false;
                while (parent && !foundInParent)
                {
                    auto parentExternType = get<ExternType>(follow(*parent));
                    foundInParent = searchPropsAndIndexer(ty, parentExternType->props, parentExternType->indexer, properties, ctx);
                    parent = parentExternType->parent;
                }

                // we move on to the next type if any of the parents we went through had the property.
                if (foundInParent)
                    continue;

                // If code reaches here,that means the property not found -> check in the metatable's __index

                // findMetatableEntry demands the ability to emit errors, so we must give it
                // the necessary state to do that, even if we intend to just eat the errors.
                ErrorVec dummy;
                std::optional<TypeId> mmType = findMetatableEntry(ctx->builtins, dummy, *externTypeIter, "__index", Location{});
                if (!mmType) // if a metatable does not exist, there is no where else to look
                    return {std::nullopt, Reduction::Erroneous, {}, {}};

                if (!tblIndexInto(ty, *mmType, properties, ctx, isRaw)) // if indexer is not in the metatable, we fail to reduce
                    return {std::nullopt, Reduction::Erroneous, {}, {}};
            }
        }
    }

    if (indexeeNormTy->hasTables())
    {
        LUAU_ASSERT(!indexeeNormTy->hasExternTypes());

        // at least one table is guaranteed to be in the iterator by .hasTables()
        for (auto tablesIter = indexeeNormTy->tables.begin(); tablesIter != indexeeNormTy->tables.end(); ++tablesIter)
        {
            for (TypeId ty : *typesToFind)
                if (!tblIndexInto(ty, *tablesIter, properties, ctx, isRaw))
                {
                    if (FFlag::LuauRawGetHandlesNil && isRaw)
                        properties.insert(ctx->builtins->nilType);
                    else
                        return {std::nullopt, Reduction::Erroneous, {}, {}};
                }
        }
    }

    // If the type being reduced to is a single type, no need to union
    if (properties.size() == 1)
        return {*properties.begin(), Reduction::MaybeOk, {}, {}};

    return {ctx->arena->addType(UnionType{std::vector<TypeId>(properties.begin(), properties.end())}), Reduction::MaybeOk, {}, {}};
}

TypeFunctionReductionResult<TypeId> indexTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("index type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return indexFunctionImpl(typeParams, packParams, ctx, /* isRaw */ false);
}

TypeFunctionReductionResult<TypeId> rawgetTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("rawget type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return indexFunctionImpl(typeParams, packParams, ctx, /* isRaw */ true);
}

TypeFunctionReductionResult<TypeId> setmetatableTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("setmetatable type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    const Location location = ctx->constraint ? ctx->constraint->location : Location{};

    TypeId targetTy = follow(typeParams.at(0));
    TypeId metatableTy = follow(typeParams.at(1));

    std::shared_ptr<const NormalizedType> targetNorm = ctx->normalizer->normalize(targetTy);

    // if the operand failed to normalize, we can't reduce, but know nothing about inhabitance.
    if (!targetNorm)
        return {std::nullopt, Reduction::MaybeOk, {}, {}};

    // cannot setmetatable on something without table parts.
    if (!targetNorm->hasTables())
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    // we're trying to reject any type that has not normalized to a table or a union/intersection of tables.
    if (targetNorm->hasTops() || targetNorm->hasBooleans() || targetNorm->hasErrors() || targetNorm->hasNils() || targetNorm->hasNumbers() ||
        targetNorm->hasStrings() || targetNorm->hasThreads() || targetNorm->hasBuffers() || targetNorm->hasFunctions() || targetNorm->hasTyvars() ||
        targetNorm->hasExternTypes())
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    // if the supposed metatable is not a table, we will fail to reduce.
    if (!get<TableType>(metatableTy) && !get<MetatableType>(metatableTy))
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    if (targetNorm->tables.size() == 1)
    {
        TypeId table = *targetNorm->tables.begin();

        // findMetatableEntry demands the ability to emit errors, so we must give it
        // the necessary state to do that, even if we intend to just eat the errors.
        ErrorVec dummy;

        std::optional<TypeId> metatableMetamethod = findMetatableEntry(ctx->builtins, dummy, table, "__metatable", location);

        // if the `__metatable` metamethod is present, then the table is locked and we cannot `setmetatable` on it.
        if (metatableMetamethod)
            return {std::nullopt, Reduction::Erroneous, {}, {}};

        TypeId withMetatable = ctx->arena->addType(MetatableType{table, metatableTy});

        return {withMetatable, Reduction::MaybeOk, {}, {}};
    }

    TypeId result = ctx->builtins->neverType;

    for (auto componentTy : targetNorm->tables)
    {
        // findMetatableEntry demands the ability to emit errors, so we must give it
        // the necessary state to do that, even if we intend to just eat the errors.
        ErrorVec dummy;

        std::optional<TypeId> metatableMetamethod = findMetatableEntry(ctx->builtins, dummy, componentTy, "__metatable", location);

        // if the `__metatable` metamethod is present, then the table is locked and we cannot `setmetatable` on it.
        if (metatableMetamethod)
            return {std::nullopt, Reduction::Erroneous, {}, {}};

        TypeId withMetatable = ctx->arena->addType(MetatableType{componentTy, metatableTy});
        SimplifyResult simplified = simplifyUnion(ctx->builtins, ctx->arena, result, withMetatable);

        if (!simplified.blockedTypes.empty())
        {
            std::vector<TypeId> blockedTypes{};
            blockedTypes.reserve(simplified.blockedTypes.size());
            for (auto ty : simplified.blockedTypes)
                blockedTypes.push_back(ty);
            return {std::nullopt, Reduction::MaybeOk, std::move(blockedTypes), {}};
        }

        result = simplified.result;
    }

    return {result, Reduction::MaybeOk, {}, {}};
}

static TypeFunctionReductionResult<TypeId> getmetatableHelper(TypeId targetTy, const Location& location, NotNull<TypeFunctionContext> ctx)
{
    targetTy = follow(targetTy);

    std::optional<TypeId> result = std::nullopt;
    bool erroneous = true;

    if (auto table = get<TableType>(targetTy))
        erroneous = false;

    if (auto mt = get<MetatableType>(targetTy))
    {
        result = mt->metatable;
        erroneous = false;
    }

    if (auto clazz = get<ExternType>(targetTy))
    {
        result = clazz->metatable;
        erroneous = false;
    }

    if (auto primitive = get<PrimitiveType>(targetTy))
    {
        result = primitive->metatable;
        erroneous = false;
    }

    if (auto singleton = get<SingletonType>(targetTy))
    {
        if (get<StringSingleton>(singleton))
        {
            auto primitiveString = get<PrimitiveType>(ctx->builtins->stringType);
            result = primitiveString->metatable;
        }
        erroneous = false;
    }

    if (get<AnyType>(targetTy))
    {
        // getmetatable<any> ~ any
        result = targetTy;
        erroneous = false;
    }

    if (erroneous)
        return {std::nullopt, Reduction::Erroneous, {}, {}};

    // findMetatableEntry demands the ability to emit errors, so we must give it
    // the necessary state to do that, even if we intend to just eat the errors.
    ErrorVec dummy;

    std::optional<TypeId> metatableMetamethod = findMetatableEntry(ctx->builtins, dummy, targetTy, "__metatable", location);

    if (metatableMetamethod)
        return {metatableMetamethod, Reduction::MaybeOk, {}, {}};

    if (result)
        return {result, Reduction::MaybeOk, {}, {}};

    return {ctx->builtins->nilType, Reduction::MaybeOk, {}, {}};
}

TypeFunctionReductionResult<TypeId> getmetatableTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("getmetatable type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    const Location location = ctx->constraint ? ctx->constraint->location : Location{};

    TypeId targetTy = follow(typeParams.at(0));

    if (isPending(targetTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {targetTy}, {}};

    if (auto ut = get<UnionType>(targetTy))
    {
        std::vector<TypeId> options{};
        options.reserve(ut->options.size());

        for (auto option : ut->options)
        {
            TypeFunctionReductionResult<TypeId> result = getmetatableHelper(option, location, ctx);

            if (!result.result)
                return result;

            options.push_back(*result.result);
        }

        return {ctx->arena->addType(UnionType{std::move(options)}), Reduction::MaybeOk, {}, {}};
    }

    if (auto it = get<IntersectionType>(targetTy))
    {
        std::vector<TypeId> parts{};
        parts.reserve(it->parts.size());

        bool erroredWithUnknown = false;

        for (auto part : it->parts)
        {
            TypeFunctionReductionResult<TypeId> result = getmetatableHelper(part, location, ctx);

            if (!result.result)
            {
                // Don't immediately error if part is unknown
                if (get<UnknownType>(follow(part)))
                {
                    erroredWithUnknown = true;
                    continue;
                }
                else
                    return result;
            }

            parts.push_back(*result.result);
        }

        // If all parts are unknown, return erroneous reduction
        if (erroredWithUnknown && parts.empty())
            return {std::nullopt, Reduction::Erroneous, {}, {}};

        if (parts.size() == 1)
            return {parts.front(), Reduction::MaybeOk, {}, {}};

        return {ctx->arena->addType(IntersectionType{std::move(parts)}), Reduction::MaybeOk, {}, {}};
    }

    return getmetatableHelper(targetTy, location, ctx);
}

TypeFunctionReductionResult<TypeId> weakoptionalTypeFunc(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
)
{
    if (typeParams.size() != 1 || !packParams.empty())
    {
        ctx->ice->ice("weakoptional type function: encountered a type function instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId targetTy = follow(typeParams.at(0));

    if (isPending(targetTy, ctx->solver))
        return {std::nullopt, Reduction::MaybeOk, {targetTy}, {}};

    if (is<NeverType>(instance))
        return {ctx->builtins->nilType, Reduction::MaybeOk, {}, {}};

    std::shared_ptr<const NormalizedType> targetNorm = ctx->normalizer->normalize(targetTy);

    if (!targetNorm)
        return {std::nullopt, Reduction::MaybeOk, {}, {}};

    auto result = ctx->normalizer->isInhabited(targetNorm.get());
    if (result == NormalizationResult::False)
        return {ctx->builtins->nilType, Reduction::MaybeOk, {}, {}};

    return {targetTy, Reduction::MaybeOk, {}, {}};
}

BuiltinTypeFunctions::BuiltinTypeFunctions()
    : userFunc{"user", userDefinedTypeFunction}
    , notFunc{"not", notTypeFunction}
    , lenFunc{"len", lenTypeFunction}
    , unmFunc{"unm", unmTypeFunction}
    , addFunc{"add", addTypeFunction}
    , subFunc{"sub", subTypeFunction}
    , mulFunc{"mul", mulTypeFunction}
    , divFunc{"div", divTypeFunction}
    , idivFunc{"idiv", idivTypeFunction}
    , powFunc{"pow", powTypeFunction}
    , modFunc{"mod", modTypeFunction}
    , concatFunc{"concat", concatTypeFunction}
    , andFunc{"and", andTypeFunction, /*canReduceGenerics*/ true}
    , orFunc{"or", orTypeFunction, /*canReduceGenerics*/ true}
    , ltFunc{"lt", ltTypeFunction}
    , leFunc{"le", leTypeFunction}
    , eqFunc{"eq", eqTypeFunction}
    , refineFunc{"refine", refineTypeFunction, /*canReduceGenerics*/ true}
    , singletonFunc{"singleton", singletonTypeFunction}
    , unionFunc{"union", unionTypeFunction}
    , intersectFunc{"intersect", intersectTypeFunction}
    , keyofFunc{"keyof", keyofTypeFunction}
    , rawkeyofFunc{"rawkeyof", rawkeyofTypeFunction}
    , indexFunc{"index", indexTypeFunction}
    , rawgetFunc{"rawget", rawgetTypeFunction}
    , setmetatableFunc{"setmetatable", setmetatableTypeFunction}
    , getmetatableFunc{"getmetatable", getmetatableTypeFunction}
    , weakoptionalFunc{"weakoptional", weakoptionalTypeFunc}
{
}

void BuiltinTypeFunctions::addToScope(NotNull<TypeArena> arena, NotNull<Scope> scope) const
{
    // make a type function for a one-argument type function
    auto mkUnaryTypeFunction = [&](const TypeFunction* tf)
    {
        TypeId t = arena->addType(GenericType{"T", Polarity::Negative});
        GenericTypeDefinition genericT{t};

        return TypeFun{{genericT}, arena->addType(TypeFunctionInstanceType{NotNull{tf}, {t}, {}})};
    };

    // make a type function for a two-argument type function with a default argument for the second type being the first
    auto mkBinaryTypeFunctionWithDefault = [&](const TypeFunction* tf)
    {
        TypeId t = arena->addType(GenericType{"T", Polarity::Negative});
        TypeId u = arena->addType(GenericType{"U", Polarity::Negative});
        GenericTypeDefinition genericT{t};
        GenericTypeDefinition genericU{u, {t}};

        return TypeFun{{genericT, genericU}, arena->addType(TypeFunctionInstanceType{NotNull{tf}, {t, u}, {}})};
    };

    // make a two-argument type function without the default arguments
    auto mkBinaryTypeFunction = [&](const TypeFunction* tf)
    {
        TypeId t = arena->addType(GenericType{"T", Polarity::Negative});
        TypeId u = arena->addType(GenericType{"U", Polarity::Negative});
        GenericTypeDefinition genericT{t};
        GenericTypeDefinition genericU{u};

        return TypeFun{{genericT, genericU}, arena->addType(TypeFunctionInstanceType{NotNull{tf}, {t, u}, {}})};
    };

    scope->exportedTypeBindings[lenFunc.name] = mkUnaryTypeFunction(&lenFunc);
    scope->exportedTypeBindings[unmFunc.name] = mkUnaryTypeFunction(&unmFunc);

    scope->exportedTypeBindings[addFunc.name] = mkBinaryTypeFunctionWithDefault(&addFunc);
    scope->exportedTypeBindings[subFunc.name] = mkBinaryTypeFunctionWithDefault(&subFunc);
    scope->exportedTypeBindings[mulFunc.name] = mkBinaryTypeFunctionWithDefault(&mulFunc);
    scope->exportedTypeBindings[divFunc.name] = mkBinaryTypeFunctionWithDefault(&divFunc);
    scope->exportedTypeBindings[idivFunc.name] = mkBinaryTypeFunctionWithDefault(&idivFunc);
    scope->exportedTypeBindings[powFunc.name] = mkBinaryTypeFunctionWithDefault(&powFunc);
    scope->exportedTypeBindings[modFunc.name] = mkBinaryTypeFunctionWithDefault(&modFunc);
    scope->exportedTypeBindings[concatFunc.name] = mkBinaryTypeFunctionWithDefault(&concatFunc);

    scope->exportedTypeBindings[ltFunc.name] = mkBinaryTypeFunctionWithDefault(&ltFunc);
    scope->exportedTypeBindings[leFunc.name] = mkBinaryTypeFunctionWithDefault(&leFunc);
    if (!FFlag::LuauNoMoreComparisonTypeFunctions)
        scope->exportedTypeBindings[eqFunc.name] = mkBinaryTypeFunctionWithDefault(&eqFunc);

    scope->exportedTypeBindings[keyofFunc.name] = mkUnaryTypeFunction(&keyofFunc);
    scope->exportedTypeBindings[rawkeyofFunc.name] = mkUnaryTypeFunction(&rawkeyofFunc);

    scope->exportedTypeBindings[indexFunc.name] = mkBinaryTypeFunction(&indexFunc);
    scope->exportedTypeBindings[rawgetFunc.name] = mkBinaryTypeFunction(&rawgetFunc);

    scope->exportedTypeBindings[setmetatableFunc.name] = mkBinaryTypeFunction(&setmetatableFunc);
    scope->exportedTypeBindings[getmetatableFunc.name] = mkUnaryTypeFunction(&getmetatableFunc);
}


const BuiltinTypeFunctions& builtinTypeFunctions_DEPRECATED()
{
    static std::unique_ptr<const BuiltinTypeFunctions> result = std::make_unique<BuiltinTypeFunctions>();

    return *result;
}

} // namespace Luau
