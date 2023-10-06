// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeFamily.h"

#include "Luau/ConstraintSolver.h"
#include "Luau/DenseHash.h"
#include "Luau/Instantiation.h"
#include "Luau/Normalize.h"
#include "Luau/Simplify.h"
#include "Luau/Substitution.h"
#include "Luau/ToString.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeCheckLimits.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"
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
    TypeFamilyContext ctx;

    std::deque<TypeId> queuedTys;
    std::deque<TypePackId> queuedTps;
    DenseHashSet<const void*> irreducible{nullptr};
    FamilyGraphReductionResult result;
    bool force = false;

    // Local to the constraint being reduced.
    Location location;

    FamilyReducer(std::deque<TypeId> queuedTys, std::deque<TypePackId> queuedTps, Location location, TypeFamilyContext ctx, bool force = false)
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
    std::deque<TypeId> queuedTys, std::deque<TypePackId> queuedTps, Location location, TypeFamilyContext ctx, bool force)
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

TypeFamilyReductionResult<TypeId> numericBinopFamilyFn(
    std::vector<TypeId> typeParams, std::vector<TypePackId> packParams, NotNull<TypeFamilyContext> ctx, const std::string metamethod)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));
    const NormalizedType* normLhsTy = ctx->normalizer->normalize(lhsTy);
    const NormalizedType* normRhsTy = ctx->normalizer->normalize(rhsTy);
    if (!normLhsTy || !normRhsTy)
    {
        return {std::nullopt, false, {}, {}};
    }
    else if (is<AnyType>(normLhsTy->tops) || is<AnyType>(normRhsTy->tops))
    {
        return {ctx->builtins->anyType, false, {}, {}};
    }
    else if ((normLhsTy->hasNumbers() || normLhsTy->hasTops()) && (normRhsTy->hasNumbers() || normRhsTy->hasTops()))
    {
        return {ctx->builtins->numberType, false, {}, {}};
    }
    else if (is<ErrorType>(lhsTy) || is<ErrorType>(rhsTy))
    {
        return {ctx->builtins->errorRecoveryType(), false, {}, {}};
    }
    else if (is<NeverType>(lhsTy) || is<NeverType>(rhsTy))
    {
        return {ctx->builtins->neverType, false, {}, {}};
    }
    else if (isPending(lhsTy, ctx->solver))
    {
        return {std::nullopt, false, {lhsTy}, {}};
    }
    else if (isPending(rhsTy, ctx->solver))
    {
        return {std::nullopt, false, {rhsTy}, {}};
    }

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

    if (std::optional<TypeId> instantiatedMmType = instantiate(ctx->builtins, ctx->arena, ctx->limits, ctx->scope, *mmType))
    {
        if (const FunctionType* instantiatedMmFtv = get<FunctionType>(*instantiatedMmType))
        {
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
        else
        {
            return {ctx->builtins->errorRecoveryType(), false, {}, {}};
        }
    }
    else
    {
        // TODO: Not the nicest logic here.
        return {std::nullopt, true, {}, {}};
    }
}

TypeFamilyReductionResult<TypeId> addFamilyFn(std::vector<TypeId> typeParams, std::vector<TypePackId> packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("add type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__add");
}

TypeFamilyReductionResult<TypeId> subFamilyFn(std::vector<TypeId> typeParams, std::vector<TypePackId> packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("sub type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__sub");
}

TypeFamilyReductionResult<TypeId> mulFamilyFn(std::vector<TypeId> typeParams, std::vector<TypePackId> packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("mul type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__mul");
}

TypeFamilyReductionResult<TypeId> divFamilyFn(std::vector<TypeId> typeParams, std::vector<TypePackId> packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("div type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__div");
}

TypeFamilyReductionResult<TypeId> idivFamilyFn(std::vector<TypeId> typeParams, std::vector<TypePackId> packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("integer div type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__idiv");
}

TypeFamilyReductionResult<TypeId> powFamilyFn(std::vector<TypeId> typeParams, std::vector<TypePackId> packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("pow type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__pow");
}

TypeFamilyReductionResult<TypeId> modFamilyFn(std::vector<TypeId> typeParams, std::vector<TypePackId> packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("modulo type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    return numericBinopFamilyFn(typeParams, packParams, ctx, "__mod");
}

TypeFamilyReductionResult<TypeId> andFamilyFn(std::vector<TypeId> typeParams, std::vector<TypePackId> packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("and type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    if (isPending(lhsTy, ctx->solver))
    {
        return {std::nullopt, false, {lhsTy}, {}};
    }
    else if (isPending(rhsTy, ctx->solver))
    {
        return {std::nullopt, false, {rhsTy}, {}};
    }

    // And evalutes to a boolean if the LHS is falsey, and the RHS type if LHS is truthy.
    SimplifyResult filteredLhs = simplifyIntersection(ctx->builtins, ctx->arena, lhsTy, ctx->builtins->falsyType);
    SimplifyResult overallResult = simplifyUnion(ctx->builtins, ctx->arena, rhsTy, filteredLhs.result);
    std::vector<TypeId> blockedTypes(filteredLhs.blockedTypes.begin(), filteredLhs.blockedTypes.end());
    blockedTypes.insert(blockedTypes.end(), overallResult.blockedTypes.begin(), overallResult.blockedTypes.end());
    return {overallResult.result, false, std::move(blockedTypes), {}};
}

TypeFamilyReductionResult<TypeId> orFamilyFn(std::vector<TypeId> typeParams, std::vector<TypePackId> packParams, NotNull<TypeFamilyContext> ctx)
{
    if (typeParams.size() != 2 || !packParams.empty())
    {
        ctx->ice->ice("or type family: encountered a type family instance without the required argument structure");
        LUAU_ASSERT(false);
    }

    TypeId lhsTy = follow(typeParams.at(0));
    TypeId rhsTy = follow(typeParams.at(1));

    if (isPending(lhsTy, ctx->solver))
    {
        return {std::nullopt, false, {lhsTy}, {}};
    }
    else if (isPending(rhsTy, ctx->solver))
    {
        return {std::nullopt, false, {rhsTy}, {}};
    }

    // Or evalutes to the LHS type if the LHS is truthy, and the RHS type if LHS is falsy.
    SimplifyResult filteredLhs = simplifyIntersection(ctx->builtins, ctx->arena, lhsTy, ctx->builtins->truthyType);
    SimplifyResult overallResult = simplifyUnion(ctx->builtins, ctx->arena, rhsTy, filteredLhs.result);
    std::vector<TypeId> blockedTypes(filteredLhs.blockedTypes.begin(), filteredLhs.blockedTypes.end());
    blockedTypes.insert(blockedTypes.end(), overallResult.blockedTypes.begin(), overallResult.blockedTypes.end());
    return {overallResult.result, false, std::move(blockedTypes), {}};
}

BuiltinTypeFamilies::BuiltinTypeFamilies()
    : addFamily{"Add", addFamilyFn}
    , subFamily{"Sub", subFamilyFn}
    , mulFamily{"Mul", mulFamilyFn}
    , divFamily{"Div", divFamilyFn}
    , idivFamily{"FloorDiv", idivFamilyFn}
    , powFamily{"Exp", powFamilyFn}
    , modFamily{"Mod", modFamilyFn}
    , andFamily{"And", andFamilyFn}
    , orFamily{"Or", orFamilyFn}
{
}

void BuiltinTypeFamilies::addToScope(NotNull<TypeArena> arena, NotNull<Scope> scope) const
{
    // make a type function for a two-argument type family
    auto mkBinaryTypeFamily = [&](const TypeFamily* family) {
        TypeId t = arena->addType(GenericType{"T"});
        TypeId u = arena->addType(GenericType{"U"});
        GenericTypeDefinition genericT{t};
        GenericTypeDefinition genericU{u};

        return TypeFun{{genericT, genericU}, arena->addType(TypeFamilyInstanceType{NotNull{family}, {t, u}, {}})};
    };

    scope->exportedTypeBindings[addFamily.name] = mkBinaryTypeFamily(&addFamily);
    scope->exportedTypeBindings[subFamily.name] = mkBinaryTypeFamily(&subFamily);
    scope->exportedTypeBindings[mulFamily.name] = mkBinaryTypeFamily(&mulFamily);
    scope->exportedTypeBindings[divFamily.name] = mkBinaryTypeFamily(&divFamily);
    scope->exportedTypeBindings[idivFamily.name] = mkBinaryTypeFamily(&idivFamily);
    scope->exportedTypeBindings[powFamily.name] = mkBinaryTypeFamily(&powFamily);
    scope->exportedTypeBindings[modFamily.name] = mkBinaryTypeFamily(&modFamily);
}

} // namespace Luau
