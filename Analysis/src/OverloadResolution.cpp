// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/OverloadResolution.h"

#include "Luau/Common.h"
#include "Luau/Instantiation2.h"
#include "Luau/Subtyping.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypePack.h"
#include "Luau/TypePath.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"

LUAU_FASTFLAG(LuauInstantiationUsesGenericPolarity2)
LUAU_FASTFLAG(LuauNewOverloadResolver2)

namespace Luau
{

SelectedOverload OverloadResolution::getUnambiguousOverload() const
{
    if (ok.size() == 1 && potentialOverloads.size() == 0)
    {
        // Unambiguously: there is exactly one overload that matches
        // without dispatching any more constraints.
        return {
            ok.front(),
            {},
            false,
        };
    }

    if (ok.size() == 0 && potentialOverloads.size() == 1)
    {
        // Unambiguously: there are _no_ overloads that match without
        // dispatching constraints, but there's exactly one that does
        // match without dispatching constraints.
        return {potentialOverloads.front().first, potentialOverloads.front().second, false};
    }

    if (ok.size() > 1)
    {
        // FIXME CLI-180645: We should try to infer a union of return
        // types here so that we get better autocomplete / type
        // inference for the rest of the function.
        return { std::nullopt, {}, false };
    }

    if (potentialOverloads.size() + ok.size() > 1)
    {
        // This is a first case of "ambiguous" overloads: we have at least
        // one overload that matches without constraints, and one that matches
        // with extra constraints.
        //
        // This is the one spot where we return `true`, which callers may use
        // to determine whether they should emit an error or try again later.
        if (ok.empty())
            return { potentialOverloads.front().first, potentialOverloads.front().second, true};
        else
        {
            LUAU_ASSERT(ok.size() == 1);
            return { ok.front(), {}, true };
        }
    }

    LUAU_ASSERT(potentialOverloads.size() + ok.size() == 0);

    // In this case, no overloads are valid. Let's try to pick the one that
    // will cause us to report the most legible errors.
    if (incompatibleOverloads.size() == 1)
    {
        // There's exactly one incompatible overload, but it has
        // the right arity, so just use that. We'll fail type checking
        // but that's ok.
        return { incompatibleOverloads.front().first, {}, false };
    }

    // FIXME: CLI-180645: if `incompatiableOverloads` is non-empty, return a
    // union of all its type packs to the user to use as the inferred return
    // type.
    //
    // FIXME CLI-180638: If we have exactly one function, but there is an
    // arity mismatch, then use that and move on.
    //
    // This is the final case:
    // - There are _no_ overloads whose arguments are clean supertypes, nor
    //   could be supertypes if constraints are dispatched.
    // - There are no overloads that have the right arity but known
    //   incompatible arguments.
    // - There are either no, or more than one, overloads that just have an
    //   arity mismatch.
    // The best we can do here is unify against the error type and move on.
    return { std::nullopt, {}, false };
}

OverloadResolver::OverloadResolver(
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    NotNull<Normalizer> normalizer,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    NotNull<Scope> scope,
    NotNull<InternalErrorReporter> reporter,
    NotNull<TypeCheckLimits> limits,
    Location callLocation
)
    : builtinTypes(builtinTypes)
    , arena(arena)
    , normalizer(normalizer)
    , typeFunctionRuntime(typeFunctionRuntime)
    , scope(scope)
    , ice(reporter)
    , limits(limits)
    , subtyping({builtinTypes, arena, normalizer, typeFunctionRuntime, ice})
    , callLoc(callLocation)
{
}

static bool reasoningIsReturnTypes(const Path& path)
{
    if (path.empty())
        return false;

    const auto& firstComponent = path.components[0];

    const auto field = get_if<TypePath::PackField>(&firstComponent);
    return field != nullptr && *field == TypePath::PackField::Returns;
}

static void ignoreReasoningForReturnType(SubtypingResult& sr)
{
    SubtypingReasonings result{kEmptyReasoning};

    for (const SubtypingReasoning& reasoning: sr.reasoning)
    {
        if (reasoningIsReturnTypes(reasoning.subPath) && reasoningIsReturnTypes(reasoning.superPath))
            continue;

        result.insert(reasoning);
    }

    std::swap(sr.reasoning, result);

    // If the return type mismatch was the only reason for the subtype failure,
    // then we actually consider this a successful match.
    if (sr.reasoning.empty() && sr.genericBoundsMismatches.empty() && sr.errors.empty())
        sr.isSubtype = true;
}

static bool areUnsatisfiedArgumentsOptional(const SubtypingReasonings& reasonings, TypePackId argPack, TypePackId funcArgPack)
{
    // If the two argument lists are incompatible solely because of the argument
    // counts, the reasonings will simply point at the argument lists
    // themselves. If the reasonings point into a pack, it's because that
    // specific argument has an incompatible type.
    if (1 != reasonings.size())
        return false;

    const TypePath::Path justArguments{TypePath::PackField::Arguments};
    const auto& reason = *reasonings.begin();
    if (reason.subPath != justArguments || reason.superPath != justArguments)
        return false;

    const auto [argHead, argTail] = flatten(argPack);
    const auto [funArgHead, funArgTail] = flatten(funcArgPack);

    if (argHead.size() >= funArgHead.size())
        return false;

    for (size_t i = argHead.size(); i < funArgHead.size(); ++i)
    {
        if (!isOptional(funArgHead[i]))
            return false;
    }
    return true;
}

OverloadResolution OverloadResolver::resolveOverload(
    TypeId ty,
    TypePackId argsPack,
    Location fnLocation,
    NotNull<DenseHashSet<TypeId>> uniqueTypes,
    bool useFreeTypeBounds
)
{
    OverloadResolution result;

    ty = follow(ty);

    if (auto it = get<IntersectionType>(ty))
    {
        for (TypeId component : it)
            testFunctionOrUnion(result, component, argsPack, fnLocation, uniqueTypes);
    }
    else
        testFunctionOrUnion(result, ty, argsPack, fnLocation, uniqueTypes);

    return result;
}

static bool isPathOnArgumentList(const Path& path)
{
    auto iter = begin(path.components);
    const auto endIter = end(path.components);

    if (iter == endIter)
        return false;

    if (auto args = get_if<TypePath::PackField>(&*iter); args && *args != TypePath::PackField::Arguments)
        return false;

    ++iter;

    while (iter != endIter)
    {
        if (get_if<TypePath::PackSlice>(&*iter) || get_if<TypePath::GenericPackMapping>(&*iter))
            ++iter;
        else if (const auto packField = get_if<TypePath::PackField>(&*iter); packField && *packField == TypePath::PackField::Tail)
            ++iter;
        else
            return false;
    }

    return true;
}

// Figuring out which argument a particular path points at can be kind of tricky
// due to generic pack substitutions.
static std::optional<size_t> getArgumentIndex(const Path& path, TypeId fnTy)
{
    auto iter = begin(path.components);
    const auto endIter = end(path.components);

    if (iter == endIter)
        return std::nullopt;

    if (auto args = get_if<TypePath::PackField>(&*iter); args && *args != TypePath::PackField::Arguments)
        return std::nullopt;

    ++iter;

    const FunctionType* ft = get<FunctionType>(fnTy);
    LUAU_ASSERT(fnTy);

    size_t result = 0;
    TypeOrPack ty = ft->argTypes;

    while (iter != endIter)
    {
        const auto& component = *iter;
        ++iter;

        if (auto index = get_if<TypePath::Index>(&component))
            return result + index->index;
        else if (auto subst = get_if<TypePath::GenericPackMapping>(&component))
            ty = subst->mappedType;
        else if (auto slice = get_if<TypePath::PackSlice>(&component))
            result += slice->start_index;
        else if (auto packField = get_if<TypePath::PackField>(&component); packField && *packField == TypePath::PackField::Tail)
        {
            // If the path component points at the tail of the pack, we need to
            // advance the count by the length of the current pack.
            TypePackId* tp = get_if<TypePackId>(&ty);
            LUAU_ASSERT(tp);
            if (!tp)
                return std::nullopt;

            // Subtyping flattens out chains of concrete packs when it generates
            // these TypePaths, so we need to do the same here.
            auto packIter = begin(*tp);
            auto packEndIter = end(*tp);
            while (packIter != packEndIter)
            {
                result += 1;
                ++packIter;
            }

            if (!packIter.tail())
                return std::nullopt;

            ty = *packIter.tail();

            continue;
        }
        else
            return std::nullopt;
    }

    return std::nullopt;
}

void OverloadResolver::reportErrors(
    ErrorVec& errors,
    TypeId fnTy,
    Location fnLocation,
    const ModuleName& moduleName,
    TypePackId argPack,
    const std::vector<AstExpr*>& argExprs,
    const SubtypingReasoning& reason
) const
{
    std::optional<size_t> argumentIndex = getArgumentIndex(reason.subPath, fnTy);

    Location argLocation;
    // If the Nth argument directly corresponds to a term in the AST, use its location.
    if (argumentIndex && *argumentIndex < argExprs.size())
        argLocation = argExprs.at(*argumentIndex)->location;
    // Else if any arguments were passed at all, use the location of the last one.
    // TODO: I think we can get the location of the close paren of the whole
    // function call.  That would be much better.
    else if (argExprs.size() != 0)
        argLocation = argExprs.back()->location;
    // If no arguments were present, just use the location of the whole function call.
    else
        argLocation = fnLocation;

    const TypeId prospectiveFunction = arena->addType(FunctionType{argPack, builtinTypes->anyTypePack});

    std::optional<TypePackId> failedSubPack = traverseForPack(prospectiveFunction, reason.superPath, builtinTypes, arena);
    std::optional<TypePackId> failedSuperPack = traverseForPack(fnTy, reason.subPath, builtinTypes, arena);

    if (failedSuperPack && get<GenericTypePack>(*failedSuperPack))
    {
        maybeEmplaceError(&errors, argLocation, moduleName, &reason, failedSuperPack, failedSubPack.value_or(builtinTypes->emptyTypePack));
        return;
    }

    // If the mismatch is on the argument list itself, then the wrong number of parameters were passed.
    if (isPathOnArgumentList(reason.subPath))
    {
        /*
         * If insufficiently many parameters are passed, we expect an empty
         * subPath.
         *
         * If too many parameters are passed, we expect a slice subPath which
         * points to the start of the unsatisfied arguments, and a superPath
         * which points at the tail of the parameter list.
         *
         * Sometimes, the superPath includes generic substitutions.  We need to
         * take this into account when computing the expected parameter count.
         */

        if (!failedSuperPack)
        {
            errors.emplace_back(fnLocation, moduleName, InternalError{"Malformed SubtypingReasoning"});
            return;
        }

        const TypePackId requiredMappedArgs = arena->addTypePack(traverseForFlattenedPack(fnTy, reason.subPath, builtinTypes, arena));
        const auto [paramsHead, paramsTail] = flatten(requiredMappedArgs);
        const auto [argHead, argTail] = flatten(argPack);

        const size_t argCount = argHead.size();
        auto [minParams, optMaxParams] = getParameterExtents(TxnLog::empty(), requiredMappedArgs);

        switch (shouldSuppressErrors(normalizer, argPack))
        {
            case ErrorSuppression::Suppress:
                return;
            case ErrorSuppression::DoNotSuppress:
                break;
            case ErrorSuppression::NormalizationFailed:
                errors.emplace_back(fnLocation, moduleName, NormalizationTooComplex{});
                return;
        }

        if (failedSuperPack)
        {
            switch (shouldSuppressErrors(normalizer, requiredMappedArgs))
            {
                case ErrorSuppression::Suppress:
                    return;
                case ErrorSuppression::DoNotSuppress:
                    break;
                case ErrorSuppression::NormalizationFailed:
                    errors.emplace_back(fnLocation, moduleName, NormalizationTooComplex{});
                    return;
            }
        }

        const bool isVariadic = argTail && Luau::isVariadic(*argTail);

        if (isVariadic)
        {
            // Not actually a count mismatch!  This can happen if the
            // required parameters are a generic pack that has not been
            // satisfied.

            maybeEmplaceError(&errors, argLocation, moduleName, &reason, failedSuperPack, failedSubPack.value_or(builtinTypes->emptyTypePack));
        }
        else
            errors.emplace_back(fnLocation, moduleName, CountMismatch{paramsHead.size(), optMaxParams, argCount, CountMismatch::Arg, isVariadic});

        return;
    }

    if (argumentIndex)
    {
        // If the Nth argument directly corresponds to a term in the AST, use its location.
        if (*argumentIndex < argExprs.size())
            argLocation = argExprs.at(*argumentIndex)->location;
        // Else if any arguments were passed at all, use the location of the last one.
        else if (argExprs.size() != 0)
            argLocation = argExprs.back()->location;
        // If no arguments were present, just use the location of the whole function call.
        else
            argLocation = fnLocation;

        // The first path component should always be PackField::Arguments
        LUAU_ASSERT(reason.subPath.components.size() > 1);
        Path superPathTail = reason.superPath;
        superPathTail.components.erase(superPathTail.components.begin());

        std::optional<TypeOrPack> failedSub = traverse(argPack, superPathTail, builtinTypes, arena);
        std::optional<TypeOrPack> failedSuper = traverse(fnTy, reason.subPath, builtinTypes, arena);

        maybeEmplaceError(&errors, argLocation, moduleName, &reason, failedSuper, failedSub);
        return;
    }

    if (failedSubPack && !failedSuperPack && get<GenericTypePack>(*failedSubPack))
    {
        errors.emplace_back(argLocation, moduleName, TypePackMismatch{*failedSubPack, builtinTypes->emptyTypePack});
    }

    if (failedSubPack && failedSuperPack)
    {
        // If a bug in type inference occurs, we may have a mismatch in the return packs.
        // This happens when inference incorrectly leaves the result type of a function free.
        // If this happens, we don't want to explode, so we'll use the function's location.
        if (argExprs.empty())
            argLocation = fnLocation;
        else
            argLocation = argExprs.at(argExprs.size() - 1)->location;

        auto errorSuppression = shouldSuppressErrors(normalizer, *failedSubPack).orElse(shouldSuppressErrors(normalizer, *failedSuperPack));
        if (errorSuppression == ErrorSuppression::Suppress)
            return;

        switch (reason.variance)
        {
        case SubtypingVariance::Covariant:
            errors.emplace_back(argLocation, moduleName, TypePackMismatch{*failedSubPack, *failedSuperPack});
            break;
        case SubtypingVariance::Contravariant:
            errors.emplace_back(argLocation, moduleName, TypePackMismatch{*failedSuperPack, *failedSubPack});
            break;
        case SubtypingVariance::Invariant:
            errors.emplace_back(argLocation, moduleName, TypePackMismatch{*failedSubPack, *failedSuperPack});
            break;
        default:
            LUAU_ASSERT(0);
            break;
        }
    }
}

// Test a single FunctionType against an argument list.  Reduces type functions
// and does a proper arity check.
void OverloadResolver::testFunction(
    OverloadResolution& result,
    TypeId fnTy,
    TypePackId argsPack,
    Location fnLocation,
    NotNull<DenseHashSet<TypeId>> uniqueTypes
)
{
    fnTy = follow(fnTy);

    // TODO: This seems like the wrong spot to do this check.
    if (is<FreeType, BlockedType, PendingExpansionType>(fnTy))
    {
        std::vector<ConstraintV> constraints; // TODO.  Luckily, these constraints are not yet used.
        result.potentialOverloads.emplace_back(fnTy, std::move(constraints));
        return;
    }

    if (auto tfit = get<TypeFunctionInstanceType>(fnTy); tfit && tfit->state == TypeFunctionInstanceState::Unsolved)
    {
        std::vector<ConstraintV> constraints; // TODO.  Luckily, these constraints are not yet used.
        result.potentialOverloads.emplace_back(fnTy, std::move(constraints));
        return;
    }

    const FunctionType* ftv = get<FunctionType>(fnTy);
    if (!ftv)
    {
        result.nonFunctions.emplace_back(fnTy);
        return;
    }

    if (!isArityCompatible(argsPack, ftv->argTypes, builtinTypes))
    {
        result.arityMismatches.emplace_back(fnTy);
        return;
    }

    TypeFunctionContext context{arena, builtinTypes, scope, normalizer, typeFunctionRuntime, ice, limits};
    FunctionGraphReductionResult reduceResult = reduceTypeFunctions(fnTy, callLoc, NotNull{&context}, /*force=*/true);
    if (!reduceResult.errors.empty())
    {
        result.incompatibleOverloads.emplace_back(fnTy, std::move(reduceResult.errors));
        return;
    }

    TypeId prospectiveFunction = arena->addType(FunctionType{argsPack, builtinTypes->anyTypePack});

    subtyping.uniqueTypes = uniqueTypes;
    SubtypingResult r = subtyping.isSubtype(fnTy, prospectiveFunction, scope);

    // Frustratingly, subtyping does not know about error suppression, so this
    // subtype test will probably fail due to the mismatched return types. Here,
    // we'll prune any SubtypingReasons that have anything to do with the return
    // type.
    //
    // TODO: I'd like to adjust the subtype test to only run across the argument
    // types so that the return pack doesn't get in the way, but that causes the
    // resultant TypePaths to change, so it's not a trivial thing to do.
    ignoreReasoningForReturnType(r);

    if (r.isSubtype)
    {
        if (r.assumedConstraints.empty())
            result.ok.emplace_back(fnTy);
        else
            result.potentialOverloads.emplace_back(fnTy, std::move(r.assumedConstraints));
    }
    else
    {
        if (!r.genericBoundsMismatches.empty())
        {
            ErrorVec errors;
            for (const auto& gbm : r.genericBoundsMismatches)
                errors.emplace_back(fnLocation, gbm);
            result.incompatibleOverloads.emplace_back(fnTy, std::move(errors));
        }
        else if (areUnsatisfiedArgumentsOptional(r.reasoning, argsPack, ftv->argTypes))
        {
            // Important!  Subtyping doesn't know anything about
            // optional arguments.  If the only reason subtyping
            // failed is because optional arguments were not provided,
            // then this overload is actually okay.
            if (r.assumedConstraints.empty())
                result.ok.emplace_back(fnTy);
            else
                result.potentialOverloads.emplace_back(fnTy, std::move(r.assumedConstraints));
        }
        else
            result.incompatibleOverloads.emplace_back(fnTy, std::move(r.reasoning));
    }
}

void OverloadResolver::testFunctionOrUnion(
    OverloadResolution& result,
    TypeId fnTy,
    TypePackId argsPack,
    Location fnLocation,
    NotNull<DenseHashSet<TypeId>> uniqueTypes
)
{
    LUAU_ASSERT(fnTy == follow(fnTy));

    if (auto ut = get<UnionType>(fnTy))
    {
        // A union of functions is a valid overload iff every type within it is a valid overload.

        OverloadResolution innerResult;
        size_t count = 0;
        for (TypeId t : ut)
        {
            ++count;
            testFunctionOrCallMetamethod(innerResult, t, argsPack, fnLocation, uniqueTypes);
        }

        if (count == innerResult.ok.size())
        {
            result.ok.emplace_back(fnTy);
        }
        else if (count == innerResult.ok.size() + innerResult.potentialOverloads.size())
        {
            std::vector<ConstraintV> allConstraints;
            for (const auto& [_t, constraints] : innerResult.potentialOverloads)
                allConstraints.insert(allConstraints.end(), constraints.begin(), constraints.end());

            result.potentialOverloads.emplace_back(fnTy, std::move(allConstraints));
        }
        else
        {
            // FIXME: We should probably report something better here, but it's
            // important for type checking that we include this.
            result.incompatibleOverloads.emplace_back(fnTy, ErrorVec{{fnLocation, CannotCallNonFunction{fnTy}}});
        }
    }
    else
        testFunctionOrCallMetamethod(result, fnTy, argsPack, fnLocation, uniqueTypes);
}

/*
 * A utility function for ::resolveOverload. If a particular overload is a table
 * with a __call metamethod, unwrap that and test it.
 *
 * Note: The __call metamethod can itself be overloaded, but it cannot be a
 * table that overloads __call.  It must be an actual function.
 *
 * TODO: It would be nice to report a good type error in this case.
 */
void OverloadResolver::testFunctionOrCallMetamethod(
    OverloadResolution& result,
    TypeId fnTy,
    TypePackId argsPack,
    Location fnLocation,
    NotNull<DenseHashSet<TypeId>> uniqueTypes
)
{
    fnTy = follow(fnTy);

    ErrorVec dummyErrors;
    if (auto callMetamethod = findMetatableEntry(builtinTypes, dummyErrors, fnTy, "__call", callLoc))
    {
        // Calling a metamethod forwards `fnTy` as self.
        argsPack = arena->addTypePack({fnTy}, argsPack);
        fnTy = follow(*callMetamethod);

        // Handle an overloaded __call metamethod.
        if (auto it = get<IntersectionType>(fnTy))
        {
            for (TypeId component : it)
            {
                component = follow(component);
                result.metamethods.insert(component);
                const FunctionType* fn = get<FunctionType>(component);

                if (fn && !isArityCompatible(argsPack, fn->argTypes, builtinTypes))
                    result.arityMismatches.emplace_back(component);
                else
                    testFunction(result, component, argsPack, fnLocation, uniqueTypes);
            }
            return;
        }

        result.metamethods.insert(fnTy);
    }

    // Handle non-metamethods and metamethods which aren't overloaded.
    testFunction(result, fnTy, argsPack, fnLocation, uniqueTypes);
}

std::pair<OverloadResolver::Analysis, TypeId> OverloadResolver::selectOverload_DEPRECATED(
    TypeId ty,
    TypePackId argsPack,
    NotNull<DenseHashSet<TypeId>> uniqueTypes,
    bool useFreeTypeBounds
)
{
    TypeId t = follow(ty);

    if (const FunctionType* fn = get<FunctionType>(t))
    {
        if (testFunctionTypeForOverloadSelection(fn, uniqueTypes, argsPack, useFreeTypeBounds))
            return {Analysis::Ok, ty};
        else
            return {Analysis::OverloadIsNonviable, ty};
    }
    else if (auto it = get<IntersectionType>(t))
    {
        for (TypeId component : it)
        {
            const FunctionType* fn = get<FunctionType>(follow(component));
            // Only consider function overloads with compatible arities
            if (!fn || !isArityCompatible(argsPack, fn->argTypes, builtinTypes))
                continue;

            if (testFunctionTypeForOverloadSelection(fn, uniqueTypes, argsPack, useFreeTypeBounds))
                return {Analysis::Ok, component};
        }
    }

    return {Analysis::OverloadIsNonviable, ty};
}

void OverloadResolver::resolve_DEPRECATED(
    TypeId fnTy,
    const TypePack* args,
    AstExpr* selfExpr,
    const std::vector<AstExpr*>* argExprs,
    NotNull<DenseHashSet<TypeId>> uniqueTypes
)
{
    fnTy = follow(fnTy);

    auto it = get<IntersectionType>(fnTy);
    if (!it)
    {
        auto [analysis, errors] = checkOverload(fnTy, args, selfExpr, argExprs, uniqueTypes);
        add(analysis, fnTy, std::move(errors));
        return;
    }

    for (TypeId ty : it)
    {
        if (resolution.find(ty) != resolution.end())
            continue;

        if (const FunctionType* fn = get<FunctionType>(follow(ty)))
        {
            // If the overload isn't arity compatible, report the mismatch and don't do more work
            const TypePackId argPack = arena->addTypePack(*args);
            if (!isArityCompatible(argPack, fn->argTypes, builtinTypes))
            {
                add(ArityMismatch, ty, {});
                continue;
            }
        }

        auto [analysis, errors] = checkOverload(ty, args, selfExpr, argExprs, uniqueTypes);
        add(analysis, ty, std::move(errors));
    }
}

std::pair<OverloadResolver::Analysis, ErrorVec> OverloadResolver::checkOverload(
    TypeId fnTy,
    const TypePack* args,
    AstExpr* fnLoc,
    const std::vector<AstExpr*>* argExprs,
    NotNull<DenseHashSet<TypeId>> uniqueTypes,
    bool callMetamethodOk
)
{
    fnTy = follow(fnTy);

    ErrorVec discard;
    if (get<AnyType>(fnTy) || get<ErrorType>(fnTy) || get<NeverType>(fnTy))
        return {Ok, {}};
    else if (auto fn = get<FunctionType>(fnTy))
        return checkOverload_(fnTy, fn, args, fnLoc, argExprs, uniqueTypes); // Intentionally split to reduce the stack pressure of this function.
    else if (auto callMm = findMetatableEntry(builtinTypes, discard, fnTy, "__call", callLoc); callMm && callMetamethodOk)
    {
        // Calling a metamethod forwards the `fnTy` as self.
        TypePack withSelf = *args;
        withSelf.head.insert(withSelf.head.begin(), fnTy);

        std::vector<AstExpr*> withSelfExprs = *argExprs;
        withSelfExprs.insert(withSelfExprs.begin(), fnLoc);

        return checkOverload(*callMm, &withSelf, fnLoc, &withSelfExprs, uniqueTypes, /*callMetamethodOk=*/false);
    }
    else
        return {TypeIsNotAFunction, {}}; // Intentionally empty. We can just fabricate the type error later on.
}

bool OverloadResolver::isLiteral(AstExpr* expr)
{
    if (auto group = expr->as<AstExprGroup>())
        return isLiteral(group->expr);
    else if (auto assertion = expr->as<AstExprTypeAssertion>())
        return isLiteral(assertion->expr);

    return expr->is<AstExprConstantNil>() || expr->is<AstExprConstantBool>() || expr->is<AstExprConstantNumber>() ||
           expr->is<AstExprConstantString>() || expr->is<AstExprFunction>() || expr->is<AstExprTable>();
}

void OverloadResolver::maybeEmplaceError(
    ErrorVec* errors,
    Location argLocation,
    const SubtypingReasoning* reason,
    const std::optional<TypeId> wantedType,
    const std::optional<TypeId> givenType
) const
{
    // This is a temporary compatibility shim for the old API. It's ok to pass
    // an empty ModuleName here because the caller of
    // OverloadResolver::resolve() will overwrite the moduleName of any errors
    // that are reported.
    return maybeEmplaceError(errors, argLocation, ModuleName{}, reason, wantedType, givenType);
}

void OverloadResolver::maybeEmplaceError(
    ErrorVec* errors,
    Location argLocation,
    const ModuleName& moduleName,
    const SubtypingReasoning* reason,
    const std::optional<TypeId> wantedType,
    const std::optional<TypeId> givenType
) const
{
    if (wantedType && givenType)
    {
        switch (shouldSuppressErrors(normalizer, *wantedType).orElse(shouldSuppressErrors(normalizer, *givenType)))
        {
        case ErrorSuppression::Suppress:
            break;
        case ErrorSuppression::NormalizationFailed:
            errors->emplace_back(argLocation, moduleName, NormalizationTooComplex{});
            // intentionally fallthrough here since we couldn't prove this was error-suppressing
            [[fallthrough]];
        case ErrorSuppression::DoNotSuppress:
            // TODO extract location from the SubtypingResult path and argExprs
            switch (reason->variance)
            {
            case SubtypingVariance::Covariant:
            case SubtypingVariance::Contravariant:
                errors->emplace_back(argLocation, moduleName, TypeMismatch{*wantedType, *givenType, TypeMismatch::CovariantContext});
                break;
            case SubtypingVariance::Invariant:
                errors->emplace_back(argLocation, moduleName, TypeMismatch{*wantedType, *givenType, TypeMismatch::InvariantContext});
                break;
            default:
                LUAU_ASSERT(0);
                break;
            }
        }
    }
}

void OverloadResolver::maybeEmplaceError(
    ErrorVec* errors,
    Location argLocation,
    const ModuleName& moduleName,
    const SubtypingReasoning* reason,
    const std::optional<TypePackId> wantedTp,
    const std::optional<TypePackId> givenTp
) const
{
    if (!wantedTp || !givenTp)
        return;
    switch (shouldSuppressErrors(normalizer, *wantedTp).orElse(shouldSuppressErrors(normalizer, *givenTp)))
    {
    case ErrorSuppression::Suppress:
        break;
    case ErrorSuppression::NormalizationFailed:
        errors->emplace_back(argLocation, moduleName, NormalizationTooComplex{});
        break;
    case ErrorSuppression::DoNotSuppress:
        errors->emplace_back(argLocation, moduleName, TypePackMismatch{*wantedTp, *givenTp});
        break;
    }
}

void OverloadResolver::maybeEmplaceError(
    ErrorVec* errors,
    Location argLocation,
    const ModuleName& moduleName,
    const SubtypingReasoning* reason,
    const std::optional<TypeOrPack> wantedType,
    const std::optional<TypeOrPack> givenType
) const
{
    if (!wantedType || !givenType)
        return;

    const TypeId* wantedTy = get_if<TypeId>(&*wantedType);
    const TypeId* givenTy = get_if<TypeId>(&*givenType);
    if (wantedTy && givenTy)
        return maybeEmplaceError(errors, argLocation, moduleName, reason, std::optional<TypeId>{*wantedTy}, std::optional<TypeId>{*givenTy});

    const TypePackId* wantedTp = get_if<TypePackId>(&*wantedType);
    const TypePackId* givenTp = get_if<TypePackId>(&*givenType);

    if (wantedTp && givenTp)
        return maybeEmplaceError(errors, argLocation, moduleName, reason, std::optional<TypePackId>{*wantedTp}, std::optional<TypePackId>{*givenTp});
}

bool OverloadResolver::isArityCompatible(const TypePackId candidate, const TypePackId desired, NotNull<BuiltinTypes> builtinTypes) const
{
    auto [candidateHead, candidateTail] = flatten(candidate);
    auto [desiredHead, desiredTail] = flatten(desired);

    // Insufficiently many parameters were passed
    if (candidateHead.size() < desiredHead.size())
    {
        if (candidateTail)
            return true; // A tail can fill in remaining values

        // If the candidate is shorter than desired and has no tail, it can only match if the extra desired args are all optional
        for (size_t i = candidateHead.size(); i < desiredHead.size(); ++i)
        {
            if (const TypeId ty = follow(desiredHead[i]); !isOptionalType(ty, builtinTypes))
                return false;
        }
    }

    // Too many parameters were passed
    if (FFlag::LuauNewOverloadResolver2)
    {
        if (candidateHead.size() > desiredHead.size())
        {
            // If the function being called accepts a variadic or generic tail, then the arities match.
            return desiredTail.has_value();
        }
    }
    else
    {
        if (desiredTail && candidateHead.size() <= desiredHead.size() && !candidateTail)
        {
            // A non-tail candidate can't match a desired tail unless the tail accepts nils
            // We don't allow generic packs to implicitly accept an empty pack here
            TypePackId desiredTailTP = follow(*desiredTail);

            if (desiredTailTP == builtinTypes->unknownTypePack || desiredTailTP == builtinTypes->anyTypePack)
                return true;

            if (const VariadicTypePack* vtp = get<VariadicTypePack>(desiredTailTP))
                return vtp->ty == builtinTypes->nilType;

            return false;
        }
    }

    // There aren't any other failure conditions; we don't care if we pass more args than needed

    return true;
}

bool OverloadResolver::testFunctionTypeForOverloadSelection(
    const FunctionType* ftv,
    NotNull<DenseHashSet<TypeId>> uniqueTypes,
    TypePackId argsPack,
    bool useFreeTypeBounds
)
{
    subtyping.uniqueTypes = uniqueTypes;
    std::vector<TypeId> generics;
    generics.reserve(ftv->generics.size());
    for (TypeId g : ftv->generics)
    {
        g = follow(g);
        if (get<GenericType>(g))
            generics.emplace_back(g);
    }
    SubtypingResult r = subtyping.isSubtype(argsPack, ftv->argTypes, scope, generics);

    if (!useFreeTypeBounds && !r.assumedConstraints.empty())
        return false;

    if (r.isSubtype)
        return true;

    return false;
}

std::pair<OverloadResolver::Analysis, ErrorVec> OverloadResolver::checkOverload_(
    TypeId fnTy,
    const FunctionType* fn,
    const TypePack* args,
    AstExpr* fnExpr,
    const std::vector<AstExpr*>* argExprs,
    NotNull<DenseHashSet<TypeId>> uniqueTypes
)
{
    TypeFunctionContext context{arena, builtinTypes, scope, normalizer, typeFunctionRuntime, ice, limits};
    FunctionGraphReductionResult result = reduceTypeFunctions(fnTy, callLoc, NotNull{&context}, /*force=*/true);
    if (!result.errors.empty())
        return {OverloadIsNonviable, result.errors};

    ErrorVec argumentErrors;
    TypePackId typ = arena->addTypePack(*args);

    TypeId prospectiveFunction = arena->addType(FunctionType{typ, builtinTypes->anyTypePack});
    subtyping.uniqueTypes = uniqueTypes;
    SubtypingResult sr = subtyping.isSubtype(fnTy, prospectiveFunction, scope);

    if (sr.isSubtype)
        return {Analysis::Ok, {}};

    if (1 == sr.reasoning.size())
    {
        const SubtypingReasoning& reason = *sr.reasoning.begin();

        const TypePath::Path justArguments{TypePath::PackField::Arguments};

        if (reason.subPath == justArguments && reason.superPath == justArguments)
        {
            // If the subtype test failed only due to an arity mismatch,
            // it is still possible that this function call is okay.
            // Subtype testing does not know anything about optional
            // function arguments.
            //
            // This can only happen if the actual function call has a
            // finite set of arguments which is too short for the
            // function being called.  If all of those unsatisfied
            // function arguments are options, then this function call
            // is ok.

            const size_t firstUnsatisfiedArgument = args->head.size();
            const auto [requiredHead, requiredTail] = flatten(fn->argTypes);

            bool isVariadic = requiredTail && Luau::isVariadic(*requiredTail);

            // If too many arguments were supplied, this overload
            // definitely does not match.
            if (args->head.size() > requiredHead.size())
            {
                auto [minParams, optMaxParams] = getParameterExtents(TxnLog::empty(), fn->argTypes);

                TypeError error{fnExpr->location, CountMismatch{minParams, optMaxParams, args->head.size(), CountMismatch::Arg, isVariadic}};

                return {Analysis::ArityMismatch, {std::move(error)}};
            }

            // If any of the unsatisfied arguments are not supertypes of
            // nil or are `unknown`, then this overload does not match.
            for (size_t i = firstUnsatisfiedArgument; i < requiredHead.size(); ++i)
            {
                if (get<UnknownType>(follow(requiredHead[i])) || !subtyping.isSubtype(builtinTypes->nilType, requiredHead[i], scope).isSubtype)
                {
                    auto [minParams, optMaxParams] = getParameterExtents(TxnLog::empty(), fn->argTypes);
                    for (auto arg : fn->argTypes)
                        if (get<UnknownType>(follow(arg)))
                            minParams += 1;

                    TypeError error{fnExpr->location, CountMismatch{minParams, optMaxParams, args->head.size(), CountMismatch::Arg, isVariadic}};

                    return {Analysis::ArityMismatch, {std::move(error)}};
                }
            }

            // All unsatisfied arguments are supertypes of nil.  This overload is a valid match.
            return {Analysis::Ok, {}};
        }

        const bool subPathArgTail = matchesPrefix(Path({TypePath::PackField::Arguments, TypePath::PackField::Tail}), reason.subPath);
        const bool superPathArgs = matchesPrefix(Path(TypePath::PackField::Arguments), reason.superPath);
        const TypePath::Component& lastSubComponent = reason.subPath.components.back();
        const bool subEndsInGenericPackMapping = get_if<TypePath::GenericPackMapping>(&lastSubComponent) != nullptr;

        // If the function's argument list ends with a generic pack, and
        // the subtype test failed because of that, we need to check the
        // pack that the generic was mapped to in order to report an
        // accurate CountMismatch error.
        if (subPathArgTail && superPathArgs && subEndsInGenericPackMapping)
        {
            const TypePack requiredMappedArgs = traverseForFlattenedPack(fnTy, reason.subPath, builtinTypes, arena);
            const std::vector<TypeId> prospectiveHead = flatten(typ).first;

            const size_t requiredHeadSize = requiredMappedArgs.head.size();
            const size_t prospectiveHeadSize = prospectiveHead.size();

            if (prospectiveHeadSize != requiredHeadSize)
            {
                TypeError error{
                    fnExpr->location,
                    CountMismatch{
                        requiredHeadSize,
                        requiredMappedArgs.tail.has_value() ? std::nullopt : std::optional{requiredHeadSize},
                        prospectiveHeadSize,
                        CountMismatch::Arg
                    }
                };

                return {Analysis::ArityMismatch, {std::move(error)}};
            }
        }
    }

    ErrorVec errors;

    // Translate SubtypingReasonings into TypeErrors that could be reported.
    for (const SubtypingReasoning& reason : sr.reasoning)
    {
        /* The return type of our prospective function is always
         * any... so any subtype failures here can only arise from
         * argument type mismatches.
         */

        Location argLocation;
        if (reason.superPath.components.size() <= 1)
            break;

        if (const Luau::TypePath::Index* pathIndexComponent = get_if<Luau::TypePath::Index>(&reason.superPath.components.at(1)))
        {
            size_t nthArgument = pathIndexComponent->index;
            // if the nth type argument to the function is less than the number of ast expressions we passed to the function
            // we should be able to pull out the location of the argument
            // If the nth type argument to the function is out of range of the ast expressions we passed to the function
            // e.g. table.pack(functionThatReturnsMultipleArguments(arg1, arg2, ....)), default to the location of the last passed expression
            // If we passed no expression arguments to the call, default to the location of the function expression.
            argLocation = nthArgument < argExprs->size() ? argExprs->at(nthArgument)->location
                          : argExprs->size() != 0        ? argExprs->back()->location
                                                         : fnExpr->location;

            std::optional<TypeId> failedSubTy = traverseForType(fnTy, reason.subPath, builtinTypes, arena);

            std::optional<TypeId> failedSuperTy = traverseForType(prospectiveFunction, reason.superPath, builtinTypes, arena);

            maybeEmplaceError(&errors, argLocation, &reason, failedSubTy, failedSuperTy);
        }
        else if (reason.superPath.components.size() > 1)
        {
            // traverseForIndex only has a value if path is of form [...PackSlice, Index]
            if (const auto index =
                    traverseForIndex(TypePath::Path{std::vector(reason.superPath.components.begin() + 1, reason.superPath.components.end())}))
            {
                if (index < argExprs->size())
                    argLocation = argExprs->at(*index)->location;
                else if (argExprs->size() != 0)
                    argLocation = argExprs->back()->location;
                else
                {
                    // this should never happen
                    LUAU_ASSERT(false);
                    argLocation = fnExpr->location;
                }
                std::optional<TypeId> failedSubTy = traverseForType(fnTy, reason.subPath, builtinTypes, arena);
                std::optional<TypeId> failedSuperTy = traverseForType(prospectiveFunction, reason.superPath, builtinTypes, arena);
                maybeEmplaceError(&errors, argLocation, &reason, failedSubTy, failedSuperTy);
            }
        }

        std::optional<TypePackId> failedSubPack = traverseForPack(fnTy, reason.subPath, builtinTypes, arena);

        std::optional<TypePackId> failedSuperPack = traverseForPack(prospectiveFunction, reason.superPath, builtinTypes, arena);

        if (failedSubPack && failedSuperPack)
        {
            // If a bug in type inference occurs, we may have a mismatch in the return packs.
            // This happens when inference incorrectly leaves the result type of a function free.
            // If this happens, we don't want to explode, so we'll use the function's location.
            if (argExprs->empty())
                argLocation = fnExpr->location;
            else
                argLocation = argExprs->at(argExprs->size() - 1)->location;

            // TODO extract location from the SubtypingResult path and argExprs
            auto errorSuppression = shouldSuppressErrors(normalizer, *failedSubPack).orElse(shouldSuppressErrors(normalizer, *failedSuperPack));
            if (errorSuppression == ErrorSuppression::Suppress)
                break;

            switch (reason.variance)
            {
            case SubtypingVariance::Covariant:
                errors.emplace_back(argLocation, TypePackMismatch{*failedSubPack, *failedSuperPack});
                break;
            case SubtypingVariance::Contravariant:
                errors.emplace_back(argLocation, TypePackMismatch{*failedSuperPack, *failedSubPack});
                break;
            case SubtypingVariance::Invariant:
                errors.emplace_back(argLocation, TypePackMismatch{*failedSubPack, *failedSuperPack});
                break;
            default:
                LUAU_ASSERT(0);
                break;
            }
        }
    }

    for (GenericBoundsMismatch& mismatch : sr.genericBoundsMismatches)
        errors.emplace_back(fnExpr->location, std::move(mismatch));

    return {Analysis::OverloadIsNonviable, std::move(errors)};
}

size_t OverloadResolver::indexof(Analysis analysis)
{
    switch (analysis)
    {
    case Ok:
        return ok.size();
    case TypeIsNotAFunction:
        return nonFunctions.size();
    case ArityMismatch:
        return arityMismatches.size();
    case OverloadIsNonviable:
        return nonviableOverloads.size();
    }

    ice->ice("Inexhaustive switch in FunctionCallResolver::indexof");
}

void OverloadResolver::add(Analysis analysis, TypeId ty, ErrorVec&& errors)
{
    resolution.insert(ty, {analysis, indexof(analysis)});

    switch (analysis)
    {
    case Ok:
        LUAU_ASSERT(errors.empty());
        ok.push_back(ty);
        break;
    case TypeIsNotAFunction:
        LUAU_ASSERT(errors.empty());
        nonFunctions.push_back(ty);
        break;
    case ArityMismatch:
        arityMismatches.emplace_back(ty, std::move(errors));
        break;
    case OverloadIsNonviable:
        nonviableOverloads.emplace_back(ty, std::move(errors));
        break;
    }
}

// we wrap calling the overload resolver in a separate function to reduce overall stack pressure in `solveFunctionCall`.
// this limits the lifetime of `OverloadResolver`, a large type, to only as long as it is actually needed.
static std::optional<TypeId> selectOverload(
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    NotNull<Normalizer> normalizer,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    NotNull<Scope> scope,
    NotNull<InternalErrorReporter> iceReporter,
    NotNull<TypeCheckLimits> limits,
    const Location& location,
    TypeId fn,
    TypePackId argsPack
)
{
    auto resolver = std::make_unique<OverloadResolver>(builtinTypes, arena, normalizer, typeFunctionRuntime, scope, iceReporter, limits, location);

    DenseHashSet<TypeId> uniqueTypes{nullptr};
    auto [status, overload] = resolver->selectOverload_DEPRECATED(fn, argsPack, NotNull{&uniqueTypes}, /*useFreeTypeBounds*/ false);

    if (status == OverloadResolver::Analysis::Ok)
        return overload;

    if (get<AnyType>(fn) || get<FreeType>(fn))
        return fn;

    return {};
}

SolveResult solveFunctionCall_DEPRECATED(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Normalizer> normalizer,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    NotNull<InternalErrorReporter> iceReporter,
    NotNull<TypeCheckLimits> limits,
    NotNull<Scope> scope,
    const Location& location,
    TypeId fn,
    TypePackId argsPack
)
{
    std::optional<TypeId> overloadToUse =
        selectOverload(builtinTypes, arena, normalizer, typeFunctionRuntime, scope, iceReporter, limits, location, fn, argsPack);
    if (!overloadToUse)
        return {SolveResult::NoMatchingOverload};

    TypePackId resultPack = arena->freshTypePack(scope);

    TypeId inferredTy = arena->addType(FunctionType{TypeLevel{}, argsPack, resultPack});
    Unifier2 u2{NotNull{arena}, builtinTypes, scope, iceReporter};

    const UnifyResult unifyResult = u2.unify(*overloadToUse, inferredTy);

    if (!u2.genericSubstitutions.empty() || !u2.genericPackSubstitutions.empty())
    {
        if (FFlag::LuauInstantiationUsesGenericPolarity2)
        {
            Subtyping subtyping{builtinTypes, arena, normalizer, typeFunctionRuntime, iceReporter};
            std::optional<TypePackId> subst = instantiate2(
                arena, std::move(u2.genericSubstitutions), std::move(u2.genericPackSubstitutions), NotNull{&subtyping}, scope, resultPack
            );
            if (!subst)
                return {SolveResult::CodeTooComplex};
            else
                resultPack = *subst;
        }
        else
        {
            auto instantiation = std::make_unique<Instantiation2>(
                arena, std::move(u2.genericSubstitutions), std::move(u2.genericPackSubstitutions));

            std::optional<TypePackId> subst = instantiation->substitute(resultPack);

            if (!subst)
                return {SolveResult::CodeTooComplex};
            else
                resultPack = *subst;
        }
    }

    switch (unifyResult)
    {
    case Luau::UnifyResult::Ok:
        break;
    case Luau::UnifyResult::OccursCheckFailed:
        return {SolveResult::CodeTooComplex};
    case Luau::UnifyResult::TooComplex:
        return {SolveResult::OccursCheckFailed};
    }

    SolveResult result;
    result.result = SolveResult::Ok;
    result.typePackId = resultPack;

    LUAU_ASSERT(overloadToUse);
    result.overloadToUse = *overloadToUse;
    result.inferredTy = inferredTy;
    result.expandedFreeTypes = std::move(u2.expandedFreeTypes);

    return result;
}

} // namespace Luau
