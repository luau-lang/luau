// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/OverloadResolution.h"

#include "Luau/Instantiation2.h"
#include "Luau/Subtyping.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypePack.h"
#include "Luau/TypePath.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"

LUAU_FASTFLAG(LuauLimitUnification)
LUAU_FASTFLAG(LuauReturnMappedGenericPacksFromSubtyping3)
LUAU_FASTFLAG(LuauSubtypingGenericsDoesntUseVariance)
LUAU_FASTFLAG(LuauVariadicAnyPackShouldBeErrorSuppressing)
LUAU_FASTFLAG(LuauSubtypingReportGenericBoundMismatches2)
LUAU_FASTFLAG(LuauSubtypingGenericPacksDoesntUseVariance)
LUAU_FASTFLAGVARIABLE(LuauFilterOverloadsByArity)
LUAU_FASTFLAG(LuauPassBindableGenericsByReference)

namespace Luau
{

OverloadResolver::OverloadResolver(
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    NotNull<Simplifier> simplifier,
    NotNull<Normalizer> normalizer,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    NotNull<Scope> scope,
    NotNull<InternalErrorReporter> reporter,
    NotNull<TypeCheckLimits> limits,
    Location callLocation
)
    : builtinTypes(builtinTypes)
    , arena(arena)
    , simplifier(simplifier)
    , normalizer(normalizer)
    , typeFunctionRuntime(typeFunctionRuntime)
    , scope(scope)
    , ice(reporter)
    , limits(limits)
    , subtyping({builtinTypes, arena, simplifier, normalizer, typeFunctionRuntime, ice})
    , callLoc(callLocation)
{
}

std::pair<OverloadResolver::Analysis, TypeId> OverloadResolver::selectOverload(
    TypeId ty,
    TypePackId argsPack,
    NotNull<DenseHashSet<TypeId>> uniqueTypes,
    bool useFreeTypeBounds
)
{
    if (FFlag::LuauFilterOverloadsByArity)
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
    else
    {
        auto tryOne = [&](TypeId f)
        {
            if (auto ftv = get<FunctionType>(f))
            {
                Subtyping::Variance variance = subtyping.variance;
                subtyping.variance = Subtyping::Variance::Contravariant;
                subtyping.uniqueTypes = uniqueTypes;
                SubtypingResult r;
                if (FFlag::LuauSubtypingGenericsDoesntUseVariance)
                {
                    std::vector<TypeId> generics;
                    generics.reserve(ftv->generics.size());
                    for (TypeId g : ftv->generics)
                    {
                        g = follow(g);
                        if (get<GenericType>(g))
                            generics.emplace_back(g);
                    }
                    r = FFlag::LuauPassBindableGenericsByReference
                        ? subtyping.isSubtype(argsPack, ftv->argTypes, scope, generics)
                                                                   : subtyping.isSubtype_DEPRECATED(argsPack, ftv->argTypes, scope, generics);
                }
                else
                    r = FFlag::LuauPassBindableGenericsByReference ? subtyping.isSubtype(argsPack, ftv->argTypes, scope, {})
                                                               : subtyping.isSubtype_DEPRECATED(argsPack, ftv->argTypes, scope);
                subtyping.variance = variance;

                if (!useFreeTypeBounds && !r.assumedConstraints.empty())
                    return false;

                if (r.isSubtype)
                    return true;
            }

            return false;
        };

        TypeId t = follow(ty);

        if (tryOne(ty))
            return {Analysis::Ok, ty};

        if (auto it = get<IntersectionType>(t))
        {
            for (TypeId component : it)
            {
                if (tryOne(component))
                    return {Analysis::Ok, component};
            }
        }

        return {Analysis::OverloadIsNonviable, ty};
    }
}

void OverloadResolver::resolve(
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

        if (FFlag::LuauFilterOverloadsByArity)
        {
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
        }

        auto [analysis, errors] = checkOverload(ty, args, selfExpr, argExprs, uniqueTypes);
        add(analysis, ty, std::move(errors));
    }
}

std::optional<ErrorVec> OverloadResolver::testIsSubtype(const Location& location, TypeId subTy, TypeId superTy)
{
    auto r = subtyping.isSubtype(subTy, superTy, scope);
    ErrorVec errors;

    if (r.normalizationTooComplex)
        errors.emplace_back(location, NormalizationTooComplex{});

    if (!r.isSubtype)
    {
        switch (shouldSuppressErrors(normalizer, subTy).orElse(shouldSuppressErrors(normalizer, superTy)))
        {
        case ErrorSuppression::Suppress:
            break;
        case ErrorSuppression::NormalizationFailed:
            errors.emplace_back(location, NormalizationTooComplex{});
            // intentionally fallthrough here since we couldn't prove this was error-suppressing
            [[fallthrough]];
        case ErrorSuppression::DoNotSuppress:
            errors.emplace_back(location, TypeMismatch{superTy, subTy});
            break;
        }
    }

    if (errors.empty())
        return std::nullopt;

    return errors;
}

std::optional<ErrorVec> OverloadResolver::testIsSubtype(const Location& location, TypePackId subTy, TypePackId superTy)
{
    auto r = FFlag::LuauPassBindableGenericsByReference ? subtyping.isSubtype(subTy, superTy, scope, {})
                                                        : subtyping.isSubtype_DEPRECATED(subTy, superTy, scope);
    ErrorVec errors;

    if (r.normalizationTooComplex)
        errors.emplace_back(location, NormalizationTooComplex{});

    if (!r.isSubtype)
    {
        switch (shouldSuppressErrors(normalizer, subTy).orElse(shouldSuppressErrors(normalizer, superTy)))
        {
        case ErrorSuppression::Suppress:
            break;
        case ErrorSuppression::NormalizationFailed:
            errors.emplace_back(location, NormalizationTooComplex{});
            // intentionally fallthrough here since we couldn't prove this was error-suppressing
            [[fallthrough]];
        case ErrorSuppression::DoNotSuppress:
            errors.emplace_back(location, TypePackMismatch{superTy, subTy});
            break;
        }
    }

    if (errors.empty())
        return std::nullopt;

    return errors;
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
    const std::optional<TypeId> failedSubTy,
    const std::optional<TypeId> failedSuperTy
) const
{
    if (failedSubTy && failedSuperTy)
    {
        switch (shouldSuppressErrors(normalizer, *failedSubTy).orElse(shouldSuppressErrors(normalizer, *failedSuperTy)))
        {
        case ErrorSuppression::Suppress:
            break;
        case ErrorSuppression::NormalizationFailed:
            errors->emplace_back(argLocation, NormalizationTooComplex{});
            // intentionally fallthrough here since we couldn't prove this was error-suppressing
            [[fallthrough]];
        case ErrorSuppression::DoNotSuppress:
            // TODO extract location from the SubtypingResult path and argExprs
            switch (reason->variance)
            {
            case SubtypingVariance::Covariant:
            case SubtypingVariance::Contravariant:
                errors->emplace_back(argLocation, TypeMismatch{*failedSubTy, *failedSuperTy, TypeMismatch::CovariantContext});
                break;
            case SubtypingVariance::Invariant:
                errors->emplace_back(argLocation, TypeMismatch{*failedSubTy, *failedSuperTy, TypeMismatch::InvariantContext});
                break;
            default:
                LUAU_ASSERT(0);
                break;
            }
        }
    }
}

bool OverloadResolver::isArityCompatible(const TypePackId candidate, const TypePackId desired, NotNull<BuiltinTypes> builtinTypes) const
{
    LUAU_ASSERT(FFlag::LuauFilterOverloadsByArity);

    auto [candidateHead, candidateTail] = flatten(candidate);
    auto [desiredHead, desiredTail] = flatten(desired);

    // Handle mismatched head sizes
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
    LUAU_ASSERT(FFlag::LuauFilterOverloadsByArity);

    Subtyping::Variance variance = subtyping.variance;
    subtyping.variance = Subtyping::Variance::Contravariant;
    subtyping.uniqueTypes = uniqueTypes;
    SubtypingResult r;
    if (FFlag::LuauSubtypingGenericsDoesntUseVariance)
    {
        std::vector<TypeId> generics;
        generics.reserve(ftv->generics.size());
        for (TypeId g : ftv->generics)
        {
            g = follow(g);
            if (get<GenericType>(g))
                generics.emplace_back(g);
        }
        r = FFlag::LuauPassBindableGenericsByReference ? subtyping.isSubtype(argsPack, ftv->argTypes, scope, generics)
                                                       : subtyping.isSubtype_DEPRECATED(argsPack, ftv->argTypes, scope, generics);
    }
    else
        r = FFlag::LuauPassBindableGenericsByReference ? subtyping.isSubtype(argsPack, ftv->argTypes, scope, {})
                                                       : subtyping.isSubtype_DEPRECATED(argsPack, ftv->argTypes, scope);
    subtyping.variance = variance;

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
    TypeFunctionContext context{arena, builtinTypes, scope, simplifier, normalizer, typeFunctionRuntime, ice, limits};
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

        if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
        {
            if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
            {
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
            // If we have an arity mismatch with generic type pack parameters, then subPath matches Args :: Tail :: ...
            // and superPath matches Args :: ...
            else if (reason.subPath.components.size() >= 2 && reason.subPath.components[0] == TypePath::PackField::Arguments &&
                     reason.subPath.components[1] == TypePath::PackField::Tail && reason.superPath.components.size() >= 1 &&
                     reason.superPath.components[0] == TypePath::PackField::Arguments)
            {
                if (const auto [requiredHead, requiredTail] = flatten(fn->argTypes); requiredTail)
                {
                    if (const auto genericTail = get<GenericTypePack>(follow(requiredTail)); genericTail)
                    {
                        // Get the concrete type pack the generic is mapped to
                        const auto mappedGenHead = flatten_DEPRECATED(*requiredTail, sr.mappedGenericPacks_DEPRECATED).first;

                        const auto prospectiveHead = flatten(typ).first;

                        // We're just doing arity checking here
                        // We've flattened the type packs, so we can check prospectiveHead = requiredHead + mappedGenHead
                        // Super path reasoning is just args, so we can ignore the tails
                        const size_t neededHeadSize = requiredHead.size() + mappedGenHead.size();
                        const size_t prospectiveHeadSize = prospectiveHead.size();
                        if (prospectiveHeadSize != neededHeadSize)
                        {
                            TypeError error{fnExpr->location, CountMismatch{neededHeadSize, std::nullopt, prospectiveHeadSize, CountMismatch::Arg}};

                            return {Analysis::ArityMismatch, {error}};
                        }
                    }
                }
            }
            else if (reason.subPath == TypePath::Path{{TypePath::PackField::Arguments, TypePath::PackField::Tail}} &&
                     reason.superPath == justArguments)
            {
                // We have an arity mismatch if the argument tail is a generic type pack
                if (auto fnArgs = get<TypePack>(fn->argTypes))
                {
                    if (get<GenericTypePack>(fnArgs->tail))
                    {
                        auto [minParams, optMaxParams] = getParameterExtents(TxnLog::empty(), fn->argTypes);
                        TypeError error{fnExpr->location, CountMismatch{minParams, optMaxParams, args->head.size(), CountMismatch::Arg}};

                        return {Analysis::ArityMismatch, {std::move(error)}};
                    }
                }
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

            // TODO: This optional can be unwrapped once we clip LuauSubtypingGenericPacksDoesntUseVariance and
            // LuauReturnMappedGenericPacksFromSubtyping3
            std::optional<TypeId> failedSubTy;
            if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
                failedSubTy = traverseForType(fnTy, reason.subPath, builtinTypes, arena);
            else if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
                failedSubTy = traverseForType_DEPRECATED(fnTy, reason.subPath, builtinTypes, NotNull{&sr.mappedGenericPacks_DEPRECATED}, arena);
            else
                failedSubTy = traverseForType_DEPRECATED(fnTy, reason.subPath, builtinTypes);

            // TODO: This optional can be unwrapped once we clip LuauSubtypingGenericPacksDoesntUseVariance and
            // LuauReturnMappedGenericPacksFromSubtyping3
            std::optional<TypeId> failedSuperTy;
            if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
                failedSuperTy = traverseForType(prospectiveFunction, reason.superPath, builtinTypes, arena);
            else if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
                failedSuperTy = traverseForType_DEPRECATED(
                    prospectiveFunction, reason.superPath, builtinTypes, NotNull{&sr.mappedGenericPacks_DEPRECATED}, arena
                );
            else
                failedSuperTy = traverseForType_DEPRECATED(prospectiveFunction, reason.superPath, builtinTypes);

            if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
                maybeEmplaceError(&errors, argLocation, &reason, failedSubTy, failedSuperTy);
            else if (failedSubTy && failedSuperTy)
            {
                switch (shouldSuppressErrors(normalizer, *failedSubTy).orElse(shouldSuppressErrors(normalizer, *failedSuperTy)))
                {
                case ErrorSuppression::Suppress:
                    break;
                case ErrorSuppression::NormalizationFailed:
                    errors.emplace_back(argLocation, NormalizationTooComplex{});
                    // intentionally fallthrough here since we couldn't prove this was error-suppressing
                    [[fallthrough]];
                case ErrorSuppression::DoNotSuppress:
                    // TODO extract location from the SubtypingResult path and argExprs
                    switch (reason.variance)
                    {
                    case SubtypingVariance::Covariant:
                    case SubtypingVariance::Contravariant:
                        errors.emplace_back(argLocation, TypeMismatch{*failedSubTy, *failedSuperTy, TypeMismatch::CovariantContext});
                        break;
                    case SubtypingVariance::Invariant:
                        errors.emplace_back(argLocation, TypeMismatch{*failedSubTy, *failedSuperTy, TypeMismatch::InvariantContext});
                        break;
                    default:
                        LUAU_ASSERT(0);
                        break;
                    }
                }
            }
        }
        else if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3 && reason.superPath.components.size() > 1)
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
                std::optional<TypeId> failedSubTy =
                    FFlag::LuauSubtypingGenericPacksDoesntUseVariance
                        ? traverseForType(fnTy, reason.subPath, builtinTypes, arena)
                        : traverseForType_DEPRECATED(fnTy, reason.subPath, builtinTypes, NotNull{&sr.mappedGenericPacks_DEPRECATED}, arena);
                std::optional<TypeId> failedSuperTy =
                    FFlag::LuauSubtypingGenericPacksDoesntUseVariance
                        ? traverseForType(prospectiveFunction, reason.superPath, builtinTypes, arena)
                        : traverseForType_DEPRECATED(
                              prospectiveFunction, reason.superPath, builtinTypes, NotNull{&sr.mappedGenericPacks_DEPRECATED}, arena
                          );
                maybeEmplaceError(&errors, argLocation, &reason, failedSubTy, failedSuperTy);
            }
        }

        std::optional<TypePackId> failedSubPack;
        if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
            failedSubPack = traverseForPack(fnTy, reason.subPath, builtinTypes, arena);
        else if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
            failedSubPack = traverseForPack_DEPRECATED(fnTy, reason.subPath, builtinTypes, NotNull{&sr.mappedGenericPacks_DEPRECATED}, arena);
        else
            failedSubPack = traverseForPack_DEPRECATED(fnTy, reason.subPath, builtinTypes);

        std::optional<TypePackId> failedSuperPack;
        if (FFlag::LuauSubtypingGenericPacksDoesntUseVariance)
            failedSuperPack = traverseForPack(prospectiveFunction, reason.superPath, builtinTypes, arena);
        else if (FFlag::LuauReturnMappedGenericPacksFromSubtyping3)
            failedSuperPack =
                traverseForPack_DEPRECATED(prospectiveFunction, reason.superPath, builtinTypes, NotNull{&sr.mappedGenericPacks_DEPRECATED}, arena);
        else
            failedSuperPack = traverseForPack_DEPRECATED(prospectiveFunction, reason.superPath, builtinTypes);

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
            if (FFlag::LuauVariadicAnyPackShouldBeErrorSuppressing)
            {
                auto errorSuppression = shouldSuppressErrors(normalizer, *failedSubPack).orElse(shouldSuppressErrors(normalizer, *failedSuperPack));
                if (errorSuppression == ErrorSuppression::Suppress)
                    break;
            }

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

    if (FFlag::LuauSubtypingReportGenericBoundMismatches2)
    {
        for (GenericBoundsMismatch& mismatch : sr.genericBoundsMismatches)
            errors.emplace_back(fnExpr->location, std::move(mismatch));
    }

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
        if (!FFlag::LuauFilterOverloadsByArity)
            LUAU_ASSERT(!errors.empty());
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
    NotNull<Simplifier> simplifier,
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
    auto resolver =
        std::make_unique<OverloadResolver>(builtinTypes, arena, simplifier, normalizer, typeFunctionRuntime, scope, iceReporter, limits, location);

    DenseHashSet<TypeId> uniqueTypes{nullptr};
    auto [status, overload] = resolver->selectOverload(fn, argsPack, NotNull{&uniqueTypes}, /*useFreeTypeBounds*/ false);

    if (status == OverloadResolver::Analysis::Ok)
        return overload;

    if (get<AnyType>(fn) || get<FreeType>(fn))
        return fn;

    return {};
}

SolveResult solveFunctionCall(
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Simplifier> simplifier,
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
        selectOverload(builtinTypes, arena, simplifier, normalizer, typeFunctionRuntime, scope, iceReporter, limits, location, fn, argsPack);
    if (!overloadToUse)
        return {SolveResult::NoMatchingOverload};

    TypePackId resultPack = arena->freshTypePack(scope);

    TypeId inferredTy = arena->addType(FunctionType{TypeLevel{}, argsPack, resultPack});
    Unifier2 u2{NotNull{arena}, builtinTypes, scope, iceReporter};

    const UnifyResult unifyResult = u2.unify(*overloadToUse, inferredTy);

    if (!u2.genericSubstitutions.empty() || !u2.genericPackSubstitutions.empty())
    {
        auto instantiation = std::make_unique<Instantiation2>(arena, std::move(u2.genericSubstitutions), std::move(u2.genericPackSubstitutions));

        std::optional<TypePackId> subst = instantiation->substitute(resultPack);

        if (!subst)
            return {SolveResult::CodeTooComplex};
        else
            resultPack = *subst;
    }

    if (FFlag::LuauLimitUnification)
    {
        switch (unifyResult)
        {
        case Luau::UnifyResult::Ok:
            break;
        case Luau::UnifyResult::OccursCheckFailed:
            return {SolveResult::CodeTooComplex};
        case Luau::UnifyResult::TooComplex:
            return {SolveResult::OccursCheckFailed};
        }
    }
    else
    {
        if (unifyResult != UnifyResult::Ok)
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
