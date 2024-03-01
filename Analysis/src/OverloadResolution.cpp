// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/OverloadResolution.h"

#include "Luau/Subtyping.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/TypeFamily.h"

namespace Luau
{

OverloadResolver::OverloadResolver(NotNull<BuiltinTypes> builtinTypes, NotNull<TypeArena> arena, NotNull<Normalizer> normalizer, NotNull<Scope> scope,
    NotNull<InternalErrorReporter> reporter, NotNull<TypeCheckLimits> limits, Location callLocation)
    : builtinTypes(builtinTypes)
    , arena(arena)
    , normalizer(normalizer)
    , scope(scope)
    , ice(reporter)
    , limits(limits)
    , subtyping({builtinTypes, arena, normalizer, ice, scope})
    , callLoc(callLocation)
{
}

std::pair<OverloadResolver::Analysis, TypeId> OverloadResolver::selectOverload(TypeId ty, TypePackId argsPack)
{
    TypeId t = follow(ty);
    if (auto it = get<IntersectionType>(t))
    {
        for (TypeId component : it)
        {
            if (auto ftv = get<FunctionType>(component))
            {
                SubtypingResult r = subtyping.isSubtype(argsPack, ftv->argTypes);
                if (r.isSubtype)
                    return {Analysis::Ok, component};
            }
            else
                continue;
        }
    }

    return {Analysis::OverloadIsNonviable, ty};
}

void OverloadResolver::resolve(TypeId fnTy, const TypePack* args, AstExpr* selfExpr, const std::vector<AstExpr*>* argExprs)
{
    fnTy = follow(fnTy);

    auto it = get<IntersectionType>(fnTy);
    if (!it)
    {
        auto [analysis, errors] = checkOverload(fnTy, args, selfExpr, argExprs);
        add(analysis, fnTy, std::move(errors));
        return;
    }

    for (TypeId ty : it)
    {
        if (resolution.find(ty) != resolution.end())
            continue;

        auto [analysis, errors] = checkOverload(ty, args, selfExpr, argExprs);
        add(analysis, ty, std::move(errors));
    }
}

std::optional<ErrorVec> OverloadResolver::testIsSubtype(const Location& location, TypeId subTy, TypeId superTy)
{
    auto r = subtyping.isSubtype(subTy, superTy);
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
    auto r = subtyping.isSubtype(subTy, superTy);
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
    TypeId fnTy, const TypePack* args, AstExpr* fnLoc, const std::vector<AstExpr*>* argExprs, bool callMetamethodOk)
{
    fnTy = follow(fnTy);

    ErrorVec discard;
    if (get<AnyType>(fnTy) || get<ErrorType>(fnTy) || get<NeverType>(fnTy))
        return {Ok, {}};
    else if (auto fn = get<FunctionType>(fnTy))
        return checkOverload_(fnTy, fn, args, fnLoc, argExprs); // Intentionally split to reduce the stack pressure of this function.
    else if (auto callMm = findMetatableEntry(builtinTypes, discard, fnTy, "__call", callLoc); callMm && callMetamethodOk)
    {
        // Calling a metamethod forwards the `fnTy` as self.
        TypePack withSelf = *args;
        withSelf.head.insert(withSelf.head.begin(), fnTy);

        std::vector<AstExpr*> withSelfExprs = *argExprs;
        withSelfExprs.insert(withSelfExprs.begin(), fnLoc);

        return checkOverload(*callMm, &withSelf, fnLoc, &withSelfExprs, /*callMetamethodOk=*/false);
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

std::pair<OverloadResolver::Analysis, ErrorVec> OverloadResolver::checkOverload_(
    TypeId fnTy, const FunctionType* fn, const TypePack* args, AstExpr* fnExpr, const std::vector<AstExpr*>* argExprs)
{
    FamilyGraphReductionResult result =
        reduceFamilies(fnTy, callLoc, TypeFamilyContext{arena, builtinTypes, scope, normalizer, ice, limits}, /*force=*/true);
    if (!result.errors.empty())
        return {OverloadIsNonviable, result.errors};

    ErrorVec argumentErrors;

    TypeId prospectiveFunction = arena->addType(FunctionType{arena->addTypePack(*args), builtinTypes->anyTypePack});
    SubtypingResult sr = subtyping.isSubtype(fnTy, prospectiveFunction);

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

            const size_t firstUnsatisfiedArgument = argExprs->size();
            const auto [requiredHead, _requiredTail] = flatten(fn->argTypes);

            // If too many arguments were supplied, this overload
            // definitely does not match.
            if (args->head.size() > requiredHead.size())
            {
                auto [minParams, optMaxParams] = getParameterExtents(TxnLog::empty(), fn->argTypes);
                TypeError error{fnExpr->location, CountMismatch{minParams, optMaxParams, args->head.size(), CountMismatch::Arg, false}};

                return {Analysis::ArityMismatch, {error}};
            }

            // If any of the unsatisfied arguments are not supertypes of
            // nil, then this overload does not match.
            for (size_t i = firstUnsatisfiedArgument; i < requiredHead.size(); ++i)
            {
                if (!subtyping.isSubtype(builtinTypes->nilType, requiredHead[i]).isSubtype)
                {
                    auto [minParams, optMaxParams] = getParameterExtents(TxnLog::empty(), fn->argTypes);
                    TypeError error{fnExpr->location, CountMismatch{minParams, optMaxParams, args->head.size(), CountMismatch::Arg, false}};

                    return {Analysis::ArityMismatch, {error}};
                }
            }

            return {Analysis::Ok, {}};
        }
    }

    ErrorVec errors;

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
            argLocation = argExprs->at(nthArgument)->location;

            std::optional<TypeId> failedSubTy = traverseForType(fnTy, reason.subPath, builtinTypes);
            std::optional<TypeId> failedSuperTy = traverseForType(prospectiveFunction, reason.superPath, builtinTypes);

            if (failedSubTy && failedSuperTy)
            {

                switch (shouldSuppressErrors(normalizer, *failedSubTy).orElse(shouldSuppressErrors(normalizer, *failedSuperTy)))
                {
                case ErrorSuppression::Suppress:
                    break;
                case ErrorSuppression::NormalizationFailed:
                    errors.emplace_back(argLocation, NormalizationTooComplex{});
                    // intentionally fallthrough here since we couldn't prove this was error-suppressing
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

        std::optional<TypePackId> failedSubPack = traverseForPack(fnTy, reason.subPath, builtinTypes);
        std::optional<TypePackId> failedSuperPack = traverseForPack(prospectiveFunction, reason.superPath, builtinTypes);

        if (failedSubPack && failedSuperPack)
        {
            LUAU_ASSERT(!argExprs->empty());
            argLocation = argExprs->at(argExprs->size() - 1)->location;

            // TODO extract location from the SubtypingResult path and argExprs
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
        LUAU_ASSERT(!errors.empty());
        arityMismatches.emplace_back(ty, std::move(errors));
        break;
    case OverloadIsNonviable:
        nonviableOverloads.emplace_back(ty, std::move(errors));
        break;
    }
}


} // namespace Luau
