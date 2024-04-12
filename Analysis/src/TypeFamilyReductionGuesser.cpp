// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeFamilyReductionGuesser.h"

#include "Luau/DenseHash.h"
#include "Luau/Normalize.h"
#include "Luau/TypeFamily.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/VecDeque.h"
#include "Luau/VisitType.h"

#include <iostream>
#include <optional>
#include <ostream>

namespace Luau
{
struct InstanceCollector2 : TypeOnceVisitor
{
    VecDeque<TypeId> tys;
    VecDeque<TypePackId> tps;
    DenseHashSet<TypeId> cyclicInstance{nullptr};
    DenseHashSet<TypeId> instanceArguments{nullptr};

    bool visit(TypeId ty, const TypeFamilyInstanceType& it) override
    {
        // TypeOnceVisitor performs a depth-first traversal in the absence of
        // cycles. This means that by pushing to the front of the queue, we will
        // try to reduce deeper instances first if we start with the first thing
        // in the queue. Consider Add<Add<Add<number, number>, number>, number>:
        // we want to reduce the innermost Add<number, number> instantiation
        // first.
        tys.push_front(ty);
        for (auto t : it.typeArguments)
            instanceArguments.insert(follow(t));
        return true;
    }

    void cycle(TypeId ty) override
    {
        /// Detected cyclic type pack
        TypeId t = follow(ty);
        if (get<TypeFamilyInstanceType>(t))
            cyclicInstance.insert(t);
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



TypeFamilyReductionGuesser::TypeFamilyReductionGuesser(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins, NotNull<Normalizer> normalizer)
    : arena(arena)
    , builtins(builtins)
    , normalizer(normalizer)
{
}

bool TypeFamilyReductionGuesser::isFunctionGenericsSaturated(const FunctionType& ftv, DenseHashSet<TypeId>& argsUsed)
{
    bool sameSize = ftv.generics.size() == argsUsed.size();
    bool allGenericsAppear = true;
    for (auto gt : ftv.generics)
        allGenericsAppear = allGenericsAppear || argsUsed.contains(gt);
    return sameSize && allGenericsAppear;
}

void TypeFamilyReductionGuesser::dumpGuesses()
{
    for (auto [tf, t] : familyReducesTo)
        printf("Type family %s ~~> %s\n", toString(tf).c_str(), toString(t).c_str());
    for (auto [t, t_] : substitutable)
        printf("Substitute %s for %s\n", toString(t).c_str(), toString(t_).c_str());
}

std::optional<TypeId> TypeFamilyReductionGuesser::guess(TypeId typ)
{
    std::optional<TypeId> guessedType = guessType(typ);

    if (!guessedType.has_value())
        return {};

    TypeId guess = follow(*guessedType);
    if (get<TypeFamilyInstanceType>(guess))
        return {};

    return guess;
}

std::optional<TypePackId> TypeFamilyReductionGuesser::guess(TypePackId tp)
{
    auto [head, tail] = flatten(tp);

    std::vector<TypeId> guessedHead;
    guessedHead.reserve(head.size());

    for (auto typ : head)
    {
        std::optional<TypeId> guessedType = guessType(typ);

        if (!guessedType.has_value())
            return {};

        TypeId guess = follow(*guessedType);
        if (get<TypeFamilyInstanceType>(guess))
            return {};

        guessedHead.push_back(*guessedType);
    }

    return arena->addTypePack(TypePack{guessedHead, tail});
}

TypeFamilyReductionGuessResult TypeFamilyReductionGuesser::guessTypeFamilyReductionForFunction(
    const AstExprFunction& expr, const FunctionType* ftv, TypeId retTy)
{
    InstanceCollector2 collector;
    collector.traverse(retTy);
    toInfer = std::move(collector.tys);
    cyclicInstances = std::move(collector.cyclicInstance);

    if (isFunctionGenericsSaturated(*ftv, collector.instanceArguments))
        return TypeFamilyReductionGuessResult{{}, nullptr, false};
    infer();

    std::vector<std::pair<std::string, TypeId>> results;
    std::vector<TypeId> args;
    for (TypeId t : ftv->argTypes)
        args.push_back(t);

    //    Submit a guess for arg types
    for (size_t i = 0; i < expr.args.size; i++)
    {
        TypeId argTy;
        AstLocal* local = expr.args.data[i];
        if (i >= args.size())
            continue;

        argTy = args[i];
        std::optional<TypeId> guessedType = guessType(argTy);
        if (!guessedType.has_value())
            continue;
        TypeId guess = follow(*guessedType);
        if (get<TypeFamilyInstanceType>(guess))
            continue;

        results.push_back({local->name.value, guess});
    }

    // Submit a guess for return types
    TypeId recommendedAnnotation;
    std::optional<TypeId> guessedReturnType = guessType(retTy);
    if (!guessedReturnType.has_value())
        recommendedAnnotation = builtins->unknownType;
    else
        recommendedAnnotation = follow(*guessedReturnType);
    if (auto t = get<TypeFamilyInstanceType>(recommendedAnnotation))
        recommendedAnnotation = builtins->unknownType;

    toInfer.clear();
    cyclicInstances.clear();
    familyReducesTo.clear();
    substitutable.clear();

    return TypeFamilyReductionGuessResult{results, recommendedAnnotation};
}

std::optional<TypeId> TypeFamilyReductionGuesser::guessType(TypeId arg)
{
    TypeId t = follow(arg);
    if (substitutable.contains(t))
    {
        TypeId subst = follow(substitutable[t]);
        if (subst == t || substitutable.contains(subst))
            return subst;
        else if (!get<TypeFamilyInstanceType>(subst))
            return subst;
        else
            return guessType(subst);
    }
    if (get<TypeFamilyInstanceType>(t))
    {
        if (familyReducesTo.contains(t))
            return familyReducesTo[t];
    }
    return {};
}

bool TypeFamilyReductionGuesser::isNumericBinopFamily(const TypeFamilyInstanceType& instance)
{
    return instance.family->name == "add" || instance.family->name == "sub" || instance.family->name == "mul" || instance.family->name == "div" ||
           instance.family->name == "idiv" || instance.family->name == "pow" || instance.family->name == "mod";
}

bool TypeFamilyReductionGuesser::isComparisonFamily(const TypeFamilyInstanceType& instance)
{
    return instance.family->name == "lt" || instance.family->name == "le" || instance.family->name == "eq";
}

bool TypeFamilyReductionGuesser::isOrAndFamily(const TypeFamilyInstanceType& instance)
{
    return instance.family->name == "or" || instance.family->name == "and";
}

bool TypeFamilyReductionGuesser::isNotFamily(const TypeFamilyInstanceType& instance)
{
    return instance.family->name == "not";
}

bool TypeFamilyReductionGuesser::isLenFamily(const TypeFamilyInstanceType& instance)
{
    return instance.family->name == "len";
}

bool TypeFamilyReductionGuesser::isUnaryMinus(const TypeFamilyInstanceType& instance)
{
    return instance.family->name == "unm";
}

// Operand is assignable if it looks like a cyclic family instance, or a generic type
bool TypeFamilyReductionGuesser::operandIsAssignable(TypeId ty)
{
    if (get<TypeFamilyInstanceType>(ty))
        return true;
    if (get<GenericType>(ty))
        return true;
    if (cyclicInstances.contains(ty))
        return true;
    return false;
}

std::shared_ptr<const NormalizedType> TypeFamilyReductionGuesser::normalize(TypeId ty)
{
    return normalizer->normalize(ty);
}


std::optional<TypeId> TypeFamilyReductionGuesser::tryAssignOperandType(TypeId ty)
{
    // Because we collect innermost instances first, if we see a typefamily instance as an operand,
    // We try to check if we guessed a type for it
    if (auto tfit = get<TypeFamilyInstanceType>(ty))
    {
        if (familyReducesTo.contains(ty))
            return {familyReducesTo[ty]};
    }

    // If ty is a generic, we need to check if we inferred a substitution
    if (auto gt = get<GenericType>(ty))
    {
        if (substitutable.contains(ty))
            return {substitutable[ty]};
    }

    // If we cannot substitute a type for this value, we return an empty optional
    return {};
}

void TypeFamilyReductionGuesser::step()
{
    TypeId t = toInfer.front();
    toInfer.pop_front();
    t = follow(t);
    if (auto tf = get<TypeFamilyInstanceType>(t))
        inferTypeFamilySubstitutions(t, tf);
}

void TypeFamilyReductionGuesser::infer()
{
    while (!done())
        step();
}

bool TypeFamilyReductionGuesser::done()
{
    return toInfer.empty();
}

void TypeFamilyReductionGuesser::inferTypeFamilySubstitutions(TypeId ty, const TypeFamilyInstanceType* instance)
{

    TypeFamilyInferenceResult result;
    LUAU_ASSERT(instance);
    // TODO: Make an inexhaustive version of this warn in the compiler?
    if (isNumericBinopFamily(*instance))
        result = inferNumericBinopFamily(instance);
    else if (isComparisonFamily(*instance))
        result = inferComparisonFamily(instance);
    else if (isOrAndFamily(*instance))
        result = inferOrAndFamily(instance);
    else if (isNotFamily(*instance))
        result = inferNotFamily(instance);
    else if (isLenFamily(*instance))
        result = inferLenFamily(instance);
    else if (isUnaryMinus(*instance))
        result = inferUnaryMinusFamily(instance);
    else
        result = {{}, builtins->unknownType};

    TypeId resultInference = follow(result.familyResultInference);
    if (!familyReducesTo.contains(resultInference))
        familyReducesTo[ty] = resultInference;

    for (size_t i = 0; i < instance->typeArguments.size(); i++)
    {
        if (i < result.operandInference.size())
        {
            TypeId arg = follow(instance->typeArguments[i]);
            TypeId inference = follow(result.operandInference[i]);
            if (auto tfit = get<TypeFamilyInstanceType>(arg))
            {
                if (!familyReducesTo.contains(arg))
                    familyReducesTo.try_insert(arg, inference);
            }
            else if (auto gt = get<GenericType>(arg))
                substitutable[arg] = inference;
        }
    }
}

TypeFamilyInferenceResult TypeFamilyReductionGuesser::inferNumericBinopFamily(const TypeFamilyInstanceType* instance)
{
    LUAU_ASSERT(instance->typeArguments.size() == 2);
    TypeFamilyInferenceResult defaultNumericBinopInference{{builtins->numberType, builtins->numberType}, builtins->numberType};
    return defaultNumericBinopInference;
}

TypeFamilyInferenceResult TypeFamilyReductionGuesser::inferComparisonFamily(const TypeFamilyInstanceType* instance)
{
    LUAU_ASSERT(instance->typeArguments.size() == 2);
    // Comparison families are lt/le/eq.
    // Heuristic: these are type functions from t -> t -> bool

    TypeId lhsTy = follow(instance->typeArguments[0]);
    TypeId rhsTy = follow(instance->typeArguments[1]);

    auto comparisonInference = [&](TypeId op) -> TypeFamilyInferenceResult {
        return TypeFamilyInferenceResult{{op, op}, builtins->booleanType};
    };

    if (std::optional<TypeId> ty = tryAssignOperandType(lhsTy))
        lhsTy = follow(*ty);
    if (std::optional<TypeId> ty = tryAssignOperandType(rhsTy))
        rhsTy = follow(*ty);
    if (operandIsAssignable(lhsTy) && !operandIsAssignable(rhsTy))
        return comparisonInference(rhsTy);
    if (operandIsAssignable(rhsTy) && !operandIsAssignable(lhsTy))
        return comparisonInference(lhsTy);
    return comparisonInference(builtins->numberType);
}

TypeFamilyInferenceResult TypeFamilyReductionGuesser::inferOrAndFamily(const TypeFamilyInstanceType* instance)
{

    LUAU_ASSERT(instance->typeArguments.size() == 2);

    TypeId lhsTy = follow(instance->typeArguments[0]);
    TypeId rhsTy = follow(instance->typeArguments[1]);

    if (std::optional<TypeId> ty = tryAssignOperandType(lhsTy))
        lhsTy = follow(*ty);
    if (std::optional<TypeId> ty = tryAssignOperandType(rhsTy))
        rhsTy = follow(*ty);
    TypeFamilyInferenceResult defaultAndOrInference{{builtins->unknownType, builtins->unknownType}, builtins->booleanType};

    std::shared_ptr<const NormalizedType> lty = normalize(lhsTy);
    std::shared_ptr<const NormalizedType> rty = normalize(lhsTy);
    bool lhsTruthy = lty ? lty->isTruthy() : false;
    bool rhsTruthy = rty ? rty->isTruthy() : false;
    // If at the end, we still don't have good substitutions, return the default type
    if (instance->family->name == "or")
    {
        if (operandIsAssignable(lhsTy) && operandIsAssignable(rhsTy))
            return defaultAndOrInference;
        if (operandIsAssignable(lhsTy))
            return TypeFamilyInferenceResult{{builtins->unknownType, rhsTy}, rhsTy};
        if (operandIsAssignable(rhsTy))
            return TypeFamilyInferenceResult{{lhsTy, builtins->unknownType}, lhsTy};
        if (lhsTruthy)
            return {{lhsTy, rhsTy}, lhsTy};
        if (rhsTruthy)
            return {{builtins->unknownType, rhsTy}, rhsTy};
    }

    if (instance->family->name == "and")
    {

        if (operandIsAssignable(lhsTy) && operandIsAssignable(rhsTy))
            return defaultAndOrInference;
        if (operandIsAssignable(lhsTy))
            return TypeFamilyInferenceResult{{}, rhsTy};
        if (operandIsAssignable(rhsTy))
            return TypeFamilyInferenceResult{{}, lhsTy};
        if (lhsTruthy)
            return {{lhsTy, rhsTy}, rhsTy};
        else
            return {{lhsTy, rhsTy}, lhsTy};
    }

    return defaultAndOrInference;
}

TypeFamilyInferenceResult TypeFamilyReductionGuesser::inferNotFamily(const TypeFamilyInstanceType* instance)
{
    LUAU_ASSERT(instance->typeArguments.size() == 1);
    TypeId opTy = follow(instance->typeArguments[0]);
    if (std::optional<TypeId> ty = tryAssignOperandType(opTy))
        opTy = follow(*ty);
    return {{opTy}, builtins->booleanType};
}

TypeFamilyInferenceResult TypeFamilyReductionGuesser::inferLenFamily(const TypeFamilyInstanceType* instance)
{
    LUAU_ASSERT(instance->typeArguments.size() == 1);
    TypeId opTy = follow(instance->typeArguments[0]);
    if (std::optional<TypeId> ty = tryAssignOperandType(opTy))
        opTy = follow(*ty);
    return {{opTy}, builtins->numberType};
}

TypeFamilyInferenceResult TypeFamilyReductionGuesser::inferUnaryMinusFamily(const TypeFamilyInstanceType* instance)
{
    LUAU_ASSERT(instance->typeArguments.size() == 1);
    TypeId opTy = follow(instance->typeArguments[0]);
    if (std::optional<TypeId> ty = tryAssignOperandType(opTy))
        opTy = follow(*ty);
    if (isNumber(opTy))
        return {{builtins->numberType}, builtins->numberType};
    return {{builtins->unknownType}, builtins->numberType};
}


} // namespace Luau
