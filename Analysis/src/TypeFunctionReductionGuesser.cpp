// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeFunctionReductionGuesser.h"

#include "Luau/DenseHash.h"
#include "Luau/Normalize.h"
#include "Luau/ToString.h"
#include "Luau/TypeFunction.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/VecDeque.h"
#include "Luau/VisitType.h"

#include <optional>

LUAU_FASTFLAG(LuauEmplaceNotPushBack)

LUAU_FASTFLAG(LuauExplicitSkipBoundTypes)

namespace Luau
{
struct InstanceCollector2 : TypeOnceVisitor
{
    VecDeque<TypeId> tys;
    VecDeque<TypePackId> tps;
    DenseHashSet<TypeId> cyclicInstance{nullptr};
    DenseHashSet<TypeId> instanceArguments{nullptr};

    InstanceCollector2()
        : TypeOnceVisitor("InstanceCollector2", FFlag::LuauExplicitSkipBoundTypes)
    {
    }

    bool visit(TypeId ty, const TypeFunctionInstanceType& it) override
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
        if (get<TypeFunctionInstanceType>(t))
            cyclicInstance.insert(t);
    }

    bool visit(TypeId ty, const ExternType&) override
    {
        return false;
    }

    bool visit(TypePackId tp, const TypeFunctionInstanceTypePack&) override
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



TypeFunctionReductionGuesser::TypeFunctionReductionGuesser(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins, NotNull<Normalizer> normalizer)
    : arena(arena)
    , builtins(builtins)
    , normalizer(normalizer)
{
}

bool TypeFunctionReductionGuesser::isFunctionGenericsSaturated(const FunctionType& ftv, DenseHashSet<TypeId>& argsUsed)
{
    bool sameSize = ftv.generics.size() == argsUsed.size();
    bool allGenericsAppear = true;
    for (auto gt : ftv.generics)
        allGenericsAppear = allGenericsAppear || argsUsed.contains(gt);
    return sameSize && allGenericsAppear;
}

void TypeFunctionReductionGuesser::dumpGuesses()
{
    for (auto [tf, t] : functionReducesTo)
        printf("Type family %s ~~> %s\n", toString(tf).c_str(), toString(t).c_str());
    for (auto [t, t_] : substitutable)
        printf("Substitute %s for %s\n", toString(t).c_str(), toString(t_).c_str());
}

std::optional<TypeId> TypeFunctionReductionGuesser::guess(TypeId typ)
{
    std::optional<TypeId> guessedType = guessType(typ);

    if (!guessedType.has_value())
        return {};

    TypeId guess = follow(*guessedType);
    if (get<TypeFunctionInstanceType>(guess))
        return {};

    return guess;
}

std::optional<TypePackId> TypeFunctionReductionGuesser::guess(TypePackId tp)
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
        if (get<TypeFunctionInstanceType>(guess))
            return {};

        guessedHead.push_back(*guessedType);
    }

    return arena->addTypePack(TypePack{std::move(guessedHead), tail});
}

TypeFunctionReductionGuessResult TypeFunctionReductionGuesser::guessTypeFunctionReductionForFunctionExpr(
    const AstExprFunction& expr,
    const FunctionType* ftv,
    TypeId retTy
)
{
    InstanceCollector2 collector;
    collector.traverse(retTy);
    toInfer = std::move(collector.tys);
    cyclicInstances = std::move(collector.cyclicInstance);

    if (isFunctionGenericsSaturated(*ftv, collector.instanceArguments))
        return TypeFunctionReductionGuessResult{{}, nullptr, false};
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
        if (get<TypeFunctionInstanceType>(guess))
            continue;

        if (FFlag::LuauEmplaceNotPushBack)
            results.emplace_back(local->name.value, guess);
        else
            results.push_back({local->name.value, guess});
    }

    // Submit a guess for return types
    TypeId recommendedAnnotation;
    std::optional<TypeId> guessedReturnType = guessType(retTy);
    if (!guessedReturnType.has_value())
        recommendedAnnotation = builtins->unknownType;
    else
        recommendedAnnotation = follow(*guessedReturnType);
    if (auto t = get<TypeFunctionInstanceType>(recommendedAnnotation))
        recommendedAnnotation = builtins->unknownType;

    toInfer.clear();
    cyclicInstances.clear();
    functionReducesTo.clear();
    substitutable.clear();

    return TypeFunctionReductionGuessResult{std::move(results), recommendedAnnotation};
}

std::optional<TypeId> TypeFunctionReductionGuesser::guessType(TypeId arg)
{
    TypeId t = follow(arg);
    if (substitutable.contains(t))
    {
        TypeId subst = follow(substitutable[t]);
        if (subst == t || substitutable.contains(subst))
            return subst;
        else if (!get<TypeFunctionInstanceType>(subst))
            return subst;
        else
            return guessType(subst);
    }
    if (get<TypeFunctionInstanceType>(t))
    {
        if (functionReducesTo.contains(t))
            return functionReducesTo[t];
    }
    return {};
}

bool TypeFunctionReductionGuesser::isNumericBinopFunction(const TypeFunctionInstanceType& instance)
{
    return instance.function->name == "add" || instance.function->name == "sub" || instance.function->name == "mul" ||
           instance.function->name == "div" || instance.function->name == "idiv" || instance.function->name == "pow" ||
           instance.function->name == "mod";
}

bool TypeFunctionReductionGuesser::isComparisonFunction(const TypeFunctionInstanceType& instance)
{
    return instance.function->name == "lt" || instance.function->name == "le" || instance.function->name == "eq";
}

bool TypeFunctionReductionGuesser::isOrAndFunction(const TypeFunctionInstanceType& instance)
{
    return instance.function->name == "or" || instance.function->name == "and";
}

bool TypeFunctionReductionGuesser::isNotFunction(const TypeFunctionInstanceType& instance)
{
    return instance.function->name == "not";
}

bool TypeFunctionReductionGuesser::isLenFunction(const TypeFunctionInstanceType& instance)
{
    return instance.function->name == "len";
}

bool TypeFunctionReductionGuesser::isUnaryMinus(const TypeFunctionInstanceType& instance)
{
    return instance.function->name == "unm";
}

// Operand is assignable if it looks like a cyclic function instance, or a generic type
bool TypeFunctionReductionGuesser::operandIsAssignable(TypeId ty)
{
    if (get<TypeFunctionInstanceType>(ty))
        return true;
    if (get<GenericType>(ty))
        return true;
    if (cyclicInstances.contains(ty))
        return true;
    return false;
}

std::shared_ptr<const NormalizedType> TypeFunctionReductionGuesser::normalize(TypeId ty)
{
    return normalizer->normalize(ty);
}


std::optional<TypeId> TypeFunctionReductionGuesser::tryAssignOperandType(TypeId ty)
{
    // Because we collect innermost instances first, if we see a type function instance as an operand,
    // We try to check if we guessed a type for it
    if (auto tfit = get<TypeFunctionInstanceType>(ty))
    {
        if (functionReducesTo.contains(ty))
            return {functionReducesTo[ty]};
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

void TypeFunctionReductionGuesser::step()
{
    TypeId t = toInfer.front();
    toInfer.pop_front();
    t = follow(t);
    if (auto tf = get<TypeFunctionInstanceType>(t))
        inferTypeFunctionSubstitutions(t, tf);
}

void TypeFunctionReductionGuesser::infer()
{
    while (!done())
        step();
}

bool TypeFunctionReductionGuesser::done()
{
    return toInfer.empty();
}

void TypeFunctionReductionGuesser::inferTypeFunctionSubstitutions(TypeId ty, const TypeFunctionInstanceType* instance)
{

    TypeFunctionInferenceResult result;
    LUAU_ASSERT(instance);
    // TODO: Make an inexhaustive version of this warn in the compiler?
    if (isNumericBinopFunction(*instance))
        result = inferNumericBinopFunction(instance);
    else if (isComparisonFunction(*instance))
        result = inferComparisonFunction(instance);
    else if (isOrAndFunction(*instance))
        result = inferOrAndFunction(instance);
    else if (isNotFunction(*instance))
        result = inferNotFunction(instance);
    else if (isLenFunction(*instance))
        result = inferLenFunction(instance);
    else if (isUnaryMinus(*instance))
        result = inferUnaryMinusFunction(instance);
    else
        result = {{}, builtins->unknownType};

    TypeId resultInference = follow(result.functionResultInference);
    if (!functionReducesTo.contains(resultInference))
        functionReducesTo[ty] = resultInference;

    for (size_t i = 0; i < instance->typeArguments.size(); i++)
    {
        if (i < result.operandInference.size())
        {
            TypeId arg = follow(instance->typeArguments[i]);
            TypeId inference = follow(result.operandInference[i]);
            if (auto tfit = get<TypeFunctionInstanceType>(arg))
            {
                if (!functionReducesTo.contains(arg))
                    functionReducesTo.try_insert(arg, inference);
            }
            else if (auto gt = get<GenericType>(arg))
                substitutable[arg] = inference;
        }
    }
}

TypeFunctionInferenceResult TypeFunctionReductionGuesser::inferNumericBinopFunction(const TypeFunctionInstanceType* instance)
{
    LUAU_ASSERT(instance->typeArguments.size() == 2);
    TypeFunctionInferenceResult defaultNumericBinopInference{{builtins->numberType, builtins->numberType}, builtins->numberType};
    return defaultNumericBinopInference;
}

TypeFunctionInferenceResult TypeFunctionReductionGuesser::inferComparisonFunction(const TypeFunctionInstanceType* instance)
{
    LUAU_ASSERT(instance->typeArguments.size() == 2);
    // Comparison functions are lt/le/eq.
    // Heuristic: these are type functions from t -> t -> bool

    TypeId lhsTy = follow(instance->typeArguments[0]);
    TypeId rhsTy = follow(instance->typeArguments[1]);

    auto comparisonInference = [&](TypeId op) -> TypeFunctionInferenceResult
    {
        return TypeFunctionInferenceResult{{op, op}, builtins->booleanType};
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

TypeFunctionInferenceResult TypeFunctionReductionGuesser::inferOrAndFunction(const TypeFunctionInstanceType* instance)
{

    LUAU_ASSERT(instance->typeArguments.size() == 2);

    TypeId lhsTy = follow(instance->typeArguments[0]);
    TypeId rhsTy = follow(instance->typeArguments[1]);

    if (std::optional<TypeId> ty = tryAssignOperandType(lhsTy))
        lhsTy = follow(*ty);
    if (std::optional<TypeId> ty = tryAssignOperandType(rhsTy))
        rhsTy = follow(*ty);
    TypeFunctionInferenceResult defaultAndOrInference{{builtins->unknownType, builtins->unknownType}, builtins->booleanType};

    std::shared_ptr<const NormalizedType> lty = normalize(lhsTy);
    std::shared_ptr<const NormalizedType> rty = normalize(lhsTy);
    bool lhsTruthy = lty ? lty->isTruthy() : false;
    bool rhsTruthy = rty ? rty->isTruthy() : false;
    // If at the end, we still don't have good substitutions, return the default type
    if (instance->function->name == "or")
    {
        if (operandIsAssignable(lhsTy) && operandIsAssignable(rhsTy))
            return defaultAndOrInference;
        if (operandIsAssignable(lhsTy))
            return TypeFunctionInferenceResult{{builtins->unknownType, rhsTy}, rhsTy};
        if (operandIsAssignable(rhsTy))
            return TypeFunctionInferenceResult{{lhsTy, builtins->unknownType}, lhsTy};
        if (lhsTruthy)
            return {{lhsTy, rhsTy}, lhsTy};
        if (rhsTruthy)
            return {{builtins->unknownType, rhsTy}, rhsTy};
    }

    if (instance->function->name == "and")
    {

        if (operandIsAssignable(lhsTy) && operandIsAssignable(rhsTy))
            return defaultAndOrInference;
        if (operandIsAssignable(lhsTy))
            return TypeFunctionInferenceResult{{}, rhsTy};
        if (operandIsAssignable(rhsTy))
            return TypeFunctionInferenceResult{{}, lhsTy};
        if (lhsTruthy)
            return {{lhsTy, rhsTy}, rhsTy};
        else
            return {{lhsTy, rhsTy}, lhsTy};
    }

    return defaultAndOrInference;
}

TypeFunctionInferenceResult TypeFunctionReductionGuesser::inferNotFunction(const TypeFunctionInstanceType* instance)
{
    LUAU_ASSERT(instance->typeArguments.size() == 1);
    TypeId opTy = follow(instance->typeArguments[0]);
    if (std::optional<TypeId> ty = tryAssignOperandType(opTy))
        opTy = follow(*ty);
    return {{opTy}, builtins->booleanType};
}

TypeFunctionInferenceResult TypeFunctionReductionGuesser::inferLenFunction(const TypeFunctionInstanceType* instance)
{
    LUAU_ASSERT(instance->typeArguments.size() == 1);
    TypeId opTy = follow(instance->typeArguments[0]);
    if (std::optional<TypeId> ty = tryAssignOperandType(opTy))
        opTy = follow(*ty);
    return {{opTy}, builtins->numberType};
}

TypeFunctionInferenceResult TypeFunctionReductionGuesser::inferUnaryMinusFunction(const TypeFunctionInstanceType* instance)
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
