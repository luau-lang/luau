// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Ast.h"
#include "Luau/VecDeque.h"
#include "Luau/DenseHash.h"
#include "Luau/TypeFunction.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/TypeUtils.h"
#include "Luau/Normalize.h"
#include "Luau/TypeFwd.h"
#include "Luau/VisitType.h"
#include "Luau/NotNull.h"
#include "TypeArena.h"

namespace Luau
{

struct TypeFunctionReductionGuessResult
{
    std::vector<std::pair<std::string, TypeId>> guessedFunctionAnnotations;
    TypeId guessedReturnType;
    bool shouldRecommendAnnotation = true;
};

// An Inference result for a type function is a list of types corresponding to the guessed argument types, followed by a type for the result
struct TypeFunctionInferenceResult
{
    std::vector<TypeId> operandInference;
    TypeId functionResultInference;
};

struct TypeFunctionReductionGuesser
{
    // Tracks our hypothesis about what a type function reduces to
    DenseHashMap<TypeId, TypeId> functionReducesTo{nullptr};
    // Tracks our constraints on type function operands
    DenseHashMap<TypeId, TypeId> substitutable{nullptr};
    // List of instances to try progress
    VecDeque<TypeId> toInfer;
    DenseHashSet<TypeId> cyclicInstances{nullptr};

    // Utilities
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtins;
    NotNull<Normalizer> normalizer;

    TypeFunctionReductionGuesser(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins, NotNull<Normalizer> normalizer);

    std::optional<TypeId> guess(TypeId typ);
    std::optional<TypePackId> guess(TypePackId typ);
    TypeFunctionReductionGuessResult guessTypeFunctionReductionForFunctionExpr(const AstExprFunction& expr, const FunctionType* ftv, TypeId retTy);

private:
    std::optional<TypeId> guessType(TypeId arg);
    void dumpGuesses();

    bool isNumericBinopFunction(const TypeFunctionInstanceType& instance);
    bool isComparisonFunction(const TypeFunctionInstanceType& instance);
    bool isOrAndFunction(const TypeFunctionInstanceType& instance);
    bool isNotFunction(const TypeFunctionInstanceType& instance);
    bool isLenFunction(const TypeFunctionInstanceType& instance);
    bool isUnaryMinus(const TypeFunctionInstanceType& instance);

    // Operand is assignable if it looks like a cyclic type function instance, or a generic type
    bool operandIsAssignable(TypeId ty);
    std::optional<TypeId> tryAssignOperandType(TypeId ty);

    std::shared_ptr<const NormalizedType> normalize(TypeId ty);
    void step();
    void infer();
    bool done();

    bool isFunctionGenericsSaturated(const FunctionType& ftv, DenseHashSet<TypeId>& instanceArgs);
    void inferTypeFunctionSubstitutions(TypeId ty, const TypeFunctionInstanceType* instance);
    TypeFunctionInferenceResult inferNumericBinopFunction(const TypeFunctionInstanceType* instance);
    TypeFunctionInferenceResult inferComparisonFunction(const TypeFunctionInstanceType* instance);
    TypeFunctionInferenceResult inferOrAndFunction(const TypeFunctionInstanceType* instance);
    TypeFunctionInferenceResult inferNotFunction(const TypeFunctionInstanceType* instance);
    TypeFunctionInferenceResult inferLenFunction(const TypeFunctionInstanceType* instance);
    TypeFunctionInferenceResult inferUnaryMinusFunction(const TypeFunctionInstanceType* instance);
};
} // namespace Luau
