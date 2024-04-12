// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Ast.h"
#include "Luau/VecDeque.h"
#include "Luau/DenseHash.h"
#include "Luau/TypeFamily.h"
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

struct TypeFamilyReductionGuessResult
{
    std::vector<std::pair<std::string, TypeId>> guessedFunctionAnnotations;
    TypeId guessedReturnType;
    bool shouldRecommendAnnotation = true;
};

// An Inference result for a type family is a list of types corresponding to the guessed argument types, followed by a type for the result
struct TypeFamilyInferenceResult
{
    std::vector<TypeId> operandInference;
    TypeId familyResultInference;
};

struct TypeFamilyReductionGuesser
{
    // Tracks our hypothesis about what a type family reduces to
    DenseHashMap<TypeId, TypeId> familyReducesTo{nullptr};
    // Tracks our constraints on type family operands
    DenseHashMap<TypeId, TypeId> substitutable{nullptr};
    // List of instances to try progress
    VecDeque<TypeId> toInfer;
    DenseHashSet<TypeId> cyclicInstances{nullptr};

    // Utilities
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtins;
    NotNull<Normalizer> normalizer;

    TypeFamilyReductionGuesser(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtins, NotNull<Normalizer> normalizer);

    std::optional<TypeId> guess(TypeId typ);
    std::optional<TypePackId> guess(TypePackId typ);
    TypeFamilyReductionGuessResult guessTypeFamilyReductionForFunction(const AstExprFunction& expr, const FunctionType* ftv, TypeId retTy);

private:
    std::optional<TypeId> guessType(TypeId arg);
    void dumpGuesses();

    bool isNumericBinopFamily(const TypeFamilyInstanceType& instance);
    bool isComparisonFamily(const TypeFamilyInstanceType& instance);
    bool isOrAndFamily(const TypeFamilyInstanceType& instance);
    bool isNotFamily(const TypeFamilyInstanceType& instance);
    bool isLenFamily(const TypeFamilyInstanceType& instance);
    bool isUnaryMinus(const TypeFamilyInstanceType& instance);

    // Operand is assignable if it looks like a cyclic family instance, or a generic type
    bool operandIsAssignable(TypeId ty);
    std::optional<TypeId> tryAssignOperandType(TypeId ty);

    std::shared_ptr<const NormalizedType> normalize(TypeId ty);
    void step();
    void infer();
    bool done();

    bool isFunctionGenericsSaturated(const FunctionType& ftv, DenseHashSet<TypeId>& instanceArgs);
    void inferTypeFamilySubstitutions(TypeId ty, const TypeFamilyInstanceType* instance);
    TypeFamilyInferenceResult inferNumericBinopFamily(const TypeFamilyInstanceType* instance);
    TypeFamilyInferenceResult inferComparisonFamily(const TypeFamilyInstanceType* instance);
    TypeFamilyInferenceResult inferOrAndFamily(const TypeFamilyInstanceType* instance);
    TypeFamilyInferenceResult inferNotFamily(const TypeFamilyInstanceType* instance);
    TypeFamilyInferenceResult inferLenFamily(const TypeFamilyInstanceType* instance);
    TypeFamilyInferenceResult inferUnaryMinusFamily(const TypeFamilyInstanceType* instance);
};
} // namespace Luau
