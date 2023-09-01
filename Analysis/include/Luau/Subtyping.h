// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Type.h"
#include "Luau/TypePack.h"
#include "Luau/UnifierSharedState.h"

#include <vector>
#include <optional>

namespace Luau
{

template<typename A, typename B>
struct TryPair;
struct InternalErrorReporter;

class TypeIds;
class Normalizer;
struct NormalizedType;
struct NormalizedClassType;

struct SubtypingResult
{
    // Did the test succeed?
    bool isSubtype = false;
    bool isErrorSuppressing = false;
    bool normalizationTooComplex = false;

    // If so, what constraints are implied by this relation?
    // If not, what happened?

    void andAlso(const SubtypingResult& other);
    void orElse(const SubtypingResult& other);

    // Only negates the `isSubtype`.
    static SubtypingResult negate(const SubtypingResult& result);
    static SubtypingResult all(const std::vector<SubtypingResult>& results);
    static SubtypingResult any(const std::vector<SubtypingResult>& results);
};

struct Subtyping
{
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<TypeArena> arena;
    NotNull<Normalizer> normalizer;
    NotNull<InternalErrorReporter> iceReporter;

    enum class Variance
    {
        Covariant,
        Contravariant
    };

    Variance variance = Variance::Covariant;

    struct GenericBounds
    {
        DenseHashSet<TypeId> lowerBound{nullptr};
        DenseHashSet<TypeId> upperBound{nullptr};
    };

    /*
     * When we encounter a generic over the course of a subtyping test, we need
     * to tentatively map that generic onto a type on the other side.
     */
    DenseHashMap<TypeId, GenericBounds> mappedGenerics{nullptr};
    DenseHashMap<TypePackId, TypePackId> mappedGenericPacks{nullptr};

    using SeenSet = std::unordered_set<std::pair<TypeId, TypeId>, TypeIdPairHash>;

    SeenSet seenTypes;

    // TODO cache
    // TODO cyclic types
    // TODO recursion limits

    SubtypingResult isSubtype(TypeId subTy, TypeId superTy);
    SubtypingResult isSubtype(TypePackId subTy, TypePackId superTy);

private:
    SubtypingResult isSubtype_(TypeId subTy, TypeId superTy);
    SubtypingResult isSubtype_(TypePackId subTy, TypePackId superTy);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isSubtype_(const TryPair<const SubTy*, const SuperTy*>& pair);

    SubtypingResult isSubtype_(TypeId subTy, const UnionType* superUnion);
    SubtypingResult isSubtype_(const UnionType* subUnion, TypeId superTy);
    SubtypingResult isSubtype_(TypeId subTy, const IntersectionType* superIntersection);
    SubtypingResult isSubtype_(const IntersectionType* subIntersection, TypeId superTy);
    SubtypingResult isSubtype_(const PrimitiveType* subPrim, const PrimitiveType* superPrim);
    SubtypingResult isSubtype_(const SingletonType* subSingleton, const PrimitiveType* superPrim);
    SubtypingResult isSubtype_(const SingletonType* subSingleton, const SingletonType* superSingleton);
    SubtypingResult isSubtype_(const TableType* subTable, const TableType* superTable);
    SubtypingResult isSubtype_(const MetatableType* subMt, const MetatableType* superMt);
    SubtypingResult isSubtype_(const MetatableType* subMt, const TableType* superTable);
    SubtypingResult isSubtype_(const ClassType* subClass, const ClassType* superClass);
    SubtypingResult isSubtype_(const ClassType* subClass, const TableType* superTable); // Actually a class <: shape.
    SubtypingResult isSubtype_(const FunctionType* subFunction, const FunctionType* superFunction);
    SubtypingResult isSubtype_(const PrimitiveType* subPrim, const TableType* superTable);
    SubtypingResult isSubtype_(const SingletonType* subSingleton, const TableType* superTable);

    SubtypingResult isSubtype_(const NormalizedType* subNorm, const NormalizedType* superNorm);
    SubtypingResult isSubtype_(const NormalizedClassType& subClass, const NormalizedClassType& superClass, const TypeIds& superTables);
    SubtypingResult isSubtype_(const TypeIds& subTypes, const TypeIds& superTypes);

    SubtypingResult isSubtype_(const VariadicTypePack* subVariadic, const VariadicTypePack* superVariadic);

    bool bindGeneric(TypeId subTp, TypeId superTp);
    bool bindGeneric(TypePackId subTp, TypePackId superTp);

    template <typename T, typename Container>
    TypeId makeAggregateType(const Container& container, TypeId orElse);

    [[noreturn]]
    void unexpected(TypePackId tp);
};

} // namespace Luau
