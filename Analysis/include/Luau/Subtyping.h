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
struct NormalizedStringType;
struct NormalizedFunctionType;


struct SubtypingResult
{
    bool isSubtype = false;
    bool isErrorSuppressing = false;
    bool normalizationTooComplex = false;

    SubtypingResult& andAlso(const SubtypingResult& other);
    SubtypingResult& orElse(const SubtypingResult& other);

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

    NotNull<Scope> scope;

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

    Subtyping(const Subtyping&) = delete;
    Subtyping& operator=(const Subtyping&) = delete;

    Subtyping(Subtyping&&) = default;
    Subtyping& operator=(Subtyping&&) = default;

    // TODO cache
    // TODO cyclic types
    // TODO recursion limits

    SubtypingResult isSubtype(TypeId subTy, TypeId superTy);
    SubtypingResult isSubtype(TypePackId subTy, TypePackId superTy);

private:
    SubtypingResult isCovariantWith(TypeId subTy, TypeId superTy);
    SubtypingResult isCovariantWith(TypePackId subTy, TypePackId superTy);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isContravariantWith(SubTy&& subTy, SuperTy&& superTy);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isInvariantWith(SubTy&& subTy, SuperTy&& superTy);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isCovariantWith(const TryPair<const SubTy*, const SuperTy*>& pair);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isContravariantWith(const TryPair<const SubTy*, const SuperTy*>& pair);

    template<typename SubTy, typename SuperTy>
    SubtypingResult isInvariantWith(const TryPair<const SubTy*, const SuperTy*>& pair);

    SubtypingResult isCovariantWith(TypeId subTy, const UnionType* superUnion);
    SubtypingResult isCovariantWith(const UnionType* subUnion, TypeId superTy);
    SubtypingResult isCovariantWith(TypeId subTy, const IntersectionType* superIntersection);
    SubtypingResult isCovariantWith(const IntersectionType* subIntersection, TypeId superTy);

    SubtypingResult isCovariantWith(const NegationType* subNegation, TypeId superTy);
    SubtypingResult isCovariantWith(const TypeId subTy, const NegationType* superNegation);

    SubtypingResult isCovariantWith(const PrimitiveType* subPrim, const PrimitiveType* superPrim);
    SubtypingResult isCovariantWith(const SingletonType* subSingleton, const PrimitiveType* superPrim);
    SubtypingResult isCovariantWith(const SingletonType* subSingleton, const SingletonType* superSingleton);
    SubtypingResult isCovariantWith(const TableType* subTable, const TableType* superTable);
    SubtypingResult isCovariantWith(const MetatableType* subMt, const MetatableType* superMt);
    SubtypingResult isCovariantWith(const MetatableType* subMt, const TableType* superTable);
    SubtypingResult isCovariantWith(const ClassType* subClass, const ClassType* superClass);
    SubtypingResult isCovariantWith(const ClassType* subClass, const TableType* superTable);
    SubtypingResult isCovariantWith(const FunctionType* subFunction, const FunctionType* superFunction);
    SubtypingResult isCovariantWith(const PrimitiveType* subPrim, const TableType* superTable);
    SubtypingResult isCovariantWith(const SingletonType* subSingleton, const TableType* superTable);

    SubtypingResult isCovariantWith(const TableIndexer& subIndexer, const TableIndexer& superIndexer);

    SubtypingResult isCovariantWith(const NormalizedType* subNorm, const NormalizedType* superNorm);
    SubtypingResult isCovariantWith(const NormalizedClassType& subClass, const NormalizedClassType& superClass);
    SubtypingResult isCovariantWith(const NormalizedClassType& subClass, const TypeIds& superTables);
    SubtypingResult isCovariantWith(const NormalizedStringType& subString, const NormalizedStringType& superString);
    SubtypingResult isCovariantWith(const NormalizedStringType& subString, const TypeIds& superTables);
    SubtypingResult isCovariantWith(const NormalizedFunctionType& subFunction, const NormalizedFunctionType& superFunction);
    SubtypingResult isCovariantWith(const TypeIds& subTypes, const TypeIds& superTypes);

    SubtypingResult isCovariantWith(const VariadicTypePack* subVariadic, const VariadicTypePack* superVariadic);

    bool bindGeneric(TypeId subTp, TypeId superTp);
    bool bindGeneric(TypePackId subTp, TypePackId superTp);

    template<typename T, typename Container>
    TypeId makeAggregateType(const Container& container, TypeId orElse);

    [[noreturn]] void unexpected(TypePackId tp);
};

} // namespace Luau
