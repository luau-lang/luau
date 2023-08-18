// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Type.h"

#include <vector>
#include <optional>

namespace Luau
{

template<typename A, typename B>
struct TryPair;

class Normalizer;
struct NormalizedType;

struct SubtypingGraph
{
    // Did the test succeed?
    bool isSubtype = false;
    bool isErrorSuppressing = false;
    bool normalizationTooComplex = false;

    // If so, what constraints are implied by this relation?
    // If not, what happened?

    SubtypingGraph and_(const SubtypingGraph& other);
    SubtypingGraph or_(const SubtypingGraph& other);

    static SubtypingGraph and_(const std::vector<SubtypingGraph>& results);
    static SubtypingGraph or_(const std::vector<SubtypingGraph>& results);
};

struct Subtyping
{
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<Normalizer> normalizer;

    // TODO cache
    // TODO cyclic types
    // TODO recursion limits

    SubtypingGraph isSubtype(TypeId subTy, TypeId superTy);
    SubtypingGraph isSubtype(TypePackId subTy, TypePackId superTy);

private:
    template<typename SubTy, typename SuperTy>
    SubtypingGraph isSubtype(const TryPair<const SubTy*, const SuperTy*>& pair);

    SubtypingGraph isSubtype(TypeId subTy, const UnionType* superUnion);
    SubtypingGraph isSubtype(const UnionType* subUnion, TypeId superTy);
    SubtypingGraph isSubtype(TypeId subTy, const IntersectionType* superIntersection);
    SubtypingGraph isSubtype(const IntersectionType* subIntersection, TypeId superTy);
    SubtypingGraph isSubtype(const PrimitiveType* subPrim, const PrimitiveType* superPrim);
    SubtypingGraph isSubtype(const SingletonType* subSingleton, const PrimitiveType* superPrim);
    SubtypingGraph isSubtype(const SingletonType* subSingleton, const SingletonType* superSingleton);
    SubtypingGraph isSubtype(const FunctionType* subFunction, const FunctionType* superFunction);

    SubtypingGraph isSubtype(const NormalizedType* subNorm, const NormalizedType* superNorm);
};

} // namespace Luau
