// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypePack.h"
#include "Luau/Variant.h"

namespace Luau
{

namespace detail
{
template<typename T>
struct ReductionEdge
{
    T type = nullptr;
    bool irreducible = false;
};

struct TypeReductionMemoization
{
    TypeReductionMemoization() = default;

    TypeReductionMemoization(const TypeReductionMemoization&) = delete;
    TypeReductionMemoization& operator=(const TypeReductionMemoization&) = delete;

    TypeReductionMemoization(TypeReductionMemoization&&) = default;
    TypeReductionMemoization& operator=(TypeReductionMemoization&&) = default;

    DenseHashMap<TypeId, ReductionEdge<TypeId>> types{nullptr};
    DenseHashMap<TypePackId, ReductionEdge<TypePackId>> typePacks{nullptr};

    bool isIrreducible(TypeId ty);
    bool isIrreducible(TypePackId tp);

    TypeId memoize(TypeId ty, TypeId reducedTy);
    TypePackId memoize(TypePackId tp, TypePackId reducedTp);

    // Reducing A into B may have a non-irreducible edge A to B for which B is not irreducible, which means B could be reduced into C.
    // Because reduction should always be transitive, A should point to C if A points to B and B points to C.
    std::optional<ReductionEdge<TypeId>> memoizedof(TypeId ty) const;
    std::optional<ReductionEdge<TypePackId>> memoizedof(TypePackId tp) const;
};
} // namespace detail

struct TypeReductionOptions
{
    /// If it's desirable for type reduction to allocate into a different arena than the TypeReduction instance you have, you will need
    /// to create a temporary TypeReduction in that case, and set [`TypeReductionOptions::allowTypeReductionsFromOtherArenas`] to true.
    /// This is because TypeReduction caches the reduced type.
    bool allowTypeReductionsFromOtherArenas = false;
};

struct TypeReduction
{
    explicit TypeReduction(
        NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<InternalErrorReporter> handle, const TypeReductionOptions& opts = {});

    TypeReduction(const TypeReduction&) = delete;
    TypeReduction& operator=(const TypeReduction&) = delete;

    TypeReduction(TypeReduction&&) = default;
    TypeReduction& operator=(TypeReduction&&) = default;

    std::optional<TypeId> reduce(TypeId ty);
    std::optional<TypePackId> reduce(TypePackId tp);
    std::optional<TypeFun> reduce(const TypeFun& fun);

private:
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<struct InternalErrorReporter> handle;

    TypeReductionOptions options;
    detail::TypeReductionMemoization memoization;

    // Computes an *estimated length* of the cartesian product of the given type.
    size_t cartesianProductSize(TypeId ty) const;

    bool hasExceededCartesianProductLimit(TypeId ty) const;
    bool hasExceededCartesianProductLimit(TypePackId tp) const;
};

} // namespace Luau
