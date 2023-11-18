// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Error.h"
#include "Luau/Location.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"

#include <memory>
#include <optional>

namespace Luau
{

struct TxnLog;
struct TypeArena;
class Normalizer;

enum class ValueContext
{
    LValue,
    RValue
};

using ScopePtr = std::shared_ptr<struct Scope>;

std::optional<TypeId> findMetatableEntry(
    NotNull<BuiltinTypes> builtinTypes, ErrorVec& errors, TypeId type, const std::string& entry, Location location);
std::optional<TypeId> findTablePropertyRespectingMeta(
    NotNull<BuiltinTypes> builtinTypes, ErrorVec& errors, TypeId ty, const std::string& name, Location location);

// Returns the minimum and maximum number of types the argument list can accept.
std::pair<size_t, std::optional<size_t>> getParameterExtents(const TxnLog* log, TypePackId tp, bool includeHiddenVariadics = false);

// Extend the provided pack to at least `length` types.
// Returns a temporary TypePack that contains those types plus a tail.
TypePack extendTypePack(
    TypeArena& arena, NotNull<BuiltinTypes> builtinTypes, TypePackId pack, size_t length, std::vector<std::optional<TypeId>> overrides = {});

/**
 * Reduces a union by decomposing to the any/error type if it appears in the
 * type list, and by merging child unions. Also strips out duplicate (by pointer
 * identity) types.
 * @param types the input type list to reduce.
 * @returns the reduced type list.
 */
std::vector<TypeId> reduceUnion(const std::vector<TypeId>& types);

/**
 * Tries to remove nil from a union type, if there's another option. T | nil
 * reduces to T, but nil itself does not reduce.
 * @param builtinTypes the singleton types to use
 * @param arena the type arena to allocate the new type in, if necessary
 * @param ty the type to remove nil from
 * @returns a type with nil removed, or nil itself if that were the only option.
 */
TypeId stripNil(NotNull<BuiltinTypes> builtinTypes, TypeArena& arena, TypeId ty);

enum class ErrorSuppression
{
    Suppress,
    DoNotSuppress,
    NormalizationFailed
};

/**
 * Normalizes the given type using the normalizer to determine if the type
 * should suppress any errors that would be reported involving it.
 * @param normalizer the normalizer to use
 * @param ty the type to check for error suppression
 * @returns an enum indicating whether or not to suppress the error or to signal a normalization failure
 */
ErrorSuppression shouldSuppressErrors(NotNull<Normalizer> normalizer, TypeId ty);

/**
 * Flattens and normalizes the given typepack using the normalizer to determine if the type
 * should suppress any errors that would be reported involving it.
 * @param normalizer the normalizer to use
 * @param tp the typepack to check for error suppression
 * @returns an enum indicating whether or not to suppress the error or to signal a normalization failure
 */
ErrorSuppression shouldSuppressErrors(NotNull<Normalizer> normalizer, TypePackId tp);

/**
 * Normalizes the two given type using the normalizer to determine if either type
 * should suppress any errors that would be reported involving it.
 * @param normalizer the normalizer to use
 * @param ty1 the first type to check for error suppression
 * @param ty2 the second type to check for error suppression
 * @returns an enum indicating whether or not to suppress the error or to signal a normalization failure
 */
ErrorSuppression shouldSuppressErrors(NotNull<Normalizer> normalizer, TypeId ty1, TypeId ty2);

/**
 * Flattens and normalizes the two given typepacks using the normalizer to determine if either type
 * should suppress any errors that would be reported involving it.
 * @param normalizer the normalizer to use
 * @param tp1 the first typepack to check for error suppression
 * @param tp2 the second typepack to check for error suppression
 * @returns an enum indicating whether or not to suppress the error or to signal a normalization failure
 */
ErrorSuppression shouldSuppressErrors(NotNull<Normalizer> normalizer, TypePackId tp1, TypePackId tp2);

// Similar to `std::optional<std::pair<A, B>>`, but whose `sizeof()` is the same as `std::pair<A, B>`
// and cooperates with C++'s `if (auto p = ...)` syntax without the extra fatness of `std::optional`.
template<typename A, typename B>
struct TryPair
{
    A first;
    B second;

    explicit operator bool() const
    {
        return bool(first) && bool(second);
    }
};

template<typename A, typename B, typename Ty>
TryPair<const A*, const B*> get2(Ty one, Ty two)
{
    const A* a = get<A>(one);
    const B* b = get<B>(two);
    if (a && b)
        return {a, b};
    else
        return {nullptr, nullptr};
}

template<typename T, typename Ty>
const T* get(std::optional<Ty> ty)
{
    if (ty)
        return get<T>(*ty);
    else
        return nullptr;
}

template<typename Ty>
std::optional<Ty> follow(std::optional<Ty> ty)
{
    if (ty)
        return follow(*ty);
    else
        return std::nullopt;
}

} // namespace Luau
