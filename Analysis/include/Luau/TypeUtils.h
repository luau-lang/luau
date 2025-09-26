// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Error.h"
#include "Luau/Location.h"
#include "Luau/Type.h"
#include "Luau/TypeIds.h"
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

/// the current context of the type checker
enum class TypeContext
{
    /// the default context
    Default,
    /// inside of a condition
    Condition,
};

bool inConditional(const TypeContext& context);

// sets the given type context to `Condition` and restores it to its original
// value when the struct drops out of scope
struct InConditionalContext
{
    TypeContext* typeContext;
    TypeContext oldValue;

    explicit InConditionalContext(TypeContext* c, TypeContext newValue = TypeContext::Condition)
        : typeContext(c)
        , oldValue(*c)
    {
        *typeContext = newValue;
    }

    ~InConditionalContext()
    {
        *typeContext = oldValue;
    }
};

using ScopePtr = std::shared_ptr<struct Scope>;

std::optional<Property> findTableProperty(
    NotNull<BuiltinTypes> builtinTypes,
    ErrorVec& errors,
    TypeId ty,
    const std::string& name,
    Location location
);

std::optional<TypeId> findMetatableEntry(
    NotNull<BuiltinTypes> builtinTypes,
    ErrorVec& errors,
    TypeId type,
    const std::string& entry,
    Location location
);
std::optional<TypeId> findTablePropertyRespectingMeta(
    NotNull<BuiltinTypes> builtinTypes,
    ErrorVec& errors,
    TypeId ty,
    const std::string& name,
    Location location
);
std::optional<TypeId> findTablePropertyRespectingMeta(
    NotNull<BuiltinTypes> builtinTypes,
    ErrorVec& errors,
    TypeId ty,
    const std::string& name,
    ValueContext context,
    Location location
);

bool occursCheck(TypeId needle, TypeId haystack);

// Returns the minimum and maximum number of types the argument list can accept.
std::pair<size_t, std::optional<size_t>> getParameterExtents(const TxnLog* log, TypePackId tp, bool includeHiddenVariadics = false);

// Extend the provided pack to at least `length` types.
// Returns a temporary TypePack that contains those types plus a tail.
TypePack extendTypePack(
    TypeArena& arena,
    NotNull<BuiltinTypes> builtinTypes,
    TypePackId pack,
    size_t length,
    std::vector<std::optional<TypeId>> overrides = {}
);

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

struct ErrorSuppression
{
    enum Value
    {
        Suppress,
        DoNotSuppress,
        NormalizationFailed,
    };

    ErrorSuppression() = default;
    constexpr ErrorSuppression(Value enumValue)
        : value(enumValue)
    {
    }

    constexpr operator Value() const
    {
        return value;
    }
    explicit operator bool() const = delete;

    ErrorSuppression orElse(const ErrorSuppression& other) const
    {
        switch (value)
        {
        case DoNotSuppress:
            return other;
        default:
            return *this;
        }
    }

private:
    Value value;
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
    static_assert(std::is_pointer_v<Ty>, "argument must be a pointer type");

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

template<typename T, typename Ty>
T* getMutable(std::optional<Ty> ty)
{
    if (ty)
        return getMutable<T>(*ty);
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

/**
 * Returns whether or not expr is a literal expression, for example:
 * - Scalar literals (numbers, booleans, strings, nil)
 * - Table literals
 * - Lambdas (a "function literal")
 */
bool isLiteral(const AstExpr* expr);

/**
 * Given a function call and a mapping from expression to type, determine
 * whether the type of any argument in said call in depends on a blocked types.
 * This is used as a precondition for bidirectional inference: be warned that
 * the behavior of this algorithm is tightly coupled to that of bidirectional
 * inference.
 * @param expr Expression to search
 * @param astTypes Mapping from AST node to TypeID
 * @returns A vector of blocked types
 */
std::vector<TypeId> findBlockedArgTypesIn(AstExprCall* expr, NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes);

/**
 * Given a scope and a free type, find the closest parent that has a present
 * `interiorFreeTypes` and append the given type to said list. This list will
 * be generalized when the requiste `GeneralizationConstraint` is resolved.
 * @param scope Initial scope this free type was attached to
 * @param ty Free type to track.
 */
void trackInteriorFreeType(Scope* scope, TypeId ty);

void trackInteriorFreeTypePack(Scope* scope, TypePackId tp);

// A fast approximation of subTy <: superTy
bool fastIsSubtype(TypeId subTy, TypeId superTy);

/**
 * @param tables A list of potential table parts of a union
 * @param exprType Type of the expression to match
 * @return An element of `tables` that best matches `exprType`.
 */
std::optional<TypeId> extractMatchingTableType(std::vector<TypeId>& tables, TypeId exprType, NotNull<BuiltinTypes> builtinTypes);

/**
 * @param item A member of a table in an AST
 * @return Whether the item is a key-value pair with a statically defined string key.
 *
 * ```
 * {
 *      ["foo"] = ..., -- is a record
 *      bar = ..., -- is a record
 *      ..., -- not a record: non-string key (number)
 *      [true] = ..., -- not a record: non-string key (boolean)
 *      [ foobar() ] = ..., -- not a record: unknown key value.
 *      ["foo" .. "bar"] = ..., -- not a record (don't make us handle it).
 * }
 * ```
 */
bool isRecord(const AstExprTable::Item& item);

/**
 * Do a quick check for whether the type `ty` is exactly `false | nil`. This
 * will *not* do any sort of semantic analysis, for example the type:
 *
 *      (boolean?) & (false | nil)
 *
 * ... will not be considered falsy, despite it being semantically equivalent
 * to `false | nil`.
 *
 * @return Whether the input is approximately `false | nil`.
 */
bool isApproximatelyFalsyType(TypeId ty);

/**
 * Do a quick check for whether the type `ty` is exactly `~(false | nil)`.
 * This will *not* do any sort of semantic analysis, for example the type:
 *
 *      unknown & ~(false | nil)
 *
 * ... will not be considered falsy, despite it being semantically equivalent
 * to `~(false | nil)`.
 *
 * @return Whether the input is approximately `~(false | nil)`.
 */
bool isApproximatelyTruthyType(TypeId ty);

// Unwraps any grouping expressions iteratively.
AstExpr* unwrapGroup(AstExpr* expr);

// Returns true if ty is optional, ie if it is a supertype of nil
bool isOptionalType(TypeId ty, NotNull<BuiltinTypes> builtinTypes);

// These are magic types used in `TypeChecker2` and `NonStrictTypeChecker`
//
// `_luau_print` causes it's argument to be printed out, as in:
//
//      local x: _luau_print<number>
//
// ... will cause `number` to be printed.
inline constexpr char kLuauPrint[] = "_luau_print";
// `_luau_force_constraint_solving_incomplete` will cause us to _always_ emit
// a constraint solving incomplete error to test semantics around that specific
// error.
inline constexpr char kLuauForceConstraintSolvingIncomplete[] = "_luau_force_constraint_solving_incomplete";
// `_luau_blocked_type` will cause us to always mint a blocked type that does
// not get emplaced by constraint solving.
inline constexpr char kLuauBlockedType[] = "_luau_blocked_type";

struct UnionBuilder
{
    UnionBuilder(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes);
    void add(TypeId ty);
    TypeId build();
    size_t size() const;
    void reserve(size_t size);

private:
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
    TypeIds options;
    bool isTop = false;
};

struct IntersectionBuilder
{
    IntersectionBuilder(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes);
    void add(TypeId ty);
    TypeId build();
    size_t size() const;
    void reserve(size_t size);

private:
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
    TypeIds parts;
    bool isBottom = false;
};

TypeId addIntersection(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, std::initializer_list<TypeId> list);
TypeId addUnion(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, std::initializer_list<TypeId> list);


} // namespace Luau
