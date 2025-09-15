// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/EqSatSimplification.h"
#include "Luau/NotNull.h"
#include "Luau/Set.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeIds.h"
#include "Luau/UnifierSharedState.h"

#include <map>
#include <memory>
#include <unordered_map>
#include <vector>

namespace Luau
{

struct InternalErrorReporter;
struct Module;
struct Scope;

using ModulePtr = std::shared_ptr<Module>;

bool isSubtype(
    TypeId subTy,
    TypeId superTy,
    NotNull<Scope> scope,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Simplifier> simplifier,
    InternalErrorReporter& ice,
    SolverMode solverMode
);
bool isSubtype(
    TypePackId subPack,
    TypePackId superPack,
    NotNull<Scope> scope,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<Simplifier> simplifier,
    InternalErrorReporter& ice,
    SolverMode solverMode
);

} // namespace Luau

template<>
struct std::hash<Luau::TypeIds>
{
    std::size_t operator()(const Luau::TypeIds& tys) const
    {
        return tys.getHash();
    }
};

template<>
struct std::hash<const Luau::TypeIds*>
{
    std::size_t operator()(const Luau::TypeIds* tys) const
    {
        return tys->getHash();
    }
};

template<>
struct std::equal_to<Luau::TypeIds>
{
    bool operator()(const Luau::TypeIds& here, const Luau::TypeIds& there) const
    {
        return here == there;
    }
};

template<>
struct std::equal_to<const Luau::TypeIds*>
{
    bool operator()(const Luau::TypeIds* here, const Luau::TypeIds* there) const
    {
        return *here == *there;
    }
};

namespace Luau
{

/** A normalized string type is either `string` (represented by `nullopt`) or a
 * union of string singletons.
 *
 * The representation is as follows:
 *
 * * A union of string singletons is finite and includes the singletons named by
 *   the `singletons` field.
 * * An intersection of negated string singletons is cofinite and includes the
 *   singletons excluded by the `singletons` field.  It is implied that cofinite
 *   values are exclusions from `string` itself.
 * * The `string` data type is a cofinite set minus zero elements.
 * * The `never` data type is a finite set plus zero elements.
 */
struct NormalizedStringType
{
    // When false, this type represents a union of singleton string types.
    // eg "a" | "b" | "c"
    //
    // When true, this type represents string intersected with negated string
    // singleton types.
    // eg string & ~"a" & ~"b" & ...
    bool isCofinite = false;

    std::map<std::string, TypeId> singletons;

    void resetToString();
    void resetToNever();

    bool isNever() const;
    bool isString() const;

    /// Returns true if the string has finite domain.
    ///
    /// Important subtlety: This method returns true for `never`.  The empty set
    /// is indeed an empty set.
    bool isUnion() const;

    /// Returns true if the string has infinite domain.
    bool isIntersection() const;

    bool includes(const std::string& str) const;

    static const NormalizedStringType never;

    NormalizedStringType();
    NormalizedStringType(bool isCofinite, std::map<std::string, TypeId> singletons);
};

bool isSubtype(const NormalizedStringType& subStr, const NormalizedStringType& superStr);

struct NormalizedExternType
{
    /** Has the following structure:
     *
     * (C1 & ~N11 & ... & ~Nn) | (C2 & ~N21 & ... & ~N2n) | ...
     *
     * C2 is either not a subtype of any other Cm, or it is and is also a
     * subtype of one of Nmn types within the same cluster.
     *
     * Each TypeId is a class type.
     */
    std::unordered_map<TypeId, TypeIds> externTypes;

    /**
     * In order to maintain a consistent insertion order, we use this vector to
     * keep track of it. An ordered std::map will sort by pointer identity,
     * which is undesirable.
     */
    std::vector<TypeId> ordering;

    void pushPair(TypeId ty, TypeIds negations);

    void resetToNever();
    bool isNever() const;
};

// A normalized function type can be `never`, the top function type `function`,
// or an intersection of function types.
//
// NOTE: type normalization can fail on function types with generics (e.g.
// because we do not support unions and intersections of generic type packs), so
// this type may contain `error`.
struct NormalizedFunctionType
{
    bool isTop = false;
    TypeIds parts;

    void resetToNever();
    void resetToTop();

    bool isNever() const;
};

// A normalized generic/free type is a union, where each option is of the form (X & T) where
// * X is either a free type, a generic or a blocked type.
// * T is a normalized type.
struct NormalizedType;
using NormalizedTyvars = std::unordered_map<TypeId, std::unique_ptr<NormalizedType>>;

// Operations provided by `Normalizer` can have ternary results:
//   1. The operation returned true.
//   2. The operation returned false.
//   3. They can hit resource limitations, which invalidates _all normalized types_.
enum class NormalizationResult
{
    // The operation returned true or succeeded.
    True,
    // The operation returned false or failed.
    False,
    // Resource limits were hit, invalidating all normalized types.
    HitLimits,
};

// A normalized type is either any, unknown, or one of the form P | T | F | G where
// * P is a union of primitive types (including singletons, extern types and the error type)
// * T is a union of table types
// * F is a union of an intersection of function types
// * G is a union of generic/free/blocked types, intersected with a normalized type
struct NormalizedType
{
    // The top part of the type.
    // This type is either never, unknown, or any.
    // If this type is not never, all the other fields are null.
    TypeId tops;

    // The boolean part of the type.
    // This type is either never, boolean type, or a boolean singleton.
    TypeId booleans;

    NormalizedExternType externTypes;

    // The error part of the type.
    // This type is either never or the error type.
    TypeId errors;

    // The nil part of the type.
    // This type is either never or nil.
    TypeId nils;

    // The number part of the type.
    // This type is either never or number.
    TypeId numbers;

    // The string part of the type.
    // This may be the `string` type, or a union of singletons.
    NormalizedStringType strings;

    // The thread part of the type.
    // This type is either never or thread.
    TypeId threads;

    // The buffer part of the type.
    // This type is either never or buffer.
    TypeId buffers;

    // The (meta)table part of the type.
    // Each element of this set is a (meta)table type, or the top `table` type.
    // An empty set denotes never.
    TypeIds tables;

    // The function part of the type.
    NormalizedFunctionType functions;

    // The generic/free part of the type.
    NormalizedTyvars tyvars;

    // Free types, blocked types, and certain other types change shape as type
    // inference is done. If we were to cache the normalization of these types,
    // we'd be reusing bad, stale data.
    bool isCacheable = true;

    explicit NormalizedType(NotNull<BuiltinTypes> builtinTypes);

    NormalizedType() = delete;
    ~NormalizedType() = default;

    NormalizedType(const NormalizedType&) = delete;
    NormalizedType& operator=(const NormalizedType&) = delete;

    NormalizedType(NormalizedType&&) = default;
    NormalizedType& operator=(NormalizedType&&) = default;

    // IsType functions
    bool isUnknown() const;
    /// Returns true if the type is exactly a number. Behaves like Type::isNumber()
    bool isExactlyNumber() const;

    /// Returns true if the type is a subtype of string(it could be a singleton). Behaves like Type::isString()
    bool isSubtypeOfString() const;

    /// Returns true if the type is a subtype of boolean(it could be a singleton). Behaves like Type::isBoolean()
    bool isSubtypeOfBooleans() const;

    /// Returns true if this type should result in error suppressing behavior.
    bool shouldSuppressErrors() const;

    /// Returns true if this type contains the primitve top table type, `table`.
    bool hasTopTable() const;

    /// Returns true if this type is `nil` or `nil | *error-type*`
    bool isNil() const;

    // Helpers that improve readability of the above (they just say if the component is present)
    bool hasTops() const;
    bool hasBooleans() const;
    bool hasExternTypes() const;
    bool hasErrors() const;
    bool hasNils() const;
    bool hasNumbers() const;
    bool hasStrings() const;
    bool hasThreads() const;
    bool hasBuffers() const;
    bool hasTables() const;
    bool hasFunctions() const;
    bool hasTyvars() const;

    bool isFalsy() const;
    bool isTruthy() const;
};


using SeenTablePropPairs = Set<std::pair<TypeId, TypeId>, TypeIdPairHash>;

class Normalizer
{
    std::unordered_map<TypeId, std::shared_ptr<NormalizedType>> cachedNormals;
    std::unordered_map<const TypeIds*, TypeId> cachedIntersections;
    std::unordered_map<const TypeIds*, TypeId> cachedUnions;
    std::unordered_map<const TypeIds*, std::unique_ptr<TypeIds>> cachedTypeIds;

    DenseHashMap<TypeId, bool> cachedIsInhabited{nullptr};
    DenseHashMap<std::pair<TypeId, TypeId>, bool, TypeIdPairHash> cachedIsInhabitedIntersection{{nullptr, nullptr}};

    bool withinResourceLimits();
    bool useNewLuauSolver() const;

public:
    TypeArena* arena;
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<UnifierSharedState> sharedState;
    bool cacheInhabitance = false;
    SolverMode solverMode;
    Normalizer(
        TypeArena* arena,
        NotNull<BuiltinTypes> builtinTypes,
        NotNull<UnifierSharedState> sharedState,
        SolverMode solver,
        bool cacheInhabitance = false
    );
    Normalizer(const Normalizer&) = delete;
    Normalizer(Normalizer&&) = delete;
    Normalizer() = delete;
    ~Normalizer() = default;
    Normalizer& operator=(Normalizer&&) = delete;
    Normalizer& operator=(Normalizer&) = delete;

    // If this returns null, the typechecker should emit a "too complex" error
    std::shared_ptr<const NormalizedType> normalize(TypeId ty);
    void clearNormal(NormalizedType& norm);

    // ------- Cached TypeIds
    TypeId unionType(TypeId here, TypeId there);
    TypeId intersectionType(TypeId here, TypeId there);
    const TypeIds* cacheTypeIds(TypeIds tys);
    void clearCaches();

    // ------- Normalizing unions
    void unionTysWithTy(TypeIds& here, TypeId there);
    TypeId unionOfTops(TypeId here, TypeId there);
    TypeId unionOfBools(TypeId here, TypeId there);
    void unionExternTypesWithExternType(TypeIds& heres, TypeId there);
    void unionExternTypes(TypeIds& heres, const TypeIds& theres);
    void unionExternTypesWithExternType(NormalizedExternType& heres, TypeId there);
    void unionExternTypes(NormalizedExternType& heres, const NormalizedExternType& theres);
    void unionStrings(NormalizedStringType& here, const NormalizedStringType& there);
    std::optional<TypePackId> unionOfTypePacks(TypePackId here, TypePackId there);
    std::optional<TypeId> unionOfFunctions(TypeId here, TypeId there);
    std::optional<TypeId> unionSaturatedFunctions(TypeId here, TypeId there);
    void unionFunctionsWithFunction(NormalizedFunctionType& heress, TypeId there);
    void unionFunctions(NormalizedFunctionType& heress, const NormalizedFunctionType& theress);
    void unionTablesWithTable(TypeIds& heres, TypeId there);
    void unionTables(TypeIds& heres, const TypeIds& theres);
    NormalizationResult unionNormals(NormalizedType& here, const NormalizedType& there, int ignoreSmallerTyvars = -1);
    NormalizationResult unionNormalWithTy(
        NormalizedType& here,
        TypeId there,
        SeenTablePropPairs& seenTablePropPairs,
        Set<TypeId>& seenSetTypes,
        int ignoreSmallerTyvars = -1
    );

    // ------- Negations
    std::optional<NormalizedType> negateNormal(const NormalizedType& here);
    TypeIds negateAll(const TypeIds& theres);
    TypeId negate(TypeId there);
    void subtractPrimitive(NormalizedType& here, TypeId ty);
    void subtractSingleton(NormalizedType& here, TypeId ty);
    NormalizationResult intersectNormalWithNegationTy(TypeId toNegate, NormalizedType& intersect);

    // ------- Normalizing intersections
    TypeId intersectionOfTops(TypeId here, TypeId there);
    TypeId intersectionOfBools(TypeId here, TypeId there);
    void intersectExternTypes(NormalizedExternType& heres, const NormalizedExternType& theres);
    void intersectExternTypesWithExternType(NormalizedExternType& heres, TypeId there);
    void intersectStrings(NormalizedStringType& here, const NormalizedStringType& there);
    std::optional<TypePackId> intersectionOfTypePacks(TypePackId here, TypePackId there);
    std::optional<TypeId> intersectionOfTables(TypeId here, TypeId there, SeenTablePropPairs& seenTablePropPairs, Set<TypeId>& seenSet);
    void intersectTablesWithTable(TypeIds& heres, TypeId there, SeenTablePropPairs& seenTablePropPairs, Set<TypeId>& seenSetTypes);
    void intersectTables(TypeIds& heres, const TypeIds& theres);
    std::optional<TypeId> intersectionOfFunctions(TypeId here, TypeId there);
    void intersectFunctionsWithFunction(NormalizedFunctionType& heress, TypeId there);
    void intersectFunctions(NormalizedFunctionType& heress, const NormalizedFunctionType& theress);
    NormalizationResult intersectTyvarsWithTy(
        NormalizedTyvars& here,
        TypeId there,
        SeenTablePropPairs& seenTablePropPairs,
        Set<TypeId>& seenSetTypes
    );
    NormalizationResult intersectNormals(NormalizedType& here, const NormalizedType& there, int ignoreSmallerTyvars = -1);
    NormalizationResult intersectNormalWithTy(NormalizedType& here, TypeId there, SeenTablePropPairs& seenTablePropPairs, Set<TypeId>& seenSetTypes);
    NormalizationResult normalizeIntersections(
        const std::vector<TypeId>& intersections,
        NormalizedType& outType,
        SeenTablePropPairs& seenTablePropPairs,
        Set<TypeId>& seenSet
    );

    // Check for inhabitance
    NormalizationResult isInhabited(TypeId ty);
    NormalizationResult isInhabited(TypeId ty, Set<TypeId>& seen);
    NormalizationResult isInhabited(const NormalizedType* norm);
    NormalizationResult isInhabited(const NormalizedType* norm, Set<TypeId>& seen);

    // Check for intersections being inhabited
    NormalizationResult isIntersectionInhabited(TypeId left, TypeId right);
    NormalizationResult isIntersectionInhabited(TypeId left, TypeId right, SeenTablePropPairs& seenTablePropPairs, Set<TypeId>& seenSet);

    // -------- Convert back from a normalized type to a type
    TypeId typeFromNormal(const NormalizedType& norm);
};

} // namespace Luau
