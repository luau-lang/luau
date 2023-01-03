// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/NotNull.h"
#include "Luau/Type.h"
#include "Luau/UnifierSharedState.h"

#include <memory>

namespace Luau
{

struct InternalErrorReporter;
struct Module;
struct Scope;
struct BuiltinTypes;

using ModulePtr = std::shared_ptr<Module>;

bool isSubtype(TypeId subTy, TypeId superTy, NotNull<Scope> scope, NotNull<BuiltinTypes> builtinTypes, InternalErrorReporter& ice);
bool isSubtype(TypePackId subTy, TypePackId superTy, NotNull<Scope> scope, NotNull<BuiltinTypes> builtinTypes, InternalErrorReporter& ice);

class TypeIds
{
private:
    std::unordered_set<TypeId> types;
    std::vector<TypeId> order;
    std::size_t hash = 0;

public:
    using iterator = std::vector<TypeId>::iterator;
    using const_iterator = std::vector<TypeId>::const_iterator;

    TypeIds(const TypeIds&) = default;
    TypeIds(TypeIds&&) = default;
    TypeIds() = default;
    ~TypeIds() = default;
    TypeIds& operator=(TypeIds&&) = default;

    void insert(TypeId ty);
    /// Erase every element that does not also occur in tys
    void retain(const TypeIds& tys);
    void clear();

    TypeId front() const;
    iterator begin();
    iterator end();
    const_iterator begin() const;
    const_iterator end() const;
    iterator erase(const_iterator it);

    size_t size() const;
    bool empty() const;
    size_t count(TypeId ty) const;

    template<class Iterator>
    void insert(Iterator begin, Iterator end)
    {
        for (Iterator it = begin; it != end; ++it)
            insert(*it);
    }

    bool operator==(const TypeIds& there) const;
    size_t getHash() const;
};

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

struct NormalizedClassType
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
    std::unordered_map<TypeId, TypeIds> classes;

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
    NormalizedFunctionType();

    bool isTop = false;
    // TODO: Remove this wrapping optional when clipping
    // FFlagLuauNegatedFunctionTypes.
    std::optional<TypeIds> parts;

    void resetToNever();
    void resetToTop();

    bool isNever() const;
};

// A normalized generic/free type is a union, where each option is of the form (X & T) where
// * X is either a free type or a generic
// * T is a normalized type.
struct NormalizedType;
using NormalizedTyvars = std::unordered_map<TypeId, std::unique_ptr<NormalizedType>>;

bool isInhabited_DEPRECATED(const NormalizedType& norm);

// A normalized type is either any, unknown, or one of the form P | T | F | G where
// * P is a union of primitive types (including singletons, classes and the error type)
// * T is a union of table types
// * F is a union of an intersection of function types
// * G is a union of generic/free normalized types, intersected with a normalized type
struct NormalizedType
{
    // The top part of the type.
    // This type is either never, unknown, or any.
    // If this type is not never, all the other fields are null.
    TypeId tops;

    // The boolean part of the type.
    // This type is either never, boolean type, or a boolean singleton.
    TypeId booleans;

    NormalizedClassType classes;

    // The class part of the type.
    // Each element of this set is a class, and none of the classes are subclasses of each other.
    TypeIds DEPRECATED_classes;

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

    // The (meta)table part of the type.
    // Each element of this set is a (meta)table type.
    TypeIds tables;

    // The function part of the type.
    NormalizedFunctionType functions;

    // The generic/free part of the type.
    NormalizedTyvars tyvars;

    NormalizedType(NotNull<BuiltinTypes> builtinTypes);

    NormalizedType() = delete;
    ~NormalizedType() = default;

    NormalizedType(const NormalizedType&) = delete;
    NormalizedType& operator=(const NormalizedType&) = delete;

    NormalizedType(NormalizedType&&) = default;
    NormalizedType& operator=(NormalizedType&&) = default;
};

class Normalizer
{
    std::unordered_map<TypeId, std::unique_ptr<NormalizedType>> cachedNormals;
    std::unordered_map<const TypeIds*, TypeId> cachedIntersections;
    std::unordered_map<const TypeIds*, TypeId> cachedUnions;
    std::unordered_map<const TypeIds*, std::unique_ptr<TypeIds>> cachedTypeIds;
    bool withinResourceLimits();

public:
    TypeArena* arena;
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<UnifierSharedState> sharedState;

    Normalizer(TypeArena* arena, NotNull<BuiltinTypes> builtinTypes, NotNull<UnifierSharedState> sharedState);
    Normalizer(const Normalizer&) = delete;
    Normalizer(Normalizer&&) = delete;
    Normalizer() = delete;
    ~Normalizer() = default;
    Normalizer& operator=(Normalizer&&) = delete;
    Normalizer& operator=(Normalizer&) = delete;

    // If this returns null, the typechecker should emit a "too complex" error
    const NormalizedType* normalize(TypeId ty);
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
    void unionClassesWithClass(TypeIds& heres, TypeId there);
    void unionClasses(TypeIds& heres, const TypeIds& theres);
    void unionClassesWithClass(NormalizedClassType& heres, TypeId there);
    void unionClasses(NormalizedClassType& heres, const NormalizedClassType& theres);
    void unionStrings(NormalizedStringType& here, const NormalizedStringType& there);
    std::optional<TypePackId> unionOfTypePacks(TypePackId here, TypePackId there);
    std::optional<TypeId> unionOfFunctions(TypeId here, TypeId there);
    std::optional<TypeId> unionSaturatedFunctions(TypeId here, TypeId there);
    void unionFunctionsWithFunction(NormalizedFunctionType& heress, TypeId there);
    void unionFunctions(NormalizedFunctionType& heress, const NormalizedFunctionType& theress);
    void unionTablesWithTable(TypeIds& heres, TypeId there);
    void unionTables(TypeIds& heres, const TypeIds& theres);
    bool unionNormals(NormalizedType& here, const NormalizedType& there, int ignoreSmallerTyvars = -1);
    bool unionNormalWithTy(NormalizedType& here, TypeId there, int ignoreSmallerTyvars = -1);

    // ------- Negations
    std::optional<NormalizedType> negateNormal(const NormalizedType& here);
    TypeIds negateAll(const TypeIds& theres);
    TypeId negate(TypeId there);
    void subtractPrimitive(NormalizedType& here, TypeId ty);
    void subtractSingleton(NormalizedType& here, TypeId ty);

    // ------- Normalizing intersections
    TypeId intersectionOfTops(TypeId here, TypeId there);
    TypeId intersectionOfBools(TypeId here, TypeId there);
    void DEPRECATED_intersectClasses(TypeIds& heres, const TypeIds& theres);
    void DEPRECATED_intersectClassesWithClass(TypeIds& heres, TypeId there);
    void intersectClasses(NormalizedClassType& heres, const NormalizedClassType& theres);
    void intersectClassesWithClass(NormalizedClassType& heres, TypeId there);
    void intersectStrings(NormalizedStringType& here, const NormalizedStringType& there);
    std::optional<TypePackId> intersectionOfTypePacks(TypePackId here, TypePackId there);
    std::optional<TypeId> intersectionOfTables(TypeId here, TypeId there);
    void intersectTablesWithTable(TypeIds& heres, TypeId there);
    void intersectTables(TypeIds& heres, const TypeIds& theres);
    std::optional<TypeId> intersectionOfFunctions(TypeId here, TypeId there);
    void intersectFunctionsWithFunction(NormalizedFunctionType& heress, TypeId there);
    void intersectFunctions(NormalizedFunctionType& heress, const NormalizedFunctionType& theress);
    bool intersectTyvarsWithTy(NormalizedTyvars& here, TypeId there);
    bool intersectNormals(NormalizedType& here, const NormalizedType& there, int ignoreSmallerTyvars = -1);
    bool intersectNormalWithTy(NormalizedType& here, TypeId there);

    // Check for inhabitance
    bool isInhabited(TypeId ty, std::unordered_set<TypeId> seen = {});
    bool isInhabited(const NormalizedType* norm, std::unordered_set<TypeId> seen = {});

    // -------- Convert back from a normalized type to a type
    TypeId typeFromNormal(const NormalizedType& norm);
};

} // namespace Luau
