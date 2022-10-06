// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/NotNull.h"
#include "Luau/TypeVar.h"
#include "Luau/UnifierSharedState.h"

#include <memory>

namespace Luau
{

struct InternalErrorReporter;
struct Module;
struct Scope;
struct SingletonTypes;

using ModulePtr = std::shared_ptr<Module>;

bool isSubtype(TypeId subTy, TypeId superTy, NotNull<Scope> scope, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice, bool anyIsTop = true);
bool isSubtype(TypePackId subTy, TypePackId superTy, NotNull<Scope> scope, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice, bool anyIsTop = true);

std::pair<TypeId, bool> normalize(
    TypeId ty, NotNull<Scope> scope, TypeArena& arena, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice);
std::pair<TypeId, bool> normalize(TypeId ty, NotNull<Module> module, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice);
std::pair<TypeId, bool> normalize(TypeId ty, const ModulePtr& module, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice);
std::pair<TypePackId, bool> normalize(
    TypePackId ty, NotNull<Scope> scope, TypeArena& arena, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice);
std::pair<TypePackId, bool> normalize(TypePackId ty, NotNull<Module> module, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice);
std::pair<TypePackId, bool> normalize(TypePackId ty, const ModulePtr& module, NotNull<SingletonTypes> singletonTypes, InternalErrorReporter& ice);

class TypeIds
{
private:
    std::unordered_set<TypeId> types;
    std::vector<TypeId> order;
    std::size_t hash = 0;

public:
    using iterator = std::vector<TypeId>::iterator;
    using const_iterator = std::vector<TypeId>::const_iterator;

    TypeIds(const TypeIds&) = delete;
    TypeIds(TypeIds&&) = default;
    TypeIds() = default;
    ~TypeIds() = default;
    TypeIds& operator=(TypeIds&&) = default;

    void insert(TypeId ty);
    /// Erase every element that does not also occur in tys
    void retain(const TypeIds& tys);
    void clear();

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

    bool operator ==(const TypeIds& there) const;
    size_t getHash() const;
};

} // namespace Luau

template<> struct std::hash<Luau::TypeIds>
{
    std::size_t operator()(const Luau::TypeIds& tys) const
    {
        return tys.getHash();
    }
};

template<> struct std::hash<const Luau::TypeIds*>
{
    std::size_t operator()(const Luau::TypeIds* tys) const
    {
        return tys->getHash();
    }
};

template<> struct std::equal_to<Luau::TypeIds>
{
    bool operator()(const Luau::TypeIds& here, const Luau::TypeIds& there) const
    {
        return here == there;
    }
};

template<> struct std::equal_to<const Luau::TypeIds*>
{
    bool operator()(const Luau::TypeIds* here, const Luau::TypeIds* there) const
    {
        return *here == *there;
    }
};

namespace Luau
{

// A normalized string type is either `string` (represented by `nullopt`)
// or a union of string singletons.
using NormalizedStringType = std::optional<std::map<std::string, TypeId>>;

// A normalized function type is either `never` (represented by `nullopt`)
// or an intersection of function types.
// NOTE: type normalization can fail on function types with generics
// (e.g. because we do not support unions and intersections of generic type packs),
// so this type may contain `error`.
using NormalizedFunctionType = std::optional<TypeIds>;

// A normalized generic/free type is a union, where each option is of the form (X & T) where
// * X is either a free type or a generic
// * T is a normalized type.
struct NormalizedType;
using NormalizedTyvars = std::unordered_map<TypeId, std::unique_ptr<NormalizedType>>;

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

    // The class part of the type.
    // Each element of this set is a class, and none of the classes are subclasses of each other.
    TypeIds classes;

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
    NormalizedStringType strings = std::map<std::string,TypeId>{};

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

    NormalizedType(NotNull<SingletonTypes> singletonTypes);

    NormalizedType(const NormalizedType&) = delete;
    NormalizedType(NormalizedType&&) = default;
    NormalizedType() = delete;
    ~NormalizedType() = default;
    NormalizedType& operator=(NormalizedType&&) = default;
    NormalizedType& operator=(NormalizedType&) = delete;
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
    NotNull<SingletonTypes> singletonTypes;
    NotNull<UnifierSharedState> sharedState;

    Normalizer(TypeArena* arena, NotNull<SingletonTypes> singletonTypes, NotNull<UnifierSharedState> sharedState);
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

    // ------- Normalizing intersections
    void intersectTysWithTy(TypeIds& here, TypeId there);
    TypeId intersectionOfTops(TypeId here, TypeId there);
    TypeId intersectionOfBools(TypeId here, TypeId there);
    void intersectClasses(TypeIds& heres, const TypeIds& theres);
    void intersectClassesWithClass(TypeIds& heres, TypeId there);
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

    // -------- Convert back from a normalized type to a type
    TypeId typeFromNormal(const NormalizedType& norm);
};

} // namespace Luau
