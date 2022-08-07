// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash.h"
#include "Luau/Predicate.h"
#include "Luau/Unifiable.h"
#include "Luau/Variant.h"
#include "Luau/Common.h"

#include <set>
#include <string>
#include <map>
#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <deque>
#include <memory>
#include <optional>

LUAU_FASTINT(LuauTableTypeMaximumStringifierLength)
LUAU_FASTINT(LuauTypeMaximumStringifierLength)

namespace Luau
{

struct TypeArena;
struct Scope;

/**
 * There are three kinds of type variables:
 * - `Free` variables are metavariables, which stand for unconstrained types.
 * - `Bound` variables are metavariables that have an equality constraint.
 * - `Generic` variables are type variables that are bound by generic functions.
 *
 * For example, consider the program:
 * ```
 * function(x, y) x.f = y end
 * ```
 * To typecheck this, we first introduce free metavariables for the types of `x` and `y`:
 * ```
 * function(x: X, y: Y) x.f = y end
 * ```
 * Type inference for the function body then produces the constraint:
 * ```
 * X = { f: Y }
 * ```
 * so `X` is now a bound metavariable. We can then quantify the metavariables,
 * which replaces any bound metavariables by their binding, and free metavariables
 * by bound generic variables:
 * ```
 * function<a>(x: { f: a }, y: a) x.f = y end
 * ```
 */

// So... why `const T*` here rather than `T*`?
// It's because we've had problems caused by the type graph being mutated
// in ways it shouldn't be, for example mutating types from other modules.
// To try to control this, we make the use of types immutable by default,
// then provide explicit mutable access via getMutable and asMutable.
// This means we can grep for all the places we're mutating the type graph,
// and it makes it possible to provide other APIs (e.g. the txn log)
// which control mutable access to the type graph.
struct TypePackVar;
using TypePackId = const TypePackVar*;

// TODO: rename to Type? CLI-39100
struct TypeVar;

// Should never be null
using TypeId = const TypeVar*;

using Name = std::string;

// A free type var is one whose exact shape has yet to be fully determined.
using FreeTypeVar = Unifiable::Free;

// When a free type var is unified with any other, it is then "bound"
// to that type var, indicating that the two types are actually the same type.
using BoundTypeVar = Unifiable::Bound<TypeId>;

using GenericTypeVar = Unifiable::Generic;

using Tags = std::vector<std::string>;

using ModuleName = std::string;

/** A TypeVar that cannot be computed.
 *
 * BlockedTypeVars essentially serve as a way to encode partial ordering on the
 * constraint graph. Until a BlockedTypeVar is unblocked by its owning
 * constraint, nothing at all can be said about it. Constraints that need to
 * process a BlockedTypeVar cannot be dispatched.
 *
 * Whenever a BlockedTypeVar is added to the graph, we also record a constraint
 * that will eventually unblock it.
 */
struct BlockedTypeVar
{
    BlockedTypeVar();
    int index;

    static int nextIndex;
};

struct PrimitiveTypeVar
{
    enum Type
    {
        NilType, // ObjC #defines Nil :(
        Boolean,
        Number,
        String,
        Thread,
    };

    Type type;
    std::optional<TypeId> metatable; // string has a metatable

    explicit PrimitiveTypeVar(Type type)
        : type(type)
    {
    }

    explicit PrimitiveTypeVar(Type type, TypeId metatable)
        : type(type)
        , metatable(metatable)
    {
    }
};

struct ConstrainedTypeVar
{
    explicit ConstrainedTypeVar(TypeLevel level)
        : level(level)
    {
    }

    explicit ConstrainedTypeVar(TypeLevel level, const std::vector<TypeId>& parts)
        : parts(parts)
        , level(level)
    {
    }

    std::vector<TypeId> parts;
    TypeLevel level;
    Scope* scope = nullptr;
};

// Singleton types https://github.com/Roblox/luau/blob/master/rfcs/syntax-singleton-types.md
// Types for true and false
struct BooleanSingleton
{
    bool value;

    bool operator==(const BooleanSingleton& rhs) const
    {
        return value == rhs.value;
    }

    bool operator!=(const BooleanSingleton& rhs) const
    {
        return !(*this == rhs);
    }
};

// Types for "foo", "bar" etc.
struct StringSingleton
{
    std::string value;

    bool operator==(const StringSingleton& rhs) const
    {
        return value == rhs.value;
    }

    bool operator!=(const StringSingleton& rhs) const
    {
        return !(*this == rhs);
    }
};

// No type for float singletons, partly because === isn't any equalivalence on floats
// (NaN != NaN).

using SingletonVariant = Luau::Variant<BooleanSingleton, StringSingleton>;

struct SingletonTypeVar
{
    explicit SingletonTypeVar(const SingletonVariant& variant)
        : variant(variant)
    {
    }

    explicit SingletonTypeVar(SingletonVariant&& variant)
        : variant(std::move(variant))
    {
    }

    // Default operator== is C++20.
    bool operator==(const SingletonTypeVar& rhs) const
    {
        return variant == rhs.variant;
    }

    bool operator!=(const SingletonTypeVar& rhs) const
    {
        return !(*this == rhs);
    }

    SingletonVariant variant;
};

template<typename T>
const T* get(const SingletonTypeVar* stv)
{
    if (stv)
        return get_if<T>(&stv->variant);
    else
        return nullptr;
}

struct GenericTypeDefinition
{
    TypeId ty;
    std::optional<TypeId> defaultValue;

    bool operator==(const GenericTypeDefinition& rhs) const;
};

struct GenericTypePackDefinition
{
    TypePackId tp;
    std::optional<TypePackId> defaultValue;

    bool operator==(const GenericTypePackDefinition& rhs) const;
};

struct FunctionArgument
{
    Name name;
    Location location;
};

struct FunctionDefinition
{
    std::optional<ModuleName> definitionModuleName;
    Location definitionLocation;
    std::optional<Location> varargLocation;
    Location originalNameLocation;
};

// TODO: Come up with a better name.
// TODO: Do we actually need this? We'll find out later if we can delete this.
// Does not exactly belong in TypeVar.h, but this is the only way to appease the compiler.
template<typename T>
struct WithPredicate
{
    T type;
    PredicateVec predicates;
};

using MagicFunction = std::function<std::optional<WithPredicate<TypePackId>>(
    struct TypeChecker&, const std::shared_ptr<struct Scope>&, const class AstExprCall&, WithPredicate<TypePackId>)>;

struct FunctionTypeVar
{
    // Global monomorphic function
    FunctionTypeVar(TypePackId argTypes, TypePackId retTypes, std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    // Global polymorphic function
    FunctionTypeVar(std::vector<TypeId> generics, std::vector<TypePackId> genericPacks, TypePackId argTypes, TypePackId retTypes,
        std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    // Local monomorphic function
    FunctionTypeVar(TypeLevel level, TypePackId argTypes, TypePackId retTypes, std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    // Local polymorphic function
    FunctionTypeVar(TypeLevel level, std::vector<TypeId> generics, std::vector<TypePackId> genericPacks, TypePackId argTypes, TypePackId retTypes,
        std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    TypeLevel level;
    Scope* scope = nullptr;
    /// These should all be generic
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;
    TypePackId argTypes;
    std::vector<std::optional<FunctionArgument>> argNames;
    TypePackId retTypes;
    std::optional<FunctionDefinition> definition;
    MagicFunction magicFunction = nullptr; // Function pointer, can be nullptr.
    bool hasSelf;
    Tags tags;
    bool hasNoGenerics = false;
};

enum class TableState
{
    // Sealed tables have an exact, known shape
    Sealed,

    // An unsealed table can have extra properties added to it
    Unsealed,

    // Tables which are not yet fully understood.  We are still in the process of learning its shape.
    Free,

    // A table which is a generic parameter to a function.  We know that certain properties are required,
    // but we don't care about the full shape.
    Generic,
};

struct TableIndexer
{
    TableIndexer(TypeId indexType, TypeId indexResultType)
        : indexType(indexType)
        , indexResultType(indexResultType)
    {
    }

    TypeId indexType;
    TypeId indexResultType;
};

struct Property
{
    TypeId type;
    bool deprecated = false;
    std::string deprecatedSuggestion;
    std::optional<Location> location = std::nullopt;
    Tags tags;
    std::optional<std::string> documentationSymbol;
};

struct TableTypeVar
{
    // We choose std::map over unordered_map here just because we have unit tests that compare
    // textual outputs.  I don't want to spend the effort making them resilient in the case where
    // random events cause the iteration order of the map elements to change.
    // If this shows up in a profile, we can revisit it.
    using Props = std::map<Name, Property>;

    TableTypeVar() = default;
    explicit TableTypeVar(TableState state, TypeLevel level);
    TableTypeVar(const Props& props, const std::optional<TableIndexer>& indexer, TypeLevel level, TableState state);

    Props props;
    std::optional<TableIndexer> indexer;

    TableState state = TableState::Unsealed;
    TypeLevel level;
    Scope* scope = nullptr;
    std::optional<std::string> name;

    // Sometimes we throw a type on a name to make for nicer error messages, but without creating any entry in the type namespace
    // We need to know which is which when we stringify types.
    std::optional<std::string> syntheticName;

    std::vector<TypeId> instantiatedTypeParams;
    std::vector<TypePackId> instantiatedTypePackParams;
    ModuleName definitionModuleName;

    std::optional<TypeId> boundTo;
    Tags tags;

    // Methods of this table that have an untyped self will use the same shared self type.
    std::optional<TypeId> selfTy;
};

// Represents a metatable attached to a table typevar. Somewhat analogous to a bound typevar.
struct MetatableTypeVar
{
    // Always points to a TableTypeVar.
    TypeId table;
    // Always points to either a TableTypeVar or a MetatableTypeVar.
    TypeId metatable;

    std::optional<std::string> syntheticName;
};

// Custom userdata of a class type
struct ClassUserData
{
    virtual ~ClassUserData() {}
};

/** The type of a class.
 *
 * Classes behave like tables in many ways, but there are some important differences:
 *
 * The properties of a class are always exactly known.
 * Classes optionally have a parent class.
 * Two different classes that share the same properties are nevertheless distinct and mutually incompatible.
 */
struct ClassTypeVar
{
    using Props = TableTypeVar::Props;

    Name name;
    Props props;
    std::optional<TypeId> parent;
    std::optional<TypeId> metatable; // metaclass?
    Tags tags;
    std::shared_ptr<ClassUserData> userData;
    ModuleName definitionModuleName;

    ClassTypeVar(Name name, Props props, std::optional<TypeId> parent, std::optional<TypeId> metatable, Tags tags,
        std::shared_ptr<ClassUserData> userData, ModuleName definitionModuleName)
        : name(name)
        , props(props)
        , parent(parent)
        , metatable(metatable)
        , tags(tags)
        , userData(userData)
        , definitionModuleName(definitionModuleName)
    {
    }
};

struct TypeFun
{
    // These should all be generic
    std::vector<GenericTypeDefinition> typeParams;
    std::vector<GenericTypePackDefinition> typePackParams;

    /** The underlying type.
     *
     * WARNING!  This is not safe to use as a type if typeParams is not empty!!
     * You must first use TypeChecker::instantiateTypeFun to turn it into a real type.
     */
    TypeId type;

    TypeFun() = default;

    explicit TypeFun(TypeId ty)
        : type(ty)
    {
    }

    TypeFun(std::vector<GenericTypeDefinition> typeParams, TypeId type)
        : typeParams(std::move(typeParams))
        , type(type)
    {
    }

    TypeFun(std::vector<GenericTypeDefinition> typeParams, std::vector<GenericTypePackDefinition> typePackParams, TypeId type)
        : typeParams(std::move(typeParams))
        , typePackParams(std::move(typePackParams))
        , type(type)
    {
    }

    bool operator==(const TypeFun& rhs) const;
};

/** Represents a pending type alias instantiation.
 *
 * In order to afford (co)recursive type aliases, we need to reason about a
 * partially-complete instantiation. This requires encoding more information in
 * a type variable than a BlockedTypeVar affords, hence this. Each
 * PendingExpansionTypeVar has a corresponding TypeAliasExpansionConstraint
 * enqueued in the solver to convert it to an actual instantiated type
 */
struct PendingExpansionTypeVar
{
    PendingExpansionTypeVar(TypeFun fn, std::vector<TypeId> typeArguments, std::vector<TypePackId> packArguments);
    TypeFun fn;
    std::vector<TypeId> typeArguments;
    std::vector<TypePackId> packArguments;
    size_t index;

    static size_t nextIndex;
};

// Anything!  All static checking is off.
struct AnyTypeVar
{
};

struct UnionTypeVar
{
    std::vector<TypeId> options;
};

struct IntersectionTypeVar
{
    std::vector<TypeId> parts;
};

struct LazyTypeVar
{
    std::function<TypeId()> thunk;
};

struct UnknownTypeVar
{
};

struct NeverTypeVar
{
};

using ErrorTypeVar = Unifiable::Error;

using TypeVariant =
    Unifiable::Variant<TypeId, PrimitiveTypeVar, ConstrainedTypeVar, BlockedTypeVar, PendingExpansionTypeVar, SingletonTypeVar, FunctionTypeVar,
        TableTypeVar, MetatableTypeVar, ClassTypeVar, AnyTypeVar, UnionTypeVar, IntersectionTypeVar, LazyTypeVar, UnknownTypeVar, NeverTypeVar>;


struct TypeVar final
{
    explicit TypeVar(const TypeVariant& ty)
        : ty(ty)
    {
    }

    explicit TypeVar(TypeVariant&& ty)
        : ty(std::move(ty))
    {
    }

    TypeVar(const TypeVariant& ty, bool persistent)
        : ty(ty)
        , persistent(persistent)
        , normal(persistent) // We assume that all persistent types are irreducable.
    {
    }

    // Re-assignes the content of the type, but doesn't change the owning arena and can't make type persistent.
    void reassign(const TypeVar& rhs)
    {
        ty = rhs.ty;
        normal = rhs.normal;
        documentationSymbol = rhs.documentationSymbol;
    }

    TypeVariant ty;

    // Kludge: A persistent TypeVar is one that belongs to the global scope.
    // Global type bindings are immutable but are reused many times.
    // Persistent TypeVars do not get cloned.
    bool persistent = false;

    // Normalization sets this for types that are fully normalized.
    // This implies that they are transitively immutable.
    bool normal = false;

    std::optional<std::string> documentationSymbol;

    // Pointer to the type arena that allocated this type.
    TypeArena* owningArena = nullptr;

    bool operator==(const TypeVar& rhs) const;
    bool operator!=(const TypeVar& rhs) const;

    TypeVar& operator=(const TypeVariant& rhs);
    TypeVar& operator=(TypeVariant&& rhs);

    TypeVar& operator=(const TypeVar& rhs);
};

using SeenSet = std::set<std::pair<const void*, const void*>>;
bool areEqual(SeenSet& seen, const TypeVar& lhs, const TypeVar& rhs);

// Follow BoundTypeVars until we get to something real
TypeId follow(TypeId t);
TypeId follow(TypeId t, std::function<TypeId(TypeId)> mapper);

std::vector<TypeId> flattenIntersection(TypeId ty);

bool isPrim(TypeId ty, PrimitiveTypeVar::Type primType);
bool isNil(TypeId ty);
bool isBoolean(TypeId ty);
bool isNumber(TypeId ty);
bool isString(TypeId ty);
bool isThread(TypeId ty);
bool isOptional(TypeId ty);
bool isTableIntersection(TypeId ty);
bool isOverloadedFunction(TypeId ty);

// True when string is a subtype of ty
bool maybeString(TypeId ty);

std::optional<TypeId> getMetatable(TypeId type);
TableTypeVar* getMutableTableType(TypeId type);
const TableTypeVar* getTableType(TypeId type);

// If the type has a name, return that.  Else if it has a synthetic name, return that.
// Returns nullptr if the type has no name.
const std::string* getName(TypeId type);

// Returns name of the module where type was defined if type has that information
std::optional<ModuleName> getDefinitionModuleName(TypeId type);

// Checks whether a union contains all types of another union.
bool isSubset(const UnionTypeVar& super, const UnionTypeVar& sub);

// Checks if a type contains generic type binders
bool isGeneric(const TypeId ty);

// Checks if a type may be instantiated to one containing generic type binders
bool maybeGeneric(const TypeId ty);

// Checks if a type is of the form T1|...|Tn where one of the Ti is a singleton
bool maybeSingleton(TypeId ty);

// Checks if the length operator can be applied on the value of type
bool hasLength(TypeId ty, DenseHashSet<TypeId>& seen, int* recursionCount);

struct SingletonTypes
{
    const TypeId nilType;
    const TypeId numberType;
    const TypeId stringType;
    const TypeId booleanType;
    const TypeId threadType;
    const TypeId trueType;
    const TypeId falseType;
    const TypeId anyType;
    const TypeId unknownType;
    const TypeId neverType;

    const TypePackId anyTypePack;
    const TypePackId neverTypePack;
    const TypePackId uninhabitableTypePack;

    SingletonTypes();
    ~SingletonTypes();
    SingletonTypes(const SingletonTypes&) = delete;
    void operator=(const SingletonTypes&) = delete;

    TypeId errorRecoveryType(TypeId guess);
    TypePackId errorRecoveryTypePack(TypePackId guess);
    TypeId errorRecoveryType();
    TypePackId errorRecoveryTypePack();

private:
    std::unique_ptr<struct TypeArena> arena;
    bool debugFreezeArena = false;

    TypeId makeStringMetatable();
};

SingletonTypes& getSingletonTypes();

void persist(TypeId ty);
void persist(TypePackId tp);

const TypeLevel* getLevel(TypeId ty);
TypeLevel* getMutableLevel(TypeId ty);

std::optional<TypeLevel> getLevel(TypePackId tp);

const Property* lookupClassProp(const ClassTypeVar* cls, const Name& name);
bool isSubclass(const ClassTypeVar* cls, const ClassTypeVar* parent);

TypeVar* asMutable(TypeId ty);

template<typename T>
const T* get(TypeId tv)
{
    LUAU_ASSERT(tv);

    if constexpr (!std::is_same_v<T, BoundTypeVar>)
        LUAU_ASSERT(get_if<BoundTypeVar>(&tv->ty) == nullptr);

    return get_if<T>(&tv->ty);
}

template<typename T>
T* getMutable(TypeId tv)
{
    LUAU_ASSERT(tv);

    if constexpr (!std::is_same_v<T, BoundTypeVar>)
        LUAU_ASSERT(get_if<BoundTypeVar>(&tv->ty) == nullptr);

    return get_if<T>(&asMutable(tv)->ty);
}

const std::vector<TypeId>& getTypes(const UnionTypeVar* utv);
const std::vector<TypeId>& getTypes(const IntersectionTypeVar* itv);
const std::vector<TypeId>& getTypes(const ConstrainedTypeVar* ctv);

template<typename T>
struct TypeIterator;

using UnionTypeVarIterator = TypeIterator<UnionTypeVar>;
UnionTypeVarIterator begin(const UnionTypeVar* utv);
UnionTypeVarIterator end(const UnionTypeVar* utv);

using IntersectionTypeVarIterator = TypeIterator<IntersectionTypeVar>;
IntersectionTypeVarIterator begin(const IntersectionTypeVar* itv);
IntersectionTypeVarIterator end(const IntersectionTypeVar* itv);

using ConstrainedTypeVarIterator = TypeIterator<ConstrainedTypeVar>;
ConstrainedTypeVarIterator begin(const ConstrainedTypeVar* ctv);
ConstrainedTypeVarIterator end(const ConstrainedTypeVar* ctv);

/* Traverses the type T yielding each TypeId.
 * If the iterator encounters a nested type T, it will instead yield each TypeId within.
 */
template<typename T>
struct TypeIterator
{
    using value_type = Luau::TypeId;
    using pointer = value_type*;
    using reference = value_type&;
    using difference_type = size_t;
    using iterator_category = std::input_iterator_tag;

    explicit TypeIterator(const T* t)
    {
        LUAU_ASSERT(t);

        const std::vector<TypeId>& types = getTypes(t);
        if (!types.empty())
            stack.push_front({t, 0});

        seen.insert(t);
    }

    TypeIterator<T>& operator++()
    {
        advance();
        descend();
        return *this;
    }

    TypeIterator<T> operator++(int)
    {
        TypeIterator<T> copy = *this;
        ++copy;
        return copy;
    }

    bool operator==(const TypeIterator<T>& rhs) const
    {
        if (!stack.empty() && !rhs.stack.empty())
            return stack.front() == rhs.stack.front();

        return stack.empty() && rhs.stack.empty();
    }

    bool operator!=(const TypeIterator<T>& rhs) const
    {
        return !(*this == rhs);
    }

    const TypeId& operator*()
    {
        LUAU_ASSERT(!stack.empty());

        descend();

        auto [t, currentIndex] = stack.front();
        LUAU_ASSERT(t);
        const std::vector<TypeId>& types = getTypes(t);
        LUAU_ASSERT(currentIndex < types.size());

        const TypeId& ty = types[currentIndex];
        LUAU_ASSERT(!get<T>(follow(ty)));
        return ty;
    }

    // Normally, we'd have `begin` and `end` be a template but there's too much trouble
    // with templates portability in this area, so not worth it. Thanks MSVC.
    friend UnionTypeVarIterator end(const UnionTypeVar*);
    friend IntersectionTypeVarIterator end(const IntersectionTypeVar*);
    friend ConstrainedTypeVarIterator end(const ConstrainedTypeVar*);

private:
    TypeIterator() = default;

    // (T* t, size_t currentIndex)
    using SavedIterInfo = std::pair<const T*, size_t>;

    std::deque<SavedIterInfo> stack;
    std::unordered_set<const T*> seen; // Only needed to protect the iterator from hanging the thread.

    void advance()
    {
        while (!stack.empty())
        {
            auto& [t, currentIndex] = stack.front();
            ++currentIndex;

            const std::vector<TypeId>& types = getTypes(t);
            if (currentIndex >= types.size())
                stack.pop_front();
            else
                break;
        }
    }

    void descend()
    {
        while (!stack.empty())
        {
            auto [current, currentIndex] = stack.front();
            const std::vector<TypeId>& types = getTypes(current);
            if (auto inner = get<T>(follow(types[currentIndex])))
            {
                // If we're about to descend into a cyclic type, we should skip over this.
                // Ideally this should never happen, but alas it does from time to time. :(
                if (seen.find(inner) != seen.end())
                    advance();
                else
                {
                    seen.insert(inner);
                    stack.push_front({inner, 0});
                }

                continue;
            }

            break;
        }
    }
};

using TypeIdPredicate = std::function<std::optional<TypeId>(TypeId)>;
std::vector<TypeId> filterMap(TypeId type, TypeIdPredicate predicate);

void attachTag(TypeId ty, const std::string& tagName);
void attachTag(Property& prop, const std::string& tagName);

bool hasTag(TypeId ty, const std::string& tagName);
bool hasTag(const Property& prop, const std::string& tagName);
bool hasTag(const Tags& tags, const std::string& tagName); // Do not use in new work.

} // namespace Luau
