// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Common.h"
#include "Luau/Connective.h"
#include "Luau/DataFlowGraph.h"
#include "Luau/DenseHash.h"
#include "Luau/Def.h"
#include "Luau/NotNull.h"
#include "Luau/Predicate.h"
#include "Luau/Unifiable.h"
#include "Luau/Variant.h"

#include <deque>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

LUAU_FASTINT(LuauTableTypeMaximumStringifierLength)
LUAU_FASTINT(LuauTypeMaximumStringifierLength)

namespace Luau
{

struct TypeArena;
struct Scope;
using ScopePtr = std::shared_ptr<Scope>;

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

struct Type;

// Should never be null
using TypeId = const Type*;

using Name = std::string;

// A free type var is one whose exact shape has yet to be fully determined.
using FreeType = Unifiable::Free;

// When a free type var is unified with any other, it is then "bound"
// to that type var, indicating that the two types are actually the same type.
using BoundType = Unifiable::Bound<TypeId>;

using GenericType = Unifiable::Generic;

using Tags = std::vector<std::string>;

using ModuleName = std::string;

/** A Type that cannot be computed.
 *
 * BlockedTypes essentially serve as a way to encode partial ordering on the
 * constraint graph. Until a BlockedType is unblocked by its owning
 * constraint, nothing at all can be said about it. Constraints that need to
 * process a BlockedType cannot be dispatched.
 *
 * Whenever a BlockedType is added to the graph, we also record a constraint
 * that will eventually unblock it.
 */
struct BlockedType
{
    BlockedType();
    int index;

    static int nextIndex;
};

struct PrimitiveType
{
    enum Type
    {
        NilType, // ObjC #defines Nil :(
        Boolean,
        Number,
        String,
        Thread,
        Function,
    };

    Type type;
    std::optional<TypeId> metatable; // string has a metatable

    explicit PrimitiveType(Type type)
        : type(type)
    {
    }

    explicit PrimitiveType(Type type, TypeId metatable)
        : type(type)
        , metatable(metatable)
    {
    }
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

struct SingletonType
{
    explicit SingletonType(const SingletonVariant& variant)
        : variant(variant)
    {
    }

    explicit SingletonType(SingletonVariant&& variant)
        : variant(std::move(variant))
    {
    }

    // Default operator== is C++20.
    bool operator==(const SingletonType& rhs) const
    {
        return variant == rhs.variant;
    }

    bool operator!=(const SingletonType& rhs) const
    {
        return !(*this == rhs);
    }

    SingletonVariant variant;
};

template<typename T>
const T* get(const SingletonType* stv)
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
// Does not exactly belong in Type.h, but this is the only way to appease the compiler.
template<typename T>
struct WithPredicate
{
    T type;
    PredicateVec predicates;
};

using MagicFunction = std::function<std::optional<WithPredicate<TypePackId>>(
    struct TypeChecker&, const std::shared_ptr<struct Scope>&, const class AstExprCall&, WithPredicate<TypePackId>)>;

struct MagicFunctionCallContext
{
    NotNull<struct ConstraintSolver> solver;
    const class AstExprCall* callSite;
    TypePackId arguments;
    TypePackId result;
};

using DcrMagicFunction = bool (*)(MagicFunctionCallContext);

struct MagicRefinementContext
{
    ScopePtr scope;
    NotNull<struct ConstraintGraphBuilder> cgb;
    NotNull<const DataFlowGraph> dfg;
    NotNull<ConnectiveArena> connectiveArena;
    std::vector<ConnectiveId> argumentConnectives;
    const class AstExprCall* callSite;
};

using DcrMagicRefinement = std::vector<ConnectiveId> (*)(const MagicRefinementContext&);

struct FunctionType
{
    // Global monomorphic function
    FunctionType(TypePackId argTypes, TypePackId retTypes, std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    // Global polymorphic function
    FunctionType(std::vector<TypeId> generics, std::vector<TypePackId> genericPacks, TypePackId argTypes, TypePackId retTypes,
        std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    // Local monomorphic function
    FunctionType(TypeLevel level, TypePackId argTypes, TypePackId retTypes, std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);
    FunctionType(
        TypeLevel level, Scope* scope, TypePackId argTypes, TypePackId retTypes, std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    // Local polymorphic function
    FunctionType(TypeLevel level, std::vector<TypeId> generics, std::vector<TypePackId> genericPacks, TypePackId argTypes, TypePackId retTypes,
        std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);
    FunctionType(TypeLevel level, Scope* scope, std::vector<TypeId> generics, std::vector<TypePackId> genericPacks, TypePackId argTypes,
        TypePackId retTypes, std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    std::optional<FunctionDefinition> definition;
    /// These should all be generic
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;
    std::vector<std::optional<FunctionArgument>> argNames;
    Tags tags;
    TypeLevel level;
    Scope* scope = nullptr;
    TypePackId argTypes;
    TypePackId retTypes;
    MagicFunction magicFunction = nullptr;
    DcrMagicFunction dcrMagicFunction = nullptr;     // Fired only while solving constraints
    DcrMagicRefinement dcrMagicRefinement = nullptr; // Fired only while generating constraints
    bool hasSelf;
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

struct TableType
{
    // We choose std::map over unordered_map here just because we have unit tests that compare
    // textual outputs.  I don't want to spend the effort making them resilient in the case where
    // random events cause the iteration order of the map elements to change.
    // If this shows up in a profile, we can revisit it.
    using Props = std::map<Name, Property>;

    TableType() = default;
    explicit TableType(TableState state, TypeLevel level, Scope* scope = nullptr);
    TableType(const Props& props, const std::optional<TableIndexer>& indexer, TypeLevel level, TableState state);
    TableType(const Props& props, const std::optional<TableIndexer>& indexer, TypeLevel level, Scope* scope, TableState state);

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
    Location definitionLocation;

    std::optional<TypeId> boundTo;
    Tags tags;

    // Methods of this table that have an untyped self will use the same shared self type.
    std::optional<TypeId> selfTy;
};

// Represents a metatable attached to a table type. Somewhat analogous to a bound type.
struct MetatableType
{
    // Always points to a TableType.
    TypeId table;
    // Always points to either a TableType or a MetatableType.
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
struct ClassType
{
    using Props = TableType::Props;

    Name name;
    Props props;
    std::optional<TypeId> parent;
    std::optional<TypeId> metatable; // metaclass?
    Tags tags;
    std::shared_ptr<ClassUserData> userData;
    ModuleName definitionModuleName;

    ClassType(Name name, Props props, std::optional<TypeId> parent, std::optional<TypeId> metatable, Tags tags,
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
 * a type variable than a BlockedType affords, hence this. Each
 * PendingExpansionType has a corresponding TypeAliasExpansionConstraint
 * enqueued in the solver to convert it to an actual instantiated type
 */
struct PendingExpansionType
{
    PendingExpansionType(std::optional<AstName> prefix, AstName name, std::vector<TypeId> typeArguments, std::vector<TypePackId> packArguments);
    std::optional<AstName> prefix;
    AstName name;
    std::vector<TypeId> typeArguments;
    std::vector<TypePackId> packArguments;
    size_t index;

    static size_t nextIndex;
};

// Anything!  All static checking is off.
struct AnyType
{
};

// `T | U`
struct UnionType
{
    std::vector<TypeId> options;
};

// `T & U`
struct IntersectionType
{
    std::vector<TypeId> parts;
};

struct LazyType
{
    std::function<TypeId()> thunk;
};

struct UnknownType
{
};

struct NeverType
{
};

// `~T`
struct NegationType
{
    TypeId ty;
};

using ErrorType = Unifiable::Error;

using TypeVariant = Unifiable::Variant<TypeId, PrimitiveType, BlockedType, PendingExpansionType, SingletonType, FunctionType, TableType,
    MetatableType, ClassType, AnyType, UnionType, IntersectionType, LazyType, UnknownType, NeverType, NegationType>;

struct Type final
{
    explicit Type(const TypeVariant& ty)
        : ty(ty)
    {
    }

    explicit Type(TypeVariant&& ty)
        : ty(std::move(ty))
    {
    }

    Type(const TypeVariant& ty, bool persistent)
        : ty(ty)
        , persistent(persistent)
    {
    }

    // Re-assignes the content of the type, but doesn't change the owning arena and can't make type persistent.
    void reassign(const Type& rhs)
    {
        ty = rhs.ty;
        documentationSymbol = rhs.documentationSymbol;
    }

    TypeVariant ty;

    // Kludge: A persistent Type is one that belongs to the global scope.
    // Global type bindings are immutable but are reused many times.
    // Persistent Types do not get cloned.
    bool persistent = false;

    std::optional<std::string> documentationSymbol;

    // Pointer to the type arena that allocated this type.
    TypeArena* owningArena = nullptr;

    bool operator==(const Type& rhs) const;
    bool operator!=(const Type& rhs) const;

    Type& operator=(const TypeVariant& rhs);
    Type& operator=(TypeVariant&& rhs);

    Type& operator=(const Type& rhs);
};

using SeenSet = std::set<std::pair<const void*, const void*>>;
bool areEqual(SeenSet& seen, const Type& lhs, const Type& rhs);

// Follow BoundTypes until we get to something real
TypeId follow(TypeId t);
TypeId follow(TypeId t, std::function<TypeId(TypeId)> mapper);

std::vector<TypeId> flattenIntersection(TypeId ty);

bool isPrim(TypeId ty, PrimitiveType::Type primType);
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

std::optional<TypeId> getMetatable(TypeId type, NotNull<struct BuiltinTypes> builtinTypes);
TableType* getMutableTableType(TypeId type);
const TableType* getTableType(TypeId type);

// If the type has a name, return that.  Else if it has a synthetic name, return that.
// Returns nullptr if the type has no name.
const std::string* getName(TypeId type);

// Returns name of the module where type was defined if type has that information
std::optional<ModuleName> getDefinitionModuleName(TypeId type);

// Checks whether a union contains all types of another union.
bool isSubset(const UnionType& super, const UnionType& sub);

// Checks if a type contains generic type binders
bool isGeneric(const TypeId ty);

// Checks if a type may be instantiated to one containing generic type binders
bool maybeGeneric(const TypeId ty);

// Checks if a type is of the form T1|...|Tn where one of the Ti is a singleton
bool maybeSingleton(TypeId ty);

// Checks if the length operator can be applied on the value of type
bool hasLength(TypeId ty, DenseHashSet<TypeId>& seen, int* recursionCount);

struct BuiltinTypes
{
    BuiltinTypes();
    ~BuiltinTypes();
    BuiltinTypes(const BuiltinTypes&) = delete;
    void operator=(const BuiltinTypes&) = delete;

    TypeId errorRecoveryType(TypeId guess);
    TypePackId errorRecoveryTypePack(TypePackId guess);
    TypeId errorRecoveryType();
    TypePackId errorRecoveryTypePack();

private:
    std::unique_ptr<struct TypeArena> arena;
    bool debugFreezeArena = false;

    TypeId makeStringMetatable();

public:
    const TypeId nilType;
    const TypeId numberType;
    const TypeId stringType;
    const TypeId booleanType;
    const TypeId threadType;
    const TypeId functionType;
    const TypeId classType;
    const TypeId trueType;
    const TypeId falseType;
    const TypeId anyType;
    const TypeId unknownType;
    const TypeId neverType;
    const TypeId errorType;
    const TypeId falsyType;
    const TypeId truthyType;

    const TypeId optionalNumberType;
    const TypeId optionalStringType;

    const TypePackId anyTypePack;
    const TypePackId neverTypePack;
    const TypePackId uninhabitableTypePack;
    const TypePackId errorTypePack;
};

void persist(TypeId ty);
void persist(TypePackId tp);

const TypeLevel* getLevel(TypeId ty);
TypeLevel* getMutableLevel(TypeId ty);

std::optional<TypeLevel> getLevel(TypePackId tp);

const Property* lookupClassProp(const ClassType* cls, const Name& name);

// Whether `cls` is a subclass of `parent`
bool isSubclass(const ClassType* cls, const ClassType* parent);

Type* asMutable(TypeId ty);

template<typename T>
const T* get(TypeId tv)
{
    LUAU_ASSERT(tv);

    if constexpr (!std::is_same_v<T, BoundType>)
        LUAU_ASSERT(get_if<BoundType>(&tv->ty) == nullptr);

    return get_if<T>(&tv->ty);
}

template<typename T>
T* getMutable(TypeId tv)
{
    LUAU_ASSERT(tv);

    if constexpr (!std::is_same_v<T, BoundType>)
        LUAU_ASSERT(get_if<BoundType>(&tv->ty) == nullptr);

    return get_if<T>(&asMutable(tv)->ty);
}

const std::vector<TypeId>& getTypes(const UnionType* utv);
const std::vector<TypeId>& getTypes(const IntersectionType* itv);

template<typename T>
struct TypeIterator;

using UnionTypeIterator = TypeIterator<UnionType>;
UnionTypeIterator begin(const UnionType* utv);
UnionTypeIterator end(const UnionType* utv);

using IntersectionTypeIterator = TypeIterator<IntersectionType>;
IntersectionTypeIterator begin(const IntersectionType* itv);
IntersectionTypeIterator end(const IntersectionType* itv);

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
        descend();
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

    TypeId operator*()
    {
        descend();

        LUAU_ASSERT(!stack.empty());

        auto [t, currentIndex] = stack.front();
        LUAU_ASSERT(t);

        const std::vector<TypeId>& types = getTypes(t);
        LUAU_ASSERT(currentIndex < types.size());

        TypeId ty = follow(types[currentIndex]);
        LUAU_ASSERT(!get<T>(ty));

        return ty;
    }

    // Normally, we'd have `begin` and `end` be a template but there's too much trouble
    // with templates portability in this area, so not worth it. Thanks MSVC.
    friend UnionTypeIterator end(const UnionType*);
    friend IntersectionTypeIterator end(const IntersectionType*);

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
