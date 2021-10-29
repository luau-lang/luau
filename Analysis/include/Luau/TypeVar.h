// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

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
LUAU_FASTFLAG(LuauAddMissingFollow)

namespace Luau
{

struct TypeArena;

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
struct ExprResult
{
    T type;
    PredicateVec predicates;
};

using MagicFunction = std::function<std::optional<ExprResult<TypePackId>>(
    struct TypeChecker&, const std::shared_ptr<struct Scope>&, const class AstExprCall&, ExprResult<TypePackId>)>;

struct FunctionTypeVar
{
    // Global monomorphic function
    FunctionTypeVar(TypePackId argTypes, TypePackId retType, std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    // Global polymorphic function
    FunctionTypeVar(std::vector<TypeId> generics, std::vector<TypePackId> genericPacks, TypePackId argTypes, TypePackId retType,
        std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    // Local monomorphic function
    FunctionTypeVar(TypeLevel level, TypePackId argTypes, TypePackId retType, std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    // Local polymorphic function
    FunctionTypeVar(TypeLevel level, std::vector<TypeId> generics, std::vector<TypePackId> genericPacks, TypePackId argTypes, TypePackId retType,
        std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    TypeLevel level;
    /// These should all be generic
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;
    TypePackId argTypes;
    std::vector<std::optional<FunctionArgument>> argNames;
    TypePackId retType;
    std::optional<FunctionDefinition> definition;
    MagicFunction magicFunction = nullptr; // Function pointer, can be nullptr.
    bool hasSelf;
    Tags tags;
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
    TableTypeVar(const Props& props, const std::optional<TableIndexer>& indexer, TypeLevel level, TableState state = TableState::Unsealed);

    Props props;
    std::optional<TableIndexer> indexer;

    TableState state = TableState::Unsealed;
    TypeLevel level;
    std::optional<std::string> name;

    // Sometimes we throw a type on a name to make for nicer error messages, but without creating any entry in the type namespace
    // We need to know which is which when we stringify types.
    std::optional<std::string> syntheticName;

    std::map<Name, Location> methodDefinitionLocations;
    std::vector<TypeId> instantiatedTypeParams;
    ModuleName definitionModuleName;

    std::optional<TypeId> boundTo;
    Tags tags;
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

    ClassTypeVar(
        Name name, Props props, std::optional<TypeId> parent, std::optional<TypeId> metatable, Tags tags, std::shared_ptr<ClassUserData> userData)
        : name(name)
        , props(props)
        , parent(parent)
        , metatable(metatable)
        , tags(tags)
        , userData(userData)
    {
    }
};

struct TypeFun
{
    /// These should all be generic
    std::vector<TypeId> typeParams;

    /** The underlying type.
     *
     * WARNING!  This is not safe to use as a type if typeParams is not empty!!
     * You must first use TypeChecker::instantiateTypeFun to turn it into a real type.
     */
    TypeId type;
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

using ErrorTypeVar = Unifiable::Error;

using TypeVariant = Unifiable::Variant<TypeId, PrimitiveTypeVar, FunctionTypeVar, TableTypeVar, MetatableTypeVar, ClassTypeVar, AnyTypeVar,
    UnionTypeVar, IntersectionTypeVar, LazyTypeVar>;

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
    {
    }

    TypeVariant ty;

    // Kludge: A persistent TypeVar is one that belongs to the global scope.
    // Global type bindings are immutable but are reused many times.
    // Persistent TypeVars do not get cloned.
    bool persistent = false;

    std::optional<std::string> documentationSymbol;

    // Pointer to the type arena that allocated this type.
    // Do not depend on the value of this under any circumstances. This is for
    // debugging purposes only. This is only set in debug builds; it is nullptr
    // in all other environments.
    TypeArena* owningArena = nullptr;

    bool operator==(const TypeVar& rhs) const;
    bool operator!=(const TypeVar& rhs) const;

    TypeVar& operator=(const TypeVariant& rhs);
    TypeVar& operator=(TypeVariant&& rhs);
};

using SeenSet = std::set<std::pair<void*, void*>>;
bool areEqual(SeenSet& seen, const TypeVar& lhs, const TypeVar& rhs);

// Follow BoundTypeVars until we get to something real
TypeId follow(TypeId t);

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

std::optional<TypeId> getMetatable(TypeId type);
TableTypeVar* getMutableTableType(TypeId type);
const TableTypeVar* getTableType(TypeId type);

// If the type has a name, return that.  Else if it has a synthetic name, return that.
// Returns nullptr if the type has no name.
const std::string* getName(TypeId type);

// Checks whether a union contains all types of another union.
bool isSubset(const UnionTypeVar& super, const UnionTypeVar& sub);

// Checks if a type conains generic type binders
bool isGeneric(const TypeId ty);

// Checks if a type may be instantiated to one containing generic type binders
bool maybeGeneric(const TypeId ty);

struct SingletonTypes
{
    const TypeId nilType = &nilType_;
    const TypeId numberType = &numberType_;
    const TypeId stringType = &stringType_;
    const TypeId booleanType = &booleanType_;
    const TypeId threadType = &threadType_;
    const TypeId anyType = &anyType_;
    const TypeId errorType = &errorType_;

    SingletonTypes();
    SingletonTypes(const SingletonTypes&) = delete;
    void operator=(const SingletonTypes&) = delete;

private:
    std::unique_ptr<struct TypeArena> arena;
    TypeVar nilType_;
    TypeVar numberType_;
    TypeVar stringType_;
    TypeVar booleanType_;
    TypeVar threadType_;
    TypeVar anyType_;
    TypeVar errorType_;

    TypeId makeStringMetatable();
};

extern SingletonTypes singletonTypes;

void persist(TypeId ty);
void persist(TypePackId tp);

struct ToDotOptions
{
    bool showPointers = true;        // Show pointer value in the node label
    bool duplicatePrimitives = true; // Display primitive types and 'any' as separate nodes
};

std::string toDot(TypeId ty, const ToDotOptions& opts);
std::string toDot(TypePackId tp, const ToDotOptions& opts);

std::string toDot(TypeId ty);
std::string toDot(TypePackId tp);

void dumpDot(TypeId ty);
void dumpDot(TypePackId tp);

const TypeLevel* getLevel(TypeId ty);
TypeLevel* getMutableLevel(TypeId ty);

const Property* lookupClassProp(const ClassTypeVar* cls, const Name& name);
bool isSubclass(const ClassTypeVar* cls, const ClassTypeVar* parent);

bool hasGeneric(TypeId ty);
bool hasGeneric(TypePackId tp);

TypeVar* asMutable(TypeId ty);

template<typename T>
const T* get(TypeId tv)
{
    if (FFlag::LuauAddMissingFollow)
    {
        LUAU_ASSERT(tv);

        if constexpr (!std::is_same_v<T, BoundTypeVar>)
            LUAU_ASSERT(get_if<BoundTypeVar>(&tv->ty) == nullptr);
    }

    return get_if<T>(&tv->ty);
}

template<typename T>
T* getMutable(TypeId tv)
{
    if (FFlag::LuauAddMissingFollow)
    {
        LUAU_ASSERT(tv);

        if constexpr (!std::is_same_v<T, BoundTypeVar>)
            LUAU_ASSERT(get_if<BoundTypeVar>(&tv->ty) == nullptr);
    }

    return get_if<T>(&asMutable(tv)->ty);
}

/* Traverses the UnionTypeVar yielding each TypeId.
 * If the iterator encounters a nested UnionTypeVar, it will instead yield each TypeId within.
 *
 * Beware: the iterator does not currently filter for unique TypeIds. This may change in the future.
 */
struct UnionTypeVarIterator
{
    using value_type = Luau::TypeId;
    using pointer = value_type*;
    using reference = value_type&;
    using difference_type = size_t;
    using iterator_category = std::input_iterator_tag;

    explicit UnionTypeVarIterator(const UnionTypeVar* utv);

    UnionTypeVarIterator& operator++();
    UnionTypeVarIterator operator++(int);
    bool operator!=(const UnionTypeVarIterator& rhs);
    bool operator==(const UnionTypeVarIterator& rhs);

    const TypeId& operator*();

    friend UnionTypeVarIterator end(const UnionTypeVar* utv);

private:
    UnionTypeVarIterator() = default;

    // (UnionTypeVar* utv, size_t currentIndex)
    using SavedIterInfo = std::pair<const UnionTypeVar*, size_t>;

    std::deque<SavedIterInfo> stack;
    std::unordered_set<const UnionTypeVar*> seen; // Only needed to protect the iterator from hanging the thread.

    void advance();
    void descend();
};

UnionTypeVarIterator begin(const UnionTypeVar* utv);
UnionTypeVarIterator end(const UnionTypeVar* utv);

using TypeIdPredicate = std::function<std::optional<TypeId>(TypeId)>;
std::vector<TypeId> filterMap(TypeId type, TypeIdPredicate predicate);

// TEMP: Clip this prototype with FFlag::LuauStringMetatable
std::optional<ExprResult<TypePackId>> magicFunctionFormat(
    struct TypeChecker& typechecker, const std::shared_ptr<struct Scope>& scope, const AstExprCall& expr, ExprResult<TypePackId> exprResult);

} // namespace Luau
