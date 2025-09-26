// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeFwd.h"

#include "Luau/Ast.h"
#include "Luau/Common.h"
#include "Luau/DenseHash.h"
#include "Luau/NotNull.h"
#include "Luau/Polarity.h"
#include "Luau/Predicate.h"
#include "Luau/Refinement.h"
#include "Luau/Unifiable.h"
#include "Luau/Variant.h"
#include "Luau/VecDeque.h"

#include <atomic>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

LUAU_FASTINT(LuauTableTypeMaximumStringifierLength)
LUAU_FASTINT(LuauTypeMaximumStringifierLength)

namespace Luau
{

struct TypeArena;
struct Scope;
using ScopePtr = std::shared_ptr<Scope>;
struct Module;

struct TypeFunction;
struct TypeFun;
struct Constraint;
struct Subtyping;
struct TypeChecker2;
struct BuiltinTypeFunctions;

enum struct SolverMode
{
    Old,
    New
};

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

using Name = std::string;

// A free type is one whose exact shape has yet to be fully determined.
struct FreeType
{
    // New constructors
    explicit FreeType(TypeLevel level, TypeId lowerBound, TypeId upperBound);
    // This one got promoted to explicit
    explicit FreeType(Scope* scope, TypeId lowerBound, TypeId upperBound, Polarity polarity = Polarity::Unknown);
    explicit FreeType(Scope* scope, TypeLevel level, TypeId lowerBound, TypeId upperBound);

    int index;
    TypeLevel level;
    Scope* scope = nullptr;

    // True if this free type variable is part of a mutually
    // recursive type alias whose definitions haven't been
    // resolved yet.
    bool forwardedTypeAlias = false;

    // Only used under local type inference
    TypeId lowerBound = nullptr;
    TypeId upperBound = nullptr;

    Polarity polarity = Polarity::Unknown;
};

struct GenericType
{
    // By default, generics are global, with a synthetic name
    GenericType();

    explicit GenericType(TypeLevel level);
    explicit GenericType(const Name& name, Polarity polarity = Polarity::Unknown);
    explicit GenericType(Scope* scope, Polarity polarity = Polarity::Unknown);

    GenericType(TypeLevel level, const Name& name);
    GenericType(Scope* scope, const Name& name);

    int index;
    TypeLevel level;
    Scope* scope = nullptr;
    Name name;
    bool explicitName = false;

    Polarity polarity = Polarity::Unknown;
};

// When an equality constraint is found, it is then "bound" to that type,
// indicating that the two types are actually the same type.
using BoundType = Unifiable::Bound<TypeId>;

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

    const Constraint* getOwner() const;
    void setOwner(const Constraint* newOwner);
    void replaceOwner(const Constraint* newOwner);

private:
    // The constraint that is intended to unblock this type. Other constraints
    // should block on this constraint if present.
    const Constraint* owner = nullptr;
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
        Table,
        Buffer,
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

// Singleton types https://github.com/luau-lang/rfcs/blob/master/docs/syntax-singleton-types.md
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

    WithPredicate() = default;
    explicit WithPredicate(T type)
        : type(type)
    {
    }

    WithPredicate(T type, PredicateVec predicates)
        : type(type)
        , predicates(std::move(predicates))
    {
    }
};

struct MagicFunctionCallContext
{
    NotNull<struct ConstraintSolver> solver;
    NotNull<const Constraint> constraint;
    NotNull<const AstExprCall> callSite;
    TypePackId arguments;
    TypePackId result;
};

struct MagicRefinementContext
{
    NotNull<Scope> scope;
    const class AstExprCall* callSite;
    std::vector<std::optional<TypeId>> discriminantTypes;
};

struct MagicFunctionTypeCheckContext
{
    NotNull<TypeChecker2> typechecker;
    NotNull<BuiltinTypes> builtinTypes;
    const class AstExprCall* callSite;
    TypePackId arguments;
    NotNull<Scope> checkScope;
};

struct MagicFunction
{
    virtual std::optional<WithPredicate<TypePackId>> handleOldSolver(
        struct TypeChecker&,
        const std::shared_ptr<struct Scope>&,
        const class AstExprCall&,
        WithPredicate<TypePackId>
    ) = 0;

    // Callback to allow custom typechecking of builtin function calls whose argument types
    // will only be resolved after constraint solving. For example, the arguments to string.format
    // have types that can only be decided after parsing the format string and unifying
    // with the passed in values, but the correctness of the call can only be decided after
    // all the types have been finalized.
    virtual bool infer(const MagicFunctionCallContext&) = 0;
    virtual void refine(const MagicRefinementContext&) {}

    // If a magic function needs to do its own special typechecking, do it here.
    // Returns true if magic typechecking was performed.  Return false if the
    // default typechecking logic should run.
    virtual bool typeCheck(const MagicFunctionTypeCheckContext&)
    {
        return false;
    }

    virtual ~MagicFunction() {}
};

struct FunctionType
{
    // Global monomorphic function
    FunctionType(TypePackId argTypes, TypePackId retTypes, std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    // Global polymorphic function
    FunctionType(
        std::vector<TypeId> generics,
        std::vector<TypePackId> genericPacks,
        TypePackId argTypes,
        TypePackId retTypes,
        std::optional<FunctionDefinition> defn = {},
        bool hasSelf = false
    );

    // Local monomorphic function
    FunctionType(TypeLevel level, TypePackId argTypes, TypePackId retTypes, std::optional<FunctionDefinition> defn = {}, bool hasSelf = false);

    // Local polymorphic function
    FunctionType(
        TypeLevel level,
        std::vector<TypeId> generics,
        std::vector<TypePackId> genericPacks,
        TypePackId argTypes,
        TypePackId retTypes,
        std::optional<FunctionDefinition> defn = {},
        bool hasSelf = false
    );

    std::optional<FunctionDefinition> definition;
    /// These should all be generic
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;
    std::vector<std::optional<FunctionArgument>> argNames;
    Tags tags;
    TypeLevel level;
    TypePackId argTypes;
    TypePackId retTypes;
    std::shared_ptr<MagicFunction> magic = nullptr;

    bool hasSelf;
    // `hasNoFreeOrGenericTypes` should be true if and only if the type does not have any free or generic types present inside it.
    // this flag is used as an optimization to exit early from procedures that manipulate free or generic types.
    bool hasNoFreeOrGenericTypes = false;
    bool isCheckedFunction = false;
    bool isDeprecatedFunction = false;
    std::shared_ptr<AstAttr::DeprecatedInfo> deprecatedInfo;
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
    static Property readonly(TypeId ty);
    static Property writeonly(TypeId ty);
    static Property rw(TypeId ty);                 // Shared read-write type.
    static Property rw(TypeId read, TypeId write); // Separate read-write type.

    // Invariant: at least one of the two optionals are not nullopt!
    // If the read type is not nullopt, but the write type is, then the property is readonly.
    // If the read type is nullopt, but the write type is not, then the property is writeonly.
    // If the read and write types are not nullopt, then the property is read and write.
    // Otherwise, an assertion where read and write types are both nullopt will be tripped.
    static Property create(std::optional<TypeId> read, std::optional<TypeId> write);

    bool deprecated = false;
    std::string deprecatedSuggestion;

    // If this property was inferred from an expression, this field will be
    // populated with the source location of the corresponding table property.
    std::optional<Location> location = std::nullopt;

    // If this property was built from an explicit type annotation, this field
    // will be populated with the source location of that table property.
    std::optional<Location> typeLocation = std::nullopt;

    Tags tags;
    std::optional<std::string> documentationSymbol;

    // DEPRECATED
    // TODO: Kill all constructors in favor of `Property::rw(TypeId read, TypeId write)` and friends.
    Property();
    Property(
        TypeId readTy,
        bool deprecated = false,
        const std::string& deprecatedSuggestion = "",
        std::optional<Location> location = std::nullopt,
        const Tags& tags = {},
        const std::optional<std::string>& documentationSymbol = std::nullopt,
        std::optional<Location> typeLocation = std::nullopt
    );

    // DEPRECATED: This should be removed with `LuauTypeSolverV2` clean up
    TypeId type_DEPRECATED() const;
    void setType(TypeId ty);

    // If this property has a present `writeTy`, set it equal to the `readTy`.
    // This is to ensure that if we normalize a property that has divergent
    // read and write types, we make them converge (for now).
    void makeShared();

    bool isShared() const;
    bool isReadOnly() const;
    bool isWriteOnly() const;
    bool isReadWrite() const;

    std::optional<TypeId> readTy;
    std::optional<TypeId> writeTy;
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

    // We track the number of as-yet-unadded properties to unsealed tables.
    // Some constraints will use this information to decide whether or not they
    // are able to dispatch.
    size_t remainingProps = 0;
};

// Represents a metatable attached to a table type. Somewhat analogous to a bound type.
struct MetatableType
{
    // Should always be a TableType.
    TypeId table;
    // Should almost always either be a TableType or another MetatableType,
    // though it is possible for other types (like AnyType and ErrorType) to
    // find their way here sometimes.
    TypeId metatable;

    std::optional<std::string> syntheticName;
};

// Custom userdata of a class type
struct ClassUserData
{
    virtual ~ClassUserData() {}
};

/** The type of an external userdata exposed to Luau.
 *
 * Extern types behave like tables in many ways, but there are some important differences:
 *
 * The properties of a class are always exactly known.
 * Extern types optionally have a parent type.
 * Two different extern types that share the same properties are nevertheless distinct and mutually incompatible.
 */
struct ExternType
{
    using Props = TableType::Props;

    Name name;
    Props props;
    std::optional<TypeId> parent;
    std::optional<TypeId> metatable; // metaclass?
    Tags tags;
    std::shared_ptr<ClassUserData> userData;
    ModuleName definitionModuleName;
    std::optional<Location> definitionLocation;
    std::optional<TableIndexer> indexer;

    ExternType(
        Name name,
        Props props,
        std::optional<TypeId> parent,
        std::optional<TypeId> metatable,
        Tags tags,
        std::shared_ptr<ClassUserData> userData,
        ModuleName definitionModuleName,
        std::optional<Location> definitionLocation
    )
        : name(std::move(name))
        , props(std::move(props))
        , parent(parent)
        , metatable(metatable)
        , tags(std::move(tags))
        , userData(std::move(userData))
        , definitionModuleName(std::move(definitionModuleName))
        , definitionLocation(definitionLocation)
    {
    }

    ExternType(
        Name name,
        Props props,
        std::optional<TypeId> parent,
        std::optional<TypeId> metatable,
        Tags tags,
        std::shared_ptr<ClassUserData> userData,
        ModuleName definitionModuleName,
        std::optional<Location> definitionLocation,
        std::optional<TableIndexer> indexer
    )
        : name(std::move(name))
        , props(std::move(props))
        , parent(parent)
        , metatable(metatable)
        , tags(std::move(tags))
        , userData(std::move(userData))
        , definitionModuleName(std::move(definitionModuleName))
        , definitionLocation(definitionLocation)
        , indexer(indexer)
    {
    }
};

// Data required to initialize a user-defined function and its environment
struct UserDefinedFunctionData
{
    // Store a weak module reference to ensure the lifetime requirements are preserved
    std::weak_ptr<Module> owner;

    // References to AST elements are owned by the Module allocator which also stores this type
    AstStatTypeFunction* definition = nullptr;

    DenseHashMap<Name, std::pair<AstStatTypeFunction*, size_t>> environmentFunction{""};
    DenseHashMap<Name, std::pair<TypeFun*, size_t>> environmentAlias{""};
};

enum struct TypeFunctionInstanceState
{
    // Indicates that further reduction might be possible.
    Unsolved,

    // Further reduction is not possible because one of the parameters is generic.
    Solved,

    // Further reduction is not possible because the application is undefined.
    // This always indicates an error in the code.
    //
    // eg add<nil, nil>
    Stuck,
};

/**
 * An instance of a type function that has not yet been reduced to a more concrete
 * type. The constraint solver receives a constraint to reduce each
 * TypeFunctionInstanceType to a concrete type. A design detail is important to
 * note here: the parameters for this instantiation of the type function are
 * contained within this type, so that they can be substituted.
 */
struct TypeFunctionInstanceType
{
    NotNull<const TypeFunction> function;

    std::vector<TypeId> typeArguments;
    std::vector<TypePackId> packArguments;

    std::optional<AstName> userFuncName; // Name of the user-defined type function; only available for UDTFs
    UserDefinedFunctionData userFuncData;

    TypeFunctionInstanceState state = TypeFunctionInstanceState::Unsolved;

    TypeFunctionInstanceType(
        NotNull<const TypeFunction> function,
        std::vector<TypeId> typeArguments,
        std::vector<TypePackId> packArguments,
        std::optional<AstName> userFuncName,
        UserDefinedFunctionData userFuncData
    )
        : function(function)
        , typeArguments(std::move(typeArguments))
        , packArguments(std::move(packArguments))
        , userFuncName(userFuncName)
        , userFuncData(std::move(userFuncData))
    {
    }

    TypeFunctionInstanceType(const TypeFunction& function, std::vector<TypeId> typeArguments)
        : function{&function}
        , typeArguments(std::move(typeArguments))
        , packArguments{}
    {
    }

    TypeFunctionInstanceType(const TypeFunction& function, std::vector<TypeId> typeArguments, std::vector<TypePackId> packArguments)
        : function{&function}
        , typeArguments(std::move(typeArguments))
        , packArguments(std::move(packArguments))
    {
    }

    TypeFunctionInstanceType(NotNull<const TypeFunction> function, std::vector<TypeId> typeArguments, std::vector<TypePackId> packArguments)
        : function{function}
        , typeArguments(std::move(typeArguments))
        , packArguments(std::move(packArguments))
    {
    }
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

// A special, trivial type for the refinement system that is always eliminated from intersections.
struct NoRefineType
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
    LazyType() = default;
    LazyType(std::function<void(LazyType&)> unwrap)
        : unwrap(std::move(unwrap))
    {
    }

    // std::atomic is sad and requires a manual copy
    LazyType(const LazyType& rhs)
        : unwrap(rhs.unwrap)
        , unwrapped(rhs.unwrapped.load())
    {
    }

    LazyType(LazyType&& rhs) noexcept
        : unwrap(std::move(rhs.unwrap))
        , unwrapped(rhs.unwrapped.load())
    {
    }

    LazyType& operator=(const LazyType& rhs)
    {
        unwrap = rhs.unwrap;
        unwrapped = rhs.unwrapped.load();

        return *this;
    }

    LazyType& operator=(LazyType&& rhs) noexcept
    {
        unwrap = std::move(rhs.unwrap);
        unwrapped = rhs.unwrapped.load();

        return *this;
    }

    std::function<void(LazyType&)> unwrap;
    std::atomic<TypeId> unwrapped = nullptr;
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

using ErrorType = Unifiable::Error<TypeId>;

using TypeVariant = Unifiable::Variant<
    TypeId,
    FreeType,
    GenericType,
    PrimitiveType,
    SingletonType,
    BlockedType,
    PendingExpansionType,
    FunctionType,
    TableType,
    MetatableType,
    ExternType,
    AnyType,
    UnionType,
    IntersectionType,
    LazyType,
    UnknownType,
    NeverType,
    NegationType,
    NoRefineType,
    TypeFunctionInstanceType>;

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

    Type(TypeVariant ty, bool persistent)
        : ty(std::move(ty))
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

    Type(Type&&) = default;
    Type& operator=(Type&&) = default;

    Type clone() const;

private:
    Type(const Type&) = default;
    Type& operator=(const Type& rhs);
};

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

    // The location of where this TypeFun was defined, if available
    std::optional<Location> definitionLocation;

    TypeFun() = default;

    explicit TypeFun(TypeId ty)
        : type(ty)
    {
    }

    TypeFun(std::vector<GenericTypeDefinition> typeParams, TypeId type, std::optional<Location> definitionLocation = std::nullopt)
        : typeParams(std::move(typeParams))
        , type(type)
        , definitionLocation(definitionLocation)
    {
    }

    TypeFun(
        std::vector<GenericTypeDefinition> typeParams,
        std::vector<GenericTypePackDefinition> typePackParams,
        TypeId type,
        std::optional<Location> definitionLocation = std::nullopt
    )
        : typeParams(std::move(typeParams))
        , typePackParams(std::move(typePackParams))
        , type(type)
        , definitionLocation(definitionLocation)
    {
    }

    bool operator==(const TypeFun& rhs) const;
};

using SeenSet = std::set<std::pair<const void*, const void*>>;
bool areEqual(SeenSet& seen, const Type& lhs, const Type& rhs);

enum class FollowOption
{
    Normal,
    DisableLazyTypeThunks,
};

// Follow BoundTypes until we get to something real
TypeId follow(TypeId t);
TypeId follow(TypeId t, FollowOption followOption);
TypeId follow(TypeId t, const void* context, TypeId (*mapper)(const void*, TypeId));
TypeId follow(TypeId t, FollowOption followOption, const void* context, TypeId (*mapper)(const void*, TypeId));

std::vector<TypeId> flattenIntersection(TypeId ty);

bool isPrim(TypeId ty, PrimitiveType::Type primType);
bool isNil(TypeId ty);
bool isBoolean(TypeId ty);
bool isNumber(TypeId ty);
bool isString(TypeId ty);
bool isThread(TypeId ty);
bool isBuffer(TypeId ty);
bool isOptional(TypeId ty);
bool isTableIntersection(TypeId ty);
bool isTableUnion(TypeId ty);
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

    TypeId errorRecoveryType(TypeId guess) const;
    TypePackId errorRecoveryTypePack(TypePackId guess) const;

    friend TypeId makeStringMetatable(NotNull<BuiltinTypes> builtinTypes, SolverMode mode);
    friend struct GlobalTypes;

private:
    std::unique_ptr<struct TypeArena> arena;
    bool debugFreezeArena = false;

public:
    std::unique_ptr<BuiltinTypeFunctions> typeFunctions;
    const TypeId nilType;
    const TypeId numberType;
    const TypeId stringType;
    const TypeId booleanType;
    const TypeId threadType;
    const TypeId bufferType;
    const TypeId functionType;
    const TypeId externType;
    const TypeId tableType;
    const TypeId emptyTableType;
    const TypeId trueType;
    const TypeId falseType;
    const TypeId anyType;
    const TypeId unknownType;
    const TypeId neverType;
    const TypeId errorType;
    const TypeId noRefineType;
    const TypeId falsyType;
    const TypeId truthyType;
    const TypeId notNilType;

    const TypeId optionalNumberType;
    const TypeId optionalStringType;

    const TypePackId emptyTypePack;
    const TypePackId anyTypePack;
    const TypePackId unknownTypePack;
    const TypePackId neverTypePack;
    const TypePackId uninhabitableTypePack;
    const TypePackId errorTypePack;
};

void persist(TypeId ty);
void persist(TypePackId tp);

const TypeLevel* getLevel(TypeId ty);
TypeLevel* getMutableLevel(TypeId ty);

std::optional<TypeLevel> getLevel(TypePackId tp);

const Property* lookupExternTypeProp(const ExternType* cls, const Name& name);

// Whether `cls` is a subclass of `parent`
bool isSubclass(const ExternType* cls, const ExternType* parent);

Type* asMutable(TypeId ty);

template<typename... Ts, typename T>
bool is(T&& tv)
{
    if (!tv)
        return false;

    if constexpr (std::is_same_v<TypeId, T> && !(std::is_same_v<BoundType, Ts> || ...))
        LUAU_ASSERT(get_if<BoundType>(&tv->ty) == nullptr);

    return (get<Ts>(tv) || ...);
}

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
        ++*this;
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

    VecDeque<SavedIterInfo> stack;
    DenseHashSet<const T*> seen{nullptr}; // Only needed to protect the iterator from hanging the thread.

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
                if (seen.contains(inner))
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

TypeId freshType(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, Scope* scope, Polarity polarity = Polarity::Unknown);

using TypeIdPredicate = std::function<std::optional<TypeId>(TypeId)>;
std::vector<TypeId> filterMap(TypeId type, TypeIdPredicate predicate);

// A tag to mark a type which doesn't derive directly from the root type as overriding the return of `typeof`.
// Any classes which derive from this type will have typeof return this type.
inline constexpr char kTypeofRootTag[] = "typeofRoot";

void attachTag(TypeId ty, const std::string& tagName);
void attachTag(Property& prop, const std::string& tagName);

bool hasTag(TypeId ty, const std::string& tagName);
bool hasTag(const Property& prop, const std::string& tagName);
bool hasTag(const Tags& tags, const std::string& tagName); // Do not use in new work.

template<typename T>
bool hasTypeInIntersection(TypeId ty)
{
    TypeId tf = follow(ty);
    if (get<T>(tf))
        return true;
    for (auto t : flattenIntersection(tf))
        if (get<T>(follow(t)))
            return true;
    return false;
}

bool hasPrimitiveTypeInIntersection(TypeId ty, PrimitiveType::Type primTy);
/*
 * Use this to change the kind of a particular type.
 *
 * LUAU_NOINLINE so that the calling frame doesn't have to pay the stack storage for the new variant.
 */
template<typename T, typename... Args>
LUAU_NOINLINE T* emplaceType(Type* ty, Args&&... args)
{
    return &ty->ty.emplace<T>(std::forward<Args>(args)...);
}

template<>
LUAU_NOINLINE Unifiable::Bound<TypeId>* emplaceType<BoundType>(Type* ty, TypeId& tyArg);

} // namespace Luau
