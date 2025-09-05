// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h" // Used for some of the enumerations
#include "Luau/DenseHash.h"
#include "Luau/NotNull.h"
#include "Luau/Variant.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeIds.h"

#include <string>
#include <memory>
#include <vector>

namespace Luau
{

enum class ValueContext;
struct Scope;

// if resultType is a freeType, assignmentType <: freeType <: resultType bounds
struct EqualityConstraint
{
    TypeId resultType;
    TypeId assignmentType;
};

// subType <: superType
struct SubtypeConstraint
{
    TypeId subType;
    TypeId superType;
};

// subPack <: superPack
struct PackSubtypeConstraint
{
    TypePackId subPack;
    TypePackId superPack;

    // HACK!! TODO clip.
    // We need to know which of `PackSubtypeConstraint` are emitted from `AstStatReturn` vs any others.
    // Then we force these specific `PackSubtypeConstraint` to only dispatch in the order of the `return`s.
    bool returns = false;
};

// generalizedType ~ gen sourceType
struct GeneralizationConstraint
{
    TypeId generalizedType;
    TypeId sourceType;

    std::vector<TypeId> interiorTypes;
    bool hasDeprecatedAttribute = false;
    AstAttr::DeprecatedInfo deprecatedInfo;

    /// If true, never introduce generics.  Always replace free types by their
    /// bounds or unknown. Presently used only to generalize the whole module.
    bool noGenerics = false;
};

// variables ~ iterate iterator
// Unpack the iterator, figure out what types it iterates over, and bind those types to variables.
struct IterableConstraint
{
    TypePackId iterator;
    std::vector<TypeId> variables;

    const AstNode* nextAstFragment;
    DenseHashMap<const AstNode*, TypeId>* astForInNextTypes;
};

// name(namedType) = name
struct NameConstraint
{
    TypeId namedType;
    std::string name;
    bool synthetic = false;
    std::vector<TypeId> typeParameters;
    std::vector<TypePackId> typePackParameters;
};

// target ~ inst target
struct TypeAliasExpansionConstraint
{
    // Must be a PendingExpansionType.
    TypeId target;
};

struct FunctionCallConstraint
{
    TypeId fn;
    TypePackId argsPack;
    TypePackId result;

    // callSite can be nullptr in the case that this constraint was
    // synthetically generated from some other constraint. eg
    // IterableConstraint.
    class AstExprCall* callSite = nullptr;
    std::vector<std::optional<TypeId>> discriminantTypes;

    // When we dispatch this constraint, we update the key at this map to record
    // the overload that we selected.
    DenseHashMap<const AstNode*, TypeId>* astOverloadResolvedTypes = nullptr;
};

// function_check fn argsPack
//
// If fn is a function type and argsPack is a partially solved
// pack of arguments to be supplied to the function, propagate the argument
// types of fn into the types of argsPack. This is used to implement
// bidirectional inference of lambda arguments.
struct FunctionCheckConstraint
{
    TypeId fn;
    TypePackId argsPack;

    class AstExprCall* callSite = nullptr;
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes;
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astExpectedTypes;
};

// prim FreeType ExpectedType PrimitiveType
//
// FreeType is bounded below by the singleton type and above by PrimitiveType
// initially. When this constraint is resolved, it will check that the bounds
// of the free type are well-formed by subtyping.
//
// If they are not well-formed, then FreeType is replaced by its lower bound
//
// If they are well-formed and ExpectedType is potentially a singleton (an
// actual singleton or a union that contains a singleton),
// then FreeType is replaced by its lower bound
//
// else FreeType is replaced by PrimitiveType
struct PrimitiveTypeConstraint
{
    TypeId freeType;

    // potentially gets used to force the lower bound?
    std::optional<TypeId> expectedType;

    // the primitive type to check against
    TypeId primitiveType;
};

// result ~ hasProp type "prop_name"
//
// If the subject is a table, bind the result to the named prop.  If the table
// has an indexer, bind it to the index result type. If the subject is a union,
// bind the result to the union of its constituents' properties.
//
// It would be nice to get rid of this constraint and someday replace it with
//
// T <: {p: X}
//
// Where {} describes an inexact shape type.
struct HasPropConstraint
{
    TypeId resultType;
    TypeId subjectType;
    std::string prop;
    ValueContext context;

    // We want to track if this `HasPropConstraint` comes from a conditional.
    // If it does, we're going to change the behavior of property look-up a bit.
    // In particular, we're going to return `unknownType` for property lookups
    // on `table` or inexact table types where the property is not present.
    //
    // This allows us to refine table types to have additional properties
    // without reporting errors in typechecking on the property tests.
    bool inConditional = false;

    // HACK: We presently need types like true|false or string|"hello" when
    // deciding whether a particular literal expression should have a singleton
    // type.  This boolean is set to true when extracting the property type of a
    // value that may be a union of tables.
    //
    // For example, in the following code fragment, we want the lookup of the
    // success property to yield true|false when extracting an expectedType in
    // this expression:
    //
    // type Result<T, E> = {success:true, result: T} | {success:false, error: E}
    //
    // local r: Result<number, string> = {success=true, result=9}
    //
    // If we naively simplify the expectedType to boolean, we will erroneously
    // compute the type boolean for the success property of the table literal.
    // This causes type checking to fail.
    bool suppressSimplification = false;
};

// resultType ~ hasIndexer subjectType indexType
//
// If the subject type is a table or table-like thing that supports indexing,
// populate the type result with the result type of such an index operation.
//
// If the subject is not indexable, resultType is bound to errorType.
struct HasIndexerConstraint
{
    TypeId resultType;
    TypeId subjectType;
    TypeId indexType;
};

// assignProp lhsType propName rhsType
//
// Assign a value of type rhsType into the named property of lhsType.

struct AssignPropConstraint
{
    TypeId lhsType;
    std::string propName;
    TypeId rhsType;

    /// If a new property is to be inserted into a table type, it will be
    /// ascribed this location.
    std::optional<Location> propLocation;

    /// The canonical write type of the property.  It is _solely_ used to
    /// populate astTypes during constraint resolution.  Nothing should ever
    /// block on it.
    TypeId propType;

    // When we generate constraints, we increment the remaining prop count on
    // the table if we are able. This flag informs the solver as to whether or
    // not it should in turn decrement the prop count when this constraint is
    // dispatched.
    bool decrementPropCount = false;
};

struct AssignIndexConstraint
{
    TypeId lhsType;
    TypeId indexType;
    TypeId rhsType;

    /// The canonical write type of the property.  It is _solely_ used to
    /// populate astTypes during constraint resolution.  Nothing should ever
    /// block on it.
    TypeId propType;
};

// resultTypes ~ unpack sourceTypePack
//
// Similar to PackSubtypeConstraint, but with one important difference: If the
// sourcePack is blocked, this constraint blocks.
struct UnpackConstraint
{
    std::vector<TypeId> resultPack;
    TypePackId sourcePack;
};

// ty ~ reduce ty
//
// Try to reduce ty, if it is a TypeFunctionInstanceType. Otherwise, do nothing.
struct ReduceConstraint
{
    TypeId ty;
};

// tp ~ reduce tp
//
// Analogous to ReduceConstraint, but for type packs.
struct ReducePackConstraint
{
    TypePackId tp;
};

// simplify ty
struct SimplifyConstraint
{
    TypeId ty;
};

// push_function_type_constraint expectedFunctionType => functionType
//
// Attempt to "push" the types of `expectedFunctionType` into `functionType`,
// assuming that `expr` is a lambda who's ungeneralized type is `functionType`.
// Similar to `FunctionCheckConstraint`. For example:
//
//  local Foo = {} :: { bar : (number) -> () }
//
//  function Foo.bar(x) end
//
// This will force `x` to be inferred as `number`.
struct PushFunctionTypeConstraint
{
    TypeId expectedFunctionType;
    TypeId functionType;
    NotNull<AstExprFunction> expr;
    bool isSelf;
};

struct PushTypeConstraint
{
    TypeId expectedType;
    TypeId targetType;
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes;
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astExpectedTypes;
    NotNull<const AstExpr> expr;
};

using ConstraintV = Variant<
    SubtypeConstraint,
    PackSubtypeConstraint,
    GeneralizationConstraint,
    IterableConstraint,
    NameConstraint,
    TypeAliasExpansionConstraint,
    FunctionCallConstraint,
    FunctionCheckConstraint,
    PrimitiveTypeConstraint,
    HasPropConstraint,
    HasIndexerConstraint,
    AssignPropConstraint,
    AssignIndexConstraint,
    UnpackConstraint,
    ReduceConstraint,
    ReducePackConstraint,
    EqualityConstraint,
    SimplifyConstraint,
    PushFunctionTypeConstraint,
    PushTypeConstraint>;

struct Constraint
{
    Constraint(NotNull<Scope> scope, const Location& location, ConstraintV&& c);

    Constraint(const Constraint&) = delete;
    Constraint& operator=(const Constraint&) = delete;

    NotNull<Scope> scope;
    Location location;
    ConstraintV c;

    std::vector<NotNull<Constraint>> dependencies;

    TypeIds getMaybeMutatedFreeTypes() const;
};

using ConstraintPtr = std::unique_ptr<Constraint>;

bool isReferenceCountedType(const TypeId typ);

inline Constraint& asMutable(const Constraint& c)
{
    return const_cast<Constraint&>(c);
}

template<typename T>
T* getMutable(Constraint& c)
{
    return ::Luau::get_if<T>(&c.c);
}

template<typename T>
const T* get(const Constraint& c)
{
    return getMutable<T>(asMutable(c));
}

} // namespace Luau
