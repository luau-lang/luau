// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h" // Used for some of the enumerations
#include "Luau/DenseHash.h"
#include "Luau/NotNull.h"
#include "Luau/Variant.h"
#include "Luau/TypeFwd.h"

#include <string>
#include <memory>
#include <vector>

namespace Luau
{

struct Scope;

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
};

// subType ~ inst superType
struct InstantiationConstraint
{
    TypeId subType;
    TypeId superType;
};

// variables ~ iterate iterator
// Unpack the iterator, figure out what types it iterates over, and bind those types to variables.
struct IterableConstraint
{
    TypePackId iterator;
    TypePackId variables;

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
    class AstExprCall* callSite = nullptr;
    std::vector<std::optional<TypeId>> discriminantTypes;

    // When we dispatch this constraint, we update the key at this map to record
    // the overload that we selected.
    DenseHashMap<const AstNode*, TypeId>* astOverloadResolvedTypes = nullptr;
};

// result ~ prim ExpectedType SomeSingletonType MultitonType
//
// If ExpectedType is potentially a singleton (an actual singleton or a union
// that contains a singleton), then result ~ SomeSingletonType
//
// else result ~ MultitonType
struct PrimitiveTypeConstraint
{
    TypeId resultType;
    TypeId expectedType;
    TypeId singletonType;
    TypeId multitonType;
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

// result ~ setProp subjectType ["prop", "prop2", ...] propType
//
// If the subject is a table or table-like thing that already has the named
// property chain, we unify propType with that existing property type.
//
// If the subject is a free table, we augment it in place.
//
// If the subject is an unsealed table, result is an augmented table that
// includes that new prop.
struct SetPropConstraint
{
    TypeId resultType;
    TypeId subjectType;
    std::vector<std::string> path;
    TypeId propType;
};

// result ~ setIndexer subjectType indexType propType
//
// If the subject is a table or table-like thing that already has an indexer,
// unify its indexType and propType with those from this constraint.
//
// If the table is a free or unsealed table, we augment it with a new indexer.
struct SetIndexerConstraint
{
    TypeId resultType;
    TypeId subjectType;
    TypeId indexType;
    TypeId propType;
};

// if negation:
//   result ~ if isSingleton D then ~D else unknown where D = discriminantType
// if not negation:
//   result ~ if isSingleton D then D else unknown where D = discriminantType
struct SingletonOrTopTypeConstraint
{
    TypeId resultType;
    TypeId discriminantType;
    bool negated;
};

// resultType ~ unpack sourceTypePack
//
// Similar to PackSubtypeConstraint, but with one important difference: If the
// sourcePack is blocked, this constraint blocks.
struct UnpackConstraint
{
    TypePackId resultPack;
    TypePackId sourcePack;

    // UnpackConstraint is sometimes used to resolve the types of assignments.
    // When this is the case, any LocalTypes in resultPack can have their
    // domains extended by the corresponding type from sourcePack.
    bool resultIsLValue = false;
};

// resultType ~ T0 op T1 op ... op TN
//
// op is either union or intersection.  If any of the input types are blocked,
// this constraint will block unless forced.
struct SetOpConstraint
{
    enum
    {
        Intersection,
        Union
    } mode;

    TypeId resultType;
    std::vector<TypeId> types;
};

// ty ~ reduce ty
//
// Try to reduce ty, if it is a TypeFamilyInstanceType. Otherwise, do nothing.
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

using ConstraintV = Variant<SubtypeConstraint, PackSubtypeConstraint, GeneralizationConstraint, InstantiationConstraint, IterableConstraint,
    NameConstraint, TypeAliasExpansionConstraint, FunctionCallConstraint, PrimitiveTypeConstraint, HasPropConstraint, SetPropConstraint,
    SetIndexerConstraint, SingletonOrTopTypeConstraint, UnpackConstraint, SetOpConstraint, ReduceConstraint, ReducePackConstraint>;

struct Constraint
{
    Constraint(NotNull<Scope> scope, const Location& location, ConstraintV&& c);

    Constraint(const Constraint&) = delete;
    Constraint& operator=(const Constraint&) = delete;

    NotNull<Scope> scope;
    Location location;
    ConstraintV c;

    std::vector<NotNull<Constraint>> dependencies;

    DenseHashSet<TypeId> getFreeTypes() const;
};

using ConstraintPtr = std::unique_ptr<Constraint>;

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
