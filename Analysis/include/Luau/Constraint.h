// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h" // Used for some of the enumerations
#include "Luau/Def.h"
#include "Luau/DenseHash.h"
#include "Luau/NotNull.h"
#include "Luau/Type.h"
#include "Luau/Variant.h"

#include <string>
#include <memory>
#include <vector>

namespace Luau
{

struct Scope;

struct Type;
using TypeId = const Type*;

struct TypePackVar;
using TypePackId = const TypePackVar*;

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

struct UnaryConstraint
{
    AstExprUnary::Op op;
    TypeId operandType;
    TypeId resultType;
};

// let L : leftType
// let R : rightType
// in
//     L op R : resultType
struct BinaryConstraint
{
    AstExprBinary::Op op;
    TypeId leftType;
    TypeId rightType;
    TypeId resultType;

    // When we dispatch this constraint, we update the key at this map to record
    // the overload that we selected.
    const void* astFragment;
    DenseHashMap<const void*, TypeId>* astOriginalCallTypes;
    DenseHashMap<const void*, TypeId>* astOverloadResolvedTypes;
};

// iteratee is iterable
// iterators is the iteration types.
struct IterableConstraint
{
    TypePackId iterator;
    TypePackId variables;
};

// name(namedType) = name
struct NameConstraint
{
    TypeId namedType;
    std::string name;
};

// target ~ inst target
struct TypeAliasExpansionConstraint
{
    // Must be a PendingExpansionType.
    TypeId target;
};

struct FunctionCallConstraint
{
    std::vector<NotNull<const struct Constraint>> innerConstraints;
    TypeId fn;
    TypePackId argsPack;
    TypePackId result;
    class AstExprCall* callSite;
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

using ConstraintV = Variant<SubtypeConstraint, PackSubtypeConstraint, GeneralizationConstraint, InstantiationConstraint, UnaryConstraint,
    BinaryConstraint, IterableConstraint, NameConstraint, TypeAliasExpansionConstraint, FunctionCallConstraint, PrimitiveTypeConstraint,
    HasPropConstraint, SetPropConstraint, SingletonOrTopTypeConstraint>;

struct Constraint
{
    Constraint(NotNull<Scope> scope, const Location& location, ConstraintV&& c);

    Constraint(const Constraint&) = delete;
    Constraint& operator=(const Constraint&) = delete;

    NotNull<Scope> scope;
    Location location; // TODO: Extract this out into only the constraints that needs a location. Not all constraints needs locations.
    ConstraintV c;

    std::vector<NotNull<Constraint>> dependencies;
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
