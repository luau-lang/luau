// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h" // Used for some of the enumerations
#include "Luau/NotNull.h"
#include "Luau/Variant.h"
#include "Luau/TypeVar.h"

#include <string>
#include <memory>
#include <vector>

namespace Luau
{

struct Scope;

struct TypeVar;
using TypeId = const TypeVar*;

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
    // Must be a PendingExpansionTypeVar.
    TypeId target;
};

struct FunctionCallConstraint
{
    std::vector<NotNull<const struct Constraint>> innerConstraints;
    TypeId fn;
    TypePackId result;
    class AstExprCall* astFragment;
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

using ConstraintV =
    Variant<SubtypeConstraint, PackSubtypeConstraint, GeneralizationConstraint, InstantiationConstraint, UnaryConstraint, BinaryConstraint,
        IterableConstraint, NameConstraint, TypeAliasExpansionConstraint, FunctionCallConstraint, PrimitiveTypeConstraint, HasPropConstraint>;

struct Constraint
{
    Constraint(NotNull<Scope> scope, const Location& location, ConstraintV&& c);

    Constraint(const Constraint&) = delete;
    Constraint& operator=(const Constraint&) = delete;

    NotNull<Scope> scope;
    Location location;
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
