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

struct BinaryConstraint
{
    AstExprBinary::Op op;
    TypeId leftType;
    TypeId rightType;
    TypeId resultType;
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

using ConstraintV = Variant<SubtypeConstraint, PackSubtypeConstraint, GeneralizationConstraint, InstantiationConstraint, UnaryConstraint,
    BinaryConstraint, NameConstraint, TypeAliasExpansionConstraint>;
using ConstraintPtr = std::unique_ptr<struct Constraint>;

struct Constraint
{
    Constraint(ConstraintV&& c, NotNull<Scope> scope);

    Constraint(const Constraint&) = delete;
    Constraint& operator=(const Constraint&) = delete;

    ConstraintV c;
    std::vector<NotNull<Constraint>> dependencies;
    NotNull<Scope> scope;
};

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
