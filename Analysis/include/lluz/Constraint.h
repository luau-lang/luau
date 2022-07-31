// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/Ast.h" // Used for some of the enumerations
#include "lluz/NotNull.h"
#include "lluz/Variant.h"

#include <string>
#include <memory>
#include <vector>

namespace lluz
{

struct Scope2;
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

// subType ~ gen superType
struct GeneralizationConstraint
{
    TypeId generalizedType;
    TypeId sourceType;
    Scope2* scope;
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

using ConstraintV = Variant<SubtypeConstraint, PackSubtypeConstraint, GeneralizationConstraint, InstantiationConstraint, UnaryConstraint,
    BinaryConstraint, NameConstraint>;
using ConstraintPtr = std::unique_ptr<struct Constraint>;

struct Constraint
{
    explicit Constraint(ConstraintV&& c);

    Constraint(const Constraint&) = delete;
    Constraint& operator=(const Constraint&) = delete;

    ConstraintV c;
    std::vector<NotNull<Constraint>> dependencies;
};

inline Constraint& asMutable(const Constraint& c)
{
    return const_cast<Constraint&>(c);
}

template<typename T>
T* getMutable(Constraint& c)
{
    return ::lluz::get_if<T>(&c.c);
}

template<typename T>
const T* get(const Constraint& c)
{
    return getMutable<T>(asMutable(c));
}

} // namespace lluz
