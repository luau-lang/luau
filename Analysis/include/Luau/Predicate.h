// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Location.h"
#include "Luau/LValue.h"
#include "Luau/Variant.h"

#include <vector>

namespace Luau
{

struct TypeVar;
using TypeId = const TypeVar*;

struct TruthyPredicate;
struct IsAPredicate;
struct TypeGuardPredicate;
struct EqPredicate;
struct AndPredicate;
struct OrPredicate;
struct NotPredicate;

using Predicate = Variant<TruthyPredicate, IsAPredicate, TypeGuardPredicate, EqPredicate, AndPredicate, OrPredicate, NotPredicate>;
using PredicateVec = std::vector<Predicate>;

struct TruthyPredicate
{
    LValue lvalue;
    Location location;
};

struct IsAPredicate
{
    LValue lvalue;
    Location location;
    TypeId ty;
};

struct TypeGuardPredicate
{
    LValue lvalue;
    Location location;
    std::string kind; // TODO: When singleton types arrive, replace this with `TypeId ty;`
    bool isTypeof;
};

struct EqPredicate
{
    LValue lvalue;
    TypeId type;
    Location location;
};

struct AndPredicate
{
    PredicateVec lhs;
    PredicateVec rhs;

    AndPredicate(PredicateVec&& lhs, PredicateVec&& rhs)
        : lhs(std::move(lhs))
        , rhs(std::move(rhs))
    {
    }
};

struct OrPredicate
{
    PredicateVec lhs;
    PredicateVec rhs;

    OrPredicate(PredicateVec&& lhs, PredicateVec&& rhs)
        : lhs(std::move(lhs))
        , rhs(std::move(rhs))
    {
    }
};

struct NotPredicate
{
    PredicateVec predicates;
};

template<typename T>
const T* get(const Predicate& predicate)
{
    return get_if<T>(&predicate);
}

} // namespace Luau
