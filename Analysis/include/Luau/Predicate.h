// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Location.h"
#include "Luau/LValue.h"
#include "Luau/Variant.h"
#include "Luau/TypeFwd.h"

#include <vector>

namespace Luau
{

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

    AndPredicate(PredicateVec&& lhs, PredicateVec&& rhs);
};

struct OrPredicate
{
    PredicateVec lhs;
    PredicateVec rhs;

    OrPredicate(PredicateVec&& lhs, PredicateVec&& rhs);
};

struct NotPredicate
{
    PredicateVec predicates;
};

// Outside definition works around clang 15 issue where vector instantiation is triggered while Predicate is still incomplete
inline AndPredicate::AndPredicate(PredicateVec&& lhs, PredicateVec&& rhs)
    : lhs(std::move(lhs))
    , rhs(std::move(rhs))
{
}

inline OrPredicate::OrPredicate(PredicateVec&& lhs, PredicateVec&& rhs)
    : lhs(std::move(lhs))
    , rhs(std::move(rhs))
{
}

template<typename T>
const T* get(const Predicate& predicate)
{
    return get_if<T>(&predicate);
}

} // namespace Luau
