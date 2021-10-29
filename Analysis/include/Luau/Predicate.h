// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Variant.h"
#include "Luau/Location.h"
#include "Luau/Symbol.h"

#include <map>
#include <memory>
#include <vector>

namespace Luau
{

struct TypeVar;
using TypeId = const TypeVar*;

struct Field;
using LValue = Variant<Symbol, Field>;

struct Field
{
    std::shared_ptr<LValue> parent; // TODO: Eventually use unique_ptr to enforce non-copyable trait.
    std::string key;
};

std::optional<LValue> tryGetLValue(const class AstExpr& expr);

// Utility function: breaks down an LValue to get at the Symbol, and reverses the vector of keys.
std::pair<Symbol, std::vector<std::string>> getFullName(const LValue& lvalue);

std::string toString(const LValue& lvalue);

template<typename T>
const T* get(const LValue& lvalue)
{
    return get_if<T>(&lvalue);
}

// Key is a stringified encoding of an LValue.
using RefinementMap = std::map<std::string, TypeId>;

void merge(RefinementMap& l, const RefinementMap& r, std::function<TypeId(TypeId, TypeId)> f);
void addRefinement(RefinementMap& refis, const LValue& lvalue, TypeId ty);

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
