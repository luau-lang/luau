// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Def.h"
#include "Luau/TypedAllocator.h"
#include "Luau/Variant.h"

#include <memory>

namespace Luau
{

struct Type;
using TypeId = const Type*;

struct Negation;
struct Conjunction;
struct Disjunction;
struct Equivalence;
struct Proposition;
using Connective = Variant<Negation, Conjunction, Disjunction, Equivalence, Proposition>;
using ConnectiveId = Connective*; // Can and most likely is nullptr.

struct Negation
{
    ConnectiveId connective;
};

struct Conjunction
{
    ConnectiveId lhs;
    ConnectiveId rhs;
};

struct Disjunction
{
    ConnectiveId lhs;
    ConnectiveId rhs;
};

struct Equivalence
{
    ConnectiveId lhs;
    ConnectiveId rhs;
};

struct Proposition
{
    DefId def;
    TypeId discriminantTy;
};

template<typename T>
const T* get(ConnectiveId connective)
{
    return get_if<T>(connective);
}

struct ConnectiveArena
{
    TypedAllocator<Connective> allocator;

    ConnectiveId negation(ConnectiveId connective);
    ConnectiveId conjunction(ConnectiveId lhs, ConnectiveId rhs);
    ConnectiveId disjunction(ConnectiveId lhs, ConnectiveId rhs);
    ConnectiveId equivalence(ConnectiveId lhs, ConnectiveId rhs);
    ConnectiveId proposition(DefId def, TypeId discriminantTy);
};

} // namespace Luau
