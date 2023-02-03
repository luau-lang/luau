// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Def.h"
#include "Luau/TypedAllocator.h"
#include "Luau/Variant.h"

namespace Luau
{

struct Type;
using TypeId = const Type*;

struct Negation;
struct Conjunction;
struct Disjunction;
struct Equivalence;
struct Proposition;
using Refinement = Variant<Negation, Conjunction, Disjunction, Equivalence, Proposition>;
using RefinementId = Refinement*; // Can and most likely is nullptr.

struct Negation
{
    RefinementId refinement;
};

struct Conjunction
{
    RefinementId lhs;
    RefinementId rhs;
};

struct Disjunction
{
    RefinementId lhs;
    RefinementId rhs;
};

struct Equivalence
{
    RefinementId lhs;
    RefinementId rhs;
};

struct Proposition
{
    DefId def;
    TypeId discriminantTy;
};

template<typename T>
const T* get(RefinementId refinement)
{
    return get_if<T>(refinement);
}

struct RefinementArena
{
    TypedAllocator<Refinement> allocator;

    RefinementId negation(RefinementId refinement);
    RefinementId conjunction(RefinementId lhs, RefinementId rhs);
    RefinementId disjunction(RefinementId lhs, RefinementId rhs);
    RefinementId equivalence(RefinementId lhs, RefinementId rhs);
    RefinementId proposition(DefId def, TypeId discriminantTy);
};

} // namespace Luau
