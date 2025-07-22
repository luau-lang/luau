// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/NotNull.h"
#include "Luau/TypedAllocator.h"
#include "Luau/Variant.h"
#include "Luau/TypeFwd.h"

namespace Luau
{

struct RefinementKey;
using DefId = NotNull<const struct Def>;

struct Variadic;
struct Negation;
struct Conjunction;
struct Disjunction;
struct Equivalence;
struct Proposition;
using Refinement = Variant<Variadic, Negation, Conjunction, Disjunction, Equivalence, Proposition>;
using RefinementId = Refinement*; // Can and most likely is nullptr.

struct Variadic
{
    std::vector<RefinementId> refinements;
};

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
    const RefinementKey* key;
    TypeId discriminantTy;
    bool implicitFromCall;
};

template<typename T>
const T* get(RefinementId refinement)
{
    return get_if<T>(refinement);
}

struct RefinementArena
{
    RefinementId variadic(const std::vector<RefinementId>& refis);
    RefinementId negation(RefinementId refinement);
    RefinementId conjunction(RefinementId lhs, RefinementId rhs);
    RefinementId disjunction(RefinementId lhs, RefinementId rhs);
    RefinementId equivalence(RefinementId lhs, RefinementId rhs);
    RefinementId proposition(const RefinementKey* key, TypeId discriminantTy);
    RefinementId implicitProposition(const RefinementKey* key, TypeId discriminantTy);

private:
    TypedAllocator<Refinement> allocator;
};

} // namespace Luau
