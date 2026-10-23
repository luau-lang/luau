// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Constraint.h"
#include "Luau/TypeIds.h"
#include "Luau/Error.h"

#include <vector>

namespace Luau
{

struct ConstraintSet
{
    NotNull<Scope> rootScope;

    std::vector<ConstraintPtr> constraints;

    // The set of all free types created during constraint generation
    TypeIds freeTypes;

    // Map a function's signature scope back to its signature type. Once we've
    // dispatched all of the constraints pertaining to a particular free type,
    // we use this mapping to generalize that free type.
    DenseHashMap<Scope*, TypeId> scopeToFunction;

    // It is pretty uncommon for constraint generation to itself produce errors, but it can happen.
    std::vector<TypeError> errors;

    // Module-level GeneralizationConstraints deferred from the main solve loop.
    // The solver dispatches these after all other constraints are resolved.
    // Only used in cyclic module inference.
    std::vector<ConstraintPtr> deferredConstraints;
};

} // namespace Luau
