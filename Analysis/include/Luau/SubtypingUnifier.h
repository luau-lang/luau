// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Subtyping.h"
#include "Luau/Constraint.h"
#include "Luau/NotNull.h"
#include "Luau/Type.h"
#include "Luau/Unifier2.h"

namespace Luau
{

/**
 * `SubtypingUnifier` performs unification of type variables by asking whether
 * the two are subtypes of one another, and then attempts to dispatch any
 * assumed constraints.
 */
struct SubtypingUnifier
{

    using UpperBounds = DenseHashMap<TypeId, std::vector<std::pair<Location, TypeId>>>;

    /**
     * Represents attempting to dispatch some number of assumed constraints.
     */
    struct Result
    {
        /**
         * Whether we were able to unify successfully. This can fail if,
         * for example, unifying two free type packs would create a cycle.
         */
        UnifyResult unified;
        /**
         * A list of outstanding constraints, e.g. subtype constraints where
         * one of the arguments is a blocked type of some kind.
         */
        std::vector<ConstraintV> outstandingConstraints;
        /**
         * Upper bound contributors to any free types that we mutated. This is
         * used if a function type's parameter is inferred to be `never`.
         */
        UpperBounds upperBoundContributors;
    };

    explicit SubtypingUnifier(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<InternalErrorReporter> reporter);

    /**
     * @param constraint The active constraint (used for location data).
     * @param assumedConstraints The set of assumed constraints for a given subtype check.
     */
    Result dispatchConstraints(NotNull<const Constraint> constraint, std::vector<ConstraintV> assumedConstraints) const;

private:
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<InternalErrorReporter> reporter;

    /**
     * Attempt to dispatch a *single* constraint.
     * @return A pair containing whether we were able to unify and if we need to defer the constraint.
     */
    std::pair<UnifyResult, bool> dispatchOneConstraint(
        NotNull<const Constraint> constraint,
        const ConstraintV& cv,
        UpperBounds& upperBoundContributors
    ) const;

    OccursCheckResult occursCheck(TypePackId needle, TypePackId haystack) const;

    bool canBeUnified(TypeId ty) const;
};

} // namespace Luau
