// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/SubtypingUnifier.h"

#include "Luau/ConstraintSolver.h"
#include "Luau/Simplify.h"
#include "Luau/Subtyping.h"
#include "Luau/TypeUtils.h"
#include "Luau/Unifier2.h"

LUAU_FASTFLAG(LuauUnifyWithSubtyping2)

namespace Luau
{

SubtypingUnifier::SubtypingUnifier(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes, NotNull<InternalErrorReporter> reporter)
    : arena{arena}
    , builtinTypes{builtinTypes}
    , reporter{reporter}
{
}


bool SubtypingUnifier::canBeUnified(TypeId ty) const
{
    ty = follow(ty);
    if (auto tbl = get<TableType>(ty))
        return tbl->state != TableState::Sealed;

    return is<FreeType>(ty) || isBlocked(ty);
}

SubtypingUnifier::Result SubtypingUnifier::dispatchConstraints(NotNull<const Constraint> constraint, std::vector<ConstraintV> assumedConstraints) const
{
    UnifyResult unifierRes = UnifyResult::Ok;
    // NOTE: You *could* potentially reuse the input vector, but this seems
    // easier to read.
    std::vector<ConstraintV> outstandingConstraints;
    outstandingConstraints.reserve(assumedConstraints.size());
    UpperBounds upperBounds{nullptr};
    for (auto& cv : assumedConstraints)
    {
        const auto& [unified, dispatched] = dispatchOneConstraint(constraint, cv, upperBounds);
        unifierRes &= unified;
        if (!dispatched)
            outstandingConstraints.push_back(std::move(cv));
    }
    return {unifierRes, std::move(outstandingConstraints), std::move(upperBounds)};
}

OccursCheckResult SubtypingUnifier::occursCheck(TypePackId needle, TypePackId haystack) const
{
    needle = follow(needle);
    haystack = follow(haystack);

    if (getMutable<ErrorTypePack>(needle))
        return OccursCheckResult::Pass;

    if (!getMutable<FreeTypePack>(needle))
        reporter->ice("Expected needle pack to be free");

    while (!getMutable<ErrorTypePack>(haystack))
    {
        if (needle == haystack)
            return OccursCheckResult::Fail;

        if (auto a = get<TypePack>(haystack); a && a->tail)
        {
            haystack = follow(*a->tail);
            continue;
        }

        break;
    }

    return OccursCheckResult::Pass;
}


std::pair<UnifyResult, bool> SubtypingUnifier::dispatchOneConstraint(
    NotNull<const Constraint> constraint,
    const ConstraintV& cv,
    UpperBounds& upperBoundContributors
) const
{
    if (const auto sc = get_if<SubtypeConstraint>(&cv))
    {
        TypeId subTy = follow(sc->subType);
        TypeId superTy = follow(sc->superType);

        LUAU_ASSERT(canBeUnified(subTy) || canBeUnified(superTy));

        if (isBlocked(subTy) || isBlocked(superTy))
            return {UnifyResult::Ok, false};

        if (auto superFreeTy = getMutable<FreeType>(superTy))
        {
            superFreeTy->lowerBound = simplifyUnion(builtinTypes, arena, subTy, superFreeTy->lowerBound).result;
        }

        if (auto subFreeType = getMutable<FreeType>(subTy))
        {
            subFreeType->upperBound = simplifyIntersection(builtinTypes, arena, subFreeType->upperBound, superTy).result;
            upperBoundContributors[subTy].emplace_back(constraint->location, superTy);
        }

        // FIXME CLI-182960: Unification shouldn't be the mechanism for adding table indexers
        if (auto pair = get2<TableType, TableType>(subTy, superTy); pair && !pair.first->indexer)
            getMutable<TableType>(subTy)->indexer = TableIndexer{pair.second->indexer->indexType, pair.second->indexer->indexResultType};
    }
    else if (auto psc = get_if<PackSubtypeConstraint>(&cv))
    {
        TypePackId subTp = follow(psc->subPack);
        TypePackId superTp = follow(psc->superPack);
        // There *should* be an assertion here that either part of the constraint is
        // free, but because free type packs are replaced on-the-spot, we *may*
        // encounter conflicting constraints. One example is for:
        //
        //  local callbacks: { () -> () } = {
        //      function () end,
        //      function () end
        //  }
        //
        // We'll end up minting two sets of constraints for each lambda (as we need
        // them to be _exactly_ `()` as per the table type).
        if (is<FreeTypePack>(subTp))
        {
            if (OccursCheckResult::Fail == occursCheck(subTp, superTp))
            {
                emplaceTypePack<BoundTypePack>(asMutable(subTp), builtinTypes->errorTypePack);
                return {UnifyResult::OccursCheckFailed, true};
            }
            emplaceTypePack<BoundTypePack>(asMutable(subTp), superTp);
            return {UnifyResult::Ok, true};
        }

        if (is<FreeTypePack>(superTp))
        {
            if (OccursCheckResult::Fail == occursCheck(superTp, subTp))
            {
                emplaceTypePack<BoundTypePack>(asMutable(superTp), builtinTypes->errorTypePack);
                return {UnifyResult::OccursCheckFailed, true};
            }

            emplaceTypePack<BoundTypePack>(asMutable(superTp), subTp);
            return {UnifyResult::Ok, true};
        }
    }
    else
    {
        LUAU_ASSERT(!"Unreachable, unexpected constraint in subtyping unifier.");
        return {UnifyResult::Ok, false};
    }

    return {UnifyResult::Ok, true};
}



} // namespace Luau
