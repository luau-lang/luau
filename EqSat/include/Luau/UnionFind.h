// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Id.h"

#include <vector>

namespace Luau::EqSat
{

/// See <https://dl.acm.org/doi/pdf/10.1145/321879.321884>.
struct UnionFind final
{
    Id makeSet();
    Id find(Id id) const;
    Id find(Id id);

    // Merge aSet with bSet and return the canonicalized Id into the merged set.
    Id merge(Id aSet, Id bSet);

private:
    std::vector<Id> parents;
    std::vector<int> ranks;

private:
    Id canonicalize(Id id) const;
};

} // namespace Luau::EqSat
