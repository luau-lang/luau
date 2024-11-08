// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/UnionFind.h"

#include "Luau/Common.h"

#include <limits>

namespace Luau::EqSat
{

Id UnionFind::makeSet()
{
    LUAU_ASSERT(parents.size() < std::numeric_limits<uint32_t>::max());

    Id id{uint32_t(parents.size())};
    parents.push_back(id);
    ranks.push_back(0);

    return id;
}

Id UnionFind::find(Id id) const
{
    return canonicalize(id);
}

Id UnionFind::find(Id id)
{
    Id set = canonicalize(id);

    // An e-class id 𝑎 is canonical iff find(𝑎) = 𝑎.
    while (id != parents[uint32_t(id)])
    {
        // Note: we don't update the ranks here since a rank
        // represents the upper bound on the maximum depth of a tree
        Id parent = parents[uint32_t(id)];
        parents[uint32_t(id)] = set;
        id = parent;
    }

    return set;
}

Id UnionFind::merge(Id a, Id b)
{
    Id aSet = find(a);
    Id bSet = find(b);
    if (aSet == bSet)
        return aSet;

    // Ensure that the rank of set A is greater than the rank of set B
    if (ranks[uint32_t(aSet)] > ranks[uint32_t(bSet)])
        std::swap(aSet, bSet);

    parents[uint32_t(bSet)] = aSet;

    if (ranks[uint32_t(aSet)] == ranks[uint32_t(bSet)])
        ranks[uint32_t(aSet)]++;

    return aSet;
}

Id UnionFind::canonicalize(Id id) const
{
    LUAU_ASSERT(uint32_t(id) < parents.size());

    // An e-class id 𝑎 is canonical iff find(𝑎) = 𝑎.
    while (id != parents[uint32_t(id)])
        id = parents[uint32_t(id)];

    return id;
}

} // namespace Luau::EqSat
