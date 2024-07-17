// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/UnionFind.h"

#include "Luau/Common.h"

namespace Luau::EqSat
{

Id UnionFind::makeSet()
{
    Id id{parents.size()};
    parents.push_back(id);
    ranks.push_back(0);

    return id;
}

Id UnionFind::find(Id id) const
{
    LUAU_ASSERT(size_t(id) < parents.size());

    // An e-class id 𝑎 is canonical iff find(𝑎) = 𝑎.
    while (id != parents[size_t(id)])
        id = parents[size_t(id)];

    return id;
}

Id UnionFind::find(Id id)
{
    LUAU_ASSERT(size_t(id) < parents.size());

    // An e-class id 𝑎 is canonical iff find(𝑎) = 𝑎.
    if (id != parents[size_t(id)])
        // Note: we don't update the ranks here since a rank
        // represents the upper bound on the maximum depth of a tree
        parents[size_t(id)] = find(parents[size_t(id)]);
    
    return parents[size_t(id)];
}

void UnionFind::merge(Id a, Id b)
{
    Id aSet = find(a);
    Id bSet = find(b);
    if (aSet == bSet)
        return;

    // Ensure that the rank of set A is greater than the rank of set B
    if (ranks[size_t(aSet)] < ranks[size_t(bSet)])
        std::swap(aSet, bSet);

    parents[size_t(bSet)] = aSet;
    
    if (ranks[size_t(aSet)] == ranks[size_t(bSet)])
        ranks[size_t(aSet)]++;
}

} // namespace Luau::EqSat
