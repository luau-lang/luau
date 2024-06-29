// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/UnionFind.h"

#include "Luau/Common.h"

namespace Luau::EqSat
{

Id UnionFind::makeSet()
{
    Id id{parents.size()};
    parents.push_back(id);
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

void UnionFind::merge(Id a, Id b)
{
    LUAU_ASSERT(size_t(a) < parents.size());
    LUAU_ASSERT(size_t(b) < parents.size());

    parents[size_t(b)] = a;
}

} // namespace Luau::EqSat
