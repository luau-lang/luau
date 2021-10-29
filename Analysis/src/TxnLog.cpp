// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TxnLog.h"

#include "Luau/TypePack.h"

#include <algorithm>

namespace Luau
{

void TxnLog::operator()(TypeId a)
{
    typeVarChanges.emplace_back(a, *a);
}

void TxnLog::operator()(TypePackId a)
{
    typePackChanges.emplace_back(a, *a);
}

void TxnLog::operator()(TableTypeVar* a)
{
    tableChanges.emplace_back(a, a->boundTo);
}

void TxnLog::rollback()
{
    for (auto it = typeVarChanges.rbegin(); it != typeVarChanges.rend(); ++it)
        std::swap(*asMutable(it->first), it->second);

    for (auto it = typePackChanges.rbegin(); it != typePackChanges.rend(); ++it)
        std::swap(*asMutable(it->first), it->second);

    for (auto it = tableChanges.rbegin(); it != tableChanges.rend(); ++it)
        std::swap(it->first->boundTo, it->second);
}

void TxnLog::concat(TxnLog rhs)
{
    typeVarChanges.insert(typeVarChanges.end(), rhs.typeVarChanges.begin(), rhs.typeVarChanges.end());
    rhs.typeVarChanges.clear();

    typePackChanges.insert(typePackChanges.end(), rhs.typePackChanges.begin(), rhs.typePackChanges.end());
    rhs.typePackChanges.clear();

    tableChanges.insert(tableChanges.end(), rhs.tableChanges.begin(), rhs.tableChanges.end());
    rhs.tableChanges.clear();

    seen.swap(rhs.seen);
    rhs.seen.clear();
}

bool TxnLog::haveSeen(TypeId lhs, TypeId rhs)
{
    const std::pair<TypeId, TypeId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    return (seen.end() != std::find(seen.begin(), seen.end(), sortedPair));
}

void TxnLog::pushSeen(TypeId lhs, TypeId rhs)
{
    const std::pair<TypeId, TypeId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    seen.push_back(sortedPair);
}

void TxnLog::popSeen(TypeId lhs, TypeId rhs)
{
    const std::pair<TypeId, TypeId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    LUAU_ASSERT(sortedPair == seen.back());
    seen.pop_back();
}

} // namespace Luau
