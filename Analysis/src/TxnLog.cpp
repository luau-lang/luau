// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TxnLog.h"

#include "Luau/TypePack.h"

#include <algorithm>

LUAU_FASTFLAGVARIABLE(LuauShareTxnSeen, false)

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

    if (FFlag::LuauShareTxnSeen)
    {
        LUAU_ASSERT(originalSeenSize <= sharedSeen->size());
        sharedSeen->resize(originalSeenSize);
    }
}

void TxnLog::concat(TxnLog rhs)
{
    typeVarChanges.insert(typeVarChanges.end(), rhs.typeVarChanges.begin(), rhs.typeVarChanges.end());
    rhs.typeVarChanges.clear();

    typePackChanges.insert(typePackChanges.end(), rhs.typePackChanges.begin(), rhs.typePackChanges.end());
    rhs.typePackChanges.clear();

    tableChanges.insert(tableChanges.end(), rhs.tableChanges.begin(), rhs.tableChanges.end());
    rhs.tableChanges.clear();

    if (!FFlag::LuauShareTxnSeen)
    {
        ownedSeen.swap(rhs.ownedSeen);
        rhs.ownedSeen.clear();
    }
}

bool TxnLog::haveSeen(TypeId lhs, TypeId rhs)
{
    const std::pair<TypeId, TypeId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    if (FFlag::LuauShareTxnSeen)
        return (sharedSeen->end() != std::find(sharedSeen->begin(), sharedSeen->end(), sortedPair));
    else
        return (ownedSeen.end() != std::find(ownedSeen.begin(), ownedSeen.end(), sortedPair));
}

void TxnLog::pushSeen(TypeId lhs, TypeId rhs)
{
    const std::pair<TypeId, TypeId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    if (FFlag::LuauShareTxnSeen)
        sharedSeen->push_back(sortedPair);
    else
        ownedSeen.push_back(sortedPair);
}

void TxnLog::popSeen(TypeId lhs, TypeId rhs)
{
    const std::pair<TypeId, TypeId> sortedPair = (lhs > rhs) ? std::make_pair(lhs, rhs) : std::make_pair(rhs, lhs);
    if (FFlag::LuauShareTxnSeen)
    {
        LUAU_ASSERT(sortedPair == sharedSeen->back());
        sharedSeen->pop_back();
    }
    else
    {
        LUAU_ASSERT(sortedPair == ownedSeen.back());
        ownedSeen.pop_back();
    }
}

} // namespace Luau
