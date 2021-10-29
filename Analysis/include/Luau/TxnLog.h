// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeVar.h"

namespace Luau
{

// Log of where what TypeIds we are rebinding and what they used to be
struct TxnLog
{
    TxnLog() = default;

    explicit TxnLog(const std::vector<std::pair<TypeId, TypeId>>& seen)
        : seen(seen)
    {
    }

    TxnLog(const TxnLog&) = delete;
    TxnLog& operator=(const TxnLog&) = delete;

    TxnLog(TxnLog&&) = default;
    TxnLog& operator=(TxnLog&&) = default;

    void operator()(TypeId a);
    void operator()(TypePackId a);
    void operator()(TableTypeVar* a);

    void rollback();

    void concat(TxnLog rhs);

    bool haveSeen(TypeId lhs, TypeId rhs);
    void pushSeen(TypeId lhs, TypeId rhs);
    void popSeen(TypeId lhs, TypeId rhs);

private:
    std::vector<std::pair<TypeId, TypeVar>> typeVarChanges;
    std::vector<std::pair<TypePackId, TypePackVar>> typePackChanges;
    std::vector<std::pair<TableTypeVar*, std::optional<TypeId>>> tableChanges;

public:
    std::vector<std::pair<TypeId, TypeId>> seen; // used to avoid infinite recursion when types are cyclic
};

} // namespace Luau
