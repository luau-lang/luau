// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash.h"
#include "Luau/TypeVar.h"
#include "Luau/TypePack.h"

#include <utility>

namespace Luau
{
struct InternalErrorReporter;

struct TypeIdPairHash
{
    size_t hashOne(Luau::TypeId key) const
    {
        return (uintptr_t(key) >> 4) ^ (uintptr_t(key) >> 9);
    }

    size_t operator()(const std::pair<Luau::TypeId, Luau::TypeId>& x) const
    {
        return hashOne(x.first) ^ (hashOne(x.second) << 1);
    }
};

struct UnifierCounters
{
    int recursionCount = 0;
    int iterationCount = 0;
};

struct UnifierSharedState
{
    UnifierSharedState(InternalErrorReporter* iceHandler)
        : iceHandler(iceHandler)
    {
    }

    InternalErrorReporter* iceHandler;

    DenseHashSet<void*> seenAny{nullptr};
    DenseHashMap<TypeId, bool> skipCacheForType{nullptr};
    DenseHashSet<std::pair<TypeId, TypeId>, TypeIdPairHash> cachedUnify{{nullptr, nullptr}};

    DenseHashSet<TypeId> tempSeenTy{nullptr};
    DenseHashSet<TypePackId> tempSeenTp{nullptr};

    UnifierCounters counters;
};

} // namespace Luau
