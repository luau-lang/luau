// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/DenseHash.h"
#include "lluz/Error.h"
#include "lluz/TypeVar.h"
#include "lluz/TypePack.h"

#include <utility>

namespace lluz
{
struct InternalErrorReporter;

struct TypeIdPairHash
{
    size_t hashOne(lluz::TypeId key) const
    {
        return (uintptr_t(key) >> 4) ^ (uintptr_t(key) >> 9);
    }

    size_t operator()(const std::pair<lluz::TypeId, lluz::TypeId>& x) const
    {
        return hashOne(x.first) ^ (hashOne(x.second) << 1);
    }
};

struct UnifierCounters
{
    int recursionCount = 0;
    int recursionLimit = 0;
    int iterationCount = 0;
    int iterationLimit = 0;
};

struct UnifierSharedState
{
    UnifierSharedState(InternalErrorReporter* iceHandler)
        : iceHandler(iceHandler)
    {
    }

    InternalErrorReporter* iceHandler;

    DenseHashMap<TypeId, bool> skipCacheForType{nullptr};
    DenseHashSet<std::pair<TypeId, TypeId>, TypeIdPairHash> cachedUnify{{nullptr, nullptr}};
    DenseHashMap<std::pair<TypeId, TypeId>, TypeErrorData, TypeIdPairHash> cachedUnifyError{{nullptr, nullptr}};

    DenseHashSet<TypeId> tempSeenTy{nullptr};
    DenseHashSet<TypePackId> tempSeenTp{nullptr};

    UnifierCounters counters;
};

} // namespace lluz
