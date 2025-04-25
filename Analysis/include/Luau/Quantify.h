// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeFwd.h"
#include "Luau/DenseHash.h"
#include "Luau/Unifiable.h"

#include <vector>
#include <optional>

namespace Luau
{

struct TypeArena;
struct Scope;

void quantify(TypeId ty, TypeLevel level);

// TODO: This is eerily similar to the pattern that NormalizedExternType
// implements. We could, and perhaps should, merge them together.
template<typename K, typename V>
struct OrderedMap
{
    std::vector<K> keys;
    DenseHashMap<K, V> pairings{nullptr};

    void push(K k, V v)
    {
        keys.push_back(k);
        pairings[k] = v;
    }
};

} // namespace Luau
