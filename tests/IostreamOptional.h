// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash.h"
#include <ostream>
#include <optional>

namespace std
{

inline std::ostream& operator<<(std::ostream& lhs, const std::nullopt_t&)
{
    return lhs << "none";
}

template<typename T>
auto operator<<(std::ostream& lhs, const std::optional<T>& t) -> decltype(lhs << *t) // SFINAE to only instantiate << for supported types
{
    if (t)
        return lhs << *t;
    else
        return lhs << "none";
}

template<typename T>
auto operator<<(std::ostream& lhs, const std::vector<T>& t) -> decltype(lhs << t[0])
{
    lhs << "{ ";
    bool first = true;
    for (const T& element : t)
    {
        if (first)
            first = false;
        else
            lhs << ", ";

        lhs << element;
    }

    return lhs << " }";
}

template<typename K, typename H, typename E>
auto operator<<(std::ostream& lhs, const Luau::DenseHashSet<K, H, E>& set) -> decltype(lhs << *set.begin())
{
    lhs << "{ ";
    bool first = true;
    for (const K& element : set)
    {
        if (first)
            first = false;
        else
            lhs << ", ";

        lhs << element;
    }

    return lhs << " }";
}

} // namespace std
