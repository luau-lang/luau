// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include <vector>
#include "Luau/DenseHash.h"

namespace Luau
{

template<typename T>
struct OrderedSet
{
    using iterator = typename std::vector<T>::iterator;
    using const_iterator = typename std::vector<T>::const_iterator;

    bool empty() const
    {
        return elements.empty();
    }

    size_t size() const
    {
        return elements.size();
    }

    void insert(T t)
    {
        if (!elementSet.contains(t))
        {
            elementSet.insert(t);
            elements.push_back(t);
        }
    }

    iterator begin()
    {
        return elements.begin();
    }

    const_iterator begin() const
    {
        return elements.begin();
    }

    iterator end()
    {
        return elements.end();
    }

    const_iterator end() const
    {
        return elements.end();
    }

    /// Move the underlying vector out of the OrderedSet.
    std::vector<T> takeVector()
    {
        elementSet.clear();
        return std::move(elements);
    }

private:
    std::vector<T> elements;
    DenseHashSet<T> elementSet{nullptr};
};

} // namespace Luau