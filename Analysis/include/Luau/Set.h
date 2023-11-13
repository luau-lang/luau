// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash.h"

namespace Luau
{

template<typename T>
using SetHashDefault = std::conditional_t<std::is_pointer_v<T>, DenseHashPointer, std::hash<T>>;

// This is an implementation of `unordered_set` using `DenseHashMap<T, bool>` to support erasure.
// This lets us work around `DenseHashSet` limitations and get a more traditional set interface.
template<typename T, typename Hash = SetHashDefault<T>>
class Set
{
private:
    DenseHashMap<T, bool, Hash> mapping;
    size_t entryCount = 0;

public:
    Set(const T& empty_key)
        : mapping{empty_key}
    {
    }

    bool insert(const T& element)
    {
        bool& entry = mapping[element];
        bool fresh = !entry;

        if (fresh)
        {
            entry = true;
            entryCount++;
        }

        return fresh;
    }

    template<class Iterator>
    void insert(Iterator begin, Iterator end)
    {
        for (Iterator it = begin; it != end; ++it)
            insert(*it);
    }

    void erase(const T& element)
    {
        bool& entry = mapping[element];

        if (entry)
        {
            entry = false;
            entryCount--;
        }
    }

    void clear()
    {
        mapping.clear();
        entryCount = 0;
    }

    size_t size() const
    {
        return entryCount;
    }

    bool empty() const
    {
        return entryCount == 0;
    }

    size_t count(const T& element) const
    {
        const bool* entry = mapping.find(element);
        return (entry && *entry) ? 1 : 0;
    }

    bool contains(const T& element) const
    {
        return count(element) != 0;
    }

    bool operator==(const Set<T>& there) const
    {
        // if the sets are unequal sizes, then they cannot possibly be equal.
        if (size() != there.size())
            return false;

        // otherwise, we'll need to check that every element we have here is in `there`.
        for (auto [elem, present] : mapping)
        {
            // if it's not, we'll return `false`
            if (present && there.contains(elem))
                return false;
        }

        // otherwise, we've proven the two equal!
        return true;
    }
};

} // namespace Luau
