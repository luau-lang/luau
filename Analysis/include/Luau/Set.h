// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/DenseHash.h"

LUAU_FASTFLAG(LuauSolverV2)

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
    using Impl = DenseHashMap<T, bool, Hash>;
    Impl mapping;
    size_t entryCount = 0;

public:
    class const_iterator;
    using iterator = const_iterator;

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

    void erase(T&& element)
    {
        bool& entry = mapping[element];

        if (entry)
        {
            entry = false;
            entryCount--;
        }
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

    const_iterator begin() const
    {
        return const_iterator(mapping.begin(), mapping.end());
    }

    const_iterator end() const
    {
        return const_iterator(mapping.end(), mapping.end());
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

    class const_iterator
    {
    public:
        using value_type = T;
        using reference = T&;
        using pointer = T*;
        using difference_type = ptrdiff_t;
        using iterator_category = std::forward_iterator_tag;

        const_iterator(typename Impl::const_iterator impl_, typename Impl::const_iterator end_)
            : impl(impl_)
            , end(end_)
        {
            while (impl != end && impl->second == false)
                ++impl;
        }

        const T& operator*() const
        {
            return impl->first;
        }

        const T* operator->() const
        {
            return &impl->first;
        }

        bool operator==(const const_iterator& other) const
        {
            return impl == other.impl;
        }

        bool operator!=(const const_iterator& other) const
        {
            return impl != other.impl;
        }


        const_iterator& operator++()
        {
            do
            {
                impl++;
            } while (impl != end && impl->second == false);
            // keep iterating past pairs where the value is `false`

            return *this;
        }

        const_iterator operator++(int)
        {
            const_iterator res = *this;
            ++*this;
            return res;
        }

    private:
        typename Impl::const_iterator impl;
        typename Impl::const_iterator end;
    };
};

} // namespace Luau
