// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"

#include <functional>
#include <utility>
#include <vector>
#include <type_traits>
#include <stdint.h>

namespace Luau
{

struct DenseHashPointer
{
    size_t operator()(const void* key) const
    {
        return (uintptr_t(key) >> 4) ^ (uintptr_t(key) >> 9);
    }
};

// Internal implementation of DenseHashSet and DenseHashMap
namespace detail
{

template<typename T>
using DenseHashDefault = std::conditional_t<std::is_pointer_v<T>, DenseHashPointer, std::hash<T>>;

template<typename Key, typename Item, typename MutableItem, typename ItemInterface, typename Hash, typename Eq>
class DenseHashTable
{
public:
    class const_iterator;

    DenseHashTable(const Key& empty_key, size_t buckets = 0)
        : count(0)
        , empty_key(empty_key)
    {
        // buckets has to be power-of-two or zero
        LUAU_ASSERT((buckets & (buckets - 1)) == 0);

        // don't move this to initializer list! this works around an MSVC codegen issue on AMD CPUs:
        // https://developercommunity.visualstudio.com/t/stdvector-constructor-from-size-t-is-25-times-slow/1546547
        if (buckets)
            data.resize(buckets, ItemInterface::create(empty_key));
    }

    void clear()
    {
        data.clear();
        count = 0;
    }

    Item* insert_unsafe(const Key& key)
    {
        // It is invalid to insert empty_key into the table since it acts as a "entry does not exist" marker
        LUAU_ASSERT(!eq(key, empty_key));

        size_t hashmod = data.size() - 1;
        size_t bucket = hasher(key) & hashmod;

        for (size_t probe = 0; probe <= hashmod; ++probe)
        {
            Item& probe_item = data[bucket];

            // Element does not exist, insert here
            if (eq(ItemInterface::getKey(probe_item), empty_key))
            {
                ItemInterface::setKey(probe_item, key);
                count++;
                return &probe_item;
            }

            // Element already exists
            if (eq(ItemInterface::getKey(probe_item), key))
            {
                return &probe_item;
            }

            // Hash collision, quadratic probing
            bucket = (bucket + probe + 1) & hashmod;
        }

        // Hash table is full - this should not happen
        LUAU_ASSERT(false);
        return NULL;
    }

    const Item* find(const Key& key) const
    {
        if (data.empty())
            return 0;
        if (eq(key, empty_key))
            return 0;

        size_t hashmod = data.size() - 1;
        size_t bucket = hasher(key) & hashmod;

        for (size_t probe = 0; probe <= hashmod; ++probe)
        {
            const Item& probe_item = data[bucket];

            // Element exists
            if (eq(ItemInterface::getKey(probe_item), key))
                return &probe_item;

            // Element does not exist
            if (eq(ItemInterface::getKey(probe_item), empty_key))
                return NULL;

            // Hash collision, quadratic probing
            bucket = (bucket + probe + 1) & hashmod;
        }

        // Hash table is full - this should not happen
        LUAU_ASSERT(false);
        return NULL;
    }

    void rehash()
    {
        size_t newsize = data.empty() ? 16 : data.size() * 2;

        if (data.empty() && data.capacity() >= newsize)
        {
            LUAU_ASSERT(count == 0);
            data.resize(newsize, ItemInterface::create(empty_key));
            return;
        }

        DenseHashTable newtable(empty_key, newsize);

        for (size_t i = 0; i < data.size(); ++i)
        {
            const Key& key = ItemInterface::getKey(data[i]);

            if (!eq(key, empty_key))
            {
                Item* item = newtable.insert_unsafe(key);
                *item = std::move(data[i]);
            }
        }

        LUAU_ASSERT(count == newtable.count);
        data.swap(newtable.data);
    }

    void rehash_if_full()
    {
        if (count >= data.size() * 3 / 4)
        {
            rehash();
        }
    }

    const_iterator begin() const
    {
        size_t start = 0;

        while (start < data.size() && eq(ItemInterface::getKey(data[start]), empty_key))
            start++;

        return const_iterator(this, start);
    }

    const_iterator end() const
    {
        return const_iterator(this, data.size());
    }

    size_t size() const
    {
        return count;
    }

    class const_iterator
    {
    public:
        const_iterator()
            : set(0)
            , index(0)
        {
        }

        const_iterator(const DenseHashTable<Key, Item, MutableItem, ItemInterface, Hash, Eq>* set, size_t index)
            : set(set)
            , index(index)
        {
        }

        const Item& operator*() const
        {
            return set->data[index];
        }

        const Item* operator->() const
        {
            return &set->data[index];
        }

        bool operator==(const const_iterator& other) const
        {
            return set == other.set && index == other.index;
        }

        bool operator!=(const const_iterator& other) const
        {
            return set != other.set || index != other.index;
        }

        const_iterator& operator++()
        {
            size_t size = set->data.size();

            do
            {
                index++;
            } while (index < size && set->eq(ItemInterface::getKey(set->data[index]), set->empty_key));

            return *this;
        }

        const_iterator operator++(int)
        {
            const_iterator res = *this;
            ++*this;
            return res;
        }

    private:
        const DenseHashTable<Key, Item, MutableItem, ItemInterface, Hash, Eq>* set;
        size_t index;
    };

private:
    std::vector<Item> data;
    size_t count;
    Key empty_key;
    Hash hasher;
    Eq eq;
};

template<typename Key>
struct ItemInterfaceSet
{
    static const Key& getKey(const Key& item)
    {
        return item;
    }

    static void setKey(Key& item, const Key& key)
    {
        item = key;
    }

    static Key create(const Key& key)
    {
        return key;
    }
};

template<typename Key, typename Value>
struct ItemInterfaceMap
{
    static const Key& getKey(const std::pair<Key, Value>& item)
    {
        return item.first;
    }

    static void setKey(std::pair<Key, Value>& item, const Key& key)
    {
        item.first = key;
    }

    static std::pair<Key, Value> create(const Key& key)
    {
        return std::pair<Key, Value>(key, Value());
    }
};

} // namespace detail

// This is a faster alternative of unordered_set, but it does not implement the same interface (i.e. it does not support erasing)
template<typename Key, typename Hash = detail::DenseHashDefault<Key>, typename Eq = std::equal_to<Key>>
class DenseHashSet
{
    typedef detail::DenseHashTable<Key, Key, Key, detail::ItemInterfaceSet<Key>, Hash, Eq> Impl;
    Impl impl;

public:
    typedef typename Impl::const_iterator const_iterator;

    DenseHashSet(const Key& empty_key, size_t buckets = 0)
        : impl(empty_key, buckets)
    {
    }

    void clear()
    {
        impl.clear();
    }

    const Key& insert(const Key& key)
    {
        impl.rehash_if_full();
        return *impl.insert_unsafe(key);
    }

    const Key* find(const Key& key) const
    {
        return impl.find(key);
    }

    bool contains(const Key& key) const
    {
        return impl.find(key) != 0;
    }

    size_t size() const
    {
        return impl.size();
    }

    bool empty() const
    {
        return impl.size() == 0;
    }

    const_iterator begin() const
    {
        return impl.begin();
    }

    const_iterator end() const
    {
        return impl.end();
    }
};

// This is a faster alternative of unordered_map, but it does not implement the same interface (i.e. it does not support erasing and has
// contains() instead of find())
template<typename Key, typename Value, typename Hash = detail::DenseHashDefault<Key>, typename Eq = std::equal_to<Key>>
class DenseHashMap
{
    typedef detail::DenseHashTable<Key, std::pair<Key, Value>, std::pair<const Key, Value>, detail::ItemInterfaceMap<Key, Value>, Hash, Eq> Impl;
    Impl impl;

public:
    typedef typename Impl::const_iterator const_iterator;

    DenseHashMap(const Key& empty_key, size_t buckets = 0)
        : impl(empty_key, buckets)
    {
    }

    void clear()
    {
        impl.clear();
    }

    // Note: this reference is invalidated by any insert operation (i.e. operator[])
    Value& operator[](const Key& key)
    {
        impl.rehash_if_full();
        return impl.insert_unsafe(key)->second;
    }

    // Note: this pointer is invalidated by any insert operation (i.e. operator[])
    const Value* find(const Key& key) const
    {
        const std::pair<Key, Value>* result = impl.find(key);

        return result ? &result->second : NULL;
    }

    // Note: this pointer is invalidated by any insert operation (i.e. operator[])
    Value* find(const Key& key)
    {
        const std::pair<Key, Value>* result = impl.find(key);

        return result ? const_cast<Value*>(&result->second) : NULL;
    }

    bool contains(const Key& key) const
    {
        return impl.find(key) != 0;
    }

    size_t size() const
    {
        return impl.size();
    }

    bool empty() const
    {
        return impl.size() == 0;
    }

    const_iterator begin() const
    {
        return impl.begin();
    }
    const_iterator end() const
    {
        return impl.end();
    }
};

} // namespace Luau
