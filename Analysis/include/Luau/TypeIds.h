// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash.h"
#include "Luau/TypeFwd.h"

#include <vector>
#include <initializer_list>

namespace Luau
{

/*
 * An ordered, hashable set of TypeIds.
 */
class TypeIds
{
private:
    DenseHashMap<TypeId, bool> types{nullptr};
    std::vector<TypeId> order;
    std::size_t hash = 0;

public:
    using iterator = std::vector<TypeId>::iterator;
    using const_iterator = std::vector<TypeId>::const_iterator;

    TypeIds() = default;
    ~TypeIds() = default;

    TypeIds(std::initializer_list<TypeId> tys);

    TypeIds(const TypeIds&) = default;
    TypeIds& operator=(const TypeIds&) = default;

    TypeIds(TypeIds&&) noexcept = default;
    TypeIds& operator=(TypeIds&&) noexcept = default;

    void insert(TypeId ty);
    /// Erase every element that does not also occur in tys
    void retain(const TypeIds& tys);
    void clear();

    TypeId front() const;
    iterator begin();
    iterator end();
    const_iterator begin() const;
    const_iterator end() const;
    iterator erase(const_iterator it);
    void erase(TypeId ty);

    size_t size() const;
    bool empty() const;
    size_t count(TypeId ty) const;

    void reserve(size_t n);

    template<class Iterator>
    void insert(Iterator begin, Iterator end)
    {
        for (Iterator it = begin; it != end; ++it)
            insert(*it);
    }

    bool operator==(const TypeIds& there) const;
    size_t getHash() const;
    bool isNever() const;

    /**
     * Moves the contents of this container into a `std::vector` and returns it.
     * This container will be empty after `take` is called.
     */
    std::vector<TypeId> take();
};

} // namespace Luau
