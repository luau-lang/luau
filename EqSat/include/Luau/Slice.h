// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"

#include <array>
#include <cstddef>

namespace Luau::EqSat
{

template<typename T>
struct Slice final
{
    Slice()
        : _data(nullptr)
        , _size(0)
    {
    }

    /// Use this constructor if you have a dynamically sized vector.
    /// The slice is valid for as long as the backing vector has not moved
    /// elsewhere in memory.
    ///
    /// In general, a slice should never be used from vectors except for
    /// any vectors whose size are statically unknown, but remains fixed
    /// upon the construction of such a slice over a vector.
    Slice(T* first, size_t last)
        : _data(first)
        , _size(last)
    {
    }

    template<size_t I>
    explicit Slice(std::array<T, I>& array)
        : _data(array.data())
        , _size(array.size())
    {
    }

    T* data() const
    {
        return _data;
    }

    size_t size() const
    {
        return _size;
    }

    bool empty() const
    {
        return _size == 0;
    }

    T& operator[](size_t i) const
    {
        LUAU_ASSERT(i < _size);
        return _data[i];
    }

public:
    T* _data;
    size_t _size;

public:
    T* begin() const
    {
        return _data;
    }

    T* end() const
    {
        return _data + _size;
    }
};

} // namespace Luau::EqSat
