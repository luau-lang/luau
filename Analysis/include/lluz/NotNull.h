// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/Common.h"

#include <functional>

namespace lluz
{

/** A non-owning, non-null pointer to a T.
 *
 * A NotNull<T> is notionally identical to a T* with the added restriction that
 * it can never store nullptr.
 *
 * The sole conversion rule from T* to NotNull<T> is the single-argument
 * constructor, which is intentionally marked explicit. This constructor
 * performs a runtime test to verify that the passed pointer is never nullptr.
 *
 * Pointer arithmetic, increment, decrement, and array indexing are all
 * forbidden.
 *
 * An implicit coersion from NotNull<T> to T* is afforded, as are the pointer
 * indirection and member access operators. (*p and p->prop)
 *
 * The explicit delete statement is permitted (but not recommended) on a
 * NotNull<T> through this implicit conversion.
 */
template<typename T>
struct NotNull
{
    explicit NotNull(T* t)
        : ptr(t)
    {
        lluz_ASSERT(t);
    }

    explicit NotNull(std::nullptr_t) = delete;
    void operator=(std::nullptr_t) = delete;

    template<typename U>
    NotNull(NotNull<U> other)
        : ptr(other.get())
    {
    }

    operator T*() const noexcept
    {
        return ptr;
    }

    T& operator*() const noexcept
    {
        return *ptr;
    }

    T* operator->() const noexcept
    {
        return ptr;
    }

    T& operator[](int) = delete;

    T& operator+(int) = delete;
    T& operator-(int) = delete;

    T* get() const noexcept
    {
        return ptr;
    }

private:
    T* ptr;
};

} // namespace lluz

namespace std
{

template<typename T>
struct hash<lluz::NotNull<T>>
{
    size_t operator()(const lluz::NotNull<T>& p) const
    {
        return std::hash<T*>()(p.get());
    }
};

} // namespace std
