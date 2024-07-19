// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Id.h"
#include "Luau/LanguageHash.h"
#include "Luau/Slice.h"
#include "Luau/Variant.h"

#include <array>
#include <algorithm>
#include <type_traits>
#include <utility>

#define LUAU_EQSAT_ATOM(name, t) \
    struct name : public ::Luau::EqSat::Atom<name, t> \
    { \
        static constexpr const char* tag = #name; \
        using Atom::Atom; \
    }

#define LUAU_EQSAT_NODE_ARRAY(name, ops) \
    struct name : public ::Luau::EqSat::NodeVector<name, std::array<::Luau::EqSat::Id, ops>> \
    { \
        static constexpr const char* tag = #name; \
        using NodeVector::NodeVector; \
    }

#define LUAU_EQSAT_NODE_VECTOR(name) \
    struct name : public ::Luau::EqSat::NodeVector<name, std::vector<::Luau::EqSat::Id>> \
    { \
        static constexpr const char* tag = #name; \
        using NodeVector::NodeVector; \
    }

#define LUAU_EQSAT_FIELD(name) \
    struct name : public ::Luau::EqSat::Field<name> \
    { \
    }

#define LUAU_EQSAT_NODE_FIELDS(name, ...) \
    struct name : public ::Luau::EqSat::NodeFields<name, __VA_ARGS__> \
    { \
        static constexpr const char* tag = #name; \
        using NodeFields::NodeFields; \
    }

namespace Luau::EqSat
{

template<typename Phantom, typename T>
struct Atom
{
    Atom(const T& value)
        : _value(value)
    {
    }

    const T& value() const
    {
        return _value;
    }

public:
    Slice<Id> operands()
    {
        return {};
    }

    Slice<const Id> operands() const
    {
        return {};
    }

    bool operator==(const Atom& rhs) const
    {
        return _value == rhs._value;
    }

    bool operator!=(const Atom& rhs) const
    {
        return !(*this == rhs);
    }

    struct Hash
    {
        size_t operator()(const Atom& value) const
        {
            return languageHash(value._value);
        }
    };

private:
    T _value;
};

template<typename Phantom, typename T>
struct NodeVector
{
    template<typename... Args>
    NodeVector(Args&&... args)
        : vector{std::forward<Args>(args)...}
    {
    }

    Id operator[](size_t i) const
    {
        return vector[i];
    }

public:
    Slice<Id> operands()
    {
        return Slice{vector.data(), vector.size()};
    }

    Slice<const Id> operands() const
    {
        return Slice{vector.data(), vector.size()};
    }

    bool operator==(const NodeVector& rhs) const
    {
        return vector == rhs.vector;
    }

    bool operator!=(const NodeVector& rhs) const
    {
        return !(*this == rhs);
    }

    struct Hash
    {
        size_t operator()(const NodeVector& value) const
        {
            return languageHash(value.vector);
        }
    };

private:
    T vector;
};

/// Empty base class just for static_asserts.
struct FieldBase
{
    FieldBase() = delete;

    FieldBase(FieldBase&&) = delete;
    FieldBase& operator=(FieldBase&&) = delete;

    FieldBase(const FieldBase&) = delete;
    FieldBase& operator=(const FieldBase&) = delete;
};

template<typename Phantom>
struct Field : FieldBase
{
};

template<typename Phantom, typename... Fields>
struct NodeFields
{
    static_assert(std::conjunction<std::is_base_of<FieldBase, Fields>...>::value);

    template<typename T>
    static constexpr int getIndex()
    {
        constexpr int N = sizeof...(Fields);
        constexpr bool is[N] = {std::is_same_v<std::decay_t<T>, Fields>...};

        for (int i = 0; i < N; ++i)
            if (is[i])
                return i;

        return -1;
    }

public:
    template<typename... Args>
    NodeFields(Args&&... args)
        : array{std::forward<Args>(args)...}
    {
    }

    Slice<Id> operands()
    {
        return Slice{array};
    }

    Slice<const Id> operands() const
    {
        return Slice{array.data(), array.size()};
    }

    template<typename T>
    Id field() const
    {
        static_assert(std::disjunction_v<std::is_same<std::decay_t<T>, Fields>...>);
        return array[getIndex<T>()];
    }

    bool operator==(const NodeFields& rhs) const
    {
        return array == rhs.array;
    }

    bool operator!=(const NodeFields& rhs) const
    {
        return !(*this == rhs);
    }

    struct Hash
    {
        size_t operator()(const NodeFields& value) const
        {
            return languageHash(value.array);
        }
    };

private:
    std::array<Id, sizeof...(Fields)> array;
};

template<typename... Ts>
struct Language final
{
    template<typename T>
    using WithinDomain = std::disjunction<std::is_same<std::decay_t<T>, Ts>...>;

    template<typename T>
    Language(T&& t, std::enable_if_t<WithinDomain<T>::value>* = 0) noexcept
        : v(std::forward<T>(t))
    {
    }

    Language(const Language&) noexcept = default;
    Language& operator=(const Language&) noexcept = default;

    Language(Language&&) noexcept = default;
    Language& operator=(Language&&) noexcept = default;

    int index() const noexcept
    {
        return v.index();
    }

    /// You should never call this function with the intention of mutating the `Id`.
    /// Reading is ok, but you should also never assume that these `Id`s are stable.
    Slice<Id> operands() noexcept
    {
        return visit([](auto&& v) -> Slice<Id> {
            return v.operands();
        }, v);
    }

    Slice<const Id> operands() const noexcept
    {
        return visit([](auto&& v) -> Slice<const Id> {
            return v.operands();
        }, v);
    }

    template<typename T>
    T* get() noexcept
    {
        static_assert(WithinDomain<T>::value);
        return v.template get_if<T>();
    }

    template<typename T>
    const T* get() const noexcept
    {
        static_assert(WithinDomain<T>::value);
        return v.template get_if<T>();
    }

    bool operator==(const Language& rhs) const noexcept
    {
        return v == rhs.v;
    }

    bool operator!=(const Language& rhs) const noexcept
    {
        return !(*this == rhs);
    }

public:
    struct Hash
    {
        size_t operator()(const Language& language) const
        {
            size_t seed = std::hash<int>{}(language.index());
            hashCombine(seed, visit([](auto&& v) {
                return typename std::decay_t<decltype(v)>::Hash{}(v);
            }, language.v));
            return seed;
        }
    };

private:
    Variant<Ts...> v;
};

} // namespace Luau::EqSat
