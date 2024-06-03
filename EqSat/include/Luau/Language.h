// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Id.h"
#include "Luau/Slice.h"

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

template<typename T, typename = void>
struct LanguageHash
{
    size_t operator()(const T&) const
    {
        // See available specializations at the bottom of this file.
        static_assert(false, "missing languageHash specialization");
    }
};

template <typename T>
std::size_t languageHash(const T& lang)
{
    return LanguageHash<T>{}(lang);
}

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
class NodeFields
{
    static_assert(std::conjunction<std::is_base_of<FieldBase, Fields>...>::value);

    std::array<Id, sizeof...(Fields)> array;

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
    const Id& field() const
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
};

// `Language` is very similar to `Luau::Variant` with enough differences warranting a different type altogether.
//
// Firstly, where `Luau::Variant` uses an `int` to decide which type the variant currently holds, we use
// a `const char*` instead. We use the pointer address for tag checking, and the string buffer for stringification.
//
// Secondly, we need `Language` to have additional methods such as:
// - `children()` to get child operands,
// - `operator==` to decide equality, and
// - `hash()` function.
//
// And finally, each `T` in `Ts` have additional requirements which `Luau::Variant` doesn't need.
template<typename... Ts>
class Language final
{
    const char* tag;
    char buffer[std::max({sizeof(Ts)...})];

private:
    template<typename T>
    using WithinDomain = std::disjunction<std::is_same<std::decay_t<T>, Ts>...>;

    using FnCopy = void (*)(void*, const void*);
    using FnMove = void (*)(void*, void*);
    using FnDtor = void (*)(void*);
    using FnPred = bool (*)(const void*, const void*);
    using FnHash = size_t (*)(const void*);

    template<typename T>
    using FnSlice = Slice<T> (*)(std::conditional_t<std::is_const_v<T>, const void*, void*>);

    template<typename T>
    static void fnCopy(void* dst, const void* src) noexcept
    {
        new (dst) T(*static_cast<const T*>(src));
    }

    template<typename T>
    static void fnMove(void* dst, void* src) noexcept
    {
        new (dst) T(static_cast<T&&>(*static_cast<T*>(src)));
    }

    template<typename T>
    static void fnDtor(void* dst) noexcept
    {
        static_cast<T*>(dst)->~T();
    }

    template<typename T>
    static bool fnPred(const void* lhs, const void* rhs) noexcept
    {
        return *static_cast<const T*>(lhs) == *static_cast<const T*>(rhs);
    }

    template<typename T>
    static size_t fnHash(const void* buffer) noexcept
    {
        return typename T::Hash{}(*static_cast<const T*>(buffer));
    }

    template<typename S, typename T>
    static Slice<S> fnOperands(std::conditional_t<std::is_const_v<S>, const void*, void*> buffer) noexcept
    {
        return static_cast<std::conditional_t<std::is_const_v<S>, const T*, T*>>(buffer)->operands();
    }

    static constexpr FnCopy tableCopy[sizeof...(Ts)] = {&fnCopy<Ts>...};
    static constexpr FnMove tableMove[sizeof...(Ts)] = {&fnMove<Ts>...};
    static constexpr FnDtor tableDtor[sizeof...(Ts)] = {&fnDtor<Ts>...};
    static constexpr FnPred tablePred[sizeof...(Ts)] = {&fnPred<Ts>...};
    static constexpr FnHash tableHash[sizeof...(Ts)] = {&fnHash<Ts>...};
    static constexpr FnSlice<Id> tableSliceId[sizeof...(Ts)] = {&fnOperands<Id, Ts>...};
    static constexpr FnSlice<const Id> tableSliceConstId[sizeof...(Ts)] = {&fnOperands<const Id, Ts>...};

    static constexpr int getIndexFromTag(const char* tag) noexcept
    {
        constexpr int N = sizeof...(Ts);
        constexpr const char* is[N] = {Ts::tag...};

        for (int i = 0; i < N; ++i)
            if (is[i] == tag)
                return i;

        return -1;
    }

public:
    template<typename T>
    Language(T&& t, std::enable_if_t<WithinDomain<T>::value>* = 0) noexcept
    {
        tag = std::decay_t<T>::tag;
        new (&buffer) std::decay_t<T>(std::forward<T>(t));
    }

    Language(const Language& other) noexcept
    {
        tag = other.tag;
        tableCopy[getIndexFromTag(tag)](&buffer, &other.buffer);
    }

    Language(Language&& other) noexcept
    {
        tag = other.tag;
        tableMove[getIndexFromTag(tag)](&buffer, &other.buffer);
    }

    ~Language() noexcept
    {
        tableDtor[getIndexFromTag(tag)](&buffer);
    }

    Language& operator=(const Language& other) noexcept
    {
        Language copy{other};
        *this = static_cast<Language&&>(copy);
        return *this;
    }

    Language& operator=(Language&& other) noexcept
    {
        if (this != &other)
        {
            tableDtor[getIndexFromTag(tag)](&buffer);
            tag = other.tag;
            tableMove[getIndexFromTag(tag)](&buffer, &other.buffer); // nothrow
        }
        return *this;
    }

    int index() const noexcept
    {
        return getIndexFromTag(tag);
    }

    /// You should never call this function with the intention of mutating the `Id`.
    /// Reading is ok, but you should also never assume that these `Id`s are stable.
    Slice<Id> operands() noexcept
    {
        return tableSliceId[getIndexFromTag(tag)](&buffer);
    }

    Slice<const Id> operands() const noexcept
    {
        return tableSliceConstId[getIndexFromTag(tag)](&buffer);
    }

    template<typename T>
    const T* get() const noexcept
    {
        static_assert(WithinDomain<T>::value);
        return tag == T::tag ? reinterpret_cast<const T*>(&buffer) : nullptr;
    }

    bool operator==(const Language& rhs) const noexcept
    {
        return tag == rhs.tag && tablePred[getIndexFromTag(tag)](&buffer, &rhs.buffer);
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
            size_t seed = std::hash<const char*>{}(language.tag);
            hashCombine(seed, tableHash[getIndexFromTag(language.tag)](&language.buffer));
            return seed;
        }
    };
};

inline void hashCombine(size_t& seed, size_t hash)
{
    // Golden Ratio constant used for better hash scattering
    // See https://softwareengineering.stackexchange.com/a/402543
    seed ^= hash + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

template<typename T>
struct LanguageHash<T, std::void_t<decltype(std::hash<T>{}(std::declval<T>()))>>
{
    size_t operator()(const T& t) const
    {
        return std::hash<T>{}(t);
    }
};

template<typename T, size_t I>
struct LanguageHash<std::array<T, I>>
{
    size_t operator()(const std::array<T, I>& array) const
    {
        size_t seed = 0;
        for (const T& t : array)
            hashCombine(seed, languageHash(t));
        return seed;
    }
};

template<typename T>
struct LanguageHash<std::vector<T>>
{
    size_t operator()(const std::vector<T>& vector) const
    {
        size_t seed = 0;
        for (const T& t : vector)
            hashCombine(seed, languageHash(t));
        return seed;
    }
};

} // namespace Luau::EqSat
