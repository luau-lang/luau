// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <algorithm>
#include <type_traits>
#include <utility>

namespace Luau::EqSat
{

#define LUAU_EQSAT_ATOM(name, t) LUAU_EQSAT_ATOM_CUSTOM(name, #name, t)
#define LUAU_EQSAT_ATOM_CUSTOM(name, custom, t) \
    struct name : public ::Luau::EqSat::Atom<name, t> \
    { \
        static constexpr const char* tag = custom; \
    }

template<typename B, typename T>
struct Atom
{
    T value;

    bool operator==(const Atom& rhs) const
    {
        return value == rhs.value;
    }

    bool operator!=(const Atom& rhs) const
    {
        return !(*this == rhs);
    }

    struct Hash
    {
        size_t operator()(const Atom& atom) const
        {
            return std::hash<T>{}(atom.value);
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
class Language
{
    const char* tag;
    char buffer[std::max({sizeof(Ts)...})];

    template<typename T>
    using WithinDomain = std::disjunction<std::is_same<std::decay_t<T>, Ts>...>;

    using FnCopy = void (*)(void*, const void*);
    using FnMove = void (*)(void*, void*);
    using FnDtor = void (*)(void*);
    using FnPred = bool (*)(const void*, const void*);
    using FnHash = size_t (*)(const void*);

    template<typename T>
    static void fnCopy(void* dst, const void* src)
    {
        new (dst) T(*static_cast<const T*>(src));
    }

    template<typename T>
    static void fnMove(void* dst, void* src)
    {
        // static_cast<T&&> is equivalent to std::move() but faster in Debug
        new (dst) T(static_cast<T&&>(*static_cast<T*>(src)));
    }

    template<typename T>
    static void fnDtor(void* dst)
    {
        static_cast<T*>(dst)->~T();
    }

    template<typename T>
    static bool fnPred(const void* lhs, const void* rhs)
    {
        return *static_cast<const T*>(lhs) == *static_cast<const T*>(rhs);
    }

    template<typename T>
    static size_t fnHash(const void* buffer)
    {
        return typename T::Hash{}(*static_cast<const T*>(buffer));
    }

    static constexpr FnCopy tableCopy[sizeof...(Ts)] = {&fnCopy<Ts>...};
    static constexpr FnMove tableMove[sizeof...(Ts)] = {&fnMove<Ts>...};
    static constexpr FnDtor tableDtor[sizeof...(Ts)] = {&fnDtor<Ts>...};
    static constexpr FnPred tablePred[sizeof...(Ts)] = {&fnPred<Ts>...};
    static constexpr FnHash tableHash[sizeof...(Ts)] = {&fnHash<Ts>...};

    static constexpr int getIndexFromTag(const char* tag)
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
    Language(T&& t, std::enable_if_t<WithinDomain<T>::value>* = 0)
    {
        tag = T::tag;
        new (&buffer) std::decay_t<T>(std::forward<T>(t));
    }

    Language(const Language& other)
    {
        tag = other.tag;
        tableCopy[getIndexFromTag(tag)](&buffer, &other.buffer);
    }

    Language(Language&& other)
    {
        tag = other.tag;
        tableMove[getIndexFromTag(tag)](&buffer, &other.buffer);
    }

    ~Language()
    {
        tableDtor[getIndexFromTag(tag)](&buffer);
    }

    Language& operator=(const Language& other)
    {
        Language copy{other};
        *this = static_cast<Language&&>(copy);
        return *this;
    }

    Language& operator=(Language&& other)
    {
        if (this != &other)
        {
            tableDtor[getIndexFromTag(tag)](&buffer);
            tag = other.tag;
            tableMove[getIndexFromTag(tag)](&buffer, &other.buffer); // nothrow
        }
        return *this;
    }

    template<typename T>
    const T* get() const
    {
        static_assert(WithinDomain<T>::value);
        return tag == T::tag ? reinterpret_cast<const T*>(&buffer) : nullptr;
    }

    bool operator==(const Language& rhs) const
    {
        return tag == rhs.tag && tablePred[getIndexFromTag(tag)](&buffer, &rhs.buffer);
    }

    bool operator!=(const Language& rhs) const
    {
        return !(*this == rhs);
    }

public:
    struct Hash
    {
        size_t operator()(const Language& language) const
        {
            size_t hash = std::hash<const char*>{}(language.tag);
            hash ^= tableHash[getIndexFromTag(language.tag)](&language.buffer);
            return hash;
        }
    };
};

} // namespace Luau::EqSat
