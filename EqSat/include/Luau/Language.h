// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <algorithm>
#include <type_traits>
#include <utility>

namespace Luau::EqSat
{

#define LUAU_EQSAT_ATOM(name, value) LUAU_EQSAT_ATOM_CUSTOM(name, #name, value)
#define LUAU_EQSAT_ATOM_CUSTOM(name, custom, value) \
    struct name : public ::Luau::EqSat::Atom<name, value> \
    { \
        static constexpr const char* tag = custom; \
    }

template<typename B, typename T>
struct Atom
{
    T value;
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

public:
    template<typename T>
    Language(T&& t)
    {
        using TT = std::decay_t<T>;
        static_assert(std::disjunction_v<std::is_same<TT, Ts>...>);

        tag = T::tag;
        new (&buffer) TT(std::forward<T>(t));
    }

    template<typename T>
    const T* get() const
    {
        static_assert(std::disjunction_v<std::is_same<std::decay_t<T>, Ts>...>);

        return tag == T::tag ? reinterpret_cast<const T*>(&buffer) : nullptr;
    }

public:
    struct Hash
    {
        size_t operator()(const Language<Ts...>& lang)
        {
            // TODO. Currently here so I can build and test this project.
            return 0;
        }
    };
};

} // namespace Luau::EqSat
