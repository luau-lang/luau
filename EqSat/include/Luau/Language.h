// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Id.h"
#include "Luau/LanguageHash.h"
#include "Luau/Slice.h"
#include "Luau/Variant.h"

#include <algorithm>
#include <array>
#include <type_traits>
#include <unordered_set>
#include <utility>
#include <vector>

#define LUAU_EQSAT_UNIT(name) \
    struct name : ::Luau::EqSat::Unit<name> \
    { \
        static constexpr const char* tag = #name; \
        using Unit::Unit; \
    }

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

#define LUAU_EQSAT_NODE_SET(name) \
    struct name : public ::Luau::EqSat::NodeSet<name, std::vector<::Luau::EqSat::Id>> \
    { \
        static constexpr const char* tag = #name; \
        using NodeSet::NodeSet; \
    }

#define LUAU_EQSAT_NODE_ATOM_WITH_VECTOR(name, t) \
    struct name : public ::Luau::EqSat::NodeAtomAndVector<name, t, std::vector<::Luau::EqSat::Id>> \
    { \
        static constexpr const char* tag = #name; \
        using NodeAtomAndVector::NodeAtomAndVector; \
    }

namespace Luau::EqSat
{

template<typename Phantom>
struct Unit
{
    Slice<Id> mutableOperands()
    {
        return {};
    }

    Slice<const Id> operands() const
    {
        return {};
    }

    bool operator==(const Unit& rhs) const
    {
        return true;
    }

    bool operator!=(const Unit& rhs) const
    {
        return false;
    }

    struct Hash
    {
        size_t operator()(const Unit& value) const
        {
            // chosen by fair dice roll.
            // guaranteed to be random.
            return 4;
        }
    };
};

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
    Slice<Id> mutableOperands()
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

template<typename Phantom, typename X, typename T>
struct NodeAtomAndVector
{
    template<typename... Args>
    NodeAtomAndVector(const X& value, Args&&... args)
        : _value(value)
        , vector{std::forward<Args>(args)...}
    {
    }

    Id operator[](size_t i) const
    {
        return vector[i];
    }

public:
    const X& value() const
    {
        return _value;
    }

    Slice<Id> mutableOperands()
    {
        return Slice{vector.data(), vector.size()};
    }

    Slice<const Id> operands() const
    {
        return Slice{vector.data(), vector.size()};
    }

    bool operator==(const NodeAtomAndVector& rhs) const
    {
        return _value == rhs._value && vector == rhs.vector;
    }

    bool operator!=(const NodeAtomAndVector& rhs) const
    {
        return !(*this == rhs);
    }

    struct Hash
    {
        size_t operator()(const NodeAtomAndVector& value) const
        {
            size_t result = languageHash(value._value);
            hashCombine(result, languageHash(value.vector));
            return result;
        }
    };

private:
    X _value;
    T vector;
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
    Slice<Id> mutableOperands()
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

template<typename Phantom, typename T>
struct NodeSet
{
    template<typename P_, typename T_, typename Find>
    friend void canonicalize(NodeSet<P_, T_>& node, Find&& find);

    template<typename... Args>
    NodeSet(Args&&... args)
        : vector{std::forward<Args>(args)...}
    {
        std::sort(begin(vector), end(vector));
        auto it = std::unique(begin(vector), end(vector));
        vector.erase(it, end(vector));
    }

    Id operator[](size_t i) const
    {
        return vector[i];
    }

public:
    Slice<Id> mutableOperands()
    {
        return Slice{vector.data(), vector.size()};
    }

    Slice<const Id> operands() const
    {
        return Slice{vector.data(), vector.size()};
    }

    bool operator==(const NodeSet& rhs) const
    {
        return vector == rhs.vector;
    }

    bool operator!=(const NodeSet& rhs) const
    {
        return !(*this == rhs);
    }

    struct Hash
    {
        size_t operator()(const NodeSet& value) const
        {
            return languageHash(value.vector);
        }
    };

protected:
    T vector;
};

template<typename... Ts>
struct Language final
{
    using VariantTy = Luau::Variant<Ts...>;

    template<typename T>
    using WithinDomain = std::disjunction<std::is_same<std::decay_t<T>, Ts>...>;

    template<typename Find, typename... Vs>
    friend void canonicalize(Language<Vs...>& enode, Find&& find);

    template<typename T>
    Language(T&& t, std::enable_if_t<WithinDomain<T>::value>* = 0) noexcept
        : v(std::forward<T>(t))
    {
    }

    int index() const noexcept
    {
        return v.index();
    }

    /// This should only be used in canonicalization!
    /// Always prefer operands()
    Slice<Id> mutableOperands() noexcept
    {
        return visit(
            [](auto&& v) -> Slice<Id>
            {
                return v.mutableOperands();
            },
            v
        );
    }

    Slice<const Id> operands() const noexcept
    {
        return visit(
            [](auto&& v) -> Slice<const Id>
            {
                return v.operands();
            },
            v
        );
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
            hashCombine(
                seed,
                visit(
                    [](auto&& v)
                    {
                        return typename std::decay_t<decltype(v)>::Hash{}(v);
                    },
                    language.v
                )
            );
            return seed;
        }
    };

private:
    VariantTy v;
};

template<typename Node, typename Find>
void canonicalize(Node& node, Find&& find)
{
    // An e-node 𝑛 is canonical iff 𝑛 = canonicalize(𝑛), where
    // canonicalize(𝑓(𝑎1, 𝑎2, ...)) = 𝑓(find(𝑎1), find(𝑎2), ...).
    for (Id& id : node.mutableOperands())
        id = find(id);
}

// Canonicalizing the Ids in a NodeSet may result in the set decreasing in size.
template<typename Phantom, typename T, typename Find>
void canonicalize(NodeSet<Phantom, T>& node, Find&& find)
{
    for (Id& id : node.vector)
        id = find(id);

    std::sort(begin(node.vector), end(node.vector));
    auto endIt = std::unique(begin(node.vector), end(node.vector));
    node.vector.erase(endIt, end(node.vector));
}

template<typename Find, typename... Vs>
void canonicalize(Language<Vs...>& enode, Find&& find)
{
    visit(
        [&](auto&& v)
        {
            Luau::EqSat::canonicalize(v, find);
        },
        enode.v
    );
}

} // namespace Luau::EqSat
