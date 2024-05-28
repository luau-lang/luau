// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Id.h"
#include "Luau/UnionFind.h"

#include <optional>
#include <unordered_map>
#include <utility>
#include <vector>

namespace Luau::EqSat
{

/// Each e-class is a set of e-nodes representing equivalent terms from a given language,
/// and an e-node is a function symbol paired with a list of children e-classes.
template<typename L, typename D>
struct EClass final
{
    Id id;
    std::vector<L> nodes;
    D data;
    std::vector<std::pair<L, Id>> parents;
};

/// In Definition 2.1, an EGraph is composed with a tuple (U, M, H) where
/// - U: [`EGraph::unionfind`]
/// - M: [`EGraph::classes`]
/// - H: [`EGraph::hashcons`]
///
/// See <https://arxiv.org/pdf/2004.03082>.
template<typename L, typename N>
struct EGraph final
{
    // TODO: static_assert L <: Language
    // TODO: static_assert N <: Analysis<L>

    Id find(Id id) const
    {
        return unionfind.find(id);
    }

    template<typename T>
    std::optional<Id> lookup(T&& enode) const
    {
        // An e-node 𝑛 is canonical iff 𝑛 = canonicalize(𝑛), where
        // canonicalize(𝑓(𝑎1, 𝑎2, ...)) = 𝑓(find(𝑎1), find(𝑎2), ...).
        for (Id& id : enode->operands())
            id = find(id);

        if (auto it = hashcons.find(enode); it != hashcons.end())
            return it->second;

        return std::nullopt;
    }

    Id add(L enode)
    {
        if (auto id = lookup(enode))
            return *id;

        Id id{hashcons.size()};
        hashcons.insert_or_assign(enode, id);
        return id;
    }

private:
    /// A union-find data structure 𝑈 stores an equivalence relation over e-class ids.
    UnionFind unionfind;

    /// The e-class map 𝑀 maps e-class ids to e-classes. All equivalent e-class ids map to the same
    /// e-class, i.e., 𝑎 ≡id 𝑏 iff 𝑀[𝑎] is the same set as 𝑀[𝑏]. An e-class id 𝑎 is said to refer to the
    /// e-class 𝑀[find(𝑎)].
    std::unordered_map<Id, EClass<L, typename N::Data>> classes;

    /// The hashcons 𝐻 is a map from e-nodes to e-class ids.
    std::unordered_map<L, Id, typename L::Hash> hashcons;
};

} // namespace Luau::EqSat
