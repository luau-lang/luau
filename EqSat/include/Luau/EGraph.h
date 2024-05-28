// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/Id.h"
#include "Luau/UnionFind.h"

#include <optional>
#include <unordered_map>
#include <utility>
#include <vector>

namespace Luau::EqSat
{

template<typename L, typename N>
class EGraph;

template<typename L, typename N>
struct Analysis final
{
    N analysis;

    typename N::Data make(const EGraph<L, N>& egraph, const L& enode) const
    {
        return analysis.make(egraph, enode);
    }
};

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

/// See <https://arxiv.org/pdf/2004.03082>.
template<typename L, typename N>
class EGraph final
{
    Analysis<L, N> analysis;

    /// A union-find data structure 𝑈 stores an equivalence relation over e-class ids.
    UnionFind unionfind;

    /// The e-class map 𝑀 maps e-class ids to e-classes. All equivalent e-class ids map to the same
    /// e-class, i.e., 𝑎 ≡id 𝑏 iff 𝑀[𝑎] is the same set as 𝑀[𝑏]. An e-class id 𝑎 is said to refer to the
    /// e-class 𝑀[find(𝑎)].
    std::unordered_map<Id, EClass<L, typename N::Data>> classes;

    /// The hashcons 𝐻 is a map from e-nodes to e-class ids.
    std::unordered_map<L, Id, typename L::Hash> hashcons;

private:
    template<typename T>
    void canonicalize(T&& enode)
    {
        // An e-node 𝑛 is canonical iff 𝑛 = canonicalize(𝑛), where
        // canonicalize(𝑓(𝑎1, 𝑎2, ...)) = 𝑓(find(𝑎1), find(𝑎2), ...).
        for (Id& id : enode.operands())
            id = find(id);
    }

    Id makeEClass(const L& enode)
    {
        Id id = unionfind.makeSet();
        classes.insert_or_assign(id, EClass<L, typename N::Data>{
            id,
            {enode},
            analysis.make(*this, enode),
            {},
        });
        return id;
    }

public:
    Id find(Id id) const
    {
        return unionfind.find(id);
    }

    template<typename T>
    std::optional<Id> lookup(T&& enode) const
    {
        if (auto it = hashcons.find(enode); it != hashcons.end())
            return it->second;

        return std::nullopt;
    }

    Id add(L enode)
    {
        canonicalize(enode);

        if (auto id = lookup(enode))
            return *id;

        Id id = makeEClass(enode);
        for (Id operand : enode.operands())
            (*this)[operand].parents.push_back({enode, id});

        hashcons.insert_or_assign(enode, id);
        // TODO clean = false
        return id;
    }

    EClass<L, typename N::Data>& operator[](Id id)
    {
        auto it = classes.find(find(id));
        LUAU_ASSERT(it != classes.end());
        return it->second;
    }
};

} // namespace Luau::EqSat
