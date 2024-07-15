// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/Id.h"
#include "Luau/Language.h"
#include "Luau/UnionFind.h"
#include "Luau/VecDeque.h"

#include <optional>
#include <unordered_map>
#include <vector>

namespace Luau::EqSat
{

template<typename L, typename N>
struct EGraph;

template<typename L, typename N>
struct Analysis final
{
    N analysis;

    using D = typename N::Data;

    template<typename T>
    static D fnMake(const N& analysis, const EGraph<L, N>& egraph, const L& enode)
    {
        return analysis.make(egraph, *enode.template get<T>());
    }

    template<typename... Ts>
    D make(const EGraph<L, N>& egraph, const Language<Ts...>& enode) const
    {
        using FnMake = D (*)(const N&, const EGraph<L, N>&, const L&);
        static constexpr FnMake tableMake[sizeof...(Ts)] = {&fnMake<Ts>...};

        return tableMake[enode.index()](analysis, egraph, enode);
    }

    void join(D& a, const D& b) const
    {
        return analysis.join(a, b);
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
struct EGraph final
{
    Id find(Id id) const
    {
        return unionfind.find(id);
    }

    std::optional<Id> lookup(const L& enode) const
    {
        LUAU_ASSERT(isCanonical(enode));

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
        return id;
    }

    void merge(Id id1, Id id2)
    {
        id1 = find(id1);
        id2 = find(id2);
        if (id1 == id2)
            return;

        unionfind.merge(id1, id2);

        EClass<L, typename N::Data>& eclass1 = get(id1);
        EClass<L, typename N::Data> eclass2 = std::move(get(id2));
        classes.erase(id2);

        worklist.reserve(worklist.size() + eclass2.parents.size());
        for (auto [enode, id] : eclass2.parents)
            worklist.push_back({std::move(enode), id});

        analysis.join(eclass1.data, eclass2.data);
    }

    void rebuild()
    {
        while (!worklist.empty())
        {
            auto [enode, id] = worklist.back();
            worklist.pop_back();
            repair(get(find(id)));
        }
    }

    size_t size() const
    {
        return classes.size();
    }

    EClass<L, typename N::Data>& operator[](Id id)
    {
        return get(find(id));
    }

    const EClass<L, typename N::Data>& operator[](Id id) const
    {
        return const_cast<EGraph*>(this)->get(find(id));
    }

private:
    Analysis<L, N> analysis;

    /// A union-find data structure 𝑈 stores an equivalence relation over e-class ids.
    UnionFind unionfind;

    /// The e-class map 𝑀 maps e-class ids to e-classes. All equivalent e-class ids map to the same
    /// e-class, i.e., 𝑎 ≡id 𝑏 iff 𝑀[𝑎] is the same set as 𝑀[𝑏]. An e-class id 𝑎 is said to refer to the
    /// e-class 𝑀[find(𝑎)].
    std::unordered_map<Id, EClass<L, typename N::Data>> classes;

    /// The hashcons 𝐻 is a map from e-nodes to e-class ids.
    std::unordered_map<L, Id, typename L::Hash> hashcons;

    VecDeque<std::pair<L, Id>> worklist;

private:
    void canonicalize(L& enode)
    {
        // An e-node 𝑛 is canonical iff 𝑛 = canonicalize(𝑛), where
        // canonicalize(𝑓(𝑎1, 𝑎2, ...)) = 𝑓(find(𝑎1), find(𝑎2), ...).
        for (Id& id : enode.operands())
            id = find(id);
    }

    bool isCanonical(const L& enode) const
    {
        bool canonical = true;
        for (Id id : enode.operands())
            canonical &= (id == find(id));
        return canonical;
    }

    Id makeEClass(const L& enode)
    {
        LUAU_ASSERT(isCanonical(enode));

        Id id = unionfind.makeSet();

        classes.insert_or_assign(id, EClass<L, typename N::Data>{
            id,
            {enode},
            analysis.make(*this, enode),
            {},
        });

        for (Id operand : enode.operands())
            get(operand).parents.push_back({enode, id});

        worklist.push_back({enode, id});
        hashcons.insert_or_assign(enode, id);

        return id;
    }

    // Looks up for an eclass from a given non-canonicalized `id`.
    // For a canonicalized eclass, use `get(find(id))` or `egraph[id]`.
    EClass<L, typename N::Data>& get(Id id)
    {
        return classes.at(id);
    }

    void repair(EClass<L, typename N::Data>& eclass)
    {
        // In the egg paper, the `repair` function makes use of two loops over the `eclass.parents`
        // by first erasing the old enode entry, and adding back the canonicalized enode with the canonical id.
        // And then in another loop that follows, deduplicate it.
        //
        // Here, we unify the two loops. I think it's equivalent?

        // After canonicalizing the enodes, the eclass may contain multiple enodes that are equivalent.
        std::unordered_map<L, Id, typename L::Hash> map;
        for (auto& [enode, id] : eclass.parents)
        {
            // By removing the old enode from the hashcons map, we will always find our new canonicalized eclass id.
            hashcons.erase(enode);
            canonicalize(enode);
            hashcons.insert_or_assign(enode, find(id));

            if (auto it = map.find(enode); it != map.end())
                merge(id, it->second);

            map.insert_or_assign(enode, find(id));
        }

        eclass.parents.clear();
        for (auto it = map.begin(); it != map.end();)
        {
            auto node = map.extract(it++);
            eclass.parents.emplace_back(std::move(node.key()), node.mapped());
        }
    }
};

} // namespace Luau::EqSat
