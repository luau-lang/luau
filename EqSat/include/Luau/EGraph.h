// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/Id.h"
#include "Luau/Language.h"
#include "Luau/UnionFind.h"

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

    Analysis() = default;

    Analysis(N a)
        : analysis(std::move(a))
    {
    }

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

template<typename L>
struct Node
{
    L node;
    bool boring = false;

    struct Hash
    {
        size_t operator()(const Node& node) const
        {
            return typename L::Hash{}(node.node);
        }
    };
};

template<typename L>
struct NodeIterator
{
private:
    using iterator = std::vector<Node<L>>;
    iterator iter;

public:
    L& operator*()
    {
        return iter->node;
    }

    const L& operator*() const
    {
        return iter->node;
    }

    iterator& operator++()
    {
        ++iter;
        return *this;
    }

    iterator operator++(int)
    {
        iterator copy = *this;
        ++*this;
        return copy;
    }

    bool operator==(const iterator& rhs) const
    {
        return iter == rhs.iter;
    }

    bool operator!=(const iterator& rhs) const
    {
        return iter != rhs.iter;
    }
};

/// Each e-class is a set of e-nodes representing equivalent terms from a given language,
/// and an e-node is a function symbol paired with a list of children e-classes.
template<typename L, typename D>
struct EClass final
{
    Id id;
    std::vector<Node<L>> nodes;
    D data;
    std::vector<std::pair<L, Id>> parents;
};

/// See <https://arxiv.org/pdf/2004.03082>.
template<typename L, typename N>
struct EGraph final
{
    using EClassT = EClass<L, typename N::Data>;

    EGraph() = default;

    explicit EGraph(N analysis)
        : analysis(std::move(analysis))
    {
    }

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

    // Returns true if the two IDs were not previously merged.
    bool merge(Id id1, Id id2)
    {
        id1 = find(id1);
        id2 = find(id2);
        if (id1 == id2)
            return false;

        const Id mergedId = unionfind.merge(id1, id2);

        // Ensure that id1 is the Id that we keep, and id2 is the id that we drop.
        if (mergedId == id2)
            std::swap(id1, id2);

        EClassT& eclass1 = get(id1);
        EClassT eclass2 = std::move(get(id2));
        classes.erase(id2);

        eclass1.nodes.insert(eclass1.nodes.end(), eclass2.nodes.begin(), eclass2.nodes.end());
        eclass1.parents.insert(eclass1.parents.end(), eclass2.parents.begin(), eclass2.parents.end());

        std::sort(
            eclass1.nodes.begin(),
            eclass1.nodes.end(),
            [](const Node<L>& left, const Node<L>& right)
            {
                return left.node.index() < right.node.index();
            }
        );

        worklist.reserve(worklist.size() + eclass1.parents.size());
        for (const auto& [eclass, id] : eclass1.parents)
            worklist.push_back(id);

        analysis.join(eclass1.data, eclass2.data);

        return true;
    }

    void rebuild()
    {
        std::unordered_set<Id> seen;

        while (!worklist.empty())
        {
            Id id = worklist.back();
            worklist.pop_back();

            const bool isFresh = seen.insert(id).second;
            if (!isFresh)
                continue;

            repair(find(id));
        }
    }

    size_t size() const
    {
        return classes.size();
    }

    EClassT& operator[](Id id)
    {
        return get(find(id));
    }

    const EClassT& operator[](Id id) const
    {
        return const_cast<EGraph*>(this)->get(find(id));
    }

    const std::unordered_map<Id, EClassT>& getAllClasses() const
    {
        return classes;
    }

    void markBoring(Id id, size_t index)
    {
        get(id).nodes[index].boring = true;
    }

private:
    Analysis<L, N> analysis;

    /// A union-find data structure 𝑈 stores an equivalence relation over e-class ids.
    UnionFind unionfind;

    /// The e-class map 𝑀 maps e-class ids to e-classes. All equivalent e-class ids map to the same
    /// e-class, i.e., 𝑎 ≡id 𝑏 iff 𝑀[𝑎] is the same set as 𝑀[𝑏]. An e-class id 𝑎 is said to refer to the
    /// e-class 𝑀[find(𝑎)].
    std::unordered_map<Id, EClassT> classes;

    /// The hashcons 𝐻 is a map from e-nodes to e-class ids.
    std::unordered_map<L, Id, typename L::Hash> hashcons;

    std::vector<Id> worklist;

private:
    void canonicalize(L& enode)
    {
        // An e-node 𝑛 is canonical iff 𝑛 = canonicalize(𝑛), where
        // canonicalize(𝑓(𝑎1, 𝑎2, ...)) = 𝑓(find(𝑎1), find(𝑎2), ...).
        Luau::EqSat::canonicalize(
            enode,
            [&](Id id)
            {
                return find(id);
            }
        );
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

        classes.insert_or_assign(
            id,
            EClassT{
                id,
                {Node<L>{enode, false}},
                analysis.make(*this, enode),
                {},
            }
        );

        for (Id operand : enode.operands())
            get(operand).parents.push_back({enode, id});

        worklist.emplace_back(id);
        hashcons.insert_or_assign(enode, id);

        return id;
    }

    // Looks up for an eclass from a given non-canonicalized `id`.
    // For a canonicalized eclass, use `get(find(id))` or `egraph[id]`.
    EClassT& get(Id id)
    {
        LUAU_ASSERT(classes.count(id));
        return classes.at(id);
    }

    void repair(Id id)
    {
        // In the egg paper, the `repair` function makes use of two loops over the `eclass.parents`
        // by first erasing the old enode entry, and adding back the canonicalized enode with the canonical id.
        // And then in another loop that follows, deduplicate it.
        //
        // Here, we unify the two loops. I think it's equivalent?

        // After canonicalizing the enodes, the eclass may contain multiple enodes that are equivalent.
        std::unordered_map<L, Id, typename L::Hash> newParents;

        // The eclass can be deallocated if it is merged into another eclass, so
        // we take what we need from it and avoid retaining a pointer.
        std::vector<std::pair<L, Id>> parents = get(id).parents;
        for (auto& pair : parents)
        {
            L& parentNode = pair.first;
            Id parentId = pair.second;

            // By removing the old enode from the hashcons map, we will always find our new canonicalized eclass id.
            hashcons.erase(parentNode);
            canonicalize(parentNode);
            hashcons.insert_or_assign(parentNode, find(parentId));

            if (auto it = newParents.find(parentNode); it != newParents.end())
                merge(parentId, it->second);

            newParents.insert_or_assign(parentNode, find(parentId));
        }

        // We reacquire the pointer because the prior loop potentially merges
        // the eclass into another, which might move it around in memory.
        EClassT* eclass = &get(find(id));

        eclass->parents.clear();

        for (const auto& [node, id] : newParents)
            eclass->parents.emplace_back(std::move(node), std::move(id));

        std::unordered_map<L, bool, typename L::Hash> newNodes;
        for (Node<L> node : eclass->nodes)
        {
            canonicalize(node.node);

            bool& b = newNodes[std::move(node.node)];
            b = b || node.boring;
        }

        eclass->nodes.clear();

        while (!newNodes.empty())
        {
            auto n = newNodes.extract(newNodes.begin());
            eclass->nodes.push_back(Node<L>{n.key(), n.mapped()});
        }

        // FIXME: Extract into sortByTag()
        std::sort(
            eclass->nodes.begin(),
            eclass->nodes.end(),
            [](const Node<L>& left, const Node<L>& right)
            {
                return left.node.index() < right.node.index();
            }
        );
    }
};

} // namespace Luau::EqSat
