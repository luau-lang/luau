// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash.h"
#include "Luau/Id.h"
#include "Luau/UnionFind.h"

#include <utility>
#include <vector>

namespace Luau::EqSat
{

/// Each e-class is a set of e-nodes representing equivalent terms from a given language,
/// and an e-node is a function symbol paired with a list of children e-classes.
template<typename L, typename D>
struct EClass {
    Id id;
    std::vector<L> nodes;
    D data;
    std::vector<std::pair<L, Id>> parents;
};

/// In Definition 2.1, an EGraph is composed with a tuple (U, M, H) where
/// - U: [`EGraph::u`]
/// - M: [`EGraph::m`]
/// - H: [`EGraph::h`]
///
/// See <https://arxiv.org/pdf/2004.03082>.
template<typename L, typename N>
struct EGraph
{
    // TODO: static_assert L <: Language
    // TODO: static_assert N <: Analysis<L>

private:
    /// A union-find data structure 𝑈 stores an equivalence relation over e-class ids.
    UnionFind u;

    /// The e-class map 𝑀 maps e-class ids to e-classes. All equivalent e-class ids map to the same
    /// e-class, i.e., 𝑎 ≡id 𝑏 iff 𝑀[𝑎] is the same set as 𝑀[𝑏]. An e-class id 𝑎 is said to refer to the
    /// e-class 𝑀[find(𝑎)].
    DenseHashMap<Id, EClass<L, typename N::Data>> m;

    /// The hashcons 𝐻 is a map from e-nodes to e-node ids.
    ///
    /// Author note: the paper says this maps from e-nodes to e-class ids, but the
    /// corresponding map in the egg crate called `memo`, they map it to e-node ids?
    DenseHashMap<EClass<L, typename N::Data>, Id> h;
};

} // namespace Luau::EqSat
