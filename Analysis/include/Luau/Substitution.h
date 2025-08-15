// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeArena.h"
#include "Luau/TypeFwd.h"
#include "Luau/DenseHash.h"

// We provide an implementation of substitution on types,
// which recursively replaces types by other types.
// Examples include quantification (replacing free types by generics)
// and instantiation (replacing generic types by free ones).
//
// To implement a substitution, implement a subclass of `Substitution`
// and provide implementations of `isDirty` (which should be true for types that
// should be replaced) and `clean` which replaces any dirty types.
//
// struct MySubst : Substitution
// {
//     bool isDirty(TypeId ty) override { ... }
//     bool isDirty(TypePackId tp) override { ... }
//     TypeId clean(TypeId ty) override { ... }
//     TypePackId clean(TypePackId tp) override { ... }
//     bool ignoreChildren(TypeId ty) override { ... }
//     bool ignoreChildren(TypePackId tp) override { ... }
// };
//
// For example, `Instantiation` in `TypeInfer.cpp` uses this.

// The implementation of substitution tries not to copy types
// unnecessarily. It first finds all the types which can reach
// a dirty type, and either cleans them (if they are dirty)
// or clones them (if they are not). It then updates the children
// of the newly created types. When considering reachability,
// we do not consider the children of any type where ignoreChildren(ty) is true.

// There is a gotcha for cyclic types, which means we can't just use
// a straightforward DFS. For example:
//
// type T = { f : () -> T, g: () -> number, h: X }
//
// If X is dirty, and is being replaced by X' then the result should be:
//
// type T' = { f : () -> T', g: () -> number, h: X' }
//
// that is the type of `f` is replaced, but the type of `g` is not.
//
// For this reason, we first use Tarjan's algorithm to find strongly
// connected components. If any type in an SCC can reach a dirty type,
// them the whole SCC can. For instance, in the above example,
// `T`, and the type of `f` are in the same SCC, which is why `f` gets
// replaced.

namespace Luau
{

struct TxnLog;

enum class TarjanResult
{
    TooManyChildren,
    Ok
};

struct TarjanWorklistVertex
{
    int index;
    int currEdge;
    int lastEdge;

    TarjanWorklistVertex(int index, int currEdge, int lastEdge)
        : index(index)
        , currEdge(currEdge)
        , lastEdge(lastEdge)
    {
    }
};

struct TarjanNode
{
    TypeId ty;
    TypePackId tp;

    bool onStack;
    bool dirty;

    // Tarjan calculates the lowlink for each vertex,
    // which is the lowest ancestor index reachable from the vertex.
    int lowlink;

    TarjanNode(TypeId ty, TypePackId tp, bool onStack, bool dirty, int lowlink)
        : ty(ty)
        , tp(tp)
        , onStack(onStack)
        , dirty(dirty)
        , lowlink(lowlink)
    {
    }
};

// Tarjan's algorithm for finding the SCCs in a cyclic structure.
// https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
struct Tarjan
{
    Tarjan();
    virtual ~Tarjan() = default;

    // Vertices (types and type packs) are indexed, using pre-order traversal.
    DenseHashMap<TypeId, int> typeToIndex{nullptr};
    DenseHashMap<TypePackId, int> packToIndex{nullptr};

    std::vector<TarjanNode> nodes;

    // Tarjan keeps a stack of vertices where we're still in the process
    // of finding their SCC.
    std::vector<int> stack;

    int childCount = 0;
    int childLimit = 0;

    // This should never be null; ensure you initialize it before calling
    // substitution methods.
    const TxnLog* log = nullptr;

    std::vector<TypeId> edgesTy;
    std::vector<TypePackId> edgesTp;
    std::vector<TarjanWorklistVertex> worklist;

    // This is hot code, so we optimize recursion to a stack.
    TarjanResult loop();

    // Find or create the index for a vertex.
    // Return a boolean which is `true` if it's a freshly created index.
    std::pair<int, bool> indexify(TypeId ty);
    std::pair<int, bool> indexify(TypePackId tp);

    // Recursively visit all the children of a vertex
    void visitChildren(TypeId ty, int index);
    void visitChildren(TypePackId tp, int index);

    void visitChild(TypeId ty);
    void visitChild(TypePackId tp);

    template<typename Ty>
    void visitChild(std::optional<Ty> ty)
    {
        if (ty)
            visitChild(*ty);
    }

    // Visit the root vertex.
    TarjanResult visitRoot(TypeId ty);
    TarjanResult visitRoot(TypePackId tp);

    // Used to reuse the object for a new operation
    void clearTarjan(const TxnLog* log);

    // Get/set the dirty bit for an index (grows the vector if needed)
    bool getDirty(int index);
    void setDirty(int index, bool d);

    // Find all the dirty vertices reachable from `t`.
    TarjanResult findDirty(TypeId t);
    TarjanResult findDirty(TypePackId t);

    // We find dirty vertices using Tarjan
    void visitEdge(int index, int parentIndex);
    void visitSCC(int index);

    // Each subclass can decide to ignore some nodes.
    virtual bool ignoreChildren(TypeId ty);
    virtual bool ignoreChildren(TypePackId ty);

    // Some subclasses might ignore children visit, but not other actions like replacing the children
    virtual bool ignoreChildrenVisit(TypeId ty);
    virtual bool ignoreChildrenVisit(TypePackId ty);

    // Subclasses should say which vertices are dirty,
    // and what to do with dirty vertices.
    virtual bool isDirty(TypeId ty) = 0;
    virtual bool isDirty(TypePackId tp) = 0;
    virtual void foundDirty(TypeId ty) = 0;
    virtual void foundDirty(TypePackId tp) = 0;
};

// And finally substitution, which finds all the reachable dirty vertices
// and replaces them with clean ones.
struct Substitution : Tarjan
{
protected:
    explicit Substitution(TypeArena* arena);
    Substitution(const TxnLog* log_, TypeArena* arena);

    /*
     * By default, Substitution assumes that the types produced by clean() are
     * freshly allocated types that are safe to mutate.
     *
     * If your clean() implementation produces a type that is not safe to
     * mutate, you must call dontTraverseInto on this type (or type pack) to
     * prevent Substitution from attempting to perform substitutions within the
     * cleaned type.
     *
     * See the test weird_cyclic_instantiation for an example.
     */
    void dontTraverseInto(TypeId ty);
    void dontTraverseInto(TypePackId tp);

public:
    TypeArena* arena;
    DenseHashMap<TypeId, TypeId> newTypes{nullptr};
    DenseHashMap<TypePackId, TypePackId> newPacks{nullptr};
    DenseHashSet<TypeId> replacedTypes{nullptr};
    DenseHashSet<TypePackId> replacedTypePacks{nullptr};

    DenseHashSet<TypeId> noTraverseTypes{nullptr};
    DenseHashSet<TypePackId> noTraverseTypePacks{nullptr};

    std::optional<TypeId> substitute(TypeId ty);
    std::optional<TypePackId> substitute(TypePackId tp);

    void resetState(const TxnLog* log, TypeArena* arena);

    TypeId replace(TypeId ty);
    TypePackId replace(TypePackId tp);

    void replaceChildren(TypeId ty);
    void replaceChildren(TypePackId tp);

    TypeId clone(TypeId ty);
    TypePackId clone(TypePackId tp);

    // Substitutions use Tarjan to find dirty nodes and replace them
    void foundDirty(TypeId ty) override;
    void foundDirty(TypePackId tp) override;

    // Implementing subclasses define how to clean a dirty type.
    virtual TypeId clean(TypeId ty) = 0;
    virtual TypePackId clean(TypePackId tp) = 0;

protected:
    // Helper functions to create new types (used by subclasses)
    template<typename T>
    TypeId addType(T tv)
    {
        return arena->addType(std::move(tv));
    }

    template<typename T>
    TypePackId addTypePack(T tp)
    {
        return arena->addTypePack(TypePackVar{std::move(tp)});
    }

private:
    template<typename Ty>
    std::optional<Ty> replace(std::optional<Ty> ty);
};

} // namespace Luau
