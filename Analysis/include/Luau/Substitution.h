// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Module.h"
#include "Luau/ModuleResolver.h"
#include "Luau/TypePack.h"
#include "Luau/TypeVar.h"
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

LUAU_FASTFLAG(DebugLuauTrackOwningArena)

namespace Luau
{

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
};

// Tarjan's algorithm for finding the SCCs in a cyclic structure.
// https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
struct Tarjan
{
    // Vertices (types and type packs) are indexed, using pre-order traversal.
    DenseHashMap<TypeId, int> typeToIndex{nullptr};
    DenseHashMap<TypePackId, int> packToIndex{nullptr};
    std::vector<TypeId> indexToType;
    std::vector<TypePackId> indexToPack;

    // Tarjan keeps a stack of vertices where we're still in the process
    // of finding their SCC.
    std::vector<int> stack;
    std::vector<bool> onStack;

    // Tarjan calculates the lowlink for each vertex,
    // which is the lowest ancestor index reachable from the vertex.
    std::vector<int> lowlink;

    int childCount = 0;

    std::vector<TypeId> edgesTy;
    std::vector<TypePackId> edgesTp;
    std::vector<TarjanWorklistVertex> worklist;
    // This is hot code, so we optimize recursion to a stack.
    TarjanResult loop();

    // Clear the state
    void clear();

    // Find or create the index for a vertex.
    // Return a boolean which is `true` if it's a freshly created index.
    std::pair<int, bool> indexify(TypeId ty);
    std::pair<int, bool> indexify(TypePackId tp);

    // Recursively visit all the children of a vertex
    void visitChildren(TypeId ty, int index);
    void visitChildren(TypePackId tp, int index);

    void visitChild(TypeId ty);
    void visitChild(TypePackId ty);

    // Visit the root vertex.
    TarjanResult visitRoot(TypeId ty);
    TarjanResult visitRoot(TypePackId ty);

    // Each subclass gets called back once for each edge,
    // and once for each SCC.
    virtual void visitEdge(int index, int parentIndex) {}
    virtual void visitSCC(int index) {}

    // Each subclass can decide to ignore some nodes.
    virtual bool ignoreChildren(TypeId ty)
    {
        return false;
    }
    virtual bool ignoreChildren(TypePackId ty)
    {
        return false;
    }
};

// We use Tarjan to calculate dirty bits. We set `dirty[i]` true
// if the vertex with index `i` can reach a dirty vertex.
struct FindDirty : Tarjan
{
    std::vector<bool> dirty;

    // Get/set the dirty bit for an index (grows the vector if needed)
    bool getDirty(int index);
    void setDirty(int index, bool d);

    // Find all the dirty vertices reachable from `t`.
    TarjanResult findDirty(TypeId t);
    TarjanResult findDirty(TypePackId t);

    // We find dirty vertices using Tarjan
    void visitEdge(int index, int parentIndex) override;
    void visitSCC(int index) override;

    // Subclasses should say which vertices are dirty,
    // and what to do with dirty vertices.
    virtual bool isDirty(TypeId ty) = 0;
    virtual bool isDirty(TypePackId tp) = 0;
    virtual void foundDirty(TypeId ty) = 0;
    virtual void foundDirty(TypePackId tp) = 0;
};

// And finally substitution, which finds all the reachable dirty vertices
// and replaces them with clean ones.
struct Substitution : FindDirty
{
    ModulePtr currentModule;
    DenseHashMap<TypeId, TypeId> newTypes{nullptr};
    DenseHashMap<TypePackId, TypePackId> newPacks{nullptr};

    std::optional<TypeId> substitute(TypeId ty);
    std::optional<TypePackId> substitute(TypePackId tp);

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

    // Helper functions to create new types (used by subclasses)
    template<typename T>
    TypeId addType(const T& tv)
    {
        TypeId allocated = currentModule->internalTypes.typeVars.allocate(tv);
        if (FFlag::DebugLuauTrackOwningArena)
            asMutable(allocated)->owningArena = &currentModule->internalTypes;

        return allocated;
    }
    template<typename T>
    TypePackId addTypePack(const T& tp)
    {
        TypePackId allocated = currentModule->internalTypes.typePacks.allocate(tp);
        if (FFlag::DebugLuauTrackOwningArena)
            asMutable(allocated)->owningArena = &currentModule->internalTypes;

        return allocated;
    }
};

} // namespace Luau
