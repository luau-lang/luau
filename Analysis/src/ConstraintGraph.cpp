// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/ConstraintGraph.h"
#include "Luau/TypeUtils.h"

#include <iostream>
#include <ostream>

LUAU_FASTFLAG(DebugLuauLogSolver)
LUAU_FASTFLAG(LuauRemovePrimitiveTypeConstraintAndSubtypingUnifier)

namespace Luau
{

namespace
{
[[maybe_unused]] bool canMutate(TypeId ty, NotNull<const Constraint> constraint)
{
    if (auto blocked = get<BlockedType>(ty))
    {
        const Constraint* owner = blocked->getOwner();
        LUAU_ASSERT(owner);
        return owner == constraint;
    }

    return true;
}

[[maybe_unused]] static bool canMutate(TypePackId tp, NotNull<const Constraint> constraint)
{
    if (auto blocked = get<BlockedTypePack>(tp))
    {
        Constraint* owner = blocked->owner;
        LUAU_ASSERT(owner);
        return owner == constraint;
    }

    return true;
}

} // namespace

size_t HashBlockedConstraintId::operator()(const BlockedConstraintId& bci) const
{
    size_t result = 0;

    if (const TypeId* ty = get_if<TypeId>(&bci))
        result = std::hash<TypeId>()(*ty);
    else if (const TypePackId* tp = get_if<TypePackId>(&bci))
        result = std::hash<TypePackId>()(*tp);
    else if (Constraint const* const* c = get_if<const Constraint*>(&bci))
        result = std::hash<const Constraint*>()(*c);
    else
        LUAU_ASSERT(!"Should be unreachable");

    return result;
}

ConstraintList::Iterator::Iterator(NotNull<ConstraintList> cl, size_t index)
    : cl{cl}
    , index{index}
{
    advanceUntilPresentOrEnd();
}

ConstraintList::Iterator& ConstraintList::Iterator::operator++()
{
    if (index < cl->order.size())
    {
        index++;
        advanceUntilPresentOrEnd();
    }
    return *this;
}

bool ConstraintList::Iterator::operator==(const Iterator& rhs) const
{
    return cl == rhs.cl && index == rhs.index;
}

bool ConstraintList::Iterator::operator!=(const Iterator& rhs) const
{
    return !(*this == rhs);
}

ConstraintList::Iterator::value_type ConstraintList::Iterator::operator*()
{
    return cl->order[index];
}

void ConstraintList::Iterator::advanceUntilPresentOrEnd()
{
    while (index < cl->order.size() && !cl->contains(cl->order[index]))
        index++;
    return;
}

ConstraintGraph::ConstraintGraph(NotNull<BuiltinTypes> builtinTypes)
    : builtinTypes(builtinTypes)
{
}

bool ConstraintList::contains(ConstraintVertex vertex) const
{
    if (auto entry = present.find(vertex))
        return *entry;
    return false;
}

void ConstraintList::insert(ConstraintVertex vertex)
{
    auto [entry, fresh] = present.try_insert(vertex, true);
    if (fresh)
    {
        order.emplace_back(vertex);
        entries++;
    }
    else if (!entry)
    {
        entry = true;
        entries++;
    }
    // If the entry was *not* fresh and its value was already true, then do
    // nothing: the set state has not changed.
}

void ConstraintList::remove(ConstraintVertex vertex)
{
    if (auto entry = present.find(vertex))
    {
        // If the entry is true then we also need to decrement the number of
        // entries in the constraint list.
        if (*entry)
            entries--;
        *entry = false;
    }
}

size_t ConstraintList::size() const
{
    return entries;
}

void ConstraintList::clear()
{
    order.clear();
    present.clear();
    entries = 0;
}

ConstraintList::Iterator ConstraintList::begin()
{
    return Iterator{NotNull{this}, 0};
}

ConstraintList::Iterator ConstraintList::end()
{
    return Iterator{NotNull{this}, order.size()};
}

bool ConstraintGraph::addDependencyOf(ConstraintVertex dependency, ConstraintVertex target)
{
    auto deps = findDependencyList(target);
    auto reverseDeps = findReverseDependencyList(dependency);

    if (deps->contains(dependency))
    {
        // If we are claiming this is not a fresh block, we _better_ be
        // tracking the reverse dependency as well!
        LUAU_ASSERT(reverseDeps->contains(target));
        return false;
    }

    deps->insert(dependency);
    reverseDeps->insert(target);
    return true;
}

bool ConstraintGraph::addDependencyOf(Constraint* dependency, Constraint* target)
{
    return addDependencyOf(static_cast<const Constraint*>(dependency), static_cast<const Constraint*>(target));
}

/**
 * Let's say we have nodes A, B, C, and D (where => means "depends on")
 *
 *  A, B, C => D
 *
 * As part of dispatching D, we need to mint E. This function sets us up such that:
 *
 *  A, B, C => E
 *
 */
void ConstraintGraph::inheritBlocks(ConstraintVertex existingVertex, ConstraintVertex newVertex)
{
    auto existingReverseDeps = findReverseDependencyList(existingVertex);
    auto newReverseDeps = findReverseDependencyList(newVertex);

    // For each reverse dependency of [existingVertex] ...
    for (auto existingRdep : *existingReverseDeps)
    {
        // ... add it as a reverse dependency of [newVertex]
        newReverseDeps->insert(existingRdep);
        /// ... and add [newVertex] as a dependency.
        auto newDeps = findDependencyList(existingRdep);
        newDeps->insert(newVertex);
    }
}

void ConstraintGraph::unblockTypeOrPack(TypeId vertex)
{
    repairTypeReferences(vertex);
    clearReverseDependenciesOf(follow(vertex));
}

void ConstraintGraph::unblockTypeOrPack(TypePackId vertex)
{
    repairTypeReferences(vertex);
    clearReverseDependenciesOf(follow(vertex));
}

ConstraintGraph::UnblockedTypes ConstraintGraph::unblockConstraint(NotNull<const Constraint> c)
{
    UnblockedTypes result;

    // The reverse dependencies of this constraint should contain all of the types
    // and type packs that this constraint may mutate, either as a free type or
    // as a blocked type.
    auto reverseDeps = findReverseDependencyList(c.get());
    for (auto rdep : *reverseDeps)
    {
        if (auto ty = rdep.get_if<TypeId>())
        {
            result.types.insert(*ty);
            auto deps = findDependencyList(*ty);
            deps->remove(c.get());
        }
        else if (auto tp = rdep.get_if<TypePackId>())
        {
            result.packs.insert(*tp);
            auto deps = findDependencyList(*tp);
            deps->remove(c.get());
        }
        else if (auto depCons = rdep.get_if<const Constraint*>())
        {
            auto deps = findDependencyList(*depCons);
            deps->remove(c.get());
            if (FFlag::DebugLuauLogSolver)
                printf("Unblocking count=%d\t%s\n", int(deps->size()), toString(**depCons, {/* exhaustive */ true}).c_str());
        }
        else
        {
            LUAU_ASSERT(!"Unknown constraint graph vertex.");
        }
    }

    /**
     * This whole song and dance is to repair the constraint graph after we
     * dispatch a constraint.
     *
     * We are assuming that, after a constraint has been dispatched, some
     * number of mutations have been made to the type graph. Importantly: if a
     * type has been mutated, then it was previously a reverse dependency of
     * [c]. If that is the case, then we can walk the reverse deps of [c] and
     * try to find bound types, shift their references over to their bounds,
     * and "repair" the dependency graph without having to track every single
     * [bind] call.
     *
     * This means that any [emplaceType] outside this file is subject to drift,
     * but it is safe as long as it occurs while the type being mutated is in
     * the reverse dependency set of the constraint being dispatched.
     *
     * We do this in two steps to ensure that [c] does not exist as a
     * dependency of *any* type while repairing references. An alternative
     * implementation would be to pass [c] to [repairTypeReferences] and know
     * *not* to transfer it as a dependency.
     */

    for (TypeId type : result.types)
        repairTypeReferences(type);

    for (TypePackId typePack : result.packs)
        repairTypeReferences(typePack);

    return result;
}

bool ConstraintGraph::hasUnsolvedDependencies(ConstraintVertex vertex)
{
    auto deps = findDependencyList(vertex);
    if (!FFlag::LuauRemovePrimitiveTypeConstraintAndSubtypingUnifier)
    {
        if (auto c = vertex.get_if<const Constraint*>())
        {
            if (auto ptc = (*c)->c.get_if<DEPRECATED_PrimitiveTypeConstraint>())
                return deps->size() > 1;
        }
    }
    return deps->size() > 0;
}

bool ConstraintGraph::DEPRECATED_hasStrictlyMoreThanOneDependency(ConstraintVertex vertex)
{
    LUAU_ASSERT(!FFlag::LuauRemovePrimitiveTypeConstraintAndSubtypingUnifier);
    auto deps = findDependencyList(vertex);
    return deps->size() > 1;
}

/**
 * For every vertex V and type T, we want to claim that T depends on V:
 * - Add a forward edge in [dependencies] from T to V
 * - Add a backwards edge in [reverseDependencies] from V to T
 * Additionally, the original vertex [originalVertex] should be removed
 * from the reverse dependency list if given.
 */
void ConstraintGraph::copyDependenciesToReachableTypes(
    std::optional<ConstraintVertex> originalVertex,
    NotNull<ConstraintList> sourceDependencies,
    TypeIds mutatedTypes,
    TypePackIds mutatedTypePacks
)
{
    for (const auto& vertex : *sourceDependencies)
    {
        // NOTE: Technically we could express this function solely in terms
        // of [addDependencyOf], but we save some cycles by fetching the
        // reverse dependency list once.
        auto vertexReverseDeps = findReverseDependencyList(vertex);

        if (originalVertex)
            vertexReverseDeps->remove(*originalVertex);

        for (auto subTarget : mutatedTypes)
        {
            auto tyDeps = findDependencyList(subTarget);
            // Add this vertex to the list of dependencies for this type.
            tyDeps->insert(vertex);
            // ... and then add the same backwards edge.
            vertexReverseDeps->insert(subTarget);
        }

        for (auto subPackTarget : mutatedTypePacks)
        {
            auto tpDeps = findDependencyList(subPackTarget);
            // Add this vertex to the list of dependencies for this type.
            tpDeps->insert(vertex);
            // ... and then add the same backwards edge.
            vertexReverseDeps->insert(subPackTarget);
        }
    }
}

void ConstraintGraph::clearReverseDependenciesOf(ConstraintVertex vertex)
{
    LUAU_ASSERT(vertex.get_if<const Constraint*>() == nullptr);

    auto revDeps = findReverseDependencyList(vertex);

    // For all of the reverse dependencies of vertex (vertices that depend on vertex) ...
    for (auto rdep : *revDeps)
    {
        // Remove vertex from the list of dependencies.
        // TODO CLI-205496: We should assert that deps contains `vertex`
        auto deps = findDependencyList(rdep);
        deps->remove(vertex);
    }

    // Then clear this set.
    revDeps->clear();
}

template<typename T>
void ConstraintGraph::shiftReferences(T source, T target)
{
    static_assert(std::is_same_v<T, TypeId> || std::is_same_v<T, TypePackId>, "Shift references can only be used with types or type packs.");
    if (source == target)
        return;

    auto sourceDependencies = findDependencyList(source);

    TypeIds mutatedTypes;
    TypePackIds mutatedTypePacks;
    ReferenceCountInitializer rci{NotNull{&mutatedTypes}, NotNull{&mutatedTypePacks}};
    rci.traverse(target);
    copyDependenciesToReachableTypes(source, sourceDependencies, std::move(mutatedTypes), std::move(mutatedTypePacks));

    // Types in the constraint graph are always dynamically discovered, so
    // when we shift a reference over, we'll remove it from the dependencies
    // of our reverse dependencies and then delete the edge from the graph.
    clearReverseDependenciesOf(source);
}

template void ConstraintGraph::shiftReferences(TypePackId source, TypePackId target);
template void ConstraintGraph::shiftReferences(TypeId source, TypeId target);

template<typename T>
void ConstraintGraph::repairTypeReferences(T ty)
{
    static_assert(std::is_same_v<T, TypeId> || std::is_same_v<T, TypePackId>, "Repair type references can only be used with types or type packs.");

    T root = follow(ty);

    // This is a strong guard against a self bound cylic type, but we
    // hopefully threw an exception above if this were the case.
    DenseHashSet<T> seen{nullptr};
    seen.insert(root);

    while (!seen.contains(ty))
    {
        seen.insert(ty);
        // This ensures:
        //  1. Any constraint that may mutate `vertex` will now signal that it
        //     mutates `root`
        //  2. Any constraint waiting on `vertex` will be unblocked.
        shiftReferences(ty, root);
        if constexpr (std::is_same_v<T, TypeId>)
        {
            if (auto bt = get<BoundType>(ty))
                ty = bt->boundTo;
        }
        else if constexpr (std::is_same_v<T, TypePackId>)
        {
            if (auto bt = get<BoundTypePack>(ty))
                ty = bt->boundTo;
        }
    }
}

template void ConstraintGraph::repairTypeReferences(TypeId ty);
template void ConstraintGraph::repairTypeReferences(TypePackId ty);

template<typename T>
void ConstraintGraph::copyDependenciesOf(T source, T target)
{
    static_assert(std::is_same_v<T, TypeId> || std::is_same_v<T, TypePackId>, "Copying dependencies can only be used with types or type packs.");
    auto sourceDependencies = findDependencyList(source);
    TypeIds mutatedTypes;
    TypePackIds mutatedTypePacks;
    ReferenceCountInitializer rci{NotNull{&mutatedTypes}, NotNull{&mutatedTypePacks}};
    rci.traverse(target);
    // We do not want to _delete_ the original vertex, so we pass nullopt here.
    copyDependenciesToReachableTypes(std::nullopt, sourceDependencies, std::move(mutatedTypes), std::move(mutatedTypePacks));
}

template void ConstraintGraph::copyDependenciesOf(TypeId source, TypeId target);
template void ConstraintGraph::copyDependenciesOf(TypePackId source, TypePackId target);

NotNull<ConstraintList> ConstraintGraph::findDependencyList(ConstraintVertex vertex)
{
    if (auto dep = dependencies.find(vertex))
        return NotNull{*dep};

    auto newlist = NotNull{constraintLists.emplace_back(new ConstraintList()).get()};

    auto [it, fresh] = dependencies.try_insert(vertex, newlist.get());
    LUAU_ASSERT(fresh);
    return NotNull{newlist};
}

NotNull<ConstraintList> ConstraintGraph::findReverseDependencyList(ConstraintVertex vertex)
{
    if (auto rdep = reverseDependencies.find(vertex))
        return NotNull{*rdep};

    auto newlist = NotNull{constraintLists.emplace_back(new ConstraintList()).get()};

    auto [it, fresh] = reverseDependencies.try_insert(vertex, newlist.get());
    LUAU_ASSERT(fresh);
    return NotNull{newlist};
}

namespace
{
std::string toString(ConstraintVertex vertex)
{
    return Luau::visit(
        overloaded{
            [&](TypeId ty)
            {
                return "Type " + toString(ty, {/* exhaustive */ true});
            },
            [&](TypePackId tp)
            {
                return "Type pack " + toString(tp, {/* exhaustive */ true});
            },
            [&](const Constraint* c)
            {
                return "Cons " + toString(*c, {/* exhaustive */ true});
            }
        },
        vertex
    );
}
} // namespace


std::string dump(ConstraintVertex vertex)
{
    auto out = toString(vertex);
    printf("%s\n", out.c_str());
    return out;
}

void dotEscape(std::ostream& os, const std::string& s)
{
    os << "\"";
    for (char c : s)
    {
        switch (c)
        {
        case '"':
            os << "\\\"";
            break;
        case '\\':
            os << "\\\\";
            break;
        case '\n':
            os << "\\n";
            break;
        case '<':
            os << "\\<";
            break;
        case '>':
            os << "\\>";
            break;
        case '{':
            os << "\\{";
            break;
        case '}':
            os << "\\}";
            break;
        case '|':
            os << "\\|";
            break;
        default:
            os << c;
            break;
        }
    }
    os << "\"";
}

void ConstraintGraph::dump()
{
    for (auto [v, deps] : dependencies)
    {
        auto vstr = toString(v);
        for (auto d : *deps)
        {
            dotEscape(std::cout, vstr);
            std::cout << " -> ";
            dotEscape(std::cout, toString(d));
            std::cout << std::endl;
        }
    }
}

void ConstraintGraph::dumpBlocked(NotNull<const Constraint> c, ToStringOptions& opts)
{
    printf("Blocked on:\n");
    auto deps = findDependencyList(c.get());
    for (auto dep : *deps)
    {
        Luau::visit(
            overloaded{
                [&](TypeId ty)
                {
                    printf("\tType %s\n", toString(ty, opts).c_str());
                },
                [&](TypePackId tp)
                {
                    printf("\tPack %s\n", toString(tp, opts).c_str());
                },
                [&](const Constraint* c)
                {
                    printf("\tCons %s\n", toString(*c, opts).c_str());
                }
            },
            dep
        );
    }
}

void ConstraintGraph::dumpWith(const std::vector<NotNull<const Constraint>>& unsolvedConstraints, ToStringOptions& opts)
{
    // TODO: It might be nice to *also* dump the types here.
    printf("constraints:\n");
    for (NotNull<const Constraint> c : unsolvedConstraints)
    {
        auto deps = findDependencyList(c.get());
        printf("\t%zu\t%s\n", deps->size(), toString(*c, opts).c_str());

        for (auto dep : *deps)
        {
            Luau::visit(
                overloaded{
                    [&](TypeId ty)
                    {
                        printf("\t\t|\tType %s\n", toString(ty, opts).c_str());
                    },
                    [&](TypePackId tp)
                    {
                        printf("\t\t|\tPack %s\n", toString(tp, opts).c_str());
                    },
                    [&](const Constraint* c)
                    {
                        printf("\t\t|\tCons %s\n", toString(*c, opts).c_str());
                    }
                },
                dep
            );
        }
    }
}
} // namespace Luau