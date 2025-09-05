// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/EqSatSimplification.h"
#include "Luau/EqSatSimplificationImpl.h"

#include "Luau/EGraph.h"
#include "Luau/HashUtil.h"
#include "Luau/Id.h"
#include "Luau/Language.h"

#include "Luau/StringUtils.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeFunction.h"

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_set>
#include <vector>

LUAU_FASTFLAGVARIABLE(DebugLuauLogSimplification)
LUAU_FASTFLAGVARIABLE(DebugLuauLogSimplificationToDot)
LUAU_FASTFLAGVARIABLE(DebugLuauExtraEqSatSanityChecks)

namespace Luau::EqSatSimplification
{
using Id = Luau::EqSat::Id;

using EGraph = Luau::EqSat::EGraph<EType, struct Simplify>;
using Luau::EqSat::Slice;

TTable::TTable(Id basis)
{
    storage.push_back(basis);
}

// I suspect that this is going to become a performance hotspot.  It would be
// nice to avoid allocating propTypes_
TTable::TTable(Id basis, std::vector<StringId> propNames_, std::vector<Id> propTypes_)
    : propNames(std::move(propNames_))
{
    storage.reserve(propTypes_.size() + 1);
    storage.push_back(basis);
    storage.insert(storage.end(), propTypes_.begin(), propTypes_.end());

    LUAU_ASSERT(storage.size() == 1 + propTypes_.size());
}

Id TTable::getBasis() const
{
    LUAU_ASSERT(!storage.empty());
    return storage[0];
}

Slice<const Id> TTable::propTypes() const
{
    LUAU_ASSERT(propNames.size() + 1 == storage.size());

    return Slice{storage.data() + 1, propNames.size()};
}

Slice<Id> TTable::mutableOperands()
{
    return Slice{storage.data(), storage.size()};
}

Slice<const Id> TTable::operands() const
{
    return Slice{storage.data(), storage.size()};
}

bool TTable::operator==(const TTable& rhs) const
{
    return storage == rhs.storage && propNames == rhs.propNames;
}

size_t TTable::Hash::operator()(const TTable& value) const
{
    size_t hash = 0;

    // We're using pointers here, which does mean platform divergence. I think
    // it's okay? (famous last words, I know)
    for (StringId s : value.propNames)
        hashCombine(hash, EqSat::languageHash(s));

    hashCombine(hash, EqSat::languageHash(value.storage));

    return hash;
}

StringId StringCache::add(std::string_view s)
{
    /* Important subtlety: This use of DenseHashMap<std::string_view, StringId>
     * is okay because std::hash<std::string_view> works solely on the bytes
     * referred by the string_view.
     *
     * In other words, two string views which contain the same bytes will have
     * the same hash whether or not their addresses are the same.
     */
    if (StringId* it = strings.find(s))
        return *it;

    char* storage = static_cast<char*>(allocator.allocate(s.size()));
    memcpy(storage, s.data(), s.size());

    StringId result = StringId(views.size());
    views.emplace_back(storage, s.size());
    strings[s] = result;
    return result;
}

std::string_view StringCache::asStringView(StringId id) const
{
    LUAU_ASSERT(id < views.size());
    return views[id];
}

std::string StringCache::asString(StringId id) const
{
    return std::string{asStringView(id)};
}

template<typename T>
Simplify::Data Simplify::make(const EGraph&, const T&) const
{
    return true;
}

void Simplify::join(Data& left, const Data& right) const
{
    left = left || right;
}

using EClass = Luau::EqSat::EClass<EType, Simplify::Data>;

// A terminal type is a type that does not contain any other types.
// Examples: any, unknown, number, string, boolean, nil, table, class, thread, function
//
// All class types are also terminal.
static bool isTerminal(const EType& node)
{
    return node.get<TNil>() || node.get<TBoolean>() || node.get<TNumber>() || node.get<TString>() || node.get<TThread>() ||
           node.get<TTopFunction>() || node.get<TTopTable>() || node.get<TTopClass>() || node.get<TBuffer>() || node.get<TOpaque>() ||
           node.get<SBoolean>() || node.get<SString>() || node.get<TClass>() || node.get<TAny>() || node.get<TError>() || node.get<TUnknown>() ||
           node.get<TNever>() || node.get<TNoRefine>();
}

static bool areTerminalAndDefinitelyDisjoint(const EType& lhs, const EType& rhs)
{
    // If either node is non-terminal, then we early exit: we're not going to
    // do a state space search for whether something like:
    //  (A | B | C | D) & (E | F | G | H)
    // ... is a disjoint intersection.
    if (!isTerminal(lhs) || !isTerminal(rhs))
        return false;

    // Special case some types that aren't strict, disjoint subsets.
    if (lhs.get<TTopClass>() || lhs.get<TClass>())
        return !(rhs.get<TTopClass>() || rhs.get<TClass>());

    // Handling strings / booleans: these are the types for which we
    // expect something like:
    //
    //  "foo" & ~"bar"
    //
    // ... to simplify to "foo".
    if (lhs.get<TString>())
        return !(rhs.get<TString>() || rhs.get<SString>());

    if (lhs.get<TBoolean>())
        return !(rhs.get<TBoolean>() || rhs.get<SBoolean>());

    if (auto lhsSString = lhs.get<SString>())
    {
        auto rhsSString = rhs.get<SString>();
        if (!rhsSString)
            return !rhs.get<TString>();
        return lhsSString->value() != rhsSString->value();
    }

    if (auto lhsSBoolean = lhs.get<SBoolean>())
    {
        auto rhsSBoolean = rhs.get<SBoolean>();
        if (!rhsSBoolean)
            return !rhs.get<TBoolean>();
        return lhsSBoolean->value() != rhsSBoolean->value();
    }

    // At this point:
    // - We know both nodes are terminal
    // - We know that the LHS is not any boolean, string, or class
    // At this point, we have two classes of checks left:
    // - Whether the two enodes are exactly the same set (now that the static
    //   sets have been covered).
    // - Whether one of the enodes is a large semantic set such as TAny,
    //   TUnknown, or TError.
    return !(
        lhs.index() == rhs.index() || lhs.get<TUnknown>() || rhs.get<TUnknown>() || lhs.get<TAny>() || rhs.get<TAny>() || lhs.get<TNoRefine>() ||
        rhs.get<TNoRefine>() || lhs.get<TError>() || rhs.get<TError>() || lhs.get<TOpaque>() || rhs.get<TOpaque>()
    );
}

static bool isTerminal(const EGraph& egraph, Id eclass)
{
    const auto& nodes = egraph[eclass].nodes;
    return std::any_of(
        nodes.begin(),
        nodes.end(),
        [](auto& a)
        {
            return isTerminal(a.node);
        }
    );
}

Id mkUnion(EGraph& egraph, std::vector<Id> parts)
{
    if (parts.size() == 0)
        return egraph.add(TNever{});
    else if (parts.size() == 1)
        return parts[0];
    else
        return egraph.add(Union{std::move(parts)});
}

Id mkIntersection(EGraph& egraph, std::vector<Id> parts)
{
    if (parts.size() == 0)
        return egraph.add(TUnknown{});
    else if (parts.size() == 1)
        return parts[0];
    else
        return egraph.add(Intersection{std::move(parts)});
}

struct ListRemover
{
    std::unordered_map<TypeId, std::pair<size_t, size_t>>& mappings2;
    TypeId ty;

    ~ListRemover()
    {
        mappings2.erase(ty);
    }
};

/*
 * Crucial subtlety: It is very extremely important that enodes and eclasses are
 * immutable.  Mutating an enode would mean that it is no longer equivalent to
 * other nodes in the same eclass.
 *
 * At the same time, many TypeIds are NOT immutable!
 *
 * The thing that makes this navigable is that it is okay if the same TypeId is
 * imported as a different Id at different times as type inference runs.  For
 * example, if we at one point import a BlockedType as a TOpaque, and later
 * import that same TypeId as some other enode type, this is all completely
 * okay.
 *
 * The main thing we have to be very cautious about, I think, is unsealed
 * tables.  Unsealed table types have properties imperatively inserted into them
 * as type inference runs.  If we were to encode that TypeId as part of an
 * enode, we could run into a situation where the egraph makes incorrect
 * assumptions about the table.
 *
 * The solution is pretty simple: Never use the contents of a mutable TypeId in
 * any reduction rule.  TOpaque is always okay because we never actually poke
 * around inside the TypeId to do anything.
 */
Id toId(
    EGraph& egraph,
    NotNull<BuiltinTypes> builtinTypes,
    std::unordered_map<size_t, Id>& mappingIdToClass,
    std::unordered_map<TypeId, std::pair<size_t, size_t>>& typeToMappingId, // (TypeId: (MappingId, count))
    std::unordered_set<Id>& boundNodes,
    StringCache& strings,
    TypeId ty
)
{
    ty = follow(ty);

    // First, handle types which do not contain other types.  They obviously
    // cannot participate in cycles, so we don't have to check for that.

    if (auto freeTy = get<FreeType>(ty))
        return egraph.add(TOpaque{ty});
    else if (get<GenericType>(ty))
        return egraph.add(TOpaque{ty});
    else if (auto prim = get<PrimitiveType>(ty))
    {
        switch (prim->type)
        {
        case Luau::PrimitiveType::NilType:
            return egraph.add(TNil{});
        case Luau::PrimitiveType::Boolean:
            return egraph.add(TBoolean{});
        case Luau::PrimitiveType::Number:
            return egraph.add(TNumber{});
        case Luau::PrimitiveType::String:
            return egraph.add(TString{});
        case Luau::PrimitiveType::Thread:
            return egraph.add(TThread{});
        case Luau::PrimitiveType::Function:
            return egraph.add(TTopFunction{});
        case Luau::PrimitiveType::Table:
            return egraph.add(TTopTable{});
        case Luau::PrimitiveType::Buffer:
            return egraph.add(TBuffer{});
        default:
            LUAU_ASSERT(!"Unimplemented");
            return egraph.add(Invalid{});
        }
    }
    else if (auto s = get<SingletonType>(ty))
    {
        if (auto bs = get<BooleanSingleton>(s))
            return egraph.add(SBoolean{bs->value});
        else if (auto ss = get<StringSingleton>(s))
            return egraph.add(SString{strings.add(ss->value)});
        else
            LUAU_ASSERT(!"Unexpected");
    }
    else if (get<BlockedType>(ty))
        return egraph.add(TOpaque{ty});
    else if (get<PendingExpansionType>(ty))
        return egraph.add(TOpaque{ty});
    else if (get<FunctionType>(ty))
        return egraph.add(TFunction{ty});
    else if (ty == builtinTypes->externType)
        return egraph.add(TTopClass{});
    else if (get<ExternType>(ty))
        return egraph.add(TClass{ty});
    else if (get<AnyType>(ty))
        return egraph.add(TAny{});
    else if (get<ErrorType>(ty))
        return egraph.add(TError{});
    else if (get<UnknownType>(ty))
        return egraph.add(TUnknown{});
    else if (get<NeverType>(ty))
        return egraph.add(TNever{});

    // Now handle composite types.

    if (auto it = typeToMappingId.find(ty); it != typeToMappingId.end())
    {
        auto& [mappingId, count] = it->second;
        ++count;
        Id res = egraph.add(TBound{mappingId});
        boundNodes.insert(res);
        return res;
    }

    typeToMappingId.emplace(ty, std::pair{mappingIdToClass.size(), 0});
    ListRemover lr{typeToMappingId, ty};

    auto cache = [&](Id res)
    {
        const auto& [mappingId, count] = typeToMappingId.at(ty);
        if (count > 0)
            mappingIdToClass.emplace(mappingId, res);
        return res;
    };

    if (auto tt = get<TableType>(ty))
        return egraph.add(TImportedTable{ty});
    else if (get<MetatableType>(ty))
        return egraph.add(TOpaque{ty});
    else if (auto ut = get<UnionType>(ty))
    {
        std::vector<EqSat::Id> parts;
        for (TypeId part : ut)
            parts.push_back(toId(egraph, builtinTypes, mappingIdToClass, typeToMappingId, boundNodes, strings, part));

        return cache(mkUnion(egraph, std::move(parts)));
    }
    else if (auto it = get<IntersectionType>(ty))
    {
        std::vector<Id> parts;
        for (TypeId part : it)
            parts.push_back(toId(egraph, builtinTypes, mappingIdToClass, typeToMappingId, boundNodes, strings, part));

        LUAU_ASSERT(parts.size() > 1);

        return cache(mkIntersection(egraph, std::move(parts)));
    }
    else if (auto negation = get<NegationType>(ty))
    {
        Id part = toId(egraph, builtinTypes, mappingIdToClass, typeToMappingId, boundNodes, strings, negation->ty);
        return cache(egraph.add(Negation{std::array{part}}));
    }
    else if (auto tfun = get<TypeFunctionInstanceType>(ty))
    {
        LUAU_ASSERT(tfun->packArguments.empty());

        if (tfun->userFuncName)
        {
            // TODO: User defined type functions are pseudo-effectful: error
            // reporting is done via the `print` statement, so running a
            // UDTF multiple times may end up double erroring. egraphs
            // currently may induce type functions to be reduced multiple
            // times. We should probably opt _not_ to process user defined
            // type functions at all.
            return egraph.add(TOpaque{ty});
        }

        std::vector<Id> parts;
        parts.reserve(tfun->typeArguments.size());
        for (TypeId part : tfun->typeArguments)
            parts.push_back(toId(egraph, builtinTypes, mappingIdToClass, typeToMappingId, boundNodes, strings, part));

        // This looks sily, but we're making a copy of the specific
        // `TypeFunctionInstanceType` outside of the provided arena so that
        // we can access the members without fear of the specific TFIT being
        // overwritten with a bound type.
        return cache(egraph.add(
            TTypeFun{
                std::make_shared<const TypeFunctionInstanceType>(
                    tfun->function, tfun->typeArguments, tfun->packArguments, tfun->userFuncName, tfun->userFuncData
                ),
                std::move(parts)
            }
        ));
    }
    else if (get<NoRefineType>(ty))
        return egraph.add(TNoRefine{});
    else
    {
        LUAU_ASSERT(!"Unhandled Type");
        return cache(egraph.add(Invalid{}));
    }
}

Id toId(EGraph& egraph, NotNull<BuiltinTypes> builtinTypes, std::unordered_map<size_t, Id>& mappingIdToClass, StringCache& strings, TypeId ty)
{
    std::unordered_map<TypeId, std::pair<size_t, size_t>> typeToMappingId;
    std::unordered_set<Id> boundNodes;
    Id id = toId(egraph, builtinTypes, mappingIdToClass, typeToMappingId, boundNodes, strings, ty);

    for (Id id : boundNodes)
    {
        for (const auto [tb, _index] : Query<TBound>(&egraph, id))
        {
            Id bindee = mappingIdToClass.at(tb->value());
            egraph.merge(id, bindee);
        }
    }

    egraph.rebuild();

    return egraph.find(id);
}

// We apply a penalty to cyclic types to guide the system away from them where
// possible.
static const int CYCLE_PENALTY = 5000;

// Composite types have cost equal to the sum of the costs of their parts plus a
// constant factor.
static const int SET_TYPE_PENALTY = 1;
static const int TABLE_TYPE_PENALTY = 2;
static const int NEGATION_PENALTY = 2;
static const int TFUN_PENALTY = 2;

// FIXME.  We don't have an accurate way to score a TImportedTable table against
// a TTable.
static const int IMPORTED_TABLE_PENALTY = 50;

// TBound shouldn't ever be selected as the best node of a class unless we are
// debugging eqsat itself and need to stringify eclasses.  We thus penalize it
// so heavily that we'll use any other alternative.
static const int BOUND_PENALTY = 999999999;

// TODO iteration count limit
// TODO also: accept an argument which is the maximum cost to consider before
// abandoning the count.
// TODO: the egraph should be the first parameter.
static size_t computeCost(std::unordered_map<Id, size_t>& bestNodes, const EGraph& egraph, std::unordered_map<Id, size_t>& costs, Id id)
{
    if (auto it = costs.find(id); it != costs.end())
        return it->second;

    const std::vector<Node<EType>>& nodes = egraph[id].nodes;

    size_t minCost = std::numeric_limits<size_t>::max();
    size_t bestNode = std::numeric_limits<size_t>::max();

    const auto updateCost = [&](size_t cost, size_t node)
    {
        if (cost < minCost)
        {
            minCost = cost;
            bestNode = node;
        }
    };

    // First, quickly scan for a terminal type.  If we can find one, it is obviously the best.
    for (size_t index = 0; index < nodes.size(); ++index)
    {
        if (isTerminal(nodes[index].node))
        {
            minCost = 1;
            bestNode = index;

            costs[id] = 1;
            const auto [iter, isFresh] = bestNodes.insert({id, index});

            // If we are forcing the cost function to select a specific node,
            // then we still need to traverse into that node, even if this
            // particular node is the obvious choice under normal circumstances.
            if (isFresh || iter->second == index)
                return 1;
        }
    }

    // If we recur into this type before this call frame completes, it is
    // because this type participates in a cycle.
    costs[id] = CYCLE_PENALTY;

    auto computeChildren = [&](Slice<const Id> parts, size_t maxCost) -> std::optional<size_t>
    {
        size_t cost = 0;
        for (Id part : parts)
        {
            cost += computeCost(bestNodes, egraph, costs, part);

            // Abandon this node if it is too costly
            if (cost > maxCost)
                return std::nullopt;
        }
        return cost;
    };

    size_t startIndex = 0;
    size_t endIndex = nodes.size();

    // FFlag::DebugLuauLogSimplification will sometimes stringify an Id and pass
    // in a prepopulated bestNodes map.  If that mapping already has an index
    // for this Id, don't look at the other nodes of this class.
    if (auto it = bestNodes.find(id); it != bestNodes.end())
    {
        LUAU_ASSERT(it->second < nodes.size());

        startIndex = it->second;
        endIndex = startIndex + 1;
    }

    for (size_t index = startIndex; index < endIndex; ++index)
    {
        const auto& node = nodes[index];

        if (node.node.get<TBound>())
            updateCost(BOUND_PENALTY, index); // TODO: This could probably be an assert now that we don't need rewrite rules to handle TBound.
        else if (node.node.get<TFunction>())
        {
            minCost = 1;
            bestNode = index;
        }
        else if (auto tbl = node.node.get<TTable>())
        {
            // TODO: We could make the penalty a parameter to computeChildren.
            std::optional<size_t> maybeCost = computeChildren(tbl->operands(), minCost);
            if (maybeCost)
                updateCost(TABLE_TYPE_PENALTY + *maybeCost, index);
        }
        else if (node.node.get<TImportedTable>())
        {
            minCost = IMPORTED_TABLE_PENALTY;
            bestNode = index;
        }
        else if (auto u = node.node.get<Union>())
        {
            std::optional<size_t> maybeCost = computeChildren(u->operands(), minCost);
            if (maybeCost)
                updateCost(SET_TYPE_PENALTY + *maybeCost, index);
        }
        else if (auto i = node.node.get<Intersection>())
        {
            std::optional<size_t> maybeCost = computeChildren(i->operands(), minCost);
            if (maybeCost)
                updateCost(SET_TYPE_PENALTY + *maybeCost, index);
        }
        else if (auto negation = node.node.get<Negation>())
        {
            std::optional<size_t> maybeCost = computeChildren(negation->operands(), minCost);
            if (maybeCost)
                updateCost(NEGATION_PENALTY + *maybeCost, index);
        }
        else if (auto tfun = node.node.get<TTypeFun>())
        {
            std::optional<size_t> maybeCost = computeChildren(tfun->operands(), minCost);
            if (maybeCost)
                updateCost(TFUN_PENALTY + *maybeCost, index);
        }
    }

    LUAU_ASSERT(bestNode < nodes.size());

    costs[id] = minCost;
    bestNodes.insert({id, bestNode});
    return minCost;
}

static std::unordered_map<Id, size_t> computeBestResult(const EGraph& egraph, Id id, const std::unordered_map<Id, size_t>& forceNodes)
{
    std::unordered_map<Id, size_t> costs;
    std::unordered_map<Id, size_t> bestNodes = forceNodes;
    computeCost(bestNodes, egraph, costs, id);
    return bestNodes;
}

static std::unordered_map<Id, size_t> computeBestResult(const EGraph& egraph, Id id)
{
    std::unordered_map<Id, size_t> costs;
    std::unordered_map<Id, size_t> bestNodes;
    computeCost(bestNodes, egraph, costs, id);
    return bestNodes;
}

TypeId fromId(
    EGraph& egraph,
    const StringCache& strings,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    const std::unordered_map<Id, size_t>& bestNodes,
    std::unordered_map<Id, TypeId>& seen,
    std::vector<TypeId>& newTypeFunctions,
    Id rootId
);

TypeId flattenTableNode(
    EGraph& egraph,
    const StringCache& strings,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    const std::unordered_map<Id, size_t>& bestNodes,
    std::unordered_map<Id, TypeId>& seen,
    std::vector<TypeId>& newTypeFunctions,
    Id rootId
)
{
    std::vector<const TTable*> stack;
    std::unordered_set<Id> seenIds;

    Id id = rootId;
    const TImportedTable* importedTable = nullptr;
    while (true)
    {
        size_t index = bestNodes.at(id);
        const auto& eclass = egraph[id];

        const auto [_iter, isFresh] = seenIds.insert(id);
        if (!isFresh)
        {
            // If a TTable is its own basis, it must be the case that some other
            // node on this eclass is a TImportedTable.  Let's use that.

            bool found = false;

            for (size_t i = 0; i < eclass.nodes.size(); ++i)
            {
                if (eclass.nodes[i].node.get<TImportedTable>())
                {
                    found = true;
                    index = i;
                    break;
                }
            }

            if (!found)
            {
                // If we couldn't find one, we don't know what to do.  Use ErrorType.
                LUAU_ASSERT(0);
                return builtinTypes->errorType;
            }
        }

        const auto& node = eclass.nodes[index];
        if (const TTable* ttable = node.node.get<TTable>())
        {
            stack.push_back(ttable);
            id = ttable->getBasis();
            continue;
        }
        else if (const TImportedTable* ti = node.node.get<TImportedTable>())
        {
            importedTable = ti;
            break;
        }
        else
            LUAU_ASSERT(0);
    }

    TableType resultTable;
    if (importedTable)
    {
        const TableType* t = Luau::get<TableType>(importedTable->value());
        LUAU_ASSERT(t);
        resultTable = *t; // Intentional shallow clone here
    }

    while (!stack.empty())
    {
        const TTable* t = stack.back();
        stack.pop_back();

        for (size_t i = 0; i < t->propNames.size(); ++i)
        {
            StringId propName = t->propNames[i];
            const Id propType = t->propTypes()[i];

            resultTable.props[strings.asString(propName)] =
                Property{fromId(egraph, strings, builtinTypes, arena, bestNodes, seen, newTypeFunctions, propType)};
        }
    }

    return arena->addType(std::move(resultTable));
}

TypeId fromId(
    EGraph& egraph,
    const StringCache& strings,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    const std::unordered_map<Id, size_t>& bestNodes,
    std::unordered_map<Id, TypeId>& seen,
    std::vector<TypeId>& newTypeFunctions,
    Id rootId
)
{
    if (auto it = seen.find(rootId); it != seen.end())
        return it->second;

    size_t index = bestNodes.at(rootId);
    LUAU_ASSERT(index <= egraph[rootId].nodes.size());

    const EType& node = egraph[rootId].nodes[index].node;

    if (node.get<TNil>())
        return builtinTypes->nilType;
    else if (node.get<TBoolean>())
        return builtinTypes->booleanType;
    else if (node.get<TNumber>())
        return builtinTypes->numberType;
    else if (node.get<TString>())
        return builtinTypes->stringType;
    else if (node.get<TThread>())
        return builtinTypes->threadType;
    else if (node.get<TTopFunction>())
        return builtinTypes->functionType;
    else if (node.get<TTopTable>())
        return builtinTypes->tableType;
    else if (node.get<TTopClass>())
        return builtinTypes->externType;
    else if (node.get<TBuffer>())
        return builtinTypes->bufferType;
    else if (auto opaque = node.get<TOpaque>())
        return opaque->value();
    else if (auto b = node.get<SBoolean>())
        return b->value() ? builtinTypes->trueType : builtinTypes->falseType;
    else if (auto s = node.get<SString>())
        return arena->addType(SingletonType{StringSingleton{strings.asString(s->value())}});
    else if (auto fun = node.get<TFunction>())
        return fun->value();
    else if (auto tbl = node.get<TTable>())
    {
        TypeId res = arena->addType(BlockedType{});
        seen[rootId] = res;

        TypeId flattened = flattenTableNode(egraph, strings, builtinTypes, arena, bestNodes, seen, newTypeFunctions, rootId);

        asMutable(res)->ty.emplace<BoundType>(flattened);
        return flattened;
    }
    else if (auto tbl = node.get<TImportedTable>())
        return tbl->value();
    else if (auto cls = node.get<TClass>())
        return cls->value();
    else if (node.get<TAny>())
        return builtinTypes->anyType;
    else if (node.get<TError>())
        return builtinTypes->errorType;
    else if (node.get<TUnknown>())
        return builtinTypes->unknownType;
    else if (node.get<TNever>())
        return builtinTypes->neverType;
    else if (auto u = node.get<Union>())
    {
        Slice<const Id> parts = u->operands();

        if (parts.empty())
            return builtinTypes->neverType;
        else if (parts.size() == 1)
        {
            TypeId placeholder = arena->addType(BlockedType{});
            seen[rootId] = placeholder;
            auto result = fromId(egraph, strings, builtinTypes, arena, bestNodes, seen, newTypeFunctions, parts[0]);
            if (follow(result) == placeholder)
            {
                emplaceType<GenericType>(asMutable(placeholder), "EGRAPH-SINGLETON-CYCLE");
            }
            else
            {
                emplaceType<BoundType>(asMutable(placeholder), result);
            }
            return result;
        }
        else
        {
            TypeId res = arena->addType(BlockedType{});

            seen[rootId] = res;

            std::vector<TypeId> partTypes;
            partTypes.reserve(parts.size());

            for (Id part : parts)
                partTypes.push_back(fromId(egraph, strings, builtinTypes, arena, bestNodes, seen, newTypeFunctions, part));

            asMutable(res)->ty.emplace<UnionType>(std::move(partTypes));

            return res;
        }
    }
    else if (auto i = node.get<Intersection>())
    {
        Slice<const Id> parts = i->operands();

        if (parts.empty())
            return builtinTypes->neverType;
        else if (parts.size() == 1)
        {
            LUAU_ASSERT(parts[0] != rootId);
            return fromId(egraph, strings, builtinTypes, arena, bestNodes, seen, newTypeFunctions, parts[0]);
        }
        else
        {
            TypeId res = arena->addType(BlockedType{});
            seen[rootId] = res;

            std::vector<TypeId> partTypes;
            partTypes.reserve(parts.size());

            for (Id part : parts)
                partTypes.push_back(fromId(egraph, strings, builtinTypes, arena, bestNodes, seen, newTypeFunctions, part));

            asMutable(res)->ty.emplace<IntersectionType>(std::move(partTypes));

            return res;
        }
    }
    else if (auto negation = node.get<Negation>())
    {
        TypeId res = arena->addType(BlockedType{});
        seen[rootId] = res;

        TypeId ty = fromId(egraph, strings, builtinTypes, arena, bestNodes, seen, newTypeFunctions, negation->operands()[0]);

        asMutable(res)->ty.emplace<NegationType>(ty);

        return res;
    }
    else if (auto tfun = node.get<TTypeFun>())
    {
        TypeId res = arena->addType(BlockedType{});
        seen[rootId] = res;

        std::vector<TypeId> args;
        for (Id part : tfun->operands())
            args.push_back(fromId(egraph, strings, builtinTypes, arena, bestNodes, seen, newTypeFunctions, part));

        auto oldInstance = tfun->value();

        asMutable(res)->ty.emplace<TypeFunctionInstanceType>(
            oldInstance->function, std::move(args), std::vector<TypePackId>(), oldInstance->userFuncName, oldInstance->userFuncData
        );

        newTypeFunctions.push_back(res);

        return res;
    }
    else if (node.get<TBound>())
        return builtinTypes->errorType;
    else if (node.get<TNoRefine>())
        return builtinTypes->noRefineType;
    else
    {
        LUAU_ASSERT(!"Unimplemented");
        return nullptr;
    }
}

static TypeId fromId(
    EGraph& egraph,
    const StringCache& strings,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    const std::unordered_map<Id, size_t>& forceNodes,
    std::vector<TypeId>& newTypeFunctions,
    Id rootId
)
{
    const std::unordered_map<Id, size_t> bestNodes = computeBestResult(egraph, rootId, forceNodes);
    std::unordered_map<Id, TypeId> seen;

    return fromId(egraph, strings, builtinTypes, arena, bestNodes, seen, newTypeFunctions, rootId);
}

static TypeId fromId(
    EGraph& egraph,
    const StringCache& strings,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    std::vector<TypeId>& newTypeFunctions,
    Id rootId
)
{
    const std::unordered_map<Id, size_t> bestNodes = computeBestResult(egraph, rootId);
    std::unordered_map<Id, TypeId> seen;

    return fromId(egraph, strings, builtinTypes, arena, bestNodes, seen, newTypeFunctions, rootId);
}

Subst::Subst(Id eclass, Id newClass, std::string desc)
    : eclass(std::move(eclass))
    , newClass(std::move(newClass))
    , desc(std::move(desc))
{
}

std::string mkDesc(
    EGraph& egraph,
    const StringCache& strings,
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    Id from,
    Id to,
    const std::unordered_map<Id, size_t>& forceNodes,
    const std::string& rule
)
{
    if (!FFlag::DebugLuauLogSimplification)
        return "";

    std::vector<TypeId> newTypeFunctions;

    TypeId fromTy = fromId(egraph, strings, builtinTypes, arena, forceNodes, newTypeFunctions, from);
    TypeId toTy = fromId(egraph, strings, builtinTypes, arena, forceNodes, newTypeFunctions, to);

    ToStringOptions opts;
    opts.useQuestionMarks = false;

    const int RULE_PADDING = 35;
    const std::string rulePadding(std::max<size_t>(0, RULE_PADDING - rule.size()), ' ');
    const std::string fromIdStr = ""; // "(" + std::to_string(uint32_t(from)) + ") ";
    const std::string toIdStr = "";   // "(" + std::to_string(uint32_t(to)) + ") ";

    return rule + ":" + rulePadding + fromIdStr + toString(fromTy, opts) + " <=> " + toIdStr + toString(toTy, opts);
}

std::string mkDesc(
    EGraph& egraph,
    const StringCache& strings,
    NotNull<TypeArena> arena,
    NotNull<BuiltinTypes> builtinTypes,
    Id from,
    Id to,
    const std::string& rule
)
{
    if (!FFlag::DebugLuauLogSimplification)
        return "";

    return mkDesc(egraph, strings, arena, builtinTypes, from, to, {}, rule);
}

static std::string getNodeName(const StringCache& strings, const EType& node)
{
    if (node.get<TNil>())
        return "nil";
    else if (node.get<TBoolean>())
        return "boolean";
    else if (node.get<TNumber>())
        return "number";
    else if (node.get<TString>())
        return "string";
    else if (node.get<TThread>())
        return "thread";
    else if (node.get<TTopFunction>())
        return "function";
    else if (node.get<TTopTable>())
        return "table";
    else if (node.get<TTopClass>())
        return "class";
    else if (node.get<TBuffer>())
        return "buffer";
    else if (node.get<TOpaque>())
        return "opaque";
    else if (auto b = node.get<SBoolean>())
        return b->value() ? "true" : "false";
    else if (auto s = node.get<SString>())
        return "\"" + strings.asString(s->value()) + "\"";
    else if (node.get<Union>())
        return "\xe2\x88\xaa";
    else if (node.get<Intersection>())
        return "\xe2\x88\xa9";
    else if (auto cls = node.get<TClass>())
    {
        const ExternType* ct = get<ExternType>(cls->value());
        LUAU_ASSERT(ct);
        return ct->name;
    }
    else if (node.get<TAny>())
        return "any";
    else if (node.get<TError>())
        return "error";
    else if (node.get<TUnknown>())
        return "unknown";
    else if (node.get<TNever>())
        return "never";
    else if (auto tfun = node.get<TTypeFun>())
        return "tfun " + tfun->value()->function->name;
    else if (node.get<Negation>())
        return "~";
    else if (node.get<Invalid>())
        return "invalid?";
    else if (node.get<TBound>())
        return "bound";

    return "???";
}

std::string toDot(const StringCache& strings, const EGraph& egraph)
{
    std::stringstream ss;
    ss << "digraph G {" << '\n';
    ss << "    graph [fontsize=10 fontname=\"Verdana\" compound=true];" << '\n';
    ss << "    node [shape=record fontsize=10 fontname=\"Verdana\"];" << '\n';

    std::set<Id> populated;

    for (const auto& [id, eclass] : egraph.getAllClasses())
    {
        for (const auto& n : eclass.nodes)
        {
            const EType& node = n.node;
            if (!node.operands().empty())
                populated.insert(id);
            for (Id op : node.operands())
                populated.insert(op);
        }
    }

    for (const auto& [id, eclass] : egraph.getAllClasses())
    {
        if (!populated.count(id))
            continue;

        const std::string className = "cluster_" + std::to_string(uint32_t(id));
        ss << "    subgraph " << className << " {" << '\n';
        ss << "        node [style=\"rounded,filled\"];" << '\n';
        ss << "        label = \"" << uint32_t(id) << "\";" << '\n';
        ss << "        color = blue;" << '\n';

        for (size_t index = 0; index < eclass.nodes.size(); ++index)
        {
            const auto& node = eclass.nodes[index].node;

            const std::string label = getNodeName(strings, node);
            const std::string nodeName = "n" + std::to_string(uint32_t(id)) + "_" + std::to_string(index);

            ss << "        " << nodeName << " [label=\"" << label << "\"];" << '\n';
        }

        ss << "    }" << '\n';
    }

    for (const auto& [id, eclass] : egraph.getAllClasses())
    {
        for (size_t index = 0; index < eclass.nodes.size(); ++index)
        {
            const auto& node = eclass.nodes[index].node;

            const std::string label = getNodeName(strings, node);
            const std::string nodeName = "n" + std::to_string(uint32_t(egraph.find(id))) + "_" + std::to_string(index);

            for (Id op : node.operands())
            {
                op = egraph.find(op);
                const std::string destNodeName = "n" + std::to_string(uint32_t(op)) + "_0";
                ss << "    " << nodeName << " -> " << destNodeName << " [lhead=cluster_" << uint32_t(op) << "];" << '\n';
            }
        }
    }

    ss << "}" << '\n';

    return ss.str();
}

template<typename Tag>
static Tag const* isTag(const EType& node)
{
    return node.get<Tag>();
}

/// Important: Only use this to test for leaf node types like TUnknown and
/// TNumber.  Things that we know cannot be simplified any further and are safe
/// to short-circuit on.
///
/// It does a linear scan and exits early, so if a particular eclass has
/// multiple "interesting" representations, this function can surprise you.
template<typename Tag>
static Tag const* isTag(const EGraph& egraph, Id id)
{
    for (const auto& node : egraph[id].nodes)
    {
        if (auto n = isTag<Tag>(node.node))
            return n;
    }
    return nullptr;
}

struct RewriteRule
{
    explicit RewriteRule(EGraph* egraph)
        : egraph(egraph)
    {
    }

    virtual void read(std::vector<Subst>& substs, Id eclass, const EType* enode) = 0;

protected:
    const EqSat::EClass<EType, Simplify::Data>& get(Id id)
    {
        return (*egraph)[id];
    }

    Id find(Id id)
    {
        return egraph->find(id);
    }

    Id add(EType enode)
    {
        return egraph->add(std::move(enode));
    }

    template<typename Tag>
    const Tag* isTag(Id id)
    {
        for (const auto& node : (*egraph)[id].nodes)
        {
            if (auto n = node.node.get<Tag>())
                return n;
        }
        return nullptr;
    }

    template<typename Tag>
    bool isTag(const EType& enode)
    {
        return enode.get<Tag>();
    }

public:
    EGraph* egraph;
};

enum SubclassRelationship
{
    LeftSuper,
    RightSuper,
    Unrelated
};

static SubclassRelationship relateClasses(const TClass* leftClass, const TClass* rightClass)
{
    const ExternType* leftExternType = Luau::get<ExternType>(leftClass->value());
    const ExternType* rightExternType = Luau::get<ExternType>(rightClass->value());

    if (isSubclass(leftExternType, rightExternType))
        return RightSuper;
    else if (isSubclass(rightExternType, leftExternType))
        return LeftSuper;
    else
        return Unrelated;
}

// Entirely analogous to NormalizedType except that it operates on eclasses instead of TypeIds.
struct CanonicalizedType
{
    std::optional<Id> nilPart;
    std::optional<Id> truePart;
    std::optional<Id> falsePart;
    std::optional<Id> numberPart;
    std::optional<Id> stringPart;
    std::vector<Id> stringSingletons;
    std::optional<Id> threadPart;
    std::optional<Id> functionPart;
    std::optional<Id> tablePart;
    std::vector<Id> classParts;
    std::optional<Id> bufferPart;
    std::optional<Id> errorPart;

    // Functions that have been union'd into the type
    std::unordered_set<Id> functionParts;

    // Anything that isn't canonical: Intersections, unions, free types, and so on.
    std::unordered_set<Id> otherParts;

    bool isUnknown() const
    {
        return nilPart && truePart && falsePart && numberPart && stringPart && threadPart && functionPart && tablePart && bufferPart;
    }
};

void unionUnknown(EGraph& egraph, CanonicalizedType& ct)
{
    ct.nilPart = egraph.add(TNil{});
    ct.truePart = egraph.add(SBoolean{true});
    ct.falsePart = egraph.add(SBoolean{false});
    ct.numberPart = egraph.add(TNumber{});
    ct.stringPart = egraph.add(TString{});
    ct.threadPart = egraph.add(TThread{});
    ct.functionPart = egraph.add(TTopFunction{});
    ct.tablePart = egraph.add(TTopTable{});
    ct.bufferPart = egraph.add(TBuffer{});

    ct.functionParts.clear();
    ct.otherParts.clear();
}

void unionAny(EGraph& egraph, CanonicalizedType& ct)
{
    unionUnknown(egraph, ct);
    ct.errorPart = egraph.add(TError{});
}

void unionClasses(EGraph& egraph, std::vector<Id>& hereParts, Id there)
{
    if (1 == hereParts.size() && isTag<TTopClass>(egraph, hereParts[0]))
        return;

    const auto thereClass = isTag<TClass>(egraph, there);
    if (!thereClass)
        return;

    for (size_t index = 0; index < hereParts.size(); ++index)
    {
        const Id herePart = hereParts[index];

        if (auto partClass = isTag<TClass>(egraph, herePart))
        {
            switch (relateClasses(partClass, thereClass))
            {
            case LeftSuper:
                return;
            case RightSuper:
                hereParts[index] = there;
                std::sort(hereParts.begin(), hereParts.end());
                return;
            case Unrelated:
                continue;
            }
        }
    }

    hereParts.push_back(there);
    std::sort(hereParts.begin(), hereParts.end());
}

void unionWithType(EGraph& egraph, CanonicalizedType& ct, Id part)
{
    if (isTag<TNil>(egraph, part))
        ct.nilPart = part;
    else if (isTag<TBoolean>(egraph, part))
        ct.truePart = ct.falsePart = part;
    else if (auto b = isTag<SBoolean>(egraph, part))
    {
        if (b->value())
            ct.truePart = part;
        else
            ct.falsePart = part;
    }
    else if (isTag<TNumber>(egraph, part))
        ct.numberPart = part;
    else if (isTag<TString>(egraph, part))
        ct.stringPart = part;
    else if (isTag<SString>(egraph, part))
        ct.stringSingletons.push_back(part);
    else if (isTag<TThread>(egraph, part))
        ct.threadPart = part;
    else if (isTag<TTopFunction>(egraph, part))
    {
        ct.functionPart = part;
        ct.functionParts.clear();
    }
    else if (isTag<TTopTable>(egraph, part))
        ct.tablePart = part;
    else if (isTag<TTopClass>(egraph, part))
        ct.classParts = {part};
    else if (isTag<TBuffer>(egraph, part))
        ct.bufferPart = part;
    else if (isTag<TFunction>(egraph, part))
    {
        if (!ct.functionPart)
            ct.functionParts.insert(part);
    }
    else if (auto tclass = isTag<TClass>(egraph, part))
        unionClasses(egraph, ct.classParts, part);
    else if (isTag<TAny>(egraph, part))
    {
        unionAny(egraph, ct);
        return;
    }
    else if (isTag<TError>(egraph, part))
        ct.errorPart = part;
    else if (isTag<TUnknown>(egraph, part))
        unionUnknown(egraph, ct);
    else if (isTag<TNever>(egraph, part))
    {
        // Nothing
    }
    else
        ct.otherParts.insert(part);
}

// Find an enode under the given eclass which is simple enough that it could be
// subtracted from a CanonicalizedType easily.
//
// A union is "simple enough" if it is acyclic and is only comprised of terminal
// types and unions that are themselves subtractable
const EType* findSubtractableClass(const EGraph& egraph, std::unordered_set<Id>& seen, Id id)
{
    if (seen.count(id))
        return nullptr;

    const EType* bestUnion = nullptr;
    std::optional<size_t> unionSize;

    for (const auto& n : egraph[id].nodes)
    {
        const EType& node = n.node;

        if (isTerminal(node))
            return &node;

        if (const auto u = node.get<Union>())
        {
            seen.insert(id);

            for (Id part : u->operands())
            {
                if (!findSubtractableClass(egraph, seen, part))
                    return nullptr;
            }

            // If multiple unions in this class are all simple enough, prefer
            // the shortest one.
            if (!unionSize || u->operands().size() < unionSize)
            {
                unionSize = u->operands().size();
                bestUnion = &node;
            }
        }
    }

    return bestUnion;
}

const EType* findSubtractableClass(const EGraph& egraph, Id id)
{
    std::unordered_set<Id> seen;

    return findSubtractableClass(egraph, seen, id);
}

// Subtract the type 'part' from 'ct'
// Returns true if the subtraction succeeded.  This function will fail if 'part` is too complicated.
bool subtract(EGraph& egraph, CanonicalizedType& ct, Id part)
{
    const EType* etype = findSubtractableClass(egraph, part);
    if (!etype)
        return false;

    if (etype->get<TNil>())
        ct.nilPart.reset();
    else if (etype->get<TBoolean>())
    {
        ct.truePart.reset();
        ct.falsePart.reset();
    }
    else if (auto b = etype->get<SBoolean>())
    {
        if (b->value())
            ct.truePart.reset();
        else
            ct.falsePart.reset();
    }
    else if (etype->get<TNumber>())
        ct.numberPart.reset();
    else if (etype->get<TString>())
        ct.stringPart.reset();
    else if (etype->get<SString>())
        return false;
    else if (etype->get<TThread>())
        ct.threadPart.reset();
    else if (etype->get<TTopFunction>())
        ct.functionPart.reset();
    else if (etype->get<TTopTable>())
        ct.tablePart.reset();
    else if (etype->get<TTopClass>())
        ct.classParts.clear();
    else if (auto tclass = etype->get<TClass>())
    {
        auto it = std::find(ct.classParts.begin(), ct.classParts.end(), part);
        if (it != ct.classParts.end())
            ct.classParts.erase(it);
        else
            return false;
    }
    else if (etype->get<TBuffer>())
        ct.bufferPart.reset();
    else if (etype->get<TAny>())
        ct = {};
    else if (etype->get<TError>())
        ct.errorPart.reset();
    else if (etype->get<TUnknown>())
    {
        std::optional<Id> errorPart = ct.errorPart;
        ct = {};
        ct.errorPart = errorPart;
    }
    else if (etype->get<TNever>())
    {
        // Nothing
    }
    else if (auto u = etype->get<Union>())
    {
        // TODO cycles
        // TODO this is super promlematic because 'part' represents a whole group of equivalent enodes.
        for (Id unionPart : u->operands())
        {
            // TODO: This recursive call will require that we re-traverse this
            // eclass to find the subtractible enode. It would be nice to do the
            // work just once and reuse it.
            bool ok = subtract(egraph, ct, unionPart);
            if (!ok)
                return false;
        }
    }
    else if (etype->get<Intersection>())
        return false;
    else
        return false;

    return true;
}

static std::pair<Id, size_t> fromCanonicalized(EGraph& egraph, CanonicalizedType& ct)
{
    if (ct.isUnknown())
    {
        if (ct.errorPart)
            return {egraph.add(TAny{}), 1};
        else
            return {egraph.add(TUnknown{}), 1};
    }

    std::vector<Id> parts;

    if (ct.nilPart)
        parts.push_back(*ct.nilPart);

    if (ct.truePart && ct.falsePart)
        parts.push_back(egraph.add(TBoolean{}));
    else if (ct.truePart)
        parts.push_back(*ct.truePart);
    else if (ct.falsePart)
        parts.push_back(*ct.falsePart);

    if (ct.numberPart)
        parts.push_back(*ct.numberPart);

    if (ct.stringPart)
        parts.push_back(*ct.stringPart);
    else if (!ct.stringSingletons.empty())
        parts.insert(parts.end(), ct.stringSingletons.begin(), ct.stringSingletons.end());

    if (ct.threadPart)
        parts.push_back(*ct.threadPart);
    if (ct.functionPart)
        parts.push_back(*ct.functionPart);
    if (ct.tablePart)
        parts.push_back(*ct.tablePart);
    parts.insert(parts.end(), ct.classParts.begin(), ct.classParts.end());
    if (ct.bufferPart)
        parts.push_back(*ct.bufferPart);
    if (ct.errorPart)
        parts.push_back(*ct.errorPart);

    parts.insert(parts.end(), ct.functionParts.begin(), ct.functionParts.end());
    parts.insert(parts.end(), ct.otherParts.begin(), ct.otherParts.end());

    std::sort(parts.begin(), parts.end());
    auto it = std::unique(parts.begin(), parts.end());
    parts.erase(it, parts.end());

    const size_t size = parts.size();
    return {mkUnion(egraph, std::move(parts)), size};
}

void addChildren(const EGraph& egraph, const EType* enode, VecDeque<Id>& worklist)
{
    for (Id id : enode->operands())
        worklist.push_back(id);
}

static bool occurs(EGraph& egraph, Id outerId, Slice<const Id> operands)
{
    for (const Id i : operands)
    {
        if (egraph.find(i) == outerId)
            return true;
    }
    return false;
}

Simplifier::Simplifier(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes)
    : arena(arena)
    , builtinTypes(builtinTypes)
    , egraph(Simplify{})
{
}

const EqSat::EClass<EType, Simplify::Data>& Simplifier::get(Id id) const
{
    return egraph[id];
}

Id Simplifier::find(Id id) const
{
    return egraph.find(id);
}

Id Simplifier::add(EType enode)
{
    return egraph.add(std::move(enode));
}

template<typename Tag>
const Tag* Simplifier::isTag(Id id) const
{
    for (const auto& node : get(id).nodes)
    {
        if (const Tag* ty = node.node.get<Tag>())
            return ty;
    }

    return nullptr;
}

template<typename Tag>
const Tag* Simplifier::isTag(const EType& enode) const
{
    return enode.get<Tag>();
}

void Simplifier::subst(Id from, Id to)
{
    substs.emplace_back(from, to, " - ");
}

void Simplifier::subst(Id from, Id to, const std::string& ruleName)
{
    std::string desc;
    if (FFlag::DebugLuauLogSimplification)
        desc = mkDesc(egraph, stringCache, arena, builtinTypes, from, to, std::move(ruleName));
    substs.emplace_back(from, to, desc);
}

void Simplifier::subst(Id from, Id to, const std::string& ruleName, const std::unordered_map<Id, size_t>& forceNodes)
{
    std::string desc;
    if (FFlag::DebugLuauLogSimplification)
        desc = mkDesc(egraph, stringCache, arena, builtinTypes, from, to, forceNodes, ruleName);
    substs.emplace_back(from, to, desc);
}

void Simplifier::subst(Id from, size_t boringIndex, Id to, const std::string& ruleName, const std::unordered_map<Id, size_t>& forceNodes)
{
    std::string desc;
    if (FFlag::DebugLuauLogSimplification)
        desc = mkDesc(egraph, stringCache, arena, builtinTypes, from, to, forceNodes, ruleName);

    egraph.markBoring(from, boringIndex);
    substs.emplace_back(from, to, desc);
}

void Simplifier::unionClasses(std::vector<Id>& hereParts, Id there)
{
    if (1 == hereParts.size() && isTag<TTopClass>(hereParts[0]))
        return;

    const auto thereClass = isTag<TClass>(there);
    if (!thereClass)
        return;

    for (size_t index = 0; index < hereParts.size(); ++index)
    {
        const Id herePart = hereParts[index];

        if (auto partClass = isTag<TClass>(herePart))
        {
            switch (relateClasses(partClass, thereClass))
            {
            case LeftSuper:
                return;
            case RightSuper:
                hereParts[index] = there;
                std::sort(hereParts.begin(), hereParts.end());
                return;
            case Unrelated:
                continue;
            }
        }
    }

    hereParts.push_back(there);
    std::sort(hereParts.begin(), hereParts.end());
}

void Simplifier::simplifyUnion(Id id)
{
    id = find(id);

    for (const auto [u, unionIndex] : Query<Union>(&egraph, id))
    {
        std::vector<Id> newParts;
        std::unordered_set<Id> seen;

        CanonicalizedType canonicalized;

        if (occurs(egraph, id, u->operands()))
            continue;

        for (Id part : u->operands())
            unionWithType(egraph, canonicalized, find(part));

        const auto [resultId, newSize] = fromCanonicalized(egraph, canonicalized);

        if (newSize < u->operands().size())
            subst(id, unionIndex, resultId, "simplifyUnion", {{id, unionIndex}});
        else
            subst(id, resultId, "simplifyUnion", {{id, unionIndex}});
    }
}

// If one of the nodes matches the given Tag, succeed and return the id and node for the other half.
// If neither matches, return nullopt.
template<typename Tag>
static std::optional<std::pair<Id, const EType*>> matchOne(Id hereId, const EType* hereNode, Id thereId, const EType* thereNode)
{
    if (hereNode->get<Tag>())
        return std::pair{thereId, thereNode};
    else if (thereNode->get<Tag>())
        return std::pair{hereId, hereNode};
    else
        return std::nullopt;
}

// If the two nodes can be intersected into a "simple" type, return that, else return nullopt.
std::optional<EType> intersectOne(EGraph& egraph, Id hereId, const EType* hereNode, Id thereId, const EType* thereNode)
{
    hereId = egraph.find(hereId);
    thereId = egraph.find(thereId);

    if (hereId == thereId)
        return *hereNode;

    if (hereNode->get<TNever>() || thereNode->get<TNever>())
        return TNever{};

    if (hereNode->get<Union>() || hereNode->get<Intersection>() || hereNode->get<Negation>() || thereNode->get<Union>() ||
        thereNode->get<Intersection>() || thereNode->get<Negation>() || hereNode->get<TOpaque>() || thereNode->get<TOpaque>())
        return std::nullopt;

    if (hereNode->get<TUnknown>())
        return *thereNode;
    if (thereNode->get<TUnknown>())
        return *hereNode;

    if (hereNode->get<TTypeFun>() || thereNode->get<TTypeFun>())
        return std::nullopt;

    if (auto res = matchOne<TTopClass>(hereId, hereNode, thereId, thereNode))
    {
        const auto [otherId, otherNode] = *res;

        if (otherNode->get<TClass>() || otherNode->get<TTopClass>())
            return *otherNode;
        else
            return TNever{};
    }
    if (auto res = matchOne<TTopTable>(hereId, hereNode, thereId, thereNode))
    {
        const auto [otherId, otherNode] = *res;

        if (otherNode->get<TTopTable>() || otherNode->get<TImportedTable>())
            return *otherNode;
    }
    if (auto res = matchOne<TImportedTable>(hereId, hereNode, thereId, thereNode))
    {
        const auto [otherId, otherNode] = *res;

        if (otherNode->get<TImportedTable>())
            return std::nullopt; // TODO
        else
            return TNever{};
    }
    if (auto hereClass = hereNode->get<TClass>())
    {
        if (auto thereClass = thereNode->get<TClass>())
        {
            switch (relateClasses(hereClass, thereClass))
            {
            case LeftSuper:
                return *thereNode;
            case RightSuper:
                return *hereNode;
            case Unrelated:
                return TNever{};
            }
        }
        else
            return TNever{};
    }
    if (auto hereBool = hereNode->get<SBoolean>())
    {
        if (auto thereBool = thereNode->get<SBoolean>())
        {
            if (hereBool->value() == thereBool->value())
                return *hereNode;
            else
                return TNever{};
        }
        else if (thereNode->get<TBoolean>())
            return *hereNode;
        else
            return TNever{};
    }
    if (auto thereBool = thereNode->get<SBoolean>())
    {
        if (auto hereBool = hereNode->get<SBoolean>())
        {
            if (thereBool->value() == hereBool->value())
                return *thereNode;
            else
                return TNever{};
        }
        else if (hereNode->get<TBoolean>())
            return *thereNode;
        else
            return TNever{};
    }
    if (hereNode->get<TBoolean>())
    {
        if (thereNode->get<TBoolean>())
            return TBoolean{};
        else if (thereNode->get<SBoolean>())
            return *thereNode;
        else
            return TNever{};
    }
    if (thereNode->get<TBoolean>())
    {
        if (hereNode->get<TBoolean>())
            return TBoolean{};
        else if (hereNode->get<SBoolean>())
            return *hereNode;
        else
            return TNever{};
    }
    if (hereNode->get<SString>())
    {
        if (thereNode->get<TString>())
            return *hereNode;
        else
            return TNever{};
    }
    if (thereNode->get<SString>())
    {
        if (hereNode->get<TString>())
            return *thereNode;
        else
            return TNever{};
    }
    if (hereNode->get<TTopFunction>())
    {
        if (thereNode->get<TFunction>() || thereNode->get<TTopFunction>())
            return *thereNode;
        else
            return TNever{};
    }
    if (thereNode->get<TTopFunction>())
    {
        if (hereNode->get<TFunction>() || hereNode->get<TTopFunction>())
            return *hereNode;
        else
            return TNever{};
    }
    if (hereNode->get<TFunction>() && thereNode->get<TFunction>())
        return std::nullopt;
    if (hereNode->get<TFunction>() && isTerminal(*thereNode))
        return TNever{};
    if (thereNode->get<TFunction>() && isTerminal(*hereNode))
        return TNever{};
    if (isTerminal(*hereNode) && isTerminal(*thereNode))
    {
        // We already know that 'here' and 'there' are different classes.
        return TNever{};
    }

    return std::nullopt;
}

void Simplifier::uninhabitedIntersection(Id id)
{
    for (const auto [intersection, index] : Query<Intersection>(&egraph, id))
    {
        Slice<const Id> parts = intersection->operands();

        if (parts.empty())
        {
            Id never = egraph.add(TNever{});
            subst(id, never, "uninhabitedIntersection");
            return;
        }
        else if (1 == parts.size())
        {
            subst(id, parts[0], "uninhabitedIntersection");
            return;
        }

        Id accumulator = egraph.add(TUnknown{});
        EType accumulatorNode = TUnknown{};

        std::vector<Id> unsimplified;

        if (occurs(egraph, id, parts))
            continue;

        for (Id partId : parts)
        {
            if (isTag<TNoRefine>(partId))
                return;

            bool found = false;

            const auto& partNodes = egraph[partId].nodes;
            for (size_t partIndex = 0; partIndex < partNodes.size(); ++partIndex)
            {
                const EType& N = partNodes[partIndex].node;
                if (std::optional<EType> intersection = intersectOne(egraph, accumulator, &accumulatorNode, partId, &N))
                {
                    if (isTag<TNever>(*intersection))
                    {
                        subst(id, egraph.add(TNever{}), "uninhabitedIntersection", {{id, index}, {partId, partIndex}});
                        return;
                    }

                    accumulator = egraph.add(*intersection);
                    accumulatorNode = *intersection;
                    found = true;
                    break;
                }
            }

            if (!found)
                unsimplified.push_back(partId);
        }

        if ((unsimplified.empty() || !isTag<TUnknown>(accumulator)) && find(accumulator) != id)
            unsimplified.push_back(accumulator);

        const bool isSmaller = unsimplified.size() < parts.size();

        const Id result = mkIntersection(egraph, std::move(unsimplified));

        if (isSmaller)
            subst(id, index, result, "uninhabitedIntersection", {{id, index}});
        else
            subst(id, result, "uninhabitedIntersection", {{id, index}});
    }
}

void Simplifier::intersectWithNegatedClass(Id id)
{
    for (const auto pair : Query<Intersection>(&egraph, id))
    {
        const Intersection* intersection = pair.first;
        const size_t intersectionIndex = pair.second;

        auto trySubst = [&](size_t i, size_t j)
        {
            Id iId = intersection->operands()[i];
            Id jId = intersection->operands()[j];

            for (const auto [negation, negationIndex] : Query<Negation>(&egraph, jId))
            {
                const Id negated = negation->operands()[0];

                if (iId == negated)
                {
                    subst(id, egraph.add(TNever{}), "intersectClassWithNegatedClass", {{id, intersectionIndex}, {jId, negationIndex}});
                    return;
                }

                for (const auto [negatedClass, negatedClassIndex] : Query<TClass>(&egraph, negated))
                {
                    const auto& iNodes = egraph[iId].nodes;
                    for (size_t iIndex = 0; iIndex < iNodes.size(); ++iIndex)
                    {
                        const EType& iNode = iNodes[iIndex].node;
                        if (isTag<TNil>(iNode) || isTag<TBoolean>(iNode) || isTag<TNumber>(iNode) || isTag<TString>(iNode) || isTag<TThread>(iNode) ||
                            isTag<TTopFunction>(iNode) ||
                            // isTag<TTopTable>(iNode) || // I'm not sure about this one.
                            isTag<SBoolean>(iNode) || isTag<SString>(iNode) || isTag<TFunction>(iNode) || isTag<TNever>(iNode))
                        {
                            // eg string & ~SomeClass
                            subst(
                                id,
                                iId,
                                "intersectClassWithNegatedClass",
                                {{id, intersectionIndex}, {iId, iIndex}, {jId, negationIndex}, {negated, negatedClassIndex}}
                            );
                            return;
                        }

                        if (const TClass* class_ = iNode.get<TClass>())
                        {
                            switch (relateClasses(class_, negatedClass))
                            {
                            case LeftSuper:
                                // eg Instance & ~Part
                                // This cannot be meaningfully reduced.
                                continue;
                            case RightSuper:
                                subst(
                                    id,
                                    egraph.add(TNever{}),
                                    "intersectClassWithNegatedClass",
                                    {{id, intersectionIndex}, {iId, iIndex}, {jId, negationIndex}, {negated, negatedClassIndex}}
                                );
                                return;
                            case Unrelated:
                                // Part & ~Folder == Part
                                {
                                    std::vector<Id> newParts;
                                    newParts.reserve(intersection->operands().size() - 1);
                                    for (Id part : intersection->operands())
                                    {
                                        if (part != jId)
                                            newParts.push_back(part);
                                    }

                                    Id substId = mkIntersection(egraph, newParts);
                                    subst(
                                        id,
                                        substId,
                                        "intersectClassWithNegatedClass",
                                        {{id, intersectionIndex}, {iId, iIndex}, {jId, negationIndex}, {negated, negatedClassIndex}}
                                    );
                                }
                            }
                        }
                    }
                }
            }
        };

        if (2 != intersection->operands().size())
            continue;

        trySubst(0, 1);
        trySubst(1, 0);
    }
}

void Simplifier::intersectWithNegatedAtom(Id id)
{
    // Let I and ~J be two arbitrary distinct operands of an intersection where
    // I and J are terminal but are not type variables. (free, generic, or
    // otherwise opaque)
    //
    // If I and J are equal, then the whole intersection is equivalent to never.
    //
    // If I and J are inequal, then J & ~I == J

    for (const auto [intersection, intersectionIndex] : Query<Intersection>(&egraph, id))
    {
        const Slice<const Id>& intersectionOperands = intersection->operands();
        for (size_t i = 0; i < intersectionOperands.size(); ++i)
        {
            for (const auto [negation, negationIndex] : Query<Negation>(&egraph, intersectionOperands[i]))
            {
                for (size_t negationOperandIndex = 0; negationOperandIndex < egraph[negation->operands()[0]].nodes.size(); ++negationOperandIndex)
                {
                    const EType* negationOperand = &egraph[negation->operands()[0]].nodes[negationOperandIndex].node;
                    if (!isTerminal(*negationOperand) || negationOperand->get<TOpaque>())
                        continue;

                    for (size_t j = 0; j < intersectionOperands.size(); ++j)
                    {
                        if (j == i)
                            continue;

                        for (size_t jNodeIndex = 0; jNodeIndex < egraph[intersectionOperands[j]].nodes.size(); ++jNodeIndex)
                        {
                            const EType* jNode = &egraph[intersectionOperands[j]].nodes[jNodeIndex].node;
                            if (!isTerminal(*jNode) || jNode->get<TOpaque>())
                                continue;

                            if (*negationOperand == *jNode)
                            {
                                // eg "Hello" & ~"Hello"
                                // or boolean & ~boolean
                                subst(
                                    id,
                                    egraph.add(TNever{}),
                                    "intersectWithNegatedAtom",
                                    {{id, intersectionIndex}, {intersectionOperands[i], negationIndex}, {intersectionOperands[j], jNodeIndex}}
                                );
                                return;
                            }
                            else if (areTerminalAndDefinitelyDisjoint(*jNode, *negationOperand))
                            {
                                // eg "Hello" & ~"World"
                                // or boolean & ~string
                                std::vector<Id> newOperands(intersectionOperands.begin(), intersectionOperands.end());
                                newOperands.erase(newOperands.begin() + std::vector<Id>::difference_type(i));

                                subst(
                                    id,
                                    mkIntersection(egraph, std::move(newOperands)),
                                    "intersectWithNegatedAtom",
                                    {{id, intersectionIndex}, {intersectionOperands[i], negationIndex}, {intersectionOperands[j], jNodeIndex}}
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}

void Simplifier::intersectWithNoRefine(Id id)
{
    for (const auto pair : Query<Intersection>(&egraph, id))
    {
        const Intersection* intersection = pair.first;
        const size_t intersectionIndex = pair.second;

        const Slice<const Id> intersectionOperands = intersection->operands();

        for (size_t index = 0; index < intersectionOperands.size(); ++index)
        {
            const auto replace = [&]()
            {
                std::vector<Id> newOperands{intersectionOperands.begin(), intersectionOperands.end()};
                newOperands.erase(newOperands.begin() + index);

                Id substId = egraph.add(Intersection{std::move(newOperands)});

                subst(id, substId, "intersectWithNoRefine", {{id, intersectionIndex}});
            };

            if (isTag<TNoRefine>(intersectionOperands[index]))
                replace();
            else
            {
                for (const auto [negation, negationIndex] : Query<Negation>(&egraph, intersectionOperands[index]))
                {
                    if (isTag<TNoRefine>(negation->operands()[0]))
                    {
                        replace();
                        break;
                    }
                }
            }
        }
    }
}

/*
 * Replace x where x = A & (B | x) with A
 *
 * Important subtlety: The egraph is routinely going to create cyclic unions and
 * intersections.  We can't arbitrarily remove things from a union just because
 * it can be referred to in a cyclic way.  We must only do this for things that
 * can only be expressed in a cyclic way.
 *
 * As an example, we will bind the following type to true:
 *
 * (true | buffer | class | function | number | string | table | thread) &
 * boolean
 *
 * The egraph represented by this type will indeed be cyclic as the 'true' class
 * includes both 'true' itself and the above type, but removing true from the
 * union will result is an incorrect judgment!
 *
 * The solution (for now) is only to consider a type to be cyclic if it was
 * cyclic on its original import.
 *
 * FIXME: I still don't think this is quite right, but I don't know how to
 * articulate what the actual rule ought to be.
 */
void Simplifier::cyclicIntersectionOfUnion(Id id)
{
    // FIXME: This has pretty terrible runtime complexity.

    for (const auto [i, intersectionIndex] : Query<Intersection>(&egraph, id))
    {
        Slice<const Id> intersectionParts = i->operands();
        for (size_t intersectionOperandIndex = 0; intersectionOperandIndex < intersectionParts.size(); ++intersectionOperandIndex)
        {
            const Id intersectionPart = find(intersectionParts[intersectionOperandIndex]);

            for (const auto [bound, _boundIndex] : Query<TBound>(&egraph, intersectionPart))
            {
                const Id pointee = find(mappingIdToClass.at(bound->value()));

                for (const auto [u, unionIndex] : Query<Union>(&egraph, pointee))
                {
                    const Slice<const Id>& unionOperands = u->operands();
                    for (size_t unionOperandIndex = 0; unionOperandIndex < unionOperands.size(); ++unionOperandIndex)
                    {
                        Id unionOperand = find(unionOperands[unionOperandIndex]);
                        if (unionOperand == id)
                        {
                            std::vector<Id> newIntersectionParts(intersectionParts.begin(), intersectionParts.end());
                            newIntersectionParts.erase(newIntersectionParts.begin() + intersectionOperandIndex);

                            subst(
                                id,
                                mkIntersection(egraph, std::move(newIntersectionParts)),
                                "cyclicIntersectionOfUnion",
                                {{id, intersectionIndex}, {pointee, unionIndex}}
                            );
                        }
                    }
                }
            }
        }
    }
}

void Simplifier::cyclicUnionOfIntersection(Id id)
{
    // FIXME: This has pretty terrible runtime complexity.

    for (const auto [union_, unionIndex] : Query<Union>(&egraph, id))
    {
        Slice<const Id> unionOperands = union_->operands();
        for (size_t unionOperandIndex = 0; unionOperandIndex < unionOperands.size(); ++unionOperandIndex)
        {
            const Id unionPart = find(unionOperands[unionOperandIndex]);

            for (const auto [bound, _boundIndex] : Query<TBound>(&egraph, unionPart))
            {
                const Id pointee = find(mappingIdToClass.at(bound->value()));

                for (const auto [intersection, intersectionIndex] : Query<Intersection>(&egraph, pointee))
                {
                    Slice<const Id> intersectionOperands = intersection->operands();
                    for (size_t intersectionOperandIndex = 0; intersectionOperandIndex < intersectionOperands.size(); ++intersectionOperandIndex)
                    {
                        const Id intersectionPart = find(intersectionOperands[intersectionOperandIndex]);
                        if (intersectionPart == id)
                        {
                            std::vector<Id> newIntersectionParts(intersectionOperands.begin(), intersectionOperands.end());
                            newIntersectionParts.erase(newIntersectionParts.begin() + intersectionOperandIndex);

                            if (!newIntersectionParts.empty())
                            {
                                Id newIntersection = mkIntersection(egraph, std::move(newIntersectionParts));

                                std::vector<Id> newIntersectionParts(unionOperands.begin(), unionOperands.end());
                                newIntersectionParts.erase(newIntersectionParts.begin() + unionOperandIndex);
                                newIntersectionParts.push_back(newIntersection);

                                subst(
                                    id,
                                    mkUnion(egraph, std::move(newIntersectionParts)),
                                    "cyclicUnionOfIntersection",
                                    {{id, unionIndex}, {pointee, intersectionIndex}}
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}

void Simplifier::expandNegation(Id id)
{
    for (const auto [negation, index] : Query<Negation>{&egraph, id})
    {
        if (isTag<TNoRefine>(negation->operands()[0]))
            return;

        CanonicalizedType canonicalized;
        unionUnknown(egraph, canonicalized);

        const bool ok = subtract(egraph, canonicalized, negation->operands()[0]);
        if (!ok)
            continue;

        subst(id, fromCanonicalized(egraph, canonicalized).first, "expandNegation", {{id, index}});
    }
}

/**
 * Let A be a class-node having the form B & C1 & ... & Cn
 * And B be a class-node having the form (D | E)
 *
 * Create a class containing the node (C1 & ... & Cn & D) | (C1 & ... & Cn & E)
 *
 * This function does nothing and returns nullopt if A and B are cyclic.
 */
static std::optional<Id> distributeIntersectionOfUnion(
    EGraph& egraph,
    Id outerClass,
    const Intersection* outerIntersection,
    Id innerClass,
    const Union* innerUnion
)
{
    Slice<const Id> outerOperands = outerIntersection->operands();

    std::vector<Id> newOperands;
    newOperands.reserve(innerUnion->operands().size());
    for (Id innerOperand : innerUnion->operands())
    {
        if (isTag<TNever>(egraph, innerOperand))
            continue;

        if (innerOperand == outerClass)
        {
            // Skip cyclic intersections of unions.  There's a separate
            // rule to get rid of those.
            return std::nullopt;
        }

        std::vector<Id> intersectionParts;
        intersectionParts.reserve(outerOperands.size());
        intersectionParts.push_back(innerOperand);

        for (const Id op : outerOperands)
        {
            if (isTag<TNever>(egraph, op))
            {
                break;
            }
            if (op != innerClass)
                intersectionParts.push_back(op);
        }

        newOperands.push_back(mkIntersection(egraph, intersectionParts));
    }

    return mkUnion(egraph, std::move(newOperands));
}

// A & (B | C) -> (A & B) | (A & C)
//
// A & B & (C | D) -> A & (B & (C | D))
//                 -> A & ((B & C) | (B & D))
//                 -> (A & B & C) | (A & B & D)
void Simplifier::intersectionOfUnion(Id id)
{
    id = find(id);

    for (const auto [intersection, intersectionIndex] : Query<Intersection>(&egraph, id))
    {
        // For each operand O
        //      For each node N
        //          If N is a union U
        //              Create a new union comprised of every operand except O intersected with every operand of U
        const Slice<const Id> operands = intersection->operands();

        if (operands.size() < 2)
            return;

        if (occurs(egraph, id, operands))
            continue;

        for (Id operand : operands)
        {
            operand = find(operand);
            if (operand == id)
                break;
            // Optimization: Decline to distribute any unions on an eclass that
            // also contains a terminal node.
            if (isTerminal(egraph, operand))
                continue;

            for (const auto [operandUnion, unionIndex] : Query<Union>(&egraph, operand))
            {
                if (occurs(egraph, id, operandUnion->operands()))
                    continue;

                std::optional<Id> distributed = distributeIntersectionOfUnion(egraph, id, intersection, operand, operandUnion);

                if (distributed)
                    subst(id, *distributed, "intersectionOfUnion", {{id, intersectionIndex}, {operand, unionIndex}});
            }
        }
    }
}

// {"a": b} & {"a": c, ...} => {"a": b & c, ...}
void Simplifier::intersectTableProperty(Id id)
{
    for (const auto [intersection, intersectionIndex] : Query<Intersection>(&egraph, id))
    {
        const Slice<const Id> intersectionParts = intersection->operands();
        for (size_t i = 0; i < intersection->operands().size(); ++i)
        {
            const Id iId = intersection->operands()[i];

            for (size_t j = 0; j < intersection->operands().size(); ++j)
            {
                if (i == j)
                    continue;

                const Id jId = intersection->operands()[j];

                if (iId == jId)
                    continue;

                for (const auto [table1, table1Index] : Query<TImportedTable>(&egraph, iId))
                {
                    const TableType* table1Ty = Luau::get<TableType>(table1->value());
                    LUAU_ASSERT(table1Ty);

                    if (table1Ty->props.size() != 1)
                        continue;

                    for (const auto [table2, table2Index] : Query<TImportedTable>(&egraph, jId))
                    {
                        const TableType* table2Ty = Luau::get<TableType>(table2->value());
                        LUAU_ASSERT(table2Ty);

                        auto it = table2Ty->props.find(table1Ty->props.begin()->first);
                        if (it != table2Ty->props.end())
                        {
                            std::vector<Id> newIntersectionParts;
                            newIntersectionParts.reserve(intersectionParts.size() - 1);

                            for (size_t index = 0; index < intersectionParts.size(); ++index)
                            {
                                if (index != i && index != j)
                                    newIntersectionParts.push_back(intersectionParts[index]);
                            }

                            Id newTableProp = egraph.add(
                                Intersection{
                                    toId(egraph, builtinTypes, mappingIdToClass, stringCache, *it->second.readTy),
                                    toId(egraph, builtinTypes, mappingIdToClass, stringCache, *table1Ty->props.begin()->second.readTy)
                                }
                            );

                            newIntersectionParts.push_back(egraph.add(TTable{jId, {stringCache.add(it->first)}, {newTableProp}}));

                            subst(
                                id,
                                mkIntersection(egraph, std::move(newIntersectionParts)),
                                "intersectTableProperty",
                                {{id, intersectionIndex}, {iId, table1Index}, {jId, table2Index}}
                            );
                        }
                    }
                }
            }
        }
    }
}

// { prop: never } == never
void Simplifier::uninhabitedTable(Id id)
{
    for (const auto [table, tableIndex] : Query<TImportedTable>(&egraph, id))
    {
        const TableType* tt = Luau::get<TableType>(table->value());
        LUAU_ASSERT(tt);

        for (const auto& [propName, prop] : tt->props)
        {
            if (prop.readTy && Luau::get<NeverType>(follow(*prop.readTy)))
            {
                subst(id, egraph.add(TNever{}), "uninhabitedTable", {{id, tableIndex}});
                return;
            }

            if (prop.writeTy && Luau::get<NeverType>(follow(*prop.writeTy)))
            {
                subst(id, egraph.add(TNever{}), "uninhabitedTable", {{id, tableIndex}});
                return;
            }
        }
    }

    for (const auto [table, tableIndex] : Query<TTable>(&egraph, id))
    {
        for (Id propType : table->propTypes())
        {
            if (isTag<TNever>(propType))
            {
                subst(id, egraph.add(TNever{}), "uninhabitedTable", {{id, tableIndex}});
                return;
            }
        }
    }
}

void Simplifier::unneededTableModification(Id id)
{
    for (const auto [tbl, tblIndex] : Query<TTable>(&egraph, id))
    {
        const Id basis = tbl->getBasis();
        for (const auto [importedTbl, importedTblIndex] : Query<TImportedTable>(&egraph, basis))
        {
            const TableType* tt = Luau::get<TableType>(importedTbl->value());
            LUAU_ASSERT(tt);

            bool skip = false;

            for (size_t i = 0; i < tbl->propNames.size(); ++i)
            {
                StringId propName = tbl->propNames[i];
                const Id propType = tbl->propTypes()[i];

                Id importedProp = toId(egraph, builtinTypes, mappingIdToClass, stringCache, *tt->props.at(stringCache.asString(propName)).readTy);

                if (find(importedProp) != find(propType))
                {
                    skip = true;
                    break;
                }
            }

            if (!skip)
                subst(id, basis, "unneededTableModification", {{id, tblIndex}, {basis, importedTblIndex}});
        }
    }
}

void Simplifier::builtinTypeFunctions(Id id)
{
    for (const auto [tfun, index] : Query<TTypeFun>(&egraph, id))
    {
        const Slice<const Id>& args = tfun->operands();

        if (args.size() != 2)
            continue;

        const std::string& name = tfun->value()->function->name;
        if (name == "add" || name == "sub" || name == "mul" || name == "div" || name == "idiv" || name == "pow" || name == "mod")
        {
            if (isTag<TNumber>(args[0]) && isTag<TNumber>(args[1]))
            {
                subst(id, add(TNumber{}), "builtinTypeFunctions", {{id, index}});
            }
        }
    }
}

// Replace union<>, intersect<>, and refine<> with unions or intersections.
// These type functions exist primarily to cause simplification to defer until
// particular points in execution, so it is safe to get rid of them here.
//
// It's not clear that these type functions should exist at all.
void Simplifier::iffyTypeFunctions(Id id)
{
    for (const auto [tfun, index] : Query<TTypeFun>(&egraph, id))
    {
        const Slice<const Id>& args = tfun->operands();

        const std::string& name = tfun->value()->function->name;

        if (name == "union")
            subst(id, add(Union{std::vector(args.begin(), args.end())}), "iffyTypeFunctions", {{id, index}});
        else if (name == "intersect")
            subst(id, add(Intersection{std::vector(args.begin(), args.end())}), "iffyTypeFunctions", {{id, index}});
    }
}

// Replace instances of `lt<X, Y>` and `le<X, Y>` when either X or Y is `number`
// or `string` with `boolean`. Lua semantics are that if we see the expression:
//
//   x < y
//
// ... we error if `x` and `y` don't have the same type. We know that for
// `string` and `number`, comparisons will always return a boolean. So if either
// of the arguments to `lt<>` are equivalent to `number` or `string`, then the
// type is effectively `boolean`: either the other type is equivalent, in which
// case we eval to `boolean`, or we diverge (raise an error).
void Simplifier::strictMetamethods(Id id)
{
    for (const auto [tfun, index] : Query<TTypeFun>(&egraph, id))
    {
        const Slice<const Id>& args = tfun->operands();

        const std::string& name = tfun->value()->function->name;

        if (!(name == "lt" || name == "le") || args.size() != 2)
            continue;

        if (isTag<TNumber>(args[0]) || isTag<TString>(args[0]) || isTag<TNumber>(args[1]) || isTag<TString>(args[1]))
        {
            subst(id, add(TBoolean{}), __FUNCTION__, {{id, index}});
        }
    }
}

static void deleteSimplifier(Simplifier* s)
{
    delete s;
}

SimplifierPtr newSimplifier(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes)
{
    return SimplifierPtr{new Simplifier(arena, builtinTypes), &deleteSimplifier};
}

} // namespace Luau::EqSatSimplification

namespace Luau
{

std::optional<EqSatSimplificationResult> eqSatSimplify(NotNull<Simplifier> simplifier, TypeId ty)
{
    using namespace Luau::EqSatSimplification;

    std::unordered_map<size_t, Id> newMappings;
    Id rootId = toId(simplifier->egraph, simplifier->builtinTypes, newMappings, simplifier->stringCache, ty);
    simplifier->mappingIdToClass.insert(newMappings.begin(), newMappings.end());

    Simplifier::RewriteRuleFn rules[] = {
        &Simplifier::simplifyUnion,
        &Simplifier::uninhabitedIntersection,
        &Simplifier::intersectWithNegatedClass,
        &Simplifier::intersectWithNegatedAtom,
        &Simplifier::intersectWithNoRefine,
        &Simplifier::cyclicIntersectionOfUnion,
        &Simplifier::cyclicUnionOfIntersection,
        &Simplifier::expandNegation,
        &Simplifier::intersectionOfUnion,
        &Simplifier::intersectTableProperty,
        &Simplifier::uninhabitedTable,
        &Simplifier::unneededTableModification,
        &Simplifier::builtinTypeFunctions,
        &Simplifier::iffyTypeFunctions,
        &Simplifier::strictMetamethods,
    };

    std::unordered_set<Id> seen;
    VecDeque<Id> worklist;

    bool progressed = true;

    int count = 0;
    const int MAX_COUNT = 1000;

    if (FFlag::DebugLuauLogSimplificationToDot)
        std::ofstream("begin.dot") << toDot(simplifier->stringCache, simplifier->egraph);

    auto& egraph = simplifier->egraph;
    const auto& builtinTypes = simplifier->builtinTypes;
    auto& arena = simplifier->arena;

    if (FFlag::DebugLuauLogSimplification)
        printf(">> simplify %s\n", toString(ty).c_str());

    while (progressed && count < MAX_COUNT)
    {
        progressed = false;
        worklist.clear();
        seen.clear();

        rootId = egraph.find(rootId);

        worklist.push_back(rootId);

        if (FFlag::DebugLuauLogSimplification)
        {
            std::vector<TypeId> newTypeFunctions;
            const TypeId t = fromId(egraph, simplifier->stringCache, builtinTypes, arena, newTypeFunctions, rootId);

            std::cout << "Begin (" << uint32_t(egraph.find(rootId)) << ")\t" << toString(t) << '\n';
        }

        while (!worklist.empty() && count < MAX_COUNT)
        {
            Id id = egraph.find(worklist.front());
            worklist.pop_front();

            const bool isFresh = seen.insert(id).second;
            if (!isFresh)
                continue;

            simplifier->substs.clear();

            // Optimization: If this class alraedy has a terminal node, don't
            // try to run any rules on it.
            bool shouldAbort = false;

            for (const auto& enode : egraph[id].nodes)
            {
                if (isTerminal(enode.node))
                {
                    shouldAbort = true;
                    break;
                }
            }

            if (shouldAbort)
                continue;

            for (const auto& enode : egraph[id].nodes)
                addChildren(egraph, &enode.node, worklist);

            for (Simplifier::RewriteRuleFn rule : rules)
                (simplifier.get()->*rule)(id);

            if (simplifier->substs.empty())
                continue;

            for (const Subst& subst : simplifier->substs)
            {
                if (subst.newClass == subst.eclass)
                    continue;

                if (FFlag::DebugLuauExtraEqSatSanityChecks)
                {
                    const Id never = egraph.find(egraph.add(TNever{}));
                    const Id str = egraph.find(egraph.add(TString{}));
                    const Id unk = egraph.find(egraph.add(TUnknown{}));
                    LUAU_ASSERT(never != str);
                    LUAU_ASSERT(never != unk);
                }

                const bool isFresh = egraph.merge(subst.newClass, subst.eclass);

                ++count;

                if (FFlag::DebugLuauLogSimplification && isFresh)
                    std::cout << "count=" << std::setw(3) << count << "\t" << subst.desc << '\n';

                if (FFlag::DebugLuauLogSimplificationToDot)
                {
                    std::string filename = format("step%03d.dot", count);
                    std::ofstream(filename) << toDot(simplifier->stringCache, egraph);
                }

                if (FFlag::DebugLuauExtraEqSatSanityChecks)
                {
                    const Id never = egraph.find(egraph.add(TNever{}));
                    const Id str = egraph.find(egraph.add(TString{}));
                    const Id unk = egraph.find(egraph.add(TUnknown{}));
                    const Id trueId = egraph.find(egraph.add(SBoolean{true}));

                    LUAU_ASSERT(never != str);
                    LUAU_ASSERT(never != unk);
                    LUAU_ASSERT(never != trueId);
                }

                progressed |= isFresh;
            }

            egraph.rebuild();
        }
    }

    EqSatSimplificationResult result;
    result.result = fromId(egraph, simplifier->stringCache, builtinTypes, arena, result.newTypeFunctions, rootId);

    if (FFlag::DebugLuauLogSimplification)
        printf("<< simplify %s\n", toString(result.result).c_str());

    return result;
}

} // namespace Luau
