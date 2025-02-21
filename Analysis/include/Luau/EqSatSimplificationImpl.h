// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/EGraph.h"
#include "Luau/Id.h"
#include "Luau/Language.h"
#include "Luau/Lexer.h" // For Allocator
#include "Luau/NotNull.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeFwd.h"

namespace Luau
{
struct TypeFunction;
}

namespace Luau::EqSatSimplification
{

using StringId = uint32_t;
using Id = Luau::EqSat::Id;

LUAU_EQSAT_UNIT(TNil);
LUAU_EQSAT_UNIT(TBoolean);
LUAU_EQSAT_UNIT(TNumber);
LUAU_EQSAT_UNIT(TString);
LUAU_EQSAT_UNIT(TThread);
LUAU_EQSAT_UNIT(TTopFunction);
LUAU_EQSAT_UNIT(TTopTable);
LUAU_EQSAT_UNIT(TTopClass);
LUAU_EQSAT_UNIT(TBuffer);

// Used for any type that eqsat can't do anything interesting with.
LUAU_EQSAT_ATOM(TOpaque, TypeId);

LUAU_EQSAT_ATOM(SBoolean, bool);
LUAU_EQSAT_ATOM(SString, StringId);

LUAU_EQSAT_ATOM(TFunction, TypeId);

LUAU_EQSAT_ATOM(TImportedTable, TypeId);

LUAU_EQSAT_ATOM(TClass, TypeId);

LUAU_EQSAT_UNIT(TAny);
LUAU_EQSAT_UNIT(TError);
LUAU_EQSAT_UNIT(TUnknown);
LUAU_EQSAT_UNIT(TNever);

LUAU_EQSAT_NODE_SET(Union);
LUAU_EQSAT_NODE_SET(Intersection);

LUAU_EQSAT_NODE_ARRAY(Negation, 1);

LUAU_EQSAT_NODE_ATOM_WITH_VECTOR(TTypeFun, std::shared_ptr<const TypeFunctionInstanceType>);

LUAU_EQSAT_UNIT(TNoRefine);
LUAU_EQSAT_UNIT(Invalid);

// enodes are immutable, but types are cyclic.  We need a way to tie the knot.
// We handle this by generating TBound nodes at points where we encounter cycles.
// Each TBound has an ordinal that we later map onto the type.
// We use a substitution rule to replace all TBound nodes with their referrent.
LUAU_EQSAT_ATOM(TBound, size_t);

// Tables are sufficiently unlike other enodes that the Language.h macros won't cut it.
struct TTable
{
    explicit TTable(Id basis);
    TTable(Id basis, std::vector<StringId> propNames_, std::vector<Id> propTypes_);

    // All TTables extend some other table.  This may be TTopTable.
    //
    // It will frequently be a TImportedTable, in which case we can reuse things
    // like source location and documentation info.
    Id getBasis() const;
    EqSat::Slice<const Id> propTypes() const;
    // TODO: Also support read-only table props
    // TODO: Indexer type, index result type.

    std::vector<StringId> propNames;

    // The enode interface
    EqSat::Slice<Id> mutableOperands();
    EqSat::Slice<const Id> operands() const;
    bool operator==(const TTable& rhs) const;
    bool operator!=(const TTable& rhs) const
    {
        return !(*this == rhs);
    }

    struct Hash
    {
        size_t operator()(const TTable& value) const;
    };

private:
    // The first element of this vector is the basis.  Subsequent elements are
    // property types. As we add other things like read-only properties and
    // indexers, the structure of this array is likely to change.
    //
    // We encode our data in this way so that the operands() method can properly
    // return a Slice<Id>.
    std::vector<Id> storage;
};

template<typename L>
using Node = EqSat::Node<L>;

using EType = EqSat::Language<
    TNil,
    TBoolean,
    TNumber,
    TString,
    TThread,
    TTopFunction,
    TTopTable,
    TTopClass,
    TBuffer,

    TOpaque,

    SBoolean,
    SString,

    TFunction,
    TTable,
    TImportedTable,
    TClass,

    TAny,
    TError,
    TUnknown,
    TNever,

    Union,
    Intersection,

    Negation,

    TTypeFun,

    Invalid,
    TNoRefine,
    TBound>;


struct StringCache
{
    Allocator allocator;
    DenseHashMap<std::string_view, StringId> strings{{}};
    std::vector<std::string_view> views;

    StringId add(std::string_view s);
    std::string_view asStringView(StringId id) const;
    std::string asString(StringId id) const;
};

using EGraph = Luau::EqSat::EGraph<EType, struct Simplify>;

struct Simplify
{
    using Data = bool;

    template<typename T>
    Data make(const EGraph&, const T&) const;

    void join(Data& left, const Data& right) const;
};

struct Subst
{
    Id eclass;
    Id newClass;

    // The node into eclass which is boring, if any
    std::optional<size_t> boringIndex;

    std::string desc;

    Subst(Id eclass, Id newClass, std::string desc = "");
};

struct Simplifier
{
    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
    EGraph egraph;
    StringCache stringCache;

    // enodes are immutable but types can be cyclic, so we need some way to
    // encode the cycle. This map is used to connect TBound nodes to the right
    // eclass.
    //
    // The cyclicIntersection rewrite rule uses this to sense when a cycle can
    // be deleted from an intersection or union.
    std::unordered_map<size_t, Id> mappingIdToClass;

    std::vector<Subst> substs;

    using RewriteRuleFn = void (Simplifier::*)(Id id);

    Simplifier(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes);

    // Utilities
    const EqSat::EClass<EType, Simplify::Data>& get(Id id) const;
    Id find(Id id) const;
    Id add(EType enode);

    template<typename Tag>
    const Tag* isTag(Id id) const;

    template<typename Tag>
    const Tag* isTag(const EType& enode) const;

    void subst(Id from, Id to);
    void subst(Id from, Id to, const std::string& ruleName);
    void subst(Id from, Id to, const std::string& ruleName, const std::unordered_map<Id, size_t>& forceNodes);
    void subst(Id from, size_t boringIndex, Id to, const std::string& ruleName, const std::unordered_map<Id, size_t>& forceNodes);

    void unionClasses(std::vector<Id>& hereParts, Id there);

    // Rewrite rules
    void simplifyUnion(Id id);
    void uninhabitedIntersection(Id id);
    void intersectWithNegatedClass(Id id);
    void intersectWithNegatedAtom(Id id);
    void intersectWithNoRefine(Id id);
    void cyclicIntersectionOfUnion(Id id);
    void cyclicUnionOfIntersection(Id id);
    void expandNegation(Id id);
    void intersectionOfUnion(Id id);
    void intersectTableProperty(Id id);
    void uninhabitedTable(Id id);
    void unneededTableModification(Id id);
    void builtinTypeFunctions(Id id);
    void iffyTypeFunctions(Id id);
    void strictMetamethods(Id id);
};

template<typename Tag>
struct QueryIterator
{
    QueryIterator();
    QueryIterator(EGraph* egraph, Id eclass);

    bool operator==(const QueryIterator& other) const;
    bool operator!=(const QueryIterator& other) const;

    std::pair<const Tag*, size_t> operator*() const;

    QueryIterator& operator++();
    QueryIterator& operator++(int);

private:
    EGraph* egraph = nullptr;
    Id eclass;
    size_t index = 0;
};

template<typename Tag>
struct Query
{
    EGraph* egraph;
    Id eclass;

    Query(EGraph* egraph, Id eclass)
        : egraph(egraph)
        , eclass(eclass)
    {
    }

    QueryIterator<Tag> begin()
    {
        return QueryIterator<Tag>{egraph, eclass};
    }

    QueryIterator<Tag> end()
    {
        return QueryIterator<Tag>{};
    }
};

template<typename Tag>
QueryIterator<Tag>::QueryIterator()
    : egraph(nullptr)
    , eclass(Id{0})
    , index(0)
{
}

template<typename Tag>
QueryIterator<Tag>::QueryIterator(EGraph* egraph_, Id eclass)
    : egraph(egraph_)
    , eclass(eclass)
    , index(0)
{
    const auto& ecl = (*egraph)[eclass];

    static constexpr const int idx = EType::VariantTy::getTypeId<Tag>();

    for (const auto& enode : ecl.nodes)
    {
        if (enode.node.index() < idx)
            ++index;
        else
            break;
    }

    if (index >= ecl.nodes.size() || ecl.nodes[index].node.index() != idx)
    {
        egraph = nullptr;
        index = 0;
    }
}

template<typename Tag>
bool QueryIterator<Tag>::operator==(const QueryIterator<Tag>& rhs) const
{
    if (egraph == nullptr && rhs.egraph == nullptr)
        return true;

    return egraph == rhs.egraph && eclass == rhs.eclass && index == rhs.index;
}

template<typename Tag>
bool QueryIterator<Tag>::operator!=(const QueryIterator<Tag>& rhs) const
{
    return !(*this == rhs);
}

template<typename Tag>
std::pair<const Tag*, size_t> QueryIterator<Tag>::operator*() const
{
    LUAU_ASSERT(egraph != nullptr);

    EGraph::EClassT& ecl = (*egraph)[eclass];

    LUAU_ASSERT(index < ecl.nodes.size());
    auto& enode = ecl.nodes[index].node;
    Tag* result = enode.template get<Tag>();
    LUAU_ASSERT(result);
    return {result, index};
}

// pre-increment
template<typename Tag>
QueryIterator<Tag>& QueryIterator<Tag>::operator++()
{
    const auto& ecl = (*egraph)[eclass];

    do
    {
        ++index;
        if (index >= ecl.nodes.size() || ecl.nodes[index].node.index() != EType::VariantTy::getTypeId<Tag>())
        {
            egraph = nullptr;
            index = 0;
            break;
        }
    } while (ecl.nodes[index].boring);

    return *this;
}

// post-increment
template<typename Tag>
QueryIterator<Tag>& QueryIterator<Tag>::operator++(int)
{
    QueryIterator<Tag> res = *this;
    ++res;
    return res;
}

} // namespace Luau::EqSatSimplification
