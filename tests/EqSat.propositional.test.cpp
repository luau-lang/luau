// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include <doctest.h>

#include "Luau/EGraph.h"
#include "Luau/Id.h"
#include "Luau/Language.h"

#include <optional>

LUAU_EQSAT_ATOM(Var, std::string);
LUAU_EQSAT_ATOM(Bool, bool);

LUAU_EQSAT_FIELD(Negated);
LUAU_EQSAT_UNARY_NODE(Not, Negated);

LUAU_EQSAT_FIELD(Left);
LUAU_EQSAT_FIELD(Right);
LUAU_EQSAT_BINARY_NODE(And, Left, Right);
LUAU_EQSAT_BINARY_NODE(Or, Left, Right);

LUAU_EQSAT_FIELD(Antecedent);
LUAU_EQSAT_FIELD(Consequent);
LUAU_EQSAT_BINARY_NODE(Implies, Antecedent, Consequent);

using namespace Luau;

using PropositionalLogic = EqSat::Language<Var, Bool, Not, And, Or, Implies>;

using EGraph = EqSat::EGraph<PropositionalLogic, struct ConstantFold>;

struct ConstantFold
{
    using Data = std::optional<bool>;

    Data make(const EGraph& egraph, const PropositionalLogic& enode) const
    {
        if (enode.get<Var>())
            return std::nullopt;
        else if (auto b = enode.get<Bool>())
            return b->value;
        else if (auto n = enode.get<Not>())
        {
            if (auto data = egraph[n->field<Negated>()].data)
                return !*data;
        }
        else if (auto a = enode.get<And>())
        {
            Data left = egraph[a->field<Left>()].data;
            Data right = egraph[a->field<Right>()].data;
            if (left && right)
                return *left && *right;
        }
        else if (auto o = enode.get<Or>())
        {
            Data left = egraph[o->field<Left>()].data;
            Data right = egraph[o->field<Right>()].data;
            if (left && right)
                return *left && *right;
        }
        else if (auto i = enode.get<Implies>())
        {
            Data antecedent = egraph[i->field<Antecedent>()].data;
            Data consequent = egraph[i->field<Consequent>()].data;
            if (antecedent && consequent)
                return !*antecedent || *consequent;
        }

        return std::nullopt;
    }

    void join(Data& a, const Data& b)
    {
        if (!a && b)
            a = b;
    }
};

TEST_SUITE_BEGIN("EqSatPropositionalLogic");

TEST_CASE("egraph_hashconsing")
{
    EGraph egraph;

    EqSat::Id id1 = egraph.add(Bool{true});
    EqSat::Id id2 = egraph.add(Bool{true});
    EqSat::Id id3 = egraph.add(Bool{false});

    CHECK(id1 == id2);
    CHECK(id2 != id3);
}

TEST_CASE("egraph_data")
{
    EGraph egraph;

    EqSat::Id id1 = egraph.add(Bool{true});
    EqSat::Id id2 = egraph.add(Bool{false});

    CHECK(egraph[id1].data == true);
    CHECK(egraph[id2].data == false);
}

TEST_CASE("egraph_merge")
{
    EGraph egraph;

    EqSat::Id id1 = egraph.add(Var{"a"});
    EqSat::Id id2 = egraph.add(Bool{true});
    egraph.merge(id1, id2);

    CHECK(egraph[id1].data == true);
    CHECK(egraph[id2].data == true);
}

TEST_SUITE_END();
