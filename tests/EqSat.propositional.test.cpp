// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include <doctest.h>

#include "Luau/EGraph.h"
#include "Luau/Id.h"
#include "Luau/Language.h"

#include <optional>
#include <string>

LUAU_EQSAT_ATOM(Var, std::string);
LUAU_EQSAT_ATOM(Bool, bool);
LUAU_EQSAT_NODE_ARRAY(Not, 1);
LUAU_EQSAT_NODE_ARRAY(And, 2);
LUAU_EQSAT_NODE_ARRAY(Or, 2);
LUAU_EQSAT_NODE_ARRAY(Implies, 2);

using namespace Luau;

using PropositionalLogic = EqSat::Language<Var, Bool, Not, And, Or, Implies>;

using EGraph = EqSat::EGraph<PropositionalLogic, struct ConstantFold>;

struct ConstantFold
{
    using Data = std::optional<bool>;

    Data make(const EGraph& egraph, const Var& var) const
    {
        return std::nullopt;
    }

    Data make(const EGraph& egraph, const Bool& b) const
    {
        return b.value();
    }

    Data make(const EGraph& egraph, const Not& n) const
    {
        Data data = egraph[n[0]].data;
        if (data)
            return !*data;

        return std::nullopt;
    }

    Data make(const EGraph& egraph, const And& a) const
    {
        Data l = egraph[a[0]].data;
        Data r = egraph[a[1]].data;
        if (l && r)
            return *l && *r;

        return std::nullopt;
    }

    Data make(const EGraph& egraph, const Or& o) const
    {
        Data l = egraph[o[0]].data;
        Data r = egraph[o[1]].data;
        if (l && r)
            return *l || *r;

        return std::nullopt;
    }

    Data make(const EGraph& egraph, const Implies& i) const
    {
        Data antecedent = egraph[i[0]].data;
        Data consequent = egraph[i[1]].data;
        if (antecedent && consequent)
            return !*antecedent || *consequent;

        return std::nullopt;
    }

    void join(Data& a, const Data& b) const
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

TEST_CASE("const_fold_true_and_true")
{
    EGraph egraph;

    EqSat::Id id1 = egraph.add(Bool{true});
    EqSat::Id id2 = egraph.add(Bool{true});
    EqSat::Id id3 = egraph.add(And{id1, id2});

    CHECK(egraph[id3].data == true);
}

TEST_CASE("const_fold_true_and_false")
{
    EGraph egraph;

    EqSat::Id id1 = egraph.add(Bool{true});
    EqSat::Id id2 = egraph.add(Bool{false});
    EqSat::Id id3 = egraph.add(And{id1, id2});

    CHECK(egraph[id3].data == false);
}

TEST_CASE("const_fold_false_and_false")
{
    EGraph egraph;

    EqSat::Id id1 = egraph.add(Bool{false});
    EqSat::Id id2 = egraph.add(Bool{false});
    EqSat::Id id3 = egraph.add(And{id1, id2});

    CHECK(egraph[id3].data == false);
}

TEST_CASE("implications")
{
    EGraph egraph;

    EqSat::Id t = egraph.add(Bool{true});
    EqSat::Id f = egraph.add(Bool{false});

    EqSat::Id a = egraph.add(Implies{t, t}); // true
    EqSat::Id b = egraph.add(Implies{t, f}); // false
    EqSat::Id c = egraph.add(Implies{f, t}); // true
    EqSat::Id d = egraph.add(Implies{f, f}); // true

    CHECK(egraph[a].data == true);
    CHECK(egraph[b].data == false);
    CHECK(egraph[c].data == true);
    CHECK(egraph[d].data == true);
}

TEST_CASE("merge_x_and_y")
{
    EGraph egraph;

    EqSat::Id x = egraph.add(Var{"x"});
    EqSat::Id y = egraph.add(Var{"y"});

    EqSat::Id a = egraph.add(Var{"a"});
    EqSat::Id ax = egraph.add(And{a, x});
    EqSat::Id ay = egraph.add(And{a, y});

    egraph.merge(x, y); // [x y] [ax] [ay] [a]
    CHECK_EQ(egraph.size(), 4);
    CHECK_EQ(egraph.find(x), egraph.find(y));
    CHECK_NE(egraph.find(ax), egraph.find(ay));
    CHECK_NE(egraph.find(a), egraph.find(x));
    CHECK_NE(egraph.find(a), egraph.find(y));

    egraph.rebuild(); // [x y] [ax ay] [a]
    CHECK_EQ(egraph.size(), 3);
    CHECK_EQ(egraph.find(x), egraph.find(y));
    CHECK_EQ(egraph.find(ax), egraph.find(ay));
    CHECK_NE(egraph.find(a), egraph.find(x));
    CHECK_NE(egraph.find(a), egraph.find(y));
}

TEST_SUITE_END();
