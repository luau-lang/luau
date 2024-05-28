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

struct ConstantFold
{
    using Data = std::optional<bool>;
};

using EGraph = EqSat::EGraph<PropositionalLogic, ConstantFold>;

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

TEST_SUITE_END();
