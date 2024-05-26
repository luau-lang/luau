// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include <doctest.h>

#include "Luau/Language.h"

LUAU_EQSAT_ATOM(Atom1, bool);
LUAU_EQSAT_ATOM(Atom2, bool);

using namespace Luau;

using Mini = EqSat::Language<Atom1, Atom2>;

TEST_SUITE_BEGIN("EqSatLanguage");

TEST_CASE("language_get_works")
{
    Mini m{Atom1{true}};

    const Atom1* atom = m.get<Atom1>();
    REQUIRE(atom);
    CHECK(atom->value);

    CHECK(!m.get<Atom2>());
}

TEST_SUITE_END();
