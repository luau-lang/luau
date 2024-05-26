// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include <doctest.h>

#include "Luau/Language.h"

#include <string>
#include <unordered_map>

LUAU_EQSAT_ATOM(I32, int);
LUAU_EQSAT_ATOM(Bool, bool);
LUAU_EQSAT_ATOM(Str, std::string);

using namespace Luau;

using Value = EqSat::Language<I32, Bool, Str>;

TEST_SUITE_BEGIN("EqSatLanguage");

TEST_CASE("atom_equality")
{
    CHECK(I32{0} == I32{0});
    CHECK(I32{0} != I32{1});
}

TEST_CASE("language_get")
{
    Value v{I32{5}};

    auto i = v.get<I32>();
    REQUIRE(i);
    CHECK(i->value);

    CHECK(!v.get<Bool>());
}

TEST_CASE("language_copy_ctor")
{
    Value v1{I32{5}};
    Value v2 = v1;

    auto i1 = v1.get<I32>();
    auto i2 = v2.get<I32>();
    REQUIRE(i1);
    REQUIRE(i2);
    CHECK(i1->value == i2->value);
}

TEST_CASE("language_move_ctor")
{
    Value v1{Str{"hello"}};
    {
        auto s1 = v1.get<Str>();
        REQUIRE(s1);
        CHECK(s1->value == "hello");
    }

    Value v2 = std::move(v1);

    auto s1 = v1.get<Str>();
    REQUIRE(s1);
    CHECK(s1->value == ""); // this also tests the dtor.

    auto s2 = v2.get<Str>();
    REQUIRE(s2);
    CHECK(s2->value == "hello");
}

TEST_CASE("language_equality")
{
    Value v1{I32{0}};
    Value v2{I32{0}};
    Value v3{I32{1}};
    Value v4{Bool{true}};

    CHECK(v1 == v2);
    CHECK(v2 != v3);
    CHECK(v3 != v4);
}

TEST_CASE("language_is_mappable")
{
    std::unordered_map<Value, int, Value::Hash> map;

    Value v1{I32{5}};
    Value v2{I32{5}};
    Value v3{Bool{true}};

    map[v1] = 1;
    map[v2] = 2;
    map[v3] = 42;

    CHECK(map[v1] == 2);
    CHECK(map[v2] == 2);
    CHECK(map[v3] == 42);
}

TEST_SUITE_END();
