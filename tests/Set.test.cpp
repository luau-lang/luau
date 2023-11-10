// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Set.h"

#include "doctest.h"

TEST_SUITE_BEGIN("SetTests");

TEST_CASE("empty_set_size_0")
{
    Luau::Set<int> s1{0};
    CHECK(s1.size() == 0);
    CHECK(s1.empty());
}

TEST_CASE("insertion_works_and_increases_size")
{
    Luau::Set<int> s1{0};
    CHECK(s1.size() == 0);
    CHECK(s1.empty());

    s1.insert(1);
    CHECK(s1.contains(1));
    CHECK(s1.size() == 1);

    s1.insert(2);
    CHECK(s1.contains(2));
    CHECK(s1.size() == 2);
}

TEST_CASE("clear_resets_size")
{
    Luau::Set<int> s1{0};
    s1.insert(1);
    s1.insert(2);
    REQUIRE(s1.size() == 2);

    s1.clear();
    CHECK(s1.size() == 0);
    CHECK(s1.empty());
}

TEST_CASE("erase_works_and_decreases_size")
{
    Luau::Set<int> s1{0};
    s1.insert(1);
    s1.insert(2);
    CHECK(s1.size() == 2);
    CHECK(s1.contains(1));
    CHECK(s1.contains(2));

    s1.erase(1);
    CHECK(s1.size() == 1);
    CHECK(!s1.contains(1));
    CHECK(s1.contains(2));

    s1.erase(2);
    CHECK(s1.size() == 0);
    CHECK(s1.empty());
    CHECK(!s1.contains(1));
    CHECK(!s1.contains(2));
}

TEST_SUITE_END();
