// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "ScopedFlags.h"
#include "Luau/Set.h"

#include "doctest.h"

#include <string>
#include <vector>

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

TEST_CASE("iterate_over_set")
{
    Luau::Set<int> s1{0};
    s1.insert(1);
    s1.insert(2);
    s1.insert(3);
    REQUIRE(s1.size() == 3);

    int sum = 0;

    for (int e : s1)
        sum += e;

    CHECK(sum == 6);
}

TEST_CASE("iterate_over_set_skips_erased_elements")
{
    Luau::Set<int> s1{0};
    s1.insert(1);
    s1.insert(2);
    s1.insert(3);
    s1.insert(4);
    s1.insert(5);
    s1.insert(6);
    REQUIRE(s1.size() == 6);

    s1.erase(2);
    s1.erase(4);
    s1.erase(6);

    int sum = 0;

    for (int e : s1)
        sum += e;

    CHECK(sum == 9);
}

TEST_CASE("iterate_over_set_skips_first_element_if_it_is_erased")
{
    /*
     * As of this writing, in the following set, the key "y" happens to occur
     * before "x" in the underlying DenseHashSet.  This is important because it
     * surfaces something that Set::const_iterator needs to do: If the
     * underlying iterator happens to start at a deleted element, we need to
     * advance until we find the first live element (or the end of the set).
     */
    Luau::Set<std::string> s1{{}};
    s1.insert("x");
    s1.insert("y");
    s1.erase("y");

    std::vector<std::string> out;
    auto it = s1.begin();
    auto endIt = s1.end();
    while (it != endIt)
    {
        out.push_back(*it);
        ++it;
    }

    CHECK(1 == out.size());
}

TEST_CASE("erase_using_const_ref_argument")
{
    Luau::Set<std::string> s1{{}};

    s1.insert("x");
    s1.insert("y");

    std::string key = "y";
    s1.erase(key);

    CHECK(s1.count("x"));
    CHECK(!s1.count("y"));
}

TEST_SUITE_END();
