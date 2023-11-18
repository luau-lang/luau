// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/InsertionOrderedMap.h"

#include <memory>

#include "doctest.h"

using namespace Luau;

struct MapFixture
{
    std::vector<std::unique_ptr<int>> ptrs;

    int* makePtr()
    {
        ptrs.push_back(std::make_unique<int>(int{}));
        return ptrs.back().get();
    }
};

TEST_SUITE_BEGIN("InsertionOrderedMap");

TEST_CASE_FIXTURE(MapFixture, "map_insertion")
{
    InsertionOrderedMap<int*, int> map;

    int* a = makePtr();
    int* b = makePtr();

    map.insert(a, 1);
    map.insert(b, 2);
}

TEST_CASE_FIXTURE(MapFixture, "map_lookup")
{
    InsertionOrderedMap<int*, int> map;

    int* a = makePtr();
    map.insert(a, 1);

    int* r = map.get(a);
    REQUIRE(r != nullptr);
    CHECK(*r == 1);

    r = map.get(makePtr());
    CHECK(r == nullptr);
}

TEST_CASE_FIXTURE(MapFixture, "insert_does_not_update")
{
    InsertionOrderedMap<int*, int> map;

    int* k = makePtr();
    map.insert(k, 1);
    map.insert(k, 2);

    int* v = map.get(k);
    REQUIRE(v != nullptr);
    CHECK(*v == 1);
}

TEST_CASE_FIXTURE(MapFixture, "insertion_order_is_iteration_order")
{
    // This one is a little hard to prove, in that if the ordering guarantees
    // fail this test isn't guaranteed to fail, but it is strictly better than
    // nothing.

    InsertionOrderedMap<int*, int> map;
    int* a = makePtr();
    int* b = makePtr();
    int* c = makePtr();
    map.insert(a, 1);
    map.insert(b, 1);
    map.insert(c, 1);

    auto it = map.begin();
    REQUIRE(it != map.end());
    CHECK(it->first == a);
    CHECK(it->second == 1);

    ++it;
    REQUIRE(it != map.end());
    CHECK(it->first == b);
    CHECK(it->second == 1);

    ++it;
    REQUIRE(it != map.end());
    CHECK(it->first == c);
    CHECK(it->second == 1);

    ++it;
    CHECK(it == map.end());
}

TEST_CASE_FIXTURE(MapFixture, "destructuring_iterator_compiles")
{
    // This test's only purpose is to successfully compile.
    InsertionOrderedMap<int*, int> map;

    for (auto [k, v] : map)
    {
        // Checks here solely to silence unused variable warnings.
        CHECK(k);
        CHECK(v > 0);
    }
}

TEST_CASE_FIXTURE(MapFixture, "map_erasure")
{
    InsertionOrderedMap<int*, int> map;

    int* a = makePtr();
    int* b = makePtr();

    map.insert(a, 1);
    map.insert(b, 2);

    map.erase(map.find(a));
    CHECK(map.size() == 1);
    CHECK(!map.contains(a));
    CHECK(map.get(a) == nullptr);

    int* v = map.get(b);
    REQUIRE(v);
}

TEST_CASE_FIXTURE(MapFixture, "map_clear")
{
    InsertionOrderedMap<int*, int> map;
    int* a = makePtr();

    map.insert(a, 1);

    map.clear();
    CHECK(map.size() == 0);
    CHECK(!map.contains(a));
    CHECK(map.get(a) == nullptr);
}

TEST_SUITE_END();
