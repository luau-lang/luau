// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/DenseHash.h"

#include "doctest.h"

/** So... why are we picking a very specific number to fill the DenseHash(Map|Set)?
 *
 * That's because that's the count that happens to trigger a specific bug.
 *
 * DHT determines if it is full by checking whether the count is greater than or equal to 75% of
 * its capacity. Once this condition triggers, it rehashes everything by doubling its buffer.
 *
 * If DHT gets rehashed, the iterator gets invalidated, but it's very important that we support
 * the use case of overwriting/merging DHTs while iterating.
 */

TEST_SUITE_BEGIN("DenseHashTests");

TEST_CASE("overwriting_an_existing_field_when_full_shouldnt_rehash")
{
    // See the note at the top on why these numbers were chosen.

    Luau::DenseHashMap<int, int> m{-1};
    for (int i = 0; i < 12; ++i)
        m[i] = i;

    REQUIRE(m.size() == 12);

    for (auto [k, a] : m)
        m[k] = a + 1;

    for (size_t i = 0; i < m.size(); ++i)
    {
        int* a = m.find(int(i));
        REQUIRE(a);
        CHECK(i + 1 == *a);
    }
}

TEST_CASE("merging_another_map_and_resolve_conflicts_that_also_just_so_happens_to_rehash_while_iterating")
{
    // See the note at the top on why these numbers were chosen.

    Luau::DenseHashMap<int, int> m1{-1};
    for (int i = 0; i < 12; ++i)
        m1[i] = i;

    Luau::DenseHashMap<int, int> m2{-1};
    for (int i = 8; i < 24; ++i)
        m2[i] = i;

    REQUIRE(m1.size() == 12);
    REQUIRE(m2.size() == 16);

    for (auto [k, a] : m1)
    {
        if (int* b = m2.find(k))
            m1[k] = a + *b;
    }

    for (auto [k, a] : m2)
    {
        if (!m1.find(k))
            m1[k] = a;
    }

    REQUIRE(m1.size() == 24);
    for (size_t i = 0; i < m1.size(); ++i)
    {
        int* a = m1.find(int(i));
        REQUIRE(a);
        if (i < 8 || i >= 12)
            CHECK(i == *a);
        else
            CHECK(i + i == *a);
    }
}

TEST_SUITE_END();
