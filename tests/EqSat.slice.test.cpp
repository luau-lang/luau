// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include <doctest.h>

#include "Luau/Slice.h"

#include <vector>

using namespace Luau;

TEST_SUITE_BEGIN("EqSatSlice");

TEST_CASE("slice_is_a_view_over_array")
{
    std::array<int, 8> a{1, 2, 3, 4, 5, 6, 7, 8};

    EqSat::Slice<int> slice{a};

    CHECK(slice.data() == a.data());
    CHECK(slice.size() == a.size());

    for (size_t i = 0; i < a.size(); ++i)
    {
        CHECK(slice[i] == a[i]);
        CHECK(&slice[i] == &a[i]);
    }
}

TEST_CASE("slice_is_a_view_over_vector")
{
    std::vector<int> vector{1, 2, 3, 4, 5, 6, 7, 8};

    EqSat::Slice<int> slice{vector.data(), vector.size()};

    CHECK(slice.data() == vector.data());
    CHECK(slice.size() == vector.size());

    for (size_t i = 0; i < vector.size(); ++i)
    {
        CHECK(slice[i] == vector[i]);
        CHECK(&slice[i] == &vector[i]);
    }
}

TEST_CASE("mutate_via_slice")
{
    std::array<int, 2> a{1, 2};
    CHECK(a[0] == 1);
    CHECK(a[1] == 2);

    EqSat::Slice<int> slice{a};
    slice[0] = 42;
    slice[1] = 37;

    CHECK(a[0] == 42);
    CHECK(a[1] == 37);
}

TEST_SUITE_END();
