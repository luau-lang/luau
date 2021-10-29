// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Error.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("ErrorTests");

TEST_CASE("TypeError_code_should_return_nonzero_code")
{
    auto e = TypeError{{{0, 0}, {0, 1}}, UnknownSymbol{"Foo"}};
    CHECK_GE(e.code(), 1000);
}

TEST_SUITE_END();
