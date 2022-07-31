// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lluz/Error.h"

#include "doctest.h"

using namespace lluz;

TEST_SUITE_BEGIN(XorStr("ErrorTests"));

TEST_CASE("TypeError_code_should_return_nonzero_code")
{
    auto e = TypeError{{{0, 0}, {0, 1}}, UnknownSymbol{"Foo"}};
    CHECK_GE(e.code(), 1000);
}

TEST_SUITE_END();
