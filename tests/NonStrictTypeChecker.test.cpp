// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/NonStrictTypeChecker.h"

#include "Fixture.h"

#include "Luau/Common.h"
#include "Luau/Ast.h"
#include "Luau/ModuleResolver.h"
#include "ScopedFlags.h"
#include "doctest.h"
#include <iostream>

using namespace Luau;

struct NonStrictTypeCheckerFixture : Fixture
{

    CheckResult checkNonStrict(const std::string& code)
    {
        ScopedFastFlag flags[] = {
            {"LuauCheckedFunctionSyntax", true},
        };

        return check(Mode::Nonstrict, code);
    }

    std::string definitions = R"BUILTIN_SRC(
declare function @checked abs(n: number): number
abs("hi")
)BUILTIN_SRC";
};


TEST_SUITE_BEGIN("NonStrictTypeCheckerTest");

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "simple_non_strict")
{
    auto res = checkNonStrict(R"BUILTIN_SRC(
declare function @checked abs(n: number): number
abs("hi")
)BUILTIN_SRC");

    // LUAU_REQUIRE_ERRORS(res);
}

TEST_SUITE_END();
