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
            {"DebugLuauDeferredConstraintResolution", true},
        };
        LoadDefinitionFileResult res = loadDefinition(definitions);
        LUAU_ASSERT(res.success);
        return check(Mode::Nonstrict, code);
    }

    std::string definitions = R"BUILTIN_SRC(
declare function @checked abs(n: number): number
)BUILTIN_SRC";
};


TEST_SUITE_BEGIN("NonStrictTypeCheckerTest");

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "simple_non_strict")
{
    auto res = checkNonStrict(R"BUILTIN_SRC(
abs("hi")
)BUILTIN_SRC");
    LUAU_REQUIRE_ERRORS(res);
    REQUIRE(res.errors.size() == 1);
    auto err = get<CheckedFunctionCallError>(res.errors[0]);
    REQUIRE(err != nullptr);
    REQUIRE(err->checkedFunctionName == "abs");
}

TEST_SUITE_END();
