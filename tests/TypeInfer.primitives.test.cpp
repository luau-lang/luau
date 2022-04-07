// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("TypeInferPrimitives");

TEST_CASE_FIXTURE(Fixture, "cannot_call_primitives")
{
    CheckResult result = check("local foo = 5    foo()");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    REQUIRE(get<CannotCallNonFunction>(result.errors[0]) != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "string_length")
{
    CheckResult result = check(R"(
        local s = "Hello, World!"
        local t = #s
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number", toString(requireType("t")));
}

TEST_CASE_FIXTURE(Fixture, "string_index")
{
    CheckResult result = check(R"(
        local s = "Hello, World!"
        local t = s[4]
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    NotATable* nat = get<NotATable>(result.errors[0]);
    REQUIRE(nat);
    CHECK_EQ("string", toString(nat->ty));

    CHECK_EQ("*unknown*", toString(requireType("t")));
}

TEST_CASE_FIXTURE(Fixture, "string_method")
{
    CheckResult result = check(R"(
        local p = ("tacos"):len()
    )");
    CHECK_EQ(0, result.errors.size());

    CHECK_EQ(*requireType("p"), *typeChecker.numberType);
}

TEST_CASE_FIXTURE(Fixture, "string_function_indirect")
{
    CheckResult result = check(R"(
        local s:string
        local l = s.lower
        local p = l(s)
    )");
    CHECK_EQ(0, result.errors.size());

    CHECK_EQ(*requireType("p"), *typeChecker.stringType);
}

TEST_CASE_FIXTURE(Fixture, "string_function_other")
{
    CheckResult result = check(R"(
        local s:string
        local p = s:match("foo")
    )");
    CHECK_EQ(0, result.errors.size());

    CHECK_EQ(toString(requireType("p")), "string?");
}

TEST_CASE_FIXTURE(Fixture, "CheckMethodsOfNumber")
{
    ScopedFastFlag sff{"LuauErrorRecoveryType", true};

    CheckResult result = check(R"(
local x: number = 9999
function x:y(z: number)
    local s: string = z
end
)");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ(toString(result.errors[0]), "Cannot add method to non-table type 'number'");
    CHECK_EQ(toString(result.errors[1]), "Type 'number' could not be converted into 'string'");
}

TEST_SUITE_END();
