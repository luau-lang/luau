// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"
#include "Luau/VisitType.h"

#include "Fixture.h"
#include "DiffAsserts.h"

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
    CHECK_EQ_DIFF(builtinTypes->numberType, requireType("t"));
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

    CHECK_EQ("*error-type*", toString(requireType("t")));
}

TEST_CASE_FIXTURE(Fixture, "string_method")
{
    CheckResult result = check(R"(
        local p = ("tacos"):len()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*requireType("p"), *builtinTypes->numberType);
}

TEST_CASE_FIXTURE(Fixture, "string_function_indirect")
{
    CheckResult result = check(R"(
        local s:string
        local l = s.lower
        local p = l(s)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*requireType("p"), *builtinTypes->stringType);
}

TEST_CASE_FIXTURE(Fixture, "CheckMethodsOfNumber")
{
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

TEST_CASE("singleton_types")
{
    BuiltinsFixture a;

    {
        BuiltinsFixture b;
    }

    // Check that Frontend 'a' environment wasn't modified by 'b'
    CheckResult result = a.check("local s: string = 'hello' local t = s:lower()");

    CHECK(result.errors.empty());
}

TEST_SUITE_END();
