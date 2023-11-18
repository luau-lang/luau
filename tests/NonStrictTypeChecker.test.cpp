// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/NonStrictTypeChecker.h"

#include "Fixture.h"

#include "Luau/Common.h"
#include "Luau/Ast.h"
#include "Luau/ModuleResolver.h"
#include "Luau/VisitType.h"
#include "ScopedFlags.h"
#include "doctest.h"
#include <iostream>

using namespace Luau;

#define NONSTRICT_REQUIRE_CHECKED_ERR(index, name, result) \
    do \
    { \
        REQUIRE(index < result.errors.size()); \
        auto err##index = get<CheckedFunctionCallError>(result.errors[index]); \
        REQUIRE(err##index != nullptr); \
        CHECK_EQ((err##index)->checkedFunctionName, name); \
    } while (false)

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
declare function @checked lower(s: string): string
declare function cond() : boolean
)BUILTIN_SRC";
};


TEST_SUITE_BEGIN("NonStrictTypeCheckerTest");

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "simple_non_strict_failure")
{
    CheckResult result = checkNonStrict(R"BUILTIN_SRC(
abs("hi")
)BUILTIN_SRC");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(0, "abs", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "nested_function_calls_constant")
{
    CheckResult result = checkNonStrict(R"(
local x
abs(lower(x))
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(0, "abs", result);
}


TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_warns_with_never_local")
{
    CheckResult result = checkNonStrict(R"(
local x : never
if cond() then
    abs(x)
else
    lower(x)
end
)");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(0, "abs", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(1, "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_warns_nil_branches")
{
    auto result = checkNonStrict(R"(
local x
if cond() then
    abs(x)
else
    lower(x)
end
)");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(0, "abs", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(1, "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_doesnt_warn_else_branch")
{
    auto result = checkNonStrict(R"(
local x : string = "hi"
if cond() then
    abs(x)
else
    lower(x)
end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(0, "abs", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_no_else")
{
    CheckResult result = checkNonStrict(R"(
local x : string
if cond() then
    abs(x)
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_no_else_err_in_cond")
{
    CheckResult result = checkNonStrict(R"(
local x : string
if abs(x) then
    lower(x)
end
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(0, "abs", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_expr_should_warn")
{
    CheckResult result = checkNonStrict(R"(
local x : never
local y = if cond() then abs(x) else lower(x)
)");
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(0, "abs", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(1, "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_expr_doesnt_warn_else_branch")
{
    CheckResult result = checkNonStrict(R"(
local x : string = "hi"
local y = if cond() then abs(x) else lower(x)
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(0, "abs", result);
}


TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "sequencing_errors")
{
    CheckResult result = checkNonStrict(R"(
function f(x)
    abs(x)
    lower(x)
end
)");
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(0, "abs", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(1, "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "sequencing_if_checked_call")
{
    CheckResult result = checkNonStrict(R"(
local x
if cond() then
  x = 5
else
  x = nil
end
lower(x)
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(0, "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "sequencing_unrelated_checked_calls")
{
    CheckResult result = checkNonStrict(R"(
function h(x, y)
    abs(x)
    lower(y)
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}


TEST_SUITE_END();
