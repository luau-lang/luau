// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/NonStrictTypeChecker.h"

#include "Fixture.h"

#include "Luau/Ast.h"
#include "Luau/Common.h"
#include "Luau/IostreamHelpers.h"
#include "Luau/ModuleResolver.h"
#include "Luau/VisitType.h"

#include "ScopedFlags.h"
#include "doctest.h"
#include <iostream>

using namespace Luau;

LUAU_FASTFLAG(LuauCheckedFunctionSyntax);

#define NONSTRICT_REQUIRE_ERR_AT_POS(pos, result, idx) \
    do \
    { \
        auto pos_ = (pos); \
        bool foundErr = false; \
        int index = 0; \
        for (const auto& err : result.errors) \
        { \
            if (err.location.begin == pos_) \
            { \
                foundErr = true; \
                break; \
            } \
            index++; \
        } \
        REQUIRE_MESSAGE(foundErr, "Expected error at " << pos_); \
        idx = index; \
    } while (false)

#define NONSTRICT_REQUIRE_CHECKED_ERR(pos, name, result) \
    do \
    { \
        int errIndex; \
        NONSTRICT_REQUIRE_ERR_AT_POS(pos, result, errIndex); \
        auto err = get<CheckedFunctionCallError>(result.errors[errIndex]); \
        REQUIRE(err != nullptr); \
        CHECK_EQ(err->checkedFunctionName, name); \
    } while (false)

#define NONSTRICT_REQUIRE_FUNC_DEFINITION_ERR(pos, argname, result) \
    do \
    { \
        int errIndex; \
        NONSTRICT_REQUIRE_ERR_AT_POS(pos, result, errIndex); \
        auto err = get<NonStrictFunctionDefinitionError>(result.errors[errIndex]); \
        REQUIRE(err != nullptr); \
        CHECK_EQ(err->argument, argname); \
    } while (false)


struct NonStrictTypeCheckerFixture : Fixture
{

    NonStrictTypeCheckerFixture()
    {
        registerHiddenTypes(&frontend);
    }

    CheckResult checkNonStrict(const std::string& code)
    {
        ScopedFastFlag flags[] = {
            {FFlag::LuauCheckedFunctionSyntax, true},
            {FFlag::DebugLuauDeferredConstraintResolution, true},
        };
        LoadDefinitionFileResult res = loadDefinition(definitions);
        LUAU_ASSERT(res.success);
        return check(Mode::Nonstrict, code);
    }

    std::string definitions = R"BUILTIN_SRC(
declare function @checked abs(n: number): number
declare function @checked lower(s: string): string
declare function cond() : boolean
declare function @checked contrived(n : Not<number>) : number

-- interesting types of things that we would like to mark as checked
declare function @checked onlyNums(...: number) : number
declare function @checked mixedArgs(x: string, ...: number) : number
declare function @checked optionalArg(x: string?) : number
)BUILTIN_SRC";
};

TEST_SUITE_BEGIN("NonStrictTypeCheckerTest");

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "interesting_checked_functions")
{
    CheckResult result = checkNonStrict(R"(
onlyNums(1,1,1)
onlyNums(1, "a")

mixedArgs("a", 1, 2)
mixedArgs(1, 1, 1)
mixedArgs("a", true)

optionalArg(nil)
optionalArg("a")
optionalArg(3)
)");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 12), "onlyNums", result); // onlyNums(1, "a")

    NONSTRICT_REQUIRE_CHECKED_ERR(Position(5, 10), "mixedArgs", result); // mixedArgs(1, 1, 1)
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(6, 15), "mixedArgs", result); // mixedArgs("a", true)

    NONSTRICT_REQUIRE_CHECKED_ERR(Position(10, 12), "optionalArg", result); // optionalArg(3)
}


TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "simple_negation_caching_example")
{
    CheckResult result = checkNonStrict(R"(
local x = 3
abs(x)
abs(x)
)");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = checkNonStrict(R"(
local x = 3
contrived(x)
contrived(x)
			      )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 10), "contrived", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 10), "contrived", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "simple_non_strict_failure")
{
    CheckResult result = checkNonStrict(R"BUILTIN_SRC(
abs("hi")
)BUILTIN_SRC");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(1, 4), "abs", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "nested_function_calls_constant")
{
    CheckResult result = checkNonStrict(R"(
local x
abs(lower(x))
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 4), "abs", result);
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
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 8), "abs", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(5, 10), "lower", result);
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
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 8), "abs", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(5, 10), "lower", result);
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
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 8), "abs", result);
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
local x : string = ""
if abs(x) then
    lower(x)
end
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 7), "abs", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_expr_should_warn")
{
    CheckResult result = checkNonStrict(R"(
local x : never
local y = if cond() then abs(x) else lower(x)
)");
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 29), "abs", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 43), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_expr_doesnt_warn_else_branch")
{
    CheckResult result = checkNonStrict(R"(
local x : string = "hi"
local y = if cond() then abs(x) else lower(x)
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 29), "abs", result);
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
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(7, 6), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_unrelated_checked_calls")
{
    CheckResult result = checkNonStrict(R"(
function h(x, y)
    abs(x)
    lower(y)
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_basic_no_errors")
{
    CheckResult result = checkNonStrict(R"(
function f(x)
    abs(x)
end
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_basic_errors")
{
    CheckResult result = checkNonStrict(R"(
function f(x : string)
    abs(x)
end
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 8), "abs", result);
}


TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_failure")
{
    CheckResult result = checkNonStrict(R"(
function f(x)
    abs(lower(x))
end
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 8), "abs", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_sequencing_errors")
{
    CheckResult result = checkNonStrict(R"(
function f(x)
    abs(x)
    lower(x)
end
)");
    LUAU_REQUIRE_ERROR_COUNT(3, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 8), "abs", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 10), "lower", result);
    NONSTRICT_REQUIRE_FUNC_DEFINITION_ERR(Position(1, 11), "x", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "local_fn_produces_error")
{
    CheckResult result = checkNonStrict(R"(
local x = 5
local function y() lower(x) end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 25), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "fn_expr_produces_error")
{
    CheckResult result = checkNonStrict(R"(
local x = 5
local y = function() lower(x) end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 27), "lower", result);
}


TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_if_warns_never")
{
    CheckResult result = checkNonStrict(R"(
function f(x)
    if cond() then
        abs(x)
    else
        lower(x)
    end
end
)");
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 12), "abs", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(5, 14), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_if_no_else")
{
    CheckResult result = checkNonStrict(R"(
function f(x)
    if cond() then
        abs(x)
    end
end
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_if_assignment_errors")
{
    CheckResult result = checkNonStrict(R"(
function f(x)
    if cond() then
        x = 5
    else
        x = nil
    end
    lower(x)
end
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(7, 10), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_if_assignment_no_errors")
{
    CheckResult result = checkNonStrict(R"(
function f(x : string | number)
    if cond() then
        x = 5
    else
        x = "hi"
    end
    abs(x)
end
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "local_only_one_warning")
{
    CheckResult result = checkNonStrict(R"(
local x = 5
lower(x)
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 6), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "phi_node_assignment")
{
    CheckResult result = checkNonStrict(R"(
local x = "a" -- x1
if cond() then
    x = 3 -- x2
end
lower(x) -- phi {x1, x2}
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "phi_node_assignment_err")
{
    CheckResult result = checkNonStrict(R"(
local x = nil
if cond() then
    if cond() then
        x = 5
    end
    abs(x)
else
    lower(x)
end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(8, 10), "lower", result);
}

TEST_SUITE_END();
