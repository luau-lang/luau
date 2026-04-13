// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "ScopedFlags.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauConst2)
LUAU_FASTFLAG(LuauConstJustReportErrorForUnderfill)

TEST_SUITE_BEGIN("ConstDeclarations");

TEST_CASE_FIXTURE(Fixture, "basic_declarations_work")
{
    ScopedFastFlag _{FFlag::LuauConst2, true};

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        const PI = 3.14
    )"));

    CHECK_EQ("number", toString(requireType("PI")));
}

TEST_CASE_FIXTURE(Fixture, "reassignments_dont_affect_type_state")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauForceOldSolver, false},
        {FFlag::LuauConst2, true},
    };

    CheckResult results = check(R"(
        const PI = 3.14
        PI = "apple"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, results);
    auto err = get<SyntaxError>(results.errors[0]);
    REQUIRE(err);
    CHECK_EQ("Assigned expression must be a variable or a field", err->message);
    CHECK_EQ("number", toString(requireType("PI")));
}


TEST_CASE_FIXTURE(Fixture, "empty_domain_is_ok")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauForceOldSolver, false},
        {FFlag::LuauConst2, true},
        // This test used to throw a compiler exception, this flag fixes it.
        {FFlag::LuauConstJustReportErrorForUnderfill, true},
    };

    CheckResult results = check(R"(
        const PI

        return PI
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, results);
    auto err = get<SyntaxError>(results.errors[0]);
    REQUIRE(err);
    CHECK_EQ("Missing initializer in const declaration", err->message);
    CHECK_EQ("nil", toString(requireType("PI")));
}

TEST_CASE_FIXTURE(Fixture, "const_extra_lvalues_are_nil_and_syntax_error_from_call")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauForceOldSolver, false},
        {FFlag::LuauConst2, true},
    };

    CheckResult results = check(R"(
        local function getparams(): (number, number)
            return 42, 13
        end

        const X, Y, Z = getparams()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, results);
    auto err = get<CountMismatch>(results.errors[0]);
    REQUIRE(err);
    CHECK_EQ(err->actual, 3);
    CHECK_EQ(err->expected,2);
    CHECK_EQ("number", toString(requireType("X")));
    CHECK_EQ("number", toString(requireType("Y")));
    CHECK_EQ("nil", toString(requireType("Z")));
}

TEST_CASE_FIXTURE(Fixture, "const_extra_lvalues_are_nil_and_syntax_error_from_underfill")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauForceOldSolver, false},
        {FFlag::LuauConst2, true},
        {FFlag::LuauConstJustReportErrorForUnderfill, true},
    };

    CheckResult results = check(R"(
        const X, Y, Z = 42, 13

        return { X, Y, Z }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, results);
    auto err = get<SyntaxError>(results.errors[0]);
    REQUIRE(err);
    // TODO: This error message could be more precise.
    CHECK_EQ("Missing initializer in const declaration", err->message);
}

TEST_CASE_FIXTURE(Fixture, "const_syntax_error_in_annotation")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauForceOldSolver, false},
        {FFlag::LuauConst2, true},
        // This test used to throw a compiler exception, this flag fixes it.
        {FFlag::LuauConstJustReportErrorForUnderfill, true},
    };

    std::ignore = check(R"(
        const foo: {
            bar
            baz
        } = {}

        return foo
    )");
}

TEST_CASE_FIXTURE(Fixture, "assign_different_values_to_const_x")
{
    ScopedFastFlag _{FFlag::LuauConst2, true};

    CheckResult result = check(R"(
        const x: string? = nil
        local a = x
        x = "hello!"
        local b = x
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err = get<SyntaxError>(result.errors[0]);
    REQUIRE(err);
    CHECK_EQ("Assigned expression must be a variable or a field", err->message);
    CHECK("string?" == toString(requireType("a")));
    CHECK("string?" == toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "const_recursive_function_works")
{
    ScopedFastFlag _{FFlag::LuauConst2, true};

    CheckResult result = check(R"(
        const function f(x)
            f(5)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    if (!FFlag::DebugLuauForceOldSolver)
        CHECK_EQ("(unknown) -> ()", toString(requireType("f")));
    else
        CHECK_EQ("(number) -> ()", toString(requireType("f")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "const_tables_are_still_mutable")
{
    ScopedFastFlag _{FFlag::LuauConst2, true};

    CheckResult result = check(R"(
        const TABLE = {}
        TABLE.foobar = "the fooest of bars!"
        TABLE.TAU = 6.12
        function TABLE.callback(x, y)
            print(math.abs(x), string.len(y))
            return true
        end

        return TABLE
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    if (FFlag::DebugLuauForceOldSolver)
        CHECK_EQ("{| TAU: number, callback: (number, string) -> boolean, foobar: string |}", toString(requireType("TABLE"), {/* exhaustive */ true}));
    else
        CHECK_EQ("{ TAU: number, callback: (number, string) -> boolean, foobar: string }", toString(requireType("TABLE"), {/* exhaustive */ true}));
}

TEST_CASE_FIXTURE(Fixture, "const_shadowing")
{
    ScopedFastFlag _{FFlag::LuauConst2, true};

    CheckResult result = check(R"(
        const X = "huh"
        const X = 3.14

        local y = X
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    // TODO CLI-197269: checking the types of `y` and `X` have different 
    // results on different platforms.
}

TEST_SUITE_END();