// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Fixture.h"

#include "doctest.h"

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

using namespace Luau;

namespace
{
struct TypeStateFixture : BuiltinsFixture
{
    ScopedFastFlag dcr{"DebugLuauDeferredConstraintResolution", true};
};
}

TEST_SUITE_BEGIN("TypeStatesTest");

TEST_CASE_FIXTURE(TypeStateFixture, "initialize_x_of_type_string_or_nil_with_nil")
{
    CheckResult result = check(R"(
        local x: string? = nil
        local a = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("nil" == toString(requireType("a")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "extraneous_lvalues_are_populated_with_nil")
{
    CheckResult result = check(R"(
        local function f(): (string, number)
            return "hello", 5
        end

        local x, y, z = f()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK("Function only returns 2 values, but 3 are required here" == toString(result.errors[0]));
    CHECK("string" == toString(requireType("x")));
    CHECK("number" == toString(requireType("y")));
    CHECK("nil" == toString(requireType("z")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "assign_different_values_to_x")
{
    CheckResult result = check(R"(
        local x: string? = nil
        local a = x
        x = "hello!"
        local b = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("nil" == toString(requireType("a")));
    CHECK("string" == toString(requireType("b")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "parameter_x_was_constrained_by_two_types")
{
    // Parameter `x` has a fresh type `'x` bounded by `never` and `unknown`.
    // The first use of `x` constrains `x`'s upper bound by `string | number`.
    // The second use of `x`, aliased by `y`, constrains `x`'s upper bound by `string?`.
    // This results in `'x <: (string | number) & (string?)`.
    // The principal type of the upper bound is `string`.
    CheckResult result = check(R"(
        local function f(x): string?
            local y: string | number = x
            return y
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("(string) -> string?" == toString(requireType("f")));
}

#if 0
TEST_CASE_FIXTURE(TypeStateFixture, "local_that_will_be_assigned_later")
{
    CheckResult result = check(R"(
        local x: string
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(TypeStateFixture, "refine_a_local_and_then_assign_it")
{
    CheckResult result = check(R"(
        local function f(x: string?)
            if typeof(x) == "string" then
                x = nil
            end

            local y: nil = x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}
#endif

TEST_CASE_FIXTURE(TypeStateFixture, "assign_a_local_and_then_refine_it")
{
    CheckResult result = check(R"(
        local function f(x: string?)
            x = nil

            if typeof(x) == "string" then
                local y: typeof(x) = "hello"
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK("Type 'string' could not be converted into 'never'" == toString(result.errors[0]));
}

TEST_CASE_FIXTURE(TypeStateFixture, "recursive_local_function")
{
    CheckResult result = check(R"(
        local function f(x)
            f(5)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(TypeStateFixture, "recursive_function")
{
    CheckResult result = check(R"(
        function f(x)
            f(5)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(TypeStateFixture, "compound_assignment")
{
    CheckResult result = check(R"(
        local x = 5
        x += 7

        local a = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(TypeStateFixture, "assignment_identity")
{
    CheckResult result = check(R"(
        local x = 5
        x = x

        local a = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("number" == toString(requireType("a")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "assignment_swap")
{
    CheckResult result = check(R"(
        local x, y = 5, "hello"
        x, y = y, x

        local a, b = x, y
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("string" == toString(requireType("a")));
    CHECK("number" == toString(requireType("b")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "parameter_x_was_constrained_by_two_types")
{
    CheckResult result = check(R"(
        local function f(x): number?
            local y: string? = nil  -- 'y <: string?
            y = x                   -- 'y ~ 'x
            return y                -- 'y <: number?

                                    -- We therefore infer 'y <: (string | nil) & (number | nil)
                                    -- or 'y <: nil
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("(nil) -> number?" == toString(requireType("f")));
}

TEST_SUITE_END();
