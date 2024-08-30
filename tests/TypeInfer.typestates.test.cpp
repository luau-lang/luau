// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Fixture.h"

#include "doctest.h"

LUAU_FASTFLAG(LuauSolverV2)

using namespace Luau;

namespace
{
struct TypeStateFixture : BuiltinsFixture
{
    ScopedFastFlag dcr{FFlag::LuauSolverV2, true};
};
} // namespace

TEST_SUITE_BEGIN("TypeStatesTest");

TEST_CASE_FIXTURE(TypeStateFixture, "initialize_x_of_type_string_or_nil_with_nil")
{
    CheckResult result = check(R"(
        local x: string? = nil
        local a = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("string?" == toString(requireType("a")));
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
    CHECK("string?" == toString(requireType("a")));
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

    if (FFlag::LuauSolverV2)
    {
        // `y` is annotated `string | number` which is explicitly not compatible with `string?`
        // as such, we produce an error here for that mismatch.
        //
        // this is not necessarily the best inference here, since we can indeed produce `string`
        // as a type for `x`, but it's a limitation we can accept for now.
        LUAU_REQUIRE_ERRORS(result);

        TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
        REQUIRE(tpm);
        CHECK("string?" == toString(tpm->wantedTp));
        CHECK("number | string" == toString(tpm->givenTp));

        CHECK("(number | string) -> string?" == toString(requireType("f")));
    }
    else
    {
        LUAU_REQUIRE_NO_ERRORS(result);

        CHECK("(string) -> string?" == toString(requireType("f")));
    }
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
#endif

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

TEST_CASE_FIXTURE(TypeStateFixture, "parameter_x_was_constrained_by_two_types_2")
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

TEST_CASE_FIXTURE(TypeStateFixture, "parameter_x_is_some_type_or_optional_then_assigned_with_alternate_value")
{
    CheckResult result = check(R"(
        local function f(x: number?)
            x = x or 5
            return x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("(number?) -> number" == toString(requireType("f")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "local_assigned_in_either_branches_that_falls_through")
{
    CheckResult result = check(R"(
        local x = nil
        if math.random() > 0.5 then
            x = 5
        else
            x = "hello"
        end
        local y = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("number | string" == toString(requireType("y")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "local_assigned_in_only_one_branch_that_falls_through")
{
    CheckResult result = check(R"(
        local x = nil
        if math.random() > 0.5 then
            x = 5
        end
        local y = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("number?" == toString(requireType("y")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "then_branch_assigns_and_else_branch_also_assigns_but_is_met_with_return")
{
    CheckResult result = check(R"(
        local x = nil
        if math.random() > 0.5 then
            x = 5
        else
            x = "hello"
            return
        end
        local y = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("number?" == toString(requireType("y")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "then_branch_assigns_but_is_met_with_return_and_else_branch_assigns")
{
    CheckResult result = check(R"(
        local x = nil
        if math.random() > 0.5 then
            x = 5
            return
        else
            x = "hello"
        end
        local y = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("string?" == toString(requireType("y")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "invalidate_type_refinements_upon_assignments")
{
    CheckResult result = check(R"(
        type Ok<T> = { tag: "ok", val: T }
        type Err<E> = { tag: "err", err: E }
        type Result<T, E> = Ok<T> | Err<E>

        local function f<T, E>(res: Result<T, E>)
            assert(res.tag == "ok")
            local tag: "ok", val: T = res.tag, res.val
            res = { tag = "err" :: "err", err = (5 :: any) :: E }
            local tag: "err", err: E = res.tag, res.err
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

#if 0
TEST_CASE_FIXTURE(TypeStateFixture, "local_t_is_assigned_a_fresh_table_with_x_assigned_a_union_and_then_assert_restricts_actual_outflow_of_types")
{
    CheckResult result = check(R"(
        local t = nil

        if math.random() > 0.5 then
            t = {}
            t.x = if math.random() > 0.5 then 5 else "hello"
            assert(typeof(t.x) == "string")
        else
            t = {}
            t.x = if math.random() > 0.5 then 7 else true
            assert(typeof(t.x) == "boolean")
        end

        local x = t.x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    // CHECK("boolean | string" == toString(requireType("x")));
    CHECK("boolean | number | number | string" == toString(requireType("x")));
}
#endif

TEST_CASE_FIXTURE(TypeStateFixture, "captured_locals_are_unions_of_all_assignments")
{
    CheckResult result = check(R"(
        local x = nil

        function f()
            print(x)
            x = "five"
        end

        x = 5
        f()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("(number | string)?" == toString(requireTypeAtPosition({4, 18})));
}

TEST_CASE_FIXTURE(TypeStateFixture, "captured_locals_are_unions_of_all_assignments_2")
{
    CheckResult result = check(R"(
        local t = {x = nil}

        function f()
            print(t.x)
            t = {x = "five"}
        end

        t = {x = 5}
        f()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("{ x: nil } | { x: number } | { x: string }" == toString(requireTypeAtPosition({4, 18}), {true}));
    CHECK("(number | string)?" == toString(requireTypeAtPosition({4, 20})));
}

TEST_CASE_FIXTURE(TypeStateFixture, "prototyped_recursive_functions")
{
    CheckResult result = check(R"(
        local f
        function f()
            if math.random() > 0.5 then
                f()
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("(() -> ())?" == toString(requireType("f")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "prototyped_recursive_functions_but_has_future_assignments")
{
    // early return if the flag isn't set since this is blocking gated commits
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local f
        function f()
            if math.random() > 0.5 then
                f()
            end
        end
        f = 5
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK("((() -> ()) | number)?" == toString(requireType("f")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "prototyped_recursive_functions_but_has_previous_assignments")
{
    CheckResult result = check(R"(
        local f
        f = 5
        function f()
            if math.random() > 0.5 then
                f()
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("((() -> ()) | number)?" == toString(requireType("f")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "multiple_assignments_in_loops")
{
    CheckResult result = check(R"(
        local x = nil

        for i = 1, 10 do
            x = 5
            x = "hello"
        end

        print(x)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("(number | string)?" == toString(requireType("x")));
}

TEST_CASE_FIXTURE(TypeStateFixture, "typestates_preserve_error_suppression")
{
    CheckResult result = check(R"(
        local a: any = 51
        a = "pickles" -- We'll have a new DefId for this iteration of `a`.  Its type must also be error-suppressing
        print(a)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("*error-type* | string" == toString(requireTypeAtPosition({3, 14}), {true}));
}


TEST_CASE_FIXTURE(BuiltinsFixture, "typestates_preserve_error_suppression_properties")
{
    // early return if the flag isn't set since this is blocking gated commits
    // unconditional return
    // CLI-117098 Type states with error suppressing properties doesn't infer the correct type for properties.
    if (!FFlag::LuauSolverV2 || FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local a: {x: any} = {x = 51}
        a.x = "pickles" -- We'll have a new DefId for this iteration of `a.x`.  Its type must also be error-suppressing
        print(a.x)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("*error-type* | string" == toString(requireTypeAtPosition({3, 16}), {true}));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typestates_do_not_apply_to_the_initial_local_definition")
{
    // early return if the flag isn't set since this is blocking gated commits
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyType = number | string
        local foo: MyType = 5
        print(foo)
        foo = 7
        print(foo)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("number | string" == toString(requireTypeAtPosition({3, 14}), {true}));
    CHECK("number" == toString(requireTypeAtPosition({5, 14}), {true}));
}

TEST_CASE_FIXTURE(Fixture, "typestate_globals")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    loadDefinition(R"(
        declare foo: string | number
        declare function f(x: string): ()
    )");

    CheckResult result = check(R"(
        foo = "a"
        f(foo)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "typestate_unknown_global")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        x = 5
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK(get<UnknownSymbol>(result.errors[0]));
}

TEST_SUITE_END();
