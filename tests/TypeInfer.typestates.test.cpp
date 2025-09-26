// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Fixture.h"

#include "doctest.h"

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauRefineDistributesOverUnions)
LUAU_FASTFLAG(LuauReduceSetTypeStackPressure)

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

        TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
        REQUIRE_MESSAGE(tm, "Expected TypeMismatch but got " << result.errors[0]);
        CHECK("string?" == toString(tm->wantedType));
        CHECK("number | string" == toString(tm->givenType));
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
    CHECK("number" == toString(requireType("y")));
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
    CHECK("string" == toString(requireType("y")));
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

TEST_CASE_FIXTURE(TypeStateFixture, "captured_locals_do_not_mutate_upvalue_type")
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

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err = get<TypeMismatch>(result.errors[0]);
    CHECK_EQ("number?", toString(err->wantedType));
    CHECK_EQ("string", toString(err->givenType));
    CHECK("number?" == toString(requireTypeAtPosition({4, 18})));
}

TEST_CASE_FIXTURE(TypeStateFixture, "captured_locals_do_not_mutate_upvalue_type_2")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauRefineDistributesOverUnions, true},
        {FFlag::LuauReduceSetTypeStackPressure, true},
    };

    CheckResult result = check(R"(
        local t = {x = nil}

        function f()
            print(t.x)
            t = {x = "five"}
        end

        t = {x = 5}
        f()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto err = get<TypeMismatch>(result.errors[0]);
    CHECK_EQ("number?", toString(err->wantedType));
    CHECK_EQ("string", toString(err->givenType));
    CHECK("{ x: number? }" == toString(requireTypeAtPosition({4, 18}), {true}));
    CHECK("number?" == toString(requireTypeAtPosition({4, 20})));
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
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
    };

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

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzzer_normalized_type_variables_are_bad" * doctest::timeout(0.5))
{
    // We do not care about the errors here, only that this finishes typing
    // in a sensible amount of time.
    LUAU_REQUIRE_ERRORS(check(R"(
        local _
        while _[""] do
            _, _ = nil
            while _.n0 do
                _, _ = nil
            end
            _, _ = nil
        end
        while _[""] do
            while if _ then if _ then _ else "" else "" do
                _, _ = nil
                do
                end
                _, _, _ = nil
            end
            _, _ = nil
            _, _, _ = nil
            while _.readi16 do
                _, _ = nil
            end
            _, _ = nil
        end
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "oss_1547_simple")
{
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local rand = 0

        function a()
            rand = (rand % 4) + 1;
        end
    )"));

    auto randTy = getType("rand");
    REQUIRE(randTy);
    CHECK_EQ("number", toString(*randTy));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "oss_1547")
{
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local rand = 0

        function a()
            rand = (rand % 4) + 1;
        end

        function b()
            rand = math.max(rand - 1, 0);
        end
    )"));

    auto randTy = getType("rand");
    REQUIRE(randTy);
    CHECK_EQ("number", toString(*randTy));
}

TEST_CASE_FIXTURE(Fixture, "modify_captured_table_field")
{
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local state = { x = 0 }
        function incr()
            state.x = state.x + 1
        end
    )"));

    auto randTy = getType("state");
    REQUIRE(randTy);
    if (FFlag::LuauSolverV2)
        CHECK_EQ("{ x: number }", toString(*randTy, {true}));
    else
        CHECK_EQ("{| x: number |}", toString(*randTy, {true}));
}

TEST_CASE_FIXTURE(Fixture, "oss_1561")
{
    loadDefinition(R"(
        declare class Vector3
            X: number
            Y: number
            Z: number
        end

        declare Vector3: {
            new: (number?, number?, number?) -> Vector3
        }
    )");

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local targetVelocity: Vector3 = Vector3.new()
        function set2D(X: number, Y: number)
            targetVelocity = Vector3.new(X, Y, targetVelocity.Z)
        end
    )"));

    CHECK_EQ("(number, number) -> ()", toString(requireType("set2D")));
}

TEST_CASE_FIXTURE(Fixture, "oss_1575")
{
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local flag = true
        local function Flip()
            flag = not flag
        end
    )"));
}

TEST_CASE_FIXTURE(Fixture, "capture_upvalue_in_returned_function")
{
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        function def()
            local i : number = 0
            local function Counter()
                i = i + 1
                return i
            end
            return Counter
        end
    )"));
    CHECK_EQ("() -> () -> number", toString(requireType("def")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "throw_in_else_branch")
{
    CheckResult result = check(R"(
        --!strict
        local x
        local coinflip : () -> boolean = (nil :: any)

        if coinflip () then
            x = "I win."
        else
            error("You lose.")
        end

        print(x)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string", toString(requireTypeAtPosition({11, 14})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "throw_in_if_branch")
{
    CheckResult result = check(R"(
        --!strict
        local x
        local coinflip : () -> boolean = (nil :: any)

        if coinflip () then
            error("You lose.")
        else
            x = "I win."
        end

        print(x)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string", toString(requireTypeAtPosition({11, 14})));
}


TEST_CASE_FIXTURE(BuiltinsFixture, "refinement_through_erroring")
{
    CheckResult result = check(R"(
        --!strict
        type Payload = { payload: number }

        local function decode(s: string): Payload?
            return (nil :: any)
        end

        local function decodeEx(s: string): Payload
            local p = decode(s)
            if not p then
                error("failed to decode payload!!!")
            end
            return p
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "refinement_through_erroring_in_loop")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        --!strict

        local x = nil

        while math.random() > 0.5 do
            x = 42
            return
        end

        print(x)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("nil", toString(requireTypeAtPosition({10, 14})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_refinement_in_loop")
{
    CheckResult result = check(R"(
        --!strict
        local function onEachString(t: { string | number })
            for _, v in t do
                if type(v) ~= "string" then
                    continue
                end
                print(v)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number | string", toString(requireTypeAtPosition({4, 24})));
    CHECK_EQ("string", toString(requireTypeAtPosition({7, 22})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "throw_in_if_branch_and_do_nothing_in_else")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        --!strict
        local x
        local coinflip : () -> boolean = (nil :: any)

        if coinflip () then
            error("You lose.")
        else
        end

        print(x)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("nil", toString(requireTypeAtPosition({10, 14})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "assign_in_an_if_branch_without_else")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        --!strict
        local x
        local coinflip : () -> boolean = (nil :: any)

        if coinflip () then
            x = "I win."
        end

        print(x)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string?", toString(requireTypeAtPosition({9, 14})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzzer_table_freeze_in_binary_expr")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};
    // CLI-154237: This currently throws an exception due to a mismatch between
    // the scopes created in the data flow graph versus the constraint generator.
    CHECK_THROWS_AS(
        check(R"(
            local _
            if _ or table.freeze(_,_) or table.freeze(_,_) then
            end
        )"),
        Luau::InternalCompilerError
    );
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_freeze_in_conditional")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};
    // NOTE: This _probably_ should be disallowed, but it is representing that
    // type stating functions in short circuiting binary expressions do not
    // reflect their type states.
    CheckResult result = check(R"(
        local t = { x = 42 }
        if math.random() > 0.5 and table.freeze(t) then
        end
        t.y = 13
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzzer_table_freeze_in_conditional_expr")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};
    // CLI-154237: This currently throws an exception due to a mismatch between
    // the scopes created in the data flow graph versus the constraint generator.
    CHECK_THROWS_AS(
        check(R"(
            local _
            if
                if table.freeze(_,_) then _ else _
            then
            end
        )"),
        Luau::InternalCompilerError
    );
}

TEST_SUITE_END();
