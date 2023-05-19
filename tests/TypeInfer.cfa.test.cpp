// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Fixture.h"

#include "Luau/Symbol.h"
#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("ControlFlowAnalysis");

TEST_CASE_FIXTURE(BuiltinsFixture, "if_not_x_return")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?)
            if not x then
                return
            end

            local foo = x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({6, 24})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "if_not_x_return_elif_not_y_return")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?, y: string?)
            if not x then
                return
            elseif not y then
                return
            end

            local foo = x
            local bar = y
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({8, 24})));
    CHECK_EQ("string", toString(requireTypeAtPosition({9, 24})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "if_not_x_return_elif_rand_return_elif_not_y_return")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?, y: string?)
            if not x then
                return
            elseif math.random() > 0.5 then
                return
            elseif not y then
                return
            end

            local foo = x
            local bar = y
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({10, 24})));
    CHECK_EQ("string", toString(requireTypeAtPosition({11, 24})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "if_not_x_return_elif_not_rand_return_elif_not_y_fallthrough")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?, y: string?)
            if not x then
                return
            elseif math.random() > 0.5 then
                return
            elseif not y then

            end

            local foo = x
            local bar = y
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({10, 24})));
    CHECK_EQ("string?", toString(requireTypeAtPosition({11, 24})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "if_not_x_return_elif_not_y_fallthrough_elif_not_z_return")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?, y: string?, z: string?)
            if not x then
                return
            elseif not y then

            elseif not z then
                return
            end

            local foo = x
            local bar = y
            local baz = z
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({10, 24})));
    CHECK_EQ("string?", toString(requireTypeAtPosition({11, 24})));
    CHECK_EQ("string?", toString(requireTypeAtPosition({12, 24})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "do_if_not_x_return")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?)
            do
                if not x then
                    return
                end
            end

            local foo = x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({8, 24})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "early_return_in_a_loop_which_isnt_guaranteed_to_run_first")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?)
            while math.random() > 0.5 do
                if not x then
                    return
                end

                local foo = x
            end

            local bar = x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({7, 28})));
    CHECK_EQ("string?", toString(requireTypeAtPosition({10, 24})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "early_return_in_a_loop_which_is_guaranteed_to_run_first")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?)
            repeat
                if not x then
                    return
                end

                local foo = x
            until math.random() > 0.5

            local bar = x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({7, 28})));
    CHECK_EQ("string?", toString(requireTypeAtPosition({10, 24}))); // TODO: This is wrong, should be `string`.
}

TEST_CASE_FIXTURE(BuiltinsFixture, "early_return_in_a_loop_which_is_guaranteed_to_run_first_2")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?)
            for i = 1, 10 do
                if not x then
                    return
                end

                local foo = x
            end

            local bar = x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({7, 28})));
    CHECK_EQ("string?", toString(requireTypeAtPosition({10, 24}))); // TODO: This is wrong, should be `string`.
}

TEST_CASE_FIXTURE(BuiltinsFixture, "if_not_x_then_error")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?)
            if not x then
                error("oops")
            end

            local foo = x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({6, 24})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "if_not_x_then_assert_false")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?)
            if not x then
                assert(false)
            end

            local foo = x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({6, 24})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "if_not_x_return_if_not_y_return")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?, y: string?)
            if not x then
                return
            end

            if not y then
                return
            end

            local foo = x
            local bar = y
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("string", toString(requireTypeAtPosition({10, 24})));
    CHECK_EQ("string", toString(requireTypeAtPosition({11, 24})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_does_not_leak_out")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?)
            if typeof(x) == "string" then
                return
            else
                type Foo = number
            end

            local foo: Foo = x
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Unknown type 'Foo'", toString(result.errors[0]));

    CHECK_EQ("nil", toString(requireTypeAtPosition({8, 29})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "prototyping_and_visiting_alias_has_the_same_scope")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    // In CGB, we walk the block to prototype aliases. We then visit the block in-order, which will resolve the prototype to a real type.
    // That second walk assumes that the name occurs in the same `Scope` that the prototype walk had. If we arbitrarily change scope midway
    // through, we'd invoke UB.
    CheckResult result = check(R"(
        local function f(x: string?)
            type Foo = number

            if typeof(x) == "string" then
                return
            end

            local foo: Foo = x
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Type 'nil' could not be converted into 'number'", toString(result.errors[0]));

    CHECK_EQ("nil", toString(requireTypeAtPosition({8, 29})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "tagged_unions")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        type Ok<T> = { tag: "ok", value: T }
        type Err<E> = { tag: "err", error: E }
        type Result<T, E> = Ok<T> | Err<E>

        local function map<T, U, E>(result: Result<T, E>, f: (T) -> U): Result<U, E>
            if result.tag == "ok" then
                local tag = result.tag
                local val = result.value

                return { tag = "ok", value = f(result.value) }
            end

            local tag = result.tag
            local err = result.error

            return result
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("\"ok\"", toString(requireTypeAtPosition({7, 35})));
    CHECK_EQ("T", toString(requireTypeAtPosition({8, 35})));

    CHECK_EQ("\"err\"", toString(requireTypeAtPosition({13, 31})));
    CHECK_EQ("E", toString(requireTypeAtPosition({14, 31})));

    CHECK_EQ("Err<E>", toString(requireTypeAtPosition({16, 19})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "do_assert_x")
{
    ScopedFastFlag sff{"LuauTinyControlFlowAnalysis", true};

    CheckResult result = check(R"(
        local function f(x: string?)
            do
                assert(x)
            end

            local foo = x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string", toString(requireTypeAtPosition({6, 24})));
}

TEST_SUITE_END();
