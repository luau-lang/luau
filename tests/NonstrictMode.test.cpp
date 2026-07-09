// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "ScopedFlags.h"
#include "doctest.h"

#include <algorithm>

using namespace Luau;


LUAU_FASTFLAG(DebugLuauMagicTypes)
LUAU_FASTFLAG(DebugLuauForceOldSolver)

TEST_SUITE_BEGIN("NonstrictModeTests");

/**
 * NOTE: In the new solver, non-strict uses the same type inference logic
 * as strict mode, but has a different error checking strategy. In the
 * old solver we used `any` in some unannotated positions.
 */

TEST_CASE_FIXTURE(Fixture, "infer_nullary_function")
{
    CheckResult result = check(R"(
        --!nonstrict
        function foo(x, y) end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    TypeId fooType = requireType("foo");
    REQUIRE(fooType);

    if (!FFlag::DebugLuauForceOldSolver)
        CHECK_EQ("(unknown, unknown) -> ()", toString(fooType));
    else
        CHECK_EQ("(any, any) -> (...any)", toString(fooType));
}

TEST_CASE_FIXTURE(Fixture, "infer_the_maximum_number_of_values_the_function_could_return")
{
    CheckResult result = check(R"(
        --!nonstrict
        function getMinCardCountForWidth(width)
            if width < 513 then
                return 3
            else
                return 8, 'jellybeans'
            end
        end
    )");

    TypeId t = requireType("getMinCardCountForWidth");
    REQUIRE(t);

    if (!FFlag::DebugLuauForceOldSolver)
        CHECK_EQ("(number) -> number", toString(t));
    else
        CHECK_EQ("(any) -> (...any)", toString(t));
}

TEST_CASE_FIXTURE(Fixture, "return_annotation_is_still_checked")
{
    CheckResult result = check(R"(
        function foo(x): number return 'hello' end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK("any" != toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "function_parameters_are_any")
{
    CheckResult result = check(R"(
        --!nonstrict
        function f(arg)
            arg = 9
            arg:concat(4)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "inconsistent_return_types_are_ok")
{
    CheckResult result = check(R"(
        --!nonstrict
        function f()
            if 1 then
                return 4
            else
                return 'hello'
            end
            return 'one', 'two'
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "locals_are_any_by_default")
{
    CheckResult result = check(R"(
        --!nonstrict
        local m = 55
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (!FFlag::DebugLuauForceOldSolver)
        CHECK("number" == toString(requireType("m"), {true}));
    else
        CHECK("any" == toString(requireType("m")));
}

TEST_CASE_FIXTURE(Fixture, "parameters_having_type_any_are_optional")
{
    CheckResult result = check(R"(
        --!nonstrict
        local function f(a, b)
            return a
        end

        f(5)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "local_tables_are_not_any")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();
    CheckResult result = check(R"(
        --!nonstrict
        local T = {}
        function T:method() end
        function T.staticmethod() end

        T.method()
        T:staticmethod()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("This function does not take self. Did you mean to use a dot instead of a colon?", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "offer_a_hint_if_you_use_a_dot_instead_of_a_colon")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();
    CheckResult result = check(R"(
        --!nonstrict
        local T = {}
        function T:method(x: number) end
        T.method(5)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("This function must be called with self. Did you mean to use a colon instead of a dot?", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "table_props_are_any")
{
    CheckResult result = check(R"(
        --!nonstrict
        local T = {}
        T.foo = 55
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (!FFlag::DebugLuauForceOldSolver)
        CHECK_EQ("{ foo: number }", toString(requireType("T"), {true}));
    else
        CHECK_EQ("{| foo: any |}", toString(requireType("T"), {true}));
}

TEST_CASE_FIXTURE(Fixture, "inline_table_props_are_also_any")
{
    CheckResult result = check(R"(
        --!nonstrict
        local T = {
            one = 1,
            two = 'two',
            three = function() return 3 end
        }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (!FFlag::DebugLuauForceOldSolver)
        CHECK_EQ("{ one: number, three: () -> number, two: string }", toString(requireType("T"), {true}));
    else
        CHECK_EQ("{| one: any, three: () -> (...any), two: any |}", toString(requireType("T"), {true}));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "for_in_iterator_variables_are_any")
{
    CheckResult result = check(R"(
        --!nonstrict
        function requires_a_table(arg: {}) end
        function requires_a_number(arg: number) end

        local T = {}
        for a, b in pairs(T) do
            requires_a_table(a)
            requires_a_table(b)
            requires_a_number(a)
            requires_a_number(b)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_dot_insert_and_recursive_calls")
{
    CheckResult result = check(R"(
        --!nonstrict
        function populateListFromIds(list, normalizedData)
            local newList = {}

            for _, value in ipairs(list) do
                if type(value) == "table" then
                    table.insert(newList, populateListFromIds(value, normalizedData))
                else
                    table.insert(newList, normalizedData[value])
                end
            end

            return newList
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "delay_function_does_not_require_its_argument_to_return_anything")
{
    CheckResult result = check(R"(
        --!nonstrict

        function delay(ms: number?, cb: () -> ()): () end

        delay(50, function() end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "inconsistent_module_return_types_are_ok")
{
    CheckResult result = check(R"(
        --!nonstrict

        local FFlag: any

        if FFlag.get('SomeFlag') then
            return {foo='bar'}
        else
            return function(prop)
                return 'bar'
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (!FFlag::DebugLuauForceOldSolver)
        // The new solver just picks the "first" return type.
        REQUIRE_EQ("{ foo: string }", toString(getMainModule()->returnType));
    else
        REQUIRE_EQ("any", toString(getMainModule()->returnType));
}

TEST_CASE_FIXTURE(Fixture, "returning_insufficient_return_values")
{
    CheckResult result = check(R"(
        --!nonstrict

        function foo(): (boolean, string?)
            if true then
                return true, "hello"
            else
                return false
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "returning_too_many_values")
{
    CheckResult result = check(R"(
        --!nonstrict

        function foo(): boolean
            if true then
                return true, "hello"
            else
                return false
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "standalone_constraint_solving_incomplete_is_hidden_nonstrict")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauForceOldSolver, false},
        {FFlag::DebugLuauMagicTypes, true},
        // This debug flag is normally on, but we turn it off as we're testing
        // the exact behavior it enables.
        {FFlag::DebugLuauAlwaysShowConstraintSolvingIncomplete, false},
    };

    CheckResult results = check(R"(
        --!nonstrict
        local function _f(_x: _luau_force_constraint_solving_incomplete) end
    )");

    LUAU_REQUIRE_NO_ERRORS(results);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "non_standalone_constraint_solving_incomplete_is_hidden_nonstrict")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauForceOldSolver, false},
        {FFlag::DebugLuauMagicTypes, true},
    };

    CheckResult results = check(R"(
        --!nonstrict
        local function _f(_x: _luau_force_constraint_solving_incomplete) end
        math.abs("pls")
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, results);
    CHECK(get<CheckedFunctionCallError>(results.errors[0]));
    CHECK(get<ConstraintSolvingIncompleteError>(results.errors[1]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "allow_error_type_nonstrict")
{
    LUAU_REQUIRE_NO_ERRORS(check(Mode::Nonstrict, R"(
        local sublist: any
        if sublist then
            for _, entry in sublist do
                local _ = string.upper(entry)
            end
        end
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "error_in_union_suppresses")
{
    LUAU_REQUIRE_NO_ERRORS(check(Mode::Nonstrict, R"(
        local sublist: any
        if sublist then
            local subitem = sublist.item
            local _ = string.upper(subitem)
        end
    )"));
}

TEST_SUITE_END();
