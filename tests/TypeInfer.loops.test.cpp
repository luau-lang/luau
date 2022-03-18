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

TEST_SUITE_BEGIN("TypeInferLoops");

TEST_CASE_FIXTURE(Fixture, "for_loop")
{
    CheckResult result = check(R"(
        local q
        for i=0, 50, 2 do
            q = i
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.numberType, *requireType("q"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop")
{
    CheckResult result = check(R"(
        local n
        local s
        for i, v in pairs({ "foo" }) do
            n = i
            s = v
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.numberType, *requireType("n"));
    CHECK_EQ(*typeChecker.stringType, *requireType("s"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_with_next")
{
    CheckResult result = check(R"(
        local n
        local s
        for i, v in next, { "foo", "bar" } do
            n = i
            s = v
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.numberType, *requireType("n"));
    CHECK_EQ(*typeChecker.stringType, *requireType("s"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_with_an_iterator_of_type_any")
{
    CheckResult result = check(R"(
        local it: any
        local a, b
        for i, v in it do
            a, b = i, v
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_should_fail_with_non_function_iterator")
{
    CheckResult result = check(R"(
        local foo = "bar"
        for i, v in foo do
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "for_in_with_just_one_iterator_is_ok")
{
    CheckResult result = check(R"(
        local function keys(dictionary)
            local new = {}
            local index = 1

            for key in pairs(dictionary) do
                new[index] = key
                index = index + 1
            end

            return new
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "for_in_with_a_custom_iterator_should_type_check")
{
    CheckResult result = check(R"(
        local function range(l, h): () -> number
            return function()
                return l
            end
        end

        for n: string in range(1, 10) do
            print(n)
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_on_error")
{
    CheckResult result = check(R"(
        function f(x)
            gobble.prop = x.otherprop
        end

        local p
        for _, part in i_am_not_defined do
            p = part
            f(part)
            part.thirdprop = false
        end
    )");

    CHECK_EQ(2, result.errors.size());

    TypeId p = requireType("p");
    CHECK_EQ("*unknown*", toString(p));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_on_non_function")
{
    CheckResult result = check(R"(
        local bad_iter = 5

        for a in bad_iter() do
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    REQUIRE(get<CannotCallNonFunction>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_error_on_factory_not_returning_the_right_amount_of_values")
{
    CheckResult result = check(R"(
        local function hasDivisors(value: number, table)
            return false
        end

        function prime_iter(state, index)
            while hasDivisors(index, state) do
                index += 1
            end

            state[index] = true
            return index
        end

        function primes1()
            return prime_iter, {}
        end

        function primes2()
            return prime_iter, {}, ""
        end

        function primes3()
            return prime_iter, {}, 2
        end

        for p in primes1() do print(p) end -- mismatch in argument count

        for p in primes2() do print(p) end -- mismatch in argument types, prime_iter takes {}, number, we are given {}, string

        for p in primes3() do print(p) end -- no error
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CountMismatch* acm = get<CountMismatch>(result.errors[0]);
    REQUIRE(acm);
    CHECK_EQ(acm->context, CountMismatch::Arg);
    CHECK_EQ(2, acm->expected);
    CHECK_EQ(1, acm->actual);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[1]);
    REQUIRE(tm);
    CHECK_EQ(typeChecker.numberType, tm->wantedType);
    CHECK_EQ(typeChecker.stringType, tm->givenType);
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_error_on_iterator_requiring_args_but_none_given")
{
    CheckResult result = check(R"(
        function prime_iter(state, index)
            return 1
        end

        for p in prime_iter do print(p) end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CountMismatch* acm = get<CountMismatch>(result.errors[0]);
    REQUIRE(acm);
    CHECK_EQ(acm->context, CountMismatch::Arg);
    CHECK_EQ(2, acm->expected);
    CHECK_EQ(0, acm->actual);
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_with_custom_iterator")
{
    CheckResult result = check(R"(
        function primes()
            return function (state: number) end,  2
        end

        for p, q in primes do
            q = ""
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(typeChecker.numberType, tm->wantedType);
    CHECK_EQ(typeChecker.stringType, tm->givenType);
}

TEST_CASE_FIXTURE(Fixture, "while_loop")
{
    CheckResult result = check(R"(
        local i
        while true do
            i = 8
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.numberType, *requireType("i"));
}

TEST_CASE_FIXTURE(Fixture, "repeat_loop")
{
    CheckResult result = check(R"(
        local i
        repeat
            i = 'hi'
        until true
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.stringType, *requireType("i"));
}

TEST_CASE_FIXTURE(Fixture, "repeat_loop_condition_binds_to_its_block")
{
    CheckResult result = check(R"(
        repeat
            local x = true
        until x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "symbols_in_repeat_block_should_not_be_visible_beyond_until_condition")
{
    CheckResult result = check(R"(
        repeat
            local x = true
        until x

        print(x)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "varlist_declared_by_for_in_loop_should_be_free")
{
    CheckResult result = check(R"(
        local T = {}

        function T.f(p)
            for i, v in pairs(p) do
                T.f(v)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "properly_infer_iteratee_is_a_free_table")
{
    // In this case, we cannot know the element type of the table {}.  It could be anything.
    // We therefore must initially ascribe a free typevar to iter.
    CheckResult result = check(R"(
        for iter in pairs({}) do
            iter:g().p = true
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "correctly_scope_locals_while")
{
    CheckResult result = check(R"(
        while true do
            local a = 1
        end

        print(a) -- oops!
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownSymbol* us = get<UnknownSymbol>(result.errors[0]);
    REQUIRE(us);
    CHECK_EQ(us->name, "a");
}

TEST_CASE_FIXTURE(Fixture, "ipairs_produces_integral_indices")
{
    CheckResult result = check(R"(
        local key
        for i, e in ipairs({}) do key = i end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    REQUIRE_EQ("number", toString(requireType("key")));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_where_iteratee_is_free")
{
    // This code doesn't pass typechecking.  We just care that it doesn't crash.
    (void)check(R"(
        --!nonstrict
        function _:_(...)
        end

        repeat
            if _ then
            else
                _ = ...
            end
        until _

        for _ in _() do
        end
    )");
}

TEST_CASE_FIXTURE(Fixture, "unreachable_code_after_infinite_loop")
{
    {
        CheckResult result = check(R"(
            function unreachablecodepath(a): number
                while true do
                    if a then return 10 end
                end
                -- unreachable
            end
            unreachablecodepath(4)
        )");

        LUAU_REQUIRE_ERROR_COUNT(0, result);
    }

    {
        CheckResult result = check(R"(
            function reachablecodepath(a): number
                while true do
                    if a then break end
                    return 10
                end

                print("x") -- correct error
            end
            reachablecodepath(4)
        )");

        LUAU_REQUIRE_ERRORS(result);
        CHECK(get<FunctionExitsWithoutReturning>(result.errors[0]));
    }

    {
        CheckResult result = check(R"(
            function unreachablecodepath(a): number
                repeat
                    if a then return 10 end
                until false

                -- unreachable
            end
            unreachablecodepath(4)
        )");

        LUAU_REQUIRE_ERROR_COUNT(0, result);
    }

    {
        CheckResult result = check(R"(
            function reachablecodepath(a, b): number
                repeat
                    if a then break end

                    if b then return 10 end
                until false

                print("x") -- correct error
            end
            reachablecodepath(4)
        )");

        LUAU_REQUIRE_ERRORS(result);
        CHECK(get<FunctionExitsWithoutReturning>(result.errors[0]));
    }

    {
        CheckResult result = check(R"(
            function unreachablecodepath(a: number?): number
                repeat
                    return 10
                until a ~= nil

                -- unreachable
            end
            unreachablecodepath(4)
        )");

        LUAU_REQUIRE_ERROR_COUNT(0, result);
    }
}

TEST_CASE_FIXTURE(Fixture, "loop_typecheck_crash_on_empty_optional")
{
    CheckResult result = check(R"(
        local t = {}
        for _ in t do
            for _ in assert(missing()) do
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_SUITE_END();
