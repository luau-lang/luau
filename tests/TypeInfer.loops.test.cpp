// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Frontend.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"
#include "Luau/VisitType.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)
LUAU_FASTFLAG(LuauOkWithIteratingOverTableProperties)

LUAU_DYNAMIC_FASTFLAG(LuauImproveNonFunctionCallError)

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

    CHECK_EQ(*builtinTypes->numberType, *requireType("q"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "iteration_no_table_passed")
{
    // This test may block CI if forced to run outside of DCR.
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};
    CheckResult result = check(R"(

type Iterable = typeof(setmetatable(
    {},
    {}::{
        __iter: (self: Iterable) -> (any, number) -> (number, string)
    }
))

local t: Iterable

for a, b in t do end
)");


    LUAU_REQUIRE_ERROR_COUNT(1, result);
    GenericError* ge = get<GenericError>(result.errors[0]);
    REQUIRE(ge);
    CHECK_EQ("__iter metamethod must return (next[, table[, state]])", ge->message);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "iteration_regression_issue_69967")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        type Iterable = typeof(setmetatable(
            {},
            {}::{
                __iter: (self: Iterable) -> () -> (number, string)
            }
        ))

        local t: Iterable

        for a, b in t do end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "iteration_regression_issue_69967_alt")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        type Iterable = typeof(setmetatable(
            {},
            {}::{
                __iter: (self: Iterable) -> () -> (number, string)
            }
        ))

        local t: Iterable
        local x, y

        for a, b in t do
            x = a
            y = b
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number", toString(requireType("x")));
    CHECK_EQ("string", toString(requireType("y")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "for_in_loop")
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

    CHECK_EQ(*builtinTypes->numberType, *requireType("n"));
    CHECK_EQ(*builtinTypes->stringType, *requireType("s"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "for_in_loop_with_next")
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

    CHECK_EQ(*builtinTypes->numberType, *requireType("n"));
    CHECK_EQ(*builtinTypes->stringType, *requireType("s"));
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

    if (DFFlag::LuauImproveNonFunctionCallError)
        CHECK_EQ("Cannot call a value of type string", toString(result.errors[0]));
    else
        CHECK_EQ("Cannot call non-function string", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "for_in_with_just_one_iterator_is_ok")
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

TEST_CASE_FIXTURE(BuiltinsFixture, "for_in_loop_with_zero_iterators_dcr")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        function no_iter() end
        for key in no_iter() do end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "for_in_with_a_custom_iterator_should_type_check")
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

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    TypeId p = requireType("p");
    CHECK_EQ("*error-type*", toString(p));
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

TEST_CASE_FIXTURE(BuiltinsFixture, "for_in_loop_error_on_factory_not_returning_the_right_amount_of_values")
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
    CHECK_EQ(builtinTypes->numberType, tm->wantedType);
    CHECK_EQ(builtinTypes->stringType, tm->givenType);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "for_in_loop_error_on_iterator_requiring_args_but_none_given")
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

TEST_CASE_FIXTURE(Fixture, "for_in_loop_with_incompatible_args_to_iterator")
{
    CheckResult result = check(R"(
        function my_iter(state: string, index: number)
            return state, index
        end

        local my_state = {}
        local first_index = "first"

        -- Type errors here.  my_state and first_index cannot be passed to my_iter
        for a, b in my_iter, my_state, first_index do
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK(get<TypeMismatch>(result.errors[1]));
    CHECK(Location{{9, 29}, {9, 37}} == result.errors[0].location);

    CHECK(get<TypeMismatch>(result.errors[1]));
    CHECK(Location{{9, 39}, {9, 50}} == result.errors[1].location);
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
    CHECK_EQ(builtinTypes->numberType, tm->wantedType);
    CHECK_EQ(builtinTypes->stringType, tm->givenType);
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

    CHECK_EQ(*builtinTypes->numberType, *requireType("i"));
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

    CHECK_EQ(*builtinTypes->stringType, *requireType("i"));
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

TEST_CASE_FIXTURE(BuiltinsFixture, "symbols_in_repeat_block_should_not_be_visible_beyond_until_condition")
{
    CheckResult result = check(R"(
        repeat
            local x = true
        until x

        print(x)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "varlist_declared_by_for_in_loop_should_be_free")
{
    CheckResult result = check(R"(
        local T = {}

        function T.f(p)
            for i, v in pairs(p) do
                T.f(v)
            end
        end
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        auto err = get<TypeMismatch>(result.errors[0]);
        CHECK(err != nullptr);
    }
    else
    {
        LUAU_REQUIRE_NO_ERRORS(result);
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "iter_constraint_before_loop_body")
{
    CheckResult result = check(R"(
        local T = {
    	    fields = {},
        }

        function f()
            for u, v in pairs(T.fields) do
                T.fields[u] = nil
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rbxl_place_file_crash_for_wrong_constraints")
{
    CheckResult result = check(R"(
local VehicleParameters = { 
    -- These are default values in the case the package structure is broken
	StrutSpringStiffnessFront = 28000,
}

local function updateFromConfiguration()
	for property, value in pairs(VehicleParameters) do
        VehicleParameters[property] = value
	end
end
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}


TEST_CASE_FIXTURE(BuiltinsFixture, "properly_infer_iteratee_is_a_free_table")
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

TEST_CASE_FIXTURE(BuiltinsFixture, "correctly_scope_locals_while")
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

TEST_CASE_FIXTURE(BuiltinsFixture, "trivial_ipairs_usage")
{
    CheckResult result = check(R"(
        local next, t, s = ipairs({1, 2, 3})
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    REQUIRE_EQ("({number}, number) -> (number?, number)", toString(requireType("next")));
    REQUIRE_EQ("{number}", toString(requireType("t")));
    REQUIRE_EQ("number", toString(requireType("s")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "ipairs_produces_integral_indices")
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

TEST_CASE_FIXTURE(BuiltinsFixture, "unreachable_code_after_infinite_loop")
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

TEST_CASE_FIXTURE(BuiltinsFixture, "loop_typecheck_crash_on_empty_optional")
{
    ScopedFastFlag sff{FFlag::LuauOkWithIteratingOverTableProperties, true};

    CheckResult result = check(R"(
        local t = {}
        for _ in t do
            for _ in assert(missing()) do
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "fuzz_fail_missing_instantitation_follow")
{
    // Just check that this doesn't assert
    check(R"(
        --!nonstrict
        function _(l0:number)
        return _
        end
        for _ in _(8) do
        end
    )");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "for_in_with_generic_next")
{
    CheckResult result = check(R"(
        for k: number, v: number in next, {1, 2, 3} do
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "loop_iter_basic")
{
    CheckResult result = check(R"(
        local t: {string} = {}
        local key
        for k: number in t do
        end
        for k: number, v: string in t do
        end
        for k, v in t do
            key = k
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // The old solver just infers the wrong type here.
    // The right type for `key` is `number?`
    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        TypeId keyTy = requireType("key");
        CHECK("number?" == toString(keyTy));
    }
    else
        CHECK_EQ(*builtinTypes->numberType, *requireType("key"));
}

TEST_CASE_FIXTURE(Fixture, "loop_iter_trailing_nil")
{
    CheckResult result = check(R"(
        local t: {string} = {}
        local extra
        for k, v, e in t do
            extra = e
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(0, result);
    CHECK_EQ(*builtinTypes->nilType, *requireType("extra"));
}

TEST_CASE_FIXTURE(Fixture, "loop_iter_no_indexer_strict")
{
    ScopedFastFlag sff{FFlag::LuauOkWithIteratingOverTableProperties, true};

    CheckResult result = check(R"(
        local t = {}
        for k, v in t do
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "loop_iter_no_indexer_nonstrict")
{
    CheckResult result = check(Mode::Nonstrict, R"(
        local t = {}
        for k, v in t do
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(0, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "loop_iter_metamethod_nil")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local t = setmetatable({}, { __iter = function(o) return next, nil end, })
        for k: number, v: string in t do
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Type 'nil' could not be converted into '{- [a]: b -}'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "loop_iter_metamethod_not_enough_returns")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local t = setmetatable({}, { __iter = function(o) end })
        for k: number, v: string in t do
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(result.errors[0] == TypeError{
                                  Location{{2, 36}, {2, 37}},
                                  GenericError{"__iter must return at least one value"},
                              });
}

TEST_CASE_FIXTURE(BuiltinsFixture, "loop_iter_metamethod_ok")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local t = setmetatable({
            children = {"foo"}
        }, { __iter = function(o) return next, o.children end })
        for k: number, v: string in t do
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(0, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "loop_iter_metamethod_ok_with_inference")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local t = setmetatable({
            children = {"foo"}
        }, { __iter = function(o) return next, o.children end })

        local a, b
        for k, v in t do
            a = k
            b = v
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireType("a")) == "number");
    CHECK(toString(requireType("b")) == "string");
}

TEST_CASE_FIXTURE(Fixture, "for_loop_lower_bound_is_string")
{
    CheckResult result = check(R"(
        for i: unknown = 1, 10 do end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "for_loop_lower_bound_is_string_2")
{
    CheckResult result = check(R"(
        for i: never = 1, 10 do end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type 'number' could not be converted into 'never'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "for_loop_lower_bound_is_string_3")
{
    CheckResult result = check(R"(
        for i: number | string = 1, 10 do end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cli_68448_iterators_need_not_accept_nil")
{
    CheckResult result = check(R"(
        local function makeEnum(members)
            local enum = {}
            for _, memberName in ipairs(members) do
                enum[memberName] = memberName
            end
            return enum
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    // HACK (CLI-68453): We name this inner table `enum`. For now, use the
    // exhaustive switch to see past it.
    CHECK(toString(requireType("makeEnum"), {true}) == "<a>({a}) -> {| [a]: a |}");
}

TEST_CASE_FIXTURE(Fixture, "iterate_over_free_table")
{
    ScopedFastFlag sff{FFlag::LuauOkWithIteratingOverTableProperties, true};

    CheckResult result = check(R"(
        function print(x) end

        function dump(tbl)
            print(tbl.whatever)
            for k, v in tbl do
                print(k)
                print(v)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "dcr_iteration_explore_raycast_minimization")
{
    CheckResult result = check(R"(
        local testResults = {}
        for _, testData in pairs(testResults) do
        end

        table.insert(testResults, {})
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "dcr_iteration_minimized_fragmented_keys_1")
{
    CheckResult result = check(R"(
        local function rawpairs(t)
            return next, t, nil
        end

        local function getFragmentedKeys(tbl)
            local _ = rawget(tbl, 0)
            for _ in rawpairs(tbl) do
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "dcr_iteration_minimized_fragmented_keys_2")
{
    CheckResult result = check(R"(
        local function getFragmentedKeys(tbl)
            local _ = rawget(tbl, 0)
            for _ in next, tbl, nil do
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "dcr_iteration_minimized_fragmented_keys_3")
{
    CheckResult result = check(R"(
        local function getFragmentedKeys(tbl)
            local _ = rawget(tbl, 0)
            for _ in pairs(tbl) do
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "dcr_iteration_fragmented_keys")
{
    CheckResult result = check(R"(
        local function isIndexKey(k, contiguousLength)
            return true
        end

        local function getTableLength(tbl)
            local length = 1
            local value = rawget(tbl, length)
            while value ~= nil do
                length += 1
                value = rawget(tbl, length)
            end
            return length - 1
        end

        local function rawpairs(t)
            return next, t, nil
        end

        local function getFragmentedKeys(tbl)
            local keys = {}
            local keysLength = 0
            local tableLength = getTableLength(tbl)
            for key, _ in rawpairs(tbl) do
                if not isIndexKey(key, tableLength) then
                    keysLength = keysLength + 1
                    keys[keysLength] = key
                end
            end
            return keys, keysLength, tableLength
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "dcr_xpath_candidates")
{
    CheckResult result = check(R"(
        type Instance = {}
        local function findCandidates(instances: { Instance },  path: { string })
            for _, name in ipairs(path) do
            end
            return {}
        end

        local canditates = findCandidates({}, {})
        for _, canditate in ipairs(canditates) do end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "dcr_iteration_on_never_gives_never")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local iter: never
        local ans
        for xs in iter do
            ans = xs
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireType("ans")) == "never");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "iterate_over_properties")
{
    ScopedFastFlag sff{FFlag::LuauOkWithIteratingOverTableProperties, true};

    CheckResult result = check(R"(
        local function f()
            local t = { p = 5, q = "hello" }
            for k, v in t do
                return k, v
            end

            error("")
        end

        local k, v = f()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("unknown", toString(requireType("k")));
    CHECK_EQ("unknown", toString(requireType("v")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "iterate_over_properties_nonstrict")
{
    ScopedFastFlag sff{FFlag::LuauOkWithIteratingOverTableProperties, true};

    CheckResult result = check(R"(
        --!nonstrict
        local function f()
            local t = { p = 5, q = "hello" }
            for k, v in t do
                return k, v
            end

            error("")
        end

        local k, v = f()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "pairs_should_not_retroactively_add_an_indexer")
{
    CheckResult result = check(R"(
        --!strict
        local prices = {
            hat = 1,
            bat = 2,
        }
        print(prices.wwwww)
        for _, _ in pairs(prices) do
        end
        print(prices.wwwww)
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        // We regress a little here: The old solver would typecheck the first
        // access to prices.wwwww on a table that had no indexer, and the second
        // on a table that does.
        LUAU_REQUIRE_ERROR_COUNT(0, result);
    }
    else
        LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "lti_fuzzer_uninitialized_loop_crash")
{
    CheckResult result = check(R"(
        for l0=_,_ do
            return _()
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "iterate_array_of_singletons")
{
    CheckResult result = check(R"(
        --!strict
        type Direction = "Left" | "Right" | "Up" | "Down"
        local Instructions: { Direction } = { "Left", "Down" }

        for _, step in Instructions do
            local dir: Direction = step
            print(dir)
        end
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
        LUAU_REQUIRE_NO_ERRORS(result);
    else
        LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "iter_mm_results_are_lvalue")
{
    CheckResult result = check(R"(
        local foo = setmetatable({}, {
            __iter = function()
                return pairs({1, 2, 3})
            end,
        })

        for k, v in foo do
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "forin_metatable_no_iter_mm")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        local t = setmetatable({1, 2, 3}, {})

        for i, v in t do
            print(i, v)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number", toString(requireTypeAtPosition({4, 18})));
    CHECK_EQ("number", toString(requireTypeAtPosition({4, 21})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "forin_metatable_iter_mm")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        type Iterable<T...> = typeof(setmetatable({}, {} :: {
            __iter: (Iterable<T...>) -> () -> T...
        }))

        for i, v in {} :: Iterable<...number> do
            print(i, v)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number", toString(requireTypeAtPosition({6, 18})));
    CHECK_EQ("number", toString(requireTypeAtPosition({6, 21})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "iteration_preserves_error_suppression")
{
    CheckResult result = check(R"(
        function first(x: any)
            for k, v in pairs(x) do
                print(k, v)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("any" == toString(requireTypeAtPosition({3, 22})));
    CHECK("any" == toString(requireTypeAtPosition({3, 25})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "tryDispatchIterableFunction_under_constrained_loop_should_not_assert")
{
    CheckResult result = check(R"(
local function foo(Instance)
	for _, Child in next, Instance:GetChildren() do
	end
end
    )");
}

TEST_SUITE_END();
