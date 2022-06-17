// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"

#include "Fixture.h"

#include "doctest.h"

#include <algorithm>

using namespace Luau;

TEST_SUITE_BEGIN("NonstrictModeTests");

TEST_CASE_FIXTURE(Fixture, "globals")
{
    CheckResult result = check(R"(
        --!nonstrict
        foo = true
        foo = "now i'm a string!"
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("any", toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "globals2")
{
    ScopedFastFlag sff[]{
        {"LuauReturnTypeInferenceInNonstrict", true},
        {"LuauLowerBoundsCalculation", true},
    };

    CheckResult result = check(R"(
        --!nonstrict
        foo = function() return 1 end
        foo = "now i'm a string!"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("() -> number", toString(tm->wantedType));
    CHECK_EQ("string", toString(tm->givenType));
    CHECK_EQ("() -> number", toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "globals_everywhere")
{
    CheckResult result = check(R"(
        --!nonstrict
        foo = 1

        if true then
            bar = 2
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("any", toString(requireType("foo")));
    CHECK_EQ("any", toString(requireType("bar")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "function_returns_number_or_string")
{
    ScopedFastFlag sff[]{{"LuauReturnTypeInferenceInNonstrict", true}, {"LuauLowerBoundsCalculation", true}};

    CheckResult result = check(R"(
        --!nonstrict
        local function f()
            if math.random() > 0.5 then
                return 5
            else
                return "hi"
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("() -> number | string" == toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "infer_nullary_function")
{
    CheckResult result = check(R"(
        --!nonstrict
        function foo(x, y) end
    )");

    TypeId fooType = requireType("foo");
    REQUIRE(fooType);

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(fooType);
    REQUIRE_MESSAGE(ftv != nullptr, "Expected a function, got " << toString(fooType));

    auto args = flatten(ftv->argTypes).first;
    REQUIRE_EQ(2, args.size());
    REQUIRE_EQ("any", toString(args[0]));
    REQUIRE_EQ("any", toString(args[1]));

    auto rets = flatten(ftv->retTypes).first;
    REQUIRE_EQ(0, rets.size());
}

TEST_CASE_FIXTURE(Fixture, "first_return_type_dictates_number_of_return_types")
{
    ScopedFastFlag sff[]{
        {"LuauReturnTypeInferenceInNonstrict", true},
        {"LuauLowerBoundsCalculation", true},
    };

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

    REQUIRE_EQ("(any) -> number", toString(t));
}

TEST_CASE_FIXTURE(Fixture, "return_annotation_is_still_checked")
{
    CheckResult result = check(R"(
        --!nonstrict
        function foo(x): number return 'hello' end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
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

    CHECK_EQ(*typeChecker.anyType, *requireType("m"));
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

    TableTypeVar* ttv = getMutable<TableTypeVar>(requireType("T"));

    REQUIRE(ttv != nullptr);

    TypeId fooProp = ttv->props["foo"].type;
    REQUIRE(fooProp != nullptr);

    CHECK_EQ(*fooProp, *typeChecker.anyType);
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

    TableTypeVar* ttv = getMutable<TableTypeVar>(requireType("T"));
    REQUIRE_MESSAGE(ttv, "Should be a table: " << toString(requireType("T")));

    CHECK_EQ(*typeChecker.anyType, *ttv->props["one"].type);
    CHECK_EQ(*typeChecker.anyType, *ttv->props["two"].type);
    CHECK_MESSAGE(get<FunctionTypeVar>(ttv->props["three"].type), "Should be a function: " << *ttv->props["three"].type);
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
    ScopedFastFlag sff[]{
        {"LuauReturnTypeInferenceInNonstrict", true},
        {"LuauLowerBoundsCalculation", true},
    };

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

    REQUIRE_EQ("((any) -> string) | {| foo: any |}", toString(getMainModule()->getModuleScope()->returnType));
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

TEST_SUITE_END();
