// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"
#include "Luau/VisitType.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)

TEST_SUITE_BEGIN("TypeInferAnyError");

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_returns_any")
{
    CheckResult result = check(R"(
        function bar(): any
            return true
        end

        local a
        for b in bar do
            a = b
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::LuauSolverV2)
        CHECK("(*error-type* | ~nil)?" == toString(requireType("a")));
    else
        CHECK(getBuiltins()->anyType == requireType("a"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_returns_any2")
{
    CheckResult result = check(R"(
        function bar(): any
            return true
        end

        local a
        for b in bar() do
            a = b
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::LuauSolverV2)
        CHECK("(*error-type* | ~nil)?" == toString(requireType("a")));
    else
        CHECK("any" == toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_is_any")
{
    CheckResult result = check(R"(
        local bar = nil :: any

        local a
        for b in bar do
            a = b
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::LuauSolverV2)
        CHECK("(*error-type* | ~nil)?" == toString(requireType("a")));
    else
        CHECK("any" == toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_is_any2")
{
    CheckResult result = check(R"(
        local bar = nil :: any

        local a
        for b in bar() do
            a = b
        end
    )");

    if (FFlag::LuauSolverV2)
        CHECK("(*error-type* | ~nil)?" == toString(requireType("a")));
    else
        CHECK("any" == toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_is_any_pack")
{
    CheckResult result = check(R"(
        function bar(): ...any end

        local a
        for b in bar() do
            a = b
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::LuauSolverV2)
        CHECK("(*error-type* | ~nil)?" == toString(requireType("a")));
    else
        CHECK("any" == toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_is_error")
{
    CheckResult result = check(R"(
        local a
        for b in bar do
            a = b
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);


    if (FFlag::LuauSolverV2)
    {
        // Bug: We do not simplify at the right time
        CHECK_EQ("*error-type*?", toString(requireType("a")));
    }
    else
    {
        CHECK_EQ("*error-type*", toString(requireType("a")));
    }
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_is_error2")
{
    CheckResult result = check(R"(
        function bar(c) return c end

        local a
        for b in bar() do
            a = b
        end
    )");

    if (FFlag::LuauSolverV2)
    {
        // CLI-97375(awe): `bar()` is returning `nil` here, which isn't wrong necessarily,
        // but then we're signaling an additional error for the access on `nil`.
        LUAU_REQUIRE_ERROR_COUNT(2, result);

        // Bug: We do not simplify at the right time
        CHECK_EQ("*error-type*?", toString(requireType("a")));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);

        CHECK_EQ("*error-type*", toString(requireType("a")));
    }
}

TEST_CASE_FIXTURE(Fixture, "length_of_error_type_does_not_produce_an_error")
{
    CheckResult result = check(R"(
        local l = #this_is_not_defined
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "indexing_error_type_does_not_produce_an_error")
{
    CheckResult result = check(R"(
        local originalReward = unknown.Parent.Reward:GetChildren()[1]
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "dot_on_error_type_does_not_produce_an_error")
{
    CheckResult result = check(R"(
        local foo = (true).x
        foo.x = foo.y
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "any_type_propagates")
{
    CheckResult result = check(R"(
        local foo: any
        local bar = foo:method("argument")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("any", toString(requireType("bar")));
}

TEST_CASE_FIXTURE(Fixture, "can_subscript_any")
{
    CheckResult result = check(R"(
        local foo: any
        local bar = foo[5]
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("any", toString(requireType("bar")));
}

// Not strictly correct: metatables permit overriding this
TEST_CASE_FIXTURE(Fixture, "can_get_length_of_any")
{
    CheckResult result = check(R"(
        local foo = ({} :: any)
        local bar = #foo
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(PrimitiveType::Number, getPrimitiveType(requireType("bar")));
}

TEST_CASE_FIXTURE(Fixture, "assign_prop_to_table_by_calling_any_yields_any")
{
    CheckResult result = check(R"(
        local f: any
        local T = {}

        T.prop = f()

        return T
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TableType* ttv = getMutable<TableType>(requireType("T"));
    REQUIRE(ttv);
    REQUIRE(ttv->props.count("prop"));

    REQUIRE(ttv->props["prop"].readTy);
    CHECK_EQ("any", toString(*ttv->props["prop"].readTy));
}

TEST_CASE_FIXTURE(Fixture, "quantify_any_does_not_bind_to_itself")
{
    CheckResult result = check(R"(
        local A : any
        function A.B() end
        A:C()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId aType = requireType("A");
    CHECK_EQ(aType, getBuiltins()->anyType);
}

TEST_CASE_FIXTURE(Fixture, "calling_error_type_yields_error")
{
    CheckResult result = check(R"(
        local a = unknown.Parent.Reward.GetChildren()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownSymbol* err = get<UnknownSymbol>(result.errors[0]);
    REQUIRE(err != nullptr);

    CHECK_EQ("unknown", err->name);

    if (FFlag::LuauSolverV2)
        CHECK_EQ("any", toString(requireType("a")));
    else
        CHECK_EQ("*error-type*", toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "chain_calling_error_type_yields_error")
{
    CheckResult result = check(R"(
        local a = Utility.Create "Foo" {}
    )");

    if (FFlag::LuauSolverV2)
        CHECK_EQ("any", toString(requireType("a")));
    else
        CHECK_EQ("*error-type*", toString(requireType("a")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "replace_every_free_type_when_unifying_a_complex_function_with_any")
{
    CheckResult result = check(R"(
        local a: any
        local b
        for _, i in pairs(a) do
            b = i
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("any", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "call_to_any_yields_any")
{
    CheckResult result = check(R"(
        local a: any
        local b = a()
    )");

    REQUIRE_EQ("any", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "CheckMethodsOfAny")
{
    CheckResult result = check(R"(
local x: any = {}
function x:y(z: number)
    local s: string = z
end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "CheckMethodsOfError")
{
    CheckResult result = check(R"(
local x = (true).foo
function x:y(z: number)
    local s: string = z
end
)");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable_of_any_can_be_a_table")
{
    CheckResult result = check(R"(
--!strict
local T: any
T = {}
T.__index = T
function T.new(...)
    local self = {}
    setmetatable(self, T)
    self:construct(...)
    return self
end
function T:construct(index)
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "type_error_addition")
{
    CheckResult result = check(R"(
--!strict
local foo = makesandwich()
local bar = foo.nutrition + 100
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    // We should definitely get this error
    CHECK_EQ("Unknown global 'makesandwich'", toString(result.errors[0]));
    // We get this error if makesandwich() returns a free type
    // CHECK_EQ("Unknown type used in + operation; consider adding a type annotation to 'foo'", toString(result.errors[1]));
}

TEST_CASE_FIXTURE(Fixture, "prop_access_on_any_with_other_options")
{
    CheckResult result = check(R"(
        local function f(thing: any | string)
            local foo = thing.SomeRandomKey
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "union_of_types_regression_test")
{
    CheckResult result = check(R"(
--!strict
local stat
stat = stat and tonumber(stat) or stat
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_of_any_calls")
{
    CheckResult result = check(R"(
        local function testFunc(input: {any})
        end

        local v = {true}

        testFunc(v)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "intersection_of_any_can_have_props")
{
    // *blocked-130* ~ hasProp any & ~(false?), "_status"
    CheckResult result = check(R"(
function foo(x: any, y)
    if x then
        return x._status
    end
    return y
end
)");

    CHECK("(any, any) -> any" == toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "cast_to_table_of_any")
{
    CheckResult result = check(R"(
        local v = {true} :: {any}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
