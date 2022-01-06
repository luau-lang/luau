// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Parser.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("IntersectionTypes");

TEST_CASE_FIXTURE(Fixture, "select_correct_union_fn")
{
    CheckResult result = check(R"(
        type A = (number) -> (string)
        type B = (string) -> (number)
        local f:A & B
        local b = f(10) -- b is a string
        local c = f("a") -- c is a number
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(requireType("b"), typeChecker.stringType);
    CHECK_EQ(requireType("c"), typeChecker.numberType);
}

TEST_CASE_FIXTURE(Fixture, "table_combines")
{
    CheckResult result = check(R"(
        type A={a:number}
        type B={b:string}
        local c:A & B = {a=10, b="s"}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "table_combines_missing")
{
    CheckResult result = check(R"(
        type A={a:number}
        type B={b:string}
        local c:A & B = {a=10}
    )");

    REQUIRE(result.errors.size() == 1);
}

TEST_CASE_FIXTURE(Fixture, "impossible_type")
{
    CheckResult result = check(R"(
        local c:number&string = 10
    )");

    REQUIRE(result.errors.size() == 1);
}

TEST_CASE_FIXTURE(Fixture, "table_extra_ok")
{
    CheckResult result = check(R"(
        type A={a:number}
        type B={b:string}
        local c:A & B
        local d:A = c
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "fx_intersection_as_argument")
{
    CheckResult result = check(R"(
        type A = (number) -> (string)
        type B = (string) -> (number)
        type C = (A) -> (number)
        local f:A & B
        local g:C
        local b = g(f)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "fx_union_as_argument_fails")
{
    CheckResult result = check(R"(
        type A = (number) -> (string)
        type B = (string) -> (number)
        type C = (A) -> (number)
        local f:A | B
        local g:C
        local b = g(f)
    )");

    REQUIRE(!result.errors.empty());
}

TEST_CASE_FIXTURE(Fixture, "argument_is_intersection")
{
    CheckResult result = check(R"(
        type A = (number | boolean) -> number
        local f: A

        f(5)
        f(true)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "should_still_pick_an_overload_whose_arguments_are_unions")
{
    CheckResult result = check(R"(
        type A = (number | boolean) -> number
        type B = (string | nil)     -> string
        local f: A & B

        local a1, a2 = f(1), f(true)
        local b1, b2 = f("foo"), f(nil)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*requireType("a1"), *typeChecker.numberType);
    CHECK_EQ(*requireType("a2"), *typeChecker.numberType);

    CHECK_EQ(*requireType("b1"), *typeChecker.stringType);
    CHECK_EQ(*requireType("b2"), *typeChecker.stringType);
}

TEST_CASE_FIXTURE(Fixture, "propagates_name")
{
    const std::string code = R"(
        type A={a:number}
        type B={b:string}

        local c:A&B
        local b = c
    )";
    const std::string expected = R"(
        type A={a:number}
        type B={b:string}

        local c:A&B
        local b:A&B=c
    )";

    CHECK_EQ(expected, decorateWithTypes(code));
}

TEST_CASE_FIXTURE(Fixture, "index_on_an_intersection_type_with_property_guaranteed_to_exist")
{
    CheckResult result = check(R"(
        type A = {x: {y: number}}
        type B = {x: {y: number}}
        local t: A & B

        local r = t.x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const IntersectionTypeVar* r = get<IntersectionTypeVar>(requireType("r"));
    REQUIRE(r);

    TableTypeVar* a = getMutable<TableTypeVar>(r->parts[0]);
    REQUIRE(a);
    CHECK_EQ(typeChecker.numberType, a->props["y"].type);

    TableTypeVar* b = getMutable<TableTypeVar>(r->parts[1]);
    REQUIRE(b);
    CHECK_EQ(typeChecker.numberType, b->props["y"].type);
}

TEST_CASE_FIXTURE(Fixture, "index_on_an_intersection_type_works_at_arbitrary_depth")
{
    CheckResult result = check(R"(
        type A = {x: {y: {z: {thing: string}}}}
        type B = {x: {y: {z: {thing: string}}}}
        local t: A & B

        local r = t.x.y.z.thing
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*typeChecker.stringType, *requireType("r"));
}

TEST_CASE_FIXTURE(Fixture, "index_on_an_intersection_type_with_mixed_types")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {x: string}
        local t: A & B

        local r = t.x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number & string", toString(requireType("r"))); // TODO(amccord): This should be an error.
}

TEST_CASE_FIXTURE(Fixture, "index_on_an_intersection_type_with_one_part_missing_the_property")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {}
        local t: A & B

        local r = t.x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number", toString(requireType("r")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_an_intersection_type_with_one_property_of_type_any")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {x: any}
        local t: A & B

        local r = t.x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*typeChecker.anyType, *requireType("r"));
}

TEST_CASE_FIXTURE(Fixture, "index_on_an_intersection_type_with_all_parts_missing_the_property")
{
    CheckResult result = check(R"(
        type A = {}
        type B = {}

        local function f(t: A & B)
            local x = t.x
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownProperty* up = get<UnknownProperty>(result.errors[0]);
    REQUIRE_MESSAGE(up, result.errors[0].data);
    CHECK_EQ(up->key, "x");
}

TEST_CASE_FIXTURE(Fixture, "table_intersection_write")
{
    CheckResult result = check(R"(
        type X = { x: number }
        type XY = X & { y: number }

        local a : XY = { x = 1, y = 2 }
        a.x = 10
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = check(R"(
        type X = {}
        type XY = X & { x: number, y: number }

        local a : XY = { x = 1, y = 2 }
        a.x = 10
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = check(R"(
        type X = { x: number }
        type Y = { y: number }
        type XY = X & Y

        local a : XY = { x = 1, y = 2 }
        a.x = 10
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = check(R"(
        type A = { x: {y: number} }
        type B = { x: {y: number} }
        local t : A & B = { x = { y = 1 } }

        t.x = { y = 4 }
        t.x.y = 40
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "table_intersection_write_sealed")
{
    CheckResult result = check(R"(
        type X = { x: number }
        type Y = { y: number }
        type XY = X & Y

        local a : XY = { x = 1, y = 2 }
        a.z = 10
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Cannot add property 'z' to table 'X & Y'");
}

TEST_CASE_FIXTURE(Fixture, "table_intersection_write_sealed_indirect")
{
    CheckResult result = check(R"(
    type X = { x: (number) -> number }
    type Y = { y: (string) -> string }

    type XY = X & Y

    local xy : XY = {
        x = function(a: number) return -a end,
        y = function(a: string) return a .. "b" end
    }
    function xy.z(a:number) return a * 10 end
    function xy:y(a:number) return a * 10 end
    function xy:w(a:number) return a * 10 end
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);
    CHECK_EQ(toString(result.errors[0]), "Cannot add property 'z' to table 'X & Y'");
    CHECK_EQ(toString(result.errors[1]), "Cannot add property 'y' to table 'X & Y'");
    CHECK_EQ(toString(result.errors[2]), "Cannot add property 'w' to table 'X & Y'");
}

TEST_CASE_FIXTURE(Fixture, "table_intersection_setmetatable")
{
    CheckResult result = check(R"(
        local t: {} & {}
        setmetatable(t, {})
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_intersection_part")
{
    CheckResult result = check(R"(
type X = { x: number }
type Y = { y: number }
type Z = { z: number }

type XYZ = X & Y & Z

local a: XYZ = 3
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), R"(Type 'number' could not be converted into 'X & Y & Z'
caused by:
  Not all intersection parts are compatible. Type 'number' could not be converted into 'X')");
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_intersection_all")
{
    CheckResult result = check(R"(
type X = { x: number }
type Y = { y: number }
type Z = { z: number }

type XYZ = X & Y & Z

local a: XYZ
local b: number = a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), R"(Type 'X & Y & Z' could not be converted into 'number'; none of the intersection parts are compatible)");
}

TEST_SUITE_END();
