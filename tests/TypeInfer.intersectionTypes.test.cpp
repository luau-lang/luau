// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);

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
    CHECK_EQ(requireType("b"), builtinTypes->stringType);
    CHECK_EQ(requireType("c"), builtinTypes->numberType);
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

    CHECK_EQ(*requireType("a1"), *builtinTypes->numberType);
    CHECK_EQ(*requireType("a2"), *builtinTypes->numberType);

    CHECK_EQ(*requireType("b1"), *builtinTypes->stringType);
    CHECK_EQ(*requireType("b2"), *builtinTypes->stringType);
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
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK("{ y: number }" == toString(requireType("r")));
    else
        CHECK("{| y: number |} & {| y: number |}" == toString(requireType("r")));
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

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("string", toString(requireType("r")));
    }
    else
    {
        CHECK_EQ("string & string", toString(requireType("r")));
    }
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
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("never", toString(requireType("r")));
    else
        CHECK_EQ("number & string", toString(requireType("r")));
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
        type A = {y: number}
        type B = {x: any}
        local t: A & B

        local r = t.x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*builtinTypes->anyType, *requireType("r"));
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
    auto e = toString(result.errors[0]);
    CHECK_EQ("Cannot add property 'z' to table 'X & Y'", e);
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

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    const std::string expected = R"(Type
    '(string, number) -> string'
could not be converted into
    '(string) -> string'
caused by:
  Argument count mismatch. Function expects 2 arguments, but only 1 is specified)";
    CHECK_EQ(expected, toString(result.errors[0]));
    CHECK_EQ(toString(result.errors[1]), "Cannot add property 'z' to table 'X & Y'");
    CHECK_EQ(toString(result.errors[2]), "Type 'number' could not be converted into 'string'");
    CHECK_EQ(toString(result.errors[3]), "Cannot add property 'w' to table 'X & Y'");
}

TEST_CASE_FIXTURE(Fixture, "table_write_sealed_indirect")
{
    // After normalization, previous 'table_intersection_write_sealed_indirect' is identical to this one
    CheckResult result = check(R"(
    type XY = { x: (number) -> number, y: (string) -> string }

    local xy : XY = {
        x = function(a: number) return -a end,
        y = function(a: string) return a .. "b" end
    }
    function xy.z(a:number) return a * 10 end
    function xy:y(a:number) return a * 10 end
    function xy:w(a:number) return a * 10 end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    const std::string expected = R"(Type
    '(string, number) -> string'
could not be converted into
    '(string) -> string'
caused by:
  Argument count mismatch. Function expects 2 arguments, but only 1 is specified)";
    CHECK_EQ(expected, toString(result.errors[0]));

    CHECK_EQ(toString(result.errors[1]), "Cannot add property 'z' to table 'XY'");
    CHECK_EQ(toString(result.errors[2]), "Type 'number' could not be converted into 'string'");
    CHECK_EQ(toString(result.errors[3]), "Cannot add property 'w' to table 'XY'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_intersection_setmetatable")
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
    const std::string expected = R"(Type 'number' could not be converted into 'X & Y & Z'
caused by:
  Not all intersection parts are compatible.
Type 'number' could not be converted into 'X')";
    CHECK_EQ(expected, toString(result.errors[0]));
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

TEST_CASE_FIXTURE(Fixture, "overload_is_not_a_function")
{
    check(R"(
--!nonstrict
function _(...):((typeof(not _))&(typeof(not _)))&((typeof(not _))&(typeof(not _)))
_(...)(setfenv,_,not _,"")[_] = nil
end
do end
_(...)(...,setfenv,_):_G()
)");
}

TEST_CASE_FIXTURE(Fixture, "no_stack_overflow_from_flattenintersection")
{
    CheckResult result = check(R"(
        local l0,l0
        repeat
        type t0 = ((any)|((any)&((any)|((any)&((any)|(any))))))&(t0)
        function _(l0):(t0)&(t0)
            while nil do
            end
        end
        until _(_)(_)._
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "intersect_bool_and_false")
{
    CheckResult result = check(R"(
        local x : (boolean & false)
        local y : false = x -- OK
        local z : true = x  -- Not OK
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Type 'boolean & false' could not be converted into 'true'; none of the intersection parts are compatible");
}

TEST_CASE_FIXTURE(Fixture, "intersect_false_and_bool_and_false")
{
    CheckResult result = check(R"(
        local x : false & (boolean & false)
        local y : false = x -- OK
        local z : true = x  -- Not OK
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    // TODO: odd stringification of `false & (boolean & false)`.)
    CHECK_EQ(toString(result.errors[0]),
        "Type 'boolean & false & false' could not be converted into 'true'; none of the intersection parts are compatible");
}

TEST_CASE_FIXTURE(Fixture, "intersect_saturate_overloaded_functions")
{
    CheckResult result = check(R"(
        local x : ((number?) -> number?) & ((string?) -> string?)
        local y : (nil) -> nil = x -- OK
        local z : (number) -> number = x -- Not OK
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '((number?) -> number?) & ((string?) -> string?)'
could not be converted into
    '(number) -> number'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "union_saturate_overloaded_functions")
{

    CheckResult result = check(R"(
        local x : ((number) -> number) & ((string) -> string)
        local y : ((number | string) -> (number | string)) = x -- OK
        local z : ((number | boolean) -> (number | boolean)) = x -- Not OK
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '((number) -> number) & ((string) -> string)'
could not be converted into
    '(boolean | number) -> boolean | number'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "intersection_of_tables")
{
    CheckResult result = check(R"(
        local x : { p : number?, q : string? } & { p : number?, q : number?, r : number? }
        local y : { p : number?, q : nil, r : number? } = x -- OK
        local z : { p : nil } = x -- Not OK
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected =
        (FFlag::DebugLuauDeferredConstraintResolution) ?
        "Type "
        "'{ p: number?, q: number?, r: number? } & { p: number?, q: string? }'"
        " could not be converted into "
        "'{ p: nil }'; none of the intersection parts are compatible" :
        R"(Type
    '{| p: number?, q: number?, r: number? |} & {| p: number?, q: string? |}'
could not be converted into
    '{| p: nil |}'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "intersection_of_tables_with_top_properties")
{
    CheckResult result = check(R"(
        local x : { p : number?, q : any } & { p : unknown, q : string? }
        local y : { p : number?, q : string? } = x -- OK
        local z : { p : string?, q : number? } = x -- Not OK
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);

        CHECK_EQ(toString(result.errors[0]),
            "Type '{| p: number?, q: string? |}' could not be converted into '{| p: string?, q: number? |}'\n"
            "caused by:\n"
            "  Property 'p' is not compatible. Type 'number?' could not be converted into 'string?'\n"
            "caused by:\n"
            "  Not all union options are compatible. Type 'number' could not be converted into 'string?'\n"
            "caused by:\n"
            "  None of the union options are compatible. For example: Type 'number' could not be converted into 'string' in an invariant context");

        CHECK_EQ(toString(result.errors[1]),
            "Type '{| p: number?, q: string? |}' could not be converted into '{| p: string?, q: number? |}'\n"
            "caused by:\n"
            "  Property 'q' is not compatible. Type 'string?' could not be converted into 'number?'\n"
            "caused by:\n"
            "  Not all union options are compatible. Type 'string' could not be converted into 'number?'\n"
            "caused by:\n"
            "  None of the union options are compatible. For example: Type 'string' could not be converted into 'number' in an invariant context");
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        const std::string expected = R"(Type
    '{| p: number?, q: any |} & {| p: unknown, q: string? |}'
could not be converted into
    '{| p: string?, q: number? |}'; none of the intersection parts are compatible)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "intersection_of_tables_with_never_properties")
{
    CheckResult result = check(R"(
        local x : { p : number?, q : never } & { p : never, q : string? } -- OK
        local y : { p : never, q : never } = x -- OK
        local z : never = x -- OK
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "overloaded_functions_returning_intersections")
{
    CheckResult result = check(R"(
        local x : ((number?) -> ({ p : number } & { q : number })) & ((string?) -> ({ p : number } & { r : number }))
        local y : (nil) -> { p : number, q : number, r : number} = x -- OK
        local z : (number?) -> { p : number, q : number, r : number} = x -- Not OK
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected =
        (FFlag::DebugLuauDeferredConstraintResolution) ?
        R"(Type
    '((number?) -> { p: number } & { q: number }) & ((string?) -> { p: number } & { r: number })'
could not be converted into
    '(number?) -> { p: number, q: number, r: number }'; none of the intersection parts are compatible)" :
        R"(Type
    '((number?) -> {| p: number |} & {| q: number |}) & ((string?) -> {| p: number |} & {| r: number |})'
could not be converted into
    '(number?) -> {| p: number, q: number, r: number |}'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloaded_functions_mentioning_generic")
{
    CheckResult result = check(R"(
      function f<a>()
        local x : ((number?) -> (a | number)) & ((string?) -> (a | string))
        local y : (nil) -> a = x -- OK
        local z : (number?) -> a = x -- Not OK
      end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '((number?) -> a | number) & ((string?) -> a | string)'
could not be converted into
    '(number?) -> a'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloaded_functions_mentioning_generics")
{
    CheckResult result = check(R"(
      function f<a,b,c>()
        local x : ((a?) -> (a | b)) & ((c?) -> (b | c))
        local y : (nil) -> ((a & c) | b) = x -- OK
        local z : (a?) -> ((a & c) | b) = x -- Not OK
      end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '((a?) -> a | b) & ((c?) -> b | c)'
could not be converted into
    '(a?) -> (a & c) | b'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloaded_functions_mentioning_generic_packs")
{
    CheckResult result = check(R"(
      function f<a...,b...>()
        local x : ((number?, a...) -> (number?, b...)) & ((string?, a...) -> (string?, b...))
        local y : ((nil, a...) -> (nil, b...)) = x -- OK
        local z : ((nil, b...) -> (nil, a...)) = x -- Not OK
      end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '((number?, a...) -> (number?, b...)) & ((string?, a...) -> (string?, b...))'
could not be converted into
    '(nil, b...) -> (nil, a...)'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_unknown_result")
{
    CheckResult result = check(R"(
      function f<a...,b...>()
        local x : ((number) -> number) & ((nil) -> unknown)
        local y : (number?) -> unknown = x -- OK
        local z : (number?) -> number? = x -- Not OK
      end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '((nil) -> unknown) & ((number) -> number)'
could not be converted into
    '(number?) -> number?'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_unknown_arguments")
{
    CheckResult result = check(R"(
      function f<a...,b...>()
        local x : ((number) -> number?) & ((unknown) -> string?)
        local y : (number) -> nil = x -- OK
        local z : (number?) -> nil = x -- Not OK
      end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '((number) -> number?) & ((unknown) -> string?)'
could not be converted into
    '(number?) -> nil'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_never_result")
{
    CheckResult result = check(R"(
      function f<a...,b...>()
        local x : ((number) -> number) & ((nil) -> never)
        local y : (number?) -> number = x -- OK
        local z : (number?) -> never = x -- Not OK
      end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '((nil) -> never) & ((number) -> number)'
could not be converted into
    '(number?) -> never'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_never_arguments")
{
    CheckResult result = check(R"(
      function f<a...,b...>()
        local x : ((number) -> number?) & ((never) -> string?)
        local y : (never) -> nil = x -- OK
        local z : (number?) -> nil = x -- Not OK
      end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '((never) -> string?) & ((number) -> number?)'
could not be converted into
    '(number?) -> nil'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_overlapping_results_and_variadics")
{
    CheckResult result = check(R"(
        local x : ((string?) -> (string | number)) & ((number?) -> ...number)
        local y : ((nil) -> (number, number?)) = x -- OK
        local z : ((string | number) -> (number, number?)) = x -- Not OK
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '((number?) -> (...number)) & ((string?) -> number | string)'
could not be converted into
    '(number | string) -> (number, number?)'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_weird_typepacks_1")
{
    CheckResult result = check(R"(
      function f<a...,b...>()
        local x : (() -> a...) & (() -> b...)
        local y : (() -> b...) & (() -> a...) = x -- OK
        local z : () -> () = x -- Not OK
      end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]),
        "Type '(() -> (a...)) & (() -> (b...))' could not be converted into '() -> ()'; none of the intersection parts are compatible");
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_weird_typepacks_2")
{
    CheckResult result = check(R"(
      function f<a...,b...>()
        local x : ((a...) -> ()) & ((b...) -> ())
        local y : ((b...) -> ()) & ((a...) -> ()) = x -- OK
        local z : () -> () = x -- Not OK
      end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]),
        "Type '((a...) -> ()) & ((b...) -> ())' could not be converted into '() -> ()'; none of the intersection parts are compatible");
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_weird_typepacks_3")
{
    CheckResult result = check(R"(
      function f<a...>()
        local x : (() -> a...) & (() -> (number?,a...))
        local y : (() -> (number?,a...)) & (() -> a...) = x -- OK
        local z : () -> (number) = x -- Not OK
      end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '(() -> (a...)) & (() -> (number?, a...))'
could not be converted into
    '() -> number'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_weird_typepacks_4")
{
    CheckResult result = check(R"(
      function f<a...>()
        local x : ((a...) -> ()) & ((number,a...) -> number)
        local y : ((number,a...) -> number) & ((a...) -> ()) = x -- OK
        local z : (number?) -> () = x -- Not OK
      end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '((a...) -> ()) & ((number, a...) -> number)'
could not be converted into
    '(number?) -> ()'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "intersect_metatables")
{
    CheckResult result = check(R"(
        local a : string? = nil
        local b : number? = nil

        local x = setmetatable({}, { p = 5, q = a });
        local y = setmetatable({}, { q = b, r = "hi" });
        local z = setmetatable({}, { p = 5, q = nil, r = "hi" });

        type X = typeof(x)
        type Y = typeof(y)
        type Z = typeof(z)

        local xy : X&Y = z;
        local yx : Y&X = z;
        z = xy;
        z = yx;
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "intersect_metatable_subtypes")
{
    CheckResult result = check(R"(
        local x = setmetatable({ a = 5 }, { p = 5 });
        local y = setmetatable({ b = "hi" }, { p = 5, q = "hi" });
        local z = setmetatable({ a = 5, b = "hi" }, { p = 5, q = "hi" });

        type X = typeof(x)
        type Y = typeof(y)
        type Z = typeof(z)

        local xy : X&Y = z;
        local yx : Y&X = z;
        z = xy;
        z = yx;
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}


TEST_CASE_FIXTURE(BuiltinsFixture, "intersect_metatables_with_properties")
{
    CheckResult result = check(R"(
        local x = setmetatable({ a = 5 }, { p = 5 });
        local y = setmetatable({ b = "hi" }, { q = "hi" });
        local z = setmetatable({ a = 5, b = "hi" }, { p = 5, q = "hi" });

        type X = typeof(x)
        type Y = typeof(y)
        type Z = typeof(z)

        local xy : X&Y = z;
        z = xy;
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "intersect_metatable_with_table")
{
    CheckResult result = check(R"(
        local x = setmetatable({ a = 5 }, { p = 5 });
        local z = setmetatable({ a = 5, b = "hi" }, { p = 5 });

        type X = typeof(x)
        type Y = { b : string }
        type Z = typeof(z)

        -- TODO: once we have shape types, we should be able to initialize these with z
        local xy : X&Y;
        local yx : Y&X;
        z = xy;
        z = yx;
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "CLI-44817")
{
    CheckResult result = check(R"(
        type X = {x: number}
        type Y = {y: number}
        type Z = {z: number}

        type XY = {x: number, y: number}
        type XYZ = {x:number, y: number, z: number}

        local xy: XY = {x = 0, y = 0}
        local xyz: XYZ = {x = 0, y = 0, z = 0}

        local xNy: X&Y = xy
        local xNyNz: X&Y&Z = xyz

        local t1: XY = xNy -- Type 'X & Y' could not be converted into 'XY'
        local t2: XY = xNyNz -- Type 'X & Y & Z' could not be converted into 'XY'
        local t3: XYZ = xNyNz -- Type 'X & Y & Z' could not be converted into 'XYZ'
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "less_greedy_unification_with_intersection_types")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local function f(t): { x: number } & { x: string }
            local x = t.x
            return t
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // TODO? We do not simplify types from explicit annotations.
    CHECK_EQ("({| x: number |} & {| x: string |}) -> {| x: number |} & {| x: string |}", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "less_greedy_unification_with_intersection_types_2")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local function f(t: { x: number } & { x: string })
            return t.x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("({ x: number } & { x: string }) -> never", toString(requireType("f")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_property_table_intersection_1")
{
    CheckResult result = check(R"(
type Foo = {
	Bar: string,
} & { Baz: number }

local x: Foo = { Bar = "1", Baz = 2 }
local y = x.Bar
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_property_table_intersection_2")
{
    CheckResult result = check(R"(
type Foo = {
	Bar: string,
} & { Baz: number }

local x: Foo = { Bar = "1", Baz = 2 }
local y = x["Bar"]
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "cli_80596_simplify_degenerate_intersections")
{
    ScopedFastFlag dcr{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
        type A = {
            x: number?,
        }

        type B = {
            x: number?,
        }

        type C = A & B
        local obj: C = {
            x = 3,
        }

        local x: number = obj.x or 3
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "cli_80596_simplify_more_realistic_intersections")
{
    ScopedFastFlag dcr{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
        type A = {
            x: number?,
            y: string?,
        }

        type B = {
            x: number?,
            z: string?,
        }

        type C = A & B
        local obj: C = {
            x = 3,
        }

        local x: number = obj.x or 3
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_SUITE_END();
