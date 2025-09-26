// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "ScopedFlags.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauReturnMappedGenericPacksFromSubtyping3)
LUAU_FASTFLAG(LuauSubtypingGenericPacksDoesntUseVariance)

TEST_SUITE_BEGIN("IntersectionTypes");

TEST_CASE_FIXTURE(Fixture, "select_correct_union_fn")
{
    CheckResult result = check(R"(
        type A = (number) -> (string)
        type B = (string) -> (number)

        local function foo(f: A & B)
            return f(10), f("a")
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("(((number) -> string) & ((string) -> number)) -> (string, number)", toString(requireType("foo")));
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

        local function f(t: A & B): A
            return t
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "fx_intersection_as_argument")
{
    CheckResult result = check(R"(
        type A = (number) -> (string)
        type B = (string) -> (number)
        type C = (A) -> (number)

        local function foo(f: A & B, g: C)
            return g(f)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "fx_union_as_argument_fails")
{
    CheckResult result = check(R"(
        type A = (number) -> (string)
        type B = (string) -> (number)
        type C = (A) -> (number)

        local function foo(f: A | B, g: C)
            return g(f)
        end
    )");

    REQUIRE(!result.errors.empty());
}

TEST_CASE_FIXTURE(Fixture, "argument_is_intersection")
{
    CheckResult result = check(R"(
        type A = (number | boolean) -> number

        local function foo(f: A)
            f(5)
            f(true)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "should_still_pick_an_overload_whose_arguments_are_unions")
{
    CheckResult result = check(R"(
        type A = (number) -> string
        type B = (string) -> number

        local function foo(f: A & B)
            return f(1), f("five")
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("(((number) -> string) & ((string) -> number)) -> (string, number)", toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "propagates_name")
{
    const std::string code = R"(
        type A={a:number}
        type B={b:string}

        local function f(t: A & B)
            return t
        end
    )";

    const std::string expected = R"(
        type A={a:number}
        type B={b:string}

        local function f(t: A & B): A&B
            return t
        end
    )";

    CHECK_EQ(expected, decorateWithTypes(code));
}

TEST_CASE_FIXTURE(Fixture, "index_on_an_intersection_type_with_property_guaranteed_to_exist")
{
    CheckResult result = check(R"(
        type A = {x: {y: number}}
        type B = {x: {y: number}}

        local function f(t: A & B)
            return t.x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::LuauSolverV2)
        CHECK("(A & B) -> { y: number }" == toString(requireType("f")));
    else
        CHECK("(A & B) -> { y: number } & { y: number }" == toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_an_intersection_type_works_at_arbitrary_depth")
{
    CheckResult result = check(R"(
        type A = {x: {y: {z: {thing: string}}}}
        type B = {x: {y: {z: {thing: string}}}}

        local function f(t: A & B)
            return t.x.y.z.thing
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::LuauSolverV2)
        CHECK_EQ("(A & B) -> string", toString(requireType("f")));
    else
        CHECK_EQ("(A & B) -> string & string", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_an_intersection_type_with_mixed_types")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {x: string}

        local function f(t: A & B)
            return t.x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::LuauSolverV2)
        CHECK_EQ("(A & B) -> never", toString(requireType("f")));
    else
        CHECK_EQ("(A & B) -> number & string", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_an_intersection_type_with_one_part_missing_the_property")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {}

        local function f(t: A & B)
            return t.x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("(A & B) -> number", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_an_intersection_type_with_one_property_of_type_any")
{
    CheckResult result = check(R"(
        type A = {y: number}
        type B = {x: any}

        local function f(t: A & B)
            return t.x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("(A & B) -> any", toString(requireType("f")));
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

        function f(t: XY)
            t.x = 10
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = check(R"(
        type X = {}
        type XY = X & { x: number, y: number }

        function f(t: XY)
            t.x = 10
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = check(R"(
        type X = { x: number }
        type Y = { y: number }
        type XY = X & Y

        function f(t: XY)
            t.x = 10
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = check(R"(
        type A = { x: {y: number} }
        type B = { x: {y: number} }

        function f(t: A & B)
            t.x = { y = 4 }
            t.x.y = 40
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "table_intersection_write_sealed")
{
    CheckResult result = check(R"(
        type X = { x: number }
        type Y = { y: number }
        type XY = X & Y

        function f(t: XY)
            t.z = 10
        end
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

        function f(t: XY)
            function t.z(a:number) return a * 10 end
            function t:y(a:number) return a * 10 end
            function t:w(a:number) return a * 10 end
        end
    )");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(3, result);
        CHECK_EQ(toString(result.errors[0]), "Cannot add property 'z' to table 'X & Y'");
        // I'm not writing this as a `toString` check, those are awful.
        auto err1 = get<TypeMismatch>(result.errors[1]);
        REQUIRE(err1);
        CHECK_EQ("number", toString(err1->givenType));
        CHECK_EQ("string", toString(err1->wantedType));
        CHECK_EQ(toString(result.errors[2]), "Cannot add property 'w' to table 'X & Y'");
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(4, result);

        const std::string expected = "Type\n\t"
                                     "'(string, number) -> string'"
                                     "\ncould not be converted into\n\t"
                                     "'(string) -> string'\n"
                                     "caused by:\n"
                                     "  Argument count mismatch. Function expects 2 arguments, but only 1 is specified";

        CHECK_EQ(expected, toString(result.errors[0]));
        CHECK_EQ(toString(result.errors[1]), "Cannot add property 'z' to table 'X & Y'");
        CHECK_EQ(toString(result.errors[2]), "Type 'number' could not be converted into 'string'");
        CHECK_EQ(toString(result.errors[3]), "Cannot add property 'w' to table 'X & Y'");
    }
}

TEST_CASE_FIXTURE(Fixture, "table_write_sealed_indirect")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();
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
    const std::string expected = "Type\n\t"
                                 "'(string, number) -> string'"
                                 "\ncould not be converted into\n\t"
                                 "'(string) -> string'\n"
                                 "caused by:\n"
                                 "  Argument count mismatch. Function expects 2 arguments, but only 1 is specified";
    CHECK_EQ(expected, toString(result.errors[0]));

    CHECK_EQ(toString(result.errors[1]), "Cannot add property 'z' to table 'XY'");
    CHECK_EQ(toString(result.errors[2]), "Type 'number' could not be converted into 'string'");
    CHECK_EQ(toString(result.errors[3]), "Cannot add property 'w' to table 'XY'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_intersection_setmetatable")
{
    CheckResult result = check(R"(
        function f(t: {} & {})
            setmetatable(t, {})
        end
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

    if (FFlag::LuauSolverV2)
    {
        const std::string expected = "Type "
                                     "'number'"
                                     " could not be converted into "
                                     "'X & Y & Z'; \n"
                                     "this is because \n\t"
                                     " * the 1st component of the intersection is `X`, and `number` is not a subtype of `X`\n\t"
                                     " * the 2nd component of the intersection is `Y`, and `number` is not a subtype of `Y`\n\t"
                                     " * the 3rd component of the intersection is `Z`, and `number` is not a subtype of `Z`";

        CHECK_EQ(expected, toString(result.errors[0]));
    }
    else
    {
        const std::string expected = R"(Type 'number' could not be converted into 'X & Y & Z'
caused by:
  Not all intersection parts are compatible.
Type 'number' could not be converted into 'X')";

        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_intersection_all")
{
    CheckResult result = check(R"(
type X = { x: number }
type Y = { y: number }
type Z = { z: number }
type XYZ = X & Y & Z

function f(a: XYZ): number
    return a
end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::LuauSolverV2)
    {
        const std::string expected = "Type "
                                     "'X & Y & Z'"
                                     " could not be converted into "
                                     "'number'; \n"
                                     "this is because \n\t"
                                     " * the 1st component of the intersection is `X`, which is not a subtype of `number`\n\t"
                                     " * the 2nd component of the intersection is `Y`, which is not a subtype of `number`\n\t"
                                     " * the 3rd component of the intersection is `Z`, which is not a subtype of `number`";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
    else
        CHECK_EQ(
            toString(result.errors[0]), R"(Type 'X & Y & Z' could not be converted into 'number'; none of the intersection parts are compatible)"
        );
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
        function f(x: boolean & false)
            local y : false = x -- OK
            local z : true = x  -- Not OK
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::LuauSolverV2)
    {
        const std::string expected = "Type "
                                     "'boolean & false'"
                                     " could not be converted into "
                                     "'true'; \n"
                                     "this is because \n\t"
                                     " * the 1st component of the intersection is `boolean`, which is not a subtype of `true`\n\t"
                                     " * the 2nd component of the intersection is `false`, which is not a subtype of `true`";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
    else
        CHECK_EQ(
            toString(result.errors[0]), "Type 'boolean & false' could not be converted into 'true'; none of the intersection parts are compatible"
        );
}

TEST_CASE_FIXTURE(Fixture, "intersect_false_and_bool_and_false")
{
    CheckResult result = check(R"(
        function f(x: false & (boolean & false))
            local y : false = x -- OK
            local z : true = x  -- Not OK
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    // TODO: odd stringification of `false & (boolean & false)`.)
    if (FFlag::LuauSolverV2)
    {
        const std::string expected = "Type "
                                     "'boolean & false & false'"
                                     " could not be converted into "
                                     "'true'; \n"
                                     "this is because \n\t"
                                     " * the 1st component of the intersection is `false`, which is not a subtype of `true`\n\t"
                                     " * the 2nd component of the intersection is `boolean`, which is not a subtype of `true`\n\t"
                                     " * the 3rd component of the intersection is `false`, which is not a subtype of `true`";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
    else
        CHECK_EQ(
            toString(result.errors[0]),
            "Type 'boolean & false & false' could not be converted into 'true'; none of the intersection parts are compatible"
        );
}

TEST_CASE_FIXTURE(Fixture, "intersect_saturate_overloaded_functions")
{
    CheckResult result = check(R"(
        function foo(x: ((number?) -> number?) & ((string?) -> string?))
            local y : (nil) -> nil = x -- Not OK (fixed in DCR)
            local z : (number) -> number = x -- Not OK
        end
    )");

    if (FFlag::LuauSolverV2)
    {
        const std::string expected1 =
            "Type\n\t"
            "'((number?) -> number?) & ((string?) -> string?)'"
            "\ncould not be converted into\n\t"
            "'(nil) -> nil'; \n"
            "this is because \n\t"
            " * in the 1st component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "union as `number` and it returns the 1st entry in the type pack is `nil`, and `number` is not a subtype of `nil`\n\t"
            " * in the 2nd component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "union as `string` and it returns the 1st entry in the type pack is `nil`, and `string` is not a subtype of `nil`";

        const std::string expected2 =
            "Type\n\t"
            "'((number?) -> number?) & ((string?) -> string?)'"
            "\ncould not be converted into\n\t"
            "'(number) -> number'; \n"
            "this is because \n\t"
            " * in the 1st component of the intersection, the function returns the 1st entry in the type pack which has the 2nd component of the "
            "union as `nil` and it returns the 1st entry in the type pack is `number`, and `nil` is not a subtype of `number`\n\t"
            " * in the 2nd component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "union as `string` and it returns the 1st entry in the type pack is `number`, and `string` is not a subtype of `number`\n\t"
            " * in the 2nd component of the intersection, the function returns the 1st entry in the type pack which has the 2nd component of the "
            "union as `nil` and it returns the 1st entry in the type pack is `number`, and `nil` is not a subtype of `number`\n\t"
            " * in the 2nd component of the intersection, the function takes the 1st entry in the type pack which is `string?` and it takes the 1st "
            "entry in the type pack is `number`, and `string?` is not a supertype of `number`";

        CHECK_EQ(expected1, toString(result.errors[0]));
        CHECK_EQ(expected2, toString(result.errors[1]));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        const std::string expected = R"(Type
	'((number?) -> number?) & ((string?) -> string?)'
could not be converted into
	'(number) -> number'; none of the intersection parts are compatible)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "union_saturate_overloaded_functions")
{
    // CLI-116474 Semantic subtyping of assignments needs to decide how to interpret intersections of functions
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f(x: ((number) -> number) & ((string) -> string))
            local y : ((number | string) -> (number | string)) = x -- OK
            local z : ((number | boolean) -> (number | boolean)) = x -- Not OK
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    const std::string expected = "Type\n\t"
                                 "'((number) -> number) & ((string) -> string)'"
                                 "\ncould not be converted into\n\t"
                                 "'(boolean | number) -> boolean | number'; none of the intersection parts are compatible";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "intersection_of_tables")
{
    CheckResult result = check(R"(
        function f(x: { p : number?, q : string? } & { p : number?, q : number?, r : number? })
            local y : { p : number?, q : nil, r : number? } = x -- OK
            local z : { p : nil } = x -- Not OK
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::LuauSolverV2)
    {
        const std::string expected = "Type "
                                     "'{ p: number?, q: number?, r: number? } & { p: number?, q: string? }'"
                                     " could not be converted into "
                                     "'{ p: nil }'; \n"
                                     "this is because \n\t"
                                     " * in the 1st component of the intersection, accessing `p` has the 1st component of the union as `number` and "
                                     "accessing `p` results in `nil`, and `number` is not exactly `nil`\n\t"
                                     " * in the 2nd component of the intersection, accessing `p` has the 1st component of the union as `number` and "
                                     "accessing `p` results in `nil`, and `number` is not exactly `nil`";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
    else
    {
        const std::string expected =
            R"(Type '{ p: number?, q: number?, r: number? } & { p: number?, q: string? }' could not be converted into '{ p: nil }'; none of the intersection parts are compatible)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "intersection_of_tables_with_top_properties")
{
    CheckResult result = check(R"(
        function f(x : { p : number?, q : any } & { p : unknown, q : string? })
            local y : { p : number?, q : string? } = x -- OK
            local z : { p : string?, q : number? } = x -- Not OK
        end
    )");

    if (FFlag::LuauSolverV2)
    {
        const std::string expected = "Type\n\t"
                                     "'{ p: number?, q: any } & { p: unknown, q: string? }'"
                                     "\ncould not be converted into\n\t"
                                     "'{ p: string?, q: number? }'; \n"
                                     "this is because \n\t"
                                     " * in the 1st component of the intersection, accessing `p` has the 1st component of the union as `number` and "
                                     "accessing `p` results in `string?`, and `number` is not exactly `string?`\n\t"
                                     " * in the 1st component of the intersection, accessing `p` results in `number?` and accessing `p` has the 1st "
                                     "component of the union as `string`, and `number?` is not exactly `string`\n\t"
                                     " * in the 1st component of the intersection, accessing `q` results in `any` and accessing `q` results in "
                                     "`number?`, and `any` is not exactly `number?`\n\t"
                                     " * in the 2nd component of the intersection, accessing `p` results in `unknown` and accessing `p` results in "
                                     "`string?`, and `unknown` is not exactly `string?`\n\t"
                                     " * in the 2nd component of the intersection, accessing `q` has the 1st component of the union as `string` and "
                                     "accessing `q` results in `number?`, and `string` is not exactly `number?`\n\t"
                                     " * in the 2nd component of the intersection, accessing `q` results in `string?` and accessing `q` has the 1st "
                                     "component of the union as `number`, and `string?` is not exactly `number`";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        const std::string expected = R"(Type
	'{ p: number?, q: any } & { p: unknown, q: string? }'
could not be converted into
	'{ p: string?, q: number? }'; none of the intersection parts are compatible)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "intersection_of_tables_with_never_properties")
{
    CheckResult result = check(R"(
        function f(x : { p : number?, q : never } & { p : never, q : string? })
            local y : { p : never, q : never } = x -- OK
            local z : never = x -- OK
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "overloaded_functions_returning_intersections")
{
    CheckResult result = check(R"(
        function f(x : ((number?) -> ({ p : number } & { q : number })) & ((string?) -> ({ p : number } & { r : number })))
            local y : (nil) -> { p : number, q : number, r : number} = x -- OK
            local z : (number?) -> { p : number, q : number, r : number} = x -- Not OK
        end
    )");

    if (FFlag::LuauSolverV2)
    {
        const std::string expected1 =
            "Type\n\t"
            "'((number?) -> { p: number } & { q: number }) & ((string?) -> { p: number } & { r: number })'"
            "\ncould not be converted into\n\t"
            "'(nil) -> { p: number, q: number, r: number }'; \n"
            "this is because \n\t"
            " * in the 1st component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "intersection as `{ p: number }` and it returns the 1st entry in the type pack is `{ p: number, q: number, r: number }`, and `{ p: "
            "number }` is not a subtype of `{ p: number, q: number, r: number }`\n\t"
            " * in the 1st component of the intersection, the function returns the 1st entry in the type pack which has the 2nd component of the "
            "intersection as `{ q: number }` and it returns the 1st entry in the type pack is `{ p: number, q: number, r: number }`, and `{ q: "
            "number }` is not a subtype of `{ p: number, q: number, r: number }`\n\t"
            " * in the 2nd component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "intersection as `{ p: number }` and it returns the 1st entry in the type pack is `{ p: number, q: number, r: number }`, and `{ p: "
            "number }` is not a subtype of `{ p: number, q: number, r: number }`\n\t"
            " * in the 2nd component of the intersection, the function returns the 1st entry in the type pack which has the 2nd component of the "
            "intersection as `{ r: number }` and it returns the 1st entry in the type pack is `{ p: number, q: number, r: number }`, and `{ r: "
            "number }` is not a subtype of `{ p: number, q: number, r: number }`";

        const std::string expected2 =
            "Type\n\t"
            "'((number?) -> { p: number } & { q: number }) & ((string?) -> { p: number } & { r: number })'"
            "\ncould not be converted into\n\t"
            "'(number?) -> { p: number, q: number, r: number }'; \n"
            "this is because \n\t"
            " * in the 1st component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "intersection as `{ p: number }` and it returns the 1st entry in the type pack is `{ p: number, q: number, r: number }`, and `{ p: "
            "number }` is not a subtype of `{ p: number, q: number, r: number }`\n\t"
            " * in the 1st component of the intersection, the function returns the 1st entry in the type pack which has the 2nd component of the "
            "intersection as `{ q: number }` and it returns the 1st entry in the type pack is `{ p: number, q: number, r: number }`, and `{ q: "
            "number }` is not a subtype of `{ p: number, q: number, r: number }`\n\t"
            " * in the 2nd component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "intersection as `{ p: number }` and it returns the 1st entry in the type pack is `{ p: number, q: number, r: number }`, and `{ p: "
            "number }` is not a subtype of `{ p: number, q: number, r: number }`\n\t"
            " * in the 2nd component of the intersection, the function returns the 1st entry in the type pack which has the 2nd component of the "
            "intersection as `{ r: number }` and it returns the 1st entry in the type pack is `{ p: number, q: number, r: number }`, and `{ r: "
            "number }` is not a subtype of `{ p: number, q: number, r: number }`\n\t"
            " * in the 2nd component of the intersection, the function takes the 1st entry in the type pack which is `string?` and it takes the 1st "
            "entry in the type pack has the 1st component of the union as `number`, and `string?` is not a supertype of `number`";

        CHECK_EQ(expected1, toString(result.errors[0]));
        CHECK_EQ(expected2, toString(result.errors[1]));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK_EQ(
            R"(Type
	'((number?) -> { p: number } & { q: number }) & ((string?) -> { p: number } & { r: number })'
could not be converted into
	'(number?) -> { p: number, q: number, r: number }'; none of the intersection parts are compatible)",
            toString(result.errors[0])
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "overloaded_functions_mentioning_generic")
{
    CheckResult result = check(R"(
        function f<a>()
            function g(x : ((number?) -> (a | number)) & ((string?) -> (a | string)))
                local y : (nil) -> a = x -- OK
                local z : (number?) -> a = x -- Not OK
            end
        end
    )");
    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(0, result);
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        const std::string expected = R"(Type
	'((number?) -> a | number) & ((string?) -> a | string)'
could not be converted into
	'(number?) -> a'; none of the intersection parts are compatible)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "overloaded_functions_mentioning_generics")
{
    CheckResult result = check(R"(
        function f<a,b,c>()
            function g(x : ((a?) -> (a | b)) & ((c?) -> (b | c)))
                local y : (nil) -> ((a & c) | b) = x -- OK
                local z : (a?) -> ((a & c) | b) = x -- Not OK
            end
        end
    )");


    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        const std::string expected = R"(Type
	'((a?) -> a | b) & ((c?) -> b | c)'
could not be converted into
	'(a?) -> (a & c) | b'; none of the intersection parts are compatible)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "overloaded_functions_mentioning_generic_packs")
{
    ScopedFastFlag sff{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
        function f<a...,b...>()
            function g(x : ((number?, a...) -> (number?, b...)) & ((string?, a...) -> (string?, b...)))
                local y : ((nil, a...) -> (nil, b...)) = x -- OK in the old solver, not OK in the new
                local z : ((nil, b...) -> (nil, a...)) = x -- Not OK
                local w : ((number?, a...) -> (number?, b...)) = x -- OK in both solvers
            end
        end
    )");
    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        const TypeMismatch* tm1 = get<TypeMismatch>(result.errors[0]);
        CHECK(tm1);
        CHECK_EQ(toString(tm1->wantedType), "(nil, a...) -> (nil, b...)");
        CHECK_EQ(toString(tm1->givenType), "((number?, a...) -> (number?, b...)) & ((string?, a...) -> (string?, b...))");
        const TypeMismatch* tm2 = get<TypeMismatch>(result.errors[1]);
        CHECK(tm2);
        CHECK_EQ(toString(tm2->wantedType), "(nil, b...) -> (nil, a...)");
        CHECK_EQ(toString(tm2->givenType), "((number?, a...) -> (number?, b...)) & ((string?, a...) -> (string?, b...))");

        const std::string expected1 =
            "Type\n\t"
            "'((number?, a...) -> (number?, b...)) & ((string?, a...) -> (string?, b...))'"
            "\ncould not be converted into\n\t"
            "'(nil, a...) -> (nil, b...)'; \n"
            "this is because \n\t"
            " * in the 1st component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "union as `number` and it returns the 1st entry in the type pack is `nil`, and `number` is not a subtype of `nil`\n\t"
            " * in the 2nd component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "union as `string` and it returns the 1st entry in the type pack is `nil`, and `string` is not a subtype of `nil`";

        const std::string expected2 =
            "Type\n\t"
            "'((number?, a...) -> (number?, b...)) & ((string?, a...) -> (string?, b...))'"
            "\ncould not be converted into\n\t"
            "'(nil, b...) -> (nil, a...)'; \n"
            "this is because \n\t"
            " * in the 1st component of the intersection, the function returns a tail of `b...` and it returns a tail of `a...`, and `b...` is not a "
            "subtype of `a...`\n\t"
            " * in the 1st component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "union as `number` and it returns the 1st entry in the type pack is `nil`, and `number` is not a subtype of `nil`\n\t"
            " * in the 1st component of the intersection, the function takes a tail of `a...` and it takes a tail of `b...`, and `a...` is not a "
            "supertype of `b...`\n\t"
            " * in the 2nd component of the intersection, the function returns a tail of `b...` and it returns a tail of `a...`, and `b...` is not a "
            "subtype of `a...`\n\t"
            " * in the 2nd component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "union as `string` and it returns the 1st entry in the type pack is `nil`, and `string` is not a subtype of `nil`\n\t"
            " * in the 2nd component of the intersection, the function takes a tail of `a...` and it takes a tail of `b...`, and `a...` is not a "
            "supertype of `b...`";

        CHECK_EQ(expected1, toString(result.errors[0]));
        CHECK_EQ(expected2, toString(result.errors[1]));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        const std::string expected = R"(Type
	'((number?, a...) -> (number?, b...)) & ((string?, a...) -> (string?, b...))'
could not be converted into
	'(nil, b...) -> (nil, a...)'; none of the intersection parts are compatible)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_unknown_result")
{
    // CLI-116474 Semantic subtyping of assignments needs to decide how to interpret intersections of functions
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f<a...,b...>()
            function g(x : ((number) -> number) & ((nil) -> unknown))
                local y : (number?) -> unknown = x -- OK
                local z : (number?) -> number? = x -- Not OK
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    const std::string expected = "Type\n\t"
                                 "'((nil) -> unknown) & ((number) -> number)'"
                                 "\ncould not be converted into\n\t"
                                 "'(number?) -> number?'; none of the intersection parts are compatible";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_unknown_arguments")
{
    // CLI-116474 Semantic subtyping of assignments needs to decide how to interpret intersections of functions
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f<a...,b...>()
            function g(x : ((number) -> number?) & ((unknown) -> string?))
                local y : (number) -> nil = x -- OK
                local z : (number?) -> nil = x -- Not OK
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    const std::string expected = "Type\n\t"
                                 "'((number) -> number?) & ((unknown) -> string?)'"
                                 "\ncould not be converted into\n\t"
                                 "'(number?) -> nil'; none of the intersection parts are compatible";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_never_result")
{
    CheckResult result = check(R"(
    function f<a...,b...>()
        function g(x : ((number) -> number) & ((nil) -> never))
            local y : (number?) -> number = x -- OK
            local z : (number?) -> never = x -- Not OK
        end
    end
    )");

    if (FFlag::LuauSolverV2)
    {
        const std::string expected1 =
            "Type\n\t"
            "'((nil) -> never) & ((number) -> number)'"
            "\ncould not be converted into\n\t"
            "'(number?) -> number'; \n"
            "this is because \n\t"
            " * in the 1st component of the intersection, the function takes the 1st entry in the type pack which is `number` and it takes the 1st "
            "entry in the type pack has the 2nd component of the union as `nil`, and `number` is not a supertype of `nil`\n\t"
            " * in the 2nd component of the intersection, the function takes the 1st entry in the type pack which is `nil` and it takes the 1st "
            "entry in the type pack has the 1st component of the union as `number`, and `nil` is not a supertype of `number`";

        const std::string expected2 =
            "Type\n\t"
            "'((nil) -> never) & ((number) -> number)'"
            "\ncould not be converted into\n\t"
            "'(number?) -> never'; \n"
            "this is because \n\t"
            " * in the 1st component of the intersection, the function returns the 1st entry in the type pack which is `number` and it returns the "
            "1st entry in the type pack is `never`, and `number` is not a subtype of `never`\n\t"
            " * in the 1st component of the intersection, the function takes the 1st entry in the type pack which is `number` and it takes the 1st "
            "entry in the type pack has the 2nd component of the union as `nil`, and `number` is not a supertype of `nil`\n\t"
            " * in the 2nd component of the intersection, the function takes the 1st entry in the type pack which is `nil` and it takes the 1st "
            "entry in the type pack has the 1st component of the union as `number`, and `nil` is not a supertype of `number`";

        CHECK_EQ(expected1, toString(result.errors[0]));
        CHECK_EQ(expected2, toString(result.errors[1]));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        const std::string expected = R"(Type
	'((nil) -> never) & ((number) -> number)'
could not be converted into
	'(number?) -> never'; none of the intersection parts are compatible)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_never_arguments")
{
    CheckResult result = check(R"(
        function f<a...,b...>()
            function g(x : ((number) -> number?) & ((never) -> string?))
                local y : (never) -> nil = x -- OK
                local z : (number?) -> nil = x -- Not OK
            end
        end
    )");

    if (FFlag::LuauSolverV2)
    {
        const std::string expected1 =
            "Type\n\t"
            "'((never) -> string?) & ((number) -> number?)'"
            "\ncould not be converted into\n\t"
            "'(never) -> nil'; \n"
            "this is because \n\t"
            " * in the 1st component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "union as `number` and it returns the 1st entry in the type pack is `nil`, and `number` is not a subtype of `nil`\n\t"
            " * in the 2nd component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "union as `string` and it returns the 1st entry in the type pack is `nil`, and `string` is not a subtype of `nil`";

        const std::string expected2 =
            "Type\n\t"
            "'((never) -> string?) & ((number) -> number?)'"
            "\ncould not be converted into\n\t"
            "'(number?) -> nil'; \n"
            "this is because \n\t"
            " * in the 1st component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "union as `number` and it returns the 1st entry in the type pack is `nil`, and `number` is not a subtype of `nil`\n\t"
            " * in the 1st component of the intersection, the function takes the 1st entry in the type pack which is `number` and it takes the 1st "
            "entry in the type pack has the 2nd component of the union as `nil`, and `number` is not a supertype of `nil`\n\t"
            " * in the 2nd component of the intersection, the function returns the 1st entry in the type pack which has the 1st component of the "
            "union as `string` and it returns the 1st entry in the type pack is `nil`, and `string` is not a subtype of `nil`\n\t"
            " * in the 2nd component of the intersection, the function takes the 1st entry in the type pack which is `never` and it takes the 1st "
            "entry in the type pack has the 1st component of the union as `number`, and `never` is not a supertype of `number`\n\t"
            " * in the 2nd component of the intersection, the function takes the 1st entry in the type pack which is `never` and it takes the 1st "
            "entry in the type pack has the 2nd component of the union as `nil`, and `never` is not a supertype of `nil`";

        CHECK_EQ(expected1, toString(result.errors[0]));
        CHECK_EQ(expected2, toString(result.errors[1]));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        const std::string expected = R"(Type
	'((never) -> string?) & ((number) -> number?)'
could not be converted into
	'(number?) -> nil'; none of the intersection parts are compatible)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_overlapping_results_and_variadics")
{
    // CLI-116474 Semantic subtyping of assignments needs to decide how to interpret intersections of functions
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f(x : ((string?) -> (string | number)) & ((number?) -> ...number))
            local y : ((nil) -> (number, number?)) = x -- OK
            local z : ((string | number) -> (number, number?)) = x -- Not OK
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    const std::string expected = "Type\n\t"
                                 "'((number?) -> (...number)) & ((string?) -> number | string)'"
                                 "\ncould not be converted into\n\t"
                                 "'(number | string) -> (number, number?)'; none of the intersection parts are compatible";
    CHECK(expected == toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_weird_typepacks_1")
{
    CheckResult result = check(R"(
        function f<a...,b...>()
            function g(x : (() -> a...) & (() -> b...))
                local y : (() -> b...) & (() -> a...) = x -- OK
                local z : () -> () = x -- Not OK
            end
        end
    )");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK_EQ(
            toString(result.errors[0]),
            "Type '(() -> (a...)) & (() -> (b...))' could not be converted into '() -> ()'; none of the intersection parts are compatible"
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_weird_typepacks_2")
{
    ScopedFastFlag _{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
        function f<a...,b...>()
            function g(x : ((a...) -> ()) & ((b...) -> ()))
                local y : ((b...) -> ()) & ((a...) -> ()) = x -- OK
                local z : () -> () = x -- Not OK
            end
        end
    )");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        const TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
        CHECK(tm);
        CHECK_EQ(toString(tm->wantedType), "() -> ()");
        CHECK_EQ(toString(tm->givenType), "((a...) -> ()) & ((b...) -> ())");
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK_EQ(
            toString(result.errors[0]),
            "Type '((a...) -> ()) & ((b...) -> ())' could not be converted into '() -> ()'; none of the intersection parts are compatible"
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_weird_typepacks_3")
{
    ScopedFastFlag sff1{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};
    ScopedFastFlag sff2{FFlag::LuauReturnMappedGenericPacksFromSubtyping3, true};

    CheckResult result = check(R"(
        function f<a...>()
            function g(x : (() -> a...) & (() -> (number?,a...)))
                local y : (() -> (number?,a...)) & (() -> a...) = x -- OK
                local z : () -> (number) = x -- Not OK
            end
        end
    )");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        const TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
        CHECK(tm);
        CHECK_EQ(toString(tm->wantedType), "() -> number");
        CHECK_EQ(toString(tm->givenType), "(() -> (a...)) & (() -> (number?, a...))");
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        const std::string expected = R"(Type
	'(() -> (a...)) & (() -> (number?, a...))'
could not be converted into
	'() -> number'; none of the intersection parts are compatible)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "overloadeded_functions_with_weird_typepacks_4")
{
    ScopedFastFlag _{FFlag::LuauReturnMappedGenericPacksFromSubtyping3, true};
    ScopedFastFlag sff{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
        function f<a...>()
            function g(x : ((a...) -> ()) & ((number,a...) -> number))
                local y : ((number,a...) -> number) & ((a...) -> ()) = x -- OK
                local z : (number?) -> () = x -- Not OK
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::LuauSolverV2)
    {
        const TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
        CHECK(tm);
        CHECK_EQ(toString(tm->wantedType), "(number?) -> ()");
        CHECK_EQ(toString(tm->givenType), "((a...) -> ()) & ((number, a...) -> number)");
        const std::string expected =
            "Type\n\t"
            "'((a...) -> ()) & ((number, a...) -> number)'"
            "\ncould not be converted into\n\t"
            "'(number?) -> ()'; \n"
            "this is because \n\t"
            " * in the 1st component of the intersection, the function takes a tail of `a...` and it takes the portion of the type pack starting at "
            "index 0 to the end`number?`, and `a...` is not a supertype of `number?`\n\t"
            " * in the 2nd component of the intersection, the function returns is `number` and it returns `()`, and `number` is not a subtype of "
            "`()`\n\t"
            " * in the 2nd component of the intersection, the function takes a tail of `a...` and it takes `number?`, and `a...` is not a supertype "
            "of `number?`\n\t"
            " * in the 2nd component of the intersection, the function takes the 1st entry in the type pack which is `number` and it takes the 1st "
            "entry in the type pack has the 2nd component of the union as `nil`, and `number` is not a supertype of `nil`";
        CHECK(expected == toString(result.errors[0]));
    }
    else
    {
        CHECK_EQ(
            R"(Type
	'((a...) -> ()) & ((number, a...) -> number)'
could not be converted into
	'(number?) -> ()'; none of the intersection parts are compatible)",
            toString(result.errors[0])
        );
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "intersect_metatables")
{
    // CLI-117121 - Intersection of types are not compatible with the equivalent alias
    if (FFlag::LuauSolverV2)
        return;

    if (FFlag::LuauSolverV2)
    {
        CheckResult result = check(R"(
            function f(a: string?, b: string?)
                local x = setmetatable({}, { p = 5, q = a })
                local y = setmetatable({}, { q = b, r = "hi" })
                local z = setmetatable({}, { p = 5, q = nil, r = "hi" })

                type X = typeof(x)
                type Y = typeof(y)
                type Z = typeof(z)

                function g(xy: X&Y, yx: Y&X): (Z, Z)
                    return xy, yx
                end

                g(z, z)
            end
        )");

        LUAU_REQUIRE_NO_ERRORS(result);
    }
    else
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
}

TEST_CASE_FIXTURE(BuiltinsFixture, "intersect_metatable_subtypes")
{
    CheckResult result = check(R"(
        local x = setmetatable({ a = 5 }, { p = 5 })
        local y = setmetatable({ b = "hi" }, { p = 5, q = "hi" })
        local z = setmetatable({ a = 5, b = "hi" }, { p = 5, q = "hi" })

        type X = typeof(x)
        type Y = typeof(y)
        type Z = typeof(z)

        function f(xy: X&Y, yx: Y&X): (Z, Z)
            return xy, yx
        end

        f(z, z)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "intersect_metatables_with_properties")
{
    CheckResult result = check(R"(
        local x = setmetatable({ a = 5 }, { p = 5 })
        local y = setmetatable({ b = "hi" }, { q = "hi" })
        local z = setmetatable({ a = 5, b = "hi" }, { p = 5, q = "hi" })

        type X = typeof(x)
        type Y = typeof(y)
        type Z = typeof(z)

        function f(xy: X&Y): Z
            return xy
        end

        f(z)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "intersect_metatable_with_table")
{
    if (FFlag::LuauSolverV2)
    {
        CheckResult result = check(R"(
            local x = setmetatable({ a = 5 }, { p = 5 })
            local z = setmetatable({ a = 5, b = "hi" }, { p = 5 })

            type X = typeof(x)
            type Y = { b : string }
            type Z = typeof(z)

            function f(xy: X&Y, yx: Y&X): (Z, Z)
                return xy, yx
            end

            f(z, z)
        )");

        LUAU_REQUIRE_NO_ERRORS(result);
    }
    else
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
}

TEST_CASE_FIXTURE(Fixture, "CLI-44817")
{
    CheckResult result = check(R"(
        type X = {x: number}
        type Y = {y: number}
        type Z = {z: number}

        type XY = {x: number, y: number}
        type XYZ = {x:number, y: number, z: number}

        function f(xy: XY, xyz: XYZ): (X&Y, X&Y&Z)
            return xy, xyz
        end

        local xNy, xNyNz = f({x = 0, y = 0}, {x = 0, y = 0, z = 0})

        local t1: XY = xNy -- Type 'X & Y' could not be converted into 'XY'
        local t2: XY = xNyNz -- Type 'X & Y & Z' could not be converted into 'XY'
        local t3: XYZ = xNyNz -- Type 'X & Y & Z' could not be converted into 'XYZ'
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "less_greedy_unification_with_intersection_types")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local function f(t): { x: number } & { x: string }
            local x = t.x
            return t
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);

    CHECK_EQ("(never) -> { x: number } & { x: string }", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "less_greedy_unification_with_intersection_types_2")
{
    if (!FFlag::LuauSolverV2)
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

function f(x: Foo)
    return x.Bar
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_property_table_intersection_2")
{
    CheckResult result = check(R"(
        type Foo = {
            Bar: string,
        } & { Baz: number }

        function f(x: Foo)
            return x["Bar"]
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "cli_80596_simplify_degenerate_intersections")
{
    ScopedFastFlag dcr{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type A = {
            x: number?,
        }

        type B = {
            x: number?,
        }

        type C = A & B

        function f(obj: C): number
            return obj.x or 3
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "cli_80596_simplify_more_realistic_intersections")
{
    ScopedFastFlag dcr{FFlag::LuauSolverV2, true};

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

        function f(obj: C): number
            return obj.x or 3
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "narrow_intersection_nevers")
{
    ScopedFastFlag sffs{FFlag::LuauSolverV2, true};

    loadDefinition(R"(
        declare class Player
            Character: unknown
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local function foo(player: Player?)
            if player and player.Character then
                print(player.Character)
            end
        end
    )"));

    CHECK_EQ("Player & { read Character: ~(false?) }", toString(requireTypeAtPosition({3, 23})));
}

TEST_SUITE_END();
