// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)

TEST_SUITE_BEGIN("UnionTypes");

TEST_CASE_FIXTURE(Fixture, "fuzzer_union_with_one_part_assertion")
{
    CheckResult result = check(R"(
local _ = {},nil
repeat

_,_ = if _.number == "" or _.number or _._ then
             _
      elseif _.__index == _._G then
            tostring
      elseif _ then
             _
      else
           ``,_._G

until _._
    )");
}

TEST_CASE_FIXTURE(Fixture, "return_types_can_be_disjoint")
{
    // CLI-114134 We need egraphs to consistently reduce the cyclic union
    // introduced by the increment here.
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        local count = 0
        function most_of_the_natural_numbers(): number?
            if count < 10 then
                count = count + 1
                return count
            else
                return nil
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionType* utv = get<FunctionType>(requireType("most_of_the_natural_numbers"));
    REQUIRE(utv != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "return_types_can_be_disjoint_using_compound_assignment")
{
    CheckResult result = check(R"(
        local count = 0
        function most_of_the_natural_numbers(): number?
            if count < 10 then
                -- count = count + 1
                count += 1
                return count
            else
                return nil
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionType* utv = get<FunctionType>(requireType("most_of_the_natural_numbers"));
    REQUIRE(utv != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "allow_specific_assign")
{
    CheckResult result = check(R"(
        local a:number|string = 22
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "allow_more_specific_assign")
{
    CheckResult result = check(R"(
        function f(a: number | string, b: (number | string)?)
            b = a
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "disallow_less_specific_assign")
{
    CheckResult result = check(R"(
        function f(a: number, b: number | string)
            a = b
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "optional_arguments")
{
    CheckResult result = check(R"(
        function f(a:string, b:string?)
        end
        f("s")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "optional_arguments_table")
{
    // CLI-115588 - Bidirectional inference does not happen for assignments
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        local a:{a:string, b:string?}
        a = {a="ok"}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "optional_arguments_table2")
{
    CheckResult result = check(R"(
        local a:{a:string, b:string}
        a = {a=""}
    )");
    REQUIRE(!result.errors.empty());
}

TEST_CASE_FIXTURE(BuiltinsFixture, "error_takes_optional_arguments")
{
    CheckResult result = check(R"(
        error("message")
        error("message", 2)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "error_optional_argument_enforces_type")
{
    CheckResult result = check(R"(
        error("message", "2")
    )");

    REQUIRE(result.errors.size() == 1);
}

TEST_CASE_FIXTURE(Fixture, "index_on_a_union_type_with_property_guaranteed_to_exist")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {x: number}

        function f(t: A | B)
            return t.x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("(A | B) -> number", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_a_union_type_with_mixed_types")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {x: string}

        function f(t: A | B)
            return t.x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("(A | B) -> number | string", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_a_union_type_works_at_arbitrary_depth")
{
    CheckResult result = check(R"(
        type A = {x: {y: {z: {thing: number}}}}
        type B = {x: {y: {z: {thing: string}}}}

        function f(t: A | B)
            return t.x.y.z.thing
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("(A | B) -> number | string", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_a_union_type_with_one_optional_property")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {x: number?}

        function f(t: A | B)
            return t.x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("(A | B) -> number?", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_a_union_type_with_missing_property")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {}

        function f(t: A | B)
            return t.x
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    MissingUnionProperty* mup = get<MissingUnionProperty>(result.errors[0]);
    REQUIRE(mup);
    CHECK_EQ("Key 'x' is missing from 'B' in the type 'A | B'", toString(result.errors[0]));

    if (FFlag::LuauSolverV2)
        CHECK_EQ("(A | B) -> number", toString(requireType("f")));
    else
        CHECK_EQ("(A | B) -> *error-type*", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_a_union_type_with_one_property_of_type_any")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {x: any}

        function f(t: A | B)
            return t.x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("(A | B) -> any", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "union_equality_comparisons")
{
    CheckResult result = check(R"(
        type A = number | string | nil
        type B = number | nil
        type C = number | boolean

        function f(a: A, b: B, c: C)
            local n = 1

            local x = a == b
            local y = a == n
            local z = a == c
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "optional_union_members")
{
    CheckResult result = check(R"(
        local a = { a = { x = 1, y = 2 }, b = 3 }
        type A = typeof(a)
        function f(b: A?)
            return b.a.y
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[0]));
    CHECK_EQ("(A?) -> number", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "optional_union_functions")
{
    CheckResult result = check(R"(
        local a = {}
        function a.foo(x:number, y:number) return x + y end
        type A = typeof(a)
        function f(b: A?)
            return b.foo(1, 2)
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[0]));
    CHECK_EQ("(A?) -> number", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "optional_union_methods")
{
    CheckResult result = check(R"(
        local a = {}
        function a:foo(x:number, y:number) return x + y end
        type A = typeof(a)
        function f(b: A?)
            return b:foo(1, 2)
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[0]));
    CHECK_EQ("(A?) -> number", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "optional_union_follow")
{
    CheckResult result = check(R"(
        local y: number? = 2
        local x = y
        function f(a: number, b: number?, c: number?) return -a end
        return f()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto acm = get<CountMismatch>(result.errors[0]);
    REQUIRE(acm);
    CHECK_EQ(1, acm->expected);
    CHECK_EQ(0, acm->actual);
    CHECK_FALSE(acm->isVariadic);
}

TEST_CASE_FIXTURE(Fixture, "optional_field_access_error")
{
    CheckResult result = check(R"(
        type A = { x: number }
        function f(b: A?)
            local c = b.x
            local d = b.y
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);
    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[0]));
    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[1]));
    CHECK_EQ("Key 'y' not found in table 'A'", toString(result.errors[2]));
}

TEST_CASE_FIXTURE(Fixture, "optional_index_error")
{
    CheckResult result = check(R"(
        type A = {number}
        function f(a: A?)
            local b = a[1]
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "optional_call_error")
{
    CheckResult result = check(R"(
        type A = (number) -> number
        function f(a: A?)
            local b = a(4)
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Value of type '((number) -> number)?' could be nil", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "optional_assignment_errors")
{
    CheckResult result = check(R"(
        type A = { x: number }
        function f(a: A?)
            a.x = 2
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "optional_assignment_errors_2")
{
    CheckResult result = check(R"(
        type A = { x: number } & { y: number }
        function f(a: A?)
            a.x = 2
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto s = toString(result.errors[0]);
    CHECK_EQ("Value of type '({ x: number } & { y: number })?' could be nil", s);
}

TEST_CASE_FIXTURE(Fixture, "optional_length_error")
{

    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type A = {number}
        function f(a: A?)
            local b = #a
        end
    )");

    // CLI-119936: This shouldn't double error but does under the new solver.
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ("Operator '#' could not be applied to operand of type A?; there is no corresponding overload for __len", toString(result.errors[0]));
    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[1]));
}

TEST_CASE_FIXTURE(Fixture, "optional_missing_key_error_details")
{
    CheckResult result = check(R"(
        type A = { x: number, y: number }
        type B = { x: number, y: number }
        type C = { x: number }
        type D = { x: number }

        function f(a: A | B | C | D)
            local y = a.y
            local z = a.z
        end

        function g(c: A | B | C | D | nil)
            local d = c.y
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK_EQ("Key 'y' is missing from 'C', 'D' in the type 'A | B | C | D'", toString(result.errors[0]));
    CHECK_EQ("Type 'A | B | C | D' does not have key 'z'", toString(result.errors[1]));

    CHECK_EQ("Value of type '(A | B | C | D)?' could be nil", toString(result.errors[2]));
    CHECK_EQ("Key 'y' is missing from 'C', 'D' in the type 'A | B | C | D'", toString(result.errors[3]));
}

TEST_CASE_FIXTURE(Fixture, "optional_iteration")
{
    CheckResult result = check(R"(
function foo(values: {number}?)
    local s = 0
    for _, value in values do
        s += value
    end
end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Value of type '{number}?' could be nil", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "unify_unsealed_table_union_check")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
local x = { x = 3 }
type A = number?
type B = string?
local y: { x: number, y: A | B }
y = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = check(R"(
local x = { x = 3 }

local a: number? = 2
local y = {}
y.x = 2
y.y = a

y = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "unify_sealed_table_union_check")
{
    CheckResult result = check(R"(
 -- the difference between this and unify_unsealed_table_union_check is the type annotation on x
local t = { x = 3, y = true }
local x: { x: number } = t
type A = number?
type B = string?
local y: { x: number, y: A | B }
-- Shouldn't typecheck!
y = x
-- If it does, we can convert any type to any other type
y.y = 5
local oh : boolean = t.y
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_union_part")
{
    CheckResult result = check(R"(
type X = { x: number }
type Y = { y: number }
type Z = { z: number }

type XYZ = X | Y | Z

function f(a: XYZ)
    local b: { w: number } = a
end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::LuauSolverV2)
    {

        CHECK_EQ(
            toString(result.errors[0]),
            "Type 'X | Y | Z' could not be converted into '{ w: number }'; \n"
            "this is because \n\t"
            " * the 1st component of the union is `X`, which is not a subtype of `{ w: number }`\n\t"
            " * the 2nd component of the union is `Y`, which is not a subtype of `{ w: number }`\n\t"
            " * the 3rd component of the union is `Z`, which is not a subtype of `{ w: number }`"
        );
    }
    else
    {
        CHECK_EQ(toString(result.errors[0]), R"(Type 'X | Y | Z' could not be converted into '{ w: number }'
caused by:
  Not all union options are compatible.
Table type 'X' not compatible with type '{ w: number }' because the former is missing field 'w')");
    }
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_union_all")
{
    CheckResult result = check(R"(
        type X = { x: number }
        type Y = { y: number }
        type Z = { z: number }

        type XYZ = X | Y | Z

        local a: XYZ = { w = 4 }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::LuauSolverV2)
        CHECK(toString(result.errors[0]) == "Type '{ w: number }' could not be converted into 'X | Y | Z'");
    else
        CHECK_EQ(toString(result.errors[0]), R"(Type 'a' could not be converted into 'X | Y | Z'; none of the union options are compatible)");
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_optional")
{
    CheckResult result = check(R"(
type X = { x: number }

local a: X? = { w = 4 }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::LuauSolverV2)
        CHECK("Table type '{ w: number }' not compatible with type 'X' because the former is missing field 'x'" == toString(result.errors[0]));
    else
    {
        const std::string expected = R"(Type 'a' could not be converted into 'X?'
caused by:
  None of the union options are compatible. For example:
Table type 'a' not compatible with type 'X' because the former is missing field 'x')";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

// We had a bug where a cyclic union caused a stack overflow.
// ex type U = number | U
TEST_CASE_FIXTURE(Fixture, "dont_allow_cyclic_unions_to_be_inferred")
{
    CheckResult result = check(R"(
        --!strict

        function f(a, b)
            a:g(b or {})
            a:g(b)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "indexing_into_a_cyclic_union_doesnt_crash")
{
    // It shouldn't be possible to craft a cyclic union, but even if we do, we
    // shouldn't blow up.

    TypeArena& arena = getFrontend().globals.globalTypes;
    unfreeze(arena);

    TypeId badCyclicUnionTy = arena.freshType(getBuiltins(), getFrontend().globals.globalScope.get());
    UnionType u;

    u.options.push_back(badCyclicUnionTy);
    u.options.push_back(arena.addType(
        TableType{
            {},
            TableIndexer{getBuiltins()->numberType, getBuiltins()->numberType},
            TypeLevel{},
            getFrontend().globals.globalScope.get(),
            TableState::Sealed
        }
    ));

    asMutable(badCyclicUnionTy)->ty.emplace<UnionType>(std::move(u));

    getFrontend().globals.globalScope->exportedTypeBindings["BadCyclicUnion"] = TypeFun{{}, badCyclicUnionTy};

    freeze(arena);

    CheckResult result = check(R"(
        function f(x: BadCyclicUnion)
            return x[0]
        end
    )");

    // this is a cyclic union of number arrays, so it _is_ a table, even if it's a nonsense type.
    // no need to generate a NotATable error here. The new solver automatically handles this and
    // correctly reports no errors.
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_union_write_indirect")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        type A = { x: number, y: (number) -> string } | { z: number, y: (number) -> string }

        function f(a: A)
            function a.y(x)
                return tostring(x * 2)
            end

            function a.y(x: string): number
                return tonumber(x) or 0
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    // NOTE: union normalization will improve this message

    const std::string expected = "Type\n\t"
                                 "'(string) -> number'"
                                 "\ncould not be converted into\n\t"
                                 "'((number) -> string) | ((number) -> string)'; none of the union options are compatible";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "union_true_and_false")
{
    CheckResult result = check(R"(
        function f(x : boolean)
            local y1 : (true | false) = x -- OK
            local y2 : (true | false | (string & number)) = x -- OK
            local y3 : (true | (string & number) | false) = x -- OK
            local y4 : (true | (boolean & true) | false) = x -- OK
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "union_of_functions")
{
    CheckResult result = check(R"(
        function f(x : (number) -> number?)
            local y : ((number?) -> number?) | ((number) -> number) = x -- OK
        end
     )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "union_of_generic_functions")
{
    CheckResult result = check(R"(
        function f(x : <a>(a) -> a?)
            local y : (<a>(a?) -> a?) | (<b>(b) -> b) = x -- Not OK
        end
     )");

    // TODO: should this example typecheck?
    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "union_of_generic_typepack_functions")
{
    CheckResult result = check(R"(
        function f(x : <a...>(number, a...) -> (number?, a...))
            local y : (<a...>(number?, a...) -> (number?, a...)) | (<b...>(number, b...) -> (number, b...)) = x -- Not OK
        end
     )");

    // TODO: should this example typecheck?
    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "union_of_functions_mentioning_generics")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f<a,b>()
            function g(x : (a) -> a?)
                local y : ((a?) -> nil) | ((a) -> a) = x -- OK
                local z : ((b?) -> nil) | ((b) -> b) = x -- Not OK
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(
        toString(result.errors[0]),
        "Type '(a) -> a?' could not be converted into '((b) -> b) | ((b?) -> nil)'; none of the union options are compatible"
    );
}

TEST_CASE_FIXTURE(Fixture, "union_of_functions_mentioning_generic_typepacks")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f<a...>()
            function g(x : (number, a...) -> (number?, a...))
                local y : ((number | string, a...) -> (number, a...)) | ((number?, a...) -> (nil, a...)) = x -- OK
                local z : ((number) -> number) | ((number?, a...) -> (number?, a...)) = x -- Not OK
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    const std::string expected = "Type\n\t"
                                 "'(number, a...) -> (number?, a...)'"
                                 "\ncould not be converted into\n\t"
                                 "'((number) -> number) | ((number?, a...) -> (number?, a...))'; none of the union options are compatible";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "union_of_functions_with_mismatching_arg_arities")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f(x : (number) -> number?)
            local y : ((number?) -> number) | ((number | string) -> nil) = x -- OK
            local z : ((number, string?) -> number) | ((number) -> nil) = x -- Not OK
        end
     )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    const std::string expected = "Type\n\t"
                                 "'(number) -> number?'"
                                 "\ncould not be converted into\n\t"
                                 "'((number) -> nil) | ((number, string?) -> number)'; none of the union options are compatible";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "union_of_functions_with_mismatching_result_arities")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f(x : () -> (number | string))
            local y : (() -> number) | (() -> string) = x -- OK
            local z : (() -> number) | (() -> (string, string)) = x -- Not OK
        end
     )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    const std::string expected = "Type\n\t"
                                 "'() -> number | string'"
                                 "\ncould not be converted into\n\t"
                                 "'(() -> (string, string)) | (() -> number)'; none of the union options are compatible";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "union_of_functions_with_variadics")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f(x : (...nil) -> (...number?))
            local y : ((...string?) -> (...number)) | ((...number?) -> nil) = x -- OK
            local z : ((...string?) -> (...number)) | ((...string?) -> nil) = x -- OK
        end
     )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    const std::string expected = "Type\n\t"
                                 "'(...nil) -> (...number?)'"
                                 "\ncould not be converted into\n\t"
                                 "'((...string?) -> (...number)) | ((...string?) -> nil)'; none of the union options are compatible";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "union_of_functions_with_mismatching_arg_variadics")
{
    CheckResult result = check(R"(
        function f(x : (number) -> ())
            local y : ((number?) -> ()) | ((...number) -> ()) = x -- OK
            local z : ((number?) -> ()) | ((...number?) -> ()) = x -- Not OK
        end
     )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::LuauSolverV2)
    {
        const std::string expected = "Type\n\t"
                                     "'(number) -> ()'"
                                     "\ncould not be converted into\n\t"
                                     "'((...number?) -> ()) | ((number?) -> ())'";
        CHECK(expected == toString(result.errors[0]));
    }
    else
    {
        const std::string expected = R"(Type
	'(number) -> ()'
could not be converted into
	'((...number?) -> ()) | ((number?) -> ())'; none of the union options are compatible)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "union_of_functions_with_mismatching_result_variadics")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f(x : () -> (number?, ...number))
            local y : (() -> (...number)) | (() -> nil) = x -- OK
            local z : (() -> (...number)) | (() -> number) = x -- OK
        end
     )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    const std::string expected = "Type\n\t"
                                 "'() -> (number?, ...number)'"
                                 "\ncould not be converted into\n\t"
                                 "'(() -> (...number)) | (() -> number)'; none of the union options are compatible";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "less_greedy_unification_with_union_types")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local function f(t): { x: number } | { x: string }
            local x = t.x
            return t
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("<a>(({ read x: a } & { x: number }) | ({ read x: a } & { x: string })) -> { x: number } | { x: string }", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "less_greedy_unification_with_union_types_2")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local function f(t: { x: number } | { x: string })
            return t.x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("({ x: number } | { x: string }) -> number | string", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "union_table_any_property")
{
    CheckResult result = check(R"(
        function f(x)
            -- x : X
            -- sup : { p : { q : X } }?
            local sup = if true then { p = { q = x } } else nil
            local sub : { p : any }
            sup = nil
            sup = sub
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "union_function_any_args")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f(sup : ((...any) -> (...any))?, sub : ((number) -> (...any)))
            sup = sub
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "optional_any")
{
    CheckResult result = check(R"(
        function f(sup : any?, sub : number)
            sup = sub
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_function_with_optional_arg")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function f<T>(x : T?) : {T}
            local result = {}
            if x then
                result[1] = x
            end
            return result
        end
        local t : {string} = f(nil)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "lookup_prop_of_intersection_containing_unions")
{
    CheckResult result = check(R"(
        local function mergeOptions<T>(options: T & ({} | {}))
            return options.variables
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    const UnknownProperty* unknownProp = get<UnknownProperty>(result.errors[0]);
    REQUIRE(unknownProp);

    CHECK("variables" == unknownProp->key);
}

TEST_CASE_FIXTURE(Fixture, "suppress_errors_for_prop_lookup_of_a_union_that_includes_error")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    registerHiddenTypes(getFrontend());

    CheckResult result = check(R"(
        function f(a: err | Not<nil>)
            local b = a.foo
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "handle_multiple_optionals")
{
    CheckResult result = check(R"(
        function f(a: string??)
            if a then
                print(a:sub(1,1))
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
