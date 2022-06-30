// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"

#include "Fixture.h"

#include "doctest.h"

LUAU_FASTFLAG(LuauLowerBoundsCalculation)

using namespace Luau;

TEST_SUITE_BEGIN("UnionTypes");

TEST_CASE_FIXTURE(Fixture, "return_types_can_be_disjoint")
{
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

    const FunctionTypeVar* utv = get<FunctionTypeVar>(requireType("most_of_the_natural_numbers"));
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
        local a:number|string = 22
        local b:number|string|nil = a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "disallow_less_specific_assign")
{
    CheckResult result = check(R"(
        local a:number = 10
        local b:number|string = 20
        a = b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "disallow_less_specific_assign2")
{
    CheckResult result = check(R"(
        local a:number? = 10
        local b:number|string? = 20
        a = b
    )");

    REQUIRE_EQ(1, result.errors.size());
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
        local t: A | B

        local r = t.x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*typeChecker.numberType, *requireType("r"));
}

TEST_CASE_FIXTURE(Fixture, "index_on_a_union_type_with_mixed_types")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {x: string}
        local t: A | B

        local r = t.x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number | string", toString(requireType("r")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_a_union_type_works_at_arbitrary_depth")
{
    CheckResult result = check(R"(
        type A = {x: {y: {z: {thing: number}}}}
        type B = {x: {y: {z: {thing: string}}}}
        local t: A | B

        local r = t.x.y.z.thing
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number | string", toString(requireType("r")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_a_union_type_with_one_optional_property")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {x: number?}
        local t: A | B

        local r = t.x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number?", toString(requireType("r")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_a_union_type_with_missing_property")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {}
        local t: A | B

        local r = t.x
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    MissingUnionProperty* mup = get<MissingUnionProperty>(result.errors[0]);
    REQUIRE(mup);
    CHECK_EQ(mup->type, requireType("t"));
    REQUIRE(mup->missing.size() == 1);
    std::optional<TypeId> bTy = lookupType("B");
    REQUIRE(bTy);
    CHECK_EQ(mup->missing[0], *bTy);
    CHECK_EQ(mup->key, "x");

    CHECK_EQ("*unknown*", toString(requireType("r")));
}

TEST_CASE_FIXTURE(Fixture, "index_on_a_union_type_with_one_property_of_type_any")
{
    CheckResult result = check(R"(
        type A = {x: number}
        type B = {x: any}
        local t: A | B

        local r = t.x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*typeChecker.anyType, *requireType("r"));
}

TEST_CASE_FIXTURE(Fixture, "union_equality_comparisons")
{
    CheckResult result = check(R"(
        type A = number | string | nil
        type B = number | nil
        type C = number | boolean

        local a: A = 1
        local b: B = nil
        local c: C = true
        local n = 1

        local x = a == b
        local y = a == n
        local z = a == c
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "optional_union_members")
{
    CheckResult result = check(R"(
local a = { a = { x = 1, y = 2 }, b = 3 }
type A = typeof(a)
local b: A? = a
local bf = b
local c = bf.a.y
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(*typeChecker.numberType, *requireType("c"));
    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "optional_union_functions")
{
    CheckResult result = check(R"(
        local a = {}
        function a.foo(x:number, y:number) return x + y end
        type A = typeof(a)
        local b: A? = a
        local c = b.foo(1, 2)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(*typeChecker.numberType, *requireType("c"));
    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "optional_union_methods")
{
    CheckResult result = check(R"(
local a = {}
function a:foo(x:number, y:number) return x + y end
type A = typeof(a)
local b: A? = a
local c = b:foo(1, 2)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(*typeChecker.numberType, *requireType("c"));
    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "optional_union_follow")
{
    CheckResult result = check(R"(
local y: number? = 2
local x = y
local function f(a: number, b: typeof(x), c: typeof(x)) return -a end
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
local b: A? = { x = 2 }
local c = b.x
local d = b.y
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
local a: A? = {1, 2, 3}
local b = a[1]
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "optional_call_error")
{
    CheckResult result = check(R"(
type A = (number) -> number
local a: A? = function(a) return -a end
local b = a(4)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Value of type '((number) -> number)?' could be nil", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "optional_assignment_errors")
{
    CheckResult result = check(R"(
type A = { x: number }
local a: A? = { x = 2 }
a.x = 2
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[0]));

    result = check(R"(
type A = { x: number } & { y: number }
local a: A? = { x = 2, y = 3 }
a.x = 2
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::LuauLowerBoundsCalculation)
        CHECK_EQ("Value of type '{| x: number, y: number |}?' could be nil", toString(result.errors[0]));
    else
        CHECK_EQ("Value of type '({| x: number |} & {| y: number |})?' could be nil", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "optional_length_error")
{
    CheckResult result = check(R"(
type A = {number}
local a: A? = {1, 2, 3}
local b = #a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Value of type 'A?' could be nil", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "optional_missing_key_error_details")
{
    CheckResult result = check(R"(
type A = { x: number, y: number }
type B = { x: number, y: number }
type C = { x: number }
type D = { x: number }

local a: A|B|C|D
local b = a.y

local c: A|(B|C)?|D
local d = c.y

local e = a.z
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK_EQ("Key 'y' is missing from 'C', 'D' in the type 'A | B | C | D'", toString(result.errors[0]));

    CHECK_EQ("Value of type '(A | B | C | D)?' could be nil", toString(result.errors[1]));
    CHECK_EQ("Key 'y' is missing from 'C', 'D' in the type 'A | B | C | D'", toString(result.errors[2]));

    CHECK_EQ("Type 'A | B | C | D' does not have key 'z'", toString(result.errors[3]));
}

TEST_CASE_FIXTURE(Fixture, "unify_unsealed_table_union_check")
{
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

local a: XYZ
local b: { w: number } = a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), R"(Type 'X | Y | Z' could not be converted into '{| w: number |}'
caused by:
  Not all union options are compatible. Table type 'X' not compatible with type '{| w: number |}' because the former is missing field 'w')");
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
    CHECK_EQ(toString(result.errors[0]), R"(Type 'a' could not be converted into 'X | Y | Z'; none of the union options are compatible)");
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_optional")
{
    CheckResult result = check(R"(
type X = { x: number }

local a: X? = { w = 4 }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), R"(Type 'a' could not be converted into 'X?'
caused by:
  None of the union options are compatible. For example: Table type 'a' not compatible with type 'X' because the former is missing field 'x')");
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

TEST_CASE_FIXTURE(BuiltinsFixture, "table_union_write_indirect")
{
    CheckResult result = check(R"(
        type A = { x: number, y: (number) -> string } | { z: number, y: (number) -> string }

        local a:A = nil

        function a.y(x)
            return tostring(x * 2)
        end

        function a.y(x: string): number
            return tonumber(x) or 0
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    // NOTE: union normalization will improve this message
    if (FFlag::LuauLowerBoundsCalculation)
        CHECK_EQ(toString(result.errors[0]), "Type '(string) -> number' could not be converted into '(number) -> string'\n"
                                             "caused by:\n"
                                             "  Argument #1 type is not compatible. Type 'number' could not be converted into 'string'");
    else
        CHECK_EQ(toString(result.errors[0]),
            R"(Type '(string) -> number' could not be converted into '((number) -> string) | ((number) -> string)'; none of the union options are compatible)");
}


TEST_SUITE_END();
