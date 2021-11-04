// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Parser.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"

#include "Fixture.h"

#include "doctest.h"

LUAU_FASTFLAG(LuauEqConstraint)

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

TEST_CASE_FIXTURE(Fixture, "error_takes_optional_arguments")
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
    ScopedFastFlag luauMissingUnionPropertyError("LuauMissingUnionPropertyError", true);

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

    TypeId r = requireType("r");
    CHECK_MESSAGE(get<ErrorTypeVar>(r), "Expected error, got " << toString(r));
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

    if (FFlag::LuauEqConstraint)
    {
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK_EQ(*typeChecker.booleanType, *requireType("x"));
        CHECK_EQ(*typeChecker.booleanType, *requireType("y"));

        TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
        REQUIRE(tm);
        CHECK_EQ("(number | string)?", toString(*tm->wantedType));
        CHECK_EQ("boolean | number", toString(*tm->givenType));
    }
}

TEST_CASE_FIXTURE(Fixture, "optional_union_members")
{
    ScopedFastFlag luauExtraNilRecovery("LuauExtraNilRecovery", true);

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
    ScopedFastFlag luauExtraNilRecovery("LuauExtraNilRecovery", true);

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
    ScopedFastFlag luauExtraNilRecovery("LuauExtraNilRecovery", true);

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
}

TEST_CASE_FIXTURE(Fixture, "optional_field_access_error")
{
    ScopedFastFlag luauExtraNilRecovery("LuauExtraNilRecovery", true);

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
    ScopedFastFlag luauExtraNilRecovery("LuauExtraNilRecovery", true);

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
    ScopedFastFlag luauExtraNilRecovery("LuauExtraNilRecovery", true);

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
    ScopedFastFlag luauExtraNilRecovery("LuauExtraNilRecovery", true);

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
    CHECK_EQ("Value of type '({| x: number |} & {| y: number |})?' could be nil", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "optional_length_error")
{
    ScopedFastFlag luauExtraNilRecovery("LuauExtraNilRecovery", true);

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
    ScopedFastFlag luauExtraNilRecovery("LuauExtraNilRecovery", true);
    ScopedFastFlag luauMissingUnionPropertyError("LuauMissingUnionPropertyError", true);

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

TEST_CASE_FIXTURE(Fixture, "unify_sealed_table_union_check")
{
    ScopedFastFlag luauSealedTableUnifyOptionalFix("LuauSealedTableUnifyOptionalFix", true);

    CheckResult result = check(R"(
local x: { x: number } = { x = 3 }
type A = number?
type B = string?
local y: { x: number, y: A | B }
y = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = check(R"(
local x: { x: number } = { x = 3 }

local a: number? = 2
local y = {}
y.x = 2
y.y = a

y = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
