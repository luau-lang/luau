// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"
#include "Luau/VisitType.h"

#include "Fixture.h"
#include "ClassFixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

TEST_SUITE_BEGIN("TypeInferOperators");

TEST_CASE_FIXTURE(Fixture, "or_joins_types")
{
    CheckResult result = check(R"(
        local s = "a" or 10
        local x:string|number = s
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ(toString(*requireType("s")), "(string & ~(false?)) | number");
        CHECK_EQ(toString(*requireType("x")), "number | string");
    }
    else
    {
        CHECK_EQ(toString(*requireType("s")), "number | string");
        CHECK_EQ(toString(*requireType("x")), "number | string");
    }
}

TEST_CASE_FIXTURE(Fixture, "or_joins_types_with_no_extras")
{
    CheckResult result = check(R"(
        local s = "a" or 10
        local x:number|string = s
        local y = x or "s"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ(toString(*requireType("s")), "(string & ~(false?)) | number");
        CHECK_EQ(toString(*requireType("y")), "((number | string) & ~(false?)) | string");
    }
    else
    {
        CHECK_EQ(toString(*requireType("s")), "number | string");
        CHECK_EQ(toString(*requireType("y")), "number | string");
    }
}

TEST_CASE_FIXTURE(Fixture, "or_joins_types_with_no_superfluous_union")
{
    CheckResult result = check(R"(
        local s = "a" or "b"
        local x:string = s
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*requireType("s"), *typeChecker.stringType);
}

TEST_CASE_FIXTURE(Fixture, "and_does_not_always_add_boolean")
{
    ScopedFastFlag sff[]{
        {"LuauTryhardAnd", true},
    };

    CheckResult result = check(R"(
        local s = "a" and 10
        local x:boolean|number = s
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ(toString(*requireType("s")), "((false?) & string) | number");
    }
    else
    {
        CHECK_EQ(toString(*requireType("s")), "number");
    }
}

TEST_CASE_FIXTURE(Fixture, "and_adds_boolean_no_superfluous_union")
{
    CheckResult result = check(R"(
        local s = "a" and true
        local x:boolean = s
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(*requireType("x"), *typeChecker.booleanType);
}

TEST_CASE_FIXTURE(Fixture, "and_or_ternary")
{
    CheckResult result = check(R"(
        local s = (1/2) > 0.5 and "a" or 10
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ(toString(*requireType("s")), "((((false?) & boolean) | string) & ~(false?)) | number");
    }
    else
    {
        CHECK_EQ(toString(*requireType("s")), "number | string");
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "primitive_arith_no_metatable")
{
    CheckResult result = check(R"(
        function add(a: number, b: string)
            return a + (tonumber(b) :: number), tostring(a) .. b
        end
        local n, s = add(2,"3")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionType* functionType = get<FunctionType>(requireType("add"));

    std::optional<TypeId> retType = first(functionType->retTypes);
    REQUIRE(retType.has_value());
    CHECK_EQ(typeChecker.numberType, follow(*retType));
    CHECK_EQ(requireType("n"), typeChecker.numberType);
    CHECK_EQ(requireType("s"), typeChecker.stringType);
}

TEST_CASE_FIXTURE(Fixture, "primitive_arith_no_metatable_with_follows")
{
    CheckResult result = check(R"(
        local PI=3.1415926535897931
        local SOLAR_MASS=4*PI * PI
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(requireType("SOLAR_MASS"), typeChecker.numberType);
}

TEST_CASE_FIXTURE(Fixture, "primitive_arith_possible_metatable")
{
    CheckResult result = check(R"(
        function add(a: number, b: any)
            return a + b
        end
        local t = add(1,2)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("any", toString(requireType("t")));
}

TEST_CASE_FIXTURE(Fixture, "some_primitive_binary_ops")
{
    CheckResult result = check(R"(
        local a = 4 + 8
        local b = a + 9
        local s = 'hotdogs'
        local t = s .. s
        local c = b - a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number", toString(requireType("a")));
    CHECK_EQ("number", toString(requireType("b")));
    CHECK_EQ("string", toString(requireType("s")));
    CHECK_EQ("string", toString(requireType("t")));
    CHECK_EQ("number", toString(requireType("c")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typecheck_overloaded_multiply_that_is_an_intersection")
{
    CheckResult result = check(R"(
        --!strict
        local Vec3 = {}
        Vec3.__index = Vec3
        function Vec3.new()
            return setmetatable({x=0, y=0, z=0}, Vec3)
        end

        export type Vec3 = typeof(Vec3.new())

        local thefun: any = function(self, o) return self end

        local multiply: ((Vec3, Vec3) -> Vec3) & ((Vec3, number) -> Vec3) = thefun

        Vec3.__mul = multiply

        local a = Vec3.new()
        local b = Vec3.new()
        local c = a * b
        local d = a * 2
        local e = a * 'cabbage'
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Vec3", toString(requireType("a")));
    CHECK_EQ("Vec3", toString(requireType("b")));
    CHECK_EQ("Vec3", toString(requireType("c")));
    CHECK_EQ("Vec3", toString(requireType("d")));
    CHECK_EQ("Vec3", toString(requireType("e")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typecheck_overloaded_multiply_that_is_an_intersection_on_rhs")
{
    CheckResult result = check(R"(
        --!strict
        local Vec3 = {}
        Vec3.__index = Vec3
        function Vec3.new()
            return setmetatable({x=0, y=0, z=0}, Vec3)
        end

        export type Vec3 = typeof(Vec3.new())

        local thefun: any = function(self, o) return self end

        local multiply: ((Vec3, Vec3) -> Vec3) & ((Vec3, number) -> Vec3) = thefun

        Vec3.__mul = multiply

        local a = Vec3.new()
        local b = Vec3.new()
        local c = b * a
        local d = 2 * a
        local e = 'cabbage' * a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Vec3", toString(requireType("a")));
    CHECK_EQ("Vec3", toString(requireType("b")));
    CHECK_EQ("Vec3", toString(requireType("c")));
    CHECK_EQ("Vec3", toString(requireType("d")));
    CHECK_EQ("Vec3", toString(requireType("e")));
}

TEST_CASE_FIXTURE(Fixture, "compare_numbers")
{
    CheckResult result = check(R"(
        local a = 441
        local b = 0
        local c = a < b
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "compare_strings")
{
    CheckResult result = check(R"(
        local a = '441'
        local b = '0'
        local c = a < b
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "cannot_indirectly_compare_types_that_do_not_have_a_metatable")
{
    CheckResult result = check(R"(
        local a = {}
        local b = {}
        local c = a < b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    GenericError* gen = get<GenericError>(result.errors[0]);

    REQUIRE_EQ(gen->message, "Type a cannot be compared with < because it has no metatable");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cannot_indirectly_compare_types_that_do_not_offer_overloaded_ordering_operators")
{
    CheckResult result = check(R"(
        local M = {}
        function M.new()
            return setmetatable({}, M)
        end
        type M = typeof(M.new())

        local a = M.new()
        local b = M.new()
        local c = a < b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    GenericError* gen = get<GenericError>(result.errors[0]);
    REQUIRE(gen != nullptr);
    REQUIRE_EQ(gen->message, "Table M does not offer metamethod __lt");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cannot_compare_tables_that_do_not_have_the_same_metatable")
{
    CheckResult result = check(R"(
        --!strict
        local M = {}
        function M.new()
            return setmetatable({}, M)
        end
        function M.__lt(left, right) return true end

        local a = M.new()
        local b = {}
        local c = a < b -- line 10
        local d = b < a -- line 11
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    REQUIRE_EQ((Location{{10, 18}, {10, 23}}), result.errors[0].location);

    REQUIRE_EQ((Location{{11, 18}, {11, 23}}), result.errors[1].location);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "produce_the_correct_error_message_when_comparing_a_table_with_a_metatable_with_one_that_does_not")
{
    CheckResult result = check(R"(
        --!strict
        local M = {}
        function M.new()
            return setmetatable({}, M)
        end
        function M.__lt(left, right) return true end
        type M = typeof(M.new())

        local a = M.new()
        local b = {}
        local c = a < b -- line 10
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto err = get<GenericError>(result.errors[0]);
    REQUIRE(err != nullptr);

    // Frail. :|
    REQUIRE_EQ("Types M and b cannot be compared with < because they do not have the same metatable", err->message);
}

TEST_CASE_FIXTURE(Fixture, "in_nonstrict_mode_strip_nil_from_intersections_when_considering_relational_operators")
{
    CheckResult result = check(R"(
        --!nonstrict

        function maybe_a_number(): number?
            return 50
        end

        local a = maybe_a_number() < maybe_a_number()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "compound_assign_basic")
{
    CheckResult result = check(R"(
        local s = 10
        s += 20
    )");
    CHECK_EQ(0, result.errors.size());
    CHECK_EQ(toString(*requireType("s")), "number");
}

TEST_CASE_FIXTURE(Fixture, "compound_assign_mismatch_op")
{
    CheckResult result = check(R"(
        local s = 10
        s += true
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(result.errors[0], (TypeError{Location{{2, 13}, {2, 17}}, TypeMismatch{typeChecker.numberType, typeChecker.booleanType}}));
}

TEST_CASE_FIXTURE(Fixture, "compound_assign_mismatch_result")
{
    CheckResult result = check(R"(
        local s = 'hello'
        s += 10
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ(result.errors[0], (TypeError{Location{{2, 8}, {2, 9}}, TypeMismatch{typeChecker.numberType, typeChecker.stringType}}));
    CHECK_EQ(result.errors[1], (TypeError{Location{{2, 8}, {2, 15}}, TypeMismatch{typeChecker.stringType, typeChecker.numberType}}));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "compound_assign_metatable")
{
    CheckResult result = check(R"(
        --!strict
        type V2B = { x: number, y: number }
        local v2b: V2B = { x = 0, y = 0 }
        local VMT = {}

        VMT.__add = function(a: V2, b: V2): V2
            return setmetatable({ x = a.x + b.x, y = a.y + b.y }, VMT)
        end

        type V2 = typeof(setmetatable(v2b, VMT))

        local v1: V2 = setmetatable({ x = 1, y = 2 }, VMT)
        local v2: V2 = setmetatable({ x = 3, y = 4 }, VMT)
        v1 += v2
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "compound_assign_result_must_be_compatible_with_var")
{
    CheckResult result = check(R"(
        function __add(left, right)
            return 123
        end

        local mt = {
            __add = __add,
        }

        local x = setmetatable({}, mt)
        local v: number

        v += x -- okay: number + x -> number
        x += v -- not okay: x </: number
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(result.errors[0] == TypeError{Location{{13, 8}, {13, 14}}, TypeMismatch{requireType("x"), builtinTypes->numberType}});
}

TEST_CASE_FIXTURE(BuiltinsFixture, "compound_assign_mismatch_metatable")
{
    CheckResult result = check(R"(
        --!strict
        type V2B = { x: number, y: number }
        local v2b: V2B = { x = 0, y = 0 }
        local VMT = {}
        type V2 = typeof(setmetatable(v2b, VMT))

        function VMT.__mod(a: V2, b: V2): number
            return a.x * b.x + a.y * b.y
        end

        local v1: V2 = setmetatable({ x = 1, y = 2 }, VMT)
        local v2: V2 = setmetatable({ x = 3, y = 4 }, VMT)
        v1 %= v2
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK("Type 'number' could not be converted into 'V2'" == toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "CallOrOfFunctions")
{
    CheckResult result = check(R"(
function f() return 1; end
function g() return 2; end
(f or g)()
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "CallAndOrOfFunctions")
{
    CheckResult result = check(R"(
function f() return 1; end
function g() return 2; end
local x = false
(x and f or g)()
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typecheck_unary_minus")
{
    CheckResult result = check(R"(
        --!strict
        local foo
        local mt = {}

        mt.__unm = function(val: typeof(foo)): string
            return tostring(val.value) .. "test"
        end

        foo = setmetatable({
            value = 10
        }, mt)

        local a = -foo

        local b = 1+-1

        local bar = {
            value = 10
        }
        local c = -bar -- disallowed
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("string", toString(requireType("a")));
    CHECK_EQ("number", toString(requireType("b")));

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK(toString(result.errors[0]) == "Type '{ value: number }' could not be converted into 'number'");
    }
    else
    {
        GenericError* gen = get<GenericError>(result.errors[0]);
        REQUIRE(gen);
        REQUIRE_EQ(gen->message, "Unary operator '-' not supported by type 'bar'");
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typecheck_unary_minus_error")
{
    CheckResult result = check(R"(
        --!strict
        local mt = {}

        mt.__unm = function(val: boolean): string
            return "test"
        end

        local foo = setmetatable({
            value = 10
        }, mt)

        local a = -foo
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("string", toString(requireType("a")));

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE_EQ(*tm->wantedType, *typeChecker.booleanType);
    // given type is the typeof(foo) which is complex to compare against
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typecheck_unary_len_error")
{
    CheckResult result = check(R"(
        --!strict
        local mt = {}

        mt.__len = function(val): string
            return "test"
        end

        local foo = setmetatable({
            value = 10,
        }, mt)

        local a = #foo
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("number", toString(requireType("a")));

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE_EQ(*tm->wantedType, *typeChecker.numberType);
    REQUIRE_EQ(*tm->givenType, *typeChecker.stringType);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "unary_not_is_boolean")
{
    CheckResult result = check(R"(
        local b = not "string"
        local c = not (math.random() > 0.5 and "string" or 7)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    REQUIRE_EQ("boolean", toString(requireType("b")));
    REQUIRE_EQ("boolean", toString(requireType("c")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "disallow_string_and_types_without_metatables_from_arithmetic_binary_ops")
{
    CheckResult result = check(R"(
        --!strict
        local a = "1.24" + 123 -- not allowed

        local foo = {
            value = 10
        }

        local b = foo + 1 -- not allowed

        local bar = {
            value = 1
        }

        local mt = {}

        setmetatable(bar, mt)

        mt.__add = function(a: typeof(bar), b: number): number
            return a.value + b
        end

        local c = bar + 1 -- allowed

        local d = bar + foo -- not allowed
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(*tm->wantedType, *typeChecker.numberType);
    CHECK_EQ(*tm->givenType, *typeChecker.stringType);

    GenericError* gen1 = get<GenericError>(result.errors[1]);
    REQUIRE(gen1);
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ(gen1->message, "Operator + is not applicable for '{ value: number }' and 'number' because neither type has a metatable");
    else
        CHECK_EQ(gen1->message, "Binary operator '+' not supported by types 'foo' and 'number'");

    TypeMismatch* tm2 = get<TypeMismatch>(result.errors[2]);
    REQUIRE(tm2);
    CHECK_EQ(*tm2->wantedType, *typeChecker.numberType);
    CHECK_EQ(*tm2->givenType, *requireType("foo"));
}

// CLI-29033
TEST_CASE_FIXTURE(Fixture, "unknown_type_in_comparison")
{
    CheckResult result = check(R"(
        function merge(lower, greater)
            if lower.y == greater.y then
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "concat_op_on_free_lhs_and_string_rhs")
{
    CheckResult result = check(R"(
        local function f(x)
            return x .. "y"
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    REQUIRE(get<CannotInferBinaryOperation>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "concat_op_on_string_lhs_and_free_rhs")
{
    CheckResult result = check(R"(
        local function f(x)
            return "foo" .. x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("(string) -> string", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "strict_binary_op_where_lhs_unknown")
{
    std::vector<std::string> ops = {"+", "-", "*", "/", "%", "^", ".."};

    std::string src = "function foo(a, b)\n";

    for (const auto& op : ops)
        src += "local _ = a " + op + " b\n";

    src += "end";

    CheckResult result = check(src);
    LUAU_REQUIRE_ERROR_COUNT(ops.size(), result);

    CHECK_EQ("Unknown type used in + operation; consider adding a type annotation to 'a'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "and_binexps_dont_unify")
{
    CheckResult result = check(R"(
    --!strict
    local t = {}
    while true and t[1] do
        print(t[1].test)
    end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "error_on_invalid_operand_types_to_relational_operators")
{
    CheckResult result = check(R"(
        local a: boolean = true
        local b: boolean = false
        local foo = a < b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    GenericError* ge = get<GenericError>(result.errors[0]);
    REQUIRE(ge);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("Types 'boolean' and 'boolean' cannot be compared with relational operator <", ge->message);
    else
        CHECK_EQ("Type 'boolean' cannot be compared with relational operator <", ge->message);
}

TEST_CASE_FIXTURE(Fixture, "error_on_invalid_operand_types_to_relational_operators2")
{
    CheckResult result = check(R"(
        local a: number | string = ""
        local b: number | string = 1
        local foo = a < b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    GenericError* ge = get<GenericError>(result.errors[0]);
    REQUIRE(ge);
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("Types 'number | string' and 'number | string' cannot be compared with relational operator <", ge->message);
    else
        CHECK_EQ("Type 'number | string' cannot be compared with relational operator <", ge->message);
}

TEST_CASE_FIXTURE(Fixture, "cli_38355_recursive_union")
{
    CheckResult result = check(R"(
        --!strict
        local _
        _ += _ and _ or _ and _ or _ and _
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type contains a self-recursive construct that cannot be resolved", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "UnknownGlobalCompoundAssign")
{
    // In non-strict mode, global definition is still allowed
    {
        CheckResult result = check(R"(
            --!nonstrict
            a = a + 1
            print(a)
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK_EQ(toString(result.errors[0]), "Unknown global 'a'");
    }

    // In strict mode we no longer generate two errors from lhs
    {
        CheckResult result = check(R"(
            --!strict
            a += 1
            print(a)
        )");

        LUAU_REQUIRE_ERRORS(result);
        CHECK_EQ(toString(result.errors[0]), "Unknown global 'a'");
    }

    // In non-strict mode, compound assignment is not a definition, it's a modification
    {
        CheckResult result = check(R"(
            --!nonstrict
            a += 1
            print(a)
        )");

        LUAU_REQUIRE_ERROR_COUNT(2, result);
        CHECK_EQ(toString(result.errors[0]), "Unknown global 'a'");
    }
}

TEST_CASE_FIXTURE(Fixture, "strip_nil_from_lhs_or_operator")
{
    CheckResult result = check(R"(
--!strict
local a: number? = nil
local b: number = a or 1
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "strip_nil_from_lhs_or_operator2")
{
    CheckResult result = check(R"(
--!nonstrict
local a: number? = nil
local b: number = a or 1
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "dont_strip_nil_from_rhs_or_operator")
{
    CheckResult result = check(R"(
--!strict
local a: number? = nil
local b: number = 1 or a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(typeChecker.numberType, tm->wantedType);
    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("((number & ~(false?)) | number)?", toString(tm->givenType));
    }
    else
    {
        CHECK_EQ("number?", toString(tm->givenType));
    }
}

TEST_CASE_FIXTURE(Fixture, "operator_eq_verifies_types_do_intersect")
{
    CheckResult result = check(R"(
        type Array<T> = { [number]: T }
        type Fiber = { id: number }
        type null = {}

        local fiberStack: Array<Fiber | null> = {}
        local index = 0

        local function f(fiber: Fiber)
            local a = fiber ~= fiberStack[index]
            local b = fiberStack[index] ~= fiber
        end

        return f
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "operator_eq_operands_are_not_subtypes_of_each_other_but_has_overlap")
{
    CheckResult result = check(R"(
        local function f(a: string | number, b: boolean | number)
            return a == b
        end
    )");

    // This doesn't produce any errors but for the wrong reasons.
    // This unit test serves as a reminder to not try and unify the operands on `==`/`~=`.
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "operator_eq_completely_incompatible")
{
    ScopedFastFlag sff{"LuauIntersectionTestForEquality", true};

    CheckResult result = check(R"(
        local a: string | number = "hi"
        local b: {x: string}? = {x = "bye"}

        local r1 = a == b
        local r2 = b == a
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(Fixture, "refine_and_or")
{
    CheckResult result = check(R"(
        local t: {x: number?}? = {x = nil}
        local u = t and t.x or 5
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("((((false?) & ({| x: number? |}?)) | a) & ~(false?)) | number", toString(requireType("u")));
    }
    else
    {
        CHECK_EQ("number", toString(requireType("u")));
    }
}

TEST_CASE_FIXTURE(Fixture, "infer_any_in_all_modes_when_lhs_is_unknown")
{
    CheckResult result = check(Mode::Strict, R"(
        local function f(x, y)
            return x + y
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Unknown type used in + operation; consider adding a type annotation to 'x'");

    result = check(Mode::Nonstrict, R"(
        local function f(x, y)
            return x + y
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // When type inference is unified, we could add an assertion that
    // the strict and nonstrict types are equivalent. This isn't actually
    // the case right now, though.
}

TEST_CASE_FIXTURE(BuiltinsFixture, "equality_operations_succeed_if_any_union_branch_succeeds")
{
    CheckResult result = check(R"(
        local mm = {}
        type Foo = typeof(setmetatable({}, mm))
        local x: Foo
        local y: Foo?

        local v1 = x == y
        local v2 = y == x
        local v3 = x ~= y
        local v4 = y ~= x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CheckResult result2 = check(R"(
        local mm1 = {
            x = "foo",
        }

        local mm2 = {
            y = "bar",
        }

        type Foo = typeof(setmetatable({}, mm1))
        type Bar = typeof(setmetatable({}, mm2))

        local x1: Foo
        local x2: Foo?
        local y1: Bar
        local y2: Bar?

        local v1 = x1 == y1
        local v2 = x2 == y2
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result2);
    CHECK(toString(result2.errors[0]) == "Types Foo and Bar cannot be compared with == because they do not have the same metatable");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "expected_types_through_binary_and")
{
    ScopedFastFlag sff{"LuauBinaryNeedsExpectedTypesToo", true};

    CheckResult result = check(R"(
        local x: "a" | "b" | boolean = math.random() > 0.5 and "a"
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "expected_types_through_binary_or")
{
    ScopedFastFlag sff{"LuauBinaryNeedsExpectedTypesToo", true};

    CheckResult result = check(R"(
        local x: "a" | "b" | boolean = math.random() > 0.5 or "b"
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "unrelated_classes_cannot_be_compared")
{
    ScopedFastFlag sff{"LuauIntersectionTestForEquality", true};

    CheckResult result = check(R"(
        local a = BaseClass.New()
        local b = UnrelatedClass.New()

        local c = a == b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "unrelated_primitives_cannot_be_compared")
{
    ScopedFastFlag sff{"LuauIntersectionTestForEquality", true};

    CheckResult result = check(R"(
        local c = 5 == true
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "mm_ops_must_return_a_value")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local mm = {
            __add = function(self, other)
                return
            end,
        }

        local x = setmetatable({}, mm)
        local y = x + 123
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK(requireType("y") == builtinTypes->errorRecoveryType());

    const GenericError* ge = get<GenericError>(result.errors[1]);
    REQUIRE(ge);
    CHECK(ge->message == "Metamethod '__add' must return a value");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "mm_comparisons_must_return_a_boolean")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local mm1 = {
            __lt = function(self, other)
                return 123
            end,
        }

        local mm2 = {
            __lt = function(self, other)
                return
            end,
        }

        local o1 = setmetatable({}, mm1)
        local v1 = o1 < o1

        local o2 = setmetatable({}, mm2)
        local v2 = o2 < o2
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);

    CHECK(requireType("v1") == builtinTypes->booleanType);
    CHECK(requireType("v2") == builtinTypes->booleanType);

    CHECK(toString(result.errors[1]) == "Metamethod '__lt' must return a boolean");
    CHECK(toString(result.errors[3]) == "Metamethod '__lt' must return a boolean");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "reworked_and")
{
    ScopedFastFlag sff[]{
        {"LuauTryhardAnd", true},
    };

    CheckResult result = check(R"(
local a: number? = 5
local b: boolean = (a or 1) > 10
local c -- free

local x = a and 1
local y = 'a' and 1
local z = b and 1
local w = c and 1
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK("((false?) & (number?)) | number" == toString(requireType("x")));
        CHECK("((false?) & string) | number" == toString(requireType("y")));
        CHECK("((false?) & boolean) | number" == toString(requireType("z")));
        CHECK("((false?) & a) | number" == toString(requireType("w")));
    }
    else
    {
        CHECK("number?" == toString(requireType("x")));
        CHECK("number" == toString(requireType("y")));
        CHECK("boolean | number" == toString(requireType("z"))); // 'false' widened to boolean
        CHECK("(boolean | number)?" == toString(requireType("w")));
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "reworked_or")
{
    ScopedFastFlag sff[]{
        {"LuauTryhardAnd", true},
    };

    CheckResult result = check(R"(
local a: number | false = 5
local b: number? = 6
local c: boolean = true
local d: true = true
local e: false = false
local f: nil = false

local a1 = a or 'a'
local b1 = b or 4
local c1 = c or 'c'
local d1 = d or 'd'
local e1 = e or 'e'
local f1 = f or 'f'
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK("((false | number) & ~(false?)) | string" == toString(requireType("a1")));
        CHECK("((number?) & ~(false?)) | number" == toString(requireType("b1")));
        CHECK("(boolean & ~(false?)) | string" == toString(requireType("c1")));
        CHECK("(true & ~(false?)) | string" == toString(requireType("d1")));
        CHECK("(false & ~(false?)) | string" == toString(requireType("e1")));
        CHECK("(nil & ~(false?)) | string" == toString(requireType("f1")));
    }
    else
    {
        CHECK("number | string" == toString(requireType("a1")));
        CHECK("number" == toString(requireType("b1")));
        CHECK("boolean | string" == toString(requireType("c1"))); // 'true' widened to boolean
        CHECK("boolean | string" == toString(requireType("d1"))); // 'true' widened to boolean
        CHECK("string" == toString(requireType("e1")));
        CHECK("string" == toString(requireType("f1")));
    }
}

TEST_SUITE_END();
