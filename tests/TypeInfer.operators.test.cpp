// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("TypeInferOperators");

TEST_CASE_FIXTURE(Fixture, "or_joins_types")
{
    CheckResult result = check(R"(
        local s = "a" or 10
        local x:string|number = s
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(*requireType("s")), "number | string");
    CHECK_EQ(toString(*requireType("x")), "number | string");
}

TEST_CASE_FIXTURE(Fixture, "or_joins_types_with_no_extras")
{
    CheckResult result = check(R"(
        local s = "a" or 10
        local x:number|string = s
        local y = x or "s"
    )");
    CHECK_EQ(0, result.errors.size());
    CHECK_EQ(toString(*requireType("s")), "number | string");
    CHECK_EQ(toString(*requireType("y")), "number | string");
}

TEST_CASE_FIXTURE(Fixture, "or_joins_types_with_no_superfluous_union")
{
    CheckResult result = check(R"(
        local s = "a" or "b"
        local x:string = s
    )");
    CHECK_EQ(0, result.errors.size());
    CHECK_EQ(*requireType("s"), *typeChecker.stringType);
}

TEST_CASE_FIXTURE(Fixture, "and_adds_boolean")
{
    CheckResult result = check(R"(
        local s = "a" and 10
        local x:boolean|number = s
    )");
    CHECK_EQ(0, result.errors.size());
    CHECK_EQ(toString(*requireType("s")), "boolean | number");
}

TEST_CASE_FIXTURE(Fixture, "and_adds_boolean_no_superfluous_union")
{
    CheckResult result = check(R"(
        local s = "a" and true
        local x:boolean = s
    )");
    CHECK_EQ(0, result.errors.size());
    CHECK_EQ(*requireType("x"), *typeChecker.booleanType);
}

TEST_CASE_FIXTURE(Fixture, "and_or_ternary")
{
    CheckResult result = check(R"(
        local s = (1/2) > 0.5 and "a" or 10
    )");
    CHECK_EQ(0, result.errors.size());
    CHECK_EQ(toString(*requireType("s")), "number | string");
}

TEST_CASE_FIXTURE(Fixture, "primitive_arith_no_metatable")
{
    CheckResult result = check(R"(
        function add(a: number, b: string)
            return a + (tonumber(b) :: number), a .. b
        end
        local n, s = add(2,"3")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionTypeVar* functionType = get<FunctionTypeVar>(requireType("add"));

    std::optional<TypeId> retType = first(functionType->retType);
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

TEST_CASE_FIXTURE(Fixture, "typecheck_overloaded_multiply_that_is_an_intersection")
{
    ScopedFastFlag sff{"LuauErrorRecoveryType", true};

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

TEST_CASE_FIXTURE(Fixture, "typecheck_overloaded_multiply_that_is_an_intersection_on_rhs")
{
    ScopedFastFlag sff{"LuauErrorRecoveryType", true};

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

TEST_CASE_FIXTURE(Fixture, "cannot_indirectly_compare_types_that_do_not_offer_overloaded_ordering_operators")
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

TEST_CASE_FIXTURE(Fixture, "cannot_compare_tables_that_do_not_have_the_same_metatable")
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

TEST_CASE_FIXTURE(Fixture, "produce_the_correct_error_message_when_comparing_a_table_with_a_metatable_with_one_that_does_not")
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

TEST_CASE_FIXTURE(Fixture, "compound_assign_metatable")
{
    CheckResult result = check(R"(
        --!strict
        type V2B = { x: number, y: number }
        local v2b: V2B = { x = 0, y = 0 }
        local VMT = {}
        type V2 = typeof(setmetatable(v2b, VMT))

        function VMT.__add(a: V2, b: V2): V2
            return setmetatable({ x = a.x + b.x, y = a.y + b.y }, VMT)
        end

        local v1: V2 = setmetatable({ x = 1, y = 2 }, VMT)
        local v2: V2 = setmetatable({ x = 3, y = 4 }, VMT)
        v1 += v2
    )");
    CHECK_EQ(0, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "compound_assign_mismatch_metatable")
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

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    CHECK_EQ(*tm->wantedType, *requireType("v2"));
    CHECK_EQ(*tm->givenType, *typeChecker.numberType);
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

TEST_CASE_FIXTURE(Fixture, "typecheck_unary_minus")
{
    CheckResult result = check(R"(
        --!strict
        local foo = {
            value = 10
        }
        local mt = {}
        setmetatable(foo, mt)

        mt.__unm = function(val: typeof(foo)): string
            return val.value .. "test"
        end

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

    GenericError* gen = get<GenericError>(result.errors[0]);
    REQUIRE_EQ(gen->message, "Unary operator '-' not supported by type 'bar'");
}

TEST_CASE_FIXTURE(Fixture, "unary_not_is_boolean")
{
    CheckResult result = check(R"(
        local b = not "string"
        local c = not (math.random() > 0.5 and "string" or 7)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    REQUIRE_EQ("boolean", toString(requireType("b")));
    REQUIRE_EQ("boolean", toString(requireType("c")));
}

TEST_CASE_FIXTURE(Fixture, "disallow_string_and_types_without_metatables_from_arithmetic_binary_ops")
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
    REQUIRE_EQ(*tm->wantedType, *typeChecker.numberType);
    REQUIRE_EQ(*tm->givenType, *typeChecker.stringType);

    TypeMismatch* tm2 = get<TypeMismatch>(result.errors[2]);
    CHECK_EQ(*tm2->wantedType, *typeChecker.numberType);
    CHECK_EQ(*tm2->givenType, *requireType("foo"));

    GenericError* gen2 = get<GenericError>(result.errors[1]);
    REQUIRE_EQ(gen2->message, "Binary operator '+' not supported by types 'foo' and 'number'");
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

    std::string src = R"(
        function foo(a, b)
    )";

    for (const auto& op : ops)
        src += "local _ = a " + op + "b\n";

    src += "end";

    CheckResult result = check(src);
    LUAU_REQUIRE_ERROR_COUNT(ops.size(), result);

    CHECK_EQ("Unknown type used in + operation; consider adding a type annotation to 'a'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "and_binexps_dont_unify")
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

TEST_CASE_FIXTURE(Fixture, "UnknownGlobalCompoundAssign")
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
    CHECK_EQ("number?", toString(tm->givenType));
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
    ScopedFastFlag sff1{"LuauEqConstraint", true};

    CheckResult result = check(R"(
        local function f(a: string | number, b: boolean | number)
            return a == b
        end
    )");

    // This doesn't produce any errors but for the wrong reasons.
    // This unit test serves as a reminder to not try and unify the operands on `==`/`~=`.
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "refine_and_or")
{
    CheckResult result = check(R"(
        local t: {x: number?}? = {x = nil}
        local u = t and t.x or 5
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number", toString(requireType("u")));
}

TEST_CASE_FIXTURE(Fixture, "infer_any_in_all_modes_when_lhs_is_unknown")
{
    ScopedFastFlag sff{"LuauDecoupleOperatorInferenceFromUnifiedTypeInference", true};

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

TEST_SUITE_END();
