// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Normalize.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"

#include "Fixture.h"

#include "doctest.h"

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

using namespace Luau;

namespace
{
std::optional<WithPredicate<TypePackId>> magicFunctionInstanceIsA(
    TypeChecker& typeChecker, const ScopePtr& scope, const AstExprCall& expr, WithPredicate<TypePackId> withPredicate)
{
    if (expr.args.size != 1)
        return std::nullopt;

    auto index = expr.func->as<Luau::AstExprIndexName>();
    auto str = expr.args.data[0]->as<Luau::AstExprConstantString>();
    if (!index || !str)
        return std::nullopt;

    std::optional<LValue> lvalue = tryGetLValue(*index->expr);
    std::optional<TypeFun> tfun = scope->lookupType(std::string(str->value.data, str->value.size));
    if (!lvalue || !tfun)
        return std::nullopt;

    ModulePtr module = typeChecker.currentModule;
    TypePackId booleanPack = module->internalTypes.addTypePack({typeChecker.booleanType});
    return WithPredicate<TypePackId>{booleanPack, {IsAPredicate{std::move(*lvalue), expr.location, tfun->type}}};
}

void dcrMagicRefinementInstanceIsA(const MagicRefinementContext& ctx)
{
    if (ctx.callSite->args.size != 1 || ctx.discriminantTypes.empty())
        return;

    auto index = ctx.callSite->func->as<Luau::AstExprIndexName>();
    auto str = ctx.callSite->args.data[0]->as<Luau::AstExprConstantString>();
    if (!index || !str)
        return;

    std::optional<TypeId> discriminantTy = ctx.discriminantTypes[0];
    if (!discriminantTy)
        return;

    std::optional<TypeFun> tfun = ctx.scope->lookupType(std::string(str->value.data, str->value.size));
    if (!tfun)
        return;

    LUAU_ASSERT(get<BlockedType>(*discriminantTy));
    asMutable(*discriminantTy)->ty.emplace<BoundType>(tfun->type);
}

struct RefinementClassFixture : BuiltinsFixture
{
    RefinementClassFixture()
    {
        TypeArena& arena = frontend.globals.globalTypes;
        NotNull<Scope> scope{frontend.globals.globalScope.get()};

        std::optional<TypeId> rootSuper = std::make_optional(builtinTypes->classType);

        unfreeze(arena);
        TypeId vec3 = arena.addType(ClassType{"Vector3", {}, rootSuper, std::nullopt, {}, nullptr, "Test"});
        getMutable<ClassType>(vec3)->props = {
            {"X", Property{builtinTypes->numberType}},
            {"Y", Property{builtinTypes->numberType}},
            {"Z", Property{builtinTypes->numberType}},
        };

        TypeId inst = arena.addType(ClassType{"Instance", {}, rootSuper, std::nullopt, {}, nullptr, "Test"});

        TypePackId isAParams = arena.addTypePack({inst, builtinTypes->stringType});
        TypePackId isARets = arena.addTypePack({builtinTypes->booleanType});
        TypeId isA = arena.addType(FunctionType{isAParams, isARets});
        getMutable<FunctionType>(isA)->magicFunction = magicFunctionInstanceIsA;
        getMutable<FunctionType>(isA)->dcrMagicRefinement = dcrMagicRefinementInstanceIsA;

        getMutable<ClassType>(inst)->props = {
            {"Name", Property{builtinTypes->stringType}},
            {"IsA", Property{isA}},
        };

        TypeId folder = frontend.globals.globalTypes.addType(ClassType{"Folder", {}, inst, std::nullopt, {}, nullptr, "Test"});
        TypeId part = frontend.globals.globalTypes.addType(ClassType{"Part", {}, inst, std::nullopt, {}, nullptr, "Test"});
        getMutable<ClassType>(part)->props = {
            {"Position", Property{vec3}},
        };

        frontend.globals.globalScope->exportedTypeBindings["Vector3"] = TypeFun{{}, vec3};
        frontend.globals.globalScope->exportedTypeBindings["Instance"] = TypeFun{{}, inst};
        frontend.globals.globalScope->exportedTypeBindings["Folder"] = TypeFun{{}, folder};
        frontend.globals.globalScope->exportedTypeBindings["Part"] = TypeFun{{}, part};

        for (const auto& [name, ty] : frontend.globals.globalScope->exportedTypeBindings)
            persist(ty.type);

        freeze(frontend.globals.globalTypes);
    }
};
} // namespace

TEST_SUITE_BEGIN("RefinementTest");

TEST_CASE_FIXTURE(Fixture, "is_truthy_constraint")
{
    CheckResult result = check(R"(
        function f(v: string?)
            if v then
                local s = v
            else
                local s = v
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string", toString(requireTypeAtPosition({3, 26})));
    CHECK_EQ("nil", toString(requireTypeAtPosition({5, 26})));
}

TEST_CASE_FIXTURE(Fixture, "invert_is_truthy_constraint")
{
    CheckResult result = check(R"(
        function f(v: string?)
            if not v then
                local s = v
            else
                local s = v
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("nil", toString(requireTypeAtPosition({3, 26})));
    CHECK_EQ("string", toString(requireTypeAtPosition({5, 26})));
}

TEST_CASE_FIXTURE(Fixture, "parenthesized_expressions_are_followed_through")
{
    CheckResult result = check(R"(
        function f(v: string?)
            if (not v) then
                local s = v
            else
                local s = v
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("nil", toString(requireTypeAtPosition({3, 26})));
    CHECK_EQ("string", toString(requireTypeAtPosition({5, 26})));
}

TEST_CASE_FIXTURE(Fixture, "and_constraint")
{
    CheckResult result = check(R"(
        function f(a: string?, b: number?)
            if a and b then
                local x = a
                local y = b
            else
                local x = a
                local y = b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string", toString(requireTypeAtPosition({3, 26})));
    CHECK_EQ("number", toString(requireTypeAtPosition({4, 26})));

    CHECK_EQ("string?", toString(requireTypeAtPosition({6, 26})));
    CHECK_EQ("number?", toString(requireTypeAtPosition({7, 26})));
}

TEST_CASE_FIXTURE(Fixture, "not_and_constraint")
{
    CheckResult result = check(R"(
        function f(a: string?, b: number?)
            if not (a and b) then
                local x = a
                local y = b
            else
                local x = a
                local y = b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string?", toString(requireTypeAtPosition({3, 26})));
    CHECK_EQ("number?", toString(requireTypeAtPosition({4, 26})));

    CHECK_EQ("string", toString(requireTypeAtPosition({6, 26})));
    CHECK_EQ("number", toString(requireTypeAtPosition({7, 26})));
}

TEST_CASE_FIXTURE(Fixture, "or_predicate_with_truthy_predicates")
{
    CheckResult result = check(R"(
        function f(a: string?, b: number?)
            if a or b then
                local x = a
                local y = b
            else
                local x = a
                local y = b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string?", toString(requireTypeAtPosition({3, 26})));
    CHECK_EQ("number?", toString(requireTypeAtPosition({4, 26})));

    CHECK_EQ("nil", toString(requireTypeAtPosition({6, 26})));
    CHECK_EQ("nil", toString(requireTypeAtPosition({7, 26})));
}

TEST_CASE_FIXTURE(Fixture, "a_and_b_or_a_and_c")
{
    CheckResult result = check(R"(
        function f(a: string?, b: number?, c: boolean)
            if (a and b) or (a and c) then
                local foo = a
                local bar = b
                local baz = c
            else
                local foo = a
                local bar = b
                local baz = c
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);


    CHECK_EQ("string", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("number?", toString(requireTypeAtPosition({4, 28})));
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("boolean", toString(requireTypeAtPosition({5, 28})));
    else
        CHECK_EQ("true", toString(requireTypeAtPosition({5, 28}))); // oh no! :(

    CHECK_EQ("string?", toString(requireTypeAtPosition({7, 28})));
    CHECK_EQ("number?", toString(requireTypeAtPosition({8, 28})));
    CHECK_EQ("boolean", toString(requireTypeAtPosition({9, 28})));
}

TEST_CASE_FIXTURE(Fixture, "type_assertion_expr_carry_its_constraints")
{
    CheckResult result = check(R"(
        function g(a: number?, b: string?)
            if (a :: any) and (b :: any) then
                local x = a
                local y = b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("number?", toString(requireTypeAtPosition({3, 26})));
        CHECK_EQ("string?", toString(requireTypeAtPosition({4, 26})));
    }
    else
    {
        // We're going to drop support for type refinements through type assertions.
        CHECK_EQ("number", toString(requireTypeAtPosition({3, 26})));
        CHECK_EQ("string", toString(requireTypeAtPosition({4, 26})));
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typeguard_in_if_condition_position")
{
    CheckResult result = check(R"(
        function f(s: any)
            if type(s) == "number" then
                local n = s
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number", toString(requireTypeAtPosition({3, 26})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typeguard_in_assert_position")
{
    CheckResult result = check(R"(
        local a
        assert(type(a) == "number")
        local b = a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    REQUIRE_EQ("number", toString(requireType("b")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "call_an_incompatible_function_after_using_typeguard")
{
    CheckResult result = check(R"(
        local function f(x: number)
            return x
        end

        local function g(x: any)
            if type(x) == "string" then
                f(x)
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Type 'string' could not be converted into 'number'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "impossible_type_narrow_is_not_an_error")
{
    // This unit test serves as a reminder to not implement this warning until Luau is intelligent enough.
    // For instance, getting a value out of the indexer and checking whether the value exists is not an error.
    CheckResult result = check(R"(
        local t: {string} = {"a", "b", "c"}
        local v = t[4]
        if not v then
            t[4] = "d"
        else
            print(v)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "truthy_constraint_on_properties")
{
    CheckResult result = check(R"(
        local t: {x: number?} = {x = 1}

        if t.x then
            local t2 = t
            local foo = t.x
        end

        local bar = t.x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK("{| x: number |}" == toString(requireTypeAtPosition({4, 23})));
        CHECK("number" == toString(requireTypeAtPosition({5, 26})));
    }

    CHECK_EQ("number?", toString(requireType("bar")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_on_a_refined_property")
{
    CheckResult result = check(R"(
        local t: {x: {y: string}?} = {x = {y = "hello!"}}

        if t.x then
            print(t.x.y)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "assert_non_binary_expressions_actually_resolve_constraints")
{
    CheckResult result = check(R"(
        local foo: string? = "hello"
        assert(foo)
        local bar: string = foo
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "lvalue_is_equal_to_another_lvalue")
{
    CheckResult result = check(R"(
        local function f(a: (string | number)?, b: boolean?)
            if a == b then
                local foo, bar = a, b
            else
                local foo, bar = a, b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireTypeAtPosition({3, 33})), "(number | string)?"); // a == b
    CHECK_EQ(toString(requireTypeAtPosition({3, 36})), "boolean?");           // a == b

    CHECK_EQ(toString(requireTypeAtPosition({5, 33})), "(number | string)?"); // a ~= b
    CHECK_EQ(toString(requireTypeAtPosition({5, 36})), "boolean?");           // a ~= b
}

TEST_CASE_FIXTURE(Fixture, "lvalue_is_equal_to_a_term")
{
    CheckResult result = check(R"(
        local function f(a: (string | number)?)
            if a == 1 then
                local foo = a
            else
                local foo = a
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireTypeAtPosition({3, 28})), "(number | string)?"); // a == 1;
    CHECK_EQ(toString(requireTypeAtPosition({5, 28})), "(number | string)?"); // a ~= 1
}

TEST_CASE_FIXTURE(Fixture, "term_is_equal_to_an_lvalue")
{
    CheckResult result = check(R"(
        local function f(a: (string | number)?)
            if "hello" == a then
                local foo = a
            else
                local foo = a
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ(toString(requireTypeAtPosition({3, 28})), R"("hello")");                         // a == "hello"
        CHECK_EQ(toString(requireTypeAtPosition({5, 28})), R"(((string & ~"hello") | number)?)"); // a ~= "hello"
    }
    else
    {
        CHECK_EQ(toString(requireTypeAtPosition({3, 28})), R"("hello")");            // a == "hello"
        CHECK_EQ(toString(requireTypeAtPosition({5, 28})), R"((number | string)?)"); // a ~= "hello"
    }
}

TEST_CASE_FIXTURE(Fixture, "lvalue_is_not_nil")
{
    CheckResult result = check(R"(
        local function f(a: (string | number)?)
            if a ~= nil then
                local foo = a
            else
                local foo = a
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireTypeAtPosition({3, 28})), "number | string");    // a ~= nil
    CHECK_EQ(toString(requireTypeAtPosition({5, 28})), "(number | string)?"); // a == nil
}

TEST_CASE_FIXTURE(Fixture, "free_type_is_equal_to_an_lvalue")
{
    CheckResult result = check(R"(
        local function f(a, b: string?)
            if a == b then
                local foo, bar = a, b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireTypeAtPosition({3, 33})), "a");       // a == b
    CHECK_EQ(toString(requireTypeAtPosition({3, 36})), "string?"); // a == b
}

TEST_CASE_FIXTURE(Fixture, "unknown_lvalue_is_not_synonymous_with_other_on_not_equal")
{
    CheckResult result = check(R"(
        local function f(a: any, b: {x: number}?)
            if a ~= b then
                local foo, bar = a, b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireTypeAtPosition({3, 33})), "any");              // a ~= b
    CHECK_EQ(toString(requireTypeAtPosition({3, 36})), "{| x: number |}?"); // a ~= b
}

TEST_CASE_FIXTURE(Fixture, "string_not_equal_to_string_or_nil")
{
    CheckResult result = check(R"(
        local t: {string} = {"hello"}

        local a: string = t[1]
        local b: string? = nil
        if a ~= b then
            local foo, bar = a, b
        else
            local foo, bar = a, b
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireTypeAtPosition({6, 29})), "string");  // a ~= b
    CHECK_EQ(toString(requireTypeAtPosition({6, 32})), "string?"); // a ~= b

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ(toString(requireTypeAtPosition({8, 29})), "string?"); // a == b
        CHECK_EQ(toString(requireTypeAtPosition({8, 32})), "string?"); // a == b
    }
    else
    {
        CHECK_EQ(toString(requireTypeAtPosition({8, 29})), "string");  // a == b
        CHECK_EQ(toString(requireTypeAtPosition({8, 32})), "string?"); // a == b
    }
}

TEST_CASE_FIXTURE(Fixture, "narrow_property_of_a_bounded_variable")
{
    CheckResult result = check(R"(
        local t
        local u: {x: number?} = {x = nil}
        t = u

        if t.x then
            local foo: number = t.x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_narrow_to_vector")
{
    CheckResult result = check(R"(
        local function f(x)
            if type(x) == "vector" then
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("*error-type*", toString(requireTypeAtPosition({3, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "nonoptional_type_can_narrow_to_nil_if_sense_is_true")
{
    CheckResult result = check(R"(
        local t = {"hello"}
        local v = t[2]
        if type(v) == "nil" then
            local foo = v
        else
            local foo = v
        end

        if not (type(v) ~= "nil") then
            local foo = v
        else
            local foo = v
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("nil", toString(requireTypeAtPosition({4, 24})));    // type(v) == "nil"
    CHECK_EQ("string", toString(requireTypeAtPosition({6, 24}))); // type(v) ~= "nil"

    CHECK_EQ("nil", toString(requireTypeAtPosition({10, 24})));    // equivalent to type(v) == "nil"
    CHECK_EQ("string", toString(requireTypeAtPosition({12, 24}))); // equivalent to type(v) ~= "nil"
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typeguard_not_to_be_string")
{
    CheckResult result = check(R"(
        local function f(x: string | number | boolean)
            if type(x) ~= "string" then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("boolean | number", toString(requireTypeAtPosition({3, 28}))); // type(x) ~= "string"
    CHECK_EQ("string", toString(requireTypeAtPosition({5, 28})));           // type(x) == "string"
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typeguard_narrows_for_table")
{
    CheckResult result = check(R"(
        local function f(x: string | {x: number} | {y: boolean})
            if type(x) == "table" then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("{| x: number |} | {| y: boolean |}", toString(requireTypeAtPosition({3, 28}))); // type(x) == "table"
    CHECK_EQ("string", toString(requireTypeAtPosition({5, 28})));                             // type(x) ~= "table"
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typeguard_narrows_for_functions")
{
    CheckResult result = check(R"(
        local function weird(x: string | ((number) -> string))
            if type(x) == "function" then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("(number) -> string", toString(requireTypeAtPosition({3, 28}))); // type(x) == "function"
    CHECK_EQ("string", toString(requireTypeAtPosition({5, 28})));             // type(x) ~= "function"
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_guard_can_filter_for_intersection_of_tables")
{
    CheckResult result = check(R"(
        type XYCoord = {x: number} & {y: number}
        local function f(t: XYCoord?)
            if type(t) == "table" then
                local foo = t
            else
                local foo = t
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions opts;
    opts.exhaustive = true;
    CHECK_EQ("{| x: number |} & {| y: number |}", toString(requireTypeAtPosition({4, 28}), opts));
    CHECK_EQ("nil", toString(requireTypeAtPosition({6, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_guard_can_filter_for_overloaded_function")
{
    CheckResult result = check(R"(
        type SomeOverloadedFunction = ((number) -> string) & ((string) -> number)
        local function f(g: SomeOverloadedFunction?)
            if type(g) == "function" then
                local foo = g
            else
                local foo = g
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("((number) -> string) & ((string) -> number)", toString(requireTypeAtPosition({4, 28})));
    CHECK_EQ("nil", toString(requireTypeAtPosition({6, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_guard_narrowed_into_nothingness")
{
    CheckResult result = check(R"(
        local function f(t: {x: number})
            if type(t) ~= "table" then
                local foo = t
                error(("Expected a table, got %s"):format(type(t)))
            end

            return t.x + 1
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("never", toString(requireTypeAtPosition({3, 28})));
}

TEST_CASE_FIXTURE(Fixture, "not_a_or_not_b")
{
    CheckResult result = check(R"(
        local function f(a: number?, b: number?)
            if (not a) or (not b) then
                local foo = a
                local bar = b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number?", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("number?", toString(requireTypeAtPosition({4, 28})));
}

TEST_CASE_FIXTURE(Fixture, "not_a_or_not_b2")
{
    CheckResult result = check(R"(
        local function f(a: number?, b: number?)
            if not (a and b) then
                local foo = a
                local bar = b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number?", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("number?", toString(requireTypeAtPosition({4, 28})));
}

TEST_CASE_FIXTURE(Fixture, "not_a_and_not_b")
{
    CheckResult result = check(R"(
        local function f(a: number?, b: number?)
            if (not a) and (not b) then
                local foo = a
                local bar = b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("nil", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("nil", toString(requireTypeAtPosition({4, 28})));
}

TEST_CASE_FIXTURE(Fixture, "not_a_and_not_b2")
{
    CheckResult result = check(R"(
        local function f(a: number?, b: number?)
            if not (a or b) then
                local foo = a
                local bar = b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("nil", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("nil", toString(requireTypeAtPosition({4, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "either_number_or_string")
{
    CheckResult result = check(R"(
        local function f(x: any)
            if type(x) == "number" or type(x) == "string" then
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number | string", toString(requireTypeAtPosition({3, 28})));
}

TEST_CASE_FIXTURE(Fixture, "not_t_or_some_prop_of_t")
{
    CheckResult result = check(R"(
        local function f(t: {x: boolean}?)
            if not t or t.x then
                local foo = t
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("{| x: true |}?", toString(requireTypeAtPosition({3, 28})));
    else
        CHECK_EQ("{| x: boolean |}?", toString(requireTypeAtPosition({3, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "assert_a_to_be_truthy_then_assert_a_to_be_number")
{
    CheckResult result = check(R"(
        local a: (number | string)?
        assert(a)
        local b = a
        assert(type(a) == "number")
        local c = a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number | string", toString(requireTypeAtPosition({3, 18})));
    CHECK_EQ("number", toString(requireTypeAtPosition({5, 18})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "merge_should_be_fully_agnostic_of_hashmap_ordering")
{
    // This bug came up because there was a mistake in Luau::merge where zipping on two maps would produce the wrong merged result.
    CheckResult result = check(R"(
        local function f(b: string | { x: string }, a)
            assert(type(a) == "string")
            assert(type(b) == "string" or type(b) == "table")

            if type(b) == "string" then
                local foo = b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string", toString(requireTypeAtPosition({6, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "refine_the_correct_types_opposite_of_when_a_is_not_number_or_string")
{
    CheckResult result = check(R"(
        local function f(a: string | number | boolean)
            if type(a) ~= "number" and type(a) ~= "string" then
                local foo = a
            else
                local foo = a
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("boolean", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("number | string", toString(requireTypeAtPosition({5, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "is_truthy_constraint_ifelse_expression")
{
    CheckResult result = check(R"(
        function f(v:string?)
            return if v then v else tostring(v)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string", toString(requireTypeAtPosition({2, 29})));
    CHECK_EQ("nil", toString(requireTypeAtPosition({2, 45})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "invert_is_truthy_constraint_ifelse_expression")
{
    CheckResult result = check(R"(
        function f(v:string?)
            return if not v then tostring(v) else v
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("nil", toString(requireTypeAtPosition({2, 42})));
    CHECK_EQ("string", toString(requireTypeAtPosition({2, 50})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_comparison_ifelse_expression")
{
    CheckResult result = check(R"(
        function returnOne(x)
            return 1
        end

        function f(v:any)
            return if typeof(v) == "number" then v else returnOne(v)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number", toString(requireTypeAtPosition({6, 49})));
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("~number", toString(requireTypeAtPosition({6, 66})));
    else
        CHECK_EQ("any", toString(requireTypeAtPosition({6, 66})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "correctly_lookup_a_shadowed_local_that_which_was_previously_refined")
{
    CheckResult result = check(R"(
        local foo: string? = "hi"
        assert(foo)
        local foo: number = 5
        print(foo:sub(1, 1))
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Type 'number' does not have key 'sub'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "correctly_lookup_property_whose_base_was_previously_refined")
{
    CheckResult result = check(R"(
        type T = {x: string | number}
        local t: T? = {x = "hi"}
        if t then
            if type(t.x) == "string" then
                local foo = t.x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string", toString(requireTypeAtPosition({5, 30})));
}

TEST_CASE_FIXTURE(Fixture, "correctly_lookup_property_whose_base_was_previously_refined2")
{
    CheckResult result = check(R"(
        type T = { x: { y: number }? }

        local function f(t: T?)
            if t and t.x then
                local foo = t.x.y
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number", toString(requireTypeAtPosition({5, 32})));
}

TEST_CASE_FIXTURE(Fixture, "apply_refinements_on_astexprindexexpr_whose_subscript_expr_is_constant_string")
{
    CheckResult result = check(R"(
        type T = { [string]: { prop: number }? }
        local t: T = {}

        if t["hello"] then
            local foo = t["hello"].prop
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "discriminate_from_truthiness_of_x")
{
    CheckResult result = check(R"(
        type T = {tag: "missing", x: nil} | {tag: "exists", x: string}

        local function f(t: T)
            if t.x then
                local foo = t
            else
                local bar = t
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(R"({| tag: "exists", x: string |})", toString(requireTypeAtPosition({5, 28})));
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ(R"({| tag: "missing", x: nil |})", toString(requireTypeAtPosition({7, 28})));
    else
        CHECK_EQ(R"({| tag: "exists", x: string |} | {| tag: "missing", x: nil |})", toString(requireTypeAtPosition({7, 28})));
}

TEST_CASE_FIXTURE(Fixture, "discriminate_tag")
{
    CheckResult result = check(R"(
        type Cat = {tag: "Cat", name: string, catfood: string}
        type Dog = {tag: "Dog", name: string, dogfood: string}
        type Animal = Cat | Dog

        local function f(animal: Animal)
            if animal.tag == "Cat" then
                local cat = animal
            elseif animal.tag == "Dog" then
                local dog = animal
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ(R"({| catfood: string, name: string, tag: "Cat" |})", toString(requireTypeAtPosition({7, 33})));
        CHECK_EQ(R"({| dogfood: string, name: string, tag: "Dog" |})", toString(requireTypeAtPosition({9, 33})));
    }
    else
    {
        CHECK_EQ("Cat", toString(requireTypeAtPosition({7, 33})));
        CHECK_EQ("Dog", toString(requireTypeAtPosition({9, 33})));
    }
}

TEST_CASE_FIXTURE(Fixture, "discriminate_tag_with_implicit_else")
{
    CheckResult result = check(R"(
        type Cat = {tag: "Cat", name: string, catfood: string}
        type Dog = {tag: "Dog", name: string, dogfood: string}
        type Animal = Cat | Dog

        local function f(animal: Animal)
            if animal.tag == "Cat" then
                local cat = animal
            else
                local dog = animal
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ(R"({| catfood: string, name: string, tag: "Cat" |})", toString(requireTypeAtPosition({7, 33})));
        CHECK_EQ(R"({| dogfood: string, name: string, tag: "Dog" |})", toString(requireTypeAtPosition({9, 33})));
    }
    else
    {
        CHECK_EQ("Cat", toString(requireTypeAtPosition({7, 33})));
        CHECK_EQ("Dog", toString(requireTypeAtPosition({9, 33})));
    }
}

TEST_CASE_FIXTURE(Fixture, "and_or_peephole_refinement")
{
    CheckResult result = check(R"(
        local function len(a: {any})
            return a and #a or nil
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "narrow_boolean_to_true_or_false")
{
    CheckResult result = check(R"(
        local function f(x: boolean)
            if x then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("true", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("false", toString(requireTypeAtPosition({5, 28})));
}

TEST_CASE_FIXTURE(Fixture, "discriminate_on_properties_of_disjoint_tables_where_that_property_is_true_or_false")
{
    CheckResult result = check(R"(
        type Ok<T> = { ok: true, value: T }
        type Err<E> = { ok: false, error: E }
        type Result<T, E> = Ok<T> | Err<E>

        local function apply<T, E>(t: Result<T, E>, f: (T) -> (), g: (E) -> ())
            if t.ok then
                f(t.value)
            else
                g(t.error)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "refine_a_property_not_to_be_nil_through_an_intersection_table")
{
    CheckResult result = check(R"(
        type T = {} & {f: ((string) -> string)?}
        local function f(t: T, x)
            if t.f then
                t.f(x)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(RefinementClassFixture, "discriminate_from_isa_of_x")
{
    CheckResult result = check(R"(
        type T = {tag: "Part", x: Part} | {tag: "Folder", x: Folder}

        local function f(t: T)
            if t.x:IsA("Part") then
                local foo = t
            else
                local bar = t
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(R"({| tag: "Part", x: Part |})", toString(requireTypeAtPosition({5, 28})));
    CHECK_EQ(R"({| tag: "Folder", x: Folder |})", toString(requireTypeAtPosition({7, 28})));
}

TEST_CASE_FIXTURE(RefinementClassFixture, "typeguard_cast_free_table_to_vector")
{
    CheckResult result = check(R"(
        local function f(vec)
            local X, Y, Z = vec.X, vec.Y, vec.Z

            if type(vec) == "vector" then
                local foo = vec
            elseif typeof(vec) == "Instance" then
                local foo = vec
            else
                local foo = vec
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Vector3", toString(requireTypeAtPosition({5, 28}))); // type(vec) == "vector"

    CHECK_EQ("never", toString(requireTypeAtPosition({7, 28}))); // typeof(vec) == "Instance"

    CHECK_EQ("{+ X: a, Y: b, Z: c +}", toString(requireTypeAtPosition({9, 28}))); // type(vec) ~= "vector" and typeof(vec) ~= "Instance"
}

TEST_CASE_FIXTURE(RefinementClassFixture, "typeguard_cast_instance_or_vector3_to_vector")
{
    CheckResult result = check(R"(
        local function f(x: Instance | Vector3)
            if typeof(x) == "Vector3" then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Vector3", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("Instance", toString(requireTypeAtPosition({5, 28})));
}

TEST_CASE_FIXTURE(RefinementClassFixture, "type_narrow_for_all_the_userdata")
{
    CheckResult result = check(R"(
        local function f(x: string | number | Instance | Vector3)
            if type(x) == "userdata" then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Instance | Vector3", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("number | string", toString(requireTypeAtPosition({5, 28})));
}

TEST_CASE_FIXTURE(RefinementClassFixture, "type_narrow_but_the_discriminant_type_isnt_a_class")
{
    CheckResult result = check(R"(
        local function f(x: string | number | Instance | Vector3)
            if type(x) == "any" then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("never", toString(requireTypeAtPosition({3, 28})));
        CHECK_EQ("Instance | Vector3 | number | string", toString(requireTypeAtPosition({5, 28})));
    }
    else
    {
        CHECK_EQ("*error-type*", toString(requireTypeAtPosition({3, 28})));
        CHECK_EQ("*error-type*", toString(requireTypeAtPosition({5, 28})));
    }
}

TEST_CASE_FIXTURE(RefinementClassFixture, "eliminate_subclasses_of_instance")
{
    CheckResult result = check(R"(
        local function f(x: Part | Folder | string)
            if typeof(x) == "Instance" then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Folder | Part", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("string", toString(requireTypeAtPosition({5, 28})));
}

TEST_CASE_FIXTURE(RefinementClassFixture, "narrow_from_subclasses_of_instance_or_string_or_vector3")
{
    CheckResult result = check(R"(
        local function f(x: Part | Folder | string | Vector3)
            if typeof(x) == "Instance" then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Folder | Part", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("Vector3 | string", toString(requireTypeAtPosition({5, 28})));
}

TEST_CASE_FIXTURE(RefinementClassFixture, "x_as_any_if_x_is_instance_elseif_x_is_table")
{
    CheckResult result = check(R"(
        --!nonstrict

        local function f(x)
            if typeof(x) == "Instance" and x:IsA("Folder") then
                local foo = x
            elseif typeof(x) == "table" then
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("Folder & Instance & {-  -}", toString(requireTypeAtPosition({5, 28})));
        CHECK_EQ("(~Folder | ~Instance) & {-  -} & never", toString(requireTypeAtPosition({7, 28})));
    }
    else
    {
        CHECK_EQ("Folder", toString(requireTypeAtPosition({5, 28})));
        CHECK_EQ("any", toString(requireTypeAtPosition({7, 28})));
    }
}

TEST_CASE_FIXTURE(RefinementClassFixture, "refine_param_of_type_instance_without_using_typeof")
{
    CheckResult result = check(R"(
        local function f(x: Instance)
            if x:IsA("Folder") then
                local foo = x
            elseif typeof(x) == "table" then
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Folder", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("never", toString(requireTypeAtPosition({5, 28})));
}

TEST_CASE_FIXTURE(RefinementClassFixture, "refine_param_of_type_folder_or_part_without_using_typeof")
{
    CheckResult result = check(R"(
        local function f(x: Part | Folder)
            if x:IsA("Folder") then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Folder", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("Part", toString(requireTypeAtPosition({5, 28})));
}

TEST_CASE_FIXTURE(RefinementClassFixture, "isa_type_refinement_must_be_known_ahead_of_time")
{
    CheckResult result = check(R"(
        local function f(x): Instance
            if x:IsA("Folder") then
                local foo = x
            else
                local foo = x
            end

            return x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Instance", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("Instance", toString(requireTypeAtPosition({5, 28})));
}

TEST_CASE_FIXTURE(RefinementClassFixture, "x_is_not_instance_or_else_not_part")
{
    CheckResult result = check(R"(
        local function f(x: Part | Folder | string)
            if typeof(x) ~= "Instance" or not x:IsA("Part") then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Folder | string", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("Part", toString(requireTypeAtPosition({5, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typeguard_doesnt_leak_to_elseif")
{
    CheckResult result = check(R"(
        function f(a)
           if type(a) == "boolean" then
                local a1 = a
            elseif a.fn() then
                local a2 = a
            else
                local a3 = a
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "refine_unknowns")
{
    CheckResult result = check(R"(
        local function f(x: unknown)
            if type(x) == "string" then
                local foo = x
            else
                local bar = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("string", toString(requireTypeAtPosition({3, 28})));
        CHECK_EQ("~string", toString(requireTypeAtPosition({5, 28})));
    }
    else
    {
        CHECK_EQ("string", toString(requireTypeAtPosition({3, 28})));
        CHECK_EQ("unknown", toString(requireTypeAtPosition({5, 28})));
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "refine_boolean")
{
    CheckResult result = check(R"(
        local function f(x: number | boolean)
            if typeof(x) == "boolean" then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("boolean", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("number", toString(requireTypeAtPosition({5, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "refine_thread")
{
    CheckResult result = check(R"(
        local function f(x: number | thread)
            if typeof(x) == "thread" then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("thread", toString(requireTypeAtPosition({3, 28})));
    CHECK_EQ("number", toString(requireTypeAtPosition({5, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "falsiness_of_TruthyPredicate_narrows_into_nil")
{
    CheckResult result = check(R"(
        local function f(t: {number})
            local x = t[1]
            if not x then
                local foo = x
            else
                local bar = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("nil", toString(requireTypeAtPosition({4, 28})));
    CHECK_EQ("number", toString(requireTypeAtPosition({6, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "what_nonsensical_condition")
{
    CheckResult result = check(R"(
        local function f(x)
            if type(x) == "string" and type(x) == "number" then
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("never", toString(requireTypeAtPosition({3, 28})));
}

TEST_CASE_FIXTURE(Fixture, "else_with_no_explicit_expression_should_also_refine_the_tagged_union")
{
    CheckResult result = check(R"(
        type Ok<T> = { tag: "ok", value: T }
        type Err<E> = { tag: "err", err: E }
        type Result<T, E> = Ok<T> | Err<E>

        function and_then<T, U, E>(r: Result<T, E>, f: (T) -> U): Result<U, E>
            if r.tag == "ok" then
                return { tag = "ok", value = f(r.value) }
            else
                return r
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "fuzz_filtered_refined_types_are_followed")
{
    CheckResult result = check(R"(
local _
do
local _ = _ ~= _ or _ or _
end
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        // Without a realistic motivating case, it's hard to tell if it's important for this to work without errors.
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK(get<NormalizationTooComplex>(result.errors[0]));
    }
    else
        LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "refine_unknown_to_table_then_take_the_length")
{
    CheckResult result = check(R"(
        local function f(x: unknown)
            if typeof(x) == "table" then
                local len = #x
            end
        end
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        LUAU_REQUIRE_NO_ERRORS(result);
        CHECK_EQ("table", toString(requireTypeAtPosition({3, 29})));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK_EQ("unknown", toString(requireTypeAtPosition({3, 29})));
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "refine_unknown_to_table_then_clone_it")
{
    CheckResult result = check(R"(
        local function f(x: unknown)
            if typeof(x) == "table" then
                local cloned: {} = table.clone(x)
            end
        end
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
    }
}

TEST_CASE_FIXTURE(RefinementClassFixture, "refine_a_param_that_got_resolved_during_constraint_solving_stage")
{
    CheckResult result = check(R"(
        type Id<T> = T

        local function f(x: Id<Id<Part | Folder> | Id<string>>)
            if typeof(x) ~= "string" and x:IsA("Part") then
                local foo = x
            else
                local foo = x
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("Part", toString(requireTypeAtPosition({5, 28})));
    CHECK_EQ("Folder | string", toString(requireTypeAtPosition({7, 28})));
}

TEST_CASE_FIXTURE(RefinementClassFixture, "refine_a_param_that_got_resolved_during_constraint_solving_stage_2")
{
    CheckResult result = check(R"(
        local function hof(f: (Instance) -> ()) end

        hof(function(inst)
            if inst:IsA("Part") then
                local foo = inst
            else
                local foo = inst
            end
        end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("Part", toString(requireTypeAtPosition({5, 28})));
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("Instance & ~Part", toString(requireTypeAtPosition({7, 28})));
    else
        CHECK_EQ("Instance", toString(requireTypeAtPosition({7, 28})));
}

TEST_CASE_FIXTURE(Fixture, "refine_a_property_of_some_global")
{
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
        foo = { bar = 5 :: number? }

        if foo.bar then
            local bar = foo.bar
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);

    CHECK_EQ("~false & ~nil", toString(requireTypeAtPosition({4, 30})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "dataflow_analysis_can_tell_refinements_when_its_appropriate_to_refine_into_nil_or_never")
{
    CheckResult result = check(R"(
        local function f(t: {string}, s: string)
            local v1 = t[5]
            local v2 = v1

            if typeof(v1) == "nil" then
                local foo = v1
            else
                local foo = v1
            end

            if typeof(v2) == "nil" then
                local foo = v2
            else
                local foo = v2
            end

            if typeof(s) == "nil" then
                local foo = s
            else
                local foo = s
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("nil", toString(requireTypeAtPosition({6, 28})));
    CHECK_EQ("string", toString(requireTypeAtPosition({8, 28})));

    CHECK_EQ("nil", toString(requireTypeAtPosition({12, 28})));
    CHECK_EQ("string", toString(requireTypeAtPosition({14, 28})));

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("never", toString(requireTypeAtPosition({18, 28})));
        CHECK_EQ("string", toString(requireTypeAtPosition({20, 28})));
    }
    else
    {
        CHECK_EQ("nil", toString(requireTypeAtPosition({18, 28})));
        CHECK_EQ("string", toString(requireTypeAtPosition({20, 28})));
    }
}

TEST_CASE_FIXTURE(Fixture, "cat_or_dog_through_a_local")
{
    CheckResult result = check(R"(
        type Cat = { tag: "cat", catfood: string }
        type Dog = { tag: "dog", dogfood: string }
        type Animal = Cat | Dog

        local function f(animal: Animal)
            local tag = animal.tag
            if tag == "dog" then
                local dog = animal
            elseif tag == "cat" then
                local cat = animal
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Cat | Dog", toString(requireTypeAtPosition({8, 28})));
    CHECK_EQ("Cat | Dog", toString(requireTypeAtPosition({10, 28})));
}

TEST_CASE_FIXTURE(Fixture, "prove_that_dataflow_analysis_isnt_doing_alias_tracking_yet")
{
    CheckResult result = check(R"(
        local function f(tag: "cat" | "dog")
            local tag2 = tag

            if tag2 == "cat" then
                local foo = tag
            else
                local foo = tag
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(R"("cat" | "dog")", toString(requireTypeAtPosition({5, 28})));
    CHECK_EQ(R"("cat" | "dog")", toString(requireTypeAtPosition({7, 28})));
}

TEST_CASE_FIXTURE(Fixture, "fail_to_refine_a_property_of_subscript_expression")
{
    CheckResult result = check(R"(
        type Foo = { foo: number? }
        local function f(t: {Foo})
            if t[1].foo then
                local foo = t[1].foo
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number?", toString(requireTypeAtPosition({4, 34})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_annotations_arent_relevant_when_doing_dataflow_analysis")
{
    CheckResult result = check(R"(
        local function s() return "hello" end

        local function f(t: {string})
            local s1: string = t[5]
            local s2: string = s()

            if typeof(s1) == "nil" and typeof(s2) == "nil" then
                local foo = s1
                local bar = s2
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("nil", toString(requireTypeAtPosition({8, 28})));
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("never", toString(requireTypeAtPosition({9, 28})));
    else
        CHECK_EQ("nil", toString(requireTypeAtPosition({9, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "function_call_with_colon_after_refining_not_to_be_nil")
{
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
        --!strict
        export type Observer<T> = {
            complete: ((self: Observer<T>) -> ())?,
        }

        local function _f(handler: Observer<any>)
            assert(handler.complete ~= nil)
            handler:complete() -- incorrectly gives Value of type '((Observer<any>) -> ())?' could be nil
            handler.complete(handler) -- works fine, both forms should avoid the error
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "refinements_should_not_affect_assignment")
{
    CheckResult result = check(R"(
        local a: unknown = true
        if a == true then
            a = 'not even remotely similar to a boolean'
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
