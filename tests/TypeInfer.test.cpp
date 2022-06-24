// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"

#include "Fixture.h"

#include "doctest.h"

#include <algorithm>

LUAU_FASTFLAG(LuauLowerBoundsCalculation);
LUAU_FASTFLAG(LuauFixLocationSpanTableIndexExpr);
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);

using namespace Luau;

TEST_SUITE_BEGIN("TypeInfer");

TEST_CASE_FIXTURE(Fixture, "tc_hello_world")
{
    CheckResult result = check("local a = 7");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId aType = requireType("a");
    CHECK_EQ(getPrimitiveType(aType), PrimitiveTypeVar::Number);
}

TEST_CASE_FIXTURE(Fixture, "tc_propagation")
{
    CheckResult result = check("local a = 7   local b = a");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId bType = requireType("b");
    CHECK_EQ(getPrimitiveType(bType), PrimitiveTypeVar::Number);
}

TEST_CASE_FIXTURE(Fixture, "tc_error")
{
    CheckResult result = check("local a = 7   local b = 'hi'   a = b");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(result.errors[0], (TypeError{Location{Position{0, 35}, Position{0, 36}}, TypeMismatch{typeChecker.numberType, typeChecker.stringType}}));
}

TEST_CASE_FIXTURE(Fixture, "tc_error_2")
{
    CheckResult result = check("local a = 7   a = 'hi'");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(result.errors[0], (TypeError{Location{Position{0, 18}, Position{0, 22}}, TypeMismatch{
                                                                                          requireType("a"),
                                                                                          typeChecker.stringType,
                                                                                      }}));
}

TEST_CASE_FIXTURE(Fixture, "infer_locals_with_nil_value")
{
    CheckResult result = check("local f = nil; f = 'hello world'");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId ty = requireType("f");
    CHECK_EQ(getPrimitiveType(ty), PrimitiveTypeVar::String);
}

TEST_CASE_FIXTURE(Fixture, "infer_locals_via_assignment_from_its_call_site")
{
    CheckResult result = check(R"(
        local a
        function f(x) a = x end
        f(1)
        f("foo")
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("number", toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "infer_in_nocheck_mode")
{
    ScopedFastFlag sff[]{
        {"DebugLuauDeferredConstraintResolution", false},
        {"LuauReturnTypeInferenceInNonstrict", true},
        {"LuauLowerBoundsCalculation", true},
    };

    CheckResult result = check(R"(
        --!nocheck
        function f(x)
            return 5
        end
         -- we get type information even if there's type errors
        f(1, 2)
    )");

    CHECK_EQ("(any) -> number", toString(requireType("f")));

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "expr_statement")
{
    CheckResult result = check("local foo = 5    foo()");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "if_statement")
{
    CheckResult result = check(R"(
        local a
        local b

        if true then
            a = 'hello'
        else
            b = 999
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.stringType, *requireType("a"));
    CHECK_EQ(*typeChecker.numberType, *requireType("b"));
}

TEST_CASE_FIXTURE(Fixture, "statements_are_topologically_sorted")
{
    CheckResult result = check(R"(
        function foo()
            return bar(999), bar("hi")
        end

        function bar(i)
            return i
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);
}

TEST_CASE_FIXTURE(Fixture, "unify_nearly_identical_recursive_types")
{
    CheckResult result = check(R"(
        local o
        o:method()

        local p
        p:method()

        o = p
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "warn_on_lowercase_parent_property")
{
    CheckResult result = check(R"(
        local M = require(script.parent.DoesNotMatter)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto ed = get<DeprecatedApiUsed>(result.errors[0]);
    REQUIRE(ed);

    REQUIRE_EQ("parent", ed->symbol);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "weird_case")
{
    CheckResult result = check(R"(
        local function f() return 4 end
        local d = math.deg(f())
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "dont_ice_when_failing_the_occurs_check")
{
    CheckResult result = check(R"(
        --!strict
        local s
        s(s, 'a')
    )");
    LUAU_REQUIRE_ERROR_COUNT(0, result);
}

TEST_CASE_FIXTURE(Fixture, "occurs_check_does_not_recurse_forever_if_asked_to_traverse_a_cyclic_type")
{
    CheckResult result = check(R"(
         --!strict
        function u(t, w)
            u(u, t)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

#if 0
// CLI-29798
TEST_CASE_FIXTURE(Fixture, "crazy_complexity")
{
    CheckResult result = check(R"(
        --!nonstrict
        A:A():A():A():A():A():A():A():A():A():A():A()
    )");

    std::cout << "OK!  Allocated " << typeChecker.typeVars.size() << " typevars" << std::endl;
}
#endif

TEST_CASE_FIXTURE(Fixture, "type_errors_infer_types")
{
    CheckResult result = check(R"(
        local err = (true).x
        local c = err.Parent.Reward.GetChildren
        local d = err.Parent.Reward
        local e = err.Parent
        local f = err
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownProperty* err = get<UnknownProperty>(result.errors[0]);
    REQUIRE(err != nullptr);
    CHECK_EQ("boolean", toString(err->table));
    CHECK_EQ("x", err->key);

    // TODO: Should we assert anything about these tests when DCR is being used?
    if (!FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("*unknown*", toString(requireType("c")));
        CHECK_EQ("*unknown*", toString(requireType("d")));
        CHECK_EQ("*unknown*", toString(requireType("e")));
        CHECK_EQ("*unknown*", toString(requireType("f")));
    }
}

TEST_CASE_FIXTURE(Fixture, "should_be_able_to_infer_this_without_stack_overflowing")
{
    CheckResult result = check(R"(
        local function f(x, y)
            return x or y
        end

        local function dont_crash(x, y)
            local z: typeof(f(x, y)) = f(x, y)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "exponential_blowup_from_copying_types")
{
    CheckResult result = check(R"(
        --!strict
        -- An example of exponential blowup in number of types
        -- The problem is that if we define function f(a) return x end
        -- then this has type <t>(t)->T where x:T
        -- *but* it copies T each time f is applied
        -- so { left = f("hi"), right = f(5) }
        -- has type { left : T_L, right : T_R }
        -- where T_L and T_R are copies of T.
        -- x0 : T0 where T0 = {}
        local x0 = {}
        -- f0 : <t>(t)->T0
        local function f0(a) return x0 end
        -- x1 : T1 where T1 = { left : T0_L, right : T0_R }
        local x1 = { left = f0("hi"), right = f0(5) }
        -- f1 : <t>(t)->T1
        local function f1(a) return x1 end
        -- x2 : T2 where T2 = { left : T1_L, right : T1_R }
        local x2 = { left = f1("hi"), right = f1(5) }
        -- f2 : <t>(t)->T2
        local function f2(a) return x2 end
        -- etc etc
        local x3 = { left = f2("hi"), right = f2(5) }
        local function f3(a) return x3 end
        local x4 = { left = f3("hi"), right = f3(5) }
        return x4
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    ModulePtr module = getMainModule();

    // If we're not careful about copying, this ends up with O(2^N) types rather than O(N)
    // (in this case 5 vs 31).
    CHECK_GE(5, module->interfaceTypes.typeVars.size());
}

// In these tests, a successful parse is required, so we need the parser to return the AST and then we can test the recursion depth limit in type
// checker. We also want it to somewhat match up with production values, so we push up the parser recursion limit a little bit instead.
TEST_CASE_FIXTURE(Fixture, "check_type_infer_recursion_count")
{
#if defined(LUAU_ENABLE_ASAN)
    int limit = 250;
#elif defined(_DEBUG) || defined(_NOOPT)
    int limit = 350;
#else
    int limit = 600;
#endif

    ScopedFastInt sfi{"LuauCheckRecursionLimit", limit};

    CheckResult result = check("function f() return " + rep("{a=", limit) + "'a'" + rep("}", limit) + " end");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(nullptr != get<CodeTooComplex>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "check_block_recursion_limit")
{
#if defined(LUAU_ENABLE_ASAN)
    int limit = 250;
#elif defined(_DEBUG) || defined(_NOOPT)
    int limit = 350;
#else
    int limit = 600;
#endif

    ScopedFastInt luauRecursionLimit{"LuauRecursionLimit", limit + 100};
    ScopedFastInt luauCheckRecursionLimit{"LuauCheckRecursionLimit", limit - 100};

    CheckResult result = check(rep("do ", limit) + "local a = 1" + rep(" end", limit));

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(nullptr != get<CodeTooComplex>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "check_expr_recursion_limit")
{
#if defined(LUAU_ENABLE_ASAN)
    int limit = 250;
#elif defined(_DEBUG) || defined(_NOOPT)
    int limit = 300;
#else
    int limit = 600;
#endif
    ScopedFastInt luauRecursionLimit{"LuauRecursionLimit", limit + 100};
    ScopedFastInt luauCheckRecursionLimit{"LuauCheckRecursionLimit", limit - 100};

    CheckResult result = check(R"(("foo"))" + rep(":lower()", limit));

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(nullptr != get<CodeTooComplex>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "globals_are_banned_in_strict_mode")
{
    CheckResult result = check(R"(
        --!strict
        foo = true
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownSymbol* us = get<UnknownSymbol>(result.errors[0]);
    REQUIRE(us);
    CHECK_EQ("foo", us->name);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "correctly_scope_locals_do")
{
    CheckResult result = check(R"(
        do
            local a = 1
        end

        print(a) -- oops!
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownSymbol* us = get<UnknownSymbol>(result.errors[0]);
    REQUIRE(us);
    CHECK_EQ(us->name, "a");
}

TEST_CASE_FIXTURE(Fixture, "checking_should_not_ice")
{
    CHECK_NOTHROW(check(R"(
        --!nonstrict
        f,g = ...
        f(g(...))[...] = nil
        f,xpcall = ...
        local value = g(...)(g(...))
    )"));

    CHECK_EQ("any", toString(requireType("value")));
}

TEST_CASE_FIXTURE(Fixture, "cyclic_follow")
{
    check(R"(
--!nonstrict
l0,table,_,_,_ = ...
_,_,_,_.time(...)._.n0,l0,_ = function(l0)
end,_.__index,(_),_.time(_.n0 or _,...)
for l0=...,_,"" do
end
_ += not _
do end
)");

    check(R"(
--!nonstrict
n13,_,table,_,l0,_,_ = ...
_,n0[(_)],_,_._(...)._.n39,l0,_._ = function(l84,...)
end,_.__index,"",_,l0._(nil)
for l0=...,table.n5,_ do
end
_:_(...).n1 /= _
do
_(_ + _)
do end
end
)");
}

struct FindFreeTypeVars
{
    bool foundOne = false;

    template<typename ID>
    void cycle(ID)
    {
    }

    template<typename ID, typename T>
    bool operator()(ID, T)
    {
        return !foundOne;
    }

    template<typename ID>
    bool operator()(ID, Unifiable::Free)
    {
        foundOne = true;
        return false;
    }
};

TEST_CASE_FIXTURE(Fixture, "tc_after_error_recovery")
{
    CheckResult result = check(R"(
        local x =
        local a = 7
    )");
    LUAU_REQUIRE_ERRORS(result);

    TypeId aType = requireType("a");
    CHECK_EQ(getPrimitiveType(aType), PrimitiveTypeVar::Number);
}

// Check that type checker knows about error expressions
TEST_CASE_FIXTURE(Fixture, "tc_after_error_recovery_no_assert")
{
    CheckResult result = check("function +() local _ = true end");
    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "tc_after_error_recovery_no_replacement_name_in_error")
{
    {
        CheckResult result = check(R"(
            --!strict
            local t = { x = 10, y = 20 }
            return t.
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
    }

    {
        CheckResult result = check(R"(
            --!strict
            export type = number
            export type = string
        )");

        LUAU_REQUIRE_ERROR_COUNT(2, result);
    }

    {
        CheckResult result = check(R"(
            --!strict
            function string.() end
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
    }

    {
        CheckResult result = check(R"(
            --!strict
            local function () end
            local function () end
        )");

        LUAU_REQUIRE_ERROR_COUNT(2, result);
    }

    {
        CheckResult result = check(R"(
            --!strict
            local dm = {}
            function dm.() end
            function dm.() end
        )");

        LUAU_REQUIRE_ERROR_COUNT(2, result);
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_expr_should_be_checked")
{
    CheckResult result = check(R"(
        local foo: any

        print(foo[(true).x])
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownProperty* up = get<UnknownProperty>(result.errors[0]); // Should probably be NotATable
    REQUIRE(up);
    CHECK_EQ("boolean", toString(up->table));
    CHECK_EQ("x", up->key);
}

TEST_CASE_FIXTURE(Fixture, "stringify_nested_unions_with_optionals")
{
    CheckResult result = check(R"(
        --!strict
        local a: number | (string | boolean) | nil
        local b: number = a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(typeChecker.numberType, tm->wantedType);
    CHECK_EQ("(boolean | number | string)?", toString(tm->givenType));
}

TEST_CASE_FIXTURE(Fixture, "cli_39932_use_unifier_in_ensure_methods")
{
    CheckResult result = check(R"(
        local x: {number|number} = {1, 2, 3}
        local y = x[1] - x[2]
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "dont_report_type_errors_within_an_AstStatError")
{
    CheckResult result = check(R"(
        foo
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "dont_report_type_errors_within_an_AstExprError")
{
    CheckResult result = check(R"(
        local a = foo:
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(Fixture, "dont_ice_on_astexprerror")
{
    CheckResult result = check(R"(
        local foo = -;
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "luau_resolves_symbols_the_same_way_lua_does")
{
    CheckResult result = check(R"(
        --!strict
        function Funky()
            local a: number = foo
        end

        local foo: string = 'hello'
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto e = result.errors.front();
    REQUIRE_MESSAGE(get<UnknownSymbol>(e) != nullptr, "Expected UnknownSymbol, but got " << e);
}

TEST_CASE_FIXTURE(Fixture, "no_stack_overflow_from_isoptional")
{
    CheckResult result = check(R"(
        function _(l0:t0): (any, ()->())
            return 0,_
        end

        type t0 = t0 | {}
        _(nil)
    )");

    LUAU_REQUIRE_ERRORS(result);

    std::optional<TypeFun> t0 = getMainModule()->getModuleScope()->lookupType("t0");
    REQUIRE(t0);
    CHECK_EQ("*unknown*", toString(t0->type));

    auto it = std::find_if(result.errors.begin(), result.errors.end(), [](TypeError& err) {
        return get<OccursCheckFailed>(err);
    });
    CHECK(it != result.errors.end());
}

TEST_CASE_FIXTURE(BuiltinsFixture, "no_stack_overflow_from_isoptional2")
{
    CheckResult result = check(R"(
        function _(l0:({})|(t0)):((((typeof((xpcall)))|(t96<t0>))|(t13))&(t96<t0>),()->typeof(...))
            return 0,_
        end

        type t0<t107> = ((typeof((_G)))|(({})|(t0)))|(t0)
        _(nil)

        local t: ({})|(t0)
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "no_infinite_loop_when_trying_to_unify_uh_this")
{
    CheckResult result = check(R"(
        function _(l22,l0):((((boolean)|(t0))|(t0))&(()->(()->(()->()->{},(t0<t22>)|(t0)),any)))
            return function():t0<t0>
            end
        end
        type t0<t0> = ((typeof(_))|(any))|(typeof(_))
        _()
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "no_heap_use_after_free_error")
{
    CheckResult result = check(R"(
        --!nonstrict
        _ += _:n0(xpcall,_)
        local l0
        do end
        while _ do
            function _:_()
                _ += _(_._(_:n0(xpcall,_)))
            end
        end
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_type_assertion_value_type")
{
    CheckResult result = check(R"(
local function f()
    return {4, "b", 3} :: {string|number}
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_assignment_value_types")
{
    CheckResult result = check(R"(
local a: (number, number) -> number = function(a, b) return a - b end

a = function(a, b) return a + b end

local b: {number|string}
local c: {number|string}
b, c = {2, "s"}, {"b", 4}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "infer_assignment_value_types_mutable_lval")
{
    CheckResult result = check(R"(
local a = {}
a.x = 2
a = setmetatable(a, { __call = function(x) end })
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_through_group_expr")
{
    CheckResult result = check(R"(
local function f(a: (number, number) -> number) return a(1, 3) end
f(((function(a, b) return a + b end)))
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "tc_if_else_expressions1")
{
    CheckResult result = check(R"(local a = if true then "true" else "false")");
    LUAU_REQUIRE_NO_ERRORS(result);
    TypeId aType = requireType("a");
    CHECK_EQ(getPrimitiveType(aType), PrimitiveTypeVar::String);
}

TEST_CASE_FIXTURE(Fixture, "tc_if_else_expressions2")
{
    // Test expression containing elseif
    CheckResult result = check(R"(
local a = if false then "a" elseif false then "b" else "c"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    TypeId aType = requireType("a");
    CHECK_EQ(getPrimitiveType(aType), PrimitiveTypeVar::String);
}

TEST_CASE_FIXTURE(Fixture, "tc_if_else_expressions_type_union")
{
    CheckResult result = check(R"(local a: number? = if true then 42 else nil)");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("a"), {true}), "number?");
}

TEST_CASE_FIXTURE(Fixture, "tc_if_else_expressions_expected_type_1")
{
    CheckResult result = check(R"(
type X = {number | string}
local a: X = if true then {"1", 2, 3} else {4, 5, 6}
)");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("a"), {true}), "{number | string}");
}

TEST_CASE_FIXTURE(Fixture, "tc_if_else_expressions_expected_type_2")
{
    CheckResult result = check(R"(
local a: number? = if true then 1 else nil
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "tc_if_else_expressions_expected_type_3")
{
    CheckResult result = check(R"(
local function times<T>(n: any, f: () -> T)
    local result: {T} = {}
    local res = f()
    table.insert(result, if true then res else n)
    return result
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

/*
 * If it wasn't instantly obvious, we have the fuzzer to thank for this gem of a test.
 *
 * We had an issue here where the scope for the `if` block here would
 * have an elevated TypeLevel even though there is no function nesting going on.
 * This would result in a free typevar for the type of _ that was much higher than
 * it should be.  This type would be erroneously quantified in the definition of `aaa`.
 * This in turn caused an ice when evaluating `_()` in the while loop.
 */
TEST_CASE_FIXTURE(Fixture, "free_typevars_introduced_within_control_flow_constructs_do_not_get_an_elevated_TypeLevel")
{
    check(R"(
        --!strict
        if _ then
            _[_], _ = nil
            _()
        end

        local aaa = function():typeof(_) return 1 end

        if aaa then
            while _() do
            end
        end
    )");

    // No ice()?  No problem.
}

/*
 * This is a bit elaborate.  Bear with me.
 *
 * The type of _ becomes free with the first statement.  With the second, we unify it with a function.
 *
 * At this point, it is important that the newly created fresh types of this new function type are promoted
 * to the same level as the original free type.  If we do not, they are incorrectly ascribed the level of the
 * containing function.
 *
 * If this is allowed to happen, the final lambda erroneously quantifies the type of _ to something ridiculous
 * just before we typecheck the invocation to _.
 */
TEST_CASE_FIXTURE(Fixture, "fuzzer_found_this")
{
    check(R"(
        l0, _ = nil

        local function p()
            _()
        end

        a = _(
            function():(typeof(p),typeof(_))
            end
        )[nil]
    )");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "recursive_metatable_crash")
{
    CheckResult result = check(R"(
local function getIt()
    local y
    y = setmetatable({}, y)
    return y
end
local a = getIt()
local b = getIt()
local c = a or b
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "bound_typepack_promote")
{
    // No assertions should trigger
    check(R"(
local function p()
    local this = {}
    this.pf = foo()
    function this:IsActive() end
    function this:Start(o) end
    return this
end

local function h(tp, o)
    ep = tp
    tp:Start(o)
    tp.pf.Connect(function()
        ep:IsActive()
    end)
end

function on()
    local t = p()
    h(t)
end
    )");
}

TEST_CASE_FIXTURE(Fixture, "cli_50041_committing_txnlog_in_apollo_client_error")
{
    CheckResult result = check(R"(
        --!strict
        --!nolint

        type FieldSpecifier = {
            fieldName: string,
        }

        type ReadFieldOptions = FieldSpecifier & { from: number? }

        type Policies = {
            getStoreFieldName: (self: Policies, fieldSpec: FieldSpecifier) -> string,
        }

        local Policies = {}

        local function foo(p: Policies)
        end

        function Policies:getStoreFieldName(specifier: FieldSpecifier): string
            return ""
        end

        function Policies:readField(options: ReadFieldOptions)
            local _ = self:getStoreFieldName(options)
            --[[
                Type error:
                TypeError { "MainModule", Location { { line = 25, col = 16 }, { line = 25, col = 20 } }, TypeMismatch { Policies, {- getStoreFieldName: (tp1) -> (a, b...) -} } }
            ]]
            foo(self)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "type_infer_recursion_limit_no_ice")
{
    ScopedFastInt sfi("LuauTypeInferRecursionLimit", 2);

    CheckResult result = check(R"(
        function complex()
          function _(l0:t0): (any, ()->())
              return 0,_
          end
          type t0 = t0 | {}
          _(nil)
        end
    )");

    LUAU_REQUIRE_ERRORS(result);
    CHECK_EQ("Code is too complex to typecheck! Consider simplifying the code around this area", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "follow_on_new_types_in_substitution")
{
    CheckResult result = check(R"(
        local obj = {}

        function obj:Method()
            self.fieldA = function(object)
                if object.a then
                    self.arr[object] = true
                elseif object.b then
                    self.fieldB[object] = object:Connect(function(arg)
                        self.arr[arg] = nil
                    end)
                end
            end
        end

        return obj
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

/**
 * The problem we had here was that the type of q in B.h was initially inferring to {} | {prop: free} before we bound
 * that second table to the enclosing union.
 */
TEST_CASE_FIXTURE(Fixture, "do_not_bind_a_free_table_to_a_union_containing_that_table")
{
    ScopedFastFlag flag[] = {
        {"LuauLowerBoundsCalculation", true},
    };

    CheckResult result = check(R"(
        --!strict

        local A = {}

        function A:f()
            local t = {}

            for key, value in pairs(self) do
                t[key] = value
            end

            return t
        end

        local B = A:f()

        function B.g(t)
            assert(type(t) == "table")
            assert(t.prop ~= nil)
        end

        function B.h(q)
            q = q or {}
            return q or {}
        end
    )");
}

TEST_SUITE_END();
