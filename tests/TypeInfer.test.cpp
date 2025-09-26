// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/Frontend.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

#include <algorithm>

LUAU_DYNAMIC_FASTINT(LuauConstraintGeneratorRecursionLimit)
LUAU_DYNAMIC_FASTINT(LuauSubtypingRecursionLimit)

LUAU_FASTFLAG(LuauFixLocationSpanTableIndexExpr)
LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauInstantiateInSubtyping)
LUAU_FASTINT(LuauCheckRecursionLimit)
LUAU_FASTINT(LuauNormalizeCacheLimit)
LUAU_FASTINT(LuauRecursionLimit)
LUAU_FASTINT(LuauTypeInferTypePackLoopLimit)
LUAU_FASTINT(LuauTypeInferRecursionLimit)
LUAU_FASTFLAG(LuauDfgAllowUpdatesInLoops)
LUAU_FASTFLAG(LuauInferActualIfElseExprType2)
LUAU_FASTFLAG(DebugLuauMagicTypes)
LUAU_FASTFLAG(LuauReturnMappedGenericPacksFromSubtyping3)
LUAU_FASTFLAG(LuauMissingFollowMappedGenericPacks)
LUAU_FASTFLAG(LuauOccursCheckInCommit)
LUAU_FASTFLAG(LuauEGFixGenericsList)
LUAU_FASTFLAG(LuauParametrizedAttributeSyntax)
LUAU_FASTFLAG(LuauNoConstraintGenRecursionLimitIce)
LUAU_FASTFLAG(LuauTryToOptimizeSetTypeUnification)
LUAU_FASTFLAG(LuauDontReferenceScopePtrFromHashTable)

using namespace Luau;

TEST_SUITE_BEGIN("TypeInfer");

TEST_CASE_FIXTURE(Fixture, "tc_hello_world")
{
    CheckResult result = check("local a = 7");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("number" == toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "tc_propagation")
{
    CheckResult result = check("local a = 7   local b = a");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId bType = requireType("b");
    CHECK_EQ(getPrimitiveType(bType), PrimitiveType::Number);
}

TEST_CASE_FIXTURE(Fixture, "tc_error")
{
    CheckResult result = check("local a = 7   local b = 'hi'   a = b");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_NO_ERRORS(result);
        CHECK("number | string" == toString(requireType("a")));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);

        CHECK_EQ(
            result.errors[0],
            (TypeError{Location{Position{0, 35}, Position{0, 36}}, TypeMismatch{getBuiltins()->numberType, getBuiltins()->stringType}})
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "tc_error_2")
{
    CheckResult result = check("local a = 7   a = 'hi'");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_NO_ERRORS(result);
        CHECK("number | string" == toString(requireType("a")));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);

        CHECK_EQ(
            result.errors[0],
            (TypeError{
                Location{Position{0, 18}, Position{0, 22}},
                TypeMismatch{
                    requireType("a"),
                    getBuiltins()->stringType,
                }
            })
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "infer_locals_with_nil_value")
{
    CheckResult result = check("local f = nil; f = 'hello world'");
    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::LuauSolverV2)
    {
        CHECK("string?" == toString(requireType("f")));
    }
    else
    {
        TypeId ty = requireType("f");
        CHECK_EQ(getPrimitiveType(ty), PrimitiveType::String);
    }
}

TEST_CASE_FIXTURE(Fixture, "infer_locals_with_nil_value_2")
{
    CheckResult result = check(R"(
        local a = 2
        local b = a,nil
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number", toString(requireType("a")));
    CHECK_EQ("number", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "infer_locals_via_assignment_from_its_call_site")
{
    CheckResult result = check(R"(
        local a
        function f(x) a = x end
        f(1)
        f("foo")
    )");

    if (FFlag::LuauSolverV2)
    {
        CHECK("unknown" == toString(requireType("a")));
        CHECK("(unknown) -> ()" == toString(requireType("f")));

        LUAU_REQUIRE_NO_ERRORS(result);
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);

        CHECK_EQ("number", toString(requireType("a")));
    }
}

TEST_CASE_FIXTURE(Fixture, "infer_in_nocheck_mode")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        --!nocheck
        function f(x)
            return x
        end
         -- we get type information even if there's type errors
        f(1, 2)
    )");

    CHECK_EQ("(any) -> (...any)", toString(requireType("f")));

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "obvious_type_error_in_nocheck_mode")
{
    CheckResult result = check(R"(
        --!nocheck
        local x: string = 5
    )");

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

    if (FFlag::LuauSolverV2)
    {
        CHECK("string?" == toString(requireType("a")));
        CHECK("number?" == toString(requireType("b")));
    }
    else
    {
        CHECK_EQ(*getBuiltins()->stringType, *requireType("a"));
        CHECK_EQ(*getBuiltins()->numberType, *requireType("b"));
    }
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
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

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
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

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

    MESSAGE("OK!  Allocated ", typeChecker.types.size(), " types");
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
    if (!FFlag::LuauSolverV2)
    {
        CHECK_EQ("*error-type*", toString(requireType("c")));
        CHECK_EQ("*error-type*", toString(requireType("d")));
        CHECK_EQ("*error-type*", toString(requireType("e")));
        CHECK_EQ("*error-type*", toString(requireType("f")));
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
    CHECK_GE(5, module->interfaceTypes.types.size());
}

// In these tests, a successful parse is required, so we need the parser to return the AST and then we can test the recursion depth limit in type
// checker. We also want it to somewhat match up with production values, so we push up the parser recursion limit a little bit instead.
TEST_CASE_FIXTURE(Fixture, "check_type_infer_recursion_count")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

#if defined(LUAU_ENABLE_ASAN)
    int limit = 250;
#elif defined(_DEBUG) || defined(_NOOPT)
    int limit = 350;
#else
    int limit = 600;
#endif

    ScopedFastInt sfi{FInt::LuauCheckRecursionLimit, limit};

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

    ScopedFastInt luauRecursionLimit{FInt::LuauRecursionLimit, limit + 100};
    ScopedFastInt luauCheckRecursionLimit{FInt::LuauCheckRecursionLimit, limit - 100};
    ScopedFastInt luauConstraintGeneratorRecursionLimit{DFInt::LuauConstraintGeneratorRecursionLimit, limit - 100};
    ScopedFastInt luauSubtypingRecursionLimit{DFInt::LuauSubtypingRecursionLimit, limit - 100};

    CheckResult result = check(rep("do ", limit) + "local a = 1" + rep(" end", limit));

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(nullptr != get<CodeTooComplex>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "check_expr_recursion_limit")
{
#if defined(LUAU_ENABLE_ASAN)
    int limit = 200;
#elif defined(_DEBUG) || defined(_NOOPT)
    int limit = 250;
#else
    int limit = 500;
#endif
    ScopedFastInt luauRecursionLimit{FInt::LuauRecursionLimit, limit + 100};
    ScopedFastInt luauCheckRecursionLimit{FInt::LuauCheckRecursionLimit, limit - 100};
    ScopedFastInt luauConstraintGeneratorRecursionLimit{DFInt::LuauConstraintGeneratorRecursionLimit, limit - 100};
    ScopedFastInt luauSubtypingRecursionLimit{DFInt::LuauSubtypingRecursionLimit, limit - 100};

    CheckResult result = check(R"(("foo"))" + rep(":lower()", limit));

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_MESSAGE(nullptr != get<CodeTooComplex>(result.errors[0]), "Expected CodeTooComplex but got " << toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "globals")
{
    // The new solver does not permit assignments to globals like this.
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        --!nonstrict
        foo = true
        foo = "now i'm a string!"
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("any", toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "globals2")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        --!nonstrict
        foo = function() return 1 end
        foo = "now i'm a string!"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("() -> (...any)", toString(tm->wantedType));
    CHECK_EQ("string", toString(tm->givenType));
    CHECK_EQ("() -> (...any)", toString(requireType("foo")));
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

TEST_CASE_FIXTURE(Fixture, "correctly_scope_locals_do")
{
    CheckResult result = check(R"(
        do
            local a = 1
        end

        local b = a -- oops!
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownSymbol* us = get<UnknownSymbol>(result.errors[0]);
    REQUIRE(us);
    CHECK_EQ(us->name, "a");
}

TEST_CASE_FIXTURE(Fixture, "checking_should_not_ice")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

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
}

TEST_CASE_FIXTURE(Fixture, "cyclic_follow_2")
{
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

struct FindFreeTypes
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

    bool operator()(TypeId, FreeType)
    {
        foundOne = true;
        return false;
    }

    bool operator()(TypePackId, FreeTypePack)
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
    CHECK_EQ(getPrimitiveType(aType), PrimitiveType::Number);
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
        DOES_NOT_PASS_NEW_SOLVER_GUARD();
        getFrontend().setLuauSolverMode(FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old);
        CheckResult result = check(R"(
            --!strict
            local t = { x = 10, y = 20 }
            return t.
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
    }

    {
        getFrontend().setLuauSolverMode(FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old);
        CheckResult result = check(R"(
            --!strict
            export type = number
            export type = string
        )");

        LUAU_REQUIRE_ERROR_COUNT(2, result);
    }

    {
        DOES_NOT_PASS_NEW_SOLVER_GUARD();
        getFrontend().setLuauSolverMode(FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old);
        CheckResult result = check(R"(
            --!strict
            function string.() end
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
    }

    {
        getFrontend().setLuauSolverMode(FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old);
        CheckResult result = check(R"(
            --!strict
            local function () end
            local function () end
        )");

        LUAU_REQUIRE_ERROR_COUNT(2, result);
    }

    {
        getFrontend().setLuauSolverMode(FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old);
        CheckResult result = check(R"(
            --!strict
            local dm = {}
            function dm.() end
            function dm.() end
        )");

        LUAU_REQUIRE_ERROR_COUNT(2, result);
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "invalide_deprecated_attribute_doesn't_chrash_checker")
{
    ScopedFastFlag sff{FFlag::LuauParametrizedAttributeSyntax, true};
    CheckResult result = check(R"(
@[deprecated{ reason = reasonString }]
function hello(x: number, y: number): number
    return x + y
end)");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
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
    CHECK_EQ(getBuiltins()->numberType, tm->wantedType);
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
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        foo
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "dont_report_type_errors_within_an_AstExprError")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

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

    std::optional<TypeId> t0 = lookupType("t0");
    REQUIRE(t0);

    if (FFlag::LuauSolverV2)
        CHECK("any" == toString(*t0));
    else
        CHECK_EQ("*error-type*", toString(*t0));

    auto it = std::find_if(
        result.errors.begin(),
        result.errors.end(),
        [](TypeError& err)
        {
            return get<OccursCheckFailed>(err);
        }
    );
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

    CHECK("string" == toString(aType));
}

TEST_CASE_FIXTURE(Fixture, "tc_if_else_expressions2")
{
    // Test expression containing elseif
    CheckResult result = check(R"(
local a = if false then "a" elseif false then "b" else "c"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    TypeId aType = requireType("a");
    CHECK("string" == toString(aType));
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

TEST_CASE_FIXTURE(Fixture, "tc_interpolated_string_basic")
{
    CheckResult result = check(R"(
        local foo: string = `hello {"world"}`
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "tc_interpolated_string_with_invalid_expression")
{
    CheckResult result = check(R"(
        local function f(x: number) end

        local foo: string = `hello {f("uh oh")}`
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "tc_interpolated_string_constant_type")
{
    CheckResult result = check(R"(
        local foo: "hello" = `hello`
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

/*
 * If it wasn't instantly obvious, we have the fuzzer to thank for this gem of a test.
 *
 * We had an issue here where the scope for the `if` block here would
 * have an elevated TypeLevel even though there is no function nesting going on.
 * This would result in a free type for the type of _ that was much higher than
 * it should be.  This type would be erroneously quantified in the definition of `aaa`.
 * This in turn caused an ice when evaluating `_()` in the while loop.
 */
TEST_CASE_FIXTURE(Fixture, "free_types_introduced_within_control_flow_constructs_do_not_get_an_elevated_TypeLevel")
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

/*
 * We had a bug where we'd improperly cache the normalization of types that are
 * not fully solved yet.  This eventually caused a crash elsewhere in the type
 * solver.
 */
TEST_CASE_FIXTURE(BuiltinsFixture, "fuzzer_found_this_2")
{
    (void)check(R"(
        local _
        if _ then
            _ = _
            while _() do
                _ = # _
            end
        end
    )");
}

TEST_CASE_FIXTURE(Fixture, "indexing_a_cyclic_intersection_does_not_crash")
{
    (void)check(R"(
        local _
        if _ then
            while nil do
                _ = _
            end
        end
        if _[if _ then ""] then
            while nil do
                _ = if _ then ""
            end
        end
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
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

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
            foo(self)
        end
    )");

    if (FFlag::LuauInstantiateInSubtyping)
    {
        // though this didn't error before the flag, it seems as though it should error since fields of a table are invariant.
        // the user's intent would likely be that these "method" fields would be read-only, but without an annotation, accepting this should be
        // unsound.

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        const std::string expected =
            "Type 'Policies' from 'MainModule' could not be converted into 'Policies' from 'MainModule'"
            "\ncaused by:\n"
            "  Property 'getStoreFieldName' is not compatible.\n"
            "Type\n\t"
            "'(Policies, FieldSpecifier & { from: number? }) -> ('a, b...)'"
            "\ncould not be converted into\n\t"
            "'(Policies, FieldSpecifier) -> string'"
            "\ncaused by:\n"
            "  Argument #2 type is not compatible.\n"
            "Type\n\t"
            "'FieldSpecifier'"
            "\ncould not be converted into\n\t"
            "'FieldSpecifier & { from: number? }'"
            "\ncaused by:\n"
            "  Not all intersection parts are compatible.\n"
            "Table type 'FieldSpecifier' not compatible with type '{ from: number? }' because the former has extra field 'fieldName'";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
    else
    {
        LUAU_REQUIRE_NO_ERRORS(result);
    }
}

TEST_CASE_FIXTURE(Fixture, "type_infer_recursion_limit_no_ice")
{
    ScopedFastInt sfi(FInt::LuauTypeInferRecursionLimit, 2);

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

    if (FFlag::LuauSolverV2)
        CHECK("Type contains a self-recursive construct that cannot be resolved" == toString(result.errors[0]));
    else
        CHECK_EQ("Code is too complex to typecheck! Consider simplifying the code around this area", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "type_infer_recursion_limit_normalizer")
{
    ScopedFastInt sfi(FInt::LuauTypeInferRecursionLimit, 10);

    CheckResult result = check(R"(
        function f<a,b,c,d,e,f,g,h,i,j>()
            local x : a&b&c&d&e&f&g&h&(i?)
            local y : (a&b&c&d&e&f&g&h&i)? = x
        end
    )");

    validateErrors(result.errors);
    REQUIRE_MESSAGE(!result.errors.empty(), getErrors(result));

    if (FFlag::LuauSolverV2)
    {
        REQUIRE(4 == result.errors.size());
        CHECK(Location{{2, 22}, {2, 42}} == result.errors[0].location);
        CHECK(Location{{3, 22}, {3, 42}} == result.errors[1].location);
        CHECK(Location{{3, 45}, {3, 46}} == result.errors[2].location);
        CHECK(Location{{3, 22}, {3, 41}} == result.errors[3].location);

        for (const TypeError& e : result.errors)
            CHECK_EQ("Code is too complex to typecheck! Consider simplifying the code around this area", toString(e));
    }
    else
    {
        CHECK(1 == result.errors.size());

        CHECK(Location{{3, 12}, {3, 46}} == result.errors[0].location);
        CHECK_EQ("Code is too complex to typecheck! Consider simplifying the code around this area", toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "type_infer_cache_limit_normalizer")
{
    ScopedFastInt sfi(FInt::LuauNormalizeCacheLimit, 10);

    CheckResult result = check(R"(
        local x : ((number) -> number) & ((string) -> string) & ((nil) -> nil) & (({}) -> {})
        local y : (number | string | nil | {}) -> (number | string | nil | {}) = x
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

TEST_CASE_FIXTURE(Fixture, "types_stored_in_astResolvedTypes")
{
    CheckResult result = check(R"(
        type alias = typeof("hello")
        local function foo(param: alias)
        end
    )");

    auto node = findNodeAtPosition(*getMainSourceModule(), {2, 16});
    auto ty = lookupType("alias");
    REQUIRE(node);
    REQUIRE(node->is<AstExprFunction>());
    REQUIRE(ty);

    auto func = node->as<AstExprFunction>();
    REQUIRE(func->args.size == 1);

    auto arg = *func->args.begin();
    auto annotation = arg->annotation;

    CHECK_EQ(*getMainModule()->astResolvedTypes.find(annotation), *ty);
}

TEST_CASE_FIXTURE(Fixture, "bidirectional_checking_of_higher_order_function")
{
    CheckResult result = check(R"(
        function higher(cb: (number) -> ()) end

        higher(function(n)      -- no error here.  n : number
            local e: string = n -- error here.  n /: string
        end)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    Location location = result.errors[0].location;
    CHECK(location.begin.line == 4);
    CHECK(location.end.line == 4);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "it_is_ok_to_have_inconsistent_number_of_return_values_in_nonstrict")
{
    CheckResult result = check(R"(
        --!nonstrict
        function validate(stats, hits, misses)
            local checked = {}

            for _,l in ipairs(hits) do
                if not (stats[l] and stats[l] > 0) then
                    return false, string.format("expected line %d to be hit", l)
                end
                checked[l] = true
            end

            for _,l in ipairs(misses) do
                if not (stats[l] and stats[l] == 0) then
                    return false, string.format("expected line %d to be missed", l)
                end
                checked[l] = true
            end

            for k,v in pairs(stats) do
                if type(k) == "number" and not checked[k] then
                    return false, string.format("expected line %d to be absent", k)
                end
            end

            return true
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "fuzz_free_table_type_change_during_index_check")
{
    CheckResult result = check(R"(
local _ = nil
while _["" >= _] do
end
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typechecking_in_type_guards")
{
    CheckResult result = check(R"(
local a = type(foo) == 'nil'
local b = typeof(foo) ~= 'nil'
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(toString(result.errors[0]) == "Unknown global 'foo'");
    CHECK(toString(result.errors[1]) == "Unknown global 'foo'");
}

TEST_CASE_FIXTURE(Fixture, "occurs_isnt_always_failure")
{
    CheckResult result = check(R"(
function f(x, c)                   -- x : X
    local y = if c then x else nil -- y : X?
    local z = if c then x else nil -- z : X?
    y = z
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "dcr_delays_expansion_of_function_containing_blocked_parameter_type")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
    };

    CheckResult result = check(R"(
        local b: any

        function f(x)
            local a = b[1] or 'Cn'
            local c = x[1]

            if a:sub(1, #c) == c then
            end
        end
    )");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "recursive_function_that_invokes_itself_with_a_refinement_of_its_parameter")
{
    CheckResult result = check(R"(
        local TRUE: true = true

        local function matches(value, t: true)
            if value then
                return true
            end
        end

        local function readValue(breakpoint)
            if matches(breakpoint, TRUE) then
                readValue(breakpoint)
            end
        end
    )");

    if (FFlag::LuauSolverV2)
        CHECK("(unknown) -> ()" == toString(requireType("readValue")));
    else
        CHECK("<a>(a) -> ()" == toString(requireType("readValue")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "recursive_function_that_invokes_itself_with_a_refinement_of_its_parameter_2")
{
    CheckResult result = check(R"(
        local function readValue(breakpoint)
            if type(breakpoint) == 'number' then
                readValue(breakpoint)
            end
        end
    )");

    if (FFlag::LuauSolverV2)
        CHECK("(unknown) -> ()" == toString(requireType("readValue")));
    else
        CHECK("(number) -> ()" == toString(requireType("readValue")));
}

/*
 * We got into a case where, as we unified two nearly identical unions with one
 * another, where we had a concatenated TxnLog that created a cycle between two
 * free types.
 *
 * This code used to crash the type checker.  See CLI-71190
 */
TEST_CASE_FIXTURE(BuiltinsFixture, "convoluted_case_where_two_TypeVars_were_bound_to_each_other")
{
    check(R"(
        type React_Ref<ElementType> = { current: ElementType } | ((ElementType) -> ())

        type React_AbstractComponent<Config, Instance> = {
            render: ((ref: React_Ref<Instance>) -> nil)
        }

        local createElement : <P, T>(React_AbstractComponent<P, T>) -> ()

        function ScrollView:render()
            local one = table.unpack(
                if true then a else b
            )

            createElement(one)
            createElement(one)
        end
    )");

    // If this code does not crash, we are in good shape.
}

/*
 * Under DCR we had an issue where constraint resolution resulted in the
 * following:
 *
 * *blocked-55* ~ hasProp {- name: *blocked-55* -}, "name"
 *
 * This is a perfectly reasonable constraint, but one that doesn't actually
 * constrain anything.  When we encounter a constraint like this, we need to
 * replace the result type by a free type that is scoped to the enclosing table.
 *
 * Conceptually, it's simplest to think of this constraint as one that is
 * tautological.  It does not actually contribute any new information.
 */
TEST_CASE_FIXTURE(Fixture, "handle_self_referential_HasProp_constraints")
{
    CheckResult result = check(R"(
        local function calculateTopBarHeight(props)
        end
        local function isTopPage(props)
            local topMostOpaquePage
            if props.avatarRoute then
                topMostOpaquePage = props.avatarRoute.opaque.name
            else
                topMostOpaquePage = props.opaquePage
            end
        end

        function TopBarContainer:updateTopBarHeight(prevProps, prevState)
            calculateTopBarHeight(self.props)
            isTopPage(self.props)
            local topMostOpaquePage
            if self.props.avatarRoute then
                topMostOpaquePage = self.props.avatarRoute.opaque.name
                --                  ^--------------------------------^
            else
                topMostOpaquePage = self.props.opaquePage
            end
        end
    )");
}

/* We had an issue where we were unifying two type packs
 *
 * free-2-0... and (string, free-4-0...)
 *
 * The correct thing to do here is to promote everything on the right side to
 * level 2-0 before binding the left pack to the right.  If we fail to do this,
 * then the code fragment here fails to typecheck because the argument and
 * return types of C are generalized before we ever get to checking the body of
 * C.
 */
TEST_CASE_FIXTURE(Fixture, "promote_tail_type_packs")
{
    CheckResult result = check(R"(
        --!strict

        local A: any = nil

        local C
        local D = A(
            A({}, {
                __call = function(a): string
                    local E: string = C(a)
                    return E
                end
            }),
            {
                F = function(s: typeof(C))
                end
            }
        )

        function C(b: any): string
            return ''
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "lti_must_record_contributing_locations")
{
    ScopedFastFlag sff_LuauSolverV2{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        local function f(a)
            if math.random() > 0.5 then
                math.abs(a)
            else
                string.len(a)
            end
        end
    )");

    // We inspect the actual errors in other tests; this test verifies that we
    // actually recorded breadcrumbs for a.
    LUAU_REQUIRE_ERROR_COUNT(3, result);
    TypeId fnTy = requireType("f");
    const FunctionType* fn = get<FunctionType>(fnTy);
    REQUIRE(fn);

    TypeId argTy = *first(fn->argTypes);
    std::vector<std::pair<Location, TypeId>> locations = getMainModule()->upperBoundContributors[argTy];
    CHECK(locations.size() == 2);
}

/*
 * CLI-49876
 *
 * We had a bug where we would not use the correct TxnLog when evaluating a
 * variadic overload. We could therefore get into a state where the TxnLog has
 * logged that a generic matches to one type, but the variadic tail has already
 * been bound to another type outside of that TxnLog.
 *
 * This caused type checking to succeed when it should have failed.
 */
TEST_CASE_FIXTURE(BuiltinsFixture, "be_sure_to_use_active_txnlog_when_evaluating_a_variadic_overload")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        local function concat<T>(target: {T}, ...: {T} | T): {T}
            return (nil :: any) :: {T}
        end

        local res = concat({"alic"}, 1, 2)
    )");

    LUAU_REQUIRE_ERRORS(result);

    for (const auto& e : result.errors)
        CHECK(5 == e.location.begin.line);
}

/*
 * We had an issue where this kind of typeof() call could produce the untestable type ~{}
 */
TEST_CASE_FIXTURE(Fixture, "typeof_cannot_refine_builtin_alias")
{
    GlobalTypes& globals = getFrontend().globals;
    TypeArena& arena = globals.globalTypes;

    unfreeze(arena);

    globals.globalScope->exportedTypeBindings["GlobalTable"] = TypeFun{{}, arena.addType(TableType{TableState::Sealed, TypeLevel{}})};

    freeze(arena);

    (void)check(R"(
        function foo(x)
            if typeof(x) == 'GlobalTable' then
            end
        end
    )");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "bad_iter_metamethod")
{
    CheckResult result = check(R"(
        function iter(): unknown
            return nil
        end

        local a = {__iter = iter}
        setmetatable(a, a)

        for i in a do
        end
    )");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);

        CannotCallNonFunction* ccnf = get<CannotCallNonFunction>(result.errors[0]);
        REQUIRE(ccnf);

        CHECK("unknown" == toString(ccnf->ty));
    }
    else
    {
        LUAU_REQUIRE_NO_ERRORS(result);
    }
}

TEST_CASE_FIXTURE(Fixture, "leading_bar")
{
    CheckResult result = check(R"(
        type Bar = | number
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("number" == toString(requireTypeAlias("Bar")));
}

TEST_CASE_FIXTURE(Fixture, "leading_bar_question_mark")
{
    CheckResult result = check(R"(
        type Bar = |?
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK("Expected type, got '?'" == toString(result.errors[0]));
    CHECK("*error-type*?" == toString(requireTypeAlias("Bar")));
}

TEST_CASE_FIXTURE(Fixture, "leading_ampersand")
{
    CheckResult result = check(R"(
        type Amp = & string
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("string" == toString(requireTypeAlias("Amp")));
}

TEST_CASE_FIXTURE(Fixture, "leading_bar_no_type")
{
    CheckResult result = check(R"(
        type Bar = |
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK("Expected type, got <eof>" == toString(result.errors[0]));
    CHECK("*error-type*" == toString(requireTypeAlias("Bar")));
}

TEST_CASE_FIXTURE(Fixture, "leading_ampersand_no_type")
{
    CheckResult result = check(R"(
        type Amp = &
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK("Expected type, got <eof>" == toString(result.errors[0]));
    CHECK("*error-type*" == toString(requireTypeAlias("Amp")));
}

TEST_CASE_FIXTURE(Fixture, "react_lua_follow_free_type_ub")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        return function(Roact)
            local Tree = Roact.Component:extend("Tree")

            function Tree:render()
                local breadth, components, depth, id, wrap =
                    self.props.breadth, self.props.components, self.props.depth, self.props.id, self.props.wrap
                local Box = components.Box
                if depth == 0 then
                    Roact.createElement(Box, {})
                else
                    Roact.createElement(Tree, {})
                end

            end
        end
    )"));
}

TEST_CASE_FIXTURE(Fixture, "visit_error_nodes_in_lvalue")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    // This should always fail to parse, but shouldn't assert. Previously this
    // would assert as we end up _roughly_ parsing this (with a lot of error
    // nodes) as:
    //
    //  do
    //      x :: T, y = z
    //  end
    //
    // We assume that `T` has some resolved type that is set up during
    // constraint generation and resolved during constraint solving to
    // be used during typechecking. We didn't descend into error nodes
    // in lvalue positions.
    LUAU_REQUIRE_ERRORS(check(R"(
        --!strict
        (::,
    )"));
}

TEST_CASE_FIXTURE(Fixture, "avoid_blocking_type_function")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    LUAU_CHECK_NO_ERRORS(check(R"(
        --!strict
        local function foo(a : string?)
            local b = a or ""
            return b:upper()
        end
    )"));
}

TEST_CASE_FIXTURE(Fixture, "avoid_double_reference_to_free_type")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    LUAU_CHECK_NO_ERRORS(check(R"(
        --!strict
        local function wtf(name: string?)
            local message
            message = "invalid alternate fiber: " .. (name or "UNNAMED alternate")
        end
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "infer_types_of_globals")
{
    ScopedFastFlag sff_LuauSolverV2{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        --!strict
        foo = 5
        print(foo)
    )");

    CHECK_EQ("number", toString(requireTypeAtPosition({3, 14})));

    REQUIRE_EQ(1, result.errors.size());
    CHECK_EQ("Unknown global 'foo'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "multiple_assignment")
{
    ScopedFastFlag sff_LuauSolverV2{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        local function requireString(arg: string) end
        local function requireNumber(arg: number) end

        local function f(): ...number end

        local w: "a", x, y, z = "a", 1, f()
        requireString(w)
        requireNumber(x)
        requireNumber(y)
        requireNumber(z)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "fuzz_global_self_assignment")
{
    // Shouldn't assert or crash
    check(R"(
        _ = _
    )");
}


TEST_CASE_FIXTURE(BuiltinsFixture, "getmetatable_works_with_any")
{
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        return {
            new = function(name: string)
                local self = newproxy(true) :: any

                getmetatable(self).__tostring = function()
                    return "Hello, I am " .. name
                end

                return self
            end,
        }
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "getmetatable_infer_any_ret")
{
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local function spooky(x: any)
            return getmetatable(x)
        end
    )"));

    CHECK_EQ("(any) -> any", toString(requireType("spooky")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "getmetatable_infer_any_param")
{
    auto result = check(R"(
        local function check(x): any
            return getmetatable(x)
        end
    )");

    if (FFlag::LuauSolverV2)
        CHECK_EQ("(unknown) -> any", toString(requireType("check")));
    else
        CHECK_EQ("({ @metatable any, {+  +} }) -> any", toString(requireType("check")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzzer_pack_check_missing_follow")
{
    // Shouldn't assert or crash
    check(R"(
_ = n255
function _()
setmetatable(_)[_[xpcall(_,setmetatable(_,_()))]] /= xpcall(_,_)
_.n16(_,_)[_[_]] *= _
end
    )");
}

TEST_CASE_FIXTURE(Fixture, "fuzzer_unify_with_free_missing_follow")
{
    // Shouldn't assert or crash
    check(R"(
for _ in ... do
repeat
local function l0(l0)
end
_ = l0["aaaa"]
repeat
_ = true,_("")
_ = _[_]
until _
until _
repeat
_ = if _ then _,_()
_ = _[_]
until _
end
    )");
}

TEST_CASE_FIXTURE(Fixture, "fuzzer_derived_unsound_loops")
{
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        for _ in ... do
            repeat
                _ = 42
            until _
            repeat
                _ = _ + 2
            until _
        end
    )"));
}

TEST_CASE_FIXTURE(Fixture, "concat_string_with_string_union")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local function concat_stuff(x: string, y : string | number)
            return x .. y
        end
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzz_local_before_declaration_ice")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        local _
        table.freeze(_, _)
    )");
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    auto err0 = get<TypeMismatch>(result.errors[0]);
    CHECK(err0);
    CHECK_EQ("nil", toString(err0->givenType));
    CHECK_EQ("table", toString(err0->wantedType));
    auto err1 = get<CountMismatch>(result.errors[1]);
    CHECK(err1);
    CHECK_EQ(1, err1->expected);
    CHECK_EQ(2, err1->actual);
}

TEST_CASE_FIXTURE(Fixture, "fuzz_dont_double_solve_compound_assignment" * doctest::timeout(1.0))
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        local _ = {}
        _[function<t0...>(...)
            _[function(...)
                _[_] %= _
                _ = {}
                _ = (- _)()
            end] %= _
            _[_] %= _
        end] %= true
    )");

    LUAU_REQUIRE_ERRORS(result);
    LUAU_REQUIRE_NO_ERROR(result, ConstraintSolvingIncompleteError);
}

TEST_CASE_FIXTURE(Fixture, "assert_allows_singleton_union_or_intersection")
{
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local x = 42 :: | number
        local y = 42 :: & number
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "assert_table_freeze_constraint_solving")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local f = table.freeze
        f(table)
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzz_assert_table_freeze_constraint_solving")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};
    // This is the original fuzzer version of the above issue.
    CheckResult results = check(R"(
        local function l0()
        end
        for l0 in false do
        _ = (if _ then table)
        repeat
        do end
        _:freeze(table)
        until if _ then {{n0=_,},(_:freeze()._[_]),}
        end
    )");
    LUAU_REQUIRE_ERRORS(results);
    LUAU_REQUIRE_NO_ERROR(results, ConstraintSolvingIncompleteError);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cyclic_unification_aborts_eventually" * doctest::timeout(0.25))
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, false},
        {FFlag::LuauInstantiateInSubtyping, true},
    };

    ScopedFastInt sfi{FInt::LuauTypeInferTypePackLoopLimit, 100};

    CheckResult result = check(R"(pcall(table.unpack({pcall})))");

    LUAU_REQUIRE_ERROR(result, CodeTooComplex);
}

TEST_CASE_FIXTURE(Fixture, "fuzz_generalize_one_remove_type_assert")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
    };

    auto result = check(R"(
        local _ = {_ = _}, l0
        _ += _
        while _ do
            while _[_] do
                if _.n0 then
                    _ = _
                else
                    _ = _
                    return _
                end
                do
                    while _ do
                        _, _ = nil
                    end
                    return function()
                    end
                end
                while _[_] do
                    _ = _._VERSION, ""
                end
            end
            local _
        end
    )");
    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "fuzz_generalize_one_remove_type_assert_2")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
    };

    CheckResult result = check(R"(
        local _ = {n0 = _.n0}, -_, _
        _ += _.n0
        _ /= _[_]
        while _.n110 do
            while _._ do
                while _ do
                    while _ do
                        _ = _
                    end
                end
                while _[_] do
                    function _()
                    end
                end
            end
            while ... do
            end
        end
    )");
    LUAU_REQUIRE_ERRORS(result);
    LUAU_REQUIRE_NO_ERROR(result, ConstraintSolvingIncompleteError);
}

#if 0

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzz_simplify_combinatorial_explosion")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
    };

    LUAU_REQUIRE_ERRORS(check(R"(
_ = {[_[`{_ + ...}`]]=_,_,[{_=nil,[_._G]=false,}]={[_[_[_]][_][_ / ...]]=_,[...]=false,_,},[_[_][_][_]]=l255,},""
local _
    )"));

    LUAU_REQUIRE_ERRORS(check(R"(
_ = {[(_G)]=_,[_[_[_]][_[_]][nil][_]]={_G=_,},_[_[_]][_][_],n0={[_]=_,_G=_,},248,}
local _
    )"));
}

#endif

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzz_missing_follow_table_freeze")
{
    LUAU_REQUIRE_ERRORS(check(R"(
        if _:freeze(_)[_][_] then
        else
        do end
        end
        if _:freeze((nil))[_][_] then
        else
        do end
        end
        _ = table,true,_(lower)
        do end
        _:freeze()[_] += {} > _
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzzer_avoid_double_negation" * doctest::timeout(0.5))
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    // We don't care about errors, only that we don't OOM during typechecking.
    LUAU_REQUIRE_ERRORS(check(R"(
local _ = _
repeat
do end
while 0 do
do
_ = _[0]
_._ *= _
end
if _ then
elseif "" then
end
_ = _[0]
_ = ""
end
_ = ""
until _
while false do
do
_ = _[0]
do end
end
_ = ""
return if _ then _,_
end
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzzer_has_indexer_can_create_cyclic_union")
{
    LUAU_REQUIRE_ERRORS(check(R"(
        local _ = nil
        repeat
            _ = {[true] = _[_]}
            do
                repeat
                    _ = {[_[l0]] = _[_]}
                    return
                until #next(_) < _
            end
            local l0 = require(module0)
        until #_[_](_) < next(_)
    )"));
}

TEST_CASE_FIXTURE(Fixture, "fuzzer_simplify_table_indexer" * doctest::timeout(0.5))
{
    LUAU_REQUIRE_ERRORS(check(R"(
        _[_] += true
        _ = {
            [{
                [_] = _[_][if ... then _ else _](),
                [-1795162112] = function()
                end,
                [{
                    _G = function()
                    end
                }] = _(_(true)),
                _G = _
            }] = _,
            [_[not _][_]] = _(),
            _
        }

    )"));
}

TEST_CASE_FIXTURE(Fixture, "fuzzer_simplify_crash")
{
    LUAU_REQUIRE_ERRORS(check(R"(
        if _ then
            _ = nil
        else if _ and _ then
            _ = nil
        end
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzzer_simplify_is_check_on_bound_type")
{
    LUAU_REQUIRE_ERRORS(check(R"(
        _[if _ then false],_,_._,log10 = {{[_]={_,},_G=not function():true
        _ = nil
        end,},[_[_ + true][_][_]]=_,sort=_,},_
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "regexp_hang" * doctest::timeout(0.5))
{
    LUAU_REQUIRE_ERRORS(check(R"(
local outln, group_id, verb_flags = {}, {}, {
    newline = 1,
    newline_seq = 1,
    not_empty = 0
}
if not escape_c then
elseif escape_c >= 48 and escape_c <= 57 then
elseif escape_c == 69 then
elseif escape_c == 81 then
elseif escape_c == 78 then
    if codes[i] ~= 125 or i == start_i then
    end
    table.insert(outln, code_point)
elseif escape_c == 80 or escape_c == 112 then
    if script_set then
    elseif not valid_categories[c_name]then
    else
        table.insert(outln, { 'category', negate, c_name })
    end
elseif escape_c == 103 and (codes[i + 1] == 123 or codes[i + 1] >= 48 and codes[i + 1] <= 57)then
elseif escape_c == 111 then
elseif escape_c == 120 then
else
    table.insert(outln, esc_char or escape_c)
end

for i, v in ipairs(outln)do
    if type(v) == 'table' and (v[1] == 40 or v[1] == 'quantifier' and type(v[5]) == 'table' and v[5][1] == 40)then
        v = v[5]
    elseif type(v) == 'table' and (v[1] == 'backref' or v[1] == 'recurmatch')then
        for i1, v1 in ipairs(outln)do
            break
        end
    end
end
    )"));
}

TEST_CASE_FIXTURE(Fixture, "self_bound_due_to_compound_assign")
{
    loadDefinition(R"(
        declare class Camera
            CameraType: string
            CFrame: number
        end
    )");

    CheckResult result = check(R"(
        --!strict
        function MT_UPDATE(CAMERA: Camera, Enum: any, totalOffsets: number, focusToCFrame: number, magnitude: number)
            if CAMERA.CameraType ~= Enum.CameraType.Custom then
                return
            end

            local goalCFrame = (CAMERA.CFrame) * totalOffsets
            if goalCFrame ~= CAMERA.CFrame then
                goalCFrame -= (focusToCFrame * magnitude) -- Offset the goalCFrame the raycast direction based on the cutoff distance.
            end
        end

        return {}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "config_reader_example")
{
    // If this flag isn't set, then we do not do the requisite setup when the
    // test suite starts, which will cause an assert if we try to eagerly
    // generalize _after_ the test is set up. Additionally, this code block
    // crashes under the new solver without flags.
    if (!FFlag::LuauSolverV2)
        return;

    fileResolver.source["game/ConfigReader"] = R"(
        --!strict
        local ConfigReader = {}
        ConfigReader.Defaults = {}

        local Defaults = ConfigReader.Defaults
        local Config = ConfigReader.Defaults

        function ConfigReader:read(config_name: string)
            if Config[config_name] ~= nil then
                return Config[config_name]
            elseif Defaults[config_name] ~= nil then
                return Defaults[config_name]
            else
                error(config_name .. " must be defined in Config")
            end
        end


        function ConfigReader:getFullConfigWithDefaults()
            local config = {}
            for key, val in pairs(ConfigReader.Defaults) do
                config[key] = val
            end
            for key, val in pairs(Config) do
                config[key] = val
            end
            return config
        end

        return ConfigReader
    )";

    fileResolver.source["game/Util"] = R"(
        --!strict
        local ConfigReader = require(script.Parent.ConfigReader)
        local _ = ConfigReader:read("foobar")()
    )";

    LUAU_REQUIRE_ERRORS(getFrontend().check("game/Util"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "is_safe_integer_example")
{
    fileResolver.source["game/isInteger"] = R"(
        --!strict
        return function(value)
            return type(value) == "number" and value ~= math.huge and value == math.floor(value)
        end
    )";

    fileResolver.source["game/MAX_SAFE_INTEGER"] = R"(
        --!strict
        return 42
    )";

    fileResolver.source["game/Util"] = R"(
        --!strict
        local isInteger = require(script.Parent.isInteger)
        local MAX_SAFE_INTEGER = require(script.Parent.MAX_SAFE_INTEGER)
        return function(value)
        	return isInteger(value) and math.abs(value) <= MAX_SAFE_INTEGER
        end
    )";

    LUAU_REQUIRE_NO_ERRORS(getFrontend().check("game/Util"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_remover_heap_use_after_free")
{
    LUAU_REQUIRE_ERRORS(check(R"(
        _ = if l0.n0.n0 then {n4(...,setmetatable(setmetatable(_),_)),_ == _,} elseif _.ceil._ then _ elseif _ then not _
    )"));

    LUAU_REQUIRE_ERRORS(check(R"(
        do
        _ = if _[_] then {[_(``)]="y",} elseif _ then _ elseif _[_] then "" elseif _ then _ elseif _[_] then {} elseif _[_] then false else ""
        end
    )"));

    LUAU_REQUIRE_ERRORS(check(R"(
        local l249 = require(module0)
        _,_ = {[`{_}`]=_,[_._G._]=(_)(),[_["" + _]._G]={_=_,_=_,[_._G[_]._]=_G,},},_,(_)()
    )"));
}

TEST_CASE_FIXTURE(Fixture, "fuzzer_missing_follow_in_assign_index_constraint")
{
    LUAU_REQUIRE_ERRORS(check(R"(
        _._G = nil
        for _ in ... do
        break
        end
        for _ in function<t0,t0,t0>(l0)
        _,_._,l0 = l0,_,_._
        local _ = l0,{[_]=_,}
        _[{nil=_,}](_)
        end,{[_]=_,} do
        end
        _ -= _
    )"));
}

TEST_CASE_FIXTURE(Fixture, "fuzzer_occurs_check_stack_overflow")
{
    // We just want this to not stack overflow, it's ok for it to barf errors.
    LUAU_REQUIRE_ERRORS(check(R"(
        _ = if _ then _
        for l0 in ... do
        type t0 = (()->((t0<t0...>)->())|(any))|(typeof(_))
        end
    )"));
}

TEST_CASE_FIXTURE(Fixture, "fuzzer_infer_divergent_rw_props")
{
    ScopedFastFlag sffs{FFlag::LuauSolverV2, true};

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        return function(l0:{_:(any)&(any),write _:any,})
        end
    )"));
}

TEST_CASE_FIXTURE(Fixture, "read_table_type_refinements_persist_scope")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    LUAU_REQUIRE_ERRORS(check(R"(
_ = {n0=_,},if _._ then ... else if _[if _ then _ else ({nil,})].setmetatable then if _ then _ elseif l0 then ... elseif _.n0 then _ elseif function<A>(l0)
return _._G,_
end then _._G else ...
    )"));
}

TEST_CASE_FIXTURE(Fixture, "oss_1815_verbatim")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauInferActualIfElseExprType2, true},
    };

    CheckResult results = check(R"(
        --!strict
        local item: "foo" = "bar"
        item = if true then "foo" else "foo"

        local item2: "foo" = if true then "doge" else "doge2"
    )");
    LUAU_REQUIRE_ERROR_COUNT(3, results);
    CHECK_EQ(results.errors[0].location, Location{{2, 28}, {2, 33}});
    auto err1 = get<TypeMismatch>(results.errors[0]);
    REQUIRE(err1);
    CHECK_EQ("\"foo\"", toString(err1->wantedType));
    CHECK_EQ("\"bar\"", toString(err1->givenType));
    CHECK_EQ(results.errors[1].location, Location{{5, 42}, {5, 48}});
    auto err2 = get<TypeMismatch>(results.errors[1]);
    REQUIRE(err2);
    CHECK_EQ("\"foo\"", toString(err2->wantedType));
    CHECK_EQ("\"doge\"", toString(err2->givenType));
    CHECK_EQ(results.errors[2].location, Location{{5, 54}, {5, 61}});
    auto err3 = get<TypeMismatch>(results.errors[2]);
    REQUIRE(err3);
    CHECK_EQ("\"foo\"", toString(err3->wantedType));
    CHECK_EQ("\"doge2\"", toString(err3->givenType));
}

TEST_CASE_FIXTURE(Fixture, "if_then_else_bidirectional_inference")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauInferActualIfElseExprType2, true},
    };

    CheckResult results = check(R"(
        type foo = {
            bar: (() -> string)?,
        }
        local qux: foo = if false then {} else 10
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, results);
    auto err = get<TypeMismatch>(results.errors[0]);
    REQUIRE(err);
    CHECK_EQ("number", toString(err->givenType));
    CHECK_EQ("foo", toString(err->wantedType));
}

TEST_CASE_FIXTURE(Fixture, "if_then_else_two_errors")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauInferActualIfElseExprType2, true},
    };

    CheckResult results = check(R"(
        type foo = {
            bar: () -> string,
        }
        local qux: foo = if false then {} else 10
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, results);
    auto err1 = get<MissingProperties>(results.errors[0]);
    REQUIRE(err1);
    CHECK_EQ("foo", toString(err1->superType));
    CHECK_EQ("{  }", toString(err1->subType));
    auto err2 = get<TypeMismatch>(results.errors[1]);
    REQUIRE(err2);
    CHECK_EQ("foo", toString(err2->wantedType));
    CHECK_EQ("number", toString(err2->givenType));
}

TEST_CASE_FIXTURE(Fixture, "standalone_constraint_solving_incomplete_is_hidden")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::DebugLuauMagicTypes, true},
        // This debug flag is normally on, but we turn it off as we're testing
        // the exact behavior it enables.
        {FFlag::DebugLuauAlwaysShowConstraintSolvingIncomplete, false},
    };

    CheckResult results = check(R"(
        local function _f(_x: _luau_force_constraint_solving_incomplete) end
    )");

    LUAU_REQUIRE_NO_ERRORS(results);
}

TEST_CASE_FIXTURE(Fixture, "non_standalone_constraint_solving_incomplete_is_hidden")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::DebugLuauMagicTypes, true},
    };

    CheckResult results = check(R"(
        local function _f(_x: _luau_force_constraint_solving_incomplete) end
        local x: number = true
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, results);
    CHECK(get<ConstraintSolvingIncompleteError>(results.errors[0]));
    CHECK(get<TypeMismatch>(results.errors[1]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzzer_missing_type_pack_follow")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauReturnMappedGenericPacksFromSubtyping3, true},
    };

    LUAU_REQUIRE_ERRORS(check(R"(
local _ = {[0]=_,}
while _ do
do
local l2 = require(module0)
end
end
do end
function _(l0:typeof(_),l0,l0)
local l0 = require(module0)
_()(l0(),_,_(_())((_)))
do end
end
_()(_(if nil then _))("",_,_(_,(_)))
do end
    )"));

    LUAU_REQUIRE_ERRORS(check(R"(
local _ = {_,}
while _ do
do
do end
end
end
_ = nil
function _(l0,l0,l0)
local l0 = require(module0)
_()(_(),_,_(_())(_,true)(_,_),l0)
do end
end
_()(_())("",_.n0,_,_(_,true,(_)))
do end
    )"));
}

#if 0 // CLI-166473: re-enable after flakiness is resolved
TEST_CASE_FIXTURE(Fixture, "txnlog_checks_for_occurrence_before_self_binding_a_type")
{
    ScopedFastFlag sff[] = {{FFlag::LuauSolverV2, false}, {FFlag::LuauOccursCheckInCommit, true}};


    CheckResult result = check(R"(
        local any = nil :: any

        function f1(x)
            x:m()
            local _ = x.A.p.a
        end

        function f2(x)
            local _ = x.d
        end

        function f3(x)
            local a = ""
            a = x.d.p
            local _ = undef[x.a]
        end

        function f4(x)
            f2(x)
            if undef and x and x:m() then
                any(x)
                return
            end
            f3(x)
            for _, v in any.x do
                local a = x[v].p
            end
            a.b = x
            if x.q ~= nil then
                f1(x) -- things go bad here
            end
        end

        return f4
    )");
}
#endif

TEST_CASE_FIXTURE(Fixture, "constraint_generation_recursion_limit")
{
    ScopedFastFlag sffs[] = {{FFlag::LuauSolverV2, true}, {FFlag::LuauNoConstraintGenRecursionLimitIce, true}};
    // Lowers the recursion limit for the constraint generator
    ScopedFastInt i{FInt::LuauCheckRecursionLimit, 5};
    ScopedFastInt luauConstraintGeneratorRecursionLimit{DFInt::LuauConstraintGeneratorRecursionLimit, 5};

    // This shouldn't ICE
    CheckResult result = check(R"(
        if true then
        elseif true then
        elseif true then
        elseif true then
        else
        local x = 1
        end
    )");
}

// https://github.com/luau-lang/luau/issues/1971
TEST_CASE_FIXTURE(Fixture, "nested_functions_can_depend_on_outer_generics")
{
    ScopedFastFlag sff{FFlag::LuauEGFixGenericsList, true};

    CheckResult result = check(R"(
        function name<P>(arg1: P)
            return function(what: P) return what end
        end

        local funcTest = name(nil)
        local out = funcTest(1) -- Doesn't report type mismatch error anymore
    )");

    CHECK("(nil) -> nil" == toString(requireType("funcTest")));

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    auto tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE_MESSAGE(tm, "Expected TypeMismatch but got " << result.errors[0]);

    CHECK("nil" == toString(tm->wantedType));
    CHECK("number" == toString(tm->givenType));
}

TEST_CASE_FIXTURE(Fixture, "avoid_unification_inferring_never_for_refined_param")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauTryToOptimizeSetTypeUnification, true},
    };

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local function __remove(__: number?) end

        function __removeItem(self, itemId: number)
            local index = self.getItem(itemId)
            if index then
               __remove(index)
            end
        end
    )"));

    CHECK_EQ("({ read getItem: (number) -> (number?, ...unknown) }, number) -> ()", toString(requireType("__removeItem")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "unterminated_function_body_causes_constraint_generator_crash")
{
    ScopedFastFlag _{FFlag::LuauDontReferenceScopePtrFromHashTable, true};
    // This should not crash
    CheckResult result = check(R"(
export type t = {
	func : typeof(
		function
	)
}

export type t1 = t12

export type t2 = {}

export type t3 = {
	foo:number
	bar:number
}

export type t4 = "foobar"

export type t5 = string

export type t6 = number

export type t7 = "foobar"

export type t8 = "foobar"

export type t9 = typeof(1)

export type t10 = typeof(1)

export type t11 = typeof(1)

export type t12 = {
	b:number
	pb:number
}
)");
}

TEST_SUITE_END();
