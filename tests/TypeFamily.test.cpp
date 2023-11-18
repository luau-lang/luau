// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeFamily.h"

#include "Luau/ConstraintSolver.h"
#include "Luau/NotNull.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

struct FamilyFixture : Fixture
{
    TypeFamily swapFamily;

    FamilyFixture()
        : Fixture(true, false)
    {
        swapFamily = TypeFamily{/* name */ "Swap",
            /* reducer */
            [](std::vector<TypeId> tys, std::vector<TypePackId> tps, NotNull<TypeFamilyContext> ctx) -> TypeFamilyReductionResult<TypeId> {
                LUAU_ASSERT(tys.size() == 1);
                TypeId param = follow(tys.at(0));

                if (isString(param))
                {
                    return TypeFamilyReductionResult<TypeId>{ctx->builtins->numberType, false, {}, {}};
                }
                else if (isNumber(param))
                {
                    return TypeFamilyReductionResult<TypeId>{ctx->builtins->stringType, false, {}, {}};
                }
                else if (is<BlockedType>(param) || is<PendingExpansionType>(param) || is<TypeFamilyInstanceType>(param) ||
                         (ctx->solver && ctx->solver->hasUnresolvedConstraints(param)))
                {
                    return TypeFamilyReductionResult<TypeId>{std::nullopt, false, {param}, {}};
                }
                else
                {
                    return TypeFamilyReductionResult<TypeId>{std::nullopt, true, {}, {}};
                }
            }};

        unfreeze(frontend.globals.globalTypes);
        TypeId t = frontend.globals.globalTypes.addType(GenericType{"T"});
        GenericTypeDefinition genericT{t};

        ScopePtr globalScope = frontend.globals.globalScope;
        globalScope->exportedTypeBindings["Swap"] =
            TypeFun{{genericT}, frontend.globals.globalTypes.addType(TypeFamilyInstanceType{NotNull{&swapFamily}, {t}, {}})};
        freeze(frontend.globals.globalTypes);
    }
};

TEST_SUITE_BEGIN("TypeFamilyTests");

TEST_CASE_FIXTURE(FamilyFixture, "basic_type_family")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        type A = Swap<number>
        type B = Swap<string>
        type C = Swap<boolean>

        local x = 123
        local y: Swap<typeof(x)> = "foo"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK("string" == toString(requireTypeAlias("A")));
    CHECK("number" == toString(requireTypeAlias("B")));
    CHECK("Swap<boolean>" == toString(requireTypeAlias("C")));
    CHECK("string" == toString(requireType("y")));
    CHECK("Type family instance Swap<boolean> is uninhabited" == toString(result.errors[0]));
};

TEST_CASE_FIXTURE(FamilyFixture, "family_as_fn_ret")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local swapper: <T>(T) -> Swap<T>
        local a = swapper(123)
        local b = swapper("foo")
        local c = swapper(false)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK("string" == toString(requireType("a")));
    CHECK("number" == toString(requireType("b")));
    CHECK("Swap<boolean>" == toString(requireType("c")));
    CHECK("Type family instance Swap<boolean> is uninhabited" == toString(result.errors[0]));
}

TEST_CASE_FIXTURE(FamilyFixture, "family_as_fn_arg")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local swapper: <T>(Swap<T>) -> T
        local a = swapper(123)
        local b = swapper(false)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    // FIXME: Can we constrain these to `never` or `unknown`?
    CHECK("a" == toString(requireType("a")));
    CHECK("a" == toString(requireType("b")));
    CHECK("Type family instance Swap<a> is uninhabited" == toString(result.errors[0]));
    CHECK("Type family instance Swap<a> is uninhabited" == toString(result.errors[1]));
}

TEST_CASE_FIXTURE(FamilyFixture, "resolve_deep_families")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local x: Swap<Swap<Swap<string>>>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("number" == toString(requireType("x")));
}

TEST_CASE_FIXTURE(FamilyFixture, "unsolvable_family")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local impossible: <T>(Swap<T>) -> Swap<Swap<T>>
        local a = impossible(123)
        local b = impossible(true)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    for (size_t i = 0; i < 2; ++i)
    {
        CHECK(toString(result.errors[i]) == "Type family instance Swap<a> is uninhabited");
    }
}

TEST_CASE_FIXTURE(FamilyFixture, "table_internal_families")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local t: <T>({T}) -> {Swap<T>}
        local a = t({1, 2, 3})
        local b = t({"a", "b", "c"})
        local c = t({true, false, true})
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(requireType("a")) == "{string}");
    CHECK(toString(requireType("b")) == "{number}");
    CHECK(toString(requireType("c")) == "{Swap<boolean>}");
    CHECK(toString(result.errors[0]) == "Type family instance Swap<boolean> is uninhabited");
}

TEST_CASE_FIXTURE(FamilyFixture, "function_internal_families")
{
    // This test is broken right now, but it's not because of type families. See
    // CLI-71143.

    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local f0: <T>(T) -> (() -> T)
        local f: <T>(T) -> (() -> Swap<T>)
        local a = f(1)
        local b = f("a")
        local c = f(true)
        local d = f0(1)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(requireType("a")) == "() -> string");
    CHECK(toString(requireType("b")) == "() -> number");
    CHECK(toString(requireType("c")) == "() -> Swap<boolean>");
    CHECK(toString(result.errors[0]) == "Type family instance Swap<boolean> is uninhabited");
}

TEST_CASE_FIXTURE(Fixture, "add_family_at_work")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local function add(a, b)
            return a + b
        end

        local a = add(1, 2)
        local b = add(1, "foo")
        local c = add("foo", 1)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(toString(requireType("a")) == "number");
    CHECK(toString(requireType("b")) == "Add<number, string>");
    CHECK(toString(requireType("c")) == "Add<string, number>");
    CHECK(toString(result.errors[0]) == "Type family instance Add<number, string> is uninhabited");
    CHECK(toString(result.errors[1]) == "Type family instance Add<string, number> is uninhabited");
}

TEST_CASE_FIXTURE(Fixture, "internal_families_raise_errors")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local function innerSum(a, b)
            local _ = a + b
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Type family instance Add<a, b> depends on generic function parameters but does not appear in the function "
                                        "signature; this construct cannot be type-checked at this time");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_families_inhabited_with_normalization")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local useGridConfig : any
        local columns = useGridConfig("columns", {}) or 1
        local gutter = useGridConfig('gutter', {}) or 0
        local margin = useGridConfig('margin', {}) or 0
        return function(frameAbsoluteWidth: number)
            local cellAbsoluteWidth = (frameAbsoluteWidth - 2 * margin + gutter) / columns - gutter
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
