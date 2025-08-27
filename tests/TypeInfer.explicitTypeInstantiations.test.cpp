#include "Fixture.h"

#include "ScopedFlags.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauExplicitTypeExpressionInstantiation)

TEST_SUITE_BEGIN("TypeInferExplicitTypeInstantiations");

#define SUBCASE_BOTH_SOLVERS() \
    for (bool enabled : { false, true }) \
        if (ScopedFastFlag sffSolver {FFlag::LuauSolverV2, enabled}; true) \
            SUBCASE(enabled ? "New solver" : "Old solver")

TEST_CASE_FIXTURE(Fixture, "as_expression_correct")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local function f<T>(): T
            return nil :: any
        end

        local correct = f<<number>>() + 5
        )");

        LUAU_REQUIRE_NO_ERRORS(result);
    }
}

TEST_CASE_FIXTURE(Fixture, "as_expression_incorrect")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local function f<T>(): T
            return nil :: any
        end

        local incorrect = f<<string>>() + 5
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
    }
}

TEST_CASE_FIXTURE(Fixture, "as_stmt_correct")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local function f<T>(a: T, b: T)
            return nil :: any
        end

        f<<number | string>>(1, "a")
        )");

        LUAU_REQUIRE_NO_ERRORS(result);
    }
}

TEST_CASE_FIXTURE(Fixture, "as_stmt_incorrect")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local function f<T>(a: T, b: T)
            return nil :: any
        end

        f<<number | boolean>>(1, "a")
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
    }
}

TEST_SUITE_END();
