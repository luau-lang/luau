#include "Fixture.h"

#include "ScopedFlags.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauExplicitTypeExpressionInstantiation)
LUAU_FASTFLAG(LuauBetterTypeMismatchErrors)

TEST_SUITE_BEGIN("TypeInferExplicitTypeInstantiations");

#define SUBCASE_BOTH_SOLVERS() \
    for (bool enabled : {true, false}) \
        if (ScopedFastFlag sffSolver{FFlag::LuauSolverV2, enabled}; true) \
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

        if (FFlag::LuauSolverV2)
        {
            REQUIRE_EQ(
                toString(result.errors[0]),
                "Operator '+' could not be applied to operands of types string and number; there is no corresponding overload for __add"
            );
        }
        else if (FFlag::LuauBetterTypeMismatchErrors)
        {
            REQUIRE_EQ(toString(result.errors[0]), "Expected this to be 'number', but got 'string'");
        }
        else
        {
            REQUIRE_EQ(toString(result.errors[0]), "Type 'string' could not be converted into 'number'");
        }
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
        if (FFlag::LuauSolverV2)
        {
            if (FFlag::LuauBetterTypeMismatchErrors)
                REQUIRE_EQ(toString(result.errors[0]), "Expected this to be 'boolean | number', but got 'string'");
            else
                REQUIRE_EQ(toString(result.errors[0]), "Type 'string' could not be converted into 'boolean | number'");
        }
        else
        {
            if (FFlag::LuauBetterTypeMismatchErrors)
                REQUIRE_EQ(
                    toString(result.errors[0]), "Expected this to be 'boolean | number', but got 'string'; none of the union options are compatible"
                );
            else
                REQUIRE_EQ(
                    toString(result.errors[0]), "Type 'string' could not be converted into 'boolean | number'; none of the union options are compatible"
                );
        }
    }
}

TEST_CASE_FIXTURE(Fixture, "multiple_calls")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local function f<T>(): T
            return nil :: any
        end

        local a: number = f<<number>>()
        local b: string = f<<string>>()
        )");

        LUAU_REQUIRE_NO_ERRORS(result);
    }
}

TEST_CASE_FIXTURE(Fixture, "anonymous_type_inferred")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local function f<T, U>(): { a: T, b: U }
            return nil :: any
        end

        local correct: { a: number, b: string } = f<<number>>()
        local incorrect: { a: number, b: string } = f<<string>>()
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);

        REQUIRE_EQ(result.errors[0].location.begin.line, 7);
        LUAU_REQUIRE_ERROR(result, TypeMismatch);
    }
}

TEST_CASE_FIXTURE(Fixture, "type_packs")
{
    ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

    // FIXME: This triggers a GenericTypePackCountMismatch error, and it's not obvious if the
    // code for explicit types is broken, or if subtyping is broken.
    ScopedFastFlag oldSolver{FFlag::LuauSolverV2, false};

    CheckResult result = check(R"(
    --!strict
    local function f<T..., U...>(...: T...): U... end

    local a: number, b: string = f<<(boolean, {}), (number, string)>>(true, {})
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "type_packs_method")
{
    ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

    // FIXME: This triggers a GenericTypePackCountMismatch error, and it's not obvious if the
    // code for explicit types is broken, or if subtyping is broken.
    ScopedFastFlag oldSolver{FFlag::LuauSolverV2, false};

    CheckResult result = check(R"(
    --!strict
    local t: {
        f: <T..., U...>(self: any, T...) -> U...,
    } = nil :: any

    local a: number, b: string = t:f<<(boolean, {}), (number, string)>>(true, {})
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "type_packs_incorrect")
{
    ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

    // FIXME: This triggers a GenericTypePackCountMismatch error, and it's not obvious if the
    // code for explicit types is broken, or if subtyping is broken.
    ScopedFastFlag oldSolver{FFlag::LuauSolverV2, false};

    CheckResult result = check(R"(
    --!strict
    local function f<T..., U...>(...: T...): U... end

    local a: number, b: string = f<<(boolean, {}), (number, string)>>(true, "uh oh")
    )");

    LUAU_REQUIRE_ERROR(result, TypeMismatch);
}

TEST_CASE_FIXTURE(Fixture, "type_packs_incorrect_method")
{
    ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

    // FIXME: This triggers a GenericTypePackCountMismatch error, and it's not obvious if the
    // code for explicit types is broken, or if subtyping is broken.
    ScopedFastFlag oldSolver{FFlag::LuauSolverV2, false};

    CheckResult result = check(R"(
    --!strict
    local t: {
        f: <T..., U...>(self: any, T...) -> U...,
    } = nil :: any

    local a: number, b: string = t:f<<(boolean, {}), (number, string)>>(true, "uh oh")
    )");

    LUAU_REQUIRE_ERROR(result, TypeMismatch);
}

TEST_CASE_FIXTURE(Fixture, "dot_index_call")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local t = {
            f = function<T>(): T
                return nil :: any
            end,
        }

        local correct: number = t.f<<number>>()
        local incorrect: number = t.f<<string>>()
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        REQUIRE_EQ(result.errors[0].location.begin.line, 9);
    }
}

TEST_CASE_FIXTURE(Fixture, "method_index_call")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local t = {
            f = function<T>(self: any): T
                return nil :: any
            end,
        }

        local correct: number = t:f<<number>>()
        local incorrect: number = t:f<<string>>()
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_REQUIRE_ERROR(result, TypeMismatch);
        REQUIRE_EQ(result.errors[0].location.begin.line, 9);
    }
}

TEST_CASE_FIXTURE(Fixture, "stored_as_variable")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local function f<T>(): T
            return nil :: any
        end

        local fNumber = f<<number>>

        local correct: number = fNumber()
        local incorrect: string = fNumber()
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_REQUIRE_ERROR(result, TypeMismatch);
        REQUIRE_EQ(result.errors[0].location.begin.line, 9);
    }
}

TEST_CASE_FIXTURE(Fixture, "not_a_function")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local oops = 3
        local stub = oops<<number>>
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_REQUIRE_ERROR(result, InstantiateGenericsOnNonFunction);

        REQUIRE_EQ(toString(result.errors[0]), "Cannot instantiate type parameters on something without type parameters.");
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable_call")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local t = setmetatable({}, {
            __call = function<T>(self): T
                return nil :: any
            end,
        })

        t<<number>>()
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_REQUIRE_ERROR(result, InstantiateGenericsOnNonFunction);
        REQUIRE_EQ(toString(result.errors[0]), "Luau does not currently support explicitly instantiating a table with a `__call` metamethod. \
                You may be able to work around this by creating a function that calls the table, and using that instead.");
    }
}

TEST_CASE_FIXTURE(Fixture, "method_call_incomplete")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local t = {
            f = function<T, U>(self: any): T | U
                return nil :: any
            end,
        }

        local correct: number | string = t:f<<number>>()
        local incorrect: number | string = t:f<<boolean>>()
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_REQUIRE_ERROR(result, TypeMismatch);
        REQUIRE_EQ(result.errors[0].location.begin.line, 9);
    }
}

TEST_CASE_FIXTURE(Fixture, "too_many_provided")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local function f<T>() end

        f<<number, string>>()
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_REQUIRE_ERROR(result, TypeInstantiationCountMismatch);

        if (FFlag::LuauSolverV2)
        {
            REQUIRE_EQ(
                toString(result.errors[0]),
                "Too many type parameters passed to 'f', which is typed as <T>(...any) -> (). Expected at most 1 type parameter, but 2 provided."
            );
        }
        else
        {
            REQUIRE_EQ(
                toString(result.errors[0]),
                "Too many type parameters passed to 'f', which is typed as <T>() -> (). Expected at most 1 type parameter, but 2 provided."
            );
        }
    }
}

TEST_CASE_FIXTURE(Fixture, "too_many_provided_type_packs")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local function f<T...>(): (T...) end

        f<<(string, number), (true, false)>>()
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_REQUIRE_ERROR(result, TypeInstantiationCountMismatch);

        if (FFlag::LuauSolverV2)
        {
            REQUIRE_EQ(
                toString(result.errors[0]),
                "Too many type parameters passed to 'f', which is typed as <T...>(...any) -> (T...). Expected at most 1 type pack, but 2 provided."
            );
        }
        else
        {
            REQUIRE_EQ(
                toString(result.errors[0]),
                "Too many type parameters passed to 'f', which is typed as <T...>() -> (T...). Expected at most 1 type pack, but 2 provided."
            );
        }
    }
}

TEST_CASE_FIXTURE(Fixture, "too_many_provided_method")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local t = {
            f = function<T>(self: any) end,
        }

        t:f<<number, string>>()
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_REQUIRE_ERROR(result, TypeInstantiationCountMismatch);
        REQUIRE_EQ(result.errors[0].location.begin.line, 6);

        if (FFlag::LuauSolverV2)
        {
            REQUIRE_EQ(
                toString(result.errors[0]),
                "Too many type parameters passed to function typed as <T>(any) -> (). Expected at most 1 type parameter, but 2 provided."
            );
        }
        else
        {
            REQUIRE_EQ(
                toString(result.errors[0]),
                "Too many type parameters passed to 't.f', which is typed as <T>(any) -> (). Expected at most 1 type parameter, but 2 provided."
            );
        }
    }
}

TEST_CASE_FIXTURE(Fixture, "too_many_type_packs_provided_method")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local t = {
            f = function<T...>(self: any): (T...) end,
        }

        t:f<<(number, string), (true, false)>>()
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_REQUIRE_ERROR(result, TypeInstantiationCountMismatch);
        REQUIRE_EQ(result.errors[0].location.begin.line, 6);

        if (FFlag::LuauSolverV2)
        {
            REQUIRE_EQ(
                toString(result.errors[0]),
                "Too many type parameters passed to function typed as <T...>(any) -> (T...). Expected at most 1 type pack, but 2 provided."
            );
        }
        else
        {
            REQUIRE_EQ(
                toString(result.errors[0]),
                "Too many type parameters passed to 't.f', which is typed as <T...>(any) -> (T...). Expected at most 1 type pack, but 2 provided."
            );
        }
    }
}

TEST_CASE_FIXTURE(Fixture, "function_intersections")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        --!strict
        local f: (<T>(T) -> T) & (<T>(T?) -> T) = nil :: any
        f<<number>>()
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_REQUIRE_ERROR(result, InstantiateGenericsOnNonFunction);
        REQUIRE_EQ(result.errors[0].location.begin.line, 3);
        REQUIRE_EQ(toString(result.errors[0]), "Luau does not currently support explicitly instantiating an overloaded function type.");
    }
}

TEST_CASE_FIXTURE(Fixture, "incomplete_type_packs")
{
    SUBCASE_BOTH_SOLVERS()
    {
        ScopedFastFlag sff{FFlag::LuauExplicitTypeExpressionInstantiation, true};

        CheckResult result = check(R"(
        local f: <A, T...>() -> (A, T...) = nil :: any
        local correct: string, b: number, c: boolean = f<<string>>()
        local incorrect: number, b: number, c: boolean = f<<string>>()
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        LUAU_REQUIRE_ERROR(result, TypeMismatch);
        REQUIRE_EQ(result.errors[0].location.begin.line, 3);
    }
}

TEST_SUITE_END();
