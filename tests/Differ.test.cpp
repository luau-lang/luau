// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Differ.h"
#include "Luau/Common.h"
#include "Luau/Error.h"
#include "Luau/Frontend.h"

#include "Fixture.h"

#include "Luau/Symbol.h"
#include "ScopedFlags.h"
#include "doctest.h"
#include <iostream>

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

TEST_SUITE_BEGIN("Differ");

TEST_CASE_FIXTURE(DifferFixture, "equal_numbers")
{
    CheckResult result = check(R"(
    local foo = 5
    local almostFoo = 78
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_strings")
{
    CheckResult result = check(R"(
    local foo = "hello"
    local almostFoo = "world"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_tables")
{
    CheckResult result = check(R"(
    local foo = { x = 1, y = "where" }
    local almostFoo = { x = 5, y = "when" }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "a_table_missing_property")
{
    CheckResult result = check(R"(
    local foo = { x = 1, y = 2 }
    local almostFoo = { x = 1, z = 3 }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        "DiffError: these two types are not equal because the left type at foo.y has type number, while the right type at almostFoo is missing "
        "the property y");
}

TEST_CASE_FIXTURE(DifferFixture, "left_table_missing_property")
{
    CheckResult result = check(R"(
    local foo = { x = 1 }
    local almostFoo = { x = 1, z = 3 }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        "DiffError: these two types are not equal because the left type at foo is missing the property z, while the right type at almostFoo.z "
        "has type number");
}

TEST_CASE_FIXTURE(DifferFixture, "a_table_wrong_type")
{
    CheckResult result = check(R"(
    local foo = { x = 1, y = 2 }
    local almostFoo = { x = 1, y = "two" }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        "DiffError: these two types are not equal because the left type at foo.y has type number, while the right type at almostFoo.y has type "
        "string");
}

TEST_CASE_FIXTURE(DifferFixture, "a_table_wrong_type")
{
    CheckResult result = check(R"(
    local foo: string
    local almostFoo: number
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        "DiffError: these two types are not equal because the left type at <unlabeled-symbol> has type string, while the right type at "
        "<unlabeled-symbol> has type number");
}

TEST_CASE_FIXTURE(DifferFixture, "a_nested_table_wrong_type")
{
    CheckResult result = check(R"(
    local foo = { x = 1, inner = { table = { has = { wrong = { value = 5 } } } } }
    local almostFoo = { x = 1, inner = { table = { has = { wrong = { value = "five" } } } } }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        "DiffError: these two types are not equal because the left type at foo.inner.table.has.wrong.value has type number, while the right "
        "type at almostFoo.inner.table.has.wrong.value has type string");
}

TEST_CASE_FIXTURE(DifferFixture, "a_nested_table_wrong_match")
{
    CheckResult result = check(R"(
    local foo = { x = 1, inner = { table = { has = { wrong = { variant = { because = { it = { goes = { on = "five" } } } } } } } } }
    local almostFoo = { x = 1, inner = { table = { has = { wrong = { variant = "five" } } } } }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        "DiffError: these two types are not equal because the left type at foo.inner.table.has.wrong.variant has type { because: { it: { goes: "
        "{ on: string } } } }, while the right type at almostFoo.inner.table.has.wrong.variant has type string");
}

TEST_CASE_FIXTURE(DifferFixture, "singleton")
{
    CheckResult result = check(R"(
    local foo: "hello" = "hello"
    local almostFoo: true = true
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> has type "hello", while the right type at <unlabeled-symbol> has type true)");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_singleton")
{
    CheckResult result = check(R"(
    local foo: "hello" = "hello"
    local almostFoo: "hello"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "singleton_string")
{
    CheckResult result = check(R"(
    local foo: "hello" = "hello"
    local almostFoo: "world" = "world"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> has type "hello", while the right type at <unlabeled-symbol> has type "world")");
}

TEST_CASE_FIXTURE(DifferFixtureWithBuiltins, "negation")
{
    // Old solver does not correctly refine test types
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
        local bar: { x: { y: unknown }}
        local almostBar: { x: { y: unknown }}

        local foo
        local almostFoo

        if typeof(bar.x.y) ~= "string" then
            foo = bar
        end

        if typeof(almostBar.x.y) ~= "number" then
            almostFoo = almostBar
        end
            
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.x.y.Negation has type string, while the right type at <unlabeled-symbol>.x.y.Negation has type number)");
}

TEST_CASE_FIXTURE(DifferFixture, "union_missing_right")
{
    CheckResult result = check(R"(
    local foo: string | number
    local almostFoo: boolean | string
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is a union containing type number, while the right type at <unlabeled-symbol> is a union missing type number)");
}

TEST_CASE_FIXTURE(DifferFixture, "union_missing_left")
{
    CheckResult result = check(R"(
    local foo: string | number
    local almostFoo: boolean | string | number
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is a union missing type boolean, while the right type at <unlabeled-symbol> is a union containing type boolean)");
}

TEST_CASE_FIXTURE(DifferFixture, "union_missing")
{
    // TODO: this test case produces an error message that is not the most UX-friendly

    CheckResult result = check(R"(
    local foo: { bar: number, pan: string } | { baz: boolean, rot: "singleton" }
    local almostFoo: { bar: number, pan: string } | { baz: string, rot: "singleton" }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is a union containing type {| baz: boolean, rot: "singleton" |}, while the right type at <unlabeled-symbol> is a union missing type {| baz: boolean, rot: "singleton" |})");
}

TEST_CASE_FIXTURE(DifferFixture, "intersection_missing_right")
{
    CheckResult result = check(R"(
    local foo: (number) -> () & (string) -> ()
    local almostFoo: (string) -> () & (boolean) -> ()
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is an intersection containing type (number) -> (), while the right type at <unlabeled-symbol> is an intersection missing type (number) -> ())");
}

TEST_CASE_FIXTURE(DifferFixture, "intersection_missing_left")
{
    CheckResult result = check(R"(
    local foo: (number) -> () & (string) -> ()
    local almostFoo: (string) -> () & (boolean) -> () & (number) -> ()
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is an intersection missing type (boolean) -> (), while the right type at <unlabeled-symbol> is an intersection containing type (boolean) -> ())");
}

TEST_CASE_FIXTURE(DifferFixture, "intersection_tables_missing_right")
{
    CheckResult result = check(R"(
    local foo: { x: number } & { y: string }
    local almostFoo: { y: string } & { z: boolean }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is an intersection containing type {| x: number |}, while the right type at <unlabeled-symbol> is an intersection missing type {| x: number |})");
}

TEST_CASE_FIXTURE(DifferFixture, "intersection_tables_missing_left")
{
    CheckResult result = check(R"(
    local foo: { x: number } & { y: string }
    local almostFoo: { y: string } & { z: boolean } & { x: number }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is an intersection missing type {| z: boolean |}, while the right type at <unlabeled-symbol> is an intersection containing type {| z: boolean |})");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_function")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number)
        return x
    end
    function almostFoo(y: number)
        return y + 10
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_function_inferred_ret_length")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function bar(x: number, y: string)
        return x, y
    end
    function almostBar(a: number, b: string)
        return a, b
    end
    function foo(x: number, y: string, z: boolean)
        return z, bar(x, y)
    end
    function almostFoo(a: number, b: string, c: boolean)
        return c, almostBar(a, b)
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_function_inferred_ret_length_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function bar(x: number, y: string)
        return x, y
    end
    function foo(x: number, y: string, z: boolean)
        return bar(x, y), z
    end
    function almostFoo(a: number, b: string, c: boolean)
        return a, c
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "function_arg_normal")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: number, z: number)
        return x * y * z
    end
    function almostFoo(a: number, b: number, msg: string)
        return a
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[3] has type number, while the right type at <unlabeled-symbol>.Arg[3] has type string)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_arg_normal_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: number, z: string)
        return x * y
    end
    function almostFoo(a: number, y: string, msg: string)
        return a
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[2] has type number, while the right type at <unlabeled-symbol>.Arg[2] has type string)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_ret_normal")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: number, z: string)
        return x
    end
    function almostFoo(a: number, b: number, msg: string)
        return msg
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Ret[1] has type number, while the right type at <unlabeled-symbol>.Ret[1] has type string)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_arg_length")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: number)
        return x
    end
    function almostFoo(x: number, y: number, c: number)
        return x
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 2 or more arguments, while the right type at <unlabeled-symbol> takes 3 or more arguments)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_arg_length_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: string, z: number)
        return z
    end
    function almostFoo(x: number, y: string)
        return x
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 3 or more arguments, while the right type at <unlabeled-symbol> takes 2 or more arguments)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_arg_length_none")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo()
        return 5
    end
    function almostFoo(x: number, y: string)
        return x
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 0 or more arguments, while the right type at <unlabeled-symbol> takes 2 or more arguments)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_arg_length_none_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number)
        return x
    end
    function almostFoo()
        return 5
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 1 or more arguments, while the right type at <unlabeled-symbol> takes 0 or more arguments)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_ret_length")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: number)
        return x
    end
    function almostFoo(x: number, y: number)
        return x, y
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> returns 1 values, while the right type at <unlabeled-symbol> returns 2 values)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_ret_length_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: string, z: number)
        return y, x, z
    end
    function almostFoo(x: number, y: string, z: number)
        return y, x
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> returns 3 values, while the right type at <unlabeled-symbol> returns 2 values)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_ret_length_none")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: string)
        return
    end
    function almostFoo(x: number, y: string)
        return x
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> returns 0 values, while the right type at <unlabeled-symbol> returns 1 values)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_ret_length_none_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo()
        return 5
    end
    function almostFoo()
        return
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> returns 1 values, while the right type at <unlabeled-symbol> returns 0 values)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_variadic_arg_normal")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: string, ...: number)
        return x, y
    end
    function almostFoo(a: number, b: string, ...: string)
        return a, b
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[Variadic] has type number, while the right type at <unlabeled-symbol>.Arg[Variadic] has type string)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_variadic_arg_missing")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: string, ...: number)
        return x, y
    end
    function almostFoo(a: number, b: string)
        return a, b
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[Variadic] has type number, while the right type at <unlabeled-symbol>.Arg[Variadic] has type any)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_variadic_arg_missing_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: string)
        return x, y
    end
    function almostFoo(a: number, b: string, ...: string)
        return a, b
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[Variadic] has type any, while the right type at <unlabeled-symbol>.Arg[Variadic] has type string)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_variadic_oversaturation")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    -- allowed to be oversaturated
    function foo(x: number, y: string)
        return x, y
    end
    -- must not be oversaturated
    local almostFoo: (number, string) -> (number, string) = foo
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 2 or more arguments, while the right type at <unlabeled-symbol> takes 2 arguments)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_variadic_oversaturation_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    -- must not be oversaturated
    local foo: (number, string) -> (number, string)
    -- allowed to be oversaturated
    function almostFoo(x: number, y: string)
        return x, y
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 2 arguments, while the right type at <unlabeled-symbol> takes 2 or more arguments)");
}

TEST_CASE_FIXTURE(DifferFixture, "generic")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x, y)
        return x, y
    end
    function almostFoo(x, y)
        return y, x
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left generic at <unlabeled-symbol>.Ret[1] cannot be the same type parameter as the right generic at <unlabeled-symbol>.Ret[1])");
}

TEST_CASE_FIXTURE(DifferFixture, "generic_one_vs_two")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo<X>(x: X, y: X)
        return
    end
    function almostFoo<T, U>(x: T, y: U)
        return
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left generic at <unlabeled-symbol>.Arg[2] cannot be the same type parameter as the right generic at <unlabeled-symbol>.Arg[2])");
}

TEST_CASE_FIXTURE(DifferFixture, "generic_three_or_three")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo<X, Y>(x: X, y: X, z: Y)
        return
    end
    function almostFoo<T, U>(x: T, y: U, z: U)
        return
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left generic at <unlabeled-symbol>.Arg[2] cannot be the same type parameter as the right generic at <unlabeled-symbol>.Arg[2])");
}

TEST_SUITE_END();
