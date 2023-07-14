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

TEST_CASE_FIXTURE(Fixture, "equal_numbers")
{
    CheckResult result = check(R"(
    local foo = 5
    local almostFoo = 78
    almostFoo = foo
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        CHECK(!diffRes.diffError.has_value());
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
}

TEST_CASE_FIXTURE(Fixture, "equal_strings")
{
    CheckResult result = check(R"(
    local foo = "hello"
    local almostFoo = "world"
    almostFoo = foo
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        CHECK(!diffRes.diffError.has_value());
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
}

TEST_CASE_FIXTURE(Fixture, "equal_tables")
{
    CheckResult result = check(R"(
    local foo = { x = 1, y = "where" }
    local almostFoo = { x = 5, y = "when" }
    almostFoo = foo
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        CHECK(!diffRes.diffError.has_value());
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
}

TEST_CASE_FIXTURE(Fixture, "a_table_missing_property")
{
    CheckResult result = check(R"(
    local foo = { x = 1, y = 2 }
    local almostFoo = { x = 1, z = 3 }
    almostFoo = foo
    )");
    LUAU_REQUIRE_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        diffMessage = diff(foo, almostFoo).diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ("DiffError: these two types are not equal because the left type at foo.y has type number, while the right type at almostFoo is missing "
             "the property y",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "left_table_missing_property")
{
    CheckResult result = check(R"(
    local foo = { x = 1 }
    local almostFoo = { x = 1, z = 3 }
    almostFoo = foo
    )");
    LUAU_REQUIRE_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        diffMessage = diff(foo, almostFoo).diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ("DiffError: these two types are not equal because the left type at foo is missing the property z, while the right type at almostFoo.z "
             "has type number",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "a_table_wrong_type")
{
    CheckResult result = check(R"(
    local foo = { x = 1, y = 2 }
    local almostFoo = { x = 1, y = "two" }
    almostFoo = foo
    )");
    LUAU_REQUIRE_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        diffMessage = diff(foo, almostFoo).diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ("DiffError: these two types are not equal because the left type at foo.y has type number, while the right type at almostFoo.y has type "
             "string",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "a_table_wrong_type")
{
    CheckResult result = check(R"(
    local foo: string
    local almostFoo: number
    almostFoo = foo
    )");
    LUAU_REQUIRE_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        diffMessage = diff(foo, almostFoo).diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ("DiffError: these two types are not equal because the left type at <unlabeled-symbol> has type string, while the right type at "
             "<unlabeled-symbol> has type number",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "a_nested_table_wrong_type")
{
    CheckResult result = check(R"(
    local foo = { x = 1, inner = { table = { has = { wrong = { value = 5 } } } } }
    local almostFoo = { x = 1, inner = { table = { has = { wrong = { value = "five" } } } } }
    almostFoo = foo
    )");
    LUAU_REQUIRE_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        diffMessage = diff(foo, almostFoo).diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ("DiffError: these two types are not equal because the left type at foo.inner.table.has.wrong.value has type number, while the right "
             "type at almostFoo.inner.table.has.wrong.value has type string",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "a_nested_table_wrong_match")
{
    CheckResult result = check(R"(
    local foo = { x = 1, inner = { table = { has = { wrong = { variant = { because = { it = { goes = { on = "five" } } } } } } } } }
    local almostFoo = { x = 1, inner = { table = { has = { wrong = { variant = "five" } } } } }
    almostFoo = foo
    )");
    LUAU_REQUIRE_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        diffMessage = diff(foo, almostFoo).diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ("DiffError: these two types are not equal because the left type at foo.inner.table.has.wrong.variant has type { because: { it: { goes: "
             "{ on: string } } } }, while the right type at almostFoo.inner.table.has.wrong.variant has type string",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "singleton")
{
    CheckResult result = check(R"(
    local foo: "hello" = "hello"
    local almostFoo: true = true
    almostFoo = foo
    )");
    LUAU_REQUIRE_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        diffMessage = diff(foo, almostFoo).diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> has type "hello", while the right type at <unlabeled-symbol> has type true)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "equal_singleton")
{
    CheckResult result = check(R"(
    local foo: "hello" = "hello"
    local almostFoo: "hello"
    almostFoo = foo
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        INFO(diffRes.diffError->toString());
        CHECK(!diffRes.diffError.has_value());
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
}

TEST_CASE_FIXTURE(Fixture, "singleton_string")
{
    CheckResult result = check(R"(
    local foo: "hello" = "hello"
    local almostFoo: "world" = "world"
    almostFoo = foo
    )");
    LUAU_REQUIRE_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        diffMessage = diff(foo, almostFoo).diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> has type "hello", while the right type at <unlabeled-symbol> has type "world")",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "equal_function")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        INFO(diffRes.diffError->toString());
        CHECK(!diffRes.diffError.has_value());
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
}

TEST_CASE_FIXTURE(Fixture, "equal_function_inferred_ret_length")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        INFO(diffRes.diffError->toString());
        CHECK(!diffRes.diffError.has_value());
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
}

TEST_CASE_FIXTURE(Fixture, "equal_function_inferred_ret_length_2")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        INFO(diffRes.diffError->toString());
        CHECK(!diffRes.diffError.has_value());
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
}

TEST_CASE_FIXTURE(Fixture, "function_arg_normal")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: number, z: number)
        return x * y * z
    end
    function almostFoo(a: number, b: number, msg: string)
        return a
    almostFoo = foo
    )");
    LUAU_REQUIRE_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        diffMessage = diff(foo, almostFoo).diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[3] has type number, while the right type at <unlabeled-symbol>.Arg[3] has type string)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_arg_normal_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", true};

    CheckResult result = check(R"(
    function foo(x: number, y: number, z: string)
        return x * y
    end
    function almostFoo(a: number, y: string, msg: string)
        return a
    almostFoo = foo
    )");
    LUAU_REQUIRE_ERRORS(result);

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        diffMessage = diff(foo, almostFoo).diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[2] has type number, while the right type at <unlabeled-symbol>.Arg[2] has type string)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_ret_normal")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Ret[1] has type number, while the right type at <unlabeled-symbol>.Ret[1] has type string)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_arg_length")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 2 or more arguments, while the right type at <unlabeled-symbol> takes 3 or more arguments)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_arg_length_2")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 3 or more arguments, while the right type at <unlabeled-symbol> takes 2 or more arguments)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_arg_length_none")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 0 or more arguments, while the right type at <unlabeled-symbol> takes 2 or more arguments)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_arg_length_none_2")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 1 or more arguments, while the right type at <unlabeled-symbol> takes 0 or more arguments)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_ret_length")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> returns 1 values, while the right type at <unlabeled-symbol> returns 2 values)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_ret_length_2")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> returns 3 values, while the right type at <unlabeled-symbol> returns 2 values)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_ret_length_none")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> returns 0 values, while the right type at <unlabeled-symbol> returns 1 values)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_ret_length_none_2")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> returns 1 values, while the right type at <unlabeled-symbol> returns 0 values)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_variadic_arg_normal")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[Variadic] has type number, while the right type at <unlabeled-symbol>.Arg[Variadic] has type string)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_variadic_arg_missing")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[Variadic] has type number, while the right type at <unlabeled-symbol>.Arg[Variadic] has type any)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_variadic_arg_missing_2")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[Variadic] has type any, while the right type at <unlabeled-symbol>.Arg[Variadic] has type string)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_variadic_oversaturation")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 2 or more arguments, while the right type at <unlabeled-symbol> takes 2 arguments)",
        diffMessage);
}

TEST_CASE_FIXTURE(Fixture, "function_variadic_oversaturation_2")
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

    TypeId foo = requireType("foo");
    TypeId almostFoo = requireType("almostFoo");
    std::string diffMessage;
    try
    {
        DifferResult diffRes = diff(foo, almostFoo);
        if (!diffRes.diffError.has_value())
        {
            INFO("Differ did not report type error, even though types are unequal");
            CHECK(false);
        }
        diffMessage = diffRes.diffError->toString();
    }
    catch (const InternalCompilerError& e)
    {
        INFO(("InternalCompilerError: " + e.message));
        CHECK(false);
    }
    CHECK_EQ(
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 2 arguments, while the right type at <unlabeled-symbol> takes 2 or more arguments)",
        diffMessage);
}

TEST_SUITE_END();
