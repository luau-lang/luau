// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Differ.h"
#include "Luau/Error.h"
#include "Luau/Frontend.h"

#include "Fixture.h"

#include "doctest.h"
#include <iostream>

using namespace Luau;

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

TEST_SUITE_END();
