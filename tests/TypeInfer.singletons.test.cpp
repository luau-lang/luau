// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "doctest.h"
#include "Luau/BuiltinDefinitions.h"

using namespace Luau;

TEST_SUITE_BEGIN("TypeSingletons");

TEST_CASE_FIXTURE(Fixture, "bool_singletons")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        local a: true = true
        local b: false = false
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "string_singletons")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        local a: "foo" = "foo"
        local b: "bar" = "bar"
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "bool_singletons_mismatch")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        local a: true = false
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type 'false' could not be converted into 'true'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "string_singletons_mismatch")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        local a: "foo" = "bar"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type '\"bar\"' could not be converted into '\"foo\"'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "string_singletons_escape_chars")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        local a: "\n" = "\000\r"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(R"(Type '"\000\r"' could not be converted into '"\n"')", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "bool_singleton_subtype")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        local a: true = true
        local b: boolean = a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "string_singleton_subtype")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        local a: "foo" = "foo"
        local b: string = a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "function_call_with_singletons")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        function f(a: true, b: "foo") end
        f(true, "foo")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "function_call_with_singletons_mismatch")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        function f(a: true, b: "foo") end
        f(true, "bar")
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type '\"bar\"' could not be converted into '\"foo\"'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "overloaded_function_call_with_singletons")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        function f(a, b) end
        local g : ((true, string) -> ()) & ((false, number) -> ()) = (f::any)
        g(true, "foo")
        g(false, 37)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "overloaded_function_call_with_singletons_mismatch")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        function f(a, b) end
        local g : ((true, string) -> ()) & ((false, number) -> ()) = (f::any)
        g(true, 37)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ("Type 'number' could not be converted into 'string'", toString(result.errors[0]));
    CHECK_EQ("Other overloads are also not viable: (false, number) -> ()", toString(result.errors[1]));
}

TEST_CASE_FIXTURE(Fixture, "enums_using_singletons")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        type MyEnum = "foo" | "bar" | "baz"
        local a : MyEnum = "foo"
        local b : MyEnum = "bar"
        local c : MyEnum = "baz"
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "enums_using_singletons_mismatch")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        type MyEnum = "foo" | "bar" | "baz"
        local a : MyEnum = "bang"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type '\"bang\"' could not be converted into '\"bar\" | \"baz\" | \"foo\"'; none of the union options are compatible",
        toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "enums_using_singletons_subtyping")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        type MyEnum1 = "foo" | "bar"
        type MyEnum2 = MyEnum1 | "baz"
        local a : MyEnum1 = "foo"
        local b : MyEnum2 = a
        local c : string = b
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "tagged_unions_using_singletons")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
        {"LuauExpectedTypesOfProperties", true},
    };

    CheckResult result = check(R"(
        type Dog = { tag: "Dog", howls: boolean }
        type Cat = { tag: "Cat", meows: boolean }
        type Animal = Dog | Cat
        local a : Dog = { tag = "Dog", howls = true }
        local b : Animal = { tag = "Cat", meows = true }
        local c : Animal = a
        c = b
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "tagged_unions_using_singletons_mismatch")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        type Dog = { tag: "Dog", howls: boolean }
        type Cat = { tag: "Cat", meows: boolean }
        type Animal = Dog | Cat
        local a : Animal = { tag = "Cat", howls = true }
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "tagged_unions_immutable_tag")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        type Dog = { tag: "Dog", howls: boolean }
        type Cat = { tag: "Cat", meows: boolean }
        type Animal = Dog | Cat
        local a : Animal = { tag = "Cat", meows = true }
        a.tag = "Dog"
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "table_properties_singleton_strings")
{
    ScopedFastFlag sffs[] = {
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        --!strict
        type T = {
            ["foo"] : number,
            ["$$bar"] : string,
            baz : boolean
        }
        local t: T =  {
            ["foo"] = 37,
            ["$$bar"] = "hi",
            baz = true
        }
        local a: number = t.foo
        local b: string = t["$$bar"]
        local c: boolean = t.baz
        t.foo = 5
        t["$$bar"] = "lo"
        t.baz = false
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}
TEST_CASE_FIXTURE(Fixture, "table_properties_singleton_strings_mismatch")
{
    ScopedFastFlag sffs[] = {
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        --!strict
        type T = {
            ["$$bar"] : string,
        }
        local t: T =  {
            ["$$bar"] = "hi",
        }
        t["$$bar"] = 5
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type 'number' could not be converted into 'string'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "table_properties_alias_or_parens_is_indexer")
{
    ScopedFastFlag sffs[] = {
        {"LuauParseSingletonTypes", true},
    };

    CheckResult result = check(R"(
        --!strict
        type S = "bar"
        type T = {
            [("foo")] : number,
            [S] : string,
        }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Syntax error: Cannot have more than one table indexer", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "table_properties_type_error_escapes")
{
    ScopedFastFlag sffs[] = {
        {"LuauParseSingletonTypes", true},
        {"LuauUnsealedTableLiteral", true},
    };

    CheckResult result = check(R"(
        --!strict
        local x: { ["<>"] : number } 
        x = { ["\n"] = 5 }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(R"(Table type '{ ["\n"]: number }' not compatible with type '{| ["<>"]: number |}' because the former is missing field '<>')",
        toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_tagged_union_mismatch_string")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
        {"LuauExpectedTypesOfProperties", true},
    };

    CheckResult result = check(R"(
type Cat = { tag: 'cat', catfood: string }
type Dog = { tag: 'dog', dogfood: string }
type Animal = Cat | Dog

local a: Animal = { tag = 'cat', cafood = 'something' }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(R"(Type 'a' could not be converted into 'Cat | Dog'
caused by:
  None of the union options are compatible. For example: Table type 'a' not compatible with type 'Cat' because the former is missing field 'catfood')",
        toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_tagged_union_mismatch_bool")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
        {"LuauExpectedTypesOfProperties", true},
    };

    CheckResult result = check(R"(
type Good = { success: true, result: string }
type Bad = { success: false, error: string }
type Result = Good | Bad

local a: Result = { success = false, result = 'something' }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(R"(Type 'a' could not be converted into 'Bad | Good'
caused by:
  None of the union options are compatible. For example: Table type 'a' not compatible with type 'Bad' because the former is missing field 'error')",
        toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "if_then_else_expression_singleton_options")
{
    ScopedFastFlag sffs[] = {
        {"LuauSingletonTypes", true},
        {"LuauParseSingletonTypes", true},
        {"LuauExpectedTypesOfProperties", true},
        {"LuauIfElseExpectedType2", true},
        {"LuauIfElseBranchTypeUnion", true},
    };

    CheckResult result = check(R"(
type Cat = { tag: 'cat', catfood: string }
type Dog = { tag: 'dog', dogfood: string }
type Animal = Cat | Dog

local a: Animal = if true then { tag = 'cat', catfood = 'something' } else { tag = 'dog', dogfood = 'other' }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
