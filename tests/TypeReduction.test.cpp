// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeReduction.h"

#include "Fixture.h"
#include "doctest.h"

using namespace Luau;

namespace
{
struct ReductionFixture : Fixture
{
    TypeArena arena;
    InternalErrorReporter iceHandler;
    UnifierSharedState unifierState{&iceHandler};
    TypeReduction reduction{NotNull{&arena}, builtinTypes, NotNull{&iceHandler}};

    ReductionFixture()
    {
        registerHiddenTypes(&frontend);
        createSomeClasses(&frontend);
    }

    TypeId reductionof(TypeId ty)
    {
        std::optional<TypeId> reducedTy = reduction.reduce(ty);
        REQUIRE(reducedTy);
        return *reducedTy;
    }

    std::optional<TypeId> tryReduce(const std::string& annotation)
    {
        CheckResult result = check("type _Res = " + annotation);
        LUAU_REQUIRE_NO_ERRORS(result);
        return reduction.reduce(requireTypeAlias("_Res"));
    }

    TypeId reductionof(const std::string& annotation)
    {
        std::optional<TypeId> reducedTy = tryReduce(annotation);
        REQUIRE_MESSAGE(reducedTy, "Exceeded the cartesian product of the type");
        return *reducedTy;
    }
};
} // namespace

TEST_SUITE_BEGIN("TypeReductionTests");

TEST_CASE_FIXTURE(ReductionFixture, "cartesian_product_exceeded")
{
    ScopedFastInt sfi{"LuauTypeReductionCartesianProductLimit", 5};

    std::optional<TypeId> ty = tryReduce(R"(
        string & (number | string | boolean) & (number | string | boolean)
    )");

    CHECK(!ty);
}

TEST_CASE_FIXTURE(ReductionFixture, "cartesian_product_exceeded_with_normal_limit")
{
    std::optional<TypeId> ty = tryReduce(R"(
        string                        -- 1         = 1
        & (number | string | boolean) -- 1     * 3 = 3
        & (number | string | boolean) -- 3     * 3 = 9
        & (number | string | boolean) -- 9     * 3 = 27
        & (number | string | boolean) -- 27    * 3 = 81
        & (number | string | boolean) -- 81    * 3 = 243
        & (number | string | boolean) -- 243   * 3 = 729
        & (number | string | boolean) -- 729   * 3 = 2187
        & (number | string | boolean) -- 2187  * 3 = 6561
        & (number | string | boolean) -- 6561  * 3 = 19683
        & (number | string | boolean) -- 19683 * 3 = 59049
        & (number | string)           -- 59049 * 2 = 118098
    )");

    CHECK(!ty);
}

TEST_CASE_FIXTURE(ReductionFixture, "cartesian_product_is_zero")
{
    ScopedFastInt sfi{"LuauTypeReductionCartesianProductLimit", 5};

    std::optional<TypeId> ty = tryReduce(R"(
        string & (number | string | boolean) & (number | string | boolean) & never
    )");

    CHECK(ty);
}

TEST_CASE_FIXTURE(ReductionFixture, "intersections_without_negations")
{
    SUBCASE("string_and_string")
    {
        TypeId ty = reductionof("string & string");
        CHECK("string" == toString(ty));
    }

    SUBCASE("never_and_string")
    {
        TypeId ty = reductionof("never & string");
        CHECK("never" == toString(ty));
    }

    SUBCASE("string_and_never")
    {
        TypeId ty = reductionof("string & never");
        CHECK("never" == toString(ty));
    }

    SUBCASE("unknown_and_string")
    {
        TypeId ty = reductionof("unknown & string");
        CHECK("string" == toString(ty));
    }

    SUBCASE("string_and_unknown")
    {
        TypeId ty = reductionof("string & unknown");
        CHECK("string" == toString(ty));
    }

    SUBCASE("any_and_string")
    {
        TypeId ty = reductionof("any & string");
        CHECK("string" == toString(ty));
    }

    SUBCASE("string_and_any")
    {
        TypeId ty = reductionof("string & any");
        CHECK("string" == toString(ty));
    }

    SUBCASE("string_or_number_and_string")
    {
        TypeId ty = reductionof("(string | number) & string");
        CHECK("string" == toString(ty));
    }

    SUBCASE("string_and_string_or_number")
    {
        TypeId ty = reductionof("string & (string | number)");
        CHECK("string" == toString(ty));
    }

    SUBCASE("string_and_a")
    {
        TypeId ty = reductionof(R"(string & "a")");
        CHECK(R"("a")" == toString(ty));
    }

    SUBCASE("boolean_and_true")
    {
        TypeId ty = reductionof("boolean & true");
        CHECK("true" == toString(ty));
    }

    SUBCASE("boolean_and_a")
    {
        TypeId ty = reductionof(R"(boolean & "a")");
        CHECK("never" == toString(ty));
    }

    SUBCASE("a_and_a")
    {
        TypeId ty = reductionof(R"("a" & "a")");
        CHECK(R"("a")" == toString(ty));
    }

    SUBCASE("a_and_b")
    {
        TypeId ty = reductionof(R"("a" & "b")");
        CHECK("never" == toString(ty));
    }

    SUBCASE("a_and_true")
    {
        TypeId ty = reductionof(R"("a" & true)");
        CHECK("never" == toString(ty));
    }

    SUBCASE("a_and_true")
    {
        TypeId ty = reductionof(R"(true & false)");
        CHECK("never" == toString(ty));
    }

    SUBCASE("function_type_and_function")
    {
        TypeId ty = reductionof("() -> () & fun");
        CHECK("() -> ()" == toString(ty));
    }

    SUBCASE("function_type_and_string")
    {
        TypeId ty = reductionof("() -> () & string");
        CHECK("never" == toString(ty));
    }

    SUBCASE("parent_and_child")
    {
        TypeId ty = reductionof("Parent & Child");
        CHECK("Child" == toString(ty));
    }

    SUBCASE("child_and_parent")
    {
        TypeId ty = reductionof("Child & Parent");
        CHECK("Child" == toString(ty));
    }

    SUBCASE("child_and_unrelated")
    {
        TypeId ty = reductionof("Child & Unrelated");
        CHECK("never" == toString(ty));
    }

    SUBCASE("string_and_table")
    {
        TypeId ty = reductionof("string & {}");
        CHECK("never" == toString(ty));
    }

    SUBCASE("string_and_child")
    {
        TypeId ty = reductionof("string & Child");
        CHECK("never" == toString(ty));
    }

    SUBCASE("string_and_function")
    {
        TypeId ty = reductionof("string & () -> ()");
        CHECK("never" == toString(ty));
    }

    SUBCASE("function_and_table")
    {
        TypeId ty = reductionof("() -> () & {}");
        CHECK("never" == toString(ty));
    }

    SUBCASE("function_and_class")
    {
        TypeId ty = reductionof("() -> () & Child");
        CHECK("never" == toString(ty));
    }

    SUBCASE("function_and_function")
    {
        TypeId ty = reductionof("() -> () & () -> ()");
        CHECK("(() -> ()) & (() -> ())" == toString(ty));
    }

    SUBCASE("table_and_table")
    {
        TypeId ty = reductionof("{} & {}");
        CHECK("{|  |}" == toString(ty));
    }

    SUBCASE("table_and_metatable")
    {
        // No setmetatable in ReductionFixture, so we mix and match.
        BuiltinsFixture fixture;
        fixture.check(R"(
            type Ty = {} & typeof(setmetatable({}, {}))
        )");

        TypeId ty = reductionof(fixture.requireTypeAlias("Ty"));
        CHECK("{ @metatable {  }, {  } } & {|  |}" == toString(ty));
    }

    SUBCASE("a_and_string")
    {
        TypeId ty = reductionof(R"("a" & string)");
        CHECK(R"("a")" == toString(ty));
    }

    SUBCASE("reducible_function_and_function")
    {
        TypeId ty = reductionof("((string | string) -> (number | number)) & fun");
        CHECK("(string) -> number" == toString(ty));
    }

    SUBCASE("string_and_error")
    {
        TypeId ty = reductionof("string & err");
        CHECK("*error-type* & string" == toString(ty));
    }

    SUBCASE("table_p_string_and_table_p_number")
    {
        TypeId ty = reductionof("{ p: string } & { p: number }");
        CHECK("never" == toString(ty));
    }

    SUBCASE("table_p_string_and_table_p_string")
    {
        TypeId ty = reductionof("{ p: string } & { p: string }");
        CHECK("{| p: string |}" == toString(ty));
    }

    SUBCASE("table_x_table_p_string_and_table_x_table_p_number")
    {
        TypeId ty = reductionof("{ x: { p: string } } & { x: { p: number } }");
        CHECK("never" == toString(ty));
    }

    SUBCASE("table_p_and_table_q")
    {
        TypeId ty = reductionof("{ p: string } & { q: number }");
        CHECK("{| p: string, q: number |}" == toString(ty));
    }

    SUBCASE("table_tag_a_or_table_tag_b_and_table_b")
    {
        TypeId ty = reductionof("({ tag: string, a: number } | { tag: number, b: string }) & { b: string }");
        CHECK("{| a: number, b: string, tag: string |} | {| b: string, tag: number |}" == toString(ty));
    }

    SUBCASE("table_string_number_indexer_and_table_string_number_indexer")
    {
        TypeId ty = reductionof("{ [string]: number } & { [string]: number }");
        CHECK("{| [string]: number |}" == toString(ty));
    }

    SUBCASE("table_string_number_indexer_and_empty_table")
    {
        TypeId ty = reductionof("{ [string]: number } & {}");
        CHECK("{| [string]: number |}" == toString(ty));
    }

    SUBCASE("empty_table_table_string_number_indexer")
    {
        TypeId ty = reductionof("{} & { [string]: number }");
        CHECK("{| [string]: number |}" == toString(ty));
    }

    SUBCASE("string_number_indexer_and_number_number_indexer")
    {
        TypeId ty = reductionof("{ [string]: number } & { [number]: number }");
        CHECK("never" == toString(ty));
    }

    SUBCASE("table_p_string_and_indexer_number_number")
    {
        TypeId ty = reductionof("{ p: string } & { [number]: number }");
        CHECK("{| [number]: number, p: string |}" == toString(ty));
    }

    SUBCASE("table_p_string_and_indexer_string_number")
    {
        TypeId ty = reductionof("{ p: string } & { [string]: number }");
        CHECK("{| [string]: number, p: string |}" == toString(ty));
    }

    SUBCASE("table_p_string_and_table_p_string_plus_indexer_string_number")
    {
        TypeId ty = reductionof("{ p: string } & { p: string, [string]: number }");
        CHECK("{| [string]: number, p: string |}" == toString(ty));
    }
} // intersections_without_negations

TEST_CASE_FIXTURE(ReductionFixture, "intersections_with_negations")
{
    SUBCASE("nil_and_not_nil")
    {
        TypeId ty = reductionof("nil & Not<nil>");
        CHECK("never" == toString(ty));
    }

    SUBCASE("nil_and_not_false")
    {
        TypeId ty = reductionof("nil & Not<false>");
        CHECK("nil" == toString(ty));
    }

    SUBCASE("string_or_nil_and_not_nil")
    {
        TypeId ty = reductionof("(string?) & Not<nil>");
        CHECK("string" == toString(ty));
    }

    SUBCASE("string_or_nil_and_not_false_or_nil")
    {
        TypeId ty = reductionof("(string?) & Not<false | nil>");
        CHECK("string" == toString(ty));
    }

    SUBCASE("string_or_nil_and_not_false_and_not_nil")
    {
        TypeId ty = reductionof("(string?) & Not<false> & Not<nil>");
        CHECK("string" == toString(ty));
    }

    SUBCASE("not_false_and_bool")
    {
        TypeId ty = reductionof("Not<false> & boolean");
        CHECK("true" == toString(ty));
    }

    SUBCASE("function_type_and_not_function")
    {
        TypeId ty = reductionof("() -> () & Not<fun>");
        CHECK("never" == toString(ty));
    }

    SUBCASE("function_type_and_not_string")
    {
        TypeId ty = reductionof("() -> () & Not<string>");
        CHECK("() -> ()" == toString(ty));
    }

    SUBCASE("not_a_and_string_or_nil")
    {
        TypeId ty = reductionof(R"(Not<"a"> & (string | nil))");
        CHECK(R"((string & ~"a")?)" == toString(ty));
    }

    SUBCASE("not_a_and_a")
    {
        TypeId ty = reductionof(R"(Not<"a"> & "a")");
        CHECK("never" == toString(ty));
    }

    SUBCASE("not_a_and_b")
    {
        TypeId ty = reductionof(R"(Not<"a"> & "b")");
        CHECK(R"("b")" == toString(ty));
    }

    SUBCASE("not_string_and_a")
    {
        TypeId ty = reductionof(R"(Not<string> & "a")");
        CHECK("never" == toString(ty));
    }

    SUBCASE("not_bool_and_true")
    {
        TypeId ty = reductionof("Not<boolean> & true");
        CHECK("never" == toString(ty));
    }

    SUBCASE("not_string_and_true")
    {
        TypeId ty = reductionof("Not<string> & true");
        CHECK("true" == toString(ty));
    }

    SUBCASE("parent_and_not_child")
    {
        TypeId ty = reductionof("Parent & Not<Child>");
        CHECK("Parent & ~Child" == toString(ty));
    }

    SUBCASE("not_child_and_parent")
    {
        TypeId ty = reductionof("Not<Child> & Parent");
        CHECK("Parent & ~Child" == toString(ty));
    }

    SUBCASE("child_and_not_parent")
    {
        TypeId ty = reductionof("Child & Not<Parent>");
        CHECK("never" == toString(ty));
    }

    SUBCASE("not_parent_and_child")
    {
        TypeId ty = reductionof("Not<Parent> & Child");
        CHECK("never" == toString(ty));
    }

    SUBCASE("not_parent_and_unrelated")
    {
        TypeId ty = reductionof("Not<Parent> & Unrelated");
        CHECK("Unrelated" == toString(ty));
    }

    SUBCASE("unrelated_and_not_parent")
    {
        TypeId ty = reductionof("Unrelated & Not<Parent>");
        CHECK("Unrelated" == toString(ty));
    }

    SUBCASE("not_unrelated_and_parent")
    {
        TypeId ty = reductionof("Not<Unrelated> & Parent");
        CHECK("Parent" == toString(ty));
    }

    SUBCASE("parent_and_not_unrelated")
    {
        TypeId ty = reductionof("Parent & Not<Unrelated>");
        CHECK("Parent" == toString(ty));
    }

    SUBCASE("reducible_function_and_not_function")
    {
        TypeId ty = reductionof("((string | string) -> (number | number)) & Not<fun>");
        CHECK("never" == toString(ty));
    }

    SUBCASE("string_and_not_error")
    {
        TypeId ty = reductionof("string & Not<err>");
        CHECK("string & ~*error-type*" == toString(ty));
    }

    SUBCASE("table_p_string_and_table_p_not_number")
    {
        TypeId ty = reductionof("{ p: string } & { p: Not<number> }");
        CHECK("{| p: string |}" == toString(ty));
    }

    SUBCASE("table_p_string_and_table_p_not_string")
    {
        TypeId ty = reductionof("{ p: string } & { p: Not<string> }");
        CHECK("never" == toString(ty));
    }

    SUBCASE("table_x_table_p_string_and_table_x_table_p_not_number")
    {
        TypeId ty = reductionof("{ x: { p: string } } & { x: { p: Not<number> } }");
        CHECK("{| x: {| p: string |} |}" == toString(ty));
    }
} // intersections_with_negations

TEST_CASE_FIXTURE(ReductionFixture, "unions_without_negations")
{
    SUBCASE("never_or_string")
    {
        TypeId ty = reductionof("never | string");
        CHECK("string" == toString(ty));
    }

    SUBCASE("string_or_never")
    {
        TypeId ty = reductionof("string | never");
        CHECK("string" == toString(ty));
    }

    SUBCASE("unknown_or_string")
    {
        TypeId ty = reductionof("unknown | string");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("string_or_unknown")
    {
        TypeId ty = reductionof("string | unknown");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("any_or_string")
    {
        TypeId ty = reductionof("any | string");
        CHECK("any" == toString(ty));
    }

    SUBCASE("string_or_any")
    {
        TypeId ty = reductionof("string | any");
        CHECK("any" == toString(ty));
    }

    SUBCASE("string_or_string_and_number")
    {
        TypeId ty = reductionof("string | (string & number)");
        CHECK("string" == toString(ty));
    }

    SUBCASE("string_or_string")
    {
        TypeId ty = reductionof("string | string");
        CHECK("string" == toString(ty));
    }

    SUBCASE("string_or_number")
    {
        TypeId ty = reductionof("string | number");
        CHECK("number | string" == toString(ty));
    }

    SUBCASE("number_or_string")
    {
        TypeId ty = reductionof("number | string");
        CHECK("number | string" == toString(ty));
    }

    SUBCASE("string_or_number_or_string")
    {
        TypeId ty = reductionof("(string | number) | string");
        CHECK("number | string" == toString(ty));
    }

    SUBCASE("string_or_number_or_string_2")
    {
        TypeId ty = reductionof("string | (number | string)");
        CHECK("number | string" == toString(ty));
    }

    SUBCASE("string_or_string_or_number")
    {
        TypeId ty = reductionof("string | (string | number)");
        CHECK("number | string" == toString(ty));
    }

    SUBCASE("string_or_string_or_number_or_boolean")
    {
        TypeId ty = reductionof("string | (string | number | boolean)");
        CHECK("boolean | number | string" == toString(ty));
    }

    SUBCASE("string_or_string_or_boolean_or_number")
    {
        TypeId ty = reductionof("string | (string | boolean | number)");
        CHECK("boolean | number | string" == toString(ty));
    }

    SUBCASE("string_or_boolean_or_string_or_number")
    {
        TypeId ty = reductionof("string | (boolean | string | number)");
        CHECK("boolean | number | string" == toString(ty));
    }

    SUBCASE("boolean_or_string_or_number_or_string")
    {
        TypeId ty = reductionof("(boolean | string | number) | string");
        CHECK("boolean | number | string" == toString(ty));
    }

    SUBCASE("boolean_or_true")
    {
        TypeId ty = reductionof("boolean | true");
        CHECK("boolean" == toString(ty));
    }

    SUBCASE("boolean_or_false")
    {
        TypeId ty = reductionof("boolean | false");
        CHECK("boolean" == toString(ty));
    }

    SUBCASE("boolean_or_true_or_false")
    {
        TypeId ty = reductionof("boolean | true | false");
        CHECK("boolean" == toString(ty));
    }

    SUBCASE("string_or_a")
    {
        TypeId ty = reductionof(R"(string | "a")");
        CHECK("string" == toString(ty));
    }

    SUBCASE("a_or_a")
    {
        TypeId ty = reductionof(R"("a" | "a")");
        CHECK(R"("a")" == toString(ty));
    }

    SUBCASE("a_or_b")
    {
        TypeId ty = reductionof(R"("a" | "b")");
        CHECK(R"("a" | "b")" == toString(ty));
    }

    SUBCASE("a_or_b_or_string")
    {
        TypeId ty = reductionof(R"("a" | "b" | string)");
        CHECK("string" == toString(ty));
    }

    SUBCASE("unknown_or_any")
    {
        TypeId ty = reductionof("unknown | any");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("any_or_unknown")
    {
        TypeId ty = reductionof("any | unknown");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("function_type_or_function")
    {
        TypeId ty = reductionof("() -> () | fun");
        CHECK("function" == toString(ty));
    }

    SUBCASE("function_or_string")
    {
        TypeId ty = reductionof("fun | string");
        CHECK("function | string" == toString(ty));
    }

    SUBCASE("parent_or_child")
    {
        TypeId ty = reductionof("Parent | Child");
        CHECK("Parent" == toString(ty));
    }

    SUBCASE("child_or_parent")
    {
        TypeId ty = reductionof("Child | Parent");
        CHECK("Parent" == toString(ty));
    }

    SUBCASE("parent_or_unrelated")
    {
        TypeId ty = reductionof("Parent | Unrelated");
        CHECK("Parent | Unrelated" == toString(ty));
    }

    SUBCASE("parent_or_child_or_unrelated")
    {
        TypeId ty = reductionof("Parent | Child | Unrelated");
        CHECK("Parent | Unrelated" == toString(ty));
    }

    SUBCASE("parent_or_unrelated_or_child")
    {
        TypeId ty = reductionof("Parent | Unrelated | Child");
        CHECK("Parent | Unrelated" == toString(ty));
    }

    SUBCASE("parent_or_child_or_unrelated_or_child")
    {
        TypeId ty = reductionof("Parent | Child | Unrelated | Child");
        CHECK("Parent | Unrelated" == toString(ty));
    }

    SUBCASE("string_or_true")
    {
        TypeId ty = reductionof("string | true");
        CHECK("string | true" == toString(ty));
    }

    SUBCASE("string_or_function")
    {
        TypeId ty = reductionof("string | () -> ()");
        CHECK("(() -> ()) | string" == toString(ty));
    }

    SUBCASE("string_or_err")
    {
        TypeId ty = reductionof("string | err");
        CHECK("*error-type* | string" == toString(ty));
    }
} // unions_without_negations

TEST_CASE_FIXTURE(ReductionFixture, "unions_with_negations")
{
    SUBCASE("string_or_not_string")
    {
        TypeId ty = reductionof("string | Not<string>");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("not_string_or_string")
    {
        TypeId ty = reductionof("Not<string> | string");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("not_number_or_string")
    {
        TypeId ty = reductionof("Not<number> | string");
        CHECK("~number" == toString(ty));
    }

    SUBCASE("string_or_not_number")
    {
        TypeId ty = reductionof("string | Not<number>");
        CHECK("~number" == toString(ty));
    }

    SUBCASE("not_hi_or_string_and_not_hi")
    {
        TypeId ty = reductionof(R"(Not<"hi"> | (string & Not<"hi">))");
        CHECK(R"(~"hi")" == toString(ty));
    }

    SUBCASE("string_and_not_hi_or_not_hi")
    {
        TypeId ty = reductionof(R"((string & Not<"hi">) | Not<"hi">)");
        CHECK(R"(~"hi")" == toString(ty));
    }

    SUBCASE("string_or_not_never")
    {
        TypeId ty = reductionof("string | Not<never>");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("not_a_or_not_a")
    {
        TypeId ty = reductionof(R"(Not<"a"> | Not<"a">)");
        CHECK(R"(~"a")" == toString(ty));
    }

    SUBCASE("not_a_or_a")
    {
        TypeId ty = reductionof(R"(Not<"a"> | "a")");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("a_or_not_a")
    {
        TypeId ty = reductionof(R"("a" | Not<"a">)");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("not_a_or_string")
    {
        TypeId ty = reductionof(R"(Not<"a"> | string)");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("string_or_not_a")
    {
        TypeId ty = reductionof(R"(string | Not<"a">)");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("not_string_or_a")
    {
        TypeId ty = reductionof(R"(Not<string> | "a")");
        CHECK(R"("a" | ~string)" == toString(ty));
    }

    SUBCASE("a_or_not_string")
    {
        TypeId ty = reductionof(R"("a" | Not<string>)");
        CHECK(R"("a" | ~string)" == toString(ty));
    }

    SUBCASE("not_number_or_a")
    {
        TypeId ty = reductionof(R"(Not<number> | "a")");
        CHECK("~number" == toString(ty));
    }

    SUBCASE("a_or_not_number")
    {
        TypeId ty = reductionof(R"("a" | Not<number>)");
        CHECK("~number" == toString(ty));
    }

    SUBCASE("not_a_or_not_b")
    {
        TypeId ty = reductionof(R"(Not<"a"> | Not<"b">)");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("boolean_or_not_false")
    {
        TypeId ty = reductionof("boolean | Not<false>");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("boolean_or_not_true")
    {
        TypeId ty = reductionof("boolean | Not<true>");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("false_or_not_false")
    {
        TypeId ty = reductionof("false | Not<false>");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("true_or_not_false")
    {
        TypeId ty = reductionof("true | Not<false>");
        CHECK("~false" == toString(ty));
    }

    SUBCASE("not_boolean_or_true")
    {
        TypeId ty = reductionof("Not<boolean> | true");
        CHECK("~false" == toString(ty));
    }

    SUBCASE("not_false_or_not_boolean")
    {
        TypeId ty = reductionof("Not<false> | Not<boolean>");
        CHECK("~false" == toString(ty));
    }

    SUBCASE("function_type_or_not_function")
    {
        TypeId ty = reductionof("() -> () | Not<fun>");
        CHECK("(() -> ()) | ~function" == toString(ty));
    }

    SUBCASE("not_parent_or_child")
    {
        TypeId ty = reductionof("Not<Parent> | Child");
        CHECK("Child | ~Parent" == toString(ty));
    }

    SUBCASE("child_or_not_parent")
    {
        TypeId ty = reductionof("Child | Not<Parent>");
        CHECK("Child | ~Parent" == toString(ty));
    }

    SUBCASE("parent_or_not_child")
    {
        TypeId ty = reductionof("Parent | Not<Child>");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("not_child_or_parent")
    {
        TypeId ty = reductionof("Not<Child> | Parent");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("parent_or_not_unrelated")
    {
        TypeId ty = reductionof("Parent | Not<Unrelated>");
        CHECK("~Unrelated" == toString(ty));
    }

    SUBCASE("not_string_or_string_and_not_a")
    {
        TypeId ty = reductionof(R"(Not<string> | (string & Not<"a">))");
        CHECK(R"(~"a")" == toString(ty));
    }

    SUBCASE("not_string_or_not_string")
    {
        TypeId ty = reductionof("Not<string> | Not<string>");
        CHECK("~string" == toString(ty));
    }

    SUBCASE("not_string_or_not_number")
    {
        TypeId ty = reductionof("Not<string> | Not<number>");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("not_a_or_not_boolean")
    {
        TypeId ty = reductionof(R"(Not<"a"> | Not<boolean>)");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("not_a_or_boolean")
    {
        TypeId ty = reductionof(R"(Not<"a"> | boolean)");
        CHECK(R"(~"a")" == toString(ty));
    }

    SUBCASE("string_or_err")
    {
        TypeId ty = reductionof("string | Not<err>");
        CHECK("string | ~*error-type*" == toString(ty));
    }
} // unions_with_negations

TEST_CASE_FIXTURE(ReductionFixture, "tables")
{
    SUBCASE("reduce_props")
    {
        ToStringOptions opts;
        opts.exhaustive = true;

        TypeId ty = reductionof("{ x: string | string, y: number | number }");
        CHECK("{| x: string, y: number |}" == toString(ty, opts));
    }

    SUBCASE("reduce_indexers")
    {
        ToStringOptions opts;
        opts.exhaustive = true;

        TypeId ty = reductionof("{ [string | string]: number | number }");
        CHECK("{| [string]: number |}" == toString(ty, opts));
    }

    SUBCASE("reduce_instantiated_type_parameters")
    {
        check(R"(
            type Foo<T> = { x: T }
            local foo: Foo<string | string> = { x = "hello" }
        )");

        TypeId ty = reductionof(requireType("foo"));
        CHECK("Foo<string>" == toString(ty));
    }

    SUBCASE("reduce_instantiated_type_pack_parameters")
    {
        check(R"(
            type Foo<T...> = { x: () -> T... }
            local foo: Foo<string | string, number | number> = { x = function() return "hi", 5 end }
        )");

        TypeId ty = reductionof(requireType("foo"));
        CHECK("Foo<string, number>" == toString(ty));
    }

    SUBCASE("reduce_tables_within_tables")
    {
        ToStringOptions opts;
        opts.exhaustive = true;

        TypeId ty = reductionof("{ x: { y: string & number } }");
        CHECK("{| x: {| y: never |} |}" == toString(ty, opts));
    }
}

TEST_CASE_FIXTURE(ReductionFixture, "metatables")
{
    SUBCASE("reduce_table_part")
    {
        TableType table;
        table.props["x"] = {arena.addType(UnionType{{builtinTypes->stringType, builtinTypes->stringType}})};
        TypeId tableTy = arena.addType(std::move(table));

        TypeId ty = reductionof(arena.addType(MetatableType{tableTy, arena.addType(TableType{})}));
        CHECK("{ @metatable {  }, { x: string } }" == toString(ty));
    }

    SUBCASE("reduce_metatable_part")
    {
        TableType table;
        table.props["x"] = {arena.addType(UnionType{{builtinTypes->stringType, builtinTypes->stringType}})};
        TypeId tableTy = arena.addType(std::move(table));

        TypeId ty = reductionof(arena.addType(MetatableType{arena.addType(TableType{}), tableTy}));
        CHECK("{ @metatable { x: string }, {  } }" == toString(ty));
    }
}

TEST_CASE_FIXTURE(ReductionFixture, "functions")
{
    SUBCASE("reduce_parameters")
    {
        TypeId ty = reductionof("(string | string) -> ()");
        CHECK("(string) -> ()" == toString(ty));
    }

    SUBCASE("reduce_returns")
    {
        TypeId ty = reductionof("() -> (string | string)");
        CHECK("() -> string" == toString(ty));
    }

    SUBCASE("reduce_parameters_and_returns")
    {
        TypeId ty = reductionof("(string | string) -> (number | number)");
        CHECK("(string) -> number" == toString(ty));
    }

    SUBCASE("reduce_tail")
    {
        TypeId ty = reductionof("() -> ...(string | string)");
        CHECK("() -> (...string)" == toString(ty));
    }

    SUBCASE("reduce_head_and_tail")
    {
        TypeId ty = reductionof("() -> (string | string, number | number, ...(boolean | boolean))");
        CHECK("() -> (string, number, ...boolean)" == toString(ty));
    }

    SUBCASE("reduce_overloaded_functions")
    {
        TypeId ty = reductionof("((number | number) -> ()) & ((string | string) -> ())");
        CHECK("((number) -> ()) & ((string) -> ())" == toString(ty));
    }
} // functions

TEST_CASE_FIXTURE(ReductionFixture, "negations")
{
    SUBCASE("not_unknown")
    {
        TypeId ty = reductionof("Not<unknown>");
        CHECK("never" == toString(ty));
    }

    SUBCASE("not_never")
    {
        TypeId ty = reductionof("Not<never>");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("not_any")
    {
        TypeId ty = reductionof("Not<any>");
        CHECK("any" == toString(ty));
    }

    SUBCASE("not_not_reduction")
    {
        TypeId ty = reductionof("Not<Not<never>>");
        CHECK("never" == toString(ty));
    }

    SUBCASE("not_string")
    {
        TypeId ty = reductionof("Not<string>");
        CHECK("~string" == toString(ty));
    }

    SUBCASE("not_string_or_number")
    {
        TypeId ty = reductionof("Not<string | number>");
        CHECK("~number & ~string" == toString(ty));
    }

    SUBCASE("not_string_and_number")
    {
        TypeId ty = reductionof("Not<string & number>");
        CHECK("unknown" == toString(ty));
    }

    SUBCASE("not_error")
    {
        TypeId ty = reductionof("Not<err>");
        CHECK("~*error-type*" == toString(ty));
    }
} // negations

TEST_CASE_FIXTURE(ReductionFixture, "discriminable_unions")
{
    SUBCASE("cat_or_dog_and_dog")
    {
        TypeId ty = reductionof(R"(({ tag: "cat", catfood: string } | { tag: "dog", dogfood: string }) & { tag: "dog" })");
        CHECK(R"({| dogfood: string, tag: "dog" |})" == toString(ty));
    }

    SUBCASE("cat_or_dog_and_not_dog")
    {
        TypeId ty = reductionof(R"(({ tag: "cat", catfood: string } | { tag: "dog", dogfood: string }) & { tag: Not<"dog"> })");
        CHECK(R"({| catfood: string, tag: "cat" |})" == toString(ty));
    }

    SUBCASE("string_or_number_and_number")
    {
        TypeId ty = reductionof("({ tag: string, a: number } | { tag: number, b: string }) & { tag: string }");
        CHECK("{| a: number, tag: string |}" == toString(ty));
    }

    SUBCASE("string_or_number_and_number")
    {
        TypeId ty = reductionof("({ tag: string, a: number } | { tag: number, b: string }) & { tag: number }");
        CHECK("{| b: string, tag: number |}" == toString(ty));
    }

    SUBCASE("child_or_unrelated_and_parent")
    {
        TypeId ty = reductionof("({ tag: Child, x: number } | { tag: Unrelated, y: string }) & { tag: Parent }");
        CHECK("{| tag: Child, x: number |}" == toString(ty));
    }

    SUBCASE("child_or_unrelated_and_not_parent")
    {
        TypeId ty = reductionof("({ tag: Child, x: number } | { tag: Unrelated, y: string }) & { tag: Not<Parent> }");
        CHECK("{| tag: Unrelated, y: string |}" == toString(ty));
    }
}

TEST_CASE_FIXTURE(ReductionFixture, "cycles")
{
    SUBCASE("recursively_defined_function")
    {
        check("type F = (f: F) -> ()");

        TypeId ty = reductionof(requireTypeAlias("F"));
        CHECK("(t1) -> () where t1 = (t1) -> ()" == toString(ty));
    }

    SUBCASE("recursively_defined_function_and_function")
    {
        check("type F = (f: F & fun) -> ()");

        TypeId ty = reductionof(requireTypeAlias("F"));
        CHECK("(t1) -> () where t1 = (function & t1) -> ()" == toString(ty));
    }

    SUBCASE("recursively_defined_table")
    {
        ToStringOptions opts;
        opts.exhaustive = true;

        check("type T = { x: T }");

        TypeId ty = reductionof(requireTypeAlias("T"));
        CHECK("{| x: t1 |} where t1 = {| x: t1 |}" == toString(ty, opts));
    }

    SUBCASE("recursively_defined_table_and_table")
    {
        ToStringOptions opts;
        opts.exhaustive = true;

        check("type T = { x: T & {} }");

        TypeId ty = reductionof(requireTypeAlias("T"));
        CHECK("{| x: t1 & {|  |} |} where t1 = {| x: t1 & {|  |} |}" == toString(ty, opts));
    }

    SUBCASE("recursively_defined_table_and_table_2")
    {
        ToStringOptions opts;
        opts.exhaustive = true;

        check("type T = { x: T } & { x: number }");

        TypeId ty = reductionof(requireTypeAlias("T"));
        CHECK("never" == toString(ty));
    }

    SUBCASE("recursively_defined_table_and_table_3")
    {
        ToStringOptions opts;
        opts.exhaustive = true;

        check("type T = { x: T } & { x: T }");

        TypeId ty = reductionof(requireTypeAlias("T"));
        CHECK("{| x: {| x: t1 |} & {| x: t1 |} & {| x: t2 & t2 & {| x: t1 |} & {| x: t1 |} |} |} where t1 = t2 & {| x: t1 |} ; t2 = {| x: t1 |}" ==
              toString(ty));
    }
}

TEST_CASE_FIXTURE(ReductionFixture, "stress_test_recursion_limits")
{
    TypeId ty = arena.addType(IntersectionType{{builtinTypes->numberType, builtinTypes->stringType}});
    for (size_t i = 0; i < 20'000; ++i)
    {
        TableType table;
        table.state = TableState::Sealed;
        table.props["x"] = {ty};
        ty = arena.addType(IntersectionType{{arena.addType(table), arena.addType(table)}});
    }

    CHECK(!reduction.reduce(ty));
}

TEST_SUITE_END();
