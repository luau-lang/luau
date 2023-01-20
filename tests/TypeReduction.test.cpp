// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeReduction.h"

#include "Fixture.h"
#include "doctest.h"

using namespace Luau;

namespace
{
struct ReductionFixture : Fixture
{
    TypeReductionOptions typeReductionOpts{/* allowTypeReductionsFromOtherArenas */ true};
    ToStringOptions toStringOpts{true};

    TypeArena arena;
    InternalErrorReporter iceHandler;
    UnifierSharedState unifierState{&iceHandler};
    TypeReduction reduction{NotNull{&arena}, builtinTypes, NotNull{&iceHandler}, typeReductionOpts};

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

    TypeId reductionof(const std::string& annotation)
    {
        check("type _Res = " + annotation);
        return reductionof(requireTypeAlias("_Res"));
    }

    std::string toStringFull(TypeId ty)
    {
        return toString(ty, toStringOpts);
    }
};
} // namespace

TEST_SUITE_BEGIN("TypeReductionTests");

TEST_CASE_FIXTURE(ReductionFixture, "cartesian_product_exceeded")
{
    ScopedFastInt sfi{"LuauTypeReductionCartesianProductLimit", 5};

    CheckResult result = check(R"(
        type T
            = string
            & (number | string | boolean)
            & (number | string | boolean)
    )");

    CHECK(!reduction.reduce(requireTypeAlias("T")));
    // LUAU_REQUIRE_ERROR_COUNT(1, result);
    // CHECK("Code is too complex to typecheck! Consider simplifying the code around this area" == toString(result.errors[0]));
}

TEST_CASE_FIXTURE(ReductionFixture, "cartesian_product_exceeded_with_normal_limit")
{
    CheckResult result = check(R"(
        type T
            = string                        -- 1         = 1
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

    CHECK(!reduction.reduce(requireTypeAlias("T")));
    // LUAU_REQUIRE_ERROR_COUNT(1, result);
    // CHECK("Code is too complex to typecheck! Consider simplifying the code around this area" == toString(result.errors[0]));
}

TEST_CASE_FIXTURE(ReductionFixture, "cartesian_product_is_zero")
{
    ScopedFastInt sfi{"LuauTypeReductionCartesianProductLimit", 5};

    CheckResult result = check(R"(
        type T
            = string
            & (number | string | boolean)
            & (number | string | boolean)
            & never
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
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

TEST_CASE_FIXTURE(ReductionFixture, "caching")
{
    SUBCASE("free_tables")
    {
        TypeId ty1 = arena.addType(TableType{});
        getMutable<TableType>(ty1)->state = TableState::Free;
        getMutable<TableType>(ty1)->props["x"] = {builtinTypes->stringType};

        TypeId ty2 = arena.addType(TableType{});
        getMutable<TableType>(ty2)->state = TableState::Sealed;

        TypeId intersectionTy = arena.addType(IntersectionType{{ty1, ty2}});

        CHECK("{- x: string -} & {|  |}" == toStringFull(reductionof(intersectionTy)));

        getMutable<TableType>(ty1)->state = TableState::Sealed;
        CHECK("{| x: string |}" == toStringFull(reductionof(intersectionTy)));
    }

    SUBCASE("unsealed_tables")
    {
        TypeId ty1 = arena.addType(TableType{});
        getMutable<TableType>(ty1)->state = TableState::Unsealed;
        getMutable<TableType>(ty1)->props["x"] = {builtinTypes->stringType};

        TypeId ty2 = arena.addType(TableType{});
        getMutable<TableType>(ty2)->state = TableState::Sealed;

        TypeId intersectionTy = arena.addType(IntersectionType{{ty1, ty2}});

        CHECK("{| x: string |}" == toStringFull(reductionof(intersectionTy)));

        getMutable<TableType>(ty1)->state = TableState::Sealed;
        CHECK("{| x: string |}" == toStringFull(reductionof(intersectionTy)));
    }

    SUBCASE("free_types")
    {
        TypeId ty1 = arena.freshType(nullptr);
        TypeId ty2 = arena.addType(TableType{});
        getMutable<TableType>(ty2)->state = TableState::Sealed;

        TypeId intersectionTy = arena.addType(IntersectionType{{ty1, ty2}});

        CHECK("a & {|  |}" == toStringFull(reductionof(intersectionTy)));

        *asMutable(ty1) = BoundType{ty2};
        CHECK("{|  |}" == toStringFull(reductionof(intersectionTy)));
    }

    SUBCASE("we_can_see_that_the_cache_works_if_we_mutate_a_normally_not_mutated_type")
    {
        TypeId ty1 = arena.addType(BoundType{builtinTypes->stringType});
        TypeId ty2 = builtinTypes->numberType;

        TypeId intersectionTy = arena.addType(IntersectionType{{ty1, ty2}});

        CHECK("never" == toStringFull(reductionof(intersectionTy))); // Bound<string> & number ~ never

        *asMutable(ty1) = BoundType{ty2};
        CHECK("never" == toStringFull(reductionof(intersectionTy))); // Bound<number> & number ~ number, but the cache is `never`.
    }

    SUBCASE("ptr_eq_irreducible_unions")
    {
        TypeId unionTy = arena.addType(UnionType{{builtinTypes->stringType, builtinTypes->numberType}});
        TypeId reducedTy = reductionof(unionTy);
        REQUIRE(unionTy == reducedTy);
    }

    SUBCASE("ptr_eq_irreducible_intersections")
    {
        TypeId intersectionTy = arena.addType(IntersectionType{{builtinTypes->stringType, arena.addType(GenericType{"G"})}});
        TypeId reducedTy = reductionof(intersectionTy);
        REQUIRE(intersectionTy == reducedTy);
    }

    SUBCASE("ptr_eq_free_table")
    {
        TypeId tableTy = arena.addType(TableType{});
        getMutable<TableType>(tableTy)->state = TableState::Free;

        TypeId reducedTy = reductionof(tableTy);
        REQUIRE(tableTy == reducedTy);
    }

    SUBCASE("ptr_eq_unsealed_table")
    {
        TypeId tableTy = arena.addType(TableType{});
        getMutable<TableType>(tableTy)->state = TableState::Unsealed;

        TypeId reducedTy = reductionof(tableTy);
        REQUIRE(tableTy == reducedTy);
    }
} // caching

TEST_CASE_FIXTURE(ReductionFixture, "intersections_without_negations")
{
    SUBCASE("string_and_string")
    {
        TypeId ty = reductionof("string & string");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("never_and_string")
    {
        TypeId ty = reductionof("never & string");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("string_and_never")
    {
        TypeId ty = reductionof("string & never");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("unknown_and_string")
    {
        TypeId ty = reductionof("unknown & string");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("string_and_unknown")
    {
        TypeId ty = reductionof("string & unknown");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("any_and_string")
    {
        TypeId ty = reductionof("any & string");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("string_and_any")
    {
        TypeId ty = reductionof("string & any");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("string_or_number_and_string")
    {
        TypeId ty = reductionof("(string | number) & string");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("string_and_string_or_number")
    {
        TypeId ty = reductionof("string & (string | number)");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("string_and_a")
    {
        TypeId ty = reductionof(R"(string & "a")");
        CHECK(R"("a")" == toStringFull(ty));
    }

    SUBCASE("boolean_and_true")
    {
        TypeId ty = reductionof("boolean & true");
        CHECK("true" == toStringFull(ty));
    }

    SUBCASE("boolean_and_a")
    {
        TypeId ty = reductionof(R"(boolean & "a")");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("a_and_a")
    {
        TypeId ty = reductionof(R"("a" & "a")");
        CHECK(R"("a")" == toStringFull(ty));
    }

    SUBCASE("a_and_b")
    {
        TypeId ty = reductionof(R"("a" & "b")");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("a_and_true")
    {
        TypeId ty = reductionof(R"("a" & true)");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("a_and_true")
    {
        TypeId ty = reductionof(R"(true & false)");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("function_type_and_function")
    {
        TypeId ty = reductionof("() -> () & fun");
        CHECK("() -> ()" == toStringFull(ty));
    }

    SUBCASE("function_type_and_string")
    {
        TypeId ty = reductionof("() -> () & string");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("parent_and_child")
    {
        TypeId ty = reductionof("Parent & Child");
        CHECK("Child" == toStringFull(ty));
    }

    SUBCASE("child_and_parent")
    {
        TypeId ty = reductionof("Child & Parent");
        CHECK("Child" == toStringFull(ty));
    }

    SUBCASE("child_and_unrelated")
    {
        TypeId ty = reductionof("Child & Unrelated");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("string_and_table")
    {
        TypeId ty = reductionof("string & {}");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("string_and_child")
    {
        TypeId ty = reductionof("string & Child");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("string_and_function")
    {
        TypeId ty = reductionof("string & () -> ()");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("function_and_table")
    {
        TypeId ty = reductionof("() -> () & {}");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("function_and_class")
    {
        TypeId ty = reductionof("() -> () & Child");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("function_and_function")
    {
        TypeId ty = reductionof("() -> () & () -> ()");
        CHECK("(() -> ()) & (() -> ())" == toStringFull(ty));
    }

    SUBCASE("table_and_table")
    {
        TypeId ty = reductionof("{} & {}");
        CHECK("{|  |}" == toStringFull(ty));
    }

    SUBCASE("table_and_metatable")
    {
        // No setmetatable in ReductionFixture, so we mix and match.
        BuiltinsFixture fixture;
        fixture.check(R"(
            type Ty = {} & typeof(setmetatable({}, {}))
        )");

        TypeId ty = reductionof(fixture.requireTypeAlias("Ty"));
        CHECK("{ @metatable {  }, {  } } & {|  |}" == toStringFull(ty));
    }

    SUBCASE("a_and_string")
    {
        TypeId ty = reductionof(R"("a" & string)");
        CHECK(R"("a")" == toStringFull(ty));
    }

    SUBCASE("reducible_function_and_function")
    {
        TypeId ty = reductionof("((string | string) -> (number | number)) & fun");
        CHECK("(string) -> number" == toStringFull(ty));
    }

    SUBCASE("string_and_error")
    {
        TypeId ty = reductionof("string & err");
        CHECK("*error-type* & string" == toStringFull(ty));
    }

    SUBCASE("table_p_string_and_table_p_number")
    {
        TypeId ty = reductionof("{ p: string } & { p: number }");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("table_p_string_and_table_p_string")
    {
        TypeId ty = reductionof("{ p: string } & { p: string }");
        CHECK("{| p: string |}" == toStringFull(ty));
    }

    SUBCASE("table_x_table_p_string_and_table_x_table_p_number")
    {
        TypeId ty = reductionof("{ x: { p: string } } & { x: { p: number } }");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("table_p_and_table_q")
    {
        TypeId ty = reductionof("{ p: string } & { q: number }");
        CHECK("{| p: string, q: number |}" == toStringFull(ty));
    }

    SUBCASE("table_tag_a_or_table_tag_b_and_table_b")
    {
        TypeId ty = reductionof("({ tag: string, a: number } | { tag: number, b: string }) & { b: string }");
        CHECK("{| a: number, b: string, tag: string |} | {| b: string, tag: number |}" == toStringFull(ty));
    }

    SUBCASE("table_string_number_indexer_and_table_string_number_indexer")
    {
        TypeId ty = reductionof("{ [string]: number } & { [string]: number }");
        CHECK("{| [string]: number |}" == toStringFull(ty));
    }

    SUBCASE("table_string_number_indexer_and_empty_table")
    {
        TypeId ty = reductionof("{ [string]: number } & {}");
        CHECK("{| [string]: number |}" == toStringFull(ty));
    }

    SUBCASE("empty_table_table_string_number_indexer")
    {
        TypeId ty = reductionof("{} & { [string]: number }");
        CHECK("{| [string]: number |}" == toStringFull(ty));
    }

    SUBCASE("string_number_indexer_and_number_number_indexer")
    {
        TypeId ty = reductionof("{ [string]: number } & { [number]: number }");
        CHECK("{number} & {| [string]: number |}" == toStringFull(ty));
    }

    SUBCASE("table_p_string_and_indexer_number_number")
    {
        TypeId ty = reductionof("{ p: string } & { [number]: number }");
        CHECK("{| [number]: number, p: string |}" == toStringFull(ty));
    }

    SUBCASE("table_p_string_and_indexer_string_number")
    {
        TypeId ty = reductionof("{ p: string } & { [string]: number }");
        CHECK("{| [string]: number, p: string |}" == toStringFull(ty));
    }

    SUBCASE("table_p_string_and_table_p_string_plus_indexer_string_number")
    {
        TypeId ty = reductionof("{ p: string } & { p: string, [string]: number }");
        CHECK("{| [string]: number, p: string |}" == toStringFull(ty));
    }

    SUBCASE("fresh_type_and_string")
    {
        TypeId freshTy = arena.freshType(nullptr);
        TypeId ty = reductionof(arena.addType(IntersectionType{{freshTy, builtinTypes->stringType}}));
        CHECK("a & string" == toStringFull(ty));
    }

    SUBCASE("string_and_fresh_type")
    {
        TypeId freshTy = arena.freshType(nullptr);
        TypeId ty = reductionof(arena.addType(IntersectionType{{builtinTypes->stringType, freshTy}}));
        CHECK("a & string" == toStringFull(ty));
    }

    SUBCASE("generic_and_string")
    {
        TypeId genericTy = arena.addType(GenericType{"G"});
        TypeId ty = reductionof(arena.addType(IntersectionType{{genericTy, builtinTypes->stringType}}));
        CHECK("G & string" == toStringFull(ty));
    }

    SUBCASE("string_and_generic")
    {
        TypeId genericTy = arena.addType(GenericType{"G"});
        TypeId ty = reductionof(arena.addType(IntersectionType{{builtinTypes->stringType, genericTy}}));
        CHECK("G & string" == toStringFull(ty));
    }

    SUBCASE("parent_and_child_or_parent_and_anotherchild_or_parent_and_unrelated")
    {
        TypeId ty = reductionof("Parent & (Child | AnotherChild | Unrelated)");
        CHECK("AnotherChild | Child" == toString(ty));
    }

    SUBCASE("parent_and_child_or_parent_and_anotherchild_or_parent_and_unrelated_2")
    {
        TypeId ty = reductionof("(Parent & Child) | (Parent & AnotherChild) | (Parent & Unrelated)");
        CHECK("AnotherChild | Child" == toString(ty));
    }
} // intersections_without_negations

TEST_CASE_FIXTURE(ReductionFixture, "intersections_with_negations")
{
    SUBCASE("nil_and_not_nil")
    {
        TypeId ty = reductionof("nil & Not<nil>");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("nil_and_not_false")
    {
        TypeId ty = reductionof("nil & Not<false>");
        CHECK("nil" == toStringFull(ty));
    }

    SUBCASE("string_or_nil_and_not_nil")
    {
        TypeId ty = reductionof("(string?) & Not<nil>");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("string_or_nil_and_not_false_or_nil")
    {
        TypeId ty = reductionof("(string?) & Not<false | nil>");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("string_or_nil_and_not_false_and_not_nil")
    {
        TypeId ty = reductionof("(string?) & Not<false> & Not<nil>");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("not_false_and_bool")
    {
        TypeId ty = reductionof("Not<false> & boolean");
        CHECK("true" == toStringFull(ty));
    }

    SUBCASE("function_type_and_not_function")
    {
        TypeId ty = reductionof("() -> () & Not<fun>");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("function_type_and_not_string")
    {
        TypeId ty = reductionof("() -> () & Not<string>");
        CHECK("() -> ()" == toStringFull(ty));
    }

    SUBCASE("not_a_and_string_or_nil")
    {
        TypeId ty = reductionof(R"(Not<"a"> & (string | nil))");
        CHECK(R"((string & ~"a")?)" == toStringFull(ty));
    }

    SUBCASE("not_a_and_a")
    {
        TypeId ty = reductionof(R"(Not<"a"> & "a")");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("not_a_and_b")
    {
        TypeId ty = reductionof(R"(Not<"a"> & "b")");
        CHECK(R"("b")" == toStringFull(ty));
    }

    SUBCASE("not_string_and_a")
    {
        TypeId ty = reductionof(R"(Not<string> & "a")");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("not_bool_and_true")
    {
        TypeId ty = reductionof("Not<boolean> & true");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("not_string_and_true")
    {
        TypeId ty = reductionof("Not<string> & true");
        CHECK("true" == toStringFull(ty));
    }

    SUBCASE("parent_and_not_child")
    {
        TypeId ty = reductionof("Parent & Not<Child>");
        CHECK("Parent & ~Child" == toStringFull(ty));
    }

    SUBCASE("not_child_and_parent")
    {
        TypeId ty = reductionof("Not<Child> & Parent");
        CHECK("Parent & ~Child" == toStringFull(ty));
    }

    SUBCASE("child_and_not_parent")
    {
        TypeId ty = reductionof("Child & Not<Parent>");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("not_parent_and_child")
    {
        TypeId ty = reductionof("Not<Parent> & Child");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("not_parent_and_unrelated")
    {
        TypeId ty = reductionof("Not<Parent> & Unrelated");
        CHECK("Unrelated" == toStringFull(ty));
    }

    SUBCASE("unrelated_and_not_parent")
    {
        TypeId ty = reductionof("Unrelated & Not<Parent>");
        CHECK("Unrelated" == toStringFull(ty));
    }

    SUBCASE("not_unrelated_and_parent")
    {
        TypeId ty = reductionof("Not<Unrelated> & Parent");
        CHECK("Parent" == toStringFull(ty));
    }

    SUBCASE("parent_and_not_unrelated")
    {
        TypeId ty = reductionof("Parent & Not<Unrelated>");
        CHECK("Parent" == toStringFull(ty));
    }

    SUBCASE("reducible_function_and_not_function")
    {
        TypeId ty = reductionof("((string | string) -> (number | number)) & Not<fun>");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("string_and_not_error")
    {
        TypeId ty = reductionof("string & Not<err>");
        CHECK("string & ~*error-type*" == toStringFull(ty));
    }

    SUBCASE("table_p_string_and_table_p_not_number")
    {
        TypeId ty = reductionof("{ p: string } & { p: Not<number> }");
        CHECK("{| p: string |}" == toStringFull(ty));
    }

    SUBCASE("table_p_string_and_table_p_not_string")
    {
        TypeId ty = reductionof("{ p: string } & { p: Not<string> }");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("table_x_table_p_string_and_table_x_table_p_not_number")
    {
        TypeId ty = reductionof("{ x: { p: string } } & { x: { p: Not<number> } }");
        CHECK("{| x: {| p: string |} |}" == toStringFull(ty));
    }
} // intersections_with_negations

TEST_CASE_FIXTURE(ReductionFixture, "unions_without_negations")
{
    SUBCASE("never_or_string")
    {
        TypeId ty = reductionof("never | string");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("string_or_never")
    {
        TypeId ty = reductionof("string | never");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("unknown_or_string")
    {
        TypeId ty = reductionof("unknown | string");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("string_or_unknown")
    {
        TypeId ty = reductionof("string | unknown");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("any_or_string")
    {
        TypeId ty = reductionof("any | string");
        CHECK("any" == toStringFull(ty));
    }

    SUBCASE("string_or_any")
    {
        TypeId ty = reductionof("string | any");
        CHECK("any" == toStringFull(ty));
    }

    SUBCASE("string_or_string_and_number")
    {
        TypeId ty = reductionof("string | (string & number)");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("string_or_string")
    {
        TypeId ty = reductionof("string | string");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("string_or_number")
    {
        TypeId ty = reductionof("string | number");
        CHECK("number | string" == toStringFull(ty));
    }

    SUBCASE("number_or_string")
    {
        TypeId ty = reductionof("number | string");
        CHECK("number | string" == toStringFull(ty));
    }

    SUBCASE("string_or_number_or_string")
    {
        TypeId ty = reductionof("(string | number) | string");
        CHECK("number | string" == toStringFull(ty));
    }

    SUBCASE("string_or_number_or_string_2")
    {
        TypeId ty = reductionof("string | (number | string)");
        CHECK("number | string" == toStringFull(ty));
    }

    SUBCASE("string_or_string_or_number")
    {
        TypeId ty = reductionof("string | (string | number)");
        CHECK("number | string" == toStringFull(ty));
    }

    SUBCASE("string_or_string_or_number_or_boolean")
    {
        TypeId ty = reductionof("string | (string | number | boolean)");
        CHECK("boolean | number | string" == toStringFull(ty));
    }

    SUBCASE("string_or_string_or_boolean_or_number")
    {
        TypeId ty = reductionof("string | (string | boolean | number)");
        CHECK("boolean | number | string" == toStringFull(ty));
    }

    SUBCASE("string_or_boolean_or_string_or_number")
    {
        TypeId ty = reductionof("string | (boolean | string | number)");
        CHECK("boolean | number | string" == toStringFull(ty));
    }

    SUBCASE("boolean_or_string_or_number_or_string")
    {
        TypeId ty = reductionof("(boolean | string | number) | string");
        CHECK("boolean | number | string" == toStringFull(ty));
    }

    SUBCASE("boolean_or_true")
    {
        TypeId ty = reductionof("boolean | true");
        CHECK("boolean" == toStringFull(ty));
    }

    SUBCASE("boolean_or_false")
    {
        TypeId ty = reductionof("boolean | false");
        CHECK("boolean" == toStringFull(ty));
    }

    SUBCASE("boolean_or_true_or_false")
    {
        TypeId ty = reductionof("boolean | true | false");
        CHECK("boolean" == toStringFull(ty));
    }

    SUBCASE("string_or_a")
    {
        TypeId ty = reductionof(R"(string | "a")");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("a_or_a")
    {
        TypeId ty = reductionof(R"("a" | "a")");
        CHECK(R"("a")" == toStringFull(ty));
    }

    SUBCASE("a_or_b")
    {
        TypeId ty = reductionof(R"("a" | "b")");
        CHECK(R"("a" | "b")" == toStringFull(ty));
    }

    SUBCASE("a_or_b_or_string")
    {
        TypeId ty = reductionof(R"("a" | "b" | string)");
        CHECK("string" == toStringFull(ty));
    }

    SUBCASE("unknown_or_any")
    {
        TypeId ty = reductionof("unknown | any");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("any_or_unknown")
    {
        TypeId ty = reductionof("any | unknown");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("function_type_or_function")
    {
        TypeId ty = reductionof("() -> () | fun");
        CHECK("function" == toStringFull(ty));
    }

    SUBCASE("function_or_string")
    {
        TypeId ty = reductionof("fun | string");
        CHECK("function | string" == toStringFull(ty));
    }

    SUBCASE("parent_or_child")
    {
        TypeId ty = reductionof("Parent | Child");
        CHECK("Parent" == toStringFull(ty));
    }

    SUBCASE("child_or_parent")
    {
        TypeId ty = reductionof("Child | Parent");
        CHECK("Parent" == toStringFull(ty));
    }

    SUBCASE("parent_or_unrelated")
    {
        TypeId ty = reductionof("Parent | Unrelated");
        CHECK("Parent | Unrelated" == toStringFull(ty));
    }

    SUBCASE("parent_or_child_or_unrelated")
    {
        TypeId ty = reductionof("Parent | Child | Unrelated");
        CHECK("Parent | Unrelated" == toStringFull(ty));
    }

    SUBCASE("parent_or_unrelated_or_child")
    {
        TypeId ty = reductionof("Parent | Unrelated | Child");
        CHECK("Parent | Unrelated" == toStringFull(ty));
    }

    SUBCASE("parent_or_child_or_unrelated_or_child")
    {
        TypeId ty = reductionof("Parent | Child | Unrelated | Child");
        CHECK("Parent | Unrelated" == toStringFull(ty));
    }

    SUBCASE("string_or_true")
    {
        TypeId ty = reductionof("string | true");
        CHECK("string | true" == toStringFull(ty));
    }

    SUBCASE("string_or_function")
    {
        TypeId ty = reductionof("string | () -> ()");
        CHECK("(() -> ()) | string" == toStringFull(ty));
    }

    SUBCASE("string_or_err")
    {
        TypeId ty = reductionof("string | err");
        CHECK("*error-type* | string" == toStringFull(ty));
    }
} // unions_without_negations

TEST_CASE_FIXTURE(ReductionFixture, "unions_with_negations")
{
    SUBCASE("string_or_not_string")
    {
        TypeId ty = reductionof("string | Not<string>");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("not_string_or_string")
    {
        TypeId ty = reductionof("Not<string> | string");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("not_number_or_string")
    {
        TypeId ty = reductionof("Not<number> | string");
        CHECK("~number" == toStringFull(ty));
    }

    SUBCASE("string_or_not_number")
    {
        TypeId ty = reductionof("string | Not<number>");
        CHECK("~number" == toStringFull(ty));
    }

    SUBCASE("not_hi_or_string_and_not_hi")
    {
        TypeId ty = reductionof(R"(Not<"hi"> | (string & Not<"hi">))");
        CHECK(R"(~"hi")" == toStringFull(ty));
    }

    SUBCASE("string_and_not_hi_or_not_hi")
    {
        TypeId ty = reductionof(R"((string & Not<"hi">) | Not<"hi">)");
        CHECK(R"(~"hi")" == toStringFull(ty));
    }

    SUBCASE("string_or_not_never")
    {
        TypeId ty = reductionof("string | Not<never>");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("not_a_or_not_a")
    {
        TypeId ty = reductionof(R"(Not<"a"> | Not<"a">)");
        CHECK(R"(~"a")" == toStringFull(ty));
    }

    SUBCASE("not_a_or_a")
    {
        TypeId ty = reductionof(R"(Not<"a"> | "a")");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("a_or_not_a")
    {
        TypeId ty = reductionof(R"("a" | Not<"a">)");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("not_a_or_string")
    {
        TypeId ty = reductionof(R"(Not<"a"> | string)");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("string_or_not_a")
    {
        TypeId ty = reductionof(R"(string | Not<"a">)");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("not_string_or_a")
    {
        TypeId ty = reductionof(R"(Not<string> | "a")");
        CHECK(R"("a" | ~string)" == toStringFull(ty));
    }

    SUBCASE("a_or_not_string")
    {
        TypeId ty = reductionof(R"("a" | Not<string>)");
        CHECK(R"("a" | ~string)" == toStringFull(ty));
    }

    SUBCASE("not_number_or_a")
    {
        TypeId ty = reductionof(R"(Not<number> | "a")");
        CHECK("~number" == toStringFull(ty));
    }

    SUBCASE("a_or_not_number")
    {
        TypeId ty = reductionof(R"("a" | Not<number>)");
        CHECK("~number" == toStringFull(ty));
    }

    SUBCASE("not_a_or_not_b")
    {
        TypeId ty = reductionof(R"(Not<"a"> | Not<"b">)");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("boolean_or_not_false")
    {
        TypeId ty = reductionof("boolean | Not<false>");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("boolean_or_not_true")
    {
        TypeId ty = reductionof("boolean | Not<true>");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("false_or_not_false")
    {
        TypeId ty = reductionof("false | Not<false>");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("true_or_not_false")
    {
        TypeId ty = reductionof("true | Not<false>");
        CHECK("~false" == toStringFull(ty));
    }

    SUBCASE("not_boolean_or_true")
    {
        TypeId ty = reductionof("Not<boolean> | true");
        CHECK("~false" == toStringFull(ty));
    }

    SUBCASE("not_false_or_not_boolean")
    {
        TypeId ty = reductionof("Not<false> | Not<boolean>");
        CHECK("~false" == toStringFull(ty));
    }

    SUBCASE("function_type_or_not_function")
    {
        TypeId ty = reductionof("() -> () | Not<fun>");
        CHECK("(() -> ()) | ~function" == toStringFull(ty));
    }

    SUBCASE("not_parent_or_child")
    {
        TypeId ty = reductionof("Not<Parent> | Child");
        CHECK("Child | ~Parent" == toStringFull(ty));
    }

    SUBCASE("child_or_not_parent")
    {
        TypeId ty = reductionof("Child | Not<Parent>");
        CHECK("Child | ~Parent" == toStringFull(ty));
    }

    SUBCASE("parent_or_not_child")
    {
        TypeId ty = reductionof("Parent | Not<Child>");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("not_child_or_parent")
    {
        TypeId ty = reductionof("Not<Child> | Parent");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("parent_or_not_unrelated")
    {
        TypeId ty = reductionof("Parent | Not<Unrelated>");
        CHECK("~Unrelated" == toStringFull(ty));
    }

    SUBCASE("not_string_or_string_and_not_a")
    {
        TypeId ty = reductionof(R"(Not<string> | (string & Not<"a">))");
        CHECK(R"(~"a")" == toStringFull(ty));
    }

    SUBCASE("not_string_or_not_string")
    {
        TypeId ty = reductionof("Not<string> | Not<string>");
        CHECK("~string" == toStringFull(ty));
    }

    SUBCASE("not_string_or_not_number")
    {
        TypeId ty = reductionof("Not<string> | Not<number>");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("not_a_or_not_boolean")
    {
        TypeId ty = reductionof(R"(Not<"a"> | Not<boolean>)");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("not_a_or_boolean")
    {
        TypeId ty = reductionof(R"(Not<"a"> | boolean)");
        CHECK(R"(~"a")" == toStringFull(ty));
    }

    SUBCASE("string_or_err")
    {
        TypeId ty = reductionof("string | Not<err>");
        CHECK("string | ~*error-type*" == toStringFull(ty));
    }
} // unions_with_negations

TEST_CASE_FIXTURE(ReductionFixture, "tables")
{
    SUBCASE("reduce_props")
    {
        TypeId ty = reductionof("{ x: string | string, y: number | number }");
        CHECK("{| x: string, y: number |}" == toStringFull(ty));
    }

    SUBCASE("reduce_indexers")
    {
        TypeId ty = reductionof("{ [string | string]: number | number }");
        CHECK("{| [string]: number |}" == toStringFull(ty));
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
        TypeId ty = reductionof("{ x: { y: string & number } }");
        CHECK("never" == toStringFull(ty));
    }
}

TEST_CASE_FIXTURE(ReductionFixture, "metatables")
{
    SUBCASE("reduce_table_part")
    {
        TableType table;
        table.state = TableState::Sealed;
        table.props["x"] = {arena.addType(UnionType{{builtinTypes->stringType, builtinTypes->stringType}})};
        TypeId tableTy = arena.addType(std::move(table));

        TypeId ty = reductionof(arena.addType(MetatableType{tableTy, arena.addType(TableType{})}));
        CHECK("{ @metatable {  }, {| x: string |} }" == toStringFull(ty));
    }

    SUBCASE("reduce_metatable_part")
    {
        TableType table;
        table.state = TableState::Sealed;
        table.props["x"] = {arena.addType(UnionType{{builtinTypes->stringType, builtinTypes->stringType}})};
        TypeId tableTy = arena.addType(std::move(table));

        TypeId ty = reductionof(arena.addType(MetatableType{arena.addType(TableType{}), tableTy}));
        CHECK("{ @metatable {| x: string |}, {  } }" == toStringFull(ty));
    }
}

TEST_CASE_FIXTURE(ReductionFixture, "functions")
{
    SUBCASE("reduce_parameters")
    {
        TypeId ty = reductionof("(string | string) -> ()");
        CHECK("(string) -> ()" == toStringFull(ty));
    }

    SUBCASE("reduce_returns")
    {
        TypeId ty = reductionof("() -> (string | string)");
        CHECK("() -> string" == toStringFull(ty));
    }

    SUBCASE("reduce_parameters_and_returns")
    {
        TypeId ty = reductionof("(string | string) -> (number | number)");
        CHECK("(string) -> number" == toStringFull(ty));
    }

    SUBCASE("reduce_tail")
    {
        TypeId ty = reductionof("() -> ...(string | string)");
        CHECK("() -> (...string)" == toStringFull(ty));
    }

    SUBCASE("reduce_head_and_tail")
    {
        TypeId ty = reductionof("() -> (string | string, number | number, ...(boolean | boolean))");
        CHECK("() -> (string, number, ...boolean)" == toStringFull(ty));
    }

    SUBCASE("reduce_overloaded_functions")
    {
        TypeId ty = reductionof("((number | number) -> ()) & ((string | string) -> ())");
        CHECK("((number) -> ()) & ((string) -> ())" == toStringFull(ty));
    }
} // functions

TEST_CASE_FIXTURE(ReductionFixture, "negations")
{
    SUBCASE("not_unknown")
    {
        TypeId ty = reductionof("Not<unknown>");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("not_never")
    {
        TypeId ty = reductionof("Not<never>");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("not_any")
    {
        TypeId ty = reductionof("Not<any>");
        CHECK("any" == toStringFull(ty));
    }

    SUBCASE("not_not_reduction")
    {
        TypeId ty = reductionof("Not<Not<never>>");
        CHECK("never" == toStringFull(ty));
    }

    SUBCASE("not_string")
    {
        TypeId ty = reductionof("Not<string>");
        CHECK("~string" == toStringFull(ty));
    }

    SUBCASE("not_string_or_number")
    {
        TypeId ty = reductionof("Not<string | number>");
        CHECK("~number & ~string" == toStringFull(ty));
    }

    SUBCASE("not_string_and_number")
    {
        TypeId ty = reductionof("Not<string & number>");
        CHECK("unknown" == toStringFull(ty));
    }

    SUBCASE("not_error")
    {
        TypeId ty = reductionof("Not<err>");
        CHECK("~*error-type*" == toStringFull(ty));
    }
} // negations

TEST_CASE_FIXTURE(ReductionFixture, "discriminable_unions")
{
    SUBCASE("cat_or_dog_and_dog")
    {
        TypeId ty = reductionof(R"(({ tag: "cat", catfood: string } | { tag: "dog", dogfood: string }) & { tag: "dog" })");
        CHECK(R"({| dogfood: string, tag: "dog" |})" == toStringFull(ty));
    }

    SUBCASE("cat_or_dog_and_not_dog")
    {
        TypeId ty = reductionof(R"(({ tag: "cat", catfood: string } | { tag: "dog", dogfood: string }) & { tag: Not<"dog"> })");
        CHECK(R"({| catfood: string, tag: "cat" |})" == toStringFull(ty));
    }

    SUBCASE("string_or_number_and_number")
    {
        TypeId ty = reductionof("({ tag: string, a: number } | { tag: number, b: string }) & { tag: string }");
        CHECK("{| a: number, tag: string |}" == toStringFull(ty));
    }

    SUBCASE("string_or_number_and_number")
    {
        TypeId ty = reductionof("({ tag: string, a: number } | { tag: number, b: string }) & { tag: number }");
        CHECK("{| b: string, tag: number |}" == toStringFull(ty));
    }

    SUBCASE("child_or_unrelated_and_parent")
    {
        TypeId ty = reductionof("({ tag: Child, x: number } | { tag: Unrelated, y: string }) & { tag: Parent }");
        CHECK("{| tag: Child, x: number |}" == toStringFull(ty));
    }

    SUBCASE("child_or_unrelated_and_not_parent")
    {
        TypeId ty = reductionof("({ tag: Child, x: number } | { tag: Unrelated, y: string }) & { tag: Not<Parent> }");
        CHECK("{| tag: Unrelated, y: string |}" == toStringFull(ty));
    }
}

TEST_CASE_FIXTURE(ReductionFixture, "cycles")
{
    SUBCASE("recursively_defined_function")
    {
        check("type F = (f: F) -> ()");

        TypeId ty = reductionof(requireTypeAlias("F"));
        CHECK("t1 where t1 = (t1) -> ()" == toStringFull(ty));
    }

    SUBCASE("recursively_defined_function_and_function")
    {
        check("type F = (f: F & fun) -> ()");

        TypeId ty = reductionof(requireTypeAlias("F"));
        CHECK("t1 where t1 = (function & t1) -> ()" == toStringFull(ty));
    }

    SUBCASE("recursively_defined_table")
    {
        check("type T = { x: T }");

        TypeId ty = reductionof(requireTypeAlias("T"));
        CHECK("t1 where t1 = {| x: t1 |}" == toStringFull(ty));
    }

    SUBCASE("recursively_defined_table_and_table")
    {
        check("type T = { x: T & {} }");

        TypeId ty = reductionof(requireTypeAlias("T"));
        CHECK("t1 where t1 = {| x: t1 & {|  |} |}" == toStringFull(ty));
    }

    SUBCASE("recursively_defined_table_and_table_2")
    {
        check("type T = { x: T } & { x: number }");

        TypeId ty = reductionof(requireTypeAlias("T"));
        CHECK("t1 where t1 = {| x: number |} & {| x: t1 |}" == toStringFull(ty));
    }

    SUBCASE("recursively_defined_table_and_table_3")
    {
        check("type T = { x: T } & { x: T }");

        TypeId ty = reductionof(requireTypeAlias("T"));
        CHECK("t1 where t1 = {| x: t1 |} & {| x: t1 |}" == toStringFull(ty));
    }
}

TEST_SUITE_END();
