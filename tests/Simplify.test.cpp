// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "doctest.h"

#include "Luau/Simplify.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauSimplifyAnyAndUnion)
LUAU_FASTFLAG(LuauSimplifyRefinementOfReadOnlyProperty)
LUAU_DYNAMIC_FASTINT(LuauSimplificationComplexityLimit)

namespace
{

struct SimplifyFixture : Fixture
{
    TypeArena _arena;
    const NotNull<TypeArena> arena{&_arena};

    ToStringOptions opts;

    Scope scope{getBuiltins()->anyTypePack};

    const TypeId anyTy = getBuiltins()->anyType;
    const TypeId unknownTy = getBuiltins()->unknownType;
    const TypeId neverTy = getBuiltins()->neverType;
    const TypeId errorTy = getBuiltins()->errorType;

    const TypeId functionTy = getBuiltins()->functionType;
    const TypeId tableTy = getBuiltins()->tableType;

    const TypeId numberTy = getBuiltins()->numberType;
    const TypeId stringTy = getBuiltins()->stringType;
    const TypeId booleanTy = getBuiltins()->booleanType;
    const TypeId nilTy = getBuiltins()->nilType;

    const TypeId classTy = getBuiltins()->externType;

    const TypeId trueTy = getBuiltins()->trueType;
    const TypeId falseTy = getBuiltins()->falseType;

    const TypeId truthyTy = getBuiltins()->truthyType;
    const TypeId falsyTy = getBuiltins()->falsyType;

    const TypeId freeTy = freshType(arena, getBuiltins(), &scope);
    const TypeId genericTy = arena->addType(GenericType{});
    const TypeId blockedTy = arena->addType(BlockedType{});
    const TypeId pendingTy = arena->addType(PendingExpansionType{{}, {}, {}, {}});

    const TypeId helloTy = arena->addType(SingletonType{StringSingleton{"hello"}});
    const TypeId worldTy = arena->addType(SingletonType{StringSingleton{"world"}});

    const TypePackId emptyTypePack = arena->addTypePack({});

    const TypeId fn1Ty = arena->addType(FunctionType{emptyTypePack, emptyTypePack});
    const TypeId fn2Ty = arena->addType(FunctionType{getBuiltins()->anyTypePack, emptyTypePack});

    TypeId parentClassTy = nullptr;
    TypeId childClassTy = nullptr;
    TypeId anotherChildClassTy = nullptr;
    TypeId unrelatedClassTy = nullptr;

    // This only affects type stringification.
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    SimplifyFixture()
    {
        createSomeExternTypes(getFrontend());

        parentClassTy = getFrontend().globals.globalScope->linearSearchForBinding("Parent")->typeId;
        childClassTy = getFrontend().globals.globalScope->linearSearchForBinding("Child")->typeId;
        anotherChildClassTy = getFrontend().globals.globalScope->linearSearchForBinding("AnotherChild")->typeId;
        unrelatedClassTy = getFrontend().globals.globalScope->linearSearchForBinding("Unrelated")->typeId;
    }

    TypeId intersect(TypeId a, TypeId b)
    {
        return simplifyIntersection(getBuiltins(), arena, a, b).result;
    }

    std::string intersectStr(TypeId a, TypeId b)
    {
        return toString(intersect(a, b), opts);
    }

    bool isIntersection(TypeId a)
    {
        return bool(get<IntersectionType>(follow(a)));
    }

    TypeId mkTable(std::map<Name, Property> propTypes)
    {
        TableType::Props props;
        for (const auto& [name, prop] : propTypes)
            props[name] = prop;

        return arena->addType(TableType{props, {}, TypeLevel{}, TableState::Sealed});
    }

    TypeId mkNegation(TypeId ty)
    {
        return arena->addType(NegationType{ty});
    }

    TypeId mkFunction(TypeId arg, TypeId ret)
    {
        return arena->addType(FunctionType{arena->addTypePack({arg}), arena->addTypePack({ret})});
    }

    TypeId union_(TypeId a, TypeId b)
    {
        return simplifyUnion(getBuiltins(), arena, a, b).result;
    }
};

} // namespace

TEST_SUITE_BEGIN("Simplify");

TEST_CASE_FIXTURE(SimplifyFixture, "overload_negation_refinement_is_never")
{
    TypeId f1 = mkFunction(stringTy, numberTy);
    TypeId f2 = mkFunction(numberTy, stringTy);
    TypeId intersection = arena->addType(IntersectionType{{f1, f2}});
    TypeId unionT = arena->addType(UnionType{{errorTy, functionTy}});
    TypeId negationT = mkNegation(unionT);
    // The intersection of string -> number & number -> string, ~(error | function)
    CHECK(neverTy == intersect(intersection, negationT));
}

TEST_CASE_FIXTURE(SimplifyFixture, "unknown_and_other_tops_and_bottom_types")
{

    CHECK(unknownTy == intersect(unknownTy, unknownTy));

    CHECK("any" == intersectStr(unknownTy, anyTy));
    CHECK("any" == intersectStr(anyTy, unknownTy));

    CHECK(neverTy == intersect(unknownTy, neverTy));
    CHECK(neverTy == intersect(neverTy, unknownTy));

    CHECK(errorTy == intersect(unknownTy, errorTy));
    CHECK(errorTy == intersect(errorTy, unknownTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "nil")
{
    CHECK(nilTy == intersect(nilTy, nilTy));
    CHECK(neverTy == intersect(nilTy, numberTy));
    CHECK(neverTy == intersect(nilTy, trueTy));
    CHECK(neverTy == intersect(nilTy, tableTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "boolean_singletons")
{
    CHECK(trueTy == intersect(trueTy, booleanTy));
    CHECK(trueTy == intersect(booleanTy, trueTy));

    CHECK(falseTy == intersect(falseTy, booleanTy));
    CHECK(falseTy == intersect(booleanTy, falseTy));

    CHECK(neverTy == intersect(falseTy, trueTy));
    CHECK(neverTy == intersect(trueTy, falseTy));

    CHECK(booleanTy == union_(trueTy, booleanTy));
    CHECK(booleanTy == union_(booleanTy, trueTy));
    CHECK(booleanTy == union_(falseTy, booleanTy));
    CHECK(booleanTy == union_(booleanTy, falseTy));
    CHECK(booleanTy == union_(falseTy, trueTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "boolean_and_truthy_and_falsy")
{
    TypeId optionalBooleanTy = arena->addType(UnionType{{booleanTy, nilTy}});

    CHECK(trueTy == intersect(booleanTy, truthyTy));

    CHECK(trueTy == intersect(optionalBooleanTy, truthyTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "any_and_indeterminate_types")
{
    CHECK("'a | *error-type*" == intersectStr(anyTy, freeTy));
    CHECK("'a | *error-type*" == intersectStr(freeTy, anyTy));

    CHECK("*error-type* | b" == intersectStr(anyTy, genericTy));
    CHECK("*error-type* | b" == intersectStr(genericTy, anyTy));

    auto anyRhsBlocked = get<UnionType>(intersect(anyTy, blockedTy));
    auto anyLhsBlocked = get<UnionType>(intersect(blockedTy, anyTy));

    REQUIRE(anyRhsBlocked);
    REQUIRE(anyRhsBlocked->options.size() == 2);
    CHECK(blockedTy == anyRhsBlocked->options[0]);
    CHECK(errorTy == anyRhsBlocked->options[1]);

    REQUIRE(anyLhsBlocked);
    REQUIRE(anyLhsBlocked->options.size() == 2);
    CHECK(blockedTy == anyLhsBlocked->options[0]);
    CHECK(errorTy == anyLhsBlocked->options[1]);

    auto anyRhsPending = get<UnionType>(intersect(anyTy, pendingTy));
    auto anyLhsPending = get<UnionType>(intersect(pendingTy, anyTy));

    REQUIRE(anyRhsPending);
    REQUIRE(anyRhsPending->options.size() == 2);
    CHECK(pendingTy == anyRhsPending->options[0]);
    CHECK(errorTy == anyRhsPending->options[1]);

    REQUIRE(anyLhsPending);
    REQUIRE(anyLhsPending->options.size() == 2);
    CHECK(pendingTy == anyLhsPending->options[0]);
    CHECK(errorTy == anyLhsPending->options[1]);
}

TEST_CASE_FIXTURE(SimplifyFixture, "union_where_lhs_elements_are_a_subset_of_the_rhs")
{
    TypeId lhs = union_(numberTy, stringTy);
    TypeId rhs = union_(stringTy, numberTy);

    CHECK("number | string" == toString(union_(lhs, rhs)));
}

TEST_CASE_FIXTURE(SimplifyFixture, "unknown_and_indeterminate_types")
{
    CHECK(freeTy == intersect(unknownTy, freeTy));
    CHECK(freeTy == intersect(freeTy, unknownTy));

    CHECK(genericTy == intersect(unknownTy, genericTy));
    CHECK(genericTy == intersect(genericTy, unknownTy));

    CHECK(blockedTy == intersect(unknownTy, blockedTy));
    CHECK(blockedTy == intersect(unknownTy, blockedTy));

    CHECK(pendingTy == intersect(unknownTy, pendingTy));
    CHECK(pendingTy == intersect(unknownTy, pendingTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "unknown_and_concrete")
{
    CHECK(numberTy == intersect(numberTy, unknownTy));
    CHECK(numberTy == intersect(unknownTy, numberTy));
    CHECK(trueTy == intersect(trueTy, unknownTy));
    CHECK(trueTy == intersect(unknownTy, trueTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "error_and_other_tops_and_bottom_types")
{
    CHECK(errorTy == intersect(errorTy, errorTy));

    CHECK(errorTy == intersect(errorTy, anyTy));
    CHECK(errorTy == intersect(anyTy, errorTy));

    CHECK(neverTy == intersect(errorTy, neverTy));
    CHECK(neverTy == intersect(neverTy, errorTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "error_and_indeterminate_types")
{
    CHECK("'a & *error-type*" == intersectStr(errorTy, freeTy));
    CHECK("'a & *error-type*" == intersectStr(freeTy, errorTy));

    CHECK("*error-type* & b" == intersectStr(errorTy, genericTy));
    CHECK("*error-type* & b" == intersectStr(genericTy, errorTy));

    CHECK(isIntersection(intersect(errorTy, blockedTy)));
    CHECK(isIntersection(intersect(blockedTy, errorTy)));

    CHECK(isIntersection(intersect(errorTy, pendingTy)));
    CHECK(isIntersection(intersect(pendingTy, errorTy)));
}

TEST_CASE_FIXTURE(SimplifyFixture, "unknown_and_concrete")
{
    CHECK(neverTy == intersect(numberTy, errorTy));
    CHECK(neverTy == intersect(errorTy, numberTy));
    CHECK(neverTy == intersect(trueTy, errorTy));
    CHECK(neverTy == intersect(errorTy, trueTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "primitives")
{
    // This shouldn't be possible, but we'll make it work even if it is.
    TypeId numberTyDuplicate = arena->addType(PrimitiveType{PrimitiveType::Number});

    CHECK(numberTy == intersect(numberTy, numberTyDuplicate));
    CHECK(neverTy == intersect(numberTy, stringTy));

    CHECK(neverTy == intersect(neverTy, numberTy));
    CHECK(neverTy == intersect(numberTy, neverTy));

    CHECK(neverTy == intersect(neverTy, functionTy));
    CHECK(neverTy == intersect(functionTy, neverTy));

    CHECK(neverTy == intersect(neverTy, tableTy));
    CHECK(neverTy == intersect(tableTy, neverTy));

    CHECK("*error-type* | number" == intersectStr(anyTy, numberTy));
    CHECK("*error-type* | number" == intersectStr(numberTy, anyTy));

    CHECK(neverTy == intersect(stringTy, nilTy));
    CHECK(neverTy == intersect(nilTy, stringTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "primitives_and_falsy")
{
    CHECK(neverTy == intersect(numberTy, falsyTy));
    CHECK(neverTy == intersect(falsyTy, numberTy));

    CHECK(nilTy == intersect(nilTy, falsyTy));
    CHECK(nilTy == intersect(falsyTy, nilTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "primitives_and_singletons")
{
    CHECK(helloTy == intersect(helloTy, stringTy));
    CHECK(helloTy == intersect(stringTy, helloTy));

    CHECK(neverTy == intersect(worldTy, helloTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "functions")
{
    CHECK(fn1Ty == intersect(fn1Ty, functionTy));
    CHECK(fn1Ty == intersect(functionTy, fn1Ty));

    // Intersections of functions are super weird if you think about it.
    CHECK("(() -> ()) & ((...any) -> ())" == intersectStr(fn1Ty, fn2Ty));
}

TEST_CASE_FIXTURE(SimplifyFixture, "negated_top_function_type")
{
    TypeId negatedFunctionTy = mkNegation(functionTy);

    CHECK(numberTy == intersect(numberTy, negatedFunctionTy));
    CHECK(numberTy == intersect(negatedFunctionTy, numberTy));

    CHECK(falsyTy == intersect(falsyTy, negatedFunctionTy));
    CHECK(falsyTy == intersect(negatedFunctionTy, falsyTy));

    TypeId f = mkFunction(stringTy, numberTy);

    CHECK(neverTy == intersect(f, negatedFunctionTy));
    CHECK(neverTy == intersect(negatedFunctionTy, f));
}

TEST_CASE_FIXTURE(SimplifyFixture, "optional_overloaded_function_and_top_function")
{
    // (((number) -> string) & ((string) -> number))? & ~function

    TypeId f1 = mkFunction(numberTy, stringTy);
    TypeId f2 = mkFunction(stringTy, numberTy);

    TypeId f12 = arena->addType(IntersectionType{{f1, f2}});

    TypeId t = arena->addType(UnionType{{f12, nilTy}});

    TypeId notFunctionTy = mkNegation(functionTy);

    CHECK(nilTy == intersect(t, notFunctionTy));
    CHECK(nilTy == intersect(notFunctionTy, t));
}

TEST_CASE_FIXTURE(SimplifyFixture, "negated_function_does_not_intersect_cleanly_with_truthy")
{
    // ~function & ~(false?)
    // ~function & ~(false | nil)
    // ~function & ~false & ~nil

    TypeId negatedFunctionTy = mkNegation(functionTy);
    CHECK(isIntersection(intersect(negatedFunctionTy, truthyTy)));
}

TEST_CASE_FIXTURE(SimplifyFixture, "tables")
{
    TypeId t1 = mkTable({{"tag", stringTy}});

    CHECK(t1 == intersect(t1, tableTy));
    CHECK(neverTy == intersect(t1, functionTy));

    TypeId t2 = mkTable({{"tag", helloTy}});

    CHECK(t2 == intersect(t1, t2));
    CHECK(t2 == intersect(t2, t1));

    TypeId t3 = mkTable({});
    // {tag : string} intersect {}
    CHECK(t1 == intersect(t1, t3));
    CHECK(t1 == intersect(t3, t1));
}

TEST_CASE_FIXTURE(SimplifyFixture, "combine_disjoint_sealed_tables")
{
    TypeId t1 = mkTable({{"prop", stringTy}});
    TypeId t2 = mkTable({{"second_prop", numberTy}});

    CHECK("{ prop: string, second_prop: number }" == toString(intersect(t1, t2)));
}

TEST_CASE_FIXTURE(SimplifyFixture, "non_disjoint_tables_do_not_simplify")
{
    TypeId t1 = mkTable({{"prop", stringTy}});
    TypeId t2 = mkTable({{"prop", unknownTy}, {"second_prop", numberTy}});

    CHECK("{ prop: string } & { prop: unknown, second_prop: number }" == toString(intersect(t1, t2)));
}

// Simplification has an extra code path especially for intersections with
// single-property tables, so it's worthwhile to separately test the case where
// both tables have multiple properties.
TEST_CASE_FIXTURE(SimplifyFixture, "non_disjoint_tables_do_not_simplify_2")
{
    TypeId t1 = mkTable({{"prop", stringTy}, {"third_prop", numberTy}});
    TypeId t2 = mkTable({{"prop", unknownTy}, {"second_prop", numberTy}});

    CHECK("{ prop: string, third_prop: number } & { prop: unknown, second_prop: number }" == toString(intersect(t1, t2)));
}

TEST_CASE_FIXTURE(SimplifyFixture, "tables_and_top_table")
{
    TypeId notTableType = mkNegation(tableTy);
    TypeId t1 = mkTable({{"prop", stringTy}, {"another", numberTy}});

    CHECK(t1 == intersect(t1, tableTy));
    CHECK(t1 == intersect(tableTy, t1));

    CHECK(neverTy == intersect(t1, notTableType));
    CHECK(neverTy == intersect(notTableType, t1));
}

TEST_CASE_FIXTURE(SimplifyFixture, "tables_and_truthy")
{
    TypeId t1 = mkTable({{"prop", stringTy}, {"another", numberTy}});

    CHECK(t1 == intersect(t1, truthyTy));
    CHECK(t1 == intersect(truthyTy, t1));
}

TEST_CASE_FIXTURE(SimplifyFixture, "table_with_a_tag")
{
    // {tag: string, prop: number} & {tag: "hello"}
    // I think we can decline to simplify this:
    TypeId t1 = mkTable({{"tag", stringTy}, {"prop", numberTy}});
    TypeId t2 = mkTable({{"tag", helloTy}});

    CHECK("{ prop: number, tag: string } & { tag: \"hello\" }" == intersectStr(t1, t2));
    CHECK("{ prop: number, tag: string } & { tag: \"hello\" }" == intersectStr(t2, t1));
}

TEST_CASE_FIXTURE(SimplifyFixture, "nested_table_tag_test")
{
    TypeId t1 = mkTable({
        {"subtable",
         mkTable({
             {"tag", helloTy},
             {"subprop", numberTy},
         })},
        {"prop", stringTy},
    });
    TypeId t2 = mkTable({
        {"subtable",
         mkTable({
             {"tag", helloTy},
         })},
    });

    CHECK(t1 == intersect(t1, t2));
    CHECK(t1 == intersect(t2, t1));
}

TEST_CASE_FIXTURE(SimplifyFixture, "union")
{
    TypeId t1 = arena->addType(UnionType{{numberTy, stringTy, nilTy, tableTy}});

    CHECK(nilTy == intersect(t1, nilTy));
    // CHECK(nilTy == intersect(nilTy, t1)); // TODO?

    CHECK(getBuiltins()->stringType == intersect(getBuiltins()->optionalStringType, truthyTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "two_unions")
{
    ScopedFastInt sfi{DFInt::LuauSimplificationComplexityLimit, 10};
    TypeId t1 = arena->addType(UnionType{{numberTy, booleanTy, stringTy, nilTy, tableTy}});

    CHECK("false?" == intersectStr(t1, falsyTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "curious_union")
{
    // (a & false) | (a & nil)
    TypeId curious =
        arena->addType(UnionType{{arena->addType(IntersectionType{{freeTy, falseTy}}), arena->addType(IntersectionType{{freeTy, nilTy}})}});

    CHECK("('a & false) | ('a & nil) | number" == toString(union_(curious, numberTy)));
}

TEST_CASE_FIXTURE(SimplifyFixture, "negations")
{
    TypeId notNumberTy = mkNegation(numberTy);
    TypeId notStringTy = mkNegation(stringTy);

    CHECK(neverTy == intersect(numberTy, notNumberTy));

    CHECK(numberTy == intersect(numberTy, notStringTy));
    CHECK(numberTy == intersect(notStringTy, numberTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "top_class_type")
{
    CHECK(neverTy == intersect(classTy, stringTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "extern_types")
{
    CHECK(childClassTy == intersect(childClassTy, parentClassTy));
    CHECK(childClassTy == intersect(parentClassTy, childClassTy));

    CHECK(parentClassTy == union_(childClassTy, parentClassTy));
    CHECK(parentClassTy == union_(parentClassTy, childClassTy));

    CHECK(neverTy == intersect(childClassTy, unrelatedClassTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "negations_of_extern_types")
{
    TypeId notChildClassTy = mkNegation(childClassTy);
    TypeId notParentClassTy = mkNegation(parentClassTy);

    CHECK(neverTy == intersect(childClassTy, notParentClassTy));
    CHECK(neverTy == intersect(notParentClassTy, childClassTy));

    CHECK("Parent & ~Child" == intersectStr(notChildClassTy, parentClassTy));
    CHECK("Parent & ~Child" == intersectStr(parentClassTy, notChildClassTy));

    CHECK(notParentClassTy == intersect(notChildClassTy, notParentClassTy));
    CHECK(notParentClassTy == intersect(notParentClassTy, notChildClassTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "intersection_of_intersection_of_a_free_type_can_result_in_removal_of_that_free_type")
{
    // a & string and number
    // (a & number) & (string & number)

    TypeId t1 = arena->addType(IntersectionType{{freeTy, stringTy}});

    CHECK(neverTy == intersect(t1, numberTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "some_tables_are_really_never")
{
    TypeId notAnyTy = mkNegation(anyTy);

    TypeId t1 = mkTable({{"someKey", notAnyTy}});

    CHECK(neverTy == intersect(t1, numberTy));
    CHECK(neverTy == intersect(numberTy, t1));
    CHECK(t1 == intersect(t1, t1));

    TypeId notUnknownTy = mkNegation(unknownTy);

    TypeId t2 = mkTable({{"someKey", notUnknownTy}});

    CHECK(neverTy == intersect(t2, numberTy));
    CHECK(neverTy == intersect(numberTy, t2));
    CHECK(neverTy == intersect(t2, t2));
}

TEST_CASE_FIXTURE(SimplifyFixture, "simplify_stops_at_cycles")
{
    TypeId t = mkTable({});
    TableType* tt = getMutable<TableType>(t);
    REQUIRE(tt);

    TypeId t2 = mkTable({});
    TableType* t2t = getMutable<TableType>(t2);
    REQUIRE(t2t);

    tt->props["cyclic"] = Property{t2};
    t2t->props["cyclic"] = Property{t};

    CHECK(t == intersect(t, unknownTy));
    CHECK(t == intersect(unknownTy, t));

    CHECK(t2 == intersect(t2, unknownTy));
    CHECK(t2 == intersect(unknownTy, t2));

    CHECK("*error-type* | t1 where t1 = { cyclic: { cyclic: t1 } }" == intersectStr(t, anyTy));
    CHECK("*error-type* | t1 where t1 = { cyclic: { cyclic: t1 } }" == intersectStr(anyTy, t));

    CHECK("*error-type* | t1 where t1 = { cyclic: { cyclic: t1 } }" == intersectStr(t2, anyTy));
    CHECK("*error-type* | t1 where t1 = { cyclic: { cyclic: t1 } }" == intersectStr(anyTy, t2));
}

TEST_CASE_FIXTURE(SimplifyFixture, "free_type_bound_by_any_with_any")
{
    CHECK("'a | *error-type*" == intersectStr(freeTy, anyTy));
    CHECK("'a | *error-type*" == intersectStr(anyTy, freeTy));

    CHECK("'a | *error-type*" == intersectStr(freeTy, anyTy));
    CHECK("'a | *error-type*" == intersectStr(anyTy, freeTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "bound_intersected_by_itself_should_be_itself")
{
    TypeId blocked = arena->addType(BlockedType{});
    CHECK(toString(blocked) == intersectStr(blocked, blocked));
}

TEST_CASE_FIXTURE(SimplifyFixture, "cyclic_never_union_and_string")
{
    // t1 where t1 = never | t1
    TypeId leftType = arena->addType(UnionType{{getBuiltins()->neverType, getBuiltins()->neverType}});
    UnionType* leftUnion = getMutable<UnionType>(leftType);
    REQUIRE(leftUnion);
    leftUnion->options[0] = leftType;

    CHECK(getBuiltins()->stringType == union_(leftType, getBuiltins()->stringType));
}

TEST_CASE_FIXTURE(SimplifyFixture, "any & (error | string)")
{
    ScopedFastFlag sff{FFlag::LuauSimplifyAnyAndUnion, true};

    TypeId errStringTy = arena->addType(UnionType{{getBuiltins()->errorType, getBuiltins()->stringType}});

    auto res = intersect(builtinTypes->anyType, errStringTy);

    CHECK("*error-type* | string" == toString(res));
}

TEST_CASE_FIXTURE(SimplifyFixture, "(error | string) & any")
{
    ScopedFastFlag sff{FFlag::LuauSimplifyAnyAndUnion, true};

    TypeId errStringTy = arena->addType(UnionType{{getBuiltins()->errorType, getBuiltins()->stringType}});

    auto res = intersect(errStringTy, builtinTypes->anyType);

    CHECK("*error-type* | string" == toString(res));
}

TEST_CASE_FIXTURE(SimplifyFixture, "{ x: number, y: number } & { x: unknown }")
{
    ScopedFastFlag sff{FFlag::LuauSimplifyRefinementOfReadOnlyProperty, true};

    TypeId leftTy = mkTable({{"x", builtinTypes->numberType}, {"y", builtinTypes->numberType}});
    TypeId rightTy = mkTable({{"x", Property::rw(builtinTypes->unknownType)}});

    CHECK(leftTy == intersect(leftTy, rightTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "{ x: number, y: number } & { read x: unknown }")
{
    ScopedFastFlag sff{FFlag::LuauSimplifyRefinementOfReadOnlyProperty, true};

    TypeId leftTy = mkTable({{"x", builtinTypes->numberType}, {"y", builtinTypes->numberType}});
    TypeId rightTy = mkTable({{"x", Property::readonly(builtinTypes->unknownType)}});

    CHECK(leftTy == intersect(leftTy, rightTy));
}

TEST_CASE_FIXTURE(SimplifyFixture, "{ read x: Child } & { x: Parent }")
{
    ScopedFastFlag sff{FFlag::LuauSimplifyRefinementOfReadOnlyProperty, true};

    createSomeExternTypes(getFrontend());

    TypeId parentTy = getFrontend().globals.globalScope->exportedTypeBindings["Parent"].type;
    REQUIRE(parentTy);

    TypeId childTy = getFrontend().globals.globalScope->exportedTypeBindings["Child"].type;
    REQUIRE(childTy);

    TypeId leftTy = mkTable({{"x", Property::readonly(childTy)}});
    TypeId rightTy = mkTable({{"x", parentTy}});

    // TODO: This could be { read x: Child, write x: Parent }
    CHECK("{ read x: Child } & { x: Parent }" == toString(intersect(leftTy, rightTy)));
}

TEST_CASE_FIXTURE(SimplifyFixture, "intersect_parts_empty_table_non_empty")
{
    TypeId emptyTable = arena->addType(TableType{});
    TableType nonEmpty;
    nonEmpty.props["p"] = arena->addType(UnionType{{getBuiltins()->numberType, getBuiltins()->stringType}});
    TypeId nonEmptyTable = arena->addType(std::move(nonEmpty));
    // FIXME CLI-170522: This is wrong.
    CHECK("never" == toString(simplifyIntersection(getBuiltins(), arena, {nonEmptyTable, emptyTable}).result));
}

TEST_SUITE_END();
