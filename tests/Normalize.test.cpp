// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "doctest.h"

#include "Luau/Normalize.h"
#include "Luau/BuiltinDefinitions.h"

using namespace Luau;

struct NormalizeFixture : Fixture
{
    ScopedFastFlag sff1{"LuauLowerBoundsCalculation", true};
    ScopedFastFlag sff2{"LuauTableSubtypingVariance2", true};
};

void createSomeClasses(TypeChecker& typeChecker)
{
    auto& arena = typeChecker.globalTypes;

    unfreeze(arena);

    TypeId parentType = arena.addType(ClassTypeVar{"Parent", {}, std::nullopt, std::nullopt, {}, nullptr, "Test"});

    ClassTypeVar* parentClass = getMutable<ClassTypeVar>(parentType);
    parentClass->props["method"] = {makeFunction(arena, parentType, {}, {})};

    parentClass->props["virtual_method"] = {makeFunction(arena, parentType, {}, {})};

    addGlobalBinding(typeChecker, "Parent", {parentType});
    typeChecker.globalScope->exportedTypeBindings["Parent"] = TypeFun{{}, parentType};

    TypeId childType = arena.addType(ClassTypeVar{"Child", {}, parentType, std::nullopt, {}, nullptr, "Test"});

    ClassTypeVar* childClass = getMutable<ClassTypeVar>(childType);
    childClass->props["virtual_method"] = {makeFunction(arena, childType, {}, {})};

    addGlobalBinding(typeChecker, "Child", {childType});
    typeChecker.globalScope->exportedTypeBindings["Child"] = TypeFun{{}, childType};

    TypeId unrelatedType = arena.addType(ClassTypeVar{"Unrelated", {}, std::nullopt, std::nullopt, {}, nullptr, "Test"});

    addGlobalBinding(typeChecker, "Unrelated", {unrelatedType});
    typeChecker.globalScope->exportedTypeBindings["Unrelated"] = TypeFun{{}, unrelatedType};

    freeze(arena);
}

static bool isSubtype(TypeId a, TypeId b)
{
    InternalErrorReporter ice;
    return isSubtype(a, b, ice);
}

TEST_SUITE_BEGIN("isSubtype");

TEST_CASE_FIXTURE(NormalizeFixture, "primitives")
{
    check(R"(
        local a = 41
        local b = 32

        local c = "hello"
        local d = "world"
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");
    TypeId d = requireType("d");

    CHECK(isSubtype(b, a));
    CHECK(isSubtype(d, c));
    CHECK(!isSubtype(d, a));
}

TEST_CASE_FIXTURE(NormalizeFixture, "functions")
{
    check(R"(
        function a(x: number): number return x end
        function b(x: number): number return x end

        function c(x: number?): number return x end
        function d(x: number): number? return x end
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");
    TypeId d = requireType("d");

    CHECK(isSubtype(b, a));
    CHECK(isSubtype(c, a));
    CHECK(!isSubtype(d, a));
    CHECK(isSubtype(a, d));
}

TEST_CASE_FIXTURE(NormalizeFixture, "functions_and_any")
{
    check(R"(
        function a(n: number) return "string" end
        function b(q: any) return 5 :: any end
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");

    // Intuition:
    //    We cannot use b where a is required because we cannot rely on b to return a string.
    //    We cannot use a where b is required because we cannot rely on a to accept non-number arguments.

    CHECK(!isSubtype(b, a));
    CHECK(!isSubtype(a, b));
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersection_of_functions_of_different_arities")
{
    check(R"(
        type A = (any) -> ()
        type B = (any, any) -> ()
        type T = A & B

        local a: A
        local b: B
        local t: T
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");

    CHECK(!isSubtype(a, b)); // !!
    CHECK(!isSubtype(b, a));

    CHECK("((any) -> ()) & ((any, any) -> ())" == toString(requireType("t")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "functions_with_mismatching_arity")
{
    check(R"(
        local a: (number) -> ()
        local b: () -> ()

        local c: () -> number
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");

    CHECK(!isSubtype(b, a));
    CHECK(!isSubtype(c, a));

    CHECK(!isSubtype(a, b));
    CHECK(!isSubtype(c, b));

    CHECK(!isSubtype(a, c));
    CHECK(!isSubtype(b, c));
}

TEST_CASE_FIXTURE(NormalizeFixture, "functions_with_mismatching_arity_but_optional_parameters")
{
    /*
     * (T0..TN) <: (T0..TN, A?)
     * (T0..TN) <: (T0..TN, any)
     * (T0..TN, A?) </: (T0..TN)            We don't technically need to spell this out, but it's quite important.
     * T <: T
     * if A <: B and B <: C then A <: C
     * T -> R <: U -> S if U <: T and R <: S
     * A | B <: T if A <: T and B <: T
     * T <: A | B if T <: A or T <: B
     */
    check(R"(
        local a: (number?) -> ()
        local b: (number) -> ()
        local c: (number, number?) -> ()
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");

    /*
     * (number) -> () </: (number?) -> ()
     *      because number? </: number (because number <: number, but nil </: number)
     */
    CHECK(!isSubtype(b, a));

    /*
     * (number, number?) </: (number?) -> ()
     *      because number? </: number (as above)
     */
    CHECK(!isSubtype(c, a));

    /*
     * (number?) -> () <: (number) -> ()
     *      because number <: number? (because number <: number)
     */
    CHECK(isSubtype(a, b));

    /*
     * (number, number?) -> () <: (number) -> (number)
     *      The packs have inequal lengths, but (number) <: (number, number?)
     *      and number <: number
     */
    CHECK(!isSubtype(c, b));

    /*
     * (number?) -> () </: (number, number?) -> ()
     *      because (number, number?) </: (number)
     */
    CHECK(!isSubtype(a, c));

    /*
     * (number) -> () </: (number, number?) -> ()
     *      because (number, number?) </: (number)
     */
    CHECK(!isSubtype(b, c));
}

TEST_CASE_FIXTURE(NormalizeFixture, "functions_with_mismatching_arity_but_any_is_an_optional_param")
{
    check(R"(
        local a: (number?) -> ()
        local b: (number) -> ()
        local c: (number, any) -> ()
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");

    /*
     * (number) -> () </: (number?) -> ()
     *      because number? </: number (because number <: number, but nil </: number)
     */
    CHECK(!isSubtype(b, a));

    /*
     * (number, any) </: (number?) -> ()
     *      because number? </: number (as above)
     */
    CHECK(!isSubtype(c, a));

    /*
     * (number?) -> () <: (number) -> ()
     *      because number <: number? (because number <: number)
     */
    CHECK(isSubtype(a, b));

    /*
     * (number, any) -> () </: (number) -> (number)
     *      The packs have inequal lengths
     */
    CHECK(!isSubtype(c, b));

    /*
     * (number?) -> () </: (number, any) -> ()
     *      The packs have inequal lengths
     */
    CHECK(!isSubtype(a, c));

    /*
     * (number) -> () </: (number, any) -> ()
     *      The packs have inequal lengths
     */
    CHECK(!isSubtype(b, c));
}

TEST_CASE_FIXTURE(NormalizeFixture, "variadic_functions_with_no_head")
{
    check(R"(
        local a: (...number) -> ()
        local b: (...number?) -> ()
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");

    CHECK(isSubtype(b, a));
    CHECK(!isSubtype(a, b));
}

#if 0
TEST_CASE_FIXTURE(NormalizeFixture, "variadic_function_with_head")
{
    check(R"(
        local a: (...number) -> ()
        local b: (number, number) -> ()
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");

    CHECK(!isSubtype(b, a));
    CHECK(isSubtype(a, b));
}
#endif

TEST_CASE_FIXTURE(NormalizeFixture, "union")
{
    check(R"(
        local a: number | string
        local b: number
        local c: string
        local d: number?
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");
    TypeId d = requireType("d");

    CHECK(isSubtype(b, a));
    CHECK(!isSubtype(a, b));

    CHECK(isSubtype(c, a));
    CHECK(!isSubtype(a, c));

    CHECK(!isSubtype(d, a));
    CHECK(!isSubtype(a, d));

    CHECK(isSubtype(b, d));
    CHECK(!isSubtype(d, b));
}

TEST_CASE_FIXTURE(NormalizeFixture, "table_with_union_prop")
{
    check(R"(
        local a: {x: number}
        local b: {x: number?}
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");

    CHECK(isSubtype(a, b));
    CHECK(!isSubtype(b, a));
}

TEST_CASE_FIXTURE(NormalizeFixture, "table_with_any_prop")
{
    check(R"(
        local a: {x: number}
        local b: {x: any}
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");

    CHECK(isSubtype(a, b));
    CHECK(!isSubtype(b, a));
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersection")
{
    check(R"(
        local a: number & string
        local b: number
        local c: string
        local d: number & nil
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");
    TypeId d = requireType("d");

    CHECK(!isSubtype(b, a));
    CHECK(isSubtype(a, b));

    CHECK(!isSubtype(c, a));
    CHECK(isSubtype(a, c));

    CHECK(!isSubtype(d, a));
    CHECK(!isSubtype(a, d));
}

TEST_CASE_FIXTURE(NormalizeFixture, "union_and_intersection")
{
    check(R"(
            local a: number & string
            local b: number | nil
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");

    CHECK(!isSubtype(b, a));
    CHECK(isSubtype(a, b));
}

TEST_CASE_FIXTURE(NormalizeFixture, "table_with_table_prop")
{
    check(R"(
        type T = {x: {y: number}} & {x: {y: string}}
        local a: T
    )");

    CHECK_EQ("{| x: {| y: number & string |} |}", toString(requireType("a")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "tables")
{
    check(R"(
        local a: {x: number}
        local b: {x: any}
        local c: {y: number}
        local d: {x: number, y: number}
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");
    TypeId d = requireType("d");

    CHECK(isSubtype(a, b));
    CHECK(!isSubtype(b, a));

    CHECK(!isSubtype(c, a));
    CHECK(!isSubtype(a, c));

    CHECK(isSubtype(d, a));
    CHECK(!isSubtype(a, d));

    CHECK(isSubtype(d, b));
    CHECK(!isSubtype(b, d));
}

#if 0
TEST_CASE_FIXTURE(NormalizeFixture, "table_indexers_are_invariant")
{
    check(R"(
        local a: {[string]: number}
        local b: {[string]: any}
        local c: {[string]: number}
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");

    CHECK(!isSubtype(b, a));
    CHECK(!isSubtype(a, b));

    CHECK(isSubtype(c, a));
    CHECK(isSubtype(a, c));
}

TEST_CASE_FIXTURE(NormalizeFixture, "mismatched_indexers")
{
    check(R"(
        local a: {x: number}
        local b: {[string]: number}
        local c: {}
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");

    CHECK(isSubtype(b, a));
    CHECK(!isSubtype(a, b));

    CHECK(!isSubtype(c, b));
    CHECK(isSubtype(b, c));
}

TEST_CASE_FIXTURE(NormalizeFixture, "cyclic_table")
{
    check(R"(
        type A = {method: (A) -> ()}
        local a: A

        type B = {method: (any) -> ()}
        local b: B

        type C = {method: (C) -> ()}
        local c: C

        type D = {method: (D) -> (), another: (D) -> ()}
        local d: D

        type E = {method: (A) -> (), another: (E) -> ()}
        local e: E
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");
    TypeId d = requireType("d");
    TypeId e = requireType("e");

    CHECK(isSubtype(b, a));
    CHECK(!isSubtype(a, b));

    CHECK(isSubtype(c, a));
    CHECK(isSubtype(a, c));

    CHECK(!isSubtype(d, a));
    CHECK(!isSubtype(a, d));

    CHECK(isSubtype(e, a));
    CHECK(!isSubtype(a, e));
}
#endif

TEST_CASE_FIXTURE(NormalizeFixture, "classes")
{
    createSomeClasses(typeChecker);

    TypeId p = typeChecker.globalScope->lookupType("Parent")->type;
    TypeId c = typeChecker.globalScope->lookupType("Child")->type;
    TypeId u = typeChecker.globalScope->lookupType("Unrelated")->type;

    CHECK(isSubtype(c, p));
    CHECK(!isSubtype(p, c));
    CHECK(!isSubtype(u, p));
    CHECK(!isSubtype(p, u));
}

#if 0
TEST_CASE_FIXTURE(NormalizeFixture, "metatable" * doctest::expected_failures{1})
{
     check(R"(
        local T = {}
        T.__index = T
        function T.new()
            return setmetatable({}, T)
        end

        function T:method() end

        local a: typeof(T.new)
        local b: {method: (any) -> ()}
     )");

     TypeId a = requireType("a");
     TypeId b = requireType("b");

     CHECK(isSubtype(a, b));
}
#endif

TEST_CASE_FIXTURE(NormalizeFixture, "intersection_of_tables")
{
    check(R"(
        type T = {x: number} & ({x: number} & {y: string?})
        local t: T
    )");

    CHECK("{| x: number, y: string? |}" == toString(requireType("t")));
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("Normalize");

TEST_CASE_FIXTURE(NormalizeFixture, "intersection_of_disjoint_tables")
{
    check(R"(
        type T = {a: number} & {b: number}
        local t: T
    )");

    CHECK_EQ("{| a: number, b: number |}", toString(requireType("t")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersection_of_overlapping_tables")
{
    check(R"(
        type T = {a: number, b: string} & {b: number, c: string}
        local t: T
    )");

    CHECK_EQ("{| a: number, b: number & string, c: string |}", toString(requireType("t")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersection_of_confluent_overlapping_tables")
{
    check(R"(
        type T = {a: number, b: string} & {b: string, c: string}
        local t: T
    )");

    CHECK_EQ("{| a: number, b: string, c: string |}", toString(requireType("t")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "union_with_overlapping_field_that_has_a_subtype_relationship")
{
    check(R"(
        local t: {x: number} | {x: number?}
    )");

    ModulePtr tempModule{new Module};

    // HACK: Normalization is an in-place operation.  We need to cheat a little here and unfreeze
    // the arena that the type lives in.
    ModulePtr mainModule = getMainModule();
    unfreeze(mainModule->internalTypes);

    TypeId tType = requireType("t");
    normalize(tType, tempModule, *typeChecker.iceHandler);

    CHECK_EQ("{| x: number? |}", toString(tType, {true}));
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersection_of_functions")
{
    check(R"(
        type T = ((any) -> string) & ((number) -> string)
        local t: T
    )");

    CHECK_EQ("(any) -> string", toString(requireType("t")));
}

TEST_CASE_FIXTURE(Fixture, "normalize_module_return_type")
{
    ScopedFastFlag sff[] = {
        {"LuauLowerBoundsCalculation", true},
        {"LuauReturnTypeInferenceInNonstrict", true},
    };

    check(R"(
        --!nonstrict

        if Math.random() then
            return function(initialState, handlers)
                return function(state, action)
                    return state
                end
            end
        else
            return function(initialState, handlers)
                return function(state, action)
                    return state
                end
            end
        end
    )");

    CHECK_EQ("(any, any) -> (any, any) -> any", toString(getMainModule()->getModuleScope()->returnType));
}

TEST_CASE_FIXTURE(Fixture, "return_type_is_not_a_constrained_intersection")
{
    check(R"(
        function foo(x:number, y:number)
            return x + y
        end
    )");

    CHECK_EQ("(number, number) -> number", toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "higher_order_function")
{
    check(R"(
        function apply(f, x)
            return f(x)
        end

        local a = apply(function(x: number) return x + x end, 5)
    )");

    TypeId aType = requireType("a");
    CHECK_MESSAGE(isNumber(follow(aType)), "Expected a number but got ", toString(aType));
}

TEST_CASE_FIXTURE(Fixture, "higher_order_function_with_annotation")
{
    check(R"(
        function apply<a, b>(f: (a) -> b, x)
            return f(x)
        end
    )");

    CHECK_EQ("<a, b>((a) -> b, a) -> b", toString(requireType("apply")));
}

TEST_CASE_FIXTURE(Fixture, "cyclic_table_is_marked_normal")
{
    ScopedFastFlag flags[] = {
        {"LuauLowerBoundsCalculation", true},
    };

    check(R"(
        type Fiber = {
            return_: Fiber?
        }

        local f: Fiber
    )");

    TypeId t = requireType("f");
    CHECK(t->normal);
}

TEST_CASE_FIXTURE(Fixture, "variadic_tail_is_marked_normal")
{
    ScopedFastFlag flags[] = {
        {"LuauLowerBoundsCalculation", true},
    };

    CheckResult result = check(R"(
        type Weirdo = (...{x: number}) -> ()

        local w: Weirdo
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId t = requireType("w");
    auto ftv = get<FunctionTypeVar>(t);
    REQUIRE(ftv);

    auto [argHead, argTail] = flatten(ftv->argTypes);
    CHECK(argHead.empty());
    REQUIRE(argTail.has_value());

    auto vtp = get<VariadicTypePack>(*argTail);
    REQUIRE(vtp);
    CHECK(vtp->ty->normal);
}

TEST_CASE_FIXTURE(Fixture, "cyclic_table_normalizes_sensibly")
{
    CheckResult result = check(R"(
        local Cyclic = {}
        function Cyclic.get()
            return Cyclic
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId ty = requireType("Cyclic");
    CHECK_EQ("t1 where t1 = { get: () -> t1 }", toString(ty, {true}));
}

TEST_CASE_FIXTURE(Fixture, "union_of_distinct_free_types")
{
    ScopedFastFlag flags[] = {
        {"LuauLowerBoundsCalculation", true},
    };

    CheckResult result = check(R"(
        function fussy(a, b)
            if math.random() > 0.5 then
                return a
            else
                return b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("<a, b>(a, b) -> a | b" == toString(requireType("fussy")));
}

TEST_CASE_FIXTURE(Fixture, "constrained_intersection_of_intersections")
{
    ScopedFastFlag flags[] = {
        {"LuauLowerBoundsCalculation", true},
    };

    CheckResult result = check(R"(
        local f : (() -> number) | ((number) -> number)
        local g : (() -> number) | ((string) -> number)

        function h()
            if math.random() then
                return f
            else
                return g
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId h = requireType("h");

    CHECK("() -> (() -> number) | ((number) -> number) | ((string) -> number)" == toString(h));
}

TEST_CASE_FIXTURE(Fixture, "intersection_inside_a_table_inside_another_intersection")
{
    ScopedFastFlag flags[] = {
        {"LuauLowerBoundsCalculation", true},
    };

    CheckResult result = check(R"(
        type X = {}
        type Y = {y: number}
        type Z = {z: string}
        type W = {w: boolean}
        type T = {x: Y & X} & {x:Z & W}

        local x: X
        local y: Y
        local z: Z
        local w: W
        local t: T
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("{|  |}" == toString(requireType("x"), {true}));
    CHECK("{| y: number |}" == toString(requireType("y"), {true}));
    CHECK("{| z: string |}" == toString(requireType("z"), {true}));
    CHECK("{| w: boolean |}" == toString(requireType("w"), {true}));
    CHECK("{| x: {| w: boolean, y: number, z: string |} |}" == toString(requireType("t"), {true}));
}

TEST_CASE_FIXTURE(Fixture, "intersection_inside_a_table_inside_another_intersection_2")
{
    ScopedFastFlag flags[] = {
        {"LuauLowerBoundsCalculation", true},
    };

    // We use a function and inferred parameter types to prevent intermediate normalizations from being performed.
    // This exposes a bug where the type of y is mutated.
    CheckResult result = check(R"(
        function strange(w, x, y, z)
            y.y = 5
            z.z = "five"
            w.w = true

            type Z = {x: typeof(x) & typeof(y)} & {x: typeof(w) & typeof(z)}

            return ((nil :: any) :: Z)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId t = requireType("strange");
    auto ftv = get<FunctionTypeVar>(t);
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> args = flatten(ftv->argTypes).first;

    REQUIRE(4 == args.size());
    CHECK("{+ w: boolean +}" == toString(args[0]));
    CHECK("a" == toString(args[1]));
    CHECK("{+ y: number +}" == toString(args[2]));
    CHECK("{+ z: string +}" == toString(args[3]));

    std::vector<TypeId> ret = flatten(ftv->retType).first;

    REQUIRE(1 == ret.size());
    CHECK("{| x: a & {- w: boolean, y: number, z: string -} |}" == toString(ret[0]));
}

TEST_CASE_FIXTURE(Fixture, "intersection_inside_a_table_inside_another_intersection_3")
{
    ScopedFastFlag flags[] = {
        {"LuauLowerBoundsCalculation", true},
    };

    // We use a function and inferred parameter types to prevent intermediate normalizations from being performed.
    // This exposes a bug where the type of y is mutated.
    CheckResult result = check(R"(
        function strange(x, y, z)
            x.x = true
            y.y = y
            z.z = "five"

            type Z = {x: typeof(y)} & {x: typeof(x) & typeof(z)}

            return ((nil :: any) :: Z)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId t = requireType("strange");
    auto ftv = get<FunctionTypeVar>(t);
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> args = flatten(ftv->argTypes).first;

    REQUIRE(3 == args.size());
    CHECK("{+ x: boolean +}" == toString(args[0]));
    CHECK("t1 where t1 = {+ y: t1 +}" == toString(args[1]));
    CHECK("{+ z: string +}" == toString(args[2]));

    std::vector<TypeId> ret = flatten(ftv->retType).first;

    REQUIRE(1 == ret.size());
    CHECK("{| x: {- x: boolean, y: t1, z: string -} |} where t1 = {+ y: t1 +}" == toString(ret[0]));
}

TEST_CASE_FIXTURE(Fixture, "intersection_inside_a_table_inside_another_intersection_4")
{
    ScopedFastFlag flags[] = {
        {"LuauLowerBoundsCalculation", true},
    };

    // We use a function and inferred parameter types to prevent intermediate normalizations from being performed.
    // This exposes a bug where the type of y is mutated.
    CheckResult result = check(R"(
        function strange(x, y, z)
            x.x = true
            z.z = "five"

            type R = {x: typeof(y)} & {x: typeof(x) & typeof(z)}
            local r: R

            y.y = r

            return r
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId t = requireType("strange");
    auto ftv = get<FunctionTypeVar>(t);
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> args = flatten(ftv->argTypes).first;

    REQUIRE(3 == args.size());
    CHECK("{+ x: boolean +}" == toString(args[0]));
    CHECK("{+ y: t1 +} where t1 = {| x: {- x: boolean, y: t1, z: string -} |}" == toString(args[1]));
    CHECK("{+ z: string +}" == toString(args[2]));

    std::vector<TypeId> ret = flatten(ftv->retType).first;

    REQUIRE(1 == ret.size());
    CHECK("t1 where t1 = {| x: {- x: boolean, y: t1, z: string -} |}" == toString(ret[0]));
}

TEST_CASE_FIXTURE(Fixture, "nested_table_normalization_with_non_table__no_ice")
{
    ScopedFastFlag flags[] = {
        {"LuauLowerBoundsCalculation", true},
        {"LuauNormalizeCombineTableFix", true},
    };
    // CLI-52787
    // ends up combining {_:any} with any, recursively
    // which used to ICE because this combines a table with a non-table.
    CheckResult result = check(R"(
        export type t0 = any & { _: {_:any} } & { _:any }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "visiting_a_type_twice_is_not_considered_normal")
{
    ScopedFastFlag sff{"LuauLowerBoundsCalculation", true};

    CheckResult result = check(R"(
        --!strict
        function f(a, b)
            local function g()
                if math.random() > 0.5 then
                    return a()
                else
                    return b
                end
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("<a>(() -> a, a) -> ()", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "fuzz_failure_instersection_combine_must_follow")
{
    ScopedFastFlag flags[] = {
        {"LuauLowerBoundsCalculation", true},
        {"LuauNormalizeCombineIntersectionFix", true},
    };

    CheckResult result = check(R"(
        export type t0 = {_:{_:any} & {_:any|string}} & {_:{_:{}}}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "fuzz_failure_bound_type_is_normal_but_not_its_bounded_to")
{
    ScopedFastFlag sff{"LuauLowerBoundsCalculation", true};

    CheckResult result = check(R"(
        type t252 = ((t0<t252...>)|(any))|(any)
        type t0 = t252<t0<any,t24...>,t24...>
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_SUITE_END();
