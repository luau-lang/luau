// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/Common.h"
#include "Luau/Type.h"
#include "doctest.h"

#include "Luau/Normalize.h"
#include "Luau/BuiltinDefinitions.h"

using namespace Luau;

namespace
{
struct IsSubtypeFixture : Fixture
{
    bool isSubtype(TypeId a, TypeId b)
    {
        ModulePtr module = getMainModule();
        REQUIRE(module);

        if (!module->hasModuleScope())
            FAIL("isSubtype: module scope data is not available");

        return ::Luau::isSubtype(a, b, NotNull{module->getModuleScope().get()}, builtinTypes, ice);
    }
};
} // namespace

TEST_SUITE_BEGIN("isSubtype");

TEST_CASE_FIXTURE(IsSubtypeFixture, "primitives")
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

TEST_CASE_FIXTURE(IsSubtypeFixture, "functions")
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

TEST_CASE_FIXTURE(IsSubtypeFixture, "functions_and_any")
{
    check(R"(
        function a(n: number) return "string" end
        function b(q: any) return 5 :: any end
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");

    // any makes things work even when it makes no sense.

    CHECK(isSubtype(b, a));
    CHECK(isSubtype(a, b));
}

TEST_CASE_FIXTURE(IsSubtypeFixture, "variadic_functions_with_no_head")
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
TEST_CASE_FIXTURE(IsSubtypeFixture, "variadic_function_with_head")
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

TEST_CASE_FIXTURE(IsSubtypeFixture, "union")
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

TEST_CASE_FIXTURE(IsSubtypeFixture, "table_with_union_prop")
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

TEST_CASE_FIXTURE(IsSubtypeFixture, "table_with_any_prop")
{
    check(R"(
        local a: {x: number}
        local b: {x: any}
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");

    CHECK(isSubtype(a, b));
    CHECK(isSubtype(b, a));
}

TEST_CASE_FIXTURE(IsSubtypeFixture, "intersection")
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

    // These types are both equivalent to never
    CHECK(isSubtype(d, a));
    CHECK(isSubtype(a, d));
}

TEST_CASE_FIXTURE(IsSubtypeFixture, "union_and_intersection")
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

TEST_CASE_FIXTURE(IsSubtypeFixture, "tables")
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
    CHECK(isSubtype(b, a));

    CHECK(!isSubtype(c, a));
    CHECK(!isSubtype(a, c));

    CHECK(isSubtype(d, a));
    CHECK(!isSubtype(a, d));

    CHECK(isSubtype(d, b));
    CHECK(!isSubtype(b, d));
}

#if 0
TEST_CASE_FIXTURE(IsSubtypeFixture, "table_indexers_are_invariant")
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

TEST_CASE_FIXTURE(IsSubtypeFixture, "mismatched_indexers")
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

TEST_CASE_FIXTURE(IsSubtypeFixture, "cyclic_table")
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

TEST_CASE_FIXTURE(IsSubtypeFixture, "classes")
{
    createSomeClasses(&frontend);

    check(""); // Ensure that we have a main Module.

    TypeId p = typeChecker.globalScope->lookupType("Parent")->type;
    TypeId c = typeChecker.globalScope->lookupType("Child")->type;
    TypeId u = typeChecker.globalScope->lookupType("Unrelated")->type;

    CHECK(isSubtype(c, p));
    CHECK(!isSubtype(p, c));
    CHECK(!isSubtype(u, p));
    CHECK(!isSubtype(p, u));
}

#if 0
TEST_CASE_FIXTURE(IsSubtypeFixture, "metatable" * doctest::expected_failures{1})
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

TEST_SUITE_END();

struct NormalizeFixture : Fixture
{
    ScopedFastFlag sff1{"LuauNegatedFunctionTypes", true};
    ScopedFastFlag sff2{"LuauNegatedClassTypes", true};

    TypeArena arena;
    InternalErrorReporter iceHandler;
    UnifierSharedState unifierState{&iceHandler};
    Normalizer normalizer{&arena, builtinTypes, NotNull{&unifierState}};

    NormalizeFixture()
    {
        registerHiddenTypes(&frontend);
    }

    const NormalizedType* toNormalizedType(const std::string& annotation)
    {
        normalizer.clearCaches();
        CheckResult result = check("type _Res = " + annotation);
        LUAU_REQUIRE_NO_ERRORS(result);
        std::optional<TypeId> ty = lookupType("_Res");
        REQUIRE(ty);
        return normalizer.normalize(*ty);
    }

    TypeId normal(const std::string& annotation)
    {
        const NormalizedType* norm = toNormalizedType(annotation);
        REQUIRE(norm);
        return normalizer.typeFromNormal(*norm);
    }
};

TEST_SUITE_BEGIN("Normalize");

TEST_CASE_FIXTURE(NormalizeFixture, "negate_string")
{
    CHECK("number" == toString(normal(R"(
        (number | string) & Not<string>
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "negate_string_from_cofinite_string_intersection")
{
    CHECK("number" == toString(normal(R"(
        (number | (string & Not<"hello"> & Not<"world">)) & Not<string>
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "no_op_negation_is_dropped")
{
    CHECK("number" == toString(normal(R"(
        number & Not<string>
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "union_of_negation")
{
    CHECK("string" == toString(normal(R"(
        (string & Not<"hello">) | "hello"
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersect_truthy")
{
    CHECK("number | string | true" == toString(normal(R"(
        (string | number | boolean | nil) & Not<false | nil>
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersect_truthy_expressed_as_intersection")
{
    CHECK("number | string | true" == toString(normal(R"(
        (string | number | boolean | nil) & Not<false> & Not<nil>
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "union_of_union")
{
    CHECK(R"("alpha" | "beta" | "gamma")" == toString(normal(R"(
        ("alpha" | "beta") | "gamma"
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "union_of_negations")
{
    CHECK(R"(string & ~"world")" == toString(normal(R"(
        (string & Not<"hello"> & Not<"world">) | (string & Not<"goodbye"> & Not<"world">)
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "disjoint_negations_normalize_to_string")
{
    CHECK(R"(string)" == toString(normal(R"(
        (string & Not<"hello"> & Not<"world">) | (string & Not<"goodbye">)
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "negate_boolean")
{
    CHECK("true" == toString(normal(R"(
        boolean & Not<false>
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "negate_boolean_2")
{
    CHECK("never" == toString(normal(R"(
        true & Not<true>
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersect_function_and_top_function")
{
    CHECK("() -> ()" == toString(normal(R"(
        fun & (() -> ())
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersect_function_and_top_function_reverse")
{
    CHECK("() -> ()" == toString(normal(R"(
        (() -> ()) & fun
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "union_function_and_top_function")
{
    CHECK("function" == toString(normal(R"(
        fun | (() -> ())
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "negated_function_is_anything_except_a_function")
{
    ScopedFastFlag{"LuauNegatedClassTypes", true};

    CHECK("(boolean | class | number | string | thread)?" == toString(normal(R"(
        Not<fun>
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "specific_functions_cannot_be_negated")
{
    CHECK(nullptr == toNormalizedType("Not<(boolean) -> boolean>"));
}

TEST_CASE_FIXTURE(NormalizeFixture, "bare_negated_boolean")
{
    ScopedFastFlag{"LuauNegatedClassTypes", true};
    // TODO: We don't yet have a way to say number | string | thread | nil | Class | Table | Function
    CHECK("(class | function | number | string | thread)?" == toString(normal(R"(
        Not<boolean>
    )")));
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

TEST_CASE_FIXTURE(BuiltinsFixture, "skip_force_normal_on_external_types")
{
    createSomeClasses(&frontend);

    CheckResult result = check(R"(
export type t0 = { a: Child }
export type t1 = { a: typeof(string.byte) }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "intersection_combine_on_bound_self")
{
    CheckResult result = check(R"(
export type t0 = (((any)&({_:l0.t0,n0:t0,_G:any,}))&({_:any,}))&(((any)&({_:l0.t0,n0:t0,_G:any,}))&({_:any,}))
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(NormalizeFixture, "unions_of_classes")
{
    ScopedFastFlag sff{"LuauNegatedClassTypes", true};

    createSomeClasses(&frontend);
    CHECK("Parent | Unrelated" == toString(normal("Parent | Unrelated")));
    CHECK("Parent" == toString(normal("Parent | Child")));
    CHECK("Parent | Unrelated" == toString(normal("Parent | Child | Unrelated")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersections_of_classes")
{
    ScopedFastFlag sff{"LuauNegatedClassTypes", true};

    createSomeClasses(&frontend);
    CHECK("Child" == toString(normal("Parent & Child")));
    CHECK("never" == toString(normal("Child & Unrelated")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "narrow_union_of_classes_with_intersection")
{
    ScopedFastFlag sff{"LuauNegatedClassTypes", true};

    createSomeClasses(&frontend);
    CHECK("Child" == toString(normal("(Child | Unrelated) & Child")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "negations_of_classes")
{
    ScopedFastFlag sff{"LuauNegatedClassTypes", true};

    createSomeClasses(&frontend);
    CHECK("(Parent & ~Child) | Unrelated" == toString(normal("(Parent & Not<Child>) | Unrelated")));
    CHECK("((class & ~Child) | boolean | function | number | string | thread)?" == toString(normal("Not<Child>")));
    CHECK("Child" == toString(normal("Not<Parent> & Child")));
    CHECK("((class & ~Parent) | Child | boolean | function | number | string | thread)?" == toString(normal("Not<Parent> | Child")));
    CHECK("(boolean | function | number | string | thread)?" == toString(normal("Not<cls>")));
    CHECK("(Parent | Unrelated | boolean | function | number | string | thread)?" ==
          toString(normal("Not<cls & Not<Parent> & Not<Child> & Not<Unrelated>>")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "classes_and_unknown")
{
    ScopedFastFlag sff{"LuauNegatedClassTypes", true};

    createSomeClasses(&frontend);
    CHECK("Parent" == toString(normal("Parent & unknown")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "classes_and_never")
{
    ScopedFastFlag sff{"LuauNegatedClassTypes", true};

    createSomeClasses(&frontend);
    CHECK("never" == toString(normal("Parent & never")));
}

TEST_SUITE_END();
