// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/Common.h"
#include "doctest.h"

#include "Luau/Normalize.h"
#include "Luau/BuiltinDefinitions.h"

using namespace Luau;

struct NormalizeFixture : Fixture
{
    bool isSubtype(TypeId a, TypeId b)
    {
        return ::Luau::isSubtype(a, b, NotNull{getMainModule()->getModuleScope().get()}, singletonTypes, ice);
    }
};

void createSomeClasses(Frontend& frontend)
{
    auto& arena = frontend.globalTypes;

    unfreeze(arena);

    TypeId parentType = arena.addType(ClassTypeVar{"Parent", {}, std::nullopt, std::nullopt, {}, nullptr, "Test"});

    ClassTypeVar* parentClass = getMutable<ClassTypeVar>(parentType);
    parentClass->props["method"] = {makeFunction(arena, parentType, {}, {})};

    parentClass->props["virtual_method"] = {makeFunction(arena, parentType, {}, {})};

    addGlobalBinding(frontend, "Parent", {parentType});
    frontend.getGlobalScope()->exportedTypeBindings["Parent"] = TypeFun{{}, parentType};

    TypeId childType = arena.addType(ClassTypeVar{"Child", {}, parentType, std::nullopt, {}, nullptr, "Test"});

    ClassTypeVar* childClass = getMutable<ClassTypeVar>(childType);
    childClass->props["virtual_method"] = {makeFunction(arena, childType, {}, {})};

    addGlobalBinding(frontend, "Child", {childType});
    frontend.getGlobalScope()->exportedTypeBindings["Child"] = TypeFun{{}, childType};

    TypeId unrelatedType = arena.addType(ClassTypeVar{"Unrelated", {}, std::nullopt, std::nullopt, {}, nullptr, "Test"});

    addGlobalBinding(frontend, "Unrelated", {unrelatedType});
    frontend.getGlobalScope()->exportedTypeBindings["Unrelated"] = TypeFun{{}, unrelatedType};

    for (const auto& [name, ty] : frontend.getGlobalScope()->exportedTypeBindings)
        persist(ty.type);

    freeze(arena);
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
    ScopedFastFlag sffs[]{
        {"LuauSubtypeNormalizer", true},
        {"LuauTypeNormalization2", true},
    };

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
    createSomeClasses(frontend);

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

TEST_SUITE_END();

TEST_SUITE_BEGIN("Normalize");

TEST_CASE_FIXTURE(NormalizeFixture, "union_with_overlapping_field_that_has_a_subtype_relationship")
{
    check(R"(
        local t: {x: number} | {x: number?}
    )");

    ModulePtr tempModule{new Module};
    tempModule->scopes.emplace_back(Location(), std::make_shared<Scope>(singletonTypes->anyTypePack));

    // HACK: Normalization is an in-place operation.  We need to cheat a little here and unfreeze
    // the arena that the type lives in.
    ModulePtr mainModule = getMainModule();
    unfreeze(mainModule->internalTypes);

    TypeId tType = requireType("t");
    normalize(tType, tempModule, singletonTypes, *typeChecker.iceHandler);

    CHECK_EQ("{| x: number? |}", toString(tType, {true}));
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
    createSomeClasses(frontend);

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

TEST_SUITE_END();
