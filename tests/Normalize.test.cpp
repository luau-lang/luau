// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/AstQuery.h"
#include "Luau/Common.h"
#include "Luau/Type.h"
#include "doctest.h"

#include "Luau/Normalize.h"
#include "Luau/BuiltinDefinitions.h"

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)
LUAU_FASTFLAG(LuauNormalizeNotUnknownIntersection)
LUAU_FASTINT(LuauTypeInferRecursionLimit)
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

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK(!isSubtype(a, b)); // table properties are invariant
    else
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

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK(!isSubtype(a, b)); // table properties are invariant
    else
        CHECK(isSubtype(a, b));
    CHECK(!isSubtype(b, a));
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

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK(!isSubtype(a, b)); // table properties are invariant
    else
        CHECK(isSubtype(a, b));
    CHECK(!isSubtype(b, a));

    CHECK(!isSubtype(c, a));
    CHECK(!isSubtype(a, c));

    CHECK(isSubtype(d, a));
    CHECK(!isSubtype(a, d));

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK(!isSubtype(d, b)); // table properties are invariant
    else
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

    TypeId p = frontend.globals.globalScope->lookupType("Parent")->type;
    TypeId c = frontend.globals.globalScope->lookupType("Child")->type;
    TypeId u = frontend.globals.globalScope->lookupType("Unrelated")->type;

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

TEST_CASE_FIXTURE(IsSubtypeFixture, "any_is_unknown_union_error")
{
    check(R"(
        local err = 5.nope.nope -- err is now an error type
        local a : any
        local b : (unknown | typeof(err))
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");

    CHECK(isSubtype(a, b));
    CHECK(isSubtype(b, a));
    CHECK_EQ("*error-type*", toString(requireType("err")));
}

TEST_CASE_FIXTURE(IsSubtypeFixture, "any_intersect_T_is_T")
{
    check(R"(
        local a : (any & string)
        local b : string
        local c : number
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");

    CHECK(isSubtype(a, b));
    CHECK(isSubtype(b, a));
    CHECK(!isSubtype(a, c));
    CHECK(!isSubtype(c, a));
}

TEST_CASE_FIXTURE(IsSubtypeFixture, "error_suppression")
{
    check("");

    TypeId any = builtinTypes->anyType;
    TypeId err = builtinTypes->errorType;
    TypeId str = builtinTypes->stringType;
    TypeId unk = builtinTypes->unknownType;

    CHECK(!isSubtype(any, err));
    CHECK(isSubtype(err, any));

    CHECK(!isSubtype(any, str));
    CHECK(isSubtype(str, any));

    // We have added this as an exception - the set of inhabitants of any is exactly the set of inhabitants of unknown (since error has no
    // inhabitants). any = err | unknown, so under semantic subtyping, {} U unknown = unknown
    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK(isSubtype(any, unk));
    }
    else
    {
        CHECK(!isSubtype(any, unk));
    }

    CHECK(!isSubtype(err, str));
    CHECK(!isSubtype(str, err));

    CHECK(!isSubtype(err, unk));
    CHECK(!isSubtype(unk, err));

    CHECK(isSubtype(str, unk));
    CHECK(!isSubtype(unk, str));
}

TEST_SUITE_END();

struct NormalizeFixture : Fixture
{
    TypeArena arena;
    InternalErrorReporter iceHandler;
    UnifierSharedState unifierState{&iceHandler};
    Normalizer normalizer{&arena, builtinTypes, NotNull{&unifierState}};
    Scope globalScope{builtinTypes->anyTypePack};

    NormalizeFixture()
    {
        registerHiddenTypes(&frontend);
    }

    std::shared_ptr<const NormalizedType> toNormalizedType(const std::string& annotation)
    {
        normalizer.clearCaches();
        CheckResult result = check("type _Res = " + annotation);
        LUAU_REQUIRE_NO_ERRORS(result);

        if (FFlag::DebugLuauDeferredConstraintResolution)
        {
            SourceModule* sourceModule = getMainSourceModule();
            REQUIRE(sourceModule);
            AstNode* node = findNodeAtPosition(*sourceModule, {0, 5});
            REQUIRE(node);
            AstStatTypeAlias* alias = node->as<AstStatTypeAlias>();
            REQUIRE(alias);
            TypeId* originalTy = getMainModule()->astResolvedTypes.find(alias->type);
            REQUIRE(originalTy);
            return normalizer.normalize(*originalTy);
        }
        else
        {
            std::optional<TypeId> ty = lookupType("_Res");
            REQUIRE(ty);
            return normalizer.normalize(*ty);
        }
    }

    TypeId normal(const std::string& annotation)
    {
        std::shared_ptr<const NormalizedType> norm = toNormalizedType(annotation);
        REQUIRE(norm);
        return normalizer.typeFromNormal(*norm);
    }
};

TEST_SUITE_BEGIN("Normalize");

TEST_CASE_FIXTURE(NormalizeFixture, "string_intersection_is_commutative")
{
    auto c4 = toString(normal(R"(
        string & (string & Not<"a"> & Not<"b">)
)"));
    auto c4Reverse = toString(normal(R"(
        (string & Not<"a"> & Not<"b">) & string
)"));
    CHECK(c4 == c4Reverse);
    CHECK_EQ("string & ~\"a\" & ~\"b\"", c4);

    auto c5 = toString(normal(R"(
        (string & Not<"a"> & Not<"b">) & (string & Not<"b"> & Not<"c">)
)"));
    auto c5Reverse = toString(normal(R"(
        (string & Not<"b"> & Not<"c">) & (string & Not<"a"> & Not<"c">)
)"));
    CHECK(c5 == c5Reverse);
    CHECK_EQ("string & ~\"a\" & ~\"b\" & ~\"c\"", c5);

    auto c6 = toString(normal(R"(
        ("a" | "b") & (string & Not<"b"> & Not<"c">)
)"));
    auto c6Reverse = toString(normal(R"(
        (string & Not<"b"> & Not<"c">) & ("a" | "b")
)"));
    CHECK(c6 == c6Reverse);
    CHECK_EQ("\"a\"", c6);

    auto c7 = toString(normal(R"(
        string & ("b" | "c")
)"));
    auto c7Reverse = toString(normal(R"(
        ("b" | "c") & string
)"));
    CHECK(c7 == c7Reverse);
    CHECK_EQ("\"b\" | \"c\"", c7);

    auto c8 = toString(normal(R"(
(string & Not<"a"> & Not<"b">) & ("b" | "c")
)"));
    auto c8Reverse = toString(normal(R"(
        ("b" | "c") & (string & Not<"a"> & Not<"b">)
)"));
    CHECK(c8 == c8Reverse);
    CHECK_EQ("\"c\"", c8);
    auto c9 = toString(normal(R"(
            ("a" | "b") & ("b" | "c")
    )"));
    auto c9Reverse = toString(normal(R"(
            ("b" | "c") & ("a" | "b")
    )"));
    CHECK(c9 == c9Reverse);
    CHECK_EQ("\"b\"", c9);

    auto l = toString(normal(R"(
         (string | number) & ("a" | true)
    )"));
    auto r = toString(normal(R"(
         ("a" | true) & (string | number)
    )"));
    CHECK(l == r);
    CHECK_EQ("\"a\"", l);
}

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

TEST_CASE_FIXTURE(NormalizeFixture, "double_negation")
{
    CHECK("number" == toString(normal(R"(
        number & Not<Not<any>>
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "negate_any")
{
    CHECK("number" == toString(normal(R"(
        number & Not<any>
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
    CHECK("(boolean | buffer | class | number | string | table | thread)?" == toString(normal(R"(
        Not<fun>
    )")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "specific_functions_cannot_be_negated")
{
    CHECK(nullptr == toNormalizedType("Not<(boolean) -> boolean>"));
}

TEST_CASE_FIXTURE(NormalizeFixture, "trivial_intersection_inhabited")
{
    // this test was used to fix a bug in normalization when working with intersections/unions of the same type.

    TypeId a = arena.addType(FunctionType{builtinTypes->emptyTypePack, builtinTypes->anyTypePack, std::nullopt, false});
    TypeId c = arena.addType(IntersectionType{{a, a}});

    std::shared_ptr<const NormalizedType> n = normalizer.normalize(c);
    REQUIRE(n);

    CHECK(normalizer.isInhabited(n.get()) == NormalizationResult::True);
}

TEST_CASE_FIXTURE(NormalizeFixture, "bare_negated_boolean")
{
    CHECK("(buffer | class | function | number | string | table | thread)?" == toString(normal(R"(
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
    createSomeClasses(&frontend);
    CHECK("Parent | Unrelated" == toString(normal("Parent | Unrelated")));
    CHECK("Parent" == toString(normal("Parent | Child")));
    CHECK("Parent | Unrelated" == toString(normal("Parent | Child | Unrelated")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersections_of_classes")
{
    createSomeClasses(&frontend);
    CHECK("Child" == toString(normal("Parent & Child")));
    CHECK("never" == toString(normal("Child & Unrelated")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "narrow_union_of_classes_with_intersection")
{
    createSomeClasses(&frontend);
    CHECK("Child" == toString(normal("(Child | Unrelated) & Child")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersection_of_metatables_where_the_metatable_is_top_or_bottom")
{
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK("{ @metatable *error-type*, {  } }" == toString(normal("Mt<{}, any> & Mt<{}, err>")));
    else
        CHECK("{ @metatable *error-type*, {|  |} }" == toString(normal("Mt<{}, any> & Mt<{}, err>")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "recurring_intersection")
{
    CheckResult result = check(R"(
        type A = any?
        type B = A & A
    )");

    std::optional<TypeId> t = lookupType("B");
    REQUIRE(t);

    std::shared_ptr<const NormalizedType> nt = normalizer.normalize(*t);
    REQUIRE(nt);

    CHECK("any" == toString(normalizer.typeFromNormal(*nt)));
}

TEST_CASE_FIXTURE(NormalizeFixture, "cyclic_union")
{
    // T where T = any & (number | T)
    TypeId t = arena.addType(BlockedType{});
    TypeId u = arena.addType(UnionType{{builtinTypes->numberType, t}});
    asMutable(t)->ty.emplace<IntersectionType>(IntersectionType{{builtinTypes->anyType, u}});

    std::shared_ptr<const NormalizedType> nt = normalizer.normalize(t);
    REQUIRE(nt);

    CHECK("number" == toString(normalizer.typeFromNormal(*nt)));
}

TEST_CASE_FIXTURE(NormalizeFixture, "cyclic_union_of_intersection")
{
    // t1 where t1 = (string & t1) | string
    TypeId boundTy = arena.addType(BlockedType{});
    TypeId intersectTy = arena.addType(IntersectionType{{builtinTypes->stringType, boundTy}});
    TypeId unionTy = arena.addType(UnionType{{builtinTypes->stringType, intersectTy}});
    asMutable(boundTy)->reassign(Type{BoundType{unionTy}});

    std::shared_ptr<const NormalizedType> nt = normalizer.normalize(unionTy);

    CHECK("string" == toString(normalizer.typeFromNormal(*nt)));
}

TEST_CASE_FIXTURE(NormalizeFixture, "cyclic_intersection_of_unions")
{
    // t1 where t1 = (string & t1) | string
    TypeId boundTy = arena.addType(BlockedType{});
    TypeId unionTy = arena.addType(UnionType{{builtinTypes->stringType, boundTy}});
    TypeId intersectionTy = arena.addType(IntersectionType{{builtinTypes->stringType, unionTy}});
    asMutable(boundTy)->reassign(Type{BoundType{intersectionTy}});

    std::shared_ptr<const NormalizedType> nt = normalizer.normalize(intersectionTy);

    CHECK("string" == toString(normalizer.typeFromNormal(*nt)));
}

TEST_CASE_FIXTURE(NormalizeFixture, "crazy_metatable")
{
    CHECK("never" == toString(normal("Mt<{}, number> & Mt<{}, string>")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "negations_of_classes")
{
    createSomeClasses(&frontend);
    CHECK("(Parent & ~Child) | Unrelated" == toString(normal("(Parent & Not<Child>) | Unrelated")));
    CHECK("((class & ~Child) | boolean | buffer | function | number | string | table | thread)?" == toString(normal("Not<Child>")));
    CHECK("Child" == toString(normal("Not<Parent> & Child")));
    CHECK("((class & ~Parent) | Child | boolean | buffer | function | number | string | table | thread)?" == toString(normal("Not<Parent> | Child")));
    CHECK("(boolean | buffer | function | number | string | table | thread)?" == toString(normal("Not<cls>")));
    CHECK("(Parent | Unrelated | boolean | buffer | function | number | string | table | thread)?" ==
          toString(normal("Not<cls & Not<Parent> & Not<Child> & Not<Unrelated>>")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "classes_and_unknown")
{
    createSomeClasses(&frontend);
    CHECK("Parent" == toString(normal("Parent & unknown")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "classes_and_never")
{
    createSomeClasses(&frontend);
    CHECK("never" == toString(normal("Parent & never")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "top_table_type")
{
    CHECK("table" == toString(normal("{} | tbl")));
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK("{  }" == toString(normal("{} & tbl")));
    else
        CHECK("{|  |}" == toString(normal("{} & tbl")));
    CHECK("never" == toString(normal("number & tbl")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "negations_of_tables")
{
    CHECK(nullptr == toNormalizedType("Not<{}>"));
    CHECK("(boolean | buffer | class | function | number | string | thread)?" == toString(normal("Not<tbl>")));
    CHECK("table" == toString(normal("Not<Not<tbl>>")));
}

TEST_CASE_FIXTURE(NormalizeFixture, "normalize_blocked_types")
{
    Type blocked{BlockedType{}};

    std::shared_ptr<const NormalizedType> norm = normalizer.normalize(&blocked);

    CHECK_EQ(normalizer.typeFromNormal(*norm), &blocked);
}

TEST_CASE_FIXTURE(NormalizeFixture, "normalize_is_exactly_number")
{
    std::shared_ptr<const NormalizedType> number = normalizer.normalize(builtinTypes->numberType);
    // 1. all types for which Types::number say true for, NormalizedType::isExactlyNumber should say true as well
    CHECK(Luau::isNumber(builtinTypes->numberType) == number->isExactlyNumber());
    // 2. isExactlyNumber should handle cases like `number & number`
    TypeId intersection = arena.addType(IntersectionType{{builtinTypes->numberType, builtinTypes->numberType}});
    std::shared_ptr<const NormalizedType> normIntersection = normalizer.normalize(intersection);
    CHECK(normIntersection->isExactlyNumber());

    // 3. isExactlyNumber should reject things that are definitely not precisely numbers `number | any`

    TypeId yoonion = arena.addType(UnionType{{builtinTypes->anyType, builtinTypes->numberType}});
    std::shared_ptr<const NormalizedType> unionIntersection = normalizer.normalize(yoonion);
    CHECK(!unionIntersection->isExactlyNumber());
}

TEST_CASE_FIXTURE(NormalizeFixture, "normalize_unknown")
{
    auto nt = toNormalizedType("Not<string> | Not<number>");
    CHECK(nt);
    CHECK(nt->isUnknown());
    CHECK(toString(normalizer.typeFromNormal(*nt)) == "unknown");
}

TEST_CASE_FIXTURE(NormalizeFixture, "read_only_props")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CHECK("{ x: string }" == toString(normal("{ read x: string } & { x: string }"), {true}));
    CHECK("{ x: string }" == toString(normal("{ x: string } & { read x: string }"), {true}));
}

TEST_CASE_FIXTURE(NormalizeFixture, "read_only_props_2")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CHECK(R"({ x: "hello" })" == toString(normal(R"({ x: "hello" } & { x: string })"), {true}));
    CHECK(R"(never)" == toString(normal(R"({ x: "hello" } & { x: "world" })"), {true}));
}

TEST_CASE_FIXTURE(NormalizeFixture, "read_only_props_3")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CHECK(R"({ read x: "hello" })" == toString(normal(R"({ read x: "hello" } & { read x: string })"), {true}));
    CHECK("never" == toString(normal(R"({ read x: "hello" } & { read x: "world" })"), {true}));
}

TEST_CASE_FIXTURE(NormalizeFixture, "final_types_are_cached")
{
    std::shared_ptr<const NormalizedType> na1 = normalizer.normalize(builtinTypes->numberType);
    std::shared_ptr<const NormalizedType> na2 = normalizer.normalize(builtinTypes->numberType);

    CHECK(na1 == na2);
}

TEST_CASE_FIXTURE(NormalizeFixture, "non_final_types_can_be_normalized_but_are_not_cached")
{
    TypeId a = arena.freshType(&globalScope);

    std::shared_ptr<const NormalizedType> na1 = normalizer.normalize(a);
    std::shared_ptr<const NormalizedType> na2 = normalizer.normalize(a);

    CHECK(na1 != na2);
}

TEST_CASE_FIXTURE(NormalizeFixture, "intersect_with_not_unknown")
{
    ScopedFastFlag sff{FFlag::LuauNormalizeNotUnknownIntersection, true};

    TypeId notUnknown = arena.addType(NegationType{builtinTypes->unknownType});
    TypeId type = arena.addType(IntersectionType{{builtinTypes->numberType, notUnknown}});
    std::shared_ptr<const NormalizedType> normalized = normalizer.normalize(type);

    CHECK("never" == toString(normalizer.typeFromNormal(*normalized.get())));
}

TEST_CASE_FIXTURE(NormalizeFixture, "cyclic_stack_overflow_1")
{
    ScopedFastInt sfi{FInt::LuauTypeInferRecursionLimit, 165};
    this->unifierState.counters.recursionLimit = FInt::LuauTypeInferRecursionLimit;
    TypeId t1 = arena.addType(TableType{});
    TypeId t2 = arena.addType(TableType{});
    TypeId t3 = arena.addType(IntersectionType{{t1, t2}});
    asMutable(t1)->ty.get_if<TableType>()->props = {{"foo", Property::readonly(t2)}};
    asMutable(t2)->ty.get_if<TableType>()->props = {{"foo", Property::readonly(t1)}};

    std::shared_ptr<const NormalizedType> normalized = normalizer.normalize(t3);
    CHECK(normalized);
}

TEST_CASE_FIXTURE(NormalizeFixture, "cyclic_stack_overflow_2")
{
    ScopedFastInt sfi{FInt::LuauTypeInferRecursionLimit, 165};
    this->unifierState.counters.recursionLimit = FInt::LuauTypeInferRecursionLimit;
    TypeId t1 = arena.addType(TableType{});
    TypeId t2 = arena.addType(TableType{});
    TypeId t3 = arena.addType(IntersectionType{{t1, t2}});
    asMutable(t1)->ty.get_if<TableType>()->props = {{"foo", Property::readonly(t3)}};
    asMutable(t2)->ty.get_if<TableType>()->props = {{"foo", Property::readonly(t1)}};

    std::shared_ptr<const NormalizedType> normalized = normalizer.normalize(t3);
    CHECK(normalized);
}

TEST_SUITE_END();
