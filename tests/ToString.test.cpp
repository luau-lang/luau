// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Scope.h"
#include "Luau/ToString.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauRecursiveTypeParameterRestriction);

TEST_SUITE_BEGIN("ToString");

TEST_CASE_FIXTURE(Fixture, "primitive")
{
    CheckResult result = check("local a = nil    local b = 44    local c = 'lalala'    local d = true");
    LUAU_REQUIRE_NO_ERRORS(result);

    // A variable without an annotation and with a nil literal should infer as 'free', not 'nil'
    CHECK_NE("nil", toString(requireType("a")));

    CHECK_EQ("number", toString(requireType("b")));
    CHECK_EQ("string", toString(requireType("c")));
    CHECK_EQ("boolean", toString(requireType("d")));
}

TEST_CASE_FIXTURE(Fixture, "bound_types")
{
    CheckResult result = check("local a = 444    local b = a");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "free_types")
{
    CheckResult result = check("local a");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("a", toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "cyclic_table")
{
    TypeVar cyclicTable{TypeVariant(TableTypeVar())};
    TableTypeVar* tableOne = getMutable<TableTypeVar>(&cyclicTable);
    tableOne->props["self"] = {&cyclicTable};

    CHECK_EQ("t1 where t1 = { self: t1 }", toString(&cyclicTable));
}

TEST_CASE_FIXTURE(Fixture, "named_table")
{
    TypeVar table{TypeVariant(TableTypeVar())};
    TableTypeVar* t = getMutable<TableTypeVar>(&table);
    t->name = "TheTable";

    CHECK_EQ("TheTable", toString(&table));
}

TEST_CASE_FIXTURE(Fixture, "empty_table")
{
    ScopedFastFlag LuauToStringTableBracesNewlines("LuauToStringTableBracesNewlines", true);
    CheckResult result = check(R"(
        local a: {}
    )");

    CHECK_EQ("{|  |}", toString(requireType("a")));

    // Should stay the same with useLineBreaks enabled
    ToStringOptions opts;
    opts.useLineBreaks = true;
    CHECK_EQ("{|  |}", toString(requireType("a"), opts));
}

TEST_CASE_FIXTURE(Fixture, "table_respects_use_line_break")
{
    ScopedFastFlag LuauToStringTableBracesNewlines("LuauToStringTableBracesNewlines", true);
    CheckResult result = check(R"(
        local a: { prop: string, anotherProp: number, thirdProp: boolean }
    )");

    ToStringOptions opts;
    opts.useLineBreaks = true;
    opts.indent = true;

    //clang-format off
    CHECK_EQ("{|\n"
             "    anotherProp: number,\n"
             "    prop: string,\n"
             "    thirdProp: boolean\n"
             "|}",
        toString(requireType("a"), opts));
    //clang-format on
}

TEST_CASE_FIXTURE(BuiltinsFixture, "exhaustive_toString_of_cyclic_table")
{
    CheckResult result = check(R"(
        --!strict
        local Vec3 = {}
        Vec3.__index = Vec3
        function Vec3.new()
            return setmetatable({x=0, y=0, z=0}, Vec3)
        end

        export type Vec3 = typeof(Vec3.new())

        local thefun: any = function(self, o) return self end

        local multiply: ((Vec3, Vec3) -> Vec3) & ((Vec3, number) -> Vec3) = thefun

        Vec3.__mul = multiply

        local a = Vec3.new()
    )");

    std::string a = toString(requireType("a"), {true});

    CHECK_EQ(std::string::npos, a.find("CYCLE"));
    CHECK_EQ(std::string::npos, a.find("TRUNCATED"));

    //clang-format off
    CHECK_EQ("t2 where "
             "t1 = { __index: t1, __mul: ((t2, number) -> t2) & ((t2, t2) -> t2), new: () -> t2 } ; "
             "t2 = { @metatable t1, {| x: number, y: number, z: number |} }",
        a);
    //clang-format on
}


TEST_CASE_FIXTURE(Fixture, "intersection_parenthesized_only_if_needed")
{
    auto utv = TypeVar{UnionTypeVar{{typeChecker.numberType, typeChecker.stringType}}};
    auto itv = TypeVar{IntersectionTypeVar{{&utv, typeChecker.booleanType}}};

    CHECK_EQ(toString(&itv), "(number | string) & boolean");
}

TEST_CASE_FIXTURE(Fixture, "union_parenthesized_only_if_needed")
{
    auto itv = TypeVar{IntersectionTypeVar{{typeChecker.numberType, typeChecker.stringType}}};
    auto utv = TypeVar{UnionTypeVar{{&itv, typeChecker.booleanType}}};

    CHECK_EQ(toString(&utv), "(number & string) | boolean");
}

TEST_CASE_FIXTURE(Fixture, "functions_are_always_parenthesized_in_unions_or_intersections")
{
    auto stringAndNumberPack = TypePackVar{TypePack{{typeChecker.stringType, typeChecker.numberType}}};
    auto numberAndStringPack = TypePackVar{TypePack{{typeChecker.numberType, typeChecker.stringType}}};

    auto sn2ns = TypeVar{FunctionTypeVar{&stringAndNumberPack, &numberAndStringPack}};
    auto ns2sn = TypeVar{FunctionTypeVar(typeChecker.globalScope->level, &numberAndStringPack, &stringAndNumberPack)};

    auto utv = TypeVar{UnionTypeVar{{&ns2sn, &sn2ns}}};
    auto itv = TypeVar{IntersectionTypeVar{{&ns2sn, &sn2ns}}};

    CHECK_EQ(toString(&utv), "((number, string) -> (string, number)) | ((string, number) -> (number, string))");
    CHECK_EQ(toString(&itv), "((number, string) -> (string, number)) & ((string, number) -> (number, string))");
}

TEST_CASE_FIXTURE(Fixture, "intersections_respects_use_line_breaks")
{
    CheckResult result = check(R"(
        local a: ((string) -> string) & ((number) -> number)
    )");

    ToStringOptions opts;
    opts.useLineBreaks = true;

    //clang-format off
    CHECK_EQ("((number) -> number)\n"
             "& ((string) -> string)",
        toString(requireType("a"), opts));
    //clang-format on
}

TEST_CASE_FIXTURE(Fixture, "unions_respects_use_line_breaks")
{
    CheckResult result = check(R"(
        local a: string | number | boolean
    )");

    ToStringOptions opts;
    opts.useLineBreaks = true;

    //clang-format off
    CHECK_EQ("boolean\n"
             "| number\n"
             "| string",
        toString(requireType("a"), opts));
    //clang-format on
}

TEST_CASE_FIXTURE(Fixture, "quit_stringifying_table_type_when_length_is_exceeded")
{
    TableTypeVar ttv{};
    for (char c : std::string("abcdefghijklmno"))
        ttv.props[std::string(1, c)] = {typeChecker.numberType};

    TypeVar tv{ttv};

    ToStringOptions o;
    o.exhaustive = false;
    o.maxTableLength = 40;
    CHECK_EQ(toString(&tv, o), "{ a: number, b: number, c: number, d: number, e: number, ... 10 more ... }");
}

TEST_CASE_FIXTURE(Fixture, "stringifying_table_type_is_still_capped_when_exhaustive")
{
    TableTypeVar ttv{};
    for (char c : std::string("abcdefg"))
        ttv.props[std::string(1, c)] = {typeChecker.numberType};

    TypeVar tv{ttv};

    ToStringOptions o;
    o.exhaustive = true;
    o.maxTableLength = 40;
    CHECK_EQ(toString(&tv, o), "{ a: number, b: number, c: number, d: number, e: number, ... 2 more ... }");
}

TEST_CASE_FIXTURE(Fixture, "quit_stringifying_type_when_length_is_exceeded")
{
    CheckResult result = check(R"(
        function f0() end
        function f1(f) return f or f0 end
        function f2(f) return f or f1 end
        function f3(f) return f or f2 end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions o;
    o.exhaustive = false;
    o.maxTypeLength = 40;
    CHECK_EQ(toString(requireType("f0"), o), "() -> ()");
    CHECK_EQ(toString(requireType("f1"), o), "(() -> ()) -> () -> ()");
    CHECK_EQ(toString(requireType("f2"), o), "((() -> ()) -> () -> ()) -> (() -> ()) -> ... <TRUNCATED>");
    CHECK_EQ(toString(requireType("f3"), o), "(((() -> ()) -> () -> ()) -> (() -> ()) -> ... <TRUNCATED>");
}

TEST_CASE_FIXTURE(Fixture, "stringifying_type_is_still_capped_when_exhaustive")
{
    CheckResult result = check(R"(
        function f0() end
        function f1(f) return f or f0 end
        function f2(f) return f or f1 end
        function f3(f) return f or f2 end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions o;
    o.exhaustive = true;
    o.maxTypeLength = 40;
    CHECK_EQ(toString(requireType("f0"), o), "() -> ()");
    CHECK_EQ(toString(requireType("f1"), o), "(() -> ()) -> () -> ()");
    CHECK_EQ(toString(requireType("f2"), o), "((() -> ()) -> () -> ()) -> (() -> ()) -> ... <TRUNCATED>");
    CHECK_EQ(toString(requireType("f3"), o), "(((() -> ()) -> () -> ()) -> (() -> ()) -> ... <TRUNCATED>");
}

TEST_CASE_FIXTURE(Fixture, "stringifying_table_type_correctly_use_matching_table_state_braces")
{
    TableTypeVar ttv{TableState::Sealed, TypeLevel{}};
    for (char c : std::string("abcdefghij"))
        ttv.props[std::string(1, c)] = {typeChecker.numberType};

    TypeVar tv{ttv};

    ToStringOptions o;
    o.maxTableLength = 40;
    CHECK_EQ(toString(&tv, o), "{| a: number, b: number, c: number, d: number, e: number, ... 5 more ... |}");
}

TEST_CASE_FIXTURE(Fixture, "stringifying_cyclic_union_type_bails_early")
{
    TypeVar tv{UnionTypeVar{{typeChecker.stringType, typeChecker.numberType}}};
    UnionTypeVar* utv = getMutable<UnionTypeVar>(&tv);
    utv->options.push_back(&tv);
    utv->options.push_back(&tv);

    CHECK_EQ("t1 where t1 = number | string", toString(&tv));
}

TEST_CASE_FIXTURE(Fixture, "stringifying_cyclic_intersection_type_bails_early")
{
    TypeVar tv{IntersectionTypeVar{}};
    IntersectionTypeVar* itv = getMutable<IntersectionTypeVar>(&tv);
    itv->parts.push_back(&tv);
    itv->parts.push_back(&tv);

    CHECK_EQ("t1 where t1 = t1 & t1", toString(&tv));
}

TEST_CASE_FIXTURE(Fixture, "stringifying_array_uses_array_syntax")
{
    TableTypeVar ttv{TableState::Sealed, TypeLevel{}};
    ttv.indexer = TableIndexer{typeChecker.numberType, typeChecker.stringType};

    CHECK_EQ("{string}", toString(TypeVar{ttv}));

    ttv.props["A"] = {typeChecker.numberType};
    CHECK_EQ("{| [number]: string, A: number |}", toString(TypeVar{ttv}));

    ttv.props.clear();
    ttv.state = TableState::Unsealed;
    CHECK_EQ("{string}", toString(TypeVar{ttv}));
}


TEST_CASE_FIXTURE(Fixture, "generic_packs_are_stringified_differently_from_generic_types")
{
    TypePackVar tpv{GenericTypePack{"a"}};
    CHECK_EQ(toString(&tpv), "a...");

    TypeVar tv{GenericTypeVar{"a"}};
    CHECK_EQ(toString(&tv), "a");
}

TEST_CASE_FIXTURE(Fixture, "function_type_with_argument_names")
{
    CheckResult result = check("type MyFunc = (a: number, string, c: number) -> string; local a : MyFunc");
    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions opts;
    opts.functionTypeArguments = true;
    CHECK_EQ("(a: number, string, c: number) -> string", toString(requireType("a"), opts));
}

TEST_CASE_FIXTURE(Fixture, "function_type_with_argument_names_generic")
{
    CheckResult result = check("local function f<a...>(n: number, ...: a...): (a...) return ... end");
    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions opts;
    opts.functionTypeArguments = true;
    CHECK_EQ("<a...>(n: number, a...) -> (a...)", toString(requireType("f"), opts));
}

TEST_CASE_FIXTURE(Fixture, "function_type_with_argument_names_and_self")
{
    CheckResult result = check(R"(
local tbl = {}
tbl.a = 2
function tbl:foo(b: number, c: number) return (self.a :: number) + b + c end
type Table = typeof(tbl)
type Foo = typeof(tbl.foo)
local u: Foo
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions opts;
    opts.functionTypeArguments = true;
    // Can't guess the name of 'self' to compare name, but at least there should be no assertion
    toString(requireType("u"), opts);
}

TEST_CASE_FIXTURE(Fixture, "generate_friendly_names_for_inferred_generics")
{
    CheckResult result = check(R"(
        function id(x) return x end

        function id2(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30)
            return a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("<a>(a) -> a", toString(requireType("id")));

    CHECK_EQ("<a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a1, b1, c1, d1>(a, b, c, d, e, f, g, h, i, j, k, l, "
             "m, n, o, p, q, r, s, t, u, v, w, x, y, z, a1, b1, c1, d1) -> (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, "
             "x, y, z, a1, b1, c1, d1)",
        toString(requireType("id2")));
}

TEST_CASE_FIXTURE(Fixture, "toStringDetailed")
{
    CheckResult result = check(R"(
        function id3(a, b, c)
            return a, b, c
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId id3Type = requireType("id3");
    ToStringResult nameData = toStringDetailed(id3Type);

    REQUIRE_EQ(3, nameData.nameMap.typeVars.size());
    REQUIRE_EQ("<a, b, c>(a, b, c) -> (a, b, c)", nameData.name);

    ToStringOptions opts;
    opts.nameMap = std::move(nameData.nameMap);

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(follow(id3Type));
    REQUIRE(ftv != nullptr);

    auto params = flatten(ftv->argTypes).first;
    REQUIRE_EQ(3, params.size());

    REQUIRE_EQ("a", toString(params[0], opts));
    REQUIRE_EQ("b", toString(params[1], opts));
    REQUIRE_EQ("c", toString(params[2], opts));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "toStringDetailed2")
{
    CheckResult result = check(R"(
        local base = {}
        function base:one() return 1 end

        local child = {}
        setmetatable(child, {__index=base})
        function child:two() return 2 end

        local inst = {}
        setmetatable(inst, {__index=child})
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId tType = requireType("inst");
    ToStringResult r = toStringDetailed(tType);
    CHECK_EQ("{ @metatable { __index: { @metatable { __index: base }, child } }, inst }", r.name);
    CHECK_EQ(0, r.nameMap.typeVars.size());

    ToStringOptions opts;
    opts.nameMap = r.nameMap;

    const MetatableTypeVar* tMeta = get<MetatableTypeVar>(tType);
    REQUIRE(tMeta);

    TableTypeVar* tMeta2 = getMutable<TableTypeVar>(tMeta->metatable);
    REQUIRE(tMeta2);
    REQUIRE(tMeta2->props.count("__index"));

    const MetatableTypeVar* tMeta3 = get<MetatableTypeVar>(tMeta2->props["__index"].type);
    REQUIRE(tMeta3);

    TableTypeVar* tMeta4 = getMutable<TableTypeVar>(tMeta3->metatable);
    REQUIRE(tMeta4);
    REQUIRE(tMeta4->props.count("__index"));

    TableTypeVar* tMeta5 = getMutable<TableTypeVar>(tMeta4->props["__index"].type);
    REQUIRE(tMeta5);

    TableTypeVar* tMeta6 = getMutable<TableTypeVar>(tMeta3->table);
    REQUIRE(tMeta6);

    ToStringResult oneResult = toStringDetailed(tMeta5->props["one"].type, opts);
    opts.nameMap = oneResult.nameMap;

    std::string twoResult = toString(tMeta6->props["two"].type, opts);

    REQUIRE_EQ("<a>(a) -> number", oneResult.name);
    REQUIRE_EQ("<b>(b) -> number", twoResult);
}


TEST_CASE_FIXTURE(Fixture, "toStringErrorPack")
{
    CheckResult result = check(R"(
local function target(callback: nil) return callback(4, "hello") end
    )");

    LUAU_REQUIRE_ERRORS(result);
    CHECK_EQ("(nil) -> (*unknown*)", toString(requireType("target")));
}

TEST_CASE_FIXTURE(Fixture, "toStringGenericPack")
{
    CheckResult result = check(R"(
function foo(a, b) return a(b) end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("foo")), "<a, b...>((a) -> (b...), a) -> (b...)");
}

TEST_CASE_FIXTURE(Fixture, "toString_the_boundTo_table_type_contained_within_a_TypePack")
{
    TypeVar tv1{TableTypeVar{}};
    TableTypeVar* ttv = getMutable<TableTypeVar>(&tv1);
    ttv->state = TableState::Sealed;
    ttv->props["hello"] = {typeChecker.numberType};
    ttv->props["world"] = {typeChecker.numberType};

    TypePackVar tpv1{TypePack{{&tv1}}};

    TypeVar tv2{TableTypeVar{}};
    TableTypeVar* bttv = getMutable<TableTypeVar>(&tv2);
    bttv->state = TableState::Free;
    bttv->props["hello"] = {typeChecker.numberType};
    bttv->boundTo = &tv1;

    TypePackVar tpv2{TypePack{{&tv2}}};

    CHECK_EQ("{| hello: number, world: number |}", toString(&tpv1));
    CHECK_EQ("{| hello: number, world: number |}", toString(&tpv2));
}

TEST_CASE_FIXTURE(Fixture, "no_parentheses_around_cyclic_function_type_in_union")
{
    CheckResult result = check(R"(
        type F = ((() -> number)?) -> F?
        local function f(p) return f end
        local g: F = f
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("t1 where t1 = ((() -> number)?) -> t1?", toString(requireType("g")));
}

TEST_CASE_FIXTURE(Fixture, "no_parentheses_around_cyclic_function_type_in_intersection")
{
    CheckResult result = check(R"(
        function f() return f end
        local a: ((number) -> ()) & typeof(f)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("((number) -> ()) & t1 where t1 = () -> t1", toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "self_recursive_instantiated_param")
{
    TypeVar tableTy{TableTypeVar{}};
    TableTypeVar* ttv = getMutable<TableTypeVar>(&tableTy);
    ttv->name = "Table";
    ttv->instantiatedTypeParams.push_back(&tableTy);

    CHECK_EQ(toString(tableTy), "Table<Table>");
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_id")
{
    CheckResult result = check(R"(
        local function id(x) return x end
    )");

    TypeId ty = requireType("id");
    const FunctionTypeVar* ftv = get<FunctionTypeVar>(follow(ty));

    CHECK_EQ("id<a>(x: a): a", toStringNamedFunction("id", *ftv));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_map")
{
    CheckResult result = check(R"(
        local function map(arr, fn)
            local t = {}
            for i = 0, #arr do
                t[i] = fn(arr[i])
            end
            return t
        end
    )");

    TypeId ty = requireType("map");
    const FunctionTypeVar* ftv = get<FunctionTypeVar>(follow(ty));

    CHECK_EQ("map<a, b>(arr: {a}, fn: (a) -> b): {b}", toStringNamedFunction("map", *ftv));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_generic_pack")
{
    CheckResult result = check(R"(
        local function f(a: number, b: string) end
        local function test<T..., U...>(...: T...): U...
            f(...)
            return 1, 2, 3
        end
    )");

    TypeId ty = requireType("test");
    const FunctionTypeVar* ftv = get<FunctionTypeVar>(follow(ty));

    CHECK_EQ("test<T..., U...>(...: T...): U...", toStringNamedFunction("test", *ftv));
}

TEST_CASE("toStringNamedFunction_unit_f")
{
    TypePackVar empty{TypePack{}};
    FunctionTypeVar ftv{&empty, &empty, {}, false};
    CHECK_EQ("f(): ()", toStringNamedFunction("f", ftv));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_variadics")
{
    CheckResult result = check(R"(
        local function f<a, b...>(x: a, ...): (a, a, b...)
            return x, x, ...
        end
    )");

    TypeId ty = requireType("f");
    auto ftv = get<FunctionTypeVar>(follow(ty));

    CHECK_EQ("f<a, b...>(x: a, ...: any): (a, a, b...)", toStringNamedFunction("f", *ftv));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_variadics2")
{
    CheckResult result = check(R"(
        local function f(): ...number
            return 1, 2, 3
        end
    )");

    TypeId ty = requireType("f");
    auto ftv = get<FunctionTypeVar>(follow(ty));

    CHECK_EQ("f(): ...number", toStringNamedFunction("f", *ftv));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_variadics3")
{
    CheckResult result = check(R"(
        local function f(): (string, ...number)
            return 'a', 1, 2, 3
        end
    )");

    TypeId ty = requireType("f");
    auto ftv = get<FunctionTypeVar>(follow(ty));

    CHECK_EQ("f(): (string, ...number)", toStringNamedFunction("f", *ftv));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_type_annotation_has_partial_argnames")
{
    CheckResult result = check(R"(
        local f: (number, y: number) -> number
    )");

    TypeId ty = requireType("f");
    auto ftv = get<FunctionTypeVar>(follow(ty));

    CHECK_EQ("f(_: number, y: number): number", toStringNamedFunction("f", *ftv));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_hide_type_params")
{
    CheckResult result = check(R"(
        local function f<T>(x: T, g: <U>(T) -> U)): ()
        end
    )");

    TypeId ty = requireType("f");
    auto ftv = get<FunctionTypeVar>(follow(ty));

    ToStringOptions opts;
    opts.hideNamedFunctionTypeParameters = true;
    CHECK_EQ("f(x: T, g: <U>(T) -> U): ()", toStringNamedFunction("f", *ftv, opts));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_overrides_param_names")
{
    CheckResult result = check(R"(
        local function test(a, b : string, ... : number) return a end
    )");

    TypeId ty = requireType("test");
    const FunctionTypeVar* ftv = get<FunctionTypeVar>(follow(ty));

    ToStringOptions opts;
    opts.namedFunctionOverrideArgNames = {"first", "second", "third"};
    CHECK_EQ("test<a>(first: a, second: string, ...: number): a", toStringNamedFunction("test", *ftv, opts));
}

TEST_CASE_FIXTURE(Fixture, "pick_distinct_names_for_mixed_explicit_and_implicit_generics")
{
    ScopedFastFlag sff[] = {
        {"LuauAlwaysQuantify", true},
    };

    CheckResult result = check(R"(
        function foo<a>(x: a, y) end
    )");

    CHECK("<a, b>(a, b) -> ()" == toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_include_self_param")
{
    CheckResult result = check(R"(
        local foo = {}
        function foo:method(arg: string): ()
        end
    )");

    TypeId parentTy = requireType("foo");
    auto ttv = get<TableTypeVar>(follow(parentTy));
    auto ftv = get<FunctionTypeVar>(ttv->props.at("method").type);

    CHECK_EQ("foo:method<a>(self: a, arg: string): ()", toStringNamedFunction("foo:method", *ftv));
}


TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_hide_self_param")
{
    CheckResult result = check(R"(
        local foo = {}
        function foo:method(arg: string): ()
        end
    )");

    TypeId parentTy = requireType("foo");
    auto ttv = get<TableTypeVar>(follow(parentTy));
    auto ftv = get<FunctionTypeVar>(ttv->props.at("method").type);

    ToStringOptions opts;
    opts.hideFunctionSelfArgument = true;
    CHECK_EQ("foo:method<a>(arg: string): ()", toStringNamedFunction("foo:method", *ftv, opts));
}

TEST_SUITE_END();
