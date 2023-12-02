// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Scope.h"
#include "Luau/ToString.h"

#include "Fixture.h"

#include "ScopedFlags.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauRecursiveTypeParameterRestriction);
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAG(LuauCheckedFunctionSyntax);
LUAU_FASTFLAG(DebugLuauSharedSelf);

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
    Type cyclicTable{TypeVariant(TableType())};
    TableType* tableOne = getMutable<TableType>(&cyclicTable);
    tableOne->props["self"] = {&cyclicTable};

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("t1 where t1 = {| self: t1 |}", toString(&cyclicTable));
    else
        CHECK_EQ("t1 where t1 = { self: t1 }", toString(&cyclicTable));
}

TEST_CASE_FIXTURE(Fixture, "named_table")
{
    Type table{TypeVariant(TableType())};
    TableType* t = getMutable<TableType>(&table);
    t->name = "TheTable";

    CHECK_EQ("TheTable", toString(&table));
}

TEST_CASE_FIXTURE(Fixture, "empty_table")
{
    CheckResult result = check(R"(
        local a: {}
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("{  }", toString(requireType("a")));
    else
        CHECK_EQ("{|  |}", toString(requireType("a")));

    // Should stay the same with useLineBreaks enabled
    ToStringOptions opts;
    opts.useLineBreaks = true;
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("{  }", toString(requireType("a"), opts));
    else
        CHECK_EQ("{|  |}", toString(requireType("a"), opts));
}

TEST_CASE_FIXTURE(Fixture, "table_respects_use_line_break")
{
    CheckResult result = check(R"(
        local a: { prop: string, anotherProp: number, thirdProp: boolean }
    )");

    ToStringOptions opts;
    opts.useLineBreaks = true;

    //clang-format off
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("{\n"
                 "    anotherProp: number,\n"
                 "    prop: string,\n"
                 "    thirdProp: boolean\n"
                 "}",
            toString(requireType("a"), opts));
    else
        CHECK_EQ("{|\n"
                 "    anotherProp: number,\n"
                 "    prop: string,\n"
                 "    thirdProp: boolean\n"
                 "|}",
            toString(requireType("a"), opts));
    //clang-format on
}

TEST_CASE_FIXTURE(Fixture, "nil_or_nil_is_nil_not_question_mark")
{
    CheckResult result = check(R"(
      type nil_ty = nil | nil
      local a : nil_ty = nil
  )");
    ToStringOptions opts;
    opts.useLineBreaks = false;
    CHECK_EQ("nil", toString(requireType("a"), opts));
}

TEST_CASE_FIXTURE(Fixture, "long_disjunct_of_nil_is_nil_not_question_mark")
{
    CheckResult result = check(R"(
      type nil_ty = nil | nil | nil | nil | nil
      local a : nil_ty = nil
  )");
    ToStringOptions opts;
    opts.useLineBreaks = false;
    CHECK_EQ("nil", toString(requireType("a"), opts));
}

TEST_CASE_FIXTURE(Fixture, "metatable")
{
    Type table{TypeVariant(TableType())};
    Type metatable{TypeVariant(TableType())};
    Type mtv{TypeVariant(MetatableType{&table, &metatable})};
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("{ @metatable {|  |}, {|  |} }", toString(&mtv));
    else
        CHECK_EQ("{ @metatable {  }, {  } }", toString(&mtv));
}

TEST_CASE_FIXTURE(Fixture, "named_metatable")
{
    Type table{TypeVariant(TableType())};
    Type metatable{TypeVariant(TableType())};
    Type mtv{TypeVariant(MetatableType{&table, &metatable, "NamedMetatable"})};
    CHECK_EQ("NamedMetatable", toString(&mtv));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "named_metatable_toStringNamedFunction")
{
    CheckResult result = check(R"(
        local function createTbl(): NamedMetatable
            return setmetatable({}, {})
        end
        type NamedMetatable = typeof(createTbl())
    )");

    TypeId ty = requireType("createTbl");
    const FunctionType* ftv = get<FunctionType>(follow(ty));
    REQUIRE(ftv);
    CHECK_EQ("createTbl(): NamedMetatable", toStringNamedFunction("createTbl", *ftv));
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
    auto utv = Type{UnionType{{builtinTypes->numberType, builtinTypes->stringType}}};
    auto itv = Type{IntersectionType{{&utv, builtinTypes->booleanType}}};

    CHECK_EQ(toString(&itv), "(number | string) & boolean");
}

TEST_CASE_FIXTURE(Fixture, "union_parenthesized_only_if_needed")
{
    auto itv = Type{IntersectionType{{builtinTypes->numberType, builtinTypes->stringType}}};
    auto utv = Type{UnionType{{&itv, builtinTypes->booleanType}}};

    CHECK_EQ(toString(&utv), "(number & string) | boolean");
}

TEST_CASE_FIXTURE(Fixture, "functions_are_always_parenthesized_in_unions_or_intersections")
{
    auto stringAndNumberPack = TypePackVar{TypePack{{builtinTypes->stringType, builtinTypes->numberType}}};
    auto numberAndStringPack = TypePackVar{TypePack{{builtinTypes->numberType, builtinTypes->stringType}}};

    auto sn2ns = Type{FunctionType{&stringAndNumberPack, &numberAndStringPack}};
    auto ns2sn = Type{FunctionType(frontend.globals.globalScope->level, &numberAndStringPack, &stringAndNumberPack)};

    auto utv = Type{UnionType{{&ns2sn, &sn2ns}}};
    auto itv = Type{IntersectionType{{&ns2sn, &sn2ns}}};

    CHECK_EQ(toString(&utv), "((number, string) -> (string, number)) | ((string, number) -> (number, string))");
    CHECK_EQ(toString(&itv), "((number, string) -> (string, number)) & ((string, number) -> (number, string))");
}

TEST_CASE_FIXTURE(Fixture, "simple_intersections_printed_on_one_line")
{
    ScopedFastFlag sff{FFlag::LuauToStringSimpleCompositeTypesSingleLine, true};
    CheckResult result = check(R"(
        local a: string & number
    )");

    ToStringOptions opts;
    opts.useLineBreaks = true;

    CHECK_EQ("number & string", toString(requireType("a"), opts));
}

TEST_CASE_FIXTURE(Fixture, "complex_intersections_printed_on_multiple_lines")
{
    ScopedFastFlag sff{FFlag::LuauToStringSimpleCompositeTypesSingleLine, true};
    CheckResult result = check(R"(
        local a: string & number & boolean
    )");

    ToStringOptions opts;
    opts.useLineBreaks = true;
    opts.compositeTypesSingleLineLimit = 2;

    //clang-format off
    CHECK_EQ("boolean\n"
             "& number\n"
             "& string",
        toString(requireType("a"), opts));
    //clang-format on
}

TEST_CASE_FIXTURE(Fixture, "overloaded_functions_always_printed_on_multiple_lines")
{
    ScopedFastFlag sff{FFlag::LuauToStringSimpleCompositeTypesSingleLine, true};
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

TEST_CASE_FIXTURE(Fixture, "simple_unions_printed_on_one_line")
{
    ScopedFastFlag sff{FFlag::LuauToStringSimpleCompositeTypesSingleLine, true};
    CheckResult result = check(R"(
        local a: number | boolean
    )");

    ToStringOptions opts;
    opts.useLineBreaks = true;

    CHECK_EQ("boolean | number", toString(requireType("a"), opts));
}

TEST_CASE_FIXTURE(Fixture, "complex_unions_printed_on_multiple_lines")
{
    ScopedFastFlag sff{FFlag::LuauToStringSimpleCompositeTypesSingleLine, true};
    CheckResult result = check(R"(
        local a: string | number | boolean
    )");

    ToStringOptions opts;
    opts.compositeTypesSingleLineLimit = 2;
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
    TableType ttv{};
    for (char c : std::string("abcdefghijklmno"))
        ttv.props[std::string(1, c)] = {builtinTypes->numberType};

    Type tv{ttv};

    ToStringOptions o;
    o.exhaustive = false;
    o.maxTableLength = 40;
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ(toString(&tv, o), "{| a: number, b: number, c: number, d: number, e: number, ... 10 more ... |}");
    else
        CHECK_EQ(toString(&tv, o), "{ a: number, b: number, c: number, d: number, e: number, ... 10 more ... }");
}

TEST_CASE_FIXTURE(Fixture, "stringifying_table_type_is_still_capped_when_exhaustive")
{
    TableType ttv{};
    for (char c : std::string("abcdefg"))
        ttv.props[std::string(1, c)] = {builtinTypes->numberType};

    Type tv{ttv};

    ToStringOptions o;
    o.exhaustive = true;
    o.maxTableLength = 40;
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ(toString(&tv, o), "{| a: number, b: number, c: number, d: number, e: number, ... 2 more ... |}");
    else
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

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        o.maxTypeLength = 30;
        CHECK_EQ(toString(requireType("f0"), o), "() -> ()");
        CHECK_EQ(toString(requireType("f1"), o), "<a>(a) -> (() -> ()) | (a & ~(false?))... *TRUNCATED*");
        CHECK_EQ(toString(requireType("f2"), o), "<b>(b) -> (<a>(a) -> (() -> ()) | (a & ~(false?))... *TRUNCATED*");
        CHECK_EQ(toString(requireType("f3"), o), "<c>(c) -> (<b>(b) -> (<a>(a) -> (() -> ()) | (a & ~(false?))... *TRUNCATED*");
    }
    else
    {
        o.maxTypeLength = 40;
        CHECK_EQ(toString(requireType("f0"), o), "() -> ()");
        CHECK_EQ(toString(requireType("f1"), o), "(() -> ()) -> () -> ()");
        CHECK_EQ(toString(requireType("f2"), o), "((() -> ()) -> () -> ()) -> (() -> ()) -> ... *TRUNCATED*");
        CHECK_EQ(toString(requireType("f3"), o), "(((() -> ()) -> () -> ()) -> (() -> ()) -> ... *TRUNCATED*");
    }
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
    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        o.maxTypeLength = 30;
        CHECK_EQ(toString(requireType("f0"), o), "() -> ()");
        CHECK_EQ(toString(requireType("f1"), o), "<a>(a) -> (() -> ()) | (a & ~(false?))... *TRUNCATED*");
        CHECK_EQ(toString(requireType("f2"), o), "<b>(b) -> (<a>(a) -> (() -> ()) | (a & ~(false?))... *TRUNCATED*");
        CHECK_EQ(toString(requireType("f3"), o), "<c>(c) -> (<b>(b) -> (<a>(a) -> (() -> ()) | (a & ~(false?))... *TRUNCATED*");
    }
    else
    {
        o.maxTypeLength = 40;
        CHECK_EQ(toString(requireType("f0"), o), "() -> ()");
        CHECK_EQ(toString(requireType("f1"), o), "(() -> ()) -> () -> ()");
        CHECK_EQ(toString(requireType("f2"), o), "((() -> ()) -> () -> ()) -> (() -> ()) -> ... *TRUNCATED*");
        CHECK_EQ(toString(requireType("f3"), o), "(((() -> ()) -> () -> ()) -> (() -> ()) -> ... *TRUNCATED*");
    }
}

TEST_CASE_FIXTURE(Fixture, "stringifying_table_type_correctly_use_matching_table_state_braces")
{
    TableType ttv{TableState::Sealed, TypeLevel{}};
    for (char c : std::string("abcdefghij"))
        ttv.props[std::string(1, c)] = {builtinTypes->numberType};

    Type tv{ttv};

    ToStringOptions o;
    o.maxTableLength = 40;
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ(toString(&tv, o), "{ a: number, b: number, c: number, d: number, e: number, ... 5 more ... }");
    else
        CHECK_EQ(toString(&tv, o), "{| a: number, b: number, c: number, d: number, e: number, ... 5 more ... |}");
}

TEST_CASE_FIXTURE(Fixture, "stringifying_cyclic_union_type_bails_early")
{
    Type tv{UnionType{{builtinTypes->stringType, builtinTypes->numberType}}};
    UnionType* utv = getMutable<UnionType>(&tv);
    utv->options.push_back(&tv);
    utv->options.push_back(&tv);

    CHECK_EQ("t1 where t1 = number | string", toString(&tv));
}

TEST_CASE_FIXTURE(Fixture, "stringifying_cyclic_intersection_type_bails_early")
{
    Type tv{IntersectionType{}};
    IntersectionType* itv = getMutable<IntersectionType>(&tv);
    itv->parts.push_back(&tv);
    itv->parts.push_back(&tv);

    CHECK_EQ("t1 where t1 = t1 & t1", toString(&tv));
}

TEST_CASE_FIXTURE(Fixture, "stringifying_array_uses_array_syntax")
{
    TableType ttv{TableState::Sealed, TypeLevel{}};
    ttv.indexer = TableIndexer{builtinTypes->numberType, builtinTypes->stringType};

    CHECK_EQ("{string}", toString(Type{ttv}));

    ttv.props["A"] = {builtinTypes->numberType};
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("{ [number]: string, A: number }", toString(Type{ttv}));
    else
        CHECK_EQ("{| [number]: string, A: number |}", toString(Type{ttv}));

    ttv.props.clear();
    ttv.state = TableState::Unsealed;
    CHECK_EQ("{string}", toString(Type{ttv}));
}


TEST_CASE_FIXTURE(Fixture, "generic_packs_are_stringified_differently_from_generic_types")
{
    TypePackVar tpv{GenericTypePack{"a"}};
    CHECK_EQ(toString(&tpv), "a...");

    Type tv{GenericType{"a"}};
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

    ToStringOptions opts;

    TypeId id3Type = requireType("id3");
    ToStringResult nameData = toStringDetailed(id3Type, opts);

    REQUIRE(3 == opts.nameMap.types.size());

    REQUIRE_EQ("<a, b, c>(a, b, c) -> (a, b, c)", nameData.name);

    const FunctionType* ftv = get<FunctionType>(follow(id3Type));
    REQUIRE(ftv != nullptr);

    auto params = flatten(ftv->argTypes).first;
    REQUIRE(3 == params.size());

    CHECK("a" == toString(params[0], opts));
    CHECK("b" == toString(params[1], opts));
    CHECK("c" == toString(params[2], opts));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "toStringDetailed2")
{
    ScopedFastFlag sff[] = {
        {FFlag::DebugLuauSharedSelf, true},
    };

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

    ToStringOptions opts;

    TypeId tType = requireType("inst");
    ToStringResult r = toStringDetailed(tType, opts);
    CHECK_EQ("{ @metatable { __index: { @metatable {| __index: base |}, child } }, inst }", r.name);
    CHECK(0 == opts.nameMap.types.size());

    const MetatableType* tMeta = get<MetatableType>(follow(tType));
    REQUIRE(tMeta);

    TableType* tMeta2 = getMutable<TableType>(follow(tMeta->metatable));
    REQUIRE(tMeta2);
    REQUIRE(tMeta2->props.count("__index"));

    const MetatableType* tMeta3 = get<MetatableType>(follow(tMeta2->props["__index"].type()));
    REQUIRE(tMeta3);

    TableType* tMeta4 = getMutable<TableType>(follow(tMeta3->metatable));
    REQUIRE(tMeta4);
    REQUIRE(tMeta4->props.count("__index"));

    TableType* tMeta5 = getMutable<TableType>(follow(tMeta4->props["__index"].type()));
    REQUIRE(tMeta5);
    REQUIRE(tMeta5->props.count("one") > 0);

    TableType* tMeta6 = getMutable<TableType>(follow(tMeta3->table));
    REQUIRE(tMeta6);
    REQUIRE(tMeta6->props.count("two") > 0);

    ToStringResult oneResult = toStringDetailed(tMeta5->props["one"].type(), opts);

    std::string twoResult = toString(tMeta6->props["two"].type(), opts);

    CHECK_EQ("<a>(a) -> number", oneResult.name);
    CHECK_EQ("<b>(b) -> number", twoResult);
}

TEST_CASE_FIXTURE(Fixture, "toStringErrorPack")
{
    CheckResult result = check(R"(
local function target(callback: nil) return callback(4, "hello") end
    )");

    LUAU_REQUIRE_ERRORS(result);
    CHECK_EQ("(nil) -> (*error-type*)", toString(requireType("target")));
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
    Type tv1{TableType{}};
    TableType* ttv = getMutable<TableType>(&tv1);
    ttv->state = TableState::Sealed;
    ttv->props["hello"] = {builtinTypes->numberType};
    ttv->props["world"] = {builtinTypes->numberType};

    TypePackVar tpv1{TypePack{{&tv1}}};

    Type tv2{TableType{}};
    TableType* bttv = getMutable<TableType>(&tv2);
    bttv->state = TableState::Free;
    bttv->props["hello"] = {builtinTypes->numberType};
    bttv->boundTo = &tv1;

    TypePackVar tpv2{TypePack{{&tv2}}};


    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        CHECK_EQ("{ hello: number, world: number }", toString(&tpv1));
        CHECK_EQ("{ hello: number, world: number }", toString(&tpv2));
    }
    else
    {
        CHECK_EQ("{| hello: number, world: number |}", toString(&tpv1));
        CHECK_EQ("{| hello: number, world: number |}", toString(&tpv2));
    }
}

TEST_CASE_FIXTURE(Fixture, "no_parentheses_around_return_type_if_pack_has_an_empty_head_link")
{
    TypeArena arena;
    TypePackId realTail = arena.addTypePack({builtinTypes->stringType});
    TypePackId emptyTail = arena.addTypePack({}, realTail);

    TypePackId argList = arena.addTypePack({builtinTypes->stringType});

    TypeId functionType = arena.addType(FunctionType{argList, emptyTail});

    CHECK("(string) -> string" == toString(functionType));
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
    Type tableTy{TableType{}};
    TableType* ttv = getMutable<TableType>(&tableTy);
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
    const FunctionType* ftv = get<FunctionType>(follow(ty));

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
    const FunctionType* ftv = get<FunctionType>(follow(ty));

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
    const FunctionType* ftv = get<FunctionType>(follow(ty));

    CHECK_EQ("test<T..., U...>(...: T...): U...", toStringNamedFunction("test", *ftv));
}

TEST_CASE("toStringNamedFunction_unit_f")
{
    TypePackVar empty{TypePack{}};
    FunctionType ftv{&empty, &empty, {}, false};
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
    auto ftv = get<FunctionType>(follow(ty));

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
    auto ftv = get<FunctionType>(follow(ty));

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
    auto ftv = get<FunctionType>(follow(ty));

    CHECK_EQ("f(): (string, ...number)", toStringNamedFunction("f", *ftv));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_type_annotation_has_partial_argnames")
{
    CheckResult result = check(R"(
        local f: (number, y: number) -> number
    )");

    TypeId ty = requireType("f");
    auto ftv = get<FunctionType>(follow(ty));

    CHECK_EQ("f(_: number, y: number): number", toStringNamedFunction("f", *ftv));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_hide_type_params")
{
    CheckResult result = check(R"(
        local function f<T>(x: T, g: <U>(T) -> U)): ()
        end
    )");

    TypeId ty = requireType("f");
    auto ftv = get<FunctionType>(follow(ty));

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
    const FunctionType* ftv = get<FunctionType>(follow(ty));

    ToStringOptions opts;
    opts.namedFunctionOverrideArgNames = {"first", "second", "third"};
    CHECK_EQ("test<a>(first: a, second: string, ...: number): a", toStringNamedFunction("test", *ftv, opts));
}

TEST_CASE_FIXTURE(Fixture, "pick_distinct_names_for_mixed_explicit_and_implicit_generics")
{
    CheckResult result = check(R"(
        function foo<a>(x: a, y) end
    )");

    CHECK("<a, b>(a, b) -> ()" == toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_include_self_param")
{
    ScopedFastFlag sff[]{
        {FFlag::DebugLuauSharedSelf, true},
    };

    CheckResult result = check(R"(
        local foo = {}
        function foo:method(arg: string): ()
        end
    )");

    TypeId parentTy = requireType("foo");
    auto ttv = get<TableType>(follow(parentTy));
    auto ftv = get<FunctionType>(follow(ttv->props.at("method").type()));

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("foo:method(self: unknown, arg: string): ()", toStringNamedFunction("foo:method", *ftv));
    else
        CHECK_EQ("foo:method<a>(self: a, arg: string): ()", toStringNamedFunction("foo:method", *ftv));
}

TEST_CASE_FIXTURE(Fixture, "toStringNamedFunction_hide_self_param")
{
    ScopedFastFlag sff[]{
        {FFlag::DebugLuauSharedSelf, true},
    };

    CheckResult result = check(R"(
        local foo = {}
        function foo:method(arg: string): ()
        end
    )");

    ToStringOptions opts;
    opts.hideFunctionSelfArgument = true;

    TypeId parentTy = requireType("foo");
    auto ttv = get<TableType>(follow(parentTy));
    REQUIRE_MESSAGE(ttv, "Expected a table but got " << toString(parentTy, opts));
    TypeId methodTy = follow(ttv->props.at("method").type());
    auto ftv = get<FunctionType>(methodTy);
    REQUIRE_MESSAGE(ftv, "Expected a function but got " << toString(methodTy, opts));

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("foo:method(arg: string): ()", toStringNamedFunction("foo:method", *ftv, opts));
    else
        CHECK_EQ("foo:method<a>(arg: string): ()", toStringNamedFunction("foo:method", *ftv, opts));
}

TEST_CASE_FIXTURE(Fixture, "tostring_unsee_ttv_if_array")
{
    CheckResult result = check(R"(
        local x: {string}
        -- This code is constructed very specifically to use the same (by pointer
        -- identity) type in the function twice.
        local y: (typeof(x), typeof(x)) -> ()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("y")) == "({string}, {string}) -> ()");
}

TEST_CASE_FIXTURE(Fixture, "tostring_error_mismatch")
{
    CheckResult result = check(R"(
--!strict
   function f1() : {a : number, b : string, c : { d : number}}
     return { a = 1, b = "a", c = {d = "a"}}
   end

)");
    //clang-format off
    std::string expected =
        (FFlag::DebugLuauDeferredConstraintResolution)
            ? R"(Type pack '{| a: number, b: string, c: {| d: string |} |}' could not be converted into '{ a: number, b: string, c: { d: number } }'; at [0]["c"]["d"], string is not exactly number)"
            :
            R"(Type
    '{ a: number, b: string, c: { d: string } }'
could not be converted into
    '{| a: number, b: string, c: {| d: number |} |}'
caused by:
  Property 'c' is not compatible.
Type
    '{ d: string }'
could not be converted into
    '{| d: number |}'
caused by:
  Property 'd' is not compatible.
Type 'string' could not be converted into 'number' in an invariant context)";
    //clang-format on
    //
    std::string actual = toString(result.errors[0]);

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK(expected == actual);
}

TEST_CASE_FIXTURE(Fixture, "checked_fn_toString")
{
    ScopedFastFlag flags[] = {
        {FFlag::LuauCheckedFunctionSyntax, true},
        {FFlag::DebugLuauDeferredConstraintResolution, true},
    };

    auto _result = loadDefinition(R"(
declare function @checked abs(n: number) : number
)");

    auto result = check(Mode::Nonstrict, R"(
local f = abs
)");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId fn = requireType("f");
    CHECK("@checked (number) -> number" == toString(fn));
}
TEST_SUITE_END();
