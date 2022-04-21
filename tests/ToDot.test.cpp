// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Scope.h"
#include "Luau/ToDot.h"

#include "Fixture.h"

#include "doctest.h"

LUAU_FASTFLAG(LuauLowerBoundsCalculation)

using namespace Luau;

struct ToDotClassFixture : Fixture
{
    ToDotClassFixture()
    {
        TypeArena& arena = typeChecker.globalTypes;

        unfreeze(arena);

        TypeId baseClassMetaType = arena.addType(TableTypeVar{});

        TypeId baseClassInstanceType = arena.addType(ClassTypeVar{"BaseClass", {}, std::nullopt, baseClassMetaType, {}, {}, "Test"});
        getMutable<ClassTypeVar>(baseClassInstanceType)->props = {
            {"BaseField", {typeChecker.numberType}},
        };
        typeChecker.globalScope->exportedTypeBindings["BaseClass"] = TypeFun{{}, baseClassInstanceType};

        TypeId childClassInstanceType = arena.addType(ClassTypeVar{"ChildClass", {}, baseClassInstanceType, std::nullopt, {}, {}, "Test"});
        getMutable<ClassTypeVar>(childClassInstanceType)->props = {
            {"ChildField", {typeChecker.stringType}},
        };
        typeChecker.globalScope->exportedTypeBindings["ChildClass"] = TypeFun{{}, childClassInstanceType};

        freeze(arena);
    }
};

TEST_SUITE_BEGIN("ToDot");

TEST_CASE_FIXTURE(Fixture, "primitive")
{
    CheckResult result = check(R"(
local a: nil
local b: number
local c: any
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_NE("nil", toDot(requireType("a")));

    CHECK_EQ(R"(digraph graphname {
n1 [label="number"];
})",
        toDot(requireType("b")));

    CHECK_EQ(R"(digraph graphname {
n1 [label="any"];
})",
        toDot(requireType("c")));

    ToDotOptions opts;
    opts.showPointers = false;
    opts.duplicatePrimitives = false;

    CHECK_EQ(R"(digraph graphname {
n1 [label="PrimitiveTypeVar number"];
})",
        toDot(requireType("b"), opts));

    CHECK_EQ(R"(digraph graphname {
n1 [label="AnyTypeVar 1"];
})",
        toDot(requireType("c"), opts));
}

TEST_CASE_FIXTURE(Fixture, "bound")
{
    CheckResult result = check(R"(
local a = 444
local b = a
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> ty = getType("b");
    REQUIRE(bool(ty));

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="BoundTypeVar 1"];
n1 -> n2;
n2 [label="number"];
})",
        toDot(*ty, opts));
}

TEST_CASE_FIXTURE(Fixture, "function")
{
    CheckResult result = check(R"(
local function f(a, ...: string) return a end
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("<a>(a, ...string) -> a", toString(requireType("f")));

    ToDotOptions opts;
    opts.showPointers = false;

    if (FFlag::LuauLowerBoundsCalculation)
    {
        CHECK_EQ(R"(digraph graphname {
n1 [label="FunctionTypeVar 1"];
n1 -> n2 [label="arg"];
n2 [label="TypePack 2"];
n2 -> n3;
n3 [label="GenericTypeVar 3"];
n2 -> n4 [label="tail"];
n4 [label="VariadicTypePack 4"];
n4 -> n5;
n5 [label="string"];
n1 -> n6 [label="ret"];
n6 [label="TypePack 6"];
n6 -> n7;
n7 [label="BoundTypeVar 7"];
n7 -> n3;
})",
            toDot(requireType("f"), opts));
    }
    else
    {
        CHECK_EQ(R"(digraph graphname {
n1 [label="FunctionTypeVar 1"];
n1 -> n2 [label="arg"];
n2 [label="TypePack 2"];
n2 -> n3;
n3 [label="GenericTypeVar 3"];
n2 -> n4 [label="tail"];
n4 [label="VariadicTypePack 4"];
n4 -> n5;
n5 [label="string"];
n1 -> n6 [label="ret"];
n6 [label="BoundTypePack 6"];
n6 -> n7;
n7 [label="TypePack 7"];
n7 -> n3;
})",
            toDot(requireType("f"), opts));
    }
}

TEST_CASE_FIXTURE(Fixture, "union")
{
    CheckResult result = check(R"(
local a: string | number
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="UnionTypeVar 1"];
n1 -> n2;
n2 [label="string"];
n1 -> n3;
n3 [label="number"];
})",
        toDot(requireType("a"), opts));
}

TEST_CASE_FIXTURE(Fixture, "intersection")
{
    CheckResult result = check(R"(
local a: string & number -- uninhabited
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="IntersectionTypeVar 1"];
n1 -> n2;
n2 [label="string"];
n1 -> n3;
n3 [label="number"];
})",
        toDot(requireType("a"), opts));
}

TEST_CASE_FIXTURE(Fixture, "table")
{
    CheckResult result = check(R"(
type A<T, U...> = { x: T, y: (U...) -> (), [string]: any }
local a: A<number, ...string>
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="TableTypeVar A"];
n1 -> n2 [label="x"];
n2 [label="number"];
n1 -> n3 [label="y"];
n3 [label="FunctionTypeVar 3"];
n3 -> n4 [label="arg"];
n4 [label="VariadicTypePack 4"];
n4 -> n5;
n5 [label="string"];
n3 -> n6 [label="ret"];
n6 [label="TypePack 6"];
n1 -> n7 [label="[index]"];
n7 [label="string"];
n1 -> n8 [label="[value]"];
n8 [label="any"];
n1 -> n9 [label="typeParam"];
n9 [label="number"];
n1 -> n4 [label="typePackParam"];
})",
        toDot(requireType("a"), opts));

    // Extra coverage with pointers (unstable values)
    (void)toDot(requireType("a"));
}

TEST_CASE_FIXTURE(Fixture, "metatable")
{
    CheckResult result = check(R"(
local a: typeof(setmetatable({}, {}))
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="MetatableTypeVar 1"];
n1 -> n2 [label="table"];
n2 [label="TableTypeVar 2"];
n1 -> n3 [label="metatable"];
n3 [label="TableTypeVar 3"];
})",
        toDot(requireType("a"), opts));
}

TEST_CASE_FIXTURE(Fixture, "free")
{
    TypeVar type{TypeVariant{FreeTypeVar{TypeLevel{0, 0}}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="FreeTypeVar 1"];
})",
        toDot(&type, opts));
}

TEST_CASE_FIXTURE(Fixture, "error")
{
    TypeVar type{TypeVariant{ErrorTypeVar{}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="ErrorTypeVar 1"];
})",
        toDot(&type, opts));
}

TEST_CASE_FIXTURE(Fixture, "generic")
{
    TypeVar type{TypeVariant{GenericTypeVar{"T"}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="GenericTypeVar T"];
})",
        toDot(&type, opts));
}

TEST_CASE_FIXTURE(ToDotClassFixture, "class")
{
    CheckResult result = check(R"(
local a: ChildClass
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="ClassTypeVar ChildClass"];
n1 -> n2 [label="ChildField"];
n2 [label="string"];
n1 -> n3 [label="[parent]"];
n3 [label="ClassTypeVar BaseClass"];
n3 -> n4 [label="BaseField"];
n4 [label="number"];
n3 -> n5 [label="[metatable]"];
n5 [label="TableTypeVar 5"];
})",
        toDot(requireType("a"), opts));
}

TEST_CASE_FIXTURE(Fixture, "free_pack")
{
    TypePackVar pack{TypePackVariant{FreeTypePack{TypeLevel{0, 0}}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="FreeTypePack 1"];
})",
        toDot(&pack, opts));
}

TEST_CASE_FIXTURE(Fixture, "error_pack")
{
    TypePackVar pack{TypePackVariant{Unifiable::Error{}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="ErrorTypePack 1"];
})",
        toDot(&pack, opts));

    // Extra coverage with pointers (unstable values)
    (void)toDot(&pack);
}

TEST_CASE_FIXTURE(Fixture, "generic_pack")
{
    TypePackVar pack1{TypePackVariant{GenericTypePack{}}};
    TypePackVar pack2{TypePackVariant{GenericTypePack{"T"}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="GenericTypePack 1"];
})",
        toDot(&pack1, opts));

    CHECK_EQ(R"(digraph graphname {
n1 [label="GenericTypePack T"];
})",
        toDot(&pack2, opts));
}

TEST_CASE_FIXTURE(Fixture, "bound_pack")
{
    TypePackVar pack{TypePackVariant{TypePack{{typeChecker.numberType}, {}}}};
    TypePackVar bound{TypePackVariant{BoundTypePack{&pack}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="BoundTypePack 1"];
n1 -> n2;
n2 [label="TypePack 2"];
n2 -> n3;
n3 [label="number"];
})",
        toDot(&bound, opts));
}

TEST_CASE_FIXTURE(Fixture, "bound_table")
{
    CheckResult result = check(R"(
local a = {x=2}
local b
b.x = 2
b = a
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> ty = getType("b");
    REQUIRE(bool(ty));

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(R"(digraph graphname {
n1 [label="TableTypeVar 1"];
n1 -> n2 [label="boundTo"];
n2 [label="TableTypeVar a"];
n2 -> n3 [label="x"];
n3 [label="number"];
})",
        toDot(*ty, opts));
}

TEST_CASE_FIXTURE(Fixture, "constrained")
{
    // ConstrainedTypeVars never appear in the final type graph, so we have to create one directly
    // to dotify it.
    TypeVar t{ConstrainedTypeVar{TypeLevel{}, {typeChecker.numberType, typeChecker.stringType, typeChecker.nilType}}};

    ToDotOptions opts;
    opts.showPointers = false;

    CHECK_EQ(R"(digraph graphname {
n1 [label="ConstrainedTypeVar 1"];
n1 -> n2;
n2 [label="number"];
n1 -> n3;
n3 [label="string"];
n1 -> n4;
n4 [label="nil"];
})",
        toDot(&t, opts));
}

TEST_CASE_FIXTURE(Fixture, "singletontypes")
{
    CheckResult result = check(R"(
        local x: "hi" | "\"hello\"" | true | false
    )");

    ToDotOptions opts;
    opts.showPointers = false;

    CHECK_EQ(R"(digraph graphname {
n1 [label="UnionTypeVar 1"];
n1 -> n2;
n2 [label="SingletonTypeVar string: hi"];
n1 -> n3;
)"
"n3 [label=\"SingletonTypeVar string: \\\"hello\\\"\"];"
R"(
n1 -> n4;
n4 [label="SingletonTypeVar boolean: true"];
n1 -> n5;
n5 [label="SingletonTypeVar boolean: false"];
})", toDot(requireType("x"), opts));
}

TEST_SUITE_END();
