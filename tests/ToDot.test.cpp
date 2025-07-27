// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Scope.h"
#include "Luau/ToDot.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2);

struct ToDotClassFixture : Fixture
{
    ToDotClassFixture()
    {
        TypeArena& arena = getFrontend().globals.globalTypes;

        unfreeze(arena);

        TypeId baseClassMetaType = arena.addType(TableType{});

        TypeId baseClassInstanceType = arena.addType(ExternType{"BaseClass", {}, std::nullopt, baseClassMetaType, {}, {}, "Test", {}});
        getMutable<ExternType>(baseClassInstanceType)->props = {
            {"BaseField", {getBuiltins()->numberType}},
        };
        getFrontend().globals.globalScope->exportedTypeBindings["BaseClass"] = TypeFun{{}, baseClassInstanceType};

        TypeId childClassInstanceType = arena.addType(ExternType{"ChildClass", {}, baseClassInstanceType, std::nullopt, {}, {}, "Test", {}});
        getMutable<ExternType>(childClassInstanceType)->props = {
            {"ChildField", {getBuiltins()->stringType}},
        };
        getFrontend().globals.globalScope->exportedTypeBindings["ChildClass"] = TypeFun{{}, childClassInstanceType};

        for (const auto& [name, ty] : getFrontend().globals.globalScope->exportedTypeBindings)
            persist(ty.type);

        freeze(arena);
    }
};

TEST_SUITE_BEGIN("ToDot");

TEST_CASE_FIXTURE(Fixture, "primitive")
{
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="nil"];
})",
        toDot(getBuiltins()->nilType)
    );

    CHECK_EQ(
        R"(digraph graphname {
n1 [label="number"];
})",
        toDot(getBuiltins()->numberType)
    );

    CHECK_EQ(
        R"(digraph graphname {
n1 [label="any"];
})",
        toDot(getBuiltins()->anyType)
    );

    CHECK_EQ(
        R"(digraph graphname {
n1 [label="unknown"];
})",
        toDot(getBuiltins()->unknownType)
    );

    CHECK_EQ(
        R"(digraph graphname {
n1 [label="never"];
})",
        toDot(getBuiltins()->neverType)
    );
}

TEST_CASE_FIXTURE(Fixture, "no_duplicatePrimitives")
{
    ToDotOptions opts;
    opts.showPointers = false;
    opts.duplicatePrimitives = false;

    CHECK_EQ(
        R"(digraph graphname {
n1 [label="PrimitiveType number"];
})",
        toDot(getBuiltins()->numberType, opts)
    );

    CHECK_EQ(
        R"(digraph graphname {
n1 [label="AnyType 1"];
})",
        toDot(getBuiltins()->anyType, opts)
    );

    CHECK_EQ(
        R"(digraph graphname {
n1 [label="UnknownType 1"];
})",
        toDot(getBuiltins()->unknownType, opts)
    );

    CHECK_EQ(
        R"(digraph graphname {
n1 [label="NeverType 1"];
})",
        toDot(getBuiltins()->neverType, opts)
    );
}

TEST_CASE_FIXTURE(Fixture, "bound")
{
    TypeArena arena;

    TypeId ty = arena.addType(BoundType{getBuiltins()->numberType});

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="BoundType 1"];
n1 -> n2;
n2 [label="number"];
})",
        toDot(ty, opts)
    );
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

    if (FFlag::LuauSolverV2)
    {
        CHECK_EQ(
            R"(digraph graphname {
n1 [label="FunctionType 1"];
n1 -> n2 [label="arg"];
n2 [label="TypePack 2"];
n2 -> n3;
n3 [label="GenericType 3"];
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
            toDot(requireType("f"), opts)
        );
    }
    else
    {
        CHECK_EQ(
            R"(digraph graphname {
n1 [label="FunctionType 1"];
n1 -> n2 [label="arg"];
n2 [label="TypePack 2"];
n2 -> n3;
n3 [label="GenericType 3"];
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
            toDot(requireType("f"), opts)
        );
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
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="UnionType 1"];
n1 -> n2;
n2 [label="string"];
n1 -> n3;
n3 [label="number"];
})",
        toDot(requireType("a"), opts)
    );
}

TEST_CASE_FIXTURE(Fixture, "intersection")
{
    TypeArena arena;

    TypeId ty = arena.addType(IntersectionType{{getBuiltins()->stringType, getBuiltins()->numberType}});

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="IntersectionType 1"];
n1 -> n2;
n2 [label="string"];
n1 -> n3;
n3 [label="number"];
})",
        toDot(ty, opts)
    );
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
    if (FFlag::LuauSolverV2)
    {
        CHECK_EQ(
            R"(digraph graphname {
n1 [label="TableType A"];
n1 -> n2 [label="x"];
n2 [label="number"];
n1 -> n3 [label="y"];
n3 [label="FunctionType 3"];
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
            toDot(requireType("a"), opts)
        );
    }
    else
    {
        CHECK_EQ(
            R"(digraph graphname {
n1 [label="TableType A"];
n1 -> n2 [label="x"];
n2 [label="number"];
n1 -> n3 [label="y"];
n3 [label="FunctionType 3"];
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
            toDot(requireType("a"), opts)
        );
    }

    // Extra coverage with pointers (unstable values)
    (void)toDot(requireType("a"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable")
{
    CheckResult result = check(R"(
local a: typeof(setmetatable({}, {}))
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="MetatableType 1"];
n1 -> n2 [label="table"];
n2 [label="TableType 2"];
n1 -> n3 [label="metatable"];
n3 [label="TableType 3"];
})",
        toDot(requireType("a"), opts)
    );
}

TEST_CASE_FIXTURE(Fixture, "free")
{
    Type type{TypeVariant{FreeType{TypeLevel{0, 0}, getBuiltins()->neverType, getBuiltins()->unknownType}}};
    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="FreeType 1"];
})",
        toDot(&type, opts)
    );
}

TEST_CASE_FIXTURE(Fixture, "free_with_constraints")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
    };

    Type type{TypeVariant{FreeType{nullptr, getBuiltins()->numberType, getBuiltins()->optionalNumberType}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="FreeType 1"];
n1 -> n2 [label="[lowerBound]"];
n2 [label="number"];
n1 -> n3 [label="[upperBound]"];
n3 [label="UnionType 3"];
n3 -> n4;
n4 [label="number"];
n3 -> n5;
n5 [label="nil"];
})",
        toDot(&type, opts)
    );
}

TEST_CASE_FIXTURE(Fixture, "error")
{
    Type type{TypeVariant{ErrorType{}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="ErrorType 1"];
})",
        toDot(&type, opts)
    );
}

TEST_CASE_FIXTURE(Fixture, "generic")
{
    Type type{TypeVariant{GenericType{"T"}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="GenericType T"];
})",
        toDot(&type, opts)
    );
}

TEST_CASE_FIXTURE(ToDotClassFixture, "class")
{
    CheckResult result = check(R"(
local a: ChildClass
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="ExternType ChildClass"];
n1 -> n2 [label="ChildField"];
n2 [label="string"];
n1 -> n3 [label="[parent]"];
n3 [label="ExternType BaseClass"];
n3 -> n4 [label="BaseField"];
n4 [label="number"];
n3 -> n5 [label="[metatable]"];
n5 [label="TableType 5"];
})",
        toDot(requireType("a"), opts)
    );
}

TEST_CASE_FIXTURE(Fixture, "free_pack")
{
    TypePackVar pack{TypePackVariant{FreeTypePack{TypeLevel{0, 0}}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="FreeTypePack 1"];
})",
        toDot(&pack, opts)
    );
}

TEST_CASE_FIXTURE(Fixture, "error_pack")
{
    TypePackVar pack{TypePackVariant{ErrorTypePack{}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="ErrorTypePack 1"];
})",
        toDot(&pack, opts)
    );

    // Extra coverage with pointers (unstable values)
    (void)toDot(&pack);
}

TEST_CASE_FIXTURE(Fixture, "generic_pack")
{
    TypePackVar pack1{TypePackVariant{GenericTypePack{}}};
    TypePackVar pack2{TypePackVariant{GenericTypePack{"T"}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="GenericTypePack 1"];
})",
        toDot(&pack1, opts)
    );

    CHECK_EQ(
        R"(digraph graphname {
n1 [label="GenericTypePack T"];
})",
        toDot(&pack2, opts)
    );
}

TEST_CASE_FIXTURE(Fixture, "bound_pack")
{
    TypePackVar pack{TypePackVariant{TypePack{{getBuiltins()->numberType}, {}}}};
    TypePackVar bound{TypePackVariant{BoundTypePack{&pack}}};

    ToDotOptions opts;
    opts.showPointers = false;
    CHECK_EQ(
        R"(digraph graphname {
n1 [label="BoundTypePack 1"];
n1 -> n2;
n2 [label="TypePack 2"];
n2 -> n3;
n3 [label="number"];
})",
        toDot(&bound, opts)
    );
}

TEST_CASE_FIXTURE(Fixture, "bound_table")
{
    TypeArena arena;

    TypeId ty = arena.addType(TableType{});
    getMutable<TableType>(ty)->props["x"] = {getBuiltins()->numberType};

    TypeId boundTy = arena.addType(TableType{});
    getMutable<TableType>(boundTy)->boundTo = ty;

    ToDotOptions opts;
    opts.showPointers = false;

    CHECK_EQ(
        R"(digraph graphname {
n1 [label="TableType 1"];
n1 -> n2 [label="boundTo"];
n2 [label="TableType 2"];
n2 -> n3 [label="x"];
n3 [label="number"];
})",
        toDot(boundTy, opts)
    );
}

TEST_CASE_FIXTURE(Fixture, "builtintypes")
{
    CheckResult result = check(R"(
        local x: "hi" | "\"hello\"" | true | false
    )");

    ToDotOptions opts;
    opts.showPointers = false;

    CHECK_EQ(
        R"(digraph graphname {
n1 [label="UnionType 1"];
n1 -> n2;
n2 [label="SingletonType string: hi"];
n1 -> n3;
)"
        "n3 [label=\"SingletonType string: \\\"hello\\\"\"];"
        R"(
n1 -> n4;
n4 [label="SingletonType boolean: true"];
n1 -> n5;
n5 [label="SingletonType boolean: false"];
})",
        toDot(requireType("x"), opts)
    );
}

TEST_CASE_FIXTURE(Fixture, "negation")
{
    TypeArena arena;
    TypeId t = arena.addType(NegationType{getBuiltins()->stringType});

    ToDotOptions opts;
    opts.showPointers = false;

    CHECK(R"(digraph graphname {
n1 [label="NegationType 1"];
n1 -> n2 [label="[negated]"];
n2 [label="string"];
})" == toDot(t, opts));
}

TEST_SUITE_END();
