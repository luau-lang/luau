// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypePath.h"

#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypePack.h"

#include "ClassFixture.h"
#include "doctest.h"
#include "Fixture.h"
#include "ScopedFlags.h"

#include <optional>

using namespace Luau;
using namespace Luau::TypePath;

LUAU_FASTFLAG(LuauSolverV2);
LUAU_DYNAMIC_FASTINT(LuauTypePathMaximumTraverseSteps);

struct TypePathFixture : Fixture
{
    ScopedFastFlag sff1{FFlag::LuauSolverV2, true};
};

struct TypePathBuiltinsFixture : BuiltinsFixture
{
    ScopedFastFlag sff1{FFlag::LuauSolverV2, true};
};

TEST_SUITE_BEGIN("TypePathManipulation");

TEST_CASE("append")
{
    SUBCASE("empty_paths")
    {
        Path p;
        CHECK(p.append(Path{}).empty());
    }

    SUBCASE("empty_path_with_path")
    {
        Path p1;
        Path p2(TypeField::Metatable);

        Path result = p1.append(p2);
        CHECK(result == Path(TypeField::Metatable));
    }

    SUBCASE("two_paths")
    {
        Path p1(TypeField::IndexLookup);
        Path p2(TypeField::Metatable);

        Path result = p1.append(p2);
        CHECK(result == Path({TypeField::IndexLookup, TypeField::Metatable}));
    }

    SUBCASE("all_components")
    {
        Path p1({TypeField::IndexLookup, TypeField::Metatable});
        Path p2({TypeField::Metatable, PackField::Arguments});

        Path result = p1.append(p2);
        CHECK(result == Path({TypeField::IndexLookup, TypeField::Metatable, TypeField::Metatable, PackField::Arguments}));
    }

    SUBCASE("does_not_mutate")
    {
        Path p1(TypeField::IndexLookup);
        Path p2(TypeField::Metatable);

        p1.append(p2);
        CHECK(p1 == Path(TypeField::IndexLookup));
        CHECK(p2 == Path(TypeField::Metatable));
    }
}

TEST_CASE("push")
{
    Path p;
    Path result = p.push(TypeField::Metatable);

    CHECK(p.empty());
    CHECK(result == Path(TypeField::Metatable));
}

TEST_CASE("pop")
{
    SUBCASE("empty_path")
    {
        Path p;
        CHECK(p.empty());
        CHECK(p.pop().empty());
    }
}

TEST_SUITE_END(); // TypePathManipulation

TEST_SUITE_BEGIN("TypePathTraversal");

#define TYPESOLVE_CODE(code) \
    do \
    { \
        CheckResult result = check(code); \
        LUAU_REQUIRE_NO_ERRORS(result); \
    } while (false);

TEST_CASE_FIXTURE(TypePathFixture, "empty_traversal")
{
    CHECK(traverseForType(builtinTypes->numberType, kEmpty, builtinTypes) == builtinTypes->numberType);
}

TEST_CASE_FIXTURE(TypePathFixture, "table_property")
{
    TYPESOLVE_CODE(R"(
        local x = { y = 123 }
    )");

    CHECK(traverseForType(requireType("x"), Path(TypePath::Property{"y", true}), builtinTypes) == builtinTypes->numberType);
}

TEST_CASE_FIXTURE(ClassFixture, "class_property")
{
    CHECK(traverseForType(vector2InstanceType, Path(TypePath::Property{"X", true}), builtinTypes) == builtinTypes->numberType);
}

TEST_CASE_FIXTURE(TypePathBuiltinsFixture, "metatable_property")
{
    SUBCASE("meta_does_not_contribute")
    {
        TYPESOLVE_CODE(R"(
            local x = setmetatable({ x = 123 }, {})
        )");
    }

    SUBCASE("meta_and_table_supply_property")
    {
        // since the table takes priority, the __index property won't matter
        TYPESOLVE_CODE(R"(
            local x = setmetatable({ x = 123 }, { __index = { x = 'foo' } })
        )");
    }

    SUBCASE("only_meta_supplies_property")
    {
        TYPESOLVE_CODE(R"(
            local x = setmetatable({}, { __index = { x = 123 } })
        )");
    }

    CHECK(traverseForType(requireType("x"), Path(TypePath::Property::read("x")), builtinTypes) == builtinTypes->numberType);
}

TEST_CASE_FIXTURE(TypePathFixture, "index")
{
    SUBCASE("unions")
    {
        TYPESOLVE_CODE(R"(
            type T = number | string | boolean
        )");

        SUBCASE("in_bounds")
        {
            CHECK(traverseForType(requireTypeAlias("T"), Path(TypePath::Index{1}), builtinTypes) == builtinTypes->stringType);
        }

        SUBCASE("out_of_bounds")
        {
            CHECK(traverseForType(requireTypeAlias("T"), Path(TypePath::Index{97}), builtinTypes) == std::nullopt);
        }
    }

    SUBCASE("intersections")
    {
        // use functions to avoid the intersection being normalized away
        TYPESOLVE_CODE(R"(
            type T = (() -> ()) & ((true) -> false) & ((false) -> true)
        )");

        SUBCASE("in_bounds")
        {
            auto result = traverseForType(requireTypeAlias("T"), Path(TypePath::Index{1}), builtinTypes);
            CHECK(result);

            if (result)
                CHECK(toString(*result) == "(true) -> false");
        }

        SUBCASE("out_of_bounds")
        {
            CHECK(traverseForType(requireTypeAlias("T"), Path(TypePath::Index{97}), builtinTypes) == std::nullopt);
        }
    }

    SUBCASE("type_packs")
    {
        // use functions to avoid the intersection being normalized away
        TYPESOLVE_CODE(R"(
            type T = (number, string, true, false) -> ()
        )");

        SUBCASE("in_bounds")
        {
            Path path = Path({TypePath::PackField::Arguments, TypePath::Index{1}});
            auto result = traverseForType(requireTypeAlias("T"), path, builtinTypes);
            CHECK(result == builtinTypes->stringType);
        }

        SUBCASE("out_of_bounds")
        {
            Path path = Path({TypePath::PackField::Arguments, TypePath::Index{72}});
            auto result = traverseForType(requireTypeAlias("T"), path, builtinTypes);
            CHECK(result == std::nullopt);
        }
    }
}

TEST_CASE_FIXTURE(ClassFixture, "metatables")
{
    SUBCASE("string")
    {
        auto result = traverseForType(builtinTypes->stringType, Path(TypeField::Metatable), builtinTypes);
        CHECK(result == getMetatable(builtinTypes->stringType, builtinTypes));
    }

    SUBCASE("string_singleton")
    {
        TYPESOLVE_CODE(R"(
            type T = "foo"
        )");

        auto result = traverseForType(requireTypeAlias("T"), Path(TypeField::Metatable), builtinTypes);
        CHECK(result == getMetatable(builtinTypes->stringType, builtinTypes));
    }

    SUBCASE("table")
    {
        TYPESOLVE_CODE(R"(
            type Table = { foo: number }
            type Metatable = { bar: number }
            local tbl: Table = { foo = 123 }
            local mt: Metatable = { bar = 456 }
            local res = setmetatable(tbl, mt)
        )");

        // Tricky test setup because 'setmetatable' mutates the argument 'tbl' type
        auto result = traverseForType(requireType("res"), Path(TypeField::Table), builtinTypes);
        auto expected = lookupType("Table");
        REQUIRE(expected);
        CHECK(result == follow(*expected));
    }

    SUBCASE("metatable")
    {
        TYPESOLVE_CODE(R"(
            local mt = { foo = 123 }
            local tbl = setmetatable({}, mt)
        )");

        auto result = traverseForType(requireType("tbl"), Path(TypeField::Metatable), builtinTypes);
        CHECK(result == requireType("mt"));
    }

    SUBCASE("class")
    {
        auto result = traverseForType(vector2InstanceType, Path(TypeField::Metatable), builtinTypes);
        // ClassFixture's Vector2 metatable is just an empty table, but it's there.
        CHECK(result);
    }
}

TEST_CASE_FIXTURE(TypePathFixture, "bounds")
{
    SUBCASE("free_type")
    {
        TypeArena& arena = frontend.globals.globalTypes;
        unfreeze(arena);

        TypeId ty = arena.freshType(frontend.globals.globalScope.get());
        FreeType* ft = getMutable<FreeType>(ty);

        SUBCASE("upper")
        {
            ft->upperBound = builtinTypes->numberType;
            auto result = traverseForType(ty, Path(TypeField::UpperBound), builtinTypes);
            CHECK(result == builtinTypes->numberType);
        }

        SUBCASE("lower")
        {
            ft->lowerBound = builtinTypes->booleanType;
            auto result = traverseForType(ty, Path(TypeField::LowerBound), builtinTypes);
            CHECK(result == builtinTypes->booleanType);
        }
    }

    SUBCASE("unbounded_type")
    {
        CHECK(traverseForType(builtinTypes->numberType, Path(TypeField::UpperBound), builtinTypes) == std::nullopt);
        CHECK(traverseForType(builtinTypes->numberType, Path(TypeField::LowerBound), builtinTypes) == std::nullopt);
    }
}

TEST_CASE_FIXTURE(TypePathFixture, "indexers")
{
    SUBCASE("table")
    {
        SUBCASE("lookup_indexer")
        {
            TYPESOLVE_CODE(R"(
                type T = { [string]: boolean }
            )");

            auto lookupResult = traverseForType(requireTypeAlias("T"), Path(TypeField::IndexLookup), builtinTypes);
            auto resultResult = traverseForType(requireTypeAlias("T"), Path(TypeField::IndexResult), builtinTypes);

            CHECK(lookupResult == builtinTypes->stringType);
            CHECK(resultResult == builtinTypes->booleanType);
        }

        SUBCASE("no_indexer")
        {
            TYPESOLVE_CODE(R"(
                type T = { y: number }
            )");

            auto lookupResult = traverseForType(requireTypeAlias("T"), Path(TypeField::IndexLookup), builtinTypes);
            auto resultResult = traverseForType(requireTypeAlias("T"), Path(TypeField::IndexResult), builtinTypes);

            CHECK(lookupResult == std::nullopt);
            CHECK(resultResult == std::nullopt);
        }
    }

    // TODO: Class types
}

TEST_CASE_FIXTURE(TypePathFixture, "negated")
{
    SUBCASE("valid")
    {
        TypeArena& arena = frontend.globals.globalTypes;
        unfreeze(arena);

        TypeId ty = arena.addType(NegationType{builtinTypes->numberType});
        auto result = traverseForType(ty, Path(TypeField::Negated), builtinTypes);
        CHECK(result == builtinTypes->numberType);
    }

    SUBCASE("not_negation")
    {
        auto result = traverseForType(builtinTypes->numberType, Path(TypeField::Negated), builtinTypes);
        CHECK(result == std::nullopt);
    }
}

TEST_CASE_FIXTURE(TypePathFixture, "variadic")
{
    SUBCASE("valid")
    {
        TypeArena& arena = frontend.globals.globalTypes;
        unfreeze(arena);

        TypePackId tp = arena.addTypePack(VariadicTypePack{builtinTypes->numberType});
        auto result = traverseForType(tp, Path(TypeField::Variadic), builtinTypes);
        CHECK(result == builtinTypes->numberType);
    }

    SUBCASE("not_variadic")
    {
        auto result = traverseForType(builtinTypes->numberType, Path(TypeField::Variadic), builtinTypes);
        CHECK(result == std::nullopt);
    }
}

TEST_CASE_FIXTURE(TypePathFixture, "arguments")
{
    SUBCASE("function")
    {
        TYPESOLVE_CODE(R"(
            function f(x: number, y: string)
            end
        )");

        auto result = traverseForPack(requireType("f"), Path(PackField::Arguments), builtinTypes);
        CHECK(result);
        if (result)
            CHECK(toString(*result) == "number, string");
    }

    SUBCASE("not_function")
    {
        auto result = traverseForPack(builtinTypes->booleanType, Path(PackField::Arguments), builtinTypes);
        CHECK(result == std::nullopt);
    }
}

TEST_CASE_FIXTURE(TypePathFixture, "returns")
{
    SUBCASE("function")
    {
        TYPESOLVE_CODE(R"(
            function f(): (number, string)
                return 123, "foo"
            end
        )");

        auto result = traverseForPack(requireType("f"), Path(PackField::Returns), builtinTypes);
        CHECK(result);
        if (result)
            CHECK(toString(*result) == "number, string");
    }

    SUBCASE("not_function")
    {
        auto result = traverseForPack(builtinTypes->booleanType, Path(PackField::Returns), builtinTypes);
        CHECK(result == std::nullopt);
    }
}

TEST_CASE_FIXTURE(TypePathFixture, "tail")
{
    SUBCASE("has_tail")
    {
        TYPESOLVE_CODE(R"(
            type T = (number, string, ...boolean) -> ()
        )");

        auto result = traverseForPack(requireTypeAlias("T"), Path({PackField::Arguments, PackField::Tail}), builtinTypes);
        CHECK(result);
        if (result)
            CHECK(toString(*result) == "...boolean");
    }

    SUBCASE("finite_pack")
    {
        TYPESOLVE_CODE(R"(
            type T = (number, string) -> ()
        )");

        auto result = traverseForPack(requireTypeAlias("T"), Path({PackField::Arguments, PackField::Tail}), builtinTypes);
        CHECK(result == std::nullopt);
    }

    SUBCASE("type")
    {
        auto result = traverseForPack(builtinTypes->stringType, Path({PackField::Arguments, PackField::Tail}), builtinTypes);
        CHECK(result == std::nullopt);
    }
}

TEST_CASE_FIXTURE(TypePathFixture, "cycles" * doctest::timeout(0.5))
{
    // This will fail an occurs check, but it's a quick example of a cyclic type
    // where there _is_ no traversal.
    SUBCASE("bound_cycle")
    {
        TypeArena& arena = frontend.globals.globalTypes;
        unfreeze(arena);

        TypeId a = arena.addType(BlockedType{});
        TypeId b = arena.addType(BoundType{a});
        asMutable(a)->ty.emplace<BoundType>(b);

        CHECK_THROWS(traverseForType(a, Path(TypeField::IndexResult), builtinTypes));
    }

    SUBCASE("table_contains_itself")
    {
        TypeArena& arena = frontend.globals.globalTypes;
        unfreeze(arena);

        TypeId tbl = arena.addType(TableType{});
        getMutable<TableType>(tbl)->props["a"] = Luau::Property(tbl);

        auto result = traverseForType(tbl, Path(TypePath::Property{"a", true}), builtinTypes);
        CHECK(result == tbl);
    }
}

TEST_CASE_FIXTURE(TypePathFixture, "step_limit")
{
    ScopedFastInt sfi(DFInt::LuauTypePathMaximumTraverseSteps, 2);

    TYPESOLVE_CODE(R"(
        type T = {
            x: {
                y: {
                    z: number
                }
            }
        }
    )");

    TypeId root = requireTypeAlias("T");
    Path path = PathBuilder().readProp("x").readProp("y").readProp("z").build();
    auto result = traverseForType(root, path, builtinTypes);
    CHECK(!result);
}

TEST_CASE_FIXTURE(TypePathBuiltinsFixture, "complex_chains")
{
    SUBCASE("add_metamethod_return_type")
    {
        TYPESOLVE_CODE(R"(
            type Meta = {
                __add: (Tab, Tab) -> number,
            }
            
            type Tab = typeof(setmetatable({}, {} :: Meta))
        )");

        TypeId root = requireTypeAlias("Tab");
        Path path = PathBuilder().mt().readProp("__add").rets().index(0).build();
        auto result = traverseForType(root, path, builtinTypes);
        CHECK(result == builtinTypes->numberType);
    }

    SUBCASE("overloaded_fn_overload_one_argument_two")
    {
        TYPESOLVE_CODE(R"(
            type Obj = {
                method: ((true, false) -> string) & ((string) -> number)
            }
        )");

        TypeId root = requireTypeAlias("Obj");
        Path path = PathBuilder().readProp("method").index(0).args().index(1).build();
        auto result = traverseForType(root, path, builtinTypes);
        CHECK(*result == builtinTypes->falseType);
    }
}

TEST_SUITE_END(); // TypePathTraversal

TEST_SUITE_BEGIN("TypePathToString");

TEST_CASE("field")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CHECK(toString(PathBuilder().prop("foo").build()) == R"(["foo"])");
}

TEST_CASE("index")
{
    CHECK(toString(PathBuilder().index(0).build()) == "[0]");
}

TEST_CASE("chain")
{
    CHECK(toString(PathBuilder().index(0).mt().build()) == "[0].metatable()");
}

TEST_SUITE_END(); // TypePathToString

TEST_SUITE_BEGIN("TypePathBuilder");

TEST_CASE("empty_path")
{
    Path p = PathBuilder().build();
    CHECK(p.empty());
}

TEST_CASE("prop")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    Path p = PathBuilder().prop("foo").build();
    CHECK(p == Path(TypePath::Property{"foo"}));
}

TEST_CASE_FIXTURE(TypePathFixture, "readProp")
{
    Path p = PathBuilder().readProp("foo").build();
    CHECK(p == Path(TypePath::Property::read("foo")));
}

TEST_CASE_FIXTURE(TypePathFixture, "writeProp")
{
    Path p = PathBuilder().writeProp("foo").build();
    CHECK(p == Path(TypePath::Property::write("foo")));
}

TEST_CASE("index")
{
    Path p = PathBuilder().index(0).build();
    CHECK(p == Path(TypePath::Index{0}));
}

TEST_CASE("fields")
{
    CHECK(PathBuilder().mt().build() == Path(TypeField::Metatable));
    CHECK(PathBuilder().lb().build() == Path(TypeField::LowerBound));
    CHECK(PathBuilder().ub().build() == Path(TypeField::UpperBound));
    CHECK(PathBuilder().indexKey().build() == Path(TypeField::IndexLookup));
    CHECK(PathBuilder().indexValue().build() == Path(TypeField::IndexResult));
    CHECK(PathBuilder().negated().build() == Path(TypeField::Negated));
    CHECK(PathBuilder().variadic().build() == Path(TypeField::Variadic));
    CHECK(PathBuilder().args().build() == Path(PackField::Arguments));
    CHECK(PathBuilder().rets().build() == Path(PackField::Returns));
    CHECK(PathBuilder().tail().build() == Path(PackField::Tail));
}

TEST_CASE("chained")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    CHECK(
        PathBuilder().index(0).readProp("foo").mt().readProp("bar").args().index(1).build() ==
        Path({Index{0}, TypePath::Property::read("foo"), TypeField::Metatable, TypePath::Property::read("bar"), PackField::Arguments, Index{1}})
    );
}

TEST_SUITE_END(); // TypePathBuilder
