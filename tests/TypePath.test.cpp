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

LUAU_FASTFLAG(DebugLuauForceOldSolver);
LUAU_FASTFLAG(LuauNewTypePathErrorMessages);
LUAU_DYNAMIC_FASTINT(LuauTypePathMaximumTraverseSteps);

std::string renderTypePathPrefix(const Path& path)
{
    return renderTypePath(path).prefix;
}

struct TypePathFixture : Fixture
{
    ScopedFastFlag sff1{FFlag::DebugLuauForceOldSolver, false};
    TypeArena arena;
    const DenseHashMap<TypePackId, TypePackId> emptyMap_DEPRECATED;
};

struct TypePathBuiltinsFixture : BuiltinsFixture
{
    ScopedFastFlag sff1{FFlag::DebugLuauForceOldSolver, false};
    TypeArena arena;
    const DenseHashMap<TypePackId, TypePackId> emptyMap_DEPRECATED;
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
    CHECK(traverseForType(getBuiltins()->numberType, kEmpty, getBuiltins(), NotNull{&arena}) == getBuiltins()->numberType);
}

TEST_CASE_FIXTURE(TypePathFixture, "table_property")
{
    TYPESOLVE_CODE(R"(
        local x = { y = 123 }
    )");

    CHECK(traverseForType(requireType("x"), Path(TypePath::Property{"y", true}), getBuiltins(), NotNull{&arena}) == getBuiltins()->numberType);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "class_property")
{
    // Force this here because vector2InstanceType won't get initialized until the frontend has been forced
    getFrontend();
    TypeArena arena;
    CHECK(traverseForType(vector2InstanceType, Path(TypePath::Property{"X", true}), getBuiltins(), NotNull{&arena}) == getBuiltins()->numberType);
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

    CHECK(traverseForType(requireType("x"), Path(TypePath::Property::read("x")), getBuiltins(), NotNull{&arena}) == getBuiltins()->numberType);
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
            CHECK(traverseForType(requireTypeAlias("T"), Path(TypePath::Index{1}), getBuiltins(), NotNull{&arena}) == getBuiltins()->stringType);
        }

        SUBCASE("out_of_bounds")
        {
            CHECK(traverseForType(requireTypeAlias("T"), Path(TypePath::Index{97}), getBuiltins(), NotNull{&arena}) == std::nullopt);
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
            auto result = traverseForType(requireTypeAlias("T"), Path(TypePath::Index{1}), getBuiltins(), NotNull{&arena});
            CHECK(result);

            if (result)
                CHECK(toString(*result) == "(true) -> false");
        }

        SUBCASE("out_of_bounds")
        {
            CHECK(traverseForType(requireTypeAlias("T"), Path(TypePath::Index{97}), getBuiltins(), NotNull{&arena}) == std::nullopt);
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
            auto result = traverseForType(requireTypeAlias("T"), path, getBuiltins(), NotNull{&arena});
            CHECK(result == getBuiltins()->stringType);
        }

        SUBCASE("out_of_bounds")
        {
            Path path = Path({TypePath::PackField::Arguments, TypePath::Index{72}});
            auto result = traverseForType(requireTypeAlias("T"), path, getBuiltins(), NotNull{&arena});
            CHECK(result == std::nullopt);
        }
    }
}

TEST_CASE_FIXTURE(ExternTypeFixture, "metatables")
{
    getFrontend();
    TypeArena arena;

    SUBCASE("string")
    {
        auto result = traverseForType(getBuiltins()->stringType, Path(TypeField::Metatable), getBuiltins(), NotNull{&arena});
        CHECK(result == getMetatable(getBuiltins()->stringType, getBuiltins()));
    }

    SUBCASE("string_singleton")
    {
        TYPESOLVE_CODE(R"(
            type T = "foo"
        )");

        auto result = traverseForType(requireTypeAlias("T"), Path(TypeField::Metatable), getBuiltins(), NotNull{&arena});
        CHECK(result == getMetatable(getBuiltins()->stringType, getBuiltins()));
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
        auto result = traverseForType(requireType("res"), Path(TypeField::Table), getBuiltins(), NotNull{&arena});
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

        auto result = traverseForType(requireType("tbl"), Path(TypeField::Metatable), getBuiltins(), NotNull{&arena});
        CHECK(result == requireType("mt"));
    }

    SUBCASE("class")
    {
        auto result = traverseForType(vector2InstanceType, Path(TypeField::Metatable), getBuiltins(), NotNull{&arena});
        // ExternTypeFixture's Vector2 metatable is just an empty table, but it's there.
        CHECK(result);
    }
}

TEST_CASE_FIXTURE(TypePathFixture, "bounds")
{
    SUBCASE("free_type")
    {
        TypeArena& arena = getFrontend().globals.globalTypes;
        unfreeze(arena);

        TypeId ty = arena.freshType(getBuiltins(), getFrontend().globals.globalScope.get());
        FreeType* ft = getMutable<FreeType>(ty);

        SUBCASE("upper")
        {
            ft->upperBound = getBuiltins()->numberType;
            auto result = traverseForType(ty, Path(TypeField::UpperBound), getBuiltins(), NotNull{&arena});
            CHECK(result == getBuiltins()->numberType);
        }

        SUBCASE("lower")
        {
            ft->lowerBound = getBuiltins()->booleanType;
            auto result = traverseForType(ty, Path(TypeField::LowerBound), getBuiltins(), NotNull{&arena});
            CHECK(result == getBuiltins()->booleanType);
        }
    }

    SUBCASE("unbounded_type")
    {
        CHECK(traverseForType(getBuiltins()->numberType, Path(TypeField::UpperBound), getBuiltins(), NotNull{&arena}) == std::nullopt);
        CHECK(traverseForType(getBuiltins()->numberType, Path(TypeField::LowerBound), getBuiltins(), NotNull{&arena}) == std::nullopt);
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

            auto lookupResult = traverseForType(requireTypeAlias("T"), Path(TypeField::IndexLookup), getBuiltins(), NotNull{&arena});
            auto resultResult = traverseForType(requireTypeAlias("T"), Path(TypeField::IndexResult), getBuiltins(), NotNull{&arena});

            CHECK(lookupResult == getBuiltins()->stringType);
            CHECK(resultResult == getBuiltins()->booleanType);
        }

        SUBCASE("no_indexer")
        {
            TYPESOLVE_CODE(R"(
                type T = { y: number }
            )");

            auto lookupResult = traverseForType(requireTypeAlias("T"), Path(TypeField::IndexLookup), getBuiltins(), NotNull{&arena});
            auto resultResult = traverseForType(requireTypeAlias("T"), Path(TypeField::IndexResult), getBuiltins(), NotNull{&arena});

            CHECK(lookupResult == std::nullopt);
            CHECK(resultResult == std::nullopt);
        }
    }

    // TODO: Extern types
}

TEST_CASE_FIXTURE(TypePathFixture, "negated")
{
    SUBCASE("valid")
    {
        TypeArena& arena = getFrontend().globals.globalTypes;
        unfreeze(arena);

        TypeId ty = arena.addType(NegationType{getBuiltins()->numberType});
        auto result = traverseForType(ty, Path(TypeField::Negated), getBuiltins(), NotNull{&arena});
        CHECK(result == getBuiltins()->numberType);
    }

    SUBCASE("not_negation")
    {
        auto result = traverseForType(getBuiltins()->numberType, Path(TypeField::Negated), getBuiltins(), NotNull{&arena});
        CHECK(result == std::nullopt);
    }
}

TEST_CASE_FIXTURE(TypePathFixture, "variadic")
{
    SUBCASE("valid")
    {
        TypeArena& arena = getFrontend().globals.globalTypes;
        unfreeze(arena);

        TypePackId tp = arena.addTypePack(VariadicTypePack{getBuiltins()->numberType});
        auto result = traverseForType(tp, Path(TypeField::Variadic), getBuiltins(), NotNull{&arena});
        CHECK(result == getBuiltins()->numberType);
    }

    SUBCASE("not_variadic")
    {
        auto result = traverseForType(getBuiltins()->numberType, Path(TypeField::Variadic), getBuiltins(), NotNull{&arena});
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

        auto result = traverseForPack(requireType("f"), Path(PackField::Arguments), getBuiltins(), NotNull{&arena});
        CHECK(result);
        if (result)
            CHECK(toString(*result) == "number, string");
    }

    SUBCASE("not_function")
    {
        auto result = traverseForPack(getBuiltins()->booleanType, Path(PackField::Arguments), getBuiltins(), NotNull{&arena});
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

        auto result = traverseForPack(requireType("f"), Path(PackField::Returns), getBuiltins(), NotNull{&arena});
        CHECK(result);
        if (result)
            CHECK(toString(*result) == "number, string");
    }

    SUBCASE("not_function")
    {
        auto result = traverseForPack(getBuiltins()->booleanType, Path(PackField::Returns), getBuiltins(), NotNull{&arena});
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

        auto result = traverseForPack(requireTypeAlias("T"), Path({PackField::Arguments, PackField::Tail}), getBuiltins(), NotNull{&arena});
        CHECK(result);
        if (result)
            CHECK(toString(*result) == "...boolean");
    }

    SUBCASE("finite_pack")
    {
        TYPESOLVE_CODE(R"(
            type T = (number, string) -> ()
        )");

        auto result = traverseForPack(requireTypeAlias("T"), Path({PackField::Arguments, PackField::Tail}), getBuiltins(), NotNull{&arena});
        CHECK(result == std::nullopt);
    }

    SUBCASE("type")
    {
        auto result = traverseForPack(getBuiltins()->stringType, Path({PackField::Arguments, PackField::Tail}), getBuiltins(), NotNull{&arena});
        CHECK(result == std::nullopt);
    }
}

TEST_CASE_FIXTURE(TypePathFixture, "pack_slice_has_tail")
{
    TypeArena& arena = getFrontend().globals.globalTypes;
    unfreeze(arena);

    TYPESOLVE_CODE(R"(
        type T = (number, string, ...boolean) -> ()
    )");

    auto result = traverseForPack(requireTypeAlias("T"), Path({PackField::Arguments, PackSlice{1}}), getBuiltins(), NotNull{&arena});
    CHECK(result);
    if (result)
        CHECK(toString(*result) == "string, ...boolean");
}

TEST_CASE_FIXTURE(TypePathFixture, "pack_slice_finite_pack")
{
    TypeArena& arena = getFrontend().globals.globalTypes;
    unfreeze(arena);

    TYPESOLVE_CODE(R"(
        type T = (number, string) -> ()
    )");

    auto result = traverseForPack(requireTypeAlias("T"), Path({PackField::Arguments, PackSlice{1}}), getBuiltins(), NotNull{&arena});
    CHECK(result);
    if (result)
        CHECK(toString(*result) == "string");
}

TEST_CASE_FIXTURE(TypePathFixture, "pack_slice_type")
{
    TypeArena& arena = getFrontend().globals.globalTypes;
    unfreeze(arena);

    auto result = traverseForPack(builtinTypes->stringType, Path({PackField::Arguments, PackSlice{1}}), getBuiltins(), NotNull{&arena});
    CHECK(result == std::nullopt);
}

TEST_CASE_FIXTURE(TypePathFixture, "cycles" * doctest::timeout(LUAU_TIMEOUT))
{
    // This will fail an occurs check, but it's a quick example of a cyclic type
    // where there _is_ no traversal.
    SUBCASE("bound_cycle")
    {
        TypeArena& arena = getFrontend().globals.globalTypes;
        unfreeze(arena);

        TypeId a = arena.addType(BlockedType{});
        TypeId b = arena.addType(BoundType{a});
        asMutable(a)->ty.emplace<BoundType>(b);

        CHECK_THROWS(traverseForType(a, Path(TypeField::IndexResult), getBuiltins(), NotNull{&arena}));
    }

    SUBCASE("table_contains_itself")
    {
        TypeArena& arena = getFrontend().globals.globalTypes;
        unfreeze(arena);

        TypeId tbl = arena.addType(TableType{});
        getMutable<TableType>(tbl)->props["a"] = Luau::Property(tbl);

        auto result = traverseForType(tbl, Path(TypePath::Property{"a", true}), getBuiltins(), NotNull{&arena});
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
    auto result = traverseForType(root, path, getBuiltins(), NotNull{&arena});
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
        auto result = traverseForType(root, path, getBuiltins(), NotNull{&arena});
        CHECK(result == getBuiltins()->numberType);
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
        auto result = traverseForType(root, path, getBuiltins(), NotNull{&arena});
        CHECK(*result == getBuiltins()->falseType);
    }
}

TEST_SUITE_END(); // TypePathTraversal

TEST_SUITE_BEGIN("TypePathToString");

TEST_CASE("field")
{
    CHECK(toString(PathBuilder().prop("foo").build()) == R"([read "foo"])");
}

TEST_CASE("index")
{
    CHECK(toString(PathBuilder().index(0).build()) == "[0]");
}

TEST_CASE("chain")
{
    CHECK(toString(PathBuilder().index(0).mt().build()) == "[0].metatable()");
}

TEST_CASE("human_property_then_metatable_portion")
{
    ScopedFastFlag sff{FFlag::DebugLuauForceOldSolver, false};

    // https://github.com/luau-lang/luau/issues/2544
    if (FFlag::LuauNewTypePathErrorMessages)
    {
        CHECK(renderTypePathPrefix(PathBuilder().readProp("a").mt().build()) == "property `a` has metatable ");
        CHECK(renderTypePathPrefix(PathBuilder().writeProp("a").mt().build()) == "property `a` accepts values with metatable ");
    }
    else
    {
        CHECK(toStringHuman_DEPRECATED(PathBuilder().readProp("a").mt().build()) == "accessing `a` has the metatable portion as ");
        CHECK(toStringHuman_DEPRECATED(PathBuilder().writeProp("a").mt().build()) == "writing to `a` has the metatable portion as ");
    }
}

TEST_CASE("human_paths_through_properties")
{
    ScopedFastFlag sff{FFlag::DebugLuauForceOldSolver, false};
    ScopedFastFlag newTypePathErrorMessages{FFlag::LuauNewTypePathErrorMessages, true};

    CHECK(renderTypePathPrefix(PathBuilder().readProp("a").readProp("b").build()) == "property `a.b` has type ");
    CHECK(
        renderTypePathPrefix(PathBuilder().writeProp("a").writeProp("b").build()) ==
        "property `b` of a value assigned to property `a` accepts values of type "
    );
    CHECK(renderTypePathPrefix(PathBuilder().writeProp("a").readProp("b").build()) == "property `b` of a value assigned to property `a` has type ");
    CHECK(renderTypePathPrefix(PathBuilder().readProp("a").writeProp("b").build()) == "property `a.b` accepts values of type ");
    CHECK(
        renderTypePathPrefix(PathBuilder().writeProp("a").writeProp("b").readProp("c").build()) ==
        "property `c` of a value assigned to property `b` of a value assigned to property `a` has type "
    );

    CHECK(renderTypePathPrefix(PathBuilder().readProp("callback").args().build()) == "the parameter types of property `callback` are ");
    CHECK(renderTypePathPrefix(PathBuilder().readProp("callback").rets().build()) == "the return types of property `callback` are ");

    CHECK(renderTypePathPrefix(PathBuilder().readProp("a").mt().readProp("b").build()) == "property `b` in the metatable of property `a` has type ");
    CHECK(
        renderTypePathPrefix(PathBuilder().readProp("a").mt().writeProp("b").build()) ==
        "property `b` in the metatable of property `a` accepts values of type "
    );

    CHECK(
        renderTypePathPrefix(PathBuilder().indexValue().readProp("edge").readProp("flag").build()) ==
        "property `edge.flag` in the indexer result has type "
    );
}

TEST_CASE_FIXTURE(TypePathFixture, "structured_human_paths")
{
    ScopedFastFlag newTypePathErrorMessages{FFlag::LuauNewTypePathErrorMessages, true};

    RenderedTypePath readProperty = renderTypePath(PathBuilder().readProp("a").readProp("b").build());
    CHECK(readProperty.subject == "property `a.b`");
    CHECK(readProperty.prefix == "property `a.b` has type ");

    RenderedTypePath writeProperty = renderTypePath(PathBuilder().readProp("a").writeProp("b").build());
    CHECK(writeProperty.subject == "values assigned to property `a.b`");
    CHECK(writeProperty.prefix == "property `a.b` accepts values of type ");

    RenderedTypePath propertyOfAssignedValue = renderTypePath(PathBuilder().writeProp("a").readProp("b").build());
    CHECK(propertyOfAssignedValue.subject == "property `b` of a value assigned to property `a`");
    CHECK(propertyOfAssignedValue.prefix == "property `b` of a value assigned to property `a` has type ");

    RenderedTypePath hiddenComponent = renderTypePath(Path(Index{0, Index::Variant::Union}));
    CHECK(hiddenComponent.subject.empty());
    CHECK(hiddenComponent.prefix.empty());

    TypePackId singletonPack = arena.addTypePack({getBuiltins()->numberType});

    TypePathRenderMetadata singularReturn;
    singularReturn.returnTypePacks.try_insert(1, TypePathRenderMetadata::ReturnTypePackInfo{singletonPack, true});

    RenderedTypePath returnValue = renderTypePath(PathBuilder().rets().index(0).build(), singularReturn);
    CHECK(returnValue.subject == "the return type");
    CHECK(returnValue.prefix == "the return type is ");

    TypePathRenderMetadata nestedSingularReturns;
    nestedSingularReturns.returnTypePacks.try_insert(1, TypePathRenderMetadata::ReturnTypePackInfo{singletonPack, true});
    nestedSingularReturns.returnTypePacks.try_insert(3, TypePathRenderMetadata::ReturnTypePackInfo{singletonPack, true});

    RenderedTypePath nestedReturnValue = renderTypePath(PathBuilder().rets().index(0).rets().index(0).build(), nestedSingularReturns);
    CHECK(nestedReturnValue.subject == "the return type of the function returned by this function");
    CHECK(nestedReturnValue.prefix == "the return type of the function returned by this function is ");

    TypeId negation = arena.addType(NegationType{getBuiltins()->numberType});
    const Path negatedPath{TypeField::Negated};
    TypePathRenderMetadata negationMetadata = deriveRenderMetadata(negation, negatedPath, getBuiltins(), NotNull{&arena});
    RenderedTypePath negatedType = renderTypePath(negatedPath, negationMetadata);
    CHECK(negatedType.enclosingNegation == negation);
}

TEST_CASE_FIXTURE(TypePathFixture, "render_options_handle_cyclic_return_packs")
{
    ScopedFastFlag newTypePathErrorMessages{FFlag::LuauNewTypePathErrorMessages, true};

    const Path returnPath = PathBuilder().rets().index(0).build();

    TypePackId returnPack = arena.addTypePack({getBuiltins()->numberType});
    getMutable<TypePack>(returnPack)->tail = returnPack;
    TypeId function = arena.addType(FunctionType{getBuiltins()->emptyTypePack, returnPack});

    TypePathRenderMetadata options = deriveRenderMetadata(function, returnPath, getBuiltins(), NotNull{&arena});
    const TypePathRenderMetadata::ReturnTypePackInfo* returnTypePack = options.returnTypePacks.find(1);
    REQUIRE(returnTypePack);
    CHECK_FALSE(returnTypePack->isSingular);
}

TEST_CASE("human_single_components")
{
    ScopedFastFlag sff{FFlag::DebugLuauForceOldSolver, false};
    ScopedFastFlag newTypePathErrorMessages{FFlag::LuauNewTypePathErrorMessages, true};

    CHECK(renderTypePathPrefix(kEmpty).empty());
    CHECK(renderTypePathPrefix(Path(TypePath::Property::read("a"))) == "property `a` has type ");
    CHECK(renderTypePathPrefix(Path(TypePath::Property::write("a"))) == "property `a` accepts values of type ");

    CHECK(renderTypePathPrefix(Path(Index{0, Index::Variant::Pack})) == "the 1st type pack entry is ");
    CHECK(renderTypePathPrefix(Path(Index{0, Index::Variant::Union})).empty());
    CHECK(renderTypePathPrefix(Path(Index{0, Index::Variant::Intersection})).empty());

    CHECK(renderTypePathPrefix(Path(TypeField::Table)) == "the table portion is ");
    CHECK(renderTypePathPrefix(Path(TypeField::Metatable)) == "the metatable is ");
    CHECK(renderTypePathPrefix(Path(TypeField::LowerBound)) == "the lower bound is ");
    CHECK(renderTypePathPrefix(Path(TypeField::UpperBound)) == "the upper bound is ");
    CHECK(renderTypePathPrefix(Path(TypeField::IndexLookup)) == "the indexer key type is ");
    CHECK(renderTypePathPrefix(Path(TypeField::IndexResult)) == "the indexer result is ");
    CHECK(renderTypePathPrefix(Path(TypeField::Negated)) == "the negated type is ");
    CHECK(renderTypePathPrefix(Path(TypeField::Variadic)) == "the variadic tail has element type ");

    CHECK(renderTypePathPrefix(Path(PackField::Arguments)) == "the parameter types are ");
    CHECK(renderTypePathPrefix(Path(PackField::Returns)) == "the return types are ");
    CHECK(renderTypePathPrefix(Path(PackField::Tail)) == "the type pack's tail is ");
}

TEST_CASE("human_paths_through_type_components")
{
    ScopedFastFlag sff{FFlag::DebugLuauForceOldSolver, false};
    ScopedFastFlag newTypePathErrorMessages{FFlag::LuauNewTypePathErrorMessages, true};

    CHECK(renderTypePathPrefix(Path({Index{0, Index::Variant::Pack}, Index{1, Index::Variant::Union}})) == "the 1st type pack entry is ");
    CHECK(renderTypePathPrefix(Path({Index{0, Index::Variant::Union}, Index{1, Index::Variant::Intersection}})).empty());
    CHECK(renderTypePathPrefix(Path({TypeField::Metatable, Index{0, Index::Variant::Union}})) == "the metatable is ");
    CHECK(renderTypePathPrefix(Path({TypeField::Table, Index{0, Index::Variant::Intersection}})) == "the table portion is ");

    CHECK(
        renderTypePathPrefix(Path({TypePath::Property::read("a"), Index{0, Index::Variant::Union}, TypePath::Property::read("b")})) ==
        "property `a.b` has type "
    );
    CHECK(
        renderTypePathPrefix(Path({TypePath::Property::read("a"), TypeField::Metatable, Index{0, Index::Variant::Union}})) ==
        "property `a` has metatable "
    );
    CHECK(
        renderTypePathPrefix(
            Path({TypePath::Property::read("a"), TypeField::Metatable, TypePath::Property::read("b"), Index{0, Index::Variant::Union}})
        ) == "property `b` in the metatable of property `a` has type "
    );

    // Error path from example in https://github.com/luau-lang/luau/issues/1951
    CHECK(renderTypePathPrefix(Path({TypePath::Property::read("build"), Index{1, Index::Variant::Union}})) == "property `build` has type ");
    // Error path from example in https://github.com/luau-lang/luau/issues/1762
    CHECK(
        renderTypePathPrefix(Path({TypePath::Property::read("instance"), Index{1, Index::Variant::Intersection}})) == "property `instance` has type "
    );

    // Error path from example in https://github.com/luau-lang/luau/issues/2357
    CHECK(
        renderTypePathPrefix(Path({Index{0, Index::Variant::Intersection}, Index{1, Index::Variant::Union}, TypePath::Property::read("can_meow")})) ==
        "property `can_meow` has type "
    );
    // Error path from example in https://github.com/luau-lang/luau/issues/2435
    CHECK(
        renderTypePathPrefix(
            Path({TypeField::IndexResult, TypePath::Property::read("edge"), Index{0, Index::Variant::Union}, TypePath::Property::read("flag")})
        ) == "property `edge.flag` in the indexer result has type "
    );

    CHECK(
        renderTypePathPrefix(
            Path({TypePath::Property::read("a"), TypeField::Metatable, TypeField::IndexResult, TypePath::Property::read("b"), TypeField::Metatable})
        ) == "property `b` in the indexer result of the metatable of property `a` has metatable "
    );

    CHECK(renderTypePathPrefix(Path({TypeField::Negated, TypePath::Property::read("a")})) == "property `a` of the negated type has type ");
    CHECK(renderTypePathPrefix(Path({TypeField::IndexResult, TypePath::Property::read("a")})) == "property `a` in the indexer result has type ");
    CHECK(
        renderTypePathPrefix(Path({TypePath::Property::read("a"), Index{0, Index::Variant::Union}, PackField::Arguments})) ==
        "the parameter types of property `a` are "
    );
}

TEST_CASE("human_paths_through_function_packs")
{
    ScopedFastFlag sff{FFlag::DebugLuauForceOldSolver, false};
    ScopedFastFlag newTypePathErrorMessages{FFlag::LuauNewTypePathErrorMessages, true};

    // Error path from example in https://github.com/luau-lang/luau/issues/2528
    // Error path from example in https://github.com/luau-lang/luau/issues/2076
    CHECK(renderTypePathPrefix(PathBuilder().args().index(0).build()) == "the 1st parameter has type ");
    CHECK(renderTypePathPrefix(PathBuilder().rets().index(0).build()) == "the 1st return value has type ");
    CHECK(renderTypePathPrefix(PathBuilder().mt().args().index(0).build()) == "the 1st parameter of the metatable function has type ");
    CHECK(renderTypePathPrefix(PathBuilder().mt().rets().index(0).build()) == "the 1st return value of the metatable function has type ");

    CHECK(
        renderTypePathPrefix(Path({Index{0, Index::Variant::Union}, PackField::Arguments, Index{1, Index::Variant::Pack}})) ==
        "the 2nd parameter has type "
    );
    CHECK(
        renderTypePathPrefix(Path({Index{0, Index::Variant::Intersection}, PackField::Returns, Index{1, Index::Variant::Pack}})) ==
        "the 2nd return value has type "
    );

    CHECK(renderTypePathPrefix(PathBuilder().readProp("callback").args().index(0).build()) == "the 1st parameter of property `callback` has type ");
    CHECK(
        renderTypePathPrefix(PathBuilder().readProp("callback").rets().index(0).build()) == "the 1st return value of property `callback` has type "
    );
    CHECK(
        renderTypePathPrefix(PathBuilder().writeProp("callback").args().index(0).build()) ==
        "the 1st parameter of a function assigned to `callback` has type "
    );
    CHECK(
        renderTypePathPrefix(PathBuilder().writeProp("callback").rets().index(0).build()) ==
        "the 1st return value of a function assigned to `callback` has type "
    );

    CHECK(
        renderTypePathPrefix(Path({PackField::Arguments, Index{0, Index::Variant::Pack}, Index{1, Index::Variant::Union}})) ==
        "the 1st parameter has type "
    );
    CHECK(
        renderTypePathPrefix(Path({PackField::Returns, Index{0, Index::Variant::Pack}, TypePath::Property::read("x")})) ==
        "property `x` of the 1st return value has type "
    );

    CHECK(renderTypePathPrefix(PathBuilder().tail().variadic().build()) == "the variadic tail has element type ");
    CHECK(renderTypePathPrefix(PathBuilder().args().tail().build()) == "the parameter type pack tail is ");
    CHECK(renderTypePathPrefix(PathBuilder().rets().tail().build()) == "the return type pack tail is ");
    CHECK(renderTypePathPrefix(PathBuilder().args().tail().variadic().build()) == "the variadic parameter has type ");
    CHECK(renderTypePathPrefix(PathBuilder().rets().tail().variadic().build()) == "the variadic return value has type ");

    CHECK(
        renderTypePathPrefix(Path({Index{0, Index::Variant::Intersection}, PackField::Arguments, PackField::Tail})) ==
        "the parameter type pack tail is "
    );
    CHECK(
        renderTypePathPrefix(Path({Index{1, Index::Variant::Intersection}, PackField::Returns, PackField::Tail, TypeField::Variadic})) ==
        "the variadic return value has type "
    );
}

TEST_CASE("pack_slice")
{
    ScopedFastFlag sff{FFlag::DebugLuauForceOldSolver, false};

    CHECK(toString(PathBuilder().packSlice(1).build()) == "[1:]");
    if (FFlag::LuauNewTypePathErrorMessages)
    {
        CHECK(renderTypePathPrefix(PathBuilder().packSlice(1).build()) == "the type pack from its 2nd entry onward is ");
        CHECK(renderTypePathPrefix(PathBuilder().args().packSlice(1).build()) == "the parameters from the 2nd onward are ");
        CHECK(renderTypePathPrefix(PathBuilder().mt().args().packSlice(1).build()) == "the metatable function's parameters from the 2nd onward are ");
        CHECK(
            renderTypePathPrefix(PathBuilder().readProp("callback").args().packSlice(1).build()) ==
            "the parameters of property `callback` from the 2nd onward are "
        );
        CHECK(
            renderTypePathPrefix(PathBuilder().writeProp("callback").rets().packSlice(1).build()) ==
            "the return values of a function assigned to `callback` from the 2nd onward are "
        );
    }
    else
        CHECK(toStringHuman_DEPRECATED(PathBuilder().packSlice(1).build()) == "the portion of the type pack starting at index 1 to the end");
}

TEST_CASE_FIXTURE(TypePathFixture, "human_reduction_and_mapped_pack_paths")
{
    ScopedFastFlag newTypePathErrorMessages{FFlag::LuauNewTypePathErrorMessages, true};
    TypePackId mappedPack = arena.addTypePack({getBuiltins()->numberType});

    CHECK(renderTypePathPrefix(Path(TypePath::Reduction{getBuiltins()->numberType})) == "the reduced type is ");
    CHECK(renderTypePathPrefix(Path(TypePath::GenericPackMapping{mappedPack})) == "the generic type pack is substituted with ");

    CHECK(
        renderTypePathPrefix(Path({TypePath::Reduction{getBuiltins()->numberType}, TypePath::Property::read("x")})) ==
        "property `x` of the reduced type has type "
    );
    CHECK(renderTypePathPrefix(Path({Index{0, Index::Variant::Union}, TypePath::Reduction{getBuiltins()->numberType}})) == "the reduced type is ");
    CHECK(renderTypePathPrefix(PathBuilder().tail().mappedGenericPack(mappedPack).build()) == "the type pack tail is substituted with ");
    CHECK(
        renderTypePathPrefix(PathBuilder().tail().mappedGenericPack(mappedPack).index(0).build()) ==
        "the 1st entry of the substituted type pack tail is "
    );
    CHECK(
        renderTypePathPrefix(PathBuilder().args().tail().mappedGenericPack(mappedPack).build()) == "the parameter type pack tail is substituted with "
    );
    CHECK(
        renderTypePathPrefix(PathBuilder().rets().tail().mappedGenericPack(mappedPack).build()) == "the return type pack tail is substituted with "
    );
    CHECK(
        renderTypePathPrefix(PathBuilder().args().tail().mappedGenericPack(mappedPack).index(0).build()) ==
        "the 1st entry of the substituted parameter type pack tail is "
    );
    CHECK(
        renderTypePathPrefix(PathBuilder().tail().mappedGenericPack(mappedPack).packSlice(1).build()) ==
        "the substituted type pack tail, from its 2nd entry onward, is "
    );
    CHECK(
        renderTypePathPrefix(PathBuilder().readProp("callback").args().tail().mappedGenericPack(mappedPack).packSlice(1).build()) ==
        "the substituted parameter tail for property `callback`, from its 2nd entry onward, is "
    );
    CHECK(
        renderTypePathPrefix(PathBuilder().readProp("callback").rets().tail().mappedGenericPack(mappedPack).packSlice(1).build()) ==
        "the substituted return tail for property `callback`, from its 2nd entry onward, is "
    );
    CHECK(
        renderTypePathPrefix(
            Path({Index{0, Index::Variant::Intersection}, PackField::Arguments, PackField::Tail, TypePath::GenericPackMapping{mappedPack}})
        ) == "the parameter type pack tail is substituted with "
    );
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
    ScopedFastFlag sff{FFlag::DebugLuauForceOldSolver, false};

    CHECK(
        PathBuilder().index(0).readProp("foo").mt().readProp("bar").args().index(1).build() ==
        Path({Index{0}, TypePath::Property::read("foo"), TypeField::Metatable, TypePath::Property::read("bar"), PackField::Arguments, Index{1}})
    );
}
TEST_CASE("pack_slice")
{
    CHECK(PathBuilder().packSlice(3).build() == Path(PackSlice{3}));
}

TEST_SUITE_END(); // TypePathBuilder
