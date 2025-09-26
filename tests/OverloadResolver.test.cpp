// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "doctest.h"
#include "Fixture.h"

#include "Luau/OverloadResolution.h"
#include "Luau/Normalize.h"
#include "Luau/UnifierSharedState.h"

LUAU_FASTFLAG(LuauFilterOverloadsByArity)

using namespace Luau;

struct OverloadResolverFixture : Fixture
{
    TypeArena arena;
    SimplifierPtr simplifier = newSimplifier(NotNull{&arena}, getBuiltins());
    UnifierSharedState sharedState{&ice};
    Normalizer normalizer{&arena, getBuiltins(), NotNull{&sharedState}, FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old};
    InternalErrorReporter iceReporter;
    TypeCheckLimits limits;
    TypeFunctionRuntime typeFunctionRuntime{NotNull{&iceReporter}, NotNull{&limits}};
    Scope rootScope{getBuiltins()->emptyTypePack};
    Location callLocation;

    OverloadResolver resolver = mkResolver();

    OverloadResolver mkResolver()
    {
        return OverloadResolver{
            getBuiltins(),
            NotNull{&arena},
            NotNull{simplifier.get()},
            NotNull{&normalizer},
            NotNull{&typeFunctionRuntime},
            NotNull{&rootScope},
            NotNull{&iceReporter},
            NotNull{&limits},
            callLocation
        };
    }

    DenseHashSet<TypeId> kEmptySet{nullptr};
    Location kDummyLocation;
    AstExprConstantNil kDummyExpr{kDummyLocation};
    std::vector<AstExpr*> kEmptyExprs;

    TypePackId pack(std::initializer_list<TypeId> tys)
    {
        return arena.addTypePack(tys);
    }

    TypePackId pack(std::initializer_list<TypeId> tys, TypePackVariant tail)
    {
        return arena.addTypePack(tys, arena.addTypePack(std::move(tail)));
    }

    TypeId fn(std::initializer_list<TypeId> args, std::initializer_list<TypeId> rets)
    {
        return arena.addType(FunctionType{pack(args), pack(rets)});
    }

    // `&`
    TypeId meet(TypeId a, TypeId b)
    {
        return arena.addType(IntersectionType{{a, b}});
    }
    TypeId meet(std::initializer_list<TypeId> parts)
    {
        return arena.addType(IntersectionType{parts});
    }

    // (number) -> number
    const TypeId numberToNumber = fn({getBuiltins()->numberType}, {getBuiltins()->numberType});
    // (number, number) -> number
    const TypeId numberNumberToNumber = fn({getBuiltins()->numberType, getBuiltins()->numberType}, {getBuiltins()->numberType});
    // (number) -> string
    const TypeId numberToString = fn({getBuiltins()->numberType}, {getBuiltins()->stringType});
    // (string) -> string
    const TypeId stringToString = fn({getBuiltins()->stringType}, {getBuiltins()->stringType});

    // (number) -> number & (string) -> string
    const TypeId numberToNumberAndStringToString = meet(numberToNumber, stringToString);
    // (number) -> number & (number, number) -> number
    const TypeId numberToNumberAndNumberNumberToNumber = meet(numberToNumber, numberNumberToNumber);
};

TEST_SUITE_BEGIN("OverloadResolverTest");

TEST_CASE_FIXTURE(OverloadResolverFixture, "basic_overload_selection")
{
    // ty: (number) -> number & (string) -> string
    // args: (number)
    auto [analysis, overload] =
        resolver.selectOverload(numberToNumberAndStringToString, pack({getBuiltins()->numberType}), NotNull{&kEmptySet}, false);

    REQUIRE_EQ(OverloadResolver::Analysis::Ok, analysis);
    REQUIRE_EQ(numberToNumber, overload);
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "basic_overload_selection1")
{
    // ty: (number) -> number & (string) -> string
    // args: (string)
    auto [analysis, overload] =
        resolver.selectOverload(numberToNumberAndStringToString, pack({getBuiltins()->stringType}), NotNull{&kEmptySet}, false);

    REQUIRE_EQ(OverloadResolver::Analysis::Ok, analysis);
    REQUIRE_EQ(stringToString, overload);
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "overloads_with_different_arities")
{
    // ty: (number) -> number & (number, number) -> number
    // args: (number)
    auto [analysis, overload] =
        resolver.selectOverload(numberToNumberAndNumberNumberToNumber, pack({getBuiltins()->numberType}), NotNull{&kEmptySet}, false);

    REQUIRE_EQ(OverloadResolver::Analysis::Ok, analysis);
    REQUIRE_EQ(numberToNumber, overload);
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "overloads_with_different_arities1")
{
    // ty: (number) -> number & (number, number) -> number
    // args: (number, number)
    auto [analysis, overload] = resolver.selectOverload(
        numberToNumberAndNumberNumberToNumber, pack({getBuiltins()->numberType, getBuiltins()->numberType}), NotNull{&kEmptySet}, false
    );

    REQUIRE_EQ(OverloadResolver::Analysis::Ok, analysis);
    REQUIRE_EQ(numberNumberToNumber, overload);
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "separate_non_viable_overloads_by_arity_mismatch")
{
    ScopedFastFlag sff{FFlag::LuauFilterOverloadsByArity, true};

    // ty: ((number)->number) & ((number)->string) & ((number, number)->number)
    // args: (string)
    OverloadResolver r = mkResolver();

    const TypePack args = TypePack{{builtinTypes->stringType}, std::nullopt};
    r.resolve(meet({numberToNumber, numberToString, numberNumberToNumber}), &args, &kDummyExpr, &kEmptyExprs, NotNull{&kEmptySet});

    CHECK(r.ok.empty());
    CHECK(r.nonFunctions.empty());
    CHECK_EQ(1, r.arityMismatches.size());
    CHECK_EQ(numberNumberToNumber, r.arityMismatches[0].first);

    CHECK_EQ(2, r.nonviableOverloads.size());
    bool numberToNumberFound = false;
    bool numberToStringFound = false;
    for (const auto& [ty, _] : r.nonviableOverloads)
    {
        if (ty == numberToNumber)
            numberToNumberFound = true;
        else if (ty == numberToString)
            numberToStringFound = true;
    }
    CHECK(numberToNumberFound);
    CHECK(numberToStringFound);
}

TEST_SUITE_END(); // OverloadResolverTest
