// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "doctest.h"
#include "Fixture.h"

#include "Luau/OverloadResolution.h"
#include "Luau/Normalize.h"
#include "Luau/UnifierSharedState.h"

LUAU_FASTFLAG(LuauNewOverloadResolver2)

using namespace Luau;

struct OverloadResolverFixture : Fixture
{
    TypeArena arena_;
    NotNull<TypeArena> arena{&arena_};
    UnifierSharedState sharedState{&ice};
    Normalizer normalizer{arena, getBuiltins(), NotNull{&sharedState}, FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old};
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
            arena,
            NotNull{&normalizer},
            NotNull{&typeFunctionRuntime},
            NotNull{&rootScope},
            NotNull{&iceReporter},
            NotNull{&limits},
            callLocation
        };
    }

    DenseHashSet<TypeId> kEmptySet{nullptr};
    NotNull<DenseHashSet<TypeId>> emptySet{&kEmptySet};
    Location kDummyLocation;
    AstExprConstantNil kDummyExpr{kDummyLocation};
    std::vector<AstExpr*> kEmptyExprs;

    TypePackId pack(std::vector<TypeId> tys) const
    {
        return arena->addTypePack(std::move(tys));
    }

    TypePackId pack(std::initializer_list<TypeId> tys) const
    {
        return arena->addTypePack(tys);
    }

    TypePackId pack(std::initializer_list<TypeId> tys, TypePackVariant tail) const
    {
        return arena->addTypePack(tys, arena->addTypePack(std::move(tail)));
    }

    TypeId fn(std::initializer_list<TypeId> args, std::initializer_list<TypeId> rets) const
    {
        return arena->addType(FunctionType{pack(args), pack(rets)});
    }

    // `&`
    TypeId meet(TypeId a, TypeId b) const
    {
        return arena->addType(IntersectionType{{a, b}});
    }
    TypeId meet(std::initializer_list<TypeId> parts) const
    {
        return arena->addType(IntersectionType{parts});
    }
    TypeId join(TypeId a, TypeId b) const
    {
        return arena->addType(UnionType{{a, b}});
    }

    TypeId tableWithCall(TypeId callMm) const
    {
        TypeId table = arena->addType(TableType{TableState::Sealed, TypeLevel{}, /*scope*/nullptr});
        TypeId metatable = arena->addType(TableType{
            TableType::Props{
                {"__call", callMm}
            },
            std::nullopt,
            TypeLevel{},
            TableState::Sealed
        });

        return arena->addType(MetatableType{table, metatable});
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
        resolver.selectOverload_DEPRECATED(numberToNumberAndStringToString, pack({getBuiltins()->numberType}), emptySet, false);

    REQUIRE_EQ(OverloadResolver::Analysis::Ok, analysis);
    REQUIRE_EQ(numberToNumber, overload);
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "basic_overload_selection1")
{
    // ty: (number) -> number & (string) -> string
    // args: (string)
    auto [analysis, overload] =
        resolver.selectOverload_DEPRECATED(numberToNumberAndStringToString, pack({getBuiltins()->stringType}), emptySet, false);

    REQUIRE_EQ(OverloadResolver::Analysis::Ok, analysis);
    REQUIRE_EQ(stringToString, overload);
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "overloads_with_different_arities")
{
    // ty: (number) -> number & (number, number) -> number
    // args: (number)
    auto [analysis, overload] =
        resolver.selectOverload_DEPRECATED(numberToNumberAndNumberNumberToNumber, pack({getBuiltins()->numberType}), emptySet, false);

    REQUIRE_EQ(OverloadResolver::Analysis::Ok, analysis);
    REQUIRE_EQ(numberToNumber, overload);
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "overloads_with_different_arities1")
{
    // ty: (number) -> number & (number, number) -> number
    // args: (number, number)
    auto [analysis, overload] = resolver.selectOverload_DEPRECATED(
        numberToNumberAndNumberNumberToNumber, pack({getBuiltins()->numberType, getBuiltins()->numberType}), emptySet, false
    );

    REQUIRE_EQ(OverloadResolver::Analysis::Ok, analysis);
    REQUIRE_EQ(numberNumberToNumber, overload);
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "separate_non_viable_overloads_by_arity_mismatch")
{
    // ty: ((number)->number) & ((number)->string) & ((number, number)->number)
    // args: (string)
    OverloadResolver r = mkResolver();

    const TypePack args = TypePack{{builtinTypes->stringType}, std::nullopt};
    r.resolve_DEPRECATED(meet({numberToNumber, numberToString, numberNumberToNumber}), &args, &kDummyExpr, &kEmptyExprs, emptySet);

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

/////////////////////////////////////////////////////////////////

TEST_CASE_FIXTURE(OverloadResolverFixture, "new_basic_overload_selection")
{
    // ty: (number) -> number & (string) -> string
    // args: (number)
    OverloadResolution result =
        resolver.resolveOverload(numberToNumberAndStringToString, pack({getBuiltins()->numberType}), Location{}, emptySet, false);

    CHECK(1 == result.ok.size());
    CHECK(result.ok.at(0) == numberToNumber);
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "new_basic_overload_selection1")
{
    // ty: (number) -> number & (string) -> string
    // args: (string)
    OverloadResolution result =
        resolver.resolveOverload(numberToNumberAndStringToString, pack({getBuiltins()->stringType}), Location{}, emptySet, false);

    CHECK(1 == result.ok.size());
    CHECK(stringToString == result.ok.at(0));

    CHECK(1 == result.incompatibleOverloads.size());
    CHECK(numberToNumber == result.incompatibleOverloads.at(0).first);
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "new_match_call_metamethod")
{
    // (unknown, number) -> number
    TypeId callMm = fn({builtinTypes->unknownType, builtinTypes->numberType}, {builtinTypes->numberType});
    TypeId tbl = tableWithCall(callMm);

    OverloadResolution result =
        resolver.resolveOverload(tbl, pack({builtinTypes->numberType}), Location{}, emptySet, false);

    // Possible design issue here: We're indicating that an overload matches,
    // but it clearly has a different arity than the type pack that was
    // passed!
    //
    // Complicated: The metatable-having table could be part of an overload group. (eg {....} & (number) -> number)
    // OR the metamethod itself could be overloaded ({__call: (number) -> number & (string) -> string})
    CHECK(1 == result.ok.size());
    CHECK(callMm == result.ok.at(0));
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "new_metamethod_could_be_overloaded")
{
    // (unknown, number) -> number
    TypeId overload1 = fn({builtinTypes->unknownType, builtinTypes->numberType}, {builtinTypes->numberType});
    // (unknown, string) -> string
    TypeId overload2 = fn({builtinTypes->unknownType, builtinTypes->stringType}, {builtinTypes->stringType});
    TypeId tbl = tableWithCall(meet(overload1, overload2));

    OverloadResolution result =
        resolver.resolveOverload(tbl, pack({builtinTypes->numberType}), Location{}, emptySet, false);

    // Possible design issue here: We're indicating that an overload matches,
    // but it clearly has a different arity than the type pack that was
    // passed!
    //
    // Complicated: The metatable-having table could be part of an overload group. (eg {....} & (number) -> number)
    // OR the metamethod itself could be overloaded ({__call: (number) -> number & (string) -> string})
    CHECK(1 == result.ok.size());
    CHECK(overload1 == result.ok.at(0));

    CHECK(1 == result.incompatibleOverloads.size());
    CHECK(overload2 == result.incompatibleOverloads.at(0).first);
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "new_overload_group_could_include_metamethod")
{
    // (unknown, number) -> number
    TypeId overload1 = fn({builtinTypes->unknownType, builtinTypes->numberType}, {builtinTypes->numberType});
    // (unknown, string) -> string
    TypeId overload2 = fn({builtinTypes->unknownType, builtinTypes->stringType}, {builtinTypes->stringType});
    TypeId tbl = tableWithCall(meet(overload1, overload2));

    TypeId monstrosity = meet(tbl, fn({builtinTypes->booleanType}, {builtinTypes->booleanType}));

    OverloadResolution result =
        resolver.resolveOverload(monstrosity, pack({builtinTypes->numberType}), Location{}, emptySet, false);

    CHECK(1 == result.ok.size());
    CHECK(overload1 == result.ok.at(0));
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "new_overloads_with_different_arities")
{
    // ty: (number) -> number & (number, number) -> number
    // args: (number)
    OverloadResolution result =
        resolver.resolveOverload(numberToNumberAndNumberNumberToNumber, pack({getBuiltins()->numberType}), Location{}, emptySet, false);

    CHECK(1 == result.ok.size());
    CHECK(numberToNumber == result.ok.at(0));

    CHECK(1 == result.arityMismatches.size());
    CHECK(numberNumberToNumber == result.arityMismatches.at(0));
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "new_overloads_with_different_arities1")
{
    ScopedFastFlag sff{FFlag::LuauNewOverloadResolver2, true};

    // ty: (number) -> number & (number, number) -> number
    // args: (number, number)
    OverloadResolution result = resolver.resolveOverload(
        numberToNumberAndNumberNumberToNumber, pack({getBuiltins()->numberType, getBuiltins()->numberType}), Location{}, emptySet, false
    );

    CHECK(1 == result.ok.size());
    CHECK(numberNumberToNumber == result.ok.at(0));

    CHECK(1 == result.arityMismatches.size());
    CHECK(numberToNumber == result.arityMismatches.at(0));
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "new_separate_non_viable_overloads_by_arity_mismatch")
{
    // ty: ((number)->number) & ((number)->string) & ((number, number)->number)
    // args: (string)
    const TypePack args = TypePack{{builtinTypes->stringType}, std::nullopt};

    OverloadResolution resolution =
        resolver.resolveOverload(
            meet({numberToNumber, numberToString, numberNumberToNumber}),
            pack({builtinTypes->stringType}),
            Location{},
            emptySet,
            false
        );

    CHECK(resolution.ok.empty());
    CHECK(resolution.nonFunctions.empty());
    CHECK_EQ(1, resolution.arityMismatches.size());
    CHECK_EQ(numberNumberToNumber, resolution.arityMismatches[0]);

    CHECK_EQ(2, resolution.incompatibleOverloads.size());
    bool numberToNumberFound = false;
    bool numberToStringFound = false;
    for (const auto& [ty, _] : resolution.incompatibleOverloads)
    {
        if (ty == numberToNumber)
            numberToNumberFound = true;
        else if (ty == numberToString)
            numberToStringFound = true;
    }
    CHECK(numberToNumberFound);
    CHECK(numberToStringFound);
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "new_select")
{
    TypeId numberOrString = join(builtinTypes->numberType, builtinTypes->stringType);
    TypePackId genericAs = arena->addTypePack(GenericTypePack{"A"});

    TypeId selectTy = arena->addType(FunctionType{
        {},
        {genericAs},
        arena->addTypePack({numberOrString}, genericAs),
        builtinTypes->anyTypePack
    });

    OverloadResolver r = mkResolver();
    OverloadResolution resolution =
        r.resolveOverload(
            selectTy,
            arena->addTypePack({numberOrString}, builtinTypes->anyTypePack),
            Location{},
            emptySet,
            false
        );

    CHECK(1 == resolution.ok.size());
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "new_pass_table_with_indexer")
{
    // {[any]: number}
    TypeId anyNumberTable = arena->addType(TableType{
        TableType::Props{},
        TableIndexer{builtinTypes->anyType, builtinTypes->numberType},
        TypeLevel{},
        &rootScope,
        TableState::Sealed
    });

    TypeId tableToTable = fn({anyNumberTable}, {anyNumberTable});

    OverloadResolver r = mkResolver();
    OverloadResolution resolution = r.resolveOverload(tableToTable, pack({anyNumberTable}), Location{}, emptySet, false);

    CHECK(1 == resolution.ok.size());
    CHECK(0 == resolution.potentialOverloads.size());
    CHECK(0 == resolution.incompatibleOverloads.size());
    CHECK(0 == resolution.nonFunctions.size());
    CHECK(0 == resolution.arityMismatches.size());
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "generic_higher_order_function_called_improperly")
{
    ScopedFastFlag sff{FFlag::LuauNewOverloadResolver2, true};

    // apply: <A, B..., C...>((A, B...) -> C..., A) -> C...
    const TypeId genericA = arena->addType(GenericType{"A"});
    const TypePackId genericBs = arena->addTypePack(GenericTypePack{"B"});
    const TypePackId genericCs = arena->addTypePack(GenericTypePack{"C"});

    TypeId functionArgument = arena->addType(FunctionType{arena->addTypePack({genericA}, genericBs), genericCs});

    TypePackId applyArgs = pack({functionArgument, genericA});

    TypeId applyTy = arena->addType(FunctionType{
        {genericA},
        {genericBs, genericCs},
        applyArgs,
        genericCs
    });

    TypePackId callArgsPack = pack({numberNumberToNumber, builtinTypes->numberType});

    OverloadResolver r = mkResolver();
    OverloadResolution resolution = r.resolveOverload(applyTy, callArgsPack, Location{}, emptySet, false);

    CHECK(1 == resolution.ok.size());
}

TEST_CASE_FIXTURE(OverloadResolverFixture, "debug_traceback")
{
    // ((message: string?, level: number?) -> string) & ((thread: thread, message: string?, level: number?) -> string)
    TypeId overload1 = fn({builtinTypes->optionalStringType, builtinTypes->optionalNumberType}, {builtinTypes->stringType});
    TypeId overload2 = fn({builtinTypes->threadType, builtinTypes->optionalStringType, builtinTypes->optionalNumberType}, {builtinTypes->stringType});

    TypeId debugTraceback = meet({overload1, overload2});

    OverloadResolver r = mkResolver();
    OverloadResolution resolution;

    SUBCASE("no_arguments")
    {
        resolution = r.resolveOverload(debugTraceback, builtinTypes->emptyTypePack, Location{}, emptySet, false);
        CHECK(1 == resolution.ok.size());
    }

    SUBCASE("message_only")
    {
        resolution = r.resolveOverload(debugTraceback, pack({builtinTypes->stringType}), Location{}, emptySet, false);
        CHECK(1 == resolution.ok.size());
    }

    SUBCASE("message_and_level")
    {
        resolution = r.resolveOverload(debugTraceback, pack({builtinTypes->stringType, builtinTypes->numberType}), Location{}, emptySet, false);
        CHECK(1 == resolution.ok.size());
    }

    SUBCASE("thread")
    {
        resolution = r.resolveOverload(debugTraceback, pack({builtinTypes->threadType}), Location{}, emptySet, false);
        CHECK(1 == resolution.ok.size());
    }

    SUBCASE("thread_and_message")
    {
        resolution = r.resolveOverload(debugTraceback, pack({builtinTypes->threadType, builtinTypes->stringType}), Location{}, emptySet, false);
        CHECK(1 == resolution.ok.size());
    }

    SUBCASE("thread_message_and_level")
    {
        resolution = r.resolveOverload(debugTraceback, pack({builtinTypes->threadType, builtinTypes->stringType, builtinTypes->numberType}), Location{}, emptySet, false);
        CHECK(1 == resolution.ok.size());
    }
}

TEST_SUITE_END(); // OverloadResolverTest
