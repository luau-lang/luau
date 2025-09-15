// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Generalization.h"
#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/Error.h"

#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(DebugLuauForbidInternalTypes)
LUAU_FASTFLAG(LuauSubtypingReportGenericBoundMismatches2)

LUAU_FASTFLAG(LuauSubtypingGenericsDoesntUseVariance)

TEST_SUITE_BEGIN("Generalization");

struct GeneralizationFixture
{
    TypeArena arena;
    BuiltinTypes builtinTypes;
    ScopePtr globalScope = std::make_shared<Scope>(builtinTypes.anyTypePack);
    ScopePtr scope = std::make_shared<Scope>(globalScope);
    ToStringOptions opts;

    DenseHashSet<TypeId> generalizedTypes_{nullptr};
    NotNull<DenseHashSet<TypeId>> generalizedTypes{&generalizedTypes_};

    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    std::pair<TypeId, FreeType*> freshType()
    {
        FreeType ft{scope.get(), builtinTypes.neverType, builtinTypes.unknownType};

        TypeId ty = arena.addType(ft);
        FreeType* ftv = getMutable<FreeType>(ty);
        REQUIRE(ftv != nullptr);

        return {ty, ftv};
    }

    std::string toString(TypeId ty)
    {
        return ::Luau::toString(ty, opts);
    }

    std::string toString(TypePackId ty)
    {
        return ::Luau::toString(ty, opts);
    }

    std::optional<TypeId> generalize(TypeId ty)
    {
        return ::Luau::generalize(NotNull{&arena}, NotNull{&builtinTypes}, NotNull{scope.get()}, generalizedTypes, ty);
    }
};

TEST_CASE_FIXTURE(GeneralizationFixture, "generalize_a_type_that_is_bounded_by_another_generalizable_type")
{
    auto [t1, ft1] = freshType();
    auto [t2, ft2] = freshType();

    // t2 <: t1 <: unknown
    // unknown <: t2 <: t1

    ft1->lowerBound = t2;
    ft2->upperBound = t1;
    ft2->lowerBound = builtinTypes.unknownType;

    auto t2generalized = generalize(t2);
    REQUIRE(t2generalized);

    CHECK(follow(t1) == follow(t2));

    auto t1generalized = generalize(t1);
    REQUIRE(t1generalized);

    CHECK(builtinTypes.unknownType == follow(t1));
    CHECK(builtinTypes.unknownType == follow(t2));
}

// Same as generalize_a_type_that_is_bounded_by_another_generalizable_type
// except that we generalize the types in the opposite order
TEST_CASE_FIXTURE(GeneralizationFixture, "generalize_a_type_that_is_bounded_by_another_generalizable_type_in_reverse_order")
{
    auto [t1, ft1] = freshType();
    auto [t2, ft2] = freshType();

    // t2 <: t1 <: unknown
    // unknown <: t2 <: t1

    ft1->lowerBound = t2;
    ft2->upperBound = t1;
    ft2->lowerBound = builtinTypes.unknownType;

    auto t1generalized = generalize(t1);
    REQUIRE(t1generalized);

    CHECK(follow(t1) == follow(t2));

    auto t2generalized = generalize(t2);
    REQUIRE(t2generalized);

    CHECK(builtinTypes.unknownType == follow(t1));
    CHECK(builtinTypes.unknownType == follow(t2));
}

TEST_CASE_FIXTURE(GeneralizationFixture, "dont_traverse_into_class_types_when_generalizing")
{
    auto [propTy, _] = freshType();

    TypeId cursedExternType =
        arena.addType(ExternType{"Cursed", {{"oh_no", Property::readonly(propTy)}}, std::nullopt, std::nullopt, {}, {}, "", {}});

    auto genExternType = generalize(cursedExternType);
    REQUIRE(genExternType);

    auto genPropTy = get<ExternType>(*genExternType)->props.at("oh_no").readTy;
    CHECK(is<FreeType>(*genPropTy));
}

TEST_CASE_FIXTURE(GeneralizationFixture, "cache_fully_generalized_types")
{
    CHECK(generalizedTypes->empty());

    TypeId tinyTable = arena.addType(
        TableType{TableType::Props{{"one", builtinTypes.numberType}, {"two", builtinTypes.stringType}}, std::nullopt, TypeLevel{}, TableState::Sealed}
    );

    generalize(tinyTable);

    CHECK(generalizedTypes->contains(tinyTable));
    CHECK(generalizedTypes->contains(builtinTypes.numberType));
    CHECK(generalizedTypes->contains(builtinTypes.stringType));
}

TEST_CASE_FIXTURE(GeneralizationFixture, "dont_cache_types_that_arent_done_yet")
{
    TypeId freeTy = arena.addType(FreeType{NotNull{globalScope.get()}, builtinTypes.neverType, builtinTypes.stringType});

    TypeId fnTy = arena.addType(FunctionType{builtinTypes.emptyTypePack, arena.addTypePack(TypePack{{builtinTypes.numberType}})});

    TypeId tableTy = arena.addType(
        TableType{TableType::Props{{"one", builtinTypes.numberType}, {"two", freeTy}, {"three", fnTy}}, std::nullopt, TypeLevel{}, TableState::Sealed}
    );

    generalize(tableTy);

    CHECK(generalizedTypes->contains(fnTy));
    CHECK(generalizedTypes->contains(builtinTypes.numberType));
    CHECK(generalizedTypes->contains(builtinTypes.neverType));
    CHECK(generalizedTypes->contains(builtinTypes.stringType));
    CHECK(!generalizedTypes->contains(freeTy));
    CHECK(!generalizedTypes->contains(tableTy));
}

TEST_CASE_FIXTURE(GeneralizationFixture, "functions_containing_cyclic_tables_can_be_cached")
{
    TypeId selfTy = arena.addType(BlockedType{});

    TypeId methodTy = arena.addType(
        FunctionType{
            arena.addTypePack({selfTy}),
            arena.addTypePack({builtinTypes.numberType}),
        }
    );

    asMutable(selfTy)->ty.emplace<TableType>(
        TableType::Props{{"count", builtinTypes.numberType}, {"method", methodTy}}, std::nullopt, TypeLevel{}, TableState::Sealed
    );

    generalize(methodTy);

    CHECK(generalizedTypes->contains(methodTy));
    CHECK(generalizedTypes->contains(selfTy));
    CHECK(generalizedTypes->contains(builtinTypes.numberType));
}

TEST_CASE_FIXTURE(GeneralizationFixture, "union_type_traversal_doesnt_crash")
{
    // t1 where t1 = ('h <: (t1 <: 'i)) | ('j <: (t1 <: 'i))
    TypeId i = arena.freshType(NotNull{&builtinTypes}, globalScope.get());
    TypeId h = arena.freshType(NotNull{&builtinTypes}, globalScope.get());
    TypeId j = arena.freshType(NotNull{&builtinTypes}, globalScope.get());
    TypeId unionType = arena.addType(UnionType{{h, j}});
    getMutable<FreeType>(h)->upperBound = i;
    getMutable<FreeType>(h)->lowerBound = builtinTypes.neverType;
    getMutable<FreeType>(i)->upperBound = builtinTypes.unknownType;
    getMutable<FreeType>(i)->lowerBound = unionType;
    getMutable<FreeType>(j)->upperBound = i;
    getMutable<FreeType>(j)->lowerBound = builtinTypes.neverType;

    generalize(unionType);
}

TEST_CASE_FIXTURE(GeneralizationFixture, "intersection_type_traversal_doesnt_crash")
{
    // t1 where t1 = ('h <: (t1 <: 'i)) & ('j <: (t1 <: 'i))
    TypeId i = arena.freshType(NotNull{&builtinTypes}, globalScope.get());
    TypeId h = arena.freshType(NotNull{&builtinTypes}, globalScope.get());
    TypeId j = arena.freshType(NotNull{&builtinTypes}, globalScope.get());
    TypeId intersectionType = arena.addType(IntersectionType{{h, j}});

    getMutable<FreeType>(h)->upperBound = i;
    getMutable<FreeType>(h)->lowerBound = builtinTypes.neverType;
    getMutable<FreeType>(i)->upperBound = builtinTypes.unknownType;
    getMutable<FreeType>(i)->lowerBound = intersectionType;
    getMutable<FreeType>(j)->upperBound = i;
    getMutable<FreeType>(j)->lowerBound = builtinTypes.neverType;

    generalize(intersectionType);
}

TEST_CASE_FIXTURE(GeneralizationFixture, "('a) -> 'a")
{
    TypeId freeTy = freshType().first;
    TypeId fnTy = arena.addType(FunctionType{arena.addTypePack({freeTy}), arena.addTypePack({freeTy})});

    generalize(fnTy);

    CHECK("<a>(a) -> a" == toString(fnTy));
}

TEST_CASE_FIXTURE(GeneralizationFixture, "(t1, (t1 <: 'b)) -> () where t1 = ('a <: (t1 <: 'b) & {number} & {number})")
{
    TableType tt;
    tt.indexer = TableIndexer{builtinTypes.numberType, builtinTypes.numberType};
    TypeId numberArray = arena.addType(TableType{tt});

    auto [aTy, aFree] = freshType();
    auto [bTy, bFree] = freshType();

    aFree->upperBound = arena.addType(IntersectionType{{bTy, numberArray, numberArray}});
    bFree->lowerBound = aTy;

    TypeId functionTy = arena.addType(FunctionType{arena.addTypePack({aTy, bTy}), builtinTypes.emptyTypePack});

    generalize(functionTy);

    CHECK("(unknown & {number}, unknown) -> ()" == toString(functionTy));
}

TEST_CASE_FIXTURE(GeneralizationFixture, "(('a <: number | string)) -> string?")
{
    auto [aTy, aFree] = freshType();

    aFree->upperBound = arena.addType(UnionType{{builtinTypes.numberType, builtinTypes.stringType}});

    TypeId fnType = arena.addType(FunctionType{arena.addTypePack({aTy}), arena.addTypePack({builtinTypes.optionalStringType})});

    generalize(fnType);

    CHECK("(number | string) -> string?" == toString(fnType));
}

TEST_CASE_FIXTURE(GeneralizationFixture, "(('a <: {'b})) -> ()")
{
    auto [aTy, aFree] = freshType();
    auto [bTy, bFree] = freshType();

    TableType tt;
    tt.indexer = TableIndexer{builtinTypes.numberType, bTy};

    aFree->upperBound = arena.addType(tt);

    TypeId functionTy = arena.addType(FunctionType{arena.addTypePack({aTy}), builtinTypes.emptyTypePack});

    generalize(functionTy);

    // The free type 'b is not replace with unknown because it appears in an
    // invariant context.
    CHECK("<a>({a}) -> ()" == toString(functionTy));
}

TEST_CASE_FIXTURE(GeneralizationFixture, "(('b <: {t1}), ('a <: t1)) -> t1 where t1 = (('a <: t1) <: 'c)")
{
    auto [aTy, aFree] = freshType();
    auto [bTy, bFree] = freshType();
    auto [cTy, cFree] = freshType();

    aFree->upperBound = cTy;
    cFree->lowerBound = aTy;

    TableType tt;
    tt.indexer = TableIndexer{builtinTypes.numberType, cTy};

    bFree->upperBound = arena.addType(tt);

    TypeId functionTy = arena.addType(FunctionType{arena.addTypePack({bTy, aTy}), arena.addTypePack({cTy})});

    generalize(functionTy);

    CHECK("<a>({a}, a) -> a" == toString(functionTy));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generalization_traversal_should_re_traverse_unions_if_they_change_type")
{
    // This test case should just not assert
    CheckResult result = check(R"(
function byId(p)
 return p.id
end

function foo()

 local productButtonPairs = {}
 local func = byId
 local dir = -1

 local function updateSearch()
  for product, button in pairs(productButtonPairs) do
   button.LayoutOrder = func(product) * dir
  end
 end

  function(mode)
   if mode == 'Name'then
   else
    if mode == 'New'then
     func = function(p)
      return p.id
     end
    elseif mode == 'Price'then
     func = function(p)
      return p.price
     end
    end

   end
  end
end
)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generalization_should_not_leak_free_type")
{
    ScopedFastFlag _{FFlag::DebugLuauForbidInternalTypes, true};

    // This test case should just not assert
    CheckResult result = check(R"(
        function foo()

            local productButtonPairs = {}
            local func
            local dir = -1

            local function updateSearch()
                for product, button in pairs(productButtonPairs) do
                    -- This line may have a floating free type pack.
                    button.LayoutOrder = func(product) * dir
                end
            end

            function(mode)
                if mode == 'New'then
                    func = function(p)
                        return p.id
                    end
                elseif mode == 'Price'then
                    func = function(p)
                        return p.price
                    end
                end
            end
        end
    )");
}

TEST_CASE_FIXTURE(Fixture, "generics_dont_leak_into_callback")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local func: <T>(T, (T) -> ()) -> () = nil :: any
        func({}, function(obj)
            local _ = obj
        end)
    )"));

    // `unknown` is correct here
    // - The lambda given can be generalized to `(unknown) -> ()`
    // - We can substitute the `T` in `func` for either `{}` or `unknown` and
    //   still have a well typed program.
    // We *probably* can do a better job bidirectionally inferring the types.
    CHECK_EQ("unknown", toString(requireTypeAtPosition(Position{3, 23})));
}

TEST_CASE_FIXTURE(Fixture, "generics_dont_leak_into_callback_2")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true}, {FFlag::LuauSubtypingReportGenericBoundMismatches2, true}, {FFlag::LuauSubtypingGenericsDoesntUseVariance, true}
    };

    CheckResult result = check(R"(
local func: <T>(T, (T) -> ()) -> () = nil :: any
local foobar: (number) -> () = nil :: any
func({}, function(obj)
    foobar(obj)
end)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const GenericBoundsMismatch* gbm = get<GenericBoundsMismatch>(result.errors[0]);
    REQUIRE_MESSAGE(gbm, "Expected GenericBoundsMismatch but got: " << toString(result.errors[0]));
    CHECK_EQ(gbm->genericName, "T");
    CHECK_EQ(gbm->lowerBounds.size(), 1);
    CHECK_EQ(toString(gbm->lowerBounds[0]), "{  }");
    CHECK_EQ(gbm->upperBounds.size(), 1);
    CHECK_EQ(toString(gbm->upperBounds[0]), "number");
    CHECK_EQ(result.errors[0].location, Location{Position{3, 0}, Position{3, 4}});
}

TEST_CASE_FIXTURE(Fixture, "generic_argument_with_singleton_oss_1808")
{
    // All we care about here is that this has no errors, and we correctly
    // infer that the `false` literal should be typed as `false`.
    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local function test<T>(value: false | (T) -> T)
            return value
        end
        test(false)
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "avoid_cross_module_mutation_in_bidirectional_inference")
{
    fileResolver.source["Module/ListFns"] = R"(
        local mod = {}
        function mod.findWhere(list, predicate): number?
            for i = 1, #list do
                if predicate(list[i], i) then
                    return i
                end
            end
            return nil
        end
        return mod
    )";

    fileResolver.source["Module/B"] = R"(
        local funs = require(script.Parent.ListFns)
        local accessories = funs.findWhere(getList(), function(accessory)
            return accessory.AccessoryType ~= accessoryTypeEnum
        end)
        return {}
    )";

    CheckResult result = getFrontend().check("Module/ListFns");
    auto modListFns = getFrontend().moduleResolver.getModule("Module/ListFns");
    freeze(modListFns->interfaceTypes);
    freeze(modListFns->internalTypes);
    LUAU_REQUIRE_NO_ERRORS(result);
    CheckResult result2 = getFrontend().check("Module/B");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
