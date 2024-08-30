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

    TypeId cursedClass = arena.addType(ClassType{"Cursed", {{"oh_no", Property::readonly(propTy)}}, std::nullopt, std::nullopt, {}, {}, "", {}});

    auto genClass = generalize(cursedClass);
    REQUIRE(genClass);

    auto genPropTy = get<ClassType>(*genClass)->props.at("oh_no").readTy;
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

    TypeId methodTy = arena.addType(FunctionType{
        arena.addTypePack({selfTy}),
        arena.addTypePack({builtinTypes.numberType}),
    });

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
    TypeId i = arena.addType(FreeType{NotNull{globalScope.get()}});
    TypeId h = arena.addType(FreeType{NotNull{globalScope.get()}});
    TypeId j = arena.addType(FreeType{NotNull{globalScope.get()}});
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
    TypeId i = arena.addType(FreeType{NotNull{globalScope.get()}});
    TypeId h = arena.addType(FreeType{NotNull{globalScope.get()}});
    TypeId j = arena.addType(FreeType{NotNull{globalScope.get()}});
    TypeId intersectionType = arena.addType(IntersectionType{{h, j}});

    getMutable<FreeType>(h)->upperBound = i;
    getMutable<FreeType>(h)->lowerBound = builtinTypes.neverType;
    getMutable<FreeType>(i)->upperBound = builtinTypes.unknownType;
    getMutable<FreeType>(i)->lowerBound = intersectionType;
    getMutable<FreeType>(j)->upperBound = i;
    getMutable<FreeType>(j)->lowerBound = builtinTypes.neverType;

    generalize(intersectionType);
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

TEST_SUITE_END();
