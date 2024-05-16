// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Generalization.h"
#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/Error.h"

#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

TEST_SUITE_BEGIN("Generalization");

struct GeneralizationFixture
{
    TypeArena arena;
    BuiltinTypes builtinTypes;
    Scope scope{builtinTypes.anyTypePack};
    ToStringOptions opts;

    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    std::pair<TypeId, FreeType*> freshType()
    {
        FreeType ft{&scope, builtinTypes.neverType, builtinTypes.unknownType};

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
        return ::Luau::generalize(NotNull{&arena}, NotNull{&builtinTypes}, NotNull{&scope}, ty);
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

    TypeId cursedClass = arena.addType(ClassType{"Cursed", {{"oh_no", Property::readonly(propTy)}}, std::nullopt, std::nullopt, {}, {}, ""});

    auto genClass = generalize(cursedClass);
    REQUIRE(genClass);

    auto genPropTy = get<ClassType>(*genClass)->props.at("oh_no").readTy;
    CHECK(is<FreeType>(*genPropTy));
}

TEST_SUITE_END();
