// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/TypeArena.h"
#include "Luau/Unifier2.h"
#include "Luau/Error.h"

#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

struct Unifier2Fixture
{
    TypeArena arena;
    BuiltinTypes builtinTypes;
    Scope scope{builtinTypes.anyTypePack};
    InternalErrorReporter iceReporter;
    Unifier2 u2{NotNull{&arena}, NotNull{&builtinTypes}, NotNull{&scope}, NotNull{&iceReporter}};
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
};

TEST_SUITE_BEGIN("Unifier2");

TEST_CASE_FIXTURE(Unifier2Fixture, "T <: number")
{
    auto [left, freeLeft] = freshType();

    CHECK(u2.unify(left, builtinTypes.numberType));

    CHECK("never" == toString(freeLeft->lowerBound));
    CHECK("number" == toString(freeLeft->upperBound));
}

TEST_CASE_FIXTURE(Unifier2Fixture, "number <: T")
{
    auto [right, freeRight] = freshType();

    CHECK(u2.unify(builtinTypes.numberType, right));

    CHECK("number" == toString(freeRight->lowerBound));
    CHECK("unknown" == toString(freeRight->upperBound));
}

TEST_CASE_FIXTURE(Unifier2Fixture, "T <: U")
{
    auto [left, freeLeft] = freshType();
    auto [right, freeRight] = freshType();

    CHECK(u2.unify(left, right));

    CHECK("t1 where t1 = ('a <: (t1 <: 'b))" == toString(left));
    CHECK("t1 where t1 = (('a <: t1) <: 'b)" == toString(right));

    CHECK("never" == toString(freeLeft->lowerBound));
    CHECK("t1 where t1 = (('a <: t1) <: 'b)" == toString(freeLeft->upperBound));

    CHECK("t1 where t1 = ('a <: (t1 <: 'b))" == toString(freeRight->lowerBound));
    CHECK("unknown" == toString(freeRight->upperBound));
}

TEST_CASE_FIXTURE(Unifier2Fixture, "(string) -> () <: (X) -> Y...")
{
    TypeId stringToUnit = arena.addType(FunctionType{arena.addTypePack({builtinTypes.stringType}), arena.addTypePack({})});

    auto [x, xFree] = freshType();
    TypePackId y = arena.freshTypePack(&scope);

    TypeId xToY = arena.addType(FunctionType{arena.addTypePack({x}), y});

    u2.unify(stringToUnit, xToY);

    CHECK("string" == toString(xFree->upperBound));

    const TypePack* yPack = get<TypePack>(follow(y));
    REQUIRE(yPack != nullptr);

    CHECK(0 == yPack->head.size());
    CHECK(!yPack->tail);
}

TEST_CASE_FIXTURE(Unifier2Fixture, "unify_binds_free_subtype_tail_pack")
{
    TypePackId numberPack = arena.addTypePack({builtinTypes.numberType});

    TypePackId freeTail = arena.freshTypePack(&scope);
    TypeId freeHead = arena.addType(FreeType{&scope, builtinTypes.neverType, builtinTypes.unknownType});
    TypePackId freeAndFree = arena.addTypePack({freeHead}, freeTail);

    u2.unify(freeAndFree, numberPack);

    CHECK("('a <: number)" == toString(freeAndFree));
}

TEST_CASE_FIXTURE(Unifier2Fixture, "unify_binds_free_supertype_tail_pack")
{
    TypePackId numberPack = arena.addTypePack({builtinTypes.numberType});

    TypePackId freeTail = arena.freshTypePack(&scope);
    TypeId freeHead = arena.addType(FreeType{&scope, builtinTypes.neverType, builtinTypes.unknownType});
    TypePackId freeAndFree = arena.addTypePack({freeHead}, freeTail);

    u2.unify(numberPack, freeAndFree);

    CHECK("(number <: 'a)" == toString(freeAndFree));
}

TEST_CASE_FIXTURE(Unifier2Fixture, "generalize_a_type_that_is_bounded_by_another_generalizable_type")
{
    auto [t1, ft1] = freshType();
    auto [t2, ft2] = freshType();

    // t2 <: t1 <: unknown
    // unknown <: t2 <: t1

    ft1->lowerBound = t2;
    ft2->upperBound = t1;
    ft2->lowerBound = builtinTypes.unknownType;

    auto t2generalized = u2.generalize(t2);
    REQUIRE(t2generalized);

    CHECK(follow(t1) == follow(t2));

    auto t1generalized = u2.generalize(t1);
    REQUIRE(t1generalized);

    CHECK(builtinTypes.unknownType == follow(t1));
    CHECK(builtinTypes.unknownType == follow(t2));
}

// Same as generalize_a_type_that_is_bounded_by_another_generalizable_type
// except that we generalize the types in the opposite order
TEST_CASE_FIXTURE(Unifier2Fixture, "generalize_a_type_that_is_bounded_by_another_generalizable_type_in_reverse_order")
{
    auto [t1, ft1] = freshType();
    auto [t2, ft2] = freshType();

    // t2 <: t1 <: unknown
    // unknown <: t2 <: t1

    ft1->lowerBound = t2;
    ft2->upperBound = t1;
    ft2->lowerBound = builtinTypes.unknownType;

    auto t1generalized = u2.generalize(t1);
    REQUIRE(t1generalized);

    CHECK(follow(t1) == follow(t2));

    auto t2generalized = u2.generalize(t2);
    REQUIRE(t2generalized);

    CHECK(builtinTypes.unknownType == follow(t1));
    CHECK(builtinTypes.unknownType == follow(t2));
}

TEST_SUITE_END();
