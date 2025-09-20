// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/TypeArena.h"
#include "Luau/Unifier2.h"
#include "Luau/Error.h"

#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauTryToOptimizeSetTypeUnification)

struct Unifier2Fixture
{
    TypeArena arena;
    BuiltinTypes builtinTypes;
    Scope scope{builtinTypes.anyTypePack};
    InternalErrorReporter iceReporter;
    Unifier2 u2{NotNull{&arena}, NotNull{&builtinTypes}, NotNull{&scope}, NotNull{&iceReporter}};
    ToStringOptions opts;

    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

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

    CHECK(UnifyResult::Ok == u2.unify(left, builtinTypes.numberType));

    CHECK("never" == toString(freeLeft->lowerBound));
    CHECK("number" == toString(freeLeft->upperBound));
}

TEST_CASE_FIXTURE(Unifier2Fixture, "number <: T")
{
    auto [right, freeRight] = freshType();

    CHECK(UnifyResult::Ok == u2.unify(builtinTypes.numberType, right));

    CHECK("number" == toString(freeRight->lowerBound));
    CHECK("unknown" == toString(freeRight->upperBound));
}

TEST_CASE_FIXTURE(Unifier2Fixture, "T <: U")
{
    auto [left, freeLeft] = freshType();
    auto [right, freeRight] = freshType();

    CHECK(UnifyResult::Ok == u2.unify(left, right));

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

TEST_CASE_FIXTURE(Unifier2Fixture, "unify_avoid_free_type_intersection_in_ub_from_union")
{
    ScopedFastFlag _{FFlag::LuauTryToOptimizeSetTypeUnification, true};
    // 'a
    TypeId freeTy = arena.addType(FreeType{&scope, builtinTypes.neverType, builtinTypes.unknownType});
    // 'a & ~(false?)
    TypeId subTy = arena.addType(IntersectionType{{freeTy, builtinTypes.truthyType}});
    // number?
    TypeId superTy = arena.addType(UnionType{{builtinTypes.numberType, builtinTypes.nilType}});
    u2.unify(subTy, superTy);

    CHECK("('a <: number?)" == toString(freeTy));
}

TEST_CASE_FIXTURE(Unifier2Fixture, "unify_unfortunate_free_type_lb_from_intersection")
{
    ScopedFastFlag _{FFlag::LuauTryToOptimizeSetTypeUnification, true};
    // 'a
    TypeId freeTy = arena.addType(FreeType{&scope, builtinTypes.neverType, builtinTypes.unknownType});
    // 'a?
    TypeId superTy = arena.addType(UnionType{{freeTy, builtinTypes.nilType}});
    // string & ~"foo"
    TypeId subTy =
        arena.addType(IntersectionType{{builtinTypes.stringType, arena.addType(NegationType{arena.addType(SingletonType{StringSingleton{"foo"}})})}});
    u2.unify(subTy, superTy);

    // TODO CLI-168953: This is not correct. The lower bound should be
    // string & ~"foo", but we're making this tradeoff for now to avoid
    // the more common case where the upper bound becomes `never` (see
    // previous test case).
    CHECK("(string | ~\"foo\" <: 'a)" == toString(freeTy));
}

TEST_SUITE_END();
