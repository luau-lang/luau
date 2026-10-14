// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Scope.h"
#include "Luau/ToString.h"
#include "Luau/TypeArena.h"
#include "Luau/Unifier2.h"
#include "Luau/Error.h"

#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauForceOldSolver)
LUAU_FASTFLAG(LuauDecomposeIntersectionOfFreeType)

struct Unifier2Fixture
{
    TypeArena arena;
    BuiltinTypes builtinTypes;
    Scope scope{builtinTypes.anyTypePack};
    InternalErrorReporter iceReporter;
    Unifier2 u2{NotNull{&arena}, NotNull{&builtinTypes}, NotNull{&scope}, NotNull{&iceReporter}};
    ToStringOptions opts;

    ScopedFastFlag sff{FFlag::DebugLuauForceOldSolver, false};

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

TEST_CASE_FIXTURE(Unifier2Fixture, "unify_free_type_intersection_in_ub_from_union")
{
    ScopedFastFlag _{FFlag::LuauDecomposeIntersectionOfFreeType, true};
    // 'a
    TypeId freeTy = arena.addType(FreeType{&scope, builtinTypes.neverType, builtinTypes.unknownType});
    // 'a & ~(false?)
    TypeId subTy = arena.addType(IntersectionType{{freeTy, builtinTypes.truthyType}});
    // number?
    TypeId superTy = arena.addType(UnionType{{builtinTypes.numberType, builtinTypes.nilType}});
    u2.unify(subTy, superTy);

    CHECK("('a <: (false | number)?)" == toString(freeTy));
}

TEST_CASE_FIXTURE(Unifier2Fixture, "unify_free_type_lb_from_intersection")
{
    // 'a
    TypeId freeTy = arena.addType(FreeType{&scope, builtinTypes.neverType, builtinTypes.unknownType});
    // 'a?
    TypeId superTy = arena.addType(UnionType{{freeTy, builtinTypes.nilType}});
    // string & ~"foo"
    TypeId subTy =
        arena.addType(IntersectionType{{builtinTypes.stringType, arena.addType(NegationType{arena.addType(SingletonType{StringSingleton{"foo"}})})}});
    u2.unify(subTy, superTy);
    CHECK("(string & ~\"foo\" <: 'a)" == toString(freeTy));
}

TEST_CASE_FIXTURE(Unifier2Fixture, "unify_free_type_result_of_or_expr")
{
    ScopedFastFlag _{FFlag::LuauDecomposeIntersectionOfFreeType, true};
    // This test simulates the result of unifying something like:
    //
    //  local function foobar(x, y): string
    //      return x or y
    //  end
    //
    // ... which may result in the constraint ...
    //
    //  ('X & ~(false?)) | 'Y <: string
    //
    // ... and the final bounds ...
    //
    //  'X <: string | false | nil, 'Y <: string

    // 'X
    TypeId freeTyX = arena.addType(FreeType{&scope, builtinTypes.neverType, builtinTypes.unknownType});
    // 'Y
    TypeId freeTyY = arena.addType(FreeType{&scope, builtinTypes.neverType, builtinTypes.unknownType});

    // 'X & ~(false?)
    TypeId orLhs = arena.addType(IntersectionType{{freeTyX, builtinTypes.truthyType}});

    // ('X & ~(false)?) | 'Y <: string
    TypeId subTy = arena.addType(UnionType{{orLhs, freeTyY}});
    u2.unify(subTy, builtinTypes.stringType);
    CHECK("('a <: (false | string)?)" == toString(freeTyX));
    CHECK("('b <: string)" == toString(freeTyY));
}

TEST_CASE_FIXTURE(Unifier2Fixture, "unify_free_type_result_of_and_expr")
{
    ScopedFastFlag _{FFlag::LuauDecomposeIntersectionOfFreeType, true};
    // This test simulates the result of unifying something like:
    //
    //  local function foobar(x, y): string
    //      return x and y
    //  end
    //
    // ... which may result in the constraint ...
    //
    //  ('X & false?) | 'Y <: string
    //
    // ... and the final bounds ...
    //
    //  'X <: string | ~(false?), 'Y <: string

    // 'X
    TypeId freeTyX = arena.addType(FreeType{&scope, builtinTypes.neverType, builtinTypes.unknownType});
    // 'Y
    TypeId freeTyY = arena.addType(FreeType{&scope, builtinTypes.neverType, builtinTypes.unknownType});

    // 'X & false?
    TypeId andLhs = arena.addType(IntersectionType{{freeTyX, builtinTypes.falsyType}});

    // ('X & false?) | 'Y <: string
    TypeId subTy = arena.addType(UnionType{{andLhs, freeTyY}});
    u2.unify(subTy, builtinTypes.stringType);
    // Not an amazing type, this should really be `'a <: ~(false?)`
    CHECK("('a <: string | ~(false?))" == toString(freeTyX));
    CHECK("('b <: string)" == toString(freeTyY));
}

TEST_SUITE_END();
