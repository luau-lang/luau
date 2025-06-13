// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/RecursionCounter.h"

#include "Luau/Type.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTINT(LuauVisitRecursionLimit);
LUAU_FASTFLAG(LuauSolverV2)

TEST_SUITE_BEGIN("VisitType");

TEST_CASE_FIXTURE(Fixture, "throw_when_limit_is_exceeded")
{
    if (FFlag::LuauSolverV2)
    {
        CheckResult result = check(R"(
            local t : {a: {b: {c: {d: {e: boolean}}}}}
        )");
        ScopedFastInt sfi{FInt::LuauVisitRecursionLimit, 3};
        TypeId tType = requireType("t");

        CHECK_THROWS_AS(toString(tType), RecursionLimitException);
    }
    else
    {
        ScopedFastInt sfi{FInt::LuauVisitRecursionLimit, 3};

        CheckResult result = check(R"(
            local t : {a: {b: {c: {d: {e: boolean}}}}}
        )");

        TypeId tType = requireType("t");

        CHECK_THROWS_AS(toString(tType), RecursionLimitException);
    }
}

TEST_CASE_FIXTURE(Fixture, "dont_throw_when_limit_is_high_enough")
{
    ScopedFastInt sfi{FInt::LuauVisitRecursionLimit, 8};

    CheckResult result = check(R"(
        local t : {a: {b: {c: {d: {e: boolean}}}}}
    )");

    TypeId tType = requireType("t");

    (void)toString(tType);
}

TEST_CASE_FIXTURE(Fixture, "some_free_types_do_not_have_bounds")
{
    Type t{FreeType{TypeLevel{}, getBuiltins()->neverType, getBuiltins()->unknownType}};

    (void)toString(&t);
}

TEST_CASE_FIXTURE(Fixture, "some_free_types_have_bounds")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    Scope scope{getBuiltins()->anyTypePack};
    Type t{FreeType{&scope, getBuiltins()->neverType, getBuiltins()->numberType}};

    CHECK("('a <: number)" == toString(&t));
}

TEST_SUITE_END();
