// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "lluz/RecursionCounter.h"

#include "doctest.h"

using namespace lluz;

lluz_FASTINT(LluVisitRecursionLimit)

TEST_SUITE_BEGIN(XorStr("VisitTypeVar"));

TEST_CASE_FIXTURE(Fixture, "throw_when_limit_is_exceeded")
{
    ScopedFastInt sfi{"lluzVisitRecursionLimit", 3};

    CheckResult result = check(R"(
        local t : {a: {b: {c: {d: {e: boolean}}}}}
    )");

    TypeId tType = requireType(XorStr("t"));

    CHECK_THROWS_AS(toString(tType), RecursionLimitException);
}

TEST_CASE_FIXTURE(Fixture, "dont_throw_when_limit_is_high_enough")
{
    ScopedFastInt sfi{"lluzVisitRecursionLimit", 8};

    CheckResult result = check(R"(
        local t : {a: {b: {c: {d: {e: boolean}}}}}
    )");

    TypeId tType = requireType(XorStr("t"));

    (void)toString(tType);
}

TEST_SUITE_END();
