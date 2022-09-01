// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"
#include "Luau/ConstraintGraphBuilder.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("ConstraintGraphBuilder");

TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "hello_world")
{
    AstStatBlock* block = parse(R"(
        local a = "hello"
        local b = a
    )");

    cgb.visit(block);
    auto constraints = collectConstraints(NotNull(cgb.rootScope));

    REQUIRE(2 == constraints.size());

    ToStringOptions opts;
    CHECK("string <: a" == toString(*constraints[0], opts));
    CHECK("a <: b" == toString(*constraints[1], opts));
}

TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "primitives")
{
    AstStatBlock* block = parse(R"(
        local s = "hello"
        local n = 555
        local b = true
        local n2 = nil
    )");

    cgb.visit(block);
    auto constraints = collectConstraints(NotNull(cgb.rootScope));

    REQUIRE(3 == constraints.size());

    ToStringOptions opts;
    CHECK("string <: a" == toString(*constraints[0], opts));
    CHECK("number <: b" == toString(*constraints[1], opts));
    CHECK("boolean <: c" == toString(*constraints[2], opts));
}

TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "nil_primitive")
{
    AstStatBlock* block = parse(R"(
        local function a() return nil end
        local b = a()
    )");

    cgb.visit(block);
    auto constraints = collectConstraints(NotNull(cgb.rootScope));

    ToStringOptions opts;
    REQUIRE(4 <= constraints.size());

    CHECK("*blocked-1* ~ gen () -> (a...)" == toString(*constraints[0], opts));
    CHECK("call *blocked-1* with { result = *blocked-tp-1* }" == toString(*constraints[1], opts));
    CHECK("*blocked-tp-1* <: b" == toString(*constraints[2], opts));
    CHECK("nil <: a..." == toString(*constraints[3], opts));
}

TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "function_application")
{
    AstStatBlock* block = parse(R"(
        local a = "hello"
        local b = a("world")
    )");

    cgb.visit(block);
    auto constraints = collectConstraints(NotNull(cgb.rootScope));

    REQUIRE(3 == constraints.size());

    ToStringOptions opts;
    CHECK("string <: a" == toString(*constraints[0], opts));
    CHECK("call a with { result = *blocked-tp-1* }" == toString(*constraints[1], opts));
    CHECK("*blocked-tp-1* <: b" == toString(*constraints[2], opts));
}

TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "local_function_definition")
{
    AstStatBlock* block = parse(R"(
        local function f(a)
            return a
        end
    )");

    cgb.visit(block);
    auto constraints = collectConstraints(NotNull(cgb.rootScope));

    REQUIRE(2 == constraints.size());

    ToStringOptions opts;
    CHECK("*blocked-1* ~ gen (a) -> (b...)" == toString(*constraints[0], opts));
    CHECK("a <: b..." == toString(*constraints[1], opts));
}

TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "recursive_function")
{
    AstStatBlock* block = parse(R"(
        local function f(a)
            return f(a)
        end
    )");

    cgb.visit(block);
    auto constraints = collectConstraints(NotNull(cgb.rootScope));

    REQUIRE(3 == constraints.size());

    ToStringOptions opts;
    CHECK("*blocked-1* ~ gen (a) -> (b...)" == toString(*constraints[0], opts));
    CHECK("call (a) -> (b...) with { result = *blocked-tp-1* }" == toString(*constraints[1], opts));
    CHECK("*blocked-tp-1* <: b..." == toString(*constraints[2], opts));
}

TEST_SUITE_END();
