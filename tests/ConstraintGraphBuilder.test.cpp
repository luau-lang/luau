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
    std::vector<const Constraint*> constraints = collectConstraints(cgb.rootScope);

    REQUIRE(2 == constraints.size());

    ToStringOptions opts;
    CHECK("a <: string" == toString(*constraints[0], opts));
    CHECK("b <: a" == toString(*constraints[1], opts));
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
    std::vector<const Constraint*> constraints = collectConstraints(cgb.rootScope);

    REQUIRE(4 == constraints.size());

    ToStringOptions opts;
    CHECK("a <: string" == toString(*constraints[0], opts));
    CHECK("b <: number" == toString(*constraints[1], opts));
    CHECK("c <: boolean" == toString(*constraints[2], opts));
    CHECK("d <: nil" == toString(*constraints[3], opts));
}

TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "function_application")
{
    AstStatBlock* block = parse(R"(
        local a = "hello"
        local b = a("world")
    )");

    cgb.visit(block);
    std::vector<const Constraint*> constraints = collectConstraints(cgb.rootScope);

    REQUIRE(4 == constraints.size());

    ToStringOptions opts;
    CHECK("a <: string" == toString(*constraints[0], opts));
    CHECK("b ~ inst a" == toString(*constraints[1], opts));
    CHECK("(string) -> (c, d...) <: b" == toString(*constraints[2], opts));
    CHECK("e <: c" == toString(*constraints[3], opts));
}

TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "local_function_definition")
{
    AstStatBlock* block = parse(R"(
        local function f(a)
            return a
        end
    )");

    cgb.visit(block);
    std::vector<const Constraint*> constraints = collectConstraints(cgb.rootScope);

    REQUIRE(2 == constraints.size());

    ToStringOptions opts;
    CHECK("a ~ gen (b) -> (c...)" == toString(*constraints[0], opts));
    CHECK("b <: c..." == toString(*constraints[1], opts));
}

TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "recursive_function")
{
    AstStatBlock* block = parse(R"(
        local function f(a)
            return f(a)
        end
    )");

    cgb.visit(block);
    std::vector<const Constraint*> constraints = collectConstraints(cgb.rootScope);

    REQUIRE(4 == constraints.size());

    ToStringOptions opts;
    CHECK("a ~ gen (b) -> (c...)" == toString(*constraints[0], opts));
    CHECK("d ~ inst a" == toString(*constraints[1], opts));
    CHECK("(b) -> (e, f...) <: d" == toString(*constraints[2], opts));
    CHECK("e <: c..." == toString(*constraints[3], opts));
}

TEST_SUITE_END();
