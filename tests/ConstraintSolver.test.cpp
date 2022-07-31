// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "doctest.h"

#include "lluz/ConstraintGraphBuilder.h"
#include "lluz/ConstraintSolver.h"

using namespace lluz;

static TypeId requireBinding(NotNull<Scope2> scope, const char* name)
{
    auto b = linearSearchForBinding(scope, name);
    lluz_ASSERT(b.has_value());
    return *b;
}

TEST_SUITE_BEGIN(XorStr("ConstraintSolver"));

TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "hello")
{
    AstStatBlock* block = parse(R"(
        local a = 55
        local b = a
    )");

    cgb.visit(block);
    NotNull<Scope2> rootScope = NotNull(cgb.rootScope);

    ConstraintSolver cs{&arena, rootScope};

    cs.run();

    TypeId bType = requireBinding(rootScope, XorStr("b"));

    CHECK("number" == toString(bType));
}

TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "generic_function")
{
    AstStatBlock* block = parse(R"(
        local function id(a)
            return a
        end
    )");

    cgb.visit(block);
    NotNull<Scope2> rootScope = NotNull(cgb.rootScope);

    ConstraintSolver cs{&arena, rootScope};

    cs.run();

    TypeId idType = requireBinding(rootScope, XorStr("id"));

    CHECK("<a>(a) -> a" == toString(idType));
}

#if 1
TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "proper_let_generalization")
{
    AstStatBlock* block = parse(R"(
        local function a(c)
            local function d(e)
                return c
            end

            return d
        end

        local b = a(5)
    )");

    cgb.visit(block);
    NotNull<Scope2> rootScope = NotNull(cgb.rootScope);

    ToStringOptions opts;

    ConstraintSolver cs{&arena, rootScope};

    cs.run();

    TypeId idType = requireBinding(rootScope, XorStr("b"));

    CHECK("<a>(a) -> number" == toString(idType, opts));
}
#endif

TEST_SUITE_END();
