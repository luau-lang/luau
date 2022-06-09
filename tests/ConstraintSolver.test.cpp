// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "doctest.h"

#include "Luau/ConstraintGraphBuilder.h"
#include "Luau/ConstraintSolver.h"

using namespace Luau;

static TypeId requireBinding(Scope2* scope, const char* name)
{
    auto b = linearSearchForBinding(scope, name);
    LUAU_ASSERT(b.has_value());
    return *b;
}

TEST_SUITE_BEGIN("ConstraintSolver");

TEST_CASE_FIXTURE(ConstraintGraphBuilderFixture, "hello")
{
    AstStatBlock* block = parse(R"(
        local a = 55
        local b = a
    )");

    cgb.visit(block);

    ConstraintSolver cs{&arena, cgb.rootScope};

    cs.run();

    TypeId bType = requireBinding(cgb.rootScope, "b");

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

    ConstraintSolver cs{&arena, cgb.rootScope};

    cs.run();

    TypeId idType = requireBinding(cgb.rootScope, "id");

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

    ToStringOptions opts;

    ConstraintSolver cs{&arena, cgb.rootScope};

    cs.run();

    TypeId idType = requireBinding(cgb.rootScope, "b");

    CHECK("<a>(a) -> number" == toString(idType, opts));
}
#endif

TEST_SUITE_END();
