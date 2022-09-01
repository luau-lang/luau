// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "doctest.h"

#include "Luau/ConstraintGraphBuilder.h"
#include "Luau/ConstraintSolver.h"

using namespace Luau;

static TypeId requireBinding(NotNull<Scope> scope, const char* name)
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
    NotNull<Scope> rootScope = NotNull(cgb.rootScope);

    NullModuleResolver resolver;
    ConstraintSolver cs{&arena, rootScope, "MainModule", NotNull(&resolver), {}};

    cs.run();

    TypeId bType = requireBinding(rootScope, "b");

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
    NotNull<Scope> rootScope = NotNull(cgb.rootScope);

    NullModuleResolver resolver;
    ConstraintSolver cs{&arena, rootScope, "MainModule", NotNull(&resolver), {}};

    cs.run();

    TypeId idType = requireBinding(rootScope, "id");

    CHECK("<a>(a) -> a" == toString(idType));
}

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
    NotNull<Scope> rootScope = NotNull(cgb.rootScope);

    ToStringOptions opts;

    NullModuleResolver resolver;
    ConstraintSolver cs{&arena, rootScope, "MainModule", NotNull(&resolver), {}};

    cs.run();

    TypeId idType = requireBinding(rootScope, "b");

    CHECK("<a>(a) -> number" == toString(idType, opts));
}

TEST_SUITE_END();
