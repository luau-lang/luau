// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "ConstraintGeneratorFixture.h"
#include "Fixture.h"
#include "doctest.h"

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);

using namespace Luau;

static TypeId requireBinding(Scope* scope, const char* name)
{
    auto b = linearSearchForBinding(scope, name);
    LUAU_ASSERT(b.has_value());
    return *b;
}

TEST_SUITE_BEGIN("ConstraintSolver");

TEST_CASE_FIXTURE(ConstraintGeneratorFixture, "constraint_basics")
{
    solve(R"(
        local a = 55
        local b = a
    )");

    TypeId bType = requireBinding(rootScope, "b");

    CHECK("number" == toString(bType));
}

TEST_CASE_FIXTURE(ConstraintGeneratorFixture, "generic_function")
{
    solve(R"(
        local function id(a)
            return a
        end
    )");

    TypeId idType = requireBinding(rootScope, "id");

    CHECK("<a>(a) -> a" == toString(idType));
}

TEST_CASE_FIXTURE(ConstraintGeneratorFixture, "proper_let_generalization")
{
    solve(R"(
        local function a(c)
            local function d(e)
                return c
            end

            return d
        end

        local b = a(5)
    )");

    TypeId idType = requireBinding(rootScope, "b");

    CHECK("(unknown) -> number" == toString(idType));
}

TEST_SUITE_END();
