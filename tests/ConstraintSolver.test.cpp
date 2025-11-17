// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "ConstraintGeneratorFixture.h"
#include "Fixture.h"
#include "doctest.h"

LUAU_FASTFLAG(LuauSolverV2);
LUAU_FASTFLAG(LuauScopedSeenSetInLookupTableProp);

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

TEST_CASE_FIXTURE(ConstraintGeneratorFixture, "table_prop_access_diamond")
{
    ScopedFastFlag sff(FFlag::LuauScopedSeenSetInLookupTableProp, true);

    CheckResult result = check(R"(
        export type ItemDetails = { Id: number }

        export type AssetDetails = ItemDetails & {}
        export type BundleDetails = ItemDetails & {}

        export type CatalogPage = { AssetDetails | BundleDetails }

        local function isRestricted(item: number) end

        -- Clear all item tiles and create new ones for the items in the specified page
        local function displayPage(catalogPage: CatalogPage)
            for _, itemDetails in catalogPage do
                if isRestricted(itemDetails.Id) then
                    continue
                end
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
