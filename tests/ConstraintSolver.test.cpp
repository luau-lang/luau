// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"
#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("ConstraintSolver");

TEST_CASE_FIXTURE(Fixture, "constraint_basics")
{
    check(R"(
        local a = 55
        local b = a
    )");

    CHECK("number" == toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "generic_function")
{
    check(R"(
        local function id(a)
            return a
        end
    )");


    CHECK("<a>(a) -> a" == toString(requireType("id")));
}

TEST_CASE_FIXTURE(Fixture, "proper_let_generalization")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();

    check(R"(
        local function a(c)
            local function d(e)
                return c
            end

            return d
        end

        local b = a(5)
    )");

    CHECK("(unknown) -> number" == toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "table_prop_access_diamond")
{
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
