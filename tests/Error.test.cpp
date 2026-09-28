// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Error.h"

#include "Fixture.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauForceOldSolver)

TEST_SUITE_BEGIN("ErrorTests");

TEST_CASE("TypeError_code_should_return_nonzero_code")
{
    auto e = TypeError{{{0, 0}, {0, 1}}, UnknownSymbol{"Foo"}};
    CHECK_GE(e.code(), 1000);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_bounds_mismatch_owns_name")
{
    std::string expectedName;
    SUBCASE("short name")
    {
        expectedName = "PhaseT";
    }
    SUBCASE("heap allocated name")
    {
        expectedName = "GenericParameterNameLongEnoughToRequireAllocatedStorage";
    }

    TypeError error;
    {
        std::string originalName = expectedName;
        error = TypeError{Location{}, GenericBoundsMismatch{originalName, {getBuiltins()->numberType}, {getBuiltins()->stringType}}};

        // Modify the still-live source buffer so a borrowed name fails deterministically,
        // without reading dangling memory in the unpatched implementation.
        for (char& character : originalName)
            character = 'x';

        const auto* mismatch = get<GenericBoundsMismatch>(error);
        REQUIRE(mismatch);
        REQUIRE_EQ(expectedName, mismatch->genericName);
    }

    CHECK_EQ(
        "No valid instantiation could be inferred for generic type parameter " + expectedName +
            ". It was expected to be at least:\n\tnumber\nand at most:\n\tstring\nbut these types are not compatible with one another.",
        toString(error)
    );
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_bounds_mismatch_name_survives_type_graph_cleanup")
{
    DOES_NOT_PASS_OLD_SOLVER_GUARD();
    getFrontend().options.retainFullTypeGraphs = false;

    CheckResult result = check(R"(
        local function combine<PhaseT>(a: { PhaseT }, b: { PhaseT }): { read PhaseT }
            return {}
        end

        local x: { number }
        local y: { boolean }
        local z = combine(x, y)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const auto* mismatch = get<GenericBoundsMismatch>(result.errors[0]);
    REQUIRE(mismatch);
    CHECK_EQ("PhaseT", mismatch->genericName);
    CHECK_EQ(0, toString(result.errors[0]).find("No valid instantiation could be inferred for generic type parameter PhaseT."));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable_names_show_instead_of_tables")
{
    DOES_NOT_PASS_WITH_EXACT_TABLES();

    getFrontend().options.retainFullTypeGraphs = false;

    CheckResult result = check(R"(
--!strict
local Account = {}
Account.__index = Account
function Account.deposit(self: Account, x: number)
	self.balance += x
end
type Account = typeof(setmetatable({} :: { balance: number }, Account))
local x: Account = 5
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Expected this to be 'Account', but got 'number'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "binary_op_type_function_errors")
{
    getFrontend().options.retainFullTypeGraphs = false;

    CheckResult result = check(R"(
        --!strict
        local x = 1 + "foo"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (!FFlag::DebugLuauForceOldSolver)
        CHECK_EQ(
            "Operator '+' could not be applied to operands of types number and string; there is no corresponding overload for __add",
            toString(result.errors[0])
        );
    else
        CHECK_EQ("Expected this to be 'number', but got 'string'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "unary_op_type_function_errors")
{
    getFrontend().options.retainFullTypeGraphs = false;

    CheckResult result = check(R"(
        --!strict
        local x = -"foo"
    )");


    if (!FFlag::DebugLuauForceOldSolver)
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        CHECK_EQ(
            "Operator '-' could not be applied to operand of type string; there is no corresponding overload for __unm", toString(result.errors[0])
        );

        CHECK_EQ("Expected this to be 'number', but got 'string'", toString(result.errors[1]));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK_EQ("Expected this to be 'number', but got 'string'", toString(result.errors[0]));
    }
}

TEST_SUITE_END();
