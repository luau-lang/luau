// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Error.h"

#include "Fixture.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)

TEST_SUITE_BEGIN("ErrorTests");

TEST_CASE("TypeError_code_should_return_nonzero_code")
{
    auto e = TypeError{{{0, 0}, {0, 1}}, UnknownSymbol{"Foo"}};
    CHECK_GE(e.code(), 1000);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable_names_show_instead_of_tables")
{
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

    CHECK_EQ("Type 'number' could not be converted into 'Account'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "binary_op_type_function_errors")
{
    getFrontend().options.retainFullTypeGraphs = false;

    CheckResult result = check(R"(
        --!strict
        local x = 1 + "foo"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::LuauSolverV2)
        CHECK_EQ(
            "Operator '+' could not be applied to operands of types number and string; there is no corresponding overload for __add",
            toString(result.errors[0])
        );
    else
        CHECK_EQ("Type 'string' could not be converted into 'number'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "unary_op_type_function_errors")
{
    getFrontend().options.retainFullTypeGraphs = false;

    CheckResult result = check(R"(
        --!strict
        local x = -"foo"
    )");


    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        CHECK_EQ(
            "Operator '-' could not be applied to operand of type string; there is no corresponding overload for __unm", toString(result.errors[0])
        );
        CHECK_EQ("Type 'string' could not be converted into 'number'", toString(result.errors[1]));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK_EQ("Type 'string' could not be converted into 'number'", toString(result.errors[0]));
    }
}

TEST_SUITE_END();
