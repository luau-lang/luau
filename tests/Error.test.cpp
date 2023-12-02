// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Error.h"

#include "Fixture.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauStacklessTypeClone3);

TEST_SUITE_BEGIN("ErrorTests");

TEST_CASE("TypeError_code_should_return_nonzero_code")
{
    auto e = TypeError{{{0, 0}, {0, 1}}, UnknownSymbol{"Foo"}};
    CHECK_GE(e.code(), 1000);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable_names_show_instead_of_tables")
{
    frontend.options.retainFullTypeGraphs = false;
    ScopedFastFlag sff{FFlag::LuauStacklessTypeClone3, true};
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

TEST_SUITE_END();
