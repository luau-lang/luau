// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/FragmentAutocomplete.h"
#include "Fixture.h"
#include "Luau/Ast.h"
#include "Luau/AstQuery.h"


using namespace Luau;

struct FragmentAutocompleteFixture : Fixture
{

    FragmentAutocompleteAncestryResult runAutocompleteVisitor(const std::string& source, const Position& cursorPos)
    {
        ParseResult p = tryParse(source); // We don't care about parsing incomplete asts
        REQUIRE(p.root);
        return findAncestryForFragmentParse(p.root, cursorPos);
    }
};

TEST_SUITE_BEGIN("FragmentAutocompleteTraversalTest");

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "just_two_locals")
{
    auto result = runAutocompleteVisitor(
        R"(
local x = 4
local y = 5
)",
        {2, 11}
    );

    CHECK_EQ(3, result.ancestry.size());
    CHECK_EQ(2, result.localStack.size());
    CHECK_EQ(result.localMap.size(), result.localStack.size());
    REQUIRE(result.nearestStatement);

    AstStatLocal* local = result.nearestStatement->as<AstStatLocal>();
    REQUIRE(local);
    CHECK(1 == local->vars.size);
    CHECK_EQ("y", std::string(local->vars.data[0]->name.value));
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "cursor_within_scope_tracks_locals_from_previous_scope")
{
    auto result = runAutocompleteVisitor(
        R"(
local x = 4
local y = 5
if x == 4 then
    local e = y
end
)",
        {4, 15}
    );

    CHECK_EQ(5, result.ancestry.size());
    CHECK_EQ(3, result.localStack.size());
    CHECK_EQ(result.localMap.size(), result.localStack.size());
    REQUIRE(result.nearestStatement);
    CHECK_EQ("e", std::string(result.localStack.back()->name.value));

    AstStatLocal* local = result.nearestStatement->as<AstStatLocal>();
    REQUIRE(local);
    CHECK(1 == local->vars.size);
    CHECK_EQ("e", std::string(local->vars.data[0]->name.value));
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "cursor_that_comes_later_shouldnt_capture_locals_in_unavailable_scope")
{
    auto result = runAutocompleteVisitor(
        R"(
local x = 4
local y = 5
if x == 4 then
    local e = y
end
local z = x + x
if y == 5 then
    local q = x + y + z
end
)",
        {8, 23}
    );

    CHECK_EQ(6, result.ancestry.size());
    CHECK_EQ(4, result.localStack.size());
    CHECK_EQ(result.localMap.size(), result.localStack.size());
    REQUIRE(result.nearestStatement);
    CHECK_EQ("q", std::string(result.localStack.back()->name.value));

    AstStatLocal* local = result.nearestStatement->as<AstStatLocal>();
    REQUIRE(local);
    CHECK(1 == local->vars.size);
    CHECK_EQ("q", std::string(local->vars.data[0]->name.value));
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "nearest_enclosing_statement_can_be_non_local")
{
    auto result = runAutocompleteVisitor(
        R"(
local x = 4
local y = 5
if x == 4 then
)",
        {3, 4}
    );

    CHECK_EQ(4, result.ancestry.size());
    CHECK_EQ(2, result.localStack.size());
    CHECK_EQ(result.localMap.size(), result.localStack.size());
    REQUIRE(result.nearestStatement);
    CHECK_EQ("y", std::string(result.localStack.back()->name.value));

    AstStatIf* ifS = result.nearestStatement->as<AstStatIf>();
    CHECK(ifS != nullptr);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "local_funcs_show_up_in_local_stack")
{
    auto result = runAutocompleteVisitor(
        R"(
local function foo() return 4 end
local x = foo()
local function bar() return x + foo() end
)",
        {3, 32}
    );

    CHECK_EQ(8, result.ancestry.size());
    CHECK_EQ(3, result.localStack.size());
    CHECK_EQ(result.localMap.size(), result.localStack.size());
    CHECK_EQ("bar", std::string(result.localStack.back()->name.value));
    auto returnSt = result.nearestStatement->as<AstStatReturn>();
    CHECK(returnSt != nullptr);
}

TEST_SUITE_END();
