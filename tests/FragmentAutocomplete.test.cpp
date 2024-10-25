// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/FragmentAutocomplete.h"
#include "Fixture.h"
#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Common.h"
#include "Luau/Frontend.h"


using namespace Luau;

LUAU_FASTFLAG(LuauAllowFragmentParsing);
LUAU_FASTFLAG(LuauStoreDFGOnModule2);

struct FragmentAutocompleteFixture : Fixture
{
    ScopedFastFlag sffs[3] = {{FFlag::LuauAllowFragmentParsing, true}, {FFlag::LuauSolverV2, true}, {FFlag::LuauStoreDFGOnModule2, true}};

    FragmentAutocompleteAncestryResult runAutocompleteVisitor(const std::string& source, const Position& cursorPos)
    {
        ParseResult p = tryParse(source); // We don't care about parsing incomplete asts
        REQUIRE(p.root);
        return findAncestryForFragmentParse(p.root, cursorPos);
    }

    CheckResult checkBase(const std::string& document)
    {
        ScopedFastFlag sff{FFlag::LuauSolverV2, true};
        FrontendOptions opts;
        opts.retainFullTypeGraphs = true;
        return this->frontend.check("MainModule", opts);
    }

    FragmentParseResult parseFragment(const std::string& document, const Position& cursorPos)
    {
        SourceModule* srcModule = this->getMainSourceModule();
        std::string_view srcString = document;
        return Luau::parseFragment(*srcModule, srcString, cursorPos);
    }

    FragmentTypeCheckResult checkFragment(const std::string& document, const Position& cursorPos)
    {
        FrontendOptions options;
        options.retainFullTypeGraphs = true;
        // Don't strictly need this in the new solver
        options.forAutocomplete = true;
        options.runLintChecks = false;
        return Luau::typecheckFragment(frontend, "MainModule", cursorPos, options, document);
    }
};

TEST_SUITE_BEGIN("FragmentAutocompleteTraversalTests");

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
    CHECK_EQ(1, result.localStack.size());
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
    CHECK_EQ(2, result.localStack.size());
    CHECK_EQ(result.localMap.size(), result.localStack.size());
    REQUIRE(result.nearestStatement);
    CHECK_EQ("y", std::string(result.localStack.back()->name.value));

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
    CHECK_EQ(3, result.localStack.size());
    CHECK_EQ(result.localMap.size(), result.localStack.size());
    REQUIRE(result.nearestStatement);
    CHECK_EQ("z", std::string(result.localStack.back()->name.value));

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
    CHECK_EQ(2, result.localStack.size());
    CHECK_EQ(result.localMap.size(), result.localStack.size());
    CHECK_EQ("x", std::string(result.localStack.back()->name.value));
    auto returnSt = result.nearestStatement->as<AstStatReturn>();
    CHECK(returnSt != nullptr);
}

TEST_SUITE_END();


TEST_SUITE_BEGIN("FragmentAutocompleteParserTests");

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "statement_in_empty_fragment_is_non_null")
{
    auto res = check(R"(

)");

    LUAU_REQUIRE_NO_ERRORS(res);

    auto fragment = parseFragment(
        R"(

)",
        Position(1, 0)
    );
    CHECK_EQ("\n", fragment.fragmentToParse);
    CHECK_EQ(2, fragment.ancestry.size());
    REQUIRE(fragment.root);
    CHECK_EQ(0, fragment.root->body.size);
    auto statBody = fragment.root->as<AstStatBlock>();
    CHECK(statBody != nullptr);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_parse_complete_fragments")
{
    auto res = check(
        R"(
local x = 4
local y = 5
)"
    );

    LUAU_REQUIRE_NO_ERRORS(res);

    auto fragment = parseFragment(
        R"(
local x = 4
local y = 5
local z = x + y
)",
        Position{3, 15}
    );

    CHECK_EQ("\nlocal z = x + y", fragment.fragmentToParse);
    CHECK_EQ(5, fragment.ancestry.size());
    REQUIRE(fragment.root);
    CHECK_EQ(1, fragment.root->body.size);
    auto stat = fragment.root->body.data[0]->as<AstStatLocal>();
    REQUIRE(stat);
    CHECK_EQ(1, stat->vars.size);
    CHECK_EQ(1, stat->values.size);
    CHECK_EQ("z", std::string(stat->vars.data[0]->name.value));

    auto bin = stat->values.data[0]->as<AstExprBinary>();
    REQUIRE(bin);
    CHECK_EQ(AstExprBinary::Op::Add, bin->op);

    auto lhs = bin->left->as<AstExprLocal>();
    auto rhs = bin->right->as<AstExprLocal>();
    REQUIRE(lhs);
    REQUIRE(rhs);
    CHECK_EQ("x", std::string(lhs->local->name.value));
    CHECK_EQ("y", std::string(rhs->local->name.value));
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_parse_fragments_in_line")
{
    auto res = check(
        R"(
local x = 4
local y = 5
)"
    );

    LUAU_REQUIRE_NO_ERRORS(res);

    auto fragment = parseFragment(
        R"(
local x = 4
local z = x + y
local y = 5
)",
        Position{2, 15}
    );

    CHECK_EQ("local z = x + y", fragment.fragmentToParse);
    CHECK_EQ(5, fragment.ancestry.size());
    REQUIRE(fragment.root);
    CHECK_EQ(1, fragment.root->body.size);
    auto stat = fragment.root->body.data[0]->as<AstStatLocal>();
    REQUIRE(stat);
    CHECK_EQ(1, stat->vars.size);
    CHECK_EQ(1, stat->values.size);
    CHECK_EQ("z", std::string(stat->vars.data[0]->name.value));

    auto bin = stat->values.data[0]->as<AstExprBinary>();
    REQUIRE(bin);
    CHECK_EQ(AstExprBinary::Op::Add, bin->op);

    auto lhs = bin->left->as<AstExprLocal>();
    auto rhs = bin->right->as<AstExprGlobal>();
    REQUIRE(lhs);
    REQUIRE(rhs);
    CHECK_EQ("x", std::string(lhs->local->name.value));
    CHECK_EQ("y", std::string(rhs->name.value));
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("FragmentAutocompleteTypeCheckerTests");

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_typecheck_simple_fragment")
{
    auto res = check(
        R"(
local x = 4
local y = 5
)"
    );

    LUAU_REQUIRE_NO_ERRORS(res);

    auto fragment = checkFragment(
        R"(
local x = 4
local y = 5
local z = x + y
)",
        Position{3, 15}
    );

    auto opt = linearSearchForBinding(fragment.freshScope, "z");
    REQUIRE(opt);
    CHECK_EQ("number", toString(*opt));
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_typecheck_fragment_inserted_inline")
{
    auto res = check(
        R"(
local x = 4
local y = 5
)"
    );

    LUAU_REQUIRE_NO_ERRORS(res);
    auto fragment = checkFragment(
        R"(
local x = 4
local z = x
local y = 5
)",
        Position{2, 11}
    );

    auto correct = linearSearchForBinding(fragment.freshScope, "z");
    REQUIRE(correct);
    CHECK_EQ("number", toString(*correct));
}

TEST_SUITE_END();
