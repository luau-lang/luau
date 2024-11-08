// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/FragmentAutocomplete.h"
#include "Fixture.h"
#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Autocomplete.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/Frontend.h"
#include "Luau/AutocompleteTypes.h"


using namespace Luau;

LUAU_FASTFLAG(LuauAllowFragmentParsing);
LUAU_FASTFLAG(LuauStoreDFGOnModule2);
LUAU_FASTFLAG(LuauAutocompleteRefactorsForIncrementalAutocomplete)

static std::optional<AutocompleteEntryMap> nullCallback(std::string tag, std::optional<const ClassType*> ptr, std::optional<std::string> contents)
{
    return std::nullopt;
}
struct FragmentAutocompleteFixture : Fixture
{
    ScopedFastFlag sffs[4] = {
        {FFlag::LuauAllowFragmentParsing, true},
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauStoreDFGOnModule2, true},
        {FFlag::LuauAutocompleteRefactorsForIncrementalAutocomplete, true}
    };

    FragmentAutocompleteFixture()
    {
        addGlobalBinding(frontend.globals, "table", Binding{builtinTypes->anyType});
        addGlobalBinding(frontend.globals, "math", Binding{builtinTypes->anyType});
    }
    FragmentAutocompleteAncestryResult runAutocompleteVisitor(const std::string& source, const Position& cursorPos)
    {
        ParseResult p = tryParse(source); // We don't care about parsing incomplete asts
        REQUIRE(p.root);
        return findAncestryForFragmentParse(p.root, cursorPos);
    }

    CheckResult checkBase(const std::string& document)
    {
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

    FragmentAutocompleteResult autocompleteFragment(const std::string& document, Position cursorPos)
    {
        FrontendOptions options;
        options.retainFullTypeGraphs = true;
        // Don't strictly need this in the new solver
        options.forAutocomplete = true;
        options.runLintChecks = false;
        return Luau::fragmentAutocomplete(frontend, document, "MainModule", cursorPos, options, nullCallback);
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

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "local_initializer")
{
    check("local a =");
    auto fragment = parseFragment("local a =", Position(0, 10));
    CHECK_EQ("local a =", fragment.fragmentToParse);
}

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

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_parse_in_correct_scope")
{

    check(R"(
        local myLocal = 4
        function abc()
             local myInnerLocal = 1

        end
  )");

    auto fragment = parseFragment(
        R"(
        local myLocal = 4
        function abc()
             local myInnerLocal = 1

        end
  )",
        Position{6, 0}
    );



    CHECK_EQ("function abc()\n             local myInnerLocal = 1\n\n        end\n", fragment.fragmentToParse);
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

    auto opt = linearSearchForBinding(fragment.freshScope.get(), "z");
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

    auto correct = linearSearchForBinding(fragment.freshScope.get(), "z");
    REQUIRE(correct);
    CHECK_EQ("number", toString(*correct));
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("FragmentAutocompleteTests");

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_autocomplete_simple_property_access")
{
    auto res = check(
        R"(
local tbl = { abc = 1234}
)"
    );

    LUAU_REQUIRE_NO_ERRORS(res);

    auto fragment = autocompleteFragment(
        R"(
local tbl = { abc = 1234}
tbl. 
)",
        Position{2, 5}
    );

    LUAU_ASSERT(fragment.freshScope);

    CHECK_EQ(1, fragment.acResults.entryMap.size());
    CHECK(fragment.acResults.entryMap.count("abc"));
    CHECK_EQ(AutocompleteContext::Property, fragment.acResults.context);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_autocomplete_nested_property_access")
{
    auto res = check(
        R"(
local tbl = { abc = { def = 1234, egh = false } }
)"
    );

    LUAU_REQUIRE_NO_ERRORS(res);

    auto fragment = autocompleteFragment(
        R"(
local tbl = { abc = { def = 1234, egh = false } }
tbl.abc.
)",
        Position{2, 8}
    );

    LUAU_ASSERT(fragment.freshScope);

    CHECK_EQ(2, fragment.acResults.entryMap.size());
    CHECK(fragment.acResults.entryMap.count("def"));
    CHECK(fragment.acResults.entryMap.count("egh"));
    CHECK_EQ(fragment.acResults.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "inline_autocomplete_picks_the_right_scope")
{
    auto res = check(
        R"(
type Table = { a: number, b: number }
do
    type Table = { x: string, y: string }
end
)"
    );

    LUAU_REQUIRE_NO_ERRORS(res);

    auto fragment = autocompleteFragment(
        R"(
type Table = { a: number, b: number }
do
    type Table = { x: string, y: string }
    local a : T
end
)",
        Position{4, 15}
    );

    LUAU_ASSERT(fragment.freshScope);

    REQUIRE(fragment.acResults.entryMap.count("Table"));
    REQUIRE(fragment.acResults.entryMap["Table"].type);
    const TableType* tv = get<TableType>(follow(*fragment.acResults.entryMap["Table"].type));
    REQUIRE(tv);
    CHECK(tv->props.count("x"));
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "nested_recursive_function")
{
    auto res = check(R"(
function foo()
end
)");

    LUAU_REQUIRE_NO_ERRORS(res);

    auto fragment = autocompleteFragment(
        R"(
function foo()
end
)",
        Position{2, 0}
    );

    CHECK(fragment.acResults.entryMap.count("foo"));
    CHECK_EQ(AutocompleteContext::Statement, fragment.acResults.context);
}


// Start compatibility tests!

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "empty_program")
{
    check("");

    auto frag = autocompleteFragment(" ", Position{0, 1});
    auto ac = frag.acResults;
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "local_initializer")
{
    check("local a =");
    auto frag = autocompleteFragment("local a =", Position{0, 9});
    auto ac = frag.acResults;

    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
    CHECK_EQ(ac.context, AutocompleteContext::Expression);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "leave_numbers_alone")
{
    check("local a = 3.");

    auto frag = autocompleteFragment("local a = 3.", Position{0, 12});
    auto ac = frag.acResults;
    CHECK(ac.entryMap.empty());
    CHECK_EQ(ac.context, AutocompleteContext::Unknown);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "user_defined_globals")
{
    check("local myLocal = 4; ");

    auto frag = autocompleteFragment("local myLocal = 4; ", Position{0, 18});
    auto ac = frag.acResults;

    CHECK(ac.entryMap.count("myLocal"));
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "dont_suggest_local_before_its_definition")
{
    check(R"(
        local myLocal = 4
        function abc()
             local myInnerLocal = 1

        end
    )");

    // autocomplete after abc but before myInnerLocal
    auto fragment = autocompleteFragment(
        R"(
        local myLocal = 4
        function abc()
             local myInnerLocal = 1

        end
)",
        Position{3, 0}
    );
    auto ac = fragment.acResults;
    CHECK(ac.entryMap.count("myLocal"));
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "myInnerLocal");

    // autocomplete after my inner local
    fragment = autocompleteFragment(
        R"(
        local myLocal = 4
        function abc()
             local myInnerLocal = 1

        end
 )",
        Position{4, 0}
    );
    ac = fragment.acResults;
    CHECK(ac.entryMap.count("myLocal"));
    CHECK(ac.entryMap.count("myInnerLocal"));

    fragment = autocompleteFragment(
        R"(
        local myLocal = 4
        function abc()
             local myInnerLocal = 1

        end
  )",
        Position{6, 0}
    );

    ac = fragment.acResults;
    CHECK(ac.entryMap.count("myLocal"));
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "myInnerLocal");
}

TEST_SUITE_END();
