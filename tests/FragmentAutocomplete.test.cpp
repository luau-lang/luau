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

#include <algorithm>
#include <chrono>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <optional>


using namespace Luau;

LUAU_FASTFLAG(LuauAllowFragmentParsing);
LUAU_FASTFLAG(LuauAutocompleteRefactorsForIncrementalAutocomplete)
LUAU_FASTFLAG(LuauSymbolEquality);
LUAU_FASTFLAG(LuauStoreSolverTypeOnModule);
LUAU_FASTFLAG(LexerResumesFromPosition)

static std::optional<AutocompleteEntryMap> nullCallback(std::string tag, std::optional<const ClassType*> ptr, std::optional<std::string> contents)
{
    return std::nullopt;
}

static FrontendOptions getOptions()
{
    FrontendOptions options;
    options.retainFullTypeGraphs = true;

    if (!FFlag::LuauSolverV2)
        options.forAutocomplete = true;

    options.runLintChecks = false;

    return options;
}

template<class BaseType>
struct FragmentAutocompleteFixtureImpl : BaseType
{
    ScopedFastFlag sffs[5] = {
        {FFlag::LuauAllowFragmentParsing, true},
        {FFlag::LuauAutocompleteRefactorsForIncrementalAutocomplete, true},
        {FFlag::LuauStoreSolverTypeOnModule, true},
        {FFlag::LuauSymbolEquality, true},
        {FFlag::LexerResumesFromPosition, true}
    };

    FragmentAutocompleteFixtureImpl()
        : BaseType(true)
    {
    }

    FragmentAutocompleteAncestryResult runAutocompleteVisitor(const std::string& source, const Position& cursorPos)
    {
        ParseResult p = this->tryParse(source); // We don't care about parsing incomplete asts
        REQUIRE(p.root);
        return findAncestryForFragmentParse(p.root, cursorPos);
    }


    FragmentParseResult parseFragment(
        const std::string& document,
        const Position& cursorPos,
        std::optional<Position> fragmentEndPosition = std::nullopt
    )
    {
        SourceModule* srcModule = this->getMainSourceModule();
        std::string_view srcString = document;
        return Luau::parseFragment(*srcModule, srcString, cursorPos, fragmentEndPosition);
    }

    CheckResult checkOldSolver(const std::string& source)
    {
        ScopedFastFlag sff{FFlag::LuauSolverV2, false};
        return this->check(Mode::Strict, source, getOptions());
    }

    FragmentTypeCheckResult checkFragment(
        const std::string& document,
        const Position& cursorPos,
        std::optional<Position> fragmentEndPosition = std::nullopt
    )
    {
        return Luau::typecheckFragment(this->frontend, "MainModule", cursorPos, getOptions(), document, fragmentEndPosition);
    }

    FragmentAutocompleteResult autocompleteFragment(
        const std::string& document,
        Position cursorPos,
        std::optional<Position> fragmentEndPosition = std::nullopt
    )
    {
        FrontendOptions options;
        return Luau::fragmentAutocomplete(this->frontend, document, "MainModule", cursorPos, getOptions(), nullCallback, fragmentEndPosition);
    }


    void autocompleteFragmentInBothSolvers(
        const std::string& document,
        const std::string& updated,
        Position cursorPos,
        std::function<void(FragmentAutocompleteResult& result)> assertions,
        std::optional<Position> fragmentEndPosition = std::nullopt
    )
    {
        ScopedFastFlag sff{FFlag::LuauSolverV2, true};
        this->check(document);

        FragmentAutocompleteResult result = autocompleteFragment(updated, cursorPos, fragmentEndPosition);
        assertions(result);

        ScopedFastFlag _{FFlag::LuauSolverV2, false};
        this->check(document, getOptions());

        result = autocompleteFragment(updated, cursorPos, fragmentEndPosition);
        assertions(result);
    }
};

struct FragmentAutocompleteFixture : FragmentAutocompleteFixtureImpl<Fixture>
{
    FragmentAutocompleteFixture()
        : FragmentAutocompleteFixtureImpl<Fixture>()
    {
        addGlobalBinding(frontend.globals, "table", Binding{builtinTypes->anyType});
        addGlobalBinding(frontend.globals, "math", Binding{builtinTypes->anyType});
        addGlobalBinding(frontend.globalsForAutocomplete, "table", Binding{builtinTypes->anyType});
        addGlobalBinding(frontend.globalsForAutocomplete, "math", Binding{builtinTypes->anyType});
    }
};

struct FragmentAutocompleteBuiltinsFixture : FragmentAutocompleteFixtureImpl<BuiltinsFixture>
{
    FragmentAutocompleteBuiltinsFixture()
        : FragmentAutocompleteFixtureImpl<BuiltinsFixture>()
    {
        const std::string fakeVecDecl = R"(
declare class FakeVec
    function dot(self, x: FakeVec) : FakeVec
    zero : FakeVec
end
)";
        // The old solver always performs a strict mode check and populates the module resolver and globals
        // for autocomplete.
        // The new solver just populates the globals and the moduleResolver.
        // Because these tests run in both the old solver and the new solver, and the test suite
        // now picks the module resolver as appropriate in order to better mimic the studio code path,
        // we have to load the definition file into both the 'globals'/'resolver' and the equivalent
        // 'for autocomplete'.
        loadDefinition(fakeVecDecl);
        loadDefinition(fakeVecDecl, /* For Autocomplete Module */ true);
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
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
    check("local a =");
    auto fragment = parseFragment("local a =", Position(0, 10));
    CHECK_EQ("local a =", fragment.fragmentToParse);
    CHECK_EQ(Location{Position{0, 0}, 9}, fragment.root->location);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "statement_in_empty_fragment_is_non_null")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
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
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
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

    CHECK_EQ(Location{Position{2, 0}, Position{3, 15}}, fragment.root->location);

    CHECK_EQ("local y = 5\nlocal z = x + y", fragment.fragmentToParse);
    CHECK_EQ(5, fragment.ancestry.size());
    REQUIRE(fragment.root);
    CHECK_EQ(2, fragment.root->body.size);
    auto stat = fragment.root->body.data[1]->as<AstStatLocal>();
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
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
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
    CHECK_EQ(Location{Position{2, 0}, Position{2, 15}}, fragment.root->location);
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
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
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

    CHECK_EQ("\n  ", fragment.fragmentToParse);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_parse_single_line_fragment_override")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
    auto res = check("function abc(foo: string) end");

    LUAU_REQUIRE_NO_ERRORS(res);

    auto callFragment = parseFragment(
        R"(function abc(foo: string) end
abc("foo")
abc("bar")
)",
        Position{1, 6},
        Position{1, 10}
    );

    CHECK_EQ("function abc(foo: string) end\nabc(\"foo\")", callFragment.fragmentToParse);
    CHECK(callFragment.nearestStatement->is<AstStatFunction>());

    CHECK_GE(callFragment.ancestry.size(), 2);

    AstNode* back = callFragment.ancestry.back();
    CHECK(back->is<AstExprConstantString>());
    CHECK_EQ(Position{1, 4}, back->location.begin);
    CHECK_EQ(Position{1, 9}, back->location.end);

    AstNode* parent = callFragment.ancestry.rbegin()[1];
    CHECK(parent->is<AstExprCall>());
    CHECK_EQ(Position{1, 0}, parent->location.begin);
    CHECK_EQ(Position{1, 10}, parent->location.end);


    auto stringFragment = parseFragment(
        R"(function abc(foo: string) end
abc("foo")
abc("bar")
)",
        Position{1, 6},
        Position{1, 9}
    );

    CHECK_EQ("function abc(foo: string) end\nabc(\"foo\")", stringFragment.fragmentToParse);
    CHECK(stringFragment.nearestStatement->is<AstStatFunction>());

    CHECK_GE(stringFragment.ancestry.size(), 1);

    back = stringFragment.ancestry.back();

    auto asString = back->as<AstExprConstantString>();
    CHECK(asString);

    CHECK_EQ(Position{1, 4}, asString->location.begin);
    CHECK_EQ(Position{1, 9}, asString->location.end);
    CHECK_EQ("foo", std::string{asString->value.data});
    CHECK_EQ(AstExprConstantString::QuotedSimple, asString->quoteStyle);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_parse_multi_line_fragment_override")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    auto res = check("function abc(foo: string) end");

    LUAU_REQUIRE_NO_ERRORS(res);

    auto fragment = parseFragment(
        R"(function abc(foo: string) end
abc(
"foo"
)
abc("bar")
)",
        Position{2, 5},
        Position{3, 1}
    );

    CHECK_EQ("function abc(foo: string) end\nabc(\n\"foo\"\n)", fragment.fragmentToParse);
    CHECK(fragment.nearestStatement->is<AstStatFunction>());

    CHECK_GE(fragment.ancestry.size(), 2);

    AstNode* back = fragment.ancestry.back();
    CHECK(back->is<AstExprConstantString>());
    CHECK_EQ(Position{2, 0}, back->location.begin);
    CHECK_EQ(Position{2, 5}, back->location.end);

    AstNode* parent = fragment.ancestry.rbegin()[1];
    CHECK(parent->is<AstExprCall>());
    CHECK_EQ(Position{1, 0}, parent->location.begin);
    CHECK_EQ(Position{3, 1}, parent->location.end);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "respects_frontend_options")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    std::string source = R"(
local tbl = { abc = 1234}
t
)";
    fileResolver.source["game/A"] = source;

    FrontendOptions opts;
    opts.forAutocomplete = true;

    frontend.check("game/A", opts);
    CHECK_NE(frontend.moduleResolverForAutocomplete.getModule("game/A"), nullptr);
    CHECK_EQ(frontend.moduleResolver.getModule("game/A"), nullptr);


    FragmentAutocompleteResult result = Luau::fragmentAutocomplete(frontend, source, "game/A", Position{2, 1}, opts, nullCallback);
    CHECK_EQ("game/A", result.incrementalModule->name);
    CHECK_NE(frontend.moduleResolverForAutocomplete.getModule("game/A"), nullptr);
    CHECK_EQ(frontend.moduleResolver.getModule("game/A"), nullptr);
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("FragmentAutocompleteTypeCheckerTests");

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_typecheck_simple_fragment")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
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
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
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


TEST_SUITE_BEGIN("MixedModeTests");

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "mixed_mode_basic_example_append")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, false};
    auto res = checkOldSolver(
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

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "mixed_mode_basic_example_inlined")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, false};
    auto res = checkOldSolver(
        R"(
local x = 4
local y = 5
)"
    );

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

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "mixed_mode_can_autocomplete_simple_property_access")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, false};
    auto res = checkOldSolver(
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

TEST_SUITE_END();

TEST_SUITE_BEGIN("FragmentAutocompleteTests");

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_autocomplete_simple_property_access")
{

    const std::string source = R"(
local tbl = { abc = 1234}
)";
    const std::string updated = R"(
local tbl = { abc = 1234}
tbl. 
)";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        Position{2, 5},
        [](FragmentAutocompleteResult& fragment)
        {
            LUAU_ASSERT(fragment.freshScope);

            CHECK_EQ(1, fragment.acResults.entryMap.size());
            CHECK(fragment.acResults.entryMap.count("abc"));
            CHECK_EQ(AutocompleteContext::Property, fragment.acResults.context);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_autocomplete_nested_property_access")
{
    const std::string source = R"(
local tbl = { abc = { def = 1234, egh = false } }
)";
    const std::string updated = R"(
local tbl = { abc = { def = 1234, egh = false } }
tbl.abc.
)";
    autocompleteFragmentInBothSolvers(
        source,
        updated,
        Position{2, 8},
        [](FragmentAutocompleteResult& fragment)
        {
            LUAU_ASSERT(fragment.freshScope);

            CHECK_EQ(2, fragment.acResults.entryMap.size());
            CHECK(fragment.acResults.entryMap.count("def"));
            CHECK(fragment.acResults.entryMap.count("egh"));
            CHECK_EQ(fragment.acResults.context, AutocompleteContext::Property);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "inline_autocomplete_picks_the_right_scope")
{
    const std::string source = R"(
type Table = { a: number, b: number }
do
    type Table = { x: string, y: string }
end
)";

    const std::string updated = R"(
type Table = { a: number, b: number }
do
    type Table = { x: string, y: string }
    local a : T
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        Position{4, 15},
        [](FragmentAutocompleteResult& fragment)
        {
            LUAU_ASSERT(fragment.freshScope);

            REQUIRE(fragment.acResults.entryMap.count("Table"));
            REQUIRE(fragment.acResults.entryMap["Table"].type);
            const TableType* tv = get<TableType>(follow(*fragment.acResults.entryMap["Table"].type));
            REQUIRE(tv);
            CHECK(tv->props.count("x"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "nested_recursive_function")
{
    const std::string source = R"(
function foo()
end
)";
    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{2, 0},
        [](FragmentAutocompleteResult& fragment)
        {
            CHECK(fragment.acResults.entryMap.count("foo"));
            CHECK_EQ(AutocompleteContext::Statement, fragment.acResults.context);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "string_literal_with_override")
{
    const std::string source = R"(
function foo(bar: string) end
foo("abc")
)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{2, 6},
        [](FragmentAutocompleteResult& fragment)
        {
            CHECK(fragment.acResults.entryMap.empty());
            CHECK_EQ(AutocompleteContext::String, fragment.acResults.context);
        },
        Position{2, 9}
    );
}

// Start compatibility tests!

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "empty_program")
{
    autocompleteFragmentInBothSolvers(
        "",
        "",
        Position{0, 1},
        [](FragmentAutocompleteResult& frag)
        {
            auto ac = frag.acResults;
            CHECK(ac.entryMap.count("table"));
            CHECK(ac.entryMap.count("math"));
            CHECK_EQ(ac.context, AutocompleteContext::Statement);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "local_initializer")
{
    const std::string source = "local a =";
    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{0, 9},
        [](FragmentAutocompleteResult& frag)
        {
            auto ac = frag.acResults;

            CHECK(ac.entryMap.count("table"));
            CHECK(ac.entryMap.count("math"));
            CHECK_EQ(ac.context, AutocompleteContext::Expression);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "leave_numbers_alone")
{
    const std::string source = "local a = 3.";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{0, 12},
        [](FragmentAutocompleteResult& frag)
        {
            auto ac = frag.acResults;
            CHECK(ac.entryMap.empty());
            CHECK_EQ(ac.context, AutocompleteContext::Unknown);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "user_defined_globals")
{
    const std::string source = "local myLocal = 4; ";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{0, 18},
        [](FragmentAutocompleteResult& frag)
        {
            auto ac = frag.acResults;

            CHECK(ac.entryMap.count("myLocal"));
            CHECK(ac.entryMap.count("table"));
            CHECK(ac.entryMap.count("math"));
            CHECK_EQ(ac.context, AutocompleteContext::Statement);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "dont_suggest_local_before_its_definition")
{
    const std::string source = R"(
        local myLocal = 4
        function abc()
             local myInnerLocal = 1

        end
    )";

    // autocomplete after abc but before myInnerLocal
    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{3, 0},
        [](FragmentAutocompleteResult& fragment)
        {
            auto ac = fragment.acResults;
            CHECK(ac.entryMap.count("myLocal"));
            LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "myInnerLocal");
        }
    );
    // autocomplete after my inner local
    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{4, 0},
        [](FragmentAutocompleteResult& fragment)
        {
            auto ac = fragment.acResults;
            CHECK(ac.entryMap.count("myLocal"));
            CHECK(ac.entryMap.count("myInnerLocal"));
        }
    );

    // autocomplete after abc, but don't include myInnerLocal(in the hidden scope)
    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{6, 0},
        [](FragmentAutocompleteResult& fragment)
        {
            auto ac = fragment.acResults;
            CHECK(ac.entryMap.count("myLocal"));
            LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "myInnerLocal");
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "nested_recursive_function")
{
    const std::string source = R"(
        local function outer()
            local function inner()
            end
        end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{3, 0},
        [](FragmentAutocompleteResult& result)
        {
            auto ac = result.acResults;
            CHECK(ac.entryMap.count("inner"));
            CHECK(ac.entryMap.count("outer"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "user_defined_local_functions_in_own_definition")
{
    const std::string source = R"(
        local function abc()

        end
    )";
    // Autocomplete inside of abc
    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{2, 0},
        [](FragmentAutocompleteResult& result)
        {
            auto ac = result.acResults;
            CHECK(ac.entryMap.count("abc"));
            CHECK(ac.entryMap.count("table"));
            CHECK(ac.entryMap.count("math"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "global_functions_are_not_scoped_lexically")
{
    const std::string source = R"(
        if true then
            function abc()

            end
        end
      )";
    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{6, 0},
        [](FragmentAutocompleteResult& result)
        {
            auto ac = result.acResults;
            CHECK(!ac.entryMap.empty());
            CHECK(ac.entryMap.count("abc"));
            CHECK(ac.entryMap.count("table"));
            CHECK(ac.entryMap.count("math"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "local_functions_fall_out_of_scope")
{
    const std::string source = R"(
        if true then
            local function abc()

            end
        end
      )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{6, 0},
        [](FragmentAutocompleteResult& result)
        {
            auto ac = result.acResults;
            CHECK_NE(0, ac.entryMap.size());
            LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "abc");
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "function_parameters")
{
    const std::string source = R"(
        function abc(test)

        end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{3, 0},
        [](FragmentAutocompleteResult& result)
        {
            auto ac = result.acResults;
            CHECK(ac.entryMap.count("test"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "unsealed_table")
{
    const std::string source = R"(
        local tbl = {}
        tbl.prop = 5
        tbl.
    )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{3, 12},
        [](FragmentAutocompleteResult& result)
        {
            auto ac = result.acResults;
            CHECK_EQ(1, ac.entryMap.size());
            CHECK(ac.entryMap.count("prop"));
            CHECK_EQ(ac.context, AutocompleteContext::Property);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "unsealed_table_2")
{
    const std::string source = R"(
        local tbl = {}
        local inner = { prop = 5 }
        tbl.inner = inner
        tbl.inner.
    )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{4, 18},
        [](FragmentAutocompleteResult& result)
        {
            auto ac = result.acResults;
            CHECK_EQ(1, ac.entryMap.size());
            CHECK(ac.entryMap.count("prop"));
            CHECK_EQ(ac.context, AutocompleteContext::Property);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "cyclic_table")
{
    const std::string source = R"(
        local abc = {}
        local def = { abc = abc }
        abc.def = def
        abc.def.
    )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{4, 16},
        [](FragmentAutocompleteResult& result)
        {
            auto ac = result.acResults;
            CHECK(ac.entryMap.count("abc"));
            CHECK_EQ(ac.context, AutocompleteContext::Property);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "table_union")
{
    const std::string source = R"(
        type t1 = { a1 : string, b2 : number }
        type t2 = { b2 : string, c3 : string }
        function func(abc : t1 | t2)

        end
    )";
    const std::string updated = R"(
        type t1 = { a1 : string, b2 : number }
        type t2 = { b2 : string, c3 : string }
        function func(abc : t1 | t2)
            abc.
        end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        Position{4, 16},
        [](FragmentAutocompleteResult& result)
        {
            auto ac = result.acResults;
            CHECK_EQ(1, ac.entryMap.size());
            CHECK(ac.entryMap.count("b2"));
            CHECK_EQ(ac.context, AutocompleteContext::Property);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "table_intersection")
{
    const std::string source = R"(
        type t1 = { a1 : string, b2 : number }
        type t2 = { b2 : number, c3 : string }
        function func(abc : t1 & t2)

        end
    )";
    const std::string updated = R"(
        type t1 = { a1 : string, b2 : number }
        type t2 = { b2 : number, c3 : string }
        function func(abc : t1 & t2)
            abc.
        end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        Position{4, 16},
        [](FragmentAutocompleteResult& result)
        {
            auto ac = result.acResults;
            CHECK_EQ(3, ac.entryMap.size());
            CHECK(ac.entryMap.count("a1"));
            CHECK(ac.entryMap.count("b2"));
            CHECK(ac.entryMap.count("c3"));
            CHECK_EQ(ac.context, AutocompleteContext::Property);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "get_suggestions_for_the_very_start_of_the_script")
{
    const std::string source = R"(

        function aaa() end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{0, 0},
        [](FragmentAutocompleteResult& result)
        {
            auto ac = result.acResults;
            CHECK(ac.entryMap.count("table"));
            CHECK_EQ(ac.context, AutocompleteContext::Statement);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "studio_ice_1")
{
    const std::string source = R"(
--Woop
@native
local function test()

end
)";

    const std::string updated = R"(
--Woop
@native
local function test()

end
function a
)";
    autocompleteFragmentInBothSolvers(source, updated, Position{6, 10}, [](FragmentAutocompleteResult& result) {});
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "method_call_inside_function_body")
{
    const std::string source = R"(
        local game = { GetService=function(s) return 'hello' end }

        function a()

        end
    )";

    const std::string updated = R"(
        local game = { GetService=function(s) return 'hello' end }

        function a()
            game:
        end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        Position{4, 17},
        [](FragmentAutocompleteResult& result)
        {
            auto ac = result.acResults;
            CHECK_NE(0, ac.entryMap.size());

            LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "math");
            CHECK_EQ(ac.context, AutocompleteContext::Property);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "tbl_function_parameter")
{
    const std::string source = R"(
--!strict
type Foo = {x : number, y : number}
local function func(abc : Foo)
   abc.
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{4, 7},
        [](FragmentAutocompleteResult& result)
        {
            CHECK_EQ(2, result.acResults.entryMap.size());
            CHECK(result.acResults.entryMap.count("x"));
            CHECK(result.acResults.entryMap.count("y"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "tbl_local_function_parameter")
{
    const std::string source = R"(
--!strict
type Foo = {x : number, y : number}
local function func(abc : Foo)
   abc.
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{4, 7},
        [](FragmentAutocompleteResult& result)
        {
            CHECK_EQ(2, result.acResults.entryMap.size());
            CHECK(result.acResults.entryMap.count("x"));
            CHECK(result.acResults.entryMap.count("y"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "vec3_function_parameter")
{
    const std::string source = R"(
--!strict
local function func(abc : FakeVec)
   abc.
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{3, 7},
        [](FragmentAutocompleteResult& result)
        {
            CHECK_EQ(2, result.acResults.entryMap.size());
            CHECK(result.acResults.entryMap.count("zero"));
            CHECK(result.acResults.entryMap.count("dot"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "vec3_local_function_parameter")
{
    const std::string source = R"(
--!strict
local function func(abc : FakeVec)
   abc.
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{3, 7},
        [](FragmentAutocompleteResult& result)
        {
            CHECK_EQ(2, result.acResults.entryMap.size());
            CHECK(result.acResults.entryMap.count("zero"));
            CHECK(result.acResults.entryMap.count("dot"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "function_parameter_not_recommending_out_of_scope_argument")
{
    const std::string source = R"(
--!strict
local function foo(abd: FakeVec)
end
local function bar(abc : FakeVec)
   a
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        Position{5, 5},
        [](FragmentAutocompleteResult& result)
        {
            CHECK(result.acResults.entryMap.count("abc"));
            CHECK(!result.acResults.entryMap.count("abd"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "bad_range")
{
    const std::string source = R"(
l
)";
    const std::string updated = R"(
local t = 1
t
)";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        Position{2, 1},
        [](FragmentAutocompleteResult& result)
        {
            auto opt = linearSearchForBinding(result.freshScope, "t");
            REQUIRE(opt);
            CHECK_EQ("number", toString(*opt));
        }
    );
}

TEST_SUITE_END();
