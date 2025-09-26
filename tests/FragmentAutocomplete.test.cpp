// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/FragmentAutocomplete.h"
#include "Fixture.h"
#include "Luau/Ast.h"
#include "Luau/AstQuery.h"
#include "Luau/Autocomplete.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/FileResolver.h"
#include "Luau/Frontend.h"
#include "Luau/AutocompleteTypes.h"
#include "Luau/ToString.h"
#include "Luau/Type.h"
#include "ScopedFlags.h"

#include <algorithm>
#include <chrono>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <memory>
#include <optional>

using namespace Luau;

LUAU_FASTINT(LuauParseErrorLimit)

LUAU_FASTFLAG(LuauBetterReverseDependencyTracking)
LUAU_FASTFLAG(LuauFragmentRequiresCanBeResolvedToAModule)
LUAU_FASTFLAG(LuauPopulateSelfTypesInFragment)
LUAU_FASTFLAG(LuauParseIncompleteInterpStringsWithLocation)
LUAU_FASTFLAG(LuauForInProvidesRecommendations)
LUAU_FASTFLAG(LuauFragmentAutocompleteTakesInnermostRefinement)
LUAU_FASTFLAG(LuauSuggestHotComments)
LUAU_FASTFLAG(LuauNumericUnaryOpsDontProduceNegationRefinements)
LUAU_FASTFLAG(LuauUnfinishedRepeatAncestryFix)
LUAU_FASTFLAG(LuauForInRangesConsiderInLocation)

static std::optional<AutocompleteEntryMap> nullCallback(std::string tag, std::optional<const ExternType*> ptr, std::optional<std::string> contents)
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

static ModuleResolver& getModuleResolver(Frontend& frontend)
{
    return FFlag::LuauSolverV2 ? frontend.moduleResolver : frontend.moduleResolverForAutocomplete;
}

template<class BaseType>
struct FragmentAutocompleteFixtureImpl : BaseType
{
    static_assert(std::is_base_of_v<Fixture, BaseType>, "BaseType must be a descendant of Fixture");

    ScopedFastFlag sffLuauPopulateSelfTypesInFragment{FFlag::LuauPopulateSelfTypesInFragment, true};
    ScopedFastFlag luauParseIncompleteInterpStringsWithLocation{FFlag::LuauParseIncompleteInterpStringsWithLocation, true};

    FragmentAutocompleteFixtureImpl()
        : BaseType(true)
    {
    }

    CheckResult checkWithOptions(const std::string& source)
    {
        return this->check(source, getOptions());
    }

    std::string cleanMarkers(const std::string& source)
    {
        markerPosition.clear();
        std::string filteredSource;
        filteredSource.reserve(source.size());

        Position curPos(0, 0);
        char prevChar{};
        for (char c : source)
        {
            if (prevChar == '@')
            {
                LUAU_ASSERT("Illegal marker character" && ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z')));
                LUAU_ASSERT("Duplicate marker found" && markerPosition.count(c) == 0);
                markerPosition.insert(std::pair{c, curPos});
            }
            else if (c == '@')
            {
                // skip the '@' character
                if (prevChar == '\\')
                {
                    // escaped @, prevent prevChar to be equal to '@' on next loop
                    c = '\0';
                    // replace escaping '\' with '@'
                    filteredSource.back() = '@';
                }
            }
            else
            {
                filteredSource.push_back(c);
                if (c == '\n')
                {
                    curPos.line++;
                    curPos.column = 0;
                }
                else
                {
                    curPos.column++;
                }
            }
            prevChar = c;
        }
        LUAU_ASSERT("Digit expected after @ symbol" && prevChar != '@');

        return filteredSource;
    }

    ParseResult parseHelper_(SourceModule& source, std::string document)
    {
        ParseOptions parseOptions;
        parseOptions.captureComments = true;
        ParseResult parseResult = Parser::parse(document.c_str(), document.length(), *source.names, *source.allocator, parseOptions);
        return parseResult;
    }

    ParseResult parseHelper(std::string document)
    {
        SourceModule& source = getSource();
        return parseHelper_(source, document);
    }

    FragmentAutocompleteAncestryResult runAutocompleteVisitor(const std::string& source, const Position& cursorPos)
    {
        ParseResult p = this->tryParse(source); // We don't care about parsing incomplete asts
        REQUIRE(p.root);
        return findAncestryForFragmentParse(p.root, cursorPos, p.root);
    }

    FragmentRegion getAutocompleteRegion(const std::string source, const Position& cursorPos)
    {
        ParseResult p = parseHelper(source);
        return Luau::getFragmentRegion(p.root, cursorPos);
    }

    std::optional<FragmentParseResult> parseFragment(
        const std::string& document,
        const Position& cursorPos,
        std::optional<Position> fragmentEndPosition = std::nullopt
    )
    {
        ParseResult p = parseHelper(document);
        ModulePtr module = this->getMainModule(getOptions().forAutocomplete);
        std::string_view srcString = document;
        return Luau::parseFragment(module->root, p.root, module->names.get(), srcString, cursorPos, fragmentEndPosition);
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
        ParseResult p = parseHelper(document);
        auto [_, result] = Luau::typecheckFragment(this->getFrontend(), "MainModule", cursorPos, getOptions(), document, fragmentEndPosition, p.root);
        return result;
    }

    FragmentAutocompleteStatusResult autocompleteFragment(
        const std::string& document,
        Position cursorPos,
        std::optional<Position> fragmentEndPosition = std::nullopt
    )
    {
        ParseOptions parseOptions;
        parseOptions.captureComments = true;
        ParseResult parseResult = parseHelper(document);
        FrontendOptions options = getOptions();
        FragmentContext context{document, parseResult, options, fragmentEndPosition};
        return Luau::tryFragmentAutocomplete(this->getFrontend(), "MainModule", cursorPos, context, nullCallback);
    }

    void autocompleteFragmentInNewSolver(
        const std::string& document,
        const std::string& updated,
        char marker,
        std::function<void(FragmentAutocompleteStatusResult& result)> assertions,
        std::optional<Position> fragmentEndPosition = std::nullopt
    )
    {
        ScopedFastFlag sff{FFlag::LuauSolverV2, true};

        std::string cleanDocument = cleanMarkers(document);
        std::string cleanUpdated = cleanMarkers(updated);
        Position cursorPos = getPosition(marker);

        this->getFrontend().setLuauSolverMode(SolverMode::New);
        this->check(document, getOptions());

        FragmentAutocompleteStatusResult result = autocompleteFragment(cleanUpdated, cursorPos, fragmentEndPosition);
        CHECK(result.status != FragmentAutocompleteStatus::InternalIce);
        assertions(result);
    }

    void autocompleteFragmentInOldSolver(
        const std::string& document,
        const std::string& updated,
        char marker,
        std::function<void(FragmentAutocompleteStatusResult& result)> assertions,
        std::optional<Position> fragmentEndPosition = std::nullopt
    )
    {
        ScopedFastFlag sff{FFlag::LuauSolverV2, false};

        std::string cleanDocument = cleanMarkers(document);
        std::string cleanUpdated = cleanMarkers(updated);
        Position cursorPos = getPosition(marker);

        this->getFrontend().setLuauSolverMode(SolverMode::Old);
        this->check(document, getOptions());

        FragmentAutocompleteStatusResult result = autocompleteFragment(cleanUpdated, cursorPos, fragmentEndPosition);
        CHECK(result.status != FragmentAutocompleteStatus::InternalIce);
        assertions(result);
    }

    void autocompleteFragmentInBothSolvers(
        const std::string& document,
        const std::string& updated,
        char marker,
        std::function<void(FragmentAutocompleteStatusResult& result)> assertions,
        std::optional<Position> fragmentEndPosition = std::nullopt
    )
    {
        std::string cleanDocument = cleanMarkers(document);
        std::string cleanUpdated = cleanMarkers(updated);
        Position cursorPos = getPosition(marker);

        ScopedFastFlag sff{FFlag::LuauSolverV2, true};
        this->getFrontend().setLuauSolverMode(SolverMode::New);
        this->check(cleanDocument, getOptions());

        FragmentAutocompleteStatusResult result = autocompleteFragment(cleanUpdated, cursorPos, fragmentEndPosition);
        CHECK(result.status != FragmentAutocompleteStatus::InternalIce);
        assertions(result);

        ScopedFastFlag _{FFlag::LuauSolverV2, false};
        this->getFrontend().setLuauSolverMode(SolverMode::Old);
        this->check(cleanDocument, getOptions());

        result = autocompleteFragment(cleanUpdated, cursorPos, fragmentEndPosition);
        CHECK(result.status != FragmentAutocompleteStatus::InternalIce);
        assertions(result);
    }

    std::pair<FragmentTypeCheckStatus, FragmentTypeCheckResult> typecheckFragmentForModule(
        const ModuleName& module,
        const std::string& document,
        Position cursorPos,
        std::optional<Position> fragmentEndPosition = std::nullopt
    )
    {
        ParseResult pr = parseHelper(document);
        return Luau::typecheckFragment(this->getFrontend(), module, cursorPos, getOptions(), document, fragmentEndPosition, pr.root);
    }

    FragmentAutocompleteStatusResult autocompleteFragmentForModule(
        const ModuleName& module,
        const std::string& document,
        Position cursorPos,
        std::optional<Position> fragmentEndPosition = std::nullopt
    )
    {
        ParseOptions parseOptions;
        parseOptions.captureComments = true;
        ParseResult parseResult = parseHelper(document);
        FrontendOptions options;
        FragmentContext context{document, parseResult, options, fragmentEndPosition};
        return Luau::tryFragmentAutocomplete(this->getFrontend().module, cursorPos, context, nullCallback);
    }

    SourceModule& getSource()
    {
        source = std::make_unique<SourceModule>();
        return *source;
    }

    const Position& getPosition(char marker) const
    {
        auto i = markerPosition.find(marker);
        LUAU_ASSERT(i != markerPosition.end());
        return i->second;
    }

    // Maps a marker character (0-9 inclusive) to a position in the source code.
    std::map<char, Position> markerPosition;

private:
    std::unique_ptr<SourceModule> source = std::make_unique<SourceModule>();
};

struct FragmentAutocompleteFixture : FragmentAutocompleteFixtureImpl<Fixture>
{
    FragmentAutocompleteFixture()
        : FragmentAutocompleteFixtureImpl<Fixture>()
    {
        addGlobalBinding(getFrontend().globals, "table", Binding{getBuiltins()->anyType});
        addGlobalBinding(getFrontend().globals, "math", Binding{getBuiltins()->anyType});
        addGlobalBinding(getFrontend().globalsForAutocomplete, "table", Binding{getBuiltins()->anyType});
        addGlobalBinding(getFrontend().globalsForAutocomplete, "math", Binding{getBuiltins()->anyType});
    }
};

struct FragmentAutocompleteBuiltinsFixture : FragmentAutocompleteFixtureImpl<BuiltinsFixture>
{
    FragmentAutocompleteBuiltinsFixture()
        : FragmentAutocompleteFixtureImpl<BuiltinsFixture>()
    {
    }

    Frontend& getFrontend() override
    {
        if (frontend)
            return *frontend;
        Frontend& f = BuiltinsFixture::getFrontend();
        Luau::unfreeze(f.globals.globalTypes);
        Luau::unfreeze(f.globalsForAutocomplete.globalTypes);
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

        addGlobalBinding(getFrontend().globals, "game", Binding{getBuiltins()->anyType});
        addGlobalBinding(getFrontend().globalsForAutocomplete, "game", Binding{getBuiltins()->anyType});
        Luau::freeze(f.globals.globalTypes);
        Luau::freeze(f.globalsForAutocomplete.globalTypes);

        return *frontend;
    }
};

TEST_SUITE_BEGIN("FragmentSelectionSpecTests");

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "just_two_locals")
{
    auto region = getAutocompleteRegion(
        R"(
local x = 4
local y = 5
)",
        {2, 11}
    );

    CHECK_EQ(Location{{2, 0}, {2, 11}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    REQUIRE(region.nearestStatement);
    CHECK(region.nearestStatement->as<AstStatLocal>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "singleline_call")
{
    auto region = getAutocompleteRegion(
        R"(
abc("foo")
)",
        {1, 10}
    );

    CHECK_EQ(Location{{1, 0}, {1, 10}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    REQUIRE(region.nearestStatement);
    CHECK(region.nearestStatement->as<AstStatExpr>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "midway_multiline_call")
{
    auto region = getAutocompleteRegion(
        R"(
abc(
"foo"
)
)",
        {2, 4}
    );

    CHECK_EQ(Location{{1, 0}, {2, 4}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    REQUIRE(region.nearestStatement);
    CHECK(region.nearestStatement->as<AstStatExpr>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "end_multiline_call")
{
    auto region = getAutocompleteRegion(
        R"(
abc(
"foo"
)
)",
        {3, 1}
    );

    CHECK_EQ(Location{{1, 0}, {3, 1}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    REQUIRE(region.nearestStatement);
    CHECK(region.nearestStatement->as<AstStatExpr>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "midway_through_call")
{
    auto region = getAutocompleteRegion(
        R"(
abc("foo")
)",
        {1, 6}
    );

    CHECK_EQ(Location{{1, 0}, {1, 6}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    REQUIRE(region.nearestStatement);
    CHECK(region.nearestStatement->as<AstStatExpr>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "inside_incomplete_do")
{
    auto region = getAutocompleteRegion(
        R"(
local x = 4
do
)",
        {2, 2}
    );

    CHECK_EQ(Location{{2, 2}, {2, 2}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatBlock>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "end_of_do")
{
    auto region = getAutocompleteRegion(
        R"(
local x = 4
do
end
)",
        {3, 3}
    );

    CHECK_EQ(Location{{3, 3}, {3, 3}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatBlock>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "inside_do")
{
    auto region = getAutocompleteRegion(
        R"(
local x = 4
do

end
)",
        {3, 3}
    );

    CHECK_EQ(Location{{3, 3}, {3, 3}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatBlock>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "partial_statement_inside_do")
{
    auto region = getAutocompleteRegion(
        R"(
local x = 4
do
    local x =
end
)",
        {3, 13}
    );

    CHECK_EQ(Location{{3, 4}, {3, 13}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatLocal>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "partial_statement_after_do")
{
    auto region = getAutocompleteRegion(
        R"(
local x = 4
do

end
local x =
)",
        {5, 9}
    );

    CHECK_EQ(Location{{5, 0}, {5, 9}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatLocal>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "before_func")
{
    auto region = getAutocompleteRegion(
        R"(
function f()
end
)",
        {1, 0}
    );
    CHECK_EQ(Location{{1, 0}, {1, 0}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "after_func_same_line")
{
    auto region = getAutocompleteRegion(
        R"(
function f()
end
)",
        {2, 3}
    );
    CHECK_EQ(Location{{2, 3}, {2, 3}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "after_func_new_line")
{
    auto region = getAutocompleteRegion(
        R"(
function f()
end

)",
        {3, 0}
    );
    CHECK_EQ(Location{{3, 0}, {3, 0}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "while_writing_func")
{
    auto region = getAutocompleteRegion(
        R"(
function f(arg1,
)",
        {1, 17}
    );
    CHECK_EQ(Location{{1, 0}, {1, 17}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "writing_func_annotation")
{
    auto region = getAutocompleteRegion(
        R"(
function f(arg1 : T
)",
        {1, 19}
    );
    CHECK_EQ(Location{{1, 0}, {1, 19}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "writing_func_return")
{
    auto region = getAutocompleteRegion(
        R"(
function f(arg1 : T) :
)",
        {1, 22}
    );
    CHECK_EQ(Location{{1, 0}, {1, 22}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "writing_func_return_pack")
{
    auto region = getAutocompleteRegion(
        R"(
function f(arg1 : T) : T...
)",
        {1, 27}
    );
    CHECK_EQ(Location{{1, 0}, {1, 27}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "before_local_func")
{
    auto region = getAutocompleteRegion(
        R"(
local function f()
end
)",
        {1, 0}
    );
    CHECK_EQ(Location{{1, 0}, {1, 0}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatLocalFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "after_local_func_same_line")
{
    auto region = getAutocompleteRegion(
        R"(
local function f()
end
)",
        {2, 3}
    );
    CHECK_EQ(Location{{2, 3}, {2, 3}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatLocalFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "after_local_func_new_line")
{
    auto region = getAutocompleteRegion(
        R"(
local function f()
end

)",
        {3, 0}
    );
    CHECK_EQ(Location{{3, 0}, {3, 0}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatLocalFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "while_writing_local_func")
{
    auto region = getAutocompleteRegion(
        R"(
local function f(arg1,
)",
        {1, 22}
    );
    CHECK_EQ(Location{{1, 0}, {1, 22}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatLocalFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "writing_local_func_annotation")
{
    auto region = getAutocompleteRegion(
        R"(
local function f(arg1 : T
)",
        {1, 25}
    );
    CHECK_EQ(Location{{1, 0}, {1, 25}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatLocalFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "writing_local_func_return")
{
    auto region = getAutocompleteRegion(
        R"(
local function f(arg1 : T) :
)",
        {1, 28}
    );
    CHECK_EQ(Location{{1, 0}, {1, 28}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatLocalFunction>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "writing_local_func_return_pack")
{
    auto region = getAutocompleteRegion(
        R"(
local function f(arg1 : T) : T...
)",
        {1, 33}
    );
    CHECK_EQ(Location{{1, 0}, {1, 33}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatLocalFunction>());
}


TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "single_line_local_and_annot")
{
    auto region = getAutocompleteRegion(
        R"(
type Part = {x : number}
local part : Part = {x = 3}; pa
)",
        {2, 32}
    );
    CHECK_EQ(Location{{2, 29}, {2, 32}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatError>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "partial_while_in_condition")
{
    auto region = getAutocompleteRegion(
        R"(
while t
)",
        Position{1, 7}
    );

    CHECK_EQ(Location{{1, 0}, {1, 7}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatWhile>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "while_inside_condition_same_line")
{
    auto region = getAutocompleteRegion(
        R"(
while true do
end
)",
        Position{1, 13}
    );

    CHECK_EQ(Location{{1, 13}, {1, 13}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatWhile>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "partial_for_numeric_in_condition")
{
    ScopedFastFlag sff{FFlag::LuauForInProvidesRecommendations, true};
    auto region = getAutocompleteRegion(
        R"(
for c = 1,3
)",
        Position{1, 11}
    );

    CHECK_EQ(Location{{1, 10}, {1, 11}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatFor>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "partial_for_numeric_in_body")
{
    auto region = getAutocompleteRegion(
        R"(
for c = 1,3 do
)",
        Position{1, 14}
    );

    CHECK_EQ(Location{{1, 14}, {1, 14}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatFor>());
}


TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "partial_for_in_in_condition_1")
{
    auto region = getAutocompleteRegion(
        R"(
for i,v in {1,2,3}
)",
        Position{1, 18}
    );

    CHECK_EQ(Location{{1, 0}, {1, 18}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatForIn>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "partial_for_in_in_condition_2")
{
    auto region = getAutocompleteRegion(
        R"(
for i,v in
)",
        Position{1, 10}
    );

    CHECK_EQ(Location{{1, 0}, {1, 10}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatForIn>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "partial_for_in_in_condition_3")
{
    auto region = getAutocompleteRegion(
        R"(
for i,
)",
        Position{1, 6}
    );

    CHECK_EQ(Location{{1, 0}, {1, 6}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatForIn>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "partial_for_in_in_body")
{
    auto region = getAutocompleteRegion(
        R"(
for i,v in {1,2,3} do
)",
        Position{1, 21}
    );

    CHECK_EQ(Location{{1, 21}, {1, 21}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatForIn>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_partial")
{
    auto region = getAutocompleteRegion(
        R"(
if)",
        Position{1, 2}
    );
    CHECK_EQ(Location{{1, 2}, {1, 2}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatIf>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_partial_in_condition_at")
{
    auto region = getAutocompleteRegion(
        R"(
if true
)",
        Position{1, 7}
    );
    CHECK_EQ(Location{{1, 3}, {1, 7}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatIf>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_partial_in_condition_after")
{
    auto region = getAutocompleteRegion(
        R"(
if true
)",
        Position{1, 8}
    );
    CHECK_EQ(Location{{1, 3}, {1, 8}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatIf>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_partial_after_condition")
{
    auto region = getAutocompleteRegion(
        R"(
if true then
)",
        Position{1, 12}
    );
    CHECK_EQ(Location{{1, 12}, {1, 12}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatIf>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_partial_new_line")
{
    auto region = getAutocompleteRegion(
        R"(
if true then

)",
        Position{2, 0}
    );
    CHECK_EQ(Location{{2, 0}, {2, 0}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatIf>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_complete_inside_scope_line")
{
    auto region = getAutocompleteRegion(
        R"(
if true then
    local x =
end

)",
        Position{2, 13}
    );
    CHECK_EQ(Location{{2, 4}, {2, 13}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatLocal>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_else_if")
{
    auto region = getAutocompleteRegion(
        R"(
if true then
elseif
end

)",
        Position{2, 8}
    );
    CHECK_EQ(Location{{2, 8}, {2, 8}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatIf>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_else_if_no_end")
{
    auto region = getAutocompleteRegion(
        R"(
if true then
elseif
)",
        Position{2, 8}
    );
    CHECK_EQ(Location{{2, 8}, {2, 8}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatIf>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_else_if_after_then")
{
    auto region = getAutocompleteRegion(
        R"(
if true then
elseif false then
end

)",
        Position{2, 17}
    );
    CHECK_EQ(Location{{2, 17}, {2, 17}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatIf>());
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_else_if_after_then_new_line")
{
    auto region = getAutocompleteRegion(
        R"(
if true then
elseif false then

end

)",
        Position{3, 0}
    );
    CHECK_EQ(Location{{3, 0}, {3, 0}}, region.fragmentLocation);
    REQUIRE(region.parentBlock);
    CHECK(region.nearestStatement->as<AstStatIf>());
}


TEST_SUITE_END();

// NOLINTBEGIN(bugprone-unchecked-optional-access)
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
    CHECK_EQ(3, result.localStack.size());
    CHECK_EQ(result.localMap.size(), result.localStack.size());
    CHECK_EQ("bar", std::string(result.localStack.back()->name.value));
    auto returnSt = result.nearestStatement->as<AstStatReturn>();
    CHECK(returnSt != nullptr);
}

TEST_SUITE_END();


TEST_SUITE_BEGIN("FragmentAutocompleteParserTests");

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "empty_program_1")
{
    checkWithOptions("");
    ScopedFastInt sfi{FInt::LuauParseErrorLimit, 1};
    auto fragment = parseFragment("", Position(0, 39));
    REQUIRE(fragment);
    CHECK(fragment->fragmentToParse == "");
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "empty_program_2")
{
    const std::string source = R"(

)";
    checkWithOptions(source);
    ScopedFastInt sfi{FInt::LuauParseErrorLimit, 1};
    auto fragment = parseFragment(source, Position(1, 39));
    REQUIRE(fragment);
    CHECK(fragment->fragmentToParse == "");
}


TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "thrown_parse_error_leads_to_null_root")
{
    checkWithOptions("type A =  ");
    ScopedFastInt sfi{FInt::LuauParseErrorLimit, 1};
    auto fragment = parseFragment("type A = <>function<> more garbage here", Position(0, 39));
    CHECK(fragment == std::nullopt);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "local_initializer")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
    checkWithOptions("local a =");
    auto fragment = parseFragment("local a =", Position(0, 9));

    REQUIRE(fragment.has_value());
    CHECK_EQ("local a =", fragment->fragmentToParse);
    CHECK_EQ(Location{Position{0, 0}, 9}, fragment->root->location);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "statement_in_empty_fragment_is_non_null")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
    auto res = checkWithOptions(R"(

)");

    LUAU_REQUIRE_NO_ERRORS(res);

    auto fragment = parseFragment(
        R"(

)",
        Position(1, 0)
    );
    REQUIRE(fragment.has_value());
    CHECK_EQ("", fragment->fragmentToParse);
    CHECK_EQ(1, fragment->ancestry.size());
    REQUIRE(fragment->root);
    CHECK_EQ(0, fragment->root->body.size);
    auto statBody = fragment->root->as<AstStatBlock>();
    CHECK(statBody != nullptr);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_parse_complete_fragments")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
    auto res = checkWithOptions(
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

    REQUIRE(fragment.has_value());

    CHECK_EQ(Location{Position{3, 0}, Position{3, 15}}, fragment->root->location);

    CHECK_EQ("local z = x + y", fragment->fragmentToParse);
    CHECK_EQ(4, fragment->ancestry.size());
    REQUIRE(fragment->root);
    CHECK_EQ(1, fragment->root->body.size);
    auto stat = fragment->root->body.data[0]->as<AstStatLocal>();
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
    auto res = checkWithOptions(
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

    REQUIRE(fragment.has_value());

    CHECK_EQ("local z = x + y", fragment->fragmentToParse);
    CHECK_EQ(4, fragment->ancestry.size());
    REQUIRE(fragment->root);
    CHECK_EQ(Location{Position{2, 0}, Position{2, 15}}, fragment->root->location);
    CHECK_EQ(1, fragment->root->body.size);
    auto stat = fragment->root->body.data[0]->as<AstStatLocal>();
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
    checkWithOptions(R"(
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

    REQUIRE(fragment.has_value());

    CHECK_EQ("", fragment->fragmentToParse);
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_parse_single_line_fragment_override")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
    auto res = checkWithOptions("function abc(foo: string) end");

    LUAU_REQUIRE_NO_ERRORS(res);

    auto callFragment = parseFragment(
        R"(function abc(foo: string) end
abc("foo")
abc("bar")
)",
        Position{1, 6},
        Position{1, 10}
    );

    REQUIRE(callFragment.has_value());

    CHECK_EQ("abc(\"foo\")", callFragment->fragmentToParse);
    CHECK(callFragment->nearestStatement);
    CHECK(callFragment->nearestStatement->is<AstStatExpr>());

    CHECK_GE(callFragment->ancestry.size(), 2);

    AstNode* back = callFragment->ancestry.back();
    CHECK(back->is<AstExprConstantString>());
    CHECK_EQ(Position{1, 4}, back->location.begin);
    CHECK_EQ(Position{1, 9}, back->location.end);

    AstNode* parent = callFragment->ancestry.rbegin()[1];
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

    REQUIRE(stringFragment.has_value());

    CHECK_EQ("abc(\"foo\"", stringFragment->fragmentToParse);
    CHECK(stringFragment->nearestStatement);
    CHECK(stringFragment->nearestStatement->is<AstStatExpr>());

    CHECK_GE(stringFragment->ancestry.size(), 1);

    back = stringFragment->ancestry.back();

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

    auto res = checkWithOptions("function abc(foo: string) end");

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

    REQUIRE(fragment.has_value());

    CHECK_EQ("abc(\n\"foo\"\n)", fragment->fragmentToParse);
    CHECK(fragment->nearestStatement);
    CHECK(fragment->nearestStatement->is<AstStatExpr>());

    CHECK_GE(fragment->ancestry.size(), 2);

    AstNode* back = fragment->ancestry.back();
    CHECK(back->is<AstExprConstantString>());
    CHECK_EQ(Position{2, 0}, back->location.begin);
    CHECK_EQ(Position{2, 5}, back->location.end);

    AstNode* parent = fragment->ancestry.rbegin()[1];
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
    getFrontend().setLuauSolverMode(FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old);
    getFrontend().check("game/A", opts);
    CHECK_NE(getFrontend().moduleResolverForAutocomplete.getModule("game/A"), nullptr);
    CHECK_EQ(getFrontend().moduleResolver.getModule("game/A"), nullptr);
    ParseOptions parseOptions;
    parseOptions.captureComments = true;
    SourceModule sourceMod;
    ParseResult parseResult = Parser::parse(source.c_str(), source.length(), *sourceMod.names, *sourceMod.allocator, parseOptions);
    FragmentContext context{source, parseResult, opts, std::nullopt};

    FragmentAutocompleteStatusResult frag = Luau::tryFragmentAutocomplete(getFrontend(), "game/A", Position{2, 1}, context, nullCallback);
    REQUIRE(frag.result);
    REQUIRE(frag.result->incrementalModule);
    CHECK_EQ("game/A", frag.result->incrementalModule->name);
    CHECK_NE(getFrontend().moduleResolverForAutocomplete.getModule("game/A"), nullptr);
    CHECK_EQ(getFrontend().moduleResolver.getModule("game/A"), nullptr);
}

TEST_SUITE_END();


TEST_SUITE_BEGIN("FragmentAutocompleteTypeCheckerTests");

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_typecheck_simple_fragment")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
    auto res = checkWithOptions(
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
    auto res = checkWithOptions(
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
    getFrontend().setLuauSolverMode(FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old);
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
    getFrontend().setLuauSolverMode(FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old);
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
    getFrontend().setLuauSolverMode(FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old);
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
    REQUIRE(fragment.result);
    LUAU_ASSERT(fragment.result->freshScope);

    CHECK_EQ(1, fragment.result->acResults.entryMap.size());
    CHECK(fragment.result->acResults.entryMap.count("abc"));
    CHECK_EQ(AutocompleteContext::Property, fragment.result->acResults.context);
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "typecheck_fragment_handles_unusable_module")
{
    const std::string sourceA = "MainModule";
    fileResolver.source[sourceA] = R"(
local Modules = game:GetService('Gui').Modules
local B = require(Modules.B)
return { hello = B }
)";

    const std::string sourceB = "game/Gui/Modules/B";
    fileResolver.source[sourceB] = R"(return {hello = "hello"})";

    CheckResult result = getFrontend().check(sourceA, getOptions());
    CHECK(!getFrontend().isDirty(sourceA, getOptions().forAutocomplete));

    std::weak_ptr<Module> weakModule = getModuleResolver(getFrontend()).getModule(sourceB);
    REQUIRE(!weakModule.expired());

    getFrontend().markDirty(sourceB);
    CHECK(getFrontend().isDirty(sourceA, getOptions().forAutocomplete));

    getFrontend().check(sourceB, getOptions());
    CHECK(weakModule.expired());

    auto [status, _] = typecheckFragmentForModule(sourceA, fileResolver.source[sourceA], Luau::Position(0, 0));
    CHECK_EQ(status, FragmentTypeCheckStatus::SkipAutocomplete);

    auto [status2, _2] = typecheckFragmentForModule(sourceB, fileResolver.source[sourceB], Luau::Position(3, 20));
    CHECK_EQ(status2, FragmentTypeCheckStatus::Success);
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("FragmentAutocompleteTests");

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "multiple_fragment_autocomplete")
{
    ToStringOptions opt;
    opt.exhaustive = true;
    opt.exhaustive = true;
    opt.functionTypeArguments = true;
    opt.maxTableLength = 0;
    opt.maxTypeLength = 0;

    auto checkAndExamine = [&](const std::string& src, const std::string& idName, const std::string& idString)
    {
        checkWithOptions(src);
        auto id = getType(idName, true);
        LUAU_ASSERT(id);
        CHECK_EQ(Luau::toString(*id, opt), idString);
    };

    auto getTypeFromModule = [](ModulePtr module, const std::string& name) -> std::optional<TypeId>
    {
        if (!module->hasModuleScope())
            return std::nullopt;
        return lookupName(module->getModuleScope(), name);
    };

    auto fragmentACAndCheck = [&](const std::string& updated,
                                  const Position& pos,
                                  const std::string& idName,
                                  const std::string& srcIdString,
                                  const std::string& fragIdString)
    {
        FragmentAutocompleteStatusResult frag = autocompleteFragment(updated, pos, std::nullopt);
        REQUIRE(frag.result);
        auto fragId = getTypeFromModule(frag.result->incrementalModule, idName);
        LUAU_ASSERT(fragId);
        CHECK_EQ(Luau::toString(*fragId, opt), fragIdString);

        auto srcId = getType(idName, true);
        LUAU_ASSERT(srcId);
        CHECK_EQ(Luau::toString(*srcId, opt), srcIdString);
    };

    const std::string source = R"(local module = {}
f
return module)";

    const std::string updated1 = R"(local module = {}
function module.a
return module)";

    const std::string updated2 = R"(local module = {}
function module.ab
return module)";

    {
        ScopedFastFlag sff{FFlag::LuauSolverV2, false};
        getFrontend().setLuauSolverMode(SolverMode::Old);
        checkAndExamine(source, "module", "{|  |}");
        fragmentACAndCheck(updated1, Position{1, 17}, "module", "{|  |}", "{| a: (%error-id%: unknown) -> () |}");
        fragmentACAndCheck(updated2, Position{1, 18}, "module", "{|  |}", "{| ab: (%error-id%: unknown) -> () |}");
    }
    {
        ScopedFastFlag sff{FFlag::LuauSolverV2, true};
        getFrontend().setLuauSolverMode(SolverMode::New);
        checkAndExamine(source, "module", "{  }");
        // [TODO] CLI-140762 Fragment autocomplete still doesn't return correct result when LuauSolverV2 is on
        return;
        fragmentACAndCheck(updated1, Position{1, 17}, "module", "{  }", "{ a: (%error-id%: unknown) -> () }");
        fragmentACAndCheck(updated2, Position{1, 18}, "module", "{  }", "{ ab: (%error-id%: unknown) -> () }");
    }
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "can_autocomplete_simple_property_access")
{

    const std::string source = R"(
local tbl = { abc = 1234}
)";
    const std::string updated = R"(
local tbl = { abc = 1234}
tbl. @1
)";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        '1',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            auto acResults = fragment.result->acResults;

            CHECK_EQ(1, acResults.entryMap.size());
            CHECK(acResults.entryMap.count("abc"));
            CHECK_EQ(AutocompleteContext::Property, acResults.context);
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
tbl.abc.@1
)";
    autocompleteFragmentInBothSolvers(
        source,
        updated,
        '1',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            LUAU_ASSERT(fragment.result->freshScope);

            CHECK_EQ(2, fragment.result->acResults.entryMap.size());
            CHECK(fragment.result->acResults.entryMap.count("def"));
            CHECK(fragment.result->acResults.entryMap.count("egh"));
            CHECK_EQ(fragment.result->acResults.context, AutocompleteContext::Property);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "multiple_functions_complex")
{
    const std::string text = R"(@1 local function f1(a1)@2
    local l1 = 1;@3
    g1 = 1;@4
end
@5
local function f2(a2)
    local l2 = 1;@6
    g2 = 1;
end @7
)";

    autocompleteFragmentInBothSolvers(
        text,
        text,
        '1',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            auto strings = fragment.result->acResults.entryMap;
            CHECK(strings.count("f1") == 0);
            CHECK(strings.count("a1") == 0);
            CHECK(strings.count("l1") == 0);
            CHECK(strings.count("g1") != 0);
            CHECK(strings.count("f2") == 0);
            CHECK(strings.count("a2") == 0);
            CHECK(strings.count("l2") == 0);
            CHECK(strings.count("g2") != 0);
        }
    );

    autocompleteFragmentInBothSolvers(
        text,
        text,
        '2',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            auto strings = fragment.result->acResults.entryMap;
            CHECK(strings.count("f1") != 0);
            CHECK(strings.count("a1") != 0);
            CHECK(strings.count("l1") == 0);
            CHECK(strings.count("g1") != 0);
            CHECK(strings.count("f2") == 0);
            CHECK(strings.count("a2") == 0);
            CHECK(strings.count("l2") == 0);
            CHECK(strings.count("g2") != 0);
        }
    );

    autocompleteFragmentInBothSolvers(
        text,
        text,
        '3',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            auto strings = fragment.result->acResults.entryMap;
            CHECK(strings.count("f1") != 0);
            CHECK(strings.count("a1") != 0);
            CHECK(strings.count("l1") != 0);
            CHECK(strings.count("g1") != 0);
            CHECK(strings.count("f2") == 0);
            CHECK(strings.count("a2") == 0);
            CHECK(strings.count("l2") == 0);
            CHECK(strings.count("g2") != 0);
        }
    );

    autocompleteFragmentInBothSolvers(
        text,
        text,
        '4',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            auto strings = fragment.result->acResults.entryMap;
            CHECK(strings.count("f1") != 0);
            CHECK(strings.count("a1") != 0);
            CHECK(strings.count("l1") != 0);
            CHECK(strings.count("g1") != 0);
            CHECK(strings.count("f2") == 0);
            CHECK(strings.count("a2") == 0);
            CHECK(strings.count("l2") == 0);
            CHECK(strings.count("g2") != 0);
        }
    );

    autocompleteFragmentInBothSolvers(
        text,
        text,
        '5',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            auto strings = fragment.result->acResults.entryMap;
            CHECK(strings.count("f1") != 0);
            CHECK(strings.count("a1") == 0);
            CHECK(strings.count("l1") == 0);
            CHECK(strings.count("g1") != 0);
            CHECK(strings.count("f2") == 0);
            CHECK(strings.count("a2") == 0);
            CHECK(strings.count("l2") == 0);
            CHECK(strings.count("g2") != 0);
        }
    );

    autocompleteFragmentInBothSolvers(
        text,
        text,
        '6',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            auto strings = fragment.result->acResults.entryMap;
            CHECK(strings.count("f1") != 0);
            CHECK(strings.count("a1") == 0);
            CHECK(strings.count("l1") == 0);
            CHECK(strings.count("g1") != 0);
            CHECK(strings.count("f2") != 0);
            CHECK(strings.count("a2") != 0);
            CHECK(strings.count("l2") != 0);
            CHECK(strings.count("g2") != 0);
        }
    );

    autocompleteFragmentInBothSolvers(
        text,
        text,
        '7',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            auto strings = fragment.result->acResults.entryMap;
            CHECK(strings.count("f1") != 0);
            CHECK(strings.count("a1") == 0);
            CHECK(strings.count("l1") == 0);
            CHECK(strings.count("g1") != 0);
            CHECK(strings.count("f2") != 0);
            CHECK(strings.count("a2") == 0);
            CHECK(strings.count("l2") == 0);
            CHECK(strings.count("g2") != 0);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "inline_autocomplete_picks_the_right_scope_1")
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
    local a : T@1
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        '1',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            LUAU_ASSERT(fragment.result->freshScope);
            REQUIRE(fragment.result->acResults.entryMap.count("Table"));
            REQUIRE(fragment.result->acResults.entryMap["Table"].type);
            const TableType* tv = get<TableType>(follow(*fragment.result->acResults.entryMap["Table"].type));
            REQUIRE(tv);
            CHECK(tv->props.count("x"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "inline_autocomplete_picks_the_right_scope_2")
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
end
local a : T@1
)";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        '1',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            LUAU_ASSERT(fragment.result->freshScope);
            REQUIRE(fragment.result->acResults.entryMap.count("Table"));
            REQUIRE(fragment.result->acResults.entryMap["Table"].type);
            const TableType* tv = get<TableType>(follow(*fragment.result->acResults.entryMap["Table"].type));
            REQUIRE(tv);
            CHECK(tv->props.count("a"));
            CHECK(tv->props.count("b"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "nested_recursive_function")
{
    const std::string source = R"(
function foo()
@1end
)";
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            CHECK(fragment.result->acResults.entryMap.count("foo"));
            CHECK_EQ(AutocompleteContext::Statement, fragment.result->acResults.context);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "string_literal_with_override")
{
    const std::string source = R"(
function foo(bar: string) end
foo("a@1bc")
)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            CHECK(fragment.result->acResults.entryMap.empty());
            CHECK_EQ(AutocompleteContext::String, fragment.result->acResults.context);
        },
        Position{2, 9}
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "empty_program")
{
    autocompleteFragmentInBothSolvers(
        "",
        "@1",
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
            CHECK(ac.entryMap.count("table"));
            CHECK(ac.entryMap.count("math"));
            CHECK_EQ(ac.context, AutocompleteContext::Statement);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "local_initializer")
{
    const std::string source = "local a =@1";
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;

            CHECK(ac.entryMap.count("table"));
            CHECK(ac.entryMap.count("math"));
            CHECK_EQ(ac.context, AutocompleteContext::Expression);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "leave_numbers_alone")
{
    const std::string source = "local a = 3.@1";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
            CHECK(ac.entryMap.empty());
            CHECK_EQ(ac.context, AutocompleteContext::Unknown);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "user_defined_globals")
{
    const std::string source = "local myLocal = 4;@1 ";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;

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
@1             local myInnerLocal = 1
@2
        end
@3    )";

    // autocomplete after abc but before myInnerLocal
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            auto ac = fragment.result->acResults;
            CHECK(ac.entryMap.count("myLocal"));
            LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "myInnerLocal");
        }
    );
    // autocomplete after my inner local
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '2',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            auto ac = fragment.result->acResults;
            CHECK(ac.entryMap.count("myLocal"));
            CHECK(ac.entryMap.count("myInnerLocal"));
        }
    );

    // autocomplete after abc, but don't include myInnerLocal(in the hidden scope)
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '3',
        [](FragmentAutocompleteStatusResult& fragment)
        {
            REQUIRE(fragment.result);
            auto ac = fragment.result->acResults;
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
@1            end
        end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
            CHECK(ac.entryMap.count("inner"));
            CHECK(ac.entryMap.count("outer"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "user_defined_local_functions_in_own_definition")
{
    const std::string source = R"(
        local function abc()
@1
        end
    )";
    // Autocomplete inside of abc
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
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
@1      )";
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
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
@1      )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
            CHECK_NE(0, ac.entryMap.size());
            LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "abc");
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "function_parameters")
{
    const std::string source = R"(
        function abc(test)

@1        end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
            CHECK(ac.entryMap.count("test"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "unsealed_table")
{
    const std::string source = R"(
        local tbl = {}
        tbl.prop = 5
        tbl.@1
    )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
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
        tbl.inner.@1
    )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
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
        abc.def.@1
    )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
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
            abc.@1
        end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
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
            abc.@1
        end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
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
    const std::string source = R"(@1

        function aaa() end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
            CHECK(ac.entryMap.count("table"));
            CHECK_EQ(ac.context, AutocompleteContext::Statement);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "studio_ice_1")
{
    const std::string source = R"(
--Woop
\@native
local function test()

end
)";

    const std::string updated = R"(
--Woop
\@native
local function test()

end
function a@1
)";
    autocompleteFragmentInBothSolvers(source, updated, '1', [](FragmentAutocompleteStatusResult& result) {});
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
            game:@1
        end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto ac = frag.result->acResults;
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
   abc.@1
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            CHECK_EQ(2, frag.result->acResults.entryMap.size());
            CHECK(frag.result->acResults.entryMap.count("x"));
            CHECK(frag.result->acResults.entryMap.count("y"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "tbl_local_function_parameter")
{
    const std::string source = R"(
--!strict
type Foo = {x : number, y : number}
local function func(abc : Foo)
   abc.@1
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            CHECK_EQ(2, frag.result->acResults.entryMap.size());
            CHECK(frag.result->acResults.entryMap.count("x"));
            CHECK(frag.result->acResults.entryMap.count("y"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "vec3_function_parameter")
{
    const std::string source = R"(
--!strict
local function func(abc : FakeVec)
   abc.@1
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            CHECK_EQ(2, frag.result->acResults.entryMap.size());
            CHECK(frag.result->acResults.entryMap.count("zero"));
            CHECK(frag.result->acResults.entryMap.count("dot"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "vec3_local_function_parameter")
{
    const std::string source = R"(
--!strict
local function func(abc : FakeVec)
   abc.@1
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            CHECK_EQ(2, frag.result->acResults.entryMap.size());
            CHECK(frag.result->acResults.entryMap.count("zero"));
            CHECK(frag.result->acResults.entryMap.count("dot"));
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
   a@1
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            CHECK(frag.result->acResults.entryMap.count("abc"));
            CHECK(!frag.result->acResults.entryMap.count("abd"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "bad_range_1")
{
    const std::string source = R"(
local t = 1
)";
    const std::string updated = R"(
t
@1)";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto opt = linearSearchForBinding(frag.result->freshScope, "t");
            REQUIRE(opt);
            CHECK_EQ("number", toString(*opt));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "bad_range_2")
{
    const std::string source = R"(
local t = 1
)";
    const std::string updated = R"(
local t = 1
t@1
)";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            auto opt = linearSearchForBinding(frag.result->freshScope, "t");
            REQUIRE(opt);
            CHECK_EQ("number", toString(*opt));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "bad_range_3")
{
    // This test makes less sense since we don't have an updated check that
    // includes l
    // instead this will recommend nothing useful because `local t` hasn't
    // been typechecked in the fresh module
    const std::string source = R"(
l
)";
    const std::string updated = R"(
local t = 1
l@1
)";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            CHECK(frag.status == FragmentAutocompleteStatus::Success);
            REQUIRE(frag.result);
        }
    );
}


TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "do_not_recommend_results_in_multiline_comment")
{
    std::string source = R"(--[[
)";
    std::string dest = R"(--[[
a@1
)";


    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            CHECK(frag.result == std::nullopt);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "no_recs_for_comments_simple")
{
    const std::string source = R"(
-- sel
-- retur
-- fo
-- if @1
-- end
-- the
)";
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            CHECK(frag.result == std::nullopt);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "no_recs_for_comments_blocks")
{
    const std::string source = R"(
--[[
comment 1
@1]]@2 local
-- [[ comment 2]]
--
-- sdfsdfsdf
--[[comment 3]]
--[[  @3
foo
@4bar
baz
]]
)";
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            CHECK(frag.result == std::nullopt);
        }
    );

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '2',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            CHECK(!frag.result->acResults.entryMap.empty());
        }
    );

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '3',
        [](FragmentAutocompleteStatusResult& frag)
        {
            CHECK(frag.result == std::nullopt);
        }
    );

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '4',
        [](FragmentAutocompleteStatusResult& frag)
        {
            CHECK(frag.result == std::nullopt);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "no_recs_for_comments")
{
    const std::string source = R"(
-- sel @1
-- retur @2
-- fo @3
--[[ sel @4]]
local @5 -- hell@6o
)";
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            CHECK(frag.result == std::nullopt);
        }
    );

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '2',
        [](FragmentAutocompleteStatusResult& frag)
        {
            CHECK(frag.result == std::nullopt);
        }
    );

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '3',
        [](FragmentAutocompleteStatusResult& frag)
        {
            CHECK(frag.result == std::nullopt);
        }
    );

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '4',
        [](FragmentAutocompleteStatusResult& frag)
        {
            CHECK(frag.result == std::nullopt);
        }
    );

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '5',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            CHECK(!frag.result->acResults.entryMap.empty());
        }
    );

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '6',
        [](FragmentAutocompleteStatusResult& frag)
        {
            CHECK(frag.result == std::nullopt);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "no_recs_for_comments_in_incremental_fragment")
{
    const std::string source = R"(
local x = 5
if x == 5
)";
    const std::string updated = R"(
local x = 5
if x == 5 then -- a comment @1
)";
    autocompleteFragmentInBothSolvers(
        source,
        updated,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            CHECK(frag.result == std::nullopt);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "fragment_autocomplete_handles_parse_errors")
{

    ScopedFastInt sfi{FInt::LuauParseErrorLimit, 1};
    const std::string source = R"(

)";
    const std::string updated = R"(
type A = <>random non code text here  @1
)";

    autocompleteFragmentInBothSolvers(
        source,
        updated,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            CHECK(frag.result->acResults.entryMap.empty());
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "require_tracing")
{
    fileResolver.source["MainModule/A"] = R"(
return { x = 0 }
    )";

    fileResolver.source["MainModule"] = R"(
local result = require(script.A)
local x = 1 + result.@1
    )";

    autocompleteFragmentInBothSolvers(
        fileResolver.source["MainModule"],
        fileResolver.source["MainModule"],
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            CHECK(frag.result->acResults.entryMap.size() == 1);
            CHECK(frag.result->acResults.entryMap.count("x"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "fragment_ac_must_traverse_typeof_and_not_ice")
{
    // This test ensures that we traverse typeof expressions for defs that are being referred to in the fragment
    // In this case, we want to ensure we populate the incremental environment with the reference to `m`
    // Without this, we would ice as we will refer to the local `m` before it's declaration
    const std::string source = R"(
--!strict
local m = {}
-- and here
function m:m1() end
type nt = typeof(m)

return m
)";
    const std::string updated = R"(
--!strict
local m = {}
-- and here
function m:m1() end
type nt = typeof(m)
l @1
return m
)";

    autocompleteFragmentInBothSolvers(source, updated, '1', [](FragmentAutocompleteStatusResult& _) {});
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "duped_alias")
{
    const std::string source = R"(
type a = typeof({})

)";
    const std::string dest = R"(
type a = typeof({})
type a = typeof({})@1
)";

    // Re-parsing and typechecking a type alias in the fragment that was defined in the base module will assert in ConstraintGenerator::checkAliases
    // unless we don't clone it This will let the incremental pass re-generate the type binding, and we will expect to see it in the type bindings
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result);
            Scope* sc = frag.result->freshScope;
            CHECK(1 == sc->privateTypeBindings.count("a"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "mutually_recursive_alias")
{
    const std::string source = R"(
type U = {f : number, g : U}

)";
    const std::string dest = R"(
type U = {f : number, g : V}
type V = {h : number, i : U?} @1
)";

    // Re-parsing and typechecking a type alias in the fragment that was defined in the base module will assert in ConstraintGenerator::checkAliases
    // unless we don't clone it This will let the incremental pass re-generate the type binding, and we will expect to see it in the type bindings
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& frag)
        {
            REQUIRE(frag.result->freshScope);
            Scope* scope = frag.result->freshScope;
            CHECK(1 == scope->privateTypeBindings.count("U"));
            CHECK(1 == scope->privateTypeBindings.count("V"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "generalization_crash_when_old_solver_freetypes_have_no_bounds_set")
{
    const std::string source = R"(
local UserInputService = game:GetService("UserInputService");

local Camera = workspace.CurrentCamera;

UserInputService.InputBegan:Connect(function(Input)
    if (Input.KeyCode == Enum.KeyCode.One) then
        local Up = Input.Foo
        local Vector = -(Up:Unit)
    end
end)
)";

    const std::string dest = R"(
local UserInputService = game:GetService("UserInputService");

local Camera = workspace.CurrentCamera;

UserInputService.InputBegan:Connect(function(Input)
    if (Input.KeyCode == Enum.KeyCode.One) then
        local Up = Input.Foo
        local Vector = -(Up:Unit()) @1
    end
end)
)";

    autocompleteFragmentInBothSolvers(source, dest, '1', [](FragmentAutocompleteStatusResult& _) {});
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "fragment_autocomplete_ensures_memory_isolation")
{
    ToStringOptions opt;
    opt.exhaustive = true;
    opt.exhaustive = true;
    opt.functionTypeArguments = true;
    opt.maxTableLength = 0;
    opt.maxTypeLength = 0;

    auto checkAndExamine = [&](const std::string& src, const std::string& idName, const std::string& idString)
    {
        checkWithOptions(src);
        auto id = getType(idName, true);
        LUAU_ASSERT(id);
        CHECK_EQ(Luau::toString(*id, opt), idString);
    };

    auto getTypeFromModule = [](ModulePtr module, const std::string& name) -> std::optional<TypeId>
    {
        if (!module->hasModuleScope())
            return std::nullopt;
        return lookupName(module->getModuleScope(), name);
    };

    auto fragmentACAndCheck = [&](const std::string& updated, const Position& pos, const std::string& idName)
    {
        FragmentAutocompleteStatusResult frag = autocompleteFragment(updated, pos, std::nullopt);
        REQUIRE(frag.result);
        auto fragId = getTypeFromModule(frag.result->incrementalModule, idName);
        LUAU_ASSERT(fragId);

        auto srcId = getType(idName, true);
        LUAU_ASSERT(srcId);

        CHECK((*fragId)->owningArena != (*srcId)->owningArena);
        CHECK(&(frag.result->incrementalModule->internalTypes) == (*fragId)->owningArena);
    };

    const std::string source = R"(local module = {}
f
return module)";

    const std::string updated1 = R"(local module = {}
function module.a
return module)";

    const std::string updated2 = R"(local module = {}
function module.ab
return module)";

    {
        ScopedFastFlag sff{FFlag::LuauSolverV2, false};
        getFrontend().setLuauSolverMode(SolverMode::Old);
        checkAndExamine(source, "module", "{|  |}");
        // [TODO] CLI-140762 we shouldn't mutate stale module in autocompleteFragment
        // early return since the following checking will fail, which it shouldn't!
        fragmentACAndCheck(updated1, Position{1, 17}, "module");
        fragmentACAndCheck(updated2, Position{1, 18}, "module");
    }

    {
        ScopedFastFlag sff{FFlag::LuauSolverV2, true};
        getFrontend().setLuauSolverMode(SolverMode::New);
        checkAndExamine(source, "module", "{  }");
        // [TODO] CLI-140762 we shouldn't mutate stale module in autocompleteFragment
        // early return since the following checking will fail, which it shouldn't!
        fragmentACAndCheck(updated1, Position{1, 17}, "module");
        fragmentACAndCheck(updated2, Position{1, 18}, "module");
    }
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "fragment_autocomplete_shouldnt_crash_on_cross_module_mutation")
{
    const std::string source = R"(local module = {}
function module.
return module
)";

    const std::string updated = R"(local module = {}
function module.f@1
return module
)";

    autocompleteFragmentInBothSolvers(source, updated, '1', [](FragmentAutocompleteStatusResult& result) {});
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "ice_caused_by_mixed_mode_use")
{
    const std::string source =
        std::string("--[[\n\tPackage link auto-generated by Rotriever\n]]\nlocal PackageIndex = script.Parent._Index\n\nlocal Package = ") +
        "require(PackageIndex[\"ReactOtter\"][\"ReactOtter\"])\n\nexport type Goal = Package.Goal\nexport type SpringOptions " +
        "= Package.SpringOptions\n\n\nreturn Pa@1";
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& _) {

        }
    );
    autocompleteFragmentInBothSolvers(source, source, '1', [](auto& _) {});
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "free_type_in_old_solver_shouldnt_trigger_not_null_assertion")
{

    const std::string source = R"(--!strict
local foo
local a, z = foo()

local e = foo().x

local f = foo().y

z
)";

    const std::string dest = R"(--!strict
local foo
local a, z = foo()

local e = foo().x

local f = foo().y

z:a@1
)";

    autocompleteFragmentInBothSolvers(source, dest, '1', [](FragmentAutocompleteStatusResult& _) {});
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "interior_free_types_assertion_caused_by_free_type_inheriting_null_scope_from_table")
{
    const std::string source = R"(--!strict
local foo
local a = foo()

local e = foo().x

local f = foo().y


)";

    const std::string dest = R"(--!strict
local foo
local a = foo()

local e = foo().x

local f = foo().y

z = a.P.E@1
)";

    autocompleteFragmentInBothSolvers(source, dest, '1', [](FragmentAutocompleteStatusResult& _) {});
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "NotNull_nil_scope_assertion_caused_by_free_type_inheriting_null_scope_from_table")
{
    const std::string source = R"(--!strict
local foo
local a = foo()

local e = foo().x

local f = foo().y


)";

    const std::string dest = R"(--!strict
local foo
local a = foo()

local e = foo().x

local f = foo().y

z = a.P.E@1
)";

    autocompleteFragmentInBothSolvers(source, dest, '1', [](FragmentAutocompleteStatusResult& _) {});
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "user_defined_type_function_local")
{
    const std::string source = R"(--!strict
type function foo(x: type): type
    if x.tag == "singleton" then
        local t = x:value()

        return types.unionof(types.singleton(t), types.singleton(nil))
    end

    return types.number
end
)";

    const std::string dest = R"(--!strict
type function foo(x: type): type
    if x.tag == "singleton" then
        local t = x:value()
        x
        return types.unionof(types.singleton(t), types.singleton(nil))
    end

    return types.number
end
)";

    // Only checking in new solver as old solver doesn't handle type functions and constraint solver will ICE
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
    this->check(source, getOptions());

    FragmentAutocompleteStatusResult result = autocompleteFragment(dest, Position{4, 9}, std::nullopt);
    CHECK(result.status != FragmentAutocompleteStatus::InternalIce);
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "for_loop_recommends")
{
    const std::string source = R"(
local testArr: {{a: number, b: number}} = {
{a = 1, b = 2},
{a = 2, b = 4},
}

for _, v in testArr do

end
)";

    const std::string dest = R"(
local testArr: {{a: number, b: number}} = {
{a = 1, b = 2},
{a = 2, b = 4},
}

for _, v in testArr do
    print(v.@1
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& result)
        {
            CHECK(result.status != FragmentAutocompleteStatus::InternalIce);
            CHECK(result.result);
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("a"));
            CHECK(result.result->acResults.entryMap.count("b"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "for_loop_recommends")
{
    const std::string source = R"(
local testArr: {string} = {
"a",
"b",
}

for _, v in testArr do

end
)";

    const std::string dest = R"(
local testArr: {string} = {
"a",
"b",
}

for _, v in testArr do
    print(v:@1)
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& result)
        {
            CHECK(result.status != FragmentAutocompleteStatus::InternalIce);
            CHECK(result.result);
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("upper"));
            CHECK(result.result->acResults.entryMap.count("sub"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "expr_function")
{
    const std::string source = R"(
local t = {}
type Input = {x : string}
function t.Do(fn : (Input) -> ())
    if t.x == "a" then
        return
    end
end

t.Do(function (f)
    f
end)
)";

    const std::string dest = R"(
local t = {}
type Input = {x : string}
function t.Do(fn : (Input) -> ())
    if t.x == "a" then
        return
    end
end

t.Do(function (f)
    f.@1
end)
)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& status)
        {
            CHECK(FragmentAutocompleteStatus::Success == status.status);
            REQUIRE(status.result);
            CHECK(!status.result->acResults.entryMap.empty());
            CHECK(status.result->acResults.entryMap.count("x"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "differ_1")
{
    const std::string source = R"()";
    const std::string dest = R"(local tbl = { foo = 1, bar = 2 };
tbl.b@1)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& status)
        {
            CHECK(FragmentAutocompleteStatus::Success == status.status);
            REQUIRE(status.result);
            CHECK(!status.result->acResults.entryMap.empty());
            CHECK(status.result->acResults.entryMap.count("foo"));
            CHECK(status.result->acResults.entryMap.count("bar"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "block_diff_test_both_empty")
{
    SourceModule stale;
    SourceModule fresh;
    ParseResult o = parseHelper_(stale, R"()");
    ParseResult n = parseHelper_(stale, R"()");
    auto pos = Luau::blockDiffStart(o.root, n.root, nullptr);
    CHECK(pos == std::nullopt);
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "block_diff_test_both_empty_e2e")
{
    const std::string source = R"(@1)";

    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](FragmentAutocompleteStatusResult& result)
        {
            CHECK(FragmentAutocompleteStatus::Success == result.status);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "block_diff_added_locals_1")
{
    SourceModule stale;
    SourceModule fresh;
    ParseResult o = parseHelper_(stale, R"()");
    ParseResult n = parseHelper_(fresh, R"(local x = 4
local y = 3
local z = 3)");
    auto pos = Luau::blockDiffStart(o.root, n.root, n.root->body.data[2]);
    REQUIRE(pos);
    CHECK(*pos == Position{0, 0});
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "block_diff_added_locals_1_e2e")
{
    const std::string source = R"()";
    const std::string dest = R"(local f1 = 4
local f2 = "a"
local f3 = f@1
)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& result)
        {
            CHECK(FragmentAutocompleteStatus::Success == result.status);
            REQUIRE(result.result);
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("f1"));
            CHECK(result.result->acResults.entryMap.count("f2"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "block_diff_added_locals_1_e2e_in_the_middle")
{
    const std::string source = R"()";
    const std::string dest = R"(local f1 = 4
local f2 = f@1
local f3 = f
)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& result)
        {
            CHECK(FragmentAutocompleteStatus::Success == result.status);
            REQUIRE(result.result);
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("f1"));
            CHECK(!result.result->acResults.entryMap.count("f3"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "block_diff_added_locals_2")
{
    SourceModule stale;
    SourceModule fresh;
    ParseResult o = parseHelper_(stale, R"(local x = 4)");
    ParseResult n = parseHelper_(fresh, R"(local x = 4
local y = 3
local z = 3)");
    auto pos = Luau::blockDiffStart(o.root, n.root, n.root->body.data[1]);
    REQUIRE(pos);
    CHECK(*pos == Position{1, 0});
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "block_diff_added_locals_3")
{
    SourceModule stale;
    SourceModule fresh;
    ParseResult o = parseHelper_(stale, R"(local x = 4
local y = 2 + 1)");
    ParseResult n = parseHelper_(fresh, R"(local x = 4
local y = 3
local z = 3
local foo = 8)");
    auto pos = Luau::blockDiffStart(o.root, n.root, n.root->body.data[3]);
    REQUIRE(pos);
    CHECK(*pos == Position{1, 0});
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "block_diff_added_locals_3")
{
    const std::string source = R"(local f1 = 4
local f2 = 2 + 1)";
    const std::string dest = R"(local f1 = 4
local f2 = 3
local f3 = 3
local foo = 8 + @1)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& result)
        {
            CHECK(FragmentAutocompleteStatus::Success == result.status);
            REQUIRE(result.result);
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("f1"));
            CHECK(result.result->acResults.entryMap.count("f2"));
            CHECK(result.result->acResults.entryMap.count("f3"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "block_diff_added_locals_fake_similarity")
{
    // Captures the bad behaviour of block based diffs
    SourceModule stale;
    SourceModule fresh;
    ParseResult o = parseHelper_(stale, R"(local x = 4
local y = true
local z = 2 + 1)");
    ParseResult n = parseHelper_(fresh, R"(local x = 4
local y = "tr"
local z = 3
local foo = 8)");
    auto pos = Luau::blockDiffStart(o.root, n.root, n.root->body.data[2]);
    REQUIRE(pos);
    CHECK(*pos == Position{2, 0});
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "TypeCorrectLocalReturn_assert")
{
    const std::string source = R"()";
    const std::string dest = R"(local function target(a: number, b: string) return a + #b end
local function bar1(a: string) reutrn a .. 'x' end
local function bar2(a: number) return -a end
return target(bar@1)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& status)
        {
            CHECK(FragmentAutocompleteStatus::Success == status.status);
            REQUIRE(status.result);
            CHECK(!status.result->acResults.entryMap.empty());
            CHECK(status.result->acResults.entryMap.count("bar1"));
            CHECK(status.result->acResults.entryMap.count("bar2"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "TypeCorrectLocalRank_assert")
{
    const std::string source = R"()";
    const std::string dest = R"(local function target(a: number, b: string) return a + #b end
local bar1 = 'hello'
local bar2 = 4
return target(bar@1)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& status)
        {
            CHECK(FragmentAutocompleteStatus::Success == status.status);
            REQUIRE(status.result);
            CHECK(!status.result->acResults.entryMap.empty());
            CHECK(status.result->acResults.entryMap.count("bar1"));
            CHECK(status.result->acResults.entryMap.count("bar2"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "str_metata_table_finished_defining")
{
    const std::string source = R"(local function foobar(): string return "" end
local foo = f)";
    const std::string dest = R"(local function foobar(): string return "" end
local foo = foobar()
foo:@1)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& res)
        {
            CHECK(FragmentAutocompleteStatus::Success == res.status);
            REQUIRE(res.result);
            CHECK(!res.result->acResults.entryMap.empty());
            CHECK(res.result->acResults.entryMap.count("len"));
            CHECK(res.result->acResults.entryMap.count("gsub"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "str_metata_table_redef")
{
    const std::string source = R"(local x = 42)";
    const std::string dest = R"(local x = 42
local x = ""
x:@1)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& res)
        {
            CHECK(FragmentAutocompleteStatus::Success == res.status);
            REQUIRE(res.result);
            CHECK(!res.result->acResults.entryMap.empty());
            CHECK(res.result->acResults.entryMap.count("len"));
            CHECK(res.result->acResults.entryMap.count("gsub"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "diff_multiple_blocks_on_same_line")
{
    const std::string source = R"(
do local function foo() end; local x = ""; end do local function bar() end)";
    const std::string dest = R"(
do local function foo() end; local x = ""; end do local function bar() end local x = {a : number}; b @1end )";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& res)
        {
            CHECK(FragmentAutocompleteStatus::Success == res.status);
            REQUIRE(res.result);
            CHECK(!res.result->acResults.entryMap.empty());
            CHECK(res.result->acResults.entryMap.count("bar"));
            CHECK(!res.result->acResults.entryMap.count("foo"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "nested_blocks_else_simple")
{
    const std::string source = R"(
local function foo(t : {foo : string})
    local x = t.foo
    do
        if t then
        end
    end
end
)";
    const std::string dest = R"(
local function foo(t : {foo : string})
    local x = t.foo
    do
        if t then
            x:@1
        end
    end
end
)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& res)
        {
            CHECK(FragmentAutocompleteStatus::Success == res.status);
            REQUIRE(res.result);
            CHECK(!res.result->acResults.entryMap.empty());
            CHECK(res.result->acResults.entryMap.count("gsub"));
            CHECK(res.result->acResults.entryMap.count("len"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "nested_blocks_else_difficult_2")
{
    const std::string source = R"(
local function foo(t : {foo : number})
    do
        if t then
        end
    end
end
)";
    const std::string dest = R"(
local function foo(t : {foo : number})
    do
        if t then
        else
            local x = 4
            return x + t@1.
        end
    end
end
)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& res)
        {
            CHECK(FragmentAutocompleteStatus::Success == res.status);
            REQUIRE(res.result);
            CHECK(!res.result->acResults.entryMap.empty());
            CHECK(res.result->acResults.entryMap.count("foo"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "NotNull_assertion_caused_by_leaking_free_type_from_stale_module")
{
    const std::string source = R"(
local Players = game:GetService("Players")

Players.PlayerAdded:Connect(function(Player)
    for_,v in script.PlayerValue:GetChildren()do
        v
    end
end)
)";

    const std::string dest = R"(
local Players = game:GetService("Players")

Players.PlayerAdded:Connect(function(Player)
    for_,v in script.PlayerValue:GetChildren()do
        v:L@1
    end
end)
)";
    autocompleteFragmentInBothSolvers(source, dest, '1', [](auto& result) {});
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_cond_no_then_recs_then")
{
    const std::string source = R"(

    )";

    const std::string dest = R"(
if x t@1
    )";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            REQUIRE(result.result);
            CHECK(result.result->acResults.entryMap.count("then"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_then_recs_else")
{
    const std::string source = R"(
if x then

end
    )";

    const std::string dest = R"(
if x then
e@1
end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            REQUIRE(result.result);
            CHECK(result.result->acResults.entryMap.count("else"));
            CHECK(result.result->acResults.entryMap.count("elseif"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_else_if_table_prop_recs_no_then")
{
    const std::string source = R"(
type T = {xa : number, y : number}
local t : T = {xa = 3, y = 3}

if t.x then
elseif
end
)";

    const std::string dest = R"(
type T = {xa : number, y : number}
local t : T = {xa = 3, y = 3}

if t.x then
elseif t.xa t@1
end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            REQUIRE(result.result);
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(!result.result->acResults.entryMap.count("xa"));
            CHECK(!result.result->acResults.entryMap.count("y"));
            CHECK(result.result->acResults.entryMap.count("then"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "if_else_if_table_prop_recs_with_then")
{
    const std::string source = R"(
type T = {xa : number, y : number}
local t : T = {xa = 3, y = 3}

if t.x then
elseif  then
end
)";

    const std::string dest = R"(
type T = {xa : number, y : number}
local t : T = {xa = 3, y = 3}

if t.x then
elseif t.@1  then
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            REQUIRE(result.result);
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("xa"));
            CHECK(result.result->acResults.entryMap.count("y"));
            CHECK(!result.result->acResults.entryMap.count("then"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "tagged_union_completion_first_branch_of_union_old_solver")
{
    const std::string source = R"(
type Ok<T> = { type: "ok", value: T}
type Err<E> = { type : "err", error : E}
type Result<T,E> = Ok<T> | Err<E>

local result = {} :: Result<number, string>

if result.type == "ok" then

end
)";

    const std::string dest = R"(
type Ok<T> = { type: "ok", value: T}
type Err<E> = { type : "err", error : E}
type Result<T,E> = Ok<T> | Err<E>

local result = {} :: Result<number, string>

if result.type == "ok" then
    result.@1
end
)";
    autocompleteFragmentInOldSolver(
        source,
        dest,
        '1',
        [](auto& result)
        {
            REQUIRE(result.result);
            CHECK_EQ(result.result->acResults.entryMap.count("type"), 1);
            CHECK_EQ(result.result->acResults.entryMap.count("value"), 1);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "tagged_union_completion_second_branch_of_union_old_solver")
{
    const std::string source = R"(
type Ok<T> = { type: "ok", value: T}
type Err<E> = { type : "err", error : E}
type Result<T,E> = Ok<T> | Err<E>

local result = {} :: Result<number, string>

if result.type == "err" then

end
)";

    const std::string dest = R"(
type Ok<T> = { type: "ok", value: T}
type Err<E> = { type : "err", error : E}
type Result<T,E> = Ok<T> | Err<E>

local result = {} :: Result<number, string>

if result.type == "err" then
    result.@1
end
)";

    autocompleteFragmentInOldSolver(
        source,
        dest,
        '1',
        [](auto& result)
        {
            REQUIRE(result.result);
            CHECK_EQ(result.result->acResults.entryMap.count("type"), 1);
            CHECK_EQ(result.result->acResults.entryMap.count("error"), 1);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "tagged_union_completion_first_branch_of_union_new_solver")
{
    // TODO: CLI-155619 - Fragment autocomplete needs to use stale refinement information for modules typechecked in the new solver as well
    const std::string source = R"(
type Ok<T> = { type: "ok", value: T}
type Err<E> = { type : "err", error : E}
type Result<T,E> = Ok<T> | Err<E>

local result = {} :: Result<number, string>

if result.type == "ok" then

end
)";

    const std::string dest = R"(
type Ok<T> = { type: "ok", value: T}
type Err<E> = { type : "err", error : E}
type Result<T,E> = Ok<T> | Err<E>

local result = {} :: Result<number, string>

if result.type == "ok" then
    result.@1
end
)";
    autocompleteFragmentInNewSolver(
        source,
        dest,
        '1',
        [](auto& result)
        {
            REQUIRE(result.result);
            CHECK_EQ(result.result->acResults.entryMap.count("type"), 1);
            CHECK_EQ(result.result->acResults.entryMap.count("value"), 1);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "tagged_union_completion_second_branch_of_union_new_solver")
{
    const std::string source = R"(
type Ok<T> = { type: "ok", value: T}
type Err<E> = { type : "err", error : E}
type Result<T,E> = Ok<T> | Err<E>

local result = {} :: Result<number, string>

if result.type == "err" then

end
)";

    const std::string dest = R"(
type Ok<T> = { type: "ok", value: T}
type Err<E> = { type : "err", error : E}
type Result<T,E> = Ok<T> | Err<E>

local result = {} :: Result<number, string>

if result.type == "err" then
    result.@1
end
)";

    autocompleteFragmentInNewSolver(
        source,
        dest,
        '1',
        [](auto& result)
        {
            REQUIRE(result.result);
            CHECK_EQ(result.result->acResults.entryMap.count("type"), 1);
            CHECK_EQ(result.result->acResults.entryMap.count("error"), 1);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "inline_prop_read_on_requires_provides_results")
{
    ScopedFastFlag sff{FFlag::LuauFragmentRequiresCanBeResolvedToAModule, true};
    const std::string moduleA = R"(
local mod = { prop1 = true}
mod.prop2 = "a"
function mod.foo(a: number)
    return a
end
return mod
)";

    const std::string mainModule = R"(

)";

    fileResolver.source["MainModule"] = mainModule;
    fileResolver.source["MainModule/A"] = moduleA;
    getFrontend().check("MainModule/A", getOptions());
    getFrontend().check("MainModule", getOptions());

    const std::string updatedMain = R"(
require(script.A).
)";

    auto result = autocompleteFragment(updatedMain, Position{1, 18});
    CHECK(!result.result->acResults.entryMap.empty());
    CHECK(result.result->acResults.entryMap.count("prop1"));
    CHECK(result.result->acResults.entryMap.count("prop2"));
    CHECK(result.result->acResults.entryMap.count("foo"));
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "self_types_provide_rich_autocomplete")
{
    const std::string source = R"(
type Service = {
    Start: (self: Service) -> (),
    Prop: number
}

local Service: Service = {}

function Service:Start()

end
)";
    const std::string dest = R"(
type Service = {
    Start: (self: Service) -> (),
    Prop: number
}

local Service: Service = {}

function Service:Start()
    self.@1
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("Prop"));
            CHECK(result.result->acResults.entryMap.count("Start"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "self_with_fancy_metatable_setting_new_solver")
{
    const std::string source = R"(
        type IAccount = {
            __index: IAccount,
            new : (string, number) -> Account,
            report: (self: Account) -> (),
        }

        export type Account = setmetatable<{
            name: string,
            balance: number
        }, IAccount>;

        local Account = {} :: IAccount
        Account.__index = Account

        function Account.new(name, balance): Account
            local self = {}
            self.name = name
            self.balance = balance
            return setmetatable(self, Account)
        end

        function Account:report()
            print("My balance is: " .. )
        end
)";

    const std::string dest = R"(
        type IAccount = {
            __index: IAccount,
            new : (string, number) -> Account,
            report: (self: Account) -> (),
        }

        export type Account = setmetatable<{
            name: string,
            balance: number
        }, IAccount>;

        local Account = {} :: IAccount
        Account.__index = Account

        function Account.new(name, balance): Account
            local self = {}
            self.name = name
            self.balance = balance
            return setmetatable(self, Account)
        end

        function Account:report()
            print("My balance is: " .. self.@1 )
        end
)";

    autocompleteFragmentInNewSolver(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("new"));
            CHECK(result.result->acResults.entryMap.count("report"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "self_with_colon_good_recommendations")
{
    const std::string source = R"(
type Service = {
    Start: (self: Service) -> (),
    Prop: number
}

local Service: Service = {}

function Service:Start()

end
)";
    const std::string dest = R"(
type Service = {
    Start: (self: Service) -> (),
    Prop: number
}

local Service: Service = {}

function Service:Start()
    self:@1
end
)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("Prop"));
            CHECK(result.result->acResults.entryMap["Prop"].wrongIndexType);
            CHECK(result.result->acResults.entryMap.count("Start"));
            CHECK(!result.result->acResults.entryMap["Start"].wrongIndexType);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "string_interpolation_format_provides_autocomplete_results")
{
    const std::string source = R"(
type Foo = {x : number, x1 : string, x2 : boolean}
local e: Foo = {x = 1, x1 = "1", x2 = true}
local s =
)";

    const std::string dest = R"(
type Foo = {x : number, x1 : string, x2 : boolean}
local e : Foo = {x = 1, x1 = "1", x2 = true}
local s = `{e.@1 }`
)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("x"));
            CHECK(result.result->acResults.entryMap.count("x1"));
            CHECK(result.result->acResults.entryMap.count("x2"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "string_interpolation_format_provides_results_inside_of_function_call")
{
    ScopedFastFlag _{FFlag::LuauParseIncompleteInterpStringsWithLocation, true};
    const std::string source = R"(
type T = {x : number, y : number, z : number}
local e = {x = 1, y = 2, z = 3}
print(`{e.x}`)
)";
    const std::string dest = R"(
type T = {x : number, y : number, z : number}
local e = {x = 1, y = 2, z = 3}
print(`{e.x} {e.@1}`)
)";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("x"));
            CHECK(result.result->acResults.entryMap.count("y"));
            CHECK(result.result->acResults.entryMap.count("z"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "for_in_should_rec")
{
    ScopedFastFlag sff{FFlag::LuauForInProvidesRecommendations, true};
    const std::string source = R"(
type T = { x : {[number] : number}, y: number}
local x : T = ({} :: T)
for _,n in pairs(x.@1) do
end
)";
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("x"));
            CHECK(result.result->acResults.entryMap.count("y"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "for_expr_in_should_rec_no_do")
{
    ScopedFastFlag sff{FFlag::LuauForInProvidesRecommendations, true};
    const std::string source = R"(
type T = { x : {[number] : number}, y: number, z: number}
local x : T = ({} :: T)
for i =
end
)";
    const std::string dest = R"(
type T = { x : {[number] : number}, y: number, z : number}
local x : T = ({} :: T)
for i = x.@1
end
)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("x"));
            CHECK(result.result->acResults.entryMap.count("y"));
            CHECK(result.result->acResults.entryMap.count("z"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "for_expr_in_should_rec_with_do_in_step")
{
    ScopedFastFlag sff{FFlag::LuauForInProvidesRecommendations, true};
    const std::string source = R"(
type T = { x : {[number] : number}, y: number, z: number}
local x : T = ({} :: T)
for i = x.y, 100 do
end
)";
    const std::string dest = R"(
type T = { x : {[number] : number}, y: number, z : number}
local x : T = ({} :: T)
for i = x.y, 100, x.@1 do
end
)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("x"));
            CHECK(result.result->acResults.entryMap.count("y"));
            CHECK(result.result->acResults.entryMap.count("z"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "for_expr_in_should_rec_with_do_in_max_delete")
{
    ScopedFastFlag sff{FFlag::LuauForInProvidesRecommendations, true};
    const std::string source = R"(
type T = { x : {[number] : number}, y: number, z: number}
local x : T = ({} :: T)
for i = x.y, x.z do
end
)";
    const std::string dest = R"(
type T = { x : {[number] : number}, y: number, z : number}
local x : T = ({} :: T)
for i = x.y, x.@1 do
end
)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("x"));
            CHECK(result.result->acResults.entryMap.count("y"));
            CHECK(result.result->acResults.entryMap.count("z"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "for_expr_in_should_rec_with_do_in_max_add")
{
    ScopedFastFlag sff{FFlag::LuauForInProvidesRecommendations, true};
    const std::string source = R"(
type T = { x : {[number] : number}, y: number, z: number}
local x : T = ({} :: T)
for i = x.y do
end
)";
    const std::string dest = R"(
type T = { x : {[number] : number}, y: number, z : number}
local x : T = ({} :: T)
for i = x.y, x.@1 do
end
)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("x"));
            CHECK(result.result->acResults.entryMap.count("y"));
            CHECK(result.result->acResults.entryMap.count("z"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "correctly_grab_innermost_refinement")
{
    ScopedFastFlag _{FFlag::LuauFragmentAutocompleteTakesInnermostRefinement, true};

    const std::string source = R"(
--!strict
type Type1 = { Type: "Type1", CommonKey: string, Type1Key: string }
type Type2 = { Type: "Type2", CommonKey: string, Type2Key: string }
type UnionType = Type1 | Type2

local foo: UnionType? = nil
if foo then
    if foo.Type == "Type2" then
    end
end
    )";

    const std::string dest = R"(
--!strict
type Type1 = { Type: "Type1", CommonKey: string, Type1Key: string }
type Type2 = { Type: "Type2", CommonKey: string, Type2Key: string }
type UnionType = Type1 | Type2

local foo: UnionType? = nil
if foo then
    if foo.Type == "Type2" then
        foo.@1
    end
end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](FragmentAutocompleteStatusResult& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("Type2Key") > 0);
            CHECK(result.result->acResults.entryMap.count("Type1Key") == 0);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "hot_comment_should_rec")
{
    ScopedFastFlag sff{FFlag::LuauSuggestHotComments, true};
    const std::string source = R"(--!@1)";
    autocompleteFragmentInBothSolvers(
        source,
        source,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("strict"));
            CHECK(result.result->acResults.entryMap.count("nonstrict"));
            CHECK(result.result->acResults.entryMap.count("nocheck"));
            CHECK(result.result->acResults.entryMap.count("native"));
            CHECK(result.result->acResults.entryMap.count("nolint"));
            CHECK(result.result->acResults.entryMap.count("optimize"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "len_operator_needs_to_provide_autocomplete_results")
{
    ScopedFastFlag sff{FFlag::LuauNumericUnaryOpsDontProduceNegationRefinements, true};
    std::string source = R"(
type Pool = { numbers: { number }}

local function foobar(p)
    local pool = p :: Pool
    if #pool
end
)";
    std::string dest = R"(
type Pool = { numbers: { number }}

local function foobar(p)
    local pool = p :: Pool
    if #pool.@1
end
)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("numbers"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "unary_minus_operator_needs_to_provide_autocomplete_results")
{
    ScopedFastFlag sff{FFlag::LuauNumericUnaryOpsDontProduceNegationRefinements, true};
    std::string source = R"(
type Pool = { x : number }

local function foobar(p)
    local pool = p :: Pool
    if -pool
end
)";
    std::string dest = R"(
type Pool = { x : number }

local function foobar(p)
    local pool = p :: Pool
    if -pool.@1
end
)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("x"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "method_in_unfinished_repeat_body_eof")
{
    ScopedFastFlag sff{FFlag::LuauUnfinishedRepeatAncestryFix, true};

    std::string source = R"(
local t = {}
function t:Foo() end
repeat)";

    std::string dest = R"(
local t = {}
function t:Foo() end
repeat
t:@1)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("Foo"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "method_in_unfinished_repeat_body_not_eof")
{
    ScopedFastFlag sff{FFlag::LuauUnfinishedRepeatAncestryFix, true};

    std::string source = R"(
local t = {}
function t:Foo() end
repeat
t

local function whatever() end
)";

    std::string dest = R"(
local t = {}
function t:Foo() end
repeat
t:@1

local function whatever() end
)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("Foo"));
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteBuiltinsFixture, "in_place_edit_of_for_loop_before_in_keyword_returns_fragment_starting_from_for")
{
    ScopedFastFlag sff{FFlag::LuauForInRangesConsiderInLocation, true};
    std::string source = R"(
local x = {}
for i, value in x do
    print(i)
end
)";

    std::string dest = R"(
local x = {}
for @1, value in x do
    print(i)
end
)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "bidirectionally_inferred_table_member")
{
    std::string source = R"(
type Foo = { foo1: string, bar1: number }
type Bar = { foo2: boolean, bar2: string }
type Baz = { foo3: number, bar3: boolean }

local X: Foo & Bar & Baz = {}
)";

    std::string dest = R"(
type Foo = { foo1: string, bar1: number }
type Bar = { foo2: boolean, bar2: string }
type Baz = { foo3: number, bar3: boolean }

local X: Foo & Bar & Baz = { f@1 }

)";
    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("foo1") > 0);
            CHECK(result.result->acResults.entryMap.count("foo2") > 0);
            CHECK(result.result->acResults.entryMap.count("foo3") > 0);
        }
    );
}

TEST_CASE_FIXTURE(FragmentAutocompleteFixture, "oss_1850")
{
    std::string source = R"(
type t = { name: "t", } | { name: "ts", person: "dog" }

local t:t
if t.name == "ts" then
end
    )";
    std::string dest = R"(
type t = { name: "t", } | { name: "ts", person: "dog" }

local t:t
if t.name == "ts" then
    t.@1
end
    )";

    autocompleteFragmentInBothSolvers(
        source,
        dest,
        '1',
        [](auto& result)
        {
            CHECK(!result.result->acResults.entryMap.empty());
            CHECK(result.result->acResults.entryMap.count("name") > 0);
            CHECK(result.result->acResults.entryMap.count("person") > 0);
        }
    );
}

// NOLINTEND(bugprone-unchecked-optional-access)

TEST_SUITE_END();
