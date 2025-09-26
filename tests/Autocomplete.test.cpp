// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Autocomplete.h"
#include "Luau/AutocompleteTypes.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"
#include "Luau/VisitType.h"
#include "Luau/StringUtils.h"


#include "ClassFixture.h"
#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

#include <map>

LUAU_DYNAMIC_FASTINT(LuauSubtypingRecursionLimit)

LUAU_FASTFLAG(LuauTraceTypesInNonstrictMode2)
LUAU_FASTFLAG(LuauSetMetatableDoesNotTimeTravel)
LUAU_FASTINT(LuauTypeInferRecursionLimit)
LUAU_FASTFLAG(LuauIncludeBreakContinueStatements)
LUAU_FASTFLAG(LuauSuggestHotComments)
LUAU_FASTFLAG(LuauUnfinishedRepeatAncestryFix)
LUAU_FASTFLAG(LuauParametrizedAttributeSyntax)
LUAU_FASTFLAG(LuauAutocompleteAttributes)

using namespace Luau;

static std::optional<AutocompleteEntryMap> nullCallback(std::string tag, std::optional<const ExternType*> ptr, std::optional<std::string> contents)
{
    return std::nullopt;
}

template<class BaseType>
struct ACFixtureImpl : BaseType
{
    ACFixtureImpl()
        : BaseType(true)
    {
    }

    AutocompleteResult autocomplete(unsigned row, unsigned column)
    {
        FrontendOptions opts;
        opts.forAutocomplete = true;
        opts.retainFullTypeGraphs = true;
        // NOTE: Autocomplete does *not* require strict checking, meaning we should
        // try to check all of these examples in `--!nocheck` mode.
        this->configResolver.defaultConfig.mode = Mode::NoCheck;
        this->getFrontend().check("MainModule", opts);

        return Luau::autocomplete(this->getFrontend(), "MainModule", Position{row, column}, nullCallback);
    }

    AutocompleteResult autocomplete(char marker, StringCompletionCallback callback = nullCallback)
    {
        FrontendOptions opts;
        opts.forAutocomplete = true;
        opts.retainFullTypeGraphs = true;
        // NOTE: Autocomplete does *not* require strict checking, meaning we should
        // try to check all of these examples in `--!nocheck` mode.
        this->configResolver.defaultConfig.mode = Mode::NoCheck;
        this->getFrontend().check("MainModule", opts);

        return Luau::autocomplete(this->getFrontend(), "MainModule", getPosition(marker), callback);
    }

    AutocompleteResult autocomplete(const ModuleName& name, Position pos, StringCompletionCallback callback = nullCallback)
    {
        FrontendOptions opts;
        opts.forAutocomplete = true;
        opts.retainFullTypeGraphs = true;
        // NOTE: Autocomplete does *not* require strict checking, meaning we should
        // try to check all of these examples in `--!nocheck` mode.
        this->configResolver.defaultConfig.mode = Mode::NoCheck;
        this->getFrontend().check(name, opts);

        return Luau::autocomplete(this->getFrontend(), name, pos, callback);
    }

    CheckResult check(const std::string& source)
    {
        this->getFrontend();
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

        // NOTE: Autocomplete does *not* require strict checking, meaning we should
        // try to check all of these examples in `--!nocheck` mode.
        return BaseType::check(Mode::NoCheck, filteredSource, std::nullopt);
    }

    LoadDefinitionFileResult loadDefinition(const std::string& source)
    {
        GlobalTypes& globals = this->getFrontend().globalsForAutocomplete;
        unfreeze(globals.globalTypes);
        LoadDefinitionFileResult result = this->getFrontend().loadDefinitionFile(
            globals, globals.globalScope, source, "@test", /* captureComments */ false, /* typeCheckForAutocomplete */ true
        );
        freeze(globals.globalTypes);

        if (FFlag::LuauSolverV2)
        {
            GlobalTypes& globals = this->getFrontend().globals;
            unfreeze(globals.globalTypes);
            LoadDefinitionFileResult result = this->getFrontend().loadDefinitionFile(
                globals, globals.globalScope, source, "@test", /* captureComments */ false, /* typeCheckForAutocomplete */ true
            );
            freeze(globals.globalTypes);
        }

        REQUIRE_MESSAGE(result.success, "loadDefinition: unable to load definition file");
        return result;
    }

    const Position& getPosition(char marker) const
    {
        auto i = markerPosition.find(marker);
        LUAU_ASSERT(i != markerPosition.end());
        return i->second;
    }
    // Maps a marker character (0-9 inclusive) to a position in the source code.
    std::map<char, Position> markerPosition;
};

struct ACFixture : ACFixtureImpl<Fixture>
{
    ACFixture()
        : ACFixtureImpl<Fixture>()
    {
    }

    Frontend& getFrontend() override
    {
        if (frontend)
            return *frontend;

        Frontend& f = Fixture::getFrontend();
        // TODO - move this into its own consructor
        addGlobalBinding(f.globals, "table", Binding{getBuiltins()->anyType});
        addGlobalBinding(f.globals, "math", Binding{getBuiltins()->anyType});
        addGlobalBinding(f.globalsForAutocomplete, "table", Binding{getBuiltins()->anyType});
        addGlobalBinding(f.globalsForAutocomplete, "math", Binding{getBuiltins()->anyType});
        return *frontend;
    }
};

struct ACBuiltinsFixture : ACFixtureImpl<BuiltinsFixture>
{
};

struct ACExternTypeFixture : ACFixtureImpl<ExternTypeFixture>
{
};

TEST_SUITE_BEGIN("AutocompleteTest");

TEST_CASE_FIXTURE(ACFixture, "empty_program")
{
    check(" @1");

    auto ac = autocomplete('1');

    CHECK(!ac.entryMap.empty());
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACFixture, "local_initializer")
{
    check("local a = @1");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
    CHECK_EQ(ac.context, AutocompleteContext::Expression);
}

TEST_CASE_FIXTURE(ACFixture, "leave_numbers_alone")
{
    check("local a = 3.@11");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.empty());
    CHECK_EQ(ac.context, AutocompleteContext::Unknown);
}

TEST_CASE_FIXTURE(ACFixture, "user_defined_globals")
{
    check("local myLocal = 4; @1");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("myLocal"));
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACFixture, "dont_suggest_local_before_its_definition")
{
    check(R"(
        local myLocal = 4
        function abc()
@1            local myInnerLocal = 1
@2
        end
@3    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("myLocal"));
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "myInnerLocal");

    ac = autocomplete('2');
    CHECK(ac.entryMap.count("myLocal"));
    CHECK(ac.entryMap.count("myInnerLocal"));

    ac = autocomplete('3');
    CHECK(ac.entryMap.count("myLocal"));
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "myInnerLocal");
}

TEST_CASE_FIXTURE(ACFixture, "recursive_function")
{
    check(R"(
        function foo()
@1        end
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("foo"));
    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACFixture, "nested_recursive_function")
{
    check(R"(
        local function outer()
            local function inner()
@1            end
        end
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("inner"));
    CHECK(ac.entryMap.count("outer"));
}

TEST_CASE_FIXTURE(ACFixture, "user_defined_local_functions_in_own_definition")
{
    check(R"(
        local function abc()
@1
        end
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("abc"));
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));

    check(R"(
        local abc = function()
@1
        end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("abc")); // FIXME: This is actually incorrect!
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "global_functions_are_not_scoped_lexically")
{
    check(R"(
        if true then
            function abc()

            end
        end
@1    )");

    auto ac = autocomplete('1');

    CHECK(!ac.entryMap.empty());
    CHECK(ac.entryMap.count("abc"));
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "local_functions_fall_out_of_scope")
{
    check(R"(
        if true then
            local function abc()

            end
        end
@1    )");

    auto ac = autocomplete('1');

    CHECK_NE(0, ac.entryMap.size());
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "abc");
}

TEST_CASE_FIXTURE(ACFixture, "function_parameters")
{
    check(R"(
        function abc(test)

@1        end
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("test"));
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "get_member_completions")
{
    check(R"(
        local a = table.@1
    )");

    auto ac = autocomplete('1');

    CHECK_EQ(17, ac.entryMap.size());
    CHECK(ac.entryMap.count("find"));
    CHECK(ac.entryMap.count("pack"));
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "math");
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "nested_member_completions")
{
    check(R"(
        local tbl = { abc = { def = 1234, egh = false } }
        tbl.abc. @1
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("def"));
    CHECK(ac.entryMap.count("egh"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "unsealed_table")
{
    check(R"(
        local tbl = {}
        tbl.prop = 5
        tbl.@1
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(1, ac.entryMap.size());
    CHECK(ac.entryMap.count("prop"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "unsealed_table_2")
{
    check(R"(
        local tbl = {}
        local inner = { prop = 5 }
        tbl.inner = inner
        tbl.inner. @1
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(1, ac.entryMap.size());
    CHECK(ac.entryMap.count("prop"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "cyclic_table")
{
    check(R"(
        local abc = {}
        local def = { abc = abc }
        abc.def = def
        abc.def. @1
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("abc"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "table_union")
{
    check(R"(
        type t1 = { a1 : string, b2 : number }
        type t2 = { b2 : string, c3 : string }
        function func(abc : t1 | t2)
            abc.  @1
        end
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(1, ac.entryMap.size());
    CHECK(ac.entryMap.count("b2"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "table_intersection")
{
    check(R"(
        type t1 = { a1 : string, b2 : number }
        type t2 = { b2 : number, c3 : string }
        function func(abc : t1 & t2)
            abc.  @1
        end
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(3, ac.entryMap.size());
    CHECK(ac.entryMap.count("a1"));
    CHECK(ac.entryMap.count("b2"));
    CHECK(ac.entryMap.count("c3"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "get_string_completions")
{
    check(R"(
        local a = ("foo"):@1
    )");

    auto ac = autocomplete('1');

    CHECK_EQ(17, ac.entryMap.size());
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "get_suggestions_for_new_statement")
{
    check("@1");

    auto ac = autocomplete('1');

    CHECK_NE(0, ac.entryMap.size());

    CHECK(ac.entryMap.count("table"));
    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACFixture, "get_suggestions_for_the_very_start_of_the_script")
{
    check(R"(@1

        function aaa() end
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("table"));
    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACFixture, "method_call_inside_function_body")
{
    check(R"(
        local game = { GetService=function(s) return 'hello' end }

        function a()
            game:  @1
        end
    )");

    auto ac = autocomplete('1');

    CHECK_NE(0, ac.entryMap.size());

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "math");
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "method_call_inside_if_conditional")
{
    check(R"(
        if table:  @1
    )");

    auto ac = autocomplete('1');

    CHECK_NE(0, ac.entryMap.size());
    CHECK(ac.entryMap.count("concat"));
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "math");
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "statement_between_two_statements")
{
    check(R"(
        function getmyscripts() end

        g@1

        getmyscripts()
    )");

    auto ac = autocomplete('1');

    CHECK_NE(0, ac.entryMap.size());

    CHECK(ac.entryMap.count("getmyscripts"));

    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACFixture, "bias_toward_inner_scope")
{
    check(R"(
        local A = {one=1}

        function B()
            local A = {two=2}

            A  @1
        end
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("A"));
    CHECK_EQ(ac.context, AutocompleteContext::Statement);

    TypeId t = follow(*ac.entryMap["A"].type);
    const TableType* tt = get<TableType>(t);
    REQUIRE(tt);

    CHECK(tt->props.count("two"));
}

TEST_CASE_FIXTURE(ACFixture, "recommend_statement_starting_keywords")
{
    check("@1");
    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("local"));
    CHECK_EQ(ac.context, AutocompleteContext::Statement);

    check("local i = @1");
    auto ac2 = autocomplete('1');
    CHECK(!ac2.entryMap.count("local"));
    CHECK_EQ(ac2.context, AutocompleteContext::Expression);
}

TEST_CASE_FIXTURE(ACFixture, "do_not_overwrite_context_sensitive_kws")
{
    check(R"(
        local function continue()
        end


@1    )");

    auto ac = autocomplete('1');

    AutocompleteEntry entry = ac.entryMap["continue"];
    CHECK(entry.kind == AutocompleteEntryKind::Binding);
    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACFixture, "dont_offer_any_suggestions_from_within_a_comment")
{
    check(R"(
        --!strict
        local foo = {}
        function foo:bar() end

        --[[
            foo:@1
        ]]
    )");

    auto ac = autocomplete('1');

    CHECK_EQ(0, ac.entryMap.size());
    CHECK_EQ(ac.context, AutocompleteContext::Unknown);
}

TEST_CASE_FIXTURE(ACFixture, "dont_offer_any_suggestions_from_within_a_broken_comment")
{
    check(R"(
        --[[ @1
    )");

    auto ac = autocomplete('1');

    CHECK_EQ(0, ac.entryMap.size());
    CHECK_EQ(ac.context, AutocompleteContext::Unknown);
}

TEST_CASE_FIXTURE(ACFixture, "dont_offer_any_suggestions_from_within_a_broken_comment_at_the_very_end_of_the_file")
{
    check("--[[@1");

    auto ac = autocomplete('1');
    CHECK_EQ(0, ac.entryMap.size());
    CHECK_EQ(ac.context, AutocompleteContext::Unknown);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_for_middle_keywords")
{
    check(R"(
        for x @1=
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.count("do"), 0);
    CHECK_EQ(ac1.entryMap.count("end"), 0);
    CHECK_EQ(ac1.context, AutocompleteContext::Unknown);

    check(R"(
        for x =@1 1
    )");

    auto ac2 = autocomplete('1');
    CHECK_EQ(ac2.entryMap.count("do"), 0);
    CHECK_EQ(ac2.entryMap.count("end"), 0);
    CHECK_EQ(ac2.context, AutocompleteContext::Unknown);

    check(R"(
        for x = 1,@1 2
    )");

    auto ac3 = autocomplete('1');
    CHECK_EQ(1, ac3.entryMap.size());
    CHECK_EQ(ac3.entryMap.count("do"), 1);
    CHECK_EQ(ac3.context, AutocompleteContext::Keyword);

    check(R"(
        for x = 1, @12,
    )");

    auto ac4 = autocomplete('1');
    CHECK_EQ(ac4.entryMap.count("do"), 0);
    CHECK_EQ(ac4.entryMap.count("end"), 0);
    CHECK_EQ(ac4.context, AutocompleteContext::Expression);

    check(R"(
        for x = 1, 2, @15
    )");

    auto ac5 = autocomplete('1');
    CHECK_EQ(ac5.entryMap.count("math"), 1);
    CHECK_EQ(ac5.entryMap.count("do"), 0);
    CHECK_EQ(ac5.entryMap.count("end"), 0);
    CHECK_EQ(ac5.context, AutocompleteContext::Expression);

    check(R"(
        for x = 1, 2, 5 f@1
    )");

    auto ac6 = autocomplete('1');
    CHECK_EQ(ac6.entryMap.size(), 1);
    CHECK_EQ(ac6.entryMap.count("do"), 1);
    CHECK_EQ(ac6.context, AutocompleteContext::Keyword);

    check(R"(
        for x = 1, 2, 5 do      @1
    )");

    auto ac7 = autocomplete('1');
    CHECK_EQ(ac7.entryMap.count("end"), 1);
    CHECK_EQ(ac7.context, AutocompleteContext::Statement);

    check(R"(local Foo = 1
        for x = @11, @22, @35
    )");

    for (int i = 0; i < 3; ++i)
    {
        auto ac8 = autocomplete('1' + i);
        CHECK_EQ(ac8.entryMap.count("Foo"), 1);
        CHECK_EQ(ac8.entryMap.count("do"), 0);
    }

    check(R"(local Foo = 1
        for x = @11, @22
    )");

    for (int i = 0; i < 2; ++i)
    {
        auto ac9 = autocomplete('1' + i);
        CHECK_EQ(ac9.entryMap.count("Foo"), 1);
        CHECK_EQ(ac9.entryMap.count("do"), 0);
    }
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_for_in_middle_keywords")
{
    check(R"(
        for @1
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(0, ac1.entryMap.size());
    CHECK_EQ(ac1.context, AutocompleteContext::Unknown);

    check(R"(
        for x@1 @2
    )");

    auto ac2 = autocomplete('1');
    CHECK_EQ(0, ac2.entryMap.size());
    CHECK_EQ(ac2.context, AutocompleteContext::Unknown);

    auto ac2a = autocomplete('2');
    CHECK_EQ(1, ac2a.entryMap.size());
    CHECK_EQ(1, ac2a.entryMap.count("in"));
    CHECK_EQ(ac2a.context, AutocompleteContext::Keyword);

    check(R"(
        for x in y@1
    )");

    auto ac3 = autocomplete('1');
    CHECK_EQ(ac3.entryMap.count("table"), 1);
    CHECK_EQ(ac3.entryMap.count("do"), 0);
    CHECK_EQ(ac3.context, AutocompleteContext::Expression);

    check(R"(
        for x in y @1
    )");

    auto ac4 = autocomplete('1');
    CHECK_EQ(ac4.entryMap.size(), 1);
    CHECK_EQ(ac4.entryMap.count("do"), 1);
    CHECK_EQ(ac4.context, AutocompleteContext::Keyword);

    check(R"(
        for x in f f@1
    )");

    auto ac5 = autocomplete('1');
    CHECK_EQ(ac5.entryMap.size(), 1);
    CHECK_EQ(ac5.entryMap.count("do"), 1);
    CHECK_EQ(ac5.context, AutocompleteContext::Keyword);

    check(R"(
        for x in y do  @1
    )");

    auto ac6 = autocomplete('1');
    CHECK_EQ(ac6.entryMap.count("in"), 0);
    CHECK_EQ(ac6.entryMap.count("table"), 1);
    CHECK_EQ(ac6.entryMap.count("end"), 1);
    CHECK_EQ(ac6.entryMap.count("function"), 1);
    CHECK_EQ(ac6.context, AutocompleteContext::Statement);

    check(R"(
        for x in y do e@1
    )");

    auto ac7 = autocomplete('1');
    CHECK_EQ(ac7.entryMap.count("in"), 0);
    CHECK_EQ(ac7.entryMap.count("table"), 1);
    CHECK_EQ(ac7.entryMap.count("end"), 1);
    CHECK_EQ(ac7.entryMap.count("function"), 1);
    CHECK_EQ(ac7.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_while_middle_keywords")
{
    check(R"(
        while@1
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.count("do"), 0);
    CHECK_EQ(ac1.entryMap.count("end"), 0);
    CHECK_EQ(ac1.context, AutocompleteContext::Expression);

    check(R"(
        while true @1
    )");

    auto ac2 = autocomplete('1');
    CHECK_EQ(3, ac2.entryMap.size());
    CHECK_EQ(ac2.entryMap.count("do"), 1);
    CHECK_EQ(ac2.entryMap.count("and"), 1);
    CHECK_EQ(ac2.entryMap.count("or"), 1);
    CHECK_EQ(ac2.context, AutocompleteContext::Keyword);

    check(R"(
        while true do  @1
    )");

    auto ac3 = autocomplete('1');
    CHECK_EQ(ac3.entryMap.count("end"), 1);
    CHECK_EQ(ac3.context, AutocompleteContext::Statement);

    check(R"(
        while true d@1
    )");

    auto ac4 = autocomplete('1');
    CHECK_EQ(3, ac4.entryMap.size());
    CHECK_EQ(ac4.entryMap.count("do"), 1);
    CHECK_EQ(ac4.entryMap.count("and"), 1);
    CHECK_EQ(ac4.entryMap.count("or"), 1);
    CHECK_EQ(ac4.context, AutocompleteContext::Keyword);

    check(R"(
        while t@1
    )");

    auto ac5 = autocomplete('1');
    CHECK_EQ(ac5.entryMap.count("do"), 0);
    CHECK_EQ(ac5.entryMap.count("true"), 1);
    CHECK_EQ(ac5.entryMap.count("false"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_if_middle_keywords")
{
    check(R"(
        if   @1
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.count("then"), 0);
    CHECK_EQ(
        ac1.entryMap.count("function"),
        1
    ); // FIXME: This is kind of dumb.  It is technically syntactically valid but you can never do anything interesting with this.
    CHECK_EQ(ac1.entryMap.count("table"), 1);
    CHECK_EQ(ac1.entryMap.count("else"), 0);
    CHECK_EQ(ac1.entryMap.count("elseif"), 0);
    CHECK_EQ(ac1.entryMap.count("end"), 0);
    CHECK_EQ(ac1.context, AutocompleteContext::Expression);

    check(R"(
        if x  @1
    )");

    auto ac2 = autocomplete('1');
    CHECK_EQ(ac2.entryMap.count("then"), 1);
    CHECK_EQ(ac2.entryMap.count("function"), 0);
    CHECK_EQ(ac2.entryMap.count("else"), 0);
    CHECK_EQ(ac2.entryMap.count("elseif"), 0);
    CHECK_EQ(ac2.entryMap.count("end"), 0);
    CHECK_EQ(ac2.context, AutocompleteContext::Keyword);

    check(R"(
        if x t@1
    )");

    auto ac3 = autocomplete('1');
    CHECK_EQ(3, ac3.entryMap.size());
    CHECK_EQ(ac3.entryMap.count("then"), 1);
    CHECK_EQ(ac3.entryMap.count("and"), 1);
    CHECK_EQ(ac3.entryMap.count("or"), 1);
    CHECK_EQ(ac3.context, AutocompleteContext::Keyword);

    check(R"(
        if x then
@1
        end
    )");

    auto ac4 = autocomplete('1');
    CHECK_EQ(ac4.entryMap.count("then"), 0);
    CHECK_EQ(ac4.entryMap.count("else"), 1);
    CHECK_EQ(ac4.entryMap.count("function"), 1);
    CHECK_EQ(ac4.entryMap.count("elseif"), 1);
    CHECK_EQ(ac4.entryMap.count("end"), 0);
    CHECK_EQ(ac4.context, AutocompleteContext::Statement);

    check(R"(
        if x then
            t@1
        end
    )");

    auto ac4a = autocomplete('1');
    CHECK_EQ(ac4a.entryMap.count("then"), 0);
    CHECK_EQ(ac4a.entryMap.count("table"), 1);
    CHECK_EQ(ac4a.entryMap.count("else"), 1);
    CHECK_EQ(ac4a.entryMap.count("elseif"), 1);
    CHECK_EQ(ac4a.context, AutocompleteContext::Statement);

    check(R"(
        if x then
@1
        elseif x then
        end
    )");

    auto ac5 = autocomplete('1');
    CHECK_EQ(ac5.entryMap.count("then"), 0);
    CHECK_EQ(ac5.entryMap.count("function"), 1);
    CHECK_EQ(ac5.entryMap.count("else"), 0);
    CHECK_EQ(ac5.entryMap.count("elseif"), 0);
    CHECK_EQ(ac5.entryMap.count("end"), 0);
    CHECK_EQ(ac5.context, AutocompleteContext::Statement);

    check(R"(
        if t@1
    )");

    auto ac6 = autocomplete('1');
    CHECK_EQ(ac6.entryMap.count("true"), 1);
    CHECK_EQ(ac6.entryMap.count("false"), 1);
    CHECK_EQ(ac6.entryMap.count("then"), 0);
    CHECK_EQ(ac6.entryMap.count("function"), 1);
    CHECK_EQ(ac6.entryMap.count("else"), 0);
    CHECK_EQ(ac6.entryMap.count("elseif"), 0);
    CHECK_EQ(ac6.entryMap.count("end"), 0);
    CHECK_EQ(ac6.context, AutocompleteContext::Expression);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_until_in_repeat")
{
    check(R"(
        repeat  @1
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("table"), 1);
    CHECK_EQ(ac.entryMap.count("until"), 1);
    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_until_expression")
{
    check(R"(
        repeat
        until   @1
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("table"), 1);
    CHECK_EQ(ac.context, AutocompleteContext::Expression);
}

TEST_CASE_FIXTURE(ACFixture, "local_names")
{
    check(R"(
        local ab@1
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.size(), 1);
    CHECK_EQ(ac1.entryMap.count("function"), 1);
    CHECK_EQ(ac1.context, AutocompleteContext::Unknown);

    check(R"(
        local ab, cd@1
    )");

    auto ac2 = autocomplete('1');
    CHECK(ac2.entryMap.empty());
    CHECK_EQ(ac2.context, AutocompleteContext::Unknown);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_end_with_fn_exprs")
{
    check(R"(
        local function f()  @1
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("end"), 1);
    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_end_with_lambda")
{
    check(R"(
        local a = function() local bar = foo en@1
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("end"), 1);
    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_end_of_do_block")
{
    check("do @1");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("end"));

    check(R"(
        function f()
            do
                @1
        end
        @2
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("end"));

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("end"));
}

TEST_CASE_FIXTURE(ACFixture, "stop_at_first_stat_when_recommending_keywords")
{
    check(R"(
        repeat
            for x @1
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.count("in"), 1);
    CHECK_EQ(ac1.entryMap.count("until"), 0);
    CHECK_EQ(ac1.context, AutocompleteContext::Keyword);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_repeat_middle_keyword")
{
    check(R"(
        repeat @1
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.count("do"), 1);
    CHECK_EQ(ac1.entryMap.count("function"), 1);
    CHECK_EQ(ac1.entryMap.count("until"), 1);

    check(R"(
        repeat f f@1
    )");

    auto ac2 = autocomplete('1');
    CHECK_EQ(ac2.entryMap.count("function"), 1);
    CHECK_EQ(ac2.entryMap.count("until"), 1);

    check(R"(
        repeat
            u@1
        until
    )");

    auto ac3 = autocomplete('1');
    CHECK_EQ(ac3.entryMap.count("until"), 0);
}

TEST_CASE_FIXTURE(ACFixture, "local_function")
{
    check(R"(
        local f@1
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.size(), 1);
    CHECK_EQ(ac1.entryMap.count("function"), 1);

    check(R"(
        local f@1, cd
    )");

    auto ac2 = autocomplete('1');
    CHECK(ac2.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "local_function")
{
    check(R"(
        local function @1
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.empty());

    check(R"(
        local function @1s@2
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.empty());

    ac = autocomplete('2');
    CHECK(ac.entryMap.empty());

    check(R"(
        local function @1()@2
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.empty());

    ac = autocomplete('2');
    CHECK(ac.entryMap.count("end"));

    check(R"(
        local function something@1
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.empty());

    check(R"(
        local tbl = {}
        function tbl.something@1() end
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "local_function_params")
{
    check(R"(
        local function @1a@2bc(@3d@4ef)@5 @6
    )");

    CHECK(autocomplete('1').entryMap.empty());
    CHECK(autocomplete('2').entryMap.empty());
    CHECK(autocomplete('3').entryMap.empty());
    CHECK(autocomplete('4').entryMap.empty());
    CHECK(!autocomplete('5').entryMap.empty());

    CHECK(!autocomplete('6').entryMap.empty());

    check(R"(
        local function abc(def)
@1        end
    )");

    for (unsigned int i = 23; i < 31; ++i)
    {
        CHECK(autocomplete(1, i).entryMap.empty());
    }
    CHECK(!autocomplete(1, 32).entryMap.empty());

    auto ac2 = autocomplete('1');
    CHECK_EQ(ac2.entryMap.count("abc"), 1);
    CHECK_EQ(ac2.entryMap.count("def"), 1);
    CHECK_EQ(ac2.context, AutocompleteContext::Statement);

    check(R"(
        local function abc(def, ghi@1)
        end
    )");

    auto ac3 = autocomplete('1');
    CHECK(ac3.entryMap.empty());
    CHECK_EQ(ac3.context, AutocompleteContext::Unknown);
}

TEST_CASE_FIXTURE(ACFixture, "global_function_params")
{
    check(R"(
        function abc(def)
    )");

    for (unsigned int i = 17; i < 25; ++i)
    {
        CHECK(autocomplete(1, i).entryMap.empty());
    }
    CHECK(!autocomplete(1, 26).entryMap.empty());

    check(R"(
        function abc(def)
        end
    )");

    for (unsigned int i = 17; i < 25; ++i)
    {
        CHECK(autocomplete(1, i).entryMap.empty());
    }
    CHECK(!autocomplete(1, 26).entryMap.empty());

    check(R"(
        function abc(def)
@1
        end
    )");

    auto ac2 = autocomplete('1');
    CHECK_EQ(ac2.entryMap.count("abc"), 1);
    CHECK_EQ(ac2.entryMap.count("def"), 1);
    CHECK_EQ(ac2.context, AutocompleteContext::Statement);

    check(R"(
        function abc(def, ghi@1)
        end
    )");

    auto ac3 = autocomplete('1');
    CHECK(ac3.entryMap.empty());
    CHECK_EQ(ac3.context, AutocompleteContext::Unknown);
}

TEST_CASE_FIXTURE(ACFixture, "arguments_to_global_lambda")
{
    check(R"(
        abc = function(def, ghi@1)
        end
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "function_expr_params")
{
    check(R"(
        abc = function(def) @1
    )");

    for (unsigned int i = 20; i < 27; ++i)
    {
        CHECK(autocomplete(1, i).entryMap.empty());
    }
    CHECK(!autocomplete('1').entryMap.empty());

    check(R"(
        abc = function(def) @1
        end
    )");

    for (unsigned int i = 20; i < 27; ++i)
    {
        CHECK(autocomplete(1, i).entryMap.empty());
    }
    CHECK(!autocomplete('1').entryMap.empty());

    check(R"(
        abc = function(def)
@1
        end
    )");

    auto ac2 = autocomplete('1');
    CHECK_EQ(ac2.entryMap.count("def"), 1);
    CHECK_EQ(ac2.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACFixture, "local_initializer")
{
    check(R"(
        local a = t@1
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("table"), 1);
    CHECK_EQ(ac.entryMap.count("true"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "local_initializer_2")
{
    check(R"(
        local a=@1
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("table"));
}

TEST_CASE_FIXTURE(ACFixture, "get_member_completions")
{
    check(R"(
        local a = 12.@13
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "sometimes_the_metatable_is_an_error")
{
    check(R"(
        local T = {}
        T.__index = T

        function T.new()
            return setmetatable({x=6}, X) -- oops!
        end
        local t = T.new()
        t.  @1
    )");

    autocomplete('1');
    // Don't crash!
}

TEST_CASE_FIXTURE(ACFixture, "local_types_builtin")
{
    check(R"(
local a: n@1
local b: string = "don't trip"
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));
    CHECK_EQ(ac.context, AutocompleteContext::Type);
}

TEST_CASE_FIXTURE(ACFixture, "private_types")
{
    check(R"(
do
    type num = number
    local a: n@1u
    local b: nu@2m
end
local a: nu@3
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("num"));
    CHECK(ac.entryMap.count("number"));

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("num"));
    CHECK(ac.entryMap.count("number"));

    ac = autocomplete('3');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "num");
    CHECK(ac.entryMap.count("number"));
}

TEST_CASE_FIXTURE(ACFixture, "type_scoping_easy")
{
    check(R"(
type Table = { a: number, b: number }
do
    type Table = { x: string, y: string }
    local a: T@1
end
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("Table"));
    REQUIRE(ac.entryMap["Table"].type);
    const TableType* tv = get<TableType>(follow(*ac.entryMap["Table"].type));
    REQUIRE(tv);
    CHECK(tv->props.count("x"));
}

TEST_CASE_FIXTURE(ACFixture, "modules_with_types")
{
    fileResolver.source["Module/A"] = R"(
export type A = { x: number, y: number }
export type B = { z: number, w: number }
return {}
    )";

    LUAU_REQUIRE_NO_ERRORS(getFrontend().check("Module/A"));

    fileResolver.source["Module/B"] = R"(
local aaa = require(script.Parent.A)
local a: aa
    )";

    getFrontend().check("Module/B");

    auto ac = autocomplete("Module/B", Position{2, 11});

    CHECK(ac.entryMap.count("aaa"));
    CHECK_EQ(ac.context, AutocompleteContext::Type);
}

TEST_CASE_FIXTURE(ACFixture, "module_type_members")
{
    fileResolver.source["Module/A"] = R"(
export type A = { x: number, y: number }
export type B = { z: number, w: number }
return {}
    )";

    LUAU_REQUIRE_NO_ERRORS(getFrontend().check("Module/A"));

    fileResolver.source["Module/B"] = R"(
local aaa = require(script.Parent.A)
local a: aaa.
    )";

    getFrontend().check("Module/B");

    auto ac = autocomplete("Module/B", Position{2, 13});

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("A"));
    CHECK(ac.entryMap.count("B"));
    CHECK_EQ(ac.context, AutocompleteContext::Type);
}

TEST_CASE_FIXTURE(ACFixture, "argument_types")
{
    check(R"(
local function f(a: n@1
local b: string = "don't trip"
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));
    CHECK_EQ(ac.context, AutocompleteContext::Type);
}

TEST_CASE_FIXTURE(ACFixture, "return_types")
{
    check(R"(
local function f(a: number): n@1
local b: string = "don't trip"
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));
    CHECK_EQ(ac.context, AutocompleteContext::Type);
}

TEST_CASE_FIXTURE(ACFixture, "as_types")
{
    check(R"(
local a: any = 5
local b: number = (a :: n@1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));
    CHECK_EQ(ac.context, AutocompleteContext::Type);
}

TEST_CASE_FIXTURE(ACFixture, "function_type_types")
{
    check(R"(
local a: (n@1
local b: (number, (n@2
local c: (number, (number) -> n@3
local d: (number, (number) -> (number, n@4
local e: (n: n@5
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));

    ac = autocomplete('3');

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));

    ac = autocomplete('4');

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));

    ac = autocomplete('5');

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));
}

TEST_CASE_FIXTURE(ACFixture, "generic_types")
{
    check(R"(
function f<Tee, Use>(a: T@1
local b: string = "don't trip"
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("Tee"));
    CHECK_EQ(ac.context, AutocompleteContext::Type);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_suggestion_in_argument")
{
    // local
    check(R"(
local function target(a: number, b: string) return a + #b end

local one = 4
local two = "hello"
return target(o@1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("one"));
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::None);

    check(R"(
local function target(a: number, b: string) return a + #b end

local one = 4
local two = "hello"
return target(one, t@1
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("two"));
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::None);

    // member
    check(R"(
local function target(a: number, b: string) return a + #b end

local a = { one = 4, two = "hello" }
return target(a.@1
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("one"));
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::None);

    check(R"(
local function target(a: number, b: string) return a + #b end

local a = { one = 4, two = "hello" }
return target(a.one, a.@1
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("two"));
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::None);

    // union match
    check(R"(
local function target(a: string?) return #b end

local a = { one = 4, two = "hello" }
return target(a.@1
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("two"));
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::None);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_suggestion_in_table")
{
    if (FFlag::LuauSolverV2) // CLI-116815 Autocomplete cannot suggest keys while autocompleting inside of a table
        return;

    check(R"(
type Foo = { a: number, b: string }
local a = { one = 4, two = "hello" }
local b: Foo = { a = a.@1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("one"));
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::None);
    CHECK_EQ(ac.context, AutocompleteContext::Property);

    check(R"(
type Foo = { a: number, b: string }
local a = { one = 4, two = "hello" }
local b: Foo = { b = a.@1
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("two"));
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::None);
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_function_return_types")
{
    check(R"(
local function target(a: number, b: string) return a + #b end
local function bar1(a: number) return -a end
local function bar2(a: string) return a .. 'x' end

return target(b@1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("bar1"));
    CHECK(ac.entryMap["bar1"].typeCorrect == TypeCorrectKind::CorrectFunctionResult);
    CHECK(ac.entryMap["bar2"].typeCorrect == TypeCorrectKind::None);

    check(R"(
local function target(a: number, b: string) return a + #b end
local function bar1(a: number) return -a end
local function bar2(a: string) return a .. 'x' end

return target(bar1, b@1
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("bar2"));
    CHECK(ac.entryMap["bar2"].typeCorrect == TypeCorrectKind::CorrectFunctionResult);
    CHECK(ac.entryMap["bar1"].typeCorrect == TypeCorrectKind::None);

    check(R"(
local function target(a: number, b: string) return a + #b end
local function bar1(a: number): (...number) return -a, a end
local function bar2(a: string) return a .. 'x' end

return target(b@1
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("bar1"));
    CHECK(ac.entryMap["bar1"].typeCorrect == TypeCorrectKind::CorrectFunctionResult);
    CHECK(ac.entryMap["bar2"].typeCorrect == TypeCorrectKind::None);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_local_type_suggestion")
{
    check(R"(
local b: s@1 = "str"
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function f() return "str" end
local b: s@1 = f()
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local b: s@1, c: n@2 = "str", 2
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function f() return 1, "str", 3 end
local a: b@1, b: n@2, c: s@3, d: n@4 = false, f()
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("boolean"));
    CHECK(ac.entryMap["boolean"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('3');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('4');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function f(): ...number return 1, 2, 3 end
local a: boolean, b: n@1 = false, f()
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_function_type_suggestion")
{
    check(R"(
local b: (n@1) -> number = function(a: number, b: string) return a + #b end
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local b: (number, s@1 = function(a: number, b: string) return a + #b end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local b: (number, string) -> b@1 = function(a: number, b: string): boolean return a + #b == 0 end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("boolean"));
    CHECK(ac.entryMap["boolean"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local b: (number, ...s@1) = function(a: number, ...: string) return a end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local b: (number) -> ...s@1 = function(a: number): ...string return "a", "b", "c" end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_full_type_suggestion")
{
    check(R"(
local b:@1 @2= "str"
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local b: @1= function(a: number) return -a end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("(number) -> number"));
    CHECK(ac.entryMap["(number) -> number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_argument_type_suggestion")
{
    check(R"(
local function target(a: number, b: string) return a + #b end

local function d(a: n@1, b)
    return target(a, b)
end
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(a: number, b: string) return a + #b end

local function d(a, b: s@1)
    return target(a, b)
end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(a: number, b: string) return a + #b end

local function d(a:@1 @2, b)
    return target(a, b)
end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(a: number, b: string) return a + #b end

local function d(a, b: @1)@2: number
    return target(a, b)
end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('2');

    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::None);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_expected_argument_type_suggestion")
{
    check(R"(
local function target(callback: (a: number, b: string) -> number) return callback(4, "hello") end

local x = target(function(a: @1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(callback: (a: number, b: string) -> number) return callback(4, "hello") end

local x = target(function(a: n@1
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(callback: (a: number, b: string) -> number) return callback(4, "hello") end

local x = target(function(a: n@1, b: @2)
    return a + #b
end)
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(callback: (...number) -> number) return callback(1, 2, 3) end

local x = target(function(a: n@1)
    return a
end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_expected_argument_type_pack_suggestion")
{
    check(R"(
local function target(callback: (...number) -> number) return callback(1, 2, 3) end

local x = target(function(...:n@1)
    return a
end
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(callback: (...number) -> number) return callback(1, 2, 3) end

local x = target(function(a:number, b:number, ...:@1)
    return a + b
end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_expected_return_type_suggestion")
{
    check(R"(
local function target(callback: () -> number) return callback() end

local x = target(function(): n@1
    return 1
end
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(callback: () -> (number, number)) return callback() end

local x = target(function(): (number, n@1
    return 1, 2
end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_expected_return_type_pack_suggestion")
{
    check(R"(
local function target(callback: () -> ...number) return callback() end

local x = target(function(): ...n@1
    return 1, 2, 3
end
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(callback: () -> ...number) return callback() end

local x = target(function(): (number, number, ...n@1
    return 1, 2, 3
end
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_expected_argument_type_suggestion_optional")
{
    check(R"(
local function target(callback: nil | (a: number, b: string) -> number) return callback(4, "hello") end

local x = target(function(a: @1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_expected_argument_type_suggestion_self")
{
    check(R"(
local t = {}
t.x = 5
function t:target(callback: (a: number, b: string) -> number) return callback(self.x, "hello") end

local x = t:target(function(a: @1, b:@2 ) end)
local y = t.target(t, function(a: number, b: @3) end)
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('3');

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "do_not_suggest_internal_module_type")
{
    fileResolver.source["Module/A"] = R"(
type done = { x: number, y: number }
local function a(a: (done) -> number) return a({x=1, y=2}) end
local function b(a: ((done) -> number) -> number) return a(function(done) return 1 end) end
return {a = a, b = b}
    )";

    LUAU_REQUIRE_NO_ERRORS(getFrontend().check("Module/A"));

    fileResolver.source["Module/B"] = R"(
local ex = require(script.Parent.A)
ex.a(function(x:
    )";

    getFrontend().check("Module/B");

    auto ac = autocomplete("Module/B", Position{2, 16});

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "done");

    fileResolver.source["Module/C"] = R"(
local ex = require(script.Parent.A)
ex.b(function(x:
    )";

    getFrontend().check("Module/C");

    ac = autocomplete("Module/C", Position{2, 16});

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "(done) -> number");
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "suggest_external_module_type")
{
    fileResolver.source["Module/A"] = R"(
export type done = { x: number, y: number }
local function a(a: (done) -> number) return a({x=1, y=2}) end
local function b(a: ((done) -> number) -> number) return a(function(done) return 1 end) end
return {a = a, b = b}
    )";

    LUAU_REQUIRE_NO_ERRORS(getFrontend().check("Module/A"));

    fileResolver.source["Module/B"] = R"(
local ex = require(script.Parent.A)
ex.a(function(x:
    )";

    getFrontend().check("Module/B");

    auto ac = autocomplete("Module/B", Position{2, 16});

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "done");
    CHECK(ac.entryMap.count("ex.done"));
    CHECK(ac.entryMap["ex.done"].typeCorrect == TypeCorrectKind::Correct);

    fileResolver.source["Module/C"] = R"(
local ex = require(script.Parent.A)
ex.b(function(x:
    )";

    getFrontend().check("Module/C");

    ac = autocomplete("Module/C", Position{2, 16});

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "(done) -> number");
    CHECK(ac.entryMap.count("(ex.done) -> number"));
    CHECK(ac.entryMap["(ex.done) -> number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "do_not_suggest_synthetic_table_name")
{
    check(R"(
local foo = { a = 1, b = 2 }
local bar: @1= foo
    )");

    auto ac = autocomplete('1');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "foo");
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_function_no_parenthesis")
{
    check(R"(
local function target(a: (number) -> number) return a(4) end
local function bar1(a: number) return -a end
local function bar2(a: string) return a .. 'x' end

return target(b@1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("bar1"));
    CHECK(ac.entryMap["bar1"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["bar1"].parens == ParenthesesRecommendation::None);
    CHECK(ac.entryMap["bar2"].typeCorrect == TypeCorrectKind::None);
}

TEST_CASE_FIXTURE(ACFixture, "function_in_assignment_has_parentheses")
{
    check(R"(
local function bar(a: number) return -a end
local abc = b@1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("bar"));
    CHECK(ac.entryMap["bar"].parens == ParenthesesRecommendation::CursorInside);
}

TEST_CASE_FIXTURE(ACFixture, "function_result_passed_to_function_has_parentheses")
{
    check(R"(
local function foo() return 1 end
local function bar(a: number) return -a end
local abc = bar(@1)
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("foo"));
    CHECK(ac.entryMap["foo"].parens == ParenthesesRecommendation::CursorAfter);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_sealed_table")
{

    check(R"(
local function f(a: { x: number, y: number }) return a.x + a.y end
local fp: @1= f
    )");

    auto ac = autocomplete('1');

    if (FFlag::LuauSolverV2)
        REQUIRE_EQ("({ x: number, y: number }) -> number", toString(requireType("f")));
    else
    {
        // NOTE: All autocomplete tests occur under no-check mode.
        REQUIRE_EQ("({ x: number, y: number }) -> (...any)", toString(requireType("f")));
    }
    CHECK(ac.entryMap.count("({ x: number, y: number }) -> number"));
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_keywords")
{
    check(R"(
local function a(x: boolean) end
local function b(x: number?) end
local function c(x: (number) -> string) end
local function d(x: ((number) -> string)?) end
local function e(x: ((number) -> string) & ((boolean) -> number)) end

local tru = {}
local ni = false

local ac = a(t@1)
local bc = b(n@2)
local cc = c(f@3)
local dc = d(f@4)
local ec = e(f@5)
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("tru"));
    CHECK(ac.entryMap["tru"].typeCorrect == TypeCorrectKind::None);
    CHECK(ac.entryMap["true"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["false"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('2');
    CHECK(ac.entryMap.count("ni"));
    CHECK(ac.entryMap["ni"].typeCorrect == TypeCorrectKind::None);
    CHECK(ac.entryMap["nil"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('3');
    CHECK(ac.entryMap.count("false"));
    CHECK(ac.entryMap["false"].typeCorrect == TypeCorrectKind::None);
    CHECK(ac.entryMap["function"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('4');
    CHECK(ac.entryMap["function"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete('5');
    CHECK(ac.entryMap["function"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_suggestion_for_overloads")
{
    if (FFlag::LuauSolverV2) // CLI-116814 Autocomplete needs to populate expected types for function arguments correctly
                             // (overloads and singletons)
        return;
    check(R"(
local target: ((number) -> string) & ((string) -> number))

local one = 4
local two = "hello"
return target(o@1)
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("one"));
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local target: ((number) -> string) & ((number) -> number))

local one = 4
local two = "hello"
return target(o@1)
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("one"));
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::None);

    check(R"(
local target: ((number, number) -> string) & ((string) -> number))

local one = 4
local two = "hello"
return target(1, o@1)
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("one"));
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::None);
}

TEST_CASE_FIXTURE(ACFixture, "optional_members")
{
    check(R"(
local a = { x = 2, y = 3 }
type A = typeof(a)
local b: A? = a
return b.@1
    )");

    auto ac = autocomplete('1');

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("x"));
    CHECK(ac.entryMap.count("y"));

    check(R"(
local a = { x = 2, y = 3 }
type A = typeof(a)
local b: nil | A = a
return b.@1
    )");

    ac = autocomplete('1');

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("x"));
    CHECK(ac.entryMap.count("y"));

    check(R"(
local b: nil | nil
return b.@1
    )");

    ac = autocomplete('1');

    CHECK_EQ(0, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "no_function_name_suggestions")
{
    check(R"(
function na@1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.empty());

    check(R"(
local function @1
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.empty());

    check(R"(
local function na@1
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "skip_current_local")
{
    check(R"(
local other = 1
local name = na@1
    )");

    auto ac = autocomplete('1');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "name");
    CHECK(ac.entryMap.count("other"));

    check(R"(
local other = 1
local name, test = na@1
    )");

    ac = autocomplete('1');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "name");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "test");
    CHECK(ac.entryMap.count("other"));
}

TEST_CASE_FIXTURE(ACFixture, "keyword_members")
{
    check(R"(
local a = { done = 1, forever = 2 }
local b = a.do@1
local c = a.for@2
local d = a.@3
do
end
    )");

    auto ac = autocomplete('1');

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("done"));
    CHECK(ac.entryMap.count("forever"));

    ac = autocomplete('2');

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("done"));
    CHECK(ac.entryMap.count("forever"));

    ac = autocomplete('3');

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("done"));
    CHECK(ac.entryMap.count("forever"));
}

TEST_CASE_FIXTURE(ACFixture, "keyword_methods")
{
    check(R"(
local a = {}
function a:done() end
local b = a:do@1
    )");

    auto ac = autocomplete('1');

    CHECK_EQ(1, ac.entryMap.size());
    CHECK(ac.entryMap.count("done"));
}

TEST_CASE_FIXTURE(ACFixture, "keyword_types")
{
    fileResolver.source["Module/A"] = R"(
export type done = { x: number, y: number }
export type other = { z: number, w: number }
return {}
    )";

    LUAU_REQUIRE_NO_ERRORS(getFrontend().check("Module/A"));

    fileResolver.source["Module/B"] = R"(
local aaa = require(script.Parent.A)
local a: aaa.do
    )";

    getFrontend().check("Module/B");

    auto ac = autocomplete("Module/B", Position{2, 15});

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("done"));
    CHECK(ac.entryMap.count("other"));
}


TEST_CASE_FIXTURE(ACFixture, "comments")
{
    fileResolver.source["Comments"] = "--foo";

    auto ac = autocomplete("Comments", Position{0, 5});
    CHECK_EQ(0, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "autocompleteProp_index_function_metamethod_is_variadic")
{
    fileResolver.source["Module/A"] = R"(
        type Foo = {x: number}
        local t = {}
        setmetatable(t, {
            __index = function(index: string): ...Foo
                return {x = 1}, {x = 2}
            end
        })

        local a = t. -- Line 9
        --          | Column 20
    )";

    auto ac = autocomplete("Module/A", Position{9, 20});
    REQUIRE_EQ(1, ac.entryMap.size());
    CHECK(ac.entryMap.count("x"));
}

TEST_CASE_FIXTURE(ACFixture, "if_then_else_full_keywords")
{
    check(R"(
local thenceforth = false
local elsewhere = false
local doover = false
local endurance = true

if 1 then@1
else@2
end

while false do@3
end

repeat@4
until
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.size() == 1);
    CHECK(ac.entryMap.count("then"));

    ac = autocomplete('2');
    CHECK(ac.entryMap.count("else"));
    CHECK(ac.entryMap.count("elseif"));

    ac = autocomplete('3');
    CHECK(ac.entryMap.count("do"));

    ac = autocomplete('4');
    CHECK(ac.entryMap.count("do"));

    // FIXME: ideally we want to handle start and end of all statements as well
}

TEST_CASE_FIXTURE(ACFixture, "if_then_else_elseif_completions")
{
    check(R"(
local elsewhere = false

if true then
    return 1
el@1
end
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("else"));
    CHECK(ac.entryMap.count("elseif"));
    CHECK(ac.entryMap.count("elsewhere") == 0);

    check(R"(
local elsewhere = false

if true then
    return 1
else
    return 2
el@1
end
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("else") == 0);
    CHECK(ac.entryMap.count("elseif") == 0);
    CHECK(ac.entryMap.count("elsewhere"));

    check(R"(
local elsewhere = false

if true then
    print("1")
elif true then
    print("2")
el@1
end
    )");
    ac = autocomplete('1');
    CHECK(ac.entryMap.count("else"));
    CHECK(ac.entryMap.count("elseif"));
    CHECK(ac.entryMap.count("elsewhere"));
}

TEST_CASE_FIXTURE(ACFixture, "not_the_var_we_are_defining")
{
    fileResolver.source["Module/A"] = "abc,de";

    auto ac = autocomplete("Module/A", Position{0, 6});
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "de");
}

TEST_CASE_FIXTURE(ACFixture, "recursive_function_global")
{
    fileResolver.source["global"] = R"(function abc()

end
)";

    auto ac = autocomplete("global", Position{1, 0});
    CHECK(ac.entryMap.count("abc"));
}



TEST_CASE_FIXTURE(ACFixture, "recursive_function_local")
{
    fileResolver.source["local"] = R"(local function abc()

end
)";

    auto ac = autocomplete("local", Position{1, 0});
    CHECK(ac.entryMap.count("abc"));
}

TEST_CASE_FIXTURE(ACFixture, "suggest_table_keys")
{
    if (FFlag::LuauSolverV2) // CLI-116812 AutocompleteTest.suggest_table_keys needs to populate expected types for nested
                             // tables without an annotation
        return;

    check(R"(
type Test = { first: number, second: number }
local t: Test = { f@1 }
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);

    // Intersection
    check(R"(
type Test = { first: number } & { second: number }
local t: Test = { f@1 }
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);

    // Union
    check(R"(
type Test = { first: number, second: number } | { second: number, third: number }
local t: Test = { s@1 }
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("second"));
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "first");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "third");
    CHECK_EQ(ac.context, AutocompleteContext::Property);

    // No parenthesis suggestion
    check(R"(
type Test = { first: (number) -> number, second: number }
local t: Test = { f@1 }
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap["first"].parens == ParenthesesRecommendation::None);
    CHECK_EQ(ac.context, AutocompleteContext::Property);

    // When key is changed
    check(R"(
type Test = { first: number, second: number }
local t: Test = { f@1 = 2 }
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);

    // Alternative key syntax
    check(R"(
type Test = { first: number, second: number }
local t: Test = { ["f@1"] }
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);

    // Not an alternative key syntax
    check(R"(
type Test = { first: number, second: number }
local t: Test = { "f@1" }
    )");

    ac = autocomplete('1');
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "first");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "second");
    CHECK_EQ(ac.context, AutocompleteContext::String);

    // Skip keys that are already defined
    check(R"(
type Test = { first: number, second: number }
local t: Test = { first = 2, s@1 }
    )");

    ac = autocomplete('1');
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "first");
    CHECK(ac.entryMap.count("second"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);

    // Don't skip active key
    check(R"(
type Test = { first: number, second: number }
local t: Test = { first@1 }
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);

    // Inference after first key
    check(R"(
local t = {
    { first = 5, second = 10 },
    { f@1 }
}
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);

    check(R"(
local t = {
    [2] = { first = 5, second = 10 },
    [5] = { f@1 }
}
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "suggest_table_keys_no_initial_character")
{
    check(R"(
type Test = { first: number, second: number }
local t: Test = { @1 }
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "suggest_table_keys_no_initial_character_2")
{
    check(R"(
type Test = { first: number, second: number }
local t: Test = { first = 1, @1 }
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("first"), 0);
    CHECK(ac.entryMap.count("second"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "suggest_table_keys_no_initial_character_3")
{
    check(R"(
type Properties = { TextScaled: boolean, Text: string }
local function create(props: Properties) end

create({ @1 })
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.size() > 0);
    CHECK(ac.entryMap.count("TextScaled"));
    CHECK(ac.entryMap.count("Text"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_documentation_symbols")
{
    loadDefinition(R"(
        declare y: {
            x: number,
        }
    )");

    check(R"(
        local a = y.@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("x"));
    CHECK_EQ(ac.entryMap["x"].documentationSymbol, "@test/global/y.x");
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_ifelse_expressions")
{
    check(R"(
local temp = false
local even = true;
local a = true
a = if t@1emp then t
a = if temp t@2
a = if temp then e@3
a = if temp then even e@4
a = if temp then even elseif t@5
a = if temp then even elseif true t@6
a = if temp then even elseif true then t@7
a = if temp then even elseif true then temp e@8
a = if temp then even elseif true then temp else e@9
        )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("temp"));
    CHECK(ac.entryMap.count("true"));
    CHECK(ac.entryMap.count("then") == 0);
    CHECK(ac.entryMap.count("else") == 0);
    CHECK(ac.entryMap.count("elseif") == 0);
    CHECK_EQ(ac.context, AutocompleteContext::Expression);

    ac = autocomplete('2');
    CHECK(ac.entryMap.count("temp") == 0);
    CHECK(ac.entryMap.count("true") == 0);
    CHECK(ac.entryMap.count("then"));
    CHECK(ac.entryMap.count("else") == 0);
    CHECK(ac.entryMap.count("elseif") == 0);
    CHECK_EQ(ac.context, AutocompleteContext::Keyword);

    ac = autocomplete('3');
    CHECK(ac.entryMap.count("even"));
    CHECK(ac.entryMap.count("then") == 0);
    CHECK(ac.entryMap.count("else") == 0);
    CHECK(ac.entryMap.count("elseif") == 0);
    CHECK_EQ(ac.context, AutocompleteContext::Expression);

    ac = autocomplete('4');
    CHECK(ac.entryMap.count("even") == 0);
    CHECK(ac.entryMap.count("then") == 0);
    CHECK(ac.entryMap.count("else"));
    CHECK(ac.entryMap.count("elseif"));
    CHECK_EQ(ac.context, AutocompleteContext::Keyword);

    ac = autocomplete('5');
    CHECK(ac.entryMap.count("temp"));
    CHECK(ac.entryMap.count("true"));
    CHECK(ac.entryMap.count("then") == 0);
    CHECK(ac.entryMap.count("else") == 0);
    CHECK(ac.entryMap.count("elseif") == 0);
    CHECK_EQ(ac.context, AutocompleteContext::Expression);

    ac = autocomplete('6');
    CHECK(ac.entryMap.count("temp") == 0);
    CHECK(ac.entryMap.count("true") == 0);
    CHECK(ac.entryMap.count("then"));
    CHECK(ac.entryMap.count("else") == 0);
    CHECK(ac.entryMap.count("elseif") == 0);
    CHECK_EQ(ac.context, AutocompleteContext::Keyword);

    ac = autocomplete('7');
    CHECK(ac.entryMap.count("temp"));
    CHECK(ac.entryMap.count("true"));
    CHECK(ac.entryMap.count("then") == 0);
    CHECK(ac.entryMap.count("else") == 0);
    CHECK(ac.entryMap.count("elseif") == 0);
    CHECK_EQ(ac.context, AutocompleteContext::Expression);

    ac = autocomplete('8');
    CHECK(ac.entryMap.count("even") == 0);
    CHECK(ac.entryMap.count("then") == 0);
    CHECK(ac.entryMap.count("else"));
    CHECK(ac.entryMap.count("elseif"));
    CHECK_EQ(ac.context, AutocompleteContext::Keyword);

    ac = autocomplete('9');
    CHECK(ac.entryMap.count("then") == 0);
    CHECK(ac.entryMap.count("else") == 0);
    CHECK(ac.entryMap.count("elseif") == 0);
    CHECK_EQ(ac.context, AutocompleteContext::Expression);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_if_else_regression")
{
    check(R"(
local abcdef = 0;
local temp = false
local even = true;
local a
a = if temp then even else@1
a = if temp then even else @2
a = if temp then even else abc@3
        )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("else") == 0);
    ac = autocomplete('2');
    CHECK(ac.entryMap.count("else") == 0);
    ac = autocomplete('3');
    CHECK(ac.entryMap.count("abcdef"));
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_interpolated_string_constant")
{
    check(R"(f(`@1`))");
    auto ac = autocomplete('1');
    CHECK(ac.entryMap.empty());
    CHECK_EQ(ac.context, AutocompleteContext::String);

    check(R"(f(`@1 {"a"}`))");
    ac = autocomplete('1');
    CHECK(ac.entryMap.empty());
    CHECK_EQ(ac.context, AutocompleteContext::String);

    check(R"(f(`{"a"} @1`))");
    ac = autocomplete('1');
    CHECK(ac.entryMap.empty());
    CHECK_EQ(ac.context, AutocompleteContext::String);

    check(R"(f(`{"a"} @1 {"b"}`))");
    ac = autocomplete('1');
    CHECK(ac.entryMap.empty());
    CHECK_EQ(ac.context, AutocompleteContext::String);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_interpolated_string_expression")
{
    check(R"(f(`expression = {@1}`))");
    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("table"));
    CHECK_EQ(ac.context, AutocompleteContext::Expression);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_interpolated_string_expression_with_comments")
{
    check(R"(f(`expression = {--[[ bla bla bla ]]@1`))");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("table"));
    CHECK_EQ(ac.context, AutocompleteContext::Expression);

    check(R"(f(`expression = {@1 --[[ bla bla bla ]]`))");
    ac = autocomplete('1');
    CHECK(!ac.entryMap.empty());
    CHECK(ac.entryMap.count("table"));
    CHECK_EQ(ac.context, AutocompleteContext::Expression);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_interpolated_string_as_singleton")
{
    check(R"(
        --!strict
        local function f(a: "cat" | "dog") end

        f(`@1`)
        f(`uhhh{'try'}@2`)
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("cat"));
    CHECK_EQ(ac.context, AutocompleteContext::String);

    ac = autocomplete('2');
    CHECK(ac.entryMap.empty());
    CHECK_EQ(ac.context, AutocompleteContext::String);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_explicit_type_pack")
{
    check(R"(
type A<T...> = () -> T...
local a: A<(number, s@1>
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap.count("string"));
    CHECK_EQ(ac.context, AutocompleteContext::Type);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_first_function_arg_expected_type")
{
    check(R"(
local function foo1() return 1 end
local function foo2() return "1" end

local function bar0() return "got" .. a end
local function bar1(a: number) return "got " .. a end
local function bar2(a: number, b: string) return "got " .. a .. b end

local t = {}
function t:bar1(a: number) return "got " .. a end

local r1 = bar0(@1)
local r2 = bar1(@2)
local r3 = bar2(@3)
local r4 = t:bar1(@4)
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("foo1"));
    CHECK(ac.entryMap["foo1"].typeCorrect == TypeCorrectKind::None);
    REQUIRE(ac.entryMap.count("foo2"));
    CHECK(ac.entryMap["foo2"].typeCorrect == TypeCorrectKind::None);

    ac = autocomplete('2');

    REQUIRE(ac.entryMap.count("foo1"));
    CHECK(ac.entryMap["foo1"].typeCorrect == TypeCorrectKind::CorrectFunctionResult);
    REQUIRE(ac.entryMap.count("foo2"));
    CHECK(ac.entryMap["foo2"].typeCorrect == TypeCorrectKind::None);

    ac = autocomplete('3');

    REQUIRE(ac.entryMap.count("foo1"));
    CHECK(ac.entryMap["foo1"].typeCorrect == TypeCorrectKind::CorrectFunctionResult);
    REQUIRE(ac.entryMap.count("foo2"));
    CHECK(ac.entryMap["foo2"].typeCorrect == TypeCorrectKind::None);

    ac = autocomplete('4');

    REQUIRE(ac.entryMap.count("foo1"));
    CHECK(ac.entryMap["foo1"].typeCorrect == TypeCorrectKind::CorrectFunctionResult);
    REQUIRE(ac.entryMap.count("foo2"));
    CHECK(ac.entryMap["foo2"].typeCorrect == TypeCorrectKind::None);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_default_type_parameters")
{
    check(R"(
type A<T = @1> = () -> T
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap.count("string"));
    CHECK_EQ(ac.context, AutocompleteContext::Type);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_default_type_pack_parameters")
{
    check(R"(
type A<T... = ...@1> = () -> T
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap.count("string"));
    CHECK_EQ(ac.context, AutocompleteContext::Type);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "autocomplete_oop_implicit_self")
{
    check(R"(
--!strict
local Class = {}
Class.__index = Class
type Class = typeof(setmetatable({} :: { x: number }, Class))
function Class.new(x: number): Class
    return setmetatable({x = x}, Class)
end
function Class.getx(self: Class)
    return self.x
end
function test()
    local c = Class.new(42)
    local n = c:@1
    print(n)
end
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("getx"));
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "autocomplete_on_string_singletons")
{
    check(R"(
        --!strict
        local foo: "hello" | "bye" = "hello"
        foo:@1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("format"));
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_string_singletons_in_literal")
{
    if (FFlag::LuauSolverV2)
        return;

    // CLI-116814: Under the new solver, we fail to properly apply the expected
    // type to `tag` as we fail to recognize that we can "break apart" unions
    // when trying to apply an expected type.

    check(R"(
        type tagged = {tag:"cat", fieldx:number} | {tag:"dog", fieldy:number}
        local x: tagged = {tag="@1"}
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("cat"));
    CHECK(ac.entryMap.count("dog"));
    CHECK_EQ(ac.context, AutocompleteContext::String);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_string_singletons")
{
    check(R"(
        type tag = "cat" | "dog"
        local function f(a: tag) end
        f("@1")
        f(@2)
        local x: tag = "@3"
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("cat"));
    CHECK(ac.entryMap.count("dog"));
    CHECK_EQ(ac.context, AutocompleteContext::String);

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("\"cat\""));
    CHECK(ac.entryMap.count("\"dog\""));
    CHECK_EQ(ac.context, AutocompleteContext::Expression);

    ac = autocomplete('3');

    CHECK(ac.entryMap.count("cat"));
    CHECK(ac.entryMap.count("dog"));
    CHECK_EQ(ac.context, AutocompleteContext::String);
}

TEST_CASE_FIXTURE(ACFixture, "string_singleton_as_table_key_iso")
{
    check(R"(
        type Direction = "up" | "down"
        local b: {[Direction]: boolean} = {["@2"] = true}
    )");

    auto ac = autocomplete('2');

    CHECK(ac.entryMap.count("up"));
    CHECK(ac.entryMap.count("down"));
}

TEST_CASE_FIXTURE(ACFixture, "string_singleton_as_table_key")
{
    check(R"(
        type Direction = "up" | "down"

        local a: {[Direction]: boolean} = {[@1] = true}
        local b: {[Direction]: boolean} = {["@2"] = true}
        local c: {[Direction]: boolean} = {u@3 = true}
        local d: {[Direction]: boolean} = {[u@4] = true}

        local e: {[Direction]: boolean} = {[@5]}
        local f: {[Direction]: boolean} = {["@6"]}
        local g: {[Direction]: boolean} = {u@7}
        local h: {[Direction]: boolean} = {[u@8]}
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("\"up\""));
    CHECK(ac.entryMap.count("\"down\""));

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("up"));
    CHECK(ac.entryMap.count("down"));

    ac = autocomplete('3');

    CHECK(ac.entryMap.count("up"));
    CHECK(ac.entryMap.count("down"));

    ac = autocomplete('4');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "up");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "down");

    CHECK(ac.entryMap.count("\"up\""));
    CHECK(ac.entryMap.count("\"down\""));

    ac = autocomplete('5');

    CHECK(ac.entryMap.count("\"up\""));
    CHECK(ac.entryMap.count("\"down\""));

    ac = autocomplete('6');

    CHECK(ac.entryMap.count("up"));
    CHECK(ac.entryMap.count("down"));

    ac = autocomplete('7');

    CHECK(ac.entryMap.count("up"));
    CHECK(ac.entryMap.count("down"));

    ac = autocomplete('8');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "up");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "down");

    CHECK(ac.entryMap.count("\"up\""));
    CHECK(ac.entryMap.count("\"down\""));
}

// https://github.com/Roblox/luau/issues/858
TEST_CASE_FIXTURE(ACFixture, "string_singleton_in_if_statement")
{
    ScopedFastFlag sff[]{
        {FFlag::LuauSolverV2, true},
    };

    check(R"(
        --!strict

        type Direction = "left" | "right"

        local dir: Direction = "left"

        if dir == @1"@2"@3 then end
        local a: {[Direction]: boolean} = {[@4"@5"@6]}

        if dir == @7`@8`@9 then end
        local a: {[Direction]: boolean} = {[@A`@B`@C]}
    )");

    Luau::AutocompleteResult ac;

    ac = autocomplete('1');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('2');

    LUAU_CHECK_HAS_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_KEY(ac.entryMap, "right");

    ac = autocomplete('3');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('4');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('5');

    LUAU_CHECK_HAS_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_KEY(ac.entryMap, "right");

    ac = autocomplete('6');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('7');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('8');

    LUAU_CHECK_HAS_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_KEY(ac.entryMap, "right");

    ac = autocomplete('9');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('A');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('B');

    LUAU_CHECK_HAS_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_KEY(ac.entryMap, "right");

    ac = autocomplete('C');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");
}

// https://github.com/Roblox/luau/issues/858
TEST_CASE_FIXTURE(ACFixture, "string_singleton_in_if_statement2")
{
    // don't run this when the DCR flag isn't set
    if (!FFlag::LuauSolverV2)
        return;

    check(R"(
        --!strict

        type Direction = "left" | "right"

        local dir: Direction
        -- typestate here means dir is actually typed as `"left"`
        dir = "left"

        if dir == @1"@2"@3 then end
        local a: {[Direction]: boolean} = {[@4"@5"@6]}

        if dir == @7`@8`@9 then end
        local a: {[Direction]: boolean} = {[@A`@B`@C]}
    )");

    Luau::AutocompleteResult ac;

    ac = autocomplete('1');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('2');

    LUAU_CHECK_HAS_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('3');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('4');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('5');

    LUAU_CHECK_HAS_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_KEY(ac.entryMap, "right");

    ac = autocomplete('6');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('7');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('8');

    LUAU_CHECK_HAS_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('9');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('A');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");

    ac = autocomplete('B');

    LUAU_CHECK_HAS_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_KEY(ac.entryMap, "right");

    ac = autocomplete('C');

    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "left");
    LUAU_CHECK_HAS_NO_KEY(ac.entryMap, "right");
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_string_singleton_equality")
{
    check(R"(
        type tagged = {tag:"cat", fieldx:number} | {tag:"dog", fieldy:number}
        local x: tagged = {tag="cat", fieldx=2}
        if x.tag == "@1" or "@2" ~= x.tag then end
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("cat"));
    CHECK(ac.entryMap.count("dog"));

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("cat"));
    CHECK(ac.entryMap.count("dog"));

    // CLI-48823: assignment to x.tag should also autocomplete, but union l-values are not supported yet
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_boolean_singleton")
{
    check(R"(
local function f(x: true) end
f(@1)
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("true"));
    CHECK(ac.entryMap["true"].typeCorrect == TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap.count("false"));
    CHECK(ac.entryMap["false"].typeCorrect == TypeCorrectKind::None);
    CHECK_EQ(ac.context, AutocompleteContext::Expression);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_string_singleton_escape")
{
    check(R"(
        type tag = "strange\t\"cat\"" | 'nice\t"dog"'
        local function f(x: tag) end
        f(@1)
        f("@2")
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("\"strange\\t\\\"cat\\\"\""));
    CHECK(ac.entryMap.count("\"nice\\t\\\"dog\\\"\""));

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("strange\\t\\\"cat\\\""));
    CHECK(ac.entryMap.count("nice\\t\\\"dog\\\""));
}

TEST_CASE_FIXTURE(ACFixture, "function_in_assignment_has_parentheses_2")
{
    check(R"(
local bar: ((number) -> number) & (number, number) -> number)
local abc = b@1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("bar"));
    CHECK(ac.entryMap["bar"].parens == ParenthesesRecommendation::CursorInside);
}

TEST_CASE_FIXTURE(ACFixture, "no_incompatible_self_calls_on_class")
{
    loadDefinition(R"(
declare class Foo
    function one(self): number
    two: () -> number
end
    )");

    {
        check(R"(
local function f(t: Foo)
    t:@1
end
        )");

        auto ac = autocomplete('1');

        REQUIRE(ac.entryMap.count("one"));
        REQUIRE(ac.entryMap.count("two"));
        CHECK(!ac.entryMap["one"].wrongIndexType);
        CHECK(ac.entryMap["two"].wrongIndexType);
        CHECK(ac.entryMap["one"].indexedWithSelf);
        CHECK(ac.entryMap["two"].indexedWithSelf);
    }

    {
        check(R"(
local function f(t: Foo)
    t.@1
end
        )");

        auto ac = autocomplete('1');

        REQUIRE(ac.entryMap.count("one"));
        REQUIRE(ac.entryMap.count("two"));
        CHECK(ac.entryMap["one"].wrongIndexType);
        CHECK(!ac.entryMap["two"].wrongIndexType);
        CHECK(!ac.entryMap["one"].indexedWithSelf);
        CHECK(!ac.entryMap["two"].indexedWithSelf);
    }
}

TEST_CASE_FIXTURE(ACFixture, "simple")
{
    check(R"(
local t = {}
function t:m() end
t:m()
    )");

    // auto ac = autocomplete('1');

    //  REQUIRE(ac.entryMap.count("m"));
    // CHECK(!ac.entryMap["m"].wrongIndexType);
}

TEST_CASE_FIXTURE(ACFixture, "do_compatible_self_calls")
{
    check(R"(
local t = {}
function t:m() end
t:@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("m"));
    CHECK(!ac.entryMap["m"].wrongIndexType);
    CHECK(ac.entryMap["m"].indexedWithSelf);
}

TEST_CASE_FIXTURE(ACFixture, "no_incompatible_self_calls")
{
    check(R"(
local t = {}
function t.m() end
t:@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("m"));
    CHECK(ac.entryMap["m"].wrongIndexType);
    CHECK(ac.entryMap["m"].indexedWithSelf);
}

TEST_CASE_FIXTURE(ACFixture, "no_incompatible_self_calls_2")
{
    check(R"(
local f: (() -> number) & ((number) -> number) = function(x: number?) return 2 end
local t = {}
t.f = f
t:@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("f"));
    CHECK(ac.entryMap["f"].wrongIndexType);
    CHECK(ac.entryMap["f"].indexedWithSelf);
}

TEST_CASE_FIXTURE(ACFixture, "do_wrong_compatible_self_calls")
{
    check(R"(
local t = {}
function t.m(x: typeof(t)) end
t:@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("m"));
    // We can make changes to mark this as a wrong way to call even though it's compatible
    CHECK(!ac.entryMap["m"].wrongIndexType);
    CHECK(ac.entryMap["m"].indexedWithSelf);
}

TEST_CASE_FIXTURE(ACFixture, "do_wrong_compatible_nonself_calls")
{
    check(R"(
local t = {}
function t:m(x: string) end
t.@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("m"));

    if (FFlag::LuauSolverV2)
        CHECK(ac.entryMap["m"].wrongIndexType);
    else
        CHECK(!ac.entryMap["m"].wrongIndexType);
    CHECK(!ac.entryMap["m"].indexedWithSelf);
}

TEST_CASE_FIXTURE(ACFixture, "no_wrong_compatible_self_calls_with_generics")
{
    check(R"(
local t = {}
function t.m<T>(a: T) end
t:@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("m"));
    // While this call is compatible with the type, this requires instantiation of a generic type which we don't perform
    CHECK(ac.entryMap["m"].wrongIndexType);
    CHECK(ac.entryMap["m"].indexedWithSelf);
}

TEST_CASE_FIXTURE(ACFixture, "string_prim_self_calls_are_fine")
{
    check(R"(
local s = "hello"
s:@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("byte"));
    CHECK(ac.entryMap["byte"].wrongIndexType == false);
    CHECK(ac.entryMap["byte"].indexedWithSelf);
    REQUIRE(ac.entryMap.count("char"));
    CHECK(ac.entryMap["char"].wrongIndexType == true);
    CHECK(ac.entryMap["char"].indexedWithSelf);
    REQUIRE(ac.entryMap.count("sub"));
    CHECK(ac.entryMap["sub"].wrongIndexType == false);
    CHECK(ac.entryMap["sub"].indexedWithSelf);
}

TEST_CASE_FIXTURE(ACFixture, "string_prim_non_self_calls_are_avoided")
{
    check(R"(
local s = "hello"
s.@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("char"));
    CHECK(ac.entryMap["char"].wrongIndexType == false);
    CHECK(!ac.entryMap["char"].indexedWithSelf);
    REQUIRE(ac.entryMap.count("sub"));
    CHECK(ac.entryMap["sub"].wrongIndexType == true);
    CHECK(!ac.entryMap["sub"].indexedWithSelf);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "library_non_self_calls_are_fine")
{
    check(R"(
string.@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("byte"));
    CHECK(ac.entryMap["byte"].wrongIndexType == false);
    CHECK(!ac.entryMap["byte"].indexedWithSelf);
    REQUIRE(ac.entryMap.count("char"));
    CHECK(ac.entryMap["char"].wrongIndexType == false);
    CHECK(!ac.entryMap["char"].indexedWithSelf);
    REQUIRE(ac.entryMap.count("sub"));
    CHECK(ac.entryMap["sub"].wrongIndexType == false);
    CHECK(!ac.entryMap["sub"].indexedWithSelf);

    check(R"(
table.@1
    )");

    ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("remove"));
    CHECK(ac.entryMap["remove"].wrongIndexType == false);
    CHECK(!ac.entryMap["remove"].indexedWithSelf);
    REQUIRE(ac.entryMap.count("getn"));
    CHECK(ac.entryMap["getn"].wrongIndexType == false);
    CHECK(!ac.entryMap["getn"].indexedWithSelf);
    REQUIRE(ac.entryMap.count("insert"));
    CHECK(ac.entryMap["insert"].wrongIndexType == false);
    CHECK(!ac.entryMap["insert"].indexedWithSelf);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "library_self_calls_are_invalid")
{
    check(R"(
string:@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("byte"));
    CHECK(ac.entryMap["byte"].wrongIndexType == true);
    CHECK(ac.entryMap["byte"].indexedWithSelf);
    REQUIRE(ac.entryMap.count("char"));
    CHECK(ac.entryMap["char"].wrongIndexType == true);
    CHECK(ac.entryMap["char"].indexedWithSelf);

    // We want the next test to evaluate to 'true', but we have to allow function defined with 'self' to be callable with ':'
    // We may change the definition of the string metatable to not use 'self' types in the future (like byte/char/pack/unpack)
    REQUIRE(ac.entryMap.count("sub"));
    CHECK(ac.entryMap["sub"].wrongIndexType == false);
    CHECK(ac.entryMap["sub"].indexedWithSelf);
}

TEST_CASE_FIXTURE(ACFixture, "source_module_preservation_and_invalidation")
{
    check(R"(
local a = { x = 2, y = 4 }
a.@1
    )");

    getFrontend().clear();

    auto ac = autocomplete('1');

    CHECK(2 == ac.entryMap.size());

    CHECK(ac.entryMap.count("x"));
    CHECK(ac.entryMap.count("y"));

    getFrontend().check("MainModule", {});

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("x"));
    CHECK(ac.entryMap.count("y"));

    getFrontend().markDirty("MainModule", nullptr);

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("x"));
    CHECK(ac.entryMap.count("y"));

    getFrontend().check("MainModule", {});

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("x"));
    CHECK(ac.entryMap.count("y"));
}

TEST_CASE_FIXTURE(ACFixture, "globals_are_order_independent")
{
    check(R"(
        local myLocal = 4
        function abc0()
            local myInnerLocal = 1
@1
        end

        function abc1()
            local myInnerLocal = 1
        end
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("myLocal"));
    CHECK(ac.entryMap.count("myInnerLocal"));
    CHECK(ac.entryMap.count("abc0"));
    CHECK(ac.entryMap.count("abc1"));
}

TEST_CASE_FIXTURE(ACFixture, "string_contents_is_available_to_callback")
{
    loadDefinition(R"(
        declare function require(path: string): any
    )");

    GlobalTypes& globals = FFlag::LuauSolverV2 ? getFrontend().globals : getFrontend().globalsForAutocomplete;

    std::optional<Binding> require = globals.globalScope->linearSearchForBinding("require");
    REQUIRE(require);
    Luau::unfreeze(globals.globalTypes);
    attachTag(require->typeId, "RequireCall");
    Luau::freeze(globals.globalTypes);

    check(R"(
        local x = require("testing/@1")
    )");

    bool isCorrect = false;
    auto ac1 = autocomplete(
        '1',
        [&isCorrect](std::string, std::optional<const ExternType*>, std::optional<std::string> contents) -> std::optional<AutocompleteEntryMap>
        {
            isCorrect = contents && *contents == "testing/";
            return std::nullopt;
        }
    );

    CHECK(isCorrect);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "require_by_string")
{
    fileResolver.source["MainModule"] = R"(
        local info = "MainModule serves as the root directory"
    )";

    fileResolver.source["MainModule/Folder"] = R"(
        local info = "MainModule/Folder serves as a subdirectory"
    )";

    fileResolver.source["MainModule/Folder/Requirer"] = R"(
        local res0 = require("@")

        local res1 = require(".")
        local res2 = require("./")
        local res3 = require("./Sib")

        local res4 = require("..")
        local res5 = require("../")
        local res6 = require("../Sib")
    )";

    fileResolver.source["MainModule/Folder/SiblingDependency"] = R"(
        return {"result"}
    )";

    fileResolver.source["MainModule/ParentDependency"] = R"(
        return {"result"}
    )";

    struct RequireCompletion
    {
        std::string label;
        std::string insertText;
    };

    auto checkEntries = [](const AutocompleteEntryMap& entryMap, const std::vector<RequireCompletion>& completions)
    {
        CHECK(completions.size() == entryMap.size());
        for (const auto& completion : completions)
        {
            CHECK(entryMap.count(completion.label));
            CHECK(entryMap.at(completion.label).insertText == completion.insertText);
        }
    };

    AutocompleteResult acResult;
    acResult = autocomplete("MainModule/Folder/Requirer", Position{1, 31});
    checkEntries(acResult.entryMap, {{"@defaultalias", "@defaultalias"}, {"./", "./"}, {"../", "../"}});

    acResult = autocomplete("MainModule/Folder/Requirer", Position{3, 31});
    checkEntries(acResult.entryMap, {{"@defaultalias", "@defaultalias"}, {"./", "./"}, {"../", "../"}});
    acResult = autocomplete("MainModule/Folder/Requirer", Position{4, 32});
    checkEntries(acResult.entryMap, {{"..", "."}, {"Requirer", "./Requirer"}, {"SiblingDependency", "./SiblingDependency"}});
    acResult = autocomplete("MainModule/Folder/Requirer", Position{5, 35});
    checkEntries(acResult.entryMap, {{"..", "."}, {"Requirer", "./Requirer"}, {"SiblingDependency", "./SiblingDependency"}});

    acResult = autocomplete("MainModule/Folder/Requirer", Position{7, 32});
    checkEntries(acResult.entryMap, {{"@defaultalias", "@defaultalias"}, {"./", "./"}, {"../", "../"}});
    acResult = autocomplete("MainModule/Folder/Requirer", Position{8, 33});
    checkEntries(acResult.entryMap, {{"..", "../.."}, {"Folder", "../Folder"}, {"ParentDependency", "../ParentDependency"}});
    acResult = autocomplete("MainModule/Folder/Requirer", Position{9, 36});
    checkEntries(acResult.entryMap, {{"..", "../.."}, {"Folder", "../Folder"}, {"ParentDependency", "../ParentDependency"}});
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_response_perf1" * doctest::timeout(0.5))
{
    if (FFlag::LuauSolverV2)
        return; // FIXME: This test is just barely at the threshhold which makes it very flaky under the new solver

    // Build a function type with a large overload set
    const int parts = 100;
    std::string source;

    for (int i = 0; i < parts; i++)
        formatAppend(source, "type T%d = { f%d: number }\n", i, i);

    source += "type Instance = { new: (('s0', extra: Instance?) -> T0)";

    for (int i = 1; i < parts; i++)
        formatAppend(source, " & (('s%d', extra: Instance?) -> T%d)", i, i);

    source += " }\n";

    source += "local Instance: Instance = {} :: any\n";
    source += "local function c(): boolean return t@1 end\n";

    check(source);

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("true"));
    CHECK(ac.entryMap.count("Instance"));
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_subtyping_recursion_limit")
{
    // TODO: in old solver, type resolve can't handle the type in this test without a stack overflow
    if (!FFlag::LuauSolverV2)
        return;

    ScopedFastInt luauTypeInferRecursionLimit{FInt::LuauTypeInferRecursionLimit, 10};
    ScopedFastInt luauSubtypingRecursionLimit{DFInt::LuauSubtypingRecursionLimit, 10};

    const int parts = 100;
    std::string source;

    source += "function f()\n";

    std::string prefix;
    for (int i = 0; i < parts; i++)
        formatAppend(prefix, "(nil|({a%d:number}&", i);
    formatAppend(prefix, "(nil|{a%d:number})", parts);
    for (int i = 0; i < parts; i++)
        formatAppend(prefix, "))");

    source += "local x1 : " + prefix + "\n";
    source += "local y : {a1:number} = x@1\n";

    source += "end\n";

    check(source);

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("true"));
    CHECK(ac.entryMap.count("x1"));
}

TEST_CASE_FIXTURE(ACFixture, "strict_mode_force")
{
    check(R"(
--!nonstrict
local a: {x: number} = {x=1}
local b = a
local c = b.@1
    )");

    auto ac = autocomplete('1');

    CHECK_EQ(1, ac.entryMap.size());
    CHECK(ac.entryMap.count("x"));
}

TEST_CASE_FIXTURE(ACFixture, "suggest_exported_types")
{
    check(R"(
export type Type = {a: number}
local a: T@1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("Type"));
    CHECK_EQ(ac.context, AutocompleteContext::Type);
}

TEST_CASE_FIXTURE(ACFixture, "getFrontend().use_correct_global_scope")
{
    loadDefinition(R"(
        declare class Instance
            Name: string
        end
    )");

    CheckResult result = check(R"(
        local a: unknown = nil
        if typeof(a) == "Instance" then
            local b = a.@1
        end
    )");
    auto ac = autocomplete('1');

    CHECK_EQ(1, ac.entryMap.size());
    CHECK(ac.entryMap.count("Name"));
}

TEST_CASE_FIXTURE(ACFixture, "string_completion_outside_quotes")
{
    loadDefinition(R"(
        declare function require(path: string): any
    )");

    GlobalTypes& globals = FFlag::LuauSolverV2 ? getFrontend().globals : getFrontend().globalsForAutocomplete;

    std::optional<Binding> require = globals.globalScope->linearSearchForBinding("require");
    REQUIRE(require);
    Luau::unfreeze(globals.globalTypes);
    attachTag(require->typeId, "RequireCall");
    Luau::freeze(globals.globalTypes);

    check(R"(
        local x = require(@1"@2"@3)
    )");

    StringCompletionCallback callback =
        [](std::string, std::optional<const ExternType*>, std::optional<std::string> contents) -> std::optional<AutocompleteEntryMap>
    {
        Luau::AutocompleteEntryMap results = {{"test", Luau::AutocompleteEntry{Luau::AutocompleteEntryKind::String, std::nullopt, false, false}}};
        return results;
    };

    auto ac = autocomplete('2', callback);

    CHECK_EQ(1, ac.entryMap.size());
    CHECK(ac.entryMap.count("test"));

    ac = autocomplete('1', callback);

    CHECK_EQ(0, ac.entryMap.size());

    ac = autocomplete('3', callback);

    CHECK_EQ(0, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_empty")
{
    check(R"(
local function foo(a: () -> ())
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function()  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_args")
{
    check(R"(
local function foo(a: (number, string) -> ())
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(a0: number, a1: string)  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_args_single_return")
{
    check(R"(
local function foo(a: (number, string) -> (string))
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(a0: number, a1: string): string  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_args_multi_return")
{
    check(R"(
local function foo(a: (number, string) -> (string, number))
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(a0: number, a1: string): (string, number)  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled__noargs_multi_return")
{
    check(R"(
local function foo(a: () -> (string, number))
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(): (string, number)  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled__varargs_multi_return")
{
    check(R"(
local function foo(a: (...number) -> (string, number))
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(...: number): (string, number)  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_multi_varargs_multi_return")
{
    check(R"(
local function foo(a: (string, ...number) -> (string, number))
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(a0: string, ...: number): (string, number)  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_multi_varargs_varargs_return")
{
    check(R"(
local function foo(a: (string, ...number) -> ...number)
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(a0: string, ...: number): ...number  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_multi_varargs_multi_varargs_return")
{
    check(R"(
local function foo(a: (string, ...number) -> (boolean, ...number))
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(a0: string, ...: number): (boolean, ...number)  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_named_args")
{
    check(R"(
local function foo(a: (foo: number, bar: string) -> (string, number))
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(foo: number, bar: string): (string, number)  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_partially_args")
{
    check(R"(
local function foo(a: (number, bar: string) -> (string, number))
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(a0: number, bar: string): (string, number)  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_partially_args_last")
{
    check(R"(
local function foo(a: (foo: number, string) -> (string, number))
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(foo: number, a1: string): (string, number)  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_typeof_args")
{
    check(R"(
local t = { a = 1, b = 2 }

local function foo(a: (foo: typeof(t)) -> ())
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(foo)  end"; // Cannot utter this type.

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_table_literal_args")
{
    check(R"(
local function foo(a: (tbl: { x: number, y: number }) -> number) return a({x=2, y = 3}) end
foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(tbl: { x: number, y: number }): number  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_typeof_returns")
{
    check(R"(
local t = { a = 1, b = 2 }

local function foo(a: () -> typeof(t))
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function()  end"; // Cannot utter this type.

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_table_literal_args")
{
    check(R"(
local function foo(a: () -> { x: number, y: number }) return {x=2, y = 3} end
foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(): { x: number, y: number }  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_typeof_vararg")
{
    check(R"(
local t = { a = 1, b = 2 }

local function foo(a: (...typeof(t)) -> ())
    a()
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(...)  end"; // Cannot utter this type.

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_generic_type_pack_vararg")
{
    // CLI-116932 - Autocomplete on a anonymous function in a function argument should not recommend a function with a generic parameter.
    if (FFlag::LuauSolverV2)
        return;
    check(R"(
local function foo<A>(a: (...A) -> number, ...: A)
	return a(...)
end

foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = "function(...): number  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "anonymous_autofilled_generic_on_argument_type_pack_vararg")
{
    // Caveat lector!  This is actually invalid syntax!
    // The correct syntax would be as follows:
    //
    // local function foo(a: <T...>(T...) -> number)
    //
    // We leave it as-written here because we still expect autocomplete to
    // handle this code sensibly.
    CheckResult result = check(R"(
        local function foo(a: <T...>(...: T...) -> number)
            return a(4, 5, 6)
        end

        foo(@1)
    )");

    const std::optional<std::string> EXPECTED_INSERT = FFlag::LuauSolverV2 ? "function(...: number): number  end" : "function(...): number  end";

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count(kGeneratedAnonymousFunctionEntryName) == 1);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].kind == Luau::AutocompleteEntryKind::GeneratedFunction);
    CHECK(ac.entryMap[kGeneratedAnonymousFunctionEntryName].typeCorrect == Luau::TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
    CHECK_EQ(EXPECTED_INSERT, *ac.entryMap[kGeneratedAnonymousFunctionEntryName].insertText);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_at_end_of_stmt_should_continue_as_part_of_stmt")
{
    check(R"(
local data = { x = 1 }
local var = data.@1
    )");
    auto ac = autocomplete('1');
    CHECK(!ac.entryMap.empty());
    CHECK(ac.entryMap.count("x"));
    CHECK_EQ(ac.context, AutocompleteContext::Property);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_after_semicolon_should_complete_a_new_statement")
{
    check(R"(
local data = { x = 1 }
local var = data;@1
    )");
    auto ac = autocomplete('1');
    CHECK(!ac.entryMap.empty());
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
    CHECK_EQ(ac.context, AutocompleteContext::Statement);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "require_tracing")
{
    fileResolver.source["Module/A"] = R"(
return { x = 0 }
    )";

    fileResolver.source["Module/B"] = R"(
local result = require(script.Parent.A)
local x = 1 + result.
    )";

    auto ac = autocomplete("Module/B", Position{2, 21});

    CHECK(ac.entryMap.size() == 1);
    CHECK(ac.entryMap.count("x"));
}

TEST_CASE_FIXTURE(ACExternTypeFixture, "ac_dont_overflow_on_recursive_union")
{
    check(R"(
        local table1: {ChildClass} = {}
        local table2 = {}

        for index, value in table2[1] do
            table.insert(table1, value)
            value.@1
        end
    )");

    auto ac = autocomplete('1');

    if (FFlag::LuauSolverV2)
    {
        CHECK(ac.entryMap.count("BaseMethod") > 0);
        CHECK(ac.entryMap.count("Method") > 0);
    }
    else
    {
        // Otherwise, we don't infer anything for `value`, which is _fine_.
        CHECK(ac.entryMap.empty());
    }
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "type_function_has_types_definitions")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    check(R"(
type function foo()
    types.@1
end
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("singleton"), 1);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "type_function_private_scope")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    // Global scope polution by the embedder has no effect
    addGlobalBinding(getFrontend().globals, "thisAlsoShouldNotBeThere", Binding{getBuiltins()->anyType});
    addGlobalBinding(getFrontend().globalsForAutocomplete, "thisAlsoShouldNotBeThere", Binding{getBuiltins()->anyType});

    check(R"(
local function thisShouldNotBeThere() end

type function thisShouldBeThere() end

type function foo()
    this@1
end

this@2
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("thisShouldNotBeThere"), 0);
    CHECK_EQ(ac.entryMap.count("thisAlsoShouldNotBeThere"), 0);
    CHECK_EQ(ac.entryMap.count("thisShouldBeThere"), 1);

    ac = autocomplete('2');
    CHECK_EQ(ac.entryMap.count("thisShouldNotBeThere"), 1);
    CHECK_EQ(ac.entryMap.count("thisAlsoShouldNotBeThere"), 1);
    CHECK_EQ(ac.entryMap.count("thisShouldBeThere"), 0);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "type_function_eval_in_autocomplete")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    check(R"(
type function foo(x)
    local tbl = types.newtable(nil, nil, nil)
    tbl:setproperty(types.singleton("boolean"), x)
    tbl:setproperty(types.singleton("number"), types.number)
    return tbl
end

local function test(a: foo<string>)
    return a.@1
end
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("boolean"), 1);
    CHECK_EQ(ac.entryMap.count("number"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_for_assignment")
{
    check(R"(
        local function foobar(tbl: { tag: "left" | "right" })
            tbl.tag = "@1"
        end
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("left"), 1);
    CHECK_EQ(ac.entryMap.count("right"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_in_local_table")
{
    check(R"(
        type Entry = { field: number, prop: string }
        local x : {Entry} = {}
        x[1] = {
           f@1,
           p@2,
        }

        local t : { key1: boolean, thing2: CFrame, aaa3: vector } = {
            k@3,
            th@4,
        }
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.count("field"), 1);
    auto ac2 = autocomplete('2');
    CHECK_EQ(ac2.entryMap.count("prop"), 1);
    auto ac3 = autocomplete('3');
    CHECK_EQ(ac3.entryMap.count("key1"), 1);
    auto ac4 = autocomplete('4');
    CHECK_EQ(ac4.entryMap.count("thing2"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_in_type_assertion")
{
    check(R"(
        type Entry = { field: number, prop: string }
        return ( { f@1, p@2 } :: Entry )
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.count("field"), 1);
    auto ac2 = autocomplete('2');
    CHECK_EQ(ac2.entryMap.count("prop"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_implicit_named_index_index_expr")
{
    // Somewhat surprisingly, the old solver didn't cover this case.
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    check(R"(
        type Constraint = "A" | "B" | "C"
        local foo : { [Constraint]: string } = {
            A = "Value for A",
            B = "Value for B",
            C = "Value for C",
        }
        foo["@1"]
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("A"), 1);
    CHECK_EQ(ac.entryMap["A"].kind, AutocompleteEntryKind::String);
    CHECK_EQ(ac.entryMap.count("B"), 1);
    CHECK_EQ(ac.entryMap["B"].kind, AutocompleteEntryKind::String);
    CHECK_EQ(ac.entryMap.count("C"), 1);
    CHECK_EQ(ac.entryMap["C"].kind, AutocompleteEntryKind::String);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_implicit_named_index_index_expr_without_annotation")
{
    ScopedFastFlag sffs{FFlag::LuauSolverV2, true};

    check(R"(
        local foo = {
            ["Item/Foo"] = 42,
            ["Item/Bar"] = "it's true",
            ["Item/Baz"] = true,
        }
        foo["@1"]
    )");

    auto ac = autocomplete('1');

    auto checkEntry = [&](auto key, auto type)
    {
        REQUIRE_EQ(ac.entryMap.count(key), 1);
        auto entry = ac.entryMap.at(key);
        CHECK_EQ(entry.kind, AutocompleteEntryKind::Property);
        REQUIRE(entry.type);
        CHECK_EQ(type, toString(*entry.type));
    };

    checkEntry("Item/Foo", "number");
    checkEntry("Item/Bar", "string");
    checkEntry("Item/Baz", "boolean");
}

TEST_CASE_FIXTURE(ACFixture, "bidirectional_autocomplete_in_function_call")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    check(R"(
        local function take(_: { choice: "left" | "right" }) end

        take({ choice = "@1" })
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("left"), 1);
    CHECK_EQ(ac.entryMap.count("right"), 1);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "autocomplete_via_bidirectional_self")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    check(R"(
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
            print("My balance is: " .. self.@1)
        end
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("name"), 1);
    CHECK_EQ(ac.entryMap.count("balance"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_include_break_continue_in_loop")
{
    ScopedFastFlag sff{FFlag::LuauIncludeBreakContinueStatements, true};

    check(R"(for x in y do
        @1
        if true then
            @2
        end
    end)");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("break") > 0);
    CHECK(ac.entryMap.count("continue") > 0);

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("break") > 0);
    CHECK(ac.entryMap.count("continue") > 0);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_exclude_break_continue_outside_loop")
{
    ScopedFastFlag sff{FFlag::LuauIncludeBreakContinueStatements, true};

    check(R"(@1if true then
        @2
    end)");

    auto ac = autocomplete('1');

    CHECK_EQ(ac.entryMap.count("break"), 0);
    CHECK_EQ(ac.entryMap.count("continue"), 0);

    ac = autocomplete('2');
    CHECK_EQ(ac.entryMap.count("break"), 0);
    CHECK_EQ(ac.entryMap.count("continue"), 0);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_exclude_break_continue_function_boundary")
{
    ScopedFastFlag sff{FFlag::LuauIncludeBreakContinueStatements, true};

    check(R"(for i = 1, 10 do
    local function helper()
        @1
    end
    end)");

    auto ac = autocomplete('1');

    CHECK_EQ(ac.entryMap.count("break"), 0);
    CHECK_EQ(ac.entryMap.count("continue"), 0);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_exclude_break_continue_in_param")
{
    ScopedFastFlag sff{FFlag::LuauIncludeBreakContinueStatements, true};

    check(R"(while @1 do
        end)");

    auto ac = autocomplete('1');

    CHECK_EQ(ac.entryMap.count("break"), 0);
    CHECK_EQ(ac.entryMap.count("continue"), 0);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_exclude_break_continue_incomplete_while")
{
    ScopedFastFlag sff{FFlag::LuauIncludeBreakContinueStatements, true};

    check("while @1");

    auto ac = autocomplete('1');

    CHECK_EQ(ac.entryMap.count("break"), 0);
    CHECK_EQ(ac.entryMap.count("continue"), 0);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_exclude_break_continue_incomplete_for")
{
    ScopedFastFlag sff{FFlag::LuauIncludeBreakContinueStatements, true};

    check("for @1 in @2 do");

    auto ac = autocomplete('1');

    ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("break"), 0);
    CHECK_EQ(ac.entryMap.count("continue"), 0);

    ac = autocomplete('2');
    CHECK_EQ(ac.entryMap.count("break"), 0);
    CHECK_EQ(ac.entryMap.count("continue"), 0);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_exclude_break_continue_expr_func")
{
    ScopedFastFlag sff{FFlag::LuauIncludeBreakContinueStatements, true};

    check(R"(while true do
        local _ = function ()
        @1
        end
    end)");

    auto ac = autocomplete('1');

    CHECK_EQ(ac.entryMap.count("break"), 0);
    CHECK_EQ(ac.entryMap.count("continue"), 0);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_include_break_continue_in_repeat")
{
    ScopedFastFlag sff{FFlag::LuauIncludeBreakContinueStatements, true};

    check(R"(repeat
        @1
    until foo())");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("break") > 0);
    CHECK(ac.entryMap.count("continue") > 0);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_include_break_continue_in_nests")
{
    ScopedFastFlag sff{FFlag::LuauIncludeBreakContinueStatements, true};

    check(R"(while ((function ()
        while true do
            @1
        end
        end)()) do
    end)");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("break") > 0);
    CHECK(ac.entryMap.count("continue") > 0);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_exclude_break_continue_in_incomplete_loop")
{
    ScopedFastFlag sff{FFlag::LuauIncludeBreakContinueStatements, true};

    check(R"(while foo() do
        @1)");

    auto ac = autocomplete('1');

    // We'd like to include break/continue here but the incomplete loop ends immediately.
    CHECK_EQ(ac.entryMap.count("break"), 0);
    CHECK_EQ(ac.entryMap.count("continue"), 0);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_suggest_hot_comments")
{
    ScopedFastFlag sff{FFlag::LuauSuggestHotComments, true};

    check("--!@1");

    auto ac = autocomplete('1');

    CHECK(!ac.entryMap.empty());
    CHECK(ac.entryMap.count("strict"));
    CHECK(ac.entryMap.count("nonstrict"));
    CHECK(ac.entryMap.count("nocheck"));
    CHECK(ac.entryMap.count("native"));
    CHECK(ac.entryMap.count("nolint"));
    CHECK(ac.entryMap.count("optimize"));
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_method_in_unfinished_repeat_body_eof")
{
    ScopedFastFlag sff{FFlag::LuauUnfinishedRepeatAncestryFix, true};

    check(R"(local t = {}
        function t:Foo() end
        repeat
        t:@1)");

    auto ac = autocomplete('1');

    CHECK(!ac.entryMap.empty());
    CHECK(ac.entryMap.count("Foo"));
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_method_in_unfinished_repeat_body_not_eof")
{
    ScopedFastFlag sff{FFlag::LuauUnfinishedRepeatAncestryFix, true};

    check(R"(local t = {}
        function t:Foo() end
        repeat
        t:@1
        )");

    auto ac = autocomplete('1');

    CHECK(!ac.entryMap.empty());
    CHECK(ac.entryMap.count("Foo"));
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_method_in_unfinished_while_body")
{
    check(R"(local t = {}
        function t:Foo() end
        while true do
        t:@1)");

    auto ac = autocomplete('1');

    CHECK(!ac.entryMap.empty());
    CHECK(ac.entryMap.count("Foo"));
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "autocomplete_empty_attribute")
{
    ScopedFastFlag sff[]{{FFlag::LuauParametrizedAttributeSyntax, true}, {FFlag::LuauAutocompleteAttributes, true}};

    check(R"(
        \@@1
        function foo() return 42 end
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("deprecated"), 1);
    CHECK_EQ(ac.entryMap.count("checked"), 1);
    CHECK_EQ(ac.entryMap.count("native"), 1);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "autocomplete_deprecated_attribute")
{
    ScopedFastFlag sff[]{{FFlag::LuauParametrizedAttributeSyntax, true}, {FFlag::LuauAutocompleteAttributes, true}};

    check(R"(
        \@dep@1
        function foo() return 42 end
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("deprecated"), 1);
    CHECK_EQ(ac.entryMap.count("checked"), 1);
    CHECK_EQ(ac.entryMap.count("native"), 1);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "autocomplete_empty_braced_attribute")
{
    ScopedFastFlag sff[]{{FFlag::LuauParametrizedAttributeSyntax, true}, {FFlag::LuauAutocompleteAttributes, true}};

    check(R"(
        \@[@1]
        function foo() return 42 end
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("deprecated"), 1);
    CHECK_EQ(ac.entryMap.count("checked"), 1);
    CHECK_EQ(ac.entryMap.count("native"), 1);
}

TEST_CASE_FIXTURE(ACBuiltinsFixture, "autocomplete_deprecated_braced_attribute")
{
    ScopedFastFlag sff[]{{FFlag::LuauParametrizedAttributeSyntax, true}, {FFlag::LuauAutocompleteAttributes, true}};

    check(R"(
        \@[dep@1]
        function foo() return 42 end
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("deprecated"), 1);
    CHECK_EQ(ac.entryMap.count("checked"), 1);
    CHECK_EQ(ac.entryMap.count("native"), 1);
}

TEST_SUITE_END();
