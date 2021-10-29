// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Autocomplete.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Parser.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"
#include "Luau/StringUtils.h"

#include "Fixture.h"

#include "doctest.h"

#include <map>

LUAU_FASTFLAG(LuauTraceTypesInNonstrictMode2)
LUAU_FASTFLAG(LuauSetMetatableDoesNotTimeTravel)

using namespace Luau;

static std::optional<AutocompleteEntryMap> nullCallback(std::string tag, std::optional<const ClassTypeVar*> ptr)
{
    return std::nullopt;
}

struct ACFixture : Fixture
{
    AutocompleteResult autocomplete(unsigned row, unsigned column)
    {
        return Luau::autocomplete(frontend, "MainModule", Position{row, column}, nullCallback);
    }

    AutocompleteResult autocomplete(char marker)
    {
        auto i = markerPosition.find(marker);
        LUAU_ASSERT(i != markerPosition.end());
        const Position& pos = i->second;
        return Luau::autocomplete(frontend, "MainModule", pos, nullCallback);
    }

    CheckResult check(const std::string& source)
    {
        markerPosition.clear();
        std::string filteredSource;
        filteredSource.reserve(source.size());

        Position curPos(0, 0);
        for (char c : source)
        {
            if (c == '@' && !filteredSource.empty())
            {
                char prevChar = filteredSource.back();
                filteredSource.pop_back();
                curPos.column--; // Adjust column position since we removed a character from the output
                LUAU_ASSERT("Illegal marker character" && prevChar >= '0' && prevChar <= '9');
                LUAU_ASSERT("Duplicate marker found" && markerPosition.count(prevChar) == 0);
                markerPosition.insert(std::pair{prevChar, curPos});
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
        }

        return Fixture::check(filteredSource);
    }

    // Maps a marker character (0-9 inclusive) to a position in the source code.
    std::map<char, Position> markerPosition;
};

TEST_SUITE_BEGIN("AutocompleteTest");

TEST_CASE_FIXTURE(ACFixture, "empty_program")
{
    check(" ");

    auto ac = autocomplete(0, 1);

    CHECK(!ac.entryMap.empty());
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "local_initializer")
{
    check("local a = ");

    auto ac = autocomplete(0, 10);
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "leave_numbers_alone")
{
    check("local a = 3.1");

    auto ac = autocomplete(0, 12);
    CHECK(ac.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "user_defined_globals")
{
    check("local myLocal = 4; ");

    auto ac = autocomplete(0, 19);

    CHECK(ac.entryMap.count("myLocal"));
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "dont_suggest_local_before_its_definition")
{
    check(R"(
        local myLocal = 4
        function abc()
            local myInnerLocal = 1

        end
    )");

    auto ac = autocomplete(3, 0);
    CHECK(ac.entryMap.count("myLocal"));
    CHECK(!ac.entryMap.count("myInnerLocal"));

    ac = autocomplete(4, 0);
    CHECK(ac.entryMap.count("myLocal"));
    CHECK(ac.entryMap.count("myInnerLocal"));

    ac = autocomplete(6, 0);
    CHECK(ac.entryMap.count("myLocal"));
    CHECK(!ac.entryMap.count("myInnerLocal"));
}

TEST_CASE_FIXTURE(ACFixture, "recursive_function")
{
    check(R"(
        function foo()
        end
    )");

    auto ac = autocomplete(2, 0);
    CHECK(ac.entryMap.count("foo"));
}

TEST_CASE_FIXTURE(ACFixture, "nested_recursive_function")
{
    check(R"(
        local function outer()
            local function inner()
            end
        end
    )");

    auto ac = autocomplete(3, 0);
    CHECK(ac.entryMap.count("inner"));
    CHECK(ac.entryMap.count("outer"));
}

TEST_CASE_FIXTURE(ACFixture, "user_defined_local_functions_in_own_definition")
{
    check(R"(
        local function abc()

        end
    )");

    auto ac = autocomplete(2, 0);

    CHECK(ac.entryMap.count("abc"));
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));

    check(R"(
        local abc = function()

        end
    )");

    ac = autocomplete(2, 0);

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
    )");

    auto ac = autocomplete(6, 0);

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
    )");

    auto ac = autocomplete(6, 0);

    CHECK_NE(0, ac.entryMap.size());
    CHECK(!ac.entryMap.count("abc"));
}

TEST_CASE_FIXTURE(ACFixture, "function_parameters")
{
    check(R"(
        function abc(test)

        end
    )");

    auto ac = autocomplete(3, 0);

    CHECK(ac.entryMap.count("test"));
}

TEST_CASE_FIXTURE(ACFixture, "get_member_completions")
{
    check(R"(
        local a = table. -- Line 1
        --             | Column 23
    )");

    auto ac = autocomplete(1, 24);

    CHECK_EQ(16, ac.entryMap.size());
    CHECK(ac.entryMap.count("find"));
    CHECK(ac.entryMap.count("pack"));
    CHECK(!ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "nested_member_completions")
{
    check(R"(
        local tbl = { abc = { def = 1234, egh = false } }
        tbl.abc.
    )");

    auto ac = autocomplete(2, 17);
    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("def"));
    CHECK(ac.entryMap.count("egh"));
}

TEST_CASE_FIXTURE(ACFixture, "unsealed_table")
{
    check(R"(
        local tbl = {}
        tbl.prop = 5
        tbl.
    )");

    auto ac = autocomplete(3, 12);
    CHECK_EQ(1, ac.entryMap.size());
    CHECK(ac.entryMap.count("prop"));
}

TEST_CASE_FIXTURE(ACFixture, "unsealed_table_2")
{
    check(R"(
        local tbl = {}
        local inner = { prop = 5 }
        tbl.inner = inner
        tbl.inner.
    )");

    auto ac = autocomplete(4, 19);
    CHECK_EQ(1, ac.entryMap.size());
    CHECK(ac.entryMap.count("prop"));
}

TEST_CASE_FIXTURE(ACFixture, "cyclic_table")
{
    check(R"(
        local abc = {}
        local def = { abc = abc }
        abc.def = def
        abc.def.
    )");

    auto ac = autocomplete(4, 17);
    CHECK(ac.entryMap.count("abc"));
}

TEST_CASE_FIXTURE(ACFixture, "table_union")
{
    check(R"(
        type t1 = { a1 : string, b2 : number }
        type t2 = { b2 : string, c3 : string }
        function func(abc : t1 | t2)
            abc.
        end
    )");

    auto ac = autocomplete(4, 18);
    CHECK_EQ(1, ac.entryMap.size());
    CHECK(ac.entryMap.count("b2"));
}

TEST_CASE_FIXTURE(ACFixture, "table_intersection")
{
    check(R"(
        type t1 = { a1 : string, b2 : number }
        type t2 = { b2 : string, c3 : string }
        function func(abc : t1 & t2)
            abc.
        end
    )");

    auto ac = autocomplete(4, 18);
    CHECK_EQ(3, ac.entryMap.size());
    CHECK(ac.entryMap.count("a1"));
    CHECK(ac.entryMap.count("b2"));
    CHECK(ac.entryMap.count("c3"));
}

TEST_CASE_FIXTURE(ACFixture, "get_string_completions")
{
    check(R"(
        local a = ("foo"):  -- Line 1
        --                | Column 26
    )");

    auto ac = autocomplete(1, 26);

    CHECK_EQ(17, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "get_suggestions_for_new_statement")
{
    check("");

    auto ac = autocomplete(0, 0);

    CHECK_NE(0, ac.entryMap.size());

    CHECK(ac.entryMap.count("table"));
}

TEST_CASE_FIXTURE(ACFixture, "get_suggestions_for_the_very_start_of_the_script")
{
    check(R"(

        function aaa() end
    )");

    auto ac = autocomplete(0, 0);

    CHECK(ac.entryMap.count("table"));
}

TEST_CASE_FIXTURE(ACFixture, "method_call_inside_function_body")
{
    check(R"(
        local game = { GetService=function(s) return 'hello' end }

        function a()
            game:
        end
    )");

    auto ac = autocomplete(4, 19);

    CHECK_NE(0, ac.entryMap.size());

    CHECK(!ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "method_call_inside_if_conditional")
{
    check(R"(
        if table:
    )");

    auto ac = autocomplete(1, 19);

    CHECK_NE(0, ac.entryMap.size());
    CHECK(ac.entryMap.count("concat"));
    CHECK(!ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "statement_between_two_statements")
{
    check(R"(
        function getmyscripts() end

        g

        getmyscripts()
    )");

    auto ac = autocomplete(3, 9);

    CHECK_NE(0, ac.entryMap.size());

    CHECK(ac.entryMap.count("getmyscripts"));
}

TEST_CASE_FIXTURE(ACFixture, "bias_toward_inner_scope")
{
    check(R"(
        local A = {one=1}

        function B()
            local A = {two=2}

            A
        end
    )");

    auto ac = autocomplete(6, 15);

    CHECK(ac.entryMap.count("A"));

    TypeId t = follow(*ac.entryMap["A"].type);
    const TableTypeVar* tt = get<TableTypeVar>(t);
    REQUIRE(tt);

    CHECK(tt->props.count("two"));
}

TEST_CASE_FIXTURE(ACFixture, "recommend_statement_starting_keywords")
{
    check("");
    auto ac = autocomplete(0, 0);
    CHECK(ac.entryMap.count("local"));

    check("local i = ");
    auto ac2 = autocomplete(0, 10);
    CHECK(!ac2.entryMap.count("local"));
}

TEST_CASE_FIXTURE(ACFixture, "do_not_overwrite_context_sensitive_kws")
{
    check(R"(
        local function continue()
        end


    )");

    auto ac = autocomplete(5, 0);

    AutocompleteEntry entry = ac.entryMap["continue"];
    CHECK(entry.kind == AutocompleteEntryKind::Binding);
}

TEST_CASE_FIXTURE(ACFixture, "dont_offer_any_suggestions_from_within_a_comment")
{
    check(R"(
        --!strict
        local foo = {}
        function foo:bar() end

        --[[
            foo:
        ]]
    )");

    auto ac = autocomplete(6, 16);

    CHECK_EQ(0, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "dont_offer_any_suggestions_from_the_end_of_a_comment")
{
    check(R"(
        --!strict
    )");

    auto ac = autocomplete(1, 17);

    CHECK_EQ(0, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "dont_offer_any_suggestions_from_within_a_broken_comment")
{
    ScopedFastFlag sff{"LuauCaptureBrokenCommentSpans", true};

    check(R"(
        --[[
    )");

    auto ac = autocomplete(1, 13);

    CHECK_EQ(0, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "dont_offer_any_suggestions_from_within_a_broken_comment_at_the_very_end_of_the_file")
{
    ScopedFastFlag sff{"LuauCaptureBrokenCommentSpans", true};

    check("--[[");

    auto ac = autocomplete(0, 4);
    CHECK_EQ(0, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_for_middle_keywords")
{
    check(R"(
        for x =
    )");

    auto ac1 = autocomplete(1, 14);
    CHECK_EQ(ac1.entryMap.count("do"), 0);
    CHECK_EQ(ac1.entryMap.count("end"), 0);

    check(R"(
        for x = 1
    )");

    auto ac2 = autocomplete(1, 15);
    CHECK_EQ(ac2.entryMap.count("do"), 0);
    CHECK_EQ(ac2.entryMap.count("end"), 0);

    check(R"(
        for x = 1, 2
    )");

    auto ac3 = autocomplete(1, 18);
    CHECK_EQ(1, ac3.entryMap.size());
    CHECK_EQ(ac3.entryMap.count("do"), 1);

    check(R"(
        for x = 1, 2,
    )");

    auto ac4 = autocomplete(1, 19);
    CHECK_EQ(ac4.entryMap.count("do"), 0);
    CHECK_EQ(ac4.entryMap.count("end"), 0);

    check(R"(
        for x = 1, 2, 5
    )");

    auto ac5 = autocomplete(1, 22);
    CHECK_EQ(ac5.entryMap.count("do"), 1);
    CHECK_EQ(ac5.entryMap.count("end"), 0);

    check(R"(
        for x = 1, 2, 5 f
    )");

    auto ac6 = autocomplete(1, 25);
    CHECK_EQ(ac6.entryMap.size(), 1);
    CHECK_EQ(ac6.entryMap.count("do"), 1);

    check(R"(
        for x = 1, 2, 5 do
    )");

    auto ac7 = autocomplete(1, 32);
    CHECK_EQ(ac7.entryMap.count("end"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_for_in_middle_keywords")
{
    check(R"(
        for
    )");

    auto ac1 = autocomplete(1, 12);
    CHECK_EQ(0, ac1.entryMap.size());

    check(R"(
        for x
    )");

    auto ac2 = autocomplete(1, 13);
    CHECK_EQ(0, ac2.entryMap.size());

    auto ac2a = autocomplete(1, 14);
    CHECK_EQ(1, ac2a.entryMap.size());
    CHECK_EQ(1, ac2a.entryMap.count("in"));

    check(R"(
        for x in y
    )");

    auto ac3 = autocomplete(1, 18);
    CHECK_EQ(ac3.entryMap.count("table"), 1);
    CHECK_EQ(ac3.entryMap.count("do"), 0);

    check(R"(
        for x in y 
    )");

    auto ac4 = autocomplete(1, 19);
    CHECK_EQ(ac4.entryMap.size(), 1);
    CHECK_EQ(ac4.entryMap.count("do"), 1);

    check(R"(
        for x in f f
    )");

    auto ac5 = autocomplete(1, 20);
    CHECK_EQ(ac5.entryMap.size(), 1);
    CHECK_EQ(ac5.entryMap.count("do"), 1);

    check(R"(
        for x in y do
    )");

    auto ac6 = autocomplete(1, 23);
    CHECK_EQ(ac6.entryMap.count("in"), 0);
    CHECK_EQ(ac6.entryMap.count("table"), 1);
    CHECK_EQ(ac6.entryMap.count("end"), 1);
    CHECK_EQ(ac6.entryMap.count("function"), 1);

    check(R"(
        for x in y do e
    )");

    auto ac7 = autocomplete(1, 23);
    CHECK_EQ(ac7.entryMap.count("in"), 0);
    CHECK_EQ(ac7.entryMap.count("table"), 1);
    CHECK_EQ(ac7.entryMap.count("end"), 1);
    CHECK_EQ(ac7.entryMap.count("function"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_while_middle_keywords")
{
    check(R"(
        while
    )");

    auto ac1 = autocomplete(1, 13);
    CHECK_EQ(ac1.entryMap.count("do"), 0);
    CHECK_EQ(ac1.entryMap.count("end"), 0);

    check(R"(
        while true
    )");

    auto ac2 = autocomplete(1, 19);
    CHECK_EQ(1, ac2.entryMap.size());
    CHECK_EQ(ac2.entryMap.count("do"), 1);

    check(R"(
        while true do
    )");

    auto ac3 = autocomplete(1, 23);
    CHECK_EQ(ac3.entryMap.count("end"), 1);

    check(R"(
        while true d
    )");

    auto ac4 = autocomplete(1, 20);
    CHECK_EQ(1, ac4.entryMap.size());
    CHECK_EQ(ac4.entryMap.count("do"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_if_middle_keywords")
{
    check(R"(
        if
    )");

    auto ac1 = autocomplete(1, 13);
    CHECK_EQ(ac1.entryMap.count("then"), 0);
    CHECK_EQ(ac1.entryMap.count("function"),
        1); // FIXME: This is kind of dumb.  It is technically syntactically valid but you can never do anything interesting with this.
    CHECK_EQ(ac1.entryMap.count("table"), 1);
    CHECK_EQ(ac1.entryMap.count("else"), 0);
    CHECK_EQ(ac1.entryMap.count("elseif"), 0);
    CHECK_EQ(ac1.entryMap.count("end"), 0);

    check(R"(
        if x
    )");

    auto ac2 = autocomplete(1, 14);
    CHECK_EQ(ac2.entryMap.count("then"), 1);
    CHECK_EQ(ac2.entryMap.count("function"), 0);
    CHECK_EQ(ac2.entryMap.count("else"), 0);
    CHECK_EQ(ac2.entryMap.count("elseif"), 0);
    CHECK_EQ(ac2.entryMap.count("end"), 0);

    check(R"(
        if x t
    )");

    auto ac3 = autocomplete(1, 14);
    CHECK_EQ(1, ac3.entryMap.size());
    CHECK_EQ(ac3.entryMap.count("then"), 1);

    check(R"(
        if x then

        end
    )");

    auto ac4 = autocomplete(2, 0);
    CHECK_EQ(ac4.entryMap.count("then"), 0);
    CHECK_EQ(ac4.entryMap.count("else"), 1);
    CHECK_EQ(ac4.entryMap.count("function"), 1);
    CHECK_EQ(ac4.entryMap.count("elseif"), 1);
    CHECK_EQ(ac4.entryMap.count("end"), 0);

    check(R"(
        if x then
            t
        end
    )");

    auto ac4a = autocomplete(2, 13);
    CHECK_EQ(ac4a.entryMap.count("then"), 0);
    CHECK_EQ(ac4a.entryMap.count("table"), 1);
    CHECK_EQ(ac4a.entryMap.count("else"), 1);
    CHECK_EQ(ac4a.entryMap.count("elseif"), 1);

    check(R"(
        if x then

        elseif x then
        end
    )");

    auto ac5 = autocomplete(2, 0);
    CHECK_EQ(ac5.entryMap.count("then"), 0);
    CHECK_EQ(ac5.entryMap.count("function"), 1);
    CHECK_EQ(ac5.entryMap.count("else"), 0);
    CHECK_EQ(ac5.entryMap.count("elseif"), 0);
    CHECK_EQ(ac5.entryMap.count("end"), 0);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_until_in_repeat")
{
    check(R"(
        repeat
    )");

    auto ac = autocomplete(1, 16);
    CHECK_EQ(ac.entryMap.count("table"), 1);
    CHECK_EQ(ac.entryMap.count("until"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_until_expression")
{
    check(R"(
        repeat
        until
    )");

    auto ac = autocomplete(2, 16);
    CHECK_EQ(ac.entryMap.count("table"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "local_names")
{
    check(R"(
        local ab
    )");

    auto ac1 = autocomplete(1, 16);
    CHECK_EQ(ac1.entryMap.size(), 1);
    CHECK_EQ(ac1.entryMap.count("function"), 1);

    check(R"(
        local ab, cd
    )");

    auto ac2 = autocomplete(1, 20);
    CHECK(ac2.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_end_with_fn_exprs")
{
    check(R"(
        local function f()
    )");

    auto ac = autocomplete(1, 28);
    CHECK_EQ(ac.entryMap.count("end"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_end_with_lambda")
{
    check(R"(
        local a = function() local bar = foo en
    )");

    auto ac = autocomplete(1, 47);
    CHECK_EQ(ac.entryMap.count("end"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "stop_at_first_stat_when_recommending_keywords")
{
    check(R"(
        repeat
            for x 
    )");

    auto ac1 = autocomplete(2, 18);
    CHECK_EQ(ac1.entryMap.count("in"), 1);
    CHECK_EQ(ac1.entryMap.count("until"), 0);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_repeat_middle_keyword")
{
    check(R"(
        repeat
    )");

    auto ac1 = autocomplete(1, 15);
    CHECK_EQ(ac1.entryMap.count("do"), 1);
    CHECK_EQ(ac1.entryMap.count("function"), 1);
    CHECK_EQ(ac1.entryMap.count("until"), 1);

    check(R"(
        repeat f f
    )");

    auto ac2 = autocomplete(1, 18);
    CHECK_EQ(ac2.entryMap.count("function"), 1);
    CHECK_EQ(ac2.entryMap.count("until"), 1);

    check(R"(
        repeat
            u
        until
    )");

    auto ac3 = autocomplete(2, 13);
    CHECK_EQ(ac3.entryMap.count("until"), 0);
}

TEST_CASE_FIXTURE(ACFixture, "local_function")
{
    check(R"(
        local f
    )");

    auto ac1 = autocomplete(1, 15);
    CHECK_EQ(ac1.entryMap.size(), 1);
    CHECK_EQ(ac1.entryMap.count("function"), 1);

    check(R"(
        local f, cd
    )");

    auto ac2 = autocomplete(1, 15);
    CHECK(ac2.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "local_function")
{
    check(R"(
        local function 
    )");

    auto ac = autocomplete(1, 23);
    CHECK(ac.entryMap.empty());

    check(R"(
        local function s
    )");

    ac = autocomplete(1, 23);
    CHECK(ac.entryMap.empty());

    ac = autocomplete(1, 24);
    CHECK(ac.entryMap.empty());

    check(R"(
        local function ()
    )");

    ac = autocomplete(1, 23);
    CHECK(ac.entryMap.empty());

    ac = autocomplete(1, 25);
    CHECK(ac.entryMap.count("end"));

    check(R"(
        local function something
    )");

    ac = autocomplete(1, 32);
    CHECK(ac.entryMap.empty());

    check(R"(
        local tbl = {}
        function tbl.something() end
    )");

    ac = autocomplete(2, 30);
    CHECK(ac.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "local_function_params")
{
    check(R"(
        local function abc(def)
    )");

    CHECK(autocomplete(1, 23).entryMap.empty());
    CHECK(autocomplete(1, 24).entryMap.empty());
    CHECK(autocomplete(1, 27).entryMap.empty());
    CHECK(autocomplete(1, 28).entryMap.empty());
    CHECK(!autocomplete(1, 31).entryMap.empty());

    CHECK(!autocomplete(1, 32).entryMap.empty());

    check(R"(
        local function abc(def)
        end
    )");

    for (unsigned int i = 23; i < 31; ++i)
    {
        CHECK(autocomplete(1, i).entryMap.empty());
    }
    CHECK(!autocomplete(1, 32).entryMap.empty());

    auto ac2 = autocomplete(2, 0);
    CHECK_EQ(ac2.entryMap.count("abc"), 1);
    CHECK_EQ(ac2.entryMap.count("def"), 1);

    check(R"(
        local function abc(def, ghi)
        end
    )");

    auto ac3 = autocomplete(1, 35);
    CHECK(ac3.entryMap.empty());
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

        end
    )");

    auto ac2 = autocomplete(2, 0);
    CHECK_EQ(ac2.entryMap.count("abc"), 1);
    CHECK_EQ(ac2.entryMap.count("def"), 1);

    check(R"(
        function abc(def, ghi)
        end
    )");

    auto ac3 = autocomplete(1, 29);
    CHECK(ac3.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "arguments_to_global_lambda")
{
    check(R"(
        abc = function(def, ghi)
        end
    )");

    auto ac = autocomplete(1, 31);
    CHECK(ac.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "function_expr_params")
{
    check(R"(
        abc = function(def)
    )");

    for (unsigned int i = 20; i < 27; ++i)
    {
        CHECK(autocomplete(1, i).entryMap.empty());
    }
    CHECK(!autocomplete(1, 28).entryMap.empty());

    check(R"(
        abc = function(def)
        end
    )");

    for (unsigned int i = 20; i < 27; ++i)
    {
        CHECK(autocomplete(1, i).entryMap.empty());
    }
    CHECK(!autocomplete(1, 28).entryMap.empty());

    check(R"(
        abc = function(def)

        end
    )");

    auto ac2 = autocomplete(2, 0);
    CHECK_EQ(ac2.entryMap.count("def"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "local_initializer")
{
    check(R"(
        local a = t
    )");

    auto ac = autocomplete(1, 19);
    CHECK_EQ(ac.entryMap.count("table"), 1);
    CHECK_EQ(ac.entryMap.count("true"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "local_initializer_2")
{
    check(R"(
        local a=
    )");

    auto ac = autocomplete(1, 16);
    CHECK(ac.entryMap.count("table"));
}

TEST_CASE_FIXTURE(ACFixture, "get_member_completions")
{
    check(R"(
        local a = 12.3
    )");

    auto ac = autocomplete(1, 21);
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
        t.
    )");

    autocomplete(8, 12);
    // Don't crash!
}

TEST_CASE_FIXTURE(ACFixture, "local_types_builtin")
{
    check(R"(
local a: n
local b: string = "don't trip"
    )");

    auto ac = autocomplete(1, 10);

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));
}

TEST_CASE_FIXTURE(ACFixture, "private_types")
{
    check(R"(
do
    type num = number
    local a: nu
    local b: num
end
local a: nu
    )");

    auto ac = autocomplete(3, 14);

    CHECK(ac.entryMap.count("num"));
    CHECK(ac.entryMap.count("number"));

    ac = autocomplete(4, 15);

    CHECK(ac.entryMap.count("num"));
    CHECK(ac.entryMap.count("number"));

    ac = autocomplete(6, 11);

    CHECK(!ac.entryMap.count("num"));
    CHECK(ac.entryMap.count("number"));
}

TEST_CASE_FIXTURE(ACFixture, "type_scoping_easy")
{
    check(R"(
type Table = { a: number, b: number }
do
    type Table = { x: string, y: string }
    local a: T
end
    )");

    auto ac = autocomplete(4, 14);

    REQUIRE(ac.entryMap.count("Table"));
    REQUIRE(ac.entryMap["Table"].type);
    const TableTypeVar* tv = get<TableTypeVar>(follow(*ac.entryMap["Table"].type));
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

    LUAU_REQUIRE_NO_ERRORS(frontend.check("Module/A"));

    fileResolver.source["Module/B"] = R"(
local aaa = require(script.Parent.A)
local a: aa
    )";

    frontend.check("Module/B");

    auto ac = Luau::autocomplete(frontend, "Module/B", Position{2, 11}, nullCallback);

    CHECK(ac.entryMap.count("aaa"));
}

TEST_CASE_FIXTURE(ACFixture, "module_type_members")
{
    fileResolver.source["Module/A"] = R"(
export type A = { x: number, y: number }
export type B = { z: number, w: number }
return {}
    )";

    LUAU_REQUIRE_NO_ERRORS(frontend.check("Module/A"));

    fileResolver.source["Module/B"] = R"(
local aaa = require(script.Parent.A)
local a: aaa.
    )";

    frontend.check("Module/B");

    auto ac = Luau::autocomplete(frontend, "Module/B", Position{2, 13}, nullCallback);

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("A"));
    CHECK(ac.entryMap.count("B"));
}

TEST_CASE_FIXTURE(ACFixture, "argument_types")
{
    check(R"(
local function f(a: n
local b: string = "don't trip"
    )");

    auto ac = autocomplete(1, 21);

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));
}

TEST_CASE_FIXTURE(ACFixture, "return_types")
{
    check(R"(
local function f(a: number): n
local b: string = "don't trip"
    )");

    auto ac = autocomplete(1, 30);

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));
}

TEST_CASE_FIXTURE(ACFixture, "as_types")
{
    check(R"(
local a: any = 5
local b: number = (a :: n
    )");

    auto ac = autocomplete(2, 25);

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));
}

TEST_CASE_FIXTURE(ACFixture, "function_type_types")
{
    check(R"(
local a: (n
local b: (number, (n
local c: (number, (number) -> n
local d: (number, (number) -> (number, n
local e: (n: n
    )");

    auto ac = autocomplete(1, 11);

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));

    ac = autocomplete(2, 20);

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));

    ac = autocomplete(3, 31);

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));

    ac = autocomplete(4, 40);

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));

    ac = autocomplete(5, 14);

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));
}

TEST_CASE_FIXTURE(ACFixture, "generic_types")
{
    ScopedFastFlag luauParseGenericFunctions("LuauParseGenericFunctions", true);
    ScopedFastFlag luauGenericFunctions("LuauGenericFunctions", true);

    check(R"(
function f<Tee, Use>(a: T
local b: string = "don't trip"
    )");

    auto ac = autocomplete(1, 25);

    CHECK(ac.entryMap.count("Tee"));
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_suggestion_in_argument")
{
    // local
    check(R"(
local function target(a: number, b: string) return a + #b end

local one = 4
local two = "hello"
return target(o
    )");

    auto ac = autocomplete(5, 15);

    CHECK(ac.entryMap.count("one"));
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::None);

    check(R"(
local function target(a: number, b: string) return a + #b end

local one = 4
local two = "hello"
return target(one, t
    )");

    ac = autocomplete(5, 20);

    CHECK(ac.entryMap.count("two"));
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::None);

    // member
    check(R"(
local function target(a: number, b: string) return a + #b end

local a = { one = 4, two = "hello" }
return target(a.
    )");

    ac = autocomplete(4, 16);

    CHECK(ac.entryMap.count("one"));
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::None);

    check(R"(
local function target(a: number, b: string) return a + #b end

local a = { one = 4, two = "hello" }
return target(a.one, a.
    )");

    ac = autocomplete(4, 23);

    CHECK(ac.entryMap.count("two"));
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::None);

    // union match
    check(R"(
local function target(a: string?) return #b end

local a = { one = 4, two = "hello" }
return target(a.
    )");

    ac = autocomplete(4, 16);

    CHECK(ac.entryMap.count("two"));
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::None);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_suggestion_in_table")
{
    check(R"(
type Foo = { a: number, b: string }
local a = { one = 4, two = "hello" }
local b: Foo = { a = a.
    )");

    auto ac = autocomplete(3, 23);

    CHECK(ac.entryMap.count("one"));
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::None);

    check(R"(
type Foo = { a: number, b: string }
local a = { one = 4, two = "hello" }
local b: Foo = { b = a.
    )");

    ac = autocomplete(3, 23);

    CHECK(ac.entryMap.count("two"));
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::None);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_function_return_types")
{
    check(R"(
local function target(a: number, b: string) return a + #b end
local function bar1(a: number) return -a end
local function bar2(a: string) reutrn a .. 'x' end

return target(b
    )");

    auto ac = autocomplete(5, 15);

    CHECK(ac.entryMap.count("bar1"));
    CHECK(ac.entryMap["bar1"].typeCorrect == TypeCorrectKind::CorrectFunctionResult);
    CHECK(ac.entryMap["bar2"].typeCorrect == TypeCorrectKind::None);

    check(R"(
local function target(a: number, b: string) return a + #b end
local function bar1(a: number) return -a end
local function bar2(a: string) return a .. 'x' end

return target(bar1, b
    )");

    ac = autocomplete(5, 21);

    CHECK(ac.entryMap.count("bar2"));
    CHECK(ac.entryMap["bar2"].typeCorrect == TypeCorrectKind::CorrectFunctionResult);
    CHECK(ac.entryMap["bar1"].typeCorrect == TypeCorrectKind::None);

    check(R"(
local function target(a: number, b: string) return a + #b end
local function bar1(a: number): (...number) return -a, a end
local function bar2(a: string) reutrn a .. 'x' end

return target(b
    )");

    ac = autocomplete(5, 15);

    CHECK(ac.entryMap.count("bar1"));
    CHECK(ac.entryMap["bar1"].typeCorrect == TypeCorrectKind::CorrectFunctionResult);
    CHECK(ac.entryMap["bar2"].typeCorrect == TypeCorrectKind::None);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_local_type_suggestion")
{
    check(R"(
local b: s = "str"
    )");

    auto ac = autocomplete(1, 10);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function f() return "str" end
local b: s = f()
    )");

    ac = autocomplete(2, 10);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local b: s, c: n = "str", 2
    )");

    ac = autocomplete(1, 10);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(1, 16);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function f() return 1, "str", 3 end
local a: b, b: n, c: s, d: n = false, f()
    )");

    ac = autocomplete(2, 10);

    CHECK(ac.entryMap.count("boolean"));
    CHECK(ac.entryMap["boolean"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(2, 16);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(2, 22);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(2, 28);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function f(): ...number return 1, 2, 3 end
local a: boolean, b: n = false, f()
    )");

    ac = autocomplete(2, 22);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_function_type_suggestion")
{
    check(R"(
local b: (n) -> number = function(a: number, b: string) return a + #b end
    )");

    auto ac = autocomplete(1, 11);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local b: (number, s = function(a: number, b: string) return a + #b end
    )");

    ac = autocomplete(1, 19);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local b: (number, string) -> b = function(a: number, b: string): boolean return a + #b == 0 end
    )");

    ac = autocomplete(1, 30);

    CHECK(ac.entryMap.count("boolean"));
    CHECK(ac.entryMap["boolean"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local b: (number, ...s) = function(a: number, ...: string) return a end
    )");

    ac = autocomplete(1, 22);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local b: (number) -> ...s = function(a: number): ...string return "a", "b", "c" end
    )");

    ac = autocomplete(1, 25);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_full_type_suggestion")
{
    check(R"(
local b: = "str"
    )");

    auto ac = autocomplete(1, 8);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(1, 9);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local b: = function(a: number) return -a end
    )");

    ac = autocomplete(1, 9);

    CHECK(ac.entryMap.count("(number) -> number"));
    CHECK(ac.entryMap["(number) -> number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_argument_type_suggestion")
{
    check(R"(
local function target(a: number, b: string) return a + #b end

local function d(a: n, b)
	return target(a, b)
end
    )");

    auto ac = autocomplete(3, 21);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(a: number, b: string) return a + #b end

local function d(a, b: s)
	return target(a, b)
end
    )");

    ac = autocomplete(3, 24);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(a: number, b: string) return a + #b end

local function d(a: , b)
	return target(a, b)
end
    )");

    ac = autocomplete(3, 19);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(3, 20);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(a: number, b: string) return a + #b end

local function d(a, b: ): number
	return target(a, b)
end
    )");

    ac = autocomplete(3, 23);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(3, 24);

    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::None);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_expected_argument_type_suggestion")
{
    check(R"(
local function target(callback: (a: number, b: string) -> number) return callback(4, "hello") end

local x = target(function(a: 
    )");

    auto ac = autocomplete(3, 29);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(callback: (a: number, b: string) -> number) return callback(4, "hello") end

local x = target(function(a: n
    )");

    ac = autocomplete(3, 30);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(callback: (a: number, b: string) -> number) return callback(4, "hello") end

local x = target(function(a: n, b: )
	return a + #b
end)
    )");

    ac = autocomplete(3, 30);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(3, 35);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(callback: (...number) -> number) return callback(1, 2, 3) end

local x = target(function(a: n)
	return a
end
    )");

    ac = autocomplete(3, 30);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_expected_argument_type_pack_suggestion")
{
    check(R"(
local function target(callback: (...number) -> number) return callback(1, 2, 3) end

local x = target(function(...:n)
	return a
end
    )");

    auto ac = autocomplete(3, 31);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(callback: (...number) -> number) return callback(1, 2, 3) end

local x = target(function(a:number, b:number, ...:)
	return a + b
end
    )");

    ac = autocomplete(3, 50);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_expected_return_type_suggestion")
{
    check(R"(
local function target(callback: () -> number) return callback() end

local x = target(function(): n
	return 1
end
    )");

    auto ac = autocomplete(3, 30);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(callback: () -> (number, number)) return callback() end

local x = target(function(): (number, n
	return 1, 2
end
    )");

    ac = autocomplete(3, 39);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_expected_return_type_pack_suggestion")
{
    check(R"(
local function target(callback: () -> ...number) return callback() end

local x = target(function(): ...n
	return 1, 2, 3
end
    )");

    auto ac = autocomplete(3, 33);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local function target(callback: () -> ...number) return callback() end

local x = target(function(): (number, number, ...n
	return 1, 2, 3
end
    )");

    ac = autocomplete(3, 50);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_expected_argument_type_suggestion_optional")
{
    check(R"(
local function target(callback: nil | (a: number, b: string) -> number) return callback(4, "hello") end

local x = target(function(a: 
    )");

    auto ac = autocomplete(3, 29);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_expected_argument_type_suggestion_self")
{
    check(R"(
local t = {}
t.x = 5
function t:target(callback: (a: number, b: string) -> number) return callback(self.x, "hello") end

local x = t:target(function(a: , b: ) end)
local y = t.target(t, function(a: number, b: ) end)
    )");

    auto ac = autocomplete(5, 31);

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap["number"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(5, 35);

    CHECK(ac.entryMap.count("string"));
    CHECK(ac.entryMap["string"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(6, 45);

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

    LUAU_REQUIRE_NO_ERRORS(frontend.check("Module/A"));

    fileResolver.source["Module/B"] = R"(
local ex = require(script.Parent.A)
ex.a(function(x:
    )";

    frontend.check("Module/B");

    auto ac = Luau::autocomplete(frontend, "Module/B", Position{2, 16}, nullCallback);

    CHECK(!ac.entryMap.count("done"));

    fileResolver.source["Module/C"] = R"(
local ex = require(script.Parent.A)
ex.b(function(x:
    )";

    frontend.check("Module/C");

    ac = Luau::autocomplete(frontend, "Module/C", Position{2, 16}, nullCallback);

    CHECK(!ac.entryMap.count("(done) -> number"));
}

TEST_CASE_FIXTURE(ACFixture, "suggest_external_module_type")
{
    fileResolver.source["Module/A"] = R"(
export type done = { x: number, y: number }
local function a(a: (done) -> number) return a({x=1, y=2}) end
local function b(a: ((done) -> number) -> number) return a(function(done) return 1 end) end
return {a = a, b = b}
    )";

    LUAU_REQUIRE_NO_ERRORS(frontend.check("Module/A"));

    fileResolver.source["Module/B"] = R"(
local ex = require(script.Parent.A)
ex.a(function(x:
    )";

    frontend.check("Module/B");

    auto ac = Luau::autocomplete(frontend, "Module/B", Position{2, 16}, nullCallback);

    CHECK(!ac.entryMap.count("done"));
    CHECK(ac.entryMap.count("ex.done"));
    CHECK(ac.entryMap["ex.done"].typeCorrect == TypeCorrectKind::Correct);

    fileResolver.source["Module/C"] = R"(
local ex = require(script.Parent.A)
ex.b(function(x:
    )";

    frontend.check("Module/C");

    ac = Luau::autocomplete(frontend, "Module/C", Position{2, 16}, nullCallback);

    CHECK(!ac.entryMap.count("(done) -> number"));
    CHECK(ac.entryMap.count("(ex.done) -> number"));
    CHECK(ac.entryMap["(ex.done) -> number"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "do_not_suggest_synthetic_table_name")
{
    check(R"(
local foo = { a = 1, b = 2 }
local bar: = foo
    )");

    auto ac = autocomplete(2, 11);

    CHECK(!ac.entryMap.count("foo"));
}

// CLI-45692: Remove UnfrozenFixture here
TEST_CASE_FIXTURE(UnfrozenFixture, "type_correct_function_no_parenthesis")
{
    check(R"(
local function target(a: (number) -> number) return a(4) end
local function bar1(a: number) return -a end
local function bar2(a: string) reutrn a .. 'x' end

return target(b
    )");

    auto ac = autocomplete(frontend, "MainModule", Position{5, 15}, nullCallback);

    CHECK(ac.entryMap.count("bar1"));
    CHECK(ac.entryMap["bar1"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["bar1"].parens == ParenthesesRecommendation::None);
    CHECK(ac.entryMap["bar2"].typeCorrect == TypeCorrectKind::None);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_sealed_table")
{
    check(R"(
local function f(a: { x: number, y: number }) return a.x + a.y end
local fp: = f
    )");

    auto ac = autocomplete(2, 10);

    CHECK(ac.entryMap.count("({ x: number, y: number }) -> number"));
}

// CLI-45692: Remove UnfrozenFixture here
TEST_CASE_FIXTURE(UnfrozenFixture, "type_correct_keywords")
{
    check(R"(
local function a(x: boolean) end
local function b(x: number?) end
local function c(x: (number) -> string) end
local function d(x: ((number) -> string)?) end
local function e(x: ((number) -> string) & ((boolean) -> number)) end

local tru = {}
local ni = false

local ac = a(t)
local bc = b(n)
local cc = c(f)
local dc = d(f)
local ec = e(f)
    )");

    auto ac = autocomplete(frontend, "MainModule", Position{10, 14}, nullCallback);
    CHECK(ac.entryMap.count("tru"));
    CHECK(ac.entryMap["tru"].typeCorrect == TypeCorrectKind::None);
    CHECK(ac.entryMap["true"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["false"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(frontend, "MainModule", Position{11, 14}, nullCallback);
    CHECK(ac.entryMap.count("ni"));
    CHECK(ac.entryMap["ni"].typeCorrect == TypeCorrectKind::None);
    CHECK(ac.entryMap["nil"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(frontend, "MainModule", Position{12, 14}, nullCallback);
    CHECK(ac.entryMap.count("false"));
    CHECK(ac.entryMap["false"].typeCorrect == TypeCorrectKind::None);
    CHECK(ac.entryMap["function"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(frontend, "MainModule", Position{13, 14}, nullCallback);
    CHECK(ac.entryMap["function"].typeCorrect == TypeCorrectKind::Correct);

    ac = autocomplete(frontend, "MainModule", Position{14, 14}, nullCallback);
    CHECK(ac.entryMap["function"].typeCorrect == TypeCorrectKind::Correct);
}

TEST_CASE_FIXTURE(ACFixture, "type_correct_suggestion_for_overloads")
{
    check(R"(
local target: ((number) -> string) & ((string) -> number))

local one = 4
local two = "hello"
return target(o)
    )");

    auto ac = autocomplete(5, 15);

    CHECK(ac.entryMap.count("one"));
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::Correct);

    check(R"(
local target: ((number) -> string) & ((number) -> number))

local one = 4
local two = "hello"
return target(o)
    )");

    ac = autocomplete(5, 15);

    CHECK(ac.entryMap.count("one"));
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::None);

    check(R"(
local target: ((number, number) -> string) & ((string) -> number))

local one = 4
local two = "hello"
return target(1, o)
    )");

    ac = autocomplete(5, 18);

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
return b.
    )");

    auto ac = autocomplete(4, 9);

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("x"));
    CHECK(ac.entryMap.count("y"));

    check(R"(
local a = { x = 2, y = 3 }
type A = typeof(a)
local b: nil | A = a
return b.
    )");

    ac = autocomplete(4, 9);

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("x"));
    CHECK(ac.entryMap.count("y"));

    check(R"(
local b: nil | nil
return b.
    )");

    ac = autocomplete(2, 9);

    CHECK_EQ(0, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "no_function_name_suggestions")
{
    check(R"(
function na
    )");

    auto ac = autocomplete(1, 11);

    CHECK(ac.entryMap.empty());

    check(R"(
local function 
    )");

    ac = autocomplete(1, 15);

    CHECK(ac.entryMap.empty());

    check(R"(
local function na
    )");

    ac = autocomplete(1, 17);

    CHECK(ac.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "skip_current_local")
{
    check(R"(
local other = 1
local name = na
    )");

    auto ac = autocomplete(2, 15);

    CHECK(!ac.entryMap.count("name"));
    CHECK(ac.entryMap.count("other"));

    check(R"(
local other = 1
local name, test = na
    )");

    ac = autocomplete(2, 21);

    CHECK(!ac.entryMap.count("name"));
    CHECK(!ac.entryMap.count("test"));
    CHECK(ac.entryMap.count("other"));
}

TEST_CASE_FIXTURE(ACFixture, "keyword_members")
{
    check(R"(
local a = { done = 1, forever = 2 }
local b = a.do
local c = a.for
local d = a.
do
end
    )");

    auto ac = autocomplete(2, 14);

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("done"));
    CHECK(ac.entryMap.count("forever"));

    ac = autocomplete(3, 15);

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("done"));
    CHECK(ac.entryMap.count("forever"));

    ac = autocomplete(4, 12);

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("done"));
    CHECK(ac.entryMap.count("forever"));
}

TEST_CASE_FIXTURE(ACFixture, "keyword_methods")
{
    check(R"(
local a = {}
function a:done() end
local b = a:do
    )");

    auto ac = autocomplete(3, 14);

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

    LUAU_REQUIRE_NO_ERRORS(frontend.check("Module/A"));

    fileResolver.source["Module/B"] = R"(
local aaa = require(script.Parent.A)
local a: aaa.do
    )";

    frontend.check("Module/B");

    auto ac = Luau::autocomplete(frontend, "Module/B", Position{2, 15}, nullCallback);

    CHECK_EQ(2, ac.entryMap.size());
    CHECK(ac.entryMap.count("done"));
    CHECK(ac.entryMap.count("other"));
}

TEST_CASE_FIXTURE(ACFixture, "autocompleteSource")
{
    std::string_view source = R"(
        local a = table. -- Line 1
        --             | Column 23
    )";

    auto ac = autocompleteSource(frontend, source, Position{1, 24}, nullCallback).result;

    CHECK_EQ(16, ac.entryMap.size());
    CHECK(ac.entryMap.count("find"));
    CHECK(ac.entryMap.count("pack"));
    CHECK(!ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "autocompleteSource_require")
{
    ScopedFastFlag luauResolveModuleNameWithoutACurrentModule("LuauResolveModuleNameWithoutACurrentModule", true);

    std::string_view source = R"(
        local a = require(w -- Line 1
        --                 | Column 27
    )";

    // CLI-43699 require shouldn't crash inside autocompleteSource
    auto ac = autocompleteSource(frontend, source, Position{1, 27}, nullCallback).result;
}

TEST_CASE_FIXTURE(ACFixture, "autocompleteSource_comments")
{
    std::string_view source = "--!str";

    auto ac = autocompleteSource(frontend, source, Position{0, 6}, nullCallback).result;
    CHECK_EQ(0, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "autocompleteProp_index_function_metamethod_is_variadic")
{
    std::string_view source = R"(
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

    auto ac = autocompleteSource(frontend, source, Position{9, 20}, nullCallback).result;
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

if 1 then
else
end

while false do
end

repeat
until
    )");

    auto ac = autocomplete(6, 9);
    CHECK(ac.entryMap.size() == 1);
    CHECK(ac.entryMap.count("then"));

    ac = autocomplete(7, 4);
    CHECK(ac.entryMap.count("else"));
    CHECK(ac.entryMap.count("elseif"));

    ac = autocomplete(10, 14);
    CHECK(ac.entryMap.count("do"));

    ac = autocomplete(13, 6);
    CHECK(ac.entryMap.count("do"));

    // FIXME: ideally we want to handle start and end of all statements as well
}

TEST_CASE_FIXTURE(ACFixture, "if_then_else_elseif_completions")
{
    ScopedFastFlag sff{"ElseElseIfCompletionImprovements", true};

    check(R"(
local elsewhere = false

if true then
    return 1
el
end
    )");

    auto ac = autocomplete(5, 2);
    CHECK(ac.entryMap.count("else"));
    CHECK(ac.entryMap.count("elseif"));
    CHECK(ac.entryMap.count("elsewhere") == 0);

    check(R"(
local elsewhere = false

if true then
    return 1
else
    return 2
el
end
    )");

    ac = autocomplete(7, 2);
    CHECK(ac.entryMap.count("else") == 0);
    CHECK(ac.entryMap.count("elseif") == 0);
    CHECK(ac.entryMap.count("elsewhere"));

    check(R"(
local elsewhere = false

if true then
    print("1")
elif true then
    print("2")
el
end
    )");
    ac = autocomplete(7, 2);
    CHECK(ac.entryMap.count("else"));
    CHECK(ac.entryMap.count("elseif"));
    CHECK(ac.entryMap.count("elsewhere"));
}

TEST_CASE_FIXTURE(ACFixture, "autocompleteSource_not_the_var_we_are_defining")
{
    std::string_view source = "abc,de";

    auto ac = autocompleteSource(frontend, source, Position{0, 6}, nullCallback).result;
    CHECK(!ac.entryMap.count("de"));
}

TEST_CASE_FIXTURE(ACFixture, "autocompleteSource_recursive_function")
{
    {
        std::string_view global = R"(function abc()

end
)";

        auto ac = autocompleteSource(frontend, global, Position{1, 0}, nullCallback).result;
        CHECK(ac.entryMap.count("abc"));
    }

    {
        std::string_view local = R"(local function abc()

end
)";

        auto ac = autocompleteSource(frontend, local, Position{1, 0}, nullCallback).result;
        CHECK(ac.entryMap.count("abc"));
    }
}

TEST_CASE_FIXTURE(ACFixture, "suggest_table_keys")
{
    check(R"(
type Test = { first: number, second: number }
local t: Test = { f }
    )");

    auto ac = autocomplete(2, 19);
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

    // Intersection
    check(R"(
type Test = { first: number } & { second: number }
local t: Test = { f }
    )");

    ac = autocomplete(2, 19);
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

    // Union
    check(R"(
type Test = { first: number, second: number } | { second: number, third: number }
local t: Test = { s }
    )");

    ac = autocomplete(2, 19);
    CHECK(ac.entryMap.count("second"));
    CHECK(!ac.entryMap.count("first"));
    CHECK(!ac.entryMap.count("third"));

    // No parenthesis suggestion
    check(R"(
type Test = { first: (number) -> number, second: number }
local t: Test = { f }
    )");

    ac = autocomplete(2, 19);
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap["first"].parens == ParenthesesRecommendation::None);

    // When key is changed
    check(R"(
type Test = { first: number, second: number }
local t: Test = { f = 2 }
    )");

    ac = autocomplete(2, 19);
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

    // Alternative key syntax
    check(R"(
type Test = { first: number, second: number }
local t: Test = { ["f"] }
    )");

    ac = autocomplete(2, 21);
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

    // Not an alternative key syntax
    check(R"(
type Test = { first: number, second: number }
local t: Test = { "f" }
    )");

    ac = autocomplete(2, 20);
    CHECK(!ac.entryMap.count("first"));
    CHECK(!ac.entryMap.count("second"));

    // Skip keys that are already defined
    check(R"(
type Test = { first: number, second: number }
local t: Test = { first = 2, s }
    )");

    ac = autocomplete(2, 30);
    CHECK(!ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

    // Don't skip active key
    check(R"(
type Test = { first: number, second: number }
local t: Test = { first }
    )");

    ac = autocomplete(2, 23);
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

    // Inference after first key
    check(R"(
local t = {
    { first = 5, second = 10 },
    { f }
}
    )");

    ac = autocomplete(3, 7);
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

    check(R"(
local t = {
    [2] = { first = 5, second = 10 },
    [5] = { f }
}
    )");

    ac = autocomplete(3, 13);
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));
}

TEST_CASE_FIXTURE(Fixture, "autocomplete_documentation_symbols")
{
    loadDefinition(R"(
        declare y: {
            x: number,
        }
    )");

    fileResolver.source["Module/A"] = R"(
        local a = y.
    )";

    frontend.check("Module/A");

    auto ac = autocomplete(frontend, "Module/A", Position{1, 21}, nullCallback);

    REQUIRE(ac.entryMap.count("x"));
    CHECK_EQ(ac.entryMap["x"].documentationSymbol, "@test/global/y.x");
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_ifelse_expressions")
{
    ScopedFastFlag sff1{"LuauIfElseExpressionBaseSupport", true};
    ScopedFastFlag sff2{"LuauIfElseExpressionAnalysisSupport", true};

    {
        check(R"(
local temp = false
local even = true;
local a = true
a = if t1@emp then t
a = if temp t2@
a = if temp then e3@
a = if temp then even e4@
a = if temp then even elseif t5@
a = if temp then even elseif true t6@
a = if temp then even elseif true then t7@
a = if temp then even elseif true then temp e8@
a = if temp then even elseif true then temp else e9@
        )");

        auto ac = autocomplete('1');
        CHECK(ac.entryMap.count("temp"));
        CHECK(ac.entryMap.count("true"));
        CHECK(ac.entryMap.count("then") == 0);
        CHECK(ac.entryMap.count("else") == 0);
        CHECK(ac.entryMap.count("elseif") == 0);

        ac = autocomplete('2');
        CHECK(ac.entryMap.count("temp") == 0);
        CHECK(ac.entryMap.count("true") == 0);
        CHECK(ac.entryMap.count("then"));
        CHECK(ac.entryMap.count("else") == 0);
        CHECK(ac.entryMap.count("elseif") == 0);

        ac = autocomplete('3');
        CHECK(ac.entryMap.count("even"));
        CHECK(ac.entryMap.count("then") == 0);
        CHECK(ac.entryMap.count("else") == 0);
        CHECK(ac.entryMap.count("elseif") == 0);

        ac = autocomplete('4');
        CHECK(ac.entryMap.count("even") == 0);
        CHECK(ac.entryMap.count("then") == 0);
        CHECK(ac.entryMap.count("else"));
        CHECK(ac.entryMap.count("elseif"));

        ac = autocomplete('5');
        CHECK(ac.entryMap.count("temp"));
        CHECK(ac.entryMap.count("true"));
        CHECK(ac.entryMap.count("then") == 0);
        CHECK(ac.entryMap.count("else") == 0);
        CHECK(ac.entryMap.count("elseif") == 0);

        ac = autocomplete('6');
        CHECK(ac.entryMap.count("temp") == 0);
        CHECK(ac.entryMap.count("true") == 0);
        CHECK(ac.entryMap.count("then"));
        CHECK(ac.entryMap.count("else") == 0);
        CHECK(ac.entryMap.count("elseif") == 0);

        ac = autocomplete('7');
        CHECK(ac.entryMap.count("temp"));
        CHECK(ac.entryMap.count("true"));
        CHECK(ac.entryMap.count("then") == 0);
        CHECK(ac.entryMap.count("else") == 0);
        CHECK(ac.entryMap.count("elseif") == 0);

        ac = autocomplete('8');
        CHECK(ac.entryMap.count("even") == 0);
        CHECK(ac.entryMap.count("then") == 0);
        CHECK(ac.entryMap.count("else"));
        CHECK(ac.entryMap.count("elseif"));

        ac = autocomplete('9');
        CHECK(ac.entryMap.count("then") == 0);
        CHECK(ac.entryMap.count("else") == 0);
        CHECK(ac.entryMap.count("elseif") == 0);
    }
}

TEST_SUITE_END();
