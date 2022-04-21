// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Autocomplete.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"
#include "Luau/StringUtils.h"

#include "Fixture.h"

#include "doctest.h"

#include <map>

LUAU_FASTFLAG(LuauTraceTypesInNonstrictMode2)
LUAU_FASTFLAG(LuauSetMetatableDoesNotTimeTravel)
LUAU_FASTFLAG(LuauSeparateTypechecks)

using namespace Luau;

static std::optional<AutocompleteEntryMap> nullCallback(std::string tag, std::optional<const ClassTypeVar*> ptr)
{
    return std::nullopt;
}

template<class BaseType>
struct ACFixtureImpl : BaseType
{
    ACFixtureImpl()
        : Fixture(true, true)
    {
    }

    AutocompleteResult autocomplete(unsigned row, unsigned column)
    {
        return Luau::autocomplete(this->frontend, "MainModule", Position{row, column}, nullCallback);
    }

    AutocompleteResult autocomplete(char marker)
    {
        return Luau::autocomplete(this->frontend, "MainModule", getPosition(marker), nullCallback);
    }

    CheckResult check(const std::string& source)
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
                LUAU_ASSERT("Illegal marker character" && c >= '0' && c <= '9');
                LUAU_ASSERT("Duplicate marker found" && markerPosition.count(c) == 0);
                markerPosition.insert(std::pair{c, curPos});
            }
            else if (c == '@')
            {
                // skip the '@' character
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

        return BaseType::check(filteredSource);
    }

    LoadDefinitionFileResult loadDefinition(const std::string& source)
    {
        if (FFlag::LuauSeparateTypechecks)
        {
            TypeChecker& typeChecker = this->frontend.typeCheckerForAutocomplete;
            unfreeze(typeChecker.globalTypes);
            LoadDefinitionFileResult result = loadDefinitionFile(typeChecker, typeChecker.globalScope, source, "@test");
            freeze(typeChecker.globalTypes);

            REQUIRE_MESSAGE(result.success, "loadDefinition: unable to load definition file");
            return result;
        }
        else
        {
            return BaseType::loadDefinition(source);
        }
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
};

TEST_SUITE_BEGIN("AutocompleteTest");

TEST_CASE_FIXTURE(ACFixture, "empty_program")
{
    check(" @1");

    auto ac = autocomplete('1');

    CHECK(!ac.entryMap.empty());
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "local_initializer")
{
    check("local a = @1");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "leave_numbers_alone")
{
    check("local a = 3.@11");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "user_defined_globals")
{
    check("local myLocal = 4; @1");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("myLocal"));
    CHECK(ac.entryMap.count("table"));
    CHECK(ac.entryMap.count("math"));
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
    CHECK(!ac.entryMap.count("myInnerLocal"));

    ac = autocomplete('2');
    CHECK(ac.entryMap.count("myLocal"));
    CHECK(ac.entryMap.count("myInnerLocal"));

    ac = autocomplete('3');
    CHECK(ac.entryMap.count("myLocal"));
    CHECK(!ac.entryMap.count("myInnerLocal"));
}

TEST_CASE_FIXTURE(ACFixture, "recursive_function")
{
    check(R"(
        function foo()
@1        end
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("foo"));
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
    CHECK(!ac.entryMap.count("abc"));
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

TEST_CASE_FIXTURE(ACFixture, "get_member_completions")
{
    check(R"(
        local a = table.@1
    )");

    auto ac = autocomplete('1');

    CHECK_EQ(17, ac.entryMap.size());
    CHECK(ac.entryMap.count("find"));
    CHECK(ac.entryMap.count("pack"));
    CHECK(!ac.entryMap.count("math"));
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
}

TEST_CASE_FIXTURE(ACFixture, "table_intersection")
{
    check(R"(
        type t1 = { a1 : string, b2 : number }
        type t2 = { b2 : string, c3 : string }
        function func(abc : t1 & t2)
            abc.  @1
        end
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(3, ac.entryMap.size());
    CHECK(ac.entryMap.count("a1"));
    CHECK(ac.entryMap.count("b2"));
    CHECK(ac.entryMap.count("c3"));
}

TEST_CASE_FIXTURE(ACFixture, "get_string_completions")
{
    check(R"(
        local a = ("foo"):@1
    )");

    auto ac = autocomplete('1');

    CHECK_EQ(17, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "get_suggestions_for_new_statement")
{
    check("@1");

    auto ac = autocomplete('1');

    CHECK_NE(0, ac.entryMap.size());

    CHECK(ac.entryMap.count("table"));
}

TEST_CASE_FIXTURE(ACFixture, "get_suggestions_for_the_very_start_of_the_script")
{
    check(R"(@1

        function aaa() end
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("table"));
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

    CHECK(!ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "method_call_inside_if_conditional")
{
    check(R"(
        if table:  @1
    )");

    auto ac = autocomplete('1');

    CHECK_NE(0, ac.entryMap.size());
    CHECK(ac.entryMap.count("concat"));
    CHECK(!ac.entryMap.count("math"));
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

    TypeId t = follow(*ac.entryMap["A"].type);
    const TableTypeVar* tt = get<TableTypeVar>(t);
    REQUIRE(tt);

    CHECK(tt->props.count("two"));
}

TEST_CASE_FIXTURE(ACFixture, "recommend_statement_starting_keywords")
{
    check("@1");
    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("local"));

    check("local i = @1");
    auto ac2 = autocomplete('1');
    CHECK(!ac2.entryMap.count("local"));
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
}

TEST_CASE_FIXTURE(ACFixture, "dont_offer_any_suggestions_from_the_end_of_a_comment")
{
    check(R"(
        --!strict@1
    )");

    auto ac = autocomplete('1');

    CHECK_EQ(0, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "dont_offer_any_suggestions_from_within_a_broken_comment")
{
    check(R"(
        --[[ @1
    )");

    auto ac = autocomplete('1');

    CHECK_EQ(0, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "dont_offer_any_suggestions_from_within_a_broken_comment_at_the_very_end_of_the_file")
{
    check("--[[@1");

    auto ac = autocomplete('1');
    CHECK_EQ(0, ac.entryMap.size());
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_for_middle_keywords")
{
    check(R"(
        for x @1=
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.count("do"), 0);
    CHECK_EQ(ac1.entryMap.count("end"), 0);

    check(R"(
        for x =@1 1
    )");

    auto ac2 = autocomplete('1');
    CHECK_EQ(ac2.entryMap.count("do"), 0);
    CHECK_EQ(ac2.entryMap.count("end"), 0);

    check(R"(
        for x = 1,@1 2
    )");

    auto ac3 = autocomplete('1');
    CHECK_EQ(1, ac3.entryMap.size());
    CHECK_EQ(ac3.entryMap.count("do"), 1);

    check(R"(
        for x = 1, @12,
    )");

    auto ac4 = autocomplete('1');
    CHECK_EQ(ac4.entryMap.count("do"), 0);
    CHECK_EQ(ac4.entryMap.count("end"), 0);

    check(R"(
        for x = 1, 2, @15
    )");

    auto ac5 = autocomplete('1');
    CHECK_EQ(ac5.entryMap.count("do"), 1);
    CHECK_EQ(ac5.entryMap.count("end"), 0);

    check(R"(
        for x = 1, 2, 5 f@1
    )");

    auto ac6 = autocomplete('1');
    CHECK_EQ(ac6.entryMap.size(), 1);
    CHECK_EQ(ac6.entryMap.count("do"), 1);

    check(R"(
        for x = 1, 2, 5 do      @1
    )");

    auto ac7 = autocomplete('1');
    CHECK_EQ(ac7.entryMap.count("end"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_for_in_middle_keywords")
{
    check(R"(
        for @1
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(0, ac1.entryMap.size());

    check(R"(
        for x@1 @2
    )");

    auto ac2 = autocomplete('1');
    CHECK_EQ(0, ac2.entryMap.size());

    auto ac2a = autocomplete('2');
    CHECK_EQ(1, ac2a.entryMap.size());
    CHECK_EQ(1, ac2a.entryMap.count("in"));

    check(R"(
        for x in y@1
    )");

    auto ac3 = autocomplete('1');
    CHECK_EQ(ac3.entryMap.count("table"), 1);
    CHECK_EQ(ac3.entryMap.count("do"), 0);

    check(R"(
        for x in y @1
    )");

    auto ac4 = autocomplete('1');
    CHECK_EQ(ac4.entryMap.size(), 1);
    CHECK_EQ(ac4.entryMap.count("do"), 1);

    check(R"(
        for x in f f@1
    )");

    auto ac5 = autocomplete('1');
    CHECK_EQ(ac5.entryMap.size(), 1);
    CHECK_EQ(ac5.entryMap.count("do"), 1);

    check(R"(
        for x in y do  @1
    )");

    auto ac6 = autocomplete('1');
    CHECK_EQ(ac6.entryMap.count("in"), 0);
    CHECK_EQ(ac6.entryMap.count("table"), 1);
    CHECK_EQ(ac6.entryMap.count("end"), 1);
    CHECK_EQ(ac6.entryMap.count("function"), 1);

    check(R"(
        for x in y do e@1
    )");

    auto ac7 = autocomplete('1');
    CHECK_EQ(ac7.entryMap.count("in"), 0);
    CHECK_EQ(ac7.entryMap.count("table"), 1);
    CHECK_EQ(ac7.entryMap.count("end"), 1);
    CHECK_EQ(ac7.entryMap.count("function"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_while_middle_keywords")
{
    check(R"(
        while@1
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.count("do"), 0);
    CHECK_EQ(ac1.entryMap.count("end"), 0);

    check(R"(
        while true @1
    )");

    auto ac2 = autocomplete('1');
    CHECK_EQ(1, ac2.entryMap.size());
    CHECK_EQ(ac2.entryMap.count("do"), 1);

    check(R"(
        while true do  @1
    )");

    auto ac3 = autocomplete('1');
    CHECK_EQ(ac3.entryMap.count("end"), 1);

    check(R"(
        while true d@1
    )");

    auto ac4 = autocomplete('1');
    CHECK_EQ(1, ac4.entryMap.size());
    CHECK_EQ(ac4.entryMap.count("do"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_if_middle_keywords")
{
    check(R"(
        if   @1
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.count("then"), 0);
    CHECK_EQ(ac1.entryMap.count("function"),
        1); // FIXME: This is kind of dumb.  It is technically syntactically valid but you can never do anything interesting with this.
    CHECK_EQ(ac1.entryMap.count("table"), 1);
    CHECK_EQ(ac1.entryMap.count("else"), 0);
    CHECK_EQ(ac1.entryMap.count("elseif"), 0);
    CHECK_EQ(ac1.entryMap.count("end"), 0);

    check(R"(
        if x  @1
    )");

    auto ac2 = autocomplete('1');
    CHECK_EQ(ac2.entryMap.count("then"), 1);
    CHECK_EQ(ac2.entryMap.count("function"), 0);
    CHECK_EQ(ac2.entryMap.count("else"), 0);
    CHECK_EQ(ac2.entryMap.count("elseif"), 0);
    CHECK_EQ(ac2.entryMap.count("end"), 0);

    check(R"(
        if x t@1
    )");

    auto ac3 = autocomplete('1');
    CHECK_EQ(1, ac3.entryMap.size());
    CHECK_EQ(ac3.entryMap.count("then"), 1);

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
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_until_in_repeat")
{
    check(R"(
        repeat  @1
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("table"), 1);
    CHECK_EQ(ac.entryMap.count("until"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_until_expression")
{
    check(R"(
        repeat
        until   @1
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("table"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "local_names")
{
    check(R"(
        local ab@1
    )");

    auto ac1 = autocomplete('1');
    CHECK_EQ(ac1.entryMap.size(), 1);
    CHECK_EQ(ac1.entryMap.count("function"), 1);

    check(R"(
        local ab, cd@1
    )");

    auto ac2 = autocomplete('1');
    CHECK(ac2.entryMap.empty());
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_end_with_fn_exprs")
{
    check(R"(
        local function f()  @1
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("end"), 1);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_end_with_lambda")
{
    check(R"(
        local a = function() local bar = foo en@1
    )");

    auto ac = autocomplete('1');
    CHECK_EQ(ac.entryMap.count("end"), 1);
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

    check(R"(
        local function abc(def, ghi@1)
        end
    )");

    auto ac3 = autocomplete('1');
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
@1
        end
    )");

    auto ac2 = autocomplete('1');
    CHECK_EQ(ac2.entryMap.count("abc"), 1);
    CHECK_EQ(ac2.entryMap.count("def"), 1);

    check(R"(
        function abc(def, ghi@1)
        end
    )");

    auto ac3 = autocomplete('1');
    CHECK(ac3.entryMap.empty());
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

    CHECK(!ac.entryMap.count("num"));
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
local function f(a: n@1
local b: string = "don't trip"
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("nil"));
    CHECK(ac.entryMap.count("number"));
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
    check(R"(
type Foo = { a: number, b: string }
local a = { one = 4, two = "hello" }
local b: Foo = { a = a.@1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("one"));
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::None);

    check(R"(
type Foo = { a: number, b: string }
local a = { one = 4, two = "hello" }
local b: Foo = { b = a.@1
    )");

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("two"));
    CHECK(ac.entryMap["two"].typeCorrect == TypeCorrectKind::Correct);
    CHECK(ac.entryMap["one"].typeCorrect == TypeCorrectKind::None);
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
local bar: @1= foo
    )");

    auto ac = autocomplete('1');

    CHECK(!ac.entryMap.count("foo"));
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

    CHECK(!ac.entryMap.count("name"));
    CHECK(ac.entryMap.count("other"));

    check(R"(
local other = 1
local name, test = na@1
    )");

    ac = autocomplete('1');

    CHECK(!ac.entryMap.count("name"));
    CHECK(!ac.entryMap.count("test"));
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

    CHECK_EQ(17, ac.entryMap.size());
    CHECK(ac.entryMap.count("find"));
    CHECK(ac.entryMap.count("pack"));
    CHECK(!ac.entryMap.count("math"));
}

TEST_CASE_FIXTURE(ACFixture, "autocompleteSource_require")
{
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
local t: Test = { f@1 }
    )");

    auto ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

    // Intersection
    check(R"(
type Test = { first: number } & { second: number }
local t: Test = { f@1 }
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

    // Union
    check(R"(
type Test = { first: number, second: number } | { second: number, third: number }
local t: Test = { s@1 }
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("second"));
    CHECK(!ac.entryMap.count("first"));
    CHECK(!ac.entryMap.count("third"));

    // No parenthesis suggestion
    check(R"(
type Test = { first: (number) -> number, second: number }
local t: Test = { f@1 }
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap["first"].parens == ParenthesesRecommendation::None);

    // When key is changed
    check(R"(
type Test = { first: number, second: number }
local t: Test = { f@1 = 2 }
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

    // Alternative key syntax
    check(R"(
type Test = { first: number, second: number }
local t: Test = { ["f@1"] }
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

    // Not an alternative key syntax
    check(R"(
type Test = { first: number, second: number }
local t: Test = { "f@1" }
    )");

    ac = autocomplete('1');
    CHECK(!ac.entryMap.count("first"));
    CHECK(!ac.entryMap.count("second"));

    // Skip keys that are already defined
    check(R"(
type Test = { first: number, second: number }
local t: Test = { first = 2, s@1 }
    )");

    ac = autocomplete('1');
    CHECK(!ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

    // Don't skip active key
    check(R"(
type Test = { first: number, second: number }
local t: Test = { first@1 }
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));

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

    check(R"(
local t = {
    [2] = { first = 5, second = 10 },
    [5] = { f@1 }
}
    )");

    ac = autocomplete('1');
    CHECK(ac.entryMap.count("first"));
    CHECK(ac.entryMap.count("second"));
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

TEST_CASE_FIXTURE(ACFixture, "autocomplete_if_else_regression")
{
    ScopedFastFlag FFlagLuauIfElseExprFixCompletionIssue("LuauIfElseExprFixCompletionIssue", true);
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

TEST_CASE_FIXTURE(ACFixture, "autocomplete_explicit_type_pack")
{
    check(R"(
type A<T...> = () -> T...
local a: A<(number, s@1>
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap.count("string"));
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
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_default_type_pack_parameters")
{
    check(R"(
type A<T... = ...@1> = () -> T
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("number"));
    CHECK(ac.entryMap.count("string"));
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_oop_implicit_self")
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

TEST_CASE_FIXTURE(ACFixture, "autocomplete_on_string_singletons")
{
    check(R"(
        --!strict
        local foo: "hello" | "bye" = "hello"
        foo:@1
    )");

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("format"));
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_string_singletons")
{
    ScopedFastFlag luauAutocompleteSingletonTypes{"LuauAutocompleteSingletonTypes", true};
    ScopedFastFlag luauExpectedTypesOfProperties{"LuauExpectedTypesOfProperties", true};

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

    ac = autocomplete('2');

    CHECK(ac.entryMap.count("\"cat\""));
    CHECK(ac.entryMap.count("\"dog\""));

    ac = autocomplete('3');

    CHECK(ac.entryMap.count("cat"));
    CHECK(ac.entryMap.count("dog"));

    check(R"(
        type tagged = {tag:"cat", fieldx:number} | {tag:"dog", fieldy:number}
        local x: tagged = {tag="@4"}
    )");

    ac = autocomplete('4');

    CHECK(ac.entryMap.count("cat"));
    CHECK(ac.entryMap.count("dog"));
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_string_singleton_equality")
{
    ScopedFastFlag luauAutocompleteSingletonTypes{"LuauAutocompleteSingletonTypes", true};

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
    ScopedFastFlag luauAutocompleteSingletonTypes{"LuauAutocompleteSingletonTypes", true};

    check(R"(
local function f(x: true) end
f(@1)
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("true"));
    CHECK(ac.entryMap["true"].typeCorrect == TypeCorrectKind::Correct);
    REQUIRE(ac.entryMap.count("false"));
    CHECK(ac.entryMap["false"].typeCorrect == TypeCorrectKind::None);
}

TEST_CASE_FIXTURE(ACFixture, "autocomplete_string_singleton_escape")
{
    ScopedFastFlag luauAutocompleteSingletonTypes{"LuauAutocompleteSingletonTypes", true};

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
    ScopedFastFlag selfCallAutocompleteFix{"LuauSelfCallAutocompleteFix", true};

    loadDefinition(R"(
declare class Foo
    function one(self): number
    two: () -> number
end
    )");

    {
        check(R"(
local t: Foo
t:@1
        )");

        auto ac = autocomplete('1');

        REQUIRE(ac.entryMap.count("one"));
        REQUIRE(ac.entryMap.count("two"));
        CHECK(!ac.entryMap["one"].wrongIndexType);
        CHECK(ac.entryMap["two"].wrongIndexType);
    }

    {
        check(R"(
local t: Foo
t.@1
        )");

        auto ac = autocomplete('1');

        REQUIRE(ac.entryMap.count("one"));
        REQUIRE(ac.entryMap.count("two"));
        CHECK(ac.entryMap["one"].wrongIndexType);
        CHECK(!ac.entryMap["two"].wrongIndexType);
    }
}

TEST_CASE_FIXTURE(ACFixture, "no_incompatible_self_calls")
{
    ScopedFastFlag selfCallAutocompleteFix{"LuauSelfCallAutocompleteFix", true};

    check(R"(
local t = {}
function t.m() end
t:@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("m"));
    CHECK(ac.entryMap["m"].wrongIndexType);
}

TEST_CASE_FIXTURE(ACFixture, "no_incompatible_self_calls_2")
{
    ScopedFastFlag selfCallAutocompleteFix{"LuauSelfCallAutocompleteFix", true};

    check(R"(
local f: (() -> number) & ((number) -> number) = function(x: number?) return 2 end
local t = {}
t.f = f
t:@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("f"));
    CHECK(ac.entryMap["f"].wrongIndexType);
}

TEST_CASE_FIXTURE(ACFixture, "no_incompatible_self_calls_provisional")
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
}

TEST_CASE_FIXTURE(ACFixture, "string_prim_self_calls_are_fine")
{
    ScopedFastFlag selfCallAutocompleteFix{"LuauSelfCallAutocompleteFix", true};

    check(R"(
local s = "hello"
s:@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("byte"));
    CHECK(ac.entryMap["byte"].wrongIndexType == false);
    REQUIRE(ac.entryMap.count("char"));
    CHECK(ac.entryMap["char"].wrongIndexType == true);
    REQUIRE(ac.entryMap.count("sub"));
    CHECK(ac.entryMap["sub"].wrongIndexType == false);
}

TEST_CASE_FIXTURE(ACFixture, "string_prim_non_self_calls_are_avoided")
{
    ScopedFastFlag selfCallAutocompleteFix{"LuauSelfCallAutocompleteFix", true};

    check(R"(
local s = "hello"
s.@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("byte"));
    CHECK(ac.entryMap["byte"].wrongIndexType == true);
    REQUIRE(ac.entryMap.count("char"));
    CHECK(ac.entryMap["char"].wrongIndexType == false);
    REQUIRE(ac.entryMap.count("sub"));
    CHECK(ac.entryMap["sub"].wrongIndexType == true);
}

TEST_CASE_FIXTURE(ACFixture, "string_library_non_self_calls_are_fine")
{
    ScopedFastFlag selfCallAutocompleteFix{"LuauSelfCallAutocompleteFix", true};

    check(R"(
string.@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("byte"));
    CHECK(ac.entryMap["byte"].wrongIndexType == false);
    REQUIRE(ac.entryMap.count("char"));
    CHECK(ac.entryMap["char"].wrongIndexType == false);
    REQUIRE(ac.entryMap.count("sub"));
    CHECK(ac.entryMap["sub"].wrongIndexType == false);
}

TEST_CASE_FIXTURE(ACFixture, "string_library_self_calls_are_invalid")
{
    ScopedFastFlag selfCallAutocompleteFix{"LuauSelfCallAutocompleteFix", true};

    check(R"(
string:@1
    )");

    auto ac = autocomplete('1');

    REQUIRE(ac.entryMap.count("byte"));
    CHECK(ac.entryMap["byte"].wrongIndexType == true);
    REQUIRE(ac.entryMap.count("char"));
    CHECK(ac.entryMap["char"].wrongIndexType == true);
    REQUIRE(ac.entryMap.count("sub"));
    CHECK(ac.entryMap["sub"].wrongIndexType == true);
}

TEST_CASE_FIXTURE(ACFixture, "source_module_preservation_and_invalidation")
{
    check(R"(
local a = { x = 2, y = 4 }
a.@1
    )");

    frontend.clear();

    auto ac = autocomplete('1');

    CHECK(ac.entryMap.count("x"));
    CHECK(ac.entryMap.count("y"));

    frontend.check("MainModule", {});

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("x"));
    CHECK(ac.entryMap.count("y"));

    frontend.markDirty("MainModule", nullptr);

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("x"));
    CHECK(ac.entryMap.count("y"));

    frontend.check("MainModule", {});

    ac = autocomplete('1');

    CHECK(ac.entryMap.count("x"));
    CHECK(ac.entryMap.count("y"));
}

TEST_SUITE_END();
