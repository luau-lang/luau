// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"
#include "lualib.h"

#include "Repl.h"

#include "doctest.h"

#include <iostream>
#include <memory>
#include <set>
#include <string>
#include <vector>

struct Completion
{
    std::string completion;
    std::string display;

    bool operator<(Completion const& other) const
    {
        return std::tie(completion, display) < std::tie(other.completion, other.display);
    }
};

using CompletionSet = std::set<Completion>;

class ReplFixture
{
public:
    ReplFixture()
        : luaState(luaL_newstate(), lua_close)
    {
        L = luaState.get();
        setupState(L);
        luaL_sandboxthread(L);

        std::string result = runCode(L, prettyPrintSource);
    }

    // Returns all of the output captured from the pretty printer
    std::string getCapturedOutput()
    {
        lua_getglobal(L, "capturedoutput");
        const char* str = lua_tolstring(L, -1, nullptr);
        std::string result(str);
        lua_pop(L, 1);
        return result;
    }

    CompletionSet getCompletionSet(const char* inputPrefix)
    {
        CompletionSet result;
        int top = lua_gettop(L);
        getCompletions(L, inputPrefix, [&result](const std::string& completion, const std::string& display) {
            result.insert(Completion{completion, display});
        });
        // Ensure that generating completions doesn't change the position of luau's stack top.
        CHECK(top == lua_gettop(L));

        return result;
    }

    bool checkCompletion(const CompletionSet& completions, const std::string& prefix, const std::string& expected)
    {
        std::string expectedDisplay(expected.substr(0, expected.find_first_of('(')));
        Completion expectedCompletion{prefix + expected, expectedDisplay};
        return completions.count(expectedCompletion) == 1;
    }

    lua_State* L;

private:
    std::unique_ptr<lua_State, void (*)(lua_State*)> luaState;

    // This is a simplistic and incomplete pretty printer.
    // It is included here to test that the pretty printer hook is being called.
    // More elaborate tests to ensure correct output can be added if we introduce
    // a more feature rich pretty printer.
    std::string prettyPrintSource = R"(
-- Accumulate pretty printer output in `capturedoutput`
capturedoutput = ""

function arraytostring(arr)
    local strings = {} 
    table.foreachi(arr, function(k,v) table.insert(strings, pptostring(v)) end )
    return "{" .. table.concat(strings, ", ") .. "}"
end

function pptostring(x)
    if type(x) == "table" then
        -- Just assume array-like tables for now.
        return arraytostring(x)
    elseif type(x) == "string" then
        return '"' .. x .. '"'
    else
        return tostring(x)
    end
end

-- Note: Instead of calling print, the pretty printer just stores the output
-- in `capturedoutput` so we can check for the correct results.
function _PRETTYPRINT(...)
    local args = table.pack(...)
    local strings = {}
    for i=1, args.n do
        local item = args[i]
        local str = pptostring(item, customoptions)
        if i == 1 then
            capturedoutput = capturedoutput .. str
        else
            capturedoutput = capturedoutput .. "\t" .. str
        end
    end
end
)";
};

TEST_SUITE_BEGIN("ReplPrettyPrint");

TEST_CASE_FIXTURE(ReplFixture, "AdditionStatement")
{
    runCode(L, "return 30 + 12");
    CHECK(getCapturedOutput() == "42");
}

TEST_CASE_FIXTURE(ReplFixture, "TableLiteral")
{
    runCode(L, "return {1, 2, 3, 4}");
    CHECK(getCapturedOutput() == "{1, 2, 3, 4}");
}

TEST_CASE_FIXTURE(ReplFixture, "StringLiteral")
{
    runCode(L, "return 'str'");
    CHECK(getCapturedOutput() == "\"str\"");
}

TEST_CASE_FIXTURE(ReplFixture, "TableWithStringLiterals")
{
    runCode(L, "return {1, 'two', 3, 'four'}");
    CHECK(getCapturedOutput() == "{1, \"two\", 3, \"four\"}");
}

TEST_CASE_FIXTURE(ReplFixture, "MultipleArguments")
{
    runCode(L, "return 3, 'three'");
    CHECK(getCapturedOutput() == "3\t\"three\"");
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("ReplCodeCompletion");

TEST_CASE_FIXTURE(ReplFixture, "CompleteGlobalVariables")
{
    runCode(L, R"(
        myvariable1 = 5
        myvariable2 = 5
)");
    {
        // Try to complete globals that are added by the user's script
        CompletionSet completions = getCompletionSet("myvar");

        std::string prefix = "";
        CHECK(completions.size() == 2);
        CHECK(checkCompletion(completions, prefix, "myvariable1"));
        CHECK(checkCompletion(completions, prefix, "myvariable2"));
    }
    {
        // Try completing some builtin functions
        CompletionSet completions = getCompletionSet("math.m");

        std::string prefix = "math.";
        CHECK(completions.size() == 3);
        CHECK(checkCompletion(completions, prefix, "max("));
        CHECK(checkCompletion(completions, prefix, "min("));
        CHECK(checkCompletion(completions, prefix, "modf("));
    }
}

TEST_CASE_FIXTURE(ReplFixture, "CompleteTableKeys")
{
    runCode(L, R"(
        t = { color = "red", size = 1, shape = "circle" }
)");
    {
        CompletionSet completions = getCompletionSet("t.");

        std::string prefix = "t.";
        CHECK(completions.size() == 3);
        CHECK(checkCompletion(completions, prefix, "color"));
        CHECK(checkCompletion(completions, prefix, "size"));
        CHECK(checkCompletion(completions, prefix, "shape"));
    }

    {
        CompletionSet completions = getCompletionSet("t.s");

        std::string prefix = "t.";
        CHECK(completions.size() == 2);
        CHECK(checkCompletion(completions, prefix, "size"));
        CHECK(checkCompletion(completions, prefix, "shape"));
    }
}

TEST_CASE_FIXTURE(ReplFixture, "StringMethods")
{
    runCode(L, R"(
        s = ""
)");
    {
        CompletionSet completions = getCompletionSet("s:l");

        std::string prefix = "s:";
        CHECK(completions.size() == 2);
        CHECK(checkCompletion(completions, prefix, "len("));
        CHECK(checkCompletion(completions, prefix, "lower("));
    }
}

TEST_CASE_FIXTURE(ReplFixture, "TableWithMetatableIndexTable")
{
    runCode(L, R"(
        -- Create 't' which is a table with a metatable with an __index table
        mt = {}
        mt.__index = mt

        t = {}
        setmetatable(t, mt)

        mt.mtkey1 = {x="x value", y="y value", 1, 2}
        mt.mtkey2 = 2

        t.tkey1 = {data1 = 2, data2 = "str", 3, 4}
        t.tkey2 = 4
)");
    {
        CompletionSet completions = getCompletionSet("t.t");

        std::string prefix = "t.";
        CHECK(completions.size() == 2);
        CHECK(checkCompletion(completions, prefix, "tkey1"));
        CHECK(checkCompletion(completions, prefix, "tkey2"));
    }
    {
        CompletionSet completions = getCompletionSet("t.tkey1.data2:re");

        std::string prefix = "t.tkey1.data2:";
        CHECK(completions.size() == 2);
        CHECK(checkCompletion(completions, prefix, "rep("));
        CHECK(checkCompletion(completions, prefix, "reverse("));
    }
    {
        CompletionSet completions = getCompletionSet("t.mtk");

        std::string prefix = "t.";
        CHECK(completions.size() == 2);
        CHECK(checkCompletion(completions, prefix, "mtkey1"));
        CHECK(checkCompletion(completions, prefix, "mtkey2"));
    }
    {
        CompletionSet completions = getCompletionSet("t.mtkey1.");

        std::string prefix = "t.mtkey1.";
        CHECK(completions.size() == 2);
        CHECK(checkCompletion(completions, prefix, "x"));
        CHECK(checkCompletion(completions, prefix, "y"));
    }
}

TEST_CASE_FIXTURE(ReplFixture, "TableWithMetatableIndexFunction")
{
    runCode(L, R"(
        -- Create 't' which is a table with a metatable with an __index function
        mt = {}
        mt.__index = function(table, key)
            print("mt.__index called")
            if key == "foo" then
                return "FOO"
            elseif key == "bar" then
                return "BAR"
            else
                return nil
            end
        end

        t = {}
        setmetatable(t, mt)
        t.tkey = 0
)");
    {
        CompletionSet completions = getCompletionSet("t.t");

        std::string prefix = "t.";
        CHECK(completions.size() == 1);
        CHECK(checkCompletion(completions, prefix, "tkey"));
    }
    {
        // t.foo is a valid key, but should not be completed because it requires calling an __index function
        CompletionSet completions = getCompletionSet("t.foo");

        CHECK(completions.size() == 0);
    }
    {
        // t.foo is a valid key, but should not be found because it requires calling an __index function
        CompletionSet completions = getCompletionSet("t.foo:");

        CHECK(completions.size() == 0);
    }
}

TEST_CASE_FIXTURE(ReplFixture, "TableWithMultipleMetatableIndexTables")
{
    runCode(L, R"(
        -- Create a table with a chain of metatables
        mt2 = {}
        mt2.__index = mt2

        mt = {}
        mt.__index = mt
        setmetatable(mt, mt2)

        t = {}
        setmetatable(t, mt)

        mt2.mt2key = {x=1, y=2}
        mt.mtkey = 2
        t.tkey = 3
)");
    {
        CompletionSet completions = getCompletionSet("t.");

        std::string prefix = "t.";
        CHECK(completions.size() == 4);
        CHECK(checkCompletion(completions, prefix, "__index"));
        CHECK(checkCompletion(completions, prefix, "tkey"));
        CHECK(checkCompletion(completions, prefix, "mtkey"));
        CHECK(checkCompletion(completions, prefix, "mt2key"));
    }
    {
        CompletionSet completions = getCompletionSet("t.__index.");

        std::string prefix = "t.__index.";
        CHECK(completions.size() == 3);
        CHECK(checkCompletion(completions, prefix, "__index"));
        CHECK(checkCompletion(completions, prefix, "mtkey"));
        CHECK(checkCompletion(completions, prefix, "mt2key"));
    }
    {
        CompletionSet completions = getCompletionSet("t.mt2key.");

        std::string prefix = "t.mt2key.";
        CHECK(completions.size() == 2);
        CHECK(checkCompletion(completions, prefix, "x"));
        CHECK(checkCompletion(completions, prefix, "y"));
    }
}

TEST_CASE_FIXTURE(ReplFixture, "TableWithDeepMetatableIndexTables")
{
    runCode(L, R"(
-- Creates a table with a chain of metatables of length `count`
function makeChainedTable(count)
    local result = {}
    result.__index = result
    result[string.format("entry%d", count)] = { count = count }
    if count == 0 then
        return result
    else
        return setmetatable(result, makeChainedTable(count - 1))
    end
end

t30 = makeChainedTable(30)
t60 = makeChainedTable(60)
)");
    {
        // Check if entry0 exists
        CompletionSet completions = getCompletionSet("t30.entry0");

        std::string prefix = "t30.";
        CHECK(checkCompletion(completions, prefix, "entry0"));
    }
    {
        // Check if entry0.count exists
        CompletionSet completions = getCompletionSet("t30.entry0.co");

        std::string prefix = "t30.entry0.";
        CHECK(checkCompletion(completions, prefix, "count"));
    }
    {
        // Check if entry0 exists.  With the max traversal limit of 50 in the repl, this should fail.
        CompletionSet completions = getCompletionSet("t60.entry0");

        CHECK(completions.size() == 0);
    }
    {
        // Check if entry0.count exists.  With the max traversal limit of 50 in the repl, this should fail.
        CompletionSet completions = getCompletionSet("t60.entry0.co");

        CHECK(completions.size() == 0);
    }
}

TEST_SUITE_END();
