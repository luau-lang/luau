// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"
#include "lualib.h"

#include "Repl.h"

#include "doctest.h"

#include <iostream>
#include <memory>
#include <string>
#include <vector>


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
    lua_State* L;

private:
    std::unique_ptr<lua_State, void (*)(lua_State*)> luaState;

    // This is a simplicitic and incomplete pretty printer.
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
