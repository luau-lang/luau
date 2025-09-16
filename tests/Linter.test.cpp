// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Linter.h"
#include "Luau/BuiltinDefinitions.h"

#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

LUAU_FASTFLAG(LuauSolverV2);
LUAU_FASTFLAG(LuauParametrizedAttributeSyntax)

using namespace Luau;

TEST_SUITE_BEGIN("Linter");

TEST_CASE_FIXTURE(Fixture, "CleanCode")
{
    LintResult result = lint(R"(
function fib(n)
    return n < 2 and 1 or fib(n-1) + fib(n-2)
end

)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "type_function_fully_reduces")
{
    LintResult result = lint(R"(
function fib(n)
    return n < 2 or  fib(n-2)
end

)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "UnknownGlobal")
{
    LintResult result = lint("--!nocheck\nreturn foo");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Unknown global 'foo'");
}

TEST_CASE_FIXTURE(Fixture, "DeprecatedGlobal")
{
    // Normally this would be defined externally, so hack it in for testing
    addGlobalBinding(getFrontend().globals, "Wait", Binding{getBuiltins()->anyType, {}, true, "wait", "@test/global/Wait"});

    LintResult result = lint("Wait(5)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Global 'Wait' is deprecated, use 'wait' instead");
}

TEST_CASE_FIXTURE(Fixture, "DeprecatedGlobalNoReplacement")
{
    // Normally this would be defined externally, so hack it in for testing
    const char* deprecationReplacementString = "";
    addGlobalBinding(getFrontend().globals, "Version", Binding{getBuiltins()->anyType, {}, true, deprecationReplacementString});

    LintResult result = lint("Version()");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Global 'Version' is deprecated");
}

TEST_CASE_FIXTURE(Fixture, "PlaceholderRead")
{
    LintResult result = lint(R"(
local _ = 5
return _
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Placeholder value '_' is read here; consider using a named variable");
}

TEST_CASE_FIXTURE(Fixture, "PlaceholderReadGlobal")
{
    LintResult result = lint(R"(
_ = 5
print(_)
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Placeholder value '_' is read here; consider using a named variable");
}

TEST_CASE_FIXTURE(Fixture, "PlaceholderWrite")
{
    LintResult result = lint(R"(
local _ = 5
_ = 6
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(BuiltinsFixture, "BuiltinGlobalWrite")
{
    LintResult result = lint(R"(
math = {}

function assert(x)
end

assert(5)
)");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Built-in global 'math' is overwritten here; consider using a local or changing the name");
    CHECK_EQ(result.warnings[1].text, "Built-in global 'assert' is overwritten here; consider using a local or changing the name");
}

TEST_CASE_FIXTURE(Fixture, "MultilineBlock")
{
    LintResult result = lint(R"(
if true then print(1) print(2) print(3) end
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "A new statement is on the same line; add semi-colon on previous statement to silence");
}

TEST_CASE_FIXTURE(Fixture, "MultilineBlockSemicolonsWhitelisted")
{
    LintResult result = lint(R"(
print(1); print(2); print(3)
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "MultilineBlockMissedSemicolon")
{
    LintResult result = lint(R"(
print(1); print(2) print(3)
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "A new statement is on the same line; add semi-colon on previous statement to silence");
}

TEST_CASE_FIXTURE(Fixture, "MultilineBlockLocalDo")
{
    LintResult result = lint(R"(
local _x do
    _x = 5
end
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "ConfusingIndentation")
{
    LintResult result = lint(R"(
print(math.max(1,
2))
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Statement spans multiple lines; use indentation to silence");
}

TEST_CASE_FIXTURE(Fixture, "GlobalAsLocal")
{
    LintResult result = lint(R"(
function bar()
    foo = 6
    return foo
end

return bar()
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Global 'foo' is only used in the enclosing function 'bar'; consider changing it to local");
}

TEST_CASE_FIXTURE(Fixture, "GlobalAsLocalMultiFx")
{
    LintResult result = lint(R"(
function bar()
    foo = 6
    return foo
end

function baz()
    foo = 6
    return foo
end

return bar() + baz()
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Global 'foo' is never read before being written. Consider changing it to local");
}

TEST_CASE_FIXTURE(Fixture, "GlobalAsLocalMultiFxWithRead")
{
    LintResult result = lint(R"(
function bar()
    foo = 6
    return foo
end

function baz()
    foo = 6
    return foo
end

function read()
    print(foo)
end

return bar() + baz() + read()
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "GlobalAsLocalWithConditional")
{
    LintResult result = lint(R"(
function bar()
    if true then foo = 6 end
    return foo
end

function baz()
    foo = 6
    return foo
end

return bar() + baz()
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "GlobalAsLocal3WithConditionalRead")
{
    LintResult result = lint(R"(
function bar()
    foo = 6
    return foo
end

function baz()
    foo = 6
    return foo
end

function read()
    if false then print(foo) end
end

return bar() + baz() + read()
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "GlobalAsLocalInnerRead")
{
    LintResult result = lint(R"(
function foo()
   local f = function() return bar end
   f()
   bar = 42
end

function baz() bar = 0 end

return foo() + baz()
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "GlobalAsLocalMulti")
{
    LintResult result = lint(R"(
local createFunction = function(configValue)
    -- Create an internal convenience function
    local function internalLogic()
        print(configValue) -- prints passed-in value
    end
    -- Here, we thought we were creating another internal convenience function
    -- that closed over the passed-in configValue, but this is actually being
    -- declared at module scope!
    function moreInternalLogic()
        print(configValue) -- nil!!!
    end
    return function()
        internalLogic()
        moreInternalLogic()
        return nil
    end
end
fnA = createFunction(true)
fnB = createFunction(false)
fnA() -- prints "true", "nil"
fnB() -- prints "false", "nil"
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(
        result.warnings[0].text, "Global 'moreInternalLogic' is only used in the enclosing function defined at line 2; consider changing it to local"
    );
}

TEST_CASE_FIXTURE(Fixture, "LocalShadowLocal")
{
    LintResult result = lint(R"(
local arg = 6
print(arg)

local arg = 5
print(arg)
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Variable 'arg' shadows previous declaration at line 2");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "LocalShadowGlobal")
{
    LintResult result = lint(R"(
local math = math
global = math

function bar()
    local global = math.max(5, 1)
    return global
end

return bar()
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Variable 'global' shadows a global variable used at line 3");
}

TEST_CASE_FIXTURE(Fixture, "LocalShadowArgument")
{
    LintResult result = lint(R"(
function bar(a, b)
    local a = b + 1
    return a
end

return bar()
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Variable 'a' shadows previous declaration at line 2");
}

TEST_CASE_FIXTURE(Fixture, "LocalUnused")
{
    LintResult result = lint(R"(
local arg = 6

local function bar()
    local arg = 5
    local blarg = 6
    if arg then
        blarg = 42
    end
end

return bar()
)");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Variable 'arg' is never used; prefix with '_' to silence");
    CHECK_EQ(result.warnings[1].text, "Variable 'blarg' is never used; prefix with '_' to silence");
}

TEST_CASE_FIXTURE(Fixture, "ImportUnused")
{
    // Normally this would be defined externally, so hack it in for testing
    addGlobalBinding(getFrontend().globals, "game", getBuiltins()->anyType, "@test");

    LintResult result = lint(R"(
local Roact = require(game.Packages.Roact)
local _Roact = require(game.Packages.Roact)
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Import 'Roact' is never used; prefix with '_' to silence");
}

TEST_CASE_FIXTURE(Fixture, "FunctionUnused")
{
    LintResult result = lint(R"(
function bar()
end

local function qux()
end

function foo()
end

local function _unusedl()
end

function _unusedg()
end

return foo()
)");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Function 'bar' is never used; prefix with '_' to silence");
    CHECK_EQ(result.warnings[1].text, "Function 'qux' is never used; prefix with '_' to silence");
}

TEST_CASE_FIXTURE(Fixture, "UnreachableCodeBasic")
{
    LintResult result = lint(R"(
do
return 'ok'
end

print("hi!")
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 5);
    CHECK_EQ(result.warnings[0].text, "Unreachable code (previous statement always returns)");
}

TEST_CASE_FIXTURE(Fixture, "UnreachableCodeLoopBreak")
{
    LintResult result = lint(R"(
while true do
    do break end
    print("nope")
end

print("hi!")
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 3);
    CHECK_EQ(result.warnings[0].text, "Unreachable code (previous statement always breaks)");
}

TEST_CASE_FIXTURE(Fixture, "UnreachableCodeLoopContinue")
{
    LintResult result = lint(R"(
while true do
    do continue end
    print("nope")
end

print("hi!")
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 3);
    CHECK_EQ(result.warnings[0].text, "Unreachable code (previous statement always continues)");
}

TEST_CASE_FIXTURE(Fixture, "UnreachableCodeIfMerge")
{
    LintResult result = lint(R"(
function foo1(a)
    if a then
        return 'x'
    else
        return 'y'
    end
    return 'z'
end

function foo2(a)
    if a then
        return 'x'
    end
    return 'z'
end

function foo3(a)
    if a then
        return 'x'
    else
        print('y')
    end
    return 'z'
end

return { foo1, foo2, foo3 }
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 7);
    CHECK_EQ(result.warnings[0].text, "Unreachable code (previous statement always returns)");
}

TEST_CASE_FIXTURE(Fixture, "UnreachableCodeErrorReturnSilent")
{
    LintResult result = lint(R"(
function foo1(a)
    if a then
        error('x')
        return 'z'
    else
        error('y')
    end
end

return foo1
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "UnreachableCodeAssertFalseReturnSilent")
{
    LintResult result = lint(R"(
function foo1(a)
    if a then
        return 'z'
    end

    assert(false)
end

return foo1
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "UnreachableCodeErrorReturnNonSilentBranchy")
{
    LintResult result = lint(R"(
function foo1(a)
    if a then
        error('x')
    else
        error('y')
    end
    return 'z'
end

return foo1
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 7);
    CHECK_EQ(result.warnings[0].text, "Unreachable code (previous statement always errors)");
}

TEST_CASE_FIXTURE(Fixture, "UnreachableCodeErrorReturnPropagate")
{
    LintResult result = lint(R"(
function foo1(a)
    if a then
        error('x')
        return 'z'
    else
        error('y')
    end
    return 'x'
end

return foo1
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 8);
    CHECK_EQ(result.warnings[0].text, "Unreachable code (previous statement always errors)");
}

TEST_CASE_FIXTURE(Fixture, "UnreachableCodeLoopWhile")
{
    LintResult result = lint(R"(
function foo1(a)
    while a do
        return 'z'
    end
    return 'x'
end

return foo1
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "UnreachableCodeLoopRepeat")
{
    LintResult result = lint(R"(
function foo1(a)
    repeat
        return 'z'
    until a
    return 'x'
end

return foo1
)");

    // this is technically a bug, since the repeat body always returns; fixing this bug is a bit more involved than I'd like
    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "UnknownType")
{
    unfreeze(getFrontend().globals.globalTypes);
    TableType::Props instanceProps{
        {"ClassName", {getBuiltins()->anyType}},
    };

    TableType instanceTable{instanceProps, std::nullopt, getFrontend().globals.globalScope->level, Luau::TableState::Sealed};
    TypeId instanceType = getFrontend().globals.globalTypes.addType(instanceTable);
    TypeFun instanceTypeFun{{}, instanceType};

    getFrontend().globals.globalScope->exportedTypeBindings["Part"] = instanceTypeFun;

    LintResult result = lint(R"(
local game = ...
local _e01 = type(game) == "Part"
local _e02 = typeof(game) == "Bar"
local _e03 = typeof(game) == "vector"

local _o01 = type(game) == "number"
local _o02 = type(game) == "vector"
local _o03 = typeof(game) == "Part"
)");

    REQUIRE(3 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 2);
    CHECK_EQ(result.warnings[0].text, "Unknown type 'Part' (expected primitive type)");
    CHECK_EQ(result.warnings[1].location.begin.line, 3);
    CHECK_EQ(result.warnings[1].text, "Unknown type 'Bar'");
    CHECK_EQ(result.warnings[2].location.begin.line, 4);
    CHECK_EQ(result.warnings[2].text, "Unknown type 'vector' (expected primitive or userdata type)");
}

TEST_CASE_FIXTURE(Fixture, "ForRangeTable")
{
    LintResult result = lint(R"(
local t = {}

for i=#t,1 do
end

for i=#t,1,-1 do
end
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 3);
    CHECK_EQ(result.warnings[0].text, "For loop should iterate backwards; did you forget to specify -1 as step?");
}

TEST_CASE_FIXTURE(Fixture, "ForRangeBackwards")
{
    LintResult result = lint(R"(
for i=8,1 do
end

for i=8,1,-1 do
end
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 1);
    CHECK_EQ(result.warnings[0].text, "For loop should iterate backwards; did you forget to specify -1 as step?");
}

TEST_CASE_FIXTURE(Fixture, "ForRangeImprecise")
{
    LintResult result = lint(R"(
for i=1.3,7.5 do
end

for i=1.3,7.5,1 do
end
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 1);
    CHECK_EQ(result.warnings[0].text, "For loop ends at 7.3 instead of 7.5; did you forget to specify step?");
}

TEST_CASE_FIXTURE(Fixture, "ForRangeZero")
{
    LintResult result = lint(R"(
for i=0,#t do
end

for i=(0),#t do -- to silence
end

for i=#t,0 do
end
)");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 1);
    CHECK_EQ(result.warnings[0].text, "For loop starts at 0, but arrays start at 1");
    CHECK_EQ(result.warnings[1].location.begin.line, 7);
    CHECK_EQ(
        result.warnings[1].text,
        "For loop should iterate backwards; did you forget to specify -1 as step? Also consider changing 0 to 1 since arrays start at 1"
    );
}

TEST_CASE_FIXTURE(Fixture, "UnbalancedAssignment")
{
    LintResult result = lint(R"(
do
local _a,_b,_c = pcall()
end
do
local _a,_b,_c = pcall(), 5
end
do
local _a,_b,_c = pcall(), 5, 6
end
do
local _a,_b,_c = pcall(), 5, 6, 7
end
do
local _a,_b,_c = pcall(), nil
end
)");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 5);
    CHECK_EQ(result.warnings[0].text, "Assigning 2 values to 3 variables initializes extra variables with nil; add 'nil' to value list to silence");
    CHECK_EQ(result.warnings[1].location.begin.line, 11);
    CHECK_EQ(result.warnings[1].text, "Assigning 4 values to 3 variables leaves some values unused");
}

TEST_CASE_FIXTURE(Fixture, "ImplicitReturn")
{
    LintResult result = lint(R"(
--!nonstrict
function f1(a)
    if not a then
        return 5
    end
end

function f2(a)
    if not a then
        return
    end
end

function f3(a)
    if not a then
        return 5
    else
        return
    end
end

function f4(a)
    for i in pairs(a) do
        if i > 5 then
            return i
        end
    end

    print("element not found")
end

function f5(a)
    for i in pairs(a) do
        if i > 5 then
            return i
        end
    end

    error("element not found")
end

f6 = function(a)
    if a == 0 then
        return 42
    end
end

function f7(a)
    repeat
        return 10
    until a ~= nil
end

return f1,f2,f3,f4,f5,f6,f7
)");

    REQUIRE(3 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 5);
    CHECK_EQ(
        result.warnings[0].text,
        "Function 'f1' can implicitly return no values even though there's an explicit return at line 5; add explicit return to silence"
    );
    CHECK_EQ(result.warnings[1].location.begin.line, 29);
    CHECK_EQ(
        result.warnings[1].text,
        "Function 'f4' can implicitly return no values even though there's an explicit return at line 26; add explicit return to silence"
    );
    CHECK_EQ(result.warnings[2].location.begin.line, 45);
    CHECK_EQ(
        result.warnings[2].text,
        "Function can implicitly return no values even though there's an explicit return at line 45; add explicit return to silence"
    );
}

TEST_CASE_FIXTURE(Fixture, "ImplicitReturnInfiniteLoop")
{
    LintResult result = lint(R"(
--!nonstrict
function f1(a)
    while true do
        if math.random() > 0.5 then
            return 5
        end
    end
end

function f2(a)
    repeat
        if math.random() > 0.5 then
            return 5
        end
    until false
end

function f3(a)
    while true do
        if math.random() > 0.5 then
            return 5
        end
        if math.random() < 0.1 then
            break
        end
    end
end

function f4(a)
    repeat
        if math.random() > 0.5 then
            return 5
        end
        if math.random() < 0.1 then
            break
        end
    until false
end

return f1,f2,f3,f4
)");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line, 26);
    CHECK_EQ(
        result.warnings[0].text,
        "Function 'f3' can implicitly return no values even though there's an explicit return at line 22; add explicit return to silence"
    );
    CHECK_EQ(result.warnings[1].location.begin.line, 37);
    CHECK_EQ(
        result.warnings[1].text,
        "Function 'f4' can implicitly return no values even though there's an explicit return at line 33; add explicit return to silence"
    );
}

TEST_CASE_FIXTURE(Fixture, "TypeAnnotationsShouldNotProduceWarnings")
{
    LintResult result = lint(R"(--!strict
type InputData = {
    id: number,
    inputType: EnumItem,
    inputState: EnumItem,
    updated: number,
    position: Vector3,
    keyCode: EnumItem,
    name: string
}
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "BreakFromInfiniteLoopMakesStatementReachable")
{
    LintResult result = lint(R"(
local bar = ...

repeat
    if bar then
        break
    end

    return 2
until true

return 1
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "IgnoreLintAll")
{
    LintResult result = lint(R"(
--!nolint
return foo
)");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "IgnoreLintSpecific")
{
    LintResult result = lint(R"(
--!nolint UnknownGlobal
local x = 1
return foo
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Variable 'x' is never used; prefix with '_' to silence");
}

TEST_CASE_FIXTURE(Fixture, "FormatStringFormat")
{
    LintResult result = lint(R"(
-- incorrect format strings
string.format("%")
string.format("%??d")
string.format("%Y")

-- incorrect format strings, self call
local _ = ("%"):format()

-- correct format strings, just to uh make sure
string.format("hello %+10d %.02f %%", 4, 5)
)");

    REQUIRE(4 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Invalid format string: unfinished format specifier");
    CHECK_EQ(result.warnings[1].text, "Invalid format string: invalid format specifier: must be a string format specifier or %");
    CHECK_EQ(result.warnings[2].text, "Invalid format string: invalid format specifier: must be a string format specifier or %");
    CHECK_EQ(result.warnings[3].text, "Invalid format string: unfinished format specifier");
}

TEST_CASE_FIXTURE(Fixture, "FormatStringPack")
{
    LintResult result = lint(R"(
-- incorrect pack specifiers
string.pack("?")
string.packsize("?")
string.unpack("?")

-- missing size
string.packsize("bc")

-- incorrect X alignment
string.packsize("X")
string.packsize("X i")

-- correct X alignment
string.packsize("Xi")

-- packsize can't be used with variable sized formats
string.packsize("s")

-- out of range size specifiers
string.packsize("i0")
string.packsize("i17")

-- a very very very out of range size specifier
string.packsize("i99999999999999999999")
string.packsize("c99999999999999999999")

-- correct format specifiers
string.packsize("=!1bbbI3c42")
)");

    REQUIRE(11 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Invalid pack format: unexpected character; must be a pack specifier or space");
    CHECK_EQ(result.warnings[1].text, "Invalid pack format: unexpected character; must be a pack specifier or space");
    CHECK_EQ(result.warnings[2].text, "Invalid pack format: unexpected character; must be a pack specifier or space");
    CHECK_EQ(result.warnings[3].text, "Invalid pack format: fixed-sized string format must specify the size");
    CHECK_EQ(result.warnings[4].text, "Invalid pack format: X must be followed by a size specifier");
    CHECK_EQ(result.warnings[5].text, "Invalid pack format: X must be followed by a size specifier");
    CHECK_EQ(result.warnings[6].text, "Invalid pack format: pack specifier must be fixed-size");
    CHECK_EQ(result.warnings[7].text, "Invalid pack format: integer size must be in range [1,16]");
    CHECK_EQ(result.warnings[8].text, "Invalid pack format: integer size must be in range [1,16]");
    CHECK_EQ(result.warnings[9].text, "Invalid pack format: size specifier is too large");
    CHECK_EQ(result.warnings[10].text, "Invalid pack format: size specifier is too large");
}

TEST_CASE_FIXTURE(Fixture, "FormatStringMatch")
{
    LintResult result = lint(R"(
local s = ...

-- incorrect character class specifiers
string.match(s, "%q")
string.gmatch(s, "%q")
string.find(s, "%q")
string.gsub(s, "%q", "")

-- various errors
string.match(s, "%")
string.match(s, "[%1]")
string.match(s, "%0")
string.match(s, "(%d)%2")
string.match(s, "%bx")
string.match(s, "%foo")
string.match(s, '(%d))')
string.match(s, '(%d')
string.match(s, '[%d')
string.match(s, '%,')

-- self call - not detected because we don't know the type!
local _ = s:match("%q")

-- correct patterns
string.match(s, "[A-Z]+(%d)%1")
)");

    REQUIRE(14 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Invalid match pattern: invalid character class, must refer to a defined class or its inverse");
    CHECK_EQ(result.warnings[1].text, "Invalid match pattern: invalid character class, must refer to a defined class or its inverse");
    CHECK_EQ(result.warnings[2].text, "Invalid match pattern: invalid character class, must refer to a defined class or its inverse");
    CHECK_EQ(result.warnings[3].text, "Invalid match pattern: invalid character class, must refer to a defined class or its inverse");
    CHECK_EQ(result.warnings[4].text, "Invalid match pattern: unfinished character class");
    CHECK_EQ(result.warnings[5].text, "Invalid match pattern: sets can not contain capture references");
    CHECK_EQ(result.warnings[6].text, "Invalid match pattern: invalid capture reference, must be 1-9");
    CHECK_EQ(result.warnings[7].text, "Invalid match pattern: invalid capture reference, must refer to a valid capture");
    CHECK_EQ(result.warnings[8].text, "Invalid match pattern: missing brace characters for balanced match");
    CHECK_EQ(result.warnings[9].text, "Invalid match pattern: missing set after a frontier pattern");
    CHECK_EQ(result.warnings[10].text, "Invalid match pattern: unexpected ) without a matching (");
    CHECK_EQ(result.warnings[11].text, "Invalid match pattern: expected ) at the end of the string to close a capture");
    CHECK_EQ(result.warnings[12].text, "Invalid match pattern: expected ] at the end of the string to close a set");
    CHECK_EQ(result.warnings[13].text, "Invalid match pattern: expected a magic character after %");
}

TEST_CASE_FIXTURE(Fixture, "FormatStringMatchNested")
{
    LintResult result = lint(R"~(
local s = ...

-- correct reference to nested pattern
string.match(s, "((a)%2)")

-- incorrect reference to nested pattern (not closed yet)
string.match(s, "((a)%1)")

-- incorrect reference to nested pattern (index out of range)
string.match(s, "((a)%3)")
)~");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Invalid match pattern: invalid capture reference, must refer to a closed capture");
    CHECK_EQ(result.warnings[0].location.begin.line, 7);
    CHECK_EQ(result.warnings[1].text, "Invalid match pattern: invalid capture reference, must refer to a valid capture");
    CHECK_EQ(result.warnings[1].location.begin.line, 10);
}

TEST_CASE_FIXTURE(Fixture, "FormatStringMatchSets")
{
    LintResult result = lint(R"~(
local s = ...

-- fake empty sets (but actually sets that aren't closed)
string.match(s, "[]")
string.match(s, "[^]")

-- character ranges in sets
string.match(s, "[%a-b]")
string.match(s, "[a-%b]")

-- invalid escapes
string.match(s, "[%q]")
string.match(s, "[%;]")

-- capture refs in sets
string.match(s, "[%1]")

-- valid escapes and - at the end
string.match(s, "[%]x-]")

-- % escapes itself
string.match(s, "[%%]")

-- this abomination is a valid pattern due to rules wrt handling empty sets
string.match(s, "[]|'[]")
string.match(s, "[^]|'[]")
)~");

    REQUIRE(7 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Invalid match pattern: expected ] at the end of the string to close a set");
    CHECK_EQ(result.warnings[1].text, "Invalid match pattern: expected ] at the end of the string to close a set");
    CHECK_EQ(result.warnings[2].text, "Invalid match pattern: character range can't include character sets");
    CHECK_EQ(result.warnings[3].text, "Invalid match pattern: character range can't include character sets");
    CHECK_EQ(result.warnings[4].text, "Invalid match pattern: invalid character class, must refer to a defined class or its inverse");
    CHECK_EQ(result.warnings[5].text, "Invalid match pattern: expected a magic character after %");
    CHECK_EQ(result.warnings[6].text, "Invalid match pattern: sets can not contain capture references");
}

TEST_CASE_FIXTURE(Fixture, "FormatStringFindArgs")
{
    LintResult result = lint(R"(
local s = ...

-- incorrect character class specifier
string.find(s, "%q")

-- raw string find
string.find(s, "%q", 1, true)
string.find(s, "%q", 1, math.random() < 0.5)

-- incorrect character class specifier
string.find(s, "%q", 1, false)

-- missing arguments
string.find()
string.find("foo");
("foo"):find()
)");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Invalid match pattern: invalid character class, must refer to a defined class or its inverse");
    CHECK_EQ(result.warnings[0].location.begin.line, 4);
    CHECK_EQ(result.warnings[1].text, "Invalid match pattern: invalid character class, must refer to a defined class or its inverse");
    CHECK_EQ(result.warnings[1].location.begin.line, 11);
}

TEST_CASE_FIXTURE(Fixture, "FormatStringReplace")
{
    LintResult result = lint(R"(
local s = ...

-- incorrect replacements
string.gsub(s, '(%d+)', "%")
string.gsub(s, '(%d+)', "%x")
string.gsub(s, '(%d+)', "%2")
string.gsub(s, '', "%1")

-- correct replacements
string.gsub(s, '[A-Z]+(%d)', "%0%1")
string.gsub(s, 'foo', "%0")
)");

    REQUIRE(4 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Invalid match replacement: unfinished replacement");
    CHECK_EQ(result.warnings[1].text, "Invalid match replacement: unexpected replacement character; must be a digit or %");
    CHECK_EQ(result.warnings[2].text, "Invalid match replacement: invalid capture index, must refer to pattern capture");
    CHECK_EQ(result.warnings[3].text, "Invalid match replacement: invalid capture index, must refer to pattern capture");
}

TEST_CASE_FIXTURE(Fixture, "FormatStringDate")
{
    LintResult result = lint(R"(
-- incorrect formats
os.date("%")
os.date("%L")
os.date("%?")
os.date("\0")

-- correct formats
os.date("it's %c now")
os.date("!*t")
)");

    REQUIRE(4 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Invalid date format: unfinished replacement");
    CHECK_EQ(result.warnings[1].text, "Invalid date format: unexpected replacement character; must be a date format specifier or %");
    CHECK_EQ(result.warnings[2].text, "Invalid date format: unexpected replacement character; must be a date format specifier or %");
    CHECK_EQ(result.warnings[3].text, "Invalid date format: date format can not contain null characters");
}

TEST_CASE_FIXTURE(Fixture, "FormatStringTyped")
{
    LintResult result = lint(R"~(
local s: string, nons = ...

string.match(s, "[]")
s:match("[]")

-- no warning here since we don't know that it's a string
nons:match("[]")
)~");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Invalid match pattern: expected ] at the end of the string to close a set");
    CHECK_EQ(result.warnings[0].location.begin.line, 3);
    CHECK_EQ(result.warnings[1].text, "Invalid match pattern: expected ] at the end of the string to close a set");
    CHECK_EQ(result.warnings[1].location.begin.line, 4);
}

TEST_CASE_FIXTURE(Fixture, "TableLiteral")
{
    LintResult result = lint(R"(-- line 1
_ = {
    first = 1,
    second = 2,
    first = 3,
}

_ = {
    first = 1,
    ["first"] = 2,
}

_ = {
    1, 2, 3,
    [1] = 42
}

_ = {
    [3] = 42,
    1, 2, 3,
}

local _: {
    first: number,
    second: string,
    first: boolean
}

_ = {
    1, 2, 3,
    [0] = 42,
    [4] = 42,
}

_ = {
    [1] = 1,
    [2] = 2,
    [1] = 3,
}

function _foo(): { first: number, second: string, first: boolean }
end
)");

    REQUIRE(7 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Table field 'first' is a duplicate; previously defined at line 3");
    CHECK_EQ(result.warnings[1].text, "Table field 'first' is a duplicate; previously defined at line 9");
    CHECK_EQ(result.warnings[2].text, "Table index 1 is a duplicate; previously defined as a list entry");
    CHECK_EQ(result.warnings[3].text, "Table index 3 is a duplicate; previously defined as a list entry");
    CHECK_EQ(result.warnings[4].text, "Table type field 'first' is a duplicate; previously defined at line 24");
    CHECK_EQ(result.warnings[5].text, "Table index 1 is a duplicate; previously defined at line 36");
    CHECK_EQ(result.warnings[6].text, "Table type field 'first' is a duplicate; previously defined at line 41");
}

TEST_CASE_FIXTURE(Fixture, "read_write_table_props")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    LintResult result = lint(R"(-- line 1
        type A = {x: number}
        type B = {read x: number, write x: number}
        type C = {x: number, read x: number} -- line 4
        type D = {x: number, write x: number}
        type E = {read x: number, x: boolean}
        type F = {read x: number, read x: number}
        type G = {write x: number, x: boolean}
        type H = {write x: number, write x: boolean}
    )");

    REQUIRE(6 == result.warnings.size());
    CHECK(result.warnings[0].text == "Table type field 'x' is already read-write; previously defined at line 4");
    CHECK(result.warnings[1].text == "Table type field 'x' is already read-write; previously defined at line 5");
    CHECK(result.warnings[2].text == "Table type field 'x' already has a read type defined at line 6");
    CHECK(result.warnings[3].text == "Table type field 'x' is a duplicate; previously defined at line 7");
    CHECK(result.warnings[4].text == "Table type field 'x' already has a write type defined at line 8");
    CHECK(result.warnings[5].text == "Table type field 'x' is a duplicate; previously defined at line 9");
}

TEST_CASE_FIXTURE(Fixture, "ImportOnlyUsedInTypeAnnotation")
{
    LintResult result = lint(R"(
        local Foo = require(script.Parent.Foo)

        local x: Foo.Y = 1
    )");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Variable 'x' is never used; prefix with '_' to silence");
}

TEST_CASE_FIXTURE(Fixture, "ImportOnlyUsedInReturnType")
{
    LintResult result = lint(R"(
        local Foo = require(script.Parent.Foo)

        function foo(): Foo.Y
        end
    )");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Function 'foo' is never used; prefix with '_' to silence");
}

TEST_CASE_FIXTURE(Fixture, "DisableUnknownGlobalWithTypeChecking")
{
    LintResult result = lint(R"(
        --!strict
        unknownGlobal()
    )");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "no_spurious_warning_after_a_function_type_alias")
{
    LintResult result = lint(R"(
        local exports = {}
        export type PathFunction<P> = (P?) -> string
        exports.tokensToFunction = function() end
        return exports
    )");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "use_all_parent_scopes_for_globals")
{
    ScopePtr testScope = getFrontend().addEnvironment("Test");
    unfreeze(getFrontend().globals.globalTypes);
    getFrontend().loadDefinitionFile(
        getFrontend().globals,
        testScope,
        R"(
        declare Foo: number
    )",
        "@test",
        /* captureComments */ false
    );
    freeze(getFrontend().globals.globalTypes);

    fileResolver.environments["A"] = "Test";

    fileResolver.source["A"] = R"(
        local _foo: Foo = 123
        -- os.clock comes from the global scope, the parent of this module's environment
        local _bar: typeof(os.clock) = os.clock
    )";

    LintResult result = lintModule("A");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "DeadLocalsUsed")
{
    LintResult result = lint(R"(
--!nolint LocalShadow
do
    local x
    for x in pairs({}) do
        print(x)
    end
    print(x) -- x is not initialized
end

do
    local a, b, c = 1, 2
    print(a, b, c) -- c is not initialized
end

do
    local a, b, c = table.unpack({})
    print(a, b, c) -- no warning as we don't know anything about c
end
    )");

    REQUIRE(3 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Variable 'x' defined at line 4 is never initialized or assigned; initialize with 'nil' to silence");
    CHECK_EQ(result.warnings[1].text, "Assigning 2 values to 3 variables initializes extra variables with nil; add 'nil' to value list to silence");
    CHECK_EQ(result.warnings[2].text, "Variable 'c' defined at line 12 is never initialized or assigned; initialize with 'nil' to silence");
}

TEST_CASE_FIXTURE(Fixture, "LocalFunctionNotDead")
{
    LintResult result = lint(R"(
local foo
function foo() end
    )");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "DuplicateGlobalFunction")
{
    LintResult result = lint(R"(
        function x() end

        function x() end

        return x
    )");

    REQUIRE_EQ(1, result.warnings.size());

    const auto& w = result.warnings[0];

    CHECK_EQ(LintWarning::Code_DuplicateFunction, w.code);
    CHECK_EQ("Duplicate function definition: 'x' also defined on line 2", w.text);
}

TEST_CASE_FIXTURE(Fixture, "DuplicateLocalFunction")
{
    LintOptions options;
    options.setDefaults();
    options.enableWarning(LintWarning::Code_DuplicateFunction);
    options.enableWarning(LintWarning::Code_LocalShadow);

    LintResult result = lint(
        R"(
        local function x() end

        print(x)

        local function x() end

        return x
    )",
        options
    );

    REQUIRE_EQ(1, result.warnings.size());

    CHECK_EQ(LintWarning::Code_DuplicateFunction, result.warnings[0].code);
}

TEST_CASE_FIXTURE(Fixture, "DuplicateMethod")
{
    LintResult result = lint(R"(
        local T = {}
        function T:x() end

        function T:x() end

        return x
    )");

    REQUIRE_EQ(1, result.warnings.size());

    const auto& w = result.warnings[0];

    CHECK_EQ(LintWarning::Code_DuplicateFunction, w.code);
    CHECK_EQ("Duplicate function definition: 'T.x' also defined on line 3", w.text);
}

TEST_CASE_FIXTURE(Fixture, "DontTriggerTheWarningIfTheFunctionsAreInDifferentScopes")
{
    LintResult result = lint(R"(
        if true then
            function c() end
        else
            function c() end
        end

        return c
    )");

    REQUIRE(0 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "LintHygieneUAF")
{
    LintResult result = lint(R"(
        local Hooty = require(workspace.A)

        local  HoHooty = require(workspace.A)

        local h: Hooty.Pointy = ruire(workspace.A)

        local h: H
        local h: Hooty.Pointy = ruire(workspace.A)

        local hh: Hooty.Pointy = ruire(workspace.A)

        local h: Hooty.Pointy = ruire(workspace.A)

        linooty.Pointy = ruire(workspace.A)

        local hh: Hooty.Pointy = ruire(workspace.A)

        local h: Hooty.Pointy = ruire(workspace.A)

        linty = ruire(workspace.A)

        local h: Hooty.Pointy = ruire(workspace.A)

        local hh: Hooty.Pointy = ruire(workspace.A)

        local h: Hooty.Pointy = ruire(workspace.A)

        local h: Hooty.Pt
    )");

    REQUIRE(12 == result.warnings.size());
}

TEST_CASE_FIXTURE(BuiltinsFixture, "DeprecatedApiTyped")
{
    unfreeze(getFrontend().globals.globalTypes);
    TypeId instanceType = getFrontend().globals.globalTypes.addType(ExternType{"Instance", {}, std::nullopt, std::nullopt, {}, {}, "Test", {}});
    persist(instanceType);
    getFrontend().globals.globalScope->exportedTypeBindings["Instance"] = TypeFun{{}, instanceType};

    getMutable<ExternType>(instanceType)->props = {
        {"Name", {getBuiltins()->stringType}},
        {"DataCost", {getBuiltins()->numberType, /* deprecated= */ true}},
        {"Wait", {getBuiltins()->anyType, /* deprecated= */ true}},
    };

    TypeId colorType =
        getFrontend().globals.globalTypes.addType(TableType{{}, std::nullopt, getFrontend().globals.globalScope->level, Luau::TableState::Sealed});

    getMutable<TableType>(colorType)->props = {{"toHSV", {getBuiltins()->anyType, /* deprecated= */ true, "Color3:ToHSV"}}};

    addGlobalBinding(getFrontend().globals, "Color3", Binding{colorType, {}});

    if (TableType* ttv = getMutable<TableType>(getGlobalBinding(getFrontend().globals, "table")))
    {
        ttv->props["foreach"].deprecated = true;
        ttv->props["getn"].deprecated = true;
        ttv->props["getn"].deprecatedSuggestion = "#";
    }

    freeze(getFrontend().globals.globalTypes);

    LintResult result = lint(R"(
return function (i: Instance)
    i:Wait(1.0)
    print(i.Name)
    print(Color3.toHSV())
    print(Color3.doesntexist, i.doesntexist) -- type error, but this verifies we correctly handle non-existent members
    print(table.getn({}))
    table.foreach({}, function() end)
    print(table.nogetn()) -- verify that we correctly handle non-existent members
    return i.DataCost
end
)");

    REQUIRE(5 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Member 'Instance.Wait' is deprecated");
    CHECK_EQ(result.warnings[1].text, "Member 'toHSV' is deprecated, use 'Color3:ToHSV' instead");
    CHECK_EQ(result.warnings[2].text, "Member 'table.getn' is deprecated, use '#' instead");
    CHECK_EQ(result.warnings[3].text, "Member 'table.foreach' is deprecated");
    CHECK_EQ(result.warnings[4].text, "Member 'Instance.DataCost' is deprecated");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "DeprecatedApiUntyped")
{
    if (TableType* ttv = getMutable<TableType>(getGlobalBinding(getFrontend().globals, "table")))
    {
        ttv->props["foreach"].deprecated = true;
        ttv->props["getn"].deprecated = true;
        ttv->props["getn"].deprecatedSuggestion = "#";
    }

    LintResult result = lint(R"(
-- TODO
return function ()
    print(table.getn({}))
    table.foreach({}, function() end)
    print(table.nogetn()) -- verify that we correctly handle non-existent members
end
)");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Member 'table.getn' is deprecated, use '#' instead");
    CHECK_EQ(result.warnings[1].text, "Member 'table.foreach' is deprecated");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "DeprecatedApiFenv")
{
    LintResult result = lint(R"(
local f, g, h = ...

getfenv(1)
getfenv(f :: () -> ())
getfenv(g :: number)
getfenv(h :: any)

setfenv(1, {})
setfenv(f :: () -> (), {})
setfenv(g :: number, {})
setfenv(h :: any, {})
)");

    REQUIRE(4 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Function 'getfenv' is deprecated; consider using 'debug.info' instead");
    CHECK_EQ(result.warnings[0].location.begin.line + 1, 4);
    CHECK_EQ(result.warnings[1].text, "Function 'getfenv' is deprecated; consider using 'debug.info' instead");
    CHECK_EQ(result.warnings[1].location.begin.line + 1, 6);
    CHECK_EQ(result.warnings[2].text, "Function 'setfenv' is deprecated");
    CHECK_EQ(result.warnings[2].location.begin.line + 1, 9);
    CHECK_EQ(result.warnings[3].text, "Function 'setfenv' is deprecated");
    CHECK_EQ(result.warnings[3].location.begin.line + 1, 11);
}

static void checkDeprecatedWarning(const Luau::LintWarning& warning, const Luau::Position& begin, const Luau::Position& end, const char* msg)
{
    CHECK_EQ(warning.code, LintWarning::Code_DeprecatedApi);
    CHECK_EQ(warning.location, Location(begin, end));
    CHECK_EQ(warning.text, msg);
}

TEST_CASE_FIXTURE(Fixture, "DeprecatedAttribute")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    // @deprecated works on local functions
    {
        LintResult result = lint(R"(
@deprecated
local function testfun(x)
    return x + 1
end

testfun(1)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(6, 0), Position(6, 7), "Function 'testfun' is deprecated");
    }

    // @deprecated works on globals functions
    {
        LintResult result = lint(R"(
@deprecated
function testfun(x)
    return x + 1
end

testfun(1)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(6, 0), Position(6, 7), "Function 'testfun' is deprecated");
    }

    // @deprecated works on fully typed functions
    {
        LintResult result = lint(R"(
@deprecated
local function testfun(x:number):number
    return x + 1
end

if math.random(2) == 2 then
    testfun(1)
end
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(7, 4), Position(7, 11), "Function 'testfun' is deprecated");
    }

    // @deprecated works on functions without an explicit return type
    {
        LintResult result = lint(R"(
@deprecated
local function testfun(x:number)
    return x + 1
end

g(testfun)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(6, 2), Position(6, 9), "Function 'testfun' is deprecated");
    }

    // @deprecated works on functions without an explicit argument type
    {
        LintResult result = lint(R"(
@deprecated
local function testfun(x):number
    if x == 1 then
        return x
    else
        return 1 + testfun(x - 1)
    end
end

testfun(1)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(10, 0), Position(10, 7), "Function 'testfun' is deprecated");
    }

    // @deprecated works on inner functions
    {
        LintResult result = lint(R"(
function flipFlop()
    local state = false

    @deprecated
    local function invert()
        state = !state
        return state
    end

    return invert
end

f = flipFlop()
assert(f() == true)
)");

        REQUIRE(2 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(10, 11), Position(10, 17), "Function 'invert' is deprecated");
        checkDeprecatedWarning(result.warnings[1], Position(14, 7), Position(14, 8), "Function 'f' is deprecated");
    }

    // @deprecated does not automatically apply to inner functions
    {
        LintResult result = lint(R"(
@deprecated
function flipFlop()
    local state = false

    local function invert()
        state = !state
        return state
    end

    return invert
end

f = flipFlop()
assert(f() == true)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(13, 4), Position(13, 12), "Function 'flipFlop' is deprecated");
    }

    // @deprecated works correctly if deprecated function is shadowed
    {
        LintResult result = lint(R"(
@deprecated
local function doTheThing()
    print("doing")
end

doTheThing()

local function shadow()
    local function doTheThing()
        print("doing!")
    end

    doTheThing()
end

shadow()
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(6, 0), Position(6, 10), "Function 'doTheThing' is deprecated");
    }

    // @deprecated does not issue warnings if a deprecated function uses itself
    {
        LintResult result = lint(R"(
@deprecated
function fibonacci(n)
    if n == 0 then
        return 0
    elseif n == 1 then
        return 1
    else
        return fibonacci(n - 1) + fibonacci(n - 2)
    end
end

fibonacci(5)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(12, 0), Position(12, 9), "Function 'fibonacci' is deprecated");
    }

    // @deprecated works for mutually recursive functions
    {
        LintResult result = lint(R"(
@deprecated
function odd(x)
    if x == 0 then
        return false
    else
        return even(x - 1)
    end
end

@deprecated
function even(x)
    if x == 0 then
        return true
    else
        return odd(x - 1)
    end
end

assert(odd(1) == true)
assert(even(0) == true)
)");

        REQUIRE(4 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(6, 15), Position(6, 19), "Function 'even' is deprecated");
        checkDeprecatedWarning(result.warnings[1], Position(15, 15), Position(15, 18), "Function 'odd' is deprecated");
        checkDeprecatedWarning(result.warnings[2], Position(19, 7), Position(19, 10), "Function 'odd' is deprecated");
        checkDeprecatedWarning(result.warnings[3], Position(20, 7), Position(20, 11), "Function 'even' is deprecated");
    }

    // @deprecated works for methods with a literal class name
    {
        LintResult result = lint(R"(
Account = { balance=0 }

@deprecated
function Account:deposit(v)
    self.balance = self.balance + v
end

Account:deposit(200.00)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(8, 0), Position(8, 15), "Member 'Account.deposit' is deprecated");
    }

    // @deprecated works for methods with a compound expression class name
    {
        LintResult result = lint(R"(
Account = { balance=0 }

function getAccount()
    return Account
end

@deprecated
function Account:deposit (v)
    self.balance = self.balance + v
end

(getAccount()):deposit(200.00)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(12, 0), Position(12, 22), "Member 'deposit' is deprecated");
    }
}

TEST_CASE_FIXTURE(Fixture, "DeprecatedAttributeWithParams")
{
    ScopedFastFlag sff{FFlag::LuauParametrizedAttributeSyntax, true};

    // @deprecated works on local functions
    {
        LintResult result = lint(R"(
@[deprecated{ use = "prodfun", reason = "Too old." }]
local function testfun(x)
    return x + 1
end

testfun(1)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(
            result.warnings[0], Position(6, 0), Position(6, 7), "Function 'testfun' is deprecated, use 'prodfun' instead. Too old."
        );
    }

    // @deprecated works on globals functions
    {
        LintResult result = lint(R"(
@[deprecated{ use = "prodfun", reason = "Too old." }]
function testfun(x)
    return x + 1
end

testfun(1)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(
            result.warnings[0], Position(6, 0), Position(6, 7), "Function 'testfun' is deprecated, use 'prodfun' instead. Too old."
        );
    }

    // @deprecated with only 'use' works on local functions
    {
        LintResult result = lint(R"(
@[deprecated{ use = "prodfun" }]
local function testfun(x)
    return x + 1
end

testfun(1)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(6, 0), Position(6, 7), "Function 'testfun' is deprecated, use 'prodfun' instead");
    }

    // @deprecated with only 'use' works on globals functions
    {
        LintResult result = lint(R"(
@[deprecated{ use = "prodfun" }]
function testfun(x)
    return x + 1
end

testfun(1)
)");
        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(6, 0), Position(6, 7), "Function 'testfun' is deprecated, use 'prodfun' instead");
    }


    // @deprecated with only 'reason' works on local functions
    {
        LintResult result = lint(R"(
@[deprecated{ reason = "Too old." }]
local function testfun(x)
    return x + 1
end

testfun(1)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(6, 0), Position(6, 7), "Function 'testfun' is deprecated. Too old.");
    }

    // @deprecated with only 'reason' works on globals functions
    {
        LintResult result = lint(R"(
@[deprecated{ reason = "Too old." }]
function testfun(x)
    return x + 1
end

testfun(1)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(6, 0), Position(6, 7), "Function 'testfun' is deprecated. Too old.");
    }

    // @deprecated works for methods with a literal class name
    {
        LintResult result = lint(R"(
Account = { balance=0 }

@[deprecated{use = 'credit', reason = 'It sounds cool'}]
function Account:deposit(v)
    self.balance = self.balance + v
end

Account:deposit(200.00)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(
            result.warnings[0], Position(8, 0), Position(8, 15), "Member 'Account.deposit' is deprecated, use 'credit' instead. It sounds cool"
        );
    }

    // @deprecated works for methods with a compound expression class name
    {
        LintResult result = lint(R"(
Account = { balance=0 }

function getAccount()
    return Account
end

@[deprecated{use = 'credit', reason = 'It sounds cool'}]
function Account:deposit (v)
    self.balance = self.balance + v
end

(getAccount()):deposit(200.00)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(
            result.warnings[0], Position(12, 0), Position(12, 22), "Member 'deposit' is deprecated, use 'credit' instead. It sounds cool"
        );
    }

    {
        loadDefinition(R"(
@[deprecated{use = 'foo', reason = 'Do better.'}] declare function bar(x: number): string
)");

        LintResult result = lint(R"(
bar(2)
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(1, 0), Position(1, 3), "Function 'bar' is deprecated, use 'foo' instead. Do better.");
    }

    {
        loadDefinition(R"(
declare Hooty : {
    tooty : @[deprecated{use = 'foo', reason = 'bar'}] @checked (number) -> number
}
)");
        LintResult result = lint(R"(
print(Hooty:tooty(2.0))
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(1, 6), Position(1, 17), "Member 'Hooty.tooty' is deprecated, use 'foo' instead. bar");
    }

    {
        loadDefinition(R"(
declare class Foo
   @[deprecated{use = 'foo', reason = 'baz'}]
   function bar(self, value: number) : number
end

declare Foo: {
   new: () -> Foo
}
)");

        LintResult result = lint(R"(
local foo = Foo.new()
print(foo:bar(2.0))
)");

        REQUIRE(1 == result.warnings.size());
        checkDeprecatedWarning(result.warnings[0], Position(2, 6), Position(2, 13), "Member 'bar' is deprecated, use 'foo' instead. baz");
    }
}

TEST_CASE_FIXTURE(Fixture, "DeprecatedAttributeFunctionDeclaration")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    // @deprecated works on function type declarations

    loadDefinition(R"(
@deprecated declare function bar(x: number): string
)");

    LintResult result = lint(R"(
bar(2)
)");

    REQUIRE(1 == result.warnings.size());
    checkDeprecatedWarning(result.warnings[0], Position(1, 0), Position(1, 3), "Function 'bar' is deprecated");
}

TEST_CASE_FIXTURE(Fixture, "DeprecatedAttributeTableDeclaration")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    // @deprecated works on table type declarations

    loadDefinition(R"(
declare Hooty : {
    tooty : @deprecated @checked (number) -> number
}
)");

    LintResult result = lint(R"(
print(Hooty:tooty(2.0))
)");

    REQUIRE(1 == result.warnings.size());
    checkDeprecatedWarning(result.warnings[0], Position(1, 6), Position(1, 17), "Member 'Hooty.tooty' is deprecated");
}

TEST_CASE_FIXTURE(Fixture, "DeprecatedAttributeMethodDeclaration")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    // @deprecated works on table type declarations

    loadDefinition(R"(
declare class Foo
   @deprecated
   function bar(self, value: number) : number
end

declare Foo: {
   new: () -> Foo
}
)");

    LintResult result = lint(R"(
local foo = Foo.new()
print(foo:bar(2.0))
)");

    REQUIRE(1 == result.warnings.size());
    checkDeprecatedWarning(result.warnings[0], Position(2, 6), Position(2, 13), "Member 'bar' is deprecated");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "TableOperations")
{
    LintResult result = lint(R"(
local t = {}
local tt = {}

table.insert(t, #t, 42)
table.insert(t, (#t), 42) -- silenced

table.insert(t, #t + 1, 42)
table.insert(t, #tt + 1, 42) -- different table, ok

table.insert(t, 0, 42)

table.remove(t, 0)

table.remove(t, #t-1)

table.insert(t, string.find("hello", "h"))

table.move(t, 0, #t, 1, tt)
table.move(t, 1, #t, 0, tt)

table.create(42, {})
table.create(42, {} :: {})
)");

    REQUIRE(10 == result.warnings.size());
    CHECK_EQ(
        result.warnings[0].text,
        "table.insert will insert the value before the last element, which is likely a bug; consider removing the "
        "second argument or wrap it in parentheses to silence"
    );
    CHECK_EQ(result.warnings[1].text, "table.insert will append the value to the table; consider removing the second argument for efficiency");
    CHECK_EQ(result.warnings[2].text, "table.insert uses index 0 but arrays are 1-based; did you mean 1 instead?");
    CHECK_EQ(result.warnings[3].text, "table.remove uses index 0 but arrays are 1-based; did you mean 1 instead?");
    CHECK_EQ(
        result.warnings[4].text,
        "table.remove will remove the value before the last element, which is likely a bug; consider removing the "
        "second argument or wrap it in parentheses to silence"
    );
    CHECK_EQ(
        result.warnings[5].text,
        "table.insert may change behavior if the call returns more than one result; consider adding parentheses around second argument"
    );
    CHECK_EQ(result.warnings[6].text, "table.move uses index 0 but arrays are 1-based; did you mean 1 instead?");
    CHECK_EQ(result.warnings[7].text, "table.move uses index 0 but arrays are 1-based; did you mean 1 instead?");
    CHECK_EQ(
        result.warnings[8].text, "table.create with a table literal will reuse the same object for all elements; consider using a for loop instead"
    );
    CHECK_EQ(
        result.warnings[9].text, "table.create with a table literal will reuse the same object for all elements; consider using a for loop instead"
    );
}

TEST_CASE_FIXTURE(BuiltinsFixture, "TableOperationsIndexer")
{
    // CLI-116824 Linter incorrectly issues false positive when taking the length of a unannotated string function argument
    if (FFlag::LuauSolverV2)
        return;

    LintResult result = lint(R"(
local t1 = {} -- ok: empty
local t2 = {1, 2} -- ok: array
local t3 = { a = 1, b = 2 } -- not ok: dictionary
local t4: {[number]: number} = {} -- ok: array
local t5: {[string]: number} = {} -- not ok: dictionary
local t6: typeof(setmetatable({1, 2}, {})) = {} -- ok: table with metatable
local t7: string = "hello" -- ok: string
local t8: {number} | {n: number} = {} -- ok: union

-- not ok
print(#t3)
print(#t5)
ipairs(t5)

-- disabled
-- ipairs(t3) adds indexer to t3, silencing error on #t3

-- ok
print(#t1)
print(#t2)
print(#t4)
print(#t6)
print(#t7)
print(#t8)

ipairs(t1)
ipairs(t2)
ipairs(t4)
ipairs(t6)
ipairs(t7)
ipairs(t8)

-- ok, subtle: text is a string here implicitly, but the type annotation isn't available
-- type checker assigns a type of generic table with the 'sub' member; we don't emit warnings on generic tables
-- to avoid generating a false positive here
function _impliedstring(element, text)
        for i = 1, #text do
                element:sendText(text:sub(i, i))
        end
end
)");

    REQUIRE(3 == result.warnings.size());
    CHECK_EQ(result.warnings[0].location.begin.line + 1, 12);
    CHECK_EQ(result.warnings[0].text, "Using '#' on a table without an array part is likely a bug");
    CHECK_EQ(result.warnings[1].location.begin.line + 1, 13);
    CHECK_EQ(result.warnings[1].text, "Using '#' on a table with string keys is likely a bug");
    CHECK_EQ(result.warnings[2].location.begin.line + 1, 14);
    CHECK_EQ(result.warnings[2].text, "Using 'ipairs' on a table with string keys is likely a bug");
}

TEST_CASE_FIXTURE(Fixture, "DuplicateConditions")
{
    LintResult result = lint(R"(
if true then
elseif false then
elseif true then -- duplicate
end

if true then
elseif false then
else
    if true then -- duplicate
    end
end

_ = true and true
_ = true or true
_ = (true and false) and true
_ = (true and true) and true
_ = (true and true) or true
_ = (true and false) and (42 and false)

_ = true and true or false -- no warning since this is is a common pattern used as a ternary replacement

_ = if true then 1 elseif true then 2 else 3
)");

    REQUIRE(8 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Condition has already been checked on line 2");
    CHECK_EQ(result.warnings[0].location.begin.line + 1, 4);
    CHECK_EQ(result.warnings[1].text, "Condition has already been checked on column 5");
    CHECK_EQ(result.warnings[2].text, "Condition has already been checked on column 5");
    CHECK_EQ(result.warnings[3].text, "Condition has already been checked on column 6");
    CHECK_EQ(result.warnings[4].text, "Condition has already been checked on column 6");
    CHECK_EQ(result.warnings[5].text, "Condition has already been checked on column 6");
    CHECK_EQ(result.warnings[6].text, "Condition has already been checked on column 15");
    CHECK_EQ(result.warnings[6].location.begin.line + 1, 19);
    CHECK_EQ(result.warnings[7].text, "Condition has already been checked on column 8");
}

TEST_CASE_FIXTURE(Fixture, "DuplicateConditionsExpr")
{
    LintResult result = lint(R"(
local correct, opaque = ...

if correct({a = 1, b = 2 * (-2), c = opaque.path['with']("calls", `string {opaque}`)}) then
elseif correct({a = 1, b = 2 * (-2), c = opaque.path['with']("calls", `string {opaque}`)}) then
elseif correct({a = 1, b = 2 * (-2), c = opaque.path['with']("calls", false)}) then
end
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Condition has already been checked on line 4");
    CHECK_EQ(result.warnings[0].location.begin.line + 1, 5);
}

TEST_CASE_FIXTURE(Fixture, "DuplicateLocal")
{
    LintResult result = lint(R"(
function foo(a1, a2, a3, a1)
end

local _, _, _ = ... -- ok!
local a1, a2, a1 = ... -- not ok

local moo = {}
function moo:bar(self)
end

return foo, moo, a1, a2
)");

    REQUIRE(4 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Function parameter 'a1' already defined on column 14");
    CHECK_EQ(result.warnings[1].text, "Variable 'a1' is never used; prefix with '_' to silence");
    CHECK_EQ(result.warnings[2].text, "Variable 'a1' already defined on column 7");
    CHECK_EQ(result.warnings[3].text, "Function parameter 'self' already defined implicitly");
}

TEST_CASE_FIXTURE(Fixture, "MisleadingAndOr")
{
    LintResult result = lint(R"(
_ = math.random() < 0.5 and true or 42
_ = math.random() < 0.5 and false or 42 -- misleading
_ = math.random() < 0.5 and nil or 42 -- misleading
_ = math.random() < 0.5 and 0 or 42
_ = (math.random() < 0.5 and false) or 42 -- currently ignored
)");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(
        result.warnings[0].text,
        "The and-or expression always evaluates to the second alternative because the first alternative is false; "
        "consider using if-then-else expression instead"
    );
    CHECK_EQ(
        result.warnings[1].text,
        "The and-or expression always evaluates to the second alternative because the first alternative is nil; "
        "consider using if-then-else expression instead"
    );
}

TEST_CASE_FIXTURE(Fixture, "WrongComment")
{
    LintResult result = lint(R"(
--!strict
--!struct
--!nolintGlobal
--!nolint Global
--!nolint KnownGlobal
--!nolint UnknownGlobal
--! no more lint
--!strict here
--!native on
do end
--!nolint
)");

    REQUIRE(7 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Unknown comment directive 'struct'; did you mean 'strict'?");
    CHECK_EQ(result.warnings[1].text, "Unknown comment directive 'nolintGlobal'");
    CHECK_EQ(result.warnings[2].text, "nolint directive refers to unknown lint rule 'Global'");
    CHECK_EQ(result.warnings[3].text, "nolint directive refers to unknown lint rule 'KnownGlobal'; did you mean 'UnknownGlobal'?");
    CHECK_EQ(result.warnings[4].text, "Comment directive with the type checking mode has extra symbols at the end of the line");
    CHECK_EQ(result.warnings[5].text, "native directive has extra symbols at the end of the line");
    CHECK_EQ(result.warnings[6].text, "Comment directive is ignored because it is placed after the first non-comment token");
}

TEST_CASE_FIXTURE(Fixture, "WrongCommentMuteSelf")
{
    LintResult result = lint(R"(
--!nolint
--!struct
)");

    REQUIRE(0 == result.warnings.size()); // --!nolint disables WrongComment lint :)
}

TEST_CASE_FIXTURE(Fixture, "DuplicateConditionsIfStatAndExpr")
{
    LintResult result = lint(R"(
if if 1 then 2 else 3 then
elseif if 1 then 2 else 3 then
elseif if 0 then 5 else 4 then
end
)");

    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Condition has already been checked on line 2");
}

TEST_CASE_FIXTURE(Fixture, "WrongCommentOptimize")
{
    LintResult result = lint(R"(
--!optimize
--!optimize me
--!optimize 100500
--!optimize 2
)");

    REQUIRE(3 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "optimize directive requires an optimization level");
    CHECK_EQ(result.warnings[1].text, "optimize directive uses unknown optimization level 'me', 0..2 expected");
    CHECK_EQ(result.warnings[2].text, "optimize directive uses unknown optimization level '100500', 0..2 expected");

    result = lint("--!optimize   ");
    REQUIRE(1 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "optimize directive requires an optimization level");
}

TEST_CASE_FIXTURE(Fixture, "TestStringInterpolation")
{
    LintResult result = lint(R"(
        --!nocheck
        local _ = `unknown {foo}`
    )");

    REQUIRE(1 == result.warnings.size());
}

TEST_CASE_FIXTURE(Fixture, "IntegerParsing")
{
    LintResult result = lint(R"(
local _ = 0b10000000000000000000000000000000000000000000000000000000000000000
local _ = 0x10000000000000000
)");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Binary number literal exceeded available precision and was truncated to 2^64");
    CHECK_EQ(result.warnings[1].text, "Hexadecimal number literal exceeded available precision and was truncated to 2^64");
}

TEST_CASE_FIXTURE(Fixture, "IntegerParsingDecimalImprecise")
{
    LintResult result = lint(R"(
local _ = 10000000000000000000000000000000000000000000000000000000000000000
local _ = 10000000000000001
local _ = -10000000000000001

-- 10^16 = 2^16 * 5^16, 5^16 only requires 38 bits
local _ = 10000000000000000
local _ = -10000000000000000

-- smallest possible number that is parsed imprecisely
local _ = 9007199254740993
local _ = -9007199254740993

-- note that numbers before and after parse precisely (number after is even => 1 more mantissa bit)
local _ = 9007199254740992
local _ = 9007199254740994

-- large powers of two should work as well (this is 2^63)
local _ = -9223372036854775808
)");

    REQUIRE(5 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Number literal exceeded available precision and was truncated to closest representable number");
    CHECK_EQ(result.warnings[0].location.begin.line, 1);
    CHECK_EQ(result.warnings[1].text, "Number literal exceeded available precision and was truncated to closest representable number");
    CHECK_EQ(result.warnings[1].location.begin.line, 2);
    CHECK_EQ(result.warnings[2].text, "Number literal exceeded available precision and was truncated to closest representable number");
    CHECK_EQ(result.warnings[2].location.begin.line, 3);
    CHECK_EQ(result.warnings[3].text, "Number literal exceeded available precision and was truncated to closest representable number");
    CHECK_EQ(result.warnings[3].location.begin.line, 10);
    CHECK_EQ(result.warnings[4].text, "Number literal exceeded available precision and was truncated to closest representable number");
    CHECK_EQ(result.warnings[4].location.begin.line, 11);
}

TEST_CASE_FIXTURE(Fixture, "IntegerParsingHexImprecise")
{
    LintResult result = lint(R"(
local _ = 0x1234567812345678

-- smallest possible number that is parsed imprecisely
local _ = 0x20000000000001

-- note that numbers before and after parse precisely (number after is even => 1 more mantissa bit)
local _ = 0x20000000000000
local _ = 0x20000000000002

-- large powers of two should work as well (this is 2^63)
local _ = 0x80000000000000
)");

    REQUIRE(2 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "Number literal exceeded available precision and was truncated to closest representable number");
    CHECK_EQ(result.warnings[0].location.begin.line, 1);
    CHECK_EQ(result.warnings[1].text, "Number literal exceeded available precision and was truncated to closest representable number");
    CHECK_EQ(result.warnings[1].location.begin.line, 4);
}

TEST_CASE_FIXTURE(Fixture, "ComparisonPrecedence")
{
    LintResult result = lint(R"(
local a, b = ...

local _ = not a == b
local _ = not a ~= b
local _ = not a <= b
local _ = a <= b == 0
local _ = a <= b <= 0

local _ = not a == not b -- weird but ok

-- silence tests for all of the above
local _ = not (a == b)
local _ = (not a) == b
local _ = not (a ~= b)
local _ = (not a) ~= b
local _ = not (a <= b)
local _ = (not a) <= b
local _ = (a <= b) == 0
local _ = a <= (b == 0)
)");

    REQUIRE(5 == result.warnings.size());
    CHECK_EQ(result.warnings[0].text, "not X == Y is equivalent to (not X) == Y; consider using X ~= Y, or add parentheses to silence");
    CHECK_EQ(result.warnings[1].text, "not X ~= Y is equivalent to (not X) ~= Y; consider using X == Y, or add parentheses to silence");
    CHECK_EQ(result.warnings[2].text, "not X <= Y is equivalent to (not X) <= Y; add parentheses to silence");
    CHECK_EQ(result.warnings[3].text, "X <= Y == Z is equivalent to (X <= Y) == Z; add parentheses to silence");
    CHECK_EQ(result.warnings[4].text, "X <= Y <= Z is equivalent to (X <= Y) <= Z; did you mean X <= Y and Y <= Z?");
}

TEST_CASE_FIXTURE(Fixture, "RedundantNativeAttribute")
{
    LintResult result = lint(R"(
--!native

@native
local function f(a)
    @native
    local function g(b)
        return (a + b)
    end
    return g
end

f(3)(4)
)");

    REQUIRE(2 == result.warnings.size());

    CHECK_EQ(result.warnings[0].text, "native attribute on a function is redundant in a native module; consider removing it");
    CHECK_EQ(result.warnings[0].location, Location(Position(3, 0), Position(3, 7)));

    CHECK_EQ(result.warnings[1].text, "native attribute on a function is redundant in a native module; consider removing it");
    CHECK_EQ(result.warnings[1].location, Location(Position(5, 4), Position(5, 11)));
}

TEST_SUITE_END();
