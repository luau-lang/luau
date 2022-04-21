// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Linter.h"
#include "Luau/BuiltinDefinitions.h"

#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("Linter");

TEST_CASE_FIXTURE(Fixture, "CleanCode")
{
    LintResult result = lint(R"(
function fib(n)
    return n < 2 and 1 or fib(n-1) + fib(n-2)
end

return math.max(fib(5), 1)
)");

    CHECK_EQ(result.warnings.size(), 0);
}

TEST_CASE_FIXTURE(Fixture, "UnknownGlobal")
{
    LintResult result = lint("--!nocheck\nreturn foo");

    REQUIRE_EQ(result.warnings.size(), 1);
    CHECK_EQ(result.warnings[0].text, "Unknown global 'foo'");
}

TEST_CASE_FIXTURE(Fixture, "DeprecatedGlobal")
{
    // Normally this would be defined externally, so hack it in for testing
    addGlobalBinding(typeChecker, "Wait", Binding{typeChecker.anyType, {}, true, "wait", "@test/global/Wait"});

    LintResult result = lintTyped("Wait(5)");

    REQUIRE_EQ(result.warnings.size(), 1);
    CHECK_EQ(result.warnings[0].text, "Global 'Wait' is deprecated, use 'wait' instead");
}

TEST_CASE_FIXTURE(Fixture, "PlaceholderRead")
{
    LintResult result = lint(R"(
local _ = 5
return _
)");

    CHECK_EQ(result.warnings.size(), 1);
    CHECK_EQ(result.warnings[0].text, "Placeholder value '_' is read here; consider using a named variable");
}

TEST_CASE_FIXTURE(Fixture, "PlaceholderReadGlobal")
{
    LintResult result = lint(R"(
_ = 5
print(_)
)");

    CHECK_EQ(result.warnings.size(), 1);
    CHECK_EQ(result.warnings[0].text, "Placeholder value '_' is read here; consider using a named variable");
}

TEST_CASE_FIXTURE(Fixture, "PlaceholderWrite")
{
    LintResult result = lint(R"(
local _ = 5
_ = 6
)");

    CHECK_EQ(result.warnings.size(), 0);
}

TEST_CASE_FIXTURE(Fixture, "BuiltinGlobalWrite")
{
    LintResult result = lint(R"(
math = {}

function assert(x)
end

assert(5)
)");

    CHECK_EQ(result.warnings.size(), 2);
    CHECK_EQ(result.warnings[0].text, "Built-in global 'math' is overwritten here; consider using a local or changing the name");
    CHECK_EQ(result.warnings[1].text, "Built-in global 'assert' is overwritten here; consider using a local or changing the name");
}

TEST_CASE_FIXTURE(Fixture, "MultilineBlock")
{
    LintResult result = lint(R"(
if true then print(1) print(2) print(3) end
)");

    CHECK_EQ(result.warnings.size(), 1);
    CHECK_EQ(result.warnings[0].text, "A new statement is on the same line; add semi-colon on previous statement to silence");
}

TEST_CASE_FIXTURE(Fixture, "MultilineBlockSemicolonsWhitelisted")
{
    LintResult result = lint(R"(
print(1); print(2); print(3)
)");

    CHECK(result.warnings.empty());
}

TEST_CASE_FIXTURE(Fixture, "MultilineBlockMissedSemicolon")
{
    LintResult result = lint(R"(
print(1); print(2) print(3)
)");

    CHECK_EQ(result.warnings.size(), 1);
    CHECK_EQ(result.warnings[0].text, "A new statement is on the same line; add semi-colon on previous statement to silence");
}

TEST_CASE_FIXTURE(Fixture, "MultilineBlockLocalDo")
{
    LintResult result = lint(R"(
local _x do
    _x = 5
end
)");

    CHECK_EQ(result.warnings.size(), 0);
}

TEST_CASE_FIXTURE(Fixture, "ConfusingIndentation")
{
    LintResult result = lint(R"(
print(math.max(1,
2))
)");

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 1);
    CHECK_EQ(result.warnings[0].text, "Global 'foo' is only used in the enclosing function 'bar'; consider changing it to local");
}

TEST_CASE_FIXTURE(Fixture, "GlobalAsLocalMultiFx")
{
    ScopedFastFlag sff{"LuauLintGlobalNeverReadBeforeWritten", true};
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

    REQUIRE_EQ(result.warnings.size(), 1);
    CHECK_EQ(result.warnings[0].text, "Global 'foo' is never read before being written. Consider changing it to local");
}

TEST_CASE_FIXTURE(Fixture, "GlobalAsLocalMultiFxWithRead")
{
    ScopedFastFlag sff{"LuauLintGlobalNeverReadBeforeWritten", true};
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

    CHECK_EQ(result.warnings.size(), 0);
}

TEST_CASE_FIXTURE(Fixture, "GlobalAsLocalWithConditional")
{
    ScopedFastFlag sff{"LuauLintGlobalNeverReadBeforeWritten", true};
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

    CHECK_EQ(result.warnings.size(), 0);
}

TEST_CASE_FIXTURE(Fixture, "GlobalAsLocal3WithConditionalRead")
{
    ScopedFastFlag sff{"LuauLintGlobalNeverReadBeforeWritten", true};
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

    CHECK_EQ(result.warnings.size(), 0);
}

TEST_CASE_FIXTURE(Fixture, "GlobalAsLocalInnerRead")
{
    ScopedFastFlag sff{"LuauLintGlobalNeverReadBeforeWritten", true};
    LintResult result = lint(R"(
function foo()
   local f = function() return bar end
   f()
   bar = 42
end

function baz() bar = 0 end

return foo() + baz()
)");

    CHECK_EQ(result.warnings.size(), 0);
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

    CHECK_EQ(result.warnings.size(), 1);
    CHECK_EQ(result.warnings[0].text,
        "Global 'moreInternalLogic' is only used in the enclosing function defined at line 2; consider changing it to local");
}

TEST_CASE_FIXTURE(Fixture, "LocalShadowLocal")
{
    LintResult result = lint(R"(
local arg = 6
print(arg)

local arg = 5
print(arg)
)");

    CHECK_EQ(result.warnings.size(), 1);
    CHECK_EQ(result.warnings[0].text, "Variable 'arg' shadows previous declaration at line 2");
}

TEST_CASE_FIXTURE(Fixture, "LocalShadowGlobal")
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

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 2);
    CHECK_EQ(result.warnings[0].text, "Variable 'arg' is never used; prefix with '_' to silence");
    CHECK_EQ(result.warnings[1].text, "Variable 'blarg' is never used; prefix with '_' to silence");
}

TEST_CASE_FIXTURE(Fixture, "ImportUnused")
{
    // Normally this would be defined externally, so hack it in for testing
    addGlobalBinding(typeChecker, "game", typeChecker.anyType, "@test");

    LintResult result = lint(R"(
local Roact = require(game.Packages.Roact)
local _Roact = require(game.Packages.Roact)
)");

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 2);
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

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 0);
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

    CHECK_EQ(result.warnings.size(), 0);
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

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 0);
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

    CHECK_EQ(result.warnings.size(),
        0); // this is technically a bug, since the repeat body always returns; fixing this bug is a bit more involved than I'd like
}

TEST_CASE_FIXTURE(Fixture, "UnknownType")
{
    unfreeze(typeChecker.globalTypes);
    TableTypeVar::Props instanceProps{
        {"ClassName", {typeChecker.anyType}},
    };

    TableTypeVar instanceTable{instanceProps, std::nullopt, typeChecker.globalScope->level, Luau::TableState::Sealed};
    TypeId instanceType = typeChecker.globalTypes.addType(instanceTable);
    TypeFun instanceTypeFun{{}, instanceType};

    typeChecker.globalScope->exportedTypeBindings["Part"] = instanceTypeFun;

    LintResult result = lint(R"(
local game = ...
local _e01 = type(game) == "Part"
local _e02 = typeof(game) == "Bar"
local _e03 = typeof(game) == "vector"

local _o01 = type(game) == "number"
local _o02 = type(game) == "vector"
local _o03 = typeof(game) == "Part"
)");

    REQUIRE_EQ(result.warnings.size(), 3);
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

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 2);
    CHECK_EQ(result.warnings[0].location.begin.line, 1);
    CHECK_EQ(result.warnings[0].text, "For loop starts at 0, but arrays start at 1");
    CHECK_EQ(result.warnings[1].location.begin.line, 7);
    CHECK_EQ(result.warnings[1].text,
        "For loop should iterate backwards; did you forget to specify -1 as step? Also consider changing 0 to 1 since arrays start at 1");
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

    CHECK_EQ(result.warnings.size(), 2);
    CHECK_EQ(result.warnings[0].location.begin.line, 5);
    CHECK_EQ(result.warnings[0].text, "Assigning 2 values to 3 variables initializes extra variables with nil; add 'nil' to value list to silence");
    CHECK_EQ(result.warnings[1].location.begin.line, 11);
    CHECK_EQ(result.warnings[1].text, "Assigning 4 values to 3 variables leaves some values unused");
}

TEST_CASE_FIXTURE(Fixture, "ImplicitReturn")
{
    LintResult result = lint(R"(
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

    CHECK_EQ(result.warnings.size(), 3);
    CHECK_EQ(result.warnings[0].location.begin.line, 4);
    CHECK_EQ(result.warnings[0].text,
        "Function 'f1' can implicitly return no values even though there's an explicit return at line 4; add explicit return to silence");
    CHECK_EQ(result.warnings[1].location.begin.line, 28);
    CHECK_EQ(result.warnings[1].text,
        "Function 'f4' can implicitly return no values even though there's an explicit return at line 25; add explicit return to silence");
    CHECK_EQ(result.warnings[2].location.begin.line, 44);
    CHECK_EQ(result.warnings[2].text,
        "Function can implicitly return no values even though there's an explicit return at line 44; add explicit return to silence");
}

TEST_CASE_FIXTURE(Fixture, "ImplicitReturnInfiniteLoop")
{
    LintResult result = lint(R"(
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

    CHECK_EQ(result.warnings.size(), 2);
    CHECK_EQ(result.warnings[0].location.begin.line, 25);
    CHECK_EQ(result.warnings[0].text,
        "Function 'f3' can implicitly return no values even though there's an explicit return at line 21; add explicit return to silence");
    CHECK_EQ(result.warnings[1].location.begin.line, 36);
    CHECK_EQ(result.warnings[1].text,
        "Function 'f4' can implicitly return no values even though there's an explicit return at line 32; add explicit return to silence");
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

    CHECK_EQ(result.warnings.size(), 0);
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

    CHECK_EQ(result.warnings.size(), 0);
}

TEST_CASE_FIXTURE(Fixture, "IgnoreLintAll")
{
    LintResult result = lint(R"(
--!nolint
return foo
)");

    CHECK_EQ(result.warnings.size(), 0);
}

TEST_CASE_FIXTURE(Fixture, "IgnoreLintSpecific")
{
    LintResult result = lint(R"(
--!nolint UnknownGlobal
local x = 1
return foo
)");

    CHECK_EQ(result.warnings.size(), 1);
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

    CHECK_EQ(result.warnings.size(), 4);
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

    CHECK_EQ(result.warnings.size(), 11);
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

    CHECK_EQ(result.warnings.size(), 14);
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

    CHECK_EQ(result.warnings.size(), 2);
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

    CHECK_EQ(result.warnings.size(), 7);
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

    CHECK_EQ(result.warnings.size(), 2);
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

    CHECK_EQ(result.warnings.size(), 4);
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

    CHECK_EQ(result.warnings.size(), 4);
    CHECK_EQ(result.warnings[0].text, "Invalid date format: unfinished replacement");
    CHECK_EQ(result.warnings[1].text, "Invalid date format: unexpected replacement character; must be a date format specifier or %");
    CHECK_EQ(result.warnings[2].text, "Invalid date format: unexpected replacement character; must be a date format specifier or %");
    CHECK_EQ(result.warnings[3].text, "Invalid date format: date format can not contain null characters");
}

TEST_CASE_FIXTURE(Fixture, "FormatStringTyped")
{
    LintResult result = lintTyped(R"~(
local s: string, nons = ...

string.match(s, "[]")
s:match("[]")

-- no warning here since we don't know that it's a string
nons:match("[]")
)~");

    CHECK_EQ(result.warnings.size(), 2);
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
)");

    CHECK_EQ(result.warnings.size(), 6);
    CHECK_EQ(result.warnings[0].text, "Table field 'first' is a duplicate; previously defined at line 3");
    CHECK_EQ(result.warnings[1].text, "Table field 'first' is a duplicate; previously defined at line 9");
    CHECK_EQ(result.warnings[2].text, "Table index 1 is a duplicate; previously defined as a list entry");
    CHECK_EQ(result.warnings[3].text, "Table index 3 is a duplicate; previously defined as a list entry");
    CHECK_EQ(result.warnings[4].text, "Table type field 'first' is a duplicate; previously defined at line 24");
    CHECK_EQ(result.warnings[5].text, "Table index 1 is a duplicate; previously defined at line 36");
}

TEST_CASE_FIXTURE(Fixture, "ImportOnlyUsedInTypeAnnotation")
{
    LintResult result = lint(R"(
        local Foo = require(script.Parent.Foo)

        local x: Foo.Y = 1
    )");

    REQUIRE_EQ(result.warnings.size(), 1);
    CHECK_EQ(result.warnings[0].text, "Variable 'x' is never used; prefix with '_' to silence");
}

TEST_CASE_FIXTURE(Fixture, "DisableUnknownGlobalWithTypeChecking")
{
    LintResult result = lint(R"(
        --!strict
        unknownGlobal()
    )");

    REQUIRE_EQ(result.warnings.size(), 0);
}

TEST_CASE_FIXTURE(Fixture, "no_spurious_warning_after_a_function_type_alias")
{
    LintResult result = lint(R"(
        local exports = {}
        export type PathFunction<P> = (P?) -> string
        exports.tokensToFunction = function() end
        return exports
    )");

    CHECK_EQ(result.warnings.size(), 0);
}

TEST_CASE_FIXTURE(Fixture, "use_all_parent_scopes_for_globals")
{
    ScopePtr testScope = frontend.addEnvironment("Test");
    unfreeze(typeChecker.globalTypes);
    loadDefinitionFile(frontend.typeChecker, testScope, R"(
        declare Foo: number
    )",
        "@test");
    freeze(typeChecker.globalTypes);

    fileResolver.environments["A"] = "Test";

    fileResolver.source["A"] = R"(
        local _foo: Foo = 123
        -- os.clock comes from the global scope, the parent of this module's environment
        local _bar: typeof(os.clock) = os.clock
    )";

    LintResult result = frontend.lint("A");

    CHECK_EQ(result.warnings.size(), 0);
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

    CHECK_EQ(result.warnings.size(), 3);
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

    CHECK_EQ(result.warnings.size(), 0);
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

    LintResult result = lint(R"(
        local function x() end

        print(x)

        local function x() end

        return x
    )",
        options);

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

    CHECK(result.warnings.empty());
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

    CHECK_EQ(result.warnings.size(), 12);
}

TEST_CASE_FIXTURE(Fixture, "DeprecatedApi")
{
    unfreeze(typeChecker.globalTypes);
    TypeId instanceType = typeChecker.globalTypes.addType(ClassTypeVar{"Instance", {}, std::nullopt, std::nullopt, {}, {}, "Test"});
    persist(instanceType);
    typeChecker.globalScope->exportedTypeBindings["Instance"] = TypeFun{{}, instanceType};

    getMutable<ClassTypeVar>(instanceType)->props = {
        {"Name", {typeChecker.stringType}},
        {"DataCost", {typeChecker.numberType, /* deprecated= */ true}},
        {"Wait", {typeChecker.anyType, /* deprecated= */ true}},
    };

    TypeId colorType = typeChecker.globalTypes.addType(TableTypeVar{{}, std::nullopt, typeChecker.globalScope->level, Luau::TableState::Sealed});

    getMutable<TableTypeVar>(colorType)->props = {{"toHSV", {typeChecker.anyType, /* deprecated= */ true, "Color3:ToHSV"}}};

    addGlobalBinding(typeChecker, "Color3", Binding{colorType, {}});

    freeze(typeChecker.globalTypes);

    LintResult result = lintTyped(R"(
return function (i: Instance)
    i:Wait(1.0)
    print(i.Name)
    print(Color3.toHSV())
    print(Color3.doesntexist, i.doesntexist) -- type error, but this verifies we correctly handle non-existent members
    return i.DataCost
end
)");

    REQUIRE_EQ(result.warnings.size(), 3);
    CHECK_EQ(result.warnings[0].text, "Member 'Instance.Wait' is deprecated");
    CHECK_EQ(result.warnings[1].text, "Member 'toHSV' is deprecated, use 'Color3:ToHSV' instead");
    CHECK_EQ(result.warnings[2].text, "Member 'Instance.DataCost' is deprecated");
}

TEST_CASE_FIXTURE(Fixture, "TableOperations")
{
    LintResult result = lintTyped(R"(
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

    REQUIRE_EQ(result.warnings.size(), 10);
    CHECK_EQ(result.warnings[0].text, "table.insert will insert the value before the last element, which is likely a bug; consider removing the "
                                      "second argument or wrap it in parentheses to silence");
    CHECK_EQ(result.warnings[1].text, "table.insert will append the value to the table; consider removing the second argument for efficiency");
    CHECK_EQ(result.warnings[2].text, "table.insert uses index 0 but arrays are 1-based; did you mean 1 instead?");
    CHECK_EQ(result.warnings[3].text, "table.remove uses index 0 but arrays are 1-based; did you mean 1 instead?");
    CHECK_EQ(result.warnings[4].text, "table.remove will remove the value before the last element, which is likely a bug; consider removing the "
                                      "second argument or wrap it in parentheses to silence");
    CHECK_EQ(result.warnings[5].text,
        "table.insert may change behavior if the call returns more than one result; consider adding parentheses around second argument");
    CHECK_EQ(result.warnings[6].text, "table.move uses index 0 but arrays are 1-based; did you mean 1 instead?");
    CHECK_EQ(result.warnings[7].text, "table.move uses index 0 but arrays are 1-based; did you mean 1 instead?");
    CHECK_EQ(
        result.warnings[8].text, "table.create with a table literal will reuse the same object for all elements; consider using a for loop instead");
    CHECK_EQ(
        result.warnings[9].text, "table.create with a table literal will reuse the same object for all elements; consider using a for loop instead");
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

    REQUIRE_EQ(result.warnings.size(), 8);
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

if correct({a = 1, b = 2 * (-2), c = opaque.path['with']("calls")}) then
elseif correct({a = 1, b = 2 * (-2), c = opaque.path['with']("calls")}) then
elseif correct({a = 1, b = 2 * (-2), c = opaque.path['with']("calls", false)}) then
end
)");

    REQUIRE_EQ(result.warnings.size(), 1);
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

    REQUIRE_EQ(result.warnings.size(), 4);
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

    REQUIRE_EQ(result.warnings.size(), 2);
    CHECK_EQ(result.warnings[0].text, "The and-or expression always evaluates to the second alternative because the first alternative is false; "
                                      "consider using if-then-else expression instead");
    CHECK_EQ(result.warnings[1].text, "The and-or expression always evaluates to the second alternative because the first alternative is nil; "
                                      "consider using if-then-else expression instead");
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
do end
--!nolint
)");

    REQUIRE_EQ(result.warnings.size(), 6);
    CHECK_EQ(result.warnings[0].text, "Unknown comment directive 'struct'; did you mean 'strict'?");
    CHECK_EQ(result.warnings[1].text, "Unknown comment directive 'nolintGlobal'");
    CHECK_EQ(result.warnings[2].text, "nolint directive refers to unknown lint rule 'Global'");
    CHECK_EQ(result.warnings[3].text, "nolint directive refers to unknown lint rule 'KnownGlobal'; did you mean 'UnknownGlobal'?");
    CHECK_EQ(result.warnings[4].text, "Comment directive with the type checking mode has extra symbols at the end of the line");
    CHECK_EQ(result.warnings[5].text, "Comment directive is ignored because it is placed after the first non-comment token");
}

TEST_CASE_FIXTURE(Fixture, "WrongCommentMuteSelf")
{
    LintResult result = lint(R"(
--!nolint
--!struct
)");

    REQUIRE_EQ(result.warnings.size(), 0); // --!nolint disables WrongComment lint :)
}

TEST_CASE_FIXTURE(Fixture, "DuplicateConditionsIfStatAndExpr")
{
    LintResult result = lint(R"(
if if 1 then 2 else 3 then
elseif if 1 then 2 else 3 then
elseif if 0 then 5 else 4 then
end
)");

    REQUIRE_EQ(result.warnings.size(), 1);
    CHECK_EQ(result.warnings[0].text, "Condition has already been checked on line 2");
}

TEST_SUITE_END();
