// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Compiler.h"
#include "Luau/BytecodeBuilder.h"
#include "Luau/StringUtils.h"

#include "ScopedFlags.h"

#include "doctest.h"

#include <sstream>
#include <string_view>

LUAU_FASTFLAG(LuauPreloadClosures)
LUAU_FASTFLAG(LuauPreloadClosuresFenv)
LUAU_FASTFLAG(LuauPreloadClosuresUpval)

using namespace Luau;

static std::string compileFunction(const char* source, uint32_t id)
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::compileOrThrow(bcb, source);

    return bcb.dumpFunction(id);
}

static std::string compileFunction0(const char* source)
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::compileOrThrow(bcb, source);

    return bcb.dumpFunction(0);
}

static std::string compileFunction0Coverage(const char* source, int level)
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Lines);

    Luau::CompileOptions opts;
    opts.coverageLevel = level;
    Luau::compileOrThrow(bcb, source, opts);

    return bcb.dumpFunction(0);
}

TEST_SUITE_BEGIN("Compiler");

TEST_CASE("CompileToBytecode")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::compileOrThrow(bcb, "return 5, 6.5");

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
LOADN R0 5
LOADK R1 K0
RETURN R0 2
)");
}

TEST_CASE("LocalsDirectReference")
{
    CHECK_EQ("\n" + compileFunction0("local a return a"), R"(
LOADNIL R0
RETURN R0 1
)");
}

TEST_CASE("BasicFunction")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::compileOrThrow(bcb, "local function foo(a, b) return b end");

    if (FFlag::LuauPreloadClosures)
    {
        CHECK_EQ("\n" + bcb.dumpFunction(1), R"(
DUPCLOSURE R0 K0
RETURN R0 0
)");
    }
    else
    {
        CHECK_EQ("\n" + bcb.dumpFunction(1), R"(
NEWCLOSURE R0 P0
RETURN R0 0
)");
    }

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
RETURN R1 1
)");
}

TEST_CASE("BasicFunctionCall")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::compileOrThrow(bcb, "local function foo(a, b) return b end function test() return foo(2) end");

    CHECK_EQ("\n" + bcb.dumpFunction(1), R"(
GETUPVAL R0 0
LOADN R1 2
CALL R0 1 -1
RETURN R0 -1
)");
}

TEST_CASE("FunctionCallOptimization")
{
    // direct call into local
    CHECK_EQ("\n" + compileFunction0("local foo = math.foo()"), R"(
GETIMPORT R0 2
CALL R0 0 1
RETURN R0 0
)");

    // direct call into temp
    CHECK_EQ("\n" + compileFunction0("local foo = math.foo(math.bar())"), R"(
GETIMPORT R0 2
GETIMPORT R1 4
CALL R1 0 -1
CALL R0 -1 1
RETURN R0 0
)");

    // can't directly call into local since foo might be used as arguments of caller
    CHECK_EQ("\n" + compileFunction0("local foo foo = math.foo(foo)"), R"(
LOADNIL R0
GETIMPORT R1 2
MOVE R2 R0
CALL R1 1 1
MOVE R0 R1
RETURN R0 0
)");
}

TEST_CASE("ReflectionBytecode")
{
    CHECK_EQ("\n" + compileFunction0(R"(
local part = Instance.new('Part', workspace)
part.Size = Vector3.new(1, 2, 3)
return part.Size.Z * part:GetMass()
)"),
        R"(
GETIMPORT R0 2
LOADK R1 K3
GETIMPORT R2 5
CALL R0 2 1
GETIMPORT R1 7
LOADN R2 1
LOADN R3 2
LOADN R4 3
CALL R1 3 1
SETTABLEKS R1 R0 K8
GETTABLEKS R3 R0 K8
GETTABLEKS R2 R3 K9
NAMECALL R3 R0 K10
CALL R3 1 1
MUL R1 R2 R3
RETURN R1 1
)");
}

TEST_CASE("ImportCall")
{
    CHECK_EQ("\n" + compileFunction0("return math.max(1, 2)"), R"(
LOADN R1 1
FASTCALL2K 18 R1 K0 +4
LOADK R2 K0
GETIMPORT R0 3
CALL R0 2 -1
RETURN R0 -1
)");
}

TEST_CASE("FakeImportCall")
{
    const char* source = "math = {} function math.max() return 0 end function test() return math.max(1, 2) end";

    CHECK_EQ("\n" + compileFunction(source, 1), R"(
GETGLOBAL R1 K0
GETTABLEKS R0 R1 K1
LOADN R1 1
LOADN R2 2
CALL R0 2 -1
RETURN R0 -1
)");
}

TEST_CASE("AssignmentLocal")
{
    CHECK_EQ("\n" + compileFunction0("local a a = 2"), R"(
LOADNIL R0
LOADN R0 2
RETURN R0 0
)");
}

TEST_CASE("AssignmentGlobal")
{
    CHECK_EQ("\n" + compileFunction0("a = 2"), R"(
LOADN R0 2
SETGLOBAL R0 K0
RETURN R0 0
)");
}

TEST_CASE("AssignmentTable")
{
    const char* source = "local c = ... local a = {} a.b = 2 a.b = c";

    CHECK_EQ("\n" + compileFunction0(source), R"(
GETVARARGS R0 1
NEWTABLE R1 1 0
LOADN R2 2
SETTABLEKS R2 R1 K0
SETTABLEKS R0 R1 K0
RETURN R0 0
)");
}

TEST_CASE("ConcatChainOptimization")
{
    CHECK_EQ("\n" + compileFunction0("return '1' .. '2'"), R"(
LOADK R1 K0
LOADK R2 K1
CONCAT R0 R1 R2
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return '1' .. '2' .. '3'"), R"(
LOADK R1 K0
LOADK R2 K1
LOADK R3 K2
CONCAT R0 R1 R3
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return ('1' .. '2') .. '3'"), R"(
LOADK R3 K0
LOADK R4 K1
CONCAT R1 R3 R4
LOADK R2 K2
CONCAT R0 R1 R2
RETURN R0 1
)");
}

TEST_CASE("RepeatLocals")
{
    CHECK_EQ("\n" + compileFunction0("repeat local a a = 5 until a - 4 < 0 or a - 4 >= 0"), R"(
LOADNIL R0
LOADN R0 5
SUBK R1 R0 K0
LOADN R2 0
JUMPIFLT R1 R2 +6
SUBK R1 R0 K0
LOADN R2 0
JUMPIFLE R2 R1 +2
JUMPBACK -11
RETURN R0 0
)");
}

TEST_CASE("ForBytecode")
{
    // basic for loop: variable directly refers to internal iteration index (R2)
    CHECK_EQ("\n" + compileFunction0("for i=1,5 do print(i) end"), R"(
LOADN R2 1
LOADN R0 5
LOADN R1 1
FORNPREP R0 +5
GETIMPORT R3 1
MOVE R4 R2
CALL R3 1 0
FORNLOOP R0 -5
RETURN R0 0
)");

    // when you assign the variable internally, we freak out and copy the variable so that you aren't changing the loop behavior
    CHECK_EQ("\n" + compileFunction0("for i=1,5 do i = 7 print(i) end"), R"(
LOADN R2 1
LOADN R0 5
LOADN R1 1
FORNPREP R0 +7
MOVE R3 R2
LOADN R3 7
GETIMPORT R4 1
MOVE R5 R3
CALL R4 1 0
FORNLOOP R0 -7
RETURN R0 0
)");

    // basic for-in loop, generic version
    CHECK_EQ("\n" + compileFunction0("for word in string.gmatch(\"Hello Lua user\", \"%a+\") do print(word) end"), R"(
GETIMPORT R0 2
LOADK R1 K3
LOADK R2 K4
CALL R0 2 3
JUMP +4
GETIMPORT R5 6
MOVE R6 R3
CALL R5 1 0
FORGLOOP R0 -5 1
RETURN R0 0
)");

    // basic for-in loop, using inext specialization
    CHECK_EQ("\n" + compileFunction0("for k,v in ipairs({}) do print(k,v) end"), R"(
GETIMPORT R0 1
NEWTABLE R1 0 0
CALL R0 1 3
FORGPREP_INEXT R0 +5
GETIMPORT R5 3
MOVE R6 R3
MOVE R7 R4
CALL R5 2 0
FORGLOOP_INEXT R0 -6
RETURN R0 0
)");

    // basic for-in loop, using next specialization
    CHECK_EQ("\n" + compileFunction0("for k,v in pairs({}) do print(k,v) end"), R"(
GETIMPORT R0 1
NEWTABLE R1 0 0
CALL R0 1 3
FORGPREP_NEXT R0 +5
GETIMPORT R5 3
MOVE R6 R3
MOVE R7 R4
CALL R5 2 0
FORGLOOP_NEXT R0 -6
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("for k,v in next,{} do print(k,v) end"), R"(
GETIMPORT R0 1
NEWTABLE R1 0 0
LOADNIL R2
FORGPREP_NEXT R0 +5
GETIMPORT R5 3
MOVE R6 R3
MOVE R7 R4
CALL R5 2 0
FORGLOOP_NEXT R0 -6
RETURN R0 0
)");
}

TEST_CASE("ForBytecodeBuiltin")
{
    // we generally recognize builtins like pairs/ipairs and emit special opcodes
    CHECK_EQ("\n" + compileFunction0("for k,v in ipairs({}) do end"), R"(
GETIMPORT R0 1
NEWTABLE R1 0 0
CALL R0 1 3
FORGPREP_INEXT R0 +0
FORGLOOP_INEXT R0 -1
RETURN R0 0
)");

    // ... even if they are using a local variable
    CHECK_EQ("\n" + compileFunction0("local ip = ipairs for k,v in ip({}) do end"), R"(
GETIMPORT R0 1
MOVE R1 R0
NEWTABLE R2 0 0
CALL R1 1 3
FORGPREP_INEXT R1 +0
FORGLOOP_INEXT R1 -1
RETURN R0 0
)");

    // ... even when it's an upvalue
    CHECK_EQ("\n" + compileFunction0("local ip = ipairs function foo() for k,v in ip({}) do end end"), R"(
GETUPVAL R0 0
NEWTABLE R1 0 0
CALL R0 1 3
FORGPREP_INEXT R0 +0
FORGLOOP_INEXT R0 -1
RETURN R0 0
)");

    // but if it's reassigned then all bets are off
    CHECK_EQ("\n" + compileFunction0("local ip = ipairs ip = pairs for k,v in ip({}) do end"), R"(
GETIMPORT R0 1
GETIMPORT R0 3
MOVE R1 R0
NEWTABLE R2 0 0
CALL R1 1 3
JUMP +0
FORGLOOP R1 -1 2
RETURN R0 0
)");

    // or if the global is hijacked
    CHECK_EQ("\n" + compileFunction0("ipairs = pairs for k,v in ipairs({}) do end"), R"(
GETIMPORT R0 1
SETGLOBAL R0 K2
GETGLOBAL R0 K2
NEWTABLE R1 0 0
CALL R0 1 3
JUMP +0
FORGLOOP R0 -1 2
RETURN R0 0
)");

    // or if we don't even know the global to begin with
    CHECK_EQ("\n" + compileFunction0("for k,v in unknown({}) do end"), R"(
GETIMPORT R0 1
NEWTABLE R1 0 0
CALL R0 1 3
JUMP +0
FORGLOOP R0 -1 2
RETURN R0 0
)");
}

TEST_CASE("TableLiterals")
{
    // empty table, note it's computed directly to target
    CHECK_EQ("\n" + compileFunction0("return {}"), R"(
NEWTABLE R0 0 0
RETURN R0 1
)");

    // we can't compute directly to target since that'd overwrite the local
    CHECK_EQ("\n" + compileFunction0("local a a = {a} return a"), R"(
LOADNIL R0
NEWTABLE R1 0 1
MOVE R2 R0
SETLIST R1 R2 1 [1]
MOVE R0 R1
RETURN R0 1
)");

    // short list
    CHECK_EQ("\n" + compileFunction0("return {1,2,3}"), R"(
NEWTABLE R0 0 3
LOADN R1 1
LOADN R2 2
LOADN R3 3
SETLIST R0 R1 3 [1]
RETURN R0 1
)");

    // long list, split into two chunks
    CHECK_EQ("\n" + compileFunction0("return {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17}"), R"(
NEWTABLE R0 0 17
LOADN R1 1
LOADN R2 2
LOADN R3 3
LOADN R4 4
LOADN R5 5
LOADN R6 6
LOADN R7 7
LOADN R8 8
LOADN R9 9
LOADN R10 10
LOADN R11 11
LOADN R12 12
LOADN R13 13
LOADN R14 14
LOADN R15 15
LOADN R16 16
SETLIST R0 R1 16 [1]
LOADN R1 17
SETLIST R0 R1 1 [17]
RETURN R0 1
)");

    // varargs; -1 indicates multret treatment; note that we don't allocate space for the ...
    CHECK_EQ("\n" + compileFunction0("return {...}"), R"(
NEWTABLE R0 0 0
GETVARARGS R1 -1
SETLIST R0 R1 -1 [1]
RETURN R0 1
)");

    // varargs with other elements; -1 indicates multret treatment; note that we don't allocate space for the ...
    CHECK_EQ("\n" + compileFunction0("return {1,2,3,...}"), R"(
NEWTABLE R0 0 3
LOADN R1 1
LOADN R2 2
LOADN R3 3
GETVARARGS R4 -1
SETLIST R0 R1 -1 [1]
RETURN R0 1
)");

    // basic literals; note that we use DUPTABLE instead of NEWTABLE
    CHECK_EQ("\n" + compileFunction0("return {a=1,b=2,c=3}"), R"(
DUPTABLE R0 3
LOADN R1 1
SETTABLEKS R1 R0 K0
LOADN R1 2
SETTABLEKS R1 R0 K1
LOADN R1 3
SETTABLEKS R1 R0 K2
RETURN R0 1
)");

    // literals+array
    CHECK_EQ("\n" + compileFunction0("return {a=1,b=2,3,4}"), R"(
NEWTABLE R0 2 2
LOADN R3 1
SETTABLEKS R3 R0 K0
LOADN R3 2
SETTABLEKS R3 R0 K1
LOADN R1 3
LOADN R2 4
SETLIST R0 R1 2 [1]
RETURN R0 1
)");

    // expression assignment
    CHECK_EQ("\n" + compileFunction0("a = 7 return {[a]=42}"), R"(
LOADN R0 7
SETGLOBAL R0 K0
NEWTABLE R0 1 0
GETGLOBAL R1 K0
LOADN R2 42
SETTABLE R2 R0 R1
RETURN R0 1
)");

    // table template caching; two DUPTABLES out of three use the same slot. Note that caching is order dependent
    CHECK_EQ("\n" + compileFunction0("return {a=1,b=2},{b=3,a=4},{a=5,b=6}"), R"(
DUPTABLE R0 2
LOADN R1 1
SETTABLEKS R1 R0 K0
LOADN R1 2
SETTABLEKS R1 R0 K1
DUPTABLE R1 3
LOADN R2 3
SETTABLEKS R2 R1 K1
LOADN R2 4
SETTABLEKS R2 R1 K0
DUPTABLE R2 2
LOADN R3 5
SETTABLEKS R3 R2 K0
LOADN R3 6
SETTABLEKS R3 R2 K1
RETURN R0 3
)");
}

TEST_CASE("TableLiteralsNumberIndex")
{
    // tables with [x] compile to SETTABLEN if the index is short
    CHECK_EQ("\n" + compileFunction0("return {[2] = 2, [256] = 256, [0] = 0, [257] = 257}"), R"(
NEWTABLE R0 4 0
LOADN R1 2
SETTABLEN R1 R0 2
LOADN R1 256
SETTABLEN R1 R0 256
LOADN R1 0
LOADN R2 0
SETTABLE R2 R0 R1
LOADN R1 257
LOADN R2 257
SETTABLE R2 R0 R1
RETURN R0 1
)");

    // tables with [x] where x is sequential compile to correctly sized array + SETTABLEN
    CHECK_EQ("\n" + compileFunction0("return {[1] = 1, [2] = 2}"), R"(
NEWTABLE R0 0 2
LOADN R1 1
SETTABLEN R1 R0 1
LOADN R1 2
SETTABLEN R1 R0 2
RETURN R0 1
)");

    // when index chain starts with 0, or isn't sequential, we disable the optimization
    CHECK_EQ("\n" + compileFunction0("return {[0] = 0, [1] = 1, [2] = 2, [42] = 42}"), R"(
NEWTABLE R0 4 0
LOADN R1 0
LOADN R2 0
SETTABLE R2 R0 R1
LOADN R1 1
SETTABLEN R1 R0 1
LOADN R1 2
SETTABLEN R1 R0 2
LOADN R1 42
SETTABLEN R1 R0 42
RETURN R0 1
)");

    // we disable this optimization when the table has list elements for simplicity
    CHECK_EQ("\n" + compileFunction0("return {[1] = 1, [2] = 2, 3}"), R"(
NEWTABLE R0 2 1
LOADN R2 1
SETTABLEN R2 R0 1
LOADN R2 2
SETTABLEN R2 R0 2
LOADN R1 3
SETLIST R0 R1 1 [1]
RETURN R0 1
)");

    // we can also correctly predict the array length for mixed tables
    CHECK_EQ("\n" + compileFunction0("return {key = 1, value = 2, [1] = 42}"), R"(
NEWTABLE R0 2 1
LOADN R1 1
SETTABLEKS R1 R0 K0
LOADN R1 2
SETTABLEKS R1 R0 K1
LOADN R1 42
SETTABLEN R1 R0 1
RETURN R0 1
)");
}

TEST_CASE("EmptyTableHashSizePredictionOptimization")
{
    const char* hashSizeSource = R"(
local t = {}
t.a = 1
t.b = 1
t.c = 1
t.d = 1
t.e = 1
t.f = 1
t.g = 1
t.h = 1
t.i = 1
)";

    const char* hashSizeSource2 = R"(
local t = {}
t.x = 1
t.x = 2
t.x = 3
t.x = 4
t.x = 5
t.x = 6
t.x = 7
t.x = 8
t.x = 9
)";

    const char* arraySizeSource = R"(
local t = {}
t[1] = 1
t[2] = 1
t[3] = 1
t[4] = 1
t[5] = 1
t[6] = 1
t[7] = 1
t[8] = 1
t[9] = 1
t[10] = 1
)";

    CHECK_EQ("\n" + compileFunction0(hashSizeSource), R"(
NEWTABLE R0 16 0
LOADN R1 1
SETTABLEKS R1 R0 K0
LOADN R1 1
SETTABLEKS R1 R0 K1
LOADN R1 1
SETTABLEKS R1 R0 K2
LOADN R1 1
SETTABLEKS R1 R0 K3
LOADN R1 1
SETTABLEKS R1 R0 K4
LOADN R1 1
SETTABLEKS R1 R0 K5
LOADN R1 1
SETTABLEKS R1 R0 K6
LOADN R1 1
SETTABLEKS R1 R0 K7
LOADN R1 1
SETTABLEKS R1 R0 K8
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0(hashSizeSource2), R"(
NEWTABLE R0 1 0
LOADN R1 1
SETTABLEKS R1 R0 K0
LOADN R1 2
SETTABLEKS R1 R0 K0
LOADN R1 3
SETTABLEKS R1 R0 K0
LOADN R1 4
SETTABLEKS R1 R0 K0
LOADN R1 5
SETTABLEKS R1 R0 K0
LOADN R1 6
SETTABLEKS R1 R0 K0
LOADN R1 7
SETTABLEKS R1 R0 K0
LOADN R1 8
SETTABLEKS R1 R0 K0
LOADN R1 9
SETTABLEKS R1 R0 K0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0(arraySizeSource), R"(
NEWTABLE R0 0 10
LOADN R1 1
SETTABLEN R1 R0 1
LOADN R1 1
SETTABLEN R1 R0 2
LOADN R1 1
SETTABLEN R1 R0 3
LOADN R1 1
SETTABLEN R1 R0 4
LOADN R1 1
SETTABLEN R1 R0 5
LOADN R1 1
SETTABLEN R1 R0 6
LOADN R1 1
SETTABLEN R1 R0 7
LOADN R1 1
SETTABLEN R1 R0 8
LOADN R1 1
SETTABLEN R1 R0 9
LOADN R1 1
SETTABLEN R1 R0 10
RETURN R0 0
)");
}

TEST_CASE("TableSizePredictionSetMetatable")
{
    CHECK_EQ("\n" + compileFunction0(R"(
local t = setmetatable({}, nil)
t.field1 = 1
t.field2 = 2
return t
)"),
        R"(
GETIMPORT R0 1
NEWTABLE R1 2 0
LOADNIL R2
CALL R0 2 1
LOADN R1 1
SETTABLEKS R1 R0 K2
LOADN R1 2
SETTABLEKS R1 R0 K3
RETURN R0 1
)");
}

TEST_CASE("ReflectionEnums")
{
    CHECK_EQ("\n" + compileFunction0("return Enum.EasingStyle.Linear"), R"(
GETIMPORT R0 3
RETURN R0 1
)");
}

TEST_CASE("CaptureSelf")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::compileOrThrow(bcb, R"(
local MaterialsListClass = {}

function MaterialsListClass:_MakeToolTip(guiElement, text)
	local function updateTooltipPosition()
		self._tweakingTooltipFrame = 5
	end

	updateTooltipPosition()
end

return MaterialsListClass
)");

    CHECK_EQ("\n" + bcb.dumpFunction(1), R"(
NEWCLOSURE R3 P0
CAPTURE VAL R0
MOVE R4 R3
CALL R4 0 0
RETURN R0 0
)");

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
GETUPVAL R0 0
LOADN R1 5
SETTABLEKS R1 R0 K0
RETURN R0 0
)");
}

TEST_CASE("ConditionalBasic")
{
    CHECK_EQ("\n" + compileFunction0("local a = ... if a then return 5 end"), R"(
GETVARARGS R0 1
JUMPIFNOT R0 +2
LOADN R1 5
RETURN R1 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = ... if not a then return 5 end"), R"(
GETVARARGS R0 1
JUMPIF R0 +2
LOADN R1 5
RETURN R1 1
RETURN R0 0
)");
}

TEST_CASE("ConditionalCompare")
{
    CHECK_EQ("\n" + compileFunction0("local a, b = ... if a < b then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFNOTLT R0 R1 +3
LOADN R2 5
RETURN R2 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = ... if a <= b then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFNOTLE R0 R1 +3
LOADN R2 5
RETURN R2 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = ... if a > b then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFNOTLT R1 R0 +3
LOADN R2 5
RETURN R2 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = ... if a >= b then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFNOTLE R1 R0 +3
LOADN R2 5
RETURN R2 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = ... if a == b then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFNOTEQ R0 R1 +3
LOADN R2 5
RETURN R2 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = ... if a ~= b then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFEQ R0 R1 +3
LOADN R2 5
RETURN R2 1
RETURN R0 0
)");
}

TEST_CASE("ConditionalNot")
{
    CHECK_EQ("\n" + compileFunction0("local a, b = ... if not (not (a < b)) then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFNOTLT R0 R1 +3
LOADN R2 5
RETURN R2 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = ... if not (not (not (a < b))) then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFLT R0 R1 +3
LOADN R2 5
RETURN R2 1
RETURN R0 0
)");
}

TEST_CASE("ConditionalAndOr")
{
    CHECK_EQ("\n" + compileFunction0("local a, b, c = ... if a < b and b < c then return 5 end"), R"(
GETVARARGS R0 3
JUMPIFNOTLT R0 R1 +5
JUMPIFNOTLT R1 R2 +3
LOADN R3 5
RETURN R3 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b, c = ... if a < b or b < c then return 5 end"), R"(
GETVARARGS R0 3
JUMPIFLT R0 R1 +3
JUMPIFNOTLT R1 R2 +3
LOADN R3 5
RETURN R3 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a,b,c,d = ... if (a or b) and not (c and d) then return 5 end"), R"(
GETVARARGS R0 4
JUMPIF R0 +1
JUMPIFNOT R1 +4
JUMPIFNOT R2 +1
JUMPIF R3 +2
LOADN R4 5
RETURN R4 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a,b,c = ... if a or not b or c then return 5 end"), R"(
GETVARARGS R0 3
JUMPIF R0 +2
JUMPIFNOT R1 +1
JUMPIFNOT R2 +2
LOADN R3 5
RETURN R3 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a,b,c = ... if a and not b and c then return 5 end"), R"(
GETVARARGS R0 3
JUMPIFNOT R0 +4
JUMPIF R1 +3
JUMPIFNOT R2 +2
LOADN R3 5
RETURN R3 1
RETURN R0 0
)");
}

TEST_CASE("AndOr")
{
    // codegen for constant, local, global for and
    CHECK_EQ("\n" + compileFunction0("local a = 1 a = a and 2 return a"), R"(
LOADN R0 1
ANDK R0 R0 K0
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("local a = 1 local b = ... a = a and b return a"), R"(
LOADN R0 1
GETVARARGS R1 1
AND R0 R0 R1
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("local a = 1 b = 2 a = a and b return a"), R"(
LOADN R0 1
LOADN R1 2
SETGLOBAL R1 K0
MOVE R1 R0
JUMPIFNOT R1 +2
GETGLOBAL R1 K0
MOVE R0 R1
RETURN R0 1
)");

    // codegen for constant, local, global for or
    CHECK_EQ("\n" + compileFunction0("local a = 1 a = a or 2 return a"), R"(
LOADN R0 1
ORK R0 R0 K0
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("local a = 1 local b = ... a = a or b return a"), R"(
LOADN R0 1
GETVARARGS R1 1
OR R0 R0 R1
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("local a = 1 b = 2 a = a or b return a"), R"(
LOADN R0 1
LOADN R1 2
SETGLOBAL R1 K0
MOVE R1 R0
JUMPIF R1 +2
GETGLOBAL R1 K0
MOVE R0 R1
RETURN R0 1
)");

    // codegen without a temp variable for and/or when we know we can assign directly into the target register
    // note: `a = a` assignment is to disable constant folding for testing purposes
    CHECK_EQ("\n" + compileFunction0("local a = 1 a = a b = 2 local c = a and b return c"), R"(
LOADN R0 1
MOVE R0 R0
LOADN R1 2
SETGLOBAL R1 K0
MOVE R1 R0
JUMPIFNOT R1 +2
GETGLOBAL R1 K0
RETURN R1 1
)");

    CHECK_EQ("\n" + compileFunction0("local a = 1 a = a b = 2 local c = a or b return c"), R"(
LOADN R0 1
MOVE R0 R0
LOADN R1 2
SETGLOBAL R1 K0
MOVE R1 R0
JUMPIF R1 +2
GETGLOBAL R1 K0
RETURN R1 1
)");
}

TEST_CASE("AndOrChainCodegen")
{
    const char* source = R"(
    return
        (1 - verticalGradientTurbulence < waterLevel + .015 and Enum.Material.Sand)
        or (sandbank>0 and sandbank<1 and Enum.Material.Sand)--this for canyonbase sandbanks
        or Enum.Material.Sandstone
    )";

    CHECK_EQ("\n" + compileFunction0(source), R"(
LOADN R2 1
GETIMPORT R3 1
SUB R1 R2 R3
GETIMPORT R3 4
ADDK R2 R3 K2
JUMPIFNOTLT R1 R2 +4
GETIMPORT R0 8
JUMPIF R0 +15
GETIMPORT R1 10
LOADN R2 0
JUMPIFNOTLT R2 R1 +9
GETIMPORT R1 10
LOADN R2 1
JUMPIFNOTLT R1 R2 +4
GETIMPORT R0 8
JUMPIF R0 +2
GETIMPORT R0 12
RETURN R0 1
)");
}

TEST_CASE("IfElseExpression")
{
    ScopedFastFlag sff1{"LuauIfElseExpressionBaseSupport", true};
    ScopedFastFlag sff2{"LuauIfElseExpressionAnalysisSupport", true};

    // codegen for a true constant condition
    CHECK_EQ("\n" + compileFunction0("return if true then 10 else 20"), R"(
LOADN R0 10
RETURN R0 1
)");

    // codegen for a false constant condition
    CHECK_EQ("\n" + compileFunction0("return if false then 10 else 20"), R"(
LOADN R0 20
RETURN R0 1
)");

    // codegen for a false (in this case 'nil') constant condition
    CHECK_EQ("\n" + compileFunction0("return if nil then 10 else 20"), R"(
LOADN R0 20
RETURN R0 1
)");

    // codegen constant if-else expression used with a binary operation involving another constant
    // The test verifies that everything constant folds down to a single constant
    CHECK_EQ("\n" + compileFunction0("return 7 + if true then 10 else 20"), R"(
LOADN R0 17
RETURN R0 1
)");

    // codegen for a non-constant condition
    CHECK_EQ("\n" + compileFunction0("return if condition then 10 else 20"), R"(
GETIMPORT R1 1
JUMPIFNOT R1 +2
LOADN R0 10
RETURN R0 1
LOADN R0 20
RETURN R0 1
)");

    // codegen for a non-constant condition using an assignment
    CHECK_EQ("\n" + compileFunction0("result = if condition then 10 else 20"), R"(
GETIMPORT R1 1
JUMPIFNOT R1 +2
LOADN R0 10
JUMP +1
LOADN R0 20
SETGLOBAL R0 K2
RETURN R0 0
)");

    // codegen for a non-constant condition using an assignment to a local variable
    CHECK_EQ("\n" + compileFunction0("local result = if condition then 10 else 20"), R"(
GETIMPORT R1 1
JUMPIFNOT R1 +2
LOADN R0 10
RETURN R0 0
LOADN R0 20
RETURN R0 0
)");

    // codegen for an if-else expression with multiple elseif's
    CHECK_EQ("\n" + compileFunction0("result = if condition1 then 10 elseif condition2 then 20 elseif condition3 then 30 else 40"), R"(
GETIMPORT R1 1
JUMPIFNOT R1 +2
LOADN R0 10
JUMP +11
GETIMPORT R1 3
JUMPIFNOT R1 +2
LOADN R0 20
JUMP +6
GETIMPORT R1 5
JUMPIFNOT R1 +2
LOADN R0 30
JUMP +1
LOADN R0 40
SETGLOBAL R0 K6
RETURN R0 0
)");
}

TEST_CASE("ConstantFoldArith")
{
    CHECK_EQ("\n" + compileFunction0("return 10 + 2"), R"(
LOADN R0 12
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return 10 - 2"), R"(
LOADN R0 8
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return 10 * 2"), R"(
LOADN R0 20
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return 10 / 2"), R"(
LOADN R0 5
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return 10 % 2"), R"(
LOADN R0 0
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return 10 ^ 2"), R"(
LOADN R0 100
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return -(2 - 5)"), R"(
LOADN R0 3
RETURN R0 1
)");

    // nested arith expression with groups
    CHECK_EQ("\n" + compileFunction0("return (2 + 2) * 2"), R"(
LOADN R0 8
RETURN R0 1
)");
}

TEST_CASE("ConstantFoldCompare")
{
    // ordered comparisons
    CHECK_EQ("\n" + compileFunction0("return 1 < 1, 1 < 2"), R"(
LOADB R0 0
LOADB R1 1
RETURN R0 2
)");
    CHECK_EQ("\n" + compileFunction0("return 1 <= 1, 1 <= 2"), R"(
LOADB R0 1
LOADB R1 1
RETURN R0 2
)");

    CHECK_EQ("\n" + compileFunction0("return 1 > 1, 1 > 2"), R"(
LOADB R0 0
LOADB R1 0
RETURN R0 2
)");

    CHECK_EQ("\n" + compileFunction0("return 1 >= 1, 1 >= 2"), R"(
LOADB R0 1
LOADB R1 0
RETURN R0 2
)");

    // equality comparisons
    CHECK_EQ("\n" + compileFunction0("return nil == 1, nil ~= 1, nil == nil, nil ~= nil"), R"(
LOADB R0 0
LOADB R1 1
LOADB R2 1
LOADB R3 0
RETURN R0 4
)");

    CHECK_EQ("\n" + compileFunction0("return 2 == 1, 2 ~= 1, 1 == 1, 1 ~= 1"), R"(
LOADB R0 0
LOADB R1 1
LOADB R2 1
LOADB R3 0
RETURN R0 4
)");

    CHECK_EQ("\n" + compileFunction0("return true == false, true ~= false, true == true, true ~= true"), R"(
LOADB R0 0
LOADB R1 1
LOADB R2 1
LOADB R3 0
RETURN R0 4
)");

    CHECK_EQ("\n" + compileFunction0("return 'a' == 'b', 'a' ~= 'b', 'a' == 'a', 'a' ~= 'a'"), R"(
LOADB R0 0
LOADB R1 1
LOADB R2 1
LOADB R3 0
RETURN R0 4
)");
}

TEST_CASE("ConstantFoldLocal")
{
    // local constant propagation, including upvalues, and no propagation for mutated locals
    CHECK_EQ("\n" + compileFunction0("local a = 1 return a + a"), R"(
LOADN R0 2
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("local a = 1 a = a + a return a"), R"(
LOADN R0 1
ADD R0 R0 R0
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction("local a = 1 function foo() return a + a end", 0), R"(
LOADN R0 2
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction("local a = 1 function foo() return a + a end function bar() a = 5 end", 0), R"(
GETUPVAL R1 0
GETUPVAL R2 0
ADD R0 R1 R2
RETURN R0 1
)");

    // local values for multiple assignments
    CHECK_EQ("\n" + compileFunction0("local a return a"), R"(
LOADNIL R0
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = 1, 3 return a + 1, b"), R"(
LOADN R0 2
LOADN R1 3
RETURN R0 2
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = 1 return a + 1, b"), R"(
LOADN R0 2
LOADNIL R1
RETURN R0 2
)");

    // local values for multiple assignments w/multret
    CHECK_EQ("\n" + compileFunction0("local a, b = ... return a + 1, b"), R"(
GETVARARGS R0 2
ADDK R2 R0 K0
MOVE R3 R1
RETURN R2 2
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = 1, ... return a + 1, b"), R"(
LOADN R0 1
GETVARARGS R1 1
LOADN R2 2
MOVE R3 R1
RETURN R2 2
)");
}

TEST_CASE("ConstantFoldAndOr")
{
    // and/or constant folding when both sides are constant
    CHECK_EQ("\n" + compileFunction0("return true and 2"), R"(
LOADN R0 2
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return false and 2"), R"(
LOADB R0 0
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return nil and 2"), R"(
LOADNIL R0
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return true or 2"), R"(
LOADB R0 1
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return false or 2"), R"(
LOADN R0 2
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return nil or 2"), R"(
LOADN R0 2
RETURN R0 1
)");

    // and/or constant folding when left hand side is constant
    CHECK_EQ("\n" + compileFunction0("return true and a"), R"(
GETIMPORT R0 1
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return false and a"), R"(
LOADB R0 0
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return true or a"), R"(
LOADB R0 1
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return false or a"), R"(
GETIMPORT R0 1
RETURN R0 1
)");

    // constant fold parts in chains of and/or statements
    CHECK_EQ("\n" + compileFunction0("return a and true and b"), R"(
GETIMPORT R0 1
JUMPIFNOT R0 +2
GETIMPORT R0 3
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return a or false or b"), R"(
GETIMPORT R0 1
JUMPIF R0 +2
GETIMPORT R0 3
RETURN R0 1
)");
}

TEST_CASE("ConstantFoldConditionalAndOr")
{
    CHECK_EQ("\n" + compileFunction0("local a = ... if false or a then print(1) end"), R"(
GETVARARGS R0 1
JUMPIFNOT R0 +4
GETIMPORT R1 1
LOADN R2 1
CALL R1 1 0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = ... if not (false or a) then print(1) end"), R"(
GETVARARGS R0 1
JUMPIF R0 +4
GETIMPORT R1 1
LOADN R2 1
CALL R1 1 0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = ... if true and a then print(1) end"), R"(
GETVARARGS R0 1
JUMPIFNOT R0 +4
GETIMPORT R1 1
LOADN R2 1
CALL R1 1 0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = ... if not (true and a) then print(1) end"), R"(
GETVARARGS R0 1
JUMPIF R0 +4
GETIMPORT R1 1
LOADN R2 1
CALL R1 1 0
RETURN R0 0
)");
}

TEST_CASE("ConstantFoldFlowControl")
{
    // if
    CHECK_EQ("\n" + compileFunction0("if true then print(1) end"), R"(
GETIMPORT R0 1
LOADN R1 1
CALL R0 1 0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("if false then print(1) end"), R"(
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("if true then print(1) else print(2) end"), R"(
GETIMPORT R0 1
LOADN R1 1
CALL R0 1 0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("if false then print(1) else print(2) end"), R"(
GETIMPORT R0 1
LOADN R1 2
CALL R0 1 0
RETURN R0 0
)");

    // while
    CHECK_EQ("\n" + compileFunction0("while true do print(1) end"), R"(
GETIMPORT R0 1
LOADN R1 1
CALL R0 1 0
JUMPBACK -5
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("while false do print(1) end"), R"(
RETURN R0 0
)");

    // repeat
    CHECK_EQ("\n" + compileFunction0("repeat print(1) until true"), R"(
GETIMPORT R0 1
LOADN R1 1
CALL R0 1 0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("repeat print(1) until false"), R"(
GETIMPORT R0 1
LOADN R1 1
CALL R0 1 0
JUMPBACK -5
RETURN R0 0
)");

    // there's an odd case in repeat..until compilation where we evaluate the expression that is always false for side-effects of the left hand side
    CHECK_EQ("\n" + compileFunction0("repeat print(1) until five and false"), R"(
GETIMPORT R0 1
LOADN R1 1
CALL R0 1 0
GETIMPORT R0 3
JUMPIFNOT R0 +0
JUMPBACK -8
RETURN R0 0
)");
}

TEST_CASE("LoopBreak")
{
    // default codegen: compile breaks as unconditional jumps
    CHECK_EQ("\n" + compileFunction0("while true do if math.random() < 0.5 then break else end end"), R"(
GETIMPORT R0 2
CALL R0 0 1
LOADK R1 K3
JUMPIFNOTLT R0 R1 +3
RETURN R0 0
JUMP +0
JUMPBACK -9
RETURN R0 0
)");

    // optimization: if then body is a break statement, flip the branches
    CHECK_EQ("\n" + compileFunction0("while true do if math.random() < 0.5 then break end end"), R"(
GETIMPORT R0 2
CALL R0 0 1
LOADK R1 K3
JUMPIFLT R0 R1 +2
JUMPBACK -7
RETURN R0 0
)");
}

TEST_CASE("LoopContinue")
{
    // default codegen: compile continue as unconditional jumps
    CHECK_EQ("\n" + compileFunction0("repeat if math.random() < 0.5 then continue else end break until false error()"), R"(
GETIMPORT R0 2
CALL R0 0 1
LOADK R1 K3
JUMPIFNOTLT R0 R1 +5
JUMP +2
JUMP +2
JUMP +1
JUMPBACK -10
GETIMPORT R0 5
CALL R0 0 0
RETURN R0 0
)");

    // optimization: if then body is a continue statement, flip the branches
    CHECK_EQ("\n" + compileFunction0("repeat if math.random() < 0.5 then continue end break until false error()"), R"(
GETIMPORT R0 2
CALL R0 0 1
LOADK R1 K3
JUMPIFLT R0 R1 +2
JUMP +1
JUMPBACK -8
GETIMPORT R0 5
CALL R0 0 0
RETURN R0 0
)");
}

TEST_CASE("LoopContinueUntil")
{
    // it's valid to use locals defined inside the loop in until expression if they're defined before continue
    CHECK_EQ("\n" + compileFunction0("repeat local r = math.random() if r > 0.5 then continue end r = r + 0.3 until r < 0.5"), R"(
GETIMPORT R0 2
CALL R0 0 1
LOADK R1 K3
JUMPIFLT R1 R0 +2
ADDK R0 R0 K4
LOADK R1 K3
JUMPIFLT R0 R1 +2
JUMPBACK -11
RETURN R0 0
)");

    // it's however invalid to use locals if they are defined after continue
    try
    {
        Luau::BytecodeBuilder bcb;
        Luau::compileOrThrow(bcb, R"(
repeat
    local r = math.random()
    if r > 0.5 then
        continue
    end
    local rr = r + 0.3
until rr < 0.5
)");

        CHECK(!"Expected CompileError");
    }
    catch (Luau::CompileError& e)
    {
        CHECK_EQ(e.getLocation().begin.line + 1, 8);
        CHECK_EQ(
            std::string(e.what()), "Local rr used in the repeat..until condition is undefined because continue statement on line 5 jumps over it");
    }

    // but it's okay if continue is inside a non-repeat..until loop, or inside a loop that doesn't use the local (here `continue` just terminates
    // inner loop)
    CHECK_EQ("\n" + compileFunction0(
                        "repeat local r = math.random() repeat if r > 0.5 then continue end r = r - 0.1 until true r = r + 0.3 until r < 0.5"),
        R"(
GETIMPORT R0 2
CALL R0 0 1
LOADK R1 K3
JUMPIFLT R1 R0 +2
SUBK R0 R0 K4
ADDK R0 R0 K5
LOADK R1 K3
JUMPIFLT R0 R1 +2
JUMPBACK -12
RETURN R0 0
)");

    // and it's also okay to use a local defined in the until expression as long as it's inside a function!
    CHECK_EQ(
        "\n" + compileFunction(
                   "repeat local r = math.random() if r > 0.5 then continue end r = r + 0.3 until (function() local a = r return a < 0.5 end)()", 1),
        R"(
GETIMPORT R0 2
CALL R0 0 1
LOADK R1 K3
JUMPIFNOTLT R1 R0 +3
CLOSEUPVALS R0
JUMP +1
ADDK R0 R0 K4
NEWCLOSURE R1 P0
CAPTURE REF R0
CALL R1 0 1
JUMPIF R1 +2
CLOSEUPVALS R0
JUMPBACK -15
CLOSEUPVALS R0
RETURN R0 0
)");

    // but not if the function just refers to an upvalue
    try
    {
        Luau::BytecodeBuilder bcb;
        Luau::compileOrThrow(bcb, R"(
repeat
    local r = math.random()
    if r > 0.5 then
        continue
    end
    local rr = r + 0.3
until (function() return rr end)() < 0.5
)");

        CHECK(!"Expected CompileError");
    }
    catch (Luau::CompileError& e)
    {
        CHECK_EQ(e.getLocation().begin.line + 1, 8);
        CHECK_EQ(
            std::string(e.what()), "Local rr used in the repeat..until condition is undefined because continue statement on line 5 jumps over it");
    }

    // unless that upvalue is from an outer scope
    CHECK_EQ("\n" + compileFunction0("local stop = false stop = true function test() repeat local r = math.random() if r > 0.5 then "
                                     "continue end r = r + 0.3 until stop or r < 0.5 end"),
        R"(
GETIMPORT R0 2
CALL R0 0 1
LOADK R1 K3
JUMPIFLT R1 R0 +2
ADDK R0 R0 K4
GETUPVAL R1 0
JUMPIF R1 +4
LOADK R1 K3
JUMPIFLT R0 R1 +2
JUMPBACK -13
RETURN R0 0
)");

    // including upvalue references from a function expression
    CHECK_EQ("\n" + compileFunction("local stop = false stop = true function test() repeat local r = math.random() if r > 0.5 then continue "
                                    "end r = r + 0.3 until (function() return stop or r < 0.5 end)() end",
                        1),
        R"(
GETIMPORT R0 2
CALL R0 0 1
LOADK R1 K3
JUMPIFNOTLT R1 R0 +3
CLOSEUPVALS R0
JUMP +1
ADDK R0 R0 K4
NEWCLOSURE R1 P0
CAPTURE UPVAL U0
CAPTURE REF R0
CALL R1 0 1
JUMPIF R1 +2
CLOSEUPVALS R0
JUMPBACK -16
CLOSEUPVALS R0
RETURN R0 0
)");
}

TEST_CASE("LoopContinueUntilOops")
{
    // this used to crash the compiler :(
    try
    {
        Luau::BytecodeBuilder bcb;
        Luau::compileOrThrow(bcb, R"(
local _
repeat
continue
until not _
)");
    }
    catch (Luau::CompileError& e)
    {
        CHECK_EQ(
            std::string(e.what()), "Local _ used in the repeat..until condition is undefined because continue statement on line 4 jumps over it");
    }
}

TEST_CASE("AndOrOptimizations")
{
    // the OR/ORK optimization triggers for cutoff since lhs is simple
    CHECK_EQ("\n" + compileFunction(R"(
local function advancedRidgedFilter(value, cutoff)
    local cutoff = cutoff or .5
    value = value - cutoff
    return 1 - (value < 0 and -value or value) * 1 / (1 - cutoff)
end
)",
                        0),
        R"(
ORK R2 R1 K0
SUB R0 R0 R2
LOADN R4 1
LOADN R8 0
JUMPIFNOTLT R0 R8 +3
MINUS R7 R0
JUMPIF R7 +1
MOVE R7 R0
MULK R6 R7 K1
LOADN R8 1
SUB R7 R8 R2
DIV R5 R6 R7
SUB R3 R4 R5
RETURN R3 1
)");

    // sometimes we need to compute a boolean; this uses LOADB with an offset
    CHECK_EQ("\n" + compileFunction(R"(
function thinSurface(surfaceGradient, surfaceThickness)
    return surfaceGradient > .5 - surfaceThickness*.4 and surfaceGradient < .5 + surfaceThickness*.4
end
)",
                        0),
        R"(
LOADB R2 0
LOADK R4 K0
MULK R5 R1 K1
SUB R3 R4 R5
JUMPIFNOTLT R3 R0 +8
LOADK R4 K0
MULK R5 R1 K1
ADD R3 R4 R5
JUMPIFLT R0 R3 +2
LOADB R2 0 +1
LOADB R2 1
RETURN R2 1
)");

    // sometimes we need to compute a boolean; this uses LOADB with an offset for the last op, note that first op is compiled better
    CHECK_EQ("\n" + compileFunction(R"(
function thickSurface(surfaceGradient, surfaceThickness)
    return surfaceGradient < .5 - surfaceThickness*.4 or surfaceGradient > .5 + surfaceThickness*.4
end
)",
                        0),
        R"(
LOADB R2 1
LOADK R4 K0
MULK R5 R1 K1
SUB R3 R4 R5
JUMPIFLT R0 R3 +8
LOADK R4 K0
MULK R5 R1 K1
ADD R3 R4 R5
JUMPIFLT R3 R0 +2
LOADB R2 0 +1
LOADB R2 1
RETURN R2 1
)");

    // trivial ternary if with constants
    CHECK_EQ("\n" + compileFunction(R"(
function testSurface(surface)
    return surface and 1 or 0
end
)",
                        0),
        R"(
JUMPIFNOT R0 +2
LOADN R1 1
RETURN R1 1
LOADN R1 0
RETURN R1 1
)");

    // canonical saturate
    CHECK_EQ("\n" + compileFunction(R"(
function saturate(x)
    return x < 0 and 0 or x > 1 and 1 or x
end
)",
                        0),
        R"(
LOADN R2 0
JUMPIFNOTLT R0 R2 +3
LOADN R1 0
RETURN R1 1
LOADN R2 1
JUMPIFNOTLT R2 R0 +3
LOADN R1 1
RETURN R1 1
MOVE R1 R0
RETURN R1 1
)");
}

TEST_CASE("JumpFold")
{
    // jump-to-return folding to return
    CHECK_EQ("\n" + compileFunction0("return a and 1 or 0"), R"(
GETIMPORT R1 1
JUMPIFNOT R1 +2
LOADN R0 1
RETURN R0 1
LOADN R0 0
RETURN R0 1
)");

    // conditional jump in the inner if() folding to jump out of the expression (JUMPIFNOT+5 skips over all jumps, JUMP+1 skips over JUMP+0)
    CHECK_EQ("\n" + compileFunction0("if a then if b then b() else end else end d()"), R"(
GETIMPORT R0 1
JUMPIFNOT R0 +8
GETIMPORT R0 3
JUMPIFNOT R0 +5
GETIMPORT R0 3
CALL R0 0 0
JUMP +1
JUMP +0
GETIMPORT R0 5
CALL R0 0 0
RETURN R0 0
)");

    // same as example before but the unconditional jumps are folded with RETURN
    CHECK_EQ("\n" + compileFunction0("if a then if b then b() else end else end"), R"(
GETIMPORT R0 1
JUMPIFNOT R0 +8
GETIMPORT R0 3
JUMPIFNOT R0 +5
GETIMPORT R0 3
CALL R0 0 0
RETURN R0 0
RETURN R0 0
RETURN R0 0
)");

    // in this example, we do *not* have a JUMP after RETURN in the if branch
    // this is important since, even though this jump is never reached, jump folding needs to be able to analyze it
    CHECK_EQ("\n" + compileFunction(R"(
local function getPerlin(x, y, z, seed, scale, raw)
local seed = seed or 0
local scale = scale or 1
if not raw then
return math.noise(x / scale + (seed * 17) + masterSeed, y / scale - masterSeed, z / scale - seed*seed)*.5 + .5 --accounts for bleeding from interpolated line
else
return math.noise(x / scale + (seed * 17) + masterSeed, y / scale - masterSeed, z / scale - seed*seed)
end
end
)",
                        0),
        R"(
ORK R6 R3 K0
ORK R7 R4 K1
JUMPIF R5 +19
GETIMPORT R10 5
DIV R13 R0 R7
MULK R14 R6 K6
ADD R12 R13 R14
GETIMPORT R13 8
ADD R11 R12 R13
DIV R13 R1 R7
GETIMPORT R14 8
SUB R12 R13 R14
DIV R14 R2 R7
MUL R15 R6 R6
SUB R13 R14 R15
CALL R10 3 1
MULK R9 R10 K2
ADDK R8 R9 K2
RETURN R8 1
GETIMPORT R8 5
DIV R11 R0 R7
MULK R12 R6 K6
ADD R10 R11 R12
GETIMPORT R11 8
ADD R9 R10 R11
DIV R11 R1 R7
GETIMPORT R12 8
SUB R10 R11 R12
DIV R12 R2 R7
MUL R13 R6 R6
SUB R11 R12 R13
CALL R8 3 -1
RETURN R8 -1
)");
}

static std::string rep(const std::string& s, size_t n)
{
    std::string r;
    r.reserve(s.length() * n);
    for (size_t i = 0; i < n; ++i)
        r += s;
    return r;
}

TEST_CASE("RecursionParse")
{
    // The test forcibly pushes the stack limit during compilation; in NoOpt, the stack consumption is much larger so we need to reduce the limit to
    // not overflow the C stack. When ASAN is enabled, stack consumption increases even more.
#if defined(LUAU_ENABLE_ASAN)
    ScopedFastInt flag("LuauRecursionLimit", 200);
#elif defined(_NOOPT) || defined(_DEBUG)
    ScopedFastInt flag("LuauRecursionLimit", 300);
#endif

    Luau::BytecodeBuilder bcb;

    try
    {
        Luau::compileOrThrow(bcb, "a=" + rep("{", 1500) + rep("}", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your expression to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, "function a" + rep(".a", 1500) + "() end");
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your function name to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, "a=1" + rep("+1", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your expression to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, "a=" + rep("(", 1500) + "1" + rep(")", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your expression to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, rep("do ", 1500) + "print()" + rep(" end", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your block to make the code compile");
    }
}

TEST_CASE("ArrayIndexLiteral")
{
    CHECK_EQ("\n" + compileFunction0("local arr = {} return arr[0], arr[1], arr[256], arr[257]"), R"(
NEWTABLE R0 0 0
LOADN R2 0
GETTABLE R1 R0 R2
GETTABLEN R2 R0 1
GETTABLEN R3 R0 256
LOADN R5 257
GETTABLE R4 R0 R5
RETURN R1 4
)");

    CHECK_EQ("\n" + compileFunction0("local arr = {} local b = ... arr[0] = b arr[1] = b arr[256] = b arr[257] = b"), R"(
NEWTABLE R0 0 1
GETVARARGS R1 1
LOADN R2 0
SETTABLE R1 R0 R2
SETTABLEN R1 R0 1
SETTABLEN R1 R0 256
LOADN R2 257
SETTABLE R1 R0 R2
RETURN R0 0
)");
}

TEST_CASE("NestedFunctionCalls")
{
    CHECK_EQ("\n" + compileFunction0("function clamp(t,a,b) return math.min(math.max(t,a),b) end"), R"(
FASTCALL2 18 R0 R1 +5
MOVE R5 R0
MOVE R6 R1
GETIMPORT R4 2
CALL R4 2 1
FASTCALL2 19 R4 R2 +4
MOVE R5 R2
GETIMPORT R3 4
CALL R3 2 -1
RETURN R3 -1
)");
}

TEST_CASE("UpvaluesLoopsBytecode")
{
    CHECK_EQ("\n" + compileFunction(R"(
function test()
	for i=1,10 do
        i = i
		foo(function() return i end)
		if bar then
			break
		end
	end
	return 0
end
)",
                        1),
        R"(
LOADN R2 1
LOADN R0 10
LOADN R1 1
FORNPREP R0 +14
MOVE R3 R2
MOVE R3 R3
GETIMPORT R4 1
NEWCLOSURE R5 P0
CAPTURE REF R3
CALL R4 1 0
GETIMPORT R4 3
JUMPIFNOT R4 +2
CLOSEUPVALS R3
JUMP +2
CLOSEUPVALS R3
FORNLOOP R0 -14
LOADN R0 0
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction(R"(
function test()
	for i in ipairs(data) do
        i = i
		foo(function() return i end)
		if bar then
			break
		end
	end
	return 0
end
)",
                        1),
        R"(
GETIMPORT R0 1
GETIMPORT R1 3
CALL R0 1 3
FORGPREP_INEXT R0 +12
MOVE R3 R3
GETIMPORT R5 5
NEWCLOSURE R6 P0
CAPTURE REF R3
CALL R5 1 0
GETIMPORT R5 7
JUMPIFNOT R5 +2
CLOSEUPVALS R3
JUMP +2
CLOSEUPVALS R3
FORGLOOP_INEXT R0 -13
LOADN R0 0
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction(R"(
function test()
	local i = 0
	while i < 5 do
		local j
        j = i
		foo(function() return j end)
		i = i + 1
		if bar then
			break
		end
	end
	return 0
end
)",
                        1),
        R"(
LOADN R0 0
LOADN R1 5
JUMPIFNOTLT R0 R1 +16
LOADNIL R1
MOVE R1 R0
GETIMPORT R2 1
NEWCLOSURE R3 P0
CAPTURE REF R1
CALL R2 1 0
ADDK R0 R0 K2
GETIMPORT R2 4
JUMPIFNOT R2 +2
CLOSEUPVALS R1
JUMP +2
CLOSEUPVALS R1
JUMPBACK -18
LOADN R1 0
RETURN R1 1
)");

    CHECK_EQ("\n" + compileFunction(R"(
function test()
	local i = 0
	repeat
		local j
        j = i
		foo(function() return j end)
		i = i + 1
		if bar then
			break
		end
	until i < 5
	return 0
end
)",
                        1),
        R"(
LOADN R0 0
LOADNIL R1
MOVE R1 R0
GETIMPORT R2 1
NEWCLOSURE R3 P0
CAPTURE REF R1
CALL R2 1 0
ADDK R0 R0 K2
GETIMPORT R2 4
JUMPIFNOT R2 +2
CLOSEUPVALS R1
JUMP +6
LOADN R2 5
JUMPIFLT R0 R2 +3
CLOSEUPVALS R1
JUMPBACK -18
CLOSEUPVALS R1
LOADN R1 0
RETURN R1 1
)");
}

TEST_CASE("TypeAliasing")
{
    Luau::BytecodeBuilder bcb;
    Luau::CompileOptions options;
    Luau::ParseOptions parseOptions;
    CHECK_NOTHROW(Luau::compileOrThrow(bcb, "type A = number local a: A = 1", options, parseOptions));
}

TEST_CASE("DebugLineInfo")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Lines);
    Luau::compileOrThrow(bcb, R"(
local kSelectedBiomes = {
    ['Mountains'] = true,
    ['Canyons'] = true,
    ['Dunes'] = true,
    ['Arctic'] = true,
    ['Lavaflow'] = true,
    ['Hills'] = true,
    ['Plains'] = true,
    ['Marsh'] = true,
    ['Water'] = true,
}
local result = ""
for k in pairs(kSelectedBiomes) do
    result = result .. k
end
return result
)");

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
2: NEWTABLE R0 16 0
3: LOADB R1 1
3: SETTABLEKS R1 R0 K0
4: LOADB R1 1
4: SETTABLEKS R1 R0 K1
5: LOADB R1 1
5: SETTABLEKS R1 R0 K2
6: LOADB R1 1
6: SETTABLEKS R1 R0 K3
7: LOADB R1 1
7: SETTABLEKS R1 R0 K4
8: LOADB R1 1
8: SETTABLEKS R1 R0 K5
9: LOADB R1 1
9: SETTABLEKS R1 R0 K6
10: LOADB R1 1
10: SETTABLEKS R1 R0 K7
11: LOADB R1 1
11: SETTABLEKS R1 R0 K8
13: LOADK R1 K9
14: GETIMPORT R2 11
14: MOVE R3 R0
14: CALL R2 1 3
14: FORGPREP_NEXT R2 +3
15: MOVE R7 R1
15: MOVE R8 R5
15: CONCAT R1 R7 R8
14: FORGLOOP_NEXT R2 -4
17: RETURN R1 1
)");
}

TEST_CASE("DebugLineInfoFor")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Lines);
    Luau::compileOrThrow(bcb, R"(
for
i
in
1
,
2
,
3
do
print(i)
end
)");

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
5: LOADN R0 1
7: LOADN R1 2
9: LOADN R2 3
9: JUMP +4
11: GETIMPORT R5 1
11: MOVE R6 R3
11: CALL R5 1 0
2: FORGLOOP R0 -5 1
13: RETURN R0 0
)");
}

TEST_CASE("DebugLineInfoWhile")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Lines);
    Luau::compileOrThrow(bcb, R"(
local count = 0
while true do
    count += 1
    if count > 1 then
        print("done!")
        break
    end
end
)");

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
2: LOADN R0 0
4: ADDK R0 R0 K0
5: LOADN R1 1
5: JUMPIFNOTLT R1 R0 +6
6: GETIMPORT R1 2
6: LOADK R2 K3
6: CALL R1 1 0
10: RETURN R0 0
3: JUMPBACK -10
10: RETURN R0 0
)");
}

TEST_CASE("DebugLineInfoRepeatUntil")
{
    CHECK_EQ("\n" + compileFunction0Coverage(R"(
local f = 0
repeat
    f += 1
    if f == 1 then 
        print(f)
    else
        f = 0
    end
until f == 0
)",
                        0),
        R"(
2: LOADN R0 0
4: ADDK R0 R0 K0
5: JUMPIFNOTEQK R0 K0 +6
6: GETIMPORT R1 2
6: MOVE R2 R0
6: CALL R1 1 0
6: JUMP +1
8: LOADN R0 0
10: JUMPIFEQK R0 K3 +2
10: JUMPBACK -12
11: RETURN R0 0
)");
}

TEST_CASE("DebugLineInfoSubTable")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Lines);
    Luau::compileOrThrow(bcb, R"(
local Value1, Value2, Value3 = ...
local Table = {}

Table.SubTable["Key"] = {
	Key1 = Value1,
	Key2 = Value2,
	Key3 = Value3,
	Key4 = true,
}
)");

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
2: GETVARARGS R0 3
3: NEWTABLE R3 0 0
5: GETTABLEKS R4 R3 K0
5: DUPTABLE R5 5
6: SETTABLEKS R0 R5 K1
7: SETTABLEKS R1 R5 K2
8: SETTABLEKS R2 R5 K3
9: LOADB R6 1
9: SETTABLEKS R6 R5 K4
5: SETTABLEKS R5 R4 K6
11: RETURN R0 0
)");
}

TEST_CASE("DebugLineInfoCall")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Lines);
    Luau::compileOrThrow(bcb, R"(
local Foo = ...

Foo:Bar(
    1,
    2,
    3)
)");

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
2: GETVARARGS R0 1
5: LOADN R3 1
6: LOADN R4 2
7: LOADN R5 3
4: NAMECALL R1 R0 K0
4: CALL R1 4 0
8: RETURN R0 0
)");
}

TEST_CASE("DebugSource")
{
    const char* source = R"(
local kSelectedBiomes = {
    ['Mountains'] = true,
    ['Canyons'] = true,
    ['Dunes'] = true,
    ['Arctic'] = true,
    ['Lavaflow'] = true,
    ['Hills'] = true,
    ['Plains'] = true,
    ['Marsh'] = true,
    ['Water'] = true,
}
local result = ""
for k in pairs(kSelectedBiomes) do
    result = result .. k
end
return result
)";

    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Source);
    bcb.setDumpSource(source);

    Luau::compileOrThrow(bcb, source);

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
    2: local kSelectedBiomes = {
NEWTABLE R0 16 0
    3:     ['Mountains'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K0
    4:     ['Canyons'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K1
    5:     ['Dunes'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K2
    6:     ['Arctic'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K3
    7:     ['Lavaflow'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K4
    8:     ['Hills'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K5
    9:     ['Plains'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K6
   10:     ['Marsh'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K7
   11:     ['Water'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K8
   13: local result = ""
LOADK R1 K9
   14: for k in pairs(kSelectedBiomes) do
GETIMPORT R2 11
MOVE R3 R0
CALL R2 1 3
FORGPREP_NEXT R2 +3
   15:     result = result .. k
MOVE R7 R1
MOVE R8 R5
CONCAT R1 R7 R8
   14: for k in pairs(kSelectedBiomes) do
FORGLOOP_NEXT R2 -4
   17: return result
RETURN R1 1
)");
}

TEST_CASE("DebugLocals")
{
    const char* source = R"(
function foo(e, f)
    local a = 1
    for i=1,3 do
        print(i)
    end
    for k,v in pairs() do
        print(k, v)
    end
    do
        local b = 2
        print(b)
    end
    do
        local c = 2
        print(b)
    end
    local function inner()
        return inner, a
    end
    return a
end
)";

    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Lines | Luau::BytecodeBuilder::Dump_Locals);
    bcb.setDumpSource(source);

    Luau::CompileOptions options;
    options.debugLevel = 2;

    Luau::compileOrThrow(bcb, source, options);

    CHECK_EQ("\n" + bcb.dumpFunction(1), R"(
local 0: reg 5, start pc 5 line 5, end pc 8 line 5
local 1: reg 6, start pc 14 line 8, end pc 18 line 8
local 2: reg 7, start pc 14 line 8, end pc 18 line 8
local 3: reg 3, start pc 21 line 12, end pc 24 line 12
local 4: reg 3, start pc 26 line 16, end pc 30 line 16
local 5: reg 0, start pc 0 line 3, end pc 34 line 21
local 6: reg 1, start pc 0 line 3, end pc 34 line 21
local 7: reg 2, start pc 1 line 4, end pc 34 line 21
local 8: reg 3, start pc 34 line 21, end pc 34 line 21
3: LOADN R2 1
4: LOADN R5 1
4: LOADN R3 3
4: LOADN R4 1
4: FORNPREP R3 +5
5: GETIMPORT R6 1
5: MOVE R7 R5
5: CALL R6 1 0
4: FORNLOOP R3 -5
7: GETIMPORT R3 3
7: CALL R3 0 3
7: FORGPREP_NEXT R3 +5
8: GETIMPORT R8 1
8: MOVE R9 R6
8: MOVE R10 R7
8: CALL R8 2 0
7: FORGLOOP_NEXT R3 -6
11: LOADN R3 2
12: GETIMPORT R4 1
12: LOADN R5 2
12: CALL R4 1 0
15: LOADN R3 2
16: GETIMPORT R4 1
16: GETIMPORT R5 5
16: CALL R4 1 0
18: NEWCLOSURE R3 P0
18: CAPTURE VAL R3
18: CAPTURE VAL R2
21: RETURN R2 1
)");
}

TEST_CASE("AssignmentConflict")
{
    // assignments are left to right
    CHECK_EQ("\n" + compileFunction0("local a, b a, b = 1, 2"), R"(
LOADNIL R0
LOADNIL R1
LOADN R2 1
LOADN R3 2
MOVE R0 R2
MOVE R1 R3
RETURN R0 0
)");

    // if assignment of a local invalidates a direct register reference in later assignments, the value is evacuated to a temp register
    CHECK_EQ("\n" + compileFunction0("local a a, a[1] = 1, 2"), R"(
LOADNIL R0
MOVE R1 R0
LOADN R2 1
LOADN R3 2
MOVE R0 R2
SETTABLEN R3 R1 1
RETURN R0 0
)");

    // note that this doesn't happen if the local assignment happens last naturally
    CHECK_EQ("\n" + compileFunction0("local a a[1], a = 1, 2"), R"(
LOADNIL R0
LOADN R1 1
LOADN R2 2
SETTABLEN R1 R0 1
MOVE R0 R2
RETURN R0 0
)");

    // this will happen if assigned register is used in any table expression, including as an object...
    CHECK_EQ("\n" + compileFunction0("local a a, a.foo = 1, 2"), R"(
LOADNIL R0
MOVE R1 R0
LOADN R2 1
LOADN R3 2
MOVE R0 R2
SETTABLEKS R3 R1 K0
RETURN R0 0
)");

    // ... or a table index ...
    CHECK_EQ("\n" + compileFunction0("local a a, foo[a] = 1, 2"), R"(
LOADNIL R0
GETIMPORT R1 1
MOVE R2 R0
LOADN R3 1
LOADN R4 2
MOVE R0 R3
SETTABLE R4 R1 R2
RETURN R0 0
)");

    // ... or both ...
    CHECK_EQ("\n" + compileFunction0("local a a, a[a] = 1, 2"), R"(
LOADNIL R0
MOVE R1 R0
LOADN R2 1
LOADN R3 2
MOVE R0 R2
SETTABLE R3 R1 R1
RETURN R0 0
)");

    // ... or both with two different locals ...
    CHECK_EQ("\n" + compileFunction0("local a, b a, b, a[b] = 1, 2, 3"), R"(
LOADNIL R0
LOADNIL R1
MOVE R2 R0
MOVE R3 R1
LOADN R4 1
LOADN R5 2
LOADN R6 3
MOVE R0 R4
MOVE R1 R5
SETTABLE R6 R2 R3
RETURN R0 0
)");

    // however note that if it participates in an expression on the left hand side, there's no point reassigning it since we'd compute the expr value
    // into a temp register
    CHECK_EQ("\n" + compileFunction0("local a a, foo[a + 1] = 1, 2"), R"(
LOADNIL R0
GETIMPORT R1 1
ADDK R2 R0 K2
LOADN R3 1
LOADN R4 2
MOVE R0 R3
SETTABLE R4 R1 R2
RETURN R0 0
)");
}

TEST_CASE("FastcallBytecode")
{
    // direct global call
    CHECK_EQ("\n" + compileFunction0("return math.abs(-5)"), R"(
LOADN R1 -5
FASTCALL1 2 R1 +2
GETIMPORT R0 2
CALL R0 1 -1
RETURN R0 -1
)");

    // call through a local variable
    CHECK_EQ("\n" + compileFunction0("local abs = math.abs return abs(-5)"), R"(
GETIMPORT R0 2
LOADN R2 -5
FASTCALL1 2 R2 +1
MOVE R1 R0
CALL R1 1 -1
RETURN R1 -1
)");

    // call through an upvalue
    CHECK_EQ("\n" + compileFunction0("local abs = math.abs function foo() return abs(-5) end return foo()"), R"(
LOADN R1 -5
FASTCALL1 2 R1 +1
GETUPVAL R0 0
CALL R0 1 -1
RETURN R0 -1
)");

    // mutating the global in the script breaks the optimization
    CHECK_EQ("\n" + compileFunction0("math = {} return math.abs(-5)"), R"(
NEWTABLE R0 0 0
SETGLOBAL R0 K0
GETGLOBAL R1 K0
GETTABLEKS R0 R1 K1
LOADN R1 -5
CALL R0 1 -1
RETURN R0 -1
)");

    // mutating the local in the script breaks the optimization
    CHECK_EQ("\n" + compileFunction0("local abs = math.abs abs = nil return abs(-5)"), R"(
GETIMPORT R0 2
LOADNIL R0
MOVE R1 R0
LOADN R2 -5
CALL R1 1 -1
RETURN R1 -1
)");

    // mutating the global in the script breaks the optimization, even if you do this after computing the local (for simplicity)
    CHECK_EQ("\n" + compileFunction0("local abs = math.abs math = {} return abs(-5)"), R"(
GETGLOBAL R1 K0
GETTABLEKS R0 R1 K1
NEWTABLE R1 0 0
SETGLOBAL R1 K0
MOVE R1 R0
LOADN R2 -5
CALL R1 1 -1
RETURN R1 -1
)");
}

TEST_CASE("LotsOfParameters")
{
    const char* source = R"(
select("#",1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
)";

    try
    {
        Luau::BytecodeBuilder bcb;
        Luau::compileOrThrow(bcb, source);
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Out of registers when trying to allocate 265 registers: exceeded limit 255");
    }
}

TEST_CASE("LotsOfIndexers")
{
    const char* source = R"(
function u(t)for t in s(t.l.l.l.l.l.l.l.l.l.l.l.l.l.l.n.l.l.l.l.l.l.l.l.l.l.l.l.n.l.l.l.l.l.l.n.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.n.l.l.l.g.l.l.l.l.l.l.l.l.l.l.l.l.l.n.l.l.l.l.l.t.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.r.l.l.l.l.l.l.n.l.l.l.l.l.l.l.l.l.l.l.l.n.l.l.l.l.l.l.n.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.g.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.n.l.l.l.l.l.l.l.l.n.l.l.l.l.l.l.l.l.l.l.l.l.n.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.r.n.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.l.n.l.l.l.n.l.l.l.l.l.l.l.n.l.l.l.l.l.l.l.l.l.l..l,l)do end
end
)";

    try
    {
        Luau::BytecodeBuilder bcb;
        Luau::compileOrThrow(bcb, source);
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Out of registers when trying to allocate 1 registers: exceeded limit 255");
    }
}

TEST_CASE("AsConstant")
{
    const char* source = R"(
--!strict
return (1 + 2) :: number
)";

    Luau::CompileOptions options;
    Luau::ParseOptions parseOptions;

    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::compileOrThrow(bcb, source, options, parseOptions);

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
LOADN R0 3
RETURN R0 1
)");
}

TEST_CASE("PreserveNegZero")
{
    CHECK_EQ("\n" + compileFunction0("return 0"), R"(
LOADN R0 0
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return -0"), R"(
LOADK R0 K0
RETURN R0 1
)");
}

TEST_CASE("CaptureImmutable")
{
    // capture argument: note capture by value
    CHECK_EQ("\n" + compileFunction("function foo(a, b) return function() return a end end", 1), R"(
NEWCLOSURE R2 P0
CAPTURE VAL R0
RETURN R2 1
)");

    // capture mutable argument: note capture by reference + close
    CHECK_EQ("\n" + compileFunction("function foo(a, b) a = 1 return function() return a end end", 1), R"(
LOADN R0 1
NEWCLOSURE R2 P0
CAPTURE REF R0
CLOSEUPVALS R0
RETURN R2 1
)");

    // capture two arguments, one mutable, one immutable
    CHECK_EQ("\n" + compileFunction("function foo(a, b) a = 1 return function() return a + b end end", 1), R"(
LOADN R0 1
NEWCLOSURE R2 P0
CAPTURE REF R0
CAPTURE VAL R1
CLOSEUPVALS R0
RETURN R2 1
)");

    // capture self
    CHECK_EQ("\n" + compileFunction("function bar:foo(a, b) return function() return self end end", 1), R"(
NEWCLOSURE R3 P0
CAPTURE VAL R0
RETURN R3 1
)");

    // capture mutable self (who mutates self?!?)
    CHECK_EQ("\n" + compileFunction("function bar:foo(a, b) self = 42 return function() return self end end", 1), R"(
LOADN R0 42
NEWCLOSURE R3 P0
CAPTURE REF R0
CLOSEUPVALS R0
RETURN R3 1
)");

    // capture upvalue: one mutable, one immutable
    CHECK_EQ("\n" + compileFunction("local a, b = math.rand() a = 42 function foo() return function() return a + b end end", 1), R"(
NEWCLOSURE R0 P0
CAPTURE UPVAL U0
CAPTURE UPVAL U1
RETURN R0 1
)");

    if (FFlag::LuauPreloadClosuresUpval)
    {
        // recursive capture
        CHECK_EQ("\n" + compileFunction("local function foo() return foo() end", 1), R"(
DUPCLOSURE R0 K0
CAPTURE VAL R0
RETURN R0 0
)");

        // multi-level recursive capture
        CHECK_EQ("\n" + compileFunction("local function foo() return function() return foo() end end", 1), R"(
DUPCLOSURE R0 K0
CAPTURE UPVAL U0
RETURN R0 1
)");

        // multi-level recursive capture where function isn't top-level
        // note: this should probably be optimized to DUPCLOSURE but doing that requires a different upval tracking flow in the compiler
        CHECK_EQ("\n" + compileFunction(R"(
local function foo()
    local function bar()
        return function() return bar() end
    end
end
)",
                            1),
            R"(
NEWCLOSURE R0 P0
CAPTURE UPVAL U0
RETURN R0 1
)");
    }
    else
    {
        // recursive capture
        CHECK_EQ("\n" + compileFunction("local function foo() return foo() end", 1), R"(
NEWCLOSURE R0 P0
CAPTURE VAL R0
RETURN R0 0
)");
    }
}

TEST_CASE("OutOfLocals")
{
    std::string source;

    for (int i = 0; i < 200; ++i)
    {
        formatAppend(source, "local foo%d\n", i);
    }

    source += "local bar\n";

    Luau::CompileOptions options;
    options.debugLevel = 2; // make sure locals aren't elided by requesting their debug info

    try
    {
        Luau::BytecodeBuilder bcb;
        Luau::compileOrThrow(bcb, source, options);

        CHECK(!"Expected CompileError");
    }
    catch (Luau::CompileError& e)
    {
        CHECK_EQ(e.getLocation().begin.line + 1, 201);
        CHECK_EQ(std::string(e.what()), "Out of local registers when trying to allocate bar: exceeded limit 200");
    }
}

TEST_CASE("OutOfUpvalues")
{
    std::string source;

    for (int i = 0; i < 150; ++i)
    {
        formatAppend(source, "local foo%d\n", i);
        formatAppend(source, "foo%d = 42\n", i);
    }

    source += "function foo()\n";

    for (int i = 0; i < 150; ++i)
    {
        formatAppend(source, "local bar%d\n", i);
        formatAppend(source, "bar%d = 42\n", i);
    }

    source += "function bar()\n";

    for (int i = 0; i < 150; ++i)
    {
        formatAppend(source, "print(foo%d, bar%d)\n", i, i);
    }

    source += "end\nend\n";

    try
    {
        Luau::BytecodeBuilder bcb;
        Luau::compileOrThrow(bcb, source);

        CHECK(!"Expected CompileError");
    }
    catch (Luau::CompileError& e)
    {
        CHECK_EQ(e.getLocation().begin.line + 1, 201);
        CHECK_EQ(std::string(e.what()), "Out of upvalue registers when trying to allocate foo100: exceeded limit 200");
    }
}

TEST_CASE("OutOfRegisters")
{
    std::string source;

    source += "print(\n";

    for (int i = 0; i < 150; ++i)
    {
        formatAppend(source, "%d,\n", i);
    }

    source += "table.pack(\n";

    for (int i = 0; i < 150; ++i)
    {
        formatAppend(source, "%d,\n", i);
    }

    source += "42))\n";

    try
    {
        Luau::BytecodeBuilder bcb;
        Luau::compileOrThrow(bcb, source);

        CHECK(!"Expected CompileError");
    }
    catch (Luau::CompileError& e)
    {
        CHECK_EQ(e.getLocation().begin.line + 1, 152);
        CHECK_EQ(std::string(e.what()), "Out of registers when trying to allocate 152 registers: exceeded limit 255");
    }
}

TEST_CASE("FastCallImportFallback")
{
    std::string source = "local t = {}\n";

    // we need to exhaust the 10-bit constant space to block GETIMPORT from being emitted
    for (int i = 1; i <= 1024; ++i)
    {
        formatAppend(source, "t[%d] = \"%d\"\n", i, i);
    }

    source += "return math.abs(-1)\n";

    std::string code = compileFunction0(source.c_str());

    std::vector<std::string_view> insns = Luau::split(code, '\n');

    CHECK_EQ(insns[insns.size() - 9], "LOADN R1 1024");
    CHECK_EQ(insns[insns.size() - 8], "LOADK R2 K1023");
    CHECK_EQ(insns[insns.size() - 7], "SETTABLE R2 R0 R1");
    CHECK_EQ(insns[insns.size() - 6], "LOADN R2 -1");
    CHECK_EQ(insns[insns.size() - 5], "FASTCALL1 2 R2 +4");
    CHECK_EQ(insns[insns.size() - 4], "GETGLOBAL R3 K1024"); // note: it's important that this doesn't overwrite R2
    CHECK_EQ(insns[insns.size() - 3], "GETTABLEKS R1 R3 K1025");
    CHECK_EQ(insns[insns.size() - 2], "CALL R1 1 -1");
    CHECK_EQ(insns[insns.size() - 1], "RETURN R1 -1");
}

TEST_CASE("CompoundAssignment")
{
    // globals vs constants
    CHECK_EQ("\n" + compileFunction0("a += 1"), R"(
GETGLOBAL R0 K0
ADDK R0 R0 K1
SETGLOBAL R0 K0
RETURN R0 0
)");

    // globals vs expressions
    CHECK_EQ("\n" + compileFunction0("a -= a"), R"(
GETGLOBAL R0 K0
GETGLOBAL R1 K0
SUB R0 R0 R1
SETGLOBAL R0 K0
RETURN R0 0
)");

    // locals vs constants
    CHECK_EQ("\n" + compileFunction0("local a = 1 a *= 2"), R"(
LOADN R0 1
MULK R0 R0 K0
RETURN R0 0
)");

    // locals vs locals
    CHECK_EQ("\n" + compileFunction0("local a = 1 a /= a"), R"(
LOADN R0 1
DIV R0 R0 R0
RETURN R0 0
)");

    // locals vs expressions
    CHECK_EQ("\n" + compileFunction0("local a = 1 a /= a + 1"), R"(
LOADN R0 1
ADDK R1 R0 K0
DIV R0 R0 R1
RETURN R0 0
)");

    // upvalues
    CHECK_EQ("\n" + compileFunction0("local a = 1 function foo() a += 4 end"), R"(
GETUPVAL R0 0
ADDK R0 R0 K0
SETUPVAL R0 0
RETURN R0 0
)");

    // table variants (indexed by string, number, variable)
    CHECK_EQ("\n" + compileFunction0("local a = {} a.foo += 5"), R"(
NEWTABLE R0 1 0
GETTABLEKS R1 R0 K0
ADDK R1 R1 K1
SETTABLEKS R1 R0 K0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = {} a[1] += 5"), R"(
NEWTABLE R0 0 1
GETTABLEN R1 R0 1
ADDK R1 R1 K0
SETTABLEN R1 R0 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = {} a[a] += 5"), R"(
NEWTABLE R0 0 0
GETTABLE R1 R0 R0
ADDK R1 R1 K0
SETTABLE R1 R0 R0
RETURN R0 0
)");

    // left hand side is evaluated once
    CHECK_EQ("\n" + compileFunction0("foo()[bar()] += 5"), R"(
GETIMPORT R0 1
CALL R0 0 1
GETIMPORT R1 3
CALL R1 0 1
GETTABLE R2 R0 R1
ADDK R2 R2 K4
SETTABLE R2 R0 R1
RETURN R0 0
)");
}

TEST_CASE("CompoundAssignmentConcat")
{
    // basic concat
    CHECK_EQ("\n" + compileFunction0("local a = '' a ..= 'a'"), R"(
LOADK R0 K0
MOVE R1 R0
LOADK R2 K1
CONCAT R0 R1 R2
RETURN R0 0
)");

    // concat chains
    CHECK_EQ("\n" + compileFunction0("local a = '' a ..= 'a' .. 'b'"), R"(
LOADK R0 K0
MOVE R1 R0
LOADK R2 K1
LOADK R3 K2
CONCAT R0 R1 R3
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = '' a ..= 'a' .. 'b' .. 'c'"), R"(
LOADK R0 K0
MOVE R1 R0
LOADK R2 K1
LOADK R3 K2
LOADK R4 K3
CONCAT R0 R1 R4
RETURN R0 0
)");

    // concat on non-local
    CHECK_EQ("\n" + compileFunction0("_VERSION ..= 'a' .. 'b'"), R"(
GETGLOBAL R1 K0
LOADK R2 K1
LOADK R3 K2
CONCAT R0 R1 R3
SETGLOBAL R0 K0
RETURN R0 0
)");
}

TEST_CASE("JumpTrampoline")
{
    std::string source;
    source += "local sum = 0\n";
    source += "for i=1,3 do\n";
    for (int i = 0; i < 10000; ++i)
    {
        source += "sum = sum + i\n";
        source += "if sum > 150000 then break end\n";
    }
    source += "end\n";
    source += "return sum\n";

    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::compileOrThrow(bcb, source.c_str());

    std::stringstream bcs(bcb.dumpFunction(0));

    std::vector<std::string> insns;
    std::string insn;
    while ((std::getline)(bcs, insn))
        insns.push_back(insn);

    // FORNPREP and early JUMPs (break) need to go through a trampoline
    CHECK_EQ(insns[0], "LOADN R0 0");
    CHECK_EQ(insns[1], "LOADN R3 1");
    CHECK_EQ(insns[2], "LOADN R1 3");
    CHECK_EQ(insns[3], "LOADN R2 1");
    CHECK_EQ(insns[4], "JUMP +1");
    CHECK_EQ(insns[5], "JUMPX +54542");
    CHECK_EQ(insns[6], "FORNPREP R1 -2");
    CHECK_EQ(insns[7], "ADD R0 R0 R3");
    CHECK_EQ(insns[8], "LOADK R4 K0");
    CHECK_EQ(insns[9], "JUMP +1");
    CHECK_EQ(insns[10], "JUMPX +54537");
    CHECK_EQ(insns[11], "JUMPIFLT R4 R0 -2");
    CHECK_EQ(insns[12], "ADD R0 R0 R3");
    CHECK_EQ(insns[13], "LOADK R4 K0");
    CHECK_EQ(insns[14], "JUMP +1");
    CHECK_EQ(insns[15], "JUMPX +54531");
    CHECK_EQ(insns[16], "JUMPIFLT R4 R0 -2");

    // FORNLOOP has to go through a trampoline since the jump is back to the beginning of the function
    // however, late JUMPs (break) don't need a trampoline since the loop end is really close by
    CHECK_EQ(insns[44539], "ADD R0 R0 R3");
    CHECK_EQ(insns[44540], "LOADK R4 K0");
    CHECK_EQ(insns[44541], "JUMPIFLT R4 R0 +8");
    CHECK_EQ(insns[44542], "ADD R0 R0 R3");
    CHECK_EQ(insns[44543], "LOADK R4 K0");
    CHECK_EQ(insns[44544], "JUMPIFLT R4 R0 +4");
    CHECK_EQ(insns[44545], "JUMP +1");
    CHECK_EQ(insns[44546], "JUMPX -54540");
    CHECK_EQ(insns[44547], "FORNLOOP R1 -2");
    CHECK_EQ(insns[44548], "RETURN R0 1");
}

TEST_CASE("CompileBytecode")
{
    // This is a coverage test, it just exercises bytecode dumping for correct and malformed code
    Luau::compile("return 5");
    Luau::compile("this is not valid lua, right?");
}

TEST_CASE("NestedNamecall")
{
    CHECK_EQ("\n" + compileFunction0(R"(
local obj = ...
return obj:Method(1):Method(2):Method(3)
)"),
        R"(
GETVARARGS R0 1
LOADN R3 1
NAMECALL R1 R0 K0
CALL R1 2 1
LOADN R3 2
NAMECALL R1 R1 K0
CALL R1 2 1
LOADN R3 3
NAMECALL R1 R1 K0
CALL R1 2 -1
RETURN R1 -1
)");
}

TEST_CASE("ElideLocals")
{
    // simple local elision: all locals are constant
    CHECK_EQ("\n" + compileFunction0(R"(
local a, b = 1, 2
return a + b
)"),
        R"(
LOADN R0 3
RETURN R0 1
)");

    // side effecting expressions block local elision
    CHECK_EQ("\n" + compileFunction0(R"(
local a = g()
return a
)"),
        R"(
GETIMPORT R0 1
CALL R0 0 1
RETURN R0 1
)");

    // ... even if they are not used
    CHECK_EQ("\n" + compileFunction0(R"(
local a = 1, g()
return a
)"),
        R"(
LOADN R0 1
GETIMPORT R1 1
CALL R1 0 1
RETURN R0 1
)");
}

TEST_CASE("ConstantJumpCompare")
{
    CHECK_EQ("\n" + compileFunction0(R"(
local obj = ...
local b = obj == 1
)"),
        R"(
GETVARARGS R0 1
JUMPIFEQK R0 K0 +2
LOADB R1 0 +1
LOADB R1 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0(R"(
local obj = ...
local b = 1 == obj
)"),
        R"(
GETVARARGS R0 1
JUMPIFEQK R0 K0 +2
LOADB R1 0 +1
LOADB R1 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0(R"(
local obj = ...
local b = "Hello, Sailor!" == obj
)"),
        R"(
GETVARARGS R0 1
JUMPIFEQK R0 K0 +2
LOADB R1 0 +1
LOADB R1 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0(R"(
local obj = ...
local b = nil == obj
)"),
        R"(
GETVARARGS R0 1
JUMPIFEQK R0 K0 +2
LOADB R1 0 +1
LOADB R1 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0(R"(
local obj = ...
local b = true == obj
)"),
        R"(
GETVARARGS R0 1
JUMPIFEQK R0 K0 +2
LOADB R1 0 +1
LOADB R1 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0(R"(
local obj = ...
local b = nil ~= obj
)"),
        R"(
GETVARARGS R0 1
JUMPIFNOTEQK R0 K0 +2
LOADB R1 0 +1
LOADB R1 1
RETURN R0 0
)");

    // table literals should not generate IFEQK variants
    CHECK_EQ("\n" + compileFunction0(R"(
local obj = ...
local b = obj == {}
)"),
        R"(
GETVARARGS R0 1
NEWTABLE R2 0 0
JUMPIFEQ R0 R2 +2
LOADB R1 0 +1
LOADB R1 1
RETURN R0 0
)");
}

TEST_CASE("TableConstantStringIndex")
{
    CHECK_EQ("\n" + compileFunction0(R"(
local t = { a = 2 }
return t['a']
)"),
        R"(
DUPTABLE R0 1
LOADN R1 2
SETTABLEKS R1 R0 K0
GETTABLEKS R1 R0 K0
RETURN R1 1
)");

    CHECK_EQ("\n" + compileFunction0(R"(
local t = {}
t['a'] = 2
)"),
        R"(
NEWTABLE R0 0 0
LOADN R1 2
SETTABLEKS R1 R0 K0
RETURN R0 0
)");
}

TEST_CASE("Coverage")
{
    // basic statement coverage
    CHECK_EQ("\n" + compileFunction0Coverage(R"(
print(1)
print(2)
)",
                        1),
        R"(
2: COVERAGE
2: GETIMPORT R0 1
2: LOADN R1 1
2: CALL R0 1 0
3: COVERAGE
3: GETIMPORT R0 1
3: LOADN R1 2
3: CALL R0 1 0
4: RETURN R0 0
)");

    // branching
    CHECK_EQ("\n" + compileFunction0Coverage(R"(
if x then
    print(1)
else
    print(2)
end
)",
                        1),
        R"(
2: COVERAGE
2: GETIMPORT R0 1
2: JUMPIFNOT R0 +6
3: COVERAGE
3: GETIMPORT R0 3
3: LOADN R1 1
3: CALL R0 1 0
7: RETURN R0 0
5: COVERAGE
5: GETIMPORT R0 3
5: LOADN R1 2
5: CALL R0 1 0
7: RETURN R0 0
)");

    // branching with comments
    // note that commented lines don't have COVERAGE insns!
    CHECK_EQ("\n" + compileFunction0Coverage(R"(
if x then
    -- first
    print(1)
else
    -- second
    print(2)
end
)",
                        1),
        R"(
2: COVERAGE
2: GETIMPORT R0 1
2: JUMPIFNOT R0 +6
4: COVERAGE
4: GETIMPORT R0 3
4: LOADN R1 1
4: CALL R0 1 0
9: RETURN R0 0
7: COVERAGE
7: GETIMPORT R0 3
7: LOADN R1 2
7: CALL R0 1 0
9: RETURN R0 0
)");

    // expression coverage for table literals
    // note: duplicate COVERAGE instructions are there since we don't deduplicate expr/stat
    CHECK_EQ("\n" + compileFunction0Coverage(R"(
local c = ...
local t = {
    a = 1,
    b = 2,
    c = c
}
)",
                        2),
        R"(
2: COVERAGE
2: COVERAGE
2: GETVARARGS R0 1
3: COVERAGE
3: COVERAGE
3: DUPTABLE R1 3
4: COVERAGE
4: COVERAGE
4: LOADN R2 1
4: SETTABLEKS R2 R1 K0
5: COVERAGE
5: COVERAGE
5: LOADN R2 2
5: SETTABLEKS R2 R1 K1
6: COVERAGE
6: SETTABLEKS R0 R1 K2
8: RETURN R0 0
)");
}

TEST_CASE("ConstantClosure")
{
    ScopedFastFlag sff("LuauPreloadClosures", true);

    // closures without upvalues are created when bytecode is loaded
    CHECK_EQ("\n" + compileFunction(R"(
return function() end
)",
                        1),
        R"(
DUPCLOSURE R0 K0
RETURN R0 1
)");

    // they can access globals just fine
    CHECK_EQ("\n" + compileFunction(R"(
return function() print("hi") end
)",
                        1),
        R"(
DUPCLOSURE R0 K0
RETURN R0 1
)");

    // if they need upvalues, we can't create them before running the code (but see SharedClosure test)
    CHECK_EQ("\n" + compileFunction(R"(
function test()
    local print = print
    return function() print("hi") end
end
)",
                        1),
        R"(
GETIMPORT R0 1
NEWCLOSURE R1 P0
CAPTURE VAL R0
RETURN R1 1
)");

    if (FFlag::LuauPreloadClosuresFenv)
    {
        // if they don't need upvalues but we sense that environment may be modified, we disable this to avoid fenv-related identity confusion
        CHECK_EQ("\n" + compileFunction(R"(
setfenv(1, {})
return function() print("hi") end
)",
                            1),
            R"(
GETIMPORT R0 1
LOADN R1 1
NEWTABLE R2 0 0
CALL R0 2 0
NEWCLOSURE R0 P0
RETURN R0 1
)");

        // note that fenv analysis isn't flow-sensitive right now, which is sort of a feature
        CHECK_EQ("\n" + compileFunction(R"(
if false then setfenv(1, {}) end
return function() print("hi") end
)",
                            1),
            R"(
NEWCLOSURE R0 P0
RETURN R0 1
)");
    }
}

TEST_CASE("SharedClosure")
{
    ScopedFastFlag sff1("LuauPreloadClosures", true);
    ScopedFastFlag sff2("LuauPreloadClosuresUpval", true);

    // closures can be shared even if functions refer to upvalues, as long as upvalues are top-level
    CHECK_EQ("\n" + compileFunction(R"(
local val = ...

local function foo()
    return function() return val end
end
)",
                        1),
        R"(
DUPCLOSURE R0 K0
CAPTURE UPVAL U0
RETURN R0 1
)");

    // ... as long as the values aren't mutated.
    CHECK_EQ("\n" + compileFunction(R"(
local val = ...

local function foo()
    return function() return val end
end

val = 5
)",
                        1),
        R"(
NEWCLOSURE R0 P0
CAPTURE UPVAL U0
RETURN R0 1
)");

    // making the upvalue non-toplevel disables the optimization since it's likely that it will change
    CHECK_EQ("\n" + compileFunction(R"(
local function foo(val)
    return function() return val end
end
)",
                        1),
        R"(
NEWCLOSURE R1 P0
CAPTURE VAL R0
RETURN R1 1
)");

    // the upvalue analysis is transitive through local functions, which allows for code reuse to not defeat the optimization
    CHECK_EQ("\n" + compileFunction(R"(
local val = ...

local function foo()
    local function bar()
        return val
    end

    return function() return bar() end
end
)",
                        2),
        R"(
DUPCLOSURE R0 K0
CAPTURE UPVAL U0
DUPCLOSURE R1 K1
CAPTURE VAL R0
RETURN R1 1
)");

    // as such, if the upvalue that we reach transitively isn't top-level we fall back to newclosure
    CHECK_EQ("\n" + compileFunction(R"(
local function foo(val)
    local function bar()
        return val
    end

    return function() return bar() end
end
)",
                        2),
        R"(
NEWCLOSURE R1 P0
CAPTURE VAL R0
NEWCLOSURE R2 P1
CAPTURE VAL R1
RETURN R2 1
)");

    // we also allow recursive function captures to share the object, even when it's not top-level
    CHECK_EQ("\n" + compileFunction("function test() local function foo() return foo() end end", 1), R"(
DUPCLOSURE R0 K0
CAPTURE VAL R0
RETURN R0 0
)");

    // multi-level recursive capture where function isn't top-level fails however.
    // note: this should probably be optimized to DUPCLOSURE but doing that requires a different upval tracking flow in the compiler
    CHECK_EQ("\n" + compileFunction(R"(
local function foo()
    local function bar()
        return function() return bar() end
    end
end
)",
                        1),
        R"(
NEWCLOSURE R0 P0
CAPTURE UPVAL U0
RETURN R0 1
)");

    // top level upvalues inside loops should not be shared -- note that the bytecode below only uses NEWCLOSURE
    CHECK_EQ("\n" + compileFunction(R"(
for i=1,10 do
    print(function() return i end)
end

for k,v in pairs(...) do
    print(function() return k end)
end

for i=1,10 do
    local j = i
    print(function() return j end)
end
)",
                        3),
        R"(
LOADN R2 1
LOADN R0 10
LOADN R1 1
FORNPREP R0 +6
GETIMPORT R3 1
NEWCLOSURE R4 P0
CAPTURE VAL R2
CALL R3 1 0
FORNLOOP R0 -6
GETIMPORT R0 3
GETVARARGS R1 -1
CALL R0 -1 3
FORGPREP_NEXT R0 +5
GETIMPORT R5 1
NEWCLOSURE R6 P1
CAPTURE VAL R3
CALL R5 1 0
FORGLOOP_NEXT R0 -6
LOADN R2 1
LOADN R0 10
LOADN R1 1
FORNPREP R0 +7
MOVE R3 R2
GETIMPORT R4 1
NEWCLOSURE R5 P2
CAPTURE VAL R3
CALL R4 1 0
FORNLOOP R0 -7
RETURN R0 0
)");
}

TEST_SUITE_END();
