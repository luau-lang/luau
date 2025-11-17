// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Compiler.h"
#include "Luau/BytecodeBuilder.h"
#include "Luau/StringUtils.h"

#include "luacode.h"

#include "ScopedFlags.h"

#include "doctest.h"

#include <sstream>
#include <string_view>

namespace Luau
{
std::string rep(const std::string& s, size_t n);
}

LUAU_FASTINT(LuauCompileInlineDepth)
LUAU_FASTINT(LuauCompileInlineThreshold)
LUAU_FASTINT(LuauCompileInlineThresholdMaxBoost)
LUAU_FASTINT(LuauCompileLoopUnrollThreshold)
LUAU_FASTINT(LuauCompileLoopUnrollThresholdMaxBoost)
LUAU_FASTINT(LuauRecursionLimit)
LUAU_FASTFLAG(LuauStringConstFolding2)
LUAU_FASTFLAG(LuauCompileTypeofFold)
LUAU_FASTFLAG(LuauInterpStringConstFolding)

using namespace Luau;

static void luauLibraryConstantLookup(const char* library, const char* member, Luau::CompileConstant* constant)
{
    // While 'vector' is built-in, because of LUA_VECTOR_SIZE VM configuration, compiler cannot provide the right default by itself
    if (strcmp(library, "vector") == 0)
    {
        if (strcmp(member, "zero") == 0)
            return Luau::setCompileConstantVector(constant, 0.0f, 0.0f, 0.0f, 0.0f);

        if (strcmp(member, "one") == 0)
            return Luau::setCompileConstantVector(constant, 1.0f, 1.0f, 1.0f, 0.0f);
    }

    if (strcmp(library, "Vector3") == 0)
    {
        if (strcmp(member, "one") == 0)
            return Luau::setCompileConstantVector(constant, 1.0f, 1.0f, 1.0f, 0.0f);

        if (strcmp(member, "xAxis") == 0)
            return Luau::setCompileConstantVector(constant, 1.0f, 0.0f, 0.0f, 0.0f);
    }

    if (strcmp(library, "test") == 0)
    {
        if (strcmp(member, "some_nil") == 0)
            return Luau::setCompileConstantNil(constant);

        if (strcmp(member, "some_boolean") == 0)
            return Luau::setCompileConstantBoolean(constant, true);

        if (strcmp(member, "some_number") == 0)
            return Luau::setCompileConstantNumber(constant, 4.75);

        if (strcmp(member, "some_string") == 0)
            return Luau::setCompileConstantString(constant, "test", 4);
    }
}

static std::string compileFunction(const char* source, uint32_t id, int optimizationLevel = 1, int typeInfoLevel = 0, bool enableVectors = false)
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::CompileOptions options;
    options.optimizationLevel = optimizationLevel;
    options.typeInfoLevel = typeInfoLevel;
    if (enableVectors)
    {
        options.vectorLib = "Vector3";
        options.vectorCtor = "new";
    }

    static const char* kLibrariesWithConstants[] = {"vector", "Vector3", "test", nullptr};
    options.librariesWithKnownMembers = kLibrariesWithConstants;

    options.libraryMemberConstantCb = luauLibraryConstantLookup;

    Luau::compileOrThrow(bcb, source, options);

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

static std::string compileTypeTable(const char* source)
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);

    Luau::CompileOptions opts;
    opts.vectorType = "Vector3";
    opts.typeInfoLevel = 1;
    Luau::compileOrThrow(bcb, source, opts);

    return bcb.dumpTypeInfo();
}

static std::string compileWithRemarks(const char* source)
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Source | Luau::BytecodeBuilder::Dump_Remarks);
    bcb.setDumpSource(source);

    Luau::CompileOptions options;
    options.optimizationLevel = 2;

    Luau::compileOrThrow(bcb, source, options);

    return bcb.dumpSourceRemarks();
}

TEST_SUITE_BEGIN("Compiler");

TEST_CASE("BytecodeIsStable")
{
    // As noted in Bytecode.h, all enums used for bytecode storage and serialization are order-sensitive
    // Adding entries in the middle will typically pass the tests but break compatibility
    // This test codifies this by validating that in each enum, the last (or close-to-last) entry has a fixed encoding

    // This test will need to get occasionally revised to "move" the checked enum entries forward as we ship newer versions
    // When doing so, please add *new* checks for more recent bytecode versions and keep existing checks in place.

    // Bytecode ops (serialized & in-memory)
    CHECK(LOP_FASTCALL2K == 75); // bytecode v1
    CHECK(LOP_JUMPXEQKS == 80);  // bytecode v3

    // Bytecode fastcall ids (serialized & in-memory)
    // Note: these aren't strictly bound to specific bytecode versions, but must monotonically increase to keep backwards compat
    CHECK(LBF_VECTOR == 54);
    CHECK(LBF_TOSTRING == 63);
    CHECK(LBF_BUFFER_WRITEF64 == 77);
    CHECK(LBF_VECTOR_MAX == 88);

    // Bytecode capture type (serialized & in-memory)
    CHECK(LCT_UPVAL == 2); // bytecode v1

    // Bytecode constants (serialized)
    CHECK(LBC_CONSTANT_CLOSURE == 6); // bytecode v1

    // Bytecode type encoding (serialized & in-memory)
    // Note: these *can* change retroactively *if* type version is bumped, but probably shouldn't
    LUAU_ASSERT(LBC_TYPE_BUFFER == 9); // type version 1
}

TEST_CASE("CompileToBytecode")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::compileOrThrow(bcb, "return 5, 6.5");

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
LOADN R0 5
LOADK R1 K0 [6.5]
RETURN R0 2
)");

    CHECK_EQ("\n" + bcb.dumpEverything(), R"(
Function 0 (??):
LOADN R0 5
LOADK R1 K0 [6.5]
RETURN R0 2

)");
}

TEST_CASE("CompileError")
{
    std::string source = "local " + rep("a,", 300) + "a = ...";

    // fails to parse
    std::string bc1 = Luau::compile(source + " !#*$!#$^&!*#&$^*");

    // parses, but fails to compile (too many locals)
    std::string bc2 = Luau::compile(source);

    // 0 acts as a special marker for error bytecode
    CHECK_EQ(bc1[0], 0);
    CHECK_EQ(bc2[0], 0);
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

    CHECK_EQ("\n" + bcb.dumpFunction(1), R"(
DUPCLOSURE R0 K0 ['foo']
RETURN R0 0
)");

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
GETIMPORT R0 2 [math.foo]
CALL R0 0 1
RETURN R0 0
)");

    // direct call into temp
    CHECK_EQ("\n" + compileFunction0("local foo = math.foo(math.bar())"), R"(
GETIMPORT R0 2 [math.foo]
GETIMPORT R1 4 [math.bar]
CALL R1 0 -1
CALL R0 -1 1
RETURN R0 0
)");

    // can't directly call into local since foo might be used as arguments of caller
    CHECK_EQ("\n" + compileFunction0("local foo foo = math.foo(foo)"), R"(
LOADNIL R0
GETIMPORT R1 2 [math.foo]
MOVE R2 R0
CALL R1 1 1
MOVE R0 R1
RETURN R0 0
)");
}

TEST_CASE("ReflectionBytecode")
{
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local part = Instance.new('Part', workspace)
part.Size = Vector3.new(1, 2, 3)
return part.Size.Z * part:GetMass()
)"),
        R"(
GETIMPORT R0 2 [Instance.new]
LOADK R1 K3 ['Part']
GETIMPORT R2 5 [workspace]
CALL R0 2 1
GETIMPORT R1 7 [Vector3.new]
LOADN R2 1
LOADN R3 2
LOADN R4 3
CALL R1 3 1
SETTABLEKS R1 R0 K8 ['Size']
GETTABLEKS R3 R0 K8 ['Size']
GETTABLEKS R2 R3 K9 ['Z']
NAMECALL R3 R0 K10 ['GetMass']
CALL R3 1 1
MUL R1 R2 R3
RETURN R1 1
)"
    );
}

TEST_CASE("ImportCall")
{
    CHECK_EQ("\n" + compileFunction0("return math.max(1, 2)"), R"(
LOADN R1 1
FASTCALL2K 18 R1 K0 L0 [2]
LOADK R2 K0 [2]
GETIMPORT R0 3 [math.max]
CALL R0 2 -1
L0: RETURN R0 -1
)");
}

TEST_CASE("FakeImportCall")
{
    const char* source = "math = {} function math.max() return 0 end function test() return math.max(1, 2) end";

    CHECK_EQ("\n" + compileFunction(source, 1), R"(
GETGLOBAL R1 K0 ['math']
GETTABLEKS R0 R1 K1 ['max']
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
SETGLOBAL R0 K0 ['a']
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
SETTABLEKS R2 R1 K0 ['b']
SETTABLEKS R0 R1 K0 ['b']
RETURN R0 0
)");
}

TEST_CASE("ConcatChainOptimization")
{
    CHECK_EQ("\n" + compileFunction0("local a, b = ...; return a .. b"), R"(
GETVARARGS R0 2
MOVE R3 R0
MOVE R4 R1
CONCAT R2 R3 R4
RETURN R2 1
)");

    CHECK_EQ("\n" + compileFunction0("local a, b, c = ...; return a .. b .. c"), R"(
GETVARARGS R0 3
MOVE R4 R0
MOVE R5 R1
MOVE R6 R2
CONCAT R3 R4 R6
RETURN R3 1
)");

    CHECK_EQ("\n" + compileFunction0("local a, b, c = ...; return (a .. b) .. c"), R"(
GETVARARGS R0 3
MOVE R6 R0
MOVE R7 R1
CONCAT R4 R6 R7
MOVE R5 R2
CONCAT R3 R4 R5
RETURN R3 1
)");
}

TEST_CASE("RepeatLocals")
{
    CHECK_EQ("\n" + compileFunction0("repeat local a a = 5 until a - 4 < 0 or a - 4 >= 0"), R"(
L0: LOADNIL R0
LOADN R0 5
SUBK R1 R0 K0 [4]
LOADN R2 0
JUMPIFLT R1 R2 L1
SUBK R1 R0 K0 [4]
LOADN R2 0
JUMPIFLE R2 R1 L1
JUMPBACK L0
L1: RETURN R0 0
)");
}

TEST_CASE("ForBytecode")
{
    // basic for loop: variable directly refers to internal iteration index (R2)
    CHECK_EQ("\n" + compileFunction0("for i=1,5 do print(i) end"), R"(
LOADN R2 1
LOADN R0 5
LOADN R1 1
FORNPREP R0 L1
L0: GETIMPORT R3 1 [print]
MOVE R4 R2
CALL R3 1 0
FORNLOOP R0 L0
L1: RETURN R0 0
)");

    // when you assign the variable internally, we freak out and copy the variable so that you aren't changing the loop behavior
    CHECK_EQ("\n" + compileFunction0("for i=1,5 do i = 7 print(i) end"), R"(
LOADN R2 1
LOADN R0 5
LOADN R1 1
FORNPREP R0 L1
L0: MOVE R3 R2
LOADN R3 7
GETIMPORT R4 1 [print]
MOVE R5 R3
CALL R4 1 0
FORNLOOP R0 L0
L1: RETURN R0 0
)");

    // basic for-in loop, generic version
    CHECK_EQ("\n" + compileFunction0("for word in string.gmatch(\"Hello Lua user\", \"%a+\") do print(word) end"), R"(
GETIMPORT R0 2 [string.gmatch]
LOADK R1 K3 ['Hello Lua user']
LOADK R2 K4 ['%a+']
CALL R0 2 3
FORGPREP R0 L1
L0: GETIMPORT R5 6 [print]
MOVE R6 R3
CALL R5 1 0
L1: FORGLOOP R0 L0 1
RETURN R0 0
)");

    // basic for-in loop, using inext specialization
    CHECK_EQ("\n" + compileFunction0("for k,v in ipairs({}) do print(k,v) end"), R"(
GETIMPORT R0 1 [ipairs]
NEWTABLE R1 0 0
CALL R0 1 3
FORGPREP_INEXT R0 L1
L0: GETIMPORT R5 3 [print]
MOVE R6 R3
MOVE R7 R4
CALL R5 2 0
L1: FORGLOOP R0 L0 2 [inext]
RETURN R0 0
)");

    // basic for-in loop, using next specialization
    CHECK_EQ("\n" + compileFunction0("for k,v in pairs({}) do print(k,v) end"), R"(
GETIMPORT R0 1 [pairs]
NEWTABLE R1 0 0
CALL R0 1 3
FORGPREP_NEXT R0 L1
L0: GETIMPORT R5 3 [print]
MOVE R6 R3
MOVE R7 R4
CALL R5 2 0
L1: FORGLOOP R0 L0 2
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("for k,v in next,{} do print(k,v) end"), R"(
GETIMPORT R0 1 [next]
NEWTABLE R1 0 0
LOADNIL R2
FORGPREP_NEXT R0 L1
L0: GETIMPORT R5 3 [print]
MOVE R6 R3
MOVE R7 R4
CALL R5 2 0
L1: FORGLOOP R0 L0 2
RETURN R0 0
)");
}

TEST_CASE("ForBytecodeBuiltin")
{
    // we generally recognize builtins like pairs/ipairs and emit special opcodes
    CHECK_EQ("\n" + compileFunction0("for k,v in ipairs({}) do end"), R"(
GETIMPORT R0 1 [ipairs]
NEWTABLE R1 0 0
CALL R0 1 3
FORGPREP_INEXT R0 L0
L0: FORGLOOP R0 L0 2 [inext]
RETURN R0 0
)");

    // ... even if they are using a local variable
    CHECK_EQ("\n" + compileFunction0("local ip = ipairs for k,v in ip({}) do end"), R"(
GETIMPORT R0 1 [ipairs]
MOVE R1 R0
NEWTABLE R2 0 0
CALL R1 1 3
FORGPREP_INEXT R1 L0
L0: FORGLOOP R1 L0 2 [inext]
RETURN R0 0
)");

    // ... even when it's an upvalue
    CHECK_EQ("\n" + compileFunction0("local ip = ipairs function foo() for k,v in ip({}) do end end"), R"(
GETUPVAL R0 0
NEWTABLE R1 0 0
CALL R0 1 3
FORGPREP_INEXT R0 L0
L0: FORGLOOP R0 L0 2 [inext]
RETURN R0 0
)");

    // but if it's reassigned then all bets are off
    CHECK_EQ("\n" + compileFunction0("local ip = ipairs ip = pairs for k,v in ip({}) do end"), R"(
GETIMPORT R0 1 [ipairs]
GETIMPORT R0 3 [pairs]
MOVE R1 R0
NEWTABLE R2 0 0
CALL R1 1 3
FORGPREP R1 L0
L0: FORGLOOP R1 L0 2
RETURN R0 0
)");

    // or if the global is hijacked
    CHECK_EQ("\n" + compileFunction0("ipairs = pairs for k,v in ipairs({}) do end"), R"(
GETIMPORT R0 1 [pairs]
SETGLOBAL R0 K2 ['ipairs']
GETGLOBAL R0 K2 ['ipairs']
NEWTABLE R1 0 0
CALL R0 1 3
FORGPREP R0 L0
L0: FORGLOOP R0 L0 2
RETURN R0 0
)");

    // or if we don't even know the global to begin with
    CHECK_EQ("\n" + compileFunction0("for k,v in unknown({}) do end"), R"(
GETIMPORT R0 1 [unknown]
NEWTABLE R1 0 0
CALL R0 1 3
FORGPREP R0 L0
L0: FORGLOOP R0 L0 2
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
SETTABLEKS R1 R0 K0 ['a']
LOADN R1 2
SETTABLEKS R1 R0 K1 ['b']
LOADN R1 3
SETTABLEKS R1 R0 K2 ['c']
RETURN R0 1
)");

    // literals+array
    CHECK_EQ("\n" + compileFunction0("return {a=1,b=2,3,4}"), R"(
NEWTABLE R0 2 2
LOADN R3 1
SETTABLEKS R3 R0 K0 ['a']
LOADN R3 2
SETTABLEKS R3 R0 K1 ['b']
LOADN R1 3
LOADN R2 4
SETLIST R0 R1 2 [1]
RETURN R0 1
)");

    // expression assignment
    CHECK_EQ("\n" + compileFunction0("a = 7 return {[a]=42}"), R"(
LOADN R0 7
SETGLOBAL R0 K0 ['a']
NEWTABLE R0 1 0
GETGLOBAL R1 K0 ['a']
LOADN R2 42
SETTABLE R2 R0 R1
RETURN R0 1
)");

    // table template caching; two DUPTABLES out of three use the same slot. Note that caching is order dependent
    CHECK_EQ("\n" + compileFunction0("return {a=1,b=2},{b=3,a=4},{a=5,b=6}"), R"(
DUPTABLE R0 2
LOADN R1 1
SETTABLEKS R1 R0 K0 ['a']
LOADN R1 2
SETTABLEKS R1 R0 K1 ['b']
DUPTABLE R1 3
LOADN R2 3
SETTABLEKS R2 R1 K1 ['b']
LOADN R2 4
SETTABLEKS R2 R1 K0 ['a']
DUPTABLE R2 2
LOADN R3 5
SETTABLEKS R3 R2 K0 ['a']
LOADN R3 6
SETTABLEKS R3 R2 K1 ['b']
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
SETTABLEKS R1 R0 K0 ['key']
LOADN R1 2
SETTABLEKS R1 R0 K1 ['value']
LOADN R1 42
SETTABLEN R1 R0 1
RETURN R0 1
)");
}

TEST_CASE("TableLiteralsIndexConstant")
{
    // validate that we use SETTTABLEKS for constant variable keys
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b = "key", "value"
        return {[a] = 42, [b] = 0}
)"),
        R"(
NEWTABLE R0 2 0
LOADN R1 42
SETTABLEKS R1 R0 K0 ['key']
LOADN R1 0
SETTABLEKS R1 R0 K1 ['value']
RETURN R0 1
)"
    );

    // validate that we use SETTABLEN for constant variable keys *and* that we predict array size
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b = 1, 2
        return {[a] = 42, [b] = 0}
)"),
        R"(
NEWTABLE R0 0 2
LOADN R1 42
SETTABLEN R1 R0 1
LOADN R1 0
SETTABLEN R1 R0 2
RETURN R0 1
)"
    );
}

TEST_CASE("TableSizePredictionBasic")
{
    CHECK_EQ(
        "\n" + compileFunction0(R"(
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
)"),
        R"(
NEWTABLE R0 16 0
LOADN R1 1
SETTABLEKS R1 R0 K0 ['a']
LOADN R1 1
SETTABLEKS R1 R0 K1 ['b']
LOADN R1 1
SETTABLEKS R1 R0 K2 ['c']
LOADN R1 1
SETTABLEKS R1 R0 K3 ['d']
LOADN R1 1
SETTABLEKS R1 R0 K4 ['e']
LOADN R1 1
SETTABLEKS R1 R0 K5 ['f']
LOADN R1 1
SETTABLEKS R1 R0 K6 ['g']
LOADN R1 1
SETTABLEKS R1 R0 K7 ['h']
LOADN R1 1
SETTABLEKS R1 R0 K8 ['i']
RETURN R0 0
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(
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
)"),
        R"(
NEWTABLE R0 1 0
LOADN R1 1
SETTABLEKS R1 R0 K0 ['x']
LOADN R1 2
SETTABLEKS R1 R0 K0 ['x']
LOADN R1 3
SETTABLEKS R1 R0 K0 ['x']
LOADN R1 4
SETTABLEKS R1 R0 K0 ['x']
LOADN R1 5
SETTABLEKS R1 R0 K0 ['x']
LOADN R1 6
SETTABLEKS R1 R0 K0 ['x']
LOADN R1 7
SETTABLEKS R1 R0 K0 ['x']
LOADN R1 8
SETTABLEKS R1 R0 K0 ['x']
LOADN R1 9
SETTABLEKS R1 R0 K0 ['x']
RETURN R0 0
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(
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
)"),
        R"(
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
)"
    );
}

TEST_CASE("TableSizePredictionObject")
{
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local t = {}
t.field = 1
function t:getfield()
    return self.field
end
return t
)",
                   1
               ),
        R"(
NEWTABLE R0 2 0
LOADN R1 1
SETTABLEKS R1 R0 K0 ['field']
DUPCLOSURE R1 K1 ['getfield']
SETTABLEKS R1 R0 K2 ['getfield']
RETURN R0 1
)"
    );
}

TEST_CASE("TableSizePredictionSetMetatable")
{
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local t = setmetatable({}, nil)
t.field1 = 1
t.field2 = 2
return t
)"),
        R"(
NEWTABLE R1 2 0
FASTCALL2K 61 R1 K0 L0 [nil]
LOADK R2 K0 [nil]
GETIMPORT R0 2 [setmetatable]
CALL R0 2 1
L0: LOADN R1 1
SETTABLEKS R1 R0 K3 ['field1']
LOADN R1 2
SETTABLEKS R1 R0 K4 ['field2']
RETURN R0 1
)"
    );
}

TEST_CASE("TableSizePredictionLoop")
{
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local t = {}
for i=1,4 do
    t[i] = 0
end
return t
)"),
        R"(
NEWTABLE R0 0 4
LOADN R3 1
LOADN R1 4
LOADN R2 1
FORNPREP R1 L1
L0: LOADN R4 0
SETTABLE R4 R0 R3
FORNLOOP R1 L0
L1: RETURN R0 1
)"
    );
}

TEST_CASE("ReflectionEnums")
{
    CHECK_EQ("\n" + compileFunction0("return Enum.EasingStyle.Linear"), R"(
GETIMPORT R0 3 [Enum.EasingStyle.Linear]
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
SETTABLEKS R1 R0 K0 ['_tweakingTooltipFrame']
RETURN R0 0
)");
}

TEST_CASE("ConditionalBasic")
{
    CHECK_EQ("\n" + compileFunction0("local a = ... if a then return 5 end"), R"(
GETVARARGS R0 1
JUMPIFNOT R0 L0
LOADN R1 5
RETURN R1 1
L0: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = ... if not a then return 5 end"), R"(
GETVARARGS R0 1
JUMPIF R0 L0
LOADN R1 5
RETURN R1 1
L0: RETURN R0 0
)");
}

TEST_CASE("ConditionalCompare")
{
    CHECK_EQ("\n" + compileFunction0("local a, b = ... if a < b then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFNOTLT R0 R1 L0
LOADN R2 5
RETURN R2 1
L0: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = ... if a <= b then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFNOTLE R0 R1 L0
LOADN R2 5
RETURN R2 1
L0: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = ... if a > b then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFNOTLT R1 R0 L0
LOADN R2 5
RETURN R2 1
L0: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = ... if a >= b then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFNOTLE R1 R0 L0
LOADN R2 5
RETURN R2 1
L0: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = ... if a == b then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFNOTEQ R0 R1 L0
LOADN R2 5
RETURN R2 1
L0: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = ... if a ~= b then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFEQ R0 R1 L0
LOADN R2 5
RETURN R2 1
L0: RETURN R0 0
)");
}

TEST_CASE("ConditionalNot")
{
    CHECK_EQ("\n" + compileFunction0("local a, b = ... if not (not (a < b)) then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFNOTLT R0 R1 L0
LOADN R2 5
RETURN R2 1
L0: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b = ... if not (not (not (a < b))) then return 5 end"), R"(
GETVARARGS R0 2
JUMPIFLT R0 R1 L0
LOADN R2 5
RETURN R2 1
L0: RETURN R0 0
)");
}

TEST_CASE("ConditionalAndOr")
{
    CHECK_EQ("\n" + compileFunction0("local a, b, c = ... if a < b and b < c then return 5 end"), R"(
GETVARARGS R0 3
JUMPIFNOTLT R0 R1 L0
JUMPIFNOTLT R1 R2 L0
LOADN R3 5
RETURN R3 1
L0: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a, b, c = ... if a < b or b < c then return 5 end"), R"(
GETVARARGS R0 3
JUMPIFLT R0 R1 L0
JUMPIFNOTLT R1 R2 L1
L0: LOADN R3 5
RETURN R3 1
L1: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a,b,c,d = ... if (a or b) and not (c and d) then return 5 end"), R"(
GETVARARGS R0 4
JUMPIF R0 L0
JUMPIFNOT R1 L2
L0: JUMPIFNOT R2 L1
JUMPIF R3 L2
L1: LOADN R4 5
RETURN R4 1
L2: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a,b,c = ... if a or not b or c then return 5 end"), R"(
GETVARARGS R0 3
JUMPIF R0 L0
JUMPIFNOT R1 L0
JUMPIFNOT R2 L1
L0: LOADN R3 5
RETURN R3 1
L1: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a,b,c = ... if a and not b and c then return 5 end"), R"(
GETVARARGS R0 3
JUMPIFNOT R0 L0
JUMPIF R1 L0
JUMPIFNOT R2 L0
LOADN R3 5
RETURN R3 1
L0: RETURN R0 0
)");
}

TEST_CASE("AndOr")
{
    // codegen for constant, local, global for and
    CHECK_EQ("\n" + compileFunction0("local a = 1 a = a and 2 return a"), R"(
LOADN R0 1
ANDK R0 R0 K0 [2]
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
SETGLOBAL R1 K0 ['b']
MOVE R1 R0
JUMPIFNOT R1 L0
GETGLOBAL R1 K0 ['b']
L0: MOVE R0 R1
RETURN R0 1
)");

    // codegen for constant, local, global for or
    CHECK_EQ("\n" + compileFunction0("local a = 1 a = a or 2 return a"), R"(
LOADN R0 1
ORK R0 R0 K0 [2]
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
SETGLOBAL R1 K0 ['b']
MOVE R1 R0
JUMPIF R1 L0
GETGLOBAL R1 K0 ['b']
L0: MOVE R0 R1
RETURN R0 1
)");

    // codegen without a temp variable for and/or when we know we can assign directly into the target register
    // note: `a = a` assignment is to disable constant folding for testing purposes
    CHECK_EQ("\n" + compileFunction0("local a = 1 a = a b = 2 local c = a and b return c"), R"(
LOADN R0 1
LOADN R1 2
SETGLOBAL R1 K0 ['b']
MOVE R1 R0
JUMPIFNOT R1 L0
GETGLOBAL R1 K0 ['b']
L0: RETURN R1 1
)");

    CHECK_EQ("\n" + compileFunction0("local a = 1 a = a b = 2 local c = a or b return c"), R"(
LOADN R0 1
LOADN R1 2
SETGLOBAL R1 K0 ['b']
MOVE R1 R0
JUMPIF R1 L0
GETGLOBAL R1 K0 ['b']
L0: RETURN R1 1
)");
}

TEST_CASE("AndOrFoldLeft")
{
    // constant folding and/or expression is possible even if just the left hand is constant
    CHECK_EQ("\n" + compileFunction0("local a = false return a and b"), R"(
LOADB R0 0
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("local a = true return a or b"), R"(
LOADB R0 1
RETURN R0 1
)");

    // if right hand side is constant we can't constant fold the entire expression
    CHECK_EQ("\n" + compileFunction0("local a = false return b and a"), R"(
GETIMPORT R1 2 [b]
ANDK R0 R1 K0 [false]
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("local a = true return b or a"), R"(
GETIMPORT R1 2 [b]
ORK R0 R1 K0 [true]
RETURN R0 1
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
GETIMPORT R2 2 [verticalGradientTurbulence]
SUBRK R1 K0 [1] R2
GETIMPORT R3 5 [waterLevel]
ADDK R2 R3 K3 [0.014999999999999999]
JUMPIFNOTLT R1 R2 L0
GETIMPORT R0 9 [Enum.Material.Sand]
JUMPIF R0 L2
L0: GETIMPORT R1 11 [sandbank]
LOADN R2 0
JUMPIFNOTLT R2 R1 L1
GETIMPORT R1 11 [sandbank]
LOADN R2 1
JUMPIFNOTLT R1 R2 L1
GETIMPORT R0 9 [Enum.Material.Sand]
JUMPIF R0 L2
L1: GETIMPORT R0 13 [Enum.Material.Sandstone]
L2: RETURN R0 1
)");
}

TEST_CASE("IfElseExpression")
{
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

    // codegen for a true constant condition with non-constant expressions
    CHECK_EQ("\n" + compileFunction0("return if true then {} else error()"), R"(
NEWTABLE R0 0 0
RETURN R0 1
)");

    // codegen for a false constant condition with non-constant expressions
    CHECK_EQ("\n" + compileFunction0("return if false then error() else {}"), R"(
NEWTABLE R0 0 0
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
GETIMPORT R1 1 [condition]
JUMPIFNOT R1 L0
LOADN R0 10
RETURN R0 1
L0: LOADN R0 20
RETURN R0 1
)");

    // codegen for a non-constant condition using an assignment
    CHECK_EQ("\n" + compileFunction0("result = if condition then 10 else 20"), R"(
GETIMPORT R1 1 [condition]
JUMPIFNOT R1 L0
LOADN R0 10
JUMP L1
L0: LOADN R0 20
L1: SETGLOBAL R0 K2 ['result']
RETURN R0 0
)");

    // codegen for a non-constant condition using an assignment to a local variable
    CHECK_EQ("\n" + compileFunction0("local result = if condition then 10 else 20"), R"(
GETIMPORT R1 1 [condition]
JUMPIFNOT R1 L0
LOADN R0 10
RETURN R0 0
L0: LOADN R0 20
RETURN R0 0
)");

    // codegen for an if-else expression with multiple elseif's
    CHECK_EQ("\n" + compileFunction0("result = if condition1 then 10 elseif condition2 then 20 elseif condition3 then 30 else 40"), R"(
GETIMPORT R1 1 [condition1]
JUMPIFNOT R1 L0
LOADN R0 10
JUMP L3
L0: GETIMPORT R1 3 [condition2]
JUMPIFNOT R1 L1
LOADN R0 20
JUMP L3
L1: GETIMPORT R1 5 [condition3]
JUMPIFNOT R1 L2
LOADN R0 30
JUMP L3
L2: LOADN R0 40
L3: SETGLOBAL R0 K6 ['result']
RETURN R0 0
)");
}

TEST_CASE("UnaryBasic")
{
    CHECK_EQ("\n" + compileFunction0("local a = ... return not a"), R"(
GETVARARGS R0 1
NOT R1 R0
RETURN R1 1
)");

    CHECK_EQ("\n" + compileFunction0("local a = ... return -a"), R"(
GETVARARGS R0 1
MINUS R1 R0
RETURN R1 1
)");

    CHECK_EQ("\n" + compileFunction0("local a = ... return #a"), R"(
GETVARARGS R0 1
LENGTH R1 R0
RETURN R1 1
)");
}

TEST_CASE("InterpStringWithNoExpressions")
{
    CHECK_EQ(compileFunction0(R"(return "hello")"), compileFunction0("return `hello`"));
}

TEST_CASE("InterpStringZeroCost")
{
    CHECK_EQ(
        "\n" + compileFunction0(R"(local _ = `hello, {42}!`)"),
        R"(
LOADK R1 K0 ['hello, %*!']
LOADN R3 42
NAMECALL R1 R1 K1 ['format']
CALL R1 2 1
MOVE R0 R1
RETURN R0 0
)"
    );
}

TEST_CASE("InterpStringRegisterCleanup")
{
    CHECK_EQ(
        "\n" + compileFunction0(R"(
            local a, b, c = nil, "um", "uh oh"
            a = `foo{42}`
            print(a)
        )"),

        R"(
LOADNIL R0
LOADK R1 K0 ['um']
LOADK R2 K1 ['uh oh']
LOADK R3 K2 ['foo%*']
LOADN R5 42
NAMECALL R3 R3 K3 ['format']
CALL R3 2 1
MOVE R0 R3
GETIMPORT R3 5 [print]
MOVE R4 R0
CALL R3 1 0
RETURN R0 0
)"
    );
}

TEST_CASE("InterpStringRegisterLimit")
{
    CHECK_THROWS_AS(compileFunction0(("local a = `" + rep("{1}", 254) + "`").c_str()), std::exception);
    CHECK_THROWS_AS(compileFunction0(("local a = `" + rep("{1}", 253) + "`").c_str()), std::exception);
}

TEST_CASE("InterpStringConstFold")
{
    ScopedFastFlag sff{FFlag::LuauInterpStringConstFolding, true};

    CHECK_EQ(
        "\n" + compileFunction0(R"(local empty = ""; return `{empty}`)"),
        R"(
LOADK R0 K0 ['']
RETURN R0 1
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(local world = "world"; return `hello, {world}!`)"),
        R"(
LOADK R0 K0 ['hello, world!']
RETURN R0 1
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(local not_string = 42; local world = "world"; return `hello, {world} {not_string}!`)"),
        R"(
LOADK R1 K0 ['hello, world %*!']
LOADN R3 42
NAMECALL R1 R1 K1 ['format']
CALL R1 2 1
MOVE R0 R1
RETURN R0 1
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(local not_string = 42; local str = "%s%s%s"; return `hello, {str} {not_string}!`)"),
        R"(
LOADK R1 K0 ['hello, %%s%%s%%s %*!']
LOADN R3 42
NAMECALL R1 R1 K1 ['format']
CALL R1 2 1
MOVE R0 R1
RETURN R0 1
)"
    );
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

TEST_CASE("ConstantFoldVectorArith")
{
    CHECK_EQ("\n" + compileFunction("local n = 2; local a, b = vector.create(1, 2, 3), vector.create(2, 4, 8); return a + b", 0, 2), R"(
LOADK R0 K0 [3, 6, 11]
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction("local n = 2; local a, b = vector.create(1, 2, 3), vector.create(2, 4, 8); return a - b", 0, 2), R"(
LOADK R0 K0 [-1, -2, -5]
RETURN R0 1
)");

    // Multiplication by infinity cannot be folded as it creates a non-zero value in W
    CHECK_EQ(
        "\n" + compileFunction(
                   "local n = 2; local a, b = vector.create(1, 2, 3), vector.create(2, 4, 8); return a * n, a * b, n * b, a * math.huge", 0, 2
               ),
        R"(
LOADK R0 K0 [2, 4, 6]
LOADK R1 K1 [2, 8, 24]
LOADK R2 K2 [4, 8, 16]
LOADK R4 K4 [1, 2, 3]
MULK R3 R4 K3 [inf]
RETURN R0 4
)"
    );

    // Divisions creating an infinity in W cannot be constant-folded
    CHECK_EQ(
        "\n" + compileFunction(
                   "local n = 2; local a, b = vector.create(1, 2, 3), vector.create(2, 4, 8); return a / n, a / b, n / b, a / math.huge", 0, 2
               ),
        R"(
LOADK R0 K0 [0.5, 1, 1.5]
LOADK R2 K1 [1, 2, 3]
LOADK R3 K2 [2, 4, 8]
DIV R1 R2 R3
LOADK R3 K2 [2, 4, 8]
DIVRK R2 K3 [2] R3
LOADK R3 K4 [0, 0, 0]
RETURN R0 4
)"
    );

    // Divisions creating an infinity in W cannot be constant-folded
    CHECK_EQ(
        "\n" + compileFunction("local n = 2; local a, b = vector.create(1, 2, 3), vector.create(2, 4, 8); return a // n, a // b, n // b", 0, 2),
        R"(
LOADK R0 K0 [0, 1, 1]
LOADK R2 K1 [1, 2, 3]
LOADK R3 K2 [2, 4, 8]
IDIV R1 R2 R3
LOADN R3 2
LOADK R4 K2 [2, 4, 8]
IDIV R2 R3 R4
RETURN R0 3
)"
    );

    CHECK_EQ("\n" + compileFunction("local a = vector.create(1, 2, 3); return -a", 0, 2), R"(
LOADK R0 K0 [-1, -2, -3]
RETURN R0 1
)");
}

TEST_CASE("ConstantFoldVectorArith4Wide")
{
    CHECK_EQ("\n" + compileFunction("local n = 2; local a, b = vector.create(1, 2, 3, 4), vector.create(2, 4, 8, 1); return a + b", 0, 2), R"(
LOADK R0 K0 [3, 6, 11, 5]
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction("local n = 2; local a, b = vector.create(1, 2, 3, 4), vector.create(2, 4, 8, 1); return a - b", 0, 2), R"(
LOADK R0 K0 [-1, -2, -5, 3]
RETURN R0 1
)");

    CHECK_EQ(
        "\n" + compileFunction(
                   "local n = 2; local a, b = vector.create(1, 2, 3, 4), vector.create(2, 4, 8, 1); return a * n, a * b, n * b, a * math.huge", 0, 2
               ),
        R"(
LOADK R0 K0 [2, 4, 6, 8]
LOADK R1 K1 [2, 8, 24, 4]
LOADK R2 K2 [4, 8, 16, 2]
LOADK R3 K3 [inf, inf, inf, inf]
RETURN R0 4
)"
    );

    CHECK_EQ(
        "\n" + compileFunction(
                   "local n = 2; local a, b = vector.create(1, 2, 3, 4), vector.create(2, 4, 8, 1); return a / n, a / b, n / b, a / math.huge", 0, 2
               ),
        R"(
LOADK R0 K0 [0.5, 1, 1.5, 2]
LOADK R1 K1 [0.5, 0.5, 0.375, 4]
LOADK R2 K2 [1, 0.5, 0.25, 2]
LOADK R3 K3 [0, 0, 0]
RETURN R0 4
)"
    );

    CHECK_EQ(
        "\n" + compileFunction("local n = 2; local a, b = vector.create(1, 2, 3, 4), vector.create(2, 4, 8, 1); return a // n, a // b, n // b", 0, 2),
        R"(
LOADK R0 K0 [0, 1, 1, 2]
LOADK R1 K1 [0, 0, 0, 4]
LOADK R2 K2 [1, 0, 0, 2]
RETURN R0 3
)"
    );

    CHECK_EQ("\n" + compileFunction("local a = vector.create(1, 2, 3, 4); return -a", 0, 2), R"(
LOADK R0 K0 [-1, -2, -3, -4]
RETURN R0 1
)");
}

TEST_CASE("ConstantFoldStringLen")
{
    CHECK_EQ("\n" + compileFunction0("return #'string', #'', #'a', #('b')"), R"(
LOADN R0 6
LOADN R1 0
LOADN R2 1
LOADN R3 1
RETURN R0 4
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
ADDK R2 R0 K0 [1]
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
GETIMPORT R0 1 [a]
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
GETIMPORT R0 1 [a]
RETURN R0 1
)");

    // constant fold parts in chains of and/or statements
    CHECK_EQ("\n" + compileFunction0("return a and true and b"), R"(
GETIMPORT R0 1 [a]
JUMPIFNOT R0 L0
GETIMPORT R0 3 [b]
L0: RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction0("return a or false or b"), R"(
GETIMPORT R0 1 [a]
JUMPIF R0 L0
GETIMPORT R0 3 [b]
L0: RETURN R0 1
)");
}

TEST_CASE("ConstantFoldConditionalAndOr")
{
    CHECK_EQ("\n" + compileFunction0("local a = ... if false or a then print(1) end"), R"(
GETVARARGS R0 1
JUMPIFNOT R0 L0
GETIMPORT R1 1 [print]
LOADN R2 1
CALL R1 1 0
L0: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = ... if not (false or a) then print(1) end"), R"(
GETVARARGS R0 1
JUMPIF R0 L0
GETIMPORT R1 1 [print]
LOADN R2 1
CALL R1 1 0
L0: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = ... if true and a then print(1) end"), R"(
GETVARARGS R0 1
JUMPIFNOT R0 L0
GETIMPORT R1 1 [print]
LOADN R2 1
CALL R1 1 0
L0: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = ... if not (true and a) then print(1) end"), R"(
GETVARARGS R0 1
JUMPIF R0 L0
GETIMPORT R1 1 [print]
LOADN R2 1
CALL R1 1 0
L0: RETURN R0 0
)");
}

TEST_CASE("ConstantFoldFlowControl")
{
    // if
    CHECK_EQ("\n" + compileFunction0("if true then print(1) end"), R"(
GETIMPORT R0 1 [print]
LOADN R1 1
CALL R0 1 0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("if false then print(1) end"), R"(
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("if true then print(1) else print(2) end"), R"(
GETIMPORT R0 1 [print]
LOADN R1 1
CALL R0 1 0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("if false then print(1) else print(2) end"), R"(
GETIMPORT R0 1 [print]
LOADN R1 2
CALL R0 1 0
RETURN R0 0
)");

    // while
    CHECK_EQ("\n" + compileFunction0("while true do print(1) end"), R"(
L0: GETIMPORT R0 1 [print]
LOADN R1 1
CALL R0 1 0
JUMPBACK L0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("while false do print(1) end"), R"(
RETURN R0 0
)");

    // repeat
    CHECK_EQ("\n" + compileFunction0("repeat print(1) until true"), R"(
GETIMPORT R0 1 [print]
LOADN R1 1
CALL R0 1 0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("repeat print(1) until false"), R"(
L0: GETIMPORT R0 1 [print]
LOADN R1 1
CALL R0 1 0
JUMPBACK L0
RETURN R0 0
)");

    // there's an odd case in repeat..until compilation where we evaluate the expression that is always false for side-effects of the left hand side
    CHECK_EQ("\n" + compileFunction0("repeat print(1) until five and false"), R"(
L0: GETIMPORT R0 1 [print]
LOADN R1 1
CALL R0 1 0
GETIMPORT R0 3 [five]
JUMPIFNOT R0 L1
L1: JUMPBACK L0
RETURN R0 0
)");
}

TEST_CASE("LoopBreak")
{
    // default codegen: compile breaks as unconditional jumps
    CHECK_EQ("\n" + compileFunction0("while true do if math.random() < 0.5 then break else end end"), R"(
L0: GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFNOTLT R0 R1 L1
RETURN R0 0
L1: JUMPBACK L0
RETURN R0 0
)");

    // optimization: if then body is a break statement, flip the branches
    CHECK_EQ("\n" + compileFunction0("while true do if math.random() < 0.5 then break end end"), R"(
L0: GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R0 R1 L1
JUMPBACK L0
L1: RETURN R0 0
)");
}

TEST_CASE("LoopContinue")
{
    // default codegen: compile continue as unconditional jumps
    CHECK_EQ("\n" + compileFunction0("repeat if math.random() < 0.5 then continue else end break until false error()"), R"(
L0: GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFNOTLT R0 R1 L2
JUMP L1
JUMP L2
L1: JUMPBACK L0
L2: GETIMPORT R0 5 [error]
CALL R0 0 0
RETURN R0 0
)");

    // optimization: if then body is a continue statement, flip the branches
    CHECK_EQ("\n" + compileFunction0("repeat if math.random() < 0.5 then continue end break until false error()"), R"(
L0: GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R0 R1 L1
JUMP L2
L1: JUMPBACK L0
L2: GETIMPORT R0 5 [error]
CALL R0 0 0
RETURN R0 0
)");
}

TEST_CASE("LoopContinueUntil")
{
    // it's valid to use locals defined inside the loop in until expression if they're defined before continue
    CHECK_EQ("\n" + compileFunction0("repeat local r = math.random() if r > 0.5 then continue end r = r + 0.3 until r < 0.5"), R"(
L0: GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R1 R0 L1
ADDK R0 R0 K4 [0.29999999999999999]
L1: LOADK R1 K3 [0.5]
JUMPIFLT R0 R1 L2
JUMPBACK L0
L2: RETURN R0 0
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
            std::string(e.what()), "Local rr used in the repeat..until condition is undefined because continue statement on line 5 jumps over it"
        );
    }

    // but it's okay if continue is inside a non-repeat..until loop, or inside a loop that doesn't use the local (here `continue` just terminates
    // inner loop)
    CHECK_EQ(
        "\n" +
            compileFunction0("repeat local r = math.random() repeat if r > 0.5 then continue end r = r - 0.1 until true r = r + 0.3 until r < 0.5"),
        R"(
L0: GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R1 R0 L1
SUBK R0 R0 K4 [0.10000000000000001]
L1: ADDK R0 R0 K5 [0.29999999999999999]
LOADK R1 K3 [0.5]
JUMPIFLT R0 R1 L2
JUMPBACK L0
L2: RETURN R0 0
)"
    );

    // and it's also okay to use a local defined in the until expression as long as it's inside a function!
    CHECK_EQ(
        "\n" + compileFunction(
                   "repeat local r = math.random() if r > 0.5 then continue end r = r + 0.3 until (function() local a = r return a < 0.5 end)()", 1
               ),
        R"(
L0: GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R1 R0 L1
ADDK R0 R0 K4 [0.29999999999999999]
L1: NEWCLOSURE R1 P0
CAPTURE REF R0
CALL R1 0 1
JUMPIF R1 L2
CLOSEUPVALS R0
JUMPBACK L0
L2: CLOSEUPVALS R0
RETURN R0 0
)"
    );

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
            std::string(e.what()), "Local rr used in the repeat..until condition is undefined because continue statement on line 5 jumps over it"
        );
    }

    // unless that upvalue is from an outer scope
    CHECK_EQ(
        "\n" + compileFunction0(
                   "local stop = false stop = true function test() repeat local r = math.random() if r > 0.5 then "
                   "continue end r = r + 0.3 until stop or r < 0.5 end"
               ),
        R"(
L0: GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R1 R0 L1
ADDK R0 R0 K4 [0.29999999999999999]
L1: GETUPVAL R1 0
JUMPIF R1 L2
LOADK R1 K3 [0.5]
JUMPIFLT R0 R1 L2
JUMPBACK L0
L2: RETURN R0 0
)"
    );

    // including upvalue references from a function expression
    CHECK_EQ(
        "\n" + compileFunction(
                   "local stop = false stop = true function test() repeat local r = math.random() if r > 0.5 then continue "
                   "end r = r + 0.3 until (function() return stop or r < 0.5 end)() end",
                   1
               ),
        R"(
L0: GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R1 R0 L1
ADDK R0 R0 K4 [0.29999999999999999]
L1: NEWCLOSURE R1 P0
CAPTURE UPVAL U0
CAPTURE REF R0
CALL R1 0 1
JUMPIF R1 L2
CLOSEUPVALS R0
JUMPBACK L0
L2: CLOSEUPVALS R0
RETURN R0 0
)"
    );
}

TEST_CASE("LoopContinueIgnoresImplicitConstant")
{
    // this used to crash the compiler :(
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local _
repeat
continue
until not _
)"),
        R"(
RETURN R0 0
RETURN R0 0
)"
    );
}

TEST_CASE("LoopContinueIgnoresExplicitConstant")
{
    // Constants do not allocate locals and 'continue' validation should skip them if their lifetime already started
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local c = true
repeat
    continue
until c
)"),
        R"(
RETURN R0 0
RETURN R0 0
)"
    );
}

TEST_CASE("LoopContinueRespectsExplicitConstant")
{
    // If local lifetime hasn't started, even if it's a constant that will not receive an allocation, it cannot be jumped over
    try
    {
        Luau::BytecodeBuilder bcb;
        Luau::compileOrThrow(bcb, R"(
repeat
    do continue end

    local c = true
until c
)");

        CHECK(!"Expected CompileError");
    }
    catch (Luau::CompileError& e)
    {
        CHECK_EQ(e.getLocation().begin.line + 1, 6);
        CHECK_EQ(
            std::string(e.what()), "Local c used in the repeat..until condition is undefined because continue statement on line 3 jumps over it"
        );
    }
}

TEST_CASE("LoopContinueIgnoresImplicitConstantAfterInline")
{
    // Inlining might also replace some locals with constants instead of allocating them
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function inline(f)
    repeat
        continue
    until f
end

local function test(...)
    inline(true)
end

test()
)",
                   1,
                   2
               ),
        R"(
RETURN R0 0
RETURN R0 0
)"
    );
}

TEST_CASE("LoopContinueCorrectlyHandlesImplicitConstantAfterUnroll")
{
    ScopedFastInt sfi(FInt::LuauCompileLoopUnrollThreshold, 200);

    // access to implicit constant that depends on the unrolled loop constant is still invalid even though we can constant-propagate it
    try
    {
        compileFunction(
            R"(
for i = 1, 2 do
    s()
    repeat
        if i == 2 then
            continue
        end
        local x = i == 1 or a
    until f(x)
end
)",
            0,
            2
        );

        CHECK(!"Expected CompileError");
    }
    catch (Luau::CompileError& e)
    {
        CHECK_EQ(e.getLocation().begin.line + 1, 9);
        CHECK_EQ(
            std::string(e.what()), "Local x used in the repeat..until condition is undefined because continue statement on line 6 jumps over it"
        );
    }
}

TEST_CASE("LoopContinueUntilCapture")
{
    // validate continue upvalue closing behavior: continue must close locals defined in the nested scopes
    // but can't close locals defined in the loop scope - these are visible to the condition and will be closed
    // when evaluating the condition instead.
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local a a = 0
repeat
    local b b = 0
    if a then
        local c
        print(function() c = 0 end)
        if a then
            continue -- must close c but not a/b
        end
        -- must close c
    end
    -- must close b but not a
until function() a = 0 b = 0 end
-- must close b on loop exit
-- must close a
)",
                   2
               ),
        R"(
LOADNIL R0
LOADN R0 0
L0: LOADNIL R1
LOADN R1 0
JUMPIFNOT R0 L2
LOADNIL R2
GETIMPORT R3 1 [print]
NEWCLOSURE R4 P0
CAPTURE REF R2
CALL R3 1 0
JUMPIFNOT R0 L1
CLOSEUPVALS R2
JUMP L2
L1: CLOSEUPVALS R2
L2: NEWCLOSURE R2 P1
CAPTURE REF R0
CAPTURE REF R1
JUMPIF R2 L3
CLOSEUPVALS R1
JUMPBACK L0
L3: CLOSEUPVALS R1
CLOSEUPVALS R0
RETURN R0 0
)"
    );

    // a simpler version of the above test doesn't need to close anything when evaluating continue
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local a a = 0
repeat
    local b b = 0
    if a then
        continue -- must not close a/b
    end
    -- must close b but not a
until function() a = 0 b = 0 end
-- must close b on loop exit
-- must close a
)",
                   1
               ),
        R"(
LOADNIL R0
LOADN R0 0
L0: LOADNIL R1
LOADN R1 0
JUMPIF R0 L1
L1: NEWCLOSURE R2 P0
CAPTURE REF R0
CAPTURE REF R1
JUMPIF R2 L2
CLOSEUPVALS R1
JUMPBACK L0
L2: CLOSEUPVALS R1
CLOSEUPVALS R0
RETURN R0 0
)"
    );
}

TEST_CASE("LoopContinueEarlyCleanup")
{
    // locals after a potential 'continue' are not accessible inside the condition and can be closed at the end of a block
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local y
repeat
    local a, b
    do continue end
    local c, d
    local function x()
        return a + b + c + d
    end

    c = 2
    a = 4

    y = x
until a
)",
                   1
               ),
        R"(
LOADNIL R0
L0: LOADNIL R1
LOADNIL R2
JUMP L1
LOADNIL R3
LOADNIL R4
NEWCLOSURE R5 P0
CAPTURE REF R1
CAPTURE REF R3
LOADN R3 2
LOADN R1 4
MOVE R0 R5
CLOSEUPVALS R3
L1: JUMPIF R1 L2
CLOSEUPVALS R1
JUMPBACK L0
L2: CLOSEUPVALS R1
RETURN R0 0
)"
    );
}

TEST_CASE("AndOrOptimizations")
{
    // the OR/ORK optimization triggers for cutoff since lhs is simple
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function advancedRidgedFilter(value, cutoff)
    local cutoff = cutoff or .5
    value = value - cutoff
    return 1 - (value < 0 and -value or value) * 1 / (1 - cutoff)
end
)",
                   0
               ),
        R"(
ORK R2 R1 K0 [0.5]
SUB R0 R0 R2
LOADN R7 0
JUMPIFNOTLT R0 R7 L0
MINUS R6 R0
JUMPIF R6 L1
L0: MOVE R6 R0
L1: MULK R5 R6 K1 [1]
SUBRK R6 K1 [1] R2
DIV R4 R5 R6
SUBRK R3 K1 [1] R4
RETURN R3 1
)"
    );

    // sometimes we need to compute a boolean; this uses LOADB with an offset
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
function thinSurface(surfaceGradient, surfaceThickness)
    return surfaceGradient > .5 - surfaceThickness*.4 and surfaceGradient < .5 + surfaceThickness*.4
end
)",
                   0
               ),
        R"(
LOADB R2 0
MULK R4 R1 K1 [0.40000000000000002]
SUBRK R3 K0 [0.5] R4
JUMPIFNOTLT R3 R0 L1
LOADK R4 K0 [0.5]
MULK R5 R1 K1 [0.40000000000000002]
ADD R3 R4 R5
JUMPIFLT R0 R3 L0
LOADB R2 0 +1
L0: LOADB R2 1
L1: RETURN R2 1
)"
    );

    // sometimes we need to compute a boolean; this uses LOADB with an offset for the last op, note that first op is compiled better
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
function thickSurface(surfaceGradient, surfaceThickness)
    return surfaceGradient < .5 - surfaceThickness*.4 or surfaceGradient > .5 + surfaceThickness*.4
end
)",
                   0
               ),
        R"(
LOADB R2 1
MULK R4 R1 K1 [0.40000000000000002]
SUBRK R3 K0 [0.5] R4
JUMPIFLT R0 R3 L1
LOADK R4 K0 [0.5]
MULK R5 R1 K1 [0.40000000000000002]
ADD R3 R4 R5
JUMPIFLT R3 R0 L0
LOADB R2 0 +1
L0: LOADB R2 1
L1: RETURN R2 1
)"
    );

    // trivial ternary if with constants
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
function testSurface(surface)
    return surface and 1 or 0
end
)",
                   0
               ),
        R"(
JUMPIFNOT R0 L0
LOADN R1 1
RETURN R1 1
L0: LOADN R1 0
RETURN R1 1
)"
    );

    // canonical saturate
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
function saturate(x)
    return x < 0 and 0 or x > 1 and 1 or x
end
)",
                   0
               ),
        R"(
LOADN R2 0
JUMPIFNOTLT R0 R2 L0
LOADN R1 0
RETURN R1 1
L0: LOADN R2 1
JUMPIFNOTLT R2 R0 L1
LOADN R1 1
RETURN R1 1
L1: MOVE R1 R0
RETURN R1 1
)"
    );
}

TEST_CASE("JumpFold")
{
    // jump-to-return folding to return
    CHECK_EQ("\n" + compileFunction0("return a and 1 or 0"), R"(
GETIMPORT R1 1 [a]
JUMPIFNOT R1 L0
LOADN R0 1
RETURN R0 1
L0: LOADN R0 0
RETURN R0 1
)");

    // conditional jump in the inner if() folding to jump out of the expression (JUMPIFNOT+5 skips over all jumps, JUMP+1 skips over JUMP+0)
    CHECK_EQ("\n" + compileFunction0("if a then if b then b() else end else end d()"), R"(
GETIMPORT R0 1 [a]
JUMPIFNOT R0 L0
GETIMPORT R0 3 [b]
JUMPIFNOT R0 L0
GETIMPORT R0 3 [b]
CALL R0 0 0
JUMP L0
JUMP L0
L0: GETIMPORT R0 5 [d]
CALL R0 0 0
RETURN R0 0
)");

    // same as example before but the unconditional jumps are folded with RETURN
    CHECK_EQ("\n" + compileFunction0("if a then if b then b() else end else end"), R"(
GETIMPORT R0 1 [a]
JUMPIFNOT R0 L0
GETIMPORT R0 3 [b]
JUMPIFNOT R0 L0
GETIMPORT R0 3 [b]
CALL R0 0 0
RETURN R0 0
RETURN R0 0
L0: RETURN R0 0
)");

    // in this example, we do *not* have a JUMP after RETURN in the if branch
    // this is important since, even though this jump is never reached, jump folding needs to be able to analyze it
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
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
                   0
               ),
        R"(
ORK R6 R3 K0 [0]
ORK R7 R4 K1 [1]
JUMPIF R5 L0
GETIMPORT R10 5 [math.noise]
DIV R13 R0 R7
MULK R14 R6 K6 [17]
ADD R12 R13 R14
GETIMPORT R13 8 [masterSeed]
ADD R11 R12 R13
DIV R13 R1 R7
GETIMPORT R14 8 [masterSeed]
SUB R12 R13 R14
DIV R14 R2 R7
MUL R15 R6 R6
SUB R13 R14 R15
CALL R10 3 1
MULK R9 R10 K2 [0.5]
ADDK R8 R9 K2 [0.5]
RETURN R8 1
L0: GETIMPORT R8 5 [math.noise]
DIV R11 R0 R7
MULK R12 R6 K6 [17]
ADD R10 R11 R12
GETIMPORT R11 8 [masterSeed]
ADD R9 R10 R11
DIV R11 R1 R7
GETIMPORT R12 8 [masterSeed]
SUB R10 R11 R12
DIV R12 R2 R7
MUL R13 R6 R6
SUB R11 R12 R13
CALL R8 3 -1
RETURN R8 -1
)"
    );
}

TEST_CASE("RecursionParse")
{
    // The test forcibly pushes the stack limit during compilation; in NoOpt, the stack consumption is much larger so we need to reduce the limit to
    // not overflow the C stack. When ASAN is enabled, stack consumption increases even more.
#if defined(LUAU_ENABLE_ASAN)
    ScopedFastInt flag(FInt::LuauRecursionLimit, 200);
#elif defined(_NOOPT) || defined(_DEBUG)
    ScopedFastInt flag(FInt::LuauRecursionLimit, 300);
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

    try
    {
        Luau::compileOrThrow(bcb, rep("a(", 1500) + "42" + rep(")", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your expression to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, "return " + rep("{", 1500) + "42" + rep("}", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your expression to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, rep("while true do ", 1500) + "print()" + rep(" end", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your expression to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, rep("for i=1,1 do ", 1500) + "print()" + rep(" end", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your expression to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, rep("function a() ", 1500) + "print()" + rep(" end", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your block to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, "return " + rep("function() return ", 1500) + "42" + rep(" end", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your block to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, "local f: " + rep("(", 1500) + "nil" + rep(")", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your type annotation to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, "local f: () " + rep("-> ()", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your type annotation to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, "local f: " + rep("{x:", 1500) + "nil" + rep("}", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your type annotation to make the code compile");
    }

    try
    {
        Luau::compileOrThrow(bcb, "local f: " + rep("(nil & ", 1500) + "nil" + rep(")", 1500));
        CHECK(!"Expected exception");
    }
    catch (std::exception& e)
    {
        CHECK_EQ(std::string(e.what()), "Exceeded allowed recursion depth; simplify your type annotation to make the code compile");
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
FASTCALL2 18 R0 R1 L0
MOVE R5 R0
MOVE R6 R1
GETIMPORT R4 2 [math.max]
CALL R4 2 1
L0: FASTCALL2 19 R4 R2 L1
MOVE R5 R2
GETIMPORT R3 4 [math.min]
CALL R3 2 -1
L1: RETURN R3 -1
)");
}

TEST_CASE("UpvaluesLoopsBytecode")
{
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
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
                   1
               ),
        R"(
LOADN R2 1
LOADN R0 10
LOADN R1 1
FORNPREP R0 L2
L0: MOVE R3 R2
GETIMPORT R4 1 [foo]
NEWCLOSURE R5 P0
CAPTURE REF R3
CALL R4 1 0
GETIMPORT R4 3 [bar]
JUMPIFNOT R4 L1
CLOSEUPVALS R3
JUMP L2
L1: CLOSEUPVALS R3
FORNLOOP R0 L0
L2: LOADN R0 0
RETURN R0 1
)"
    );

    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
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
                   1
               ),
        R"(
GETIMPORT R0 1 [ipairs]
GETIMPORT R1 3 [data]
CALL R0 1 3
FORGPREP_INEXT R0 L2
L0: GETIMPORT R5 5 [foo]
NEWCLOSURE R6 P0
CAPTURE REF R3
CALL R5 1 0
GETIMPORT R5 7 [bar]
JUMPIFNOT R5 L1
CLOSEUPVALS R3
JUMP L3
L1: CLOSEUPVALS R3
L2: FORGLOOP R0 L0 1 [inext]
L3: LOADN R0 0
RETURN R0 1
)"
    );

    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
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
                   1
               ),
        R"(
LOADN R0 0
L0: LOADN R1 5
JUMPIFNOTLT R0 R1 L2
LOADNIL R1
MOVE R1 R0
GETIMPORT R2 1 [foo]
NEWCLOSURE R3 P0
CAPTURE REF R1
CALL R2 1 0
ADDK R0 R0 K2 [1]
GETIMPORT R2 4 [bar]
JUMPIFNOT R2 L1
CLOSEUPVALS R1
JUMP L2
L1: CLOSEUPVALS R1
JUMPBACK L0
L2: LOADN R1 0
RETURN R1 1
)"
    );

    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
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
                   1
               ),
        R"(
LOADN R0 0
L0: LOADNIL R1
MOVE R1 R0
GETIMPORT R2 1 [foo]
NEWCLOSURE R3 P0
CAPTURE REF R1
CALL R2 1 0
ADDK R0 R0 K2 [1]
GETIMPORT R2 4 [bar]
JUMPIFNOT R2 L1
CLOSEUPVALS R1
JUMP L3
L1: LOADN R2 5
JUMPIFLT R0 R2 L2
CLOSEUPVALS R1
JUMPBACK L0
L2: CLOSEUPVALS R1
L3: LOADN R1 0
RETURN R1 1
)"
    );
}

TEST_CASE("TypeAliasing")
{
    Luau::BytecodeBuilder bcb;
    Luau::CompileOptions options;
    Luau::ParseOptions parseOptions;
    CHECK_NOTHROW(Luau::compileOrThrow(bcb, "type A = number local a: A = 1", options, parseOptions));
}

TEST_CASE("TypeFunction")
{
    Luau::BytecodeBuilder bcb;
    Luau::CompileOptions options;
    Luau::ParseOptions parseOptions;
    CHECK_NOTHROW(Luau::compileOrThrow(bcb, "type function a() return types.any end", options, parseOptions));
}

TEST_CASE("NoTypeFunctionsInBytecode")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::compileOrThrow(bcb, R"(
type function a() return types.any end
function b() return 2 end
return b()
)");

    CHECK_EQ("\n" + bcb.dumpEverything(), R"(
Function 0 (b):
LOADN R0 2
RETURN R0 1

Function 1 (??):
DUPCLOSURE R0 K0 ['b']
SETGLOBAL R0 K1 ['b']
GETGLOBAL R0 K1 ['b']
CALL R0 0 -1
RETURN R0 -1

)");
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
3: SETTABLEKS R1 R0 K0 ['Mountains']
4: LOADB R1 1
4: SETTABLEKS R1 R0 K1 ['Canyons']
5: LOADB R1 1
5: SETTABLEKS R1 R0 K2 ['Dunes']
6: LOADB R1 1
6: SETTABLEKS R1 R0 K3 ['Arctic']
7: LOADB R1 1
7: SETTABLEKS R1 R0 K4 ['Lavaflow']
8: LOADB R1 1
8: SETTABLEKS R1 R0 K5 ['Hills']
9: LOADB R1 1
9: SETTABLEKS R1 R0 K6 ['Plains']
10: LOADB R1 1
10: SETTABLEKS R1 R0 K7 ['Marsh']
11: LOADB R1 1
11: SETTABLEKS R1 R0 K8 ['Water']
13: LOADK R1 K9 ['']
14: GETIMPORT R2 11 [pairs]
14: MOVE R3 R0
14: CALL R2 1 3
14: FORGPREP_NEXT R2 L1
15: L0: MOVE R7 R1
15: MOVE R8 R5
15: CONCAT R1 R7 R8
14: L1: FORGLOOP R2 L0 1
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
9: FORGPREP R0 L1
11: L0: GETIMPORT R5 1 [print]
11: MOVE R6 R3
11: CALL R5 1 0
2: L1: FORGLOOP R0 L0 1
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
4: L0: ADDK R0 R0 K0 [1]
5: LOADN R1 1
5: JUMPIFNOTLT R1 R0 L1
6: GETIMPORT R1 2 [print]
6: LOADK R2 K3 ['done!']
6: CALL R1 1 0
7: RETURN R0 0
3: L1: JUMPBACK L0
10: RETURN R0 0
)");
}

TEST_CASE("DebugLineInfoRepeatUntil")
{
    CHECK_EQ(
        "\n" + compileFunction0Coverage(
                   R"(
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
                   0
               ),
        R"(
2: LOADN R0 0
4: L0: ADDK R0 R0 K0 [1]
5: JUMPXEQKN R0 K0 L1 NOT [1]
6: GETIMPORT R1 2 [print]
6: MOVE R2 R0
6: CALL R1 1 0
6: JUMP L2
8: L1: LOADN R0 0
10: L2: JUMPXEQKN R0 K3 L3 [0]
10: JUMPBACK L0
11: L3: RETURN R0 0
)"
    );
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
5: GETTABLEKS R4 R3 K0 ['SubTable']
5: DUPTABLE R5 5
6: SETTABLEKS R0 R5 K1 ['Key1']
7: SETTABLEKS R1 R5 K2 ['Key2']
8: SETTABLEKS R2 R5 K3 ['Key3']
9: LOADB R6 1
9: SETTABLEKS R6 R5 K4 ['Key4']
5: SETTABLEKS R5 R4 K6 ['Key']
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
4: NAMECALL R1 R0 K0 ['Bar']
4: CALL R1 4 0
8: RETURN R0 0
)");
}

TEST_CASE("DebugLineInfoCallChain")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Lines);
    Luau::compileOrThrow(bcb, R"(
local Foo = ...

Foo
:Bar(1)
:Baz(2)
.Qux(3)
)");

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
2: GETVARARGS R0 1
5: LOADN R4 1
5: NAMECALL R2 R0 K0 ['Bar']
5: CALL R2 2 1
6: LOADN R4 2
6: NAMECALL R2 R2 K1 ['Baz']
6: CALL R2 2 1
7: GETTABLEKS R1 R2 K2 ['Qux']
7: LOADN R2 3
7: CALL R1 1 0
8: RETURN R0 0
)");
}

TEST_CASE("DebugLineInfoFastCall")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Lines);
    Luau::compileOrThrow(bcb, R"(
local Foo, Bar = ...

return
    math.max(
        Foo,
        Bar)
)");

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
2: GETVARARGS R0 2
5: FASTCALL2 18 R0 R1 L0
5: MOVE R3 R0
5: MOVE R4 R1
5: GETIMPORT R2 2 [math.max]
5: CALL R2 2 -1
5: L0: RETURN R2 -1
)");
}

TEST_CASE("DebugLineInfoAssignment")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Lines);
    Luau::compileOrThrow(bcb, R"(
   local a = { b = { c = { d = 3 } } }

a
["b"]
["c"]
["d"] = 4
)");

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
2: DUPTABLE R0 1
2: DUPTABLE R1 3
2: DUPTABLE R2 5
2: LOADN R3 3
2: SETTABLEKS R3 R2 K4 ['d']
2: SETTABLEKS R2 R1 K2 ['c']
2: SETTABLEKS R1 R0 K0 ['b']
5: GETTABLEKS R2 R0 K0 ['b']
6: GETTABLEKS R1 R2 K2 ['c']
7: LOADN R2 4
7: SETTABLEKS R2 R1 K4 ['d']
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
SETTABLEKS R1 R0 K0 ['Mountains']
    4:     ['Canyons'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K1 ['Canyons']
    5:     ['Dunes'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K2 ['Dunes']
    6:     ['Arctic'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K3 ['Arctic']
    7:     ['Lavaflow'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K4 ['Lavaflow']
    8:     ['Hills'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K5 ['Hills']
    9:     ['Plains'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K6 ['Plains']
   10:     ['Marsh'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K7 ['Marsh']
   11:     ['Water'] = true,
LOADB R1 1
SETTABLEKS R1 R0 K8 ['Water']
   13: local result = ""
LOADK R1 K9 ['']
   14: for k in pairs(kSelectedBiomes) do
GETIMPORT R2 11 [pairs]
MOVE R3 R0
CALL R2 1 3
FORGPREP_NEXT R2 L1
   15:     result = result .. k
L0: MOVE R7 R1
MOVE R8 R5
CONCAT R1 R7 R8
   14: for k in pairs(kSelectedBiomes) do
L1: FORGLOOP R2 L0 1
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
local 3: reg 3, start pc 22 line 12, end pc 25 line 12
local 4: reg 3, start pc 27 line 16, end pc 31 line 16
local 5: reg 0, start pc 0 line 3, end pc 35 line 21
local 6: reg 1, start pc 0 line 3, end pc 35 line 21
local 7: reg 2, start pc 1 line 4, end pc 35 line 21
local 8: reg 3, start pc 35 line 21, end pc 35 line 21
3: LOADN R2 1
4: LOADN R5 1
4: LOADN R3 3
4: LOADN R4 1
4: FORNPREP R3 L1
5: L0: GETIMPORT R6 1 [print]
5: MOVE R7 R5
5: CALL R6 1 0
4: FORNLOOP R3 L0
7: L1: GETIMPORT R3 3 [pairs]
7: CALL R3 0 3
7: FORGPREP_NEXT R3 L3
8: L2: GETIMPORT R8 1 [print]
8: MOVE R9 R6
8: MOVE R10 R7
8: CALL R8 2 0
7: L3: FORGLOOP R3 L2 2
11: LOADN R3 2
12: GETIMPORT R4 1 [print]
12: LOADN R5 2
12: CALL R4 1 0
15: LOADN R3 2
16: GETIMPORT R4 1 [print]
16: GETIMPORT R5 5 [b]
16: CALL R4 1 0
18: NEWCLOSURE R3 P0
18: CAPTURE VAL R3
18: CAPTURE VAL R2
21: RETURN R2 1
)");
}

TEST_CASE("DebugLocals2")
{
    const char* source = R"(
function foo(x)
    repeat
        local a, b
    until true
end
)";

    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Lines | Luau::BytecodeBuilder::Dump_Locals);
    bcb.setDumpSource(source);

    Luau::CompileOptions options;
    options.debugLevel = 2;

    Luau::compileOrThrow(bcb, source, options);

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
local 0: reg 1, start pc 2 line 6, no live range
local 1: reg 2, start pc 2 line 6, no live range
local 2: reg 0, start pc 0 line 4, end pc 2 line 6
4: LOADNIL R1
4: LOADNIL R2
6: RETURN R0 0
)");
}

TEST_CASE("DebugLocals3")
{
    const char* source = R"(
function foo(x)
    repeat
        local a, b
        do continue end
        local c, d = 2
    until true
end
)";

    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Lines | Luau::BytecodeBuilder::Dump_Locals);
    bcb.setDumpSource(source);

    Luau::CompileOptions options;
    options.debugLevel = 2;

    Luau::compileOrThrow(bcb, source, options);

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
local 0: reg 3, start pc 5 line 8, no live range
local 1: reg 4, start pc 5 line 8, no live range
local 2: reg 1, start pc 2 line 5, end pc 4 line 6
local 3: reg 2, start pc 2 line 5, end pc 4 line 6
local 4: reg 0, start pc 0 line 4, end pc 5 line 8
4: LOADNIL R1
4: LOADNIL R2
5: RETURN R0 0
6: LOADN R3 2
6: LOADNIL R4
8: RETURN R0 0
)");
}

TEST_CASE("DebugRemarks")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Remarks);

    uint32_t fid = bcb.beginFunction(0);

    bcb.addDebugRemark("test remark #%d", 1);
    bcb.emitABC(LOP_LOADNIL, 0, 0, 0);
    bcb.addDebugRemark("test remark #%d", 2);
    bcb.addDebugRemark("test remark #%d", 3);
    bcb.emitABC(LOP_RETURN, 0, 1, 0);

    bcb.endFunction(1, 0);

    bcb.setMainFunction(fid);
    bcb.finalize();

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
REMARK test remark #1
LOADNIL R0
REMARK test remark #2
REMARK test remark #3
RETURN R0 0
)");
}

TEST_CASE("DebugTypes")
{
    const char* source = R"(
local up: number = 2

function foo(e: vector, f: mat3, g: sequence)
    local h = e * e

    for i=1,3 do
        print(i)
    end

    print(e * f)
    print(g)
    print(h)

    up += a
    return a
end
)";

    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Types);
    bcb.setDumpSource(source);

    Luau::CompileOptions options;
    options.vectorCtor = "vector";
    options.vectorType = "vector";

    options.typeInfoLevel = 1;

    static const char* kUserdataCompileTypes[] = {"vec2", "color", "mat3", nullptr};
    options.userdataTypes = kUserdataCompileTypes;

    Luau::compileOrThrow(bcb, source, options);

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
R0: vector [argument]
R1: mat3 [argument]
R2: userdata [argument]
U0: number
R6: any from 1 to 9
R3: vector from 0 to 30
MUL R3 R0 R0
LOADN R6 1
LOADN R4 3
LOADN R5 1
FORNPREP R4 L1
L0: GETIMPORT R7 1 [print]
MOVE R8 R6
CALL R7 1 0
FORNLOOP R4 L0
L1: GETIMPORT R4 1 [print]
MUL R5 R0 R1
CALL R4 1 0
GETIMPORT R4 1 [print]
MOVE R5 R2
CALL R4 1 0
GETIMPORT R4 1 [print]
MOVE R5 R3
CALL R4 1 0
GETUPVAL R4 0
GETIMPORT R5 3 [a]
ADD R4 R4 R5
SETUPVAL R4 0
GETIMPORT R4 3 [a]
RETURN R4 1
)");
}

TEST_CASE("CostModelRemarks")
{
    CHECK_EQ(
        compileWithRemarks(R"(
local a, b = ...

local function foo(x)
    return(math.abs(x))
end

return foo(a) + foo(assert(b))
)"),
        R"(
local a, b = ...

local function foo(x)
    -- remark: builtin math.abs/1
    return(math.abs(x))
end

-- remark: builtin assert/1
-- remark: inlining succeeded (cost 2, profit 2.50x, depth 0)
return foo(a) + foo(assert(b))
)"
    );

    CHECK_EQ(
        compileWithRemarks(R"(
local value = true

local function foo()
    return value
end

return foo()
)"),
        R"(
local value = true

local function foo()
    return value
end

-- remark: inlining succeeded (cost 0, profit 3.00x, depth 0)
return foo()
)"
    );

    CHECK_EQ(
        compileWithRemarks(R"(
local value = true

local function foo()
    return not value
end

return foo()
)"),
        R"(
local value = true

local function foo()
    return not value
end

-- remark: inlining succeeded (cost 0, profit 3.00x, depth 0)
return foo()
)"
    );

    CHECK_EQ(
        compileWithRemarks(R"(
local function foo()
    local s = 0
    for i = 1, 100 do s += i end
    return s
end

return foo()
)"),
        R"(
local function foo()
    local s = 0
    -- remark: loop unroll failed: too many iterations (100)
    for i = 1, 100 do s += i end
    return s
end

-- remark: inlining failed: too expensive (cost 127, profit 1.02x)
return foo()
)"
    );

    CHECK_EQ(
        compileWithRemarks(R"(
local function foo()
    local s = 0
    for i = 1, 4 * 25 do s += i end
    return s
end

return foo()
)"),
        R"(
local function foo()
    local s = 0
    -- remark: loop unroll failed: too many iterations (100)
    for i = 1, 4 * 25 do s += i end
    return s
end

-- remark: inlining failed: too expensive (cost 127, profit 1.02x)
return foo()
)"
    );

    CHECK_EQ(
        compileWithRemarks(R"(
local x = ...
local function test(a)
    while a < 0 do
        a += 1
    end
    for i=10,1,-1 do
        a += 1
    end
    for i in pairs({}) do
        a += 1
        if a % 2 == 0 then continue end
    end
    repeat
        a += 1
        if a % 2 == 0 then break end
    until a > 10
    return a
end
local a = test(x)
local b = test(2)
)"),
        R"(
local x = ...
local function test(a)
    while a < 0 do
        a += 1
    end
    -- remark: loop unroll succeeded (iterations 10, cost 10, profit 2.00x)
    for i=10,1,-1 do
        a += 1
    end
    -- remark: allocation: table hash 0
    for i in pairs({}) do
        a += 1
        if a % 2 == 0 then continue end
    end
    repeat
        a += 1
        if a % 2 == 0 then break end
    until a > 10
    return a
end
-- remark: inlining failed: too expensive (cost 76, profit 1.03x)
local a = test(x)
-- remark: inlining failed: too expensive (cost 73, profit 1.08x)
local b = test(2)
)"
    );
}

TEST_CASE("AssignmentConflict")
{
    // assignments are left to right
    CHECK_EQ("\n" + compileFunction0("local a, b a, b = 1, 2"), R"(
LOADNIL R0
LOADNIL R1
LOADN R0 1
LOADN R1 2
RETURN R0 0
)");

    // if assignment of a local invalidates a direct register reference in later assignments, the value is assigned to a temp register first
    CHECK_EQ("\n" + compileFunction0("local a a, a[1] = 1, 2"), R"(
LOADNIL R0
LOADN R1 1
LOADN R2 2
SETTABLEN R2 R0 1
MOVE R0 R1
RETURN R0 0
)");

    // note that this doesn't happen if the local assignment happens last naturally
    CHECK_EQ("\n" + compileFunction0("local a a[1], a = 1, 2"), R"(
LOADNIL R0
LOADN R2 1
LOADN R1 2
SETTABLEN R2 R0 1
MOVE R0 R1
RETURN R0 0
)");

    // this will happen if assigned register is used in any table expression, including as an object...
    CHECK_EQ("\n" + compileFunction0("local a a, a.foo = 1, 2"), R"(
LOADNIL R0
LOADN R1 1
LOADN R2 2
SETTABLEKS R2 R0 K0 ['foo']
MOVE R0 R1
RETURN R0 0
)");

    // ... or a table index ...
    CHECK_EQ("\n" + compileFunction0("local a a, foo[a] = 1, 2"), R"(
LOADNIL R0
GETIMPORT R1 1 [foo]
LOADN R2 1
LOADN R3 2
SETTABLE R3 R1 R0
MOVE R0 R2
RETURN R0 0
)");

    // ... or both ...
    CHECK_EQ("\n" + compileFunction0("local a a, a[a] = 1, 2"), R"(
LOADNIL R0
LOADN R1 1
LOADN R2 2
SETTABLE R2 R0 R0
MOVE R0 R1
RETURN R0 0
)");

    // ... or both with two different locals ...
    CHECK_EQ("\n" + compileFunction0("local a, b a, b, a[b] = 1, 2, 3"), R"(
LOADNIL R0
LOADNIL R1
LOADN R2 1
LOADN R3 2
LOADN R4 3
SETTABLE R4 R0 R1
MOVE R0 R2
MOVE R1 R3
RETURN R0 0
)");

    // however note that if it participates in an expression on the left hand side, there's no point reassigning it since we'd compute the expr value
    // into a temp register
    CHECK_EQ("\n" + compileFunction0("local a a, foo[a + 1] = 1, 2"), R"(
LOADNIL R0
GETIMPORT R1 1 [foo]
ADDK R2 R0 K2 [1]
LOADN R0 1
LOADN R3 2
SETTABLE R3 R1 R2
RETURN R0 0
)");
}

TEST_CASE("FastcallBytecode")
{
    // direct global call
    CHECK_EQ("\n" + compileFunction0("return math.abs(-5)"), R"(
LOADN R1 -5
FASTCALL1 2 R1 L0
GETIMPORT R0 2 [math.abs]
CALL R0 1 -1
L0: RETURN R0 -1
)");

    // call through a local variable
    CHECK_EQ("\n" + compileFunction0("local abs = math.abs return abs(-5)"), R"(
GETIMPORT R0 2 [math.abs]
LOADN R2 -5
FASTCALL1 2 R2 L0
MOVE R1 R0
CALL R1 1 -1
L0: RETURN R1 -1
)");

    // call through an upvalue
    CHECK_EQ("\n" + compileFunction0("local abs = math.abs function foo() return abs(-5) end return foo()"), R"(
LOADN R1 -5
FASTCALL1 2 R1 L0
GETUPVAL R0 0
CALL R0 1 -1
L0: RETURN R0 -1
)");

    // mutating the global in the script breaks the optimization
    CHECK_EQ("\n" + compileFunction0("math = {} return math.abs(-5)"), R"(
NEWTABLE R0 0 0
SETGLOBAL R0 K0 ['math']
GETGLOBAL R1 K0 ['math']
GETTABLEKS R0 R1 K1 ['abs']
LOADN R1 -5
CALL R0 1 -1
RETURN R0 -1
)");

    // mutating the local in the script breaks the optimization
    CHECK_EQ("\n" + compileFunction0("local abs = math.abs abs = nil return abs(-5)"), R"(
GETIMPORT R0 2 [math.abs]
LOADNIL R0
MOVE R1 R0
LOADN R2 -5
CALL R1 1 -1
RETURN R1 -1
)");

    // mutating the global in the script breaks the optimization, even if you do this after computing the local (for simplicity)
    CHECK_EQ("\n" + compileFunction0("local abs = math.abs math = {} return abs(-5)"), R"(
GETGLOBAL R1 K0 ['math']
GETTABLEKS R0 R1 K1 ['abs']
NEWTABLE R1 0 0
SETGLOBAL R1 K0 ['math']
MOVE R1 R0
LOADN R2 -5
CALL R1 1 -1
RETURN R1 -1
)");
}

TEST_CASE("Fastcall3")
{
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local a, b, c = ...
return math.min(a, b, c) + math.clamp(a, b, c)
)"),
        R"(
GETVARARGS R0 3
FASTCALL3 19 R0 R1 R2 L0
MOVE R5 R0
MOVE R6 R1
MOVE R7 R2
GETIMPORT R4 2 [math.min]
CALL R4 3 1
L0: FASTCALL3 46 R0 R1 R2 L1
MOVE R6 R0
MOVE R7 R1
MOVE R8 R2
GETIMPORT R5 4 [math.clamp]
CALL R5 3 1
L1: ADD R3 R4 R5
RETURN R3 1
)"
    );
}

TEST_CASE("FastcallSelect")
{
    // select(_, ...) compiles to a builtin call
    CHECK_EQ("\n" + compileFunction0("return (select('#', ...))"), R"(
LOADK R1 K0 ['#']
FASTCALL1 57 R1 L0
GETIMPORT R0 2 [select]
GETVARARGS R2 -1
CALL R0 -1 1
L0: RETURN R0 1
)");

    // more complex example: select inside a for loop bound + select from a iterator
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local sum = 0
for i=1, select('#', ...) do
    sum += select(i, ...)
end
return sum
)"),
        R"(
LOADN R0 0
LOADN R3 1
LOADK R5 K0 ['#']
FASTCALL1 57 R5 L0
GETIMPORT R4 2 [select]
GETVARARGS R6 -1
CALL R4 -1 1
L0: MOVE R1 R4
LOADN R2 1
FORNPREP R1 L3
L1: FASTCALL1 57 R3 L2
GETIMPORT R4 2 [select]
MOVE R5 R3
GETVARARGS R6 -1
CALL R4 -1 1
L2: ADD R0 R0 R4
FORNLOOP R1 L1
L3: RETURN R0 1
)"
    );

    // currently we assume a single value return to avoid dealing with stack resizing
    CHECK_EQ("\n" + compileFunction0("return select('#', ...)"), R"(
GETIMPORT R0 1 [select]
LOADK R1 K2 ['#']
GETVARARGS R2 -1
CALL R0 -1 -1
RETURN R0 -1
)");

    // note that select with a non-variadic second argument doesn't get optimized
    CHECK_EQ("\n" + compileFunction0("return select('#')"), R"(
GETIMPORT R0 1 [select]
LOADK R1 K2 ['#']
CALL R0 1 -1
RETURN R0 -1
)");

    // note that select with a non-variadic second argument doesn't get optimized
    CHECK_EQ("\n" + compileFunction0("return select('#', foo())"), R"(
GETIMPORT R0 1 [select]
LOADK R1 K2 ['#']
GETIMPORT R2 4 [foo]
CALL R2 0 -1
CALL R0 -1 -1
RETURN R0 -1
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
LOADK R0 K0 [-0]
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

    // recursive capture
    CHECK_EQ("\n" + compileFunction("local function foo() return foo() end", 1), R"(
DUPCLOSURE R0 K0 ['foo']
CAPTURE VAL R0
RETURN R0 0
)");

    // multi-level recursive capture
    CHECK_EQ("\n" + compileFunction("local function foo() return function() return foo() end end", 1), R"(
DUPCLOSURE R0 K0 []
CAPTURE UPVAL U0
RETURN R0 1
)");

    // multi-level recursive capture where function isn't top-level
    // note: this should probably be optimized to DUPCLOSURE but doing that requires a different upval tracking flow in the compiler
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo()
    local function bar()
        return function() return bar() end
    end
end
)",
                   1
               ),
        R"(
NEWCLOSURE R0 P0
CAPTURE UPVAL U0
RETURN R0 1
)"
    );
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

    std::string fragment;
    for (size_t i = 9; i > 1; --i)
    {
        fragment += std::string(insns[insns.size() - i]);
        fragment += "\n";
    }

    // note: it's important that GETGLOBAL below doesn't overwrite R2
    CHECK_EQ("\n" + fragment, R"(
LOADN R1 1024
LOADK R2 K1023 ['1024']
SETTABLE R2 R0 R1
LOADN R2 -1
FASTCALL1 2 R2 L0
GETGLOBAL R3 K1024 ['math']
GETTABLEKS R1 R3 K1025 ['abs']
CALL R1 1 -1
)");
}

TEST_CASE("CompoundAssignment")
{
    // globals vs constants
    CHECK_EQ("\n" + compileFunction0("a += 1"), R"(
GETGLOBAL R0 K0 ['a']
ADDK R0 R0 K1 [1]
SETGLOBAL R0 K0 ['a']
RETURN R0 0
)");

    // globals vs expressions
    CHECK_EQ("\n" + compileFunction0("a -= a"), R"(
GETGLOBAL R0 K0 ['a']
GETGLOBAL R1 K0 ['a']
SUB R0 R0 R1
SETGLOBAL R0 K0 ['a']
RETURN R0 0
)");

    // locals vs constants
    CHECK_EQ("\n" + compileFunction0("local a = 1 a *= 2"), R"(
LOADN R0 1
MULK R0 R0 K0 [2]
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
ADDK R1 R0 K0 [1]
DIV R0 R0 R1
RETURN R0 0
)");

    // upvalues
    CHECK_EQ("\n" + compileFunction0("local a = 1 function foo() a += 4 end"), R"(
GETUPVAL R0 0
ADDK R0 R0 K0 [4]
SETUPVAL R0 0
RETURN R0 0
)");

    // table variants (indexed by string, number, variable)
    CHECK_EQ("\n" + compileFunction0("local a = {} a.foo += 5"), R"(
NEWTABLE R0 0 0
GETTABLEKS R1 R0 K0 ['foo']
ADDK R1 R1 K1 [5]
SETTABLEKS R1 R0 K0 ['foo']
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = {} a[1] += 5"), R"(
NEWTABLE R0 0 0
GETTABLEN R1 R0 1
ADDK R1 R1 K0 [5]
SETTABLEN R1 R0 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = {} a[a] += 5"), R"(
NEWTABLE R0 0 0
GETTABLE R1 R0 R0
ADDK R1 R1 K0 [5]
SETTABLE R1 R0 R0
RETURN R0 0
)");

    // left hand side is evaluated once
    CHECK_EQ("\n" + compileFunction0("foo()[bar()] += 5"), R"(
GETIMPORT R0 1 [foo]
CALL R0 0 1
GETIMPORT R1 3 [bar]
CALL R1 0 1
GETTABLE R2 R0 R1
ADDK R2 R2 K4 [5]
SETTABLE R2 R0 R1
RETURN R0 0
)");
}

TEST_CASE("CompoundAssignmentConcat")
{
    // basic concat
    CHECK_EQ("\n" + compileFunction0("local a = '' a ..= 'a'"), R"(
LOADK R0 K0 ['']
MOVE R1 R0
LOADK R2 K1 ['a']
CONCAT R0 R1 R2
RETURN R0 0
)");

    // concat chains
    CHECK_EQ("\n" + compileFunction0("local a = '' a ..= 'a' .. 'b'"), R"(
LOADK R0 K0 ['']
MOVE R1 R0
LOADK R2 K1 ['a']
LOADK R3 K2 ['b']
CONCAT R0 R1 R3
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = '' a ..= 'a' .. 'b' .. 'c'"), R"(
LOADK R0 K0 ['']
MOVE R1 R0
LOADK R2 K1 ['a']
LOADK R3 K2 ['b']
LOADK R4 K3 ['c']
CONCAT R0 R1 R4
RETURN R0 0
)");

    // concat on non-local
    CHECK_EQ("\n" + compileFunction0("_VERSION ..= 'a' .. 'b'"), R"(
GETGLOBAL R1 K0 ['_VERSION']
LOADK R2 K1 ['a']
LOADK R3 K2 ['b']
CONCAT R0 R1 R3
SETGLOBAL R0 K0 ['_VERSION']
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
    std::string head;
    for (size_t i = 0; i < 16; ++i)
        head += insns[i] + "\n";

    CHECK_EQ("\n" + head, R"(
LOADN R0 0
LOADN R3 1
LOADN R1 3
LOADN R2 1
JUMP L1
L0: JUMPX L14543
L1: FORNPREP R1 L0
L2: ADD R0 R0 R3
LOADK R4 K0 [150000]
JUMP L4
L3: JUMPX L14543
L4: JUMPIFLT R4 R0 L3
ADD R0 R0 R3
LOADK R4 K0 [150000]
JUMP L6
L5: JUMPX L14543
)");

    // FORNLOOP has to go through a trampoline since the jump is back to the beginning of the function
    // however, late JUMPs (break) don't need a trampoline since the loop end is really close by
    std::string tail;
    for (size_t i = 44539; i < insns.size(); ++i)
        tail += insns[i] + "\n";

    CHECK_EQ("\n" + tail, R"(
ADD R0 R0 R3
LOADK R4 K0 [150000]
JUMPIFLT R4 R0 L14543
ADD R0 R0 R3
LOADK R4 K0 [150000]
JUMPIFLT R4 R0 L14543
JUMP L14542
L14541: JUMPX L2
L14542: FORNLOOP R1 L14541
L14543: RETURN R0 1
)");
}

TEST_CASE("CompileBytecode")
{
    // This is a coverage test, it just exercises bytecode dumping for correct and malformed code
    Luau::compile("return 5");
    Luau::compile("this is not valid lua, right?");
}

TEST_CASE("NestedNamecall")
{
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local obj = ...
return obj:Method(1):Method(2):Method(3)
)"),
        R"(
GETVARARGS R0 1
LOADN R3 1
NAMECALL R1 R0 K0 ['Method']
CALL R1 2 1
LOADN R3 2
NAMECALL R1 R1 K0 ['Method']
CALL R1 2 1
LOADN R3 3
NAMECALL R1 R1 K0 ['Method']
CALL R1 2 -1
RETURN R1 -1
)"
    );
}

TEST_CASE("ElideLocals")
{
    // simple local elision: all locals are constant
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local a, b = 1, 2
return a + b
)"),
        R"(
LOADN R0 3
RETURN R0 1
)"
    );

    // side effecting expressions block local elision
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local a = g()
return a
)"),
        R"(
GETIMPORT R0 1 [g]
CALL R0 0 1
RETURN R0 1
)"
    );

    // ... even if they are not used
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local a = 1, g()
return a
)"),
        R"(
LOADN R0 1
GETIMPORT R1 1 [g]
CALL R1 0 1
RETURN R0 1
)"
    );
}

TEST_CASE("ConstantJumpCompare")
{
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local obj = ...
local b = obj == 1
)"),
        R"(
GETVARARGS R0 1
JUMPXEQKN R0 K0 L0 [1]
LOADB R1 0 +1
L0: LOADB R1 1
L1: RETURN R0 0
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(
local obj = ...
local b = 1 == obj
)"),
        R"(
GETVARARGS R0 1
JUMPXEQKN R0 K0 L0 [1]
LOADB R1 0 +1
L0: LOADB R1 1
L1: RETURN R0 0
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(
local obj = ...
local b = "Hello, Sailor!" == obj
)"),
        R"(
GETVARARGS R0 1
JUMPXEQKS R0 K0 L0 ['Hello, Sailor!']
LOADB R1 0 +1
L0: LOADB R1 1
L1: RETURN R0 0
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(
local obj = ...
local b = nil == obj
)"),
        R"(
GETVARARGS R0 1
JUMPXEQKNIL R0 L0
LOADB R1 0 +1
L0: LOADB R1 1
L1: RETURN R0 0
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(
local obj = ...
local b = true == obj
)"),
        R"(
GETVARARGS R0 1
JUMPXEQKB R0 1 L0
LOADB R1 0 +1
L0: LOADB R1 1
L1: RETURN R0 0
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(
local obj = ...
local b = nil ~= obj
)"),
        R"(
GETVARARGS R0 1
JUMPXEQKNIL R0 L0 NOT
LOADB R1 0 +1
L0: LOADB R1 1
L1: RETURN R0 0
)"
    );

    // table literals should not generate IFEQK variants
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local obj = ...
local b = obj == {}
)"),
        R"(
GETVARARGS R0 1
NEWTABLE R2 0 0
JUMPIFEQ R0 R2 L0
LOADB R1 0 +1
L0: LOADB R1 1
L1: RETURN R0 0
)"
    );
}

TEST_CASE("TableConstantStringIndex")
{
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local t = { a = 2 }
return t['a']
)"),
        R"(
DUPTABLE R0 1
LOADN R1 2
SETTABLEKS R1 R0 K0 ['a']
GETTABLEKS R1 R0 K0 ['a']
RETURN R1 1
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(
local t = {}
t['a'] = 2
)"),
        R"(
NEWTABLE R0 0 0
LOADN R1 2
SETTABLEKS R1 R0 K0 ['a']
RETURN R0 0
)"
    );
}

TEST_CASE("Coverage")
{
    // basic statement coverage
    CHECK_EQ(
        "\n" + compileFunction0Coverage(
                   R"(
print(1)
print(2)
)",
                   1
               ),
        R"(
2: COVERAGE
2: GETIMPORT R0 1 [print]
2: LOADN R1 1
2: CALL R0 1 0
3: COVERAGE
3: GETIMPORT R0 1 [print]
3: LOADN R1 2
3: CALL R0 1 0
4: RETURN R0 0
)"
    );

    // branching
    CHECK_EQ(
        "\n" + compileFunction0Coverage(
                   R"(
if x then
    print(1)
else
    print(2)
end
)",
                   1
               ),
        R"(
2: COVERAGE
2: GETIMPORT R0 1 [x]
2: JUMPIFNOT R0 L0
3: COVERAGE
3: GETIMPORT R0 3 [print]
3: LOADN R1 1
3: CALL R0 1 0
3: RETURN R0 0
5: L0: COVERAGE
5: GETIMPORT R0 3 [print]
5: LOADN R1 2
5: CALL R0 1 0
7: RETURN R0 0
)"
    );

    // branching with comments
    // note that commented lines don't have COVERAGE insns!
    CHECK_EQ(
        "\n" + compileFunction0Coverage(
                   R"(
if x then
    -- first
    print(1)
else
    -- second
    print(2)
end
)",
                   1
               ),
        R"(
2: COVERAGE
2: GETIMPORT R0 1 [x]
2: JUMPIFNOT R0 L0
4: COVERAGE
4: GETIMPORT R0 3 [print]
4: LOADN R1 1
4: CALL R0 1 0
4: RETURN R0 0
7: L0: COVERAGE
7: GETIMPORT R0 3 [print]
7: LOADN R1 2
7: CALL R0 1 0
9: RETURN R0 0
)"
    );

    // expression coverage for table literals
    // note: duplicate COVERAGE instructions are there since we don't deduplicate expr/stat
    CHECK_EQ(
        "\n" + compileFunction0Coverage(
                   R"(
local c = ...
local t = {
    a = 1,
    b = 2,
    c = c
}
)",
                   2
               ),
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
4: SETTABLEKS R2 R1 K0 ['a']
5: COVERAGE
5: COVERAGE
5: LOADN R2 2
5: SETTABLEKS R2 R1 K1 ['b']
6: COVERAGE
6: SETTABLEKS R0 R1 K2 ['c']
8: RETURN R0 0
)"
    );
}

TEST_CASE("ConstantClosure")
{
    // closures without upvalues are created when bytecode is loaded
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
return function() end
)",
                   1
               ),
        R"(
DUPCLOSURE R0 K0 []
RETURN R0 1
)"
    );

    // they can access globals just fine
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
return function() print("hi") end
)",
                   1
               ),
        R"(
DUPCLOSURE R0 K0 []
RETURN R0 1
)"
    );

    // if they need upvalues, we can't create them before running the code (but see SharedClosure test)
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
function test()
    local print = print
    return function() print("hi") end
end
)",
                   1
               ),
        R"(
GETIMPORT R0 1 [print]
NEWCLOSURE R1 P0
CAPTURE VAL R0
RETURN R1 1
)"
    );

    // if they don't need upvalues but we sense that environment may be modified, we disable this to avoid fenv-related identity confusion
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
setfenv(1, {})
return function() print("hi") end
)",
                   1
               ),
        R"(
GETIMPORT R0 1 [setfenv]
LOADN R1 1
NEWTABLE R2 0 0
CALL R0 2 0
NEWCLOSURE R0 P0
RETURN R0 1
)"
    );

    // note that fenv analysis isn't flow-sensitive right now, which is sort of a feature
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
if false then setfenv(1, {}) end
return function() print("hi") end
)",
                   1
               ),
        R"(
NEWCLOSURE R0 P0
RETURN R0 1
)"
    );
}

TEST_CASE("SharedClosure")
{
    // closures can be shared even if functions refer to upvalues, as long as upvalues are top-level
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local val = ...

local function foo()
    return function() return val end
end
)",
                   1
               ),
        R"(
DUPCLOSURE R0 K0 []
CAPTURE UPVAL U0
RETURN R0 1
)"
    );

    // ... as long as the values aren't mutated.
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local val = ...

local function foo()
    return function() return val end
end

val = 5
)",
                   1
               ),
        R"(
NEWCLOSURE R0 P0
CAPTURE UPVAL U0
RETURN R0 1
)"
    );

    // making the upvalue non-toplevel disables the optimization since it's likely that it will change
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(val)
    return function() return val end
end
)",
                   1
               ),
        R"(
NEWCLOSURE R1 P0
CAPTURE VAL R0
RETURN R1 1
)"
    );

    // the upvalue analysis is transitive through local functions, which allows for code reuse to not defeat the optimization
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local val = ...

local function foo()
    local function bar()
        return val
    end

    return function() return bar() end
end
)",
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['bar']
CAPTURE UPVAL U0
DUPCLOSURE R1 K1 []
CAPTURE VAL R0
RETURN R1 1
)"
    );

    // as such, if the upvalue that we reach transitively isn't top-level we fall back to newclosure
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(val)
    local function bar()
        return val
    end

    return function() return bar() end
end
)",
                   2
               ),
        R"(
NEWCLOSURE R1 P0
CAPTURE VAL R0
NEWCLOSURE R2 P1
CAPTURE VAL R1
RETURN R2 1
)"
    );

    // we also allow recursive function captures to share the object, even when it's not top-level
    CHECK_EQ("\n" + compileFunction("function test() local function foo() return foo() end end", 1), R"(
DUPCLOSURE R0 K0 ['foo']
CAPTURE VAL R0
RETURN R0 0
)");

    // multi-level recursive capture where function isn't top-level fails however.
    // note: this should probably be optimized to DUPCLOSURE but doing that requires a different upval tracking flow in the compiler
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo()
    local function bar()
        return function() return bar() end
    end
end
)",
                   1
               ),
        R"(
NEWCLOSURE R0 P0
CAPTURE UPVAL U0
RETURN R0 1
)"
    );

    // top level upvalues inside loops should not be shared -- note that the bytecode below only uses NEWCLOSURE
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
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
                   3
               ),
        R"(
LOADN R2 1
LOADN R0 10
LOADN R1 1
FORNPREP R0 L1
L0: GETIMPORT R3 1 [print]
NEWCLOSURE R4 P0
CAPTURE VAL R2
CALL R3 1 0
FORNLOOP R0 L0
L1: GETIMPORT R0 3 [pairs]
GETVARARGS R1 -1
CALL R0 -1 3
FORGPREP_NEXT R0 L3
L2: GETIMPORT R5 1 [print]
NEWCLOSURE R6 P1
CAPTURE VAL R3
CALL R5 1 0
L3: FORGLOOP R0 L2 2
LOADN R2 1
LOADN R0 10
LOADN R1 1
FORNPREP R0 L5
L4: GETIMPORT R3 1 [print]
NEWCLOSURE R4 P2
CAPTURE VAL R2
CALL R3 1 0
FORNLOOP R0 L4
L5: RETURN R0 0
)"
    );
}

TEST_CASE("MutableGlobals")
{
    const char* source = R"(
print()
Game.print()
Workspace.print()
_G.print()
game.print()
plugin.print()
script.print()
shared.print()
workspace.print()
)";

    // Check Roblox globals are no longer here
    CHECK_EQ("\n" + compileFunction0(source), R"(
GETIMPORT R0 1 [print]
CALL R0 0 0
GETIMPORT R0 3 [Game.print]
CALL R0 0 0
GETIMPORT R0 5 [Workspace.print]
CALL R0 0 0
GETIMPORT R1 7 [_G]
GETTABLEKS R0 R1 K0 ['print']
CALL R0 0 0
GETIMPORT R0 9 [game.print]
CALL R0 0 0
GETIMPORT R0 11 [plugin.print]
CALL R0 0 0
GETIMPORT R0 13 [script.print]
CALL R0 0 0
GETIMPORT R0 15 [shared.print]
CALL R0 0 0
GETIMPORT R0 17 [workspace.print]
CALL R0 0 0
RETURN R0 0
)");

    // Check we can add them back
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::CompileOptions options;
    const char* mutableGlobals[] = {"Game", "Workspace", "game", "plugin", "script", "shared", "workspace", NULL};
    options.mutableGlobals = mutableGlobals;
    Luau::compileOrThrow(bcb, source, options);

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
GETIMPORT R0 1 [print]
CALL R0 0 0
GETIMPORT R1 3 [Game]
GETTABLEKS R0 R1 K0 ['print']
CALL R0 0 0
GETIMPORT R1 5 [Workspace]
GETTABLEKS R0 R1 K0 ['print']
CALL R0 0 0
GETIMPORT R1 7 [_G]
GETTABLEKS R0 R1 K0 ['print']
CALL R0 0 0
GETIMPORT R1 9 [game]
GETTABLEKS R0 R1 K0 ['print']
CALL R0 0 0
GETIMPORT R1 11 [plugin]
GETTABLEKS R0 R1 K0 ['print']
CALL R0 0 0
GETIMPORT R1 13 [script]
GETTABLEKS R0 R1 K0 ['print']
CALL R0 0 0
GETIMPORT R1 15 [shared]
GETTABLEKS R0 R1 K0 ['print']
CALL R0 0 0
GETIMPORT R1 17 [workspace]
GETTABLEKS R0 R1 K0 ['print']
CALL R0 0 0
RETURN R0 0
)");
}

TEST_CASE("ConstantsNoFolding")
{
    const char* source = "return nil, true, 42, 'hello'";

    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::CompileOptions options;
    options.optimizationLevel = 0;
    Luau::compileOrThrow(bcb, source, options);

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
LOADNIL R0
LOADB R1 1
LOADK R2 K0 [42]
LOADK R3 K1 ['hello']
RETURN R0 4
)");
}

TEST_CASE("VectorFastCall")
{
    const char* source = "return Vector3.new(1, 2, 3)";

    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::CompileOptions options;
    options.vectorLib = "Vector3";
    options.vectorCtor = "new";
    Luau::compileOrThrow(bcb, source, options);

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
LOADN R1 1
LOADN R2 2
LOADN R3 3
FASTCALL 54 L0
GETIMPORT R0 2 [Vector3.new]
CALL R0 3 -1
L0: RETURN R0 -1
)");
}

TEST_CASE("VectorFastCall3")
{
    const char* source = R"(
local a, b, c = ...
return Vector3.new(a, b, c)
)";

    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::CompileOptions options;
    options.vectorLib = "Vector3";
    options.vectorCtor = "new";
    Luau::compileOrThrow(bcb, source, options);

    CHECK_EQ("\n" + bcb.dumpFunction(0), R"(
GETVARARGS R0 3
FASTCALL3 54 R0 R1 R2 L0
MOVE R4 R0
MOVE R5 R1
MOVE R6 R2
GETIMPORT R3 2 [Vector3.new]
CALL R3 3 -1
L0: RETURN R3 -1
)");
}

TEST_CASE("VectorConstants")
{
    CHECK_EQ("\n" + compileFunction("return vector.create(1, 2)", 0, 2, 0), R"(
LOADK R0 K0 [1, 2, 0]
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction("return vector.create(1, 2, 3)", 0, 2, 0), R"(
LOADK R0 K0 [1, 2, 3]
RETURN R0 1
)");

    CHECK_EQ("\n" + compileFunction("print(vector.create(1, 2, 3))", 0, 2, 0), R"(
GETIMPORT R0 1 [print]
LOADK R1 K2 [1, 2, 3]
CALL R0 1 0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction("print(vector.create(1, 2, 3, 4))", 0, 2, 0), R"(
GETIMPORT R0 1 [print]
LOADK R1 K2 [1, 2, 3, 4]
CALL R0 1 0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction("return vector.create(0, 0, 0), vector.create(-0, 0, 0)", 0, 2, 0), R"(
LOADK R0 K0 [0, 0, 0]
LOADK R1 K1 [-0, 0, 0]
RETURN R0 2
)");

    CHECK_EQ("\n" + compileFunction("return type(vector.create(0, 0, 0))", 0, 2, 0), R"(
LOADK R0 K0 ['vector']
RETURN R0 1
)");

    // test legacy constructor
    CHECK_EQ("\n" + compileFunction("return Vector3.new(1, 2, 3)", 0, 2, 0, /*enableVectors*/ true), R"(
LOADK R0 K0 [1, 2, 3]
RETURN R0 1
)");
}

TEST_CASE("VectorConstantFields")
{
    CHECK_EQ("\n" + compileFunction("return vector.one, vector.zero", 0, 2), R"(
LOADK R0 K0 [1, 1, 1]
LOADK R1 K1 [0, 0, 0]
RETURN R0 2
)");

    CHECK_EQ("\n" + compileFunction("return Vector3.one, Vector3.xAxis", 0, 2, 0, /*enableVectors*/ true), R"(
LOADK R0 K0 [1, 1, 1]
LOADK R1 K1 [1, 0, 0]
RETURN R0 2
)");

    CHECK_EQ("\n" + compileFunction("return vector.one == vector.create(1, 1, 1)", 0, 2), R"(
LOADB R0 1
RETURN R0 1
)");
}

TEST_CASE("CustomConstantFields")
{
    CHECK_EQ("\n" + compileFunction("return test.some_nil, test.some_boolean, test.some_number, test.some_string", 0, 2), R"(
LOADNIL R0
LOADB R1 1
LOADK R2 K0 [4.75]
LOADK R3 K1 ['test']
RETURN R0 4
)");
}

TEST_CASE("TypeAssertion")
{
    // validate that type assertions work with the compiler and that the code inside type assertion isn't evaluated
    CHECK_EQ(
        "\n" + compileFunction0(R"(
print(foo() :: typeof(error("compile time")))
)"),
        R"(
GETIMPORT R0 1 [print]
GETIMPORT R1 3 [foo]
CALL R1 0 1
CALL R0 1 0
RETURN R0 0
)"
    );

    // note that above, foo() is treated as single-arg function; removing type assertion changes the bytecode
    CHECK_EQ(
        "\n" + compileFunction0(R"(
print(foo())
)"),
        R"(
GETIMPORT R0 1 [print]
GETIMPORT R1 3 [foo]
CALL R1 0 -1
CALL R0 -1 0
RETURN R0 0
)"
    );
}

TEST_CASE("Arithmetics")
{
    // basic arithmetics codegen with non-constants
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local a, b = ...
return a + b, a - b, a / b, a * b, a % b, a ^ b
)"),
        R"(
GETVARARGS R0 2
ADD R2 R0 R1
SUB R3 R0 R1
DIV R4 R0 R1
MUL R5 R0 R1
MOD R6 R0 R1
POW R7 R0 R1
RETURN R2 6
)"
    );

    // basic arithmetics codegen with constants on the right side
    // note that we don't simplify these expressions as we don't know the type of a
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local a = ...
return a + 1, a - 1, a / 1, a * 1, a % 1, a ^ 1
)"),
        R"(
GETVARARGS R0 1
ADDK R1 R0 K0 [1]
SUBK R2 R0 K0 [1]
DIVK R3 R0 K0 [1]
MULK R4 R0 K0 [1]
MODK R5 R0 K0 [1]
POWK R6 R0 K0 [1]
RETURN R1 6
)"
    );
}

TEST_CASE("LoopUnrollBasic")
{
    // forward loops
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local t = {}
for i=1,2 do
    t[i] = i
end
return t
)",
                   0,
                   2
               ),
        R"(
NEWTABLE R0 0 2
LOADN R1 1
SETTABLEN R1 R0 1
LOADN R1 2
SETTABLEN R1 R0 2
RETURN R0 1
)"
    );

    // backward loops
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local t = {}
for i=2,1,-1 do
    t[i] = i
end
return t
)",
                   0,
                   2
               ),
        R"(
NEWTABLE R0 0 0
LOADN R1 2
SETTABLEN R1 R0 2
LOADN R1 1
SETTABLEN R1 R0 1
RETURN R0 1
)"
    );

    // loops with step that doesn't divide to-from
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local t = {}
for i=1,4,2 do
    t[i] = i
end
return t
)",
                   0,
                   2
               ),
        R"(
NEWTABLE R0 0 0
LOADN R1 1
SETTABLEN R1 R0 1
LOADN R1 3
SETTABLEN R1 R0 3
RETURN R0 1
)"
    );

    // empty loops
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
for i=2,1 do
end
)",
                   0,
                   2
               ),
        R"(
RETURN R0 0
)"
    );
}

TEST_CASE("LoopUnrollNested")
{
    // we can unroll nested loops just fine
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local t = {}
for i=0,1 do
    for j=0,1 do
        t[i*2+(j+1)] = 0
    end
end
)",
                   0,
                   2
               ),
        R"(
NEWTABLE R0 0 0
LOADN R1 0
SETTABLEN R1 R0 1
LOADN R1 0
SETTABLEN R1 R0 2
LOADN R1 0
SETTABLEN R1 R0 3
LOADN R1 0
SETTABLEN R1 R0 4
RETURN R0 0
)"
    );

    // if the inner loop is too expensive, we won't unroll the outer loop though, but we'll still unroll the inner loop!
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local t = {}
for i=0,3 do
    for j=0,3 do
        t[i*4+(j+1)] = 0
    end
end
)",
                   0,
                   2
               ),
        R"(
NEWTABLE R0 0 0
LOADN R3 0
LOADN R1 3
LOADN R2 1
FORNPREP R1 L1
L0: MULK R5 R3 K1 [4]
ADDK R4 R5 K0 [1]
LOADN R5 0
SETTABLE R5 R0 R4
MULK R5 R3 K1 [4]
ADDK R4 R5 K2 [2]
LOADN R5 0
SETTABLE R5 R0 R4
MULK R5 R3 K1 [4]
ADDK R4 R5 K3 [3]
LOADN R5 0
SETTABLE R5 R0 R4
MULK R5 R3 K1 [4]
ADDK R4 R5 K1 [4]
LOADN R5 0
SETTABLE R5 R0 R4
FORNLOOP R1 L0
L1: RETURN R0 0
)"
    );

    // note, we sometimes can even unroll a loop with varying internal iterations
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local t = {}
for i=0,1 do
    for j=0,i do
        t[i*2+(j+1)] = 0
    end
end
)",
                   0,
                   2
               ),
        R"(
NEWTABLE R0 0 0
LOADN R1 0
SETTABLEN R1 R0 1
LOADN R1 0
SETTABLEN R1 R0 3
LOADN R1 0
SETTABLEN R1 R0 4
RETURN R0 0
)"
    );
}

TEST_CASE("LoopUnrollUnsupported")
{
    // can't unroll loops with non-constant bounds
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
for i=x,y,z do
end
)",
                   0,
                   2
               ),
        R"(
GETIMPORT R2 1 [x]
GETIMPORT R0 3 [y]
GETIMPORT R1 5 [z]
FORNPREP R0 L1
L0: FORNLOOP R0 L0
L1: RETURN R0 0
)"
    );

    // can't unroll loops with bounds where we can't compute trip count
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
for i=1,1,0 do
end
)",
                   0,
                   2
               ),
        R"(
LOADN R2 1
LOADN R0 1
LOADN R1 0
FORNPREP R0 L1
L0: FORNLOOP R0 L0
L1: RETURN R0 0
)"
    );

    // can't unroll loops with bounds that might be imprecise (non-integer)
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
for i=1,2,0.1 do
end
)",
                   0,
                   2
               ),
        R"(
LOADN R2 1
LOADN R0 2
LOADK R1 K0 [0.10000000000000001]
FORNPREP R0 L1
L0: FORNLOOP R0 L0
L1: RETURN R0 0
)"
    );

    // can't unroll loops if the bounds are too large, as it might overflow trip count math
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
for i=4294967295,4294967296 do
end
)",
                   0,
                   2
               ),
        R"(
LOADK R2 K0 [4294967295]
LOADK R0 K1 [4294967296]
LOADN R1 1
FORNPREP R0 L1
L0: FORNLOOP R0 L0
L1: RETURN R0 0
)"
    );
}

TEST_CASE("LoopUnrollControlFlow")
{
    ScopedFastInt sfis[] = {
        {FInt::LuauCompileLoopUnrollThreshold, 50},
        {FInt::LuauCompileLoopUnrollThresholdMaxBoost, 300},
    };

    // break jumps to the end
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
for i=1,3 do
    if math.random() < 0.5 then
        break
    end
end
)",
                   0,
                   2
               ),
        R"(
GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R0 R1 L0
GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R0 R1 L0
GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R0 R1 L0
L0: RETURN R0 0
)"
    );

    // continue jumps to the next iteration
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
for i=1,3 do
    if math.random() < 0.5 then
        continue
    end
    print(i)
end
)",
                   0,
                   2
               ),
        R"(
GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R0 R1 L0
GETIMPORT R0 5 [print]
LOADN R1 1
CALL R0 1 0
L0: GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R0 R1 L1
GETIMPORT R0 5 [print]
LOADN R1 2
CALL R0 1 0
L1: GETIMPORT R0 2 [math.random]
CALL R0 0 1
LOADK R1 K3 [0.5]
JUMPIFLT R0 R1 L2
GETIMPORT R0 5 [print]
LOADN R1 3
CALL R0 1 0
L2: RETURN R0 0
)"
    );

    // continue needs to properly close upvalues
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
for i=1,1 do
    local j = global(i)
    print(function() return j end)
    if math.random() < 0.5 then
        continue
    end
    j += 1
end
)",
                   1,
                   2
               ),
        R"(
GETIMPORT R0 1 [global]
LOADN R1 1
CALL R0 1 1
GETIMPORT R1 3 [print]
NEWCLOSURE R2 P0
CAPTURE REF R0
CALL R1 1 0
GETIMPORT R1 6 [math.random]
CALL R1 0 1
LOADK R2 K7 [0.5]
JUMPIFNOTLT R1 R2 L0
CLOSEUPVALS R0
RETURN R0 0
L0: ADDK R0 R0 K8 [1]
CLOSEUPVALS R0
RETURN R0 0
)"
    );

    // this weird contraption just disappears
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
for i=1,1 do
    for j=1,1 do
        if i == 1 then
            continue
        else
            break
        end
    end
end
)",
                   0,
                   2
               ),
        R"(
RETURN R0 0
RETURN R0 0
)"
    );
}

TEST_CASE("LoopUnrollNestedClosure")
{
    // if the body has functions that refer to loop variables, we unroll the loop and use MOVE+CAPTURE for upvalues
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
for i=1,2 do
    local x = function() return i end
end
)",
                   1,
                   2
               ),
        R"(
LOADN R1 1
NEWCLOSURE R0 P0
CAPTURE VAL R1
LOADN R1 2
NEWCLOSURE R0 P0
CAPTURE VAL R1
RETURN R0 0
)"
    );
}

TEST_CASE("LoopUnrollCost")
{
    ScopedFastInt sfis[] = {
        {FInt::LuauCompileLoopUnrollThreshold, 25},
        {FInt::LuauCompileLoopUnrollThresholdMaxBoost, 300},
    };

    // loops with short body
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local t = {}
for i=1,10 do
    t[i] = i
end
return t
)",
                   0,
                   2
               ),
        R"(
NEWTABLE R0 0 10
LOADN R1 1
SETTABLEN R1 R0 1
LOADN R1 2
SETTABLEN R1 R0 2
LOADN R1 3
SETTABLEN R1 R0 3
LOADN R1 4
SETTABLEN R1 R0 4
LOADN R1 5
SETTABLEN R1 R0 5
LOADN R1 6
SETTABLEN R1 R0 6
LOADN R1 7
SETTABLEN R1 R0 7
LOADN R1 8
SETTABLEN R1 R0 8
LOADN R1 9
SETTABLEN R1 R0 9
LOADN R1 10
SETTABLEN R1 R0 10
RETURN R0 1
)"
    );

    // loops with body that's too long
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local t = {}
for i=1,100 do
    t[i] = i
end
return t
)",
                   0,
                   2
               ),
        R"(
NEWTABLE R0 0 0
LOADN R3 1
LOADN R1 100
LOADN R2 1
FORNPREP R1 L1
L0: SETTABLE R3 R0 R3
FORNLOOP R1 L0
L1: RETURN R0 1
)"
    );

    // loops with body that's long but has a high boost factor due to constant folding
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local t = {}
for i=1,25 do
    t[i] = i * i * i
end
return t
)",
                   0,
                   2
               ),
        R"(
NEWTABLE R0 0 0
LOADN R1 1
SETTABLEN R1 R0 1
LOADN R1 8
SETTABLEN R1 R0 2
LOADN R1 27
SETTABLEN R1 R0 3
LOADN R1 64
SETTABLEN R1 R0 4
LOADN R1 125
SETTABLEN R1 R0 5
LOADN R1 216
SETTABLEN R1 R0 6
LOADN R1 343
SETTABLEN R1 R0 7
LOADN R1 512
SETTABLEN R1 R0 8
LOADN R1 729
SETTABLEN R1 R0 9
LOADN R1 1000
SETTABLEN R1 R0 10
LOADN R1 1331
SETTABLEN R1 R0 11
LOADN R1 1728
SETTABLEN R1 R0 12
LOADN R1 2197
SETTABLEN R1 R0 13
LOADN R1 2744
SETTABLEN R1 R0 14
LOADN R1 3375
SETTABLEN R1 R0 15
LOADN R1 4096
SETTABLEN R1 R0 16
LOADN R1 4913
SETTABLEN R1 R0 17
LOADN R1 5832
SETTABLEN R1 R0 18
LOADN R1 6859
SETTABLEN R1 R0 19
LOADN R1 8000
SETTABLEN R1 R0 20
LOADN R1 9261
SETTABLEN R1 R0 21
LOADN R1 10648
SETTABLEN R1 R0 22
LOADN R1 12167
SETTABLEN R1 R0 23
LOADN R1 13824
SETTABLEN R1 R0 24
LOADN R1 15625
SETTABLEN R1 R0 25
RETURN R0 1
)"
    );

    // loops with body that's long and doesn't have a high boost factor
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local t = {}
for i=1,10 do
    t[i] = math.abs(math.sin(i))
end
return t
)",
                   0,
                   2
               ),
        R"(
NEWTABLE R0 0 10
LOADN R3 1
LOADN R1 10
LOADN R2 1
FORNPREP R1 L3
L0: FASTCALL1 24 R3 L1
MOVE R6 R3
GETIMPORT R5 2 [math.sin]
CALL R5 1 1
L1: FASTCALL1 2 R5 L2
GETIMPORT R4 4 [math.abs]
CALL R4 1 1
L2: SETTABLE R4 R0 R3
FORNLOOP R1 L0
L3: RETURN R0 1
)"
    );
}

TEST_CASE("LoopUnrollMutable")
{
    // can't unroll loops that mutate iteration variable
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
for i=1,3 do
    i = 3
    print(i) -- should print 3 three times in a row
end
)",
                   0,
                   2
               ),
        R"(
LOADN R2 1
LOADN R0 3
LOADN R1 1
FORNPREP R0 L1
L0: MOVE R3 R2
LOADN R3 3
GETIMPORT R4 1 [print]
MOVE R5 R3
CALL R4 1 0
FORNLOOP R0 L0
L1: RETURN R0 0
)"
    );
}

TEST_CASE("LoopUnrollCostBuiltins")
{
    ScopedFastInt sfis[] = {
        {FInt::LuauCompileLoopUnrollThreshold, 25},
        {FInt::LuauCompileLoopUnrollThresholdMaxBoost, 300},
    };

    // this loop uses builtins and is close to the cost budget so it's important that we model builtins as cheaper than regular calls
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
function cipher(block, nonce)
    for i = 0,3 do
        block[i + 1] = bit32.band(bit32.rshift(nonce, i * 8), 0xff)
    end
end
)",
                   0,
                   2
               ),
        R"(
FASTCALL2K 39 R1 K0 L0 [0]
MOVE R4 R1
LOADK R5 K0 [0]
GETIMPORT R3 3 [bit32.rshift]
CALL R3 2 1
L0: FASTCALL2K 29 R3 K4 L1 [255]
LOADK R4 K4 [255]
GETIMPORT R2 6 [bit32.band]
CALL R2 2 1
L1: SETTABLEN R2 R0 1
FASTCALL2K 39 R1 K7 L2 [8]
MOVE R4 R1
LOADK R5 K7 [8]
GETIMPORT R3 3 [bit32.rshift]
CALL R3 2 1
L2: FASTCALL2K 29 R3 K4 L3 [255]
LOADK R4 K4 [255]
GETIMPORT R2 6 [bit32.band]
CALL R2 2 1
L3: SETTABLEN R2 R0 2
FASTCALL2K 39 R1 K8 L4 [16]
MOVE R4 R1
LOADK R5 K8 [16]
GETIMPORT R3 3 [bit32.rshift]
CALL R3 2 1
L4: FASTCALL2K 29 R3 K4 L5 [255]
LOADK R4 K4 [255]
GETIMPORT R2 6 [bit32.band]
CALL R2 2 1
L5: SETTABLEN R2 R0 3
FASTCALL2K 39 R1 K9 L6 [24]
MOVE R4 R1
LOADK R5 K9 [24]
GETIMPORT R3 3 [bit32.rshift]
CALL R3 2 1
L6: FASTCALL2K 29 R3 K4 L7 [255]
LOADK R4 K4 [255]
GETIMPORT R2 6 [bit32.band]
CALL R2 2 1
L7: SETTABLEN R2 R0 4
RETURN R0 0
)"
    );

    // note that if we break compiler's ability to reason about bit32 builtin the loop is no longer unrolled as it's too expensive
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
bit32 = {}

function cipher(block, nonce)
    for i = 0,3 do
        block[i + 1] = bit32.band(bit32.rshift(nonce, i * 8), 0xff)
    end
end
)",
                   0,
                   2
               ),
        R"(
LOADN R4 0
LOADN R2 3
LOADN R3 1
FORNPREP R2 L1
L0: ADDK R5 R4 K0 [1]
GETGLOBAL R7 K1 ['bit32']
GETTABLEKS R6 R7 K2 ['band']
GETGLOBAL R8 K1 ['bit32']
GETTABLEKS R7 R8 K3 ['rshift']
MOVE R8 R1
MULK R9 R4 K4 [8]
CALL R7 2 1
LOADN R8 255
CALL R6 2 1
SETTABLE R6 R0 R5
FORNLOOP R2 L0
L1: RETURN R0 0
)"
    );

    // additionally, if we pass too many constants the builtin stops being cheap because of argument setup
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
function cipher(block, nonce)
    for i = 0,3 do
        block[i + 1] = bit32.band(bit32.rshift(nonce, i * 8), 0xff, 0xff, 0xff, 0xff, 0xff)
    end
end
)",
                   0,
                   2
               ),
        R"(
LOADN R4 0
LOADN R2 3
LOADN R3 1
FORNPREP R2 L3
L0: ADDK R5 R4 K0 [1]
MULK R9 R4 K1 [8]
FASTCALL2 39 R1 R9 L1
MOVE R8 R1
GETIMPORT R7 4 [bit32.rshift]
CALL R7 2 1
L1: LOADN R8 255
LOADN R9 255
LOADN R10 255
LOADN R11 255
LOADN R12 255
FASTCALL 29 L2
GETIMPORT R6 6 [bit32.band]
CALL R6 6 1
L2: SETTABLE R6 R0 R5
FORNLOOP R2 L0
L3: RETURN R0 0
)"
    );
}

TEST_CASE("InlineBasic")
{
    // inline function that returns a constant
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo()
    return 42
end

local x = foo()
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADN R1 42
RETURN R1 1
)"
    );

    // inline function that returns the argument
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return a
end

local x = foo(42)
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADN R1 42
RETURN R1 1
)"
    );

    // inline function that returns one of the two arguments
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a, b, c)
    if a then
        return b
    else
        return c
    end
end

local x = foo(true, math.random(), 5)
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETIMPORT R2 3 [math.random]
CALL R2 0 1
MOVE R1 R2
RETURN R1 1
RETURN R1 1
)"
    );

    // inline function that returns one of the two arguments
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a, b, c)
    if a then
        return b
    else
        return c
    end
end

local x = foo(true, 5, math.random())
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETIMPORT R2 3 [math.random]
CALL R2 0 1
LOADN R1 5
RETURN R1 1
RETURN R1 1
)"
    );
}

TEST_CASE("InlineProhibited")
{
    // we can't inline variadic functions
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(...)
    return 42
end

local x = foo()
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
MOVE R1 R0
CALL R1 0 1
RETURN R1 1
)"
    );

    // we can't inline any functions in modules with getfenv/setfenv
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo()
    return 42
end

local x = foo()
getfenv()
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
MOVE R1 R0
CALL R1 0 1
GETIMPORT R2 2 [getfenv]
CALL R2 0 0
RETURN R1 1
)"
    );
}

TEST_CASE("InlineProhibitedRecursion")
{
    // we can't inline recursive invocations of functions in the functions
    // this is actually profitable in certain cases, but it complicates the compiler as it means a local has multiple registers/values

    // in this example, inlining is blocked because we're compiling fact() and we don't yet have the cost model / profitability data for fact()
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function fact(n)
    return if n <= 1 then 1 else fact(n-1)*n
end

return fact
)",
                   0,
                   2
               ),
        R"(
LOADN R2 1
JUMPIFNOTLE R0 R2 L0
LOADN R1 1
RETURN R1 1
L0: GETUPVAL R2 0
SUBK R3 R0 K0 [1]
CALL R2 1 1
MUL R1 R2 R0
RETURN R1 1
)"
    );

    // in this example, inlining of fact() succeeds, but the nested call to fact() fails since fact is already on the inline stack
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function fact(n)
    return if n <= 1 then 1 else fact(n-1)*n
end

local function factsafe(n)
    assert(n >= 1)
    return fact(n)
end

return factsafe
)",
                   1,
                   2
               ),
        R"(
LOADN R3 1
JUMPIFLE R3 R0 L0
LOADB R2 0 +1
L0: LOADB R2 1
L1: FASTCALL1 1 R2 L2
GETIMPORT R1 1 [assert]
CALL R1 1 0
L2: LOADN R2 1
JUMPIFNOTLE R0 R2 L3
LOADN R1 1
RETURN R1 1
L3: GETUPVAL R2 0
SUBK R3 R0 K2 [1]
CALL R2 1 1
MUL R1 R2 R0
RETURN R1 1
)"
    );
}

TEST_CASE("InlineNestedLoops")
{
    // functions with basic loops get inlined
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(t)
    for i=1,3 do
        t[i] = i
    end
    return t
end

local x = foo({})
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
NEWTABLE R2 0 0
LOADN R3 1
SETTABLEN R3 R2 1
LOADN R3 2
SETTABLEN R3 R2 2
LOADN R3 3
SETTABLEN R3 R2 3
MOVE R1 R2
RETURN R1 1
)"
    );

    // we can even unroll the loops based on inline argument
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(t, n)
    for i=1, n do
        t[i] = i
    end
    return t
end

local x = foo({}, 3)
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
NEWTABLE R2 0 0
LOADN R3 1
SETTABLEN R3 R2 1
LOADN R3 2
SETTABLEN R3 R2 2
LOADN R3 3
SETTABLEN R3 R2 3
MOVE R1 R2
RETURN R1 1
)"
    );
}

TEST_CASE("InlineNestedClosures")
{
    // we can inline functions that contain/return functions
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(x)
    return function(y) return x + y end
end

local x = foo(1)(2)
return x
)",
                   2,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADN R2 1
NEWCLOSURE R1 P1
CAPTURE VAL R2
LOADN R2 2
CALL R1 1 1
RETURN R1 1
)"
    );
}

TEST_CASE("InlineMutate")
{
    // if the argument is mutated, it gets a register even if the value is constant
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    a = a or 5
    return a
end

local x = foo(42)
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADN R2 42
ORK R2 R2 K1 [5]
MOVE R1 R2
RETURN R1 1
)"
    );

    // if the argument is a local, it can be used directly
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return a
end

local x = ...
local y = foo(x)
return y
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETVARARGS R1 1
MOVE R2 R1
RETURN R2 1
)"
    );

    // ... but if it's mutated, we move it in case it is mutated through a capture during the inlined function
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return a
end

local x = ...
x = nil
local y = foo(x)
return y
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETVARARGS R1 1
LOADNIL R1
MOVE R3 R1
MOVE R2 R3
RETURN R2 1
)"
    );

    // we also don't inline functions if they have been assigned to
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return a
end

foo = foo

local x = foo(42)
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
MOVE R1 R0
LOADN R2 42
CALL R1 1 1
RETURN R1 1
)"
    );
}

TEST_CASE("InlineUpval")
{
    // if the argument is an upvalue, we naturally need to copy it to a local
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return a
end

local b = ...

function bar()
    local x = foo(b)
    return x
end
)",
                   1,
                   2
               ),
        R"(
GETUPVAL R1 0
MOVE R0 R1
RETURN R0 1
)"
    );

    // if the function uses an upvalue it's more complicated, because the lexical upvalue may become a local
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local b = ...

local function foo(a)
    return a + b
end

local x = foo(42)
return x
)",
                   1,
                   2
               ),
        R"(
GETVARARGS R0 1
DUPCLOSURE R1 K0 ['foo']
CAPTURE VAL R0
LOADN R3 42
ADD R2 R3 R0
RETURN R2 1
)"
    );

    // sometimes the lexical upvalue is deep enough that it's still an upvalue though
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local b = ...

function bar()
    local function foo(a)
        return a + b
    end

    local x = foo(42)
    return x
end
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
CAPTURE UPVAL U0
LOADN R2 42
GETUPVAL R3 0
ADD R1 R2 R3
RETURN R1 1
)"
    );
}

TEST_CASE("InlineCapture")
{
    // if the argument is captured by a nested closure, normally we can rely on capture by value
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return function() return a end
end

local x = ...
local y = foo(x)
return y
)",
                   2,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETVARARGS R1 1
NEWCLOSURE R2 P1
CAPTURE VAL R1
RETURN R2 1
)"
    );

    // if the argument is a constant, we move it to a register so that capture by value can happen
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return function() return a end
end

local y = foo(42)
return y
)",
                   2,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADN R2 42
NEWCLOSURE R1 P1
CAPTURE VAL R2
RETURN R1 1
)"
    );

    // if the argument is an externally mutated variable, we copy it to an argument and capture it by value
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return function() return a end
end

local x x = 42
local y = foo(x)
return y
)",
                   2,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADNIL R1
LOADN R1 42
MOVE R3 R1
NEWCLOSURE R2 P1
CAPTURE VAL R3
RETURN R2 1
)"
    );

    // finally, if the argument is mutated internally, we must capture it by reference and close the upvalue
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    a = a or 42
    return function() return a end
end

local y = foo()
return y
)",
                   2,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADNIL R2
ORK R2 R2 K1 [42]
NEWCLOSURE R1 P1
CAPTURE REF R2
CLOSEUPVALS R2
RETURN R1 1
)"
    );

    // note that capture might need to be performed during the fallthrough block
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    a = a or 42
    print(function() return a end)
end

local x = ...
local y = foo(x)
return y
)",
                   2,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETVARARGS R1 1
MOVE R3 R1
ORK R3 R3 K1 [42]
GETIMPORT R4 3 [print]
NEWCLOSURE R5 P1
CAPTURE REF R3
CALL R4 1 0
LOADNIL R2
CLOSEUPVALS R3
RETURN R2 1
)"
    );

    // note that mutation and capture might be inside internal control flow
    // TODO: this has an oddly redundant CLOSEUPVALS after JUMP; it's not due to inlining, and is an artifact of how StatBlock/StatReturn interact
    // fixing this would reduce the number of redundant CLOSEUPVALS a bit but it only affects bytecode size as these instructions aren't executed
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    if not a then
        local b b = 42
        return function() return b end
    end
end

local x = ...
local y = foo(x)
return y, x
)",
                   2,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETVARARGS R1 1
JUMPIF R1 L0
LOADNIL R3
LOADN R3 42
NEWCLOSURE R2 P1
CAPTURE REF R3
CLOSEUPVALS R3
JUMP L1
CLOSEUPVALS R3
L0: LOADNIL R2
L1: MOVE R3 R2
MOVE R4 R1
RETURN R3 2
)"
    );
}

TEST_CASE("InlineFallthrough")
{
    // if the function doesn't return, we still fill the results with nil
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo()
end

local a, b = foo()
return a, b
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADNIL R1
LOADNIL R2
RETURN R1 2
)"
    );

    // this happens even if the function returns conditionally
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    if a then return 42 end
end

local a, b = foo(false)
return a, b
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADNIL R1
LOADNIL R2
RETURN R1 2
)"
    );

    // note though that we can't inline a function like this in multret context
    // this is because we don't have a SETTOP instruction
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo()
end

return foo()
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
MOVE R1 R0
CALL R1 0 -1
RETURN R1 -1
)"
    );
}

TEST_CASE("InlineArgMismatch")
{
    // when inlining a function, we must respect all the usual rules

    // caller might not have enough arguments
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return a
end

local x = foo()
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADNIL R1
RETURN R1 1
)"
    );

    // caller might be using multret for arguments
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a, b)
    return a + b
end

local x = foo(math.modf(1.5))
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADK R3 K1 [1.5]
FASTCALL1 20 R3 L0
GETIMPORT R2 4 [math.modf]
CALL R2 1 2
L0: ADD R1 R2 R3
RETURN R1 1
)"
    );

    // caller might be using varargs for arguments
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a, b)
    return a + b
end

local x = foo(...)
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETVARARGS R2 2
ADD R1 R2 R3
RETURN R1 1
)"
    );

    // caller might have too many arguments, but we still need to compute them for side effects
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return a
end

local x = foo(42, print())
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETIMPORT R2 2 [print]
CALL R2 0 1
LOADN R1 42
RETURN R1 1
)"
    );

    // caller might not have enough arguments, and the arg might be mutated so it needs a register
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    a = 42
    return a
end

local x = foo()
return x
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADNIL R2
LOADN R2 42
MOVE R1 R2
RETURN R1 1
)"
    );
}

TEST_CASE("InlineMultiple")
{
    // we call this with a different set of variable/constant args
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a, b)
    return a + b
end

local x, y = ...
local a = foo(x, 1)
local b = foo(1, x)
local c = foo(1, 2)
local d = foo(x, y)
return a, b, c, d
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETVARARGS R1 2
ADDK R3 R1 K1 [1]
LOADN R5 1
ADD R4 R5 R1
LOADN R5 3
ADD R6 R1 R2
RETURN R3 4
)"
    );
}

TEST_CASE("InlineChain")
{
    // inline a chain of functions
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a, b)
    return a + b
end

local function bar(x)
    return foo(x, 1) * foo(x, -1)
end

local function baz()
    return (bar(42))
end

return (baz())
)",
                   3,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
DUPCLOSURE R1 K1 ['bar']
DUPCLOSURE R2 K2 ['baz']
LOADN R4 43
LOADN R5 41
MUL R3 R4 R5
RETURN R3 1
)"
    );
}

TEST_CASE("InlineThresholds")
{
    ScopedFastInt sfis[] = {
        {FInt::LuauCompileInlineThreshold, 25},
        {FInt::LuauCompileInlineThresholdMaxBoost, 300},
        {FInt::LuauCompileInlineDepth, 2},
    };

    // this function has enormous register pressure (50 regs) so we choose not to inline it
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo()
    return {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
end

return (foo())
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
MOVE R1 R0
CALL R1 0 1
RETURN R1 1
)"
    );

    // this function has less register pressure but a large cost
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo()
    return {},{},{},{},{}
end

return (foo())
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
MOVE R1 R0
CALL R1 0 1
RETURN R1 1
)"
    );

    // this chain of function is of length 3 but our limit in this test is 2, so we call foo twice
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a, b)
    return a + b
end

local function bar(x)
    return foo(x, 1) * foo(x, -1)
end

local function baz()
    return (bar(42))
end

return (baz())
)",
                   3,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
DUPCLOSURE R1 K1 ['bar']
DUPCLOSURE R2 K2 ['baz']
MOVE R4 R0
LOADN R5 42
LOADN R6 1
CALL R4 2 1
MOVE R5 R0
LOADN R6 42
LOADN R7 -1
CALL R5 2 1
MUL R3 R4 R5
RETURN R3 1
)"
    );
}

TEST_CASE("InlineIIFE")
{
    // IIFE with arguments
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
function choose(a, b, c)
    return ((function(a, b, c) if a then return b else return c end end)(a, b, c))
end
)",
                   1,
                   2
               ),
        R"(
JUMPIFNOT R0 L0
MOVE R3 R1
RETURN R3 1
L0: MOVE R3 R2
RETURN R3 1
RETURN R3 1
)"
    );

    // IIFE with upvalues
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
function choose(a, b, c)
    return ((function() if a then return b else return c end end)())
end
)",
                   1,
                   2
               ),
        R"(
JUMPIFNOT R0 L0
MOVE R3 R1
RETURN R3 1
L0: MOVE R3 R2
RETURN R3 1
RETURN R3 1
)"
    );
}

TEST_CASE("InlineRecurseArguments")
{
    // the example looks silly but we preserve it verbatim as it was found by fuzzer for a previous version of the compiler
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a, b)
end
foo(foo(foo,foo(foo,foo))[foo])
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADNIL R3
LOADNIL R2
GETTABLE R1 R2 R0
RETURN R0 0
)"
    );

    // verify that invocations of the inlined function in any position for computing the arguments to itself compile
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a, b)
    return a + b
end

local x, y, z = ...

return foo(foo(x, y), foo(z, 1))
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETVARARGS R1 3
ADD R5 R1 R2
ADDK R6 R3 K1 [1]
ADD R4 R5 R6
RETURN R4 1
)"
    );

    // verify that invocations of the inlined function in any position for computing the arguments to itself compile, including constants and locals
    // note that foo(k1, k2) doesn't get constant folded, so there's still actual math emitted for some of the calls below
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a, b)
    return a + b
end

local x, y, z = ...

return
    foo(foo(1, 2), 3),
    foo(1, foo(2, 3)),
    foo(x, foo(2, 3)),
    foo(x, foo(y, 3)),
    foo(x, foo(y, z)),
    foo(x+0, foo(y, z)),
    foo(x+0, foo(y+0, z)),
    foo(x+0, foo(y, z+0)),
    foo(1, foo(x, y))
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETVARARGS R1 3
LOADN R5 3
ADDK R4 R5 K1 [3]
LOADN R6 5
LOADN R7 1
ADD R5 R7 R6
LOADN R7 5
ADD R6 R1 R7
ADDK R8 R2 K1 [3]
ADD R7 R1 R8
ADD R9 R2 R3
ADD R8 R1 R9
ADDK R10 R1 K2 [0]
ADD R11 R2 R3
ADD R9 R10 R11
ADDK R11 R1 K2 [0]
ADDK R13 R2 K2 [0]
ADD R12 R13 R3
ADD R10 R11 R12
ADDK R12 R1 K2 [0]
ADDK R14 R3 K2 [0]
ADD R13 R2 R14
ADD R11 R12 R13
ADD R13 R1 R2
LOADN R14 1
ADD R12 R14 R13
RETURN R4 9
)"
    );
}

TEST_CASE("InlineFastCallK")
{
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function set(l0)
    rawset({}, l0)
end

set(false)
set({})
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['set']
NEWTABLE R2 0 0
FASTCALL2K 49 R2 K1 L0 [false]
LOADK R3 K1 [false]
GETIMPORT R1 3 [rawset]
CALL R1 2 0
L0: NEWTABLE R1 0 0
NEWTABLE R3 0 0
FASTCALL2 49 R3 R1 L1
MOVE R4 R1
GETIMPORT R2 3 [rawset]
CALL R2 2 0
L1: RETURN R0 0
)"
    );
}

TEST_CASE("InlineExprIndexK")
{
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local _ = function(l0)
local _ = nil
while _(_)[_] do
end
end
local _ = _(0)[""]
if _ then
do
for l0=0,8 do
end
end
elseif _ then
_ = nil
do
for l0=0,8 do
return true
end
end
end
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 []
L0: LOADNIL R4
LOADNIL R5
CALL R4 1 1
LOADNIL R5
GETTABLE R3 R4 R5
JUMPIFNOT R3 L1
JUMPBACK L0
L1: LOADNIL R2
GETTABLEKS R1 R2 K1 ['']
JUMPIFNOT R1 L2
RETURN R0 0
L2: JUMPIFNOT R1 L3
LOADNIL R1
LOADB R2 1
RETURN R2 1
LOADB R2 1
RETURN R2 1
LOADB R2 1
RETURN R2 1
LOADB R2 1
RETURN R2 1
LOADB R2 1
RETURN R2 1
LOADB R2 1
RETURN R2 1
LOADB R2 1
RETURN R2 1
LOADB R2 1
RETURN R2 1
LOADB R2 1
RETURN R2 1
L3: RETURN R0 0
)"
    );
}

TEST_CASE("InlineHiddenMutation")
{
    // when the argument is assigned inside the function, we can't reuse the local
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    a = 42
    return a
end

local x = ...
local y = foo(x :: number)
return y
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETVARARGS R1 1
MOVE R3 R1
LOADN R3 42
MOVE R2 R3
RETURN R2 1
)"
    );

    // and neither can we do that when it's assigned outside the function
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    mutator()
    return a
end

local x = ...
mutator = function() x = 42 end

local y = foo(x :: number)
return y
)",
                   2,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
GETVARARGS R1 1
NEWCLOSURE R2 P1
CAPTURE REF R1
SETGLOBAL R2 K1 ['mutator']
MOVE R3 R1
GETGLOBAL R4 K1 ['mutator']
CALL R4 0 0
MOVE R2 R3
CLOSEUPVALS R1
RETURN R2 1
)"
    );
}

TEST_CASE("InlineMultret")
{
    // inlining a function in multret context is prohibited since we can't adjust L->top outside of CALL/GETVARARGS
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return a()
end

return foo(42)
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
MOVE R1 R0
LOADN R2 42
CALL R1 1 -1
RETURN R1 -1
)"
    );

    // however, if we can deduce statically that a function always returns a single value, the inlining will work
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return a
end

return foo(42)
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADN R1 42
RETURN R1 1
)"
    );

    // this analysis will also propagate through other functions
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return a
end

local function bar(a)
    return foo(a)
end

return bar(42)
)",
                   2,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
DUPCLOSURE R1 K1 ['bar']
LOADN R2 42
RETURN R2 1
)"
    );

    // we currently don't do this analysis fully for recursive functions since they can't be inlined anyway
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return foo(a)
end

return foo(42)
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
CAPTURE VAL R0
MOVE R1 R0
LOADN R2 42
CALL R1 1 -1
RETURN R1 -1
)"
    );

    // we do this for builtins though as we assume getfenv is not used or is not changing arity
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return math.abs(a)
end

return foo(42)
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADN R1 42
RETURN R1 1
)"
    );
}

TEST_CASE("InlineNonConstInitializers")
{
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function caller(f)
    f(1)
end

local function callback(n)
    print(n + 5)
end

caller(callback)
)",
                   2,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['caller']
DUPCLOSURE R1 K1 ['callback']
GETIMPORT R2 3 [print]
LOADN R3 6
CALL R2 1 0
RETURN R0 0
)"
    );
}

TEST_CASE("InlineNonConstInitializers2")
{
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local x, y, z = ...
local function test(a, b, c, comp)
    return comp(a, b) and comp(b, c)
end

local function greater(a, b)
    return a > b
end

test(x, y, z, greater)
)",
                   2,
                   2
               ),
        R"(
GETVARARGS R0 3
DUPCLOSURE R3 K0 ['test']
DUPCLOSURE R4 K1 ['greater']
JUMPIFLT R1 R0 L0
LOADB R5 0 +1
L0: LOADB R5 1
L1: JUMPIFNOT R5 L3
JUMPIFLT R2 R1 L2
LOADB R5 0 +1
L2: LOADB R5 1
L3: RETURN R0 0
)"
    );
}

TEST_CASE("ReturnConsecutive")
{
    // we can return a single local directly
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x = ...
return x
)"),
        R"(
GETVARARGS R0 1
RETURN R0 1
)"
    );

    // or multiple, when they are allocated in consecutive registers
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x, y = ...
return x, y
)"),
        R"(
GETVARARGS R0 2
RETURN R0 2
)"
    );

    // but not if it's an expression
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x, y = ...
return x, y + 1
)"),
        R"(
GETVARARGS R0 2
MOVE R2 R0
ADDK R3 R1 K0 [1]
RETURN R2 2
)"
    );

    // or a local with wrong register number
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x, y = ...
return y, x
)"),
        R"(
GETVARARGS R0 2
MOVE R2 R1
MOVE R3 R0
RETURN R2 2
)"
    );

    // also double check the optimization doesn't trip on no-argument return (these are rare)
    CHECK_EQ(
        "\n" + compileFunction0(R"(
return
)"),
        R"(
RETURN R0 0
)"
    );

    // this optimization also works in presence of group / type casts
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x, y = ...
return (x), y :: number
)"),
        R"(
GETVARARGS R0 2
RETURN R0 2
)"
    );
}

TEST_CASE("OptimizationLevel")
{
    // at optimization level 1, no inlining is performed
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return a
end

return foo(42)
)",
                   1,
                   1
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
MOVE R1 R0
LOADN R2 42
CALL R1 1 -1
RETURN R1 -1
)"
    );

    // you can override the level from 1 to 2 to force it
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
--!optimize 2
local function foo(a)
    return a
end

return foo(42)
)",
                   1,
                   1
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADN R1 42
RETURN R1 1
)"
    );

    // you can also override it externally
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function foo(a)
    return a
end

return foo(42)
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
LOADN R1 42
RETURN R1 1
)"
    );

    // ... after which you can downgrade it back via hot comment
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
--!optimize 1
local function foo(a)
    return a
end

return foo(42)
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['foo']
MOVE R1 R0
LOADN R2 42
CALL R1 1 -1
RETURN R1 -1
)"
    );
}

TEST_CASE("BuiltinFolding")
{
    ScopedFastFlag luauCompileTypeofFold{FFlag::LuauCompileTypeofFold, true};

    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
return
    math.abs(-42),
    math.acos(1),
    math.asin(0),
    math.atan2(0, 1),
    math.atan(0),
    math.ceil(1.5),
    math.cosh(0),
    math.cos(0),
    math.deg(3.14159265358979323846),
    math.exp(0),
    math.floor(-1.5),
    math.fmod(7, 3),
    math.ldexp(0.5, 3),
    math.log10(100),
    math.log(1),
    math.log(4, 2),
    math.log(64, 4),
    math.max(1, 2, 3),
    math.min(1, 2, 3),
    math.pow(3, 3),
    math.floor(math.rad(180)),
    math.sinh(0),
    math.sin(0),
    math.sqrt(9),
    math.tanh(0),
    math.tan(0),
    bit32.arshift(-10, 1),
    bit32.arshift(10, 1),
    bit32.band(1, 3),
    bit32.bnot(-2),
    bit32.bor(1, 2),
    bit32.bxor(3, 7),
    bit32.btest(1, 3),
    bit32.extract(100, 1, 3),
    bit32.lrotate(100, -1),
    bit32.lshift(100, 1),
    bit32.replace(100, 5, 1, 3),
    bit32.rrotate(100, -1),
    bit32.rshift(100, 1),
    type(100),
    string.byte("a"),
    string.byte("abc", 2),
    string.len("abc"),
    typeof(true),
    math.clamp(-1, 0, 1),
    math.sign(77),
    math.round(7.6),
    bit32.extract(-1, 31),
    bit32.replace(100, 1, 0),
    math.log(100, 10),
    typeof(nil),
    type(vector.create(1, 0, 0)),
    (type("fin"))
)",
                   0,
                   2
               ),
        R"(
LOADN R0 42
LOADN R1 0
LOADN R2 0
LOADN R3 0
LOADN R4 0
LOADN R5 2
LOADN R6 1
LOADN R7 1
LOADN R8 180
LOADN R9 1
LOADN R10 -2
LOADN R11 1
LOADN R12 4
LOADN R13 2
LOADN R14 0
LOADN R15 2
LOADN R16 3
LOADN R17 3
LOADN R18 1
LOADN R19 27
LOADN R20 3
LOADN R21 0
LOADN R22 0
LOADN R23 3
LOADN R24 0
LOADN R25 0
LOADK R26 K0 [4294967291]
LOADN R27 5
LOADN R28 1
LOADN R29 1
LOADN R30 3
LOADN R31 4
LOADB R32 1
LOADN R33 2
LOADN R34 50
LOADN R35 200
LOADN R36 106
LOADN R37 200
LOADN R38 50
LOADK R39 K1 ['number']
LOADN R40 97
LOADN R41 98
LOADN R42 3
LOADK R43 K2 ['boolean']
LOADN R44 0
LOADN R45 1
LOADN R46 8
LOADN R47 1
LOADN R48 101
LOADN R49 2
LOADK R50 K3 ['nil']
LOADK R51 K4 ['vector']
LOADK R52 K5 ['string']
RETURN R0 53
)"
    );
}

TEST_CASE("BuiltinFoldingProhibited")
{
    ScopedFastFlag luauCompileTypeofFold{FFlag::LuauCompileTypeofFold, true};

    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
return
    math.abs(),
    math.max(1, true),
    string.byte("abc", 42),
    bit32.rshift(10, 42),
    bit32.extract(1, 2, "3"),
    bit32.bor(1, true),
    bit32.band(1, true),
    bit32.bxor(1, true),
    bit32.btest(1, true),
    math.min(1, true),
    typeof(vector.create(1, 0, 0))
)",
                   0,
                   2
               ),
        R"(
FASTCALL 2 L0
GETIMPORT R0 2 [math.abs]
CALL R0 0 1
L0: LOADN R2 1
FASTCALL2K 18 R2 K3 L1 [true]
LOADK R3 K3 [true]
GETIMPORT R1 5 [math.max]
CALL R1 2 1
L1: LOADK R3 K6 ['abc']
FASTCALL2K 41 R3 K7 L2 [42]
LOADK R4 K7 [42]
GETIMPORT R2 10 [string.byte]
CALL R2 2 1
L2: LOADN R4 10
FASTCALL2K 39 R4 K7 L3 [42]
LOADK R5 K7 [42]
GETIMPORT R3 13 [bit32.rshift]
CALL R3 2 1
L3: LOADN R5 1
LOADN R6 2
LOADK R7 K14 ['3']
FASTCALL 34 L4
GETIMPORT R4 16 [bit32.extract]
CALL R4 3 1
L4: LOADN R6 1
FASTCALL2K 31 R6 K3 L5 [true]
LOADK R7 K3 [true]
GETIMPORT R5 18 [bit32.bor]
CALL R5 2 1
L5: LOADN R7 1
FASTCALL2K 29 R7 K3 L6 [true]
LOADK R8 K3 [true]
GETIMPORT R6 20 [bit32.band]
CALL R6 2 1
L6: LOADN R8 1
FASTCALL2K 32 R8 K3 L7 [true]
LOADK R9 K3 [true]
GETIMPORT R7 22 [bit32.bxor]
CALL R7 2 1
L7: LOADN R9 1
FASTCALL2K 33 R9 K3 L8 [true]
LOADK R10 K3 [true]
GETIMPORT R8 24 [bit32.btest]
CALL R8 2 1
L8: LOADN R10 1
FASTCALL2K 19 R10 K3 L9 [true]
LOADK R11 K3 [true]
GETIMPORT R9 26 [math.min]
CALL R9 2 1
L9: LOADK R11 K27 [1, 0, 0]
FASTCALL1 44 R11 L10
GETIMPORT R10 29 [typeof]
CALL R10 1 1
L10: RETURN R0 11
)"
    );
}

TEST_CASE("BuiltinFoldingProhibitedCoverage")
{
    const char* builtins[] = {
        "math.abs",  "math.acos",   "math.asin",   "math.atan2",    "math.atan",     "math.ceil",    "math.cosh",     "math.cos",      "math.deg",
        "math.exp",  "math.floor",  "math.fmod",   "math.ldexp",    "math.log10",    "math.log",     "math.max",      "math.min",      "math.pow",
        "math.rad",  "math.sinh",   "math.sin",    "math.sqrt",     "math.tanh",     "math.tan",     "bit32.arshift", "bit32.band",    "bit32.bnot",
        "bit32.bor", "bit32.bxor",  "bit32.btest", "bit32.extract", "bit32.lrotate", "bit32.lshift", "bit32.replace", "bit32.rrotate", "bit32.rshift",
        "type",      "string.byte", "string.len",  "typeof",        "math.clamp",    "math.sign",    "math.round",
    };

    for (const char* func : builtins)
    {
        std::string source = "return ";
        source += func;
        source += "()";

        std::string bc = compileFunction(source.c_str(), 0, 2);

        CHECK(bc.find("FASTCALL") != std::string::npos);
    }
}

TEST_CASE("BuiltinFoldingMultret")
{
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local NoLanes: Lanes = --[[                             ]] 0b0000000000000000000000000000000
local OffscreenLane: Lane = --[[                        ]] 0b1000000000000000000000000000000

local function getLanesToRetrySynchronouslyOnError(root: FiberRoot): Lanes
    local everythingButOffscreen = bit32.band(root.pendingLanes, bit32.bnot(OffscreenLane))
    if everythingButOffscreen ~= NoLanes then
        return everythingButOffscreen
    end
    if bit32.band(everythingButOffscreen, OffscreenLane) ~= 0 then
        return OffscreenLane
    end
    return NoLanes
end
)",
                   0,
                   2
               ),
        R"(
GETTABLEKS R2 R0 K0 ['pendingLanes']
FASTCALL2K 29 R2 K1 L0 [3221225471]
LOADK R3 K1 [3221225471]
GETIMPORT R1 4 [bit32.band]
CALL R1 2 1
L0: JUMPXEQKN R1 K5 L1 [0]
RETURN R1 1
L1: FASTCALL2K 29 R1 K6 L2 [1073741824]
MOVE R3 R1
LOADK R4 K6 [1073741824]
GETIMPORT R2 4 [bit32.band]
CALL R2 2 1
L2: JUMPXEQKN R2 K5 L3 [0]
LOADK R2 K6 [1073741824]
RETURN R2 1
L3: LOADN R2 0
RETURN R2 1
)"
    );

    // Note: similarly, here we should have folded the return value but haven't because it's the last call in the sequence
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
return math.abs(-42)
)",
                   0,
                   2
               ),
        R"(
LOADN R0 42
RETURN R0 1
)"
    );
}

TEST_CASE("BuiltinFoldingProhibitedInOptions")
{
    Luau::BytecodeBuilder bcb;
    bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code);
    Luau::CompileOptions options;
    options.optimizationLevel = 2;

    // math.floor from the test is excluded in this list on purpose
    static const char* kDisabledBuiltins[] = {"tostring", "math.abs", "math.sqrt", nullptr};
    options.disabledBuiltins = kDisabledBuiltins;

    Luau::compileOrThrow(bcb, "return math.abs(-42), math.floor(-1.5), math.sqrt(9), (tostring(2))", options);

    std::string result = bcb.dumpFunction(0);

    CHECK_EQ(
        "\n" + result,
        R"(
GETIMPORT R0 2 [math.abs]
LOADN R1 -42
CALL R0 1 1
LOADN R1 -2
GETIMPORT R2 4 [math.sqrt]
LOADN R3 9
CALL R2 1 1
GETIMPORT R3 6 [tostring]
LOADN R4 2
CALL R3 1 1
RETURN R0 4
)"
    );
}

TEST_CASE("LocalReassign")
{
    // locals can be re-assigned and the register gets reused
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local function test(a, b)
    local c = a
    return c + b
end
)"),
        R"(
ADD R2 R0 R1
RETURN R2 1
)"
    );

    // this works if the expression is using type casts or grouping
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local function test(a, b)
    local c = (a :: number)
    return c + b
end
)"),
        R"(
ADD R2 R0 R1
RETURN R2 1
)"
    );

    // the optimization requires that neither local is mutated
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local function test(a, b)
    local c = a
    c += 0
    local d = b
    b += 0
    return c + d
end
)"),
        R"(
MOVE R2 R0
ADDK R2 R2 K0 [0]
MOVE R3 R1
ADDK R1 R1 K0 [0]
ADD R4 R2 R3
RETURN R4 1
)"
    );

    // sanity check for two values
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local function test(a, b)
    local c = a
    local d = b
    return c + d
end
)"),
        R"(
ADD R2 R0 R1
RETURN R2 1
)"
    );

    // note: we currently only support this for single assignments
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local function test(a, b)
    local c, d = a, b
    return c + d
end
)"),
        R"(
MOVE R2 R0
MOVE R3 R1
ADD R4 R2 R3
RETURN R4 1
)"
    );

    // of course, captures capture the original register as well (by value since it's immutable)
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function test(a, b)
    local c = a
    local d = b
    return function() return c + d end
end
)",
                   1
               ),
        R"(
NEWCLOSURE R2 P0
CAPTURE VAL R0
CAPTURE VAL R1
RETURN R2 1
)"
    );
}

TEST_CASE("MultipleAssignments")
{
    // order of assignments is left to right
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b
        a, b = f(1), f(2)
    )"),
        R"(
LOADNIL R0
LOADNIL R1
GETIMPORT R2 1 [f]
LOADN R3 1
CALL R2 1 1
MOVE R0 R2
GETIMPORT R2 1 [f]
LOADN R3 2
CALL R2 1 1
MOVE R1 R2
RETURN R0 0
)"
    );

    // this includes table assignments
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local t
        t[1], t[2] = 3, 4
    )"),
        R"(
LOADNIL R0
LOADNIL R1
LOADN R2 3
LOADN R3 4
SETTABLEN R2 R0 1
SETTABLEN R3 R1 2
RETURN R0 0
)"
    );

    // semantically, we evaluate the right hand side first; this allows us to e.g swap elements in a table easily
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local t = ...
        t[1], t[2] = t[2], t[1]
    )"),
        R"(
GETVARARGS R0 1
GETTABLEN R1 R0 2
GETTABLEN R2 R0 1
SETTABLEN R1 R0 1
SETTABLEN R2 R0 2
RETURN R0 0
)"
    );

    // however, we need to optimize local assignments; to do this well, we need to handle assignment conflicts
    // let's first go through a few cases where there are no conflicts:

    // when multiple assignments have no conflicts (all local vars are read after being assigned), codegen is the same as a series of single
    // assignments
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local xm1, x, xp1, xi = ...

        xm1,x,xp1,xi = x,xp1,xp1+1,xi-1
    )"),
        R"(
GETVARARGS R0 4
MOVE R0 R1
MOVE R1 R2
ADDK R2 R2 K0 [1]
SUBK R3 R3 K0 [1]
RETURN R0 0
)"
    );

    // similar example to above from a more complex case
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b, c, d, e, f, g, h, t1, t2 = ...

        h, g, f, e, d, c, b, a = g, f, e, d + t1, c, b, a, t1 + t2
    )"),
        R"(
GETVARARGS R0 10
MOVE R7 R6
MOVE R6 R5
MOVE R5 R4
ADD R4 R3 R8
MOVE R3 R2
MOVE R2 R1
MOVE R1 R0
ADD R0 R8 R9
RETURN R0 0
)"
    );

    // when locals have a conflict, we assign temporaries instead of locals, and at the end copy the values back
    // the basic example of this is a swap/rotate
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b = ...
        a, b = b, a
    )"),
        R"(
GETVARARGS R0 2
MOVE R2 R1
MOVE R1 R0
MOVE R0 R2
RETURN R0 0
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b, c = ...
        a, b, c = c, a, b
    )"),
        R"(
GETVARARGS R0 3
MOVE R3 R2
MOVE R4 R0
MOVE R2 R1
MOVE R0 R3
MOVE R1 R4
RETURN R0 0
)"
    );

    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b, c = ...
        a, b, c = b, c, a
    )"),
        R"(
GETVARARGS R0 3
MOVE R3 R1
MOVE R1 R2
MOVE R2 R0
MOVE R0 R3
RETURN R0 0
)"
    );

    // multiple assignments with multcall handling - foo() evalutes to temporary registers and they are copied out to target
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b, c, d = ...
        a, b, c, d = 1, foo()
    )"),
        R"(
GETVARARGS R0 4
LOADN R0 1
GETIMPORT R4 1 [foo]
CALL R4 0 3
MOVE R1 R4
MOVE R2 R5
MOVE R3 R6
RETURN R0 0
)"
    );

    // note that during this we still need to handle local reassignment, eg when table assignments are performed
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b, c, d = ...
        a, b[a], c[d], d = 1, foo()
    )"),
        R"(
GETVARARGS R0 4
LOADN R4 1
GETIMPORT R6 1 [foo]
CALL R6 0 3
SETTABLE R6 R1 R0
SETTABLE R7 R2 R3
MOVE R0 R4
MOVE R3 R8
RETURN R0 0
)"
    );

    // multiple assignments with multcall handling - foo evaluates to a single argument so all remaining locals are assigned to nil
    // note that here we don't assign the locals directly, as this case is very rare so we use the similar code path as above
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b, c, d = ...
        a, b, c, d = 1, foo
    )"),
        R"(
GETVARARGS R0 4
LOADN R0 1
GETIMPORT R4 1 [foo]
LOADNIL R5
LOADNIL R6
MOVE R1 R4
MOVE R2 R5
MOVE R3 R6
RETURN R0 0
)"
    );

    // note that we also try to use locals as a source of assignment directly when assigning fields; this works using old local value when possible
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b = ...
        a[1], a[2] = b, b + 1
    )"),
        R"(
GETVARARGS R0 2
ADDK R2 R1 K0 [1]
SETTABLEN R1 R0 1
SETTABLEN R2 R0 2
RETURN R0 0
)"
    );

    // ... of course if the local is reassigned, we defer the assignment until later
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b = ...
        b, a[1] = 42, b
    )"),
        R"(
GETVARARGS R0 2
LOADN R2 42
SETTABLEN R1 R0 1
MOVE R1 R2
RETURN R0 0
)"
    );

    // when there are more expressions when values, we evalute them for side effects, but they also participate in conflict handling
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b = ...
        a, b = 1, 2, a + b
    )"),
        R"(
GETVARARGS R0 2
LOADN R2 1
LOADN R3 2
ADD R4 R0 R1
MOVE R0 R2
MOVE R1 R3
RETURN R0 0
)"
    );

    // because we perform assignments to complex l-values after assignments to locals, we make sure register conflicts are tracked accordingly
    CHECK_EQ(
        "\n" + compileFunction0(R"(
        local a, b = ...
        a[1], b = b, b + 1
    )"),
        R"(
GETVARARGS R0 2
ADDK R2 R1 K0 [1]
SETTABLEN R1 R0 1
MOVE R1 R2
RETURN R0 0
)"
    );
}

TEST_CASE("BuiltinExtractK")
{
    // below, K0 refers to a packed f+w constant for bit32.extractk builtin
    // K1 and K2 refer to 1 and 3 and are only used during fallback path
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local v = ...

return bit32.extract(v, 1, 3)
)"),
        R"(
GETVARARGS R0 1
FASTCALL2K 59 R0 K0 L0 [65]
MOVE R2 R0
LOADK R3 K1 [1]
LOADK R4 K2 [3]
GETIMPORT R1 5 [bit32.extract]
CALL R1 3 -1
L0: RETURN R1 -1
)"
    );
}

TEST_CASE("SkipSelfAssignment")
{
    CHECK_EQ("\n" + compileFunction0("local a a = a"), R"(
LOADNIL R0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a a = a :: number"), R"(
LOADNIL R0
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a a = (((a)))"), R"(
LOADNIL R0
RETURN R0 0
)");

    // Keep it on optimization level 0
    CHECK_EQ("\n" + compileFunction("local a a = a", 0, 0), R"(
LOADNIL R0
MOVE R0 R0
RETURN R0 0
)");
}

TEST_CASE("ElideJumpAfterIf")
{
    // break refers to outer loop => we can elide unconditional branches
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local foo, bar = ...
repeat
    if foo then break
    elseif bar then break
    end
    print(1234)
until foo == bar
)"),
        R"(
GETVARARGS R0 2
L0: JUMPIFNOT R0 L1
RETURN R0 0
L1: JUMPIF R1 L2
GETIMPORT R2 1 [print]
LOADN R3 1234
CALL R2 1 0
JUMPIFEQ R0 R1 L2
JUMPBACK L0
L2: RETURN R0 0
)"
    );

    // break refers to inner loop => branches remain
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local foo, bar = ...
repeat
    if foo then while true do break end
    elseif bar then while true do break end
    end
    print(1234)
until foo == bar
)"),
        R"(
GETVARARGS R0 2
L0: JUMPIFNOT R0 L1
JUMP L2
JUMPBACK L2
JUMP L2
L1: JUMPIFNOT R1 L2
JUMP L2
JUMPBACK L2
L2: GETIMPORT R2 1 [print]
LOADN R3 1234
CALL R2 1 0
JUMPIFEQ R0 R1 L3
JUMPBACK L0
L3: RETURN R0 0
)"
    );
}

TEST_CASE("BuiltinArity")
{
    // by default we can't assume that we know parameter/result count for builtins as they can be overridden at runtime
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
return math.abs(unknown())
)",
                   0,
                   1
               ),
        R"(
GETIMPORT R1 1 [unknown]
CALL R1 0 -1
FASTCALL 2 L0
GETIMPORT R0 4 [math.abs]
CALL R0 -1 -1
L0: RETURN R0 -1
)"
    );

    // however, when using optimization level 2, we assume compile time knowledge about builtin behavior even if we can't deoptimize that with fenv
    // in the test case below, this allows us to synthesize a more efficient FASTCALL1 (and use a fixed-return call to unknown)
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
return math.abs(unknown())
)",
                   0,
                   2
               ),
        R"(
GETIMPORT R1 1 [unknown]
CALL R1 0 1
FASTCALL1 2 R1 L0
GETIMPORT R0 4 [math.abs]
CALL R0 1 1
L0: RETURN R0 1
)"
    );

    // some builtins are variadic, and as such they can't use fixed-length fastcall variants
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
return math.max(0, unknown())
)",
                   0,
                   2
               ),
        R"(
LOADN R1 0
GETIMPORT R2 1 [unknown]
CALL R2 0 -1
FASTCALL 18 L0
GETIMPORT R0 4 [math.max]
CALL R0 -1 1
L0: RETURN R0 1
)"
    );

    // some builtins are not variadic but don't have a fixed number of arguments; we currently don't optimize this although we might start to in the
    // future
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
return bit32.extract(0, 1, unknown())
)",
                   0,
                   2
               ),
        R"(
LOADN R1 0
LOADN R2 1
GETIMPORT R3 1 [unknown]
CALL R3 0 -1
FASTCALL 34 L0
GETIMPORT R0 4 [bit32.extract]
CALL R0 -1 1
L0: RETURN R0 1
)"
    );

    // some builtins are not variadic and have a fixed number of arguments but are not none-safe, meaning that we can't replace calls that may
    // return none with calls that will return nil
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
return type(unknown())
)",
                   0,
                   2
               ),
        R"(
GETIMPORT R1 1 [unknown]
CALL R1 0 -1
FASTCALL 40 L0
GETIMPORT R0 3 [type]
CALL R0 -1 1
L0: RETURN R0 1
)"
    );

    // importantly, this optimization also helps us get around the multret inlining restriction for builtin wrappers
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function new()
    return setmetatable({}, MT)
end

return new()
)",
                   1,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['new']
NEWTABLE R2 0 0
GETIMPORT R3 2 [MT]
FASTCALL2 61 R2 R3 L0
GETIMPORT R1 4 [setmetatable]
CALL R1 2 1
L0: RETURN R1 1
)"
    );

    // note that the results of this optimization are benign in fixed-arg contexts which dampens the effect of fenv substitutions on correctness in
    // practice
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local x = ...
local y, z = type(x)
return type(y, z)
)",
                   0,
                   2
               ),
        R"(
GETVARARGS R0 1
FASTCALL1 40 R0 L0
MOVE R2 R0
GETIMPORT R1 1 [type]
CALL R1 1 2
L0: FASTCALL2 40 R1 R2 L1
MOVE R4 R1
MOVE R5 R2
GETIMPORT R3 1 [type]
CALL R3 2 1
L1: RETURN R3 1
)"
    );
}

TEST_CASE("EncodedTypeTable")
{
    CHECK_EQ(
        "\n" + compileTypeTable(R"(
function myfunc(test: string, num: number)
    print(test)
end

function myfunc2(test: number?)
end

function myfunc3(test: string, n: number)
end

function myfunc4(test: string | number, n: number)
end

-- Promoted to function(any, any) since general unions are not supported.
-- Functions with all `any` parameters will have omitted type info.
function myfunc5(test: string | number, n: number | boolean)
end

function myfunc6(test: (number) -> string)
end

myfunc('test')
)"),
        R"(
0: function(string, number)
1: function(number?)
2: function(string, number)
3: function(any, number)
5: function(function)
)"
    );

    CHECK_EQ(
        "\n" + compileTypeTable(R"(
local Str = {
    a = 1
}

-- Implicit `self` parameter is automatically assumed to be table type.
function Str:test(n: number)
    print(self.a, n)
end

Str:test(234)
)"),
        R"(
0: function(table, number)
)"
    );
}

TEST_CASE("HostTypesAreUserdata")
{
    CHECK_EQ(
        "\n" + compileTypeTable(R"(
function myfunc(test: string, num: number)
    print(test)
end

function myfunc2(test: Instance, num: number)
end

type Foo = string

function myfunc3(test: string, n: Foo)
end

function myfunc4<Bar>(test: Bar, n: Part)
end
)"),
        R"(
0: function(string, number)
1: function(userdata, number)
2: function(string, string)
3: function(any, userdata)
)"
    );
}

TEST_CASE("HostTypesVector")
{
    CHECK_EQ(
        "\n" + compileTypeTable(R"(
function myfunc(test: Instance, pos: Vector3)
end

function myfunc2<Vector3>(test: Instance, pos: Vector3)
end

do
    type Vector3 = number

    function myfunc3(test: Instance, pos: Vector3)
    end
end
)"),
        R"(
0: function(userdata, vector)
1: function(userdata, any)
2: function(userdata, number)
)"
    );
}

TEST_CASE("BuiltinTypeVector")
{
    CHECK_EQ(
        "\n" + compileTypeTable(R"(
function myfunc(test: Instance, pos: vector)
end
)"),
        R"(
0: function(userdata, vector)
)"
    );
}

TEST_CASE("TypeAliasScoping")
{
    CHECK_EQ(
        "\n" + compileTypeTable(R"(
do
    type Part = number
end

function myfunc1(test: Part, num: number)
end

do
    type Part = number

    function myfunc2(test: Part, num: number)
    end
end

repeat
    type Part = number
until (function(test: Part, num: number) end)()

function myfunc4(test: Instance, num: number)
end

type Instance = string
)"),
        R"(
0: function(userdata, number)
1: function(number, number)
2: function(number, number)
3: function(string, number)
)"
    );
}

TEST_CASE("TypeAliasResolve")
{
    CHECK_EQ(
        "\n" + compileTypeTable(R"(
type Foo1 = number
type Foo2 = { number }
type Foo3 = Part
type Foo4 = Foo1 -- we do not resolve aliases within aliases
type Foo5<X> = X

function myfunc(f1: Foo1, f2: Foo2, f3: Foo3, f4: Foo4, f5: Foo5<number>)
end

function myfuncerr(f1: Foo1<string>, f2: Foo5)
end

)"),
        R"(
0: function(number, table, userdata, any, any)
1: function(number, any)
)"
    );
}

TEST_CASE("TypeUnionIntersection")
{
    CHECK_EQ(
        "\n" + compileTypeTable(R"(
function myfunc(test: string | nil, foo: nil)
end

function myfunc2(test: string & nil, foo: nil)
end

function myfunc3(test: string | number, foo: nil)
end

function myfunc4(test: string & number, foo: nil)
end
)"),
        R"(
0: function(string?, nil)
1: function(any, nil)
2: function(any, nil)
3: function(any, nil)
)"
    );
}

TEST_CASE("TypeGroup")
{
    CHECK_EQ(
        "\n" + compileTypeTable(R"(
function myfunc(test: (string), foo: nil)
end

function myfunc2(test: (string | nil), foo: nil)
end
)"),
        R"(
0: function(string, nil)
1: function(string?, nil)
)"
    );
}

TEST_CASE("BuiltinFoldMathK")
{
    // we can fold math.pi at optimization level 2
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
function test()
    return math.pi * 2
end
)",
                   0,
                   2
               ),
        R"(
LOADK R0 K0 [6.2831853071795862]
RETURN R0 1
)"
    );

    // we don't do this at optimization level 1 because it may interfere with environment substitution
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
function test()
    return math.pi * 2
end
)",
                   0,
                   1
               ),
        R"(
GETIMPORT R1 3 [math.pi]
MULK R0 R1 K0 [2]
RETURN R0 1
)"
    );

    // we also don't do it if math global is assigned to
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
function test()
    return math.pi * 2
end

math = { pi = 4 }
)",
                   0,
                   2
               ),
        R"(
GETGLOBAL R2 K1 ['math']
GETTABLEKS R1 R2 K2 ['pi']
MULK R0 R1 K0 [2]
RETURN R0 1
)"
    );
}

TEST_CASE("NoBuiltinFoldFenv")
{
    // builtin folding is disabled when getfenv/setfenv is used in the module
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
getfenv()

function test()
    return math.pi, math.sin(0)
end
)",
                   0,
                   2
               ),
        R"(
GETIMPORT R0 2 [math.pi]
LOADN R2 0
FASTCALL1 24 R2 L0
GETIMPORT R1 4 [math.sin]
CALL R1 1 1
L0: RETURN R0 2
)"
    );
}

TEST_CASE("IfThenElseAndOr")
{
    // if v then v else k can be optimized to ORK
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x = ...
return if x then x else 0
)"),
        R"(
GETVARARGS R0 1
ORK R1 R0 K0 [0]
RETURN R1 1
)"
    );

    // if v then v else l can be optimized to OR
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x, y = ...
return if x then x else y
)"),
        R"(
GETVARARGS R0 2
OR R2 R0 R1
RETURN R2 1
)"
    );

    // this also works in presence of type casts
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x, y = ...
return if x then x :: number else 0
)"),
        R"(
GETVARARGS R0 2
ORK R2 R0 K0 [0]
RETURN R2 1
)"
    );

    // if v then k else v can be optimized to ANDK
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x = ...
return if x then 0 else x
)"),
        R"(
GETVARARGS R0 1
ANDK R1 R0 K0 [0]
RETURN R1 1
)"
    );

    // if v then l else v can be optimized to AND
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x, y = ...
return if x then y else x
)"),
        R"(
GETVARARGS R0 2
AND R2 R0 R1
RETURN R2 1
)"
    );

    // this also works in presence of type casts
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x, y = ...
return if x then y else x :: number
)"),
        R"(
GETVARARGS R0 2
AND R2 R0 R1
RETURN R2 1
)"
    );

    // all of the above work when the target is a temporary register, which is safe because the value is only mutated once
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x, y = ...
x = if x then x else y
x = if x then y else x
)"),
        R"(
GETVARARGS R0 2
OR R0 R0 R1
AND R0 R0 R1
RETURN R0 0
)"
    );

    // note that we can't do this transformation if the expression has possible side effects
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x = ...
return if x.data then x.data else 0
)"),
        R"(
GETVARARGS R0 1
GETTABLEKS R2 R0 K0 ['data']
JUMPIFNOT R2 L0
GETTABLEKS R1 R0 K0 ['data']
RETURN R1 1
L0: LOADN R1 0
RETURN R1 1
)"
    );
}

TEST_CASE("SideEffects")
{
    // we do not evaluate expressions in some cases when we know they can't carry side effects
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x = 5, print
local y = 5, 42
local z = 5, table.find -- considered side effecting because of metamethods
)"),
        R"(
LOADN R0 5
LOADN R1 5
LOADN R2 5
GETIMPORT R3 2 [table.find]
RETURN R0 0
)"
    );

    // this also applies to returns in cases where a function gets inlined
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local function test1()
    return 42
end

local function test2()
    return print
end

local function test3()
    return function() print(test3) end
end

local function test4()
    return table.find -- considered side effecting because of metamethods
end

test1()
test2()
test3()
test4()
)",
                   5,
                   2
               ),
        R"(
DUPCLOSURE R0 K0 ['test1']
DUPCLOSURE R1 K1 ['test2']
DUPCLOSURE R2 K2 ['test3']
CAPTURE VAL R2
DUPCLOSURE R3 K3 ['test4']
GETIMPORT R4 6 [table.find]
RETURN R0 0
)"
    );
}

TEST_CASE("IfElimination")
{
    // if the left hand side of a condition is constant, it constant folds and we don't emit the branch
    CHECK_EQ("\n" + compileFunction0("local a = false if a and b then b() end"), R"(
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = true if a or b then b() end"), R"(
GETIMPORT R0 1 [b]
CALL R0 0 0
RETURN R0 0
)");

    // of course this keeps the other branch if present
    CHECK_EQ("\n" + compileFunction0("local a = false if a and b then b() else return 42 end"), R"(
LOADN R0 42
RETURN R0 1
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = true if a or b then b() else return 42 end"), R"(
GETIMPORT R0 1 [b]
CALL R0 0 0
RETURN R0 0
)");

    // if the right hand side is constant, the condition doesn't constant fold but we still could eliminate one of the branches for 'a and K'
    CHECK_EQ("\n" + compileFunction0("local a = false if b and a then return 1 end"), R"(
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = false if b and a then return 1 else return 2 end"), R"(
LOADN R0 2
RETURN R0 1
)");

    // of course if the right hand side of 'and' is 'true', we still need to actually evaluate the left hand side
    CHECK_EQ("\n" + compileFunction0("local a = true if b and a then return 1 end"), R"(
GETIMPORT R0 1 [b]
JUMPIFNOT R0 L0
LOADN R0 1
RETURN R0 1
L0: RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = true if b and a then return 1 else return 2 end"), R"(
GETIMPORT R0 1 [b]
JUMPIFNOT R0 L0
LOADN R0 1
RETURN R0 1
L0: LOADN R0 2
RETURN R0 1
)");

    // also even if we eliminate the branch, we still need to compute side effects
    CHECK_EQ("\n" + compileFunction0("local a = false if b.test and a then return 1 end"), R"(
GETIMPORT R0 2 [b.test]
RETURN R0 0
)");

    CHECK_EQ("\n" + compileFunction0("local a = false if b.test and a then return 1 else return 2 end"), R"(
GETIMPORT R0 2 [b.test]
LOADN R0 2
RETURN R0 1
)");
}

TEST_CASE("ArithRevK")
{
    // - and / have special optimized form for reverse constants; in absence of type information, we can't optimize other ops
    CHECK_EQ(
        "\n" + compileFunction0(R"(
local x: number = unknown
return 2 + x, 2 - x, 2 * x, 2 / x, 2 % x, 2 // x, 2 ^ x
)"),
        R"(
GETIMPORT R0 1 [unknown]
LOADN R2 2
ADD R1 R2 R0
SUBRK R2 K2 [2] R0
LOADN R4 2
MUL R3 R4 R0
DIVRK R4 K2 [2] R0
LOADN R6 2
MOD R5 R6 R0
LOADN R7 2
IDIV R6 R7 R0
LOADN R8 2
POW R7 R8 R0
RETURN R1 7
)"
    );

    // the same code with type information can optimize commutative operators (+ and *) as well
    // other operators are not important enough to optimize reverse constant forms for
    CHECK_EQ(
        "\n" + compileFunction(
                   R"(
local x: number = unknown
return 2 + x, 2 - x, 2 * x, 2 / x, 2 % x, 2 // x, 2 ^ x
)",
                   0,
                   2,
                   1
               ),
        R"(
GETIMPORT R0 1 [unknown]
ADDK R1 R0 K2 [2]
SUBRK R2 K2 [2] R0
MULK R3 R0 K2 [2]
DIVRK R4 K2 [2] R0
LOADN R6 2
MOD R5 R6 R0
LOADN R7 2
IDIV R6 R7 R0
LOADN R8 2
POW R7 R8 R0
RETURN R1 7
)"
    );
}

TEST_CASE("ConstStringFolding")
{
    ScopedFastFlag sff{FFlag::LuauStringConstFolding2, true};

    CHECK_EQ(
        "\n" + compileFunction(R"(return "" .. "")", 0, 2),
        R"(
LOADK R0 K0 ['']
RETURN R0 1
)"
    );

    CHECK_EQ(
        "\n" + compileFunction(R"(return "a" .. "")", 0, 2),
        R"(
LOADK R0 K0 ['a']
RETURN R0 1
)"
    );

    CHECK_EQ(
        "\n" + compileFunction(R"(return "" .. "a")", 0, 2),
        R"(
LOADK R0 K0 ['a']
RETURN R0 1
)"
    );

    CHECK_EQ(
        "\n" + compileFunction(R"(local hello = "hello"; local world = "world"; return hello .. " " .. world)", 0, 2),
        R"(
LOADK R0 K0 ['hello world']
RETURN R0 1
)"
    );
}

TEST_SUITE_END();
