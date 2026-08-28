// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BytecodeBuilder.h"
#include "Luau/BytecodeDump.h"
#include "Luau/BytecodeGraph.h"
#include "Luau/BytecodeValidation.h"
#include "Luau/BytecodeWire.h"
#include "Luau/Compiler.h"
#include "Luau/Parser.h"

#include <algorithm>
#include <optional>

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;
using namespace Luau::Bytecode;

LUAU_FASTFLAG(LuauEmitCallFeedback)
LUAU_FASTFLAG(LuauCompileFastpcall)

namespace
{

std::string extractCode(std::string bytecode)
{
    size_t offset = 5;
    const char* data = bytecode.data();
    int32_t typeInfoSize = readVarInt(data, offset);
    offset += typeInfoSize;

    int32_t codesize = readVarInt(data, offset);
    return bytecode.substr(offset, codesize * sizeof(Instruction));
}

struct BytecodeCompilerFixture
{
    BytecodeCompilerFixture() {}

    ParseResult parseCode(std::string_view src, Allocator& allocator, AstNameTable& names)
    {
        ParseResult result = Parser::parse(src.data(), src.size(), names, allocator, ParseOptions{});

        if (!result.errors.empty())
        {
            std::string message;

            for (const auto& error : result.errors)
            {
                if (!message.empty())
                    message += "\n";

                message += error.what();
            }

            printf("Parse error: %s\n", message.c_str());
        }

        return result;
    }

    bool compileCode(
        BytecodeBuilder& bcb,
        ParseResult& result,
        AstNameTable& names,
        uint32_t dumpFlags,
        int optimizationLevel,
        bool ignoreCompilationErrors
    )
    {
        bcb.setDumpFlags(dumpFlags);

        try
        {
            CompileOptions opts;
            opts.optimizationLevel = optimizationLevel;
            compileOrThrow(bcb, result, names, opts);
            return true;
        }
        catch (CompileError& e)
        {
            if (!ignoreCompilationErrors)
            {
                std::string error = format(":%d: %s", e.getLocation().begin.line + 1, e.what());
                BytecodeBuilder::getError(error);
                printf("Compilation error: %s\n", error.c_str());
            }
        }

        return false;
    }

    std::vector<std::string> extractStringTable(BytecodeBuilder& bcb)
    {
        std::string bytecode = bcb.getBytecode();
        const char* data = bytecode.data();
        size_t offset = 2; // skip versions
        std::vector<std::string> result;
        uint32_t stringsCount = readVarInt(data, offset);
        for (uint32_t i = 0; i < stringsCount; i++)
        {
            uint32_t strLen = readVarInt(data, offset);
            std::string str;
            str.assign(data + offset, strLen);
            offset += strLen;
            result.push_back(str);
        }
        return result;
    }

    std::optional<CompTimeBcFunction> fromFunctionBytecode(std::string fnData, std::vector<std::string>& strings)
    {
        std::vector<std::string_view> table;
        for (std::string& s : strings)
            table.push_back(s);

        return Bytecode::fromFunctionBytecode(fnData, table);
    }

    std::optional<std::string> getFunctionBytecode(std::string_view src, int optimizationLevel = 0, uint32_t functionId = 0)
    {
        Allocator allocator;
        AstNameTable names(allocator);
        ParseResult result = parseCode(src, allocator, names);
        BytecodeBuilder bcb;

        if (compileCode(bcb, result, names, BytecodeBuilder::Dump_Code, optimizationLevel, false))
        {
            strings = extractStringTable(bcb);
            return {{bcb.getFunctionData(functionId)}};
        }

        return {};
    }

    std::optional<Bytecode::CompTimeBcFunction> buildBytecode(std::string_view src, int optimizationLevel = 0, uint32_t functionId = 0)
    {
        auto bytecode = getFunctionBytecode(src, optimizationLevel, functionId);
        if (bytecode)
            return {fromFunctionBytecode(*bytecode, strings)};

        return {};
    }

    std::string getRoundtripFunctionBytecode(std::string_view src, uint32_t dumpFlags, int optimizationLevel = 0, uint32_t functionId = 0)
    {
        Allocator allocator;
        AstNameTable names(allocator);
        ParseResult result = parseCode(src, allocator, names);
        BytecodeBuilder bcb;
        bool compiled = compileCode(bcb, result, names, BytecodeBuilder::Dump_Code, optimizationLevel, false);
        REQUIRE(compiled);

        BytecodeBuilder reserializer;
        reserializer.setDumpFlags(dumpFlags);

        std::vector<std::string> strings = extractStringTable(bcb);

        for (uint32_t fi = 0; fi <= functionId; fi++)
        {
            std::optional<CompTimeBcFunction> fn = fromFunctionBytecode(bcb.getFunctionData(fi), strings);
            REQUIRE(fn);
            Bytecode::toFunctionBytecode(reserializer, *fn);
            reserializer.clearStrings();
        }

        return reserializer.dumpFunction(functionId);
    }

    void checkRoundtrip(std::string_view snippet, bool ignoreCompilationErrors = false)
    {
        Allocator allocator;
        AstNameTable names(allocator);
        ParseResult result = parseCode(snippet, allocator, names);

        for (int optLevel = 0; optLevel <= 2; optLevel++)
        {
            BytecodeBuilder bcb;
            compileCode(bcb, result, names, BytecodeBuilder::Dump_Code, optLevel, true);

            std::vector<std::string> strings = extractStringTable(bcb);

            // We share a single BytecodeBuilder for reserializing every function, since serializing NEWCLOSURE requires functions that were
            // previously serialized
            BytecodeBuilder reserializer;
            for (uint32_t fi = 0; fi < bcb.getFunctionCount(); fi++)
            {
                std::string fnData = bcb.getFunctionData(fi);
                std::optional<CompTimeBcFunction> fn = fromFunctionBytecode(fnData, strings);
                REQUIRE(fn);
                std::string orig = extractCode(fnData);
                std::string dumped = extractCode(Bytecode::toFunctionBytecode(reserializer, *fn));
                REQUIRE_EQ(orig, dumped);
                // The StringRefs added to reserializer's string table are invalidated when fn goes out of scope
                reserializer.clearStrings();
            }
        }
    }

    // String table has to be retained for the lifetime of CompTimeBcFunction
    std::vector<std::string> strings;
};

} // namespace

TEST_SUITE_BEGIN("BytecodeCompiler");

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "from_function_bytecode")
{
    ScopedFastFlag luauEmitCallFeedback{FFlag::LuauEmitCallFeedback, true};

    auto fn = buildBytecode(R"(
        function fn(a, b)
            local extra = 0
            if a > b then extra = 1 end 
            return extra + a + b
        end
    )");

    REQUIRE(fn);

    // function meta
    REQUIRE_EQ(fn->constants.size(), 2);

    REQUIRE_EQ(verifyUseConsistency(*fn), true);

    CHECK_EQ(
        "\n" + toString(*fn, true),
        R"(
; function fn($arg0, $arg1) line 2 maxstacksize: 5 upvalues: 0 flags: 8
bb_0 (entry):
; successors: bb_2 [branch], bb_3 [fallthrough]
  %0 = LOADK K0 (0)                                          ; uses: phi.0
  %1 = JUMPIFNOTLT R1, R0, bb_2

bb_3:
; predecessors: bb_0 [fallthrough]
; successors: bb_2 [fallthrough]
  %2 = LOADK K1 (1)                                          ; uses: phi.0

bb_2:
; predecessors: bb_0 [branch], bb_3 [fallthrough]
; successors: bb_1 [fallthrough]
  phi.0 = %0 from bb_0, %2 from bb_3                         ; uses: %3
  %3 = ADD phi.0, R0                                         ; uses: %4
  %4 = ADD %3, R1                                            ; uses: %5
  %5 = RETURN 1, %4

bb_1 (exit):
; predecessors: bb_2 [fallthrough]
)"
    );
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "repeat_until_loop")
{
    ScopedFastFlag luauEmitCallFeedback{FFlag::LuauEmitCallFeedback, true};

    auto fn = buildBytecode(R"(
        function fn()
            local var = 0
            repeat var += 1 until var < 10
            --return var
        end
    )");

    REQUIRE(fn);
    REQUIRE_EQ(verifyUseConsistency(*fn), true);

    CHECK_EQ(
        "\n" + toString(*fn, true),
        R"(
; function fn() line 2 maxstacksize: 2 upvalues: 0 flags: 8
bb_0 (entry):
; successors: bb_4 [fallthrough]
  %0 = LOADK K0 (0)                                          ; uses: phi.0

bb_4:
; predecessors: bb_0 [fallthrough], bb_3 [loop]
; successors: bb_2 [branch], bb_3 [fallthrough]
  phi.0 = %0 from bb_0, %2 from bb_4                         ; uses: %2
  %1 = LOADK K1 (1)                                          ; uses: %2
  %2 = ADD phi.0, %1                                         ; uses: %4, phi.0
  %3 = LOADK K2 (10)                                         ; uses: %4
  %4 = JUMPIFLT %2, %3, bb_2

bb_3:
; predecessors: bb_4 [fallthrough]
; successors: bb_4 [loop]
  %5 = JUMPBACK bb_4

bb_2:
; predecessors: bb_4 [branch]
; successors: bb_1 [fallthrough]
  %6 = RETURN 0, R0

bb_1 (exit):
; predecessors: bb_2 [fallthrough]
)"
    );
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "for_loop_and_backward_input")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    auto fn = buildBytecode(R"(
        function fn()
            local var = 3
            for i = 1, 10 do
                if var > 0 then print(i) end
                var -= 1;
            end
        end
    )");

    REQUIRE_EQ(verifyUseConsistency(*fn), true);

    CHECK_EQ(
        "\n" + toString(*fn, true),
        R"(
; function fn() line 2 maxstacksize: 6 upvalues: 0 flags: 8
bb_0 (entry):
; successors: bb_2 [branch], bb_3 [fallthrough]
  %0 = LOADK K0 (3)                                          ; uses: phi.0
  %1 = LOADK K1 (1)                                          ; uses: %4
  %2 = LOADK K2 (10)                                         ; uses: %4
  %3 = LOADN 1                                               ; uses: %4
  %4 = FORNPREP %2, %3, %1, bb_2

bb_3:
; predecessors: bb_0 [fallthrough], bb_4 [loop]
; successors: bb_4 [branch], bb_5 [fallthrough]
  phi.0 = %0 from bb_0, %11 from bb_4                        ; uses: %6, phi.2, phi.2, %11
  %5 = LOADK K3 (0)                                          ; uses: %6
  %6 = JUMPIFNOTLT %5, phi.0, bb_4

bb_5:
; predecessors: bb_3 [fallthrough]
; successors: bb_4 [fallthrough]
  %7 = GETGLOBAL 70, K4 ('print')                            ; uses: %9
  %8 = MOVE %4[2]                                            ; uses: %9
  %9 = CALLFB 1, 0, 0, %7, %8

bb_4:
; predecessors: bb_3 [branch], bb_5 [fallthrough]
; successors: bb_3 [loop], bb_2 [fallthrough]
  %10 = LOADK K1 (1)                                         ; uses: %11
  %11 = SUB phi.0, %10                                       ; uses: phi.0
  %12 = FORNLOOP %4[0], %4[1], %4[2], bb_3

bb_2:
; predecessors: bb_0 [branch], bb_4 [fallthrough]
; successors: bb_1 [fallthrough]
  %13 = RETURN 0, R0

bb_1 (exit):
; predecessors: bb_2 [fallthrough]
)"
    );
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "nested_loops")
{
    ScopedFastFlag luauEmitCallFeedback{FFlag::LuauEmitCallFeedback, true};

    auto fn = buildBytecode(R"(
        function fn()
            local res = 0
            local var = 0
            repeat
                local i = 0
                repeat
                    res += i * var
                    i += 1
                until i < 5
                var += 1
            until var < 10
        end
    )");

    REQUIRE_EQ(verifyUseConsistency(*fn), true);

    CHECK_EQ(
        "\n" + toString(*fn, true),
        R"(
; function fn() line 2 maxstacksize: 4 upvalues: 0 flags: 8
bb_0 (entry):
; successors: bb_7 [fallthrough]
  %0 = LOADK K0 (0)                                          ; uses: phi.4
  %1 = LOADK K0 (0)                                          ; uses: phi.3

bb_7:
; predecessors: bb_0 [fallthrough], bb_6 [loop]
; successors: bb_4 [fallthrough]
  phi.3 = %1 from bb_0, %11 from bb_2                        ; uses: phi.1, %3, %11
  phi.4 = %0 from bb_0, %4 from bb_4                         ; uses: phi.2
  %2 = LOADK K0 (0)                                          ; uses: phi.0

bb_4:
; predecessors: bb_7 [fallthrough], bb_3 [loop]
; successors: bb_2 [branch], bb_3 [fallthrough]
  phi.0 = %2 from bb_7, %6 from bb_4                         ; uses: %3, %6
  phi.2 = phi.4, %4 from bb_4                                ; uses: %4
  %3 = MUL phi.0, phi.3                                      ; uses: %4
  %4 = ADD phi.2, %3                                         ; uses: phi.2, phi.4
  %5 = LOADK K1 (1)                                          ; uses: %6
  %6 = ADD phi.0, %5                                         ; uses: %8, phi.0
  %7 = LOADK K2 (5)                                          ; uses: %8
  %8 = JUMPIFLT %6, %7, bb_2

bb_3:
; predecessors: bb_4 [fallthrough]
; successors: bb_4 [loop]
  %9 = JUMPBACK bb_4

bb_2:
; predecessors: bb_4 [branch]
; successors: bb_5 [branch], bb_6 [fallthrough]
  %10 = LOADK K1 (1)                                         ; uses: %11
  %11 = ADD phi.3, %10                                       ; uses: %13, phi.3
  %12 = LOADK K3 (10)                                        ; uses: %13
  %13 = JUMPIFLT %11, %12, bb_5

bb_6:
; predecessors: bb_2 [fallthrough]
; successors: bb_7 [loop]
  %14 = JUMPBACK bb_7

bb_5:
; predecessors: bb_2 [branch]
; successors: bb_1 [fallthrough]
  %15 = RETURN 0, R0

bb_1 (exit):
; predecessors: bb_5 [fallthrough]
)"
    );
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "multi_call_fixed")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    auto fn = buildBytecode(R"(
        local function x()
            local a, b = f()
            return b, a
        end
    )");

    REQUIRE_EQ(verifyUseConsistency(*fn), true);

    CHECK_EQ(
        "\n" + toString(*fn, false),
        R"(
; function x() line 2 maxstacksize: 4 upvalues: 0 flags: 8
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %0 = GETGLOBAL 135, K0 ('f')
  %1 = CALLFB 0, 2, 0, %0
  %2 = MOVE %1[1]
  %3 = MOVE %1[0]
  %4 = RETURN 2, %2, %3

bb_1 (exit):
; predecessors: bb_0 [fallthrough]
)"
    );
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "multi_call_variadic")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    auto fn = buildBytecode(R"(
        local function fn(n)
            if n > 0 then
                return 0, 1
            else
                local a, b = fn(n - 1)
                return a + b, fn(n)
            end
        end
    )");

    REQUIRE_EQ(verifyUseConsistency(*fn), true);

    CHECK_EQ(
        "\n" + toString(*fn, true),
        R"(
; function fn($arg0) line 2 maxstacksize: 6 upvalues: 1 flags: 0
bb_0 (entry):
; successors: bb_2 [branch], bb_3 [fallthrough]
  %0 = LOADK K0 (0)                                          ; uses: %1
  %1 = JUMPIFNOTLT %0, R0, bb_2

bb_3:
; predecessors: bb_0 [fallthrough]
; successors: bb_1 [fallthrough]
  %2 = LOADK K0 (0)                                          ; uses: %4
  %3 = LOADK K1 (1)                                          ; uses: %4
  %4 = RETURN 2, %2, %3

bb_2:
; predecessors: bb_0 [branch]
; successors: bb_1 [fallthrough]
  %5 = GETUPVAL U0                                           ; uses: %8
  %6 = LOADK K1 (1)                                          ; uses: %7
  %7 = SUB R0, %6                                            ; uses: %8
  %8 = CALLFB 1, 2, 0, %5, %7
  %9 = ADD %8[0], %8[1]                                      ; uses: %13
  %10 = GETUPVAL U0                                          ; uses: %12
  %11 = MOVE R0                                              ; uses: %12
  %12 = CALL 1, -1, %10, %11                                 ; uses: %13
  %13 = RETURN -1, %9, %12

bb_1 (exit):
; predecessors: bb_3 [fallthrough], bb_2 [fallthrough]
)"
    );
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "variadic_function")
{
    auto fn = buildBytecode(R"(
        local function fn(a, ...)
            local b, c = ...
            local l = {...}
            return a + b + c + l[1], ...
        end
    )");

    REQUIRE_EQ(verifyUseConsistency(*fn), true);

    CHECK_EQ(
        "\n" + toString(*fn, true),
        R"(
; function fn($arg0, ...) line 2 maxstacksize: 8 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %0 = PREPVARARGS 1
  %1 = GETVARARGS R1, 2
  %2 = NEWTABLE 0, 0                                         ; uses: %4, %8
  %3 = GETVARARGS R4, -1                                     ; uses: %4
  %4 = SETLIST 1, -1, %2, %3
  %5 = ADD R0, %1[0]                                         ; uses: %6
  %6 = ADD %5, %1[1]                                         ; uses: %9
  %7 = LOADK K0 (1)                                          ; uses: %8
  %8 = GETTABLE %2, %7                                       ; uses: %9
  %9 = ADD %6, %8                                            ; uses: %11
  %10 = GETVARARGS R5, -1                                    ; uses: %11
  %11 = RETURN -1, %9, %10

bb_1 (exit):
; predecessors: bb_0 [fallthrough]
)"
    );
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "tables_strings_and_fastcall")
{
    auto fn = buildBytecode(
        R"(
        local tt = {}
        local function fn(x)
            local t = { a = x, b = x .. 42 }
            return table.insert({t}, tt)
        end
    )",
        1
    );

    REQUIRE_EQ(verifyUseConsistency(*fn), true);

    CHECK_EQ(
        "\n" + toString(*fn, true),
        R"(
; function fn($arg0) line 3 maxstacksize: 5 upvalues: 1 flags: 0
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %0 = DUPTABLE K2 ({...})                                   ; uses: %1, %5, %7
  %1 = SETTABLEKS R0, %0, 128, K0 ('a')
  %2 = MOVE R0                                               ; uses: %4
  %3 = LOADN 42                                              ; uses: %4
  %4 = CONCAT %2, %3                                         ; uses: %5
  %5 = SETTABLEKS %4, %0, 131, K1 ('b')
  %6 = NEWTABLE 0, 1                                         ; uses: %8, %10, %12
  %7 = MOVE %0                                               ; uses: %8
  %8 = SETLIST 1, 1, %6, %7
  %9 = GETUPVAL U0                                           ; uses: %10, %12
  %10 = FASTCALL2 52, %6, %9, 3
  %11 = GETIMPORT K5 (table.insert), 2, K3 ('table'), K4 ('insert') ; uses: %12
  %12 = CALL 2, -1, %11, %6, %9                              ; uses: %13
  %13 = RETURN -1, %12

bb_1 (exit):
; predecessors: bb_0 [fallthrough]
)"
    );
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "def_use_chains")
{
    ScopedFastFlag luauEmitCallFeedback{FFlag::LuauEmitCallFeedback, true};

    auto fn = buildBytecode(R"(
        local function fn(a, b, c)
            local s = a + b
            local x = s + c
            local y = s + a
            return x + y
        end
    )");

    /*
        // Block 1 (entry)
        ADD R3 R0 R1   ; s = a + b
        ADD R4 R3 R2   ; x = s + c
        ADD R5 R3 R0   ; y = s + a
        ADD R4 R4 R5   ; r = x + y
        RETURN R4 1
        // Block 2 (exit)
    */

    REQUIRE(fn);
    REQUIRE_EQ(verifyUseConsistency(*fn), true);

    CHECK_EQ(
        "\n" + toString(*fn, true),
        R"(
; function fn($arg0, $arg1, $arg2) line 2 maxstacksize: 7 upvalues: 0 flags: 8
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %0 = ADD R0, R1                                            ; uses: %1, %2
  %1 = ADD %0, R2                                            ; uses: %3
  %2 = ADD %0, R0                                            ; uses: %3
  %3 = ADD %1, %2                                            ; uses: %4
  %4 = RETURN 1, %3

bb_1 (exit):
; predecessors: bb_0 [fallthrough]
)"
    );
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "loop_invariant_inst_phi_collapse")
{
    ScopedFastFlag luauEmitCallFeedback{FFlag::LuauEmitCallFeedback, true};

    auto fn = buildBytecode(R"(
        local function fn(a, b)
            local s = a + b
            local acc = 0
            repeat acc += s until acc < 100
            return acc
        end
    )");

    REQUIRE(fn);
    REQUIRE_EQ(verifyUseConsistency(*fn), true);

    CHECK_EQ(
        "\n" + toString(*fn, true),
        R"(
; function fn($arg0, $arg1) line 2 maxstacksize: 5 upvalues: 0 flags: 8
bb_0 (entry):
; successors: bb_4 [fallthrough]
  %0 = ADD R0, R1                                            ; uses: phi.1, %2
  %1 = LOADK K0 (0)                                          ; uses: phi.0

bb_4:
; predecessors: bb_0 [fallthrough], bb_3 [loop]
; successors: bb_2 [branch], bb_3 [fallthrough]
  phi.0 = %1 from bb_0, %2 from bb_4                         ; uses: %2
  %2 = ADD phi.0, %0                                         ; uses: %4, phi.0, %6
  %3 = LOADK K1 (100)                                        ; uses: %4
  %4 = JUMPIFLT %2, %3, bb_2

bb_3:
; predecessors: bb_4 [fallthrough]
; successors: bb_4 [loop]
  %5 = JUMPBACK bb_4

bb_2:
; predecessors: bb_4 [branch]
; successors: bb_1 [fallthrough]
  %6 = RETURN 1, %2

bb_1 (exit):
; predecessors: bb_2 [fallthrough]
)"
    );
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "bytecode_roundtrip")
{
    std::string snippets[] = {
        R"(
        function fn(a, b)
            local extra = 0
            if a > b then extra = 1 end 
            return extra + a + b
        end
    )",
        R"(
        function fn()
            local var = 0
            repeat var += 1 until var < 10
        end
    )",
        R"(
        function fn()
            local var = 3
            for i = 1, 10 do
                if var > 0 then print(i) end
                var -= 1;
            end
        end
    )",
        R"(
        function fn()
            local res = 0
            local var = 0
            repeat
                local i = 0
                repeat
                    res += i * var
                    i += 1
                until i < 5
                var += 1
            until var < 10
        end
    )",
        R"(
        local function x()
            local a, b = f()
            return b, a
        end
    )",
        R"(
        local function fn(n)
            if n > 0 then
                return 0, 1
            else
                local a, b = fn(n - 1)
                return a + b, fn(n)
            end
        end
    )",
        R"(
        local function fn(a, ...)
            local b, c = ...
            local l = {...}
            return a + b + c + l[1], ...
        end
    )",
        R"(
        local function fn(x)
            local f = function (a, b) return a .. " and " .. b .. " and agian " .. b end
            return f(x, "eleven")
        end
    )",
        R"(
        local tt = {}
        local function fn(x)
            local t = { a = x, b = x .. 42 }
            return table.insert({t}, tt)
        end
    )",
    };

    for (auto& snippet : snippets)
        checkRoundtrip(snippet);
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "classes_bytecode_roundtrips")
{

    ScopedFastFlag _{FFlag::DebugLuauUserDefinedClasses, true};

    checkRoundtrip(R"(
        class Point
            public x
            public y

            function magnitude(self)
                return math.sqrt(self.x * self.x + self.y * self.y)
            end

            function __mul(self, other)
                return Point { x = self.x * other.x, y = self.y * other.y }
            end

            function __add(self, other)
                return Point { x = self.x + other.x, y = self.y + other.y }
            end

            function __eq(self, other)
                return self.x == other.x and self.y == other.y
            end

            function zero()
                return Point { x = 0, y = 0 }
            end

            function asserttriple(self)
                local mag = self:magnitude()
                assert(mag == math.ceil(mag), "Not a pythagorean triple!")
            end

            function __tostring(self)
                return `Point(x={self.x}, y={self.y})`
            end

        end

        print(Point)

        return { Point = Point }
    )");
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "inheriting_classes_bytecode_roundtrips")
{
    ScopedFastFlag _{FFlag::DebugLuauUserDefinedClasses, true};

    std::string_view source = R"(
open class Animal
    public species: string

    function __tostring(self)
        return "I am an animal."
    end

    function live(self)
        return "I am alive"
    end
end

class Cat extends Animal
    public breed: string

    function __tostring(self): string
        return `{Animal.__tostring(self)} I am a {self.breed} cat.`
    end
end

print(Cat)

return { Animal = Animal, Cat = Cat }
    )";

    checkRoundtrip(source);

    std::string dump = getRoundtripFunctionBytecode(source, BytecodeBuilder::Dump_Code | BytecodeBuilder::Dump_Constants, 1, 3);

    CHECK_EQ("\n" + dump, R"(
K0: 'Animal'
K1: 'species'
K2: function __tostring
K3: '__tostring'
K4: function live
K5: 'live'
K6: 'new'
K7: '__init'
K8: class Animal (props: 1, methods: 4)
  props:
    K1 ['species']
  methods:
    K3 ['__tostring']
    K5 ['live']
    K6 ['new']
    K7 ['__init']
K9: 'Cat'
K10: 'breed'
K11: class Cat (props: 1, methods: 3)
  props:
    K10 ['breed']
  methods:
    K3 ['__tostring']
    K6 ['new']
    K7 ['__init']
K12: 'print'
K13: print
K14: {['Animal'] #1, ['Cat'] #0} sizenode=2
LOADNIL R0
LOADNIL R1
NEWCLASS R0 no_base K8 1 [class Animal (props: 1, methods: 4)]
DUPCLOSURE R2 K2 ['__tostring']
NEWCLASSMEMBER R0 R2 ['__tostring']
DUPCLOSURE R2 K4 ['live']
NEWCLASSMEMBER R0 R2 ['live']
NEWCLASS R1 R0 K11 0 [class Cat (props: 1, methods: 3)]
NEWCLOSURE R2 P2
CAPTURE REF R0
NEWCLASSMEMBER R1 R2 ['__tostring']
GETIMPORT R2 13 [print]
MOVE R3 R1
CALL R2 1 0
DUPTABLE R2 14
SETTABLEKS R0 R2 K0 ['Animal']
SETTABLEKS R1 R2 K9 ['Cat']
CLOSEUPVALS R0
RETURN R2 1
)");
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "fastpcall_roundtrip")
{
    ScopedFastFlag luauCompileFastpcall{FFlag::LuauCompileFastpcall, true};

    checkRoundtrip(R"(
        local function test(fn)
            return pcall(fn, 42)
        end
    )");

    checkRoundtrip(R"(
        local function test(fn, errf)
            return xpcall(fn, errf, 1, 2, 3)
        end
    )");
}

TEST_SUITE_END();
