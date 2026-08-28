// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BytecodeBuilder.h"
#include "Luau/BytecodeGraph.h"
#include "Luau/BytecodeWire.h"
#include "Luau/BytecodeValidation.h"
#include "Luau/BytecodeCallInliner.h"
#include "Luau/Compiler.h"
#include "Luau/Parser.h"
#include "Luau/Sccp.h"

#include <optional>

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;
using namespace Luau::Bytecode;

LUAU_FASTFLAG(LuauEmitCallFeedback)

namespace
{

struct BytecodeRes
{
    std::string inlineeBytecode;
    std::string callerBytecode;
    std::vector<std::string> stringTable;
};

struct BytecodeInlinerFixture
{

    std::optional<std::pair<Bytecode::CompTimeBcFunction, Bytecode::CompTimeBcFunction>> compileAndInline(
        std::string_view src,
        uint32_t callIdx = 0,
        int optimizationLevel = 0
    )
    {
        auto res = buildBytecode(src, optimizationLevel);

        REQUIRE(res);

        auto& [inlinee, caller] = *res;
        BcOp call;
        uint32_t idx = 0;
        for (uint32_t i = 0; i < caller.instructions.size(); i++)
            if (caller.instructions[i].op == LOP_CALLFB && idx++ == callIdx)
            {
                call = BcOp{BcOpKind::Inst, i};
                break;
            }
        LUAU_ASSERT(call.kind != BcOpKind::None);
        if (!inlineCall(caller, inlinee, call, 0))
            return {};
        return res;
    }

    std::string inlineAndPrint(std::string_view src, uint32_t callIdx = 0, bool foldConstants = false, int optimizationLevel = 0)
    {
        auto res = compileAndInline(src, callIdx, optimizationLevel);

        REQUIRE(res);
        REQUIRE_EQ(verifyUseConsistency(res->second), true);

        if (foldConstants)
        {
            BcVmConstImpl impl(res->second);
            Bytecode::foldConstants(res->second, impl);
        }
        REQUIRE(verifyUseConsistency(res->second));

        BytecodeBuilder bcb;
        bcb.setDumpFlags(BytecodeBuilder::Dump_Code);
        std::string result = toFunctionBytecode(bcb, res->second);
        REQUIRE(!result.empty());
        return bcb.dumpFunction(0);
    }

    std::optional<std::pair<Bytecode::CompTimeBcFunction, Bytecode::CompTimeBcFunction>> buildBytecode(
        std::string_view src,
        int optimizationLevel = 0
    )
    {
        auto bytecode = getFunctionBytecode(src, optimizationLevel);
        if (bytecode)
        {
            strings = bytecode->stringTable;
            std::vector<std::string_view> table;
            for (std::string& s : strings)
                table.push_back(s);
            std::optional<CompTimeBcFunction> inlinee = Bytecode::fromFunctionBytecode(bytecode->inlineeBytecode, table);
            LUAU_ASSERT(inlinee && inlinee->debugname == "inlinee");
            std::optional<CompTimeBcFunction> caller = Bytecode::fromFunctionBytecode(bytecode->callerBytecode, table);
            LUAU_ASSERT(caller && caller->debugname == "caller");
            return {{*inlinee, *caller}};
        }
        return {};
    }

    std::vector<CompTimeBcFunction> buildGraphs(std::string_view src, int optimizationLevel = 1)
    {
        Allocator allocator;
        AstNameTable names(allocator);
        ParseResult result = Parser::parse(src.data(), src.size(), names, allocator, ParseOptions{});
        REQUIRE(result.errors.empty());

        BytecodeBuilder bcb;
        bcb.setDumpFlags(BytecodeBuilder::Dump_Code);
        CompileOptions opts;
        opts.optimizationLevel = optimizationLevel;
        compileOrThrow(bcb, result, names, opts);

        strings = extractStringTable(bcb);
        std::vector<std::string_view> table;
        table.reserve(strings.size());
        for (std::string& s : strings)
            table.push_back(s);

        std::vector<CompTimeBcFunction> graphs;
        for (uint32_t fi = 0; fi < bcb.getFunctionCount(); fi++)
        {
            std::optional<CompTimeBcFunction> fn = Bytecode::fromFunctionBytecode(bcb.getFunctionData(fi), table);
            REQUIRE(fn);
            graphs.push_back(std::move(*fn));
        }
        return graphs;
    }

    std::optional<BytecodeRes> getFunctionBytecode(std::string_view src, int optimizationLevel = 0)
    {
        Allocator allocator;
        AstNameTable names(allocator);
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
        BytecodeBuilder bcb;
        bcb.setDumpFlags(BytecodeBuilder::Dump_Code);
        try
        {
            CompileOptions opts;
            opts.optimizationLevel = optimizationLevel;
            compileOrThrow(bcb, result, names, opts);
            uint32_t funcCount = bcb.getFunctionCount();
            LUAU_ASSERT(funcCount >= 3);
            return {{bcb.getFunctionData(funcCount - 3), bcb.getFunctionData(funcCount - 2), extractStringTable(bcb)}};
        }
        catch (CompileError& e)
        {
            std::string error = format(":%d: %s", e.getLocation().begin.line + 1, e.what());
            BytecodeBuilder::getError(error);
            printf("Compilation error: %s\n", error.c_str());
        }
        return {};
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

    std::vector<std::string> strings;
};

} // namespace

TEST_SUITE_BEGIN("BytecodeInliner");

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "simple_inlining")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        ADD R2 R0 R1
        RETURN R2 1

        Function 1 (caller):
        GETUPVAL R1 0
        MOVE R2 R0
        LOADK R3 K0 [42]
        CALLFB R1 2 1 [0]
        LOADK R3 K1 [2]
        ADD R2 R1 R3
        RETURN R2 1
    */
    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a, b)
            return a + b
        end
    
        local function caller(x)
            local result = inlinee(x, 42)
            return result + 2
        end
    )"),
        R"(
GETUPVAL R1 0
MOVE R2 R0
LOADK R3 K0 [42]
CMPPROTO R1 #0 L0
ADD R4 R2 R3
MOVE R1 R4
JUMP L1
L0: CALLFB R1 2 1 [-1]
L1: LOADK R3 K1 [2]
ADD R2 R1 R3
RETURN R2 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "simple_inlining_undercall")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        MOVE R3 R1
        JUMPIF R3 L0
        LOADK R3 K0 [42]
        L0: ADD R2 R0 R3
        RETURN R2 1

        Function 1 (caller):
        GETUPVAL R1 0
        MOVE R2 R0
        CALL R1 1 1
        LOADK R3 K0 [2]
        ADD R2 R1 R3
        RETURN R2 1
    */
    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a, b)
            return a + (b or 42)
        end
    
        local function caller(x)
            local result = inlinee(x)
            return result + 2
        end
    )"),
        R"(
GETUPVAL R1 0
MOVE R2 R0
CMPPROTO R1 #0 L1
LOADNIL R3
MOVE R5 R3
JUMPIF R5 L0
LOADK R5 K1 [42]
L0: ADD R4 R2 R5
MOVE R1 R4
JUMP L2
L1: CALLFB R1 1 1 [-1]
L2: LOADK R3 K0 [2]
ADD R2 R1 R3
RETURN R2 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "simple_inlining_under_return")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        RETURN R0 1

        Function 1 (caller):
        GETUPVAL R0 0
        LOADK R1 K0 [10]
        CALL R0 1 2
        RETURN R1 1
    */
    // NB: there are 2 RETURNs because BytecodeBuilder replaces JUMP to RETURN with RETURN
    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a)
            return a
        end

        local function caller()
            local r1, r2 = inlinee(10)
            return r2
        end
    )"),
        R"(
GETUPVAL R0 0
LOADK R1 K0 [10]
CMPPROTO R0 #0 L0
MOVE R0 R1
LOADNIL R1
RETURN R1 1
L0: CALLFB R0 1 2 [-1]
RETURN R1 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "namecall_inlining")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        GETTABLEKS R3 R0 K0 ['v']
        ADD R2 R3 R1
        RETURN R2 1

        Function 1 (caller):
        DUPTABLE R1 2
        LOADK R2 K0 ['v']
        LOADK R3 K3 [7]
        SETTABLE R3 R1 R2
        LOADK R2 K1 ['inlinee']
        GETUPVAL R3 0
        SETTABLE R3 R1 R2
        LOADK R4 K4 [42]
        NAMECALL R2 R1 K1 ['inlinee']
        CALLFB R2 2 1 [0]
        LOADK R4 K5 [2]
        ADD R3 R2 R4
        RETURN R3 1
    */

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(t, x)
            return t.v + x
        end
    
        local function caller(x)
            local t = {v = 7, inlinee = inlinee}
            local result = t:inlinee(42)
            return result + 2
        end
    )"),
        R"(
DUPTABLE R1 2
LOADK R2 K0 ['v']
LOADK R3 K3 [7]
SETTABLE R3 R1 R2
LOADK R2 K1 ['inlinee']
GETUPVAL R3 0
SETTABLE R3 R1 R2
LOADK R4 K4 [42]
MOVE R3 R1
GETTABLEKS R2 R3 K1 ['inlinee']
CMPPROTO R2 #0 L0
GETTABLEKS R6 R3 K0 ['v']
ADD R5 R6 R4
MOVE R2 R5
JUMP L1
L0: NAMECALL R2 R3 K1 ['inlinee']
CALLFB R2 2 1 [-1]
L1: LOADK R4 K5 [2]
ADD R3 R2 R4
RETURN R3 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "early_return_inlining")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        LOADK R2 K0 [0]
        JUMPIFNOTLT R1 R2 L0
        SUB R2 R0 R1
        RETURN R2 1
        L0: ADD R2 R0 R1
        RETURN R2 1

        Function 1 (caller):
        GETUPVAL R1 0
        MOVE R2 R0
        LOADK R3 K0 [42]
        CALL R1 2 1
        LOADK R3 K1 [2]
        ADD R2 R1 R3
        RETURN R2 1
    */

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a, b)
            if b < 0 then return a - b end
            return a + b
        end
    
        local function caller(x)
            local result = inlinee(x, 42)
            return result + 2
        end
    )"),
        R"(
GETUPVAL R1 0
MOVE R2 R0
LOADK R3 K0 [42]
CMPPROTO R1 #0 L1
LOADK R4 K2 [0]
JUMPIFNOTLT R3 R4 L0
SUB R4 R2 R3
MOVE R1 R4
JUMP L2
L0: ADD R4 R2 R3
MOVE R1 R4
JUMP L2
L1: CALLFB R1 2 1 [-1]
L2: LOADK R3 K1 [2]
ADD R2 R1 R3
RETURN R2 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "multi_return_inlining")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        LOADK R2 K0 [0]
        JUMPIFNOTLT R1 R2 L0
        SUB R2 R0 R1
        RETURN R2 1
        L0: ADD R2 R0 R1
        LOADK R3 K2 [12]
        RETURN R2 1

        Function 1 (caller):
        GETUPVAL R1 0
        MOVE R2 R0
        LOADK R3 K0 [42]
        CALLFB R1 2 1 [0]
        LOADK R3 K1 [2]
        ADD R2 R1 R3
        RETURN R2 1
    */

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a, b)
            if b < 0 then return a - b end
            return a + b, 12
        end
    
        local function caller(x)
            local result = inlinee(x, 42)
            return result + 2
        end
    )"),
        R"(
GETUPVAL R1 0
MOVE R2 R0
LOADK R3 K0 [42]
CMPPROTO R1 #0 L1
LOADK R4 K2 [0]
JUMPIFNOTLT R3 R4 L0
SUB R4 R2 R3
MOVE R1 R4
JUMP L2
L0: ADD R4 R2 R3
LOADK R5 K3 [12]
MOVE R1 R4
MOVE R2 R5
JUMP L2
L1: CALLFB R1 2 1 [-1]
L2: LOADK R3 K1 [2]
ADD R2 R1 R3
RETURN R2 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "var_return_inlining")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    // If target contains vararg returns it cannot be inlined.
    REQUIRE(!compileAndInline(R"(
        local function inlinee(a, b)
            return g(a, b)
        end

        local function caller(x)
            local a, b = inlinee(x, 42)
            return a + b
        end
    )"));
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "vararg_func_inlining")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        LOADK R0 K0 [12]
        GETVARARGS R1 2
        LOADK R3 K1 [0]
        JUMPIFNOTLT R2 R3 L0
        SUB R3 R1 R2
        RETURN R3 1
        L0: ADD R3 R1 R2
        RETURN R3 1

        Function 1 (caller):
        GETUPVAL R1 0
        MOVE R2 R0
        LOADK R3 K0 [42]
        CALL R1 2 1
        LOADK R3 K1 [2]
        ADD R2 R1 R3
        RETURN R2 1
    */

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(...)
            local x = 12
            local a, b = ...
            if b < 0 then return a - b end
            return a + b
        end

        local function caller(x)
            local result = inlinee(x, 42)
            return result + 2
        end
    )"),
        R"(
GETUPVAL R1 0
MOVE R2 R0
LOADK R3 K0 [42]
CMPPROTO R1 #0 L1
LOADK R4 K2 [12]
MOVE R5 R2
MOVE R6 R3
LOADK R7 K3 [0]
JUMPIFNOTLT R6 R7 L0
SUB R7 R5 R6
MOVE R1 R7
JUMP L2
L0: ADD R7 R5 R6
MOVE R1 R7
JUMP L2
L1: CALLFB R1 2 1 [-1]
L2: LOADK R3 K1 [2]
ADD R2 R1 R3
RETURN R2 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "mixed_vararg_func_inlining")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        GETVARARGS R1 1
        ADD R2 R0 R1
        RETURN R2 1

        Function 1 (caller):
        GETUPVAL R1 0
        MOVE R2 R0
        LOADK R3 K0 [100]
        CALL R1 2 1
        LOADK R3 K1 [2]
        ADD R2 R1 R3
        RETURN R2 1
    */

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a, ...)
            local b = ...
            return a + b
        end
        local function caller(x)
            local result = inlinee(x, 100)
            return result + 2
        end
    )"),
        R"(
GETUPVAL R1 0
MOVE R2 R0
LOADK R3 K0 [100]
CMPPROTO R1 #0 L0
MOVE R5 R3
ADD R6 R2 R5
MOVE R1 R6
JUMP L1
L0: CALLFB R1 2 1 [-1]
L1: LOADK R3 K1 [2]
ADD R2 R1 R3
RETURN R2 1
)"
    );
}


TEST_CASE_FIXTURE(BytecodeInlinerFixture, "mixed_vararg_func_inlining_nil_factory")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        GETVARARGS R2 2
        ADD R6 R0 R1
        ADD R5 R6 R2
        ADD R4 R5 R3
        RETURN R4 1

        Function 1 (caller):
        GETUPVAL R1 0
        MOVE R2 R0
        LOADK R3 K0 [100]
        CALL R1 2 1
        LOADK R3 K1 [2]
        ADD R2 R1 R3
        RETURN R2 1
    */

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a, b, ...)
            local c, d = ...
            return a + b + c + d
        end
        local function caller(x)
            local result = inlinee(x, 100)
            return result + 2
        end
    )"),
        R"(
GETUPVAL R1 0
MOVE R2 R0
LOADK R3 K0 [100]
CMPPROTO R1 #0 L0
LOADNIL R6
LOADNIL R7
ADD R10 R2 R3
ADD R9 R10 R6
ADD R8 R9 R7
MOVE R1 R8
JUMP L1
L0: CALLFB R1 2 1 [-1]
L1: LOADK R3 K1 [2]
ADD R2 R1 R3
RETURN R2 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "vararg_func_vararg_multi_usage")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        NEWTABLE R0 0 2
        LOADK R1 K0 [1]
        LOADK R2 K1 [2]
        GETVARARGS R3 -1
        SETLIST R0 R1 -1 [1]
        LOADK R2 K2 [3]
        GETTABLE R1 R0 R2
        RETURN R1 1

        Function 1 (caller):
        GETUPVAL R0 0
        LOADK R1 K0 [10]
        LOADK R2 K1 [20]
        LOADK R3 K2 [30]
        CALL R0 3 1
        RETURN R0 1
    */

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(...)
            local t = {1, 2, ...}
            return t[3]
        end

        local function caller()
            local result = inlinee(10, 20, 30)
            return result
        end
    )"),
        R"(
GETUPVAL R0 0
LOADK R1 K0 [10]
LOADK R2 K1 [20]
LOADK R3 K2 [30]
CMPPROTO R0 #0 L0
NEWTABLE R4 0 2
LOADK R5 K3 [1]
LOADK R6 K4 [2]
MOVE R7 R1
MOVE R8 R2
MOVE R9 R3
SETLIST R4 R5 5 [1]
LOADK R6 K5 [3]
GETTABLE R5 R4 R6
MOVE R0 R5
RETURN R0 1
L0: CALLFB R0 3 1 [-1]
RETURN R0 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "vararg_func_vararg_multi_usage_2")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        NEWTABLE R1 0 2
        LOADK R2 K0 [1]
        MOVE R3 R0
        GETVARARGS R4 -1
        SETLIST R1 R2 -1 [1]
        LOADK R3 K1 [3]
        GETTABLE R2 R1 R3
        RETURN R2 1

        Function 1 (caller):
        GETUPVAL R0 0
        LOADK R1 K0 [10]
        LOADK R2 K1 [20]
        LOADK R3 K2 [30]
        CALL R0 3 1
        RETURN R0 1
    */

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a, ...)
            local t = {1, a, ...}
            return t[3]
        end

        local function caller()
            local result = inlinee(10, 20, 30)
            return result
        end
    )"),
        R"(
GETUPVAL R0 0
LOADK R1 K0 [10]
LOADK R2 K1 [20]
LOADK R3 K2 [30]
CMPPROTO R0 #0 L0
NEWTABLE R5 0 2
LOADK R6 K3 [1]
MOVE R7 R1
MOVE R8 R2
MOVE R9 R3
SETLIST R5 R6 4 [1]
LOADK R7 K4 [3]
GETTABLE R6 R5 R7
MOVE R0 R6
RETURN R0 1
L0: CALLFB R0 3 1 [-1]
RETURN R0 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "loop_phis")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        LOADK R1 K0 [0]
        LOADK R4 K1 [1]
        MOVE R2 R0
        LOADN R3 1
        FORNPREP R2 L3
        L0: LOADK R7 K1 [1]
        MOVE R5 R4
        LOADN R6 1
        FORNPREP R5 L2
        L1: ADD R1 R1 R7
        FORNLOOP R5 L1
        L2: FORNLOOP R2 L0
        L3: RETURN R1 1

        Function 1 (caller):
        GETUPVAL R1 0
        MOVE R2 R0
        CALL R1 1 1
        RETURN R1 1
    */

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(n)
            local sum = 0
            for i = 1, n do
                for j = 1, i do
                    sum = sum + j
                end
            end
            return sum
        end

        local function caller(x)
            local r = inlinee(x)
            return r
        end
    )"),
        R"(
GETUPVAL R1 0
MOVE R2 R0
CMPPROTO R1 #0 L4
LOADK R3 K0 [0]
LOADK R6 K1 [1]
MOVE R4 R2
LOADN R5 1
FORNPREP R4 L3
L0: LOADK R9 K1 [1]
MOVE R7 R6
LOADN R8 1
FORNPREP R7 L2
L1: ADD R3 R3 R9
FORNLOOP R7 L1
L2: FORNLOOP R4 L0
L3: MOVE R1 R3
RETURN R1 1
L4: CALLFB R1 1 1 [-1]
RETURN R1 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "retain_target_on_block_split")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    /*
        Function 0 (inlinee):
        LOADK R2 K0 [1]
        ADD R1 R0 R2
        RETURN R1 1

        Function 1 (caller):
        LOADK R1 K0 [0]
        LOADK R4 K1 [1]
        MOVE R2 R0
        LOADN R3 1
        FORNPREP R2 L1
        L0: GETUPVAL R5 0
        MOVE R6 R4
        CALL R5 1 1
        ADD R1 R1 R5
        FORNLOOP R2 L0
        L1: RETURN R1 1
    */

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a)
            return a + 1
        end

        local function caller(n)
            local sum = 0
            for i = 1, n do
                sum = sum + inlinee(i)
            end
            return sum
        end
    )"),
        R"(
LOADK R1 K0 [0]
LOADK R4 K1 [1]
MOVE R2 R0
LOADN R3 1
FORNPREP R2 L3
L0: GETUPVAL R5 0
MOVE R6 R4
CMPPROTO R5 #0 L1
LOADK R8 K1 [1]
ADD R7 R6 R8
MOVE R5 R7
JUMP L2
L1: CALLFB R5 1 1 [-1]
L2: ADD R1 R1 R5
FORNLOOP R2 L0
L3: RETURN R1 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_constants")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    std::string result = inlineAndPrint(
        R"(
        local function inlinee(a, b)
            return a + b
        end

        local function caller(x)
            local result = inlinee(5, 42)
            return result + 2
        end
    )",
        0,
        true
    );

    // The inlined ADD (5+42) folds to LOADK 47, and the MOVE copying it also folds.
    // The second ADD (result+2) cannot fold because R1 merges from both the inlined path (constant 47) and the CALLFB fallback path
    REQUIRE_EQ(
        "\n" + result,
        R"(
GETUPVAL R1 0
LOADK R2 K0 [5]
LOADK R3 K1 [42]
CMPPROTO R1 #0 L0
LOADK R4 K3 [47]
LOADK R1 K3 [47]
JUMP L1
L0: CALLFB R1 2 1 [-1]
L1: ADDK R2 R1 K2 [2]
RETURN R2 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_constants_chained")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    // Multiple arithmetic ops that should all fold
    std::string result = inlineAndPrint(
        R"(
        local function inlinee(a, b)
            return (a + b) * 2
        end

        local function caller(x)
            local result = inlinee(10, 11)
            return result + x
        end
    )",
        0,
        true
    );

    REQUIRE_EQ(
        "\n" + result,
        R"(
GETUPVAL R1 0
LOADK R2 K0 [10]
LOADK R3 K1 [11]
CMPPROTO R1 #0 L0
LOADK R5 K3 [21]
LOADK R6 K2 [2]
LOADK R4 K4 [42]
LOADK R1 K4 [42]
JUMP L1
L0: CALLFB R1 2 1 [-1]
L1: ADD R2 R1 R0
RETURN R2 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_constants_div_by_zero")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    // Division by zero should fold to inf
    std::string result = inlineAndPrint(
        R"(
        local function inlinee(a, b)
            return a / b
        end

        local function caller(x)
            local result = inlinee(10, 0)
            return result + x
        end
    )",
        0,
        true
    );

    REQUIRE_EQ(
        "\n" + result,
        R"(
GETUPVAL R1 0
LOADK R2 K0 [10]
LOADK R3 K1 [0]
CMPPROTO R1 #0 L0
LOADK R4 K2 [inf]
LOADK R1 K2 [inf]
JUMP L1
L0: CALLFB R1 2 1 [-1]
L1: ADD R2 R1 R0
RETURN R2 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_constants_with_branch")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    std::string result = inlineAndPrint(
        R"(
            local function inlinee(a)
                if a then
                    return 1
                else
                    return 2
                end
            end

            local function caller()
                local t = true
                local res = inlinee(t)
                return res
            end
            )",
        0,
        true
    );

    REQUIRE_EQ(
        "\n" + result,
        R"(
LOADB R0 1
GETUPVAL R1 0
LOADB R2 1
CMPPROTO R1 #0 L0
LOADK R3 K0 [1]
LOADK R1 K0 [1]
RETURN R1 1
L0: CALLFB R1 1 1 [-1]
RETURN R1 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_constants_with_branches")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    std::string result = inlineAndPrint(
        R"(
            local function inlinee(a)
                if a then
                    if a > 1 then
                        return 3
                    else
                        return 1
                    end
                else
                    return 2
                end
            end

            local function caller()
                local res = inlinee(5)
                return res
            end
            )",
        0,
        true
    );

    REQUIRE_EQ("\n" + result, R"(
GETUPVAL R0 0
LOADK R1 K0 [5]
CMPPROTO R0 #0 L0
LOADK R2 K1 [1]
LOADK R2 K2 [3]
LOADK R0 K2 [3]
RETURN R0 1
L0: CALLFB R0 1 1 [-1]
RETURN R0 1
)");
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_constants_with_for_loop")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    std::string result = inlineAndPrint(
        R"(
            local function inlinee(a)
                local sum = 0
                for i = 1, a do
                    sum = sum + i
                end
                return sum
            end

            local function caller()
                local t = 10
                local res = inlinee(t)
                return res
            end
            )",
        0,
        true
    );

    REQUIRE_EQ(
        "\n" + result,
        R"(
LOADK R0 K0 [10]
GETUPVAL R1 0
LOADK R2 K0 [10]
CMPPROTO R1 #0 L2
LOADK R3 K1 [0]
LOADK R6 K2 [1]
LOADK R4 K0 [10]
LOADN R5 1
FORNPREP R4 L1
L0: ADD R3 R3 R6
FORNLOOP R4 L0
L1: MOVE R1 R3
RETURN R1 1
L2: CALLFB R1 1 1 [-1]
RETURN R1 1
)"
    );
}

// tests JUMPIF(NOT)LT path in evaluateComparisonCondition
TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_constants_prunes_ordering_branch")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    std::string result = inlineAndPrint(
        R"(
        local function inlinee(a, b)
            if a < b then
                return a
            end
            return b
        end

        local function caller()
            local res = inlinee(3, 10)
            return res
        end
    )",
        0,
        true
    );

    REQUIRE_EQ(
        "\n" + result,
        R"(
GETUPVAL R0 0
LOADK R1 K0 [3]
LOADK R2 K1 [10]
CMPPROTO R0 #0 L0
LOADK R0 K0 [3]
RETURN R0 1
L0: CALLFB R0 2 1 [-1]
RETURN R0 1
)"
    );
}

// JUMPIFEQ equality comparison folding
TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_constants_prunes_equality_branch")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    std::string result = inlineAndPrint(
        R"(
        local function inlinee(a, b)
            if a == b then
                return 1
            end
            return 2
        end

        local function caller()
            local res = inlinee(7, 7)
            return res
        end
    )",
        0,
        true
    );

    REQUIRE_EQ(
        "\n" + result,
        R"(
GETUPVAL R0 0
LOADK R1 K0 [7]
LOADK R2 K0 [7]
CMPPROTO R0 #0 L0
LOADK R3 K1 [1]
LOADK R0 K1 [1]
RETURN R0 1
L0: CALLFB R0 2 1 [-1]
RETURN R0 1
)"
    );
}

// constant string argument compared against a string literal tests evaluateXeqkCondition
TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_constants_string_equality")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    std::string result = inlineAndPrint(
        R"(
        local function inlinee(s)
            if s == "yes" then
                return 1
            end
            return 0
        end

        local function caller()
            local res = inlinee("yes")
            return res
        end
    )",
        0,
        true
    );

    REQUIRE_EQ(
        "\n" + result,
        R"(
GETUPVAL R0 0
LOADK R1 K0 ['yes']
CMPPROTO R0 #0 L0
LOADK R2 K0 ['yes']
LOADK R2 K1 [1]
LOADK R0 K1 [1]
RETURN R0 1
L0: CALLFB R0 1 1 [-1]
RETURN R0 1
)"
    );
}

// the missing parameter is treated as a LOADNIL, which SCCP can fold to prune `return 1`
TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_constants_nil_argument")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    std::string result = inlineAndPrint(
        R"(
        local function inlinee(a)
            if a == nil then
                return 0
            end
            return 1
        end

        local function caller()
            local res = inlinee()
            return res
        end
    )",
        0,
        true
    );

    REQUIRE_EQ(
        "\n" + result,
        R"(
GETUPVAL R0 0
CMPPROTO R0 #0 L0
LOADNIL R1
LOADNIL R2
LOADK R2 K0 [0]
LOADK R0 K0 [0]
RETURN R0 1
L0: CALLFB R0 0 1 [-1]
RETURN R0 1
)"
    );
}

//  when the caller passes runtime values, no operand is constant, so the inlined ADD must survive unfolded
TEST_CASE_FIXTURE(BytecodeInlinerFixture, "does_not_fold_runtime_arguments")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    std::string result = inlineAndPrint(
        R"(
        local function inlinee(a, b)
            return a + b
        end

        local function caller(x, y)
            local res = inlinee(x, y)
            return res
        end
    )",
        0,
        true
    );

    REQUIRE_EQ(
        "\n" + result,
        R"(
GETUPVAL R2 0
MOVE R3 R0
MOVE R4 R1
CMPPROTO R2 #0 L0
ADD R5 R3 R4
MOVE R2 R5
RETURN R2 1
L0: CALLFB R2 2 1 [-1]
RETURN R2 1
)"
    );
}

// Regression for the SCCP loop-exit phi fix
// A register defined inside a loop and used several blocks downstream of the loop exit must resolve through a loop-exit phi, not the pre-loop LOADNIL
// Without the phi, SCCP sees a constant nil for `y` and folds `if not y` the wrong way
// In the cdx benchmark this caused infinite recursion
TEST_CASE_FIXTURE(BytecodeInlinerFixture, "graph_builds_loop_exit_phi_for_downstream_use")
{
    // `y` is initialized to nil before the loop, reassigned inside it, and tested only after several intervening blocks (`local z`, `if flag`), so
    // the use is downstream of the loop exit rather than in an immediate successor
    std::vector<CompTimeBcFunction> graphs = buildGraphs(R"(
        function treeInsertLike(root, key, flag)
            local y = nil
            local x = root
            while x do
                y = x
                local cmp = key - x.k
                if cmp < 0 then
                    x = x.left
                elseif cmp > 0 then
                    x = x.right
                else
                    return "found"
                end
            end
            local z = { k = key }
            if flag then
                z.tag = 1
            else
                z.tag = 2
            end
            if not y then
                return "root"
            else
                return "child"
            end
        end
    )");

    // pick the one that actually contains a loop
    CompTimeBcFunction* loopFn = nullptr;
    BcOp header;
    for (CompTimeBcFunction& fn : graphs)
    {
        for (uint32_t bi = 0; bi < fn.blocks.size() && !loopFn; bi++)
        {
            for (const BcBlockEdge& e : fn.blocks[bi].predecessors)
            {
                if (e.kind == BcBlockEdgeKind::Loop)
                {
                    loopFn = &fn;
                    header = BcOp{BcOpKind::Block, bi};
                    break;
                }
            }
        }
    }
    REQUIRE(loopFn != nullptr);

    auto isLoadNil = [&](BcOp op)
    {
        return op.kind == BcOpKind::Inst && loopFn->instOp(op).op == LOP_LOADNIL;
    };

    // the exit phi for `y` (LOADNIL merged with the in-loop MOVE) is anchored in the loop header
    bool headerHasExitPhi = false;
    for (BcOp phiOp : loopFn->blockOp(header).phis) // NOLINT
    {
        BcPhi& phi = loopFn->phiOp(phiOp);
        if (phi.ops.size() >= 2 && std::any_of(phi.ops.begin(), phi.ops.end(), isLoadNil))
            headerHasExitPhi = true;
    }
    CHECK(headerHasExitPhi);

    // the only LOADNIL is `y = nil`, and should not exist after the fix
    for (uint32_t bi = 0; bi < loopFn->blocks.size(); bi++)
        for (BcOp instOp : loopFn->blocks[bi].ops)
        {
            BcInst& inst = loopFn->instOp(instOp);
            if (inst.op != LOP_JUMPIF && inst.op != LOP_JUMPIFNOT)
                continue;
            for (BcOp in : inst.ops)
                CHECK_FALSE(isLoadNil(in));
        }
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "empty_inlinee_with_vararg")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a, ...)
        end

        local function caller()
            return {inlinee, (inlinee())}
        end
    )"),
        R"(
NEWTABLE R0 0 2
GETUPVAL R1 0
GETUPVAL R2 0
CMPPROTO R2 #0 L0
LOADNIL R3
LOADNIL R2
JUMP L1
L0: CALLFB R2 0 1 [-1]
L1: SETLIST R0 R1 2 [1]
RETURN R0 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "empty_varargs_sequence_in_setlist")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(...)
            return {...}
        end

        local function caller()
            inlinee()
        end
    )"),
        R"(
GETUPVAL R0 0
CMPPROTO R0 #0 L0
NEWTABLE R1 0 0
MOVE R0 R1
RETURN R0 0
L0: CALLFB R0 0 0 [-1]
RETURN R0 0
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "empty_varargs_sequence_in_for_loop")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a, ...)
            for _ in ... do
                pcall += _
            end
        end

        local function caller()
            inlinee()
        end
    )"),
        R"(
GETUPVAL R0 0
CMPPROTO R0 #0 L2
LOADNIL R1
LOADNIL R3
LOADNIL R4
LOADNIL R5
FORGPREP R3 L1
L0: GETGLOBAL R8 K0 ['pcall']
ADD R8 R8 R6
SETGLOBAL R8 K0 ['pcall']
L1: FORGLOOP R3 L0 1
RETURN R0 0
L2: CALLFB R0 0 0 [-1]
RETURN R0 0
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "vararg_in_loops_phi")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
            local function inlinee(...)
                repeat
                    local a = ...
                    while a do break end
                until false
            end

            local function caller()
                inlinee()
            end
    )"),
        R"(
GETUPVAL R0 0
CMPPROTO R0 #0 L3
L0: LOADNIL R1
L1: JUMPIFNOT R1 L2
JUMP L2
JUMPBACK L1
L2: LOADB R2 0
JUMPIF R2 L4
JUMPBACK L0
RETURN R0 0
L3: CALLFB R0 0 0 [-1]
L4: RETURN R0 0
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_keeps_unreachable_closeupvals_block")
{
    std::vector<CompTimeBcFunction> graphs = buildGraphs(R"(
        local function caller()
            local f = function() end
            while true do
                local cap = function() f = f end
                f()
            end
        end
        caller()
    )");

    CompTimeBcFunction* caller = nullptr;
    for (CompTimeBcFunction& fn : graphs)
        if (fn.debugname == "caller")
            caller = &fn;
    REQUIRE(caller);

    BcVmConstImpl impl(*caller);
    Bytecode::foldConstants(*caller, impl);

    bool foundCloseUpvalsBlock = false;
    for (const BcBlock& block : caller->blocks)
    {
        bool hasClose = false;
        for (BcOp op : block.ops)
            if (caller->instOp(op).op == LOP_CLOSEUPVALS)
                hasClose = true;

        if (hasClose)
        {
            foundCloseUpvalsBlock = true;
            CHECK((block.flags & BcBlockFlag::Dead) == 0);
        }
    }
    CHECK(foundCloseUpvalsBlock);
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "vararg_projection_in_return_phi")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a, ...)
            return a and ...
        end

        local function caller(x)
            local r = inlinee(x)
            return r
        end
    )"),
        R"(
GETUPVAL R1 0
MOVE R2 R0
CMPPROTO R1 #0 L1
MOVE R4 R2
JUMPIFNOT R4 L0
LOADNIL R4
L0: MOVE R1 R4
RETURN R1 1
L1: CALLFB R1 1 1 [-1]
RETURN R1 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_sub_constant_lhs_is_negation_not_move")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a, x)
            return a - x
        end
        local function caller(x)
            local r = inlinee(0, x)
            return r + x
        end
    )", 0, true),
        R"(
GETUPVAL R1 0
LOADK R2 K0 [0]
MOVE R3 R0
CMPPROTO R1 #0 L0
SUB R4 R2 R3
MOVE R1 R4
JUMP L1
L0: CALLFB R1 2 1 [-1]
L1: ADD R2 R1 R0
RETURN R2 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_div_constant_lhs_one_is_reciprocal_not_move")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(a, x)
            return a / x
        end
        local function caller(x)
            local r = inlinee(1, x)
            return r + x
        end
    )", 0, true),
        R"(
GETUPVAL R1 0
LOADK R2 K0 [1]
MOVE R3 R0
CMPPROTO R1 #0 L0
DIV R4 R2 R3
MOVE R1 R4
JUMP L1
L0: CALLFB R1 2 1 [-1]
L1: ADD R2 R1 R0
RETURN R2 1
)"
    );
}

TEST_CASE_FIXTURE(BytecodeInlinerFixture, "fold_jumpxeqkb_bool_immediate_value")
{
    ScopedFastFlag emitCallFb{FFlag::LuauEmitCallFeedback, true};

    REQUIRE_EQ(
        "\n" + inlineAndPrint(R"(
        local function inlinee(flag)
            if flag == true then return 1 else return 2 end
        end
        local function caller(x)
            local r = inlinee(true)
            return r + x
        end
    )", 0, true, 1),
        R"(
GETUPVAL R1 0
LOADB R2 1
CMPPROTO R1 #0 L0
LOADN R3 1
LOADN R1 1
JUMP L1
L0: CALLFB R1 1 1 [-1]
L1: ADD R2 R1 R0
RETURN R2 1
)"
    );
}

TEST_SUITE_END();
