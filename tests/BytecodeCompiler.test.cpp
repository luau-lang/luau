// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BytecodeBuilder.h"
#include "Luau/BytecodeGraph.h"
#include "Luau/BytecodeWire.h"
#include "Luau/Compiler.h"
#include "Luau/Parser.h"

#include <optional>

#include "lua.h"
#include "lualib.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;
using namespace Luau::Bytecode;

namespace
{

struct BytecodeCompilerFixture
{
    BytecodeCompilerFixture() {}

    std::optional<Bytecode::BcFunction> buildBytecode(std::string_view src, int optimizationLevel = 0)
    {
        auto bytecode = getFunctionBytecode(src, optimizationLevel);
        if (bytecode)
        {
            strings = bytecode->second;
            std::vector<std::string_view> table;
            for (std::string& s : strings)
                table.push_back(s);
            return {Bytecode::fromFunctionBytecode(bytecode->first, table)};
        }
        return {};
    }

    std::optional<std::pair<std::string, std::vector<std::string>>> getFunctionBytecode(std::string_view src, int optimizationLevel = 0)
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
            return {{bcb.getFunctionData(0), extractStringTable(bcb)}};
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

TEST_SUITE_BEGIN("BytecodeCompiler");

bool checkOps(BcFunction& fn, std::list<BcOp>& ops, std::initializer_list<LuauOpcode> expected_ops)
{
    std::vector<LuauOpcode> expected = expected_ops;
    if (ops.size() != expected.size())
    {
        WARN_EQ(ops.size(), expected.size());
        return false;
    }
    int i = 0;
    for (auto& op : ops)
    {
        if (fn.instOp(op).op != expected[i])
        {
            WARN_EQ(fn.instOp(op).op, expected[i]);
            return false;
        }
        i++;
    }
    return true;
}

bool checkEdges(BcEdges& edges, std::initializer_list<BcBlockEdgeKind> expected_edges)
{
    std::vector<BcBlockEdgeKind> expected = expected_edges;
    if (edges.size() != expected.size())
        return false;
    int i = 0;
    for (auto& e : edges)
        if (e.kind != expected[i++])
            return false;
    return true;
}

inline BcOp getOp(BcBlock& block, int idx)
{
    return *std::next(block.ops.begin(), idx);
}

inline BcOp getBlockOp(BcEdges& edges, BcBlockEdgeKind kind)
{
    for (auto& e : edges)
        if (e.kind == kind)
            return e.target;
    LUAU_UNREACHABLE();
}

inline BcOp fallthroughOp(BcEdges& edges)
{
    return getBlockOp(edges, BcBlockEdgeKind::Fallthrough);
}

inline BcOp branchOp(BcEdges& edges)
{
    return getBlockOp(edges, BcBlockEdgeKind::Branch);
}

inline BcOp loopOp(BcEdges& edges)
{
    return getBlockOp(edges, BcBlockEdgeKind::Loop);
}

inline BcBlock& getBlock(BcFunction& fn, BcEdges& edges, BcBlockEdgeKind kind)
{
    return fn.blockOp(getBlockOp(edges, kind));
}

inline BcBlock& fallthroughBlock(BcFunction& fn, BcEdges& edges)
{
    return getBlock(fn, edges, BcBlockEdgeKind::Fallthrough);
}

inline BcBlock& branchBlock(BcFunction& fn, BcEdges& edges)
{
    return getBlock(fn, edges, BcBlockEdgeKind::Branch);
}

inline BcBlock& loopBlock(BcFunction& fn, BcEdges& edges)
{
    return getBlock(fn, edges, BcBlockEdgeKind::Loop);
}

inline bool isPhiOf(BcFunction& fn, BcOp op, BcOp left, BcOp right)
{
    if (op.kind != BcOpKind::Phi)
        return false;
    BcPhi& opPhi = fn.phiOp(op);
    if (opPhi.ops.size() != 2)
        return false;
    return opPhi.ops.size() == 2 && opPhi.ops[0] == left && opPhi.ops[1] == right;
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "from_function_bytecode")
{
    auto fn = buildBytecode(R"(
        function fn(a, b)
            local extra = 0
            if a > b then extra = 1 end 
            return extra + a + b
        end
    )");

    /*
        Function 0 (fn):
        // Block 1 (entry)
        2: LOADK R2 K0 [0]
        3: JUMPIFNOTLT R1 R0 L0
        // Block 2 (condTrue)
        3: LOADK R2 K1 [1]
        // Block 3 (condFalse)
        4: L0: ADD R4 R2 R0
        4: ADD R3 R4 R1
        4: RETURN R3 1
        // Block 4 (exit)
    */

    REQUIRE(fn);
    // function meta
    REQUIRE_EQ(fn->nups, 0);
    REQUIRE_EQ(fn->numparams, 2);
    REQUIRE_EQ(fn->constants.size(), 2);

    // CFG Blocks
    REQUIRE_EQ(fn->blocks.size(), 4);
    BcBlock& entry = fn->blockOp(fn->entryBlock);
    // Entry block ends with if
    REQUIRE(checkEdges(entry.successors, {BcBlockEdgeKind::Branch, BcBlockEdgeKind::Fallthrough}));
    BcOp condFalseOp = branchOp(entry.successors);
    BcBlock& condTrue = fn->blockOp(entry.successors[1].target);
    REQUIRE(checkEdges(condTrue.successors, {BcBlockEdgeKind::Fallthrough}));
    REQUIRE_EQ(fallthroughOp(condTrue.successors), condFalseOp);
    BcBlock& condFalse = fn->blockOp(condFalseOp);
    REQUIRE(checkEdges(condFalse.successors, {BcBlockEdgeKind::Fallthrough}));
    REQUIRE_EQ(fallthroughOp(condFalse.successors), fn->exitBlock);
    BcBlock& exit = fn->blockOp(fn->exitBlock);

    // Instructions
    // Entry
    BcOp loadKOp;
    REQUIRE_EQ(entry.ops.size(), 2);
    {
        auto it = entry.ops.begin();
        loadKOp = *it++;
        BcInst& loadK = fn->instOp(loadKOp);
        REQUIRE_EQ(loadK.op, LOP_LOADK);
        REQUIRE_EQ(loadK.ops.size(), 1);
        REQUIRE_EQ(loadK.ops[0].kind, BcOpKind::VmConst);
        REQUIRE_EQ(loadK.ops[0].index, 0);
        REQUIRE_EQ(fn->constants[0].kind, BcVmConstKind::Number);
        REQUIRE_EQ(fn->constants[0].valueNumber, 0);

        BcInst& jumpIfNotLt = fn->instOp(*it);
        REQUIRE_EQ(jumpIfNotLt.op, LOP_JUMPIFNOTLT);
        REQUIRE_EQ(jumpIfNotLt.ops.size(), 3);
    }

    REQUIRE(checkOps(*fn, condTrue.ops, {LOP_LOADK}));
    REQUIRE(checkOps(*fn, condFalse.ops, {LOP_ADD, LOP_ADD, LOP_RETURN}));
    REQUIRE(checkOps(*fn, exit.ops, {}));
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "repeat_until_loop")
{
    auto fn = buildBytecode(R"(
        function fn()
            local var = 0
            repeat var += 1 until var < 10
        end
    )");

    /*
        // Block 1 (entry)
        LOADK R0 K0 [0]
        // Block 2 (loopBody)
        L0: LOADK R1 K1 [1]
        ADD R0 R0 R1
        LOADK R1 K2 [10]
        JUMPIFLT R0 R1 L1
        // Block 3 (loopJumpBack)
        JUMPBACK L0
        // Block 4 (ret)
        L1: RETURN R0 0
        // Block 5 (exit)
    */

    // CFG Blocks
    REQUIRE_EQ(fn->blocks.size(), 5);
    BcBlock& entry = fn->blockOp(fn->entryBlock);
    REQUIRE(checkEdges(entry.successors, {BcBlockEdgeKind::Fallthrough}));
    BcBlock& loopBody = fallthroughBlock(*fn, entry.successors);
    REQUIRE(checkEdges(loopBody.predecessors, {BcBlockEdgeKind::Fallthrough, BcBlockEdgeKind::Loop}));
    REQUIRE(checkEdges(loopBody.successors, {BcBlockEdgeKind::Branch, BcBlockEdgeKind::Fallthrough}));
    BcBlock& loopJumpBack = fallthroughBlock(*fn, loopBody.successors);
    REQUIRE(checkEdges(loopJumpBack.successors, {BcBlockEdgeKind::Loop}));
    BcBlock& ret = branchBlock(*fn, loopBody.successors);

    // Instructions
    REQUIRE(checkOps(*fn, entry.ops, {LOP_LOADK}));
    REQUIRE(checkOps(*fn, loopBody.ops, {LOP_LOADK, LOP_ADD, LOP_LOADK, LOP_JUMPIFLT}));
    REQUIRE(checkOps(*fn, loopJumpBack.ops, {LOP_JUMPBACK}));
    REQUIRE(checkOps(*fn, ret.ops, {LOP_RETURN}));
    {
        BcOp varInitOp = getOp(entry, 0);
        BcOp loadKOneOp = getOp(loopBody, 0);
        BcOp addVarOp = getOp(loopBody, 1);
        BcInst& addVar = fn->instOp(addVarOp);
        REQUIRE_EQ(addVar.ops.size(), 2);
        REQUIRE_EQ(addVar.ops[0].kind, BcOpKind::Phi);
        BcPhi& addVarPhi = fn->phiOp(addVar.ops[0]);
        REQUIRE_EQ(addVarPhi.ops[0], varInitOp);
        REQUIRE_EQ(addVarPhi.ops[1], addVarOp);
        REQUIRE_EQ(addVar.ops[1], loadKOneOp);
    }
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "for_loop_and_backward_input")
{
    auto fn = buildBytecode(R"(
        function fn()
            local var = 3
            for i = 1, 10 do
                if var > 0 then print(i) end
                var -= 1;
            end
        end
    )");

    /*
        // Block 1 (entry)
        LOADK R0 K0 [3]
        // initialize loop variable
        LOADK R3 K1 [1]
        LOADK R1 K2 [10]
        LOADN R2 1
        FORNPREP R1 L2
        // Block 2 (loopEnter)
        L0: LOADK R4 K3 [0]
        JUMPIFNOTLT R4 R0 L1
        // Block 3 (loopCond)
        GETGLOBAL R4 K4 ['print']
        MOVE R5 R3
        CALL R4 1 0
        // Block 4 (loopEpllog)
        L1: LOADK R4 K1 [1]
        SUB R0 R0 R4
        FORNLOOP R1 L0
        // Block 5 (ret)
        L2: RETURN R0 0
        // Block 6 (exit)
    */

    // CFG Blocks
    REQUIRE_EQ(fn->blocks.size(), 6);
    BcBlock& entry = fn->blockOp(fn->entryBlock);
    // Entry block ends with loop header
    REQUIRE_EQ(entry.successors.size(), 2);
    REQUIRE(checkEdges(entry.successors, {BcBlockEdgeKind::Branch, BcBlockEdgeKind::Fallthrough}));
    BcOp loopEnterOp = fallthroughOp(entry.successors);
    BcBlock& loopEnter = fn->blockOp(loopEnterOp);
    // Check we have 2 incoming edges: 1 fallthrough from FORNPREP and 1 back edge from FORNLOOP
    REQUIRE(checkEdges(loopEnter.predecessors, {BcBlockEdgeKind::Fallthrough, BcBlockEdgeKind::Loop}));
    REQUIRE(checkEdges(loopEnter.successors, {BcBlockEdgeKind::Branch, BcBlockEdgeKind::Fallthrough}));
    BcBlock& loopCond = fallthroughBlock(*fn, loopEnter.successors);
    REQUIRE(checkEdges(loopCond.successors, {BcBlockEdgeKind::Fallthrough}));
    BcOp loopEpllogOp = branchOp(loopEnter.successors);
    REQUIRE_EQ(fallthroughOp(loopCond.successors), loopEpllogOp);
    BcBlock& loopEpllog = fn->blockOp(loopEpllogOp);
    REQUIRE(checkEdges(loopEpllog.successors, {BcBlockEdgeKind::Loop, BcBlockEdgeKind::Fallthrough}));
    REQUIRE_EQ(loopOp(loopEpllog.successors), loopEnterOp);
    BcBlock& ret = fn->blockOp(loopEpllog.successors[1].target);

    // Instructions
    REQUIRE(checkOps(*fn, entry.ops, {LOP_LOADK, LOP_LOADK, LOP_LOADK, LOP_LOADN, LOP_FORNPREP}));
    REQUIRE(checkOps(*fn, loopEnter.ops, {LOP_LOADK, LOP_JUMPIFNOTLT}));
    {
        BcOp varInitOp = *entry.ops.begin();
        BcOp subVarOp = *std::next(loopEpllog.ops.begin(), 1);
        BcInst& jumpIfNotLt = fn->instOp(*std::next(loopEnter.ops.begin(), 1));
        REQUIRE_EQ(jumpIfNotLt.ops.size(), 3);
        // first input is LOADK
        REQUIRE_EQ(jumpIfNotLt.ops[0], *loopEnter.ops.begin());
        // second input is Phi coming outside of the loop and from the loop forward
        REQUIRE(isPhiOf(*fn, jumpIfNotLt.ops[1], varInitOp, subVarOp));
        // third input is target block
        REQUIRE_EQ(jumpIfNotLt.ops[2], loopEpllogOp);

        BcInst& subVar = fn->instOp(subVarOp);
        REQUIRE_EQ(subVar.ops.size(), 2);
        // second input is LOADK
        REQUIRE(isPhiOf(*fn, subVar.ops[0], varInitOp, subVarOp));
        REQUIRE_EQ(subVar.ops[1], *loopEpllog.ops.begin());
    }
    REQUIRE(checkOps(*fn, loopCond.ops, {LOP_GETGLOBAL, LOP_MOVE, LOP_CALL}));
    REQUIRE(checkOps(*fn, loopEpllog.ops, {LOP_LOADK, LOP_SUB, LOP_FORNLOOP}));
    REQUIRE(checkOps(*fn, ret.ops, {LOP_RETURN}));
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "nested_loops")
{
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

    /*
        // Block 1 (entry)
        LOADK R0 K0 [0]
        LOADK R1 K0 [0]
        // Block 2 (outerEntry)
        L0: LOADK R2 K0 [0]
        // Block 3 (innerEntry)
        L1: MUL R3 R2 R1
        ADD R0 R0 R3
        LOADK R3 K1 [1]
        ADD R2 R2 R3
        LOADK R3 K2 [5]
        JUMPIFLT R2 R3 L2
        // Block 4 (innerBackLoop)
        JUMPBACK L1
        // Block 5 (outerEpllog)
        L2: LOADK R3 K1 [1]
        ADD R1 R1 R3
        LOADK R3 K3 [10]
        JUMPIFLT R1 R3 L3
        // Block 6 (outerBackLoop)
        JUMPBACK L0
        // Block 7 (ret)
        L3: RETURN R0 0
        // Block 8 (exit)
    */

    // CFG Blocks
    REQUIRE_EQ(fn->blocks.size(), 8);
    BcBlock& entry = fn->blockOp(fn->entryBlock);
    REQUIRE(checkEdges(entry.successors, {BcBlockEdgeKind::Fallthrough}));
    BcOp outerEntryOp = fallthroughOp(entry.successors);
    BcBlock& outerEntry = fn->blockOp(outerEntryOp);
    REQUIRE(checkEdges(outerEntry.predecessors, {BcBlockEdgeKind::Fallthrough, BcBlockEdgeKind::Loop}));
    REQUIRE(checkEdges(outerEntry.successors, {BcBlockEdgeKind::Fallthrough}));
    BcOp innerEntryOp = fallthroughOp(outerEntry.successors);
    BcBlock& innerEntry = fn->blockOp(innerEntryOp);
    REQUIRE(checkEdges(innerEntry.predecessors, {BcBlockEdgeKind::Fallthrough, BcBlockEdgeKind::Loop}));
    REQUIRE(checkEdges(innerEntry.successors, {BcBlockEdgeKind::Branch, BcBlockEdgeKind::Fallthrough}));
    BcBlock& innerBackLoop = fallthroughBlock(*fn, innerEntry.successors);
    REQUIRE(checkEdges(innerBackLoop.successors, {BcBlockEdgeKind::Loop}));
    REQUIRE_EQ(loopOp(innerBackLoop.successors), innerEntryOp);
    BcBlock& outerEpllog = branchBlock(*fn, innerEntry.successors);
    REQUIRE(checkEdges(outerEpllog.successors, {BcBlockEdgeKind::Branch, BcBlockEdgeKind::Fallthrough}));
    BcBlock& outerBackLoop = fallthroughBlock(*fn, outerEpllog.successors);
    REQUIRE(checkEdges(outerBackLoop.successors, {BcBlockEdgeKind::Loop}));
    REQUIRE_EQ(loopOp(outerBackLoop.successors), outerEntryOp);
    BcBlock& ret = branchBlock(*fn, outerEpllog.successors);

    // Instructions
    REQUIRE(checkOps(*fn, entry.ops, {LOP_LOADK, LOP_LOADK}));
    REQUIRE(checkOps(*fn, outerEntry.ops, {LOP_LOADK}));
    REQUIRE(checkOps(*fn, innerEntry.ops, {LOP_MUL, LOP_ADD, LOP_LOADK, LOP_ADD, LOP_LOADK, LOP_JUMPIFLT}));
    REQUIRE(checkOps(*fn, innerBackLoop.ops, {LOP_JUMPBACK}));
    REQUIRE(checkOps(*fn, outerEpllog.ops, {LOP_LOADK, LOP_ADD, LOP_LOADK, LOP_JUMPIFLT}));
    REQUIRE(checkOps(*fn, outerBackLoop.ops, {LOP_JUMPBACK}));
    REQUIRE(checkOps(*fn, ret.ops, {LOP_RETURN}));
    {
        BcOp varInitOp = getOp(entry, 1);
        BcOp varIncOp = getOp(outerEpllog, 1);
        BcOp iInitOp = getOp(outerEntry, 0);
        BcOp iIncOp = getOp(innerEntry, 3);
        BcOp iTimesVarOp = getOp(innerEntry, 0);
        BcInst& iTimesVar = fn->instOp(iTimesVarOp);
        REQUIRE_EQ(iTimesVar.ops.size(), 2);
        REQUIRE(isPhiOf(*fn, iTimesVar.ops[0], iInitOp, iIncOp));
        REQUIRE(isPhiOf(*fn, iTimesVar.ops[1], varInitOp, varIncOp));
    }
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "multi_call_fixed")
{
    auto fn = buildBytecode(R"(
        local function x()
            local a, b = f()
            return b, a
        end
    )");

    /*
        GETGLOBAL R0 K0 ['f']
        CALL R0 0 2
        MOVE R2 R1
        MOVE R3 R0
        RETURN R2 2
    */

    // CFG Blocks
    BcBlock& entry = fn->blockOp(fn->entryBlock);

    // Instructions
    REQUIRE(checkOps(*fn, entry.ops, {LOP_GETGLOBAL, LOP_CALL, LOP_MOVE, LOP_MOVE, LOP_RETURN}));
    {
        BcOp callOp = getOp(entry, 1);
        BcOp move1op = getOp(entry, 2);
        BcInst& move1 = fn->instOp(move1op);
        REQUIRE_EQ(move1.ops.size(), 1);
        REQUIRE_EQ(move1.ops[0].kind, BcOpKind::Proj);
        BcProj& move1proj = fn->projOp(move1.ops[0]);
        REQUIRE_EQ(move1proj.op, callOp);
        REQUIRE_EQ(move1proj.index, 1);
        BcOp move2op = getOp(entry, 3);
        BcInst& move2 = fn->instOp(move2op);
        REQUIRE_EQ(move2.ops.size(), 1);
        REQUIRE_EQ(move2.ops[0].kind, BcOpKind::Proj);
        BcProj& move2proj = fn->projOp(move2.ops[0]);
        REQUIRE_EQ(move2proj.op, callOp);
        REQUIRE_EQ(move2proj.index, 0);
        BcInst& ret = fn->instOp(getOp(entry, 4));
        REQUIRE_EQ(ret.ops.size(), 3);
        REQUIRE_EQ(ret.ops[0].kind, BcOpKind::Imm);
        BcImm& retCount = fn->immOp(ret.ops[0]);
        REQUIRE_EQ(retCount.kind, BcImmKind::Int);
        REQUIRE_EQ(retCount.valueInt, 2);
        REQUIRE_EQ(ret.ops[1], move1op);
        REQUIRE_EQ(ret.ops[2], move2op);
    }
}

TEST_CASE_FIXTURE(BytecodeCompilerFixture, "multi_call_variadic")
{
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

    /*
        // Block 1 (entry)
        LOADK R1 K0 [0]
        JUMPIFNOTLT R1 R0 L0
        // Block 2 (ifTrue)
        LOADK R1 K0 [0]
        LOADK R2 K1 [1]
        RETURN R1 2
        // Block 3 (ifFalse)
        L0: GETUPVAL R1 0
        LOADK R3 K1 [1]
        SUB R2 R0 R3
        CALL R1 1 2
        ADD R3 R1 R2
        GETUPVAL R4 0
        MOVE R5 R0
        CALL R4 1 -1
        RETURN R3 -1
        // Block 4 (exit)
    */

    // CFG Blocks
    REQUIRE_EQ(fn->blocks.size(), 4);
    BcBlock& entry = fn->blockOp(fn->entryBlock);
    REQUIRE(checkEdges(entry.successors, {BcBlockEdgeKind::Branch, BcBlockEdgeKind::Fallthrough}));
    BcBlock& ifTrue = fallthroughBlock(*fn, entry.successors);
    REQUIRE(checkEdges(ifTrue.successors, {BcBlockEdgeKind::Fallthrough}));
    BcBlock& ifFalse = branchBlock(*fn, entry.successors);
    REQUIRE(checkEdges(ifTrue.successors, {BcBlockEdgeKind::Fallthrough}));

    // Instructions
    REQUIRE(checkOps(*fn, entry.ops, {LOP_LOADK, LOP_JUMPIFNOTLT}));
    REQUIRE(checkOps(*fn, ifTrue.ops, {LOP_LOADK, LOP_LOADK, LOP_RETURN}));
    REQUIRE(checkOps(*fn, ifFalse.ops, {LOP_GETUPVAL, LOP_LOADK, LOP_SUB, LOP_CALL, LOP_ADD, LOP_GETUPVAL, LOP_MOVE, LOP_CALL, LOP_RETURN}));
    {
        BcInst& ret = fn->instOp(getOp(ifFalse, 8));
        REQUIRE_EQ(ret.ops.size(), 3);
        REQUIRE_EQ(ret.ops[0].kind, BcOpKind::Imm);
        BcImm& retCount = fn->immOp(ret.ops[0]);
        REQUIRE_EQ(retCount.kind, BcImmKind::Int);
        REQUIRE_EQ(retCount.valueInt, -1);
        BcOp addOp = getOp(ifFalse, 4);
        REQUIRE_EQ(ret.ops[1], addOp);
        BcOp multiCallOp = getOp(ifFalse, 7);
        REQUIRE_EQ(ret.ops[2], multiCallOp);
    }
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

    /*
        // Block 1 (entry)
        GETVARARGS R1 2
        NEWTABLE R3 0 0
        GETVARARGS R4 -1
        SETLIST R3 R4 -1 [1]
        ADD R6 R0 R1
        ADD R5 R6 R2
        LOADK R7 K0 [1]
        GETTABLE R6 R3 R7
        ADD R4 R5 R6
        GETVARARGS R5 -1
        RETURN R4 -1
        // Block 2 (exit)
    */

    // CFG Blocks
    REQUIRE_EQ(fn->blocks.size(), 2);
    BcBlock& entry = fn->blockOp(fn->entryBlock);

    // Instructions
    REQUIRE(checkOps(
        *fn,
        entry.ops,
        {LOP_PREPVARARGS,
         LOP_GETVARARGS,
         LOP_NEWTABLE,
         LOP_GETVARARGS,
         LOP_SETLIST,
         LOP_ADD,
         LOP_ADD,
         LOP_LOADK,
         LOP_GETTABLE,
         LOP_ADD,
         LOP_GETVARARGS,
         LOP_RETURN}
    ));
    {
        BcInst& getVarArgs1 = fn->instOp(getOp(entry, 1));
        REQUIRE_EQ(getVarArgs1.ops.size(), 2);
        REQUIRE_EQ(getVarArgs1.ops[0].kind, BcOpKind::VmReg);
        REQUIRE_EQ(getVarArgs1.ops[0].index, 1);
        REQUIRE_EQ(getVarArgs1.ops[1].kind, BcOpKind::Imm);
        BcImm& getVarArgs1Count = fn->immOp(getVarArgs1.ops[1]);
        REQUIRE_EQ(getVarArgs1Count.kind, BcImmKind::Int);
        REQUIRE_EQ(getVarArgs1Count.valueInt, 2);

        BcOp getVarArgs2Op = getOp(entry, 3);
        BcInst& getVarArgs2 = fn->instOp(getVarArgs2Op);
        REQUIRE_EQ(getVarArgs2.ops.size(), 2);
        REQUIRE_EQ(getVarArgs2.ops[0].kind, BcOpKind::VmReg);
        REQUIRE_EQ(getVarArgs2.ops[0].index, 4);
        REQUIRE_EQ(getVarArgs2.ops[1].kind, BcOpKind::Imm);
        BcImm& getVarArgs2Count = fn->immOp(getVarArgs2.ops[1]);
        REQUIRE_EQ(getVarArgs2Count.kind, BcImmKind::Int);
        REQUIRE_EQ(getVarArgs2Count.valueInt, -1);

        BcInst& setList = fn->instOp(getOp(entry, 4));
        REQUIRE_EQ(setList.ops.size(), 4);
        BcImm& setListStartIdx = fn->immOp(setList.ops[0]);
        REQUIRE_EQ(setListStartIdx.kind, BcImmKind::Int);
        REQUIRE_EQ(setListStartIdx.valueInt, 1);
        BcImm& setListCount = fn->immOp(setList.ops[1]);
        REQUIRE_EQ(setListCount.kind, BcImmKind::Int);
        REQUIRE_EQ(setListCount.valueInt, -1);
        BcOp newTableOp = getOp(entry, 2);
        REQUIRE_EQ(setList.ops[2], newTableOp);
        REQUIRE_EQ(setList.ops[3], getVarArgs2Op);
    }
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

    /*
        // Block 1 (entry)
        DUPTABLE R1 2
        SETTABLEKS R0 R1 K0 ['a']
        MOVE R3 R0
        LOADN R4 42
        CONCAT R2 R3 R4
        SETTABLEKS R2 R1 K1 ['b']
        NEWTABLE R3 0 1
        MOVE R4 R1
        SETLIST R3 R4 1 [1]
        GETUPVAL R4 0
        FASTCALL2 52 R3 R4 L0
        GETIMPORT R2 5 [table.insert]
        CALL R2 2 -1
        L0: RETURN R2 -1
        // Block 2 (exit)
    */

    // CFG Blocks
    REQUIRE_EQ(fn->blocks.size(), 2);
    BcBlock& entry = fn->blockOp(fn->entryBlock);

    // Instructions
    REQUIRE(checkOps(
        *fn,
        entry.ops,
        {LOP_DUPTABLE,
         LOP_SETTABLEKS,
         LOP_MOVE,
         LOP_LOADN,
         LOP_CONCAT,
         LOP_SETTABLEKS,
         LOP_NEWTABLE,
         LOP_MOVE,
         LOP_SETLIST,
         LOP_GETUPVAL,
         LOP_FASTCALL2,
         LOP_GETIMPORT,
         LOP_CALL,
         LOP_RETURN}
    ));
    {
    }
}

std::string extractCode(std::string bytecode)
{
    size_t offset = 5;
    const char* data = bytecode.data();
    int32_t typeInfoSize = readVarInt(data, offset);
    offset += typeInfoSize;

    int32_t codesize = readVarInt(data, offset);
    return bytecode.substr(offset, codesize * sizeof(Instruction));
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
    for (int optLevel = 0; optLevel <= 2; optLevel++)
        for (auto& snippet : snippets)
        {
            auto bytecode = getFunctionBytecode(snippet, optLevel);
            REQUIRE(bytecode);
            std::vector<std::string_view> table;
            for (std::string& s : bytecode->second)
                table.push_back(s);
            std::optional<BcFunction> func = Bytecode::fromFunctionBytecode(bytecode->first, table);
            std::string orig = extractCode(bytecode->first);
            std::string dumped = extractCode(Bytecode::toFunctionBytecode(*func));
            REQUIRE_EQ(orig, dumped);
        }
}

TEST_SUITE_END();
