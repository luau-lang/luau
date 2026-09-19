// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Sccp.h"
#include "Luau/Bytecode.h"
#include "Luau/BytecodeDump.h"
#include "Luau/BytecodeGraph.h"

#include "doctest.h"

using namespace Luau;
using namespace Luau::Bytecode;

TEST_SUITE_BEGIN("Sccp");

TEST_CASE("sccp_does_not_fold_boolean_ordering")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.entryBlock = func.addBlock();
    BcOp bTrue = func.addBlock();
    BcOp bFalse = func.addBlock();
    BcOp exit = func.exitBlock = func.addBlock();

    func.entryBlock = entry;
    func.exitBlock = exit;

    BcOp loadb = func.addInst(LOP_LOADB, entry, {func.addImmBool(true)}, Reg(0));
    func.addInst(LOP_JUMPIFLT, entry, {loadb, loadb, bTrue});
    func.addEdge(entry, bTrue, BcBlockEdgeKind::Branch);
    func.addEdge(entry, bFalse, BcBlockEdgeKind::Fallthrough);

    func.addInst(LOP_RETURN, bTrue, {func.addImmInt(1), loadb});
    func.addEdge(bTrue, exit, BcBlockEdgeKind::Fallthrough);

    func.addInst(LOP_RETURN, bFalse, {func.addImmInt(1), loadb});
    func.addEdge(bFalse, exit, BcBlockEdgeKind::Fallthrough);

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    // Both branches should remain reachable because JUMPIFLT on boolean must not fold
    CHECK_EQ(
        "\n" + toString(func, true),
        R"(
; function() maxstacksize: 0 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_1 [branch], bb_2 [fallthrough]
  %0 = LOADB true                                            ; uses: %1, %1, %2, %3
  %1 = JUMPIFLT %0, %0, bb_1

bb_1:
; predecessors: bb_0 [branch]
; successors: bb_3 [fallthrough]
  %2 = RETURN 1, %0

bb_2:
; predecessors: bb_0 [fallthrough]
; successors: bb_3 [fallthrough]
  %3 = RETURN 1, %0

bb_3 (exit):
; predecessors: bb_1 [fallthrough], bb_2 [fallthrough]
)"
    );
}

TEST_CASE("sccp_folds_number_ordering")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.entryBlock = func.addBlock();
    BcOp bTrue = func.addBlock();
    BcOp bFalse = func.addBlock();
    BcOp exit = func.exitBlock = func.addBlock();

    func.entryBlock = entry;
    func.exitBlock = exit;

    BcOp lhs = func.addInst(LOP_LOADN, entry, {func.addImmInt(1)}, Reg(0));
    BcOp rhs = func.addInst(LOP_LOADN, entry, {func.addImmInt(2)}, Reg(1));
    func.addInst(LOP_JUMPIFLT, entry, {lhs, rhs, bTrue});
    func.addEdge(entry, bTrue, BcBlockEdgeKind::Branch);
    func.addEdge(entry, bFalse, BcBlockEdgeKind::Fallthrough);

    func.addInst(LOP_RETURN, bTrue, {func.addImmInt(1), lhs});
    func.addEdge(bTrue, exit, BcBlockEdgeKind::Fallthrough);

    func.addInst(LOP_RETURN, bFalse, {func.addImmInt(1), rhs});
    func.addEdge(bFalse, exit, BcBlockEdgeKind::Fallthrough);

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    // 1 < 2 is always true, so bTrue should be live and bFalse dead
    CHECK_EQ(
        "\n" + toString(func, true),
        R"(
; function() maxstacksize: 0 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %0 = LOADN 1                                               ; uses: %3
  %1 = LOADN 2                                               ; uses: %4

bb_1:
; predecessors: bb_0 [branch]
; successors: bb_3 [fallthrough]
  %3 = RETURN 1, %0

bb_3 (exit):
; predecessors: bb_1 [fallthrough], bb_2 [fallthrough]
)"
    );
}

TEST_CASE("sccp_phi_filters_dead_predecessor")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.entryBlock = func.addBlock();
    BcOp bTrue = func.addBlock();
    BcOp bFalse = func.addBlock();
    BcOp merge = func.addBlock();
    BcOp exit = func.exitBlock = func.addBlock();

    func.entryBlock = entry;
    func.exitBlock = exit;

    BcOp condition = func.addInst(LOP_LOADB, entry, {func.addImmBool(true)}, Reg(0));
    func.addInst(LOP_JUMPIF, entry, {condition, bTrue});
    func.addEdge(entry, bTrue, BcBlockEdgeKind::Branch);
    func.addEdge(entry, bFalse, BcBlockEdgeKind::Fallthrough);

    BcOp trueValue = func.addInst(LOP_LOADK, bTrue, {addVmConstNumber(func, 42.0)}, Reg(1));
    func.addEdge(bTrue, merge, BcBlockEdgeKind::Fallthrough);

    BcOp falseValue = func.addInst(LOP_LOADK, bFalse, {addVmConstNumber(func, 99.0)}, Reg(1));
    func.addEdge(bFalse, merge, BcBlockEdgeKind::Fallthrough);

    BcOp phi0 = func.addPhi(merge, {trueValue, falseValue}, Reg(1));
    func.addInst(LOP_RETURN, merge, {func.addImmInt(1), phi0});
    func.addEdge(merge, exit, BcBlockEdgeKind::Fallthrough);

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();

    // bFalse is dead because the condition is always true.
    // The phi operand from bFalse stays Undetermined (dead block never visited),
    // so the phi resolves to the live operand's value (42).
    auto it = sccp.state.opConstness.find(phi0);
    REQUIRE(it != nullptr);
    CHECK(it->kind == Constness::Constant);
    CHECK(impl.asNumber(it->constant.value()) == 42.0);

    sccp.rewrite();

    // TODO: why did phi retain unreacable block in it?
    CHECK_EQ(
        "\n" + toString(func, true),
        R"(
; function() maxstacksize: 0 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %0 = LOADB true

bb_1:
; predecessors: bb_0 [branch]
; successors: bb_3 [fallthrough]
  %2 = LOADK K0 (42)                                         ; uses: phi.0

bb_3:
; predecessors: bb_1 [fallthrough], bb_2 [fallthrough]
; successors: bb_4 [fallthrough]
  phi.0 = %2 from bb_1, %3 from bb_2                         ; uses: %4
  %4 = RETURN 1, phi.0

bb_4 (exit):
; predecessors: bb_3 [fallthrough]
)"
    );
}

TEST_CASE("sccp_erases_trivial_phi")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.entryBlock = func.addBlock();
    BcOp exit = func.exitBlock = func.addBlock();

    BcOp value = func.addInst(LOP_LOADK, entry, {addVmConstNumber(func, 42.0)}, Reg(0));
    func.addEdge(entry, exit, BcBlockEdgeKind::Fallthrough);

    BcOp phi = func.addPhi(exit, {value, value}, Reg(0));
    func.addInst(LOP_RETURN, exit, {func.addImmInt(1), phi});

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    CHECK_EQ(
        "\n" + toString(func, true),
        R"(
; function() maxstacksize: 0 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %0 = LOADK K0 (42)                                         ; uses: %1

bb_1 (exit):
; predecessors: bb_0 [fallthrough]
  %1 = RETURN 1, %0
)"
    );
}

TEST_CASE("sccp_loadk_mul_to_mulk")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.entryBlock = func.addBlock();
    BcOp exit = func.exitBlock = func.addBlock();

    BcOp constant = func.addInst(LOP_LOADK, entry, {addVmConstNumber(func, 42.0)}, Reg(0));
    BcOp upvalue = func.addInst(LOP_GETUPVAL, entry, {func.addVmUpvalue(0)}, Reg(1));
    BcOp result = func.addInst(LOP_MUL, entry, {constant, upvalue}, Reg(2));
    func.addInst(LOP_RETURN, entry, {func.addImmInt(1), result});
    func.addEdge(entry, exit, BcBlockEdgeKind::Fallthrough);

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    CHECK_EQ(
        "\n" + toString(func, true),
        R"(
; function() maxstacksize: 0 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %1 = GETUPVAL U0                                           ; uses: %2
  %2 = MULK %1, K0 (42)                                      ; uses: %3
  %3 = RETURN 1, %2

bb_1 (exit):
; predecessors: bb_0 [fallthrough]
)"
    );
}

TEST_CASE("sccp_loadn_does_not_promote_to_k")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.entryBlock = func.addBlock();
    BcOp exit = func.exitBlock = func.addBlock();

    BcOp constant = func.addInst(LOP_LOADN, entry, {func.addImmInt(2)}, Reg(0));
    BcOp upvalue = func.addInst(LOP_GETUPVAL, entry, {func.addVmUpvalue(0)}, Reg(1));
    BcOp result = func.addInst(LOP_MUL, entry, {upvalue, constant}, Reg(2));
    func.addInst(LOP_RETURN, entry, {func.addImmInt(1), result});
    func.addEdge(entry, exit, BcBlockEdgeKind::Fallthrough);

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    CHECK_EQ(
        "\n" + toString(func, true),
        R"(
; function() maxstacksize: 0 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %0 = LOADN 2                                               ; uses: %2
  %1 = GETUPVAL U0                                           ; uses: %2
  %2 = MUL %1, %0                                            ; uses: %3
  %3 = RETURN 1, %2

bb_1 (exit):
; predecessors: bb_0 [fallthrough]
)"
    );
}

TEST_CASE("sccp_loadk_div_to_divrk")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.entryBlock = func.addBlock();
    BcOp exit = func.exitBlock = func.addBlock();

    BcOp constant = func.addInst(LOP_LOADK, entry, {addVmConstNumber(func, 42.0)}, Reg(0));
    BcOp upvalue = func.addInst(LOP_GETUPVAL, entry, {func.addVmUpvalue(0)}, Reg(1));
    BcOp result = func.addInst(LOP_DIV, entry, {constant, upvalue}, Reg(2));
    func.addInst(LOP_RETURN, entry, {func.addImmInt(1), result});
    func.addEdge(entry, exit, BcBlockEdgeKind::Fallthrough);

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    CHECK_EQ(
        "\n" + toString(func, true),
        R"(
; function() maxstacksize: 0 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %1 = GETUPVAL U0                                           ; uses: %2
  %2 = DIVRK K0 (42), %1                                     ; uses: %3
  %3 = RETURN 1, %2

bb_1 (exit):
; predecessors: bb_0 [fallthrough]
)"
    );
}

TEST_CASE("sccp_immediate_floor_division_toward_zero")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.entryBlock = func.addBlock();
    BcOp exit = func.exitBlock = func.addBlock();

    BcOp lhs = func.addInst(LOP_LOADN, entry, {func.addImmInt(1)}, Reg(0));
    BcOp rhs = func.addInst(LOP_LOADN, entry, {func.addImmInt(-2)}, Reg(1));
    BcOp result = func.addInst(LOP_IDIV, entry, {lhs, rhs}, Reg(2));
    func.addInst(LOP_RETURN, entry, {func.addImmInt(1), result});
    func.addEdge(entry, exit, BcBlockEdgeKind::Fallthrough);

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    CHECK_EQ(
        "\n" + toString(func, true),
        R"(
; function() maxstacksize: 0 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %0 = LOADN 1
  %1 = LOADN -2
  %2 = LOADK K0 (-1)                                         ; uses: %3
  %3 = RETURN 1, %2

bb_1 (exit):
; predecessors: bb_0 [fallthrough]
)"
    );
}

TEST_CASE("sccp_nan_compare_check1")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.entryBlock = func.addBlock();
    BcOp bTrue = func.addBlock();
    BcOp bFalse = func.addBlock();
    BcOp exit = func.exitBlock = func.addBlock();

    BcOp nan = func.addInst(LOP_LOADK, entry, {addVmConstNumber(func, std::numeric_limits<double>::quiet_NaN())}, Reg(0));
    func.addInst(LOP_JUMPIFEQ, entry, {nan, nan, bTrue});
    func.addEdge(entry, bTrue, BcBlockEdgeKind::Branch);
    func.addEdge(entry, bFalse, BcBlockEdgeKind::Fallthrough);

    func.addInst(LOP_RETURN, bTrue, {func.addImmInt(1), addVmConstNumber(func, 1.0)});
    func.addEdge(bTrue, exit, BcBlockEdgeKind::Fallthrough);

    func.addInst(LOP_RETURN, bFalse, {func.addImmInt(1), addVmConstNumber(func, 0.0)});
    func.addEdge(bFalse, exit, BcBlockEdgeKind::Fallthrough);

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    // NaN == NaN is false
    CHECK_EQ(
        "\n" + toString(func, true),
        R"(
; function() maxstacksize: 0 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_2 [fallthrough]
  %0 = LOADK K0 (nan)

bb_2:
; predecessors: bb_0 [fallthrough]
; successors: bb_3 [fallthrough]
  %3 = RETURN 1, K2 (0)

bb_3 (exit):
; predecessors: bb_1 [fallthrough], bb_2 [fallthrough]
)"
    );
}

TEST_CASE("sccp_nan_compare_check2")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.entryBlock = func.addBlock();
    BcOp bTrue = func.addBlock();
    BcOp bFalse = func.addBlock();
    BcOp exit = func.exitBlock = func.addBlock();

    BcOp nan = func.addInst(LOP_LOADK, entry, {addVmConstNumber(func, std::numeric_limits<double>::quiet_NaN())}, Reg(0));
    func.addInst(LOP_JUMPIFLE, entry, {nan, nan, bTrue});
    func.addEdge(entry, bTrue, BcBlockEdgeKind::Branch);
    func.addEdge(entry, bFalse, BcBlockEdgeKind::Fallthrough);

    func.addInst(LOP_RETURN, bTrue, {func.addImmInt(1), addVmConstNumber(func, 1.0)});
    func.addEdge(bTrue, exit, BcBlockEdgeKind::Fallthrough);

    func.addInst(LOP_RETURN, bFalse, {func.addImmInt(1), addVmConstNumber(func, 0.0)});
    func.addEdge(bFalse, exit, BcBlockEdgeKind::Fallthrough);

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    // NaN <= NaN is false
    CHECK_EQ(
        "\n" + toString(func, true),
        R"(
; function() maxstacksize: 0 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_2 [fallthrough]
  %0 = LOADK K0 (nan)

bb_2:
; predecessors: bb_0 [fallthrough]
; successors: bb_3 [fallthrough]
  %3 = RETURN 1, K2 (0)

bb_3 (exit):
; predecessors: bb_1 [fallthrough], bb_2 [fallthrough]
)"
    );
}

TEST_CASE("sccp_nan_compare_check3")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.entryBlock = func.addBlock();
    BcOp bTrue = func.addBlock();
    BcOp bFalse = func.addBlock();
    BcOp exit = func.exitBlock = func.addBlock();

    BcOp one = func.addInst(LOP_LOADK, entry, {addVmConstNumber(func, 1.0)}, Reg(0));
    BcOp nan = func.addInst(LOP_LOADK, entry, {addVmConstNumber(func, std::numeric_limits<double>::quiet_NaN())}, Reg(1));
    func.addInst(LOP_JUMPIFNOTLE, entry, {one, nan, bTrue});
    func.addEdge(entry, bTrue, BcBlockEdgeKind::Branch);
    func.addEdge(entry, bFalse, BcBlockEdgeKind::Fallthrough);

    func.addInst(LOP_RETURN, bTrue, {func.addImmInt(1), addVmConstNumber(func, 1.0)});
    func.addEdge(bTrue, exit, BcBlockEdgeKind::Fallthrough);

    func.addInst(LOP_RETURN, bFalse, {func.addImmInt(1), addVmConstNumber(func, 0.0)});
    func.addEdge(bFalse, exit, BcBlockEdgeKind::Fallthrough);

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    // !(1 < NaN) is true
    CHECK_EQ(
        "\n" + toString(func, true),
        R"(
; function() maxstacksize: 0 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %0 = LOADK K0 (1)
  %1 = LOADK K1 (nan)

bb_1:
; predecessors: bb_0 [branch]
; successors: bb_3 [fallthrough]
  %3 = RETURN 1, K2 (1)

bb_3 (exit):
; predecessors: bb_1 [fallthrough], bb_2 [fallthrough]
)"
    );
}

TEST_CASE("sccp_loadn_plus_loadk_fold")
{
    BcFunction<BcVmConst> func{};
    BcOp entry = func.entryBlock = func.addBlock();
    BcOp exit = func.exitBlock = func.addBlock();

    BcOp lhs = func.addInst(LOP_LOADN, entry, {func.addImmInt(1)}, Reg(0));
    BcOp rhs = func.addInst(LOP_LOADK, entry, {addVmConstNumber(func, 2.0)}, Reg(1));
    BcOp result = func.addInst(LOP_ADD, entry, {lhs, rhs}, Reg(2));
    func.addInst(LOP_RETURN, entry, {func.addImmInt(1), result});
    func.addEdge(entry, exit, BcBlockEdgeKind::Fallthrough);

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    CHECK_EQ(
        "\n" + toString(func, true),
        R"(
; function() maxstacksize: 0 upvalues: 0 flags: 0
bb_0 (entry):
; successors: bb_1 [fallthrough]
  %0 = LOADN 1
  %1 = LOADK K0 (2)
  %2 = LOADK K1 (3)                                          ; uses: %3
  %3 = RETURN 1, %2

bb_1 (exit):
; predecessors: bb_0 [fallthrough]
)"
    );
}

TEST_SUITE_END();
