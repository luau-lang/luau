// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Sccp.h"
#include "Luau/Bytecode.h"
#include "Luau/BytecodeGraph.h"

#include "doctest.h"

using namespace Luau;
using namespace Luau::Bytecode;

TEST_SUITE_BEGIN("Sccp");

TEST_CASE("sccp_does_not_fold_boolean_ordering")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.addBlock();
    BcOp bTrue = func.addBlock();
    BcOp bFalse = func.addBlock();
    BcOp exit = func.addBlock();

    func.entryBlock = entry;
    func.exitBlock = exit;

    // entry: LOADB R0 true
    BcOp immTrue = func.addImm(BcImmKind::Boolean);
    func.immediates[immTrue.index].valueBoolean = true;
    BcOp loadb = func.addInst();
    func.instructions[loadb.index].op = LOP_LOADB;
    func.instructions[loadb.index].block = entry;
    func.instructions[loadb.index].ops.push_back(immTrue);
    func.blockOp(entry).ops.push_back(loadb);
    func.regs[loadb] = 0;

    // entry: JUMPIFLT loadb loadb bTrue
    BcOp jflt = func.addInst();
    func.instructions[jflt.index].op = LOP_JUMPIFLT;
    func.instructions[jflt.index].block = entry;
    func.instructions[jflt.index].ops.push_back(loadb);
    func.instructions[jflt.index].ops.push_back(loadb);
    func.instructions[jflt.index].ops.push_back(bTrue);
    func.blockOp(entry).ops.push_back(jflt);
    func.blockOp(entry).successors.push_back({BcBlockEdgeKind::Branch, bTrue});
    func.blockOp(entry).successors.push_back({BcBlockEdgeKind::Fallthrough, bFalse});
    func.blockOp(bTrue).predecessors.push_back({BcBlockEdgeKind::Branch, entry});
    func.blockOp(bFalse).predecessors.push_back({BcBlockEdgeKind::Fallthrough, entry});

    // bTrue: RETURN R0 1
    BcOp ret1 = func.addInst();
    func.instructions[ret1.index].op = LOP_RETURN;
    func.instructions[ret1.index].block = bTrue;
    func.instructions[ret1.index].ops.push_back(BcOp{BcOpKind::VmReg, 0});
    func.instructions[ret1.index].ops.push_back(BcOp{BcOpKind::Imm, 1});
    func.blockOp(bTrue).ops.push_back(ret1);
    func.blockOp(bTrue).successors.push_back({BcBlockEdgeKind::Fallthrough, exit});
    func.blockOp(exit).predecessors.push_back({BcBlockEdgeKind::Fallthrough, bTrue});

    // bFalse: RETURN R0 1
    BcOp ret2 = func.addInst();
    func.instructions[ret2.index].op = LOP_RETURN;
    func.instructions[ret2.index].block = bFalse;
    func.instructions[ret2.index].ops.push_back(BcOp{BcOpKind::VmReg, 0});
    func.instructions[ret2.index].ops.push_back(BcOp{BcOpKind::Imm, 1});
    func.blockOp(bFalse).ops.push_back(ret2);
    func.blockOp(bFalse).successors.push_back({BcBlockEdgeKind::Fallthrough, exit});
    func.blockOp(exit).predecessors.push_back({BcBlockEdgeKind::Fallthrough, bFalse});

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    // Both branches should remain reachable because JUMPIFLT on boolean must not fold
    CHECK((func.blockOp(bTrue).flags & BcBlockFlag::Dead) == 0);
    CHECK((func.blockOp(bFalse).flags & BcBlockFlag::Dead) == 0);
}

TEST_CASE("sccp_folds_number_ordering")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.addBlock();
    BcOp bTrue = func.addBlock();
    BcOp bFalse = func.addBlock();
    BcOp exit = func.addBlock();

    func.entryBlock = entry;
    func.exitBlock = exit;

    // entry: LOADN R0 1, LOADN R1 2
    BcOp imm1 = func.addImm(BcImmKind::Int);
    func.immediates[imm1.index].valueInt = 1;
    BcOp load1 = func.addInst();
    func.instructions[load1.index].op = LOP_LOADN;
    func.instructions[load1.index].block = entry;
    func.instructions[load1.index].ops.push_back(imm1);
    func.blockOp(entry).ops.push_back(load1);
    func.regs[load1] = 0;

    BcOp imm2 = func.addImm(BcImmKind::Int);
    func.immediates[imm2.index].valueInt = 2;
    BcOp load2 = func.addInst();
    func.instructions[load2.index].op = LOP_LOADN;
    func.instructions[load2.index].block = entry;
    func.instructions[load2.index].ops.push_back(imm2);
    func.blockOp(entry).ops.push_back(load2);
    func.regs[load2] = 1;

    // entry: JUMPIFLT load1 load2 bTrue  (1 < 2 is true)
    BcOp jflt = func.addInst();
    func.instructions[jflt.index].op = LOP_JUMPIFLT;
    func.instructions[jflt.index].block = entry;
    func.instructions[jflt.index].ops.push_back(load1);
    func.instructions[jflt.index].ops.push_back(load2);
    func.instructions[jflt.index].ops.push_back(bTrue);
    func.blockOp(entry).ops.push_back(jflt);
    func.blockOp(entry).successors.push_back({BcBlockEdgeKind::Branch, bTrue});
    func.blockOp(entry).successors.push_back({BcBlockEdgeKind::Fallthrough, bFalse});
    func.blockOp(bTrue).predecessors.push_back({BcBlockEdgeKind::Branch, entry});
    func.blockOp(bFalse).predecessors.push_back({BcBlockEdgeKind::Fallthrough, entry});

    // bTrue: RETURN R0 1
    BcOp ret1 = func.addInst();
    func.instructions[ret1.index].op = LOP_RETURN;
    func.instructions[ret1.index].block = bTrue;
    func.instructions[ret1.index].ops.push_back(BcOp{BcOpKind::VmReg, 0});
    func.instructions[ret1.index].ops.push_back(BcOp{BcOpKind::Imm, 1});
    func.blockOp(bTrue).ops.push_back(ret1);
    func.blockOp(bTrue).successors.push_back({BcBlockEdgeKind::Fallthrough, exit});
    func.blockOp(exit).predecessors.push_back({BcBlockEdgeKind::Fallthrough, bTrue});

    // bFalse: RETURN R0 1
    BcOp ret2 = func.addInst();
    func.instructions[ret2.index].op = LOP_RETURN;
    func.instructions[ret2.index].block = bFalse;
    func.instructions[ret2.index].ops.push_back(BcOp{BcOpKind::VmReg, 0});
    func.instructions[ret2.index].ops.push_back(BcOp{BcOpKind::Imm, 1});
    func.blockOp(bFalse).ops.push_back(ret2);
    func.blockOp(bFalse).successors.push_back({BcBlockEdgeKind::Fallthrough, exit});
    func.blockOp(exit).predecessors.push_back({BcBlockEdgeKind::Fallthrough, bFalse});

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    // 1 < 2 is always true, so bTrue should be live and bFalse dead
    CHECK((func.blockOp(bTrue).flags & BcBlockFlag::Dead) == 0);
    CHECK((func.blockOp(bFalse).flags & BcBlockFlag::Dead) != 0);
}

TEST_CASE("sccp_phi_filters_dead_predecessor")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.addBlock();
    BcOp bTrue = func.addBlock();
    BcOp bFalse = func.addBlock();
    BcOp merge = func.addBlock();
    BcOp exit = func.addBlock();

    func.entryBlock = entry;
    func.exitBlock = exit;

    // entry: LOADB R0 true
    BcOp immTrue = func.addImm(BcImmKind::Boolean);
    func.immediates[immTrue.index].valueBoolean = true;
    BcOp loadb = func.addInst();
    func.instructions[loadb.index].op = LOP_LOADB;
    func.instructions[loadb.index].block = entry;
    func.instructions[loadb.index].ops.push_back(immTrue);
    func.blockOp(entry).ops.push_back(loadb);
    func.regs[loadb] = 0;

    // entry: JUMPIF loadb bTrue  (always true, so bFalse fallthrough is dead)
    BcOp jif = func.addInst();
    func.instructions[jif.index].op = LOP_JUMPIF;
    func.instructions[jif.index].block = entry;
    func.instructions[jif.index].ops.push_back(loadb);
    func.instructions[jif.index].ops.push_back(bTrue);
    func.blockOp(entry).ops.push_back(jif);
    func.blockOp(entry).successors.push_back({BcBlockEdgeKind::Branch, bTrue});
    func.blockOp(entry).successors.push_back({BcBlockEdgeKind::Fallthrough, bFalse});
    func.blockOp(bTrue).predecessors.push_back({BcBlockEdgeKind::Branch, entry});
    func.blockOp(bFalse).predecessors.push_back({BcBlockEdgeKind::Fallthrough, entry});

    // bTrue: LOADK R1 const 42
    BcVmConst c42;
    c42.kind = BcVmConstKind::Number;
    c42.valueNumber = 42.0;
    BcOp const42 = func.addConst(c42);
    BcOp loadk1 = func.addInst();
    func.instructions[loadk1.index].op = LOP_LOADK;
    func.instructions[loadk1.index].block = bTrue;
    func.instructions[loadk1.index].ops.push_back(const42);
    func.blockOp(bTrue).ops.push_back(loadk1);
    func.regs[loadk1] = 1;

    // bFalse: LOADK R1 const 99
    BcVmConst c99;
    c99.kind = BcVmConstKind::Number;
    c99.valueNumber = 99.0;
    BcOp const99 = func.addConst(c99);
    BcOp loadk2 = func.addInst();
    func.instructions[loadk2.index].op = LOP_LOADK;
    func.instructions[loadk2.index].block = bFalse;
    func.instructions[loadk2.index].ops.push_back(const99);
    func.blockOp(bFalse).ops.push_back(loadk2);
    func.regs[loadk2] = 1;

    // bTrue -> merge, bFalse -> merge
    func.blockOp(bTrue).successors.push_back({BcBlockEdgeKind::Fallthrough, merge});
    func.blockOp(bFalse).successors.push_back({BcBlockEdgeKind::Fallthrough, merge});
    func.blockOp(merge).predecessors.push_back({BcBlockEdgeKind::Fallthrough, bTrue});
    func.blockOp(merge).predecessors.push_back({BcBlockEdgeKind::Fallthrough, bFalse});

    // merge: phi(R1) = {loadk1, loadk2}
    BcOp phi0 = func.addPhi();
    func.phis[phi0.index].ops.push_back(loadk1);
    func.phis[phi0.index].ops.push_back(loadk2);
    func.blockOp(merge).phis.push_back(phi0);
    func.regs[phi0] = 1;

    // merge: RETURN R1 1
    BcOp ret = func.addInst();
    func.instructions[ret.index].op = LOP_RETURN;
    func.instructions[ret.index].block = merge;
    func.instructions[ret.index].ops.push_back(phi0);
    func.instructions[ret.index].ops.push_back(BcOp{BcOpKind::Imm, 1});
    func.blockOp(merge).ops.push_back(ret);
    func.blockOp(merge).successors.push_back({BcBlockEdgeKind::Fallthrough, exit});
    func.blockOp(exit).predecessors.push_back({BcBlockEdgeKind::Fallthrough, merge});

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();

    // bFalse is dead because the condition is always true.
    // The phi operand from bFalse stays Undetermined (dead block never visited),
    // so the phi resolves to the live operand's value (42).
    auto it = sccp.state.opConstness.find(phi0);
    REQUIRE(it != nullptr);
    CHECK(it->kind == Constness::VmConstant);
    CHECK(func.constOp(it->vmConst.value()).valueNumber == 42.0);

    sccp.rewrite();

    CHECK((func.blockOp(bFalse).flags & BcBlockFlag::Dead) != 0);
}

TEST_CASE("sccp_erases_trivial_phi")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.addBlock();
    BcOp exit = func.addBlock();

    func.entryBlock = entry;
    func.exitBlock = exit;

    // entry: LOADK R0 const 42
    BcVmConst c42;
    c42.kind = BcVmConstKind::Number;
    c42.valueNumber = 42.0;
    BcOp const42 = func.addConst(c42);
    BcOp loadk = func.addInst();
    func.instructions[loadk.index].op = LOP_LOADK;
    func.instructions[loadk.index].block = entry;
    func.instructions[loadk.index].ops.push_back(const42);
    func.blockOp(entry).ops.push_back(loadk);
    func.regs[loadk] = 0;

    // entry -> exit
    func.blockOp(entry).successors.push_back({BcBlockEdgeKind::Fallthrough, exit});
    func.blockOp(exit).predecessors.push_back({BcBlockEdgeKind::Fallthrough, entry});

    // exit: phi(R0) = {loadk, loadk}
    BcOp phi0 = func.addPhi();
    func.phis[phi0.index].ops.push_back(loadk);
    func.phis[phi0.index].ops.push_back(loadk);
    func.blockOp(exit).phis.push_back(phi0);
    func.regs[phi0] = 0;

    // exit: RETURN phi0 1
    BcOp ret = func.addInst();
    func.instructions[ret.index].op = LOP_RETURN;
    func.instructions[ret.index].block = exit;
    func.instructions[ret.index].ops.push_back(phi0);
    func.instructions[ret.index].ops.push_back(BcOp{BcOpKind::Imm, 1});
    func.blockOp(exit).ops.push_back(ret);

    BcVmConstImpl impl(func);
    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    // After simplifyPhis(), the trivial phi should be removed from exit block
    CHECK(func.blockOp(exit).phis.empty());
}

TEST_CASE("sccp_loadk_mul_to_mulk")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.addBlock();
    BcOp exit = func.addBlock();

    func.entryBlock = entry;
    func.exitBlock = exit;

    BcVmConst c42;
    c42.kind = BcVmConstKind::Number;
    c42.valueNumber = 42.0;
    BcOp const42 = func.addConst(c42);
    BcOp loadk = func.addInst();
    func.instructions[loadk.index].op = LOP_LOADK;
    func.instructions[loadk.index].block = entry;
    func.instructions[loadk.index].ops.push_back(const42);
    func.blockOp(entry).ops.push_back(loadk);
    func.regs[loadk] = 0;

    BcOp upval = func.addInst();
    func.instructions[upval.index].op = LOP_GETUPVAL;
    func.instructions[upval.index].block = entry;
    func.instructions[upval.index].ops.push_back(BcOp{BcOpKind::Imm, 0});
    func.blockOp(entry).ops.push_back(upval);
    func.regs[upval] = 1;

    BcOp mul = func.addInst();
    func.instructions[mul.index].op = LOP_MUL;
    func.instructions[mul.index].block = entry;
    func.instructions[mul.index].ops.push_back(loadk);
    func.instructions[mul.index].ops.push_back(upval);
    func.blockOp(entry).ops.push_back(mul);
    func.regs[mul] = 2;

    BcOp ret = func.addInst();
    func.instructions[ret.index].op = LOP_RETURN;
    func.instructions[ret.index].block = entry;
    func.instructions[ret.index].ops.push_back(mul);
    func.instructions[ret.index].ops.push_back(BcOp{BcOpKind::Imm, 1});
    func.blockOp(entry).ops.push_back(ret);
    func.blockOp(entry).successors.push_back({BcBlockEdgeKind::Fallthrough, exit});
    func.blockOp(exit).predecessors.push_back({BcBlockEdgeKind::Fallthrough, entry});

    BcVmConstImpl impl(func);

    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    CHECK(func.blockOp(entry).ops.size() == 3);
    CHECK(func.instructions[mul.index].op == LOP_MULK);
}

TEST_CASE("sccp_loadk_div_to_divrk")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.addBlock();
    BcOp exit = func.addBlock();

    func.entryBlock = entry;
    func.exitBlock = exit;

    BcVmConst c42;
    c42.kind = BcVmConstKind::Number;
    c42.valueNumber = 42.0;
    BcOp const42 = func.addConst(c42);
    BcOp loadk = func.addInst();
    func.instructions[loadk.index].op = LOP_LOADK;
    func.instructions[loadk.index].block = entry;
    func.instructions[loadk.index].ops.push_back(const42);
    func.blockOp(entry).ops.push_back(loadk);
    func.regs[loadk] = 0;

    BcOp upval = func.addInst();
    func.instructions[upval.index].op = LOP_GETUPVAL;
    func.instructions[upval.index].block = entry;
    func.instructions[upval.index].ops.push_back(BcOp{BcOpKind::Imm, 0});
    func.blockOp(entry).ops.push_back(upval);
    func.regs[upval] = 1;

    BcOp div = func.addInst();
    func.instructions[div.index].op = LOP_DIV;
    func.instructions[div.index].block = entry;
    func.instructions[div.index].ops.push_back(loadk);
    func.instructions[div.index].ops.push_back(upval);
    func.blockOp(entry).ops.push_back(div);
    func.regs[div] = 2;

    BcOp ret = func.addInst();
    func.instructions[ret.index].op = LOP_RETURN;
    func.instructions[ret.index].block = entry;
    func.instructions[ret.index].ops.push_back(div);
    func.instructions[ret.index].ops.push_back(BcOp{BcOpKind::Imm, 1});
    func.blockOp(entry).ops.push_back(ret);
    func.blockOp(entry).successors.push_back({BcBlockEdgeKind::Fallthrough, exit});
    func.blockOp(exit).predecessors.push_back({BcBlockEdgeKind::Fallthrough, entry});

    BcVmConstImpl impl(func);

    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    CHECK(func.blockOp(entry).ops.size() == 3);
    CHECK(func.instructions[div.index].op == LOP_DIVRK);
    CHECK(func.vmConst(func.instructions[div.index].ops[0])->kind == BcVmConstKind::Number);
    CHECK(func.vmConst(func.instructions[div.index].ops[0])->valueNumber == 42.0);
}

TEST_CASE("sccp_loadk_mul_to_zero")
{
    BcFunction<BcVmConst> func;
    BcOp entry = func.addBlock();
    BcOp exit = func.addBlock();

    func.entryBlock = entry;
    func.exitBlock = exit;

    BcVmConst c42;
    c42.kind = BcVmConstKind::Number;
    c42.valueNumber = 0;
    BcOp const42 = func.addConst(c42);
    BcOp loadk = func.addInst();
    func.instructions[loadk.index].op = LOP_LOADK;
    func.instructions[loadk.index].block = entry;
    func.instructions[loadk.index].ops.push_back(const42);
    func.blockOp(entry).ops.push_back(loadk);
    func.regs[loadk] = 0;

    BcOp upval = func.addInst();
    func.instructions[upval.index].op = LOP_GETUPVAL;
    func.instructions[upval.index].block = entry;
    func.instructions[upval.index].ops.push_back(BcOp{BcOpKind::Imm, 0});
    func.blockOp(entry).ops.push_back(upval);
    func.regs[upval] = 1;

    BcOp mul = func.addInst();
    func.instructions[mul.index].op = LOP_MUL;
    func.instructions[mul.index].block = entry;
    func.instructions[mul.index].ops.push_back(loadk);
    func.instructions[mul.index].ops.push_back(upval);
    func.blockOp(entry).ops.push_back(mul);
    func.regs[mul] = 2;

    BcOp ret = func.addInst();
    func.instructions[ret.index].op = LOP_RETURN;
    func.instructions[ret.index].block = entry;
    func.instructions[ret.index].ops.push_back(mul);
    func.instructions[ret.index].ops.push_back(BcOp{BcOpKind::Imm, 1});
    func.blockOp(entry).ops.push_back(ret);
    func.blockOp(entry).successors.push_back({BcBlockEdgeKind::Fallthrough, exit});
    func.blockOp(exit).predecessors.push_back({BcBlockEdgeKind::Fallthrough, entry});

    BcVmConstImpl impl(func);

    Sccp<BcVmConst> sccp(func, &impl);
    sccp.propagate();
    sccp.rewrite();

    CHECK(func.blockOp(entry).ops.size() == 3);
    CHECK(func.instructions[mul.index].op == LOP_LOADN);
    CHECK(func.immOp(func.instructions[mul.index].ops[0]).valueInt == 0);
}

TEST_SUITE_END();
