// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrBuilder.h"
#include "Luau/IrAnalysis.h"
#include "Luau/IrDump.h"
#include "Luau/OptimizeFinalX64.h"

#include "doctest.h"

using namespace Luau::CodeGen;

class IrBuilderFixture
{
public:
    IrBuilder build;
};

TEST_SUITE_BEGIN("Optimization");

TEST_CASE_FIXTURE(IrBuilderFixture, "FinalX64OptCheckTag")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);
    IrOp tag1 = build.inst(IrCmd::LOAD_TAG, build.vmReg(2));
    build.inst(IrCmd::CHECK_TAG, tag1, build.constTag(0), fallback);
    IrOp tag2 = build.inst(IrCmd::LOAD_TAG, build.vmConst(5));
    build.inst(IrCmd::CHECK_TAG, tag2, build.constTag(0), fallback);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    build.beginBlock(fallback);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    updateUseCounts(build.function);
    optimizeMemoryOperandsX64(build.function);

    // Load from memory is 'inlined' into CHECK_TAG
    CHECK("\n" + toString(build.function, /* includeDetails */ false) == R"(
bb_0:
   CHECK_TAG R2, tnil, bb_fallback_1
   CHECK_TAG K5, tnil, bb_fallback_1
   LOP_RETURN 0u

bb_fallback_1:
   LOP_RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FinalX64OptBinaryArith")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);
    IrOp opA = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    IrOp opB = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(2));
    build.inst(IrCmd::ADD_NUM, opA, opB);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    updateUseCounts(build.function);
    optimizeMemoryOperandsX64(build.function);

    // Load from memory is 'inlined' into second argument
    CHECK("\n" + toString(build.function, /* includeDetails */ false) == R"(
bb_0:
   %0 = LOAD_DOUBLE R1
   %2 = ADD_NUM %0, R2
   LOP_RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FinalX64OptEqTag1")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp trueBlock = build.block(IrBlockKind::Internal);
    IrOp falseBlock = build.block(IrBlockKind::Internal);

    build.beginBlock(block);
    IrOp opA = build.inst(IrCmd::LOAD_TAG, build.vmReg(1));
    IrOp opB = build.inst(IrCmd::LOAD_TAG, build.vmReg(2));
    build.inst(IrCmd::JUMP_EQ_TAG, opA, opB, trueBlock, falseBlock);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    build.beginBlock(trueBlock);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    updateUseCounts(build.function);
    optimizeMemoryOperandsX64(build.function);

    // Load from memory is 'inlined' into first argument
    CHECK("\n" + toString(build.function, /* includeDetails */ false) == R"(
bb_0:
   %1 = LOAD_TAG R2
   JUMP_EQ_TAG R1, %1, bb_1, bb_2

bb_1:
   LOP_RETURN 0u

bb_2:
   LOP_RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FinalX64OptEqTag2")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp trueBlock = build.block(IrBlockKind::Internal);
    IrOp falseBlock = build.block(IrBlockKind::Internal);

    build.beginBlock(block);
    IrOp opA = build.inst(IrCmd::LOAD_TAG, build.vmReg(1));
    IrOp opB = build.inst(IrCmd::LOAD_TAG, build.vmReg(2));
    build.inst(IrCmd::STORE_TAG, build.vmReg(6), opA);
    build.inst(IrCmd::JUMP_EQ_TAG, opA, opB, trueBlock, falseBlock);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    build.beginBlock(trueBlock);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    updateUseCounts(build.function);
    optimizeMemoryOperandsX64(build.function);

    // Load from memory is 'inlined' into second argument is it can't be done for the first one
    // We also swap first and second argument to generate memory access on the LHS
    CHECK("\n" + toString(build.function, /* includeDetails */ false) == R"(
bb_0:
   %0 = LOAD_TAG R1
   STORE_TAG R6, %0
   JUMP_EQ_TAG R2, %0, bb_1, bb_2

bb_1:
   LOP_RETURN 0u

bb_2:
   LOP_RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FinalX64OptEqTag3")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp trueBlock = build.block(IrBlockKind::Internal);
    IrOp falseBlock = build.block(IrBlockKind::Internal);

    build.beginBlock(block);
    IrOp table = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    IrOp arrElem = build.inst(IrCmd::GET_ARR_ADDR, table, build.constUint(0));
    IrOp opA = build.inst(IrCmd::LOAD_TAG, arrElem);
    build.inst(IrCmd::JUMP_EQ_TAG, opA, build.constTag(0), trueBlock, falseBlock);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    build.beginBlock(trueBlock);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    updateUseCounts(build.function);
    optimizeMemoryOperandsX64(build.function);

    // Load from memory is 'inlined' into first argument
    CHECK("\n" + toString(build.function, /* includeDetails */ false) == R"(
bb_0:
   %0 = LOAD_POINTER R1
   %1 = GET_ARR_ADDR %0, 0u
   %2 = LOAD_TAG %1
   JUMP_EQ_TAG %2, tnil, bb_1, bb_2

bb_1:
   LOP_RETURN 0u

bb_2:
   LOP_RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FinalX64OptJumpCmpNum")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp trueBlock = build.block(IrBlockKind::Internal);
    IrOp falseBlock = build.block(IrBlockKind::Internal);

    build.beginBlock(block);
    IrOp opA = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    IrOp opB = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(2));
    build.inst(IrCmd::JUMP_CMP_NUM, opA, opB, trueBlock, falseBlock);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    build.beginBlock(trueBlock);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::LOP_RETURN, build.constUint(0));

    updateUseCounts(build.function);
    optimizeMemoryOperandsX64(build.function);

    // Load from memory is 'inlined' into first argument
    CHECK("\n" + toString(build.function, /* includeDetails */ false) == R"(
bb_0:
   %1 = LOAD_DOUBLE R2
   JUMP_CMP_NUM R1, %1, bb_1, bb_2

bb_1:
   LOP_RETURN 0u

bb_2:
   LOP_RETURN 0u

)");
}

TEST_SUITE_END();
