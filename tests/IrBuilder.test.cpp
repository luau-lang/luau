// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrBuilder.h"
#include "Luau/IrAnalysis.h"
#include "Luau/IrDump.h"
#include "Luau/IrUtils.h"
#include "Luau/OptimizeConstProp.h"
#include "Luau/OptimizeFinalX64.h"

#include "doctest.h"

#include <limits.h>

using namespace Luau::CodeGen;

class IrBuilderFixture
{
public:
    void constantFold()
    {
        for (IrBlock& block : build.function.blocks)
        {
            if (block.kind == IrBlockKind::Dead)
                continue;

            for (size_t i = block.start; i <= block.finish; i++)
            {
                IrInst& inst = build.function.instructions[i];

                applySubstitutions(build.function, inst);
                foldConstants(build, build.function, block, uint32_t(i));
            }
        }
    }

    template<typename F>
    void withOneBlock(F&& f)
    {
        IrOp main = build.block(IrBlockKind::Internal);
        IrOp a = build.block(IrBlockKind::Internal);

        build.beginBlock(main);
        f(a);

        build.beginBlock(a);
        build.inst(IrCmd::RETURN, build.constUint(1));
    };

    template<typename F>
    void withTwoBlocks(F&& f)
    {
        IrOp main = build.block(IrBlockKind::Internal);
        IrOp a = build.block(IrBlockKind::Internal);
        IrOp b = build.block(IrBlockKind::Internal);

        build.beginBlock(main);
        f(a, b);

        build.beginBlock(a);
        build.inst(IrCmd::RETURN, build.constUint(1));

        build.beginBlock(b);
        build.inst(IrCmd::RETURN, build.constUint(2));
    };

    void checkEq(IrOp instOp, const IrInst& inst)
    {
        const IrInst& target = build.function.instOp(instOp);
        CHECK(target.cmd == inst.cmd);
        CHECK(target.a == inst.a);
        CHECK(target.b == inst.b);
        CHECK(target.c == inst.c);
        CHECK(target.d == inst.d);
        CHECK(target.e == inst.e);
        CHECK(target.f == inst.f);
    }

    IrBuilder build;

    // Luau.VM headers are not accessible
    static const int tnil = 0;
    static const int tboolean = 1;
    static const int tnumber = 3;
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
    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.constUint(1));

    updateUseCounts(build.function);
    optimizeMemoryOperandsX64(build.function);

    // Load from memory is 'inlined' into CHECK_TAG
    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   CHECK_TAG R2, tnil, bb_fallback_1
   CHECK_TAG K5, tnil, bb_fallback_1
   RETURN 0u

bb_fallback_1:
   RETURN 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FinalX64OptBinaryArith")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);
    IrOp opA = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    IrOp opB = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(2));
    build.inst(IrCmd::ADD_NUM, opA, opB);
    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    optimizeMemoryOperandsX64(build.function);

    // Load from memory is 'inlined' into second argument
    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_DOUBLE R1
   %2 = ADD_NUM %0, R2
   RETURN 0u

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

    build.beginBlock(trueBlock);
    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    optimizeMemoryOperandsX64(build.function);

    // Load from memory is 'inlined' into first argument
    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %1 = LOAD_TAG R2
   JUMP_EQ_TAG R1, %1, bb_1, bb_2

bb_1:
   RETURN 0u

bb_2:
   RETURN 0u

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

    build.beginBlock(trueBlock);
    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    optimizeMemoryOperandsX64(build.function);

    // Load from memory is 'inlined' into second argument is it can't be done for the first one
    // We also swap first and second argument to generate memory access on the LHS
    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_TAG R1
   STORE_TAG R6, %0
   JUMP_EQ_TAG R2, %0, bb_1, bb_2

bb_1:
   RETURN 0u

bb_2:
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FinalX64OptEqTag3")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp trueBlock = build.block(IrBlockKind::Internal);
    IrOp falseBlock = build.block(IrBlockKind::Internal);

    build.beginBlock(block);
    IrOp table = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    IrOp arrElem = build.inst(IrCmd::GET_ARR_ADDR, table, build.constInt(0));
    IrOp opA = build.inst(IrCmd::LOAD_TAG, arrElem);
    build.inst(IrCmd::JUMP_EQ_TAG, opA, build.constTag(0), trueBlock, falseBlock);

    build.beginBlock(trueBlock);
    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    optimizeMemoryOperandsX64(build.function);

    // Load from memory is 'inlined' into first argument
    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_POINTER R1
   %1 = GET_ARR_ADDR %0, 0i
   %2 = LOAD_TAG %1
   JUMP_EQ_TAG %2, tnil, bb_1, bb_2

bb_1:
   RETURN 0u

bb_2:
   RETURN 0u

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

    build.beginBlock(trueBlock);
    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    optimizeMemoryOperandsX64(build.function);

    // Load from memory is 'inlined' into first argument
    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %1 = LOAD_DOUBLE R2
   JUMP_CMP_NUM R1, %1, bb_1, bb_2

bb_1:
   RETURN 0u

bb_2:
   RETURN 0u

)");
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("ConstantFolding");

TEST_CASE_FIXTURE(IrBuilderFixture, "Numeric")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);
    build.inst(IrCmd::STORE_INT, build.vmReg(0), build.inst(IrCmd::ADD_INT, build.constInt(10), build.constInt(20)));
    build.inst(IrCmd::STORE_INT, build.vmReg(1), build.inst(IrCmd::ADD_INT, build.constInt(INT_MAX), build.constInt(1)));

    build.inst(IrCmd::STORE_INT, build.vmReg(2), build.inst(IrCmd::SUB_INT, build.constInt(10), build.constInt(20)));
    build.inst(IrCmd::STORE_INT, build.vmReg(3), build.inst(IrCmd::SUB_INT, build.constInt(INT_MIN), build.constInt(1)));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(4), build.inst(IrCmd::ADD_NUM, build.constDouble(2), build.constDouble(5)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(5), build.inst(IrCmd::SUB_NUM, build.constDouble(2), build.constDouble(5)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(6), build.inst(IrCmd::MUL_NUM, build.constDouble(2), build.constDouble(5)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(7), build.inst(IrCmd::DIV_NUM, build.constDouble(2), build.constDouble(5)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(8), build.inst(IrCmd::MOD_NUM, build.constDouble(5), build.constDouble(2)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(10), build.inst(IrCmd::MIN_NUM, build.constDouble(5), build.constDouble(2)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(11), build.inst(IrCmd::MAX_NUM, build.constDouble(5), build.constDouble(2)));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(12), build.inst(IrCmd::UNM_NUM, build.constDouble(5)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(13), build.inst(IrCmd::FLOOR_NUM, build.constDouble(2.5)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(14), build.inst(IrCmd::CEIL_NUM, build.constDouble(2.5)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(15), build.inst(IrCmd::ROUND_NUM, build.constDouble(2.5)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(16), build.inst(IrCmd::SQRT_NUM, build.constDouble(16)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(17), build.inst(IrCmd::ABS_NUM, build.constDouble(-4)));

    build.inst(IrCmd::STORE_INT, build.vmReg(18), build.inst(IrCmd::NOT_ANY, build.constTag(tnil), build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1))));
    build.inst(
        IrCmd::STORE_INT, build.vmReg(19), build.inst(IrCmd::NOT_ANY, build.constTag(tnumber), build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1))));
    build.inst(IrCmd::STORE_INT, build.vmReg(20), build.inst(IrCmd::NOT_ANY, build.constTag(tboolean), build.constInt(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(21), build.inst(IrCmd::NOT_ANY, build.constTag(tboolean), build.constInt(1)));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_INT R0, 30i
   STORE_INT R1, -2147483648i
   STORE_INT R2, -10i
   STORE_INT R3, 2147483647i
   STORE_DOUBLE R4, 7
   STORE_DOUBLE R5, -3
   STORE_DOUBLE R6, 10
   STORE_DOUBLE R7, 0.40000000000000002
   STORE_DOUBLE R8, 1
   STORE_DOUBLE R10, 2
   STORE_DOUBLE R11, 5
   STORE_DOUBLE R12, -5
   STORE_DOUBLE R13, 2
   STORE_DOUBLE R14, 3
   STORE_DOUBLE R15, 3
   STORE_DOUBLE R16, 4
   STORE_DOUBLE R17, 4
   STORE_INT R18, 1i
   STORE_INT R19, 0i
   STORE_INT R20, 1i
   STORE_INT R21, 0i
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "NumericConversions")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.inst(IrCmd::INT_TO_NUM, build.constInt(8)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.inst(IrCmd::UINT_TO_NUM, build.constInt(0xdeee0000u)));
    build.inst(IrCmd::STORE_INT, build.vmReg(2), build.inst(IrCmd::NUM_TO_INT, build.constDouble(200.0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(3), build.inst(IrCmd::NUM_TO_UINT, build.constDouble(3740139520.0)));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_DOUBLE R0, 8
   STORE_DOUBLE R1, 3740139520
   STORE_INT R2, 200i
   STORE_INT R3, -554827776i
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "NumericConversionsBlocked")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    IrOp nan = build.inst(IrCmd::DIV_NUM, build.constDouble(0.0), build.constDouble(0.0));
    build.inst(IrCmd::STORE_INT, build.vmReg(0), build.inst(IrCmd::NUM_TO_INT, build.constDouble(1e20)));
    build.inst(IrCmd::STORE_INT, build.vmReg(1), build.inst(IrCmd::NUM_TO_UINT, build.constDouble(-10)));
    build.inst(IrCmd::STORE_INT, build.vmReg(2), build.inst(IrCmd::NUM_TO_INT, nan));
    build.inst(IrCmd::STORE_INT, build.vmReg(3), build.inst(IrCmd::NUM_TO_UINT, nan));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %1 = NUM_TO_INT 1e+20
   STORE_INT R0, %1
   %3 = NUM_TO_UINT -10
   STORE_INT R1, %3
   %5 = NUM_TO_INT nan
   STORE_INT R2, %5
   %7 = NUM_TO_UINT nan
   STORE_INT R3, %7
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "Bit32")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    IrOp unk = build.inst(IrCmd::LOAD_INT, build.vmReg(0));
    build.inst(IrCmd::STORE_INT, build.vmReg(0), build.inst(IrCmd::BITAND_UINT, build.constInt(0xfe), build.constInt(0xe)));
    build.inst(IrCmd::STORE_INT, build.vmReg(1), build.inst(IrCmd::BITAND_UINT, unk, build.constInt(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(2), build.inst(IrCmd::BITAND_UINT, build.constInt(0), unk));
    build.inst(IrCmd::STORE_INT, build.vmReg(3), build.inst(IrCmd::BITAND_UINT, unk, build.constInt(~0u)));
    build.inst(IrCmd::STORE_INT, build.vmReg(4), build.inst(IrCmd::BITAND_UINT, build.constInt(~0u), unk));
    build.inst(IrCmd::STORE_INT, build.vmReg(5), build.inst(IrCmd::BITXOR_UINT, build.constInt(0xfe), build.constInt(0xe)));
    build.inst(IrCmd::STORE_INT, build.vmReg(6), build.inst(IrCmd::BITXOR_UINT, unk, build.constInt(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(7), build.inst(IrCmd::BITXOR_UINT, build.constInt(0), unk));
    build.inst(IrCmd::STORE_INT, build.vmReg(8), build.inst(IrCmd::BITXOR_UINT, unk, build.constInt(~0u)));
    build.inst(IrCmd::STORE_INT, build.vmReg(9), build.inst(IrCmd::BITXOR_UINT, build.constInt(~0u), unk));
    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITOR_UINT, build.constInt(0xf0), build.constInt(0xe)));
    build.inst(IrCmd::STORE_INT, build.vmReg(11), build.inst(IrCmd::BITOR_UINT, unk, build.constInt(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(12), build.inst(IrCmd::BITOR_UINT, build.constInt(0), unk));
    build.inst(IrCmd::STORE_INT, build.vmReg(13), build.inst(IrCmd::BITOR_UINT, unk, build.constInt(~0u)));
    build.inst(IrCmd::STORE_INT, build.vmReg(14), build.inst(IrCmd::BITOR_UINT, build.constInt(~0u), unk));
    build.inst(IrCmd::STORE_INT, build.vmReg(15), build.inst(IrCmd::BITNOT_UINT, build.constInt(0xe)));
    build.inst(IrCmd::STORE_INT, build.vmReg(16), build.inst(IrCmd::BITLSHIFT_UINT, build.constInt(0xf0), build.constInt(4)));
    build.inst(IrCmd::STORE_INT, build.vmReg(17), build.inst(IrCmd::BITLSHIFT_UINT, unk, build.constInt(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(18), build.inst(IrCmd::BITRSHIFT_UINT, build.constInt(0xdeee0000u), build.constInt(8)));
    build.inst(IrCmd::STORE_INT, build.vmReg(19), build.inst(IrCmd::BITRSHIFT_UINT, unk, build.constInt(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(20), build.inst(IrCmd::BITARSHIFT_UINT, build.constInt(0xdeee0000u), build.constInt(8)));
    build.inst(IrCmd::STORE_INT, build.vmReg(21), build.inst(IrCmd::BITARSHIFT_UINT, unk, build.constInt(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(22), build.inst(IrCmd::BITLROTATE_UINT, build.constInt(0xdeee0000u), build.constInt(8)));
    build.inst(IrCmd::STORE_INT, build.vmReg(23), build.inst(IrCmd::BITLROTATE_UINT, unk, build.constInt(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(24), build.inst(IrCmd::BITRROTATE_UINT, build.constInt(0xdeee0000u), build.constInt(8)));
    build.inst(IrCmd::STORE_INT, build.vmReg(25), build.inst(IrCmd::BITRROTATE_UINT, unk, build.constInt(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(26), build.inst(IrCmd::BITCOUNTLZ_UINT, build.constInt(0xff00)));
    build.inst(IrCmd::STORE_INT, build.vmReg(27), build.inst(IrCmd::BITCOUNTLZ_UINT, build.constInt(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(28), build.inst(IrCmd::BITCOUNTRZ_UINT, build.constInt(0xff00)));
    build.inst(IrCmd::STORE_INT, build.vmReg(29), build.inst(IrCmd::BITCOUNTRZ_UINT, build.constInt(0)));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_INT R0
   STORE_INT R0, 14i
   STORE_INT R1, 0i
   STORE_INT R2, 0i
   STORE_INT R3, %0
   STORE_INT R4, %0
   STORE_INT R5, 240i
   STORE_INT R6, %0
   STORE_INT R7, %0
   %17 = BITNOT_UINT %0
   STORE_INT R8, %17
   %19 = BITNOT_UINT %0
   STORE_INT R9, %19
   STORE_INT R10, 254i
   STORE_INT R11, %0
   STORE_INT R12, %0
   STORE_INT R13, -1i
   STORE_INT R14, -1i
   STORE_INT R15, -15i
   STORE_INT R16, 3840i
   STORE_INT R17, %0
   STORE_INT R18, 14609920i
   STORE_INT R19, %0
   STORE_INT R20, -2167296i
   STORE_INT R21, %0
   STORE_INT R22, -301989666i
   STORE_INT R23, %0
   STORE_INT R24, 14609920i
   STORE_INT R25, %0
   STORE_INT R26, 16i
   STORE_INT R27, 32i
   STORE_INT R28, 8i
   STORE_INT R29, 32i
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "Bit32Blocked")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITLSHIFT_UINT, build.constInt(0xf), build.constInt(-10)));
    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITLSHIFT_UINT, build.constInt(0xf), build.constInt(140)));
    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITRSHIFT_UINT, build.constInt(0xf), build.constInt(-10)));
    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITRSHIFT_UINT, build.constInt(0xf), build.constInt(140)));
    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITARSHIFT_UINT, build.constInt(0xf), build.constInt(-10)));
    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITARSHIFT_UINT, build.constInt(0xf), build.constInt(140)));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = BITLSHIFT_UINT 15i, -10i
   STORE_INT R10, %0
   %2 = BITLSHIFT_UINT 15i, 140i
   STORE_INT R10, %2
   %4 = BITRSHIFT_UINT 15i, -10i
   STORE_INT R10, %4
   %6 = BITRSHIFT_UINT 15i, 140i
   STORE_INT R10, %6
   %8 = BITARSHIFT_UINT 15i, -10i
   STORE_INT R10, %8
   %10 = BITARSHIFT_UINT 15i, 140i
   STORE_INT R10, %10
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "ReplacementPreservesUses")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    IrOp unk = build.inst(IrCmd::LOAD_INT, build.vmReg(0));
    build.inst(IrCmd::STORE_INT, build.vmReg(8), build.inst(IrCmd::BITXOR_UINT, unk, build.constInt(~0u)));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, /* includeUseInfo */ true) == R"(
bb_0:                                                       ; useCount: 0
   %0 = LOAD_INT R0                                          ; useCount: 1, lastUse: %0
   %1 = BITNOT_UINT %0                                       ; useCount: 1, lastUse: %0
   STORE_INT R8, %1                                          ; %2
   RETURN 0u                                                 ; %3

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "NumericNan")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    IrOp nan = build.inst(IrCmd::DIV_NUM, build.constDouble(0.0), build.constDouble(0.0));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.inst(IrCmd::MIN_NUM, nan, build.constDouble(2)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.inst(IrCmd::MIN_NUM, build.constDouble(1), nan));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.inst(IrCmd::MAX_NUM, nan, build.constDouble(2)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.inst(IrCmd::MAX_NUM, build.constDouble(1), nan));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_DOUBLE R0, 2
   STORE_DOUBLE R0, nan
   STORE_DOUBLE R0, 2
   STORE_DOUBLE R0, nan
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "ControlFlowEq")
{
    withTwoBlocks([this](IrOp a, IrOp b) {
        build.inst(IrCmd::JUMP_EQ_TAG, build.constTag(tnil), build.constTag(tnil), a, b);
    });

    withTwoBlocks([this](IrOp a, IrOp b) {
        build.inst(IrCmd::JUMP_EQ_TAG, build.constTag(tnil), build.constTag(tnumber), a, b);
    });

    withTwoBlocks([this](IrOp a, IrOp b) {
        build.inst(IrCmd::JUMP_EQ_INT, build.constInt(0), build.constInt(0), a, b);
    });

    withTwoBlocks([this](IrOp a, IrOp b) {
        build.inst(IrCmd::JUMP_EQ_INT, build.constInt(0), build.constInt(1), a, b);
    });

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   JUMP bb_1

bb_1:
   RETURN 1u

bb_3:
   JUMP bb_5

bb_5:
   RETURN 2u

bb_6:
   JUMP bb_7

bb_7:
   RETURN 1u

bb_9:
   JUMP bb_11

bb_11:
   RETURN 2u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "NumToIndex")
{
    withOneBlock([this](IrOp a) {
        build.inst(IrCmd::STORE_INT, build.vmReg(0), build.inst(IrCmd::TRY_NUM_TO_INDEX, build.constDouble(4), a));
        build.inst(IrCmd::RETURN, build.constUint(0));
    });

    withOneBlock([this](IrOp a) {
        build.inst(IrCmd::STORE_INT, build.vmReg(0), build.inst(IrCmd::TRY_NUM_TO_INDEX, build.constDouble(1.2), a));
        build.inst(IrCmd::RETURN, build.constUint(0));
    });

    withOneBlock([this](IrOp a) {
        IrOp nan = build.inst(IrCmd::DIV_NUM, build.constDouble(0.0), build.constDouble(0.0));
        build.inst(IrCmd::STORE_INT, build.vmReg(0), build.inst(IrCmd::TRY_NUM_TO_INDEX, nan, a));
        build.inst(IrCmd::RETURN, build.constUint(0));
    });

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_INT R0, 4i
   RETURN 0u

bb_2:
   JUMP bb_3

bb_3:
   RETURN 1u

bb_4:
   JUMP bb_5

bb_5:
   RETURN 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "Guards")
{
    withOneBlock([this](IrOp a) {
        build.inst(IrCmd::CHECK_TAG, build.constTag(tnumber), build.constTag(tnumber), a);
        build.inst(IrCmd::RETURN, build.constUint(0));
    });

    withOneBlock([this](IrOp a) {
        build.inst(IrCmd::CHECK_TAG, build.constTag(tnil), build.constTag(tnumber), a);
        build.inst(IrCmd::RETURN, build.constUint(0));
    });

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   RETURN 0u

bb_2:
   JUMP bb_3

bb_3:
   RETURN 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "ControlFlowCmpNum")
{
    auto compareFold = [this](IrOp lhs, IrOp rhs, IrCondition cond, bool result) {
        IrOp instOp;
        IrInst instExpected;

        withTwoBlocks([&](IrOp a, IrOp b) {
            IrOp nan = build.inst(IrCmd::DIV_NUM, build.constDouble(0.0), build.constDouble(0.0));
            instOp = build.inst(
                IrCmd::JUMP_CMP_NUM, lhs.kind == IrOpKind::None ? nan : lhs, rhs.kind == IrOpKind::None ? nan : rhs, build.cond(cond), a, b);
            instExpected = IrInst{IrCmd::JUMP, result ? a : b};
        });

        updateUseCounts(build.function);
        constantFold();
        checkEq(instOp, instExpected);
    };

    IrOp nan; // Empty operand is used to signal a placement of a 'nan'

    compareFold(build.constDouble(1), build.constDouble(1), IrCondition::Equal, true);
    compareFold(build.constDouble(1), build.constDouble(2), IrCondition::Equal, false);
    compareFold(nan, nan, IrCondition::Equal, false);

    compareFold(build.constDouble(1), build.constDouble(1), IrCondition::NotEqual, false);
    compareFold(build.constDouble(1), build.constDouble(2), IrCondition::NotEqual, true);
    compareFold(nan, nan, IrCondition::NotEqual, true);

    compareFold(build.constDouble(1), build.constDouble(1), IrCondition::Less, false);
    compareFold(build.constDouble(1), build.constDouble(2), IrCondition::Less, true);
    compareFold(build.constDouble(2), build.constDouble(1), IrCondition::Less, false);
    compareFold(build.constDouble(1), nan, IrCondition::Less, false);

    compareFold(build.constDouble(1), build.constDouble(1), IrCondition::NotLess, true);
    compareFold(build.constDouble(1), build.constDouble(2), IrCondition::NotLess, false);
    compareFold(build.constDouble(2), build.constDouble(1), IrCondition::NotLess, true);
    compareFold(build.constDouble(1), nan, IrCondition::NotLess, true);

    compareFold(build.constDouble(1), build.constDouble(1), IrCondition::LessEqual, true);
    compareFold(build.constDouble(1), build.constDouble(2), IrCondition::LessEqual, true);
    compareFold(build.constDouble(2), build.constDouble(1), IrCondition::LessEqual, false);
    compareFold(build.constDouble(1), nan, IrCondition::LessEqual, false);

    compareFold(build.constDouble(1), build.constDouble(1), IrCondition::NotLessEqual, false);
    compareFold(build.constDouble(1), build.constDouble(2), IrCondition::NotLessEqual, false);
    compareFold(build.constDouble(2), build.constDouble(1), IrCondition::NotLessEqual, true);
    compareFold(build.constDouble(1), nan, IrCondition::NotLessEqual, true);

    compareFold(build.constDouble(1), build.constDouble(1), IrCondition::Greater, false);
    compareFold(build.constDouble(1), build.constDouble(2), IrCondition::Greater, false);
    compareFold(build.constDouble(2), build.constDouble(1), IrCondition::Greater, true);
    compareFold(build.constDouble(1), nan, IrCondition::Greater, false);

    compareFold(build.constDouble(1), build.constDouble(1), IrCondition::NotGreater, true);
    compareFold(build.constDouble(1), build.constDouble(2), IrCondition::NotGreater, true);
    compareFold(build.constDouble(2), build.constDouble(1), IrCondition::NotGreater, false);
    compareFold(build.constDouble(1), nan, IrCondition::NotGreater, true);

    compareFold(build.constDouble(1), build.constDouble(1), IrCondition::GreaterEqual, true);
    compareFold(build.constDouble(1), build.constDouble(2), IrCondition::GreaterEqual, false);
    compareFold(build.constDouble(2), build.constDouble(1), IrCondition::GreaterEqual, true);
    compareFold(build.constDouble(1), nan, IrCondition::GreaterEqual, false);

    compareFold(build.constDouble(1), build.constDouble(1), IrCondition::NotGreaterEqual, false);
    compareFold(build.constDouble(1), build.constDouble(2), IrCondition::NotGreaterEqual, true);
    compareFold(build.constDouble(2), build.constDouble(1), IrCondition::NotGreaterEqual, false);
    compareFold(build.constDouble(1), nan, IrCondition::NotGreaterEqual, true);
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("ConstantPropagation");

TEST_CASE_FIXTURE(IrBuilderFixture, "RememberTagsAndValues")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::STORE_INT, build.vmReg(1), build.constInt(10));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), build.constDouble(0.5));

    // We know constants from those loads
    build.inst(IrCmd::STORE_TAG, build.vmReg(3), build.inst(IrCmd::LOAD_TAG, build.vmReg(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(4), build.inst(IrCmd::LOAD_INT, build.vmReg(1)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(5), build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(2)));

    // We know that these overrides have no effect
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::STORE_INT, build.vmReg(1), build.constInt(10));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), build.constDouble(0.5));

    // But we can invalidate them with unknown values
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.inst(IrCmd::LOAD_TAG, build.vmReg(6)));
    build.inst(IrCmd::STORE_INT, build.vmReg(1), build.inst(IrCmd::LOAD_INT, build.vmReg(7)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(8)));

    // So now the constant stores have to be made
    build.inst(IrCmd::STORE_TAG, build.vmReg(9), build.inst(IrCmd::LOAD_TAG, build.vmReg(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::LOAD_INT, build.vmReg(1)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(11), build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(2)));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_TAG R0, tnumber
   STORE_INT R1, 10i
   STORE_DOUBLE R2, 0.5
   STORE_TAG R3, tnumber
   STORE_INT R4, 10i
   STORE_DOUBLE R5, 0.5
   %12 = LOAD_TAG R6
   STORE_TAG R0, %12
   %14 = LOAD_INT R7
   STORE_INT R1, %14
   %16 = LOAD_DOUBLE R8
   STORE_DOUBLE R2, %16
   %18 = LOAD_TAG R0
   STORE_TAG R9, %18
   STORE_INT R10, %14
   STORE_DOUBLE R11, %16
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "PropagateThroughTvalue")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(0.5));

    IrOp tv = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(1), tv);

    // We know constants from those loads
    build.inst(IrCmd::STORE_TAG, build.vmReg(3), build.inst(IrCmd::LOAD_TAG, build.vmReg(1)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(3), build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1)));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_TAG R0, tnumber
   STORE_DOUBLE R0, 0.5
   %2 = LOAD_TVALUE R0
   STORE_TVALUE R1, %2
   STORE_TAG R3, tnumber
   STORE_DOUBLE R3, 0.5
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "SkipCheckTag")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(0)), build.constTag(tnumber), fallback);
    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_TAG R0, tnumber
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "SkipOncePerBlockChecks")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    build.inst(IrCmd::CHECK_SAFE_ENV);
    build.inst(IrCmd::CHECK_SAFE_ENV);
    build.inst(IrCmd::CHECK_GC);
    build.inst(IrCmd::CHECK_GC);

    build.inst(IrCmd::DO_LEN, build.vmReg(1), build.vmReg(2)); // Can make env unsafe
    build.inst(IrCmd::CHECK_SAFE_ENV);

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   CHECK_SAFE_ENV
   CHECK_GC
   DO_LEN R1, R2
   CHECK_SAFE_ENV
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "RememberTableState")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    IrOp table = build.inst(IrCmd::LOAD_POINTER, build.vmReg(0));

    build.inst(IrCmd::CHECK_NO_METATABLE, table, fallback);
    build.inst(IrCmd::CHECK_READONLY, table, fallback);

    build.inst(IrCmd::CHECK_NO_METATABLE, table, fallback);
    build.inst(IrCmd::CHECK_READONLY, table, fallback);

    build.inst(IrCmd::DO_LEN, build.vmReg(1), build.vmReg(2)); // Can access all heap memory

    build.inst(IrCmd::CHECK_NO_METATABLE, table, fallback);
    build.inst(IrCmd::CHECK_READONLY, table, fallback);

    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_POINTER R0
   CHECK_NO_METATABLE %0, bb_fallback_1
   CHECK_READONLY %0, bb_fallback_1
   DO_LEN R1, R2
   CHECK_NO_METATABLE %0, bb_fallback_1
   CHECK_READONLY %0, bb_fallback_1
   RETURN 0u

bb_fallback_1:
   RETURN 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "SkipUselessBarriers")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    IrOp table = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    build.inst(IrCmd::BARRIER_TABLE_FORWARD, table, build.vmReg(0));
    IrOp something = build.inst(IrCmd::LOAD_POINTER, build.vmReg(2));
    build.inst(IrCmd::BARRIER_OBJ, something, build.vmReg(0));
    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_TAG R0, tnumber
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "ConcatInvalidation")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::STORE_INT, build.vmReg(1), build.constInt(10));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), build.constDouble(0.5));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(3), build.constDouble(2.0));

    build.inst(IrCmd::CONCAT, build.vmReg(0), build.constUint(3));

    build.inst(IrCmd::STORE_TAG, build.vmReg(4), build.inst(IrCmd::LOAD_TAG, build.vmReg(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(5), build.inst(IrCmd::LOAD_INT, build.vmReg(1)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(6), build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(2)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(7), build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(3)));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_TAG R0, tnumber
   STORE_INT R1, 10i
   STORE_DOUBLE R2, 0.5
   STORE_DOUBLE R3, 2
   CONCAT R0, 3u
   %5 = LOAD_TAG R0
   STORE_TAG R4, %5
   %7 = LOAD_INT R1
   STORE_INT R5, %7
   %9 = LOAD_DOUBLE R2
   STORE_DOUBLE R6, %9
   STORE_DOUBLE R7, 2
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "BuiltinFastcallsMayInvalidateMemory")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(0.5));

    IrOp table = build.inst(IrCmd::LOAD_POINTER, build.vmReg(0));

    build.inst(IrCmd::CHECK_NO_METATABLE, table, fallback);
    build.inst(IrCmd::CHECK_READONLY, table, fallback);

    build.inst(IrCmd::INVOKE_FASTCALL, build.constUint(LBF_SETMETATABLE), build.vmReg(1), build.vmReg(2), build.vmReg(3), build.constInt(3),
        build.constInt(1));

    build.inst(IrCmd::CHECK_NO_METATABLE, table, fallback);
    build.inst(IrCmd::CHECK_READONLY, table, fallback);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0))); // At least R0 wasn't touched

    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_DOUBLE R0, 0.5
   %1 = LOAD_POINTER R0
   CHECK_NO_METATABLE %1, bb_fallback_1
   CHECK_READONLY %1, bb_fallback_1
   %4 = INVOKE_FASTCALL 61u, R1, R2, R3, 3i, 1i
   CHECK_NO_METATABLE %1, bb_fallback_1
   CHECK_READONLY %1, bb_fallback_1
   STORE_DOUBLE R1, 0.5
   RETURN 0u

bb_fallback_1:
   RETURN 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "RedundantStoreCheckConstantType")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_INT, build.vmReg(0), build.constInt(10));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(0.5));
    build.inst(IrCmd::STORE_INT, build.vmReg(0), build.constInt(10));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_INT R0, 10i
   STORE_DOUBLE R0, 0.5
   STORE_INT R0, 10i
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "TagCheckPropagation")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    IrOp unknown = build.inst(IrCmd::LOAD_TAG, build.vmReg(0));

    build.inst(IrCmd::CHECK_TAG, unknown, build.constTag(tnumber), fallback);
    build.inst(IrCmd::CHECK_TAG, unknown, build.constTag(tnumber), fallback);

    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_TAG R0
   CHECK_TAG %0, tnumber, bb_fallback_1
   RETURN 0u

bb_fallback_1:
   RETURN 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "TagCheckPropagationConflicting")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    IrOp unknown = build.inst(IrCmd::LOAD_TAG, build.vmReg(0));

    build.inst(IrCmd::CHECK_TAG, unknown, build.constTag(tnumber), fallback);
    build.inst(IrCmd::CHECK_TAG, unknown, build.constTag(tnil), fallback);

    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_TAG R0
   CHECK_TAG %0, tnumber, bb_fallback_1
   JUMP bb_fallback_1

bb_fallback_1:
   RETURN 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "TruthyTestRemoval")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp trueBlock = build.block(IrBlockKind::Internal);
    IrOp falseBlock = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);
    IrOp unknown = build.inst(IrCmd::LOAD_TAG, build.vmReg(1));
    build.inst(IrCmd::CHECK_TAG, unknown, build.constTag(tnumber), fallback);
    build.inst(IrCmd::JUMP_IF_TRUTHY, build.vmReg(1), trueBlock, falseBlock);

    build.beginBlock(trueBlock);
    build.inst(IrCmd::RETURN, build.constUint(1));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.constUint(2));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.constUint(3));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_TAG R1
   CHECK_TAG %0, tnumber, bb_fallback_3
   JUMP bb_1

bb_1:
   RETURN 1u

bb_fallback_3:
   RETURN 3u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FalsyTestRemoval")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp trueBlock = build.block(IrBlockKind::Internal);
    IrOp falseBlock = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);
    IrOp unknown = build.inst(IrCmd::LOAD_TAG, build.vmReg(1));
    build.inst(IrCmd::CHECK_TAG, unknown, build.constTag(tnumber), fallback);
    build.inst(IrCmd::JUMP_IF_FALSY, build.vmReg(1), trueBlock, falseBlock);

    build.beginBlock(trueBlock);
    build.inst(IrCmd::RETURN, build.constUint(1));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.constUint(2));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.constUint(3));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_TAG R1
   CHECK_TAG %0, tnumber, bb_fallback_3
   JUMP bb_2

bb_2:
   RETURN 2u

bb_fallback_3:
   RETURN 3u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "TagEqRemoval")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp trueBlock = build.block(IrBlockKind::Internal);
    IrOp falseBlock = build.block(IrBlockKind::Internal);

    build.beginBlock(block);
    IrOp tag = build.inst(IrCmd::LOAD_TAG, build.vmReg(1));
    build.inst(IrCmd::CHECK_TAG, tag, build.constTag(tboolean));
    build.inst(IrCmd::JUMP_EQ_TAG, tag, build.constTag(tnumber), trueBlock, falseBlock);

    build.beginBlock(trueBlock);
    build.inst(IrCmd::RETURN, build.constUint(1));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.constUint(2));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_TAG R1
   CHECK_TAG %0, tboolean
   JUMP bb_2

bb_2:
   RETURN 2u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "IntEqRemoval")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp trueBlock = build.block(IrBlockKind::Internal);
    IrOp falseBlock = build.block(IrBlockKind::Internal);

    build.beginBlock(block);
    IrOp value = build.inst(IrCmd::LOAD_INT, build.vmReg(1));
    build.inst(IrCmd::STORE_INT, build.vmReg(1), build.constInt(5));
    build.inst(IrCmd::JUMP_EQ_INT, value, build.constInt(5), trueBlock, falseBlock);

    build.beginBlock(trueBlock);
    build.inst(IrCmd::RETURN, build.constUint(1));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.constUint(2));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_INT R1, 5i
   JUMP bb_1

bb_1:
   RETURN 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "NumCmpRemoval")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp trueBlock = build.block(IrBlockKind::Internal);
    IrOp falseBlock = build.block(IrBlockKind::Internal);

    build.beginBlock(block);
    IrOp value = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(4.0));
    build.inst(IrCmd::JUMP_CMP_NUM, value, build.constDouble(8.0), build.cond(IrCondition::Greater), trueBlock, falseBlock);

    build.beginBlock(trueBlock);
    build.inst(IrCmd::RETURN, build.constUint(1));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.constUint(2));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_DOUBLE R1, 4
   JUMP bb_2

bb_2:
   RETURN 2u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DataFlowsThroughDirectJumpToUniqueSuccessor")
{
    IrOp block1 = build.block(IrBlockKind::Internal);
    IrOp block2 = build.block(IrBlockKind::Internal);

    build.beginBlock(block1);

    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::JUMP, block2);

    build.beginBlock(block2);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.inst(IrCmd::LOAD_TAG, build.vmReg(0)));
    build.inst(IrCmd::RETURN, build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_TAG R0, tnumber
   JUMP bb_1

bb_1:
   STORE_TAG R1, tnumber
   RETURN 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DataDoesNotFlowThroughDirectJumpToNonUniqueSuccessor")
{
    IrOp block1 = build.block(IrBlockKind::Internal);
    IrOp block2 = build.block(IrBlockKind::Internal);
    IrOp block3 = build.block(IrBlockKind::Internal);

    build.beginBlock(block1);

    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::JUMP, block2);

    build.beginBlock(block2);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.inst(IrCmd::LOAD_TAG, build.vmReg(0)));
    build.inst(IrCmd::RETURN, build.constUint(1));

    build.beginBlock(block3);
    build.inst(IrCmd::JUMP, block2);

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_TAG R0, tnumber
   JUMP bb_1

bb_1:
   %2 = LOAD_TAG R0
   STORE_TAG R1, %2
   RETURN 1u

bb_2:
   JUMP bb_1

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "EntryBlockUseRemoval")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp exit = build.block(IrBlockKind::Internal);
    IrOp repeat = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::JUMP_IF_TRUTHY, build.vmReg(0), exit, repeat);

    build.beginBlock(exit);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(0));

    build.beginBlock(repeat);
    build.inst(IrCmd::INTERRUPT, build.constUint(0));
    build.inst(IrCmd::JUMP, entry);

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_TAG R0, tnumber
   JUMP bb_1

bb_1:
   RETURN R0, 0i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "RecursiveSccUseRemoval1")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp exit = build.block(IrBlockKind::Internal);
    IrOp repeat = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(0));

    build.beginBlock(block);
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::JUMP_IF_TRUTHY, build.vmReg(0), exit, repeat);

    build.beginBlock(exit);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(0));

    build.beginBlock(repeat);
    build.inst(IrCmd::INTERRUPT, build.constUint(0));
    build.inst(IrCmd::JUMP, block);

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   RETURN R0, 0i

bb_1:
   STORE_TAG R0, tnumber
   JUMP bb_2

bb_2:
   RETURN R0, 0i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "RecursiveSccUseRemoval2")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp exit1 = build.block(IrBlockKind::Internal);
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp exit2 = build.block(IrBlockKind::Internal);
    IrOp repeat = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::JUMP_EQ_INT, build.constInt(0), build.constInt(1), block, exit1);

    build.beginBlock(exit1);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(0));

    build.beginBlock(block);
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::JUMP_IF_TRUTHY, build.vmReg(0), exit2, repeat);

    build.beginBlock(exit2);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(0));

    build.beginBlock(repeat);
    build.inst(IrCmd::INTERRUPT, build.constUint(0));
    build.inst(IrCmd::JUMP, block);

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   JUMP bb_1

bb_1:
   RETURN R0, 0i

bb_2:
   STORE_TAG R0, tnumber
   JUMP bb_3

bb_3:
   RETURN R0, 0i

)");
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("LinearExecutionFlowExtraction");

TEST_CASE_FIXTURE(IrBuilderFixture, "SimplePathExtraction")
{
    IrOp block1 = build.block(IrBlockKind::Internal);
    IrOp fallback1 = build.block(IrBlockKind::Fallback);
    IrOp block2 = build.block(IrBlockKind::Internal);
    IrOp fallback2 = build.block(IrBlockKind::Fallback);
    IrOp block3 = build.block(IrBlockKind::Internal);
    IrOp block4 = build.block(IrBlockKind::Internal);

    build.beginBlock(block1);

    IrOp tag1 = build.inst(IrCmd::LOAD_TAG, build.vmReg(2));
    build.inst(IrCmd::CHECK_TAG, tag1, build.constTag(tnumber), fallback1);
    build.inst(IrCmd::JUMP, block2);

    build.beginBlock(fallback1);
    build.inst(IrCmd::DO_LEN, build.vmReg(1), build.vmReg(2));
    build.inst(IrCmd::JUMP, block2);

    build.beginBlock(block2);
    IrOp tag2 = build.inst(IrCmd::LOAD_TAG, build.vmReg(2));
    build.inst(IrCmd::CHECK_TAG, tag2, build.constTag(tnumber), fallback2);
    build.inst(IrCmd::JUMP, block3);

    build.beginBlock(fallback2);
    build.inst(IrCmd::DO_LEN, build.vmReg(0), build.vmReg(2));
    build.inst(IrCmd::JUMP, block3);

    build.beginBlock(block3);
    build.inst(IrCmd::JUMP, block4);

    build.beginBlock(block4);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);
    createLinearBlocks(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_TAG R2
   CHECK_TAG %0, tnumber, bb_fallback_1
   JUMP bb_linear_6

bb_fallback_1:
   DO_LEN R1, R2
   JUMP bb_2

bb_2:
   %5 = LOAD_TAG R2
   CHECK_TAG %5, tnumber, bb_fallback_3
   JUMP bb_4

bb_fallback_3:
   DO_LEN R0, R2
   JUMP bb_4

bb_4:
   JUMP bb_5

bb_5:
   RETURN R0, 0i

bb_linear_6:
   RETURN R0, 0i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "NoPathExtractionForBlocksWithLiveOutValues")
{
    IrOp block1 = build.block(IrBlockKind::Internal);
    IrOp fallback1 = build.block(IrBlockKind::Fallback);
    IrOp block2 = build.block(IrBlockKind::Internal);
    IrOp fallback2 = build.block(IrBlockKind::Fallback);
    IrOp block3 = build.block(IrBlockKind::Internal);
    IrOp block4a = build.block(IrBlockKind::Internal);
    IrOp block4b = build.block(IrBlockKind::Internal);

    build.beginBlock(block1);

    IrOp tag1 = build.inst(IrCmd::LOAD_TAG, build.vmReg(2));
    build.inst(IrCmd::CHECK_TAG, tag1, build.constTag(tnumber), fallback1);
    build.inst(IrCmd::JUMP, block2);

    build.beginBlock(fallback1);
    build.inst(IrCmd::DO_LEN, build.vmReg(1), build.vmReg(2));
    build.inst(IrCmd::JUMP, block2);

    build.beginBlock(block2);
    IrOp tag2 = build.inst(IrCmd::LOAD_TAG, build.vmReg(2));
    build.inst(IrCmd::CHECK_TAG, tag2, build.constTag(tnumber), fallback2);
    build.inst(IrCmd::JUMP, block3);

    build.beginBlock(fallback2);
    build.inst(IrCmd::DO_LEN, build.vmReg(0), build.vmReg(2));
    build.inst(IrCmd::JUMP, block3);

    build.beginBlock(block3);
    IrOp tag3a = build.inst(IrCmd::LOAD_TAG, build.vmReg(3));
    build.inst(IrCmd::JUMP_EQ_TAG, tag3a, build.constTag(tnil), block4a, block4b);

    build.beginBlock(block4a);
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), tag3a);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(0));

    build.beginBlock(block4b);
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), tag3a);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);
    createLinearBlocks(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_TAG R2
   CHECK_TAG %0, tnumber, bb_fallback_1
   JUMP bb_2

bb_fallback_1:
   DO_LEN R1, R2
   JUMP bb_2

bb_2:
   %5 = LOAD_TAG R2
   CHECK_TAG %5, tnumber, bb_fallback_3
   JUMP bb_4

bb_fallback_3:
   DO_LEN R0, R2
   JUMP bb_4

bb_4:
   %10 = LOAD_TAG R3
   JUMP_EQ_TAG %10, tnil, bb_5, bb_6

bb_5:
   STORE_TAG R0, %10
   RETURN R0, 0i

bb_6:
   STORE_TAG R0, %10
   RETURN R0, 0i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "InfiniteLoopInPathAnalysis")
{
    IrOp block1 = build.block(IrBlockKind::Internal);
    IrOp block2 = build.block(IrBlockKind::Internal);

    build.beginBlock(block1);

    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::JUMP, block2);

    build.beginBlock(block2);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tboolean));
    build.inst(IrCmd::JUMP, block2);

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);
    createLinearBlocks(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   STORE_TAG R0, tnumber
   JUMP bb_1

bb_1:
   STORE_TAG R1, tboolean
   JUMP bb_1

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "PartialStoreInvalidation")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_TVALUE, build.vmReg(1), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(0.5));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(1), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0))); // Should be reloaded
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tboolean));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(1), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0))); // Should be reloaded

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_TVALUE R0
   STORE_TVALUE R1, %0
   STORE_DOUBLE R0, 0.5
   %3 = LOAD_TVALUE R0
   STORE_TVALUE R1, %3
   STORE_TAG R0, tboolean
   %6 = LOAD_TVALUE R0
   STORE_TVALUE R1, %6
   RETURN 0u

)");
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("Analysis");

TEST_CASE_FIXTURE(IrBuilderFixture, "SimpleDiamond")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp a = build.block(IrBlockKind::Internal);
    IrOp b = build.block(IrBlockKind::Internal);
    IrOp exit = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::JUMP_EQ_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(0)), build.constTag(tnumber), a, b);

    build.beginBlock(a);
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(2), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(1)));
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(b);
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(3), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(1)));
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(exit);
    build.inst(IrCmd::RETURN, build.vmReg(2), build.constInt(2));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
; successors: bb_1, bb_2
; in regs: R0, R1, R2, R3
; out regs: R1, R2, R3
   %0 = LOAD_TAG R0
   JUMP_EQ_TAG %0, tnumber, bb_1, bb_2

bb_1:
; predecessors: bb_0
; successors: bb_3
; in regs: R1, R3
; out regs: R2, R3
   %2 = LOAD_TVALUE R1
   STORE_TVALUE R2, %2
   JUMP bb_3

bb_2:
; predecessors: bb_0
; successors: bb_3
; in regs: R1, R2
; out regs: R2, R3
   %5 = LOAD_TVALUE R1
   STORE_TVALUE R3, %5
   JUMP bb_3

bb_3:
; predecessors: bb_1, bb_2
; in regs: R2, R3
   RETURN R2, 2i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "ImplicitFixedRegistersInVarargCall")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp exit = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::FALLBACK_GETVARARGS, build.constUint(0), build.vmReg(3), build.constInt(-1));
    build.inst(IrCmd::CALL, build.vmReg(0), build.constInt(-1), build.constInt(5));
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(exit);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(5));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
; successors: bb_1
; in regs: R0, R1, R2
; out regs: R0, R1, R2, R3, R4
   FALLBACK_GETVARARGS 0u, R3, -1i
   CALL R0, -1i, 5i
   JUMP bb_1

bb_1:
; predecessors: bb_0
; in regs: R0, R1, R2, R3, R4
   RETURN R0, 5i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "ExplicitUseOfRegisterInVarargSequence")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp exit = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::FALLBACK_GETVARARGS, build.constUint(0), build.vmReg(1), build.constInt(-1));
    IrOp results = build.inst(
        IrCmd::INVOKE_FASTCALL, build.constUint(0), build.vmReg(0), build.vmReg(1), build.vmReg(2), build.constInt(-1), build.constInt(-1));
    build.inst(IrCmd::ADJUST_STACK_TO_REG, build.vmReg(0), results);
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(exit);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(-1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
; successors: bb_1
; out regs: R0...
   FALLBACK_GETVARARGS 0u, R1, -1i
   %1 = INVOKE_FASTCALL 0u, R0, R1, R2, -1i, -1i
   ADJUST_STACK_TO_REG R0, %1
   JUMP bb_1

bb_1:
; predecessors: bb_0
; in regs: R0...
   RETURN R0, -1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "VariadicSequenceRestart")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp exit = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::CALL, build.vmReg(1), build.constInt(0), build.constInt(-1));
    build.inst(IrCmd::CALL, build.vmReg(0), build.constInt(-1), build.constInt(-1));
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(exit);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(-1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
; successors: bb_1
; in regs: R0, R1
; out regs: R0...
   CALL R1, 0i, -1i
   CALL R0, -1i, -1i
   JUMP bb_1

bb_1:
; predecessors: bb_0
; in regs: R0...
   RETURN R0, -1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FallbackDoesNotFlowUp")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);
    IrOp exit = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::FALLBACK_GETVARARGS, build.constUint(0), build.vmReg(1), build.constInt(-1));
    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(0)), build.constTag(tnumber), fallback);
    build.inst(IrCmd::CALL, build.vmReg(0), build.constInt(-1), build.constInt(-1));
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(fallback);
    build.inst(IrCmd::CALL, build.vmReg(0), build.constInt(-1), build.constInt(-1));
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(exit);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(-1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
; successors: bb_fallback_1, bb_2
; in regs: R0
; out regs: R0...
   FALLBACK_GETVARARGS 0u, R1, -1i
   %1 = LOAD_TAG R0
   CHECK_TAG %1, tnumber, bb_fallback_1
   CALL R0, -1i, -1i
   JUMP bb_2

bb_fallback_1:
; predecessors: bb_0
; successors: bb_2
; in regs: R0, R1...
; out regs: R0...
   CALL R0, -1i, -1i
   JUMP bb_2

bb_2:
; predecessors: bb_0, bb_fallback_1
; in regs: R0...
   RETURN R0, -1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "VariadicSequencePeeling")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp a = build.block(IrBlockKind::Internal);
    IrOp b = build.block(IrBlockKind::Internal);
    IrOp exit = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::FALLBACK_GETVARARGS, build.constUint(0), build.vmReg(3), build.constInt(-1));
    build.inst(IrCmd::JUMP_EQ_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(0)), build.constTag(tnumber), a, b);

    build.beginBlock(a);
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(2), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0)));
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(b);
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(2), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(1)));
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(exit);
    build.inst(IrCmd::RETURN, build.vmReg(2), build.constInt(-1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
; successors: bb_1, bb_2
; in regs: R0, R1
; out regs: R0, R1, R3...
   FALLBACK_GETVARARGS 0u, R3, -1i
   %1 = LOAD_TAG R0
   JUMP_EQ_TAG %1, tnumber, bb_1, bb_2

bb_1:
; predecessors: bb_0
; successors: bb_3
; in regs: R0, R3...
; out regs: R2...
   %3 = LOAD_TVALUE R0
   STORE_TVALUE R2, %3
   JUMP bb_3

bb_2:
; predecessors: bb_0
; successors: bb_3
; in regs: R1, R3...
; out regs: R2...
   %6 = LOAD_TVALUE R1
   STORE_TVALUE R2, %6
   JUMP bb_3

bb_3:
; predecessors: bb_1, bb_2
; in regs: R2...
   RETURN R2, -1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "BuiltinVariadicStart")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp exit = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(1.0));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), build.constDouble(2.0));
    build.inst(IrCmd::ADJUST_STACK_TO_REG, build.vmReg(2), build.constInt(1));
    build.inst(IrCmd::CALL, build.vmReg(1), build.constInt(-1), build.constInt(1));
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(exit);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(2));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
; successors: bb_1
; in regs: R0
; out regs: R0, R1
   STORE_DOUBLE R1, 1
   STORE_DOUBLE R2, 2
   ADJUST_STACK_TO_REG R2, 1i
   CALL R1, -1i, 1i
   JUMP bb_1

bb_1:
; predecessors: bb_0
; in regs: R0, R1
   RETURN R0, 2i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "SetTable")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::SET_TABLE, build.vmReg(0), build.vmReg(1), build.constUint(1));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
; in regs: R0, R1
   SET_TABLE R0, R1, 1u
   RETURN R0, 1i

)");
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("ValueNumbering");

TEST_CASE_FIXTURE(IrBuilderFixture, "RemoveDuplicateCalculation")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp op1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp op2 = build.inst(IrCmd::UNM_NUM, op1);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), op2);
    IrOp op3 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0)); // Load propagation is tested here
    IrOp op4 = build.inst(IrCmd::UNM_NUM, op3);                // And allows value numbering to trigger here
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), op4);
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(2));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_DOUBLE R0
   %1 = UNM_NUM %0
   STORE_DOUBLE R1, %1
   STORE_DOUBLE R2, %1
   RETURN R1, 2i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "LateTableStateLink")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    IrOp tmp = build.inst(IrCmd::DUP_TABLE, build.vmReg(0));
    build.inst(IrCmd::STORE_POINTER, build.vmReg(0), tmp);        // Late tmp -> R0 link is tested here
    IrOp table = build.inst(IrCmd::LOAD_POINTER, build.vmReg(0)); // Store to load propagation test

    build.inst(IrCmd::CHECK_NO_METATABLE, table, fallback);
    build.inst(IrCmd::CHECK_READONLY, table, fallback);

    build.inst(IrCmd::CHECK_NO_METATABLE, table, fallback);
    build.inst(IrCmd::CHECK_READONLY, table, fallback);

    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = DUP_TABLE R0
   STORE_POINTER R0, %0
   CHECK_NO_METATABLE %0, bb_fallback_1
   CHECK_READONLY %0, bb_fallback_1
   RETURN 0u

bb_fallback_1:
   RETURN 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "RegisterVersioning")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp op1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp op2 = build.inst(IrCmd::UNM_NUM, op1);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), op2);
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber)); // Doesn't prevent previous store propagation
    IrOp op3 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));             // No longer 'op1'
    IrOp op4 = build.inst(IrCmd::UNM_NUM, op3);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), op4);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(2));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_DOUBLE R0
   %1 = UNM_NUM %0
   STORE_DOUBLE R0, %1
   STORE_TAG R0, tnumber
   %5 = UNM_NUM %1
   STORE_DOUBLE R1, %5
   RETURN R0, 2i

)");
}

// This can be relaxed in the future when SETLIST becomes aware of register allocator
TEST_CASE_FIXTURE(IrBuilderFixture, "SetListIsABlocker")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp op1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    build.inst(IrCmd::SETLIST);
    IrOp op2 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp sum = build.inst(IrCmd::ADD_NUM, op1, op2);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), sum);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_DOUBLE R0
   SETLIST
   %2 = LOAD_DOUBLE R0
   %3 = ADD_NUM %0, %2
   STORE_DOUBLE R0, %3
   RETURN R0, 1i

)");
}

// Luau call will reuse the same stack and spills will be lost
// However, in the future we might propagate values that can be rematerialized
TEST_CASE_FIXTURE(IrBuilderFixture, "CallIsABlocker")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp op1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    build.inst(IrCmd::CALL, build.vmReg(1), build.constInt(1), build.vmReg(2), build.constInt(1));
    IrOp op2 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp sum = build.inst(IrCmd::ADD_NUM, op1, op2);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), sum);
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(2));

    updateUseCounts(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
bb_0:
   %0 = LOAD_DOUBLE R0
   CALL R1, 1i, R2, 1i
   %2 = LOAD_DOUBLE R0
   %3 = ADD_NUM %0, %2
   STORE_DOUBLE R1, %3
   RETURN R1, 2i

)");
}

// While constant propagation correctly versions captured registers, IrValueLocationTracking doesn't (yet)
TEST_CASE_FIXTURE(IrBuilderFixture, "NoPropagationOfCapturedRegs")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::CAPTURE, build.vmReg(0), build.constBool(true));
    IrOp op1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp op2 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp sum = build.inst(IrCmd::ADD_NUM, op1, op2);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), sum);
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build, true);

    CHECK("\n" + toString(build.function, /* includeUseInfo */ false) == R"(
; captured regs: R0

bb_0:
; in regs: R0
   CAPTURE R0, true
   %1 = LOAD_DOUBLE R0
   %2 = LOAD_DOUBLE R0
   %3 = ADD_NUM %1, %2
   STORE_DOUBLE R1, %3
   RETURN R1, 1i

)");
}

TEST_SUITE_END();
