// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrBuilder.h"
#include "Luau/IrAnalysis.h"
#include "Luau/IrDump.h"
#include "Luau/IrUtils.h"
#include "Luau/OptimizeConstProp.h"
#include "Luau/OptimizeDeadStore.h"
#include "Luau/OptimizeFinalX64.h"
#include "ScopedFlags.h"

#include "doctest.h"

#include <limits.h>

LUAU_FASTFLAG(DebugLuauAbortingChecks)

using namespace Luau::CodeGen;

class IrBuilderFixture
{
public:
    IrBuilderFixture()
        : build(hooks)
    {
    }

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

    void defineCfgTree(const std::vector<std::vector<uint32_t>>& successorSets)
    {
        for (const std::vector<uint32_t>& successorSet : successorSets)
        {
            build.beginBlock(build.block(IrBlockKind::Internal));

            build.function.cfg.successorsOffsets.push_back(uint32_t(build.function.cfg.successors.size()));
            build.function.cfg.successors.insert(build.function.cfg.successors.end(), successorSet.begin(), successorSet.end());
        }

        // Brute-force the predecessor list
        for (int i = 0; i < int(build.function.blocks.size()); i++)
        {
            build.function.cfg.predecessorsOffsets.push_back(uint32_t(build.function.cfg.predecessors.size()));

            for (int k = 0; k < int(build.function.blocks.size()); k++)
            {
                for (uint32_t succIdx : successors(build.function.cfg, k))
                {
                    if (succIdx == uint32_t(i))
                        build.function.cfg.predecessors.push_back(k);
                }
            }
        }

        computeCfgImmediateDominators(build.function);
        computeCfgDominanceTreeChildren(build.function);
    }

    HostIrHooks hooks;
    IrBuilder build;

    // Luau.VM headers are not accessible
    static const int tnil = 0;
    static const int tboolean = 1;
    static const int tnumber = 3;
    static const int tvector = 4;
    static const int tstring = 5;
    static const int ttable = 6;
    static const int tfunction = 7;
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
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
        IrCmd::STORE_INT, build.vmReg(19), build.inst(IrCmd::NOT_ANY, build.constTag(tnumber), build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1)))
    );
    build.inst(IrCmd::STORE_INT, build.vmReg(20), build.inst(IrCmd::NOT_ANY, build.constTag(tboolean), build.constInt(0)));
    build.inst(IrCmd::STORE_INT, build.vmReg(21), build.inst(IrCmd::NOT_ANY, build.constTag(tboolean), build.constInt(1)));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(22), build.inst(IrCmd::SIGN_NUM, build.constDouble(-4)));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
   STORE_DOUBLE R22, -1
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

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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

TEST_CASE_FIXTURE(IrBuilderFixture, "Bit32RangeReduction")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITLSHIFT_UINT, build.constInt(0xf), build.constInt(-10)));
    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITLSHIFT_UINT, build.constInt(0xf), build.constInt(140)));
    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITRSHIFT_UINT, build.constInt(0xffffff), build.constInt(-10)));
    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITRSHIFT_UINT, build.constInt(0xffffff), build.constInt(140)));
    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITARSHIFT_UINT, build.constInt(0xffffff), build.constInt(-10)));
    build.inst(IrCmd::STORE_INT, build.vmReg(10), build.inst(IrCmd::BITARSHIFT_UINT, build.constInt(0xffffff), build.constInt(140)));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_INT R10, 62914560i
   STORE_INT R10, 61440i
   STORE_INT R10, 3i
   STORE_INT R10, 4095i
   STORE_INT R10, 3i
   STORE_INT R10, 4095i
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

    CHECK("\n" + toString(build.function, IncludeUseInfo::Yes) == R"(
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

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    withTwoBlocks(
        [this](IrOp a, IrOp b)
        {
            build.inst(IrCmd::JUMP_EQ_TAG, build.constTag(tnil), build.constTag(tnil), a, b);
        }
    );

    withTwoBlocks(
        [this](IrOp a, IrOp b)
        {
            build.inst(IrCmd::JUMP_EQ_TAG, build.constTag(tnil), build.constTag(tnumber), a, b);
        }
    );

    withTwoBlocks(
        [this](IrOp a, IrOp b)
        {
            build.inst(IrCmd::JUMP_CMP_INT, build.constInt(0), build.constInt(0), build.cond(IrCondition::Equal), a, b);
        }
    );

    withTwoBlocks(
        [this](IrOp a, IrOp b)
        {
            build.inst(IrCmd::JUMP_CMP_INT, build.constInt(0), build.constInt(1), build.cond(IrCondition::Equal), a, b);
        }
    );

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    withOneBlock(
        [this](IrOp a)
        {
            build.inst(IrCmd::STORE_INT, build.vmReg(0), build.inst(IrCmd::TRY_NUM_TO_INDEX, build.constDouble(4), a));
            build.inst(IrCmd::RETURN, build.constUint(0));
        }
    );

    withOneBlock(
        [this](IrOp a)
        {
            build.inst(IrCmd::STORE_INT, build.vmReg(0), build.inst(IrCmd::TRY_NUM_TO_INDEX, build.constDouble(1.2), a));
            build.inst(IrCmd::RETURN, build.constUint(0));
        }
    );

    withOneBlock(
        [this](IrOp a)
        {
            IrOp nan = build.inst(IrCmd::DIV_NUM, build.constDouble(0.0), build.constDouble(0.0));
            build.inst(IrCmd::STORE_INT, build.vmReg(0), build.inst(IrCmd::TRY_NUM_TO_INDEX, nan, a));
            build.inst(IrCmd::RETURN, build.constUint(0));
        }
    );

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    withOneBlock(
        [this](IrOp a)
        {
            build.inst(IrCmd::CHECK_TAG, build.constTag(tnumber), build.constTag(tnumber), a);
            build.inst(IrCmd::RETURN, build.constUint(0));
        }
    );

    withOneBlock(
        [this](IrOp a)
        {
            build.inst(IrCmd::CHECK_TAG, build.constTag(tnil), build.constTag(tnumber), a);
            build.inst(IrCmd::RETURN, build.constUint(0));
        }
    );

    updateUseCounts(build.function);
    constantFold();

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    auto compareFold = [this](IrOp lhs, IrOp rhs, IrCondition cond, bool result)
    {
        IrOp instOp;
        IrInst instExpected;

        withTwoBlocks(
            [&](IrOp a, IrOp b)
            {
                IrOp nan = build.inst(IrCmd::DIV_NUM, build.constDouble(0.0), build.constDouble(0.0));
                instOp = build.inst(
                    IrCmd::JUMP_CMP_NUM, lhs.kind == IrOpKind::None ? nan : lhs, rhs.kind == IrOpKind::None ? nan : rhs, build.cond(cond), a, b
                );
                instExpected = IrInst{IrCmd::JUMP, result ? a : b};
            }
        );

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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_TAG R0, tnumber
   STORE_DOUBLE R0, 0.5
   STORE_SPLIT_TVALUE R1, tnumber, 0.5
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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

TEST_CASE_FIXTURE(IrBuilderFixture, "RememberNewTableState")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    IrOp newtable = build.inst(IrCmd::NEW_TABLE, build.constUint(16), build.constUint(32));
    build.inst(IrCmd::STORE_POINTER, build.vmReg(0), newtable);

    IrOp table = build.inst(IrCmd::LOAD_POINTER, build.vmReg(0));

    build.inst(IrCmd::CHECK_NO_METATABLE, table, fallback);
    build.inst(IrCmd::CHECK_READONLY, table, fallback);
    build.inst(IrCmd::CHECK_ARRAY_SIZE, table, build.constInt(14), fallback);

    build.inst(IrCmd::SET_TABLE, build.vmReg(1), build.vmReg(0), build.constUint(13)); // Invalidate table knowledge

    build.inst(IrCmd::CHECK_NO_METATABLE, table, fallback);
    build.inst(IrCmd::CHECK_READONLY, table, fallback);
    build.inst(IrCmd::CHECK_ARRAY_SIZE, table, build.constInt(14), fallback);

    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = NEW_TABLE 16u, 32u
   STORE_POINTER R0, %0
   SET_TABLE R1, R0, 13u
   CHECK_NO_METATABLE %0, bb_fallback_1
   CHECK_READONLY %0, bb_fallback_1
   CHECK_ARRAY_SIZE %0, 14i, bb_fallback_1
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
    build.inst(IrCmd::BARRIER_TABLE_FORWARD, table, build.vmReg(0), build.undef());
    IrOp something = build.inst(IrCmd::LOAD_POINTER, build.vmReg(2));
    build.inst(IrCmd::BARRIER_OBJ, something, build.vmReg(0), build.undef());
    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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

    build.inst(
        IrCmd::INVOKE_FASTCALL,
        build.constUint(LBF_SETMETATABLE),
        build.vmReg(1),
        build.vmReg(2),
        build.vmReg(3),
        build.undef(),
        build.constInt(3),
        build.constInt(1)
    );

    build.inst(IrCmd::CHECK_NO_METATABLE, table, fallback);
    build.inst(IrCmd::CHECK_READONLY, table, fallback);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0))); // At least R0 wasn't touched

    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_DOUBLE R0, 0.5
   %1 = LOAD_POINTER R0
   CHECK_NO_METATABLE %1, bb_fallback_1
   CHECK_READONLY %1, bb_fallback_1
   %4 = INVOKE_FASTCALL 61u, R1, R2, R3, undef, 3i, 1i
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    build.inst(IrCmd::STORE_INT, build.vmReg(1), build.constInt(5));
    IrOp value = build.inst(IrCmd::LOAD_INT, build.vmReg(1));
    build.inst(IrCmd::JUMP_CMP_INT, value, build.constInt(5), build.cond(IrCondition::Equal), trueBlock, falseBlock);

    build.beginBlock(trueBlock);
    build.inst(IrCmd::RETURN, build.constUint(1));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.constUint(2));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(4.0));
    IrOp value = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    build.inst(IrCmd::JUMP_CMP_NUM, value, build.constDouble(8.0), build.cond(IrCondition::Greater), trueBlock, falseBlock);

    build.beginBlock(trueBlock);
    build.inst(IrCmd::RETURN, build.constUint(1));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.constUint(2));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    build.inst(IrCmd::JUMP_CMP_INT, build.constInt(0), build.constInt(1), build.cond(IrCondition::Equal), block, exit1);

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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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

TEST_CASE_FIXTURE(IrBuilderFixture, "IntNumIntPeepholes")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    IrOp i1 = build.inst(IrCmd::LOAD_INT, build.vmReg(0));
    IrOp u1 = build.inst(IrCmd::LOAD_INT, build.vmReg(1));
    IrOp ni1 = build.inst(IrCmd::INT_TO_NUM, i1);
    IrOp nu1 = build.inst(IrCmd::UINT_TO_NUM, u1);
    IrOp i2 = build.inst(IrCmd::NUM_TO_INT, ni1);
    IrOp u2 = build.inst(IrCmd::NUM_TO_UINT, nu1);
    build.inst(IrCmd::STORE_INT, build.vmReg(0), i2);
    build.inst(IrCmd::STORE_INT, build.vmReg(1), u2);
    build.inst(IrCmd::RETURN, build.constUint(2));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_INT R0
   %1 = LOAD_INT R1
   STORE_INT R0, %0
   STORE_INT R1, %1
   RETURN 2u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "InvalidateReglinkVersion")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(tstring));
    IrOp tv2 = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(2));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(1), tv2);
    IrOp ft = build.inst(IrCmd::NEW_TABLE, build.constUint(0), build.constUint(0));
    build.inst(IrCmd::STORE_POINTER, build.vmReg(2), ft);
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(ttable));
    IrOp tv1 = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(1));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(0), tv1);
    IrOp tag = build.inst(IrCmd::LOAD_TAG, build.vmReg(0));
    build.inst(IrCmd::CHECK_TAG, tag, build.constTag(ttable), fallback);
    build.inst(IrCmd::RETURN, build.constUint(0));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_TAG R2, tstring
   %1 = LOAD_TVALUE R2
   STORE_TVALUE R1, %1
   %3 = NEW_TABLE 0u, 0u
   STORE_POINTER R2, %3
   STORE_TAG R2, ttable
   STORE_TVALUE R0, %1
   %8 = LOAD_TAG R0
   CHECK_TAG %8, ttable, bb_fallback_1
   RETURN 0u

bb_fallback_1:
   RETURN 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "NumericSimplifications")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);
    IrOp value = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.inst(IrCmd::SUB_NUM, value, build.constDouble(0.0)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), build.inst(IrCmd::ADD_NUM, value, build.constDouble(-0.0)));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(3), build.inst(IrCmd::MUL_NUM, value, build.constDouble(1.0)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(4), build.inst(IrCmd::MUL_NUM, value, build.constDouble(2.0)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(5), build.inst(IrCmd::MUL_NUM, value, build.constDouble(-1.0)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(6), build.inst(IrCmd::MUL_NUM, value, build.constDouble(3.0)));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(7), build.inst(IrCmd::DIV_NUM, value, build.constDouble(1.0)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(8), build.inst(IrCmd::DIV_NUM, value, build.constDouble(-1.0)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(9), build.inst(IrCmd::DIV_NUM, value, build.constDouble(32.0)));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(10), build.inst(IrCmd::DIV_NUM, value, build.constDouble(6.0)));

    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(9));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_DOUBLE R0
   STORE_DOUBLE R1, %0
   STORE_DOUBLE R2, %0
   STORE_DOUBLE R3, %0
   %7 = ADD_NUM %0, %0
   STORE_DOUBLE R4, %7
   %9 = UNM_NUM %0
   STORE_DOUBLE R5, %9
   %11 = MUL_NUM %0, 3
   STORE_DOUBLE R6, %11
   STORE_DOUBLE R7, %0
   %15 = UNM_NUM %0
   STORE_DOUBLE R8, %15
   %17 = MUL_NUM %0, 0.03125
   STORE_DOUBLE R9, %17
   %19 = DIV_NUM %0, 6
   STORE_DOUBLE R10, %19
   RETURN R1, 9i

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
    constPropInBlockChains(build);
    createLinearBlocks(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);
    createLinearBlocks(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);
    createLinearBlocks(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(1), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0)));

    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_TVALUE R0
   STORE_TVALUE R1, %0
   STORE_DOUBLE R0, 0.5
   %3 = LOAD_TVALUE R0
   STORE_TVALUE R1, %3
   STORE_TAG R0, tnumber
   STORE_SPLIT_TVALUE R1, tnumber, 0.5
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "VaridicRegisterRangeInvalidation")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(tnumber));
    build.inst(IrCmd::FALLBACK_GETVARARGS, build.constUint(0), build.vmReg(1), build.constInt(-1));
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(tnumber));
    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_TAG R2, tnumber
   FALLBACK_GETVARARGS 0u, R1, -1i
   STORE_TAG R2, tnumber
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "LoadPropagatesOnlyRightType")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    build.inst(IrCmd::STORE_INT, build.vmReg(0), build.constInt(2));
    IrOp value1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), value1);
    IrOp value2 = build.inst(IrCmd::LOAD_INT, build.vmReg(1));
    build.inst(IrCmd::STORE_INT, build.vmReg(2), value2);
    build.inst(IrCmd::RETURN, build.constUint(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_INT R0, 2i
   %1 = LOAD_DOUBLE R0
   STORE_DOUBLE R1, %1
   %3 = LOAD_INT R1
   STORE_INT R2, %3
   RETURN 0u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DuplicateHashSlotChecks")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    // This roughly corresponds to 'return t.a + t.a'
    IrOp table1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    IrOp slot1 = build.inst(IrCmd::GET_SLOT_NODE_ADDR, table1, build.constUint(3), build.vmConst(1));
    build.inst(IrCmd::CHECK_SLOT_MATCH, slot1, build.vmConst(1), fallback);
    IrOp value1 = build.inst(IrCmd::LOAD_TVALUE, slot1, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(3), value1);

    IrOp slot1b = build.inst(IrCmd::GET_SLOT_NODE_ADDR, table1, build.constUint(8), build.vmConst(1)); // This will be removed
    build.inst(IrCmd::CHECK_SLOT_MATCH, slot1b, build.vmConst(1), fallback);                           // Key will be replaced with undef here
    IrOp value1b = build.inst(IrCmd::LOAD_TVALUE, slot1b, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(4), value1b);

    IrOp a = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(3));
    IrOp b = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(4));
    IrOp sum = build.inst(IrCmd::ADD_NUM, a, b);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), sum);

    build.inst(IrCmd::RETURN, build.vmReg(2), build.constUint(1));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    // In the future, we might even see duplicate identical TValue loads go away
    // In the future, we might even see loads of different VM regs with the same value go away
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_POINTER R1
   %1 = GET_SLOT_NODE_ADDR %0, 3u, K1
   CHECK_SLOT_MATCH %1, K1, bb_fallback_1
   %3 = LOAD_TVALUE %1, 0i
   STORE_TVALUE R3, %3
   CHECK_NODE_VALUE %1, bb_fallback_1
   %7 = LOAD_TVALUE %1, 0i
   STORE_TVALUE R4, %7
   %9 = LOAD_DOUBLE R3
   %10 = LOAD_DOUBLE R4
   %11 = ADD_NUM %9, %10
   STORE_DOUBLE R2, %11
   RETURN R2, 1u

bb_fallback_1:
   RETURN R0, 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DuplicateHashSlotChecksAvoidNil")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    IrOp table1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    IrOp slot1 = build.inst(IrCmd::GET_SLOT_NODE_ADDR, table1, build.constUint(3), build.vmConst(1));
    build.inst(IrCmd::CHECK_SLOT_MATCH, slot1, build.vmConst(1), fallback);
    IrOp value1 = build.inst(IrCmd::LOAD_TVALUE, slot1, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(3), value1);

    IrOp table2 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(2));
    IrOp slot2 = build.inst(IrCmd::GET_SLOT_NODE_ADDR, table2, build.constUint(6), build.vmConst(1));
    build.inst(IrCmd::CHECK_SLOT_MATCH, slot2, build.vmConst(1), fallback);
    build.inst(IrCmd::CHECK_READONLY, table2, fallback);

    build.inst(IrCmd::STORE_TAG, build.vmReg(4), build.constTag(tnil));
    IrOp valueNil = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(4));
    build.inst(IrCmd::STORE_TVALUE, slot2, valueNil, build.constInt(0));

    // In the future, we might get to track that value became 'nil' and that fallback will be taken
    IrOp slot1b = build.inst(IrCmd::GET_SLOT_NODE_ADDR, table1, build.constUint(8), build.vmConst(1)); // This will be removed
    build.inst(IrCmd::CHECK_SLOT_MATCH, slot1b, build.vmConst(1), fallback);                           // Key will be replaced with undef here
    IrOp value1b = build.inst(IrCmd::LOAD_TVALUE, slot1b, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(3), value1b);

    IrOp slot2b = build.inst(IrCmd::GET_SLOT_NODE_ADDR, table2, build.constUint(11), build.vmConst(1)); // This will be removed
    build.inst(IrCmd::CHECK_SLOT_MATCH, slot2b, build.vmConst(1), fallback);                            // Key will be replaced with undef here
    build.inst(IrCmd::CHECK_READONLY, table2, fallback);

    build.inst(IrCmd::STORE_SPLIT_TVALUE, slot2b, build.constTag(tnumber), build.constDouble(1), build.constInt(0));

    build.inst(IrCmd::RETURN, build.vmReg(3), build.constUint(2));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constUint(2));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_POINTER R1
   %1 = GET_SLOT_NODE_ADDR %0, 3u, K1
   CHECK_SLOT_MATCH %1, K1, bb_fallback_1
   %3 = LOAD_TVALUE %1, 0i
   STORE_TVALUE R3, %3
   %5 = LOAD_POINTER R2
   %6 = GET_SLOT_NODE_ADDR %5, 6u, K1
   CHECK_SLOT_MATCH %6, K1, bb_fallback_1
   CHECK_READONLY %5, bb_fallback_1
   STORE_TAG R4, tnil
   %10 = LOAD_TVALUE R4
   STORE_TVALUE %6, %10, 0i
   CHECK_NODE_VALUE %1, bb_fallback_1
   %14 = LOAD_TVALUE %1, 0i
   STORE_TVALUE R3, %14
   CHECK_NODE_VALUE %6, bb_fallback_1
   STORE_SPLIT_TVALUE %6, tnumber, 1, 0i
   RETURN R3, 2u

bb_fallback_1:
   RETURN R1, 2u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DuplicateHashSlotChecksInvalidation")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    // This roughly corresponds to 'return t.a + t.a' with a stange GC assist in the middle
    IrOp table1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    IrOp slot1 = build.inst(IrCmd::GET_SLOT_NODE_ADDR, table1, build.constUint(3), build.vmConst(1));
    build.inst(IrCmd::CHECK_SLOT_MATCH, slot1, build.vmConst(1), fallback);
    IrOp value1 = build.inst(IrCmd::LOAD_TVALUE, slot1, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(3), value1);

    build.inst(IrCmd::CHECK_GC);

    IrOp slot1b = build.inst(IrCmd::GET_SLOT_NODE_ADDR, table1, build.constUint(8), build.vmConst(1));
    build.inst(IrCmd::CHECK_SLOT_MATCH, slot1b, build.vmConst(1), fallback);
    IrOp value1b = build.inst(IrCmd::LOAD_TVALUE, slot1b, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(4), value1b);

    IrOp a = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(3));
    IrOp b = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(4));
    IrOp sum = build.inst(IrCmd::ADD_NUM, a, b);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), sum);

    build.inst(IrCmd::RETURN, build.vmReg(2), build.constUint(1));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    // In the future, we might even see duplicate identical TValue loads go away
    // In the future, we might even see loads of different VM regs with the same value go away
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_POINTER R1
   %1 = GET_SLOT_NODE_ADDR %0, 3u, K1
   CHECK_SLOT_MATCH %1, K1, bb_fallback_1
   %3 = LOAD_TVALUE %1, 0i
   STORE_TVALUE R3, %3
   CHECK_GC
   %6 = GET_SLOT_NODE_ADDR %0, 8u, K1
   CHECK_SLOT_MATCH %6, K1, bb_fallback_1
   %8 = LOAD_TVALUE %6, 0i
   STORE_TVALUE R4, %8
   %10 = LOAD_DOUBLE R3
   %11 = LOAD_DOUBLE R4
   %12 = ADD_NUM %10, %11
   STORE_DOUBLE R2, %12
   RETURN R2, 1u

bb_fallback_1:
   RETURN R0, 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DuplicateArrayElemChecksSameIndex")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    // This roughly corresponds to 'return t[1] + t[1]'
    IrOp table1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    build.inst(IrCmd::CHECK_ARRAY_SIZE, table1, build.constInt(0), fallback);
    IrOp elem1 = build.inst(IrCmd::GET_ARR_ADDR, table1, build.constInt(0));
    IrOp value1 = build.inst(IrCmd::LOAD_TVALUE, elem1, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(3), value1);

    build.inst(IrCmd::CHECK_ARRAY_SIZE, table1, build.constInt(0), fallback); // This will be removed
    IrOp elem2 = build.inst(IrCmd::GET_ARR_ADDR, table1, build.constInt(0));  // And this will be substituted
    IrOp value1b = build.inst(IrCmd::LOAD_TVALUE, elem2, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(4), value1b);

    IrOp a = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(3));
    IrOp b = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(4));
    IrOp sum = build.inst(IrCmd::ADD_NUM, a, b);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), sum);

    build.inst(IrCmd::RETURN, build.vmReg(2), build.constUint(1));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    // In the future, we might even see duplicate identical TValue loads go away
    // In the future, we might even see loads of different VM regs with the same value go away
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_POINTER R1
   CHECK_ARRAY_SIZE %0, 0i, bb_fallback_1
   %2 = GET_ARR_ADDR %0, 0i
   %3 = LOAD_TVALUE %2, 0i
   STORE_TVALUE R3, %3
   %7 = LOAD_TVALUE %2, 0i
   STORE_TVALUE R4, %7
   %9 = LOAD_DOUBLE R3
   %10 = LOAD_DOUBLE R4
   %11 = ADD_NUM %9, %10
   STORE_DOUBLE R2, %11
   RETURN R2, 1u

bb_fallback_1:
   RETURN R0, 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DuplicateArrayElemChecksSameValue")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    // This roughly corresponds to 'return t[i] + t[i]'
    IrOp table1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    IrOp index = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(2));
    IrOp validIndex = build.inst(IrCmd::TRY_NUM_TO_INDEX, index, fallback);
    IrOp validOffset = build.inst(IrCmd::SUB_INT, validIndex, build.constInt(1));
    build.inst(IrCmd::CHECK_ARRAY_SIZE, table1, validOffset, fallback);
    IrOp elem1 = build.inst(IrCmd::GET_ARR_ADDR, table1, build.constInt(0));
    IrOp value1 = build.inst(IrCmd::LOAD_TVALUE, elem1, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(3), value1);

    IrOp validIndex2 = build.inst(IrCmd::TRY_NUM_TO_INDEX, index, fallback);
    IrOp validOffset2 = build.inst(IrCmd::SUB_INT, validIndex2, build.constInt(1));
    build.inst(IrCmd::CHECK_ARRAY_SIZE, table1, validOffset2, fallback);     // This will be removed
    IrOp elem2 = build.inst(IrCmd::GET_ARR_ADDR, table1, build.constInt(0)); // And this will be substituted
    IrOp value1b = build.inst(IrCmd::LOAD_TVALUE, elem2, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(4), value1b);

    IrOp a = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(3));
    IrOp b = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(4));
    IrOp sum = build.inst(IrCmd::ADD_NUM, a, b);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), sum);

    build.inst(IrCmd::RETURN, build.vmReg(2), build.constUint(1));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    // In the future, we might even see duplicate identical TValue loads go away
    // In the future, we might even see loads of different VM regs with the same value go away
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_POINTER R1
   %1 = LOAD_DOUBLE R2
   %2 = TRY_NUM_TO_INDEX %1, bb_fallback_1
   %3 = SUB_INT %2, 1i
   CHECK_ARRAY_SIZE %0, %3, bb_fallback_1
   %5 = GET_ARR_ADDR %0, 0i
   %6 = LOAD_TVALUE %5, 0i
   STORE_TVALUE R3, %6
   %12 = LOAD_TVALUE %5, 0i
   STORE_TVALUE R4, %12
   %14 = LOAD_DOUBLE R3
   %15 = LOAD_DOUBLE R4
   %16 = ADD_NUM %14, %15
   STORE_DOUBLE R2, %16
   RETURN R2, 1u

bb_fallback_1:
   RETURN R0, 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DuplicateArrayElemChecksLowerIndex")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    // This roughly corresponds to 'return t[2] + t[1]'
    IrOp table1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    build.inst(IrCmd::CHECK_ARRAY_SIZE, table1, build.constInt(1), fallback);
    IrOp elem1 = build.inst(IrCmd::GET_ARR_ADDR, table1, build.constInt(1));
    IrOp value1 = build.inst(IrCmd::LOAD_TVALUE, elem1, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(3), value1);

    build.inst(IrCmd::CHECK_ARRAY_SIZE, table1, build.constInt(0), fallback); // This will be removed
    IrOp elem2 = build.inst(IrCmd::GET_ARR_ADDR, table1, build.constInt(0));
    IrOp value1b = build.inst(IrCmd::LOAD_TVALUE, elem2, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(4), value1b);

    IrOp a = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(3));
    IrOp b = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(4));
    IrOp sum = build.inst(IrCmd::ADD_NUM, a, b);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), sum);

    build.inst(IrCmd::RETURN, build.vmReg(2), build.constUint(1));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_POINTER R1
   CHECK_ARRAY_SIZE %0, 1i, bb_fallback_1
   %2 = GET_ARR_ADDR %0, 1i
   %3 = LOAD_TVALUE %2, 0i
   STORE_TVALUE R3, %3
   %6 = GET_ARR_ADDR %0, 0i
   %7 = LOAD_TVALUE %6, 0i
   STORE_TVALUE R4, %7
   %9 = LOAD_DOUBLE R3
   %10 = LOAD_DOUBLE R4
   %11 = ADD_NUM %9, %10
   STORE_DOUBLE R2, %11
   RETURN R2, 1u

bb_fallback_1:
   RETURN R0, 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DuplicateArrayElemChecksInvalidations")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    // This roughly corresponds to 'return t[1] + t[1]' with a strange table.insert in the middle
    IrOp table1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    build.inst(IrCmd::CHECK_ARRAY_SIZE, table1, build.constInt(0), fallback);
    IrOp elem1 = build.inst(IrCmd::GET_ARR_ADDR, table1, build.constInt(0));
    IrOp value1 = build.inst(IrCmd::LOAD_TVALUE, elem1, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(3), value1);

    build.inst(IrCmd::TABLE_SETNUM, table1, build.constInt(2));

    build.inst(IrCmd::CHECK_ARRAY_SIZE, table1, build.constInt(0), fallback); // This will be removed
    IrOp elem2 = build.inst(IrCmd::GET_ARR_ADDR, table1, build.constInt(0));  // And this will be substituted
    IrOp value1b = build.inst(IrCmd::LOAD_TVALUE, elem2, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(4), value1b);

    IrOp a = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(3));
    IrOp b = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(4));
    IrOp sum = build.inst(IrCmd::ADD_NUM, a, b);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), sum);

    build.inst(IrCmd::RETURN, build.vmReg(2), build.constUint(1));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_POINTER R1
   CHECK_ARRAY_SIZE %0, 0i, bb_fallback_1
   %2 = GET_ARR_ADDR %0, 0i
   %3 = LOAD_TVALUE %2, 0i
   STORE_TVALUE R3, %3
   %5 = TABLE_SETNUM %0, 2i
   CHECK_ARRAY_SIZE %0, 0i, bb_fallback_1
   %7 = GET_ARR_ADDR %0, 0i
   %8 = LOAD_TVALUE %7, 0i
   STORE_TVALUE R4, %8
   %10 = LOAD_DOUBLE R3
   %11 = LOAD_DOUBLE R4
   %12 = ADD_NUM %10, %11
   STORE_DOUBLE R2, %12
   RETURN R2, 1u

bb_fallback_1:
   RETURN R0, 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "ArrayElemChecksNegativeIndex")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    // This roughly corresponds to 'return t[1] + t[0]'
    IrOp table1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    build.inst(IrCmd::CHECK_ARRAY_SIZE, table1, build.constInt(0), fallback);
    IrOp elem1 = build.inst(IrCmd::GET_ARR_ADDR, table1, build.constInt(0));
    IrOp value1 = build.inst(IrCmd::LOAD_TVALUE, elem1, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(3), value1);

    build.inst(IrCmd::CHECK_ARRAY_SIZE, table1, build.constInt(-1), fallback); // This will jump directly to fallback
    IrOp elem2 = build.inst(IrCmd::GET_ARR_ADDR, table1, build.constInt(-1));
    IrOp value1b = build.inst(IrCmd::LOAD_TVALUE, elem2, build.constInt(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(4), value1b);

    IrOp a = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(3));
    IrOp b = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(4));
    IrOp sum = build.inst(IrCmd::ADD_NUM, a, b);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), sum);

    build.inst(IrCmd::RETURN, build.vmReg(2), build.constUint(1));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_POINTER R1
   CHECK_ARRAY_SIZE %0, 0i, bb_fallback_1
   %2 = GET_ARR_ADDR %0, 0i
   %3 = LOAD_TVALUE %2, 0i
   STORE_TVALUE R3, %3
   JUMP bb_fallback_1

bb_fallback_1:
   RETURN R0, 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DuplicateBufferLengthChecks")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    IrOp sourceBuf = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0));

    build.inst(IrCmd::STORE_TVALUE, build.vmReg(2), sourceBuf);
    IrOp buffer1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(2));
    build.inst(IrCmd::CHECK_BUFFER_LEN, buffer1, build.constInt(12), build.constInt(4), fallback);
    build.inst(IrCmd::BUFFER_WRITEI32, buffer1, build.constInt(12), build.constInt(32));

    // Now with lower index, should be removed
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(2), sourceBuf);
    IrOp buffer2 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(2));
    build.inst(IrCmd::CHECK_BUFFER_LEN, buffer2, build.constInt(8), build.constInt(4), fallback);
    build.inst(IrCmd::BUFFER_WRITEI32, buffer2, build.constInt(8), build.constInt(30));

    // Now with higher index, should raise the initial check bound
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(2), sourceBuf);
    IrOp buffer3 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(2));
    build.inst(IrCmd::CHECK_BUFFER_LEN, buffer3, build.constInt(16), build.constInt(4), fallback);
    build.inst(IrCmd::BUFFER_WRITEI32, buffer3, build.constInt(16), build.constInt(60));

    // Now with different access size, should not reuse previous checks (can be improved in the future)
    build.inst(IrCmd::CHECK_BUFFER_LEN, buffer3, build.constInt(16), build.constInt(2), fallback);
    build.inst(IrCmd::BUFFER_WRITEI16, buffer3, build.constInt(16), build.constInt(55));

    // Now with same, but unknown index value
    IrOp index = build.inst(IrCmd::LOAD_INT, build.vmReg(1));
    build.inst(IrCmd::CHECK_BUFFER_LEN, buffer3, index, build.constInt(2), fallback);
    build.inst(IrCmd::BUFFER_WRITEI16, buffer3, index, build.constInt(1));
    build.inst(IrCmd::CHECK_BUFFER_LEN, buffer3, index, build.constInt(2), fallback);
    build.inst(IrCmd::BUFFER_WRITEI16, buffer3, index, build.constInt(2));

    build.inst(IrCmd::RETURN, build.vmReg(1), build.constUint(1));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_TVALUE R0
   STORE_TVALUE R2, %0
   %2 = LOAD_POINTER R2
   CHECK_BUFFER_LEN %2, 16i, 4i, bb_fallback_1
   BUFFER_WRITEI32 %2, 12i, 32i
   BUFFER_WRITEI32 %2, 8i, 30i
   BUFFER_WRITEI32 %2, 16i, 60i
   CHECK_BUFFER_LEN %2, 16i, 2i, bb_fallback_1
   BUFFER_WRITEI16 %2, 16i, 55i
   %15 = LOAD_INT R1
   CHECK_BUFFER_LEN %2, %15, 2i, bb_fallback_1
   BUFFER_WRITEI16 %2, %15, 1i
   BUFFER_WRITEI16 %2, %15, 2i
   RETURN R1, 1u

bb_fallback_1:
   RETURN R0, 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "BufferLenghtChecksNegativeIndex")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(block);

    IrOp sourceBuf = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0));

    build.inst(IrCmd::STORE_TVALUE, build.vmReg(2), sourceBuf);
    IrOp buffer1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(2));
    build.inst(IrCmd::CHECK_BUFFER_LEN, buffer1, build.constInt(-4), build.constInt(4), fallback);
    build.inst(IrCmd::BUFFER_WRITEI32, buffer1, build.constInt(-4), build.constInt(32));
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constUint(1));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_TVALUE R0
   STORE_TVALUE R2, %0
   JUMP bb_fallback_1

bb_fallback_1:
   RETURN R0, 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "TagVectorSkipErrorFix")
{
    IrOp block = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    IrOp a = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0));
    IrOp b = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(1));

    IrOp mul = build.inst(IrCmd::TAG_VECTOR, build.inst(IrCmd::MUL_VEC, a, b));

    IrOp t1 = build.inst(IrCmd::TAG_VECTOR, build.inst(IrCmd::ADD_VEC, mul, mul));
    IrOp t2 = build.inst(IrCmd::TAG_VECTOR, build.inst(IrCmd::SUB_VEC, mul, mul));

    IrOp t3 = build.inst(IrCmd::TAG_VECTOR, build.inst(IrCmd::DIV_VEC, t1, build.inst(IrCmd::UNM_VEC, t2)));

    build.inst(IrCmd::STORE_TVALUE, build.vmReg(0), t3);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constUint(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::Yes) == R"(
bb_0:                                                       ; useCount: 0
   %0 = LOAD_TVALUE R0                                       ; useCount: 1, lastUse: %0
   %1 = LOAD_TVALUE R1                                       ; useCount: 1, lastUse: %0
   %2 = MUL_VEC %0, %1                                       ; useCount: 4, lastUse: %0
   %4 = ADD_VEC %2, %2                                       ; useCount: 1, lastUse: %0
   %6 = SUB_VEC %2, %2                                       ; useCount: 1, lastUse: %0
   %8 = UNM_VEC %6                                           ; useCount: 1, lastUse: %0
   %9 = DIV_VEC %4, %8                                       ; useCount: 1, lastUse: %0
   %10 = TAG_VECTOR %9                                       ; useCount: 1, lastUse: %0
   STORE_TVALUE R0, %10                                      ; %11
   RETURN R0, 1u                                             ; %12

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "ForgprepInvalidation")
{
    IrOp block = build.block(IrBlockKind::Internal);
    IrOp followup = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    IrOp tbl = build.inst(IrCmd::LOAD_POINTER, build.vmReg(0));
    build.inst(IrCmd::CHECK_READONLY, tbl, build.vmExit(1));

    build.inst(IrCmd::FALLBACK_FORGPREP, build.constUint(2), build.vmReg(1), followup);

    build.beginBlock(followup);
    build.inst(IrCmd::CHECK_READONLY, tbl, build.vmExit(2));

    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(3));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; successors: bb_1
; in regs: R0, R1, R2, R3
; out regs: R1, R2, R3
   %0 = LOAD_POINTER R0
   CHECK_READONLY %0, exit(1)
   FALLBACK_FORGPREP 2u, R1, bb_1

bb_1:
; predecessors: bb_0
; in regs: R1, R2, R3
   CHECK_READONLY %0, exit(2)
   RETURN R1, 3i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FastCallEffects1")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::FASTCALL, build.constUint(LBF_MATH_FREXP), build.vmReg(1), build.vmReg(2), build.constInt(2));
    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(1)), build.constTag(tnumber), build.vmExit(1));
    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(2)), build.constTag(tnumber), build.vmExit(1));
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(2));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; in regs: R2
   FASTCALL 14u, R1, R2, 2i
   RETURN R1, 2i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FastCallEffects2")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::FASTCALL, build.constUint(LBF_MATH_MODF), build.vmReg(1), build.vmReg(2), build.constInt(1));
    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(1)), build.constTag(tnumber), build.vmExit(1));
    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(2)), build.constTag(tnumber), build.vmExit(1));
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(2));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; in regs: R2
   FASTCALL 20u, R1, R2, 1i
   %3 = LOAD_TAG R2
   CHECK_TAG %3, tnumber, exit(1)
   RETURN R1, 2i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "InferNumberTagFromLimitedContext")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(2.0));
    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(0)), build.constTag(ttable), build.vmExit(1));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(1), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0)));
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_DOUBLE R0, 2
   JUMP exit(1)

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DoNotProduceInvalidSplitStore1")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_INT, build.vmReg(0), build.constInt(1));
    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(0)), build.constTag(ttable), build.vmExit(1));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(1), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0)));
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_INT R0, 1i
   %1 = LOAD_TAG R0
   CHECK_TAG %1, ttable, exit(1)
   %3 = LOAD_TVALUE R0
   STORE_TVALUE R1, %3
   RETURN R1, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DoNotProduceInvalidSplitStore2")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_INT, build.vmReg(0), build.constInt(1));
    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(0)), build.constTag(tnumber), build.vmExit(1));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(1), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0)));
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_INT R0, 1i
   %1 = LOAD_TAG R0
   CHECK_TAG %1, tnumber, exit(1)
   %3 = LOAD_TVALUE R0
   STORE_TVALUE R1, %3
   RETURN R1, 1i

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

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
        IrCmd::INVOKE_FASTCALL,
        build.constUint(0),
        build.vmReg(0),
        build.vmReg(1),
        build.vmReg(2),
        build.undef(),
        build.constInt(-1),
        build.constInt(-1)
    );
    build.inst(IrCmd::ADJUST_STACK_TO_REG, build.vmReg(0), results);
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(exit);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(-1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; successors: bb_1
; out regs: R0...
   FALLBACK_GETVARARGS 0u, R1, -1i
   %1 = INVOKE_FASTCALL 0u, R0, R1, R2, undef, -1i, -1i
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

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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

TEST_CASE_FIXTURE(IrBuilderFixture, "ForgprepImplicitUse")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp direct = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Internal);
    IrOp exit = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(1.0));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), build.constDouble(10.0));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(3), build.constDouble(1.0));
    IrOp tag = build.inst(IrCmd::LOAD_TAG, build.vmReg(0));
    build.inst(IrCmd::JUMP_EQ_TAG, tag, build.constTag(tnumber), direct, fallback);

    build.beginBlock(direct);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    build.beginBlock(fallback);
    build.inst(IrCmd::FALLBACK_FORGPREP, build.constUint(0), build.vmReg(1), exit);

    build.beginBlock(exit);
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(3));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; successors: bb_1, bb_2
; in regs: R0
; out regs: R0, R1, R2, R3
   STORE_DOUBLE R1, 1
   STORE_DOUBLE R2, 10
   STORE_DOUBLE R3, 1
   %3 = LOAD_TAG R0
   JUMP_EQ_TAG %3, tnumber, bb_1, bb_2

bb_1:
; predecessors: bb_0
; in regs: R0
   RETURN R0, 1i

bb_2:
; predecessors: bb_0
; successors: bb_3
; in regs: R1, R2, R3
; out regs: R1, R2, R3
   FALLBACK_FORGPREP 0u, R1, bb_3

bb_3:
; predecessors: bb_2
; in regs: R1, R2, R3
   RETURN R1, 3i

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

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; in regs: R0, R1
   SET_TABLE R0, R1, 1u
   RETURN R0, 1i

)");
}

// 'A Simple, Fast Dominance Algorithm' [Keith D. Cooper, et al]. Figure 2.
TEST_CASE_FIXTURE(IrBuilderFixture, "DominanceVerification1")
{
    defineCfgTree({{1, 2}, {3}, {4}, {4}, {3}});

    CHECK(build.function.cfg.idoms == std::vector<uint32_t>{{~0u, 0, 0, 0, 0}});
}

// 'A Linear Time Algorithm for Placing Phi-Nodes' [Vugranam C.Sreedhar]. Figure 1.
TEST_CASE_FIXTURE(IrBuilderFixture, "DominanceVerification2")
{
    defineCfgTree({{1, 16}, {2, 3, 4}, {4, 7}, {9}, {5}, {6}, {2, 8}, {8}, {7, 15}, {10, 11}, {12}, {12}, {13}, {3, 14, 15}, {12}, {16}, {}});

    CHECK(build.function.cfg.idoms == std::vector<uint32_t>{~0u, 0, 1, 1, 1, 4, 5, 1, 1, 3, 9, 9, 9, 12, 13, 1, 0});
}

// 'A Linear Time Algorithm for Placing Phi-Nodes' [Vugranam C.Sreedhar]. Figure 4.
TEST_CASE_FIXTURE(IrBuilderFixture, "DominanceVerification3")
{
    defineCfgTree({{1, 2}, {3}, {3, 4}, {5}, {5, 6}, {7}, {7}, {}});

    CHECK(build.function.cfg.idoms == std::vector<uint32_t>{~0u, 0, 0, 0, 2, 0, 4, 0});
}

// 'Static Single Assignment Book' Figure 4.1
TEST_CASE_FIXTURE(IrBuilderFixture, "DominanceVerification4")
{
    defineCfgTree({{1}, {2, 10}, {3, 7}, {4}, {5}, {4, 6}, {1}, {8}, {5, 9}, {7}, {}});

    IdfContext ctx;

    computeIteratedDominanceFrontierForDefs(ctx, build.function, {0, 2, 3, 6}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10});
    CHECK(ctx.idf == std::vector<uint32_t>{1, 4, 5});
}

// 'Static Single Assignment Book' Figure 4.5
TEST_CASE_FIXTURE(IrBuilderFixture, "DominanceVerification4")
{
    defineCfgTree({{1}, {2}, {3, 7}, {4, 5}, {6}, {6}, {8}, {8}, {9}, {10, 11}, {11}, {9, 12}, {2}});

    IdfContext ctx;

    computeIteratedDominanceFrontierForDefs(ctx, build.function, {4, 5, 7, 12}, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12});
    CHECK(ctx.idf == std::vector<uint32_t>{2, 6, 8});

    // Pruned form, when variable is only live-in in limited set of blocks
    computeIteratedDominanceFrontierForDefs(ctx, build.function, {4, 5, 7, 12}, {6, 8, 9});
    CHECK(ctx.idf == std::vector<uint32_t>{6, 8});
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
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
    build.inst(IrCmd::CAPTURE, build.vmReg(0), build.constUint(1));
    IrOp op1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp op2 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp sum = build.inst(IrCmd::ADD_NUM, op1, op2);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), sum);
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
; captured regs: R0

bb_0:
; in regs: R0
   CAPTURE R0, 1u
   %1 = LOAD_DOUBLE R0
   %2 = LOAD_DOUBLE R0
   %3 = ADD_NUM %1, %2
   STORE_DOUBLE R1, %3
   RETURN R1, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "NoDeadLoadReuse")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp op1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp op1i = build.inst(IrCmd::NUM_TO_INT, op1);
    IrOp res = build.inst(IrCmd::BITAND_UINT, op1i, build.constInt(0));
    IrOp resd = build.inst(IrCmd::INT_TO_NUM, res);
    IrOp op2 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp sum = build.inst(IrCmd::ADD_NUM, resd, op2);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), sum);
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %4 = LOAD_DOUBLE R0
   %5 = ADD_NUM 0, %4
   STORE_DOUBLE R1, %5
   RETURN R1, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "NoDeadValueReuse")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp op1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp op1i = build.inst(IrCmd::NUM_TO_INT, op1);
    IrOp res = build.inst(IrCmd::BITAND_UINT, op1i, build.constInt(0));
    IrOp op2i = build.inst(IrCmd::NUM_TO_INT, op1);
    IrOp sum = build.inst(IrCmd::ADD_INT, res, op2i);
    IrOp resd = build.inst(IrCmd::INT_TO_NUM, sum);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), resd);
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_DOUBLE R0
   %3 = NUM_TO_INT %0
   %4 = ADD_INT 0i, %3
   %5 = INT_TO_NUM %4
   STORE_DOUBLE R1, %5
   RETURN R1, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "TValueLoadToSplitStore")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);

    build.beginBlock(entry);
    IrOp op1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp op1v2 = build.inst(IrCmd::ADD_NUM, op1, build.constDouble(4.0));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), op1v2);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));

    // Check that this TValue store will be replaced by a split store
    IrOp tv = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(1));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(2), tv);

    // Check that tag and value can be extracted from R2 now (removing the fallback)
    IrOp tag2 = build.inst(IrCmd::LOAD_TAG, build.vmReg(2));
    build.inst(IrCmd::CHECK_TAG, tag2, build.constTag(tnumber), fallback);
    IrOp op2 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(2));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(3), op2);
    build.inst(IrCmd::STORE_TAG, build.vmReg(3), build.constTag(tnumber));

    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(1));

    build.beginBlock(fallback);
    build.inst(IrCmd::RETURN, build.vmReg(2), build.constInt(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_DOUBLE R0
   %1 = ADD_NUM %0, 4
   STORE_DOUBLE R1, %1
   STORE_TAG R1, tnumber
   STORE_SPLIT_TVALUE R2, tnumber, %1
   STORE_DOUBLE R3, %1
   STORE_TAG R3, tnumber
   RETURN R1, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "TagStoreUpdatesValueVersion")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);

    IrOp op1 = build.inst(IrCmd::LOAD_POINTER, build.vmReg(0));
    build.inst(IrCmd::STORE_POINTER, build.vmReg(1), op1);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tstring));

    IrOp str = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    build.inst(IrCmd::STORE_POINTER, build.vmReg(2), str);
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(tstring));

    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(1));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %0 = LOAD_POINTER R0
   STORE_POINTER R1, %0
   STORE_TAG R1, tstring
   STORE_POINTER R2, %0
   STORE_TAG R2, tstring
   RETURN R1, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "TagStoreUpdatesSetUpval")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);

    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(0.5));

    build.inst(IrCmd::SET_UPVALUE, build.vmUpvalue(0), build.vmReg(0), build.undef());

    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(0));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_TAG R0, tnumber
   STORE_DOUBLE R0, 0.5
   SET_UPVALUE U0, R0, tnumber
   RETURN R0, 0i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "TagSelfEqualityCheckRemoval")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp trueBlock = build.block(IrBlockKind::Internal);
    IrOp falseBlock = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);

    IrOp tag1 = build.inst(IrCmd::LOAD_TAG, build.vmReg(0));
    IrOp tag2 = build.inst(IrCmd::LOAD_TAG, build.vmReg(0));
    build.inst(IrCmd::JUMP_EQ_TAG, tag1, tag2, trueBlock, falseBlock);

    build.beginBlock(trueBlock);
    build.inst(IrCmd::RETURN, build.constUint(1));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.constUint(2));

    updateUseCounts(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   JUMP bb_1

bb_1:
   RETURN 1u

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "TaggedValuePropagationIntoTvalueChecksRegisterVersion")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp a1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(0));
    IrOp b1 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    IrOp sum1 = build.inst(IrCmd::ADD_NUM, a1, b1);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(7), sum1);
    build.inst(IrCmd::STORE_TAG, build.vmReg(7), build.constTag(tnumber));

    IrOp a2 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(2));
    IrOp b2 = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(3));
    IrOp sum2 = build.inst(IrCmd::ADD_NUM, a2, b2);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(8), sum2);
    build.inst(IrCmd::STORE_TAG, build.vmReg(8), build.constTag(tnumber));

    IrOp old7 = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(7), build.constInt(0), build.constTag(tnumber));
    IrOp old8 = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(8), build.constInt(0), build.constTag(tnumber));

    build.inst(IrCmd::STORE_TVALUE, build.vmReg(8), old7); // Invalidate R8
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(9), old8); // Old R8 cannot be substituted as it was invalidated

    build.inst(IrCmd::RETURN, build.vmReg(8), build.constInt(2));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; in regs: R0, R1, R2, R3
   %0 = LOAD_DOUBLE R0
   %1 = LOAD_DOUBLE R1
   %2 = ADD_NUM %0, %1
   STORE_DOUBLE R7, %2
   STORE_TAG R7, tnumber
   %5 = LOAD_DOUBLE R2
   %6 = LOAD_DOUBLE R3
   %7 = ADD_NUM %5, %6
   STORE_DOUBLE R8, %7
   STORE_TAG R8, tnumber
   %11 = LOAD_TVALUE R8, 0i, tnumber
   STORE_SPLIT_TVALUE R8, tnumber, %2
   STORE_TVALUE R9, %11
   RETURN R8, 2i

)");
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("DeadStoreRemoval");

TEST_CASE_FIXTURE(IrBuilderFixture, "SimpleDoubleStore")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(1.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(2.0)); // Should remove previous store

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), build.constDouble(1.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(tnumber));
    build.inst(IrCmd::STORE_INT, build.vmReg(2), build.constInt(4));
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(tboolean)); // Should remove previous store of different type

    build.inst(IrCmd::STORE_TAG, build.vmReg(3), build.constTag(tnil));
    build.inst(IrCmd::STORE_TAG, build.vmReg(3), build.constTag(tnumber));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(3), build.constDouble(4.0));

    build.inst(IrCmd::STORE_TAG, build.vmReg(4), build.constTag(tnil));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(4), build.constDouble(1.0));
    build.inst(IrCmd::STORE_SPLIT_TVALUE, build.vmReg(4), build.constTag(tnumber), build.constDouble(2.0)); // Should remove two previous stores

    IrOp someTv = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(5), build.constTag(tnil));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(5), build.constDouble(1.0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(5), someTv); // Should remove two previous stores

    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(5));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; in regs: R0
   STORE_SPLIT_TVALUE R1, tnumber, 2
   STORE_SPLIT_TVALUE R2, tboolean, 4i
   STORE_TAG R3, tnumber
   STORE_DOUBLE R3, 4
   STORE_SPLIT_TVALUE R4, tnumber, 2
   %13 = LOAD_TVALUE R0
   STORE_TVALUE R5, %13
   RETURN R1, 5i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "UnusedAtReturn")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(1.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::STORE_INT, build.vmReg(2), build.constInt(4));
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(tboolean));
    build.inst(IrCmd::STORE_SPLIT_TVALUE, build.vmReg(4), build.constTag(tnumber), build.constDouble(2.0));

    IrOp someTv = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(0));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(5), someTv);

    build.inst(IrCmd::STORE_TAG, build.vmReg(6), build.constTag(tnil));

    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; in regs: R0
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "UnusedAtReturnPartial")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(1.0));
    build.inst(IrCmd::STORE_INT, build.vmReg(2), build.constInt(4));
    build.inst(IrCmd::STORE_TAG, build.vmReg(3), build.constTag(tnumber));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    // Partial stores cannot be removed, even if unused
    // Existance of an unpaired partial store means that the other valid part is a block live in (even if not present is this test)
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; in regs: R0
   STORE_DOUBLE R1, 1
   STORE_INT R2, 4i
   STORE_TAG R3, tnumber
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "HiddenPointerUse1")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp somePtr = build.inst(IrCmd::LOAD_POINTER, build.vmReg(0));
    build.inst(IrCmd::STORE_POINTER, build.vmReg(1), somePtr);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(ttable));
    build.inst(IrCmd::CALL, build.vmReg(2), build.constInt(0), build.constInt(1));
    build.inst(IrCmd::RETURN, build.vmReg(2), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; in regs: R0, R2
   %0 = LOAD_POINTER R0
   STORE_POINTER R1, %0
   STORE_TAG R1, ttable
   CALL R2, 0i, 1i
   RETURN R2, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "HiddenPointerUse2")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp somePtrA = build.inst(IrCmd::LOAD_POINTER, build.vmReg(0));
    build.inst(IrCmd::STORE_POINTER, build.vmReg(1), somePtrA);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(ttable));
    build.inst(IrCmd::CALL, build.vmReg(2), build.constInt(0), build.constInt(1));
    IrOp somePtrB = build.inst(IrCmd::LOAD_POINTER, build.vmReg(2));
    build.inst(IrCmd::STORE_POINTER, build.vmReg(1), somePtrB);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(ttable));
    build.inst(IrCmd::RETURN, build.vmReg(2), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    // Stores to pointers can be safely removed at 'return' point, but have to preserved for any GC assist trigger (such as a call)
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; in regs: R0, R2
   %0 = LOAD_POINTER R0
   STORE_POINTER R1, %0
   STORE_TAG R1, ttable
   CALL R2, 0i, 1i
   RETURN R2, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "HiddenPointerUse3")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp somePtrA = build.inst(IrCmd::LOAD_POINTER, build.vmReg(0));
    build.inst(IrCmd::STORE_POINTER, build.vmReg(1), somePtrA);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(ttable));
    IrOp someTv = build.inst(IrCmd::LOAD_TVALUE, build.vmReg(2));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(1), someTv);
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    // Stores to pointers can be safely removed if there are no potential implicit uses by any GC assists
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; in regs: R0, R2
   %3 = LOAD_TVALUE R2
   STORE_TVALUE R1, %3
   RETURN R1, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "HiddenPointerUse4")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(1.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::CHECK_GC);
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnil));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    // It is important for tag overwrite to TNIL to kill not only the previous tag store, but the value as well
    // This is important in a following scenario:
    // - R0 might have been a GCO on entry to bb_0
    // - R0 is overwritten by a number
    // - Stack is visited by GC assist
    // - R0 is overwritten by nil
    // If only number tag write would have been killed, there will be a GCO tag with a double value on stack
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   CHECK_GC
   STORE_TAG R0, tnil
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "PartialVsFullStoresWithRecombination")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(1.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(0), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(1)));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_SPLIT_TVALUE R0, tnumber, 1
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "IgnoreFastcallAdjustment")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(-1.0));
    build.inst(IrCmd::ADJUST_STACK_TO_REG, build.vmReg(1), build.constInt(1));
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(1.0));
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   ADJUST_STACK_TO_REG R1, 1i
   STORE_SPLIT_TVALUE R1, tnumber, 1
   RETURN R1, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "JumpImplicitLiveOut")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp next = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(1.0));
    build.inst(IrCmd::JUMP, next);

    build.beginBlock(next);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(1.0));
    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    // Even though bb_0 doesn't have R1 as a live out, chain optimization used the knowledge of those writes happening to optimize duplicate stores
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; successors: bb_1
   STORE_TAG R1, tnumber
   STORE_DOUBLE R1, 1
   JUMP bb_1

bb_1:
; predecessors: bb_0
   RETURN R1, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "KeepCapturedRegisterStores")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::CAPTURE, build.vmReg(1), build.constUint(1));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(1.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::DO_ARITH, build.vmReg(0), build.vmReg(2), build.vmReg(3), build.constInt(0));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(-1.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::DO_ARITH, build.vmReg(1), build.vmReg(4), build.vmReg(5), build.constInt(0));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(2));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    // Captured registers may be modified from called user functions (plain or hidden in metamethods)
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
; captured regs: R1

bb_0:
; in regs: R1, R2, R3, R4, R5
   CAPTURE R1, 1u
   STORE_DOUBLE R1, 1
   STORE_TAG R1, tnumber
   DO_ARITH R0, R2, R3, 0i
   STORE_DOUBLE R1, -1
   STORE_TAG R1, tnumber
   DO_ARITH R1, R4, R5, 0i
   RETURN R0, 2i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "StoreCannotBeReplacedWithCheck")
{
    ScopedFastFlag debugLuauAbortingChecks{FFlag::DebugLuauAbortingChecks, true};

    IrOp block = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);
    IrOp last = build.block(IrBlockKind::Internal);

    build.beginBlock(block);

    IrOp ptr = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));

    build.inst(IrCmd::STORE_POINTER, build.vmReg(2), ptr);
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(ttable));

    build.inst(IrCmd::CHECK_READONLY, ptr, fallback);

    build.inst(IrCmd::STORE_POINTER, build.vmReg(2), build.inst(IrCmd::LOAD_POINTER, build.vmReg(0)));
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(ttable));

    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(tnil));

    build.inst(IrCmd::JUMP, last);

    build.beginBlock(fallback);
    IrOp fallbackPtr = build.inst(IrCmd::LOAD_POINTER, build.vmReg(1));
    build.inst(IrCmd::STORE_POINTER, build.vmReg(2), fallbackPtr);
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(ttable));
    build.inst(IrCmd::CHECK_GC);
    build.inst(IrCmd::JUMP, last);

    build.beginBlock(last);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(3));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; successors: bb_fallback_1, bb_2
; in regs: R0, R1
; out regs: R0, R1, R2
   %0 = LOAD_POINTER R1
   CHECK_READONLY %0, bb_fallback_1
   STORE_TAG R2, tnil
   JUMP bb_2

bb_fallback_1:
; predecessors: bb_0
; successors: bb_2
; in regs: R0, R1
; out regs: R0, R1, R2
   %9 = LOAD_POINTER R1
   STORE_POINTER R2, %9
   STORE_TAG R2, ttable
   CHECK_GC
   JUMP bb_2

bb_2:
; predecessors: bb_0, bb_fallback_1
; in regs: R0, R1, R2
   RETURN R0, 3i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FullStoreHasToBeObservableFromFallbacks")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);
    IrOp last = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_POINTER, build.vmReg(1), build.inst(IrCmd::NEW_TABLE, build.constUint(16), build.constUint(32)));
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(ttable));
    build.inst(IrCmd::CHECK_SAFE_ENV, fallback);
    build.inst(IrCmd::STORE_POINTER, build.vmReg(1), build.inst(IrCmd::NEW_TABLE, build.constUint(16), build.constUint(32)));
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(ttable));
    build.inst(IrCmd::JUMP, last);

    build.beginBlock(fallback);
    build.inst(IrCmd::CHECK_GC);
    build.inst(IrCmd::STORE_SPLIT_TVALUE, build.vmReg(1), build.constTag(tnumber), build.constDouble(1.0));
    build.inst(IrCmd::JUMP, last);

    build.beginBlock(last);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(2));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    // Even though R1 is not live in of the fallback, stack state cannot be left in a partial store state
    // Either tag+pointer store should both remain before the guard, or they both have to be made after
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; successors: bb_fallback_1, bb_2
; in regs: R0
; out regs: R0, R1
   CHECK_SAFE_ENV bb_fallback_1
   %4 = NEW_TABLE 16u, 32u
   STORE_SPLIT_TVALUE R1, ttable, %4
   JUMP bb_2

bb_fallback_1:
; predecessors: bb_0
; successors: bb_2
; in regs: R0
; out regs: R0, R1
   CHECK_GC
   STORE_SPLIT_TVALUE R1, tnumber, 1
   JUMP bb_2

bb_2:
; predecessors: bb_0, bb_fallback_1
; in regs: R0, R1
   RETURN R0, 2i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FullStoreHasToBeObservableFromFallbacks2")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);
    IrOp last = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber)); // Tag store unpaired to a visible value store
    build.inst(IrCmd::CHECK_SAFE_ENV, fallback);
    build.inst(IrCmd::STORE_TVALUE, build.vmReg(1), build.inst(IrCmd::LOAD_TVALUE, build.vmReg(2)));
    build.inst(IrCmd::JUMP, last);

    build.beginBlock(fallback);
    build.inst(IrCmd::CHECK_GC);
    build.inst(IrCmd::STORE_SPLIT_TVALUE, build.vmReg(1), build.constTag(tnumber), build.constDouble(1.0));
    build.inst(IrCmd::JUMP, last);

    build.beginBlock(last);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(2));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    // If table tag store at the start is removed, GC assists in the fallback can observe value with a wrong tag
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; successors: bb_fallback_1, bb_2
; in regs: R0, R2
; out regs: R0, R1
   STORE_TAG R1, tnumber
   CHECK_SAFE_ENV bb_fallback_1
   %2 = LOAD_TVALUE R2
   STORE_TVALUE R1, %2
   JUMP bb_2

bb_fallback_1:
; predecessors: bb_0
; successors: bb_2
; in regs: R0
; out regs: R0, R1
   CHECK_GC
   STORE_SPLIT_TVALUE R1, tnumber, 1
   JUMP bb_2

bb_2:
; predecessors: bb_0, bb_fallback_1
; in regs: R0, R1
   RETURN R0, 2i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "FullStoreHasToBeObservableFromFallbacks3")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);
    IrOp last = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(1)), build.constTag(tfunction), fallback);
    build.inst(IrCmd::STORE_POINTER, build.vmReg(1), build.inst(IrCmd::LOAD_POINTER, build.vmConst(10)));
    build.inst(IrCmd::CHECK_SAFE_ENV, fallback);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(1));
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::JUMP, last);

    build.beginBlock(fallback);
    build.inst(IrCmd::CHECK_GC);
    build.inst(IrCmd::STORE_SPLIT_TVALUE, build.vmReg(1), build.constTag(tnumber), build.constDouble(1.0));
    build.inst(IrCmd::JUMP, last);

    build.beginBlock(last);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(2));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    markDeadStoresInBlockChains(build);

    // Tag check establishes that at that point, the tag of the value IS a function (as an exit here has to be with well-formed stack)
    // Later additional function pointer store can be removed, even if it observable from the GC in the fallback
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; successors: bb_fallback_1, bb_fallback_1, bb_2
; in regs: R0, R1
; out regs: R0, R1
   %0 = LOAD_TAG R1
   CHECK_TAG %0, tfunction, bb_fallback_1
   CHECK_SAFE_ENV bb_fallback_1
   STORE_DOUBLE R1, 1
   STORE_TAG R1, tnumber
   JUMP bb_2

bb_fallback_1:
; predecessors: bb_0, bb_0
; successors: bb_2
; in regs: R0
; out regs: R0, R1
   CHECK_GC
   STORE_SPLIT_TVALUE R1, tnumber, 1
   JUMP bb_2

bb_2:
; predecessors: bb_0, bb_fallback_1
; in regs: R0, R1
   RETURN R0, 2i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "SafePartialValueStoresWithPreservedTag")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);
    IrOp last = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_SPLIT_TVALUE, build.vmReg(1), build.constTag(tnumber), build.constDouble(1));
    build.inst(IrCmd::CHECK_SAFE_ENV, fallback);                           // While R1 has to be observed in full by the fallback
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(2)); // This partial store is safe to remove because number tag is established
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(3)); // And so is this
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(4));
    build.inst(IrCmd::JUMP, last);

    build.beginBlock(fallback);
    build.inst(IrCmd::CHECK_GC);
    build.inst(IrCmd::JUMP, last);

    build.beginBlock(last);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(2));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    // If table tag store at the start is removed, GC assists in the fallback can observe value with a wrong tag
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; successors: bb_fallback_1, bb_2
; in regs: R0
; out regs: R0, R1
   STORE_SPLIT_TVALUE R1, tnumber, 1
   CHECK_SAFE_ENV bb_fallback_1
   STORE_DOUBLE R1, 4
   JUMP bb_2

bb_fallback_1:
; predecessors: bb_0
; successors: bb_2
; in regs: R0, R1
; out regs: R0, R1
   CHECK_GC
   JUMP bb_2

bb_2:
; predecessors: bb_0, bb_fallback_1
; in regs: R0, R1
   RETURN R0, 2i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "SafePartialValueStoresWithPreservedTag2")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp fallback = build.block(IrBlockKind::Fallback);
    IrOp last = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_SPLIT_TVALUE, build.vmReg(1), build.constTag(tnumber), build.constDouble(1));
    build.inst(IrCmd::CHECK_SAFE_ENV, fallback);                           // While R1 has to be observed in full by the fallback
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), build.constDouble(2)); // This partial store is safe to remove because tag is established
    build.inst(IrCmd::STORE_SPLIT_TVALUE, build.vmReg(1), build.constTag(tnumber), build.constDouble(4));
    build.inst(IrCmd::JUMP, last);

    build.beginBlock(fallback);
    build.inst(IrCmd::CHECK_GC);
    build.inst(IrCmd::JUMP, last);

    build.beginBlock(last);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(2));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    // If table tag store at the start is removed, GC assists in the fallback can observe value with a wrong tag
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; successors: bb_fallback_1, bb_2
; in regs: R0
; out regs: R0, R1
   STORE_SPLIT_TVALUE R1, tnumber, 1
   CHECK_SAFE_ENV bb_fallback_1
   STORE_SPLIT_TVALUE R1, tnumber, 4
   JUMP bb_2

bb_fallback_1:
; predecessors: bb_0
; successors: bb_2
; in regs: R0, R1
; out regs: R0, R1
   CHECK_GC
   JUMP bb_2

bb_2:
; predecessors: bb_0, bb_fallback_1
; in regs: R0, R1
   RETURN R0, 2i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "DoNotReturnWithPartialStores")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp success = build.block(IrBlockKind::Internal);
    IrOp fail = build.block(IrBlockKind::Internal);
    IrOp exit = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_POINTER, build.vmReg(1), build.inst(IrCmd::NEW_TABLE, build.constUint(0), build.constUint(0)));
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(ttable));
    IrOp toUint = build.inst(IrCmd::NUM_TO_UINT, build.constDouble(-1));
    IrOp bitAnd = build.inst(IrCmd::BITAND_UINT, toUint, build.constInt(4));
    build.inst(IrCmd::JUMP_CMP_INT, bitAnd, build.constInt(0), build.cond(IrCondition::Equal), success, fail);

    build.beginBlock(success);
    build.inst(IrCmd::STORE_INT, build.vmReg(1), build.constInt(0));
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(fail);
    build.inst(IrCmd::STORE_INT, build.vmReg(1), build.constInt(1));
    build.inst(IrCmd::JUMP, exit);

    build.beginBlock(exit);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tboolean));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    constPropInBlockChains(build);
    markDeadStoresInBlockChains(build);

    // Even though R1 is not live out at return, we stored table tag followed by an integer value
    // Boolean tag store has to remain, even if unused, because all stack slots are visible to GC
    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
; successors: bb_1, bb_2
; in regs: R0
; out regs: R0
   %0 = NEW_TABLE 0u, 0u
   STORE_POINTER R1, %0
   STORE_TAG R1, ttable
   %3 = NUM_TO_UINT -1
   %4 = BITAND_UINT %3, 4i
   JUMP_CMP_INT %4, 0i, eq, bb_1, bb_2

bb_1:
; predecessors: bb_0
; successors: bb_3
; in regs: R0
; out regs: R0
   STORE_INT R1, 0i
   JUMP bb_3

bb_2:
; predecessors: bb_0
; successors: bb_3
; in regs: R0
; out regs: R0
   STORE_INT R1, 1i
   JUMP bb_3

bb_3:
; predecessors: bb_1, bb_2
; in regs: R0
   STORE_TAG R1, tboolean
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "PartialOverFullValue")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_SPLIT_TVALUE, build.vmReg(0), build.constTag(tnumber), build.constDouble(1.0));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(2.0));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(4.0));
    build.inst(
        IrCmd::STORE_SPLIT_TVALUE, build.vmReg(0), build.constTag(ttable), build.inst(IrCmd::NEW_TABLE, build.constUint(16), build.constUint(32))
    );
    build.inst(IrCmd::STORE_POINTER, build.vmReg(0), build.inst(IrCmd::NEW_TABLE, build.constUint(8), build.constUint(16)));
    build.inst(IrCmd::STORE_POINTER, build.vmReg(0), build.inst(IrCmd::NEW_TABLE, build.constUint(4), build.constUint(8)));
    build.inst(IrCmd::STORE_SPLIT_TVALUE, build.vmReg(0), build.constTag(tnumber), build.constDouble(1.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tstring));
    IrOp newtable = build.inst(IrCmd::NEW_TABLE, build.constUint(16), build.constUint(32));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(ttable));
    build.inst(IrCmd::STORE_POINTER, build.vmReg(0), newtable);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   %11 = NEW_TABLE 16u, 32u
   STORE_SPLIT_TVALUE R0, ttable, %11
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "VectorOverNumber")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(2.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::STORE_VECTOR, build.vmReg(0), build.constDouble(1.0), build.constDouble(2.0), build.constDouble(4.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tvector));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_VECTOR R0, 1, 2, 4, tvector
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "VectorOverVector")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_VECTOR, build.vmReg(0), build.constDouble(4.0), build.constDouble(2.0), build.constDouble(1.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tvector));
    build.inst(IrCmd::STORE_VECTOR, build.vmReg(0), build.constDouble(1.0), build.constDouble(2.0), build.constDouble(4.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tvector));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_VECTOR R0, 1, 2, 4, tvector
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "NumberOverVector")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_VECTOR, build.vmReg(0), build.constDouble(1.0), build.constDouble(2.0), build.constDouble(4.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tvector));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(2.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_SPLIT_TVALUE R0, tnumber, 2
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "NumberOverNil")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnil));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(2.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_SPLIT_TVALUE R0, tnumber, 2
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "VectorOverNil")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnil));
    build.inst(IrCmd::STORE_VECTOR, build.vmReg(0), build.constDouble(1.0), build.constDouble(2.0), build.constDouble(4.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tvector));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_VECTOR R0, 1, 2, 4, tvector
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "NumberOverCombinedVector")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(2.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::STORE_VECTOR, build.vmReg(0), build.constDouble(1.0), build.constDouble(2.0), build.constDouble(4.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tvector));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(3.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_SPLIT_TVALUE R0, tnumber, 3
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "VectorOverCombinedVector")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(2.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::STORE_VECTOR, build.vmReg(0), build.constDouble(1.0), build.constDouble(2.0), build.constDouble(4.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tvector));
    build.inst(IrCmd::STORE_VECTOR, build.vmReg(0), build.constDouble(8.0), build.constDouble(16.0), build.constDouble(32.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tvector));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_VECTOR R0, 8, 16, 32, tvector
   RETURN R0, 1i

)");
}

TEST_CASE_FIXTURE(IrBuilderFixture, "VectorOverCombinedNumber")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(2.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(0), build.constDouble(4.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tnumber));
    build.inst(IrCmd::STORE_VECTOR, build.vmReg(0), build.constDouble(8.0), build.constDouble(16.0), build.constDouble(32.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tvector));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    updateUseCounts(build.function);
    computeCfgInfo(build.function);
    markDeadStoresInBlockChains(build);

    CHECK("\n" + toString(build.function, IncludeUseInfo::No) == R"(
bb_0:
   STORE_VECTOR R0, 8, 16, 32, tvector
   RETURN R0, 1i

)");
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("Dump");

TEST_CASE_FIXTURE(IrBuilderFixture, "ToDot")
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

    // note: we don't validate the output of these to avoid test churn when formatting changes; we run these to make sure they don't assert/crash
    toDot(build.function, /* includeInst= */ true);
    toDotCfg(build.function);
    toDotDjGraph(build.function);
}

TEST_SUITE_END();
