// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrCallWrapperX64.h"
#include "Luau/IrRegAllocX64.h"

#include "doctest.h"

using namespace Luau::CodeGen;
using namespace Luau::CodeGen::X64;

class IrCallWrapperX64Fixture
{
public:
    IrCallWrapperX64Fixture(ABIX64 abi = ABIX64::Windows)
        : build(/* logText */ true, abi)
        , regs(build, function, nullptr)
        , callWrap(regs, build, ~0u)
    {
    }

    void checkMatch(std::string expected)
    {
        regs.assertAllFree();

        build.finalize();

        CHECK("\n" + build.text == expected);
    }

    AssemblyBuilderX64 build;
    IrFunction function;
    IrRegAllocX64 regs;
    IrCallWrapperX64 callWrap;

    // Tests rely on these to force interference between registers
    static constexpr RegisterX64 rArg1 = rcx;
    static constexpr RegisterX64 rArg1d = ecx;
    static constexpr RegisterX64 rArg2 = rdx;
    static constexpr RegisterX64 rArg2d = edx;
    static constexpr RegisterX64 rArg3 = r8;
    static constexpr RegisterX64 rArg3d = r8d;
    static constexpr RegisterX64 rArg4 = r9;
    static constexpr RegisterX64 rArg4d = r9d;
};

class IrCallWrapperX64FixtureSystemV : public IrCallWrapperX64Fixture
{
public:
    IrCallWrapperX64FixtureSystemV()
        : IrCallWrapperX64Fixture(ABIX64::SystemV)
    {
    }
};

TEST_SUITE_BEGIN("IrCallWrapperX64");

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "SimpleRegs")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rax, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rArg2, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, tmp1);
    callWrap.addArgument(SizeX64::qword, tmp2); // Already in its place
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         rcx,rax
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "TrickyUse1")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, tmp1.reg); // Already in its place
    callWrap.addArgument(SizeX64::qword, tmp1.release());
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         rdx,rcx
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "TrickyUse2")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, qword[tmp1.reg]);
    callWrap.addArgument(SizeX64::qword, tmp1.release());
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         rdx,rcx
 mov         rcx,qword ptr [rcx]
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "SimpleMemImm")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rax, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rsi, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::dword, 32);
    callWrap.addArgument(SizeX64::dword, -1);
    callWrap.addArgument(SizeX64::qword, qword[r14 + 32]);
    callWrap.addArgument(SizeX64::qword, qword[tmp1.release() + tmp2.release()]);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         r8,qword ptr [r14+020h]
 mov         r9,qword ptr [rax+rsi]
 mov         ecx,20h
 mov         edx,FFFFFFFFh
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "SimpleStackArgs")
{
    ScopedRegX64 tmp{regs, regs.takeReg(rax, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, tmp);
    callWrap.addArgument(SizeX64::qword, qword[r14 + 16]);
    callWrap.addArgument(SizeX64::qword, qword[r14 + 32]);
    callWrap.addArgument(SizeX64::qword, qword[r14 + 48]);
    callWrap.addArgument(SizeX64::dword, 1);
    callWrap.addArgument(SizeX64::qword, qword[r13]);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         rdx,qword ptr [r13]
 mov         qword ptr [rsp+028h],rdx
 mov         rcx,rax
 mov         rdx,qword ptr [r14+010h]
 mov         r8,qword ptr [r14+020h]
 mov         r9,qword ptr [r14+030h]
 mov         dword ptr [rsp+020h],1
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "FixedRegisters")
{
    callWrap.addArgument(SizeX64::dword, 1);
    callWrap.addArgument(SizeX64::qword, 2);
    callWrap.addArgument(SizeX64::qword, 3);
    callWrap.addArgument(SizeX64::qword, 4);
    callWrap.addArgument(SizeX64::qword, r14);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         qword ptr [rsp+020h],r14
 mov         ecx,1
 mov         rdx,2
 mov         r8,3
 mov         r9,4
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "EasyInterference")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rdi, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rsi, kInvalidInstIdx)};
    ScopedRegX64 tmp3{regs, regs.takeReg(rArg2, kInvalidInstIdx)};
    ScopedRegX64 tmp4{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, tmp1);
    callWrap.addArgument(SizeX64::qword, tmp2);
    callWrap.addArgument(SizeX64::qword, tmp3);
    callWrap.addArgument(SizeX64::qword, tmp4);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         r8,rdx
 mov         rdx,rsi
 mov         r9,rcx
 mov         rcx,rdi
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "FakeInterference")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rArg2, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, qword[tmp1.release() + 8]);
    callWrap.addArgument(SizeX64::qword, qword[tmp2.release() + 8]);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         rcx,qword ptr [rcx+8]
 mov         rdx,qword ptr [rdx+8]
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "HardInterferenceInt")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg4, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rArg3, kInvalidInstIdx)};
    ScopedRegX64 tmp3{regs, regs.takeReg(rArg2, kInvalidInstIdx)};
    ScopedRegX64 tmp4{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, tmp1);
    callWrap.addArgument(SizeX64::qword, tmp2);
    callWrap.addArgument(SizeX64::qword, tmp3);
    callWrap.addArgument(SizeX64::qword, tmp4);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         rax,r9
 mov         r9,rcx
 mov         rcx,rax
 mov         rax,r8
 mov         r8,rdx
 mov         rdx,rax
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "HardInterferenceInt2")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg4d, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rArg3d, kInvalidInstIdx)};
    ScopedRegX64 tmp3{regs, regs.takeReg(rArg2d, kInvalidInstIdx)};
    ScopedRegX64 tmp4{regs, regs.takeReg(rArg1d, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::dword, tmp1);
    callWrap.addArgument(SizeX64::dword, tmp2);
    callWrap.addArgument(SizeX64::dword, tmp3);
    callWrap.addArgument(SizeX64::dword, tmp4);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         eax,r9d
 mov         r9d,ecx
 mov         ecx,eax
 mov         eax,r8d
 mov         r8d,edx
 mov         edx,eax
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "HardInterferenceFp")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(xmm1, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(xmm0, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::xmmword, tmp1);
    callWrap.addArgument(SizeX64::xmmword, tmp2);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 vmovsd      xmm2,xmm1,xmm1
 vmovsd      xmm1,xmm0,xmm0
 vmovsd      xmm0,xmm2,xmm2
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "HardInterferenceBoth")
{
    ScopedRegX64 int1{regs, regs.takeReg(rArg2, kInvalidInstIdx)};
    ScopedRegX64 int2{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    ScopedRegX64 fp1{regs, regs.takeReg(xmm3, kInvalidInstIdx)};
    ScopedRegX64 fp2{regs, regs.takeReg(xmm2, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, int1);
    callWrap.addArgument(SizeX64::qword, int2);
    callWrap.addArgument(SizeX64::xmmword, fp1);
    callWrap.addArgument(SizeX64::xmmword, fp2);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         rax,rdx
 mov         rdx,rcx
 mov         rcx,rax
 vmovsd      xmm0,xmm3,xmm3
 vmovsd      xmm3,xmm2,xmm2
 vmovsd      xmm2,xmm0,xmm0
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "FakeMultiuseInterferenceMem")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rArg2, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, qword[tmp1.reg + tmp2.reg + 8]);
    callWrap.addArgument(SizeX64::qword, qword[tmp2.reg + 16]);
    tmp1.release();
    tmp2.release();
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         rcx,qword ptr [rcx+rdx+8]
 mov         rdx,qword ptr [rdx+010h]
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "HardMultiuseInterferenceMem1")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rArg2, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, qword[tmp1.reg + tmp2.reg + 8]);
    callWrap.addArgument(SizeX64::qword, qword[tmp1.reg + 16]);
    tmp1.release();
    tmp2.release();
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         rax,rcx
 mov         rcx,qword ptr [rax+rdx+8]
 mov         rdx,qword ptr [rax+010h]
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "HardMultiuseInterferenceMem2")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rArg2, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, qword[tmp1.reg + tmp2.reg + 8]);
    callWrap.addArgument(SizeX64::qword, qword[tmp1.reg + tmp2.reg + 16]);
    tmp1.release();
    tmp2.release();
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         rax,rcx
 mov         rcx,qword ptr [rax+rdx+8]
 mov         rdx,qword ptr [rax+rdx+010h]
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "HardMultiuseInterferenceMem3")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg3, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rArg2, kInvalidInstIdx)};
    ScopedRegX64 tmp3{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, qword[tmp1.reg + tmp2.reg + 8]);
    callWrap.addArgument(SizeX64::qword, qword[tmp2.reg + tmp3.reg + 16]);
    callWrap.addArgument(SizeX64::qword, qword[tmp3.reg + tmp1.reg + 16]);
    tmp1.release();
    tmp2.release();
    tmp3.release();
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         rax,r8
 mov         r8,qword ptr [rcx+rax+010h]
 mov         rbx,rdx
 mov         rdx,qword ptr [rbx+rcx+010h]
 mov         rcx,qword ptr [rax+rbx+8]
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "InterferenceWithCallArg1")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, qword[tmp1.reg + 8]);
    callWrap.call(qword[tmp1.release() + 16]);

    checkMatch(R"(
 mov         rax,rcx
 mov         rcx,qword ptr [rax+8]
 call        qword ptr [rax+010h]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "InterferenceWithCallArg2")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rArg2, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, tmp2);
    callWrap.call(qword[tmp1.release() + 16]);

    checkMatch(R"(
 mov         rax,rcx
 mov         rcx,rdx
 call        qword ptr [rax+010h]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "InterferenceWithCallArg3")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, tmp1.reg);
    callWrap.call(qword[tmp1.release() + 16]);

    checkMatch(R"(
 call        qword ptr [rcx+010h]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "WithLastIrInstUse1")
{
    IrInst irInst1;
    IrOp irOp1 = {IrOpKind::Inst, 0};
    irInst1.regX64 = regs.takeReg(xmm0, irOp1.index);
    irInst1.lastUse = 1;
    function.instructions.push_back(irInst1);
    callWrap.instIdx = irInst1.lastUse;

    callWrap.addArgument(SizeX64::xmmword, irInst1.regX64, irOp1); // Already in its place
    callWrap.addArgument(SizeX64::xmmword, qword[r12 + 8]);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 vmovsd      xmm1,qword ptr [r12+8]
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "WithLastIrInstUse2")
{
    IrInst irInst1;
    IrOp irOp1 = {IrOpKind::Inst, 0};
    irInst1.regX64 = regs.takeReg(xmm0, irOp1.index);
    irInst1.lastUse = 1;
    function.instructions.push_back(irInst1);
    callWrap.instIdx = irInst1.lastUse;

    callWrap.addArgument(SizeX64::xmmword, qword[r12 + 8]);
    callWrap.addArgument(SizeX64::xmmword, irInst1.regX64, irOp1);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 vmovsd      xmm1,xmm0,xmm0
 vmovsd      xmm0,qword ptr [r12+8]
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "WithLastIrInstUse3")
{
    IrInst irInst1;
    IrOp irOp1 = {IrOpKind::Inst, 0};
    irInst1.regX64 = regs.takeReg(xmm0, irOp1.index);
    irInst1.lastUse = 1;
    function.instructions.push_back(irInst1);
    callWrap.instIdx = irInst1.lastUse;

    callWrap.addArgument(SizeX64::xmmword, irInst1.regX64, irOp1);
    callWrap.addArgument(SizeX64::xmmword, irInst1.regX64, irOp1);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 vmovsd      xmm1,xmm0,xmm0
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "WithLastIrInstUse4")
{
    IrInst irInst1;
    IrOp irOp1 = {IrOpKind::Inst, 0};
    irInst1.regX64 = regs.takeReg(rax, irOp1.index);
    irInst1.lastUse = 1;
    function.instructions.push_back(irInst1);
    callWrap.instIdx = irInst1.lastUse;

    ScopedRegX64 tmp{regs, regs.takeReg(rdx, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, r15);
    callWrap.addArgument(SizeX64::qword, irInst1.regX64, irOp1);
    callWrap.addArgument(SizeX64::qword, tmp);
    callWrap.call(qword[r12]);

    checkMatch(R"(
 mov         rcx,r15
 mov         r8,rdx
 mov         rdx,rax
 call        qword ptr [r12]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "ExtraCoverage")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rArg2, kInvalidInstIdx)};
    callWrap.addArgument(SizeX64::qword, addr[r12 + 8]);
    callWrap.addArgument(SizeX64::qword, addr[r12 + 16]);
    callWrap.addArgument(SizeX64::xmmword, xmmword[r13]);
    callWrap.call(qword[tmp1.release() + tmp2.release()]);

    checkMatch(R"(
 vmovups     xmm2,xmmword ptr [r13]
 mov         rax,rcx
 lea         rcx,[r12+8]
 mov         rbx,rdx
 lea         rdx,[r12+010h]
 call        qword ptr [rax+rbx]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "AddressInStackArguments")
{
    callWrap.addArgument(SizeX64::dword, 1);
    callWrap.addArgument(SizeX64::dword, 2);
    callWrap.addArgument(SizeX64::dword, 3);
    callWrap.addArgument(SizeX64::dword, 4);
    callWrap.addArgument(SizeX64::qword, addr[r12 + 16]);
    callWrap.call(qword[r14]);

    checkMatch(R"(
 lea         rax,[r12+010h]
 mov         qword ptr [rsp+020h],rax
 mov         ecx,1
 mov         edx,2
 mov         r8d,3
 mov         r9d,4
 call        qword ptr [r14]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64Fixture, "ImmediateConflictWithFunction")
{
    ScopedRegX64 tmp1{regs, regs.takeReg(rArg1, kInvalidInstIdx)};
    ScopedRegX64 tmp2{regs, regs.takeReg(rArg2, kInvalidInstIdx)};

    callWrap.addArgument(SizeX64::dword, 1);
    callWrap.addArgument(SizeX64::dword, 2);
    callWrap.call(qword[tmp1.release() + tmp2.release()]);

    checkMatch(R"(
 mov         rax,rcx
 mov         ecx,1
 mov         rbx,rdx
 mov         edx,2
 call        qword ptr [rax+rbx]
)");
}

TEST_CASE_FIXTURE(IrCallWrapperX64FixtureSystemV, "SuggestedConflictWithReserved")
{
    ScopedRegX64 tmp{regs, regs.takeReg(r9, kInvalidInstIdx)};

    IrCallWrapperX64 callWrap(regs, build);
    callWrap.addArgument(SizeX64::qword, r12);
    callWrap.addArgument(SizeX64::qword, r13);
    callWrap.addArgument(SizeX64::qword, r14);
    callWrap.addArgument(SizeX64::dword, 2);
    callWrap.addArgument(SizeX64::qword, 1);

    RegisterX64 reg = callWrap.suggestNextArgumentRegister(SizeX64::dword);
    build.mov(reg, 10);
    callWrap.addArgument(SizeX64::dword, reg);

    callWrap.call(tmp.release());

    checkMatch(R"(
 mov         eax,Ah
 mov         rdi,r12
 mov         rsi,r13
 mov         rdx,r14
 mov         rcx,r9
 mov         r9d,eax
 mov         rax,rcx
 mov         ecx,2
 mov         r8,1
 call        rax
)");
}

TEST_SUITE_END();
