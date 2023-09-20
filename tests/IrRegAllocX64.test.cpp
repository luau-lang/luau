// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrRegAllocX64.h"

#include "doctest.h"

using namespace Luau::CodeGen;
using namespace Luau::CodeGen::X64;

class IrRegAllocX64Fixture
{
public:
    IrRegAllocX64Fixture()
        : build(/* logText */ true, ABIX64::Windows)
        , regs(build, function, nullptr)
    {
    }

    void checkMatch(std::string expected)
    {
        build.finalize();

        CHECK("\n" + build.text == expected);
    }

    AssemblyBuilderX64 build;
    IrFunction function;
    IrRegAllocX64 regs;
};

TEST_SUITE_BEGIN("IrRegAllocX64");

TEST_CASE_FIXTURE(IrRegAllocX64Fixture, "RelocateFix")
{
    IrInst irInst0{IrCmd::LOAD_DOUBLE};
    irInst0.lastUse = 2;
    function.instructions.push_back(irInst0);

    IrInst irInst1{IrCmd::LOAD_DOUBLE};
    irInst1.lastUse = 2;
    function.instructions.push_back(irInst1);

    function.instructions[0].regX64 = regs.takeReg(rax, 0);
    regs.preserve(function.instructions[0]);

    function.instructions[1].regX64 = regs.takeReg(rax, 1);
    regs.restore(function.instructions[0], true);

    LUAU_ASSERT(function.instructions[0].regX64 == rax);
    LUAU_ASSERT(function.instructions[1].spilled);

    checkMatch(R"(
 vmovsd      qword ptr [rsp+048h],rax
 vmovsd      qword ptr [rsp+050h],rax
 vmovsd      rax,qword ptr [rsp+048h]
)");
}

TEST_SUITE_END();
