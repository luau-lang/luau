// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/CodeGen.h"
#include "Luau/IrAnalysis.h"
#include "Luau/IrBuilder.h"
#include "Luau/IrDump.h"

#include "doctest.h"
#include "ScopedFlags.h"

#include <regex>

LUAU_FASTFLAG(LuauCodegenX64IntSpillRestore)

LUAU_FASTFLAG(LuauCodegenExitSyncUpdate)

LUAU_FASTFLAG(LuauCodegenScopedSpillKeepLazy)

using namespace Luau::CodeGen;

static void stripLinesContaining(std::string& text, const char* needle)
{
    size_t pos = 0;

    while ((pos = text.find(needle, pos)) != std::string::npos)
    {
        size_t lineStart = text.rfind('\n', pos);
        lineStart = (lineStart == std::string::npos) ? 0 : lineStart + 1;

        size_t lineEnd = text.find('\n', pos);

        if (lineEnd == std::string::npos)
            text.erase(lineStart);
        else
            text.erase(lineStart, lineEnd - lineStart + 1);

        pos = lineStart;
    }
}

// To not have to update results every time a new field is added to lua_State/global_State, we replace the offsets
static void normalizeStateOffsets(std::string& text)
{
    std::string result;
    result.reserve(text.size());

    std::string pendingReg;

    size_t pos = 0;
    while (pos < text.size())
    {
        size_t eol = text.find('\n', pos);
        if (eol == std::string::npos)
            eol = text.size();

        std::string line = text.substr(pos, eol - pos);

        std::smatch match;
        if (std::regex_search(line, match, std::regex(R"((\w+),.*\[r15\+[^\]]+\])")))
        {
            pendingReg = match[1].str();
            line = std::regex_replace(line, std::regex(R"(\[r15\+[^\]]+\])"), "[r15+<offset>]");
        }
        else if (!pendingReg.empty() && pendingReg != "r14")
        {
            std::regex deref("\\[" + pendingReg + "\\+[^\\]]+\\]");
            line = std::regex_replace(line, deref, "[" + pendingReg + "+<offset>]");
        }

        result += line;
        if (eol < text.size())
            result += '\n';
        pos = eol + 1;
    }

    text = std::move(result);
}

class IrAssemblyFixture
{
public:
    IrAssemblyFixture()
        : build(hooks, {})
    {
        options.target = AssemblyOptions::X64_Windows;

        options.outputBinary = false;

        options.includeAssembly = true;
        options.includeIr = true;
        options.includeOutlinedCode = false;
        options.includeIrTypes = true;

        options.includeIrPrefix = IncludeIrPrefix::No;
        options.includeUseInfo = IncludeUseInfo::No;
        options.includeCfgInfo = IncludeCfgInfo::No;
        options.includeRegFlowInfo = IncludeRegFlowInfo::No;
    }

    std::string lower()
    {
        std::string text = getAssemblyFromIr(build, options);
        stripLinesContaining(text, "; skipping ");
        normalizeStateOffsets(text);

        return text;
    }

    HostIrHooks hooks;
    IrBuilder build;
    AssemblyOptions options;

    // Luau.VM headers are not accessible
    int tnil = parseTagName("tnil");
    int tboolean = parseTagName("tboolean");
    int tnumber = parseTagName("tnumber");
    int tinteger = parseTagName("tinteger");
    int tvector = parseTagName("tvector");
    int tstring = parseTagName("tstring");
    int ttable = parseTagName("ttable");
    int tfunction = parseTagName("tfunction");
    int tuserdata = parseTagName("tuserdata");
    int tbuffer = parseTagName("tbuffer");
};

TEST_SUITE_BEGIN("IrAssembly");

TEST_CASE_FIXTURE(IrAssemblyFixture, "PreserveIntChainedFromDoubleVmReg")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp d = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    IrOp i = build.inst(IrCmd::NUM_TO_INT, d);
    build.inst(IrCmd::INTERRUPT, build.constUint(0));
    build.inst(IrCmd::STORE_INT, build.vmReg(0), i);
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tboolean));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));
    updateUseCounts(build.function);

    // %1 after INTERRUPT spill is restored from R1 using vcvttsd2si conversion
    CHECK_EQ(
        "\n" + lower(),
        R"(
; align 32 using ud2
bb_0:
.L11:
  %0 = LOAD_DOUBLE R1
 vmovsd      xmm0,qword ptr [r14+010h]
  %1 = NUM_TO_INT %0
 vcvttsd2si  eax,xmm0
  INTERRUPT 0u
 mov         rax,qword ptr [r15+<offset>]
 cmp         qword ptr [rax+<offset>],0
 jne         .L12
.L13:
  STORE_INT R0, %1
 vcvttsd2si  eax,qword ptr [r14+010h]
 mov         dword ptr [r14],eax
  STORE_TAG R0, tboolean
 mov         dword ptr [r14+0Ch],1
  RETURN R0, 1i
 vmovups     xmm0,xmmword ptr [r14]
 vmovups     xmmword ptr [r14-010h],xmm0
 mov         rdi,r14
 mov         ecx,1
 jmp         .L7

)"
    );
}

TEST_CASE_FIXTURE(IrAssemblyFixture, "PreserveIntChainedFromDoubleVmRegBoth")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp d = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(2));
    IrOp i = build.inst(IrCmd::NUM_TO_INT, d);
    build.inst(IrCmd::INTERRUPT, build.constUint(0));
    build.inst(IrCmd::STORE_INT, build.vmReg(0), i);
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tboolean));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), d);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(2));
    updateUseCounts(build.function);

    // Both %0 and %1 restore from R2, integer restore uses vcvttsd2si
    CHECK_EQ(
        "\n" + lower(),
        R"(
; align 32 using ud2
bb_0:
.L11:
  %0 = LOAD_DOUBLE R2
 vmovsd      xmm0,qword ptr [r14+020h]
  %1 = NUM_TO_INT %0
 vcvttsd2si  eax,xmm0
  INTERRUPT 0u
 mov         rax,qword ptr [r15+<offset>]
 cmp         qword ptr [rax+<offset>],0
 jne         .L12
.L13:
  STORE_INT R0, %1
 vcvttsd2si  eax,qword ptr [r14+020h]
 mov         dword ptr [r14],eax
  STORE_TAG R0, tboolean
 mov         dword ptr [r14+0Ch],1
  STORE_DOUBLE R1, %0
 vmovsd      xmm0,qword ptr [r14+020h]
 vmovsd      qword ptr [r14+010h],xmm0
  STORE_TAG R1, tnumber
 mov         dword ptr [r14+01Ch],3
  RETURN R0, 2i
 lea         rdi,[r14-010h]
 vmovups     xmm0,xmmword ptr [r14]
 vmovups     xmmword ptr [rdi],xmm0
 vmovups     xmm0,xmmword ptr [r14+010h]
 vmovups     xmmword ptr [rdi+010h],xmm0
 add         rdi,20h
 mov         ecx,2
 jmp         .L7

)"
    );
}

TEST_CASE_FIXTURE(IrAssemblyFixture, "PreserveIntWithoutChainSpillsToStack")
{
    IrOp entry = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp a = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    IrOp b = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(2));
    IrOp sum = build.inst(IrCmd::ADD_NUM, a, b);
    IrOp i = build.inst(IrCmd::NUM_TO_INT, sum);
    build.inst(IrCmd::INTERRUPT, build.constUint(0));
    build.inst(IrCmd::STORE_INT, build.vmReg(3), i);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(0));
    updateUseCounts(build.function);

    // %3 is restored from a stack spill as there is no VM register store location for it
    CHECK_EQ(
        "\n" + lower(),
        R"(
; align 32 using ud2
bb_0:
.L11:
  %0 = LOAD_DOUBLE R1
 vmovsd      xmm0,qword ptr [r14+010h]
  %2 = ADD_NUM %0, R2
 vaddsd      xmm0,xmm0,qword ptr [r14+020h]
  %3 = NUM_TO_INT %2
 vcvttsd2si  eax,xmm0
  INTERRUPT 0u
 mov         dword ptr [rsp+048h],eax
 mov         rax,qword ptr [r15+<offset>]
 cmp         qword ptr [rax+<offset>],0
 jne         .L12
.L13:
  STORE_INT R3, %3
 mov         eax,dword ptr [rsp+048h]
 mov         dword ptr [r14+030h],eax
  RETURN R0, 0i
 lea         rdi,[r14-010h]
 xor         ecx,ecx
 jmp         .L7

)"
    );
}

TEST_CASE_FIXTURE(IrAssemblyFixture, "DseHintMaterializesIntIntoDeadVmReg")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    build.beginBlock(entry);

    IrOp d = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    IrOp i = build.inst(IrCmd::NUM_TO_INT, d);

    // Kill R1 as a potential restore location
    IrOp doubled = build.inst(IrCmd::ADD_NUM, d, d);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), doubled);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));

    // Prepare R4 store what will be removed by DSE, but preserved as a lazy restore location
    IrOp roundtrip = build.inst(IrCmd::INT_TO_NUM, i);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(4), roundtrip);
    build.inst(IrCmd::STORE_TAG, build.vmReg(4), build.constTag(tnumber));

    build.inst(IrCmd::INTERRUPT, build.constUint(0));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), roundtrip);
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(tnumber));

    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(2));
    updateUseCounts(build.function);

    // INTERRUPT spills %5 to R4 and later we read from it
    CHECK_EQ(
        "\n" + lower(),
        R"(
; align 32 using ud2
bb_0:
.L11:
  %0 = LOAD_DOUBLE R1
 vmovsd      xmm0,qword ptr [r14+010h]
  %1 = NUM_TO_INT %0
 vcvttsd2si  eax,xmm0
  %2 = ADD_NUM %0, %0
 vaddsd      xmm0,xmm0,xmm0
  STORE_DOUBLE R1, %2
 vmovsd      qword ptr [r14+010h],xmm0
  STORE_TAG R1, tnumber
 mov         dword ptr [r14+01Ch],3
  %5 = INT_TO_NUM %1
 vcvtsi2sd   xmm0,xmm0,eax
  INTERRUPT 0u
 vmovsd      qword ptr [r14+040h],xmm0
 mov         dword ptr [r14+04Ch],0
 mov         rax,qword ptr [r15+<offset>]
 cmp         qword ptr [rax+<offset>],0
 jne         .L12
.L13:
  STORE_DOUBLE R2, %5
 vmovsd      xmm0,qword ptr [r14+040h]
 vmovsd      qword ptr [r14+020h],xmm0
  STORE_TAG R2, tnumber
 mov         dword ptr [r14+02Ch],3
  RETURN R1, 2i
 lea         rdi,[r14-010h]
 vmovups     xmm0,xmmword ptr [r14+010h]
 vmovups     xmmword ptr [rdi],xmm0
 vmovups     xmm0,xmmword ptr [r14+020h]
 vmovups     xmmword ptr [rdi+010h],xmm0
 add         rdi,20h
 mov         ecx,2
 jmp         .L7

)"
    );
}

TEST_CASE_FIXTURE(IrAssemblyFixture, "DseHintCorruptsTagOnPartialValueKill")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    build.beginBlock(entry);

    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(1)), build.constTag(tnumber), build.vmExit(0));
    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(2)), build.constTag(tnumber), build.vmExit(0));

    IrOp r1Val = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    IrOp r2Val = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(2));
    IrOp computed = build.inst(IrCmd::ADD_NUM, r1Val, r2Val);

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), computed);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber)); // Will be removed as redundant

    build.inst(IrCmd::INTERRUPT, build.constUint(0)); // Trigger a spill

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(3), computed);
    build.inst(IrCmd::STORE_TAG, build.vmReg(3), build.constTag(tnumber));

    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(4)), build.constTag(tnumber), build.vmExit(0));
    IrOp newVal = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(4));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), newVal);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber)); // Will be removed as redundant

    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(3));
    updateUseCounts(build.function);

    // With no established tag+value store after redundant tag store removal, there should be no DSE hint used for R1 spill
    CHECK_EQ(
        "\n" + lower(),
        R"(
; align 32 using ud2
bb_0:
.L11:
  CHECK_TAG R1, tnumber, exit(0)
 cmp         dword ptr [r14+01Ch],3
 jne         .L12
  CHECK_TAG R2, tnumber, exit(0)
 cmp         dword ptr [r14+02Ch],3
 jne         .L12
  %4 = LOAD_DOUBLE R1
 vmovsd      xmm0,qword ptr [r14+010h]
  %6 = ADD_NUM %4, R2
 vaddsd      xmm0,xmm0,qword ptr [r14+020h]
  INTERRUPT 0u
 vmovsd      qword ptr [rsp+048h],xmm0
 mov         rax,qword ptr [r15+<offset>]
 cmp         qword ptr [rax+<offset>],0
 jne         .L13
.L14:
  STORE_DOUBLE R3, %6
 vmovsd      xmm0,qword ptr [rsp+048h]
 vmovsd      qword ptr [r14+030h],xmm0
  STORE_TAG R3, tnumber
 mov         dword ptr [r14+03Ch],3
  CHECK_TAG R4, tnumber, bb_exit_1
   ; exit sync: R1, {%6}
 cmp         dword ptr [r14+04Ch],3
 jne         .L15
  %14 = LOAD_DOUBLE R4
 vmovsd      xmm0,qword ptr [r14+040h]
  STORE_DOUBLE R1, %14
 vmovsd      qword ptr [r14+010h],xmm0
  RETURN R1, 3i
 lea         rdi,[r14-010h]
 vmovups     xmm0,xmmword ptr [r14+010h]
 vmovups     xmmword ptr [rdi],xmm0
 vmovups     xmm0,xmmword ptr [r14+020h]
 vmovups     xmmword ptr [rdi+010h],xmm0
 vmovups     xmm0,xmmword ptr [r14+030h]
 vmovups     xmmword ptr [rdi+020h],xmm0
 add         rdi,30h
 mov         ecx,3
 jmp         .L7

)"
    );
}

TEST_CASE_FIXTURE(IrAssemblyFixture, "MultiNumToXSharedSourceStrandsRestore")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    build.beginBlock(entry);

    IrOp d = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    IrOp i = build.inst(IrCmd::NUM_TO_INT, d);
    IrOp u = build.inst(IrCmd::NUM_TO_UINT, d);

    // Kill R1 as a potential restore location
    IrOp doubled = build.inst(IrCmd::ADD_NUM, d, d);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), doubled);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));

    build.inst(IrCmd::INTERRUPT, build.constUint(0));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), build.inst(IrCmd::INT_TO_NUM, i));
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(tnumber));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(3), build.inst(IrCmd::UINT_TO_NUM, u));
    build.inst(IrCmd::STORE_TAG, build.vmReg(3), build.constTag(tnumber));

    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(3));
    updateUseCounts(build.function);

    // Both %1 and %2 restore from stack since R1 restore location was killed
    CHECK_EQ(
        "\n" + lower(),
        R"(
; align 32 using ud2
bb_0:
.L11:
  %0 = LOAD_DOUBLE R1
 vmovsd      xmm0,qword ptr [r14+010h]
  %1 = NUM_TO_INT %0
 vcvttsd2si  eax,xmm0
  %2 = NUM_TO_UINT %0
 vcvttsd2si  rdx,xmm0
  %3 = ADD_NUM %0, %0
 vaddsd      xmm0,xmm0,xmm0
  STORE_DOUBLE R1, %3
 vmovsd      qword ptr [r14+010h],xmm0
  STORE_TAG R1, tnumber
 mov         dword ptr [r14+01Ch],3
  INTERRUPT 0u
 mov         dword ptr [rsp+048h],eax
 mov         dword ptr [rsp+04Ch],edx
 mov         rax,qword ptr [r15+<offset>]
 cmp         qword ptr [rax+<offset>],0
 jne         .L12
.L13:
  %7 = INT_TO_NUM %1
 mov         eax,dword ptr [rsp+048h]
 vcvtsi2sd   xmm0,xmm0,eax
  STORE_DOUBLE R2, %7
 vmovsd      qword ptr [r14+020h],xmm0
  STORE_TAG R2, tnumber
 mov         dword ptr [r14+02Ch],3
  %10 = UINT_TO_NUM %2
 mov         edx,dword ptr [rsp+04Ch]
 mov         eax,edx
 vcvtsi2sd   xmm0,xmm0,rax
  STORE_DOUBLE R3, %10
 vmovsd      qword ptr [r14+030h],xmm0
  STORE_TAG R3, tnumber
 mov         dword ptr [r14+03Ch],3
  RETURN R1, 3i
 lea         rdi,[r14-010h]
 vmovups     xmm0,xmmword ptr [r14+010h]
 vmovups     xmmword ptr [rdi],xmm0
 vmovups     xmm0,xmmword ptr [r14+020h]
 vmovups     xmmword ptr [rdi+010h],xmm0
 vmovups     xmm0,xmmword ptr [r14+030h]
 vmovups     xmmword ptr [rdi+020h],xmm0
 add         rdi,30h
 mov         ecx,3
 jmp         .L7

)"
    );
}

TEST_CASE_FIXTURE(IrAssemblyFixture, "DseHintUpdateRedirectsLazyRestoreToLaterReg")
{
    IrOp entry = build.block(IrBlockKind::Internal);
    build.beginBlock(entry);

    IrOp d = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    IrOp i = build.inst(IrCmd::NUM_TO_INT, d);

    // Kill R1 as a potential non-lazy restore location for 'd'
    IrOp doubled = build.inst(IrCmd::ADD_NUM, d, d);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), doubled);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));

    IrOp roundtrip = build.inst(IrCmd::INT_TO_NUM, i);

    // Two dead stores in R4 and R5, final lazy restore location should be R5
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(4), roundtrip);
    build.inst(IrCmd::STORE_TAG, build.vmReg(4), build.constTag(tnumber));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(5), roundtrip);
    build.inst(IrCmd::STORE_TAG, build.vmReg(5), build.constTag(tnumber));

    build.inst(IrCmd::INTERRUPT, build.constUint(0));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), roundtrip);
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(tnumber));

    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(2));
    updateUseCounts(build.function);

    // INTERRUPT spills to R5 at [r14+050h]
    CHECK_EQ(
        "\n" + lower(),
        R"(
; align 32 using ud2
bb_0:
.L11:
  %0 = LOAD_DOUBLE R1
 vmovsd      xmm0,qword ptr [r14+010h]
  %1 = NUM_TO_INT %0
 vcvttsd2si  eax,xmm0
  %2 = ADD_NUM %0, %0
 vaddsd      xmm0,xmm0,xmm0
  STORE_DOUBLE R1, %2
 vmovsd      qword ptr [r14+010h],xmm0
  STORE_TAG R1, tnumber
 mov         dword ptr [r14+01Ch],3
  %5 = INT_TO_NUM %1
 vcvtsi2sd   xmm0,xmm0,eax
  INTERRUPT 0u
 vmovsd      qword ptr [r14+050h],xmm0
 mov         dword ptr [r14+05Ch],0
 mov         rax,qword ptr [r15+<offset>]
 cmp         qword ptr [rax+<offset>],0
 jne         .L12
.L13:
  STORE_DOUBLE R2, %5
 vmovsd      xmm0,qword ptr [r14+050h]
 vmovsd      qword ptr [r14+020h],xmm0
  STORE_TAG R2, tnumber
 mov         dword ptr [r14+02Ch],3
  RETURN R1, 2i
 lea         rdi,[r14-010h]
 vmovups     xmm0,xmmword ptr [r14+010h]
 vmovups     xmmword ptr [rdi],xmm0
 vmovups     xmm0,xmmword ptr [r14+020h]
 vmovups     xmmword ptr [rdi+010h],xmm0
 add         rdi,20h
 mov         ecx,2
 jmp         .L7

)"
    );
}

TEST_CASE_FIXTURE(IrAssemblyFixture, "IntStackSpillIgnoresConvertedRestoreLocation")
{
    ScopedFastFlag luauCodegenX64IntSpillRestore{FFlag::LuauCodegenX64IntSpillRestore, true};

    options.includeRegSpills = true;

    IrOp entry = build.block(IrBlockKind::Internal);
    IrOp trueBlock = build.block(IrBlockKind::Internal);
    IrOp falseBlock = build.block(IrBlockKind::Internal);

    build.beginBlock(entry);
    IrOp d = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    IrOp i = build.inst(IrCmd::NUM_TO_INT, d);
    build.inst(IrCmd::JUMP_IF_TRUTHY, build.vmReg(2), trueBlock, falseBlock);

    // Interrupt will spill 'i' to stack because cross-block restore location is not used
    build.beginBlock(trueBlock);
    build.inst(IrCmd::INTERRUPT, build.constUint(0));
    build.inst(IrCmd::STORE_INT, build.vmReg(0), i);
    build.inst(IrCmd::STORE_TAG, build.vmReg(0), build.constTag(tboolean));
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(1));

    build.beginBlock(falseBlock);
    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(0));

    updateUseCounts(build.function);

    // 'i' spill restore must be done from stack if it was recorded as such, even when restore location info is available
    CHECK_EQ(
        "\n" + lower(),
        R"(
; align 32 using ud2
bb_0:
.L11:
  %0 = LOAD_DOUBLE R1
 vmovsd      xmm0,qword ptr [r14+010h]
  ; %0 can be restored from R1
  %1 = NUM_TO_INT %0
 vcvttsd2si  eax,xmm0
  ; %1 can be restored from R1 as int
  JUMP_IF_TRUTHY R2, bb_1, bb_2
 cmp         dword ptr [r14+02Ch],0
 je          .L12
 cmp         dword ptr [r14+02Ch],1
 jne         .L13
 cmp         dword ptr [r14+020h],0
 jne         .L13
 jmp         .L12
bb_1:
.L13:
  INTERRUPT 0u
 mov         dword ptr [rsp+048h],eax
  ; spill %1 (int eax) to slot 0
 mov         rax,qword ptr [r15+<offset>]
 cmp         qword ptr [rax+<offset>],0
 jne         .L14
.L15:
  STORE_INT R0, %1
 mov         eax,dword ptr [rsp+048h]
  ; restore %1 (int eax) from slot 0
 mov         dword ptr [r14],eax
  STORE_TAG R0, tboolean
 mov         dword ptr [r14+0Ch],1
  RETURN R0, 1i
 vmovups     xmm0,xmmword ptr [r14]
 vmovups     xmmword ptr [r14-010h],xmm0
 mov         rdi,r14
 mov         ecx,1
 jmp         .L7
bb_2:
.L12:
  RETURN R0, 0i
 lea         rdi,[r14-010h]
 xor         ecx,ecx
 jmp         .L7

)"
    );
}

TEST_CASE_FIXTURE(IrAssemblyFixture, "ExitSyncRestoreConflictedRegisters")
{
    ScopedFastFlag luauCodegenExitSyncUpdate{FFlag::LuauCodegenExitSyncUpdate, true};

    options.includeOutlinedCode = true;
    options.includeRegSpills = true;

    IrOp entry = build.block(IrBlockKind::Internal);
    build.beginBlock(entry);

    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(3)), build.constTag(tnumber), build.vmExit(0));
    IrOp x = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(3));

    // These two stores are dead, but R1 also becomes a restore location for %2 (x) from R3
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), x);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(3), build.constDouble(5.0));
    build.inst(IrCmd::STORE_TAG, build.vmReg(3), build.constTag(tnumber));

    // Interrupt will spill %2 (x) into its restorable location R3
    build.inst(IrCmd::INTERRUPT, build.constUint(0));

    build.inst(IrCmd::CHECK_TAG, build.inst(IrCmd::LOAD_TAG, build.vmReg(4)), build.constTag(tnumber), build.vmExit(0));

    IrOp y = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(4));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), y);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(3), y);
    build.inst(IrCmd::STORE_TAG, build.vmReg(3), build.constTag(tnumber));

    build.inst(IrCmd::RETURN, build.vmReg(0), build.constInt(4));
    updateUseCounts(build.function);

    std::string result = lower();

    // Truncate the output to the interesting part
    if (size_t pos = result.find("bb_exit_1:"); pos != std::string::npos)
        result = result.substr(pos);

    // Exit sync restore order is R3 {5.0}, R1 {x}, so before we store to R3, we must restore %2 (x) which is in R3
    CHECK_EQ(
        "\n" + result,
        R"(
bb_exit_1:
.L15:
 vmovsd      xmm0,qword ptr [r14+030h]
  ; restore %2 (double xmm0) from R3
  STORE_DOUBLE R3, 5
 vmovsd      xmm1,qword ptr [.start-8]
 vmovsd      qword ptr [r14+030h],xmm1
  STORE_TAG R1, tnumber
 mov         dword ptr [r14+01Ch],3
  STORE_DOUBLE R1, %2
  ; %10 can no longer be restored from R1
 vmovsd      qword ptr [r14+010h],xmm0
  JUMP exit(0)
 jmp         .L12
; interrupt handlers
.L13:
 mov         eax,1
 lea         rbx,.L14
 jmp         .L5
; exit handlers
.L12:
 mov         edx,0
 jmp         .L1
.L18:
 ud2

)"
    );
}

TEST_CASE_FIXTURE(IrAssemblyFixture, "LazyHintNotMaterializedInsideScopedSpills")
{
    ScopedFastFlag luauCodegenScopedSpillKeepLazy{FFlag::LuauCodegenScopedSpillKeepLazy, true};

    options.includeRegSpills = true;

    IrOp entry = build.block(IrBlockKind::Internal);
    build.beginBlock(entry);

    IrOp d = build.inst(IrCmd::LOAD_DOUBLE, build.vmReg(1));
    IrOp i = build.inst(IrCmd::NUM_TO_INT, d);

    // Kill R1 as a potential non-lazy restore location for 'd'
    IrOp doubled = build.inst(IrCmd::ADD_NUM, d, d);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(1), doubled);
    build.inst(IrCmd::STORE_TAG, build.vmReg(1), build.constTag(tnumber));

    // Dead store to R4 to create a lazy restore location
    IrOp roundtrip = build.inst(IrCmd::INT_TO_NUM, i);
    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(4), roundtrip);
    build.inst(IrCmd::STORE_TAG, build.vmReg(4), build.constTag(tnumber));

    build.inst(IrCmd::CHECK_GC);

    build.inst(IrCmd::INTERRUPT, build.constUint(0));

    build.inst(IrCmd::STORE_DOUBLE, build.vmReg(2), roundtrip);
    build.inst(IrCmd::STORE_TAG, build.vmReg(2), build.constTag(tnumber));

    build.inst(IrCmd::RETURN, build.vmReg(1), build.constInt(2));
    updateUseCounts(build.function);

    // Both CHECK_GC and INTERRUPT should have a lazy evict of xmm0 into R4
    CHECK_EQ(
        "\n" + lower(),
        R"(
; align 32 using ud2
bb_0:
.L11:
  %0 = LOAD_DOUBLE R1
 vmovsd      xmm0,qword ptr [r14+010h]
  ; %0 can be restored from R1
  %1 = NUM_TO_INT %0
 vcvttsd2si  eax,xmm0
  ; %1 can be restored from R1 as int
  %2 = ADD_NUM %0, %0
 vaddsd      xmm0,xmm0,xmm0
  STORE_DOUBLE R1, %2
  ; %0 can no longer be restored from R1
  ; %1 can no longer be restored from R1 as int
 vmovsd      qword ptr [r14+010h],xmm0
  STORE_TAG R1, tnumber
 mov         dword ptr [r14+01Ch],3
  %5 = INT_TO_NUM %1
 vcvtsi2sd   xmm0,xmm0,eax
  ; %5 has a lazy restore location R4
  CHECK_GC
 mov         rax,qword ptr [r15+<offset>]
 mov         rdx,qword ptr [rax+<offset>]
 cmp         rdx,qword ptr [rax+<offset>]
 jb          .L12
 mov         rcx,r15
 mov         edx,1
 vmovsd      qword ptr [r14+040h],xmm0
 mov         dword ptr [r14+04Ch],0
  ; evict %5 (double xmm0) into R4 [lazy]
 call        qword ptr [r13+0C8h]
 mov         r14,qword ptr [r15+<offset>]
 vmovsd      xmm0,qword ptr [r14+040h]
  ; restore %5 (double xmm0) from R4
.L12:
  INTERRUPT 0u
 vmovsd      qword ptr [r14+040h],xmm0
 mov         dword ptr [r14+04Ch],0
  ; evict %5 (double xmm0) into R4 [lazy]
 mov         rax,qword ptr [r15+<offset>]
 cmp         qword ptr [rax+<offset>],0
 jne         .L13
.L14:
  STORE_DOUBLE R2, %5
 vmovsd      xmm0,qword ptr [r14+040h]
  ; restore %5 (double xmm0) from R4
 vmovsd      qword ptr [r14+020h],xmm0
  STORE_TAG R2, tnumber
 mov         dword ptr [r14+02Ch],3
  RETURN R1, 2i
 lea         rdi,[r14-010h]
 vmovups     xmm0,xmmword ptr [r14+010h]
 vmovups     xmmword ptr [rdi],xmm0
 vmovups     xmm0,xmmword ptr [r14+020h]
 vmovups     xmmword ptr [rdi+010h],xmm0
 add         rdi,20h
 mov         ecx,2
 jmp         .L7

)"
    );
}

TEST_SUITE_END();
