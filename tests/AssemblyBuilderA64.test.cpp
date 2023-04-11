// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AssemblyBuilderA64.h"
#include "Luau/StringUtils.h"

#include "doctest.h"

#include <string.h>

using namespace Luau::CodeGen;
using namespace Luau::CodeGen::A64;

static std::string bytecodeAsArray(const std::vector<uint8_t>& bytecode)
{
    std::string result = "{";

    for (size_t i = 0; i < bytecode.size(); i++)
        Luau::formatAppend(result, "%s0x%02x", i == 0 ? "" : ", ", bytecode[i]);

    return result.append("}");
}

static std::string bytecodeAsArray(const std::vector<uint32_t>& code)
{
    std::string result = "{";

    for (size_t i = 0; i < code.size(); i++)
        Luau::formatAppend(result, "%s0x%08x", i == 0 ? "" : ", ", code[i]);

    return result.append("}");
}

class AssemblyBuilderA64Fixture
{
public:
    bool check(void (*f)(AssemblyBuilderA64& build), std::vector<uint32_t> code, std::vector<uint8_t> data = {}, unsigned int features = 0)
    {
        AssemblyBuilderA64 build(/* logText= */ false, features);

        f(build);

        build.finalize();

        if (build.code != code)
        {
            printf("Expected code: %s\nReceived code: %s\n", bytecodeAsArray(code).c_str(), bytecodeAsArray(build.code).c_str());
            return false;
        }

        if (build.data != data)
        {
            printf("Expected data: %s\nReceived data: %s\n", bytecodeAsArray(data).c_str(), bytecodeAsArray(build.data).c_str());
            return false;
        }

        return true;
    }
};

// armconverter.com can be used to validate instruction sequences
TEST_SUITE_BEGIN("A64Assembly");

#define SINGLE_COMPARE(inst, ...) \
    CHECK(check( \
        [](AssemblyBuilderA64& build) { \
            build.inst; \
        }, \
        {__VA_ARGS__}))

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "Unary")
{
    SINGLE_COMPARE(neg(x0, x1), 0xCB0103E0);
    SINGLE_COMPARE(neg(w0, w1), 0x4B0103E0);
    SINGLE_COMPARE(mvn(x0, x1), 0xAA2103E0);

    SINGLE_COMPARE(clz(x0, x1), 0xDAC01020);
    SINGLE_COMPARE(clz(w0, w1), 0x5AC01020);
    SINGLE_COMPARE(rbit(x0, x1), 0xDAC00020);
    SINGLE_COMPARE(rbit(w0, w1), 0x5AC00020);
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "Binary")
{
    // reg, reg
    SINGLE_COMPARE(add(x0, x1, x2), 0x8B020020);
    SINGLE_COMPARE(add(w0, w1, w2), 0x0B020020);
    SINGLE_COMPARE(add(x0, x1, x2, 7), 0x8B021C20);
    SINGLE_COMPARE(sub(x0, x1, x2), 0xCB020020);
    SINGLE_COMPARE(and_(x0, x1, x2), 0x8A020020);
    SINGLE_COMPARE(orr(x0, x1, x2), 0xAA020020);
    SINGLE_COMPARE(eor(x0, x1, x2), 0xCA020020);
    SINGLE_COMPARE(lsl(x0, x1, x2), 0x9AC22020);
    SINGLE_COMPARE(lsl(w0, w1, w2), 0x1AC22020);
    SINGLE_COMPARE(lsr(x0, x1, x2), 0x9AC22420);
    SINGLE_COMPARE(asr(x0, x1, x2), 0x9AC22820);
    SINGLE_COMPARE(ror(x0, x1, x2), 0x9AC22C20);
    SINGLE_COMPARE(cmp(x0, x1), 0xEB01001F);

    // reg, imm
    SINGLE_COMPARE(add(x3, x7, 78), 0x910138E3);
    SINGLE_COMPARE(add(w3, w7, 78), 0x110138E3);
    SINGLE_COMPARE(sub(w3, w7, 78), 0x510138E3);
    SINGLE_COMPARE(cmp(w0, 42), 0x7100A81F);
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "Loads")
{
    // address forms
    SINGLE_COMPARE(ldr(x0, x1), 0xF9400020);
    SINGLE_COMPARE(ldr(x0, mem(x1, 8)), 0xF9400420);
    SINGLE_COMPARE(ldr(x0, mem(x1, x7)), 0xF8676820);
    SINGLE_COMPARE(ldr(x0, mem(x1, -7)), 0xF85F9020);

    // load sizes
    SINGLE_COMPARE(ldr(x0, x1), 0xF9400020);
    SINGLE_COMPARE(ldr(w0, x1), 0xB9400020);
    SINGLE_COMPARE(ldrb(w0, x1), 0x39400020);
    SINGLE_COMPARE(ldrh(w0, x1), 0x79400020);
    SINGLE_COMPARE(ldrsb(x0, x1), 0x39800020);
    SINGLE_COMPARE(ldrsb(w0, x1), 0x39C00020);
    SINGLE_COMPARE(ldrsh(x0, x1), 0x79800020);
    SINGLE_COMPARE(ldrsh(w0, x1), 0x79C00020);
    SINGLE_COMPARE(ldrsw(x0, x1), 0xB9800020);

    // paired loads
    SINGLE_COMPARE(ldp(x0, x1, mem(x2, 8)), 0xA9408440);
    SINGLE_COMPARE(ldp(w0, w1, mem(x2, -8)), 0x297F0440);
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "Stores")
{
    // address forms
    SINGLE_COMPARE(str(x0, x1), 0xF9000020);
    SINGLE_COMPARE(str(x0, mem(x1, 8)), 0xF9000420);
    SINGLE_COMPARE(str(x0, mem(x1, x7)), 0xF8276820);
    SINGLE_COMPARE(strh(w0, mem(x1, -7)), 0x781F9020);

    // store sizes
    SINGLE_COMPARE(str(x0, x1), 0xF9000020);
    SINGLE_COMPARE(str(w0, x1), 0xB9000020);
    SINGLE_COMPARE(strb(w0, x1), 0x39000020);
    SINGLE_COMPARE(strh(w0, x1), 0x79000020);

    // paired stores
    SINGLE_COMPARE(stp(x0, x1, mem(x2, 8)), 0xA9008440);
    SINGLE_COMPARE(stp(w0, w1, mem(x2, -8)), 0x293F0440);
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "Moves")
{
    SINGLE_COMPARE(mov(x0, x1), 0xAA0103E0);
    SINGLE_COMPARE(mov(w0, w1), 0x2A0103E0);

    SINGLE_COMPARE(movz(x0, 42), 0xD2800540);
    SINGLE_COMPARE(movz(w0, 42), 0x52800540);
    SINGLE_COMPARE(movn(x0, 42), 0x92800540);
    SINGLE_COMPARE(movn(w0, 42), 0x12800540);
    SINGLE_COMPARE(movk(x0, 42, 16), 0xF2A00540);

    CHECK(check(
        [](AssemblyBuilderA64& build) {
            build.mov(x0, 42);
        },
        {0xD2800540}));

    CHECK(check(
        [](AssemblyBuilderA64& build) {
            build.mov(x0, 424242);
        },
        {0xD28F2640, 0xF2A000C0}));

    CHECK(check(
        [](AssemblyBuilderA64& build) {
            build.mov(x0, -42);
        },
        {0x92800520}));

    CHECK(check(
        [](AssemblyBuilderA64& build) {
            build.mov(x0, -424242);
        },
        {0x928F2620, 0xF2BFFF20}));

    CHECK(check(
        [](AssemblyBuilderA64& build) {
            build.mov(x0, -65536);
        },
        {0x929FFFE0}));

    CHECK(check(
        [](AssemblyBuilderA64& build) {
            build.mov(x0, -65537);
        },
        {0x92800000, 0xF2BFFFC0}));
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "ControlFlow")
{
    // Jump back
    CHECK(check(
        [](AssemblyBuilderA64& build) {
            Label start = build.setLabel();
            build.mov(x0, x1);
            build.b(ConditionA64::Equal, start);
        },
        {0xAA0103E0, 0x54FFFFE0}));

    // Jump forward
    CHECK(check(
        [](AssemblyBuilderA64& build) {
            Label skip;
            build.b(ConditionA64::Equal, skip);
            build.mov(x0, x1);
            build.setLabel(skip);
        },
        {0x54000040, 0xAA0103E0}));

    // Jumps
    CHECK(check(
        [](AssemblyBuilderA64& build) {
            Label skip;
            build.b(ConditionA64::Equal, skip);
            build.cbz(x0, skip);
            build.cbnz(x0, skip);
            build.setLabel(skip);
            build.b(skip);
        },
        {0x54000060, 0xB4000040, 0xB5000020, 0x5400000E}));

    // Basic control flow
    SINGLE_COMPARE(br(x0), 0xD61F0000);
    SINGLE_COMPARE(blr(x0), 0xD63F0000);
    SINGLE_COMPARE(ret(), 0xD65F03C0);
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "StackOps")
{
    SINGLE_COMPARE(mov(x0, sp), 0x910003E0);
    SINGLE_COMPARE(mov(sp, x0), 0x9100001F);

    SINGLE_COMPARE(add(sp, sp, 4), 0x910013FF);
    SINGLE_COMPARE(sub(sp, sp, 4), 0xD10013FF);

    SINGLE_COMPARE(add(x0, sp, 4), 0x910013E0);
    SINGLE_COMPARE(sub(sp, x0, 4), 0xD100101F);

    SINGLE_COMPARE(ldr(x0, mem(sp, 8)), 0xF94007E0);
    SINGLE_COMPARE(str(x0, mem(sp, 8)), 0xF90007E0);
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "Constants")
{
    // clang-format off
    CHECK(check(
        [](AssemblyBuilderA64& build) {
            char arr[12] = "hello world";
            build.adr(x0, arr, 12);
            build.adr(x0, uint64_t(0x1234567887654321));
            build.adr(x0, 1.0);
        },
        {
            0x10ffffa0, 0x10ffff20, 0x10fffec0
        },
        {
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f,
            0x21, 0x43, 0x65, 0x87, 0x78, 0x56, 0x34, 0x12,
            0x00, 0x00, 0x00, 0x00, // 4b padding to align double
            'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', 0x0,
        }));
    // clang-format on
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "AddressOfLabel")
{
    // clang-format off
    CHECK(check(
        [](AssemblyBuilderA64& build) {
            Label label;
            build.adr(x0, label);
            build.add(x0, x0, x0);
            build.setLabel(label);
        },
        {
            0x10000040, 0x8b000000,
        }));
    // clang-format on
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "FPBasic")
{
    SINGLE_COMPARE(fmov(d0, d1), 0x1E604020);
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "FPMath")
{
    SINGLE_COMPARE(fabs(d1, d2), 0x1E60C041);
    SINGLE_COMPARE(fadd(d1, d2, d3), 0x1E632841);
    SINGLE_COMPARE(fdiv(d1, d2, d3), 0x1E631841);
    SINGLE_COMPARE(fmul(d1, d2, d3), 0x1E630841);
    SINGLE_COMPARE(fneg(d1, d2), 0x1E614041);
    SINGLE_COMPARE(fsqrt(d1, d2), 0x1E61C041);
    SINGLE_COMPARE(fsub(d1, d2, d3), 0x1E633841);

    SINGLE_COMPARE(frinta(d1, d2), 0x1E664041);
    SINGLE_COMPARE(frintm(d1, d2), 0x1E654041);
    SINGLE_COMPARE(frintp(d1, d2), 0x1E64C041);

    SINGLE_COMPARE(fcvtzs(w1, d2), 0x1E780041);
    SINGLE_COMPARE(fcvtzs(x1, d2), 0x9E780041);
    SINGLE_COMPARE(fcvtzu(w1, d2), 0x1E790041);
    SINGLE_COMPARE(fcvtzu(x1, d2), 0x9E790041);

    SINGLE_COMPARE(scvtf(d1, w2), 0x1E620041);
    SINGLE_COMPARE(scvtf(d1, x2), 0x9E620041);
    SINGLE_COMPARE(ucvtf(d1, w2), 0x1E630041);
    SINGLE_COMPARE(ucvtf(d1, x2), 0x9E630041);

    CHECK(check(
        [](AssemblyBuilderA64& build) {
            build.fjcvtzs(w1, d2);
        },
        {0x1E7E0041}, {}, A64::Feature_JSCVT));
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "FPLoadStore")
{
    // address forms
    SINGLE_COMPARE(ldr(d0, x1), 0xFD400020);
    SINGLE_COMPARE(ldr(d0, mem(x1, 8)), 0xFD400420);
    SINGLE_COMPARE(ldr(d0, mem(x1, x7)), 0xFC676820);
    SINGLE_COMPARE(ldr(d0, mem(x1, -7)), 0xFC5F9020);
    SINGLE_COMPARE(str(d0, x1), 0xFD000020);
    SINGLE_COMPARE(str(d0, mem(x1, 8)), 0xFD000420);
    SINGLE_COMPARE(str(d0, mem(x1, x7)), 0xFC276820);
    SINGLE_COMPARE(str(d0, mem(x1, -7)), 0xFC1F9020);

    // load/store sizes
    SINGLE_COMPARE(ldr(d0, x1), 0xFD400020);
    SINGLE_COMPARE(ldr(q0, x1), 0x3DC00020);
    SINGLE_COMPARE(str(d0, x1), 0xFD000020);
    SINGLE_COMPARE(str(q0, x1), 0x3D800020);
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "FPCompare")
{
    SINGLE_COMPARE(fcmp(d0, d1), 0x1E612000);
    SINGLE_COMPARE(fcmpz(d1), 0x1E602028);
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "AddressOffsetSize")
{
    SINGLE_COMPARE(ldr(w0, mem(x1, 16)), 0xB9401020);
    SINGLE_COMPARE(ldr(x0, mem(x1, 16)), 0xF9400820);
    SINGLE_COMPARE(ldr(d0, mem(x1, 16)), 0xFD400820);
    SINGLE_COMPARE(ldr(q0, mem(x1, 16)), 0x3DC00420);

    SINGLE_COMPARE(str(w0, mem(x1, 16)), 0xB9001020);
    SINGLE_COMPARE(str(x0, mem(x1, 16)), 0xF9000820);
    SINGLE_COMPARE(str(d0, mem(x1, 16)), 0xFD000820);
    SINGLE_COMPARE(str(q0, mem(x1, 16)), 0x3D800420);
}

TEST_CASE_FIXTURE(AssemblyBuilderA64Fixture, "ConditionalSelect")
{
    SINGLE_COMPARE(csel(x0, x1, x2, ConditionA64::Equal), 0x9A820020);
    SINGLE_COMPARE(csel(w0, w1, w2, ConditionA64::Equal), 0x1A820020);
    SINGLE_COMPARE(fcsel(d0, d1, d2, ConditionA64::Equal), 0x1E620C20);
}

TEST_CASE("LogTest")
{
    AssemblyBuilderA64 build(/* logText= */ true);

    build.add(sp, sp, 4);
    build.add(w0, w1, w2);
    build.add(x0, x1, x2, 2);
    build.add(w7, w8, 5);
    build.add(x7, x8, 5);
    build.ldr(x7, x8);
    build.ldr(x7, mem(x8, 8));
    build.ldr(x7, mem(x8, x9));
    build.mov(x1, x2);
    build.movk(x1, 42, 16);
    build.cmp(x1, x2);
    build.blr(x0);

    Label l;
    build.b(ConditionA64::Plus, l);
    build.cbz(x7, l);

    build.ldp(x0, x1, mem(x8, 8));
    build.adr(x0, l);

    build.fabs(d1, d2);
    build.ldr(q1, x2);

    build.csel(x0, x1, x2, ConditionA64::Equal);

    build.fcmp(d0, d1);
    build.fcmpz(d0);

    build.setLabel(l);
    build.ret();

    build.finalize();

    std::string expected = R"(
 add         sp,sp,#4
 add         w0,w1,w2
 add         x0,x1,x2 LSL #2
 add         w7,w8,#5
 add         x7,x8,#5
 ldr         x7,[x8]
 ldr         x7,[x8,#8]
 ldr         x7,[x8,x9]
 mov         x1,x2
 movk        x1,#42 LSL #16
 cmp         x1,x2
 blr         x0
 b.pl        .L1
 cbz         x7,.L1
 ldp         x0,x1,[x8,#8]
 adr         x0,.L1
 fabs        d1,d2
 ldr         q1,[x2]
 csel        x0,x1,x2,eq
 fcmp        d0,d1
 fcmp        d0,#0
.L1:
 ret
)";

    CHECK("\n" + build.text == expected);
}

TEST_SUITE_END();
