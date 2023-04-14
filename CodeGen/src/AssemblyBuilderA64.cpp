// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AssemblyBuilderA64.h"

#include "BitUtils.h"
#include "ByteUtils.h"

#include <stdarg.h>

namespace Luau
{
namespace CodeGen
{
namespace A64
{

static const uint8_t codeForCondition[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14};
static_assert(sizeof(codeForCondition) / sizeof(codeForCondition[0]) == size_t(ConditionA64::Count), "all conditions have to be covered");

static const char* textForCondition[] = {
    "b.eq", "b.ne", "b.cs", "b.cc", "b.mi", "b.pl", "b.vs", "b.vc", "b.hi", "b.ls", "b.ge", "b.lt", "b.gt", "b.le", "b.al"};
static_assert(sizeof(textForCondition) / sizeof(textForCondition[0]) == size_t(ConditionA64::Count), "all conditions have to be covered");

const unsigned kMaxAlign = 32;

AssemblyBuilderA64::AssemblyBuilderA64(bool logText, unsigned int features)
    : logText(logText)
    , features(features)
{
    data.resize(4096);
    dataPos = data.size(); // data is filled backwards

    code.resize(1024);
    codePos = code.data();
    codeEnd = code.data() + code.size();
}

AssemblyBuilderA64::~AssemblyBuilderA64()
{
    LUAU_ASSERT(finalized);
}

void AssemblyBuilderA64::mov(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::w || dst.kind == KindA64::x || dst == sp);
    LUAU_ASSERT(dst.kind == src.kind || (dst.kind == KindA64::x && src == sp) || (dst == sp && src.kind == KindA64::x));

    if (dst == sp || src == sp)
        placeR1("mov", dst, src, 0b00'100010'0'000000000000);
    else
        placeSR2("mov", dst, src, 0b01'01010);
}

void AssemblyBuilderA64::mov(RegisterA64 dst, int src)
{
    if (src >= 0)
    {
        movz(dst, src & 0xffff);
        if (src > 0xffff)
            movk(dst, src >> 16, 16);
    }
    else
    {
        movn(dst, ~src & 0xffff);
        if (src < -0x10000)
            movk(dst, (src >> 16) & 0xffff, 16);
    }
}

void AssemblyBuilderA64::movz(RegisterA64 dst, uint16_t src, int shift)
{
    placeI16("movz", dst, src, 0b10'100101, shift);
}

void AssemblyBuilderA64::movn(RegisterA64 dst, uint16_t src, int shift)
{
    placeI16("movn", dst, src, 0b00'100101, shift);
}

void AssemblyBuilderA64::movk(RegisterA64 dst, uint16_t src, int shift)
{
    placeI16("movk", dst, src, 0b11'100101, shift);
}

void AssemblyBuilderA64::add(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, int shift)
{
    placeSR3("add", dst, src1, src2, 0b00'01011, shift);
}

void AssemblyBuilderA64::add(RegisterA64 dst, RegisterA64 src1, uint16_t src2)
{
    placeI12("add", dst, src1, src2, 0b00'10001);
}

void AssemblyBuilderA64::sub(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, int shift)
{
    placeSR3("sub", dst, src1, src2, 0b10'01011, shift);
}

void AssemblyBuilderA64::sub(RegisterA64 dst, RegisterA64 src1, uint16_t src2)
{
    placeI12("sub", dst, src1, src2, 0b10'10001);
}

void AssemblyBuilderA64::neg(RegisterA64 dst, RegisterA64 src)
{
    placeSR2("neg", dst, src, 0b10'01011);
}

void AssemblyBuilderA64::cmp(RegisterA64 src1, RegisterA64 src2)
{
    RegisterA64 dst = src1.kind == KindA64::x ? xzr : wzr;

    placeSR3("cmp", dst, src1, src2, 0b11'01011);
}

void AssemblyBuilderA64::cmp(RegisterA64 src1, uint16_t src2)
{
    RegisterA64 dst = src1.kind == KindA64::x ? xzr : wzr;

    placeI12("cmp", dst, src1, src2, 0b11'10001);
}

void AssemblyBuilderA64::csel(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, ConditionA64 cond)
{
    LUAU_ASSERT(dst.kind == KindA64::x || dst.kind == KindA64::w);

    placeCS("csel", dst, src1, src2, cond, 0b11010'10'0, 0b00);
}

void AssemblyBuilderA64::cset(RegisterA64 dst, ConditionA64 cond)
{
    LUAU_ASSERT(dst.kind == KindA64::x || dst.kind == KindA64::w);

    RegisterA64 src = dst.kind == KindA64::x ? xzr : wzr;

    placeCS("cset", dst, src, src, cond, 0b11010'10'0, 0b01, /* invert= */ 1);
}

void AssemblyBuilderA64::and_(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2)
{
    placeSR3("and", dst, src1, src2, 0b00'01010);
}

void AssemblyBuilderA64::orr(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2)
{
    placeSR3("orr", dst, src1, src2, 0b01'01010);
}

void AssemblyBuilderA64::eor(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2)
{
    placeSR3("eor", dst, src1, src2, 0b10'01010);
}

void AssemblyBuilderA64::bic(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2)
{
    placeSR3("bic", dst, src1, src2, 0b00'01010, /* shift= */ 0, /* N= */ 1);
}

void AssemblyBuilderA64::tst(RegisterA64 src1, RegisterA64 src2)
{
    RegisterA64 dst = src1.kind == KindA64::x ? xzr : wzr;

    placeSR3("tst", dst, src1, src2, 0b11'01010);
}

void AssemblyBuilderA64::mvn(RegisterA64 dst, RegisterA64 src)
{
    placeSR2("mvn", dst, src, 0b01'01010, 0b1);
}

void AssemblyBuilderA64::and_(RegisterA64 dst, RegisterA64 src1, uint32_t src2)
{
    placeBM("and", dst, src1, src2, 0b00'100100);
}

void AssemblyBuilderA64::orr(RegisterA64 dst, RegisterA64 src1, uint32_t src2)
{
    placeBM("orr", dst, src1, src2, 0b01'100100);
}

void AssemblyBuilderA64::eor(RegisterA64 dst, RegisterA64 src1, uint32_t src2)
{
    placeBM("eor", dst, src1, src2, 0b10'100100);
}

void AssemblyBuilderA64::tst(RegisterA64 src1, uint32_t src2)
{
    RegisterA64 dst = src1.kind == KindA64::x ? xzr : wzr;

    placeBM("tst", dst, src1, src2, 0b11'100100);
}

void AssemblyBuilderA64::lsl(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2)
{
    placeR3("lsl", dst, src1, src2, 0b11010110, 0b0010'00);
}

void AssemblyBuilderA64::lsr(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2)
{
    placeR3("lsr", dst, src1, src2, 0b11010110, 0b0010'01);
}

void AssemblyBuilderA64::asr(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2)
{
    placeR3("asr", dst, src1, src2, 0b11010110, 0b0010'10);
}

void AssemblyBuilderA64::ror(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2)
{
    placeR3("ror", dst, src1, src2, 0b11010110, 0b0010'11);
}

void AssemblyBuilderA64::clz(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::w || dst.kind == KindA64::x);
    LUAU_ASSERT(dst.kind == src.kind);

    placeR1("clz", dst, src, 0b10'11010110'00000'00010'0);
}

void AssemblyBuilderA64::rbit(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::w || dst.kind == KindA64::x);
    LUAU_ASSERT(dst.kind == src.kind);

    placeR1("rbit", dst, src, 0b10'11010110'00000'0000'00);
}

void AssemblyBuilderA64::ldr(RegisterA64 dst, AddressA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::x || dst.kind == KindA64::w || dst.kind == KindA64::d || dst.kind == KindA64::q);

    switch (dst.kind)
    {
    case KindA64::w:
        placeA("ldr", dst, src, 0b11100001, 0b10, 2);
        break;
    case KindA64::x:
        placeA("ldr", dst, src, 0b11100001, 0b11, 3);
        break;
    case KindA64::d:
        placeA("ldr", dst, src, 0b11110001, 0b11, 3);
        break;
    case KindA64::q:
        placeA("ldr", dst, src, 0b11110011, 0b00, 4);
        break;
    case KindA64::none:
        LUAU_ASSERT(!"Unexpected register kind");
    }
}

void AssemblyBuilderA64::ldrb(RegisterA64 dst, AddressA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::w);

    placeA("ldrb", dst, src, 0b11100001, 0b00, 2);
}

void AssemblyBuilderA64::ldrh(RegisterA64 dst, AddressA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::w);

    placeA("ldrh", dst, src, 0b11100001, 0b01, 2);
}

void AssemblyBuilderA64::ldrsb(RegisterA64 dst, AddressA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::x || dst.kind == KindA64::w);

    placeA("ldrsb", dst, src, 0b11100010 | uint8_t(dst.kind == KindA64::w), 0b00, 0);
}

void AssemblyBuilderA64::ldrsh(RegisterA64 dst, AddressA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::x || dst.kind == KindA64::w);

    placeA("ldrsh", dst, src, 0b11100010 | uint8_t(dst.kind == KindA64::w), 0b01, 1);
}

void AssemblyBuilderA64::ldrsw(RegisterA64 dst, AddressA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::x);

    placeA("ldrsw", dst, src, 0b11100010, 0b10, 2);
}

void AssemblyBuilderA64::ldp(RegisterA64 dst1, RegisterA64 dst2, AddressA64 src)
{
    LUAU_ASSERT(dst1.kind == KindA64::x || dst1.kind == KindA64::w);
    LUAU_ASSERT(dst1.kind == dst2.kind);

    placeP("ldp", dst1, dst2, src, 0b101'0'010'1, uint8_t(dst1.kind == KindA64::x) << 1, dst1.kind == KindA64::x ? 3 : 2);
}

void AssemblyBuilderA64::str(RegisterA64 src, AddressA64 dst)
{
    LUAU_ASSERT(src.kind == KindA64::x || src.kind == KindA64::w || src.kind == KindA64::d || src.kind == KindA64::q);

    switch (src.kind)
    {
    case KindA64::w:
        placeA("str", src, dst, 0b11100000, 0b10, 2);
        break;
    case KindA64::x:
        placeA("str", src, dst, 0b11100000, 0b11, 3);
        break;
    case KindA64::d:
        placeA("str", src, dst, 0b11110000, 0b11, 3);
        break;
    case KindA64::q:
        placeA("str", src, dst, 0b11110010, 0b00, 4);
        break;
    case KindA64::none:
        LUAU_ASSERT(!"Unexpected register kind");
    }
}

void AssemblyBuilderA64::strb(RegisterA64 src, AddressA64 dst)
{
    LUAU_ASSERT(src.kind == KindA64::w);

    placeA("strb", src, dst, 0b11100000, 0b00, 2);
}

void AssemblyBuilderA64::strh(RegisterA64 src, AddressA64 dst)
{
    LUAU_ASSERT(src.kind == KindA64::w);

    placeA("strh", src, dst, 0b11100000, 0b01, 2);
}

void AssemblyBuilderA64::stp(RegisterA64 src1, RegisterA64 src2, AddressA64 dst)
{
    LUAU_ASSERT(src1.kind == KindA64::x || src1.kind == KindA64::w);
    LUAU_ASSERT(src1.kind == src2.kind);

    placeP("stp", src1, src2, dst, 0b101'0'010'0, uint8_t(src1.kind == KindA64::x) << 1, src1.kind == KindA64::x ? 3 : 2);
}

void AssemblyBuilderA64::b(Label& label)
{
    // Note: we aren't using 'b' form since it has a 26-bit immediate which requires custom fixup logic
    placeBC("b", label, 0b0101010'0, codeForCondition[int(ConditionA64::Always)]);
}

void AssemblyBuilderA64::b(ConditionA64 cond, Label& label)
{
    placeBC(textForCondition[int(cond)], label, 0b0101010'0, codeForCondition[int(cond)]);
}

void AssemblyBuilderA64::cbz(RegisterA64 src, Label& label)
{
    placeBCR("cbz", label, 0b011010'0, src);
}

void AssemblyBuilderA64::cbnz(RegisterA64 src, Label& label)
{
    placeBCR("cbnz", label, 0b011010'1, src);
}

void AssemblyBuilderA64::br(RegisterA64 src)
{
    placeBR("br", src, 0b1101011'0'0'00'11111'0000'0'0);
}

void AssemblyBuilderA64::blr(RegisterA64 src)
{
    placeBR("blr", src, 0b1101011'0'0'01'11111'0000'0'0);
}

void AssemblyBuilderA64::ret()
{
    place0("ret", 0b1101011'0'0'10'11111'0000'0'0'11110'00000);
}

void AssemblyBuilderA64::adr(RegisterA64 dst, const void* ptr, size_t size)
{
    size_t pos = allocateData(size, 4);
    uint32_t location = getCodeSize();

    memcpy(&data[pos], ptr, size);
    placeADR("adr", dst, 0b10000);

    patchImm19(location, -int(location) - int((data.size() - pos) / 4));
}

void AssemblyBuilderA64::adr(RegisterA64 dst, uint64_t value)
{
    size_t pos = allocateData(8, 8);
    uint32_t location = getCodeSize();

    writeu64(&data[pos], value);
    placeADR("adr", dst, 0b10000);

    patchImm19(location, -int(location) - int((data.size() - pos) / 4));
}

void AssemblyBuilderA64::adr(RegisterA64 dst, double value)
{
    size_t pos = allocateData(8, 8);
    uint32_t location = getCodeSize();

    writef64(&data[pos], value);
    placeADR("adr", dst, 0b10000);

    patchImm19(location, -int(location) - int((data.size() - pos) / 4));
}

void AssemblyBuilderA64::adr(RegisterA64 dst, Label& label)
{
    placeADR("adr", dst, 0b10000, label);
}

void AssemblyBuilderA64::fmov(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::d && src.kind == KindA64::d);

    placeR1("fmov", dst, src, 0b000'11110'01'1'0000'00'10000);
}

void AssemblyBuilderA64::fabs(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::d && src.kind == KindA64::d);

    placeR1("fabs", dst, src, 0b000'11110'01'1'0000'01'10000);
}

void AssemblyBuilderA64::fadd(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2)
{
    LUAU_ASSERT(dst.kind == KindA64::d && src1.kind == KindA64::d && src2.kind == KindA64::d);

    placeR3("fadd", dst, src1, src2, 0b11110'01'1, 0b0010'10);
}

void AssemblyBuilderA64::fdiv(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2)
{
    LUAU_ASSERT(dst.kind == KindA64::d && src1.kind == KindA64::d && src2.kind == KindA64::d);

    placeR3("fdiv", dst, src1, src2, 0b11110'01'1, 0b0001'10);
}

void AssemblyBuilderA64::fmul(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2)
{
    LUAU_ASSERT(dst.kind == KindA64::d && src1.kind == KindA64::d && src2.kind == KindA64::d);

    placeR3("fmul", dst, src1, src2, 0b11110'01'1, 0b0000'10);
}

void AssemblyBuilderA64::fneg(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::d && src.kind == KindA64::d);

    placeR1("fneg", dst, src, 0b000'11110'01'1'0000'10'10000);
}

void AssemblyBuilderA64::fsqrt(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::d && src.kind == KindA64::d);

    placeR1("fsqrt", dst, src, 0b000'11110'01'1'0000'11'10000);
}

void AssemblyBuilderA64::fsub(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2)
{
    LUAU_ASSERT(dst.kind == KindA64::d && src1.kind == KindA64::d && src2.kind == KindA64::d);

    placeR3("fsub", dst, src1, src2, 0b11110'01'1, 0b0011'10);
}

void AssemblyBuilderA64::frinta(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::d && src.kind == KindA64::d);

    placeR1("frinta", dst, src, 0b000'11110'01'1'001'100'10000);
}

void AssemblyBuilderA64::frintm(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::d && src.kind == KindA64::d);

    placeR1("frintm", dst, src, 0b000'11110'01'1'001'010'10000);
}

void AssemblyBuilderA64::frintp(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::d && src.kind == KindA64::d);

    placeR1("frintp", dst, src, 0b000'11110'01'1'001'001'10000);
}

void AssemblyBuilderA64::fcvtzs(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::w || dst.kind == KindA64::x);
    LUAU_ASSERT(src.kind == KindA64::d);

    placeR1("fcvtzs", dst, src, 0b000'11110'01'1'11'000'000000);
}

void AssemblyBuilderA64::fcvtzu(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::w || dst.kind == KindA64::x);
    LUAU_ASSERT(src.kind == KindA64::d);

    placeR1("fcvtzu", dst, src, 0b000'11110'01'1'11'001'000000);
}

void AssemblyBuilderA64::scvtf(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::d);
    LUAU_ASSERT(src.kind == KindA64::w || src.kind == KindA64::x);

    placeR1("scvtf", dst, src, 0b000'11110'01'1'00'010'000000);
}

void AssemblyBuilderA64::ucvtf(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::d);
    LUAU_ASSERT(src.kind == KindA64::w || src.kind == KindA64::x);

    placeR1("ucvtf", dst, src, 0b000'11110'01'1'00'011'000000);
}

void AssemblyBuilderA64::fjcvtzs(RegisterA64 dst, RegisterA64 src)
{
    LUAU_ASSERT(dst.kind == KindA64::w);
    LUAU_ASSERT(src.kind == KindA64::d);
    LUAU_ASSERT(features & Feature_JSCVT);

    placeR1("fjcvtzs", dst, src, 0b000'11110'01'1'11'110'000000);
}

void AssemblyBuilderA64::fcmp(RegisterA64 src1, RegisterA64 src2)
{
    LUAU_ASSERT(src1.kind == KindA64::d && src2.kind == KindA64::d);

    placeFCMP("fcmp", src1, src2, 0b11110'01'1, 0b00);
}

void AssemblyBuilderA64::fcmpz(RegisterA64 src)
{
    LUAU_ASSERT(src.kind == KindA64::d);

    placeFCMP("fcmp", src, RegisterA64{src.kind, 0}, 0b11110'01'1, 0b01);
}

void AssemblyBuilderA64::fcsel(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, ConditionA64 cond)
{
    LUAU_ASSERT(dst.kind == KindA64::d);

    placeCS("fcsel", dst, src1, src2, cond, 0b11110'01'1, 0b11);
}

bool AssemblyBuilderA64::finalize()
{
    code.resize(codePos - code.data());

    // Resolve jump targets
    for (Label fixup : pendingLabels)
    {
        // If this assertion fires, a label was used in jmp without calling setLabel
        LUAU_ASSERT(labelLocations[fixup.id - 1] != ~0u);
        int value = int(labelLocations[fixup.id - 1]) - int(fixup.location);

        patchImm19(fixup.location, value);
    }

    size_t dataSize = data.size() - dataPos;

    // Shrink data
    if (dataSize > 0)
        memmove(&data[0], &data[dataPos], dataSize);

    data.resize(dataSize);

    finalized = true;

    return !overflowed;
}

Label AssemblyBuilderA64::setLabel()
{
    Label label{nextLabel++, getCodeSize()};
    labelLocations.push_back(~0u);

    if (logText)
        log(label);

    return label;
}

void AssemblyBuilderA64::setLabel(Label& label)
{
    if (label.id == 0)
    {
        label.id = nextLabel++;
        labelLocations.push_back(~0u);
    }

    label.location = getCodeSize();
    labelLocations[label.id - 1] = label.location;

    if (logText)
        log(label);
}

void AssemblyBuilderA64::logAppend(const char* fmt, ...)
{
    char buf[256];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    text.append(buf);
}

uint32_t AssemblyBuilderA64::getCodeSize() const
{
    return uint32_t(codePos - code.data());
}

void AssemblyBuilderA64::place0(const char* name, uint32_t op)
{
    if (logText)
        log(name);

    place(op);
    commit();
}

void AssemblyBuilderA64::placeSR3(const char* name, RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, uint8_t op, int shift, int N)
{
    if (logText)
        log(name, dst, src1, src2, shift);

    LUAU_ASSERT(dst.kind == KindA64::w || dst.kind == KindA64::x);
    LUAU_ASSERT(dst.kind == src1.kind && dst.kind == src2.kind);
    LUAU_ASSERT(shift >= 0 && shift < 64); // right shift requires changing some encoding bits

    uint32_t sf = (dst.kind == KindA64::x) ? 0x80000000 : 0;

    place(dst.index | (src1.index << 5) | (shift << 10) | (src2.index << 16) | (N << 21) | (op << 24) | sf);
    commit();
}

void AssemblyBuilderA64::placeSR2(const char* name, RegisterA64 dst, RegisterA64 src, uint8_t op, uint8_t op2)
{
    if (logText)
        log(name, dst, src);

    LUAU_ASSERT(dst.kind == KindA64::w || dst.kind == KindA64::x);
    LUAU_ASSERT(dst.kind == src.kind);

    uint32_t sf = (dst.kind == KindA64::x) ? 0x80000000 : 0;

    place(dst.index | (0x1f << 5) | (src.index << 16) | (op2 << 21) | (op << 24) | sf);
    commit();
}

void AssemblyBuilderA64::placeR3(const char* name, RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, uint8_t op, uint8_t op2)
{
    if (logText)
        log(name, dst, src1, src2);

    LUAU_ASSERT(dst.kind == KindA64::w || dst.kind == KindA64::x || dst.kind == KindA64::d);
    LUAU_ASSERT(dst.kind == src1.kind && dst.kind == src2.kind);

    uint32_t sf = (dst.kind == KindA64::x) ? 0x80000000 : 0;

    place(dst.index | (src1.index << 5) | (op2 << 10) | (src2.index << 16) | (op << 21) | sf);
    commit();
}

void AssemblyBuilderA64::placeR1(const char* name, RegisterA64 dst, RegisterA64 src, uint32_t op)
{
    if (logText)
        log(name, dst, src);

    uint32_t sf = (dst.kind == KindA64::x || src.kind == KindA64::x) ? 0x80000000 : 0;

    place(dst.index | (src.index << 5) | (op << 10) | sf);
    commit();
}

void AssemblyBuilderA64::placeI12(const char* name, RegisterA64 dst, RegisterA64 src1, int src2, uint8_t op)
{
    if (logText)
        log(name, dst, src1, src2);

    LUAU_ASSERT(dst.kind == KindA64::w || dst.kind == KindA64::x || dst == sp);
    LUAU_ASSERT(dst.kind == src1.kind || (dst.kind == KindA64::x && src1 == sp) || (dst == sp && src1.kind == KindA64::x));
    LUAU_ASSERT(src2 >= 0 && src2 < (1 << 12));

    uint32_t sf = (dst.kind != KindA64::w) ? 0x80000000 : 0;

    place(dst.index | (src1.index << 5) | (src2 << 10) | (op << 24) | sf);
    commit();
}

void AssemblyBuilderA64::placeI16(const char* name, RegisterA64 dst, int src, uint8_t op, int shift)
{
    if (logText)
        log(name, dst, src, shift);

    LUAU_ASSERT(dst.kind == KindA64::w || dst.kind == KindA64::x);
    LUAU_ASSERT(src >= 0 && src <= 0xffff);
    LUAU_ASSERT(shift == 0 || shift == 16 || shift == 32 || shift == 48);

    uint32_t sf = (dst.kind == KindA64::x) ? 0x80000000 : 0;

    place(dst.index | (src << 5) | ((shift >> 4) << 21) | (op << 23) | sf);
    commit();
}

void AssemblyBuilderA64::placeA(const char* name, RegisterA64 dst, AddressA64 src, uint8_t op, uint8_t size, int sizelog)
{
    if (logText)
        log(name, dst, src);

    switch (src.kind)
    {
    case AddressKindA64::imm:
        if (src.data >= 0 && (src.data >> sizelog) < 1024 && (src.data & ((1 << sizelog) - 1)) == 0)
            place(dst.index | (src.base.index << 5) | ((src.data >> sizelog) << 10) | (op << 22) | (1 << 24) | (size << 30));
        else if (src.data >= -256 && src.data <= 255)
            place(dst.index | (src.base.index << 5) | ((src.data & ((1 << 9) - 1)) << 12) | (op << 22) | (size << 30));
        else
            LUAU_ASSERT(!"Unable to encode large immediate offset");
        break;
    case AddressKindA64::reg:
        place(dst.index | (src.base.index << 5) | (0b10 << 10) | (0b011 << 13) | (src.offset.index << 16) | (1 << 21) | (op << 22) | (size << 30));
        break;
    }

    commit();
}

void AssemblyBuilderA64::placeBC(const char* name, Label& label, uint8_t op, uint8_t cond)
{
    place(cond | (op << 24));
    commit();

    patchLabel(label);

    if (logText)
        log(name, label);
}

void AssemblyBuilderA64::placeBCR(const char* name, Label& label, uint8_t op, RegisterA64 cond)
{
    LUAU_ASSERT(cond.kind == KindA64::w || cond.kind == KindA64::x);

    uint32_t sf = (cond.kind == KindA64::x) ? 0x80000000 : 0;

    place(cond.index | (op << 24) | sf);
    commit();

    patchLabel(label);

    if (logText)
        log(name, cond, label);
}

void AssemblyBuilderA64::placeBR(const char* name, RegisterA64 src, uint32_t op)
{
    if (logText)
        log(name, src);

    LUAU_ASSERT(src.kind == KindA64::x);

    place((src.index << 5) | (op << 10));
    commit();
}

void AssemblyBuilderA64::placeADR(const char* name, RegisterA64 dst, uint8_t op)
{
    if (logText)
        log(name, dst);

    LUAU_ASSERT(dst.kind == KindA64::x);

    place(dst.index | (op << 24));
    commit();
}

void AssemblyBuilderA64::placeADR(const char* name, RegisterA64 dst, uint8_t op, Label& label)
{
    LUAU_ASSERT(dst.kind == KindA64::x);

    place(dst.index | (op << 24));
    commit();

    patchLabel(label);

    if (logText)
        log(name, dst, label);
}

void AssemblyBuilderA64::placeP(const char* name, RegisterA64 src1, RegisterA64 src2, AddressA64 dst, uint8_t op, uint8_t opc, int sizelog)
{
    if (logText)
        log(name, src1, src2, dst);

    LUAU_ASSERT(dst.kind == AddressKindA64::imm);
    LUAU_ASSERT(dst.data >= -128 * (1 << sizelog) && dst.data <= 127 * (1 << sizelog));
    LUAU_ASSERT(dst.data % (1 << sizelog) == 0);

    place(src1.index | (dst.base.index << 5) | (src2.index << 10) | (((dst.data >> sizelog) & 127) << 15) | (op << 22) | (opc << 30));
    commit();
}

void AssemblyBuilderA64::placeCS(
    const char* name, RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, ConditionA64 cond, uint8_t op, uint8_t opc, int invert)
{
    if (logText)
        log(name, dst, src1, src2, cond);

    LUAU_ASSERT(dst.kind == src1.kind && dst.kind == src2.kind);

    uint32_t sf = (dst.kind == KindA64::x) ? 0x80000000 : 0;

    place(dst.index | (src1.index << 5) | (opc << 10) | ((codeForCondition[int(cond)] ^ invert) << 12) | (src2.index << 16) | (op << 21) | sf);
    commit();
}

void AssemblyBuilderA64::placeFCMP(const char* name, RegisterA64 src1, RegisterA64 src2, uint8_t op, uint8_t opc)
{
    if (logText)
    {
        if (opc)
            log(name, src1, 0);
        else
            log(name, src1, src2);
    }

    LUAU_ASSERT(src1.kind == src2.kind);

    place((opc << 3) | (src1.index << 5) | (0b1000 << 10) | (src2.index << 16) | (op << 21));
    commit();
}

void AssemblyBuilderA64::placeBM(const char* name, RegisterA64 dst, RegisterA64 src1, uint32_t src2, uint8_t op)
{
    if (logText)
        log(name, dst, src1, src2);

    LUAU_ASSERT(dst.kind == KindA64::w || dst.kind == KindA64::x);
    LUAU_ASSERT(dst.kind == src1.kind);

    uint32_t sf = (dst.kind == KindA64::x) ? 0x80000000 : 0;

    int lz = countlz(src2);
    int rz = countrz(src2);

    LUAU_ASSERT(lz + rz > 0 && lz + rz < 32);               // must have at least one 0 and at least one 1
    LUAU_ASSERT((src2 >> rz) == (1 << (32 - lz - rz)) - 1); // sequence of 1s must be contiguous

    int imms = 31 - lz - rz;   // count of 1s minus 1
    int immr = (32 - rz) & 31; // right rotate amount

    place(dst.index | (src1.index << 5) | (imms << 10) | (immr << 16) | (op << 23) | sf);
    commit();
}

void AssemblyBuilderA64::place(uint32_t word)
{
    LUAU_ASSERT(codePos < codeEnd);
    *codePos++ = word;
}

void AssemblyBuilderA64::patchLabel(Label& label)
{
    uint32_t location = getCodeSize() - 1;

    if (label.location == ~0u)
    {
        if (label.id == 0)
        {
            label.id = nextLabel++;
            labelLocations.push_back(~0u);
        }

        pendingLabels.push_back({label.id, location});
    }
    else
    {
        int value = int(label.location) - int(location);

        patchImm19(location, value);
    }
}

void AssemblyBuilderA64::patchImm19(uint32_t location, int value)
{
    // imm19 encoding word offset, at bit offset 5
    // note that 18 bits of word offsets = 20 bits of byte offsets = +-1MB
    if (value > -(1 << 18) && value < (1 << 18))
        code[location] |= (value & ((1 << 19) - 1)) << 5;
    else
        overflowed = true;
}

void AssemblyBuilderA64::commit()
{
    LUAU_ASSERT(codePos <= codeEnd);

    if (codeEnd == codePos)
        extend();
}

void AssemblyBuilderA64::extend()
{
    uint32_t count = getCodeSize();

    code.resize(code.size() * 2);
    codePos = code.data() + count;
    codeEnd = code.data() + code.size();
}

size_t AssemblyBuilderA64::allocateData(size_t size, size_t align)
{
    LUAU_ASSERT(align > 0 && align <= kMaxAlign && (align & (align - 1)) == 0);

    if (dataPos < size)
    {
        size_t oldSize = data.size();
        data.resize(data.size() * 2);
        memcpy(&data[oldSize], &data[0], oldSize);
        memset(&data[0], 0, oldSize);
        dataPos += oldSize;
    }

    dataPos = (dataPos - size) & ~(align - 1);

    return dataPos;
}

void AssemblyBuilderA64::log(const char* opcode)
{
    logAppend(" %s\n", opcode);
}

void AssemblyBuilderA64::log(const char* opcode, RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, int shift)
{
    logAppend(" %-12s", opcode);
    if (dst != xzr && dst != wzr)
    {
        log(dst);
        text.append(",");
    }
    log(src1);
    text.append(",");
    log(src2);
    if (shift > 0)
        logAppend(" LSL #%d", shift);
    text.append("\n");
}

void AssemblyBuilderA64::log(const char* opcode, RegisterA64 dst, RegisterA64 src1, int src2)
{
    logAppend(" %-12s", opcode);
    if (dst != xzr && dst != wzr)
    {
        log(dst);
        text.append(",");
    }
    log(src1);
    text.append(",");
    logAppend("#%d", src2);
    text.append("\n");
}

void AssemblyBuilderA64::log(const char* opcode, RegisterA64 dst, AddressA64 src)
{
    logAppend(" %-12s", opcode);
    log(dst);
    text.append(",");
    log(src);
    text.append("\n");
}

void AssemblyBuilderA64::log(const char* opcode, RegisterA64 dst1, RegisterA64 dst2, AddressA64 src)
{
    logAppend(" %-12s", opcode);
    log(dst1);
    text.append(",");
    log(dst2);
    text.append(",");
    log(src);
    text.append("\n");
}

void AssemblyBuilderA64::log(const char* opcode, RegisterA64 dst, RegisterA64 src)
{
    logAppend(" %-12s", opcode);
    log(dst);
    text.append(",");
    log(src);
    text.append("\n");
}

void AssemblyBuilderA64::log(const char* opcode, RegisterA64 dst, int src, int shift)
{
    logAppend(" %-12s", opcode);
    log(dst);
    text.append(",");
    logAppend("#%d", src);
    if (shift > 0)
        logAppend(" LSL #%d", shift);
    text.append("\n");
}

void AssemblyBuilderA64::log(const char* opcode, RegisterA64 src, Label label)
{
    logAppend(" %-12s", opcode);
    log(src);
    text.append(",");
    logAppend(".L%d\n", label.id);
}

void AssemblyBuilderA64::log(const char* opcode, RegisterA64 src)
{
    logAppend(" %-12s", opcode);
    log(src);
    text.append("\n");
}

void AssemblyBuilderA64::log(const char* opcode, Label label)
{
    logAppend(" %-12s.L%d\n", opcode, label.id);
}

void AssemblyBuilderA64::log(const char* opcode, RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, ConditionA64 cond)
{
    logAppend(" %-12s", opcode);
    log(dst);
    if ((src1 != wzr && src1 != xzr) || (src2 != wzr && src2 != xzr))
    {
        text.append(",");
        log(src1);
        text.append(",");
        log(src2);
    }
    text.append(",");
    text.append(textForCondition[int(cond)] + 2); // skip b.
    text.append("\n");
}

void AssemblyBuilderA64::log(Label label)
{
    logAppend(".L%d:\n", label.id);
}

void AssemblyBuilderA64::log(RegisterA64 reg)
{
    switch (reg.kind)
    {
    case KindA64::w:
        if (reg.index == 31)
            text.append("wzr");
        else
            logAppend("w%d", reg.index);
        break;

    case KindA64::x:
        if (reg.index == 31)
            text.append("xzr");
        else
            logAppend("x%d", reg.index);
        break;

    case KindA64::d:
        logAppend("d%d", reg.index);
        break;

    case KindA64::q:
        logAppend("q%d", reg.index);
        break;

    case KindA64::none:
        if (reg.index == 31)
            text.append("sp");
        else
            LUAU_ASSERT(!"Unexpected register kind");
        break;
    }
}

void AssemblyBuilderA64::log(AddressA64 addr)
{
    text.append("[");
    switch (addr.kind)
    {
    case AddressKindA64::imm:
        log(addr.base);
        if (addr.data != 0)
            logAppend(",#%d", addr.data);
        break;
    case AddressKindA64::reg:
        log(addr.base);
        text.append(",");
        log(addr.offset);
        if (addr.data != 0)
            logAppend(" LSL #%d", addr.data);
        break;
    }
    text.append("]");
}

} // namespace A64
} // namespace CodeGen
} // namespace Luau
