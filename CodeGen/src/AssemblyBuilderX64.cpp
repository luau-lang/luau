// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AssemblyBuilderX64.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

namespace Luau
{
namespace CodeGen
{
// TODO: more assertions on operand sizes

const uint8_t codeForCondition[] = {
    0x0, 0x1, 0x2, 0x3, 0x2, 0x6, 0x7, 0x3, 0x4, 0xc, 0xe, 0xf, 0xd, 0x3, 0x7, 0x6, 0x2, 0x5, 0xd, 0xf, 0xe, 0xc, 0x4, 0x5};
static_assert(sizeof(codeForCondition) / sizeof(codeForCondition[0]) == size_t(Condition::Count), "all conditions have to be covered");

#define OP_PLUS_REG(op, reg) ((op) + (reg & 0x7))
#define OP_PLUS_CC(op, cc) ((op) + uint8_t(cc))

#define REX_W(value) (value ? 0x8 : 0x0)
#define REX_R(reg) (((reg).index & 0x8) >> 1)
#define REX_X(reg) (((reg).index & 0x8) >> 2)
#define REX_B(reg) (((reg).index & 0x8) >> 3)

#define AVX_W(value) (!(value) ? 0x80 : 0x0)
#define AVX_R(reg) ((~(reg).index & 0x8) << 4)
#define AVX_X(reg) ((~(reg).index & 0x8) << 3)
#define AVX_B(reg) ((~(reg).index & 0x8) << 2)

#define AVX_3_1() 0b11000100
#define AVX_3_2(r, x, b, m) (AVX_R(r) | AVX_X(x) | AVX_B(b) | (m))
#define AVX_3_3(w, v, l, p) (AVX_W(w) | ((~(v.index) & 0xf) << 3) | ((l) << 2) | (p))

#define MOD_RM(mod, reg, rm) (((mod) << 6) | (((reg)&0x7) << 3) | ((rm)&0x7))
#define SIB(scale, index, base) ((getScaleEncoding(scale) << 6) | (((index)&0x7) << 3) | ((base)&0x7))

const unsigned AVX_0F = 0b0001;
[[maybe_unused]] const unsigned AVX_0F38 = 0b0010;
[[maybe_unused]] const unsigned AVX_0F3A = 0b0011;

const unsigned AVX_NP = 0b00;
const unsigned AVX_66 = 0b01;
const unsigned AVX_F3 = 0b10;
const unsigned AVX_F2 = 0b11;

const unsigned kMaxAlign = 16;

AssemblyBuilderX64::AssemblyBuilderX64(bool logText)
    : logText(logText)
{
    data.resize(4096);
    dataPos = data.size(); // data is filled backwards

    code.resize(4096);
    codePos = code.data();
    codeEnd = code.data() + code.size();
}

AssemblyBuilderX64::~AssemblyBuilderX64()
{
    LUAU_ASSERT(finalized);
}

void AssemblyBuilderX64::add(OperandX64 lhs, OperandX64 rhs)
{
    placeBinary("add", lhs, rhs, 0x80, 0x81, 0x83, 0x00, 0x01, 0x02, 0x03, 0);
}

void AssemblyBuilderX64::sub(OperandX64 lhs, OperandX64 rhs)
{
    placeBinary("sub", lhs, rhs, 0x80, 0x81, 0x83, 0x28, 0x29, 0x2a, 0x2b, 5);
}

void AssemblyBuilderX64::cmp(OperandX64 lhs, OperandX64 rhs)
{
    placeBinary("cmp", lhs, rhs, 0x80, 0x81, 0x83, 0x38, 0x39, 0x3a, 0x3b, 7);
}

void AssemblyBuilderX64::and_(OperandX64 lhs, OperandX64 rhs)
{
    placeBinary("and", lhs, rhs, 0x80, 0x81, 0x83, 0x20, 0x21, 0x22, 0x23, 4);
}

void AssemblyBuilderX64::or_(OperandX64 lhs, OperandX64 rhs)
{
    placeBinary("or", lhs, rhs, 0x80, 0x81, 0x83, 0x08, 0x09, 0x0a, 0x0b, 1);
}

void AssemblyBuilderX64::xor_(OperandX64 lhs, OperandX64 rhs)
{
    placeBinary("xor", lhs, rhs, 0x80, 0x81, 0x83, 0x30, 0x31, 0x32, 0x33, 6);
}

void AssemblyBuilderX64::sal(OperandX64 lhs, OperandX64 rhs)
{
    placeShift("sal", lhs, rhs, 4);
}

void AssemblyBuilderX64::sar(OperandX64 lhs, OperandX64 rhs)
{
    placeShift("sar", lhs, rhs, 7);
}

void AssemblyBuilderX64::shl(OperandX64 lhs, OperandX64 rhs)
{
    placeShift("shl", lhs, rhs, 4); // same as sal
}

void AssemblyBuilderX64::shr(OperandX64 lhs, OperandX64 rhs)
{
    placeShift("shr", lhs, rhs, 5);
}

void AssemblyBuilderX64::mov(OperandX64 lhs, OperandX64 rhs)
{
    if (logText)
        log("mov", lhs, rhs);

    if (lhs.cat == CategoryX64::reg && rhs.cat == CategoryX64::imm)
    {
        SizeX64 size = lhs.base.size;

        placeRex(lhs.base);

        if (size == SizeX64::byte)
        {
            place(OP_PLUS_REG(0xb0, lhs.base.index));
            placeImm8(rhs.imm);
        }
        else if (size == SizeX64::dword)
        {
            place(OP_PLUS_REG(0xb8, lhs.base.index));
            placeImm32(rhs.imm);
        }
        else
        {
            LUAU_ASSERT(size == SizeX64::qword);

            place(OP_PLUS_REG(0xb8, lhs.base.index));
            placeImm64(rhs.imm);
        }
    }
    else if (lhs.cat == CategoryX64::mem && rhs.cat == CategoryX64::imm)
    {
        SizeX64 size = lhs.memSize;

        placeRex(lhs);

        if (size == SizeX64::byte)
        {
            place(0xc6);
            placeModRegMem(lhs, 0);
            placeImm8(rhs.imm);
        }
        else
        {
            LUAU_ASSERT(size == SizeX64::dword || size == SizeX64::qword);

            place(0xc7);
            placeModRegMem(lhs, 0);
            placeImm32(rhs.imm);
        }
    }
    else if (lhs.cat == CategoryX64::reg && (rhs.cat == CategoryX64::reg || rhs.cat == CategoryX64::mem))
    {
        placeBinaryRegAndRegMem(lhs, rhs, 0x8a, 0x8b);
    }
    else if (lhs.cat == CategoryX64::mem && rhs.cat == CategoryX64::reg)
    {
        placeBinaryRegMemAndReg(lhs, rhs, 0x88, 0x89);
    }
    else
    {
        LUAU_ASSERT(!"No encoding for this operand combination");
    }

    commit();
}

void AssemblyBuilderX64::mov64(RegisterX64 lhs, int64_t imm)
{
    if (logText)
    {
        text.append(" mov         ");
        log(lhs);
        logAppend(",%llXh\n", (unsigned long long)imm);
    }

    LUAU_ASSERT(lhs.size == SizeX64::qword);

    placeRex(lhs);
    place(OP_PLUS_REG(0xb8, lhs.index));
    placeImm64(imm);
    commit();
}

void AssemblyBuilderX64::div(OperandX64 op)
{
    placeUnaryModRegMem("div", op, 0xf6, 0xf7, 6);
}

void AssemblyBuilderX64::idiv(OperandX64 op)
{
    placeUnaryModRegMem("idiv", op, 0xf6, 0xf7, 7);
}

void AssemblyBuilderX64::mul(OperandX64 op)
{
    placeUnaryModRegMem("mul", op, 0xf6, 0xf7, 4);
}

void AssemblyBuilderX64::neg(OperandX64 op)
{
    placeUnaryModRegMem("neg", op, 0xf6, 0xf7, 3);
}

void AssemblyBuilderX64::not_(OperandX64 op)
{
    placeUnaryModRegMem("not", op, 0xf6, 0xf7, 2);
}

void AssemblyBuilderX64::test(OperandX64 lhs, OperandX64 rhs)
{
    // No forms for r/m*, imm8 and reg, r/m*
    placeBinary("test", lhs, rhs, 0xf6, 0xf7, 0xf7, 0x84, 0x85, 0x84, 0x85, 0);
}

void AssemblyBuilderX64::lea(OperandX64 lhs, OperandX64 rhs)
{
    if (logText)
        log("lea", lhs, rhs);

    placeBinaryRegAndRegMem(lhs, rhs, 0x8d, 0x8d);
}

void AssemblyBuilderX64::push(OperandX64 op)
{
    if (logText)
        log("push", op);

    LUAU_ASSERT(op.cat == CategoryX64::reg && op.base.size == SizeX64::qword);
    placeRex(op.base);
    place(OP_PLUS_REG(0x50, op.base.index));
    commit();
}

void AssemblyBuilderX64::pop(OperandX64 op)
{
    if (logText)
        log("pop", op);

    LUAU_ASSERT(op.cat == CategoryX64::reg && op.base.size == SizeX64::qword);
    placeRex(op.base);
    place(OP_PLUS_REG(0x58, op.base.index));
    commit();
}

void AssemblyBuilderX64::ret()
{
    if (logText)
        log("ret");

    place(0xc3);
    commit();
}

void AssemblyBuilderX64::jcc(Condition cond, Label& label)
{
    placeJcc("je", label, codeForCondition[size_t(cond)]);
}

void AssemblyBuilderX64::jmp(Label& label)
{
    place(0xe9);
    placeLabel(label);

    if (logText)
        log("jmp", label);

    commit();
}

void AssemblyBuilderX64::jmp(OperandX64 op)
{
    if (logText)
        log("jmp", op);

    place(0xff);
    placeModRegMem(op, 4);
    commit();
}

void AssemblyBuilderX64::vaddpd(OperandX64 dst, OperandX64 src1, OperandX64 src2)
{
    placeAvx("vaddpd", dst, src1, src2, 0x58, false, AVX_0F, AVX_66);
}

void AssemblyBuilderX64::vaddps(OperandX64 dst, OperandX64 src1, OperandX64 src2)
{
    placeAvx("vaddps", dst, src1, src2, 0x58, false, AVX_0F, AVX_NP);
}

void AssemblyBuilderX64::vaddsd(OperandX64 dst, OperandX64 src1, OperandX64 src2)
{
    placeAvx("vaddsd", dst, src1, src2, 0x58, false, AVX_0F, AVX_F2);
}

void AssemblyBuilderX64::vaddss(OperandX64 dst, OperandX64 src1, OperandX64 src2)
{
    placeAvx("vaddss", dst, src1, src2, 0x58, false, AVX_0F, AVX_F3);
}

void AssemblyBuilderX64::vsqrtpd(OperandX64 dst, OperandX64 src)
{
    placeAvx("vsqrtpd", dst, src, 0x51, false, AVX_0F, AVX_66);
}

void AssemblyBuilderX64::vsqrtps(OperandX64 dst, OperandX64 src)
{
    placeAvx("vsqrtps", dst, src, 0x51, false, AVX_0F, AVX_NP);
}

void AssemblyBuilderX64::vsqrtsd(OperandX64 dst, OperandX64 src1, OperandX64 src2)
{
    placeAvx("vsqrtsd", dst, src1, src2, 0x51, false, AVX_0F, AVX_F2);
}

void AssemblyBuilderX64::vsqrtss(OperandX64 dst, OperandX64 src1, OperandX64 src2)
{
    placeAvx("vsqrtss", dst, src1, src2, 0x51, false, AVX_0F, AVX_F3);
}

void AssemblyBuilderX64::vmovsd(OperandX64 dst, OperandX64 src)
{
    placeAvx("vmovsd", dst, src, 0x10, 0x11, false, AVX_0F, AVX_F2);
}

void AssemblyBuilderX64::vmovsd(OperandX64 dst, OperandX64 src1, OperandX64 src2)
{
    placeAvx("vmovsd", dst, src1, src2, 0x10, false, AVX_0F, AVX_F2);
}

void AssemblyBuilderX64::vmovss(OperandX64 dst, OperandX64 src)
{
    placeAvx("vmovss", dst, src, 0x10, 0x11, false, AVX_0F, AVX_F3);
}

void AssemblyBuilderX64::vmovss(OperandX64 dst, OperandX64 src1, OperandX64 src2)
{
    placeAvx("vmovss", dst, src1, src2, 0x10, false, AVX_0F, AVX_F3);
}

void AssemblyBuilderX64::vmovapd(OperandX64 dst, OperandX64 src)
{
    placeAvx("vmovapd", dst, src, 0x28, 0x29, false, AVX_0F, AVX_66);
}

void AssemblyBuilderX64::vmovaps(OperandX64 dst, OperandX64 src)
{
    placeAvx("vmovaps", dst, src, 0x28, 0x29, false, AVX_0F, AVX_NP);
}

void AssemblyBuilderX64::vmovupd(OperandX64 dst, OperandX64 src)
{
    placeAvx("vmovupd", dst, src, 0x10, 0x11, false, AVX_0F, AVX_66);
}

void AssemblyBuilderX64::vmovups(OperandX64 dst, OperandX64 src)
{
    placeAvx("vmovups", dst, src, 0x10, 0x11, false, AVX_0F, AVX_NP);
}

void AssemblyBuilderX64::finalize()
{
    code.resize(codePos - code.data());

    // Resolve jump targets
    for (Label fixup : pendingLabels)
    {
        uint32_t value = labelLocations[fixup.id - 1] - (fixup.location + 4);
        memcpy(&code[fixup.location], &value, sizeof(value));
    }

    size_t dataSize = data.size() - dataPos;

    // Shrink data
    if (dataSize > 0)
        memmove(&data[0], &data[dataPos], dataSize);

    data.resize(dataSize);

    finalized = true;
}

Label AssemblyBuilderX64::setLabel()
{
    Label label{nextLabel++, getCodeSize()};
    labelLocations.push_back(0);

    if (logText)
        log(label);

    return label;
}

void AssemblyBuilderX64::setLabel(Label& label)
{
    if (label.id == 0)
    {
        label.id = nextLabel++;
        labelLocations.push_back(0);
    }

    label.location = getCodeSize();
    labelLocations[label.id - 1] = label.location;

    if (logText)
        log(label);
}

OperandX64 AssemblyBuilderX64::i64(int64_t value)
{
    size_t pos = allocateData(8, 8);
    memcpy(&data[pos], &value, sizeof(value));
    return OperandX64(SizeX64::qword, noreg, 1, rip, int32_t(pos - data.size()));
}

OperandX64 AssemblyBuilderX64::f32(float value)
{
    size_t pos = allocateData(4, 4);
    memcpy(&data[pos], &value, sizeof(value));
    return OperandX64(SizeX64::dword, noreg, 1, rip, int32_t(pos - data.size()));
}

OperandX64 AssemblyBuilderX64::f64(double value)
{
    size_t pos = allocateData(8, 8);
    memcpy(&data[pos], &value, sizeof(value));
    return OperandX64(SizeX64::qword, noreg, 1, rip, int32_t(pos - data.size()));
}

OperandX64 AssemblyBuilderX64::f32x4(float x, float y, float z, float w)
{
    size_t pos = allocateData(16, 16);
    memcpy(&data[pos], &x, sizeof(x));
    memcpy(&data[pos + 4], &y, sizeof(y));
    memcpy(&data[pos + 8], &z, sizeof(z));
    memcpy(&data[pos + 12], &w, sizeof(w));
    return OperandX64(SizeX64::xmmword, noreg, 1, rip, int32_t(pos - data.size()));
}

void AssemblyBuilderX64::placeBinary(const char* name, OperandX64 lhs, OperandX64 rhs, uint8_t codeimm8, uint8_t codeimm, uint8_t codeimmImm8,
    uint8_t code8rev, uint8_t coderev, uint8_t code8, uint8_t code, uint8_t opreg)
{
    if (logText)
        log(name, lhs, rhs);

    if ((lhs.cat == CategoryX64::reg || lhs.cat == CategoryX64::mem) && rhs.cat == CategoryX64::imm)
        placeBinaryRegMemAndImm(lhs, rhs, codeimm8, codeimm, codeimmImm8, opreg);
    else if (lhs.cat == CategoryX64::reg && (rhs.cat == CategoryX64::reg || rhs.cat == CategoryX64::mem))
        placeBinaryRegAndRegMem(lhs, rhs, code8, code);
    else if (lhs.cat == CategoryX64::mem && rhs.cat == CategoryX64::reg)
        placeBinaryRegMemAndReg(lhs, rhs, code8rev, coderev);
    else
        LUAU_ASSERT(!"No encoding for this operand combination");
}

void AssemblyBuilderX64::placeBinaryRegMemAndImm(OperandX64 lhs, OperandX64 rhs, uint8_t code8, uint8_t code, uint8_t codeImm8, uint8_t opreg)
{
    LUAU_ASSERT(lhs.cat == CategoryX64::reg || lhs.cat == CategoryX64::mem);
    LUAU_ASSERT(rhs.cat == CategoryX64::imm);

    SizeX64 size = lhs.base.size;

    placeRex(lhs.base);

    if (size == SizeX64::byte)
    {
        place(code8);
        placeModRegMem(lhs, opreg);
        placeImm8(rhs.imm);
    }
    else
    {
        LUAU_ASSERT(size == SizeX64::dword || size == SizeX64::qword);

        if (int8_t(rhs.imm) == rhs.imm && code != codeImm8)
        {
            place(codeImm8);
            placeModRegMem(lhs, opreg);
            placeImm8(rhs.imm);
        }
        else
        {
            place(code);
            placeModRegMem(lhs, opreg);
            placeImm32(rhs.imm);
        }
    }

    commit();
}

void AssemblyBuilderX64::placeBinaryRegAndRegMem(OperandX64 lhs, OperandX64 rhs, uint8_t code8, uint8_t code)
{
    LUAU_ASSERT(lhs.cat == CategoryX64::reg && (rhs.cat == CategoryX64::reg || rhs.cat == CategoryX64::mem));
    LUAU_ASSERT(lhs.base.size == (rhs.cat == CategoryX64::reg ? rhs.base.size : rhs.memSize));

    SizeX64 size = lhs.base.size;
    LUAU_ASSERT(size == SizeX64::byte || size == SizeX64::dword || size == SizeX64::qword);

    placeRex(lhs.base, rhs);
    place(size == SizeX64::byte ? code8 : code);
    placeRegAndModRegMem(lhs, rhs);

    commit();
}

void AssemblyBuilderX64::placeBinaryRegMemAndReg(OperandX64 lhs, OperandX64 rhs, uint8_t code8, uint8_t code)
{
    // In two operand instructions, first operand is always a register, but data flow direction is reversed
    placeBinaryRegAndRegMem(rhs, lhs, code8, code);
}

void AssemblyBuilderX64::placeUnaryModRegMem(const char* name, OperandX64 op, uint8_t code8, uint8_t code, uint8_t opreg)
{
    if (logText)
        log(name, op);

    LUAU_ASSERT(op.cat == CategoryX64::reg || op.cat == CategoryX64::mem);

    SizeX64 size = op.cat == CategoryX64::reg ? op.base.size : op.memSize;
    LUAU_ASSERT(size == SizeX64::byte || size == SizeX64::dword || size == SizeX64::qword);

    placeRex(op);
    place(size == SizeX64::byte ? code8 : code);
    placeModRegMem(op, opreg);

    commit();
}

void AssemblyBuilderX64::placeShift(const char* name, OperandX64 lhs, OperandX64 rhs, uint8_t opreg)
{
    if (logText)
        log(name, lhs, rhs);

    LUAU_ASSERT(lhs.cat == CategoryX64::reg || lhs.cat == CategoryX64::mem);
    LUAU_ASSERT(rhs.cat == CategoryX64::imm || (rhs.cat == CategoryX64::reg && rhs.base == cl));

    SizeX64 size = lhs.base.size;

    placeRex(lhs.base);

    if (rhs.cat == CategoryX64::imm && rhs.imm == 1)
    {
        place(size == SizeX64::byte ? 0xd0 : 0xd1);
        placeModRegMem(lhs, opreg);
    }
    else if (rhs.cat == CategoryX64::imm)
    {
        LUAU_ASSERT(int8_t(rhs.imm) == rhs.imm);

        place(size == SizeX64::byte ? 0xc0 : 0xc1);
        placeModRegMem(lhs, opreg);
        placeImm8(rhs.imm);
    }
    else
    {
        place(size == SizeX64::byte ? 0xd2 : 0xd3);
        placeModRegMem(lhs, opreg);
    }

    commit();
}

void AssemblyBuilderX64::placeJcc(const char* name, Label& label, uint8_t cc)
{
    place(0x0f);
    place(OP_PLUS_CC(0x80, cc));
    placeLabel(label);

    if (logText)
        log(name, label);

    commit();
}

void AssemblyBuilderX64::placeAvx(const char* name, OperandX64 dst, OperandX64 src, uint8_t code, bool setW, uint8_t mode, uint8_t prefix)
{
    LUAU_ASSERT(dst.cat == CategoryX64::reg);
    LUAU_ASSERT(src.cat == CategoryX64::reg || src.cat == CategoryX64::mem);

    if (logText)
        log(name, dst, src);

    placeVex(dst, noreg, src, setW, mode, prefix);
    place(code);
    placeRegAndModRegMem(dst, src);

    commit();
}

void AssemblyBuilderX64::placeAvx(
    const char* name, OperandX64 dst, OperandX64 src, uint8_t code, uint8_t coderev, bool setW, uint8_t mode, uint8_t prefix)
{
    LUAU_ASSERT((dst.cat == CategoryX64::mem && src.cat == CategoryX64::reg) || (dst.cat == CategoryX64::reg && src.cat == CategoryX64::mem));

    if (logText)
        log(name, dst, src);

    if (dst.cat == CategoryX64::mem)
    {
        placeVex(src, noreg, dst, setW, mode, prefix);
        place(coderev);
        placeRegAndModRegMem(src, dst);
    }
    else
    {
        placeVex(dst, noreg, src, setW, mode, prefix);
        place(code);
        placeRegAndModRegMem(dst, src);
    }

    commit();
}

void AssemblyBuilderX64::placeAvx(
    const char* name, OperandX64 dst, OperandX64 src1, OperandX64 src2, uint8_t code, bool setW, uint8_t mode, uint8_t prefix)
{
    LUAU_ASSERT(dst.cat == CategoryX64::reg);
    LUAU_ASSERT(src1.cat == CategoryX64::reg);
    LUAU_ASSERT(src2.cat == CategoryX64::reg || src2.cat == CategoryX64::mem);

    if (logText)
        log(name, dst, src1, src2);

    placeVex(dst, src1, src2, setW, mode, prefix);
    place(code);
    placeRegAndModRegMem(dst, src2);

    commit();
}

void AssemblyBuilderX64::placeRex(RegisterX64 op)
{
    uint8_t code = REX_W(op.size == SizeX64::qword) | REX_B(op);

    if (code != 0)
        place(code | 0x40);
}

void AssemblyBuilderX64::placeRex(OperandX64 op)
{
    uint8_t code = 0;

    if (op.cat == CategoryX64::reg)
        code = REX_W(op.base.size == SizeX64::qword) | REX_B(op.base);
    else if (op.cat == CategoryX64::mem)
        code = REX_W(op.memSize == SizeX64::qword) | REX_X(op.index) | REX_B(op.base);
    else
        LUAU_ASSERT(!"No encoding for left operand of this category");

    if (code != 0)
        place(code | 0x40);
}

void AssemblyBuilderX64::placeRex(RegisterX64 lhs, OperandX64 rhs)
{
    uint8_t code = REX_W(lhs.size == SizeX64::qword);

    if (rhs.cat == CategoryX64::imm)
        code |= REX_B(lhs);
    else
        code |= REX_R(lhs) | REX_X(rhs.index) | REX_B(rhs.base);

    if (code != 0)
        place(code | 0x40);
}

void AssemblyBuilderX64::placeVex(OperandX64 dst, OperandX64 src1, OperandX64 src2, bool setW, uint8_t mode, uint8_t prefix)
{
    LUAU_ASSERT(dst.cat == CategoryX64::reg);
    LUAU_ASSERT(src1.cat == CategoryX64::reg);
    LUAU_ASSERT(src2.cat == CategoryX64::reg || src2.cat == CategoryX64::mem);

    place(AVX_3_1());
    place(AVX_3_2(dst.base, src2.index, src2.base, mode));
    place(AVX_3_3(setW, src1.base, dst.base.size == SizeX64::ymmword, prefix));
}

uint8_t getScaleEncoding(uint8_t scale)
{
    static const uint8_t scales[9] = {0xff, 0, 1, 0xff, 2, 0xff, 0xff, 0xff, 3};

    LUAU_ASSERT(scale < 9 && scales[scale] != 0xff);
    return scales[scale];
}

void AssemblyBuilderX64::placeRegAndModRegMem(OperandX64 lhs, OperandX64 rhs)
{
    LUAU_ASSERT(lhs.cat == CategoryX64::reg);

    placeModRegMem(rhs, lhs.base.index);
}

void AssemblyBuilderX64::placeModRegMem(OperandX64 rhs, uint8_t regop)
{
    if (rhs.cat == CategoryX64::reg)
    {
        place(MOD_RM(0b11, regop, rhs.base.index));
    }
    else if (rhs.cat == CategoryX64::mem)
    {
        RegisterX64 index = rhs.index;
        RegisterX64 base = rhs.base;

        uint8_t mod = 0b00;

        if (rhs.imm != 0)
        {
            if (int8_t(rhs.imm) == rhs.imm)
                mod = 0b01;
            else
                mod = 0b10;
        }
        else
        {
            // r13/bp-based addressing requires a displacement
            if ((base.index & 0x7) == 0b101)
                mod = 0b01;
        }

        if (index != noreg && base != noreg)
        {
            place(MOD_RM(mod, regop, 0b100));
            place(SIB(rhs.scale, index.index, base.index));

            if (mod != 0b00)
                placeImm8Or32(rhs.imm);
        }
        else if (index != noreg && rhs.scale != 1)
        {
            place(MOD_RM(0b00, regop, 0b100));
            place(SIB(rhs.scale, index.index, 0b101));
            placeImm32(rhs.imm);
        }
        else if ((base.index & 0x7) == 0b100) // r12/sp-based addressing requires SIB
        {
            LUAU_ASSERT(rhs.scale == 1);
            LUAU_ASSERT(index == noreg);

            place(MOD_RM(mod, regop, 0b100));
            place(SIB(rhs.scale, 0b100, base.index));

            if (rhs.imm != 0)
                placeImm8Or32(rhs.imm);
        }
        else if (base == rip)
        {
            place(MOD_RM(0b00, regop, 0b101));
            placeImm32(-int32_t(getCodeSize() + 4) + rhs.imm);
        }
        else if (base != noreg)
        {
            place(MOD_RM(mod, regop, base.index));

            if (mod != 0b00)
                placeImm8Or32(rhs.imm);
        }
        else
        {
            place(MOD_RM(0b00, regop, 0b100));
            place(SIB(1, 0b100, 0b101));
            placeImm32(rhs.imm);
        }
    }
    else
    {
        LUAU_ASSERT(!"No encoding for right operand of this category");
    }
}

void AssemblyBuilderX64::placeImm8Or32(int32_t imm)
{
    int8_t imm8 = int8_t(imm);

    if (imm8 == imm)
        place(imm8);
    else
        placeImm32(imm);
}

void AssemblyBuilderX64::placeImm8(int32_t imm)
{
    int8_t imm8 = int8_t(imm);

    if (imm8 == imm)
        place(imm8);
    else
        LUAU_ASSERT(!"Invalid immediate value");
}

void AssemblyBuilderX64::placeImm32(int32_t imm)
{
    LUAU_ASSERT(codePos + sizeof(imm) < codeEnd);
    memcpy(codePos, &imm, sizeof(imm));
    codePos += sizeof(imm);
}

void AssemblyBuilderX64::placeImm64(int64_t imm)
{
    LUAU_ASSERT(codePos + sizeof(imm) < codeEnd);
    memcpy(codePos, &imm, sizeof(imm));
    codePos += sizeof(imm);
}

void AssemblyBuilderX64::placeLabel(Label& label)
{
    if (label.location == ~0u)
    {
        if (label.id == 0)
        {
            label.id = nextLabel++;
            labelLocations.push_back(0);
        }

        pendingLabels.push_back({label.id, getCodeSize()});
        placeImm32(0);
    }
    else
    {
        placeImm32(int32_t(label.location - (4 + getCodeSize())));
    }
}

void AssemblyBuilderX64::place(uint8_t byte)
{
    LUAU_ASSERT(codePos < codeEnd);
    *codePos++ = byte;
}

void AssemblyBuilderX64::commit()
{
    LUAU_ASSERT(codePos <= codeEnd);

    if (codeEnd - codePos < 16)
        extend();
}

void AssemblyBuilderX64::extend()
{
    uint32_t count = getCodeSize();

    code.resize(code.size() * 2);
    codePos = code.data() + count;
    codeEnd = code.data() + code.size();
}

uint32_t AssemblyBuilderX64::getCodeSize()
{
    return uint32_t(codePos - code.data());
}

size_t AssemblyBuilderX64::allocateData(size_t size, size_t align)
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

void AssemblyBuilderX64::log(const char* opcode)
{
    logAppend(" %s\n", opcode);
}

void AssemblyBuilderX64::log(const char* opcode, OperandX64 op)
{
    logAppend(" %-12s", opcode);
    log(op);
    text.append("\n");
}

void AssemblyBuilderX64::log(const char* opcode, OperandX64 op1, OperandX64 op2)
{
    logAppend(" %-12s", opcode);
    log(op1);
    text.append(",");
    log(op2);
    text.append("\n");
}

void AssemblyBuilderX64::log(const char* opcode, OperandX64 op1, OperandX64 op2, OperandX64 op3)
{
    logAppend(" %-12s", opcode);
    log(op1);
    text.append(",");
    log(op2);
    text.append(",");
    log(op3);
    text.append("\n");
}

void AssemblyBuilderX64::log(Label label)
{
    logAppend(".L%d:\n", label.id);
}

void AssemblyBuilderX64::log(const char* opcode, Label label)
{
    logAppend(" %-12s.L%d\n", opcode, label.id);
}

void AssemblyBuilderX64::log(OperandX64 op)
{
    switch (op.cat)
    {
    case CategoryX64::reg:
        logAppend("%s", getRegisterName(op.base));
        break;
    case CategoryX64::mem:
        if (op.base == rip)
        {
            logAppend("%s ptr [.start%+d]", getSizeName(op.memSize), op.imm);
            return;
        }

        logAppend("%s ptr [", getSizeName(op.memSize));

        if (op.base != noreg)
            logAppend("%s", getRegisterName(op.base));

        if (op.index != noreg)
            logAppend("%s%s", op.base != noreg ? "+" : "", getRegisterName(op.index));

        if (op.scale != 1)
            logAppend("*%d", op.scale);

        if (op.imm != 0)
        {
            if (op.imm >= 0 && op.imm <= 9)
                logAppend("+%d", op.imm);
            else
                logAppend("+0%Xh", op.imm);
        }

        text.append("]");
        break;
    case CategoryX64::imm:
        if (op.imm >= 0 && op.imm <= 9)
            logAppend("%d", op.imm);
        else
            logAppend("%Xh", op.imm);
        break;
    default:
        LUAU_ASSERT(!"Unknown operand category");
    }
}

void AssemblyBuilderX64::logAppend(const char* fmt, ...)
{
    char buf[256];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    text.append(buf);
}

const char* AssemblyBuilderX64::getSizeName(SizeX64 size)
{
    static const char* sizeNames[] = {"none", "byte", "word", "dword", "qword", "xmmword", "ymmword"};

    LUAU_ASSERT(unsigned(size) < sizeof(sizeNames) / sizeof(sizeNames[0]));
    return sizeNames[unsigned(size)];
}

const char* AssemblyBuilderX64::getRegisterName(RegisterX64 reg)
{
    static const char* names[][16] = {{"rip", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""},
        {"al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b"},
        {"ax", "cx", "dx", "bx", "sp", "bp", "si", "di", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w"},
        {"eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"},
        {"rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"},
        {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"},
        {"ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7", "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15"}};

    LUAU_ASSERT(reg.index < 16);
    LUAU_ASSERT(reg.size <= SizeX64::ymmword);
    return names[size_t(reg.size)][reg.index];
}

} // namespace CodeGen
} // namespace Luau
