// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/Condition.h"
#include "Luau/Label.h"
#include "Luau/OperandX64.h"
#include "Luau/RegisterX64.h"

#include <string>
#include <vector>

namespace Luau
{
namespace CodeGen
{

class AssemblyBuilderX64
{
public:
    explicit AssemblyBuilderX64(bool logText);
    ~AssemblyBuilderX64();

    // Base two operand instructions with 9 opcode selection
    void add(OperandX64 lhs, OperandX64 rhs);
    void sub(OperandX64 lhs, OperandX64 rhs);
    void cmp(OperandX64 lhs, OperandX64 rhs);
    void and_(OperandX64 lhs, OperandX64 rhs);
    void or_(OperandX64 lhs, OperandX64 rhs);
    void xor_(OperandX64 lhs, OperandX64 rhs);

    // Binary shift instructions with special rhs handling
    void sal(OperandX64 lhs, OperandX64 rhs);
    void sar(OperandX64 lhs, OperandX64 rhs);
    void shl(OperandX64 lhs, OperandX64 rhs);
    void shr(OperandX64 lhs, OperandX64 rhs);

    // Two operand mov instruction has additional specialized encodings
    void mov(OperandX64 lhs, OperandX64 rhs);
    void mov64(RegisterX64 lhs, int64_t imm);

    // Base one operand instruction with 2 opcode selection
    void div(OperandX64 op);
    void idiv(OperandX64 op);
    void mul(OperandX64 op);
    void neg(OperandX64 op);
    void not_(OperandX64 op);

    void test(OperandX64 lhs, OperandX64 rhs);
    void lea(OperandX64 lhs, OperandX64 rhs);

    void push(OperandX64 op);
    void pop(OperandX64 op);
    void ret();

    // Control flow
    void jcc(Condition cond, Label& label);
    void jmp(Label& label);
    void jmp(OperandX64 op);

    // AVX
    void vaddpd(OperandX64 dst, OperandX64 src1, OperandX64 src2);
    void vaddps(OperandX64 dst, OperandX64 src1, OperandX64 src2);
    void vaddsd(OperandX64 dst, OperandX64 src1, OperandX64 src2);
    void vaddss(OperandX64 dst, OperandX64 src1, OperandX64 src2);

    void vsqrtpd(OperandX64 dst, OperandX64 src);
    void vsqrtps(OperandX64 dst, OperandX64 src);
    void vsqrtsd(OperandX64 dst, OperandX64 src1, OperandX64 src2);
    void vsqrtss(OperandX64 dst, OperandX64 src1, OperandX64 src2);

    void vmovsd(OperandX64 dst, OperandX64 src);
    void vmovsd(OperandX64 dst, OperandX64 src1, OperandX64 src2);
    void vmovss(OperandX64 dst, OperandX64 src);
    void vmovss(OperandX64 dst, OperandX64 src1, OperandX64 src2);
    void vmovapd(OperandX64 dst, OperandX64 src);
    void vmovaps(OperandX64 dst, OperandX64 src);
    void vmovupd(OperandX64 dst, OperandX64 src);
    void vmovups(OperandX64 dst, OperandX64 src);

    // Run final checks
    void finalize();

    // Places a label at current location and returns it
    Label setLabel();

    // Assigns label position to the current location
    void setLabel(Label& label);

    // Constant allocation (uses rip-relative addressing)
    OperandX64 i64(int64_t value);
    OperandX64 f32(float value);
    OperandX64 f64(double value);
    OperandX64 f32x4(float x, float y, float z, float w);

    // Resulting data and code that need to be copied over one after the other
    // The *end* of 'data' has to be aligned to 16 bytes, this will also align 'code'
    std::vector<uint8_t> data;
    std::vector<uint8_t> code;

    std::string text;

private:
    // Instruction archetypes
    void placeBinary(const char* name, OperandX64 lhs, OperandX64 rhs, uint8_t codeimm8, uint8_t codeimm, uint8_t codeimmImm8, uint8_t code8rev,
        uint8_t coderev, uint8_t code8, uint8_t code, uint8_t opreg);
    void placeBinaryRegMemAndImm(OperandX64 lhs, OperandX64 rhs, uint8_t code8, uint8_t code, uint8_t codeImm8, uint8_t opreg);
    void placeBinaryRegAndRegMem(OperandX64 lhs, OperandX64 rhs, uint8_t code8, uint8_t code);
    void placeBinaryRegMemAndReg(OperandX64 lhs, OperandX64 rhs, uint8_t code8, uint8_t code);

    void placeUnaryModRegMem(const char* name, OperandX64 op, uint8_t code8, uint8_t code, uint8_t opreg);

    void placeShift(const char* name, OperandX64 lhs, OperandX64 rhs, uint8_t opreg);

    void placeJcc(const char* name, Label& label, uint8_t cc);

    void placeAvx(const char* name, OperandX64 dst, OperandX64 src, uint8_t code, bool setW, uint8_t mode, uint8_t prefix);
    void placeAvx(const char* name, OperandX64 dst, OperandX64 src, uint8_t code, uint8_t coderev, bool setW, uint8_t mode, uint8_t prefix);
    void placeAvx(const char* name, OperandX64 dst, OperandX64 src1, OperandX64 src2, uint8_t code, bool setW, uint8_t mode, uint8_t prefix);

    // Instruction components
    void placeRegAndModRegMem(OperandX64 lhs, OperandX64 rhs);
    void placeModRegMem(OperandX64 rhs, uint8_t regop);
    void placeRex(RegisterX64 op);
    void placeRex(OperandX64 op);
    void placeRex(RegisterX64 lhs, OperandX64 rhs);
    void placeVex(OperandX64 dst, OperandX64 src1, OperandX64 src2, bool setW, uint8_t mode, uint8_t prefix);
    void placeImm8Or32(int32_t imm);
    void placeImm8(int32_t imm);
    void placeImm32(int32_t imm);
    void placeImm64(int64_t imm);
    void placeLabel(Label& label);
    void place(uint8_t byte);

    void commit();
    LUAU_NOINLINE void extend();
    uint32_t getCodeSize();

    // Data
    size_t allocateData(size_t size, size_t align);

    // Logging of assembly in text form (Intel asm with VS disassembly formatting)
    LUAU_NOINLINE void log(const char* opcode);
    LUAU_NOINLINE void log(const char* opcode, OperandX64 op);
    LUAU_NOINLINE void log(const char* opcode, OperandX64 op1, OperandX64 op2);
    LUAU_NOINLINE void log(const char* opcode, OperandX64 op1, OperandX64 op2, OperandX64 op3);
    LUAU_NOINLINE void log(Label label);
    LUAU_NOINLINE void log(const char* opcode, Label label);
    void log(OperandX64 op);
    void logAppend(const char* fmt, ...);

    const char* getSizeName(SizeX64 size);
    const char* getRegisterName(RegisterX64 reg);

    uint32_t nextLabel = 1;
    std::vector<Label> pendingLabels;
    std::vector<uint32_t> labelLocations;

    bool logText = false;
    bool finalized = false;

    size_t dataPos = 0;

    uint8_t* codePos = nullptr;
    uint8_t* codeEnd = nullptr;
};

} // namespace CodeGen
} // namespace Luau
