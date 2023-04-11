// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/RegisterA64.h"
#include "Luau/AddressA64.h"
#include "Luau/ConditionA64.h"
#include "Luau/Label.h"

#include <string>
#include <vector>

namespace Luau
{
namespace CodeGen
{
namespace A64
{

enum FeaturesA64
{
    Feature_JSCVT = 1 << 0,
};

class AssemblyBuilderA64
{
public:
    explicit AssemblyBuilderA64(bool logText, unsigned int features = 0);
    ~AssemblyBuilderA64();

    // Moves
    void mov(RegisterA64 dst, RegisterA64 src);
    void mov(RegisterA64 dst, int src); // macro

    // Moves of 32-bit immediates get decomposed into one or more of these
    void movz(RegisterA64 dst, uint16_t src, int shift = 0);
    void movn(RegisterA64 dst, uint16_t src, int shift = 0);
    void movk(RegisterA64 dst, uint16_t src, int shift = 0);

    // Arithmetics
    // TODO: support various kinds of shifts
    void add(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, int shift = 0);
    void add(RegisterA64 dst, RegisterA64 src1, uint16_t src2);
    void sub(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, int shift = 0);
    void sub(RegisterA64 dst, RegisterA64 src1, uint16_t src2);
    void neg(RegisterA64 dst, RegisterA64 src);

    // Comparisons
    // Note: some arithmetic instructions also have versions that update flags (ADDS etc) but we aren't using them atm
    void cmp(RegisterA64 src1, RegisterA64 src2);
    void cmp(RegisterA64 src1, uint16_t src2);
    void csel(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, ConditionA64 cond);

    // Bitwise
    // TODO: support immediate arguments (they have odd encoding and forbid many values)
    // TODO: support bic (andnot)
    // TODO: support shifts
    // TODO: support bitfield ops
    void and_(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2);
    void orr(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2);
    void eor(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2);
    void mvn(RegisterA64 dst, RegisterA64 src);

    // Shifts
    void lsl(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2);
    void lsr(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2);
    void asr(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2);
    void ror(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2);
    void clz(RegisterA64 dst, RegisterA64 src);
    void rbit(RegisterA64 dst, RegisterA64 src);

    // Load
    // Note: paired loads are currently omitted for simplicity
    void ldr(RegisterA64 dst, AddressA64 src);
    void ldrb(RegisterA64 dst, AddressA64 src);
    void ldrh(RegisterA64 dst, AddressA64 src);
    void ldrsb(RegisterA64 dst, AddressA64 src);
    void ldrsh(RegisterA64 dst, AddressA64 src);
    void ldrsw(RegisterA64 dst, AddressA64 src);
    void ldp(RegisterA64 dst1, RegisterA64 dst2, AddressA64 src);

    // Store
    void str(RegisterA64 src, AddressA64 dst);
    void strb(RegisterA64 src, AddressA64 dst);
    void strh(RegisterA64 src, AddressA64 dst);
    void stp(RegisterA64 src1, RegisterA64 src2, AddressA64 dst);

    // Control flow
    // TODO: support tbz/tbnz; they have 15-bit offsets but they can be useful in constrained cases
    void b(Label& label);
    void b(ConditionA64 cond, Label& label);
    void cbz(RegisterA64 src, Label& label);
    void cbnz(RegisterA64 src, Label& label);
    void br(RegisterA64 src);
    void blr(RegisterA64 src);
    void ret();

    // Address of embedded data
    void adr(RegisterA64 dst, const void* ptr, size_t size);
    void adr(RegisterA64 dst, uint64_t value);
    void adr(RegisterA64 dst, double value);

    // Address of code (label)
    void adr(RegisterA64 dst, Label& label);

    // Floating-point scalar moves
    void fmov(RegisterA64 dst, RegisterA64 src);

    // Floating-point scalar math
    void fabs(RegisterA64 dst, RegisterA64 src);
    void fadd(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2);
    void fdiv(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2);
    void fmul(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2);
    void fneg(RegisterA64 dst, RegisterA64 src);
    void fsqrt(RegisterA64 dst, RegisterA64 src);
    void fsub(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2);

    // Floating-point rounding and conversions
    void frinta(RegisterA64 dst, RegisterA64 src);
    void frintm(RegisterA64 dst, RegisterA64 src);
    void frintp(RegisterA64 dst, RegisterA64 src);
    void fcvtzs(RegisterA64 dst, RegisterA64 src);
    void fcvtzu(RegisterA64 dst, RegisterA64 src);
    void scvtf(RegisterA64 dst, RegisterA64 src);
    void ucvtf(RegisterA64 dst, RegisterA64 src);

    // Floating-point conversion to integer using JS rules (wrap around 2^32) and set Z flag
    // note: this is part of ARM8.3 (JSCVT feature); support of this instruction needs to be checked at runtime
    void fjcvtzs(RegisterA64 dst, RegisterA64 src);

    // Floating-point comparisons
    void fcmp(RegisterA64 src1, RegisterA64 src2);
    void fcmpz(RegisterA64 src);
    void fcsel(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, ConditionA64 cond);

    // Run final checks
    bool finalize();

    // Places a label at current location and returns it
    Label setLabel();

    // Assigns label position to the current location
    void setLabel(Label& label);

    // Extracts code offset (in bytes) from label
    uint32_t getLabelOffset(const Label& label)
    {
        LUAU_ASSERT(label.location != ~0u);
        return label.location * 4;
    }

    void logAppend(const char* fmt, ...) LUAU_PRINTF_ATTR(2, 3);

    uint32_t getCodeSize() const;

    // Resulting data and code that need to be copied over one after the other
    // The *end* of 'data' has to be aligned to 16 bytes, this will also align 'code'
    std::vector<uint8_t> data;
    std::vector<uint32_t> code;

    std::string text;

    const bool logText = false;
    const unsigned int features = 0;

    // Maximum immediate argument to functions like add/sub/cmp
    static constexpr size_t kMaxImmediate = (1 << 12) - 1;

private:
    // Instruction archetypes
    void place0(const char* name, uint32_t word);
    void placeSR3(const char* name, RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, uint8_t op, int shift = 0);
    void placeSR2(const char* name, RegisterA64 dst, RegisterA64 src, uint8_t op, uint8_t op2 = 0);
    void placeR3(const char* name, RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, uint8_t op, uint8_t op2);
    void placeR1(const char* name, RegisterA64 dst, RegisterA64 src, uint32_t op);
    void placeI12(const char* name, RegisterA64 dst, RegisterA64 src1, int src2, uint8_t op);
    void placeI16(const char* name, RegisterA64 dst, int src, uint8_t op, int shift = 0);
    void placeA(const char* name, RegisterA64 dst, AddressA64 src, uint8_t op, uint8_t size, int sizelog);
    void placeBC(const char* name, Label& label, uint8_t op, uint8_t cond);
    void placeBCR(const char* name, Label& label, uint8_t op, RegisterA64 cond);
    void placeBR(const char* name, RegisterA64 src, uint32_t op);
    void placeADR(const char* name, RegisterA64 src, uint8_t op);
    void placeADR(const char* name, RegisterA64 src, uint8_t op, Label& label);
    void placeP(const char* name, RegisterA64 dst1, RegisterA64 dst2, AddressA64 src, uint8_t op, uint8_t opc, int sizelog);
    void placeCS(const char* name, RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, ConditionA64 cond, uint8_t op, uint8_t opc);
    void placeFCMP(const char* name, RegisterA64 src1, RegisterA64 src2, uint8_t op, uint8_t opc);

    void place(uint32_t word);

    void patchLabel(Label& label);
    void patchImm19(uint32_t location, int value);

    void commit();
    LUAU_NOINLINE void extend();

    // Data
    size_t allocateData(size_t size, size_t align);

    // Logging of assembly in text form
    LUAU_NOINLINE void log(const char* opcode);
    LUAU_NOINLINE void log(const char* opcode, RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, int shift = 0);
    LUAU_NOINLINE void log(const char* opcode, RegisterA64 dst, RegisterA64 src1, int src2);
    LUAU_NOINLINE void log(const char* opcode, RegisterA64 dst, RegisterA64 src);
    LUAU_NOINLINE void log(const char* opcode, RegisterA64 dst, int src, int shift = 0);
    LUAU_NOINLINE void log(const char* opcode, RegisterA64 dst, AddressA64 src);
    LUAU_NOINLINE void log(const char* opcode, RegisterA64 dst1, RegisterA64 dst2, AddressA64 src);
    LUAU_NOINLINE void log(const char* opcode, RegisterA64 src, Label label);
    LUAU_NOINLINE void log(const char* opcode, RegisterA64 src);
    LUAU_NOINLINE void log(const char* opcode, Label label);
    LUAU_NOINLINE void log(const char* opcode, RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, ConditionA64 cond);
    LUAU_NOINLINE void log(Label label);
    LUAU_NOINLINE void log(RegisterA64 reg);
    LUAU_NOINLINE void log(AddressA64 addr);

    uint32_t nextLabel = 1;
    std::vector<Label> pendingLabels;
    std::vector<uint32_t> labelLocations;

    bool finalized = false;
    bool overflowed = false;

    size_t dataPos = 0;

    uint32_t* codePos = nullptr;
    uint32_t* codeEnd = nullptr;
};

} // namespace A64
} // namespace CodeGen
} // namespace Luau
