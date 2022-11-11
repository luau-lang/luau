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

class AssemblyBuilderA64
{
public:
    explicit AssemblyBuilderA64(bool logText);
    ~AssemblyBuilderA64();

    // Moves
    void mov(RegisterA64 dst, RegisterA64 src);
    void mov(RegisterA64 dst, uint16_t src, int shift = 0);
    void movk(RegisterA64 dst, uint16_t src, int shift = 0);

    // Arithmetics
    void add(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, int shift = 0);
    void add(RegisterA64 dst, RegisterA64 src1, int src2);
    void sub(RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, int shift = 0);
    void sub(RegisterA64 dst, RegisterA64 src1, int src2);
    void neg(RegisterA64 dst, RegisterA64 src);

    // Comparisons
    // Note: some arithmetic instructions also have versions that update flags (ADDS etc) but we aren't using them atm
    void cmp(RegisterA64 src1, RegisterA64 src2);
    void cmp(RegisterA64 src1, int src2);

    // Bitwise
    // Note: shifted-register support and bitfield operations are omitted for simplicity
    // TODO: support immediate arguments (they have odd encoding and forbid many values)
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

    // Store
    void str(RegisterA64 src, AddressA64 dst);
    void strb(RegisterA64 src, AddressA64 dst);
    void strh(RegisterA64 src, AddressA64 dst);

    // Control flow
    // Note: tbz/tbnz are currently not supported because they have 15-bit offsets and we don't support branch thunks
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

    // Run final checks
    bool finalize();

    // Places a label at current location and returns it
    Label setLabel();

    // Assigns label position to the current location
    void setLabel(Label& label);

    void logAppend(const char* fmt, ...) LUAU_PRINTF_ATTR(2, 3);

    uint32_t getCodeSize() const;

    // Resulting data and code that need to be copied over one after the other
    // The *end* of 'data' has to be aligned to 16 bytes, this will also align 'code'
    std::vector<uint8_t> data;
    std::vector<uint32_t> code;

    std::string text;

    const bool logText = false;

private:
    // Instruction archetypes
    void place0(const char* name, uint32_t word);
    void placeSR3(const char* name, RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, uint8_t op, int shift = 0);
    void placeSR2(const char* name, RegisterA64 dst, RegisterA64 src, uint8_t op, uint8_t op2 = 0);
    void placeR3(const char* name, RegisterA64 dst, RegisterA64 src1, RegisterA64 src2, uint8_t op, uint8_t op2);
    void placeR1(const char* name, RegisterA64 dst, RegisterA64 src, uint32_t op);
    void placeI12(const char* name, RegisterA64 dst, RegisterA64 src1, int src2, uint8_t op);
    void placeI16(const char* name, RegisterA64 dst, int src, uint8_t op, int shift = 0);
    void placeA(const char* name, RegisterA64 dst, AddressA64 src, uint8_t op, uint8_t size);
    void placeBC(const char* name, Label& label, uint8_t op, uint8_t cond);
    void placeBCR(const char* name, Label& label, uint8_t op, RegisterA64 cond);
    void placeBR(const char* name, RegisterA64 src, uint32_t op);
    void placeADR(const char* name, RegisterA64 src, uint8_t op);

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
    LUAU_NOINLINE void log(const char* opcode, RegisterA64 src, Label label);
    LUAU_NOINLINE void log(const char* opcode, RegisterA64 src);
    LUAU_NOINLINE void log(const char* opcode, Label label);
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

} // namespace CodeGen
} // namespace Luau
