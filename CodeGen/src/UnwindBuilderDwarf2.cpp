// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/UnwindBuilderDwarf2.h"

#include "ByteUtils.h"

#include <string.h>

// General information about Dwarf2 format can be found at:
// https://dwarfstd.org/doc/dwarf-2.0.0.pdf [DWARF Debugging Information Format]
// Main part for async exception unwinding is in section '6.4 Call Frame Information'

// Information about System V ABI (AMD64) can be found at:
// https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf [System V Application Binary Interface (AMD64 Architecture Processor Supplement)]
// Interaction between Dwarf2 and System V ABI can be found in sections '3.6.2 DWARF Register Number Mapping' and '4.2.4 EH_FRAME sections'

// Call frame instruction opcodes (Dwarf2, page 78, ch. 7.23 figure 37)
#define DW_CFA_advance_loc 0x40
#define DW_CFA_offset 0x80
#define DW_CFA_restore 0xc0
#define DW_CFA_set_loc 0x01
#define DW_CFA_advance_loc1 0x02
#define DW_CFA_advance_loc2 0x03
#define DW_CFA_advance_loc4 0x04
#define DW_CFA_offset_extended 0x05
#define DW_CFA_restore_extended 0x06
#define DW_CFA_undefined 0x07
#define DW_CFA_same_value 0x08
#define DW_CFA_register 0x09
#define DW_CFA_remember_state 0x0a
#define DW_CFA_restore_state 0x0b
#define DW_CFA_def_cfa 0x0c
#define DW_CFA_def_cfa_register 0x0d
#define DW_CFA_def_cfa_offset 0x0e
#define DW_CFA_def_cfa_expression 0x0f
#define DW_CFA_nop 0x00
#define DW_CFA_lo_user 0x1c
#define DW_CFA_hi_user 0x3f

// Register numbers for x64 (System V ABI, page 57, ch. 3.7, figure 3.36)
#define DW_REG_RAX 0
#define DW_REG_RDX 1
#define DW_REG_RCX 2
#define DW_REG_RBX 3
#define DW_REG_RSI 4
#define DW_REG_RDI 5
#define DW_REG_RBP 6
#define DW_REG_RSP 7
#define DW_REG_R8 8
#define DW_REG_R9 9
#define DW_REG_R10 10
#define DW_REG_R11 11
#define DW_REG_R12 12
#define DW_REG_R13 13
#define DW_REG_R14 14
#define DW_REG_R15 15
#define DW_REG_RA 16

const int regIndexToDwRegX64[16] = {DW_REG_RAX, DW_REG_RCX, DW_REG_RDX, DW_REG_RBX, DW_REG_RSP, DW_REG_RBP, DW_REG_RSI, DW_REG_RDI, DW_REG_R8,
    DW_REG_R9, DW_REG_R10, DW_REG_R11, DW_REG_R12, DW_REG_R13, DW_REG_R14, DW_REG_R15};

const int kCodeAlignFactor = 1;
const int kDataAlignFactor = 8;
const int kDwarfAlign = 8;
const int kFdeInitialLocationOffset = 8;
const int kFdeAddressRangeOffset = 16;

// Define canonical frame address expression as [reg + offset]
static uint8_t* defineCfaExpression(uint8_t* pos, int dwReg, uint32_t stackOffset)
{
    pos = writeu8(pos, DW_CFA_def_cfa);
    pos = writeuleb128(pos, dwReg);
    pos = writeuleb128(pos, stackOffset);
    return pos;
}

// Update offset value in canonical frame address expression
static uint8_t* defineCfaExpressionOffset(uint8_t* pos, uint32_t stackOffset)
{
    pos = writeu8(pos, DW_CFA_def_cfa_offset);
    pos = writeuleb128(pos, stackOffset);
    return pos;
}

static uint8_t* defineSavedRegisterLocation(uint8_t* pos, int dwReg, uint32_t stackOffset)
{
    LUAU_ASSERT(stackOffset % kDataAlignFactor == 0 && "stack offsets have to be measured in kDataAlignFactor units");

    if (dwReg <= 15)
    {
        pos = writeu8(pos, DW_CFA_offset + dwReg);
    }
    else
    {
        pos = writeu8(pos, DW_CFA_offset_extended);
        pos = writeuleb128(pos, dwReg);
    }

    pos = writeuleb128(pos, stackOffset / kDataAlignFactor);
    return pos;
}

static uint8_t* advanceLocation(uint8_t* pos, uint8_t offset)
{
    pos = writeu8(pos, DW_CFA_advance_loc1);
    pos = writeu8(pos, offset);
    return pos;
}

static uint8_t* alignPosition(uint8_t* start, uint8_t* pos)
{
    size_t size = pos - start;
    size_t pad = ((size + kDwarfAlign - 1) & ~(kDwarfAlign - 1)) - size;

    for (size_t i = 0; i < pad; i++)
        pos = writeu8(pos, DW_CFA_nop);

    return pos;
}

namespace Luau
{
namespace CodeGen
{

void UnwindBuilderDwarf2::setBeginOffset(size_t beginOffset)
{
    this->beginOffset = beginOffset;
}

size_t UnwindBuilderDwarf2::getBeginOffset() const
{
    return beginOffset;
}

void UnwindBuilderDwarf2::start()
{
    uint8_t* cieLength = pos;
    pos = writeu32(pos, 0); // Length (to be filled later)

    pos = writeu32(pos, 0); // CIE id. 0 -- .eh_frame
    pos = writeu8(pos, 1);  // Version

    pos = writeu8(pos, 0); // CIE augmentation String ""

    pos = writeuleb128(pos, kCodeAlignFactor);         // Code align factor
    pos = writeuleb128(pos, -kDataAlignFactor & 0x7f); // Data align factor of (as signed LEB128)
    pos = writeu8(pos, DW_REG_RA);                     // Return address register

    // Optional CIE augmentation section (not present)

    // Call frame instructions (common for all FDEs, of which we have 1)
    stackOffset = 8; // Return address was pushed by calling the function

    pos = defineCfaExpression(pos, DW_REG_RSP, stackOffset); // Define CFA to be the rsp + 8
    pos = defineSavedRegisterLocation(pos, DW_REG_RA, 8);    // Define return address register (RA) to be located at CFA - 8

    pos = alignPosition(cieLength, pos);
    writeu32(cieLength, unsigned(pos - cieLength - 4)); // Length field itself is excluded from length

    fdeEntryStart = pos;                          // Will be written at the end
    pos = writeu32(pos, 0);                       // Length (to be filled later)
    pos = writeu32(pos, unsigned(pos - rawData)); // CIE pointer
    pos = writeu64(pos, 0);                       // Initial location (to be filled later)
    pos = writeu64(pos, 0);                       // Address range (to be filled later)

    // Optional CIE augmentation section (not present)

    // Function call frame instructions to follow
}

void UnwindBuilderDwarf2::spill(int espOffset, RegisterX64 reg)
{
    pos = advanceLocation(pos, 5); // REX.W mov [rsp + imm8], reg
}

void UnwindBuilderDwarf2::save(RegisterX64 reg)
{
    stackOffset += 8;
    pos = advanceLocation(pos, 2); // REX.W push reg
    pos = defineCfaExpressionOffset(pos, stackOffset);
    pos = defineSavedRegisterLocation(pos, regIndexToDwRegX64[reg.index], stackOffset);
}

void UnwindBuilderDwarf2::allocStack(int size)
{
    stackOffset += size;
    pos = advanceLocation(pos, 4); // REX.W sub rsp, imm8
    pos = defineCfaExpressionOffset(pos, stackOffset);
}

void UnwindBuilderDwarf2::setupFrameReg(RegisterX64 reg, int espOffset)
{
    if (espOffset != 0)
        pos = advanceLocation(pos, 5); // REX.W lea rbp, [rsp + imm8]
    else
        pos = advanceLocation(pos, 3); // REX.W mov rbp, rsp

    // Cfa is based on rsp, so no additonal commands are required
}

void UnwindBuilderDwarf2::finish()
{
    LUAU_ASSERT(stackOffset % 16 == 0 && "stack has to be aligned to 16 bytes after prologue");

    pos = alignPosition(fdeEntryStart, pos);
    writeu32(fdeEntryStart, unsigned(pos - fdeEntryStart - 4)); // Length field itself is excluded from length

    // Terminate section
    pos = writeu32(pos, 0);

    LUAU_ASSERT(getSize() <= kRawDataLimit);
}

size_t UnwindBuilderDwarf2::getSize() const
{
    return size_t(pos - rawData);
}

void UnwindBuilderDwarf2::finalize(char* target, void* funcAddress, size_t funcSize) const
{
    memcpy(target, rawData, getSize());

    unsigned fdeEntryStartPos = unsigned(fdeEntryStart - rawData);
    writeu64((uint8_t*)target + fdeEntryStartPos + kFdeInitialLocationOffset, uintptr_t(funcAddress));
    writeu64((uint8_t*)target + fdeEntryStartPos + kFdeAddressRangeOffset, funcSize);
}

} // namespace CodeGen
} // namespace Luau
