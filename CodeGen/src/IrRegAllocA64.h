// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/IrData.h"
#include "Luau/RegisterA64.h"

#include <initializer_list>
#include <utility>
#include <vector>

namespace Luau
{
namespace CodeGen
{

struct LoweringStats;

namespace A64
{

class AssemblyBuilderA64;

struct IrRegAllocA64
{
    IrRegAllocA64(IrFunction& function, LoweringStats* stats, std::initializer_list<std::pair<RegisterA64, RegisterA64>> regs);

    RegisterA64 allocReg(KindA64 kind, uint32_t index);
    RegisterA64 allocTemp(KindA64 kind);
    RegisterA64 allocReuse(KindA64 kind, uint32_t index, std::initializer_list<IrOp> oprefs);

    RegisterA64 takeReg(RegisterA64 reg, uint32_t index);

    void freeReg(RegisterA64 reg);

    void freeLastUseReg(IrInst& target, uint32_t index);
    void freeLastUseRegs(const IrInst& inst, uint32_t index);

    void freeTempRegs();

    // Spills all live registers that outlive current instruction; all allocated registers are assumed to be undefined
    size_t spill(AssemblyBuilderA64& build, uint32_t index, std::initializer_list<RegisterA64> live = {});

    // Restores registers starting from the offset returned by spill(); all spills will be restored to the original registers
    void restore(AssemblyBuilderA64& build, size_t start);

    // Restores register for a single instruction; may not assign the previously used register!
    void restoreReg(AssemblyBuilderA64& build, IrInst& inst);

    struct Set
    {
        // which registers are in the set that the allocator manages (initialized at construction)
        uint32_t base = 0;

        // which subset of initial set is free
        uint32_t free = 0;

        // which subset of initial set is allocated as temporary
        uint32_t temp = 0;

        // which instruction is defining which register (for spilling); only valid if not free and not temp
        uint32_t defs[32];
    };

    struct Spill
    {
        uint32_t inst;

        RegisterA64 origin;
        int8_t slot;
    };

    Set& getSet(KindA64 kind);

    IrFunction& function;
    LoweringStats* stats = nullptr;
    Set gpr, simd;

    std::vector<Spill> spills;

    // which 8-byte slots are free
    uint32_t freeSpillSlots = 0;

    bool error = false;
};

} // namespace A64
} // namespace CodeGen
} // namespace Luau
