// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/IrData.h"
#include "Luau/RegisterA64.h"

#include <initializer_list>
#include <utility>

namespace Luau
{
namespace CodeGen
{
namespace A64
{

struct IrRegAllocA64
{
    IrRegAllocA64(IrFunction& function, std::initializer_list<std::pair<RegisterA64, RegisterA64>> regs);

    RegisterA64 allocReg(KindA64 kind);
    RegisterA64 allocTemp(KindA64 kind);
    RegisterA64 allocReuse(KindA64 kind, uint32_t index, std::initializer_list<IrOp> oprefs);

    void freeReg(RegisterA64 reg);

    void freeLastUseReg(IrInst& target, uint32_t index);
    void freeLastUseRegs(const IrInst& inst, uint32_t index);

    void freeTempRegs();

    void assertAllFree() const;

    IrFunction& function;

    struct Set
    {
        // which registers are in the set that the allocator manages (initialized at construction)
        uint32_t base = 0;

        // which subset of initial set is free
        uint32_t free = 0;

        // which subset of initial set is allocated as temporary
        uint32_t temp = 0;
    };

    Set gpr, simd;

    Set& getSet(KindA64 kind);
};

} // namespace A64
} // namespace CodeGen
} // namespace Luau
