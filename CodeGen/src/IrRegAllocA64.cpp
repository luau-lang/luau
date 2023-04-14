// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrRegAllocA64.h"

#include "BitUtils.h"

namespace Luau
{
namespace CodeGen
{
namespace A64
{

IrRegAllocA64::IrRegAllocA64(IrFunction& function, std::initializer_list<std::pair<RegisterA64, RegisterA64>> regs)
    : function(function)
{
    for (auto& p : regs)
    {
        LUAU_ASSERT(p.first.kind == p.second.kind && p.first.index <= p.second.index);

        Set& set = getSet(p.first.kind);

        for (int i = p.first.index; i <= p.second.index; ++i)
            set.base |= 1u << i;
    }

    gpr.free = gpr.base;
    simd.free = simd.base;
}

RegisterA64 IrRegAllocA64::allocReg(KindA64 kind)
{
    Set& set = getSet(kind);

    if (set.free == 0)
    {
        LUAU_ASSERT(!"Out of registers to allocate");
        return noreg;
    }

    int index = 31 - countlz(set.free);
    set.free &= ~(1u << index);

    return RegisterA64{kind, uint8_t(index)};
}

RegisterA64 IrRegAllocA64::allocTemp(KindA64 kind)
{
    Set& set = getSet(kind);

    if (set.free == 0)
    {
        LUAU_ASSERT(!"Out of registers to allocate");
        return noreg;
    }

    int index = 31 - countlz(set.free);

    set.free &= ~(1u << index);
    set.temp |= 1u << index;

    return RegisterA64{kind, uint8_t(index)};
}

RegisterA64 IrRegAllocA64::allocReuse(KindA64 kind, uint32_t index, std::initializer_list<IrOp> oprefs)
{
    for (IrOp op : oprefs)
    {
        if (op.kind != IrOpKind::Inst)
            continue;

        IrInst& source = function.instructions[op.index];

        if (source.lastUse == index && !source.reusedReg)
        {
            LUAU_ASSERT(source.regA64.kind == kind);

            source.reusedReg = true;
            return source.regA64;
        }
    }

    return allocReg(kind);
}

void IrRegAllocA64::freeReg(RegisterA64 reg)
{
    Set& set = getSet(reg.kind);

    LUAU_ASSERT((set.base & (1u << reg.index)) != 0);
    LUAU_ASSERT((set.free & (1u << reg.index)) == 0);
    set.free |= 1u << reg.index;
}

void IrRegAllocA64::freeLastUseReg(IrInst& target, uint32_t index)
{
    if (target.lastUse == index && !target.reusedReg)
    {
        // Register might have already been freed if it had multiple uses inside a single instruction
        if (target.regA64 == noreg)
            return;

        freeReg(target.regA64);
        target.regA64 = noreg;
    }
}

void IrRegAllocA64::freeLastUseRegs(const IrInst& inst, uint32_t index)
{
    auto checkOp = [this, index](IrOp op) {
        if (op.kind == IrOpKind::Inst)
            freeLastUseReg(function.instructions[op.index], index);
    };

    checkOp(inst.a);
    checkOp(inst.b);
    checkOp(inst.c);
    checkOp(inst.d);
    checkOp(inst.e);
    checkOp(inst.f);
}

void IrRegAllocA64::freeTempRegs()
{
    LUAU_ASSERT((gpr.free & gpr.temp) == 0);
    gpr.free |= gpr.temp;
    gpr.temp = 0;

    LUAU_ASSERT((simd.free & simd.temp) == 0);
    simd.free |= simd.temp;
    simd.temp = 0;
}

void IrRegAllocA64::assertAllFree() const
{
    LUAU_ASSERT(gpr.free == gpr.base);
    LUAU_ASSERT(simd.free == simd.base);
}

void IrRegAllocA64::assertAllFreeExcept(RegisterA64 reg) const
{
    const Set& set = const_cast<IrRegAllocA64*>(this)->getSet(reg.kind);
    const Set& other = &set == &gpr ? simd : gpr;

    LUAU_ASSERT(set.free == (set.base & ~(1u << reg.index)));
    LUAU_ASSERT(other.free == other.base);
}

IrRegAllocA64::Set& IrRegAllocA64::getSet(KindA64 kind)
{
    switch (kind)
    {
    case KindA64::x:
    case KindA64::w:
        return gpr;

    case KindA64::d:
    case KindA64::q:
        return simd;

    default:
        LUAU_ASSERT(!"Unexpected register kind");
        LUAU_UNREACHABLE();
    }
}

} // namespace A64
} // namespace CodeGen
} // namespace Luau
