// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrRegAllocA64.h"

#include "Luau/AssemblyBuilderA64.h"

#include "BitUtils.h"
#include "EmitCommonA64.h"

namespace Luau
{
namespace CodeGen
{
namespace A64
{

static int allocSpill(uint32_t& free, KindA64 kind)
{
    LUAU_ASSERT(kStackSize <= 256); // to support larger stack frames, we need to ensure qN is allocated at 16b boundary to fit in ldr/str encoding

    // qN registers use two consecutive slots
    int slot = countrz(kind == KindA64::q ? free & (free >> 1) : free);
    if (slot == 32)
        return -1;

    uint32_t mask = (kind == KindA64::q ? 3u : 1u) << slot;

    LUAU_ASSERT((free & mask) == mask);
    free &= ~mask;

    return slot;
}

static void freeSpill(uint32_t& free, KindA64 kind, uint8_t slot)
{
    // qN registers use two consecutive slots
    uint32_t mask = (kind == KindA64::q ? 3u : 1u) << slot;

    LUAU_ASSERT((free & mask) == 0);
    free |= mask;
}

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

    memset(gpr.defs, -1, sizeof(gpr.defs));
    memset(simd.defs, -1, sizeof(simd.defs));

    LUAU_ASSERT(kSpillSlots <= 32);
    freeSpillSlots = (kSpillSlots == 32) ? ~0u : (1u << kSpillSlots) - 1;
}

RegisterA64 IrRegAllocA64::allocReg(KindA64 kind, uint32_t index)
{
    Set& set = getSet(kind);

    if (set.free == 0)
    {
        LUAU_ASSERT(!"Out of registers to allocate");
        return noreg;
    }

    int reg = 31 - countlz(set.free);
    set.free &= ~(1u << reg);
    set.defs[reg] = index;

    return RegisterA64{kind, uint8_t(reg)};
}

RegisterA64 IrRegAllocA64::allocTemp(KindA64 kind)
{
    Set& set = getSet(kind);

    if (set.free == 0)
    {
        LUAU_ASSERT(!"Out of registers to allocate");
        return noreg;
    }

    int reg = 31 - countlz(set.free);

    set.free &= ~(1u << reg);
    set.temp |= 1u << reg;
    LUAU_ASSERT(set.defs[reg] == kInvalidInstIdx);

    return RegisterA64{kind, uint8_t(reg)};
}

RegisterA64 IrRegAllocA64::allocReuse(KindA64 kind, uint32_t index, std::initializer_list<IrOp> oprefs)
{
    for (IrOp op : oprefs)
    {
        if (op.kind != IrOpKind::Inst)
            continue;

        IrInst& source = function.instructions[op.index];

        if (source.lastUse == index && !source.reusedReg && !source.spilled && source.regA64 != noreg)
        {
            LUAU_ASSERT(source.regA64.kind == kind);

            Set& set = getSet(kind);
            LUAU_ASSERT(set.defs[source.regA64.index] == op.index);
            set.defs[source.regA64.index] = index;

            source.reusedReg = true;
            return source.regA64;
        }
    }

    return allocReg(kind, index);
}

RegisterA64 IrRegAllocA64::takeReg(RegisterA64 reg, uint32_t index)
{
    Set& set = getSet(reg.kind);

    LUAU_ASSERT(set.free & (1u << reg.index));
    LUAU_ASSERT(set.defs[reg.index] == kInvalidInstIdx);

    set.free &= ~(1u << reg.index);
    set.defs[reg.index] = index;

    return reg;
}

void IrRegAllocA64::freeReg(RegisterA64 reg)
{
    Set& set = getSet(reg.kind);

    LUAU_ASSERT((set.base & (1u << reg.index)) != 0);
    LUAU_ASSERT((set.free & (1u << reg.index)) == 0);
    LUAU_ASSERT((set.temp & (1u << reg.index)) == 0);

    set.free |= 1u << reg.index;
    set.defs[reg.index] = kInvalidInstIdx;
}

void IrRegAllocA64::freeLastUseReg(IrInst& target, uint32_t index)
{
    if (target.lastUse == index && !target.reusedReg)
    {
        LUAU_ASSERT(!target.spilled);

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

size_t IrRegAllocA64::spill(AssemblyBuilderA64& build, uint32_t index, std::initializer_list<RegisterA64> live)
{
    static const KindA64 sets[] = {KindA64::x, KindA64::q};

    size_t start = spills.size();

    for (RegisterA64 reg : live)
    {
        Set& set = getSet(reg.kind);

        // make sure registers that we expect to survive past spill barrier are not allocated
        // TODO: we need to handle this condition somehow in the future; if this fails, this likely means the caller has an aliasing hazard
        LUAU_ASSERT(set.free & (1u << reg.index));
    }

    for (KindA64 kind : sets)
    {
        Set& set = getSet(kind);

        // early-out
        if (set.free == set.base)
            continue;

        // free all temp registers
        LUAU_ASSERT((set.free & set.temp) == 0);
        set.free |= set.temp;
        set.temp = 0;

        // spill all allocated registers unless they aren't used anymore
        uint32_t regs = set.base & ~set.free;

        while (regs)
        {
            int reg = 31 - countlz(regs);

            uint32_t inst = set.defs[reg];
            LUAU_ASSERT(inst != kInvalidInstIdx);

            IrInst& def = function.instructions[inst];
            LUAU_ASSERT(def.regA64.index == reg);
            LUAU_ASSERT(!def.spilled);
            LUAU_ASSERT(!def.reusedReg);

            if (def.lastUse == index)
            {
                // instead of spilling the register to never reload it, we assume the register is not needed anymore
                def.regA64 = noreg;
            }
            else
            {
                int slot = allocSpill(freeSpillSlots, def.regA64.kind);
                LUAU_ASSERT(slot >= 0); // TODO: remember the error and fail lowering

                Spill s = {inst, def.regA64, uint8_t(slot)};
                spills.push_back(s);

                def.spilled = true;
                def.regA64 = noreg;
            }

            regs &= ~(1u << reg);
            set.free |= 1u << reg;
            set.defs[reg] = kInvalidInstIdx;
        }

        LUAU_ASSERT(set.free == set.base);
    }

    if (start < spills.size())
    {
        // TODO: use stp for consecutive slots
        for (size_t i = start; i < spills.size(); ++i)
            build.str(spills[i].origin, mem(sp, sSpillArea.data + spills[i].slot * 8));
    }

    return start;
}

void IrRegAllocA64::restore(AssemblyBuilderA64& build, size_t start)
{
    LUAU_ASSERT(start <= spills.size());

    if (start < spills.size())
    {
        // TODO: use ldp for consecutive slots
        for (size_t i = start; i < spills.size(); ++i)
            build.ldr(spills[i].origin, mem(sp, sSpillArea.data + spills[i].slot * 8));

        for (size_t i = start; i < spills.size(); ++i)
        {
            Spill s = spills[i]; // copy in case takeReg reallocates spills

            IrInst& def = function.instructions[s.inst];
            LUAU_ASSERT(def.spilled);
            LUAU_ASSERT(def.regA64 == noreg);

            def.spilled = false;
            def.regA64 = takeReg(s.origin, s.inst);

            freeSpill(freeSpillSlots, s.origin.kind, s.slot);
        }

        spills.resize(start);
    }
}

void IrRegAllocA64::restoreReg(AssemblyBuilderA64& build, IrInst& inst)
{
    uint32_t index = function.getInstIndex(inst);

    LUAU_ASSERT(inst.spilled);
    LUAU_ASSERT(inst.regA64 == noreg);

    for (size_t i = 0; i < spills.size(); ++i)
    {
        if (spills[i].inst == index)
        {
            Spill s = spills[i]; // copy in case allocReg reallocates spills
            RegisterA64 reg = allocReg(s.origin.kind, index);

            build.ldr(reg, mem(sp, sSpillArea.data + s.slot * 8));

            inst.spilled = false;
            inst.regA64 = reg;

            freeSpill(freeSpillSlots, reg.kind, s.slot);

            spills[i] = spills.back();
            spills.pop_back();
            return;
        }
    }

    LUAU_ASSERT(!"Expected to find a spill record");
}

void IrRegAllocA64::assertNoSpills() const
{
    LUAU_ASSERT(spills.empty());
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
