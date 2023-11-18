// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrRegAllocA64.h"

#include "Luau/AssemblyBuilderA64.h"
#include "Luau/CodeGen.h"
#include "Luau/IrUtils.h"

#include "BitUtils.h"
#include "EmitCommonA64.h"

#include <string.h>

LUAU_FASTFLAGVARIABLE(DebugCodegenChaosA64, false)

namespace Luau
{
namespace CodeGen
{
namespace A64
{

static const int8_t kInvalidSpill = 64;

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

static int getReloadOffset(IrCmd cmd)
{
    switch (getCmdValueKind(cmd))
    {
    case IrValueKind::Unknown:
    case IrValueKind::None:
        LUAU_ASSERT(!"Invalid operand restore value kind");
        break;
    case IrValueKind::Tag:
        return offsetof(TValue, tt);
    case IrValueKind::Int:
        return offsetof(TValue, value);
    case IrValueKind::Pointer:
        return offsetof(TValue, value.gc);
    case IrValueKind::Double:
        return offsetof(TValue, value.n);
    case IrValueKind::Tvalue:
        return 0;
    }

    LUAU_ASSERT(!"Invalid operand restore value kind");
    LUAU_UNREACHABLE();
}

static AddressA64 getReloadAddress(const IrFunction& function, const IrInst& inst, bool limitToCurrentBlock)
{
    IrOp location = function.findRestoreOp(inst, limitToCurrentBlock);

    if (location.kind == IrOpKind::VmReg)
        return mem(rBase, vmRegOp(location) * sizeof(TValue) + getReloadOffset(inst.cmd));

    // loads are 4/8/16 bytes; we conservatively limit the offset to fit assuming a 4b index
    if (location.kind == IrOpKind::VmConst && vmConstOp(location) * sizeof(TValue) <= AddressA64::kMaxOffset * 4)
        return mem(rConstants, vmConstOp(location) * sizeof(TValue) + getReloadOffset(inst.cmd));

    return AddressA64(xzr); // dummy
}

static void restoreInst(AssemblyBuilderA64& build, uint32_t& freeSpillSlots, IrFunction& function, const IrRegAllocA64::Spill& s, RegisterA64 reg)
{
    IrInst& inst = function.instructions[s.inst];
    LUAU_ASSERT(inst.regA64 == noreg);

    if (s.slot >= 0)
    {
        build.ldr(reg, mem(sp, sSpillArea.data + s.slot * 8));

        if (s.slot != kInvalidSpill)
            freeSpill(freeSpillSlots, reg.kind, s.slot);
    }
    else
    {
        LUAU_ASSERT(!inst.spilled && inst.needsReload);
        AddressA64 addr = getReloadAddress(function, function.instructions[s.inst], /*limitToCurrentBlock*/ false);
        LUAU_ASSERT(addr.base != xzr);
        build.ldr(reg, addr);
    }

    inst.spilled = false;
    inst.needsReload = false;
    inst.regA64 = reg;
}

IrRegAllocA64::IrRegAllocA64(IrFunction& function, LoweringStats* stats, std::initializer_list<std::pair<RegisterA64, RegisterA64>> regs)
    : function(function)
    , stats(stats)
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
        error = true;
        return RegisterA64{kind, 0};
    }

    int reg = 31 - countlz(set.free);

    if (FFlag::DebugCodegenChaosA64)
        reg = countrz(set.free); // allocate from low end; this causes extra conflicts for calls

    set.free &= ~(1u << reg);
    set.defs[reg] = index;

    return RegisterA64{kind, uint8_t(reg)};
}

RegisterA64 IrRegAllocA64::allocTemp(KindA64 kind)
{
    Set& set = getSet(kind);

    if (set.free == 0)
    {
        error = true;
        return RegisterA64{kind, 0};
    }

    int reg = 31 - countlz(set.free);

    if (FFlag::DebugCodegenChaosA64)
        reg = countrz(set.free); // allocate from low end; this causes extra conflicts for calls

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

        if (source.lastUse == index && !source.reusedReg && source.regA64 != noreg)
        {
            LUAU_ASSERT(!source.spilled && !source.needsReload);
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
        LUAU_ASSERT(!target.spilled && !target.needsReload);

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

    uint32_t poisongpr = 0;
    uint32_t poisonsimd = 0;

    if (FFlag::DebugCodegenChaosA64)
    {
        poisongpr = gpr.base & ~gpr.free;
        poisonsimd = simd.base & ~simd.free;

        for (RegisterA64 reg : live)
        {
            Set& set = getSet(reg.kind);
            (&set == &simd ? poisonsimd : poisongpr) &= ~(1u << reg.index);
        }
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
            LUAU_ASSERT(!def.reusedReg);
            LUAU_ASSERT(!def.spilled);
            LUAU_ASSERT(!def.needsReload);

            if (def.lastUse == index)
            {
                // instead of spilling the register to never reload it, we assume the register is not needed anymore
            }
            else if (getReloadAddress(function, def, /*limitToCurrentBlock*/ true).base != xzr)
            {
                // instead of spilling the register to stack, we can reload it from VM stack/constants
                // we still need to record the spill for restore(start) to work
                Spill s = {inst, def.regA64, -1};
                spills.push_back(s);

                def.needsReload = true;

                if (stats)
                    stats->spillsToRestore++;
            }
            else
            {
                int slot = allocSpill(freeSpillSlots, def.regA64.kind);
                if (slot < 0)
                {
                    slot = kInvalidSpill;
                    error = true;
                }

                build.str(def.regA64, mem(sp, sSpillArea.data + slot * 8));

                Spill s = {inst, def.regA64, int8_t(slot)};
                spills.push_back(s);

                def.spilled = true;

                if (stats)
                {
                    stats->spillsToSlot++;

                    if (slot != kInvalidSpill && unsigned(slot + 1) > stats->maxSpillSlotsUsed)
                        stats->maxSpillSlotsUsed = slot + 1;
                }
            }

            def.regA64 = noreg;

            regs &= ~(1u << reg);
            set.free |= 1u << reg;
            set.defs[reg] = kInvalidInstIdx;
        }

        LUAU_ASSERT(set.free == set.base);
    }

    if (FFlag::DebugCodegenChaosA64)
    {
        for (int reg = 0; reg < 32; ++reg)
        {
            if (poisongpr & (1u << reg))
                build.mov(RegisterA64{KindA64::x, uint8_t(reg)}, 0xdead);
            if (poisonsimd & (1u << reg))
                build.fmov(RegisterA64{KindA64::d, uint8_t(reg)}, -0.125);
        }
    }

    return start;
}

void IrRegAllocA64::restore(AssemblyBuilderA64& build, size_t start)
{
    LUAU_ASSERT(start <= spills.size());

    if (start < spills.size())
    {
        for (size_t i = start; i < spills.size(); ++i)
        {
            Spill s = spills[i]; // copy in case takeReg reallocates spills
            RegisterA64 reg = takeReg(s.origin, s.inst);

            restoreInst(build, freeSpillSlots, function, s, reg);
        }

        spills.resize(start);
    }
}

void IrRegAllocA64::restoreReg(AssemblyBuilderA64& build, IrInst& inst)
{
    uint32_t index = function.getInstIndex(inst);

    for (size_t i = 0; i < spills.size(); ++i)
    {
        if (spills[i].inst == index)
        {
            Spill s = spills[i]; // copy in case allocReg reallocates spills
            RegisterA64 reg = allocReg(s.origin.kind, index);

            restoreInst(build, freeSpillSlots, function, s, reg);

            spills[i] = spills.back();
            spills.pop_back();
            return;
        }
    }

    LUAU_ASSERT(!"Expected to find a spill record");
}

IrRegAllocA64::Set& IrRegAllocA64::getSet(KindA64 kind)
{
    switch (kind)
    {
    case KindA64::x:
    case KindA64::w:
        return gpr;

    case KindA64::s:
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
