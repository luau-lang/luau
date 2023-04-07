// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrRegAllocX64.h"

#include "EmitCommonX64.h"

namespace Luau
{
namespace CodeGen
{
namespace X64
{

static const RegisterX64 kGprAllocOrder[] = {rax, rdx, rcx, rbx, rsi, rdi, r8, r9, r10, r11};

static bool isFullTvalueOperand(IrCmd cmd)
{
    return cmd == IrCmd::LOAD_TVALUE || cmd == IrCmd::LOAD_NODE_VALUE_TV;
}

IrRegAllocX64::IrRegAllocX64(AssemblyBuilderX64& build, IrFunction& function)
    : build(build)
    , function(function)
{
    freeGprMap.fill(true);
    gprInstUsers.fill(kInvalidInstIdx);
    freeXmmMap.fill(true);
    xmmInstUsers.fill(kInvalidInstIdx);
}

RegisterX64 IrRegAllocX64::allocGprReg(SizeX64 preferredSize, uint32_t instIdx)
{
    LUAU_ASSERT(
        preferredSize == SizeX64::byte || preferredSize == SizeX64::word || preferredSize == SizeX64::dword || preferredSize == SizeX64::qword);

    for (RegisterX64 reg : kGprAllocOrder)
    {
        if (freeGprMap[reg.index])
        {
            freeGprMap[reg.index] = false;
            gprInstUsers[reg.index] = instIdx;
            return RegisterX64{preferredSize, reg.index};
        }
    }

    // If possible, spill the value with the furthest next use
    if (uint32_t furthestUseTarget = findInstructionWithFurthestNextUse(gprInstUsers); furthestUseTarget != kInvalidInstIdx)
        return takeReg(function.instructions[furthestUseTarget].regX64, instIdx);

    LUAU_ASSERT(!"Out of GPR registers to allocate");
    return noreg;
}

RegisterX64 IrRegAllocX64::allocXmmReg(uint32_t instIdx)
{
    for (size_t i = 0; i < freeXmmMap.size(); ++i)
    {
        if (freeXmmMap[i])
        {
            freeXmmMap[i] = false;
            xmmInstUsers[i] = instIdx;
            return RegisterX64{SizeX64::xmmword, uint8_t(i)};
        }
    }

    // Out of registers, spill the value with the furthest next use
    if (uint32_t furthestUseTarget = findInstructionWithFurthestNextUse(xmmInstUsers); furthestUseTarget != kInvalidInstIdx)
        return takeReg(function.instructions[furthestUseTarget].regX64, instIdx);

    LUAU_ASSERT(!"Out of XMM registers to allocate");
    return noreg;
}

RegisterX64 IrRegAllocX64::allocGprRegOrReuse(SizeX64 preferredSize, uint32_t instIdx, std::initializer_list<IrOp> oprefs)
{
    for (IrOp op : oprefs)
    {
        if (op.kind != IrOpKind::Inst)
            continue;

        IrInst& source = function.instructions[op.index];

        if (source.lastUse == instIdx && !source.reusedReg && !source.spilled)
        {
            LUAU_ASSERT(source.regX64.size != SizeX64::xmmword);
            LUAU_ASSERT(source.regX64 != noreg);

            source.reusedReg = true;
            gprInstUsers[source.regX64.index] = instIdx;
            return RegisterX64{preferredSize, source.regX64.index};
        }
    }

    return allocGprReg(preferredSize, instIdx);
}

RegisterX64 IrRegAllocX64::allocXmmRegOrReuse(uint32_t instIdx, std::initializer_list<IrOp> oprefs)
{
    for (IrOp op : oprefs)
    {
        if (op.kind != IrOpKind::Inst)
            continue;

        IrInst& source = function.instructions[op.index];

        if (source.lastUse == instIdx && !source.reusedReg && !source.spilled)
        {
            LUAU_ASSERT(source.regX64.size == SizeX64::xmmword);
            LUAU_ASSERT(source.regX64 != noreg);

            source.reusedReg = true;
            xmmInstUsers[source.regX64.index] = instIdx;
            return source.regX64;
        }
    }

    return allocXmmReg(instIdx);
}

RegisterX64 IrRegAllocX64::takeReg(RegisterX64 reg, uint32_t instIdx)
{
    if (reg.size == SizeX64::xmmword)
    {
        if (!freeXmmMap[reg.index])
        {
            LUAU_ASSERT(xmmInstUsers[reg.index] != kInvalidInstIdx);
            preserve(function.instructions[xmmInstUsers[reg.index]]);
        }

        LUAU_ASSERT(freeXmmMap[reg.index]);
        freeXmmMap[reg.index] = false;
        xmmInstUsers[reg.index] = instIdx;
    }
    else
    {
        if (!freeGprMap[reg.index])
        {
            LUAU_ASSERT(gprInstUsers[reg.index] != kInvalidInstIdx);
            preserve(function.instructions[gprInstUsers[reg.index]]);
        }

        LUAU_ASSERT(freeGprMap[reg.index]);
        freeGprMap[reg.index] = false;
        gprInstUsers[reg.index] = instIdx;
    }

    return reg;
}

void IrRegAllocX64::freeReg(RegisterX64 reg)
{
    if (reg.size == SizeX64::xmmword)
    {
        LUAU_ASSERT(!freeXmmMap[reg.index]);
        freeXmmMap[reg.index] = true;
        xmmInstUsers[reg.index] = kInvalidInstIdx;
    }
    else
    {
        LUAU_ASSERT(!freeGprMap[reg.index]);
        freeGprMap[reg.index] = true;
        gprInstUsers[reg.index] = kInvalidInstIdx;
    }
}

void IrRegAllocX64::freeLastUseReg(IrInst& target, uint32_t instIdx)
{
    if (isLastUseReg(target, instIdx))
    {
        // Register might have already been freed if it had multiple uses inside a single instruction
        if (target.regX64 == noreg)
            return;

        freeReg(target.regX64);
        target.regX64 = noreg;
    }
}

void IrRegAllocX64::freeLastUseRegs(const IrInst& inst, uint32_t instIdx)
{
    auto checkOp = [this, instIdx](IrOp op) {
        if (op.kind == IrOpKind::Inst)
            freeLastUseReg(function.instructions[op.index], instIdx);
    };

    checkOp(inst.a);
    checkOp(inst.b);
    checkOp(inst.c);
    checkOp(inst.d);
    checkOp(inst.e);
    checkOp(inst.f);
}

bool IrRegAllocX64::isLastUseReg(const IrInst& target, uint32_t instIdx) const
{
    return target.lastUse == instIdx && !target.reusedReg;
}

void IrRegAllocX64::preserve(IrInst& inst)
{
    bool doubleSlot = isFullTvalueOperand(inst.cmd);

    // Find a free stack slot. Two consecutive slots might be required for 16 byte TValues, so '- 1' is used
    for (unsigned i = 0; i < unsigned(usedSpillSlots.size() - 1); ++i)
    {
        if (usedSpillSlots.test(i))
            continue;

        if (doubleSlot && usedSpillSlots.test(i + 1))
        {
            ++i; // No need to retest this double position
            continue;
        }

        if (inst.regX64.size == SizeX64::xmmword && doubleSlot)
        {
            build.vmovups(xmmword[sSpillArea + i * 8], inst.regX64);
        }
        else if (inst.regX64.size == SizeX64::xmmword)
        {
            build.vmovsd(qword[sSpillArea + i * 8], inst.regX64);
        }
        else
        {
            OperandX64 location = addr[sSpillArea + i * 8];
            location.memSize = inst.regX64.size; // Override memory access size
            build.mov(location, inst.regX64);
        }

        usedSpillSlots.set(i);

        if (i + 1 > maxUsedSlot)
            maxUsedSlot = i + 1;

        if (doubleSlot)
        {
            usedSpillSlots.set(i + 1);

            if (i + 2 > maxUsedSlot)
                maxUsedSlot = i + 2;
        }

        IrSpillX64 spill;
        spill.instIdx = function.getInstIndex(inst);
        spill.useDoubleSlot = doubleSlot;
        spill.stackSlot = uint8_t(i);
        spill.originalLoc = inst.regX64;

        spills.push_back(spill);

        freeReg(inst.regX64);

        inst.regX64 = noreg;
        inst.spilled = true;
        return;
    }

    LUAU_ASSERT(!"nowhere to spill");
}

void IrRegAllocX64::restore(IrInst& inst, bool intoOriginalLocation)
{
    uint32_t instIdx = function.getInstIndex(inst);

    for (size_t i = 0; i < spills.size(); i++)
    {
        const IrSpillX64& spill = spills[i];

        if (spill.instIdx == instIdx)
        {
            LUAU_ASSERT(spill.stackSlot != kNoStackSlot);
            RegisterX64 reg;

            if (spill.originalLoc.size == SizeX64::xmmword)
            {
                reg = intoOriginalLocation ? takeReg(spill.originalLoc, instIdx) : allocXmmReg(instIdx);

                if (spill.useDoubleSlot)
                    build.vmovups(reg, xmmword[sSpillArea + spill.stackSlot * 8]);
                else
                    build.vmovsd(reg, qword[sSpillArea + spill.stackSlot * 8]);
            }
            else
            {
                reg = intoOriginalLocation ? takeReg(spill.originalLoc, instIdx) : allocGprReg(spill.originalLoc.size, instIdx);

                OperandX64 location = addr[sSpillArea + spill.stackSlot * 8];
                location.memSize = reg.size; // Override memory access size
                build.mov(reg, location);
            }

            inst.regX64 = reg;
            inst.spilled = false;

            usedSpillSlots.set(spill.stackSlot, false);

            if (spill.useDoubleSlot)
                usedSpillSlots.set(spill.stackSlot + 1, false);

            spills[i] = spills.back();
            spills.pop_back();
            return;
        }
    }
}

void IrRegAllocX64::preserveAndFreeInstValues()
{
    for (uint32_t instIdx : gprInstUsers)
    {
        if (instIdx != kInvalidInstIdx)
            preserve(function.instructions[instIdx]);
    }

    for (uint32_t instIdx : xmmInstUsers)
    {
        if (instIdx != kInvalidInstIdx)
            preserve(function.instructions[instIdx]);
    }
}

bool IrRegAllocX64::shouldFreeGpr(RegisterX64 reg) const
{
    if (reg == noreg)
        return false;

    LUAU_ASSERT(reg.size != SizeX64::xmmword);

    for (RegisterX64 gpr : kGprAllocOrder)
    {
        if (reg.index == gpr.index)
            return true;
    }

    return false;
}

uint32_t IrRegAllocX64::findInstructionWithFurthestNextUse(const std::array<uint32_t, 16>& regInstUsers) const
{
    uint32_t furthestUseTarget = kInvalidInstIdx;
    uint32_t furthestUseLocation = 0;

    for (uint32_t regInstUser : regInstUsers)
    {
        // Cannot spill temporary registers or the register of the value that's defined in the current instruction
        if (regInstUser == kInvalidInstIdx || regInstUser == currInstIdx)
            continue;

        uint32_t nextUse = getNextInstUse(function, regInstUser, currInstIdx);

        // Cannot spill value that is about to be used in the current instruction
        if (nextUse == currInstIdx)
            continue;

        if (furthestUseTarget == kInvalidInstIdx || nextUse > furthestUseLocation)
        {
            furthestUseLocation = nextUse;
            furthestUseTarget = regInstUser;
        }
    }

    return furthestUseTarget;
}

void IrRegAllocX64::assertFree(RegisterX64 reg) const
{
    if (reg.size == SizeX64::xmmword)
        LUAU_ASSERT(freeXmmMap[reg.index]);
    else
        LUAU_ASSERT(freeGprMap[reg.index]);
}

void IrRegAllocX64::assertAllFree() const
{
    for (RegisterX64 reg : kGprAllocOrder)
        LUAU_ASSERT(freeGprMap[reg.index]);

    for (bool free : freeXmmMap)
        LUAU_ASSERT(free);
}

void IrRegAllocX64::assertNoSpills() const
{
    LUAU_ASSERT(spills.empty());
}

ScopedRegX64::ScopedRegX64(IrRegAllocX64& owner)
    : owner(owner)
    , reg(noreg)
{
}

ScopedRegX64::ScopedRegX64(IrRegAllocX64& owner, SizeX64 size)
    : owner(owner)
    , reg(noreg)
{
    alloc(size);
}

ScopedRegX64::ScopedRegX64(IrRegAllocX64& owner, RegisterX64 reg)
    : owner(owner)
    , reg(reg)
{
}

ScopedRegX64::~ScopedRegX64()
{
    if (reg != noreg)
        owner.freeReg(reg);
}

void ScopedRegX64::alloc(SizeX64 size)
{
    LUAU_ASSERT(reg == noreg);

    if (size == SizeX64::xmmword)
        reg = owner.allocXmmReg(kInvalidInstIdx);
    else
        reg = owner.allocGprReg(size, kInvalidInstIdx);
}

void ScopedRegX64::free()
{
    LUAU_ASSERT(reg != noreg);
    owner.freeReg(reg);
    reg = noreg;
}

RegisterX64 ScopedRegX64::release()
{
    RegisterX64 tmp = reg;
    reg = noreg;
    return tmp;
}

ScopedSpills::ScopedSpills(IrRegAllocX64& owner)
    : owner(owner)
{
    snapshot = owner.spills;
}

ScopedSpills::~ScopedSpills()
{
    // Taking a copy of current spills because we are going to potentially restore them
    std::vector<IrSpillX64> current = owner.spills;

    // Restore registers that were spilled inside scope protected by this object
    for (IrSpillX64& curr : current)
    {
        // If spill existed before current scope, it can be restored outside of it
        if (!wasSpilledBefore(curr))
        {
            IrInst& inst = owner.function.instructions[curr.instIdx];

            owner.restore(inst, /*intoOriginalLocation*/ true);
        }
    }
}

bool ScopedSpills::wasSpilledBefore(const IrSpillX64& spill) const
{
    for (const IrSpillX64& preexisting : snapshot)
    {
        if (spill.instIdx == preexisting.instIdx)
            return true;
    }

    return false;
}

} // namespace X64
} // namespace CodeGen
} // namespace Luau
