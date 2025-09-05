// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrRegAllocX64.h"

#include "Luau/IrUtils.h"
#include "Luau/LoweringStats.h"

#include "EmitCommonX64.h"

LUAU_FASTFLAG(LuauCodeGenRegAutoSpillA64)

namespace Luau
{
namespace CodeGen
{
namespace X64
{

static const RegisterX64 kGprAllocOrder[] = {rax, rdx, rcx, rbx, rsi, rdi, r8, r9, r10, r11};

IrRegAllocX64::IrRegAllocX64(AssemblyBuilderX64& build, IrFunction& function, LoweringStats* stats)
    : build(build)
    , function(function)
    , stats(stats)
    , usableXmmRegCount(getXmmRegisterCount(build.abi))
{
    freeGprMap.fill(true);
    gprInstUsers.fill(kInvalidInstIdx);
    freeXmmMap.fill(true);
    xmmInstUsers.fill(kInvalidInstIdx);
}

RegisterX64 IrRegAllocX64::allocReg(SizeX64 size, uint32_t instIdx)
{
    if (size == SizeX64::xmmword)
    {
        for (size_t i = 0; i < usableXmmRegCount; ++i)
        {
            if (freeXmmMap[i])
            {
                freeXmmMap[i] = false;
                xmmInstUsers[i] = instIdx;
                return RegisterX64{size, uint8_t(i)};
            }
        }
    }
    else
    {
        for (RegisterX64 reg : kGprAllocOrder)
        {
            if (freeGprMap[reg.index])
            {
                freeGprMap[reg.index] = false;
                gprInstUsers[reg.index] = instIdx;
                return RegisterX64{size, reg.index};
            }
        }
    }

    // Out of registers, spill the value with the furthest next use
    const std::array<uint32_t, 16>& regInstUsers = size == SizeX64::xmmword ? xmmInstUsers : gprInstUsers;
    if (uint32_t furthestUseTarget = findInstructionWithFurthestNextUse(regInstUsers); furthestUseTarget != kInvalidInstIdx)
    {
        RegisterX64 reg = function.instructions[furthestUseTarget].regX64;
        reg.size = size; // Adjust size to the requested

        return takeReg(reg, instIdx);
    }

    CODEGEN_ASSERT(!"Out of registers to allocate");
    return noreg;
}

RegisterX64 IrRegAllocX64::allocRegOrReuse(SizeX64 size, uint32_t instIdx, std::initializer_list<IrOp> oprefs)
{
    for (IrOp op : oprefs)
    {
        if (op.kind != IrOpKind::Inst)
            continue;

        IrInst& source = function.instructions[op.index];

        if (source.lastUse == instIdx && !source.reusedReg && !source.spilled && !source.needsReload)
        {
            // Not comparing size directly because we only need matching register set
            if ((size == SizeX64::xmmword) != (source.regX64.size == SizeX64::xmmword))
                continue;

            CODEGEN_ASSERT(source.regX64 != noreg);

            source.reusedReg = true;

            if (size == SizeX64::xmmword)
                xmmInstUsers[source.regX64.index] = instIdx;
            else
                gprInstUsers[source.regX64.index] = instIdx;

            return RegisterX64{size, source.regX64.index};
        }
    }

    return allocReg(size, instIdx);
}

RegisterX64 IrRegAllocX64::takeReg(RegisterX64 reg, uint32_t instIdx)
{
    if (reg.size == SizeX64::xmmword)
    {
        if (!freeXmmMap[reg.index])
        {
            CODEGEN_ASSERT(xmmInstUsers[reg.index] != kInvalidInstIdx);
            preserve(function.instructions[xmmInstUsers[reg.index]]);
        }

        CODEGEN_ASSERT(freeXmmMap[reg.index]);
        freeXmmMap[reg.index] = false;
        xmmInstUsers[reg.index] = instIdx;
    }
    else
    {
        if (!freeGprMap[reg.index])
        {
            CODEGEN_ASSERT(gprInstUsers[reg.index] != kInvalidInstIdx);
            preserve(function.instructions[gprInstUsers[reg.index]]);
        }

        CODEGEN_ASSERT(freeGprMap[reg.index]);
        freeGprMap[reg.index] = false;
        gprInstUsers[reg.index] = instIdx;
    }

    return reg;
}

bool IrRegAllocX64::canTakeReg(RegisterX64 reg) const
{
    const std::array<bool, 16>& freeMap = reg.size == SizeX64::xmmword ? freeXmmMap : freeGprMap;
    const std::array<uint32_t, 16>& instUsers = reg.size == SizeX64::xmmword ? xmmInstUsers : gprInstUsers;

    return freeMap[reg.index] || instUsers[reg.index] != kInvalidInstIdx;
}

void IrRegAllocX64::freeReg(RegisterX64 reg)
{
    if (reg.size == SizeX64::xmmword)
    {
        CODEGEN_ASSERT(!freeXmmMap[reg.index]);
        freeXmmMap[reg.index] = true;
        xmmInstUsers[reg.index] = kInvalidInstIdx;
    }
    else
    {
        CODEGEN_ASSERT(!freeGprMap[reg.index]);
        freeGprMap[reg.index] = true;
        gprInstUsers[reg.index] = kInvalidInstIdx;
    }
}

void IrRegAllocX64::freeLastUseReg(IrInst& target, uint32_t instIdx)
{
    if (isLastUseReg(target, instIdx))
    {
        CODEGEN_ASSERT(!target.spilled && !target.needsReload);

        // Register might have already been freed if it had multiple uses inside a single instruction
        if (target.regX64 == noreg)
            return;

        freeReg(target.regX64);
        target.regX64 = noreg;
    }
}

void IrRegAllocX64::freeLastUseRegs(const IrInst& inst, uint32_t instIdx)
{
    auto checkOp = [this, instIdx](IrOp op)
    {
        if (op.kind == IrOpKind::Inst)
            freeLastUseReg(function.instructions[op.index], instIdx);
    };

    checkOp(inst.a);
    checkOp(inst.b);
    checkOp(inst.c);
    checkOp(inst.d);
    checkOp(inst.e);
    checkOp(inst.f);
    checkOp(inst.g);
}

bool IrRegAllocX64::isLastUseReg(const IrInst& target, uint32_t instIdx) const
{
    return target.lastUse == instIdx && !target.reusedReg;
}

void IrRegAllocX64::preserve(IrInst& inst)
{
    IrSpillX64 spill;
    spill.instIdx = function.getInstIndex(inst);
    spill.valueKind = getCmdValueKind(inst.cmd);
    spill.spillId = nextSpillId++;
    spill.originalLoc = inst.regX64;

    // Loads from VmReg/VmConst don't have to be spilled, they can be restored from a register later
    if (!hasRestoreOp(inst))
    {
        unsigned i = findSpillStackSlot(spill.valueKind);

        if (spill.valueKind == IrValueKind::Tvalue)
            build.vmovups(xmmword[sSpillArea + i * 8], inst.regX64);
        else if (spill.valueKind == IrValueKind::Double)
            build.vmovsd(qword[sSpillArea + i * 8], inst.regX64);
        else if (spill.valueKind == IrValueKind::Pointer)
            build.mov(qword[sSpillArea + i * 8], inst.regX64);
        else if (spill.valueKind == IrValueKind::Tag || spill.valueKind == IrValueKind::Int)
            build.mov(dword[sSpillArea + i * 8], inst.regX64);
        else
            CODEGEN_ASSERT(!"Unsupported value kind");

        usedSpillSlots.set(i);

        if (i + 1 > maxUsedSlot)
            maxUsedSlot = i + 1;

        if (spill.valueKind == IrValueKind::Tvalue)
        {
            usedSpillSlots.set(i + 1);

            if (i + 2 > maxUsedSlot)
                maxUsedSlot = i + 2;
        }

        spill.stackSlot = uint8_t(i);
        inst.spilled = true;

        if (stats)
            stats->spillsToSlot++;
    }
    else
    {
        inst.needsReload = true;

        if (stats)
            stats->spillsToRestore++;
    }

    spills.push_back(spill);

    freeReg(inst.regX64);
    inst.regX64 = noreg;
}

void IrRegAllocX64::restore(IrInst& inst, bool intoOriginalLocation)
{
    uint32_t instIdx = function.getInstIndex(inst);

    for (size_t i = 0; i < spills.size(); i++)
    {
        if (spills[i].instIdx == instIdx)
        {
            RegisterX64 reg = intoOriginalLocation ? takeReg(spills[i].originalLoc, instIdx) : allocReg(spills[i].originalLoc.size, instIdx);
            OperandX64 restoreLocation = noreg;

            // Previous call might have relocated the spill vector, so this reference can't be taken earlier
            const IrSpillX64& spill = spills[i];

            if (spill.stackSlot != kNoStackSlot)
            {
                restoreLocation = addr[sSpillArea + spill.stackSlot * 8];
                restoreLocation.memSize = reg.size;

                usedSpillSlots.set(spill.stackSlot, false);

                if (spill.valueKind == IrValueKind::Tvalue)
                    usedSpillSlots.set(spill.stackSlot + 1, false);
            }
            else
            {
                restoreLocation = getRestoreAddress(inst, getRestoreOp(inst));
            }

            if (spill.valueKind == IrValueKind::Tvalue)
                build.vmovups(reg, restoreLocation);
            else if (spill.valueKind == IrValueKind::Double)
                build.vmovsd(reg, restoreLocation);
            else
                build.mov(reg, restoreLocation);

            inst.regX64 = reg;
            inst.spilled = false;
            inst.needsReload = false;

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

    CODEGEN_ASSERT(reg.size != SizeX64::xmmword);

    for (RegisterX64 gpr : kGprAllocOrder)
    {
        if (reg.index == gpr.index)
            return true;
    }

    return false;
}

unsigned IrRegAllocX64::findSpillStackSlot(IrValueKind valueKind)
{
    // Find a free stack slot. Two consecutive slots might be required for 16 byte TValues, so '- 1' is used
    for (unsigned i = 0; i < unsigned(usedSpillSlots.size() - 1); ++i)
    {
        if (usedSpillSlots.test(i))
            continue;

        if (valueKind == IrValueKind::Tvalue && usedSpillSlots.test(i + 1))
        {
            ++i; // No need to retest this double position
            continue;
        }

        return i;
    }

    CODEGEN_ASSERT(!"Nowhere to spill");
    return ~0u;
}

IrOp IrRegAllocX64::getRestoreOp(const IrInst& inst) const
{
    // When restoring the value, we allow cross-block restore because we have commited to the target location at spill time
    if (IrOp location = function.findRestoreOp(inst, /*limitToCurrentBlock*/ false);
        location.kind == IrOpKind::VmReg || location.kind == IrOpKind::VmConst)
        return location;

    return IrOp();
}

bool IrRegAllocX64::hasRestoreOp(const IrInst& inst) const
{
    // When checking if value has a restore operation to spill it, we only allow it in the same block
    IrOp location = function.findRestoreOp(inst, /*limitToCurrentBlock*/ true);

    return location.kind == IrOpKind::VmReg || location.kind == IrOpKind::VmConst;
}

OperandX64 IrRegAllocX64::getRestoreAddress(const IrInst& inst, IrOp restoreOp)
{
    CODEGEN_ASSERT(restoreOp.kind != IrOpKind::None);

    switch (getCmdValueKind(inst.cmd))
    {
    case IrValueKind::Unknown:
    case IrValueKind::None:
        CODEGEN_ASSERT(!"Invalid operand restore value kind");
        break;
    case IrValueKind::Tag:
        return restoreOp.kind == IrOpKind::VmReg ? luauRegTag(vmRegOp(restoreOp)) : luauConstantTag(vmConstOp(restoreOp));
    case IrValueKind::Int:
        CODEGEN_ASSERT(restoreOp.kind == IrOpKind::VmReg);
        return luauRegValueInt(vmRegOp(restoreOp));
    case IrValueKind::Pointer:
        return restoreOp.kind == IrOpKind::VmReg ? luauRegValue(vmRegOp(restoreOp)) : luauConstantValue(vmConstOp(restoreOp));
    case IrValueKind::Double:
        return restoreOp.kind == IrOpKind::VmReg ? luauRegValue(vmRegOp(restoreOp)) : luauConstantValue(vmConstOp(restoreOp));
    case IrValueKind::Tvalue:
        return restoreOp.kind == IrOpKind::VmReg ? luauReg(vmRegOp(restoreOp)) : luauConstant(vmConstOp(restoreOp));
    }

    CODEGEN_ASSERT(!"Failed to find restore operand location");
    return noreg;
}

uint32_t IrRegAllocX64::findInstructionWithFurthestNextUse(const std::array<uint32_t, 16>& regInstUsers) const
{
    if (FFlag::LuauCodeGenRegAutoSpillA64)
    {
        if (currInstIdx == kInvalidInstIdx)
            return kInvalidInstIdx;
    }

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
        CODEGEN_ASSERT(freeXmmMap[reg.index]);
    else
        CODEGEN_ASSERT(freeGprMap[reg.index]);
}

void IrRegAllocX64::assertAllFree() const
{
    for (RegisterX64 reg : kGprAllocOrder)
        CODEGEN_ASSERT(freeGprMap[reg.index]);

    for (bool free : freeXmmMap)
        CODEGEN_ASSERT(free);
}

void IrRegAllocX64::assertNoSpills() const
{
    CODEGEN_ASSERT(spills.empty());
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

void ScopedRegX64::take(RegisterX64 reg)
{
    CODEGEN_ASSERT(this->reg == noreg);
    this->reg = owner.takeReg(reg, kInvalidInstIdx);
}

void ScopedRegX64::alloc(SizeX64 size)
{
    CODEGEN_ASSERT(reg == noreg);
    reg = owner.allocReg(size, kInvalidInstIdx);
}

void ScopedRegX64::free()
{
    CODEGEN_ASSERT(reg != noreg);
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
    startSpillId = owner.nextSpillId;
}

ScopedSpills::~ScopedSpills()
{
    unsigned endSpillId = owner.nextSpillId;

    for (size_t i = 0; i < owner.spills.size();)
    {
        IrSpillX64& spill = owner.spills[i];

        // Restoring spills inside this scope cannot create new spills
        CODEGEN_ASSERT(spill.spillId < endSpillId);

        // If spill was created inside current scope, it has to be restored
        if (spill.spillId >= startSpillId)
        {
            IrInst& inst = owner.function.instructions[spill.instIdx];

            owner.restore(inst, /*intoOriginalLocation*/ true);

            // Spill restore removes the spill entry, so loop is repeated at the same 'i'
        }
        else
        {
            i++;
        }
    }
}

} // namespace X64
} // namespace CodeGen
} // namespace Luau
