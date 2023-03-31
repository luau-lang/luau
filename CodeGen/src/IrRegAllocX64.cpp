// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrRegAllocX64.h"

namespace Luau
{
namespace CodeGen
{
namespace X64
{

static const RegisterX64 kGprAllocOrder[] = {rax, rdx, rcx, rbx, rsi, rdi, r8, r9, r10, r11};

IrRegAllocX64::IrRegAllocX64(IrFunction& function)
    : function(function)
{
    freeGprMap.fill(true);
    freeXmmMap.fill(true);
}

RegisterX64 IrRegAllocX64::allocGprReg(SizeX64 preferredSize)
{
    LUAU_ASSERT(
        preferredSize == SizeX64::byte || preferredSize == SizeX64::word || preferredSize == SizeX64::dword || preferredSize == SizeX64::qword);

    for (RegisterX64 reg : kGprAllocOrder)
    {
        if (freeGprMap[reg.index])
        {
            freeGprMap[reg.index] = false;
            return RegisterX64{preferredSize, reg.index};
        }
    }

    LUAU_ASSERT(!"Out of GPR registers to allocate");
    return noreg;
}

RegisterX64 IrRegAllocX64::allocXmmReg()
{
    for (size_t i = 0; i < freeXmmMap.size(); ++i)
    {
        if (freeXmmMap[i])
        {
            freeXmmMap[i] = false;
            return RegisterX64{SizeX64::xmmword, uint8_t(i)};
        }
    }

    LUAU_ASSERT(!"Out of XMM registers to allocate");
    return noreg;
}

RegisterX64 IrRegAllocX64::allocGprRegOrReuse(SizeX64 preferredSize, uint32_t index, std::initializer_list<IrOp> oprefs)
{
    for (IrOp op : oprefs)
    {
        if (op.kind != IrOpKind::Inst)
            continue;

        IrInst& source = function.instructions[op.index];

        if (source.lastUse == index && !source.reusedReg)
        {
            LUAU_ASSERT(source.regX64.size != SizeX64::xmmword);
            LUAU_ASSERT(source.regX64 != noreg);

            source.reusedReg = true;
            return RegisterX64{preferredSize, source.regX64.index};
        }
    }

    return allocGprReg(preferredSize);
}

RegisterX64 IrRegAllocX64::allocXmmRegOrReuse(uint32_t index, std::initializer_list<IrOp> oprefs)
{
    for (IrOp op : oprefs)
    {
        if (op.kind != IrOpKind::Inst)
            continue;

        IrInst& source = function.instructions[op.index];

        if (source.lastUse == index && !source.reusedReg)
        {
            LUAU_ASSERT(source.regX64.size == SizeX64::xmmword);
            LUAU_ASSERT(source.regX64 != noreg);

            source.reusedReg = true;
            return source.regX64;
        }
    }

    return allocXmmReg();
}

RegisterX64 IrRegAllocX64::takeReg(RegisterX64 reg)
{
    // In a more advanced register allocator, this would require a spill for the current register user
    // But at the current stage we don't have register live ranges intersecting forced register uses
    if (reg.size == SizeX64::xmmword)
    {
        LUAU_ASSERT(freeXmmMap[reg.index]);
        freeXmmMap[reg.index] = false;
    }
    else
    {
        LUAU_ASSERT(freeGprMap[reg.index]);
        freeGprMap[reg.index] = false;
    }

    return reg;
}

void IrRegAllocX64::freeReg(RegisterX64 reg)
{
    if (reg.size == SizeX64::xmmword)
    {
        LUAU_ASSERT(!freeXmmMap[reg.index]);
        freeXmmMap[reg.index] = true;
    }
    else
    {
        LUAU_ASSERT(!freeGprMap[reg.index]);
        freeGprMap[reg.index] = true;
    }
}

void IrRegAllocX64::freeLastUseReg(IrInst& target, uint32_t index)
{
    if (isLastUseReg(target, index))
    {
        // Register might have already been freed if it had multiple uses inside a single instruction
        if (target.regX64 == noreg)
            return;

        freeReg(target.regX64);
        target.regX64 = noreg;
    }
}

void IrRegAllocX64::freeLastUseRegs(const IrInst& inst, uint32_t index)
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

bool IrRegAllocX64::isLastUseReg(const IrInst& target, uint32_t index) const
{
    return target.lastUse == index && !target.reusedReg;
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
        reg = owner.allocXmmReg();
    else
        reg = owner.allocGprReg(size);
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

} // namespace X64
} // namespace CodeGen
} // namespace Luau
