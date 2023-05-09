// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/RegisterA64.h"
#include "Luau/RegisterX64.h"

#include <initializer_list>

#include <stddef.h>
#include <stdint.h>

namespace Luau
{
namespace CodeGen
{

// This value is used in 'finishFunction' to mark the function that spans to the end of the whole code block
static uint32_t kFullBlockFuncton = ~0u;

class UnwindBuilder
{
public:
    enum Arch
    {
        X64,
        A64
    };

    virtual ~UnwindBuilder() = default;

    virtual void setBeginOffset(size_t beginOffset) = 0;
    virtual size_t getBeginOffset() const = 0;

    virtual void startInfo(Arch arch) = 0;
    virtual void startFunction() = 0;
    virtual void finishFunction(uint32_t beginOffset, uint32_t endOffset) = 0;
    virtual void finishInfo() = 0;

    // A64-specific; prologue must look like this:
    //   sub sp, sp, stackSize
    //   store sequence that saves regs to [sp..sp+regs.size*8) in the order specified in regs; regs should start with x29, x30 (fp, lr)
    //   mov x29, sp
    virtual void prologueA64(uint32_t prologueSize, uint32_t stackSize, std::initializer_list<A64::RegisterA64> regs) = 0;

    // X64-specific; prologue must look like this:
    //   optional, indicated by setupFrame:
    //     push rbp
    //     mov rbp, rsp
    //   push reg in the order specified in regs
    //   sub rsp, stackSize
    virtual void prologueX64(uint32_t prologueSize, uint32_t stackSize, bool setupFrame, std::initializer_list<X64::RegisterX64> regs) = 0;

    virtual size_t getSize() const = 0;
    virtual size_t getFunctionCount() const = 0;

    // This will place the unwinding data at the target address and might update values of some fields
    virtual void finalize(char* target, size_t offset, void* funcAddress, size_t funcSize) const = 0;
};

} // namespace CodeGen
} // namespace Luau
