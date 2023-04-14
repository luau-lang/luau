// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/RegisterX64.h"

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
    virtual ~UnwindBuilder() = default;

    virtual void setBeginOffset(size_t beginOffset) = 0;
    virtual size_t getBeginOffset() const = 0;

    virtual void startInfo() = 0;

    virtual void startFunction() = 0;
    virtual void spill(int espOffset, X64::RegisterX64 reg) = 0;
    virtual void save(X64::RegisterX64 reg) = 0;
    virtual void allocStack(int size) = 0;
    virtual void setupFrameReg(X64::RegisterX64 reg, int espOffset) = 0;
    virtual void finishFunction(uint32_t beginOffset, uint32_t endOffset) = 0;

    virtual void finishInfo() = 0;

    virtual size_t getSize() const = 0;
    virtual size_t getFunctionCount() const = 0;

    // This will place the unwinding data at the target address and might update values of some fields
    virtual void finalize(char* target, size_t offset, void* funcAddress, size_t funcSize) const = 0;
};

} // namespace CodeGen
} // namespace Luau
