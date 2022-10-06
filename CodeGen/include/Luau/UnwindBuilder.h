// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/RegisterX64.h"

#include <stddef.h>
#include <stdint.h>

namespace Luau
{
namespace CodeGen
{

class UnwindBuilder
{
public:
    virtual ~UnwindBuilder() = default;

    virtual void setBeginOffset(size_t beginOffset) = 0;
    virtual size_t getBeginOffset() const = 0;

    virtual void start() = 0;

    virtual void spill(int espOffset, RegisterX64 reg) = 0;
    virtual void save(RegisterX64 reg) = 0;
    virtual void allocStack(int size) = 0;
    virtual void setupFrameReg(RegisterX64 reg, int espOffset) = 0;

    virtual void finish() = 0;

    virtual size_t getSize() const = 0;

    // This will place the unwinding data at the target address and might update values of some fields
    virtual void finalize(char* target, void* funcAddress, size_t funcSize) const = 0;
};

} // namespace CodeGen
} // namespace Luau
